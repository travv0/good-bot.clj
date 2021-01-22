(ns good-bot.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.core.async :refer [chan close!]]
            [clojure.core.match :refer [match]]
            [clojure.tools.logging :as log]
            [discljord.messaging :as discord-rest]
            [discljord.connections :as discord-ws]
            [discljord.formatting :refer [mention-user]]
            [discljord.events :refer [message-pump!]]
            [clj-http.client :as http]
            [nrepl.server :refer [start-server stop-server]]
            [cider.nrepl :refer (cider-nrepl-handler)]))

(defonce state (atom nil))

(defonce bot-id (atom nil))

(defonce config (atom nil))

(defonce db (atom nil))

(defonce server (atom nil))

(defmulti handle-event (fn [type _data] type))

(defn create-message! [channel-id msg]
  (discord-rest/create-message! (:rest @state)
                                channel-id
                                :content msg))

(defn command? [command msg]
  (-> msg str/lower-case (str/starts-with? (str (:command-prefix @config) command))))

(defn drop-command [message]
  (let [without-prefix (str/replace-first message (:command-prefix @config) "")]
    (str/join " " (rest (str/split without-prefix #" ")))))

(defn russian-roulette [guild-id channel-id author]
  (if (= 0 (rand-int 6))
    (let [msg "Bang!"]
      (create-message! channel-id msg)
      (discord-rest/create-guild-ban! (:rest @state) guild-id author :reason msg))
    (create-message! channel-id "Click.")))

(defn build-define-output
  ([word definitions]
   (build-define-output word definitions nil))
  ([word definitions part-of-speech]
   (when-not (empty? definitions)
     (let [definitions (match definitions
                              [d] d
                              defs (str/join
                                    "\n\n"
                                    (map (fn [i def] (str i ". " def))
                                         (drop 1 (range))
                                         defs)))]
       (str "**" word "**"
            (if part-of-speech
              (str " *" part-of-speech "*")
              "")
            "\n" definitions)))))

(defn get-merriam-output [word]
  (when (:dict-key @config)
    (let [response (json/read-str
                    (:body
                     (http/get (str "https://dictionaryapi.com/api/v3/references/collegiate/json/"
                                    word)
                               {:query-params {:key (:dict-key @config)}})))
          result (->> response
                      (map #(build-define-output word
                                                 (get % "shortdef")
                                                 (get % "fl")))
                      (remove nil?)
                      (str/join "\n\n"))]
      (if (empty? result) nil result))))

(defn get-urban-output [word]
  (when (:urban-key @config)
    (let [response (json/read-str
                    (:body
                     (http/get "https://mashape-community-urban-dictionary.p.rapidapi.com/define"
                               {:query-params {:term word}
                                :headers {:x-rapidapi-key (:urban-key @config)
                                          :x-rapidapi-host "mashape-community-urban-dictionary.p.rapidapi.com"
                                          :useQueryString "true"}})))
          result (->> (get response "list")
                      (map #(get % "definition"))
                      (build-define-output word))]
      (if (empty? result) nil result))))

(defn get-define-output [word]
  (or (get-merriam-output word)
      (get-urban-output word)
      (str "No definition found for **" word "**")))

(defn define [msg channel-id]
  (let [phrase (drop-command msg)]
    (if (empty? phrase)
      (create-message! channel-id "Missing word/phrase to define")
      (create-message! channel-id (get-define-output phrase)))))

(defn mentions-me? [mentions]
  (some #{@bot-id} (map :id mentions)))

(defn respond [msg channel-id]
  (let [msg (str/lower-case msg)]
    (create-message!
     channel-id
     (cond (or (str/includes? msg "thanks")
               (str/includes? msg "thank")
               (str/includes? msg "thx")
               (str/includes? msg "thk"))
           "u r welcome"

           (or (str/includes? msg "hi")
               (str/includes? msg "hello")
               (str/includes? msg "yo")
               (str/includes? msg "sup")
               (and (str/includes? msg "what")
                    (str/includes? msg "up"))
               (str/includes? msg "howdy"))
           "hi"

           (or (str/includes? msg "wb")
               (str/includes? msg "welcom")
               (str/includes? msg "welcum"))
           "thx"

           (or (str/includes? msg "mornin")
               (str/includes? msg "gm"))
           "gm"

           (or (str/includes? msg "night")
               (str/includes? msg "gn"))
           "gn"

           (and (str/includes? msg "how")
                (or (str/includes? msg " u")
                    (str/includes? msg " you")))
           "i am fine thank u and u?"

           :else (rand-nth (:responses @db))))))

(defn write-db! []
  (spit (:db-file @config) @db))

(defn add-response [message channel-id]
  (let [response (drop-command message)]
    (if (empty? response)
      (create-message! channel-id "Missing response to add")
      (do
        (swap! db (fn [d] (update d :responses #(vec (distinct (conj % response))))))
        (write-db!)
        (create-message! channel-id (str "Added **" response "** to responses"))))))

(defn remove-response [message channel-id]
  (let [response (drop-command message)]
    (if (empty? response)
      (create-message! channel-id "Missing response to remove")
      (if (some #{response} (:responses @db))
        (do
          (swap! db (fn [d] (update d :responses #(vec (remove (partial = response) %)))))
          (write-db!)
          (create-message! channel-id (str "Removed **" response "** from responses")))
        (create-message! channel-id (str "Response **" response "** not found"))))))

(defn update-status [message channel-id]
  (let [status (drop-command message)]
    (if (empty? status)
      (do (discord-ws/status-update! (:gateway @state))
          (swap! db (fn [d] (dissoc d :status)))
          (create-message! channel-id "Removed status"))
      (do (discord-ws/status-update! (:gateway @state)
                                     :activity (discord-ws/create-activity :name status))
          (swap! db (fn [d] (assoc d :status status)))
          (create-message! channel-id (str "Updated status to **" status "**"))))
    (write-db!)))

(defn list-responses [channel-id]
  (create-message! channel-id (str/join "\n" (:responses @db))))

(defmethod handle-event :message-create
  [_ {:keys [guild-id channel-id author content mentions]}]
  (cond
    (= (:id author) "235148962103951360")
    (create-message! channel-id "Carl is a cuck")

    (not (:bot author))
    (cond (command? "rr" content) (russian-roulette guild-id channel-id author)
          (command? "define" content) (define content channel-id)
          (command? "add" content) (add-response content channel-id)
          (command? "remove" content) (remove-response content channel-id)
          (command? "list" content) (list-responses channel-id)
          (command? "help" content) (create-message! channel-id "lol u dumb")
          (command? "playing" content) (update-status content channel-id)
          (mentions-me? mentions) (respond content channel-id))))

(defmethod handle-event :typing-start
  [_ {:keys [channel-id user-id] :as data}]
  (when (= 0 (rand-int 1000))
    (create-message! channel-id (str "shut up <@" user-id ">"))))

(defmethod handle-event :ready
  [_ _]
  (when (:status @db)
    (discord-ws/status-update! (:gateway @state)
                               :activity (discord-ws/create-activity :name (:status @db)))))

(defmethod handle-event :default [event-type _]
  (log/info "unhandled event:" event-type))

(defn start-bot! [token & intents]
  (let [event-channel (chan 100)
        gateway-connection (discord-ws/connect-bot! token event-channel :intents (set intents))
        rest-connection (discord-rest/start-connection! token)]
    {:events  event-channel
     :gateway gateway-connection
     :rest    rest-connection}))

(defn stop-bot! [{:keys [rest gateway events] :as _state}]
  (discord-rest/stop-connection! rest)
  (discord-ws/disconnect-bot! gateway)
  (close! events))

(defn initialize! [config-file]
  (let [file-config (edn/read-string (slurp config-file))]
    (reset! config
            {:dict-key (let [key (:dict-key file-config)]
                         (if (empty? key) nil key))
             :urban-key (let [key (:urban-key file-config)]
                          (if (empty? key) nil key))
             :command-prefix (or (:command-prefix file-config) "!")
             :db-file (or (:db-file file-config) "db.edn")})
    (reset! db (edn/read-string (try (slurp (:db-file @config)) (catch Exception e))))
    (when-not @server
      (reset! server (start-server :port (:nrepl-port file-config)
                                   :handler cider-nrepl-handler)))
    (when @state
      (stop-bot! @state))
    (reset! state (start-bot! (:token file-config) :guild-messages :guild-message-typing))
    (reset! bot-id (:id @(discord-rest/get-current-user! (:rest @state))))
    file-config))

(defn -main [& args]
  (let [[config-file] args]
    (initialize! (or config-file "config.edn"))
    (try
      (message-pump! (:events @state) handle-event)
      (finally (stop-bot! @state)))))
