(ns bigbot.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.core.async :refer [chan close!]]
            [clojure.core.match :refer [match]]
            [discljord.messaging :as discord-rest]
            [discljord.connections :as discord-ws]
            [discljord.formatting :refer [mention-user]]
            [discljord.events :refer [message-pump!]]
            [clj-http.client :as http]
            [nrepl.server :refer [start-server stop-server]]
            [cider.nrepl :refer (cider-nrepl-handler)]))

(defonce server (start-server :port 7888 :handler cider-nrepl-handler))

(defonce state (atom nil))

(defonce bot-id (atom nil))

(def config (edn/read-string (slurp "config.edn")))

(def dict-key (:dict-key config))

(def urban-key (:urban-key config))

(defmulti handle-event (fn [type _data] type))

(defn russian-roulette? [msg]
  (-> msg str/lower-case (str/starts-with? "!rr")))

(defn russian-roulette [guild-id channel-id author]
  (if (= 0 (rand-int 6))
    (let [msg "Bang!"]
      (discord-rest/create-message! (:rest @state) channel-id :content msg)
      (discord-rest/create-guild-ban! (:rest @state) guild-id author :reason msg))
    (discord-rest/create-message! (:rest @state) channel-id :content "Click.")))

(defn define? [msg]
  (-> msg str/lower-case (str/starts-with? "!define ")))

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
  (let [response (json/read-str
                  (:body
                   (http/get (str "https://dictionaryapi.com/api/v3/references/collegiate/json/"
                                  word)
                             {:query-params {:key dict-key}})))
        result (->> response
                    (map #(build-define-output word
                                               (get % "shortdef")
                                               (get % "fl")))
                    (remove nil?)
                    (str/join "\n\n"))]
    (if (empty? result) nil result)))

(defn get-urban-output [word]
  (let [response (json/read-str
                  (:body
                   (http/get "https://mashape-community-urban-dictionary.p.rapidapi.com/define"
                             {:query-params {:term word}
                              :headers {:x-rapidapi-key urban-key
                                        :x-rapidapi-host "mashape-community-urban-dictionary.p.rapidapi.com"
                                        :useQueryString "true"}})))
        result (->> (get response "list")
                    (map #(get % "definition"))
                    (build-define-output word))]
    (if (empty? result) nil result)))

(defn get-define-output [word]
  (or (get-merriam-output word)
      (get-urban-output word)
      (str "No definition found for **" word "**")))

(defn define [msg channel-id]
  (let [phrase (str/join " " (rest (str/split msg #" ")))]
    (discord-rest/create-message! (:rest @state)
                                  channel-id
                                  :content (get-define-output phrase))))

(defn mentions-me? [mentions]
  (some #{@bot-id} (map :id mentions)))

(defn respond [msg channel-id]
  (let [msg (str/lower-case msg)]
    (discord-rest/create-message!
     (:rest @state)
     channel-id
     :content
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

           :else (rand-nth ["what u want" "stfu" "u r ugly" "i love u"])))))

(defn liar? [msg]
  (-> msg str/lower-case (str/starts-with? "-liar")))

(defn create-message [channel-id msg]
  (discord-rest/create-message! (:rest @state)
                                channel-id
                                :content msg))

(defmethod handle-event :message-create
  [_ {:keys [guild-id channel-id author content mentions]}]
  (when-not (:bot author)
    (cond (russian-roulette? content) (russian-roulette guild-id channel-id author)
          (define? content) (define content channel-id)
          (liar? content) (do (Thread/sleep 500)
                              (create-message channel-id "Carl is a cuck"))
          (mentions-me? mentions) (respond content channel-id))))

(defmethod handle-event :typing-start
  [_ {:keys [channel-id author content]}]
  (when (= 0 (rand-int 1000))
    (discord-rest/create-message! (:rest @state)
                                  channel-id
                                  :content (str "shut up <@" author ">"))))

(defmethod handle-event :ready
  [_ _]
  (when (:playing config)
    (discord-ws/status-update! (:gateway @state)
                               :activity (discord-ws/create-activity :name (:playing config)))))

(defmethod handle-event :default [_ _])

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

(defn -main [& args]
  (reset! state (start-bot! (:token config) :guild-messages))
  (reset! bot-id (:id @(discord-rest/get-current-user! (:rest @state))))
  (try
    (message-pump! (:events @state) handle-event)
    (finally (stop-bot! @state))))
