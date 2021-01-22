(defproject good-bot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.suskalo/discljord "1.1.1"]
                 [org.clojure/data.json "1.0.0"]
                 [org.clojure/core.match "1.0.0"]
                 [clj-http "3.11.0"]
                 [nrepl "0.8.3"]
                 [cider/cider-nrepl "0.25.2"]]
  :repl-options {:init-ns good-bot.core}
  :main good-bot.core)
