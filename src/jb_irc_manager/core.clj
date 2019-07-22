(ns jb-irc-manager.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.tools.cli :as cli]
   [clojure.core.async
    :as async
    :refer [>! <! go chan thread timeout]])
  (:gen-class))

;; configuration management

(defn load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  (try
    (with-open [r (io/reader source)]
      (edn/read (java.io.PushbackReader. r)))

    (catch java.io.IOException e
      (println "Couldn't open '%s': %s\n" source (.getMessage e)))))

(defn event-logger
  "Create channel for processing and displaying event data"
  [in]
  (let [out (chan)]
    (go (while true
          (let [event (<! in)]
            (println (str "Type: " (:type event) ", Data: " (:data event))))))))

(def state (atom {:event-channel (chan)
                  :fetch-status "initialising"
                  :accepted-file-status nil}))

(defn import-state
  [config-path]
  (swap! state conj (load-edn config-path)))

(def cli-options
  [["-c" "--config \"path/to/config.edn\"" "Manager configuration file"
    :id :config
    :default "resources/config.edn"
    :parse-fn #(str %)]
   ["-s" "--search \"search term\"" "Resource search term"
    :id :search]
   ["-f" "--fetch \"fetch term\"" "Command for irc bot fetch action"
    :id :fetch]
   ["-h" "--help"]])

;; search utility

(defn space-split
  "Split input to list on space char"
  [s]
  (string/split s #" "))

(defn normalise-text
  "Normalise string for comparative purposes"
  [text]
  (-> text
      string/lower-case
      string/trim))

(defn normalised-includes?
  "Check occurence of normalised text in normalised target string"
  [x y]
  (string/includes? (normalise-text x) (normalise-text y)))

(defn fuzzy-match-search?
  "Return true if all words in search string appear in target, nil otherwise"
  [s search]
  (if (every? (partial normalised-includes? s) (space-split search)) s))

(defn generate-resource-files
  "Return list of file objects found in current path"
  [path]
  (.listFiles (io/file path)))

(defn search-resource-file
  "Return list of matching search entries within resource file"
  [resource search]
  (with-open [rdr (io/reader  resource)]
    (doall (filter #(fuzzy-match-search? % search) (line-seq rdr)))))

(defn search-resources
  "Return list of matching search entries across all resource files in directory (recur?)"
  [resource-path search]
  (reduce #(concat %1 (search-resource-file %2 search))
          '()
          (generate-resource-files resource-path)))

(defn log-map
  "CLI debug println all keys in map"
  [target]
  (doseq [[k v] target]
    (prn (str k ": " v))))

;; irc handling

(defn parse-generic-message
  "Parse irc generic event message to produce local event data"
  [event]
  (if-let [user (.getUser event)]
    {:type "generic-message" :data (str user ": " (.getMessage event))}
    {:type "generic-message" :data (.getMessage event)}))

(defn parse-generic-channel
  "Parse irc generic channel message to produce local event data"
  [event]
  (if-let [channel (.getChannel event)]
    {:type "generic-channel" :data {:channel (.getName channel) :message (.getMessage event)}}
    {:type "generic-channel" :data "no data"}))

(defn parse-join
  "Parse irc generic channel message to produce local event data"
  [event]
  (let [channel (.getChannel event)
        user (.getUser event)]
    {:type "join" :data {:channel (.getName channel) :user (.getRealName user)}}))

(defn parse-incoming-file-transfer
  "Parse irc incoming file transfer to produce local event data"
  [event]
  {:type "incoming-file-transfer" :data {:safe-filename (.getSafeFilename event) :raw-filename (.getRawFilename event)}})

(defn create-temp-file
  "Generate temp file reference"
  [filename]
  (. java.io.File (createTempFile filename ".tmp")))

(defn accept-file-transfer
  "Accept incoming file transfer request with created file reference"
  [event temp-file]
  (-> event
      (.accept temp-file)
      .transfer)
  {:type "accepted-file-transfer" :data temp-file})

(defn push-event
  "Move parsed event data to event channel"
  [event-data]
  (go (>! (:event-channel @state) event-data)))

(defn copy-resource
  "Copy fetched resource from tmp directory"
  [src-path dest-path]
  (io/copy (io/file src-path) (io/file dest-path))
  {:type "copy-resource"})

(defn handle-incoming-file-transfer
  "Handle file creation, transfer, and log for incoming dcc transfer event, returns newly created file reference"
  [event request]
  (let [event-data (parse-incoming-file-transfer event)
        safe-filename (get-in event-data [:data :safe-filename])
        temp-file (create-temp-file safe-filename)]
    (push-event event-data)
    (if (normalised-includes? request safe-filename)
      (let [transfer-event (accept-file-transfer event temp-file)
            dest-path (str (:transfer-file-path @state) safe-filename)]
        (swap! state assoc :accepted-file-status temp-file)
        (push-event transfer-event)
        (push-event (copy-resource temp-file dest-path)))
      (push-event {:type "unexpected-file-transfer" :data (:data event-data)}))))

(defn listener
  "Create proxy for ListenerAdapter with event methods over-ridden"
  [request]
  (proxy [org.pircbotx.hooks.ListenerAdapter] []
    (onGenericMessage [event] (push-event (parse-generic-message event)))
    (onGenericChannel [event] (push-event (parse-generic-channel event)))
    (onJoin [event] (push-event (parse-join event)))
    (onIncomingFileTransfer [event] (handle-incoming-file-transfer event request))))

(defn build-configuration
  "Construct configuration object to initialise pircbotx"
  [connection request]
  (-> (org.pircbotx.Configuration$Builder.)
      (.setName (:nick connection))
      (.setLogin (:login connection))
      (.addServer (:server connection))
      (.addAutoJoinChannel (:channel connection))
      (.addListener (listener request))
      .buildConfiguration))

(defn start-bot
  "Initialse a pircbotx instance and return event data"
  [bot]
  (try
    (let [future-connect (future (.startBot bot))
          connect-status (deref future-connect 5000 "unresolved")]
      {:type "connection status" :data connect-status})
    (catch Exception e
      {:type "connection error" :data (.toString e)})))

(defn bot-status
  "Return bot connection status data"
  [bot]
  {:type "bot status" :data (.isConnected bot)})

(defn bot-quit-server
  "Execute the irc quit server operation and return event data"
  [output]
  (try
    (.quitServer output)
    {:type "server quit" :data "executed"}
    (catch Exception e
      {:type "close error" :data (.toString e)})))

(defn bot-message
  "Call message output to channel"
  [output message]
  (let [channel (get-in @state [:irc :channel])]
    (.message output channel message)
    {:type "message out" :data {:message message :channel channel}}))

(defn poll-incoming-file-transfer
  "Check for incoming file transfer status or timeout"
  [event-channel output]
  (loop [n (range)]
    (if (or (:accepted-file-status @state) (= (first n) 30))
      (async/>!! event-channel (bot-quit-server output))
      (do
        (push-event {:type "poll" :data {:elapsed (* (first n) 2) :status (:accepted-file-status @state)}})
        (Thread/sleep 2000)
        (recur (next n))))))

(defn bot-fetch-resource
  "Run bot to attempt resource fetch"
  [fetch]
  (let [irc (:irc @state)
        event-channel (:event-channel @state)
        connection-config (build-configuration irc fetch)
        bot (org.pircbotx.PircBotX. connection-config)
        output (org.pircbotx.output.OutputIRC. bot)]
    (async/>!! event-channel {:type "fetch-status" :data (:fetch-status @state)})
    (async/>!! event-channel (start-bot bot))
    (async/>!! event-channel (bot-status bot))
    (Thread/sleep 30000)
    (async/>!! event-channel (bot-message output fetch))
    (swap! state assoc :fetch-status "fetching")
    (async/>!! event-channel {:type "fetch-status" :data (:fetch-status @state)})
    (poll-incoming-file-transfer event-channel output)))

;; io definitions

(defn output-search-results
  "CLI interaction collects stdout"
  [search]
  (doseq [result (search-resources (:resource-file-path @state) search)] (prn result)))
  
(defn -main
  "IRC resource management bot"
  [& args]
  (let [options (:options (cli/parse-opts args cli-options))
        {config :config
         search :search
         fetch :fetch} options]
    (import-state config)
    (event-logger (:event-channel @state))
    (async/>!! (:event-channel @state) {:type "options" :data options})
    (if (nil? fetch)
      (output-search-results search)
      (bot-fetch-resource fetch))))

(comment
  (doc Thread/sleep)
  (doc delay?)
  (doc delay)
  (doc thread)
  (doc async/thread)
  (doc async/go)
  (doc async/<!)

  (go (>! event-channel (start-bot bot)))

  (defn fake-sleep []
    (async/thread
      (Thread/sleep 3000)
      "slept"))
  (let [msg (fake-sleep)]
    (async/go (prn (async/<! msg)))
    (prn "no sleep"))

  (start-bot (:bot @bot-state))
  (bot-status (:bot @bot-state))
  (:bot @bot-state)
  (.isConnected (:bot @bot-state))
  (.message (:output @bot-state) (get-in @test-state [:irc :channel]) "test message")
  (stop-bot (:bot @bot-state))

  state (import-state (:config options))
  search (:search options)
  (println (str "Resource path: " (:resource-file-path @state)))
  (println (search-resources (:resource-file-path @state) search))
  (println (str "IRC Server: " (get-in @state [:irc :server])))

  (def test-state (import-state "resources/config.edn"))
  (defn build-bot-state
    [state request]
    (let [irc (:irc @state)
          connection-config (build-configuration irc request)
          bot (org.pircbotx.PircBotX. connection-config)
          output (org.pircbotx.output.OutputIRC. bot)]
      (prn request)
      (atom {:connection-config connection-config
             :bot bot
             :output output})))

  (def bot-state (build-bot-state (import-state "resources/config.edn") "!test .yarnrc (test)")))
