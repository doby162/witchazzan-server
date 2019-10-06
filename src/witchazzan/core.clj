;;namespace
(ns witchazzan.core
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
  (:gen-class))
;;namespace
;;
;;configuration and global state
(load-file "config/config.clj")
(def channels [])
(def map1 (json/read-str (slurp (str (:tilemap-path settings) (first (:tilemaps settings))))))
;;configuration and global state
;;
;;websocket infrastructure
(defn handler [request]
  (println "A new player has entered Witchazzan")
  (println request)
  (server/with-channel request channel
    (def channels (conj channels channel)); add this to our collection of channels
    (server/on-close channel (fn [status]
                               (def channels (filter #(not (= % channel)) channels))
                               (println "channel closed: " status)))
    (server/on-receive channel (fn [data] ;; echo it back
                          (server/send! channel data)))))

(defn broadcast [channels data]
  (map (fn [channel] (server/send! channel data)) channels))

(server/run-server handler {:port (:port settings)})

;;websocket infrastructure
