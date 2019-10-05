(ns witchazzan.core
  (:require [clojure.data.json :as json])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(load-file "config/config.clj")
(require '[org.httpkit.server :as server])

;(defn app [req]
;  {:status  200
;   :headers {"Content-Type" "text/html"}
;   :body    "hello HTTP!"})
;(server/run-server app {:port (:port settings)})

(defn handler [request]
  (server/with-channel request channel
    (server/on-close channel (fn [status] (println "channel closed: " status)))
    (server/on-receive channel (fn [data] ;; echo it back
                          (server/send! channel data)))))
(server/run-server handler {:port (:port settings)})

(def map1 (json/read-str (slurp (str (:tilemap-path settings) (first (:tilemaps settings))))))
