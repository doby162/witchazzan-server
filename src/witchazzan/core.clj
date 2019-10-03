(ns witchazzan.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



(load-file "config/config.clj")
(require '[org.httpkit.server :as server])
(defn app [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "hello HTTP!"})
(server/run-server app {:port (:port settings)})
