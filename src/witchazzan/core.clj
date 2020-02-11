(ns witchazzan.core
  (:gen-class))

(def game-state
  (atom {:game-pieces {} :auto-increment-id 0 :stopwatch (System/currentTimeMillis) :clock 0 :calendar 0}))

(def network-mail
  (atom []))

(defn players [] (filter
                  #(and
                    (not (= false (:active %)))
                    (= "player" (:type %)))
                  (vals (:game-pieces @game-state))))

(defn scene->players [scene] (filter #(= (:scene %) scene) (players)))

(defn sock->player [sock]
  (first (filter #(= (:sock %) sock) (vals (:game-pieces @game-state)))))

(load-file "config/default-config.clj")
(try (load-file "config/config.clj")
     (catch Exception e
       (println "No custom configuration found at config/config.clj.
                Add settings like (setting \"port\" 1234)")))

(load-file "src/witchazzan/comms.clj")
(load-file "src/witchazzan/world.clj")
(load-file "src/witchazzan/behavior.clj")

(-main)
