(ns witchazzan.common
  (:gen-class))

(def game-state)

(load-file "config/default-config.clj")
(try (load-file "config/config.clj")
     (catch Exception e
       (println "No custom configuration found at config/config.clj.
                Add settings like (setting \"port\" 1234)")))
(defn load-game
  "reads and executes the code stored in config/save.clj, repopulating the game-state"
  []
  (reset! game-state (read-string (slurp "config/save.clj"))))

(defn init []
  (def game-state
    (atom {:game-pieces {} :auto-increment-id 0 :stopwatch (System/currentTimeMillis) :clock 0 :calendar 0}))

  (try (when (setting "auto-load") (load-game))
       (catch Exception e (println "Failed to load save file")))

  (def network-mail
    (atom [])))

(defn players [] (filter
                  #(and
                    (not (= false (:active %)))
                    (= "player" (:type %)))
                  (vals (:game-pieces @game-state))))

(defn scene->players-all
  "only for network comms"
  [scene]
  (filter #(= (:scene %) scene) (players)))

(defn scene->players
  [scene]
  (filter #(and (= (:scene %) scene) (not (:dead %))) (players)))

(defn sock->player [sock]
  (first (filter #(= (:sock %) sock) (vals (:game-pieces @game-state)))))
