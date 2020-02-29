(ns witchazzan.common
  (:require [clojure.edn :as edn])
  (:require [clojure.java.io :as io])
  (:gen-class))

(when (not (.exists (io/file "config/config.edn")))
  (println "No config file found, creating config/config.edn with defaults.")
  (spit "config/config.edn" (slurp "config/default-config.edn")))

(defonce network-mail (atom {}))
(defonce game-state (atom {}))
(def blank-game-state {:game-pieces {} :auto-increment-id 0
                       :stopwatch (System/currentTimeMillis)
                       :clock 0 :calendar 0})
(def settings (atom (merge
                     (edn/read-string (slurp "config/default-config.edn"))
                     (edn/read-string (slurp "config/config.edn")))))
(defn setting
  ([key value]
   (swap! settings #(merge % {(keyword key) value})))
  ([key] ((keyword key) @settings)))

(defn load-game
  []
  (when (not (.exists (io/file "config/save.edn")))
    (println "No save file found, creating config/save.edn")
    (spit "config/save.edn" blank-game-state))
  (reset! game-state (edn/read-string (slurp "config/save.edn"))))

(defn init []
  (cond
    (setting "auto-load")
    (try (load-game)
         (catch Exception e (println e)
                (println "Failed to load save file, it might be invalid.")))
    :else (reset! game-state blank-game-state)))

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

(defmacro cmap
  "configurable map, single or multi core:"
  [one two]
  (cond
    (setting "threaded")
    `(pmap ~one ~two)
    :else
    `(map ~one ~two)))
