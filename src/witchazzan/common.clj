(ns witchazzan.common
  (:require [clojure.edn :as edn])
  (:require [clojure.pprint :as pp])
  (:require [clojure.java.io :as io])
  (:require [clojure.core.reducers :as r])
  (:gen-class))

(when (not (.exists (io/file "config/config.edn")))
  (println "No config file found, creating config/config.edn with defaults.")
  (spit "config/config.edn" "{}"))

(def ffilter "(first (filter ))" (comp first filter))

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

(defn rand-nth-safe
  [list]
  "If a list is empty, return nil"
  (cond
    (= (count list) 0)
    nil
    :else
    (rand-nth list)))

(defn players [] (filter
                  #(and
                    (not (= false (:active %)))
                    (= "player" (:type %)))
                  (vals (:game-pieces @game-state))))

(defn load-game
  []
  (when (not (.exists (io/file "config/save.edn")))
    (println "No save file found, creating config/save.edn")
    (spit "config/save.edn" blank-game-state))
  (let [players
        (reduce merge (map
                       (fn [player]
                         {(keyword (str (:id player))) player})
                       (players)))]
    (reset! game-state (edn/read-string (slurp "config/save.edn")))
    (swap! game-state
           (fn [state] (merge state {:game-pieces (merge (:game-pieces state) players)})))))

(defn save
  "Serializes the entire state of play. All mutable state exists in the resulting file"
  []
  (let
   [save-data
    (merge @game-state {:game-pieces
                        (apply merge (map (fn [object]
                                            {(keyword (str (:id object))) object})
                                          (map #(dissoc
                                                 (merge % {:active false}) :sock)
                                               (vals (:game-pieces @game-state)))))})]
    (spit "config/save.edn" (with-out-str (pp/pprint save-data)))
    (slurp "config/save.edn")))

(defn init []
  (cond
    (setting "auto-load")
    (try (load-game)
         (catch Exception e (println e)
                (println "Failed to load save file, it might be invalid.")))
    :else (reset! game-state blank-game-state)))

(defn scene->players-all
  "only for network comms"
  [scene]
  (filter #(= (:scene %) scene) (players)))

(defn scene->players
  [scene]
  (filter #(and (= (:scene %) scene) (not (:dead %))) (players)))

(defn sock->player [sock]
  (first (filter #(= (:sock %) sock) (vals (:game-pieces @game-state)))))

(defn ppmap
  "Partitioned pmap, for grouping map ops together to make parallel
  overhead worthwhile. Thank you Brave Clojure!"
  [grain-size f & colls]
  (apply concat
         (apply pmap
                (fn [& pgroups] (doall (apply map f pgroups)))
                (map (partial partition-all grain-size) colls))))

(defmacro cmap
  "configurable map, single or multi core:"
  [one two]
  (cond
    (= "pmap" (setting "pmap"))
    `(pmap ~one ~two)
    (= "ppmap" (setting "ppmap"))
    `(ppmap 256 ~one ~two)
    ;this isn't really an optimal use of reducers, but that isn't really
    ;the point right now, this macro is just for benchmarking.
    (= "foldcat" (setting "foldcat"))
    `(r/foldcat (r/map ~one ~two))
    :else
    `(map ~one ~two)))

(defn reset
  "delete save, but not player data"
  []
  (io/delete-file "config/save.edn" true)
  (let [players
        (reduce merge (map
                       (fn [player]
                         {(keyword (str (:id player))) player})
                       (players)))]
    (init)
    (swap!
     game-state
     (fn [state] (merge state {:game-pieces players})))))
