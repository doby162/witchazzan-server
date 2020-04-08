(ns witchazzan.common
  (:require [clojure.edn :as edn])
  (:require [clojure.string :as str])
  (:require [clojure.data.json :as json])
  (:require [clojure.pprint :as pp])
  (:require [clojure.java.io :as io])
  (:require [clojure.core.reducers :as r])
  (:gen-class))

;;settings
(when (not (.exists (io/file "config/config.edn")))
  (println "No local config file found, creating config/config.edn.")
  (spit "config/config.edn" "{}"))

(def ffilter "(first (filter ))" (comp first filter))

(def settings (atom (merge
                     (edn/read-string (slurp "config/default-config.edn"))
                     (edn/read-string (slurp "config/config.edn")))))

(defn setting
  ([key value]
   (swap! settings #(merge % {(keyword key) value})))
  ([key] ((keyword key) @settings)))

(defn process-map
  "returns an immutable representation of a single tilemap,
  including a helper for collisions"
  [data name]
  (let [width (get data "width")
        height (get data "height")
        layers (get data "layers")
        syri (get (ffilter #(= (get % "name") "Stuff You Run Into") layers) "data")
        teleport (get (ffilter #(= (get % "name") "Teleport") layers) "layers")
        objects (get (ffilter #(= (get % "name") "Objects") layers) "objects")]
    {:name (first (str/split name #"\."))
     :width width
     :height height
     :layers layers
     :syri syri ; stuff you run into
     :teleport teleport
     :tilewidth (get data "tilewidth")
     :objects objects
     :get-tile-walkable (fn [coords] (= 0 (get syri (+ (int (:x coords)) (* width (int (:y coords)))))))}))

(def tilemaps (map ; tilemaps don't go in the game state because they are immutable
               #(process-map (json/read-str (slurp (str (setting "tilemap-path") %))) %)
               (setting "tilemaps")))
;;settings
;;game agnostic helpers
(defn square-range
  "like range but for coordinates. Delivers coords with 0.5 added to center
  pieces on tiles"
  [size]
  (map
   #(zipmap '(:x :y) (list (+ 0.5 (quot % size)) (+ 0.5 (rem % size))))
   (range (* size size))))

(defn thread-debug
  "both print and return the value"
  [x]
  (pp/pprint x)
  x)

(defn realize-map
  "Just do the math you lazy bum"
  [coll] (run! #(doall (second %)) coll))

(defn log [data]
  (when (setting "log-to-repl") (println data))
  (spit "config/log"
        (str (System/currentTimeMillis) " : " data "\n")
        :append true))

(defn ppmap
  "Partitioned pmap, for grouping map ops together to make parallel
  overhead worthwhile. Thank you Brave Clojure!"
  [grain-size f & colls]
  (apply concat
         (apply pmap
                (fn [& pgroups] (doall (apply map f pgroups)))
                (map (partial partition-all grain-size) colls))))

(defn within-n
  [a b n]
  (and (>= a (- b n)) (<= a (+ b n))))
;;game agnostic helpers
;;state management and access
(defonce game-state (atom
                     {:game-pieces []
                      :auto-increment-id 0}))

#_(defn load-game
    []
    (when (not (.exists (io/file "config/save.edn")))
      (log "No save file found, creating config/save.edn")
      (spit "config/save.edn" blank-game-state))
    (let [players
          (reduce merge (map
                         (fn [player]
                           {(keyword (str (:id player))) player})
                         (players)))]
      (reset! game-state (edn/read-string (slurp "config/save.edn")))
      (swap! game-state
             (fn [state] (merge state {:game-pieces (merge (:game-pieces state) players)})))))

#_(defn save
    "Serializes the entire state of play. All mutable state exists in the resulting file"
    []
    (let
     [save-data
      (merge @game-state {:game-pieces
                          (apply merge (map (fn [object]
                                              {(keyword (str (:id object))) object})
                                            (map #(dissoc
                                                   (merge % {:active false}) :socket)
                                                 (vals (:game-pieces @game-state)))))})]
      (spit "config/save.edn" (with-out-str (pp/pprint save-data)))
      (slurp "config/save.edn")))

#_(defn reset
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

(defn game-pieces
  ([]
   (:game-pieces @game-state))
  ([id]
   (ffilter #(= id (:id @%)) (game-pieces)))
  ([key value]
   (filter #(= value ((keyword key) @%)) (game-pieces))))

(defn tile-occupied
  [scene coords]
  (not (empty? (filter
                (fn [object]
                  (let [ob-coords @object]
                    (and
                     (= (int (:x coords)) (int (:x ob-coords)))
                     (= (int (:y coords)) (int (:y ob-coords))))))
                (game-pieces)))))

(defn name->scene [name]
  (first (filter #(= name (:name %)) tilemaps)))

(defn find-empty-tile
  ;tood FIXME
  [scene]
  (let [map (name->scene scene) tile-size (max (:width map) (:height map))]
    (->>
     (square-range tile-size)
     (shuffle)
     (filter #((:get-tile-walkable map) %))
     (filter #(not (tile-occupied scene %)))
     (take 1))))
;;state management and access
;;init
(defn init []
  #_(cond
      (setting "auto-load")
      (try (load-game)
           (catch Exception e (log e)
                  (log "Failed to load save file, it might be invalid.")))
      :else (reset! game-state blank-game-state)))
;;init
(let [count (atom 0)]
  (defn gen-id [] (swap! count #(+ 1 %))))
