(ns witchazzan.common
  (:require [clojure.edn :as edn])
  (:require [clojure.string :as str])
  (:require [clojure.data.json :as json])
  (:require [clojure.pprint :as pp])
  (:require [clojure.java.io :as io])
  (:require [next.jdbc :as jdbc])
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
    {:name (keyword (first (str/split name #"\.")))
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
        (str (new java.util.Date) " : " data "\n")
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
  "are a and b within n"
  [a b n]
  (and (>= a (- b n)) (<= a (+ b n))))
;;game agnostic helpers
;;state management and access
(defonce game-state (atom
                     {:game-pieces []
                      :auto-increment-id 0
                      :hour 0
                      :day 0
                      :last-updated (System/currentTimeMillis)
                      :start-time (System/currentTimeMillis)}))

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

(defn map-sub?
  "Is the second arg a superset of the first arg?"
  [sub super]
  (every?
   true?
   (map #(or
          (= (second %) (get super (first %)))
          (and
           (number? (second %))
           (number? (get super (first %)))
           ;All numbers are treated as integers for comparison.
           ;This is because tiles, and not the actual location in space,
           ;are the intended use case.
           (== (int (second %)) (int (get super (first %))))))
        sub)))

(defn game-pieces
  "Access game pieces in the form of agents.
  Returns all pieces when called with no args, the piece with a given ID when called
  with an integer, the pieces for which key A matches value B when called with two args,
  and returns all pieces which are a superset of a supplied map when called with a map."
  ([]
   (:game-pieces @game-state))
  ([id]
   (filter #(map-sub? id @%) (game-pieces))))

(defn one-game-piece
  [id]
  (ffilter #(= id (:id @%)) (game-pieces)))

(defn active-pieces
  [& args]
  (filter #(:active @% true) (apply game-pieces args)))

(defn tile-occupied
  [scene coords]
  (seq (filter
        (fn [object]
          (let [ob-coords @object]
            (and
             (= (int (:x coords)) (int (:x ob-coords)))
             (= (int (:y coords)) (int (:y ob-coords))))))
        (game-pieces {:scene scene}))))

;this version of the function is so much nicer but
;it takes longer. Revisit after improving game-pieces
#_(defn tile-occupied
    [scene coords]
    (seq (game-pieces {:scene scene :x (:x coords) :y (:y coords)})))

(defn name->scene [name]
  (first (filter #(= name (:name %)) tilemaps)))

(defn find-empty-tile
  [scene]
  (let [map (name->scene scene) tile-size (max (:width map) (:height map))]
    (->>
     (square-range tile-size)
     (shuffle)
     (filter #((:get-tile-walkable map) %))
     (filter #(not (tile-occupied scene %)))
     (first))))
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

(defn scene-active [scene]
  (seq (active-pieces {:type :player :scene scene})))

(def db
  {:dbtype "mariadb" :dbname (setting :db-db) :user (setting :db-username)
   :password (setting :db-password) :host (setting :db-host)
   :useSSL true :port (setting :db-port) :trustServerCertificate true})
; https://mariadb.com/kb/en/using-tls-ssl-with-mariadb-java-connector/
(def ds (jdbc/get-datasource db))

(comment
  (jdbc/execute! ds ["create table users(id int not null auto_increment,
                     username text, password text, admin tinyint(1) not null,
                     token text,
                     primary key (id));"]))
(comment
  (jdbc/execute! ds ["show tables;"]))
(comment
  (jdbc/execute! ds ["select * from users;"]))
(comment
  (jdbc/execute! ds ["drop table users;"]))
(comment
  (jdbc/execute! ds ["update users set admin = 1 where id=1;"]))
(comment
  (jdbc/execute-one! ds ["alter table users add column token text;"]))
