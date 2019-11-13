;;namespace
(ns witchazzan.core
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
  (:gen-class))
;;namespace
;;
;;configuration and global state
(load-file "config/config.clj") ; todo: add defaults and setters
(def players []) ; should this also be an atom?
(def objects [])
(let [id (atom 0)]
  (defn gen-id []
    (swap! id inc)
    @id))

(def game-state (atom {:game-pieces {}}))
;this is a map to leave room for other types of game state
(defn add-game-piece
  "adds a game piece to the global game state
  game pieces must have, at a minumum:
  x y type scene behavior"
  [new-object]
  (let [id (gen-id) obj (merge new-object {:id  id})]
    (swap! ; todo, throw exception when object is invalid
     game-state
     #(merge % {:game-pieces (merge (:game-pieces %) {(keyword (str id)) obj})}))))

(defn update-game-piece
  "adds or replaces attribues in a game-piece
  setting an attribute to null is equivilant to deleting it"
  [id vals]
  (swap!
   game-state
   (fn [state] (update-in state [:game-pieces (keyword (str id))] #(merge % vals)))))

(load-file "src/witchazzan/json-handlers.clj")

(defn process-map
  "returns an immutable representation of a single tilemap,
  including a helper for collisions"
  [map name]
  (let [width (get map "width") height (get map "height")
        syri (get (first (filter #(= (get % "name") "Stuff You Run Into") (get map "layers"))) "data")]
    {:name (first (str/split name #"\."))
     :width width :height height :syri syri
     :tilewidth (get map "tilewidth")
     :get-tile-walkable (fn [coords]
                          (= 0 (get syri (int (+ (:x coords) (* width (:y coords)))))))}))

(def tilemaps (map ; tilemaps don't go in the game state because they are immutable
               #(process-map (json/read-str (slurp (str (:tilemap-path settings) %))) %)
               (:tilemaps settings)))

(defn name->scene [name]
  (first (filter #(= name (:name %)) tilemaps)))

(defn id->object [id]
  (first (filter #(= id (:id %)) (:game-pieces @game-state))))

(defn tile-location
  "takes a player or game object and returns an x and y offset"
  [object]
  (let [tilewidth (:tilewidth (name->scene (:scene object)))]
    {:x (Math/floor (/ (:x object) tilewidth)) :y (Math/floor (/ (:y object) tilewidth))}))
;;configuration and global state
;;
;;websocket infrastructure
(defn make-player ; deprecated
  "defines the structure of a player object"
  [x y sock scene direction]
  (atom {:x x :y y :sock sock :name "unknown-human" :keys {} :id (gen-id)
         :scene scene :direction direction}))
(defn make-object ; deprecated
  "defines the structure of a game piece"
  [x y type scene behavior attributes]
  (atom {:x x :y y :type type :scene scene :id (gen-id) :behavior behavior
         :attributes attributes}))

(defn call-func-by-string
  "(call-func-by-string \"+\" [5 5]) => 10"
  [name args]
  (apply (resolve (symbol name)) args))

(defn handler [request]
  (println "A new player has entered Witchazzan")
  (println request)
  (server/with-channel request channel
    (server/on-receive
     channel
     (fn [data]
       (try ; checking for bad json and that a handler can be found
         (let [message (json/read-str data)
               message-type (get message "message_type")]
           (try ;checking if the function exists
             (call-func-by-string
              (str "witchazzan.core/handle-" message-type) [message channel])
             (catch java.lang.NullPointerException e (println e)))
                                        ;here we are interpreting the "messasge_type" json value as
                                        ;the second half of a function name and calling that function
           )(catch java.lang.Exception e
              (println "invalid json: " data) (println e)))))))
(server/run-server handler {:port (:port settings)})
;;websocket infrastructure
;;
;;game loop
(defn update-clients []
  (when (< 0 (count objects))
    (broadcast
     {:messageType "object-state" :objects
      (map (fn [q] {:id (:id @q) :x (:x @q) :y (:y @q) :type (:type @q)})
           objects)}))
  (when (< 0 (count players))
    (broadcast
     {:messageType "player-state" :players
      (map (fn [q] {:id (:id @q) :x (:x @q) :y (:y @q) :name (:name @q)
                    :scene (:scene @q) :direction (:direction @q)})
           players)})))

(defn process-object-behavior []
  (def objects (remove #(nil? @%) objects))
  (dorun (map #(swap! % (:behavior @%)) objects)))

(defn game-loop []
  (loop []
    (let [start-ms (System/currentTimeMillis)]
      (update-clients)
      (process-object-behavior)
      (Thread/sleep (- (:frame-time settings) (- (System/currentTimeMillis) start-ms))))
    (when (not (:pause settings)) (recur))))

(defn threadify [func] (future (func)))

(when (not (:pause settings)) (threadify game-loop))
;;game loop
