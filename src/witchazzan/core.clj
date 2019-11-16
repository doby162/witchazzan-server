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

(def game-state (atom {:game-pieces {} :auto-increment-id 0}))

(defn objects [] (filter #(not (= "player" (:type %))) (vals (:game-pieces @game-state))))

(defn players [] (filter #(= "player" (:type %))) (vals (:game-pieces @game-state)))

(defn gen-id []
  (swap! game-state #(merge % {:auto-increment-id (inc (:auto-increment-id %))}))
  (:auto-increment-id @game-state))

;this is a map to leave room for other types of game state
(defn add-game-piece
  "adds a game piece to the global game state
  game pieces must have, at a minumum:
  x y type scene behavior"
  [new-object]
  (let [id (gen-id) obj (merge new-object {:id  id :delete-me false})]
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

(defn trash-filter
  "does all the work for collect-garbage"
  [game-state]
  (merge ; replace the game-pieces structure with one that is missing deleted pieces
   game-state
   {:game-pieces
    (into {} (filter (fn [piece]
                       (= false (:delete-me (second piece))))
                     (:game-pieces game-state)))}))

(defn collect-garbage
  "removes game-pieces with the delete-me attribute set to true"
  []
  (swap! game-state trash-filter))

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
  (when (< 0 (count (objects)))
    (broadcast
     {:messageType "object-state" :objects
      (map (fn [%] {:id (:id %) :x (:x %) :y (:y %) :type (:type) :scene (:scene %)})
           (objects))}))
  (when (< 0 (count (players)))
    (broadcast
     {:messageType "player-state" :players
      (map (fn [%] {:id (:id %) :x (:x %) :y (:y %) :name (:name %)
                    :scene (:scene %) :direction (:direction %)})
           (players))})))

(defn process-object-behavior
  "run the :behavior method of every game-piece, it takes a state
  and returns a new state"
  []
  (run!
   (fn [object]
     (update-game-piece (:id object) ((:behavior object) object)))
   (vals (:game-pieces @game-state))))

(defn game-loop []
  (loop []
    (let [start-ms (System/currentTimeMillis)]
      (update-clients)
      (process-object-behavior)
      (collect-garbage)
      (try (Thread/sleep
            (- (:frame-time settings) (- (System/currentTimeMillis) start-ms))) (catch Exception e)))
    (when (not (:pause settings)) (recur))))

(defn threadify [func] (future (func)))

(when (not (:pause settings)) (threadify game-loop))
;;game loop
