;;namespace
(ns witchazzan.world
  (:refer witchazzan.common)
  (:require [witchazzan.comms :as comms])
  (:require [witchazzan.behavior :as behavior])
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
  (:require [clojure.java.io :as io])
  (:gen-class))
(declare coordinate-spawns)
(declare name->scene)
(declare clear-corrupted)
(declare square-range)
;;namespace
;;configuration and global state
#_(defn gen-id! []
  (swap! game-state #(merge % {:auto-increment-id (inc (:auto-increment-id %))}))
  (:auto-increment-id @game-state))

#_(defn hourglass! []
  (coordinate-spawns)
  (swap! game-state #(merge % {:stopwatch (System/currentTimeMillis) :clock (inc (:clock %))}))
  (when (< 23 (:clock @game-state))
    (swap! game-state #(merge % {:clock 0 :calendar (inc (:calendar %))}))
    (when (setting "auto-save") (save))
    (log (str "day " (:calendar @game-state))))
  (when (= (:clock @game-state) 6)
    (comms/broadcast
     {:messageType "chat" :name "Witchazzan.core" :id -1
      :content (str "Dawn of day " (:calendar @game-state))}))
  (when (= (:clock @game-state) 20)
    (comms/broadcast
     {:messageType "chat" :name "Witchazzan.core" :id -1
      :content "Night falls"}))
  (comms/broadcast {:time (:clock @game-state)}))

#_(defn add-game-piece!
  "adds a game piece to the global game state
  game pieces must have, at a minumum:
  x y type scene behavior"
  [new-object]
  (let [id (gen-id!) obj (merge new-object {:id id :delete-me false})]
    (swap! ; todo, throw exception when object is invalid
     game-state
     (fn [%]
       (merge % {:game-pieces (merge (:game-pieces %) {(keyword (str id)) obj})})))
    id))

#_(defn update-game-piece!
  "adds or replaces attribues in a game-piece
  setting an attribute to null is equivilant to deleting it"
  [id vals]
  (swap!
   game-state
   (fn [state] (update-in state [:game-pieces (keyword (str id))] #(merge % vals)))))

#_(defn trash-filter
  "does all the work for collect-garbage"
  [game-state]
  (merge ; replace the game-pieces structure with one that is missing deleted pieces
   game-state
   {:game-pieces
    (into {} (filter (fn [piece]
                       (= false (:delete-me (second piece))))
                     (:game-pieces game-state)))}))

#_(defn collect-garbage!
  "removes game-pieces with the delete-me attribute set to true"
  []
  (swap! game-state trash-filter))

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
     ;refactor me (get-tile-walkable)
     :get-tile-walkable (fn [coords] (= 0 (get syri (+ (int (:x coords)) (* width (int (:y coords)))))))}))

(def tilemaps (map ; tilemaps don't go in the game state because they are immutable
               #(process-map (json/read-str (slurp (str (setting "tilemap-path") %))) %)
               (setting "tilemaps")))

(defn name->scene [name]
  (first (filter #(= name (:name %)) tilemaps)))

;;configuration and global state
;;
;;websocket infrastructure
(defn call-func-by-string
  "(call-func-by-string \"+\" [5 5]) => 10"
  [name args]
  (try
    (apply (resolve (symbol name)) args)
    (catch NullPointerException e
      (log (str "call-func-by-string failed: " name " " e)))))

(defn handler [request]
  (log "A player has entered Witchazzan!")
  (server/with-channel request channel
    (server/on-close
     channel
     (fn [data]
       #_(update-game-piece! (:id (sock->player channel)) {:active false})))
    (server/on-receive
     channel
     (fn [data]
       (try ; checking for bad json and that a handler can be found
         (let [message (json/read-str data)
               message-type (get message "message_type")]
           (try ;checking if the function exists
             (call-func-by-string
              (str "witchazzan.comms/handle-" message-type) [message channel])
             (catch java.lang.NullPointerException e (log e)))
                                        ;here we are interpreting the "messasge_type" json value as
                                        ;the second half of a function name and calling that function
           )(catch java.lang.Exception e
              (log "invalid json: " data) (log e)))))))
;;websocket infrastructure
;;
;;game loop
#_(defn update-clients []
  (run!
   (fn [tilemap] (comms/broadcast
                  {:messageType "game-piece-list"
                   :pieces (map (fn [%] (dissoc % :sock))
                                (scene->pieces (:name tilemap)))}
                  (scene->players-all (:name tilemap))))
   tilemaps))


(defn threadify [func] (future (func)))
;;game loop
;;nature
(defn square-range
  "like range but for coordinates. Delivers coords with 0.5 added to center
  pieces on tiles"
  [size]
  (map
   #(zipmap '(:x :y) (list (+ 0.5 (quot % size)) (+ 0.5 (rem % size))))
   (range (* size size))))

(defn generate-genes
  "makes a list of keywords into a map of ints, arbitrarily limited by settings"
  [& keywords]
  (zipmap keywords
          (repeatedly #(rand-int (+ 1 (setting "gene-max"))))))

;;nature
;;admin stuff

(defn ten-x []
  (setting "millis-per-hour" (/ (setting "millis-per-hour") 10))
  (setting "millis-per-frame" (/ (setting "millis-per-frame") 10)))

(defn tenth-x []
  (setting "millis-per-hour" (* (setting "millis-per-hour") 10))
  (setting "millis-per-frame" (* (setting "millis-per-frame") 10)))

(defn short-day []
  (setting "millis-per-hour" 600))

#_(defn seed-nature []
  (run! (fn [scene] (spawn-carrot (:name scene))) tilemaps))

;;admin stuff
(defn main
  [& args]
  (log "Booting...")
  (server/run-server handler {:port (setting "port")})
  (log (str "Running server on port " (setting "port")))
  #_(when (not (setting "pause"))
    (threadify game-loop)
    (seed-nature)))

#_(defn spawn-points
  "assumes spawn-type is both a function and a valid object name, upgrade this to take a list later"
  [type & rand]
  (let [coord-pairs
        (filter #(:x %) ;check if valid coords were returned
                (map (fn [tilemap] ; assume one spawn of type per scene because it's easy
                       (let [properties
                             (ffilter
                              #(= (str "spawn-" type) (get % "name"))
                              (:objects tilemap))]
                         (when properties
                           {:scene (:name tilemap)
                            :x (/ (get properties "x") (:tilewidth tilemap))
                            :y (/ (get properties "y") (:tilewidth tilemap))})))
                     tilemaps))]
    (cond
      rand
      (run! #(call-func-by-string (str "witchazzan.world/spawn-" type)
                                  (list (:scene %) (find-empty-tile (:scene %))))
            coord-pairs)
      :else
      (run! #(call-func-by-string (str "witchazzan.world/spawn-" type) (list (:scene %) %))
            coord-pairs))))

#_(defn coordinate-spawns []
  (when (and
         (< (:clock @game-state) 5)
         (< (count (filter #(= (:type %) "slime") (objects))) 5))
    (spawn-points "slime"))
  (when (and
         (> (:clock @game-state) 20)
         (< (count (filter #(= (:type %) "carrot") (objects))) 5))
    (spawn-points "carrot" true)))
