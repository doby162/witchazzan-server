;;namespace
(ns witchazzan.world
  (:refer witchazzan.common)
  (:require [witchazzan.comms :as comms])
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
  (:require [clojure.java.io :as io])
  (:gen-class))
(declare coordinate-spawns)
(declare within-n)
(declare name->scene)
(declare log)
(declare clear-corrupted)
(declare square-range)
;;namespace
;;
;;configuration and global state

(defn objects [] (filter #(not (= "player" (:type %))) (vals (:game-pieces @game-state))))

(defn scene->pieces [scene] (filter #(and
                                      (= (:scene %) scene)
                                      (not (and (= "player" (:type %)) (= false (:active %))))
                                      (not (:dead %)))
                                    (vals (:game-pieces @game-state))))

(defn id->piece [id] ((keyword (str id)) (:game-pieces @game-state)))

(defn gen-id! []
  (swap! game-state #(merge % {:auto-increment-id (inc (:auto-increment-id %))}))
  (:auto-increment-id @game-state))

(defn hourglass! []
  (when (>= (System/currentTimeMillis) (+ (setting "millis-per-hour") (:stopwatch @game-state)))
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
    (comms/broadcast {:time (:clock @game-state)})))

;this is a map to leave room for other types of game state
(defn add-game-piece!
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

(defn update-game-piece!
  "adds or replaces attribues in a game-piece
  setting an attribute to null is equivilant to deleting it"
  [id vals]
  (swap!
   game-state
   (fn [state] (update-in state [:game-pieces (keyword (str id))] #(merge % vals)))))

(defn trash-filter
  "does all the work for collect-garbage"
  [game-state]
  (merge ; replace the game-pieces structure with one that is missing deleted pieces
   game-state
   {:game-pieces
    (into {} (filter (fn [piece]
                       (= false (:delete-me (second piece))))
                     (:game-pieces game-state)))}))

(defn collect-garbage!
  "removes game-pieces with the delete-me attribute set to true"
  []
  (swap! game-state trash-filter))

(defn process-map
  "returns an immutable representation of a single tilemap,
  including a helper for collisions"
  [map name]
  (let [width (get map "width") height (get map "height")
        syri (get (first (filter #(= (get % "name") "Stuff You Run Into") (get map "layers"))) "data")
        objects (get (first (filter #(= (get % "name") "Objects") (get map "layers"))) "objects")]
    {:name (first (str/split name #"\."))
     :width width :height height :syri syri
     :objects objects
     :tilewidth (get map "tilewidth")
     :teleport
     (fn [coords]
       (let [tele (filter
                   #(and
                     (= "SwitchToScene" (get % "type"))
                     (within-n (:x coords) (+ 8 (get % "x")) (get % "width"))
                     (within-n (:y coords) (+ 8 (get % "y")) (get % "height")))
                   objects)]
         (when (= 1 (count tele))
           (let [result (first (filter #(and
                                         (= "Entrance" (get % "type"))
                                         (= (get (first (second (first (first tele)))) "value") (get % "name")))
                     ;this line assumes that the Entrance's first property is it's name
                     ;no, this is not a good assumption
                                       (:objects (name->scene (get (first tele) "name")))))]
             {:scene (get (first tele) "name")
              :x (get result "x")
              :y (get result "y")
              :teleport-debounce true}))))
     :get-tile-walkable (fn [coords]
                          (= 0 (get syri (int (+ (:x coords) (* width (:y coords)))))))}))

(def tilemaps (map ; tilemaps don't go in the game state because they are immutable
               #(process-map (json/read-str (slurp (str (setting "tilemap-path") %))) %)
               (setting "tilemaps")))

(defn name->scene [name]
  (first (filter #(= name (:name %)) tilemaps)))

(defn tile-location
  "takes a player or game object and returns an x and y offset"
  [object]
  (let [tilewidth (:tilewidth (name->scene (:scene object)))]
    {:x (quot (:x object) tilewidth) :y (quot (:y object) tilewidth)}))

(defn pixel-location
  "takes a pair of coordinates and returns pixel values"
  [scene coords]
  (let [width (:tilewidth (name->scene scene))]
    {:x (* width (:x coords)) :y (* width (:y coords))}))
;;configuration and global state
;;
;;websocket infrastructure
(defn call-func-by-string
  "(call-func-by-string \"+\" [5 5]) => 10"
  [name args]
  (try
    (apply (resolve (symbol name)) args)
    (catch NullPointerException e
      (log (str "call-func-by-string failed: " name)))))

(defn method
  "shorthand to call game-piece methods"
  [object key args]
  (try
    (call-func-by-string (str "witchazzan.behavior/" (get object key)) (conj args object))
    (catch Exception e
      (log (str "method failed: " e)))))

(defn handler [request]
  (println "A player has entered Witchazzan!")
  (server/with-channel request channel
    (server/on-close
     channel
     (fn [data]
       (update-game-piece! (:id (sock->player channel)) {:active false})))
    (server/on-receive
     channel
     (fn [data]
       (try ; checking for bad json and that a handler can be found
         (let [message (json/read-str data)
               message-type (get message "message_type")]
           (try ;checking if the function exists
             (call-func-by-string
              (str "witchazzan.comms/handle-" message-type) [message channel])
             (catch java.lang.NullPointerException e (println e)))
                                        ;here we are interpreting the "messasge_type" json value as
                                        ;the second half of a function name and calling that function
           )(catch java.lang.Exception e
              (println "invalid json: " data) (println e)))))))
;;websocket infrastructure
;;
;;game loop
(defn update-clients []
  (run!
   (fn [tilemap] (comms/broadcast
                  {:messageType "game-piece-list"
                   :pieces (map (fn [%] (dissoc % :sock))
                                (scene->pieces (:name tilemap)))}
                  (scene->players-all (:name tilemap))))
   tilemaps))

(defn process-behavior
  "calls the behavior method on all pieces to calculate the next frame"
  [this]
  {(first this) (method (second this) :behavior (list))})

(defn process-mail
  "calls the handle-mail method on all pieces to calculate the next frame"
  [this]
  {(first this) (method (second this) :handle-mail (list))})

(defn clear-outbox
  "clears outbox to avoid duplicate processing"
  [this]
  {(first this) (merge (second this) {:outbox nil})})

(defn process-objects!
  "coordinates the process-behavior function accross the game-pieces"
  [fun]
  (swap!
   game-state
   (fn [state]
     (->>
      (:game-pieces state)
      (cmap fun)
      (apply merge)
      (vector :game-pieces)
      (merge state)))))

(defn create-objects!
  [mail-queue]
  (run!
   (fn [message]
     (add-game-piece! message))
   mail-queue))

(defn attach-mail [mail-queue piece]
  (letfn [(filt [item] (= (:id (second piece)) (:mail-to item)))
          (not-filt [item] (not (= (:id (second piece)) (:mail-to item))))]
    (let [net-mail @network-mail]
      (swap! network-mail
           ;complement takes the boolean inverse of a function
             #(filter not-filt %))
      {(first piece)
       (merge (second piece)
              {:inbox
               (filter filt mail-queue)}
              {:net-inbox
               (filter filt net-mail)})})))

(defn mail-room
  "puts the mail where it needs to go"
  [mail-queue]
  (create-objects! (filter #(= "new-object" (:mail-to %)) (apply conj mail-queue @network-mail)))
  (swap! network-mail
         #(filter (fn [this] (not (= "new-object" (:mail-to this)))) %))
  ;remove create object commands from mail queue.
  ;TODO either merge mail and net mail completly or make a general solution for this nonsens
  (process-objects! #(attach-mail mail-queue %)))

(def empty-tiles (atom {}))

(defn tile-occupied
  [scene coords]
  (not (empty? (filter
                (fn [object]
                  (let [ob-coords (tile-location object)]
                    (and
                     (= (:x coords) (:x ob-coords))
                     (= (:y coords) (:y ob-coords)))))
                (scene->pieces scene)))))

(defn find-empty-tiles
  "returns the coordinates of a random empty tile from a map"
  [scene]
  (let [map (name->scene scene) tile-size (max (:width map) (:height map))]
    (->>
     (square-range tile-size)
     (filter #((:get-tile-walkable map) %))
     (filter #(not (tile-occupied scene %))))))

(defn set-empty-tiles []
  (run!
   (fn [scene]
     (let [scene-name (subs scene 0 (- (count scene) 5))]
       (swap! empty-tiles
              #(merge % {(keyword scene-name) (find-empty-tiles scene-name)}))))
   (setting "tilemaps")))

(defn find-empty-tile
  [scene]
  (pixel-location
   scene
   (rand-nth
    ((keyword scene) @empty-tiles))))

(defn game-loop []
  (loop []
    (let [start-ms (System/currentTimeMillis)]
      (hourglass!)
      (set-empty-tiles)
      (process-objects! process-behavior)
      (let
       [mail-queue
        (filter (fn [item] (not (nil? item)))
                (cmap #(:outbox (second %)) (:game-pieces @game-state)))]
        (mail-room mail-queue))
      (process-objects! clear-outbox)
      (process-objects! process-mail)
      (collect-garbage!)
      (update-clients)

      ;temporary mesure to see if I can track down a bug
      (when (> (count (filter (fn [object] (= nil (:x object))) (objects))) 0)
        (clear-corrupted)
        (log "detected corrupted objects"))

      (try (Thread/sleep
            (- (setting "millis-per-frame") (- (System/currentTimeMillis) start-ms)))
           (catch Exception e
             (log "long frame"))))
    (when (not (setting "pause")) (recur))))

(defn threadify [func] (future (func)))
;;game loop
;;nature
(defn square-range
  "like range but for coordinates"
  [size]
  (map
   #(zipmap '(:x :y) (list (quot % size) (rem % size)))
   (range (* size size))))

(defn generate-genes
  "makes a list of keywords into a map of ints, arbitrarily limited by settings"
  [& keywords]
  (zipmap keywords
          (repeatedly #(rand-int (+ 1 (setting "gene-max"))))))

(defn within-n
  [a b n]
  (and (>= a (- b n)) (<= a (+ b n))))

(defn spawn-slime
  "create a slime in the world"
  [scene & mods]
  (def scene scene)
  (def mods mods)
  (add-game-piece!
   (conj
    (find-empty-tile scene)
    {:scene scene
     :sprite "gloobScaryman"
     :type "slime"
     :energy 24
     :behavior "slime-behavior"
     :hourly "slime-hourly"
     :reproduce "plant-reproduce"
     :hunt "slime-hunt"
     :clock 1
     :handle-mail "slime-inbox"
     :max-speed 4
     :health 1
     :genes
     (generate-genes
      :speed :repro-threshold :repro-chance)}
    (dissoc (first mods) :mail-to)))) ; passed params overwrite anything

(defn spawn-carrot
  "create a carrot in the world"
  [scene & mods]
  (add-game-piece!
   (conj
    (find-empty-tile scene)
    {:scene scene
     :sprite "carrot"
     :type "carrot"
     :energy 24
     :behavior "hourly-behavior"
     :hourly "carrot-hourly"
     :reproduce "plant-reproduce"
     :photosynth "photosynth"
     :clock 1 ;some things happen on the hour
     :handle-mail "carrot-inbox"
     :health 1
     :genes
     (generate-genes
      :repro-threshold :repro-chance)}
    (first mods))))
;;nature
;;admin stuff
(defn log [data]
  (spit "config/log"
        (str (System/currentTimeMillis) " : " data "\n")
        :append true))

(defn clear-corrupted []
  (run! #(update-game-piece! (:id %) {:delete-me true})
        (filter (fn [object] (= nil (:x object))) (objects))))

(defn ten-x []
  (setting "millis-per-hour" (/ (setting "millis-per-hour") 10))
  (setting "millis-per-frame" (/ (setting "millis-per-frame") 10)))

(defn tenth-x []
  (setting "millis-per-hour" (* (setting "millis-per-hour") 10))
  (setting "millis-per-frame" (* (setting "millis-per-frame") 10)))

(defn short-day []
  (setting "millis-per-hour" 600))

(defn seed-nature []
  (try
    (run! (fn [scene] (spawn-carrot (:name scene))) tilemaps)
    (catch Exception e
      (println "seeding nature failed")
      (log "seeding nature failed")
      (clear-corrupted))))

;;admin stuff
(defn main
  [& args]
  (server/run-server handler {:port (setting "port")})
  (println (str "Running server on port " (setting "port")))
  (when (not (setting "pause"))
    (threadify game-loop) (seed-nature)))

(defn spawn-points
  "assumes spawn-type is both a function and a valid object name, upgrade this to take a list later"
  [type & rand]
  (let [coord-pairs
        (filter #(:x %) ;check if valid coords were returned
                (map (fn [tilemap] ; assume one spawn of type per scene because it's easy
                       (let [properties
                             (first
                              (filter
                               #(= (str "spawn-" type) (get % "name"))
                               (:objects tilemap)))]
                         {:scene (:name tilemap) :x (get properties "x") :y (get properties "y")}))
                     tilemaps))]
    (cond
      rand
      (run! #(call-func-by-string (str "witchazzan.world/spawn-" type)
                                  (list (:scene %) (find-empty-tile (:scene %))))
            coord-pairs)
      :else
      (run! #(call-func-by-string (str "witchazzan.world/spawn-" type) (list (:scene %) %))
            coord-pairs))))

(defn coordinate-spawns []
  (when (and
         (< (:clock @game-state) 5)
         (< (count (filter #(= (:type %) "slime") (objects))) 5))
    (spawn-points "slime"))
  (when (and
         (> (:clock @game-state) 20)
         (< (count (filter #(= (:type %) "carrot") (objects))) 5))
    (spawn-points "carrot" true)))
