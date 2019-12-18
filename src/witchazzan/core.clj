;;namespace
(ns witchazzan.core
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
  (:gen-class))
(declare broadcast)
(declare within-n)
;;namespace
;;
;;configuration and global state
(load-file "config/default-config.clj")
(try (load-file "config/config.clj")
     (catch Exception e
       (println "No custom configuration found at config/config.clj.
                Add settings like (setting \"port\" 1234)")))
(load-file "src/witchazzan/behavior.clj")

(def game-state (atom {:game-pieces {} :auto-increment-id 0 :clock 0 :calendar 0}))

(defn save
  "Serializes the entire state of play. All mutable state exists in the resulting file"
  []
  (let
   [save-data
    (merge @game-state {:game-pieces
                        (apply merge (map (fn [object]
                                            {(keyword (str (:id object))) object})
                                          (map #(dissoc % :sock) (vals (:game-pieces @game-state)))))})]
    (def step save-data)
    (spit "config/save.clj"
          (str "(def game-state (atom " save-data "))"))
    (slurp "config/save.clj")))

(defn load-game
  "reads and executes the code stored in config/save.clj, repopulating the game-state"
  []
  (load-string (slurp "config/save.clj")))

(defn objects [] (filter #(not (= "player" (:type %))) (vals (:game-pieces @game-state))))

(defn players [] (filter #(= "player" (:type %)) (vals (:game-pieces @game-state))))

;the scene-> functions filter players by active
(defn scene->players [scene] (filter #(and
                                       (= (:scene %) scene)
                                       (not (= false (:active %))))
                                     (players)))

(defn scene->pieces [scene] (filter #(and
                                      (= (:scene %) scene)
                                      (not (= false (:active %))))
                                    (vals (:game-pieces @game-state))))

(defn id->piece [id] ((keyword (str id)) (:game-pieces @game-state)))

(defn gen-id! []
  (swap! game-state #(merge % {:auto-increment-id (inc (:auto-increment-id %))}))
  (:auto-increment-id @game-state))

(defn hourglass! []
  (swap! game-state #(merge % {:clock (inc (:clock %))}))
  (when (< 23 (:clock @game-state))
    (do
      (swap! game-state #(merge % {:clock 0 :calendar (inc (:calendar %))})))
    (when (setting "auto-save") (save)))
  (when (= (:clock @game-state) 6)
    (broadcast
     {:messageType "chat" :name "Witchazzan.core" :id -1
      :content (str "Dawn of day " (:calendar @game-state))} (players)))
  (when (= (:clock @game-state) 20)
    (broadcast
     {:messageType "chat" :name "Witchazzan.core" :id -1
      :content "Night falls"} (players)))
  (broadcast {:time (:clock @game-state)} (players))
  (Thread/sleep (setting "milis-per-hour"))
  (recur))

;this is a map to leave room for other types of game state
(defn add-game-piece!
  "adds a game piece to the global game state
  game pieces must have, at a minumum:
  x y type scene behavior"
  [new-object]
  (let [id (gen-id!) obj (merge new-object {:id  id :delete-me false})]
    (swap! ; todo, throw exception when object is invalid
     game-state
     #(merge % {:game-pieces (merge (:game-pieces %) {(keyword (str id)) obj})}))))

(defn update-game-piece!
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

(defn collect-garbage!
  "removes game-pieces with the delete-me attribute set to true"
  []
  (swap! game-state trash-filter))

(defn process-map
  "returns an immutable representation of a single tilemap,
  including a helper for collisions"
  [map name]
  (def tmap map)
  (let [width (get map "width") height (get map "height")
        syri (get (first (filter #(= (get % "name") "Stuff You Run Into") (get map "layers"))) "data")
        objects (get (first (filter #(= (get % "name") "Objects") (get tmap "layers"))) "objects")]
    {:name (first (str/split name #"\."))
     :width width :height height :syri syri
     :objects objects
     :tilewidth (get map "tilewidth")
     :teleport
     (fn [coords]
       (let [tele (filter
                   #(and
                     (= "SwitchToScene" (get % "type"))
                     (within-n (:x coords) (get % "x") (* 2 (get % "width")))
                     (within-n (:y coords) (get % "y") (* 2 (get % "height"))))
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
    {:x (int (Math/floor (/ (:x object) tilewidth))) :y (int (Math/floor (/ (:y object) tilewidth)))}))

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
  (apply (resolve (symbol name)) args))

(defn method
  "shorthand to call game-piece methods"
  [object key args]
  (try
    (call-func-by-string (get object key) (conj args object))
    (catch Exception e)))

(defn handler [request]
  (println "A new player has entered Witchazzan")
  (println request)
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
              (str "witchazzan.core/handle-" message-type) [message channel])
             (catch java.lang.NullPointerException e (println e)))
                                        ;here we are interpreting the "messasge_type" json value as
                                        ;the second half of a function name and calling that function
           )(catch java.lang.Exception e
              (println "invalid json: " data) (println e)))))))
(server/run-server handler {:port (setting "port")})
;;websocket infrastructure
;;
;;game loop
(defn update-clients []
  (run!
   (fn [tilemap] (broadcast
                  {:messageType "game-piece-list"
                   :pieces (map (fn [%] {:id (:id %) :x (:x %) :y (:y %)
                                         :type (:type %) :scene (:scene %)
                                         :health (:health %) :defence (:defence %)
                                         :sprite (:sprite %) :animation (:animation %)
                                         :moving (:moving %)
                                         :name (:name %) :direction (:direction %)})
                                (scene->pieces (:name tilemap)))}
                  (scene->players (:name tilemap))))
   tilemaps))

(defn process-behavior
  "calls the behavior method on all pieces to calculate the next frame"
  [this]
  {(first this) (method (second this) :behavior (list))})

(defn process-behaviors!
  "coordinates the process-behavior function accross the game-pieces"
  []
  (swap!
   game-state
   (fn [state]
     (->>
      (:game-pieces state)
      (pmap #(process-behavior %))
      (apply merge)
      (vector :game-pieces)
      (merge state)))))

(defn game-loop []
  (loop []
    (let [start-ms (System/currentTimeMillis)]
      (update-clients)
      (process-behaviors!)
      (collect-garbage!)
      (try (Thread/sleep
            (- (setting "frame-time") (- (System/currentTimeMillis) start-ms))) (catch Exception e)))
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

(defn tile-occupied
  [scene coords]
  (not (empty? (filter
                (fn [object]
                  (let [ob-coords (tile-location object)]
                    (and
                     (= (:x coords) (:x ob-coords))
                     (= (:y coords) (:y ob-coords)))))
                (scene->pieces scene)))))

(defn find-empty-tile
  ;this needs to somehow return nil instead of crashing on imossible requests
  "returns the coordinates of a random empty tile from a map"
  [scene]
  (let [map (name->scene scene) tile-size (max (:width map) (:height map))]
    (->>
     (square-range tile-size)
     (filter #((:get-tile-walkable map) %))
     (filter #(not (tile-occupied scene %)))
     (rand-nth)
     (pixel-location scene))))

(defn generate-genes
  "makes a list of keywords into a map of ints, arbitrarily limited by settings"
  [& keywords]
  (zipmap keywords
          (repeatedly #(rand-int (+ 1 (setting "gene-max"))))))

(defn mutate-genes
  "each gene can be incremeneted, decremented or stay the same with equal chance"
  [genes]
  (zipmap (keys genes)
          (map #(+ (- (rand-int 3) 1) %) (vals genes))))

(defn normalize-genes
  "prevent mutation from moving genes outside the 0-gene-max range"
  [genes]
  (zipmap (keys genes)
          (map
           #(cond (> % (setting "gene-max"))
                  (- % (setting "gene-max"))
                  (< % 0)
                  (+ % (setting "gene-max"))
                  :else %)
           (vals genes))))

(defn within-n
  [a b n]
  (and (>= a (- b n)) (<= a (+ b n))))

(defn find-adjacent
  "returns a list of all pieces residing in the 9 adjacent tiles to the arg"
  [object]
  (let [map (name->scene (:scene object)) n (:tilewidth map)]
    (filter
     #(and (within-n (:x %) (:x object) n) (within-n (:y %) (:y object) n))
     (scene->pieces (:scene object)))))

(defn spawn-carrot
  "create a carrot in the world"
  [scene]
  (add-game-piece!
   (conj
    (find-empty-tile scene)
    {:scene scene
     :sprite "carrot"
     :type "type"
     :hit "witchazzan.core/plant-hit"
     :energy 24
     :behavior "witchazzan.core/hourly-behavior"
     :hourly "witchazzan.core/carrot-hourly"
     :reproduce "witchazzan.core/plant-reproduce"
     :photosynth "witchazzan.core/photosynth"
     :clock 1 ;some things happen on the hour
     :genes
     (generate-genes
      :repro-threshold :repro-chance)})))
;;nature
;;admin stuff
(defn ten-x []
  (setting "milis-per-hour" (/ (setting "milis-per-hour") 10))
  (setting "frame-time" (/ (setting "frame-time") 10)))
(defn tenth-x []
  (setting "milis-per-hour" (* (setting "milis-per-hour") 10))
  (setting "frame-time" (* (setting "frame-time") 10)))
(defn short-day []
  (setting "milis-per-hour" 600))
(defn seed-nature []
  (run! (fn [scene] (spawn-carrot (:name scene))) tilemaps))
;;admin stuff
;;basically the main function
(when (not (setting "pause"))
  (do
    (try (when (setting "auto-load") (load-game))
         (catch Exception e (println "Failed to load save file")))
    (threadify game-loop) (threadify hourglass!) (seed-nature)))
;;basically the main function
