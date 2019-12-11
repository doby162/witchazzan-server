;;namespace
(ns witchazzan.core
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
  (:gen-class))
(declare broadcast)
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

;this function is a big oof. the clojure reader can't
;handle socket literals, for good reasons, and since
;the game state is full of 'em, I have to filter them out.
;using a regex for this is probably the worst idea
;but hey, it works for now!
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

(defn gen-id []
  (swap! game-state #(merge % {:auto-increment-id (inc (:auto-increment-id %))}))
  (:auto-increment-id @game-state))

(defn hourglass []
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
               #(process-map (json/read-str (slurp (str (setting "tilemap-path") %))) %)
               (setting "tilemaps")))

(defn name->scene [name]
  (first (filter #(= name (:name %)) tilemaps)))

(defn id->object [id]
  (first (filter #(= id (:id %)) (:game-pieces @game-state))))

(defn tile-location
  "takes a player or game object and returns an x and y offset"
  [object]
  (let [tilewidth (:tilewidth (name->scene (:scene object)))]
    {:x (Math/floor (/ (:x object) tilewidth)) :y (Math/floor (/ (:y object) tilewidth))}))

(defn pixel-location
  "takes a pair of coordinates and returns pixel values"
  [coords scene]
  (let [width (:width (name->scene scene)) height (:height (name->scene scene))]
    {:x (* width (:x coords)) :y (* height (:y coords))}))
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
    (catch Exception e
      ;(pp/pprint e)
      (pp/pprint "failed to call method")
      (pp/pprint args)
      (pp/pprint key)
      (pp/pprint (:id object)))))

(defn handler [request]
  (println "A new player has entered Witchazzan")
  (println request)
  (server/with-channel request channel
    (server/on-close
     channel
     (fn [data]
       (update-game-piece (:id (sock->player channel)) {:active false})))
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
                                         :name (:name %) :direction (:direction %)})
                                (scene->pieces (:name tilemap)))}
                  (scene->players (:name tilemap))))
   tilemaps))

(defn process-object-behavior
  "run the :behavior method of every game-piece, it takes a state
  and returns a new state"
  []
  (run!
   (fn [object]
     (update-game-piece (:id object) (method object :behavior (list))))
   (vals (:game-pieces @game-state))))

(defn game-loop []
  (loop []
    (let [start-ms (System/currentTimeMillis)]
      (update-clients)
      (process-object-behavior)
      (collect-garbage)
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

(defn find-empty-tile
  "returns the coordinates of a random empty tile from a map"
  [scene]
  (let [map (name->scene scene) size (:tilewidth map)]
    (pixel-location (rand-nth (filter #((:get-tile-walkable map) %) (square-range size))) scene)))

(defn spawn-carrot
  "create a carrot in the world"
  [scene]
  (add-game-piece
   (conj
    (find-empty-tile scene)
    {:scene scene
     :sprite "carrot"
     :type "type"
     :hit "witchazzan.core/plant-hit"
     :energy 1
     :behavior "witchazzan.core/carrot-behavior"
     :reproduce "witchazzan.core/plant-reproduce"
     :photosynth "witchazzan.core/photosynth"})))

(defn seed-nature []
  (run! (fn [scene] (spawn-carrot (:name scene))) tilemaps))
;;nature
;;basically the main function
(when (not (setting "pause"))
  (do
    (try (when (setting "auto-load") (load-game))
         (catch Exception e (println "Failed to load save file")))
    (threadify game-loop) (threadify hourglass) (seed-nature)))
