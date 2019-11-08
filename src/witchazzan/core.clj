;;namespace
(ns witchazzan.core
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
  (:require [clojure.string :as str])
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

(def game-state (atom {:game-pieces []}))
;this is a map to leave room for other types of game state
(defn add-game-piece
  "adds a game piece to the global game state"
  [new-object]
  (swap!
   game-state
   #(merge % {:game-pieces (conj (:game-pieces %) [(merge new-object {:id  (gen-id)})])})))

(defn process-map
  "returns an immutable representation of a single tilemap, inclusing a helper
  for collisions"
  [map name]
  (let [width (get map "width") height (get map "height")
        syri (get (first (filter #(= (get % "name") "Stuff You Run Into") (get map "layers"))) "data")]
    {:name (first (str/split name #"\."))
     :width width :height height :syri syri
     :tilewidth (get map "tilewidth")
     :get-tile-walkable (fn [coords]
                          (= 0 (get syri (int (+ (:x coords) (* width (:y coords)))))))}))
(def tilemaps (map
               #(process-map (json/read-str (slurp (str (:tilemap-path settings) %))) %)
               (:tilemaps settings)))

(defn name->scene [name]
  (first (filter #(= name (:name %)) tilemaps)))

(defn tile-location
  "takes a player or game object and returns an x and y offset"
  [object]
  (let [tilewidth (:tilewidth (name->scene (:scene object)))]
    {:x (Math/floor (/ (:x object) tilewidth)) :y (Math/floor (/ (:y object) tilewidth))}))
;;configuration and global state
;;
;;websocket infrastructure
(defn make-player
  "defines the structure of a player object"
  [x y sock scene direction]
  (atom {:x x :y y :sock sock :name "unknown-human" :keys {} :id (gen-id)
         :scene scene :direction direction}))
(defn make-object
  "defines the structure of a game piece"
  [x y type scene behavior attributes]
  (atom {:x x :y y :type type :scene scene :id (gen-id) :behavior behavior
         :attributes attributes}))

(defn message-player [data player]
  (server/send! (:sock @player) (json/write-str data)))

(defn broadcast
  "takes an n-level map and distributes it to all clients as json"
  [data]
  (dorun (map #(message-player data %) players)))

(defn establish-identity
  "comunicates to a client which player object belongs to them"
  [player]
  (message-player {:messageType "identity" :id (:id @player)
                   :name (:name @player)} player))

(defn handle-chat
  "broadcasts chats as json"
  [player message]
  (broadcast  {:messageType "chat" :name (:name @player) :id (:id @player)
               :content (get message "text")}))

(defn handle-location-update [player message]
  (let [new-x (get message "x") new-y (get message "y")
        new-scene (get message "scene") new-direction (get message "direction")]
    (swap! player #(merge % {:x new-x :y new-y :scene new-scene :direction new-direction}))))

(defn handle-login [player message]
  (let [username (get message "username") password (get message "password")]
    (swap! player #(merge % {:name username}))
    (establish-identity player)))

(defn handle-keyboard-update [player message]
  (swap! player
         #(merge %
                 {:keys
                  (merge
                   (:keys %)
                   {(str ":" (get message "key")) (get message "state")})})))

(defn handle-command
  "this handler is a bit of a switch case inside of a switch case,
  it handles all of the text commands entered
  via the command bar on the client"
  [player message]
  (when (re-find #"^look" (get message "command"))
    (message-player {"response"
                     "You see a rhelm of unimaginable possibility."}
                    player))
  (when (re-find #"^listen" (get message "command"))
    (message-player {"response"
                     "You hear the distant chatter of a keyboard.
                     A developer is hard at work."}
                    player)))

(def objects [])
(defn handle-fireball
  "generate a fireball object and add it to the object registry"
  [player message]
  (def objects (conj
                objects
                (make-object (:x @player) (:y @player) "fireball" (:scene @player) ;standard properties
                             (fn [this]
                               (cond
                                 ((:collide (:attributes this)) this) nil
                                 :else ((:move (:attributes this)) this)))
                             {:owner player ;attributes
                              :collide (fn [this]
                                         (not
                                          ((:get-tile-walkable
                                            (name->scene (:scene this))) (tile-location this))))
                              :move (cond
                                      (= "north" (get message "direction")) #(conj % {:y (dec (:y %))})
                                      (= "south" (get message "direction")) #(conj % {:y (inc (:y %))})
                                      (= "east" (get message "direction")) #(conj % {:x (inc (:x %))})
                                      :else #(conj % {:x (dec (:x %))}))
                              :collide-wall #(not true)}))))

(defn call-func-by-string
  "(call-func-by-string \"+\" [5 5]) => 10"
  [name args]
  (apply (resolve (symbol name)) args))

(defn handler [request]
  (println "A new player has entered Witchazzan")
  (println request)
  (server/with-channel request channel
    (def players (conj players (make-player 0 0 channel "" ""))); add this to our collection of players
    (establish-identity (first (filter #(= (:sock @%) channel) players)))
    (server/on-close channel (fn [status]
                               (def players (filter #(not (= (:sock @%) channel)) players))
                               (println "channel closed: " status)))
    (server/on-receive channel (fn [data]
                                 (try ; checking for bad json and that a handler can be found
                                   (let [player (first (filter #(= (:sock @%) channel) players))
                                         message (json/read-str data)
                                         message-type (get message "message_type")]
                                     (try ;checking if the function exists
                                       (call-func-by-string
                                        (str "witchazzan.core/handle-" message-type) [player message])
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
