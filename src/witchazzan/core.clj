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
(def anon-names ["arc" "clojure" "clojurescript" "common-lisp" "pico lisp" "scheme" "chicken"
                 "emacs lisp" "maclisp" "racket"])

(defn process-map [map name]
  "returns an immutable representation of a single tilemap, inclusing a helper for collisions"
  (let [width (get map "width") height (get map "height")
        syri (get (first (filter #(= (get % "name") "Stuff You Run Into") (get map "layers"))) "data")]
    {:name (first (str/split name #"\."))
     :width width :height height :syri syri
     :get-tile-walkable #(= 0 (get syri (+ %1 (* width %2))))}))
(def tilemaps (map
                #(process-map (json/read-str (slurp (str (:tilemap-path settings) %))) %)
                (:tilemaps settings)))

;;configuration and global state
;;
;;websocket infrastructure
(defn make-player [x y sock scene]
  (atom {:x x :y y :sock sock :name "unknown-human" :keys {} :id (gen-id) :scene scene}));definition of a player
(defn make-object [x y scene type attributes behaviors]
  (atom {:x x :y y :type type :attributes attributes :behaviors behaviors :id (gen-id) :scene scene}));definition of an object.
; Objects are very general, but they will all have some things in common

(defn message-player [data player]
  (server/send! (:sock @player) (json/write-str data)))

(defn broadcast [data]
  "takes an n-level map and distributes it to all clients as json"
  (dorun (map #(message-player data %) players)))

(defn establish-identity [player]
  (message-player {:messageType "identity" :id (:id @player) :name (:name @player)} player))

(defn handle-chat [player message]
  "broadcasts chats as json"
  (broadcast  {:messageType "chat" :name (:name @player) :content (get message "text")}))

(defn handle-location-update [player message]
  (let [new-x (get message "x") new-y (get message "y") new-scene (get message "scene")]
    (swap! player #(merge % {:x new-x :y new-y :scene new-scene}))))

(defn handle-login [player message]
  (let [username (get message "username") password (get message "password")]
    (swap! player #(merge % {:name username}))
    (establish-identity player)))

(defn handle-keyboard-update [player message]
  (swap! player #(merge
                   %
                   {:keys (merge
                            (:keys %)
                            {(str ":" (get message "key")) (get message "state")})})))

(defn handle-command [player message]
  "this handler is a bit of a switch case inside of a switch case, it handles all of the text commands entered
  via the command bar on the client"
  (when (re-find #"^look" (get message "command"))
    (message-player {"response" "You see a rhelm of unimaginable possibility."} player))
  (when (re-find #"^listen" (get message "command"))
    (message-player {"response" "You hear the distant chatter of a keyboard. A developer is hard at work."} player))
  )

(defn handle-fireball [player message]
  (def objects (conj objects (make-object (:x @player) (:y @player) (:scene @player) "fireball"
                                          {:direction (get message "direction") :owner player} {}))))


;(atom {:id (gen-id) :type "fireball" :x (:x @player) :y (:y @player)
;                                   :direction (get message "direction") :owner player})

(defn call-func-by-string [name args]
  "(call-func-by-string \"+\" [5 5]) => 10"
  (apply (resolve (symbol name)) args))

(defn handler [request]
  (println "A new player has entered Witchazzan")
  (println request)
  (server/with-channel request channel
    (def players (conj players (make-player 0 0 channel ""))); add this to our collection of players
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
                                           (println "invalid json: " data) (println e)))
                                 ))))
(server/run-server handler {:port (:port settings)})
;;websocket infrastructure
;;
;;game loop
(defn update-clients []
  (when (< 0 (count objects)) (broadcast {:messageType "object-state" :objects (map (fn [q] {:id (:id @q) :x (:x @q) :y (:y @q) :type (:type @q)}) objects)}))
  (when (< 0 (count players)) (broadcast {:messageType "player-state" :players (map (fn [q] {:id (:id @q) :x (:x @q) :y (:y @q) :name (:name @q) :scene (:scene @q)}) players)})))

(defn game-loop []
  (future
    (loop []
      (let [start-ms (System/currentTimeMillis)]
        (update-clients)
;        (process-object-behavior)
        (Thread/sleep (- (:frame-time settings) (- (System/currentTimeMillis) start-ms))))
      (when (not (:pause settings)) (recur)))))
(when (not (:pause settings)) (game-loop))
;;game loop
