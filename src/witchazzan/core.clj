;;namespace
(ns witchazzan.core
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
  (:require [clojure.string :as str])
  (:gen-class))
;;namespace
;;
;;configuration and global state
(load-file "config/config.clj")
(def players []) ; should this also be an atom?
(def anon-names ["arc" "clojure" "clojurescript" "common-lisp" "pico lisp" "scheme" "chicken"
                 "emacs lisp" "maclisp" "racket"])
(defn get-anon-name [] (let [first-name (first anon-names) rest-names (rest anon-names)]
                         (def anon-names rest-names)
                         first-name))
;some names for those who do not have logins to borrow. Probably not a permanent fixture of the game.
(def map1 (json/read-str (slurp (str (:tilemap-path settings) (first (:tilemaps settings))))))
;;configuration and global state
;;
;;websocket infrastructure
(defn make-player [x y sock] (atom {:x x :y y :sock sock :name (get-anon-name)}));definition of a player

(defn broadcast [data]
  "takes an n-level map and distributes it to all clients as josn"
  (dorun (map (fn [player] (server/send! (:sock @player) (json/write-str data))) players)))

(defn handle-chat [player message]
  "broadcasts chats as json"
  (broadcast  {:message-type "chat" :name (:name @player) :content (get message "text")}))

(defn handle-location-update [player message]
  (let [new-x (get message "x") new-y (get message "y")]
    (swap! player #(merge % {:x new-x :y new-y}))))

(defn handle-login [player message]
  (let [username (get message "username") password (get message "password")]
    (swap! player #(merge % {:name username}))))

(defn call-func-by-string [name args]
  "(call-func-by-string \"+\" [5 5]) => 10"
  (apply (resolve (symbol name)) args))

(defn handler [request]
  (println "A new player has entered Witchazzan")
  (println request)
  (server/with-channel request channel
    (def players (conj players (make-player 0 0 channel))); add this to our collection of players
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
  (broadcast {:message-type "player-state" :players (map (fn [q] {:name (:name @q) :x (:x @q) :y (:y @q)}) players)}))

(defn game-loop []
  (future
    (loop []
      (let [start-ms (System/currentTimeMillis)]
        (update-clients)
        (Thread/sleep (- (:frame-time settings) (- (System/currentTimeMillis) start-ms))))
      (when (not (:pause settings)) (recur)))))
(when (not (:pause settings)) (game-loop))
;;game loop
