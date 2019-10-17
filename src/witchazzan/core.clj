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

(defn handle-chat [player split]
  "broadcasts chats as json"
  (broadcast  {:message-type "chat" :name (:name @player) :content (clojure.string/join (rest split))}))

(defn handle-location-update [player split]
  (let [new-x (Float/parseFloat  (nth split 1)) new-y (Float/parseFloat (nth split 2))]
    (swap! player #(merge % {:x new-x :y new-y}))))

(defn handle-login [player split];communicationsObject.socket.send("log,uname,pword");
  (let [username (nth split 1) password (nth split 2)]
    (swap! player #(merge % {:name username}))))


(defn handler [request]
  (println "A new player has entered Witchazzan")
  (println request)
  (server/with-channel request channel
    (def players (conj players (make-player 0 0 channel))); add this to our collection of players
    (server/on-close channel (fn [status]
                               (def players (filter #(not (= (:sock @%) channel)) players))
                               (println "channel closed: " status)))
    (server/on-receive channel (fn [data]
                                 (let [player (first (filter #(= (:sock @%) channel) players))
                                       split (str/split data #",")]
                                   (when (= "msg" (first split)) (handle-chat player split))
                                   (when (= "loc" (first split)) (handle-location-update player split))
                                   (when (= "log" (first split)) (handle-login player split)))))))
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
