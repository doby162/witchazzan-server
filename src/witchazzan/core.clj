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
(def anon-names ["arc" "clojure" "clojurescript" "common-lisp" "pico lisp" "scheme" "chicken" "emacs lisp" "maclisp" "racket"])
(defn get-anon-name [] (let [first-name (first anon-names) rest-names (rest anon-names)]
                         (def anon-names rest-names)
                         first-name))
;some names for those who do not have logins to borrow. Probably not a permanent fixture of the game.
(def map1 (json/read-str (slurp (str (:tilemap-path settings) (first (:tilemaps settings))))))
;;configuration and global state
;;
;;websocket infrastructure
(defn make-player [x y sock] (atom {:x x :y y :sock sock :name (get-anon-name)}));definition of a player

(defn broadcast [players data]
  (dorun (map (fn [player] (server/send! (:sock @player) data)) players))); dorun in this context is bad form but I want to get this commit out

(defn handler [request]
  (println "A new player has entered Witchazzan")
  (println request)
  (server/with-channel request channel
    (def players (conj players (make-player 0 0 channel))); add this to our collection of players
    (server/on-close channel (fn [status]
                               (def players (filter #(not (= (:sock @%) channel)) players))
                               (println "channel closed: " status)))
    (server/on-receive channel (fn [data] ;; echo it back
                          ;(server/send! channel data)
                          (when (re-find #"^msg," data)
                            (broadcast players
                                       (str "chat,"
                                            (:name @(first (filter #(= (:sock @%) channel) players)))
                                            ": " (last (str/split data  #"msg,")))))
                          #_(when (re-find #"loc,"))
                          ))))
(server/run-server handler {:port (:port settings)})
;;websocket infrastructure
;;
;;game loop
(defn update-clients []
  (broadcast players (json/write-str
                       {:players (map (fn [q] {:name (:name @q) :x (:x @q) :y (:y @q)}) players)})))

(defn game-loop []
  (future
    (loop []
      (let [start-ms (System/currentTimeMillis)]
        (update-clients)
        (Thread/sleep (- (:frame-time settings) (- (System/currentTimeMillis) start-ms))))
      (when (not (:pause settings)) (recur)))))
(when (not (:pause settings)) (game-loop))
;;game loop
