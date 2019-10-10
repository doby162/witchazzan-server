;;namespace
(ns witchazzan.core
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
  (:gen-class))
;;namespace
;;
;;configuration and global state
(load-file "config/config.clj")
(def players []) ; should this also be an atom?
(def map1 (json/read-str (slurp (str (:tilemap-path settings) (first (:tilemaps settings))))))
;;configuration and global state
;;
;;websocket infrastructure
(defn make-player [x y sock] (atom {:x x :y y :sock  sock}));definition of a player

(defn handler [request]
  (println "A new player has entered Witchazzan")
  (println request)
  (server/with-channel request channel
    (def players (conj players (make-player 0 0 channel))); add this to our collection of players
    (server/on-close channel (fn [status]
                               (def players (filter #(not (= (:sock @%) channel)) players))
                               (println "channel closed: " status)))
    (server/on-receive channel (fn [data] ;; echo it back
                          (server/send! channel data)))))

(defn broadcast [players data]
  (map (fn [player] (server/send! (:sock @player) data)) players))

(server/run-server handler {:port (:port settings)})
;;websocket infrastructure
;;

;;create a functional object to store client state on a per instance basis
;;specify an api of commands and document it in the readme. We need to be able to recieve 1) inputs like the keyboard 2) state updates like an x coord 3)chat broadcasts 4)nicknames and login credentials
;;simulate movement based on keyboard, update on client state commands, try to aproximate the front end based on only input
;;broadcast function that specifies the physical state of all game pieces, sends chats, welcomes new players, etc
;;
