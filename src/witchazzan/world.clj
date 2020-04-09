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
;;namespace
;;websocket infrastructure
(defn call-func-by-string
  "(call-func-by-string \"+\" [5 5]) => 10"
  [name args]
  (try
    (apply (resolve (symbol name)) args)
    (catch NullPointerException e
      (log (str "call-func-by-string failed: " name args " " e)))))

(defn handler [request]
  (log "A player has entered Witchazzan!")
  (server/with-channel request channel
    (server/on-close
     channel
     (fn [data]
       ;logout
       (behavior/delete
         @(first (game-pieces :socket channel)))))
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
              (log (str "invalid json: " data)) (log e)))))))
;;websocket infrastructure
;;
;;game loop
(defn update-clients []
  (run!
   (fn [tilemap] (comms/broadcast
                  {:messageType "game-piece-list"
                   :pieces (map (fn [%] (dissoc (into {} @%) :socket))
                                (game-pieces "scene" (:name tilemap)))}
                  (filter #(= (:scene @%) (:name tilemap)) (game-pieces "type" "player"))))
   tilemaps))

(defn threadify [func] (future (func)))
;;game loop
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
;;
(defn game-loop []
  (loop []
    (run!
     (fn [game-piece]
       (send game-piece (fn [this] (behavior/behavior this))))
     (:game-pieces @game-state))
    ;wait for a minumum of 10 milliseconds and a maximum of 100 milliseconds
    ;to queue up the next round of agent actions. If it takes more than 100 millis to
    ;finish processing, they'll just pile up in order.
    ;agents keep track of elapsed time on an individual basis.
    (update-clients)
    (try (Thread/sleep 10) (catch Exception e))
    (apply await-for 90 (:game-pieces @game-state))
    (when (not (setting "pause")) (recur))))

(defn main
  [& args]
  (log "Booting...")
  (server/run-server handler {:port (setting "port")})
  (log (str "Running server on port " (setting "port")))
  (when (not (setting "pause"))
    (log "Not paused, running game")
    (threadify game-loop)))

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
