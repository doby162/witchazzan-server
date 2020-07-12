;;namespace
(ns witchazzan.world
  (:require [witchazzan.common :refer :all])
  (:require [witchazzan.comms :as comms])
  (:require [witchazzan.behavior :as behavior])
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
  (:require [clojure.string :as str])
  (:require [witchazzan.api :as api])
  (:gen-class))
;;namespace
;;game loop
(defn update-clients [scene]
  (comms/broadcast
   {:messageType "game-piece-list"
    :pieces (map (fn [%] (dissoc (into {} @%) :socket :vector))
                 (active-pieces {:scene scene}))}
   (active-pieces {:type :player :scene scene})))
;;game loop
;;admin stuff
(defn log-and-clear-agents
  []
  (run!
   (fn [agent]
     (log (with-out-str (println (agent-error agent) " " @agent)))
     (behavior/delete @agent))
   (filter #(agent-error %) (game-pieces))))

(defn ten-x []
  (setting "millis-per-hour" (/ (setting "millis-per-hour") 10))
  (setting "millis-per-frame" (/ (setting "millis-per-frame") 10)))

(defn tenth-x []
  (setting "millis-per-hour" (* (setting "millis-per-hour") 10))
  (setting "millis-per-frame" (* (setting "millis-per-frame") 10)))

(defn short-day []
  (setting "millis-per-hour" 600))

(defn seed-nature []
  (loop [n 0]
    (run!
     (fn [scene] (behavior/spawn-carrot (merge {:scene (:name scene)} (find-empty-tile (:name scene)))))
     tilemaps)
    (when (< n (setting :starting-carrots)) (recur (inc n)))))
;;admin stuff
;;loooop
(defn spawn-points
  "assumes spawn-type is both a function and a valid object name, upgrade this to take a list later"
  [type]
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
    (run!
     #(call-func-by-string (str "witchazzan.behavior/spawn-" type) %)
     coord-pairs)))

(defn coordinate-spawns []
  #_(when (empty? (active-pieces {:type :slime}))
      (spawn-points "slime"))
  (when (empty? (active-pieces {:type :herbivore}))
    (spawn-points "herbivore"))
  (when (empty? (active-pieces {:type :carrot}))
    (spawn-points "carrot")))

(defn keep-time! []
  (let [old-state @game-state
        new-hour (int (mod (/ (- (System/currentTimeMillis) (:start-time @game-state))
                              (setting "millis-per-hour")) (setting "hours-per-day")))
        new-day (int (/ (/ (- (System/currentTimeMillis) (:start-time @game-state))
                           (setting "millis-per-hour")) (setting "hours-per-day")))]
    (swap! game-state merge {:last-updated (System/currentTimeMillis)})
    (when (not (= new-hour (:hour old-state)))
      (swap! game-state update-in [:hour]
             (fn [_] new-hour))
      (swap! game-state update-in [:day]
             (fn [_] new-day))
      (when (= new-hour (setting "dawn"))
        (comms/broadcast
         {:messageType "chat" :name "Witchazzan.core" :id -1
          :content (str "Dawn of day " new-day)}))
      (when (= new-hour (setting "sunset"))
        (comms/broadcast
         {:messageType "chat" :name "Witchazzan.core" :id -1
          :content (str "Night Falls")}))
      (coordinate-spawns))))

(defn game-loop [scene]
  (log scene)
  (loop []
    (try
      (run!
       (fn [game-piece]
         (send game-piece behavior/behavior))
       (active-pieces {:scene scene}))
      (Thread/sleep (setting "min-millis-per-frame"))
      (apply await (active-pieces {:scene scene}))
      (if (scene-active scene)
        (update-clients scene)
        (Thread/sleep (setting "idle-millis-per-frame")))
      (catch Exception e
        (log (str "error in " scene))
        (log e)
        (log-and-clear-agents)))
    (when (not (setting "pause")) (recur))))

(defn threadify [func] (future (func)))

(defn launch-threads []; this function will also advance all scenes by one tic when the game is paused
  (threadify
   #(loop [] (keep-time!) (Thread/sleep (setting "idle-millis-per-frame"))
          (when (not (setting "pause")) (recur))))
  (run! #(threadify (fn [] (game-loop %))) (map #(:name %) tilemaps)))

(defn pause
  "stops the game, but not the flow of time. Use justify-time before unpausing"
  []
  (swap! settings merge {:pause true}))

(defn unpause []
  (swap! settings merge {:pause false})
  (launch-threads))

(defn justify-time
  "after the simulation has been paused, run this to catch the clock up with reality"
  []
  (run! #(send % merge {:milliseconds (System/currentTimeMillis)}) (game-pieces))
  (swap!
   game-state
   #(merge % {:start-time (+ (- (System/currentTimeMillis) (:last-updated %)) (:start-time %))})))

(defn main
  [& _]
  (log "Booting...")
  (server/run-server witchazzan.api/all-routes {:port (setting "port")})
  (log (str "Running server on port " (setting "port")))
  (when (not (setting "pause"))
    (log "Not paused, running game")
    (launch-threads))
  (seed-nature))

(defn frame [] (justify-time) (Thread/sleep 10) (unpause) (pause) nil)
