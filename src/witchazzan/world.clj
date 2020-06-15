;;namespace
(ns witchazzan.world
  (:require [witchazzan.common :refer :all])
  (:require [witchazzan.comms :as comms])
  (:require [witchazzan.behavior :as behavior])
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
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
       (when (seq (game-pieces {:socket channel}))
         (send (first (game-pieces {:socket channel})) #(merge % {:active false})))))
    (server/on-receive
     channel
     (fn [data]
       (try ; checking for bad json and that a handler can be found
         (let [message (json/read-str data)
               message-type (get message "message_type")]
           (try ;checking if the function exists
             (call-func-by-string
              (str "witchazzan.comms/handle-" message-type) [(dissoc message "message_type") channel])
             (catch java.lang.NullPointerException e (log e)))
                                        ;here we are interpreting the "messasge_type" json value as
                                        ;the second half of a function name and calling that function
           )(catch java.lang.Exception e
              (log (str "invalid json: " data)) (log e)))))))
;;websocket infrastructure
;;
;;game loop
(defn update-clients [scene]
  (comms/broadcast
   {:messageType "game-piece-list"
    :pieces (map (fn [%] (dissoc (into {} @%) :socket))
                 (active-pieces {:scene scene}))}
   (typed-pieces witchazzan.behavior.player {:scene scene})))

(defn threadify [func] (future (func)))
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
  (run!
   (fn [scene] (behavior/spawn-carrot (merge {:scene (:name scene)} (find-empty-tile (:name scene)))))
   tilemaps))
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
  #_(when (empty? (typed-pieces witchazzan.behavior.slime))
      (spawn-points "slime"))
  (when (empty? (typed-pieces witchazzan.behavior.carrot))
    (spawn-points "carrot")))

(defn keep-time! []
  (let [old-state @game-state
        new-hour (int (mod (/ (- (System/currentTimeMillis) (:start-time @game-state))
                              (setting "millis-per-hour")) (setting "hours-per-day")))
        new-day (int (/ (/ (- (System/currentTimeMillis) (:start-time @game-state))
                           (setting "millis-per-hour")) (setting "hours-per-day")))]
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
    (keep-time!)
    (try
      (run!
       (fn [game-piece]
         (send game-piece behavior/behavior))
       (active-pieces {:scene scene}))
      (try (Thread/sleep (setting "min-millis-per-frame")) (catch Exception _))
      (apply await (active-pieces {:scene scene}))
      (if (seq (typed-pieces witchazzan.behavior.player {:scene scene}))
        (update-clients scene)
        (try (Thread/sleep (setting "idle-millis-per-frame")) (catch Exception _)))
      (catch Exception e
        (log (str "error in " scene))
        (log e)
        (log-and-clear-agents)))
    (when (not (setting "pause")) (recur))))

(defn main
  [& _]
  (log "Booting...")
  (server/run-server handler {:port (setting "port")})
  (log (str "Running server on port " (setting "port")))
  (when (not (setting "pause"))
    (log "Not paused, running game")
    (run! #(threadify (fn [] (game-loop %))) (map #(:name %) tilemaps)))
  (seed-nature))

(defn ascii-graph
  [dataset]
  (let [increment 10 min 5 max (setting "gene-max")]
    (loop [i min]
      (let [num
            (get (frequencies (map #(within-n % i min) dataset)) true 0)]
        (println (- i min) "-" (- (+ increment i) min) ":" (apply str (repeatedly num #(str "#")))))
      (when (< i max) (recur (+ i 10))))))


;;analysis functions


(defn analyze-gene
  [gene population]
  (let [dataset (sort (filter #(not (nil? %)) (map #((keyword gene) (:genes @%)) population)))]
    (print "Sample size: ")
    (prn (count dataset))
    (print "Mode: ")
    (prn (first (last (sort-by second (frequencies dataset)))))
    (print "Mean: ")
    (prn (int (/ (apply + dataset) (count dataset))))
    (print "Median: ")
    (prn (nth dataset (/ (count dataset) 2)))
    (println "frequencies")
    (ascii-graph dataset)
    (print "Full dataset: ")
    (prn dataset)))

