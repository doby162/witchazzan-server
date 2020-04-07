;;namespace
(ns witchazzan.behavior
  (:refer witchazzan.common)
  (:require [clojure.pprint :as pp])
  (:gen-class))
;;namespace
;;game-piece creation helpers
(defn normalize-genes
  "prevent mutation from moving genes outside the 0-gene-max range"
  [genes]
  (zipmap (keys genes)
          (map
           #(cond (> % (setting "gene-max"))
                  (- % (setting "gene-max"))
                  (< % 0)
                  (+ % (setting "gene-max"))
                  :else %)
           (vals genes))))

(defn mutate-genes
  "each gene can be incremeneted, decremented or stay the same with equal chance"
  [genes]
  (zipmap (keys genes)
          (map #(+ (- (rand-int 3) 1) %) (vals genes))))

(defn generate-genes
  "makes a list of keywords into a map of ints, arbitrarily limited by settings"
  [& keywords]
  (zipmap keywords
          (repeatedly #(rand-int (+ 1 (setting "gene-max"))))))
;;helpers

(defprotocol game-piece
  (behavior [this])
  (die [this]))

(defrecord carrot
           [id
            genes
            energy
            scene
            sprite
            milliseconds
            health
            x
            y]
  game-piece
  (behavior
    [this]
    (let [time (System/currentTimeMillis)
          delta (- time milliseconds)]
      (-> this
          (merge {:milliseconds time})
          (merge {:energy (+ (/ delta 1000) energy)})))))

(defrecord player
           [id
            socket
            x
            y
            name
            health
            sprite]
  game-piece
  (behavior
    [this]
    this))

(defn add-game-piece!
  [piece]
  (swap! game-state
         (fn [state] (update-in state [:game-pieces]
                                (fn [game-pieces] (merge game-pieces (agent piece)))))))

(defn spawn-carrot []
  (let [scene "LoruleH8"
        coords (find-empty-tile scene)]
    (add-game-piece!
     (map->carrot
      {:id (gen-id)
       :genes (generate-genes {:a-gene :b-gene})
       :energy 20
       :scene scene
       :sprite "carrot"
       :milliseconds (System/currentTimeMillis)
       :x (:x coords)
       :y (:y coords)
       :health 1}))))
