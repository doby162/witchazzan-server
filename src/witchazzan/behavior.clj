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
;;shared behavior
(defn add-game-piece!
  [piece]
  (swap! game-state
         (fn [state] (update-in state [:game-pieces]
                                (fn [game-pieces] (merge game-pieces (agent piece)))))))

(defn delete
    [this]
    (swap! game-state
      (fn [state] (update-in state [:game-pieces]
                             (fn [game-pieces] (filter #(not (= (:id this) (:id @%))) game-pieces))))))
;;shared behavior
;;defprotocol

(defprotocol game-piece
  (behavior [this])
  (die [this])
  (reproduce [this]))

;reproduce is next, just need to make a copy plus mutation
;spells will be based off the of the :spell key and it's text
;default is to do nothing, but if a spell matches one of our list
;we dispatch it. All fireballs can be the same function and record type

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
  (die
    [this]
    (delete this)
    nil)
  (behavior
    [this]
    (let [time (System/currentTimeMillis)
          delta (- time milliseconds)]
      (-> this
          (merge {:milliseconds time})
          (merge {:energy (+ (/ delta 1000) energy)}))))
  (reproduce
    [this]
    (let [energy (/ energy 3)
          tile (find-empty-tile scene)
          genes (mutate-genes genes)]
          (add-game-piece!
            (map->carrot (into {} (merge this
                                         {:genes genes
                                          :x (:x tile)
                                          :y (:y tile)
                                          :energy energy
                                          :id (gen-id)}))))
          (merge this {:energy energy}))))

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
    this
    ))

(defn spawn-carrot []
  (let [scene "LoruleH8"
        coords (find-empty-tile scene)]
    (add-game-piece!
     (map->carrot
      {:id (gen-id)
       :genes (generate-genes :a-gene :b-gene)
       :energy 20
       :scene scene
       :sprite "carrot"
       :milliseconds (System/currentTimeMillis)
       :x (:x coords)
       :y (:y coords)
       :health 1}))))
