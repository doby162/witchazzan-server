;;namespace
(ns witchazzan.behavior
  (:refer witchazzan.common)
  (:require [witchazzan.comms :as comms])
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
  (behavior [this]))

(defrecord carrot
           [id
            genes
            energy
            scene
            sprite
            delta
            health]
  game-piece
  (behavior [this] (println (str sprite " is my carrot name")) this))

(defn add-game-piece
  [piece]
  (swap! game-state
         (fn [state] (update-in state [:game-pieces]
                                (fn [game-pieces] (merge game-pieces (agent piece)))))))

(defn spawn-carrot []
  (add-game-piece
   (->carrot
    1
    (generate-genes {:a-gene :b-gene})
    20
    "LoruleH8"
    "carrot"
    0
    1)))
