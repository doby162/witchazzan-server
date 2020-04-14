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

(defn teleport [this]
  "check for and apply teleports"
  (let [scene (name->scene (:scene this))
        ^clojure.lang.LazySeq tp-collisions
        (map #(get (get % "data") (+ (int (:x this)) (* (:width scene) (int (:y this))))) (:teleport scene))]
    (cond
      (and
       (some #(not (= 0 %)) tp-collisions))
      (let [target (nth (:teleport scene) (.indexOf tp-collisions (apply max tp-collisions)))
            tilewidth (:tilewidth scene)
            target-obj-name (get (first (get target "properties")) "value")
            target-obj
            (or
             (ffilter ; intended entrance
              #(= (get % "name") target-obj-name)
              (:objects (name->scene (get target "name"))))
             (ffilter ; backup entrance.
              #(= (get % "name") "Default Spawn Point")
              (:objects (name->scene (get target "name")))))]
        (merge this
               {:x (/ (get target-obj "x") tilewidth) ; null pointer on fail to find tp
                :y (/ (get target-obj "y") tilewidth)
                :scene (get target "name")}))
      :else this)))
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

(defn shift [this]
  (let [collisions
        (filter
         #(= (str (class @%)) "class witchazzan.behavior.carrot")
         (game-pieces {:scene (:scene this) :x (:x this) :y (:y this)}))]
    (cond
      (and
       (>= (count collisions) 2)
       (<
        (:energy this)
        (reduce max
                (map #(:energy @% -1) collisions))))
      (merge this (find-empty-tile (:scene this)))
      :else
      this)))
;;shared behavior
;;defprotocol

(defprotocol game-piece
  (behavior [this])
  (die [this])
  (reproduce [this]))

;todo
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
          (merge {:energy (+ (/ delta 1000) energy)})
          (shift)
          (teleport))))
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
    this))

;ok, how do carrots handle being crowded?
;one genne determines the size of it's territory.
;A higher number means it gets effected by more distand carrots.
;A higher number ALSO means higher photosynthesis yield.
;Then we want some strategies for handling incoming damage.
;1, lower both of your energy by the (min en1 en2) and kill competitors
;at the risk of starving?
;2, parasite, sap sone energy every turn?
;3 maybe a more cooperative option? How about a carrot that just takes reduced
;penalty for having nearby carrot?
;Not sure how these are for game balance but it would at least
;be more interesting


(defn spawn-carrot []
  (let [scene "LoruleH8"
        coords (find-empty-tile scene)]
    (add-game-piece!
     (map->carrot
      {:id (gen-id)
       :genes (generate-genes :repro-threshold :b-gene)
       :energy 20
       :scene scene
       :sprite "carrot"
       :milliseconds (System/currentTimeMillis)
       :x (:x coords)
       :y (:y coords)
       :health 1}))))
