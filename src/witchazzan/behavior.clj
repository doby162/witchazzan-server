;;namespace
(ns witchazzan.core
  (:gen-class))
(declare name->scene)
(declare scene->players)
(declare scene->pieces)
(declare tile-location)
(declare update-game-piece)
(declare add-game-piece)
(declare method)
(declare game-state)
(declare find-empty-tile)
;todo: seperate namespace
;;namespace

(defn player-hit
  [this strength]
  (update-game-piece
   (:id this)
   {:health
    (- (:health this) (max 0 (- strength (:defence this))))}))
(defn plant-hit
  [this strength]
  (update-game-piece
   (:id this) ;drop items?
   {:delete-me true}))

(defn hourly-behavior
  "an abstraction for all objects running their code on the hour"
  [this]
  (cond (not (= (:clock @game-state) (:clock this)))
        (method (merge this {:clock (:clock @game-state)}) :hourly (list))
        :else this))

(defn carrot-hourly
  [this]
  (cond (> (:energy this) 10000)
        (method this :reproduce (list))
        (< (:energy this) 0)
        (merge this {:delete-me true})
        :else
        (merge this {:energy (method this :photosynth (list))})))

(defn plant-reproduce [this]
  (let [energy (/ (:energy this) 4)]
    (add-game-piece
     (-> this
         (merge {:energy energy})
         (merge (find-empty-tile (:scene this)))))
    (merge this {:energy energy})))

(defn sunny?
  "so how's the weather?"
  []
  (cond (and (>= (:clock @game-state) 6) (< (:clock @game-state) 20)) true
        :else false))

(defn photosynth
  [this]
  (cond (sunny?) (+ 2 (:energy this))
        :else (dec (:energy this))))

(defn fireball-collide [this]
  (not
   ((:get-tile-walkable
     (name->scene (:scene this))) (tile-location this))))

(defn fireball-collide-players [this]
  (first
   (filter #(and
             (=
              (tile-location %)
              (tile-location this))
             (not (or (= (:id %) (:owner this)) (= (:id %) (:id this)))))
           (scene->pieces (:scene this)))))
(defn fireball-move
  [this]
  (cond
    (= "north" (:direction this)) (conj this {:y (dec (:y this))})
    (= "south" (:direction this)) (conj this {:y (inc (:y this))})
    (= "east" (:direction this)) (conj this {:x (inc (:x this))})
    :else (conj this {:x (dec (:x this))})))

(defn fireball-behavior
  [this]
  (cond
    (method this :collide-players (list))
    (do
      (method (method this :collide-players (list)) :hit (list 1))
      (merge this {:delete-me true}))
    (method this :collide (list)) (merge this {:delete-me true})
    :else (method this :move (list))))

(defn player-behavior
  [this]
  this)

(defn blank-behavior [& args] true)
