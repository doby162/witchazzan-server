;;namespace
(ns witchazzan.core
  (:gen-class))
(declare name->scene)
(declare scene->players)
(declare scene->pieces)
(declare tile-location)
(declare update-game-piece)
(declare method)
;todo: seperate namespace
;;namespace

(defn player-hit
  [this strength]
  (update-game-piece
   (:id this)
   {:health
    (- (:health this) (max 0 (- strength (:defence this))))}))

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
             (not (= (:id %) (:id (:owner this)))))
           (scene->players (:scene this)))))
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
