;;namespace
(ns witchazzan.core
  (:gen-class))
(declare name->scene)
(declare scene->players)
(declare scene->pieces)
(declare tile-location)
(declare method)
(declare game-state)
(declare find-empty-tile)
(declare mutate-genes)
(declare normalize-genes)
(declare find-adjacent)
(declare id->piece)
(declare rand-nth-safe)
;todo: seperate namespace
;;namespace

(defn hourly-behavior
  "an abstraction for all objects running their code on the hour"
  [this]
  (cond (not (= (:clock @game-state) (:clock this)))
        (method (merge this {:clock (:clock @game-state)}) :hourly (list))
        :else this))

(defn teleport [this]
  "teleports to a random tile on the target scene"
  (when
   (not (:teleport-debounce this))
    ((:teleport (name->scene (:scene this))) this)))

(defn carrot-hourly
  [this]
  (cond (and
         (>= (:repro-chance (:genes this)) (rand-int (setting "gene-max")))
         (>= (:energy this) (:repro-threshold (:genes this))))
        (method this :reproduce (list))
        (<= (:energy this) 0)
        (merge this {:delete-me true})
        :else
        (->
         this
         (merge {:energy (method this :photosynth (list))})
         (merge (teleport this)))))

(defn plant-reproduce [this]
  (let [energy (/ (:energy this) 3)]
    (merge
     this
     {:energy energy
      :outbox (conj (:outbox this)
                    (-> this
                        (merge {:outbox nil :teleport-debounce nil :id nil})
                        (merge {:mail-to "new-object"}) ;the new object handler will open this mail
                        (merge {:energy energy})
                        (merge (find-empty-tile (:scene this)))
                        (merge {:genes (normalize-genes (mutate-genes (:genes this)))})))})))

(defn sunny?
  "so how's the weather?"
  []
  (cond (and (>= (:clock @game-state) 6) (< (:clock @game-state) 20)) true
        :else false))

(defn photosynth
  [this]
  (cond
    (sunny?)
    (+ (:energy this); add to energy 1 energy minus 10% per adjacent item, not including the object
       (* 0.1 (- 11 (count (find-adjacent this)))))
    :else
    (- (:energy this) 1)))

(defn fireball-collide [this]
  (not
   ((:get-tile-walkable
     (name->scene (:scene this))) (tile-location this))))

(defn fireball-collide-players [this]
  (first
   (filter #(and
             (within-n (:x this) (:x %) (:tilewidth (name->scene (:scene this))))
             (within-n (:y this) (:y %) (:tilewidth (name->scene (:scene this))))
             (not (or (= (:id %) (:owner this)) (= (:id %) (:id this)))))
           (scene->pieces (:scene this)))))

(defn fireball-move
  [this]
  (let [speed (:speed this)]
    (cond
      (= "north" (:direction this)) (conj this {:y (- (:y this) speed)})
      (= "south" (:direction this)) (conj this {:y (+ (:y this) speed)})
      (= "east" (:direction this)) (conj this {:x (+ (:x this) speed)})
      :else (conj this {:x (- (:x this) speed)}))))

(defn fireball-behavior
  [this]
  (let [collide-player (:id (method this :collide-players (list)))]
    (cond
      collide-player
      (merge this {:delete-me true
                   :outbox
                   {:mail-to collide-player :method "hit"}})
      (method this :collide (list))
      (merge this {:delete-me true})
      :else (method this :move (list)))))

(defn player-behavior
  [this]
  this)

;put this somewhere
(defn thread-debug
  "both print and return the value"
  [x]
  (pp/pprint x)
  x)

(defn player-inbox
  [this]
  (let [hits (filter #(= (:method %) "hit") (:inbox this))
        location-updates (dissoc
                          (apply merge (reverse (filter #(= (:method %) "location-update") (:net-inbox this))))
                          [:mail-to])]
    (as->
     this t
      (merge t {:inbox nil})
      (merge t {:net-inbox nil})
      (cond (> (count hits) 0)
            (merge t {:health (- (:health t) 1)})
            :else t)
      (merge t location-updates))))

(defn carrot-inbox
  [this]
  (let [hits (filter #(= (:method %) "hit") (:inbox this))]
    (cond
      (> (count hits) 0)
      (merge this {:delete-me true})
      :else
      (merge this {:inbox nil}))))

(defn ignore-inbox
  [this]
  (merge this {:inbox nil}))

(defn blank-behavior [this & args] this)

(defn walk-towards-object
  [this that speed]
  (let [angle (Math/atan2 (- (:x this) (:x that)) (- (:y this) (:y that)))]
    (merge this
           {:x (- (:x this) (* speed (Math/sin angle))) :y (- (:y this) (* speed (Math/cos angle)))})))

(defn slime-hunt
  [this]
  (cond
    (= (:scene (id->piece (:hunted this))) (:scene this))
    (walk-towards-object this (id->piece (:hunted this)) 5)
    :else
    (merge this
           {:hunted
            (:id (rand-nth-safe (scene->players (:scene this))))})))

(defn slime-behavior
  [this]
  (->
   this
   (hourly-behavior)
   (method :hunt (list))
   (merge (teleport this))))

(defn slime-hourly
  [this]
  this)

(defn slime-inbox
  [this] (carrot-inbox this))
