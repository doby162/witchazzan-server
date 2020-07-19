;;namespace
(ns witchazzan.behavior
  (:require [witchazzan.common :refer :all])
  (:require [clojure.pprint :as pp])
  (:require [witchazzan.astar :refer [astar draw-path]])
  (:gen-class))

(declare die)
(declare reproduce)
(declare behavior)
;;namespace
;;game-piece creation helpers
;normalize settings values
(def hunger-constant (/ (setting :hunger) (setting :millis-per-hour)))
(def photosynthesis-constant (/ (setting :photosynthesis) (setting :millis-per-hour)))

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
  (normalize-genes
   (zipmap (keys genes)
           (map #(+ (- (rand-int 3) 1) %) (vals genes)))))

(defn generate-genes
  "makes a list of keywords into a map of ints, arbitrarily limited by settings"
  [& keywords]
  (zipmap keywords
          (repeatedly #(rand-int (+ 1 (setting "gene-max"))))))

(defn apply-over-time
  "Apply a change to a numeric value on the target spread over time.
  Returns a future. Check status with (realized? future)"
  [{target :target
    key :key
    value :value
    time :time
    absolute :absolute
    added-values :added-values}]
  (let [time-step (setting :min-millis-per-frame)
        steps (Math/ceil (/ time time-step))
        value-step (if absolute (/ (- value (key @target)) steps) (/ value steps))]
    (future
      (loop [n 1]
        (send target #(merge % added-values {key (+ (key @target) value-step)}))
        (Thread/sleep time-step)
        (if (< n steps)
          (recur (inc n))
          nil)))))

(defn rand-bool [] (zero? (rand-int 2)))

(defn shift
  [this]
  (let [collisions
        (active-pieces {:type (:type this) :scene (:scene this) :x (:x this) :y (:y this)})
        empty-tile (find-empty-tile (:scene this))]
    (cond
      (and (>= (count collisions) 2) empty-tile)
      (merge this empty-tile)
      :else
      this)))

(defn teleport
  "check for and apply teleports"
  [this]
  (let [scene (name->scene (:scene this))
        ^clojure.lang.LazySeq tp-collisions
        (map #(get (get % "data") (+ (int (:x this)) (* (:width scene) (int (:y this))))) (:teleport scene))]
    (cond
      (some #(not (= 0 %)) tp-collisions)
      (let [target (nth (:teleport scene) (.indexOf tp-collisions (apply max tp-collisions)))
            tilewidth (:tilewidth scene)
            target-obj-name
            (get
             (ffilter
              #(=
                "DestinationScene"
                (get % "name")) ;get the value of the property name DestinationScene
              (get target "properties"))
             "value")
            target-entrance-name
            (get
             (ffilter
              #(=
                "Entrance"
                (get % "name"))
              (get target "properties"))
             "value")
            target-obj
            (or
             (ffilter ; intended entrance
              #(= (get % "name") target-entrance-name)
              (:objects (name->scene (keyword target-obj-name))))
             (ffilter ; backup entrance.
              #(= (get % "name") "Default Spawn Point")
              (:objects (name->scene (keyword target-obj-name)))))]
        (if (and target-entrance-name target-obj-name target-obj)
          (shift
           (merge this
                  {:x (/ (get target-obj "x") tilewidth)
                   :y (/ (get target-obj "y") tilewidth)
                   :scene (keyword target-obj-name)}))
          this)) ;if we can't find a target-obj, the scene we want doesn't exist.
      :else this)))
;;helpers
;;shared behavior
(defn add-game-piece!
  [piece]
  (let [new-piece (agent piece)]
    (swap! game-state
           (fn [state] (update-in state [:game-pieces]
                                  (fn [game-pieces] (merge game-pieces new-piece)))))))

(defn delete
  [this]
  (swap! game-state
         (fn [state] (update-in state [:game-pieces]
                                (fn [game-pieces]
                                  (into #{}
                                        (filter
                                         #(not (= (:id this) (:id @%)))
                                         game-pieces)))))))
;;shared behavior
(defn hunger
  [this]
  (when (< (:energy this) 0) (die this))
  (merge this {:energy (- (:energy this) (* (:delta this) hunger-constant))}))

(defn hit-points
  [this]
  (cond
    (< (:health this) 1)
    (die this)
    :else
    this))

(defn sunny?
  [this]
  (and
   (> (:hour @game-state) (setting "dawn"))
   (< (:hour @game-state) (setting "sunset"))
   (not (re-seq #"Cave" (str (:scene this))))))

(defn crowded?
  [this]
  (let [this-scene (active-pieces {:type (:type this) :scene (:scene this)})]
    (seq (filter
          #(and
            (not (= (:id this) (:id @%)))
            (within-n (:x @%) (:x this) (setting :crowding-factor))
            (within-n (:y @%) (:y this) (setting :crowding-factor)))
          this-scene))))

(defn photosynthesis
  [this]
  (cond
    (and (sunny? this) (not (crowded? this)))
    (merge this {:energy (+ (:energy this) (* (:delta this) photosynthesis-constant))})
    :else this))

(defn carrot-repro-decide
  [this]
  (cond
    (> (:energy this) (:repro-threshold (:genes this)))
    (reproduce this)
    :else this))

(defn spell-terrain-collide [this]
  (cond
    (not ((:get-tile-walkable (name->scene (:scene this))) this))
    (die this)
    :else this))

(defn spell-object-collide [this]
  (let [collisions (filter
                    #(and (not= (:id this) (:id @%)) (not= (:owner-id this) (:id @%)))
                    (active-pieces {:x (:x this) :y (:y this) :scene (:scene this)}))]
    (cond
      (seq collisions)
      (do
        (when (= "fireball" (:spell this))
          (run! (fn [that] (send that merge {:health (- (:health @that 1) 1)})) collisions))
        (when (= "teleball" (:spell this))
          (run! (fn [that] (send that merge (find-empty-tile (:scene this)) {:force true})) collisions))
        (when (= "push" (:spell this))
          (run! (fn [that] (apply-over-time {:target that
                                             :key (if (rand-bool) :x :y)
                                             :value (if (rand-bool) (+ 1 (rand-int 3)) (- 0 (+ 1 (rand-int 3))))
                                             :time 250
                                             :added-values {:force true}})) collisions))
        (die this))
      :else this)))

(defn cast-spell
  [this]
  (let [spell (:spell this)]
    (cond
      (or (= "push" spell) (= "teleball" spell) (= "fireball" spell))
      (add-game-piece! {:id (gen-id)
                        :x (:x this)
                        :y (:y this)
                        :type :spell
                        :spell spell
                        :scene (:scene this)
                        :sprite spell
                        :speed 0.01
                        :direction (:direction this)
                        :milliseconds (System/currentTimeMillis)
                        :owner-id (:id this)}))
    (merge this {:spell nil})))

(defn spawn-carrot [& coords]
  (let [scene (or (:scene (into {} coords)) :LoruleH8)
        coords (if (seq coords) (into {} coords) (find-empty-tile scene))]
    (cond
      coords
      (add-game-piece!
       {:id (gen-id)
        :genes (generate-genes :repro-threshold :color)
        :energy 20
        :scene scene
        :sprite "carrot"
        :milliseconds (System/currentTimeMillis)
        :x (:x coords)
        :y (:y coords)
        :parent-id -1
        :health 1
        :type :carrot})
      :else (log "spawn-carrot failed to find an empty tile"))))

(defn spawn-herbivore [& coords]
  (let [scene (or (:scene (into {} coords)) :LoruleH8)
        coords (if (seq coords) (into {} coords) (find-empty-tile scene))]
    (cond
      coords
      (add-game-piece!
       {:id (gen-id)
        :genes (generate-genes :repro-threshold)
        :energy 50
        :scene scene
        :sprite "herbivore"
        :milliseconds (System/currentTimeMillis)
        :x (:x coords)
        :y (:y coords)
        :parent-id -1
        :health 1
        :type :herbivore
        :vector (future nil)})
      :else (log "spawn-herbivore failed to find an empty tile"))))

(defmulti behavior :type)

(defmethod behavior :carrot
  [this]
  (let [time (System/currentTimeMillis)
        delta (- time (:milliseconds this))]
    (-> this
        (merge {:milliseconds time})
        (merge {:delta delta})
        (photosynthesis)
        (carrot-repro-decide)
        (teleport)
        (hunger)
        (hit-points))))

(defmethod behavior :player
  [this]
  (let [time (System/currentTimeMillis)
        delta (- time (:milliseconds this))]
    (-> this
        (merge {:milliseconds time})
        (merge {:delta delta})
        (cast-spell)
        (hunger)
        (hit-points))))

(defmethod behavior :spell
  [this]
  (let [time (System/currentTimeMillis)
        delta (- time (:milliseconds this))]
    (-> this
        (merge {:milliseconds time})
        (merge
         (cond
           (= (:direction this) "up")
           {:y (- (:y this) (* (:speed this) delta))}
           (= (:direction this) "down")
           {:y (+ (:y this) (* (:speed this) delta))}
           (= (:direction this) "left")
           {:x (- (:x this) (* (:speed this) delta))}
           (= (:direction this) "right")
           {:x (+ (:x this) (* (:speed this) delta))}))
        (spell-terrain-collide)
        (spell-object-collide)
        (teleport))))

(defn walk-step-rand
  "one small step for an animal, one giant leap for this game engine"
  [this]
  (if (realized? (:vector this))
    (merge this {:vector
                 (apply-over-time {:target (one-game-piece (:id this))
                                   :key (if (rand-bool) :x :y)
                                   :value (- 1 (rand-int 3))
                                   :time (setting :idle-millis-per-frame)})})
    this))

(defn walk-step
  "one small step for an animal, one giant leap for this game engine
  always attempts to stay centered on tiles"
  [this]
  (if (and (realized? (:vector this)) (first (:path this)))
    (let [path-next (first (:path this))
          path-rest (rest (:path this))
          loc [(int (:y this)) (int (:x this))]
          key (if (= (first loc) (first path-next)) :x :y)
          difference (- (apply + path-next) (apply + loc))
          direction (cond
                      (and (= key :x) (> difference 0)) "right" (and (= key :x) (< difference 0)) "left"
                      (and (= key :y) (> difference 0)) "down" (and (= key :y) (< difference 0)) "up")]
      (if (and (instance? Long difference) (>= 1 (Math/abs difference))) ; if difference is too high it indicates that we have teleported
        (merge this {:path path-rest
                     :vector
                     (apply-over-time {:target (one-game-piece (:id this))
                                       :key key
                                       :value (+ 0.5 (if (= key :y) (first path-next) (last path-next)))
                                       :time (setting :idle-millis-per-frame)
                                       :absolute true
                                       :added-values {:moving (if (zero? difference) false true) :direction direction}})})
        (dissoc this :path :dest)))
    this))

(defn munch
  [this types]
  (let [collision-object
        (ffilter #(contains? types (:type @%))
                 (active-pieces {:scene (:scene this) :x (:x this) :y (:y this)}))]
    (if collision-object
      (let [collision-data @collision-object]
        (send collision-object die) ; plz to be eaten
        (merge this {:path [(list (:y this) (:x this)) (list (:y this) (:x this))] :energy (+ (:energy this) (/ (:energy collision-data) (setting :herbivore-efficiency)))}))
      this)))

(defn herbivore-choose-dest
  "Pick somewhere to walk to"
  [this]
  (let [tile-map (name->scene (:scene this))
        map-height (:height tile-map)
        map-width (:width tile-map)
        veggie-tiles (map #(deref %) (game-pieces {:scene (:scene this) :type :carrot}))
        dest (if (seq veggie-tiles) (rand-nth veggie-tiles) (find-empty-tile (:scene this)))
        start-xy [(int (:y this)) (int (:x this))]
        dest-xy [(int (:y dest)) (int (:x dest))]
        terrain-layer (map #(if (zero? %) 1 100) (:syri tile-map))
        tiles-partitioned (partition map-width terrain-layer)
        node-costs (vec (map #(vec %) tiles-partitioned))
        setup {:start start-xy, :finish dest-xy}
        path (astar node-costs setup)
        correctness (map #((:get-tile-walkable (name->scene (:scene this))) {:y (first %) :x (last %)}) (:path path))]
    ;(draw-path node-costs (:path path)) ; uncomment for some rad debugging
    (if (> 99 (:cost path))
      (merge this {:destination dest :path (:path path)})
      this)))

(defmethod behavior :herbivore
  [this]
  (let [time (System/currentTimeMillis)
        delta (- time (:milliseconds this))]
    (-> this
        (merge {:milliseconds time})
        (merge {:delta delta})
        (as-> t (if (and (:destination t) (first (:path t))) t (herbivore-choose-dest t)))
        (walk-step)
        (munch #{:carrot})
        (carrot-repro-decide)
        (teleport)
        (hunger)
        (hit-points))))

(defmulti die :type)

(defmethod die :carrot
  [this]
  (delete this)
  nil)

(defmethod die :player
  [this]
  (log (str "player " (:name this) " would die if we implemented death! energy:" (int (:energy this)) " health:" (:health this)))
  (merge this {:health 100 :energy 100}))

(defmethod die :spell
  [this]
  (delete this)
  nil)

(defmethod die :herbivore
  [this]
  (delete this)
  nil)

(defmulti reproduce :type)

(defmethod reproduce :carrot
  [this]
  (let [energy (/ (:energy this) 3)
        tile (find-empty-tile (:scene this))
        genes (mutate-genes (:genes this))]
    (add-game-piece!
     (merge this
            {:genes genes
             :x (:x tile)
             :y (:y tile)
             :energy energy
             :parent-id (:id this)
             :id (gen-id)}))
    (merge this {:energy energy})))

(defmethod reproduce :player
  [this]
  this)

(defmethod reproduce :spell
  [this]
  this)

(defmethod reproduce :herbivore
  [this]
  (let [energy (/ (:energy this) 3)
        tile (find-empty-tile (:scene this))
        genes (mutate-genes (:genes this))]
    (add-game-piece!
     (merge this
            {:genes genes
             :x (:x tile)
             :y (:y tile)
             :energy energy
             :parent-id (:id this)
             :id (gen-id)}))
    (merge this {:energy energy})))
