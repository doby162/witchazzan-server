;;namespace
(ns witchazzan.behavior
  (:require [witchazzan.common :refer :all])
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

;;let's define A* pathfinding with a few functions
;;thank you, "The Joy of Clojure"

(def world [[1 1 1 1 1]
            [9 9 9 9 1]
            [1 1 1 1 1]
            [1 9 9 9 9]
            [1 1 1 1 1]])

(defn min-by
  "Find the smallest in a collection, by supplied metric"
  [f coll]
  (when (seq coll)
    (reduce (fn [min other]
              (if (> (f min) (f other))
                other
                min))
            coll)))

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
                                     size
                                     yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %))
                deltas))))

(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2)))

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (or (:cost cheapest-nbr) 0)))

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost
                                   (keep #(get-in routes %)
                                         nbr-yxs))
              newcost (path-cost (get-in cell-costs yx)
                                 cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx
                             {:cost newcost
                              :yxs (conj (:yxs cheapest-nbr [])
                                         yx)})
                   (into rest-work-todo
                         (map
                           (fn [w]
                             (let [[y x] w]
                               [(total-cost newcost step-est size y x) w]))
                           nbr-yxs)))))))))

(comment (astar [0 0] 5 world))

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

(defn teleport
  "check for and apply teleports"
  [this]
  (let [scene (name->scene (:scene this))
        ^clojure.lang.LazySeq tp-collisions
        (map #(get (get % "data") (+ (int (:x this)) (* (:width scene) (int (:y this))))) (:teleport scene))]
    (cond
      (and
       (some #(not (= 0 %)) tp-collisions))
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
            target-obj
            (or
             (ffilter ; intended entrance
              #(= (get % "name") target-obj-name)
              (:objects (name->scene (keyword (get target "name")))))
             (ffilter ; backup entrance.
              #(= (get % "name") "Default Spawn Point")
              (:objects (name->scene (keyword (get target "name"))))))]
        (cond
          target-obj
          (merge this
                 {:x (/ (get target-obj "x") tilewidth)
                  :y (/ (get target-obj "y") tilewidth)
                  :scene (keyword (get target "name"))})
          :else (merge this (find-empty-tile (:scene this)))))
        ;if we can't find a target-obj, the scene we want doesn't exist.
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

(defn shift
  [this]
  (let [collisions
        (active-pieces {:type (:type this) :scene (:scene this) :x (:x this) :y (:y this)})]
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
        :genes (generate-genes :repro-threshold :color-r :color-g :color-b)
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
        (shift)
        (hunger)
        (hit-points)
        (carrot-repro-decide)
        (teleport))))

(defmethod behavior :player
  [this]
  (let [time (System/currentTimeMillis)
        delta (- time (:milliseconds this))]
    (-> this
        (merge {:milliseconds time})
        (merge {:delta delta})
        (cast-spell)
        (hit-points)
        (hunger))))

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

(defn walk-step
  "one small step for an animal, one giant leap for this game engine"
  [this]
  (if (realized? (:vector this))
    (merge this {:vector
                 (apply-over-time {:target (one-game-piece (:id this))
                                   :key (if (rand-bool) :x :y)
                                   :value (- 1 (rand-int 3))
                                   :time (setting :idle-millis-per-frame)})})
    this))

(defn munch
  [this types]
  (let [collision-object
        (ffilter #(contains? types (:type @%))
                 (active-pieces {:scene (:scene this) :x (:x this) :y (:y this)}))]
    (if collision-object
      (let [collision-data @collision-object]
        (send collision-object die) ; plz to be eaten
        (merge this {:energy (+ (:energy this) (/ (:energy collision-data) 10))}))
      this)))

 (defn herbivore-choose-dest
   "Pick somewhere to walk to"
   [this]
   (let [dest (find-empty-tile (:scene this))
         start-yx [(int (:y this)) (int (:x this))]
         step-est 10
         tile-map (name->scene (:scene this))
         terrain-layer (map #(+ 1 (* 10 %)) (:syri tile-map))
         map-height (:height tile-map)
         map-width (:width tile-map)
         veggie-tiles (->> (square-range (max map-height map-width )) (filter #(not (tile-occupied (:scene this) %))))
         tiles-partitioned (partition map-width terrain-layer)
         node-costs (vec (map #(vec %) tiles-partitioned))
         path (astar start-yx step-est node-costs)
         ]
     (merge this {:destination dest :plan path})))

 (defn check-dest
   "Are we there yet?"
   [this]
   (if (and (within-n 1 (:x this) (:x (:destination this))) (within-n 1 (:y this) (:y (:destination this))))
     (dissoc this :destination :path)
     this))

(defmethod behavior :herbivore
  [this]
  (let [time (System/currentTimeMillis)
        delta (- time (:milliseconds this))]
    (-> this
        (merge {:milliseconds time})
        (merge {:delta delta})
        (as-> t (if (:destination t) this (herbivore-choose-dest t)))
        (check-dest)
        (walk-step)
        (hunger)
        (munch #{:carrot})
        (hit-points)
        (carrot-repro-decide)
        (teleport))))

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
