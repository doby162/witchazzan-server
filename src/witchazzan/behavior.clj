;;namespace
(ns witchazzan.behavior
  (:require [witchazzan.common :refer :all])
  (:require [clojure.pprint :as pp])
  (:require [clojure.data.priority-map :refer [priority-map]])
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
  (if (and (realized? (:vector this)) (peek (:path this)))
    (let [path-next (peek (:path this))
          path-rest (rest (:path this))
          loc [(int (:y this)) (int (:x this))]
          key (if (= (first loc) (first path-next)) :x :y)
          difference (- (apply + path-next) (apply + loc))]
      (if (>= 1 (Math/abs difference)) ; if difference is too high it indicates that we have teleported
        (merge this {:path path-rest
                     :vector
                     (apply-over-time {:target (one-game-piece (:id this))
                                       :key key
                                       :value (+ 0.5 (if (= key :y) (first path-next) (last path-next)))
                                       :time (setting :idle-millis-per-frame)
                                       :absolute true})})
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
        (merge this {:path [(list (:y this) (:x this))] :energy (+ (:energy this) (/ (:energy collision-data) (setting :herbivore-efficiency)))}))
      this)))

(declare astar)
(declare draw-path)

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
        (as-> t (if (and (:destination t) (peek (:path t))) t (herbivore-choose-dest t)))
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

;pathfinding, thank you planjure!
;https://github.com/elben/planjure

(def setup
  {:start [5 0]
   :finish [0 1]})

;; Assumes a path of start to finish
(def path [[5 0] [5 1] [5 2] [5 3] [4 3] [3 3] [2 3] [1 3] [1 2] [1 1] [0 1]])

(defn random-tile
  "Randomly generate a tile with chance of it being cost of 1."
  [chance]
  (if (< (rand) chance)
    1
    (+ 1 (rand-int 5))))

(defn random-world [rows cols]
  (vec (repeatedly rows
                   (fn [] (vec (repeatedly cols #(random-tile 0.7)))))))

(defn create-empty-ascii
  [world]
  (let [height (count world)
        width (count (first world))
        ascii (vec (repeat height (vec (repeat width " "))))]
    ascii))

(defn build-basic-ascii
  [world]
  (vec (for [row world]
         (vec (for [node row]
                (if (> node 0)
                  "#"
                  " "))))))

(defn build-ascii-path
  [world path]
  (loop [ascii (build-basic-ascii world)
         path path]
    (if-not (empty? path)
      (let [[r c] (first path)
            row (ascii r)]
        (recur (assoc ascii r (assoc row c "@"))
               (rest path)))
      ascii)))

(defn draw-world
  [world]
  (let [cols (count (first world))]
    (print "   ")
    (doseq [i (range cols)]
      (print i ""))
    (println)
    (doseq [i (range (count world))]
      (println i (world i)))))

(defn draw-path
  [world path]
  (let [ascii (build-ascii-path world path)]
    (doseq [row ascii]
      (println row))))

(defn cost
  "Cost of traversing from a neighboring node to specified node in world. In
  theory, the edges hold the cost. But in our current world, the nodes hold the
  cost. Means that the cost of moving from any of its neighboring node to
  specified node is the value of the node cell."
  [world node]
  (let [[r c] node]
    ((world r) c)))

(defn cost-heuristic
  "Similar to `cost`, but use Manhattan distance heuristics from node to finish
  node."
  [world node finish]
  (let [[r c] node
        [r2 c2] finish
        g-cost (cost world node)
        ;; Add tie-breaker nudge into h-cost. Note that this breaks
        ;; admissability, so path may not be optimal.
        ;; http://theory.stanford.edu/~amitp/GameProgramming/Heuristics.html#breaking-ties
        h-cost (* (+ (Math/abs (- r r2)) (Math/abs (- c c2))) 1.005)]
    (+ g-cost h-cost)))

(defn find-path
  "Find path from start to finish. Previous is a backtrack mapping of nodes,
  and setup is a map containing :start and :finish nodes. Returns partial path
  if no is available (e.g. no path or recursive path).
  
  Returns map:

  {:path [...]
   :cost Int}
  "
  [previous world {:keys [start finish] :as setup}]
  (if-not (contains? previous finish)
    {:path [] :cost 0}
    (loop [path []
           seen #{}
           node finish
           cost-sum 0]
      (cond
        ;; This check needs to go first, before the contains? check, since
        ;; start is not in previous mapping.
        ;;
        ;; Done, found path from finish to start. Reverse so the path presented
        ;; is start to finish.
        (= node start) {:path (reverse (conj path start)) :cost cost-sum}

        ;; Cannot complete path. Return best path, ending in finish.
        (not (contains? previous node)) {:path (reverse path) :cost cost-sum}

        ;; Seen before. Recursive. Cannot complete path.
        (contains? seen node) {:path (reverse (conj path node)) :cost cost-sum}

        ;; Can backtrack, so recur.
        :else (recur (conj path node) (conj seen node) (previous node) (+ cost-sum (cost world node)))))))



; dfs algorithm
; - each node, we note whether or not we've gone through this, storing the
;   lowest cost.
; - push current node into stack
; - while true:
;   - pop node n from stack.
;   - if empty, done.
;   - if found finish, done.
;
;   - for each neighbor neigh:
;     - calculate cost for node.g + cost(node, neigh)
;     - if this new cost is lower than the old cost (default cost is infinite):
;       - save g-costs(neigh)
  ;     - set previous(neigh) = node
;     - mark as visited
;     - push neigh into stack.
; - backtrack from start to finish.
; - draw path.
;
;
; finishing cases (nothing left in stack):
; - if there's nothing left, there's no work left to be done.
; - we can now examine finish node and see the cost (g-costs), and traverse back
;   to start (via previous)
; - if finish has un-initialized g-costs, or if we can't traverse back to start,
;   then the path of start to finish is infinitely high


(defn neighbors
  "Return the neighbors of node. Only four directions. Don't include start node
  as neighbor, because we assume it's never better to back through the start
  node (that is, we assume consistency in costs)."
  [world node {:keys [start]}]
  (let [rows (count world)
        cols (count (first world))
        find-neighbors (fn [neighs [row-mod col-mod]]
                         (let [row (+ (node 0) row-mod)
                               col (+ (node 1) col-mod)]
                           (if (and
                                 ;; Don't include start node
                                (not (= [row col] start))

                                 ;; Check bounds.
                                (< row rows)
                                (>= row 0)
                                (< col cols)
                                (>= col 0))
                             (conj neighs [row col])
                             neighs)))]
    (reduce find-neighbors [] [[1 0] [0 1] [-1 0] [0 -1]])))

(defn lookup-g-cost
  [g-costs node]
  (if (contains? g-costs node)
    (g-costs node)
    0))

(defn nodes-with-improved-costs
  "Find nodes that would improve its cost if it were to arrive from node instead
  of the cost specified in g-cost. Exclude nodes that would increase its cost.

  If a cost function is not provided, default to the default cost function,
  `cost`, defined in this namespace. If provided, use that.

  Returns map of nodes to cost."
  ([world g-costs base-node nodes]
   (nodes-with-improved-costs world g-costs base-node nodes cost))
  ([world g-costs base-node nodes cost-fn]
   (let [rows (count world)
         cols (count (first world))
         collect-improved-nodes (fn
                                  [improved-nodes node]
                                  (let [new-cost (+ (lookup-g-cost g-costs base-node) (cost-fn world node))]
                                    (if (or
                                         (not (contains? g-costs node))
                                         (< new-cost (lookup-g-cost g-costs node)))

                                      ;; Use this node if it wasn't in the g-costs
                                      ;; before (never been traversed to), or if it's
                                      ;; cheaper coming from this base-node than before.
                                      (assoc improved-nodes node new-cost)

                                      ;; If more expensive, don't use this node
                                      improved-nodes)))]
     (reduce collect-improved-nodes {} nodes))))

(defn generic-astar
  "A* algorithm. Takes a world, setup, and cost function.

  Returns a map containing:

  {
  :path    [...]
  :visited [...]
  }

  Where :path is the optimal path from start to finish, and :visited are the
  nodes visited by the search."
  [world {:keys [start finish] :as setup} cost-heuristic]
  (loop [pq (priority-map start 0)
         g-costs {}
         previous {}]
    (cond
      (empty? pq)
      ;; Never found finish. Plan as best as we can.
      (assoc (find-path previous world setup) :visited (map first previous))

      (= (first (first pq)) finish)
      ;; We're done.
      ;;
      ;; If we popped finish, that means that finish was pushed as a neighbor
      ;; and thus we have 'previous' set up already.
      (assoc (find-path previous world setup) :visited (map first previous))

      :else
      (let [node (first (first pq)) ;; Get highest priority node (throw away priority).
            pq (pop pq)
            neighs (neighbors world node setup)

            ;; The *real* costs for these neighbors
            improved-neighbor-costs (nodes-with-improved-costs world g-costs node neighs cost)
            improved-neighbors (keys improved-neighbor-costs)

            ;; Heuristic costs for neighbors.
            ;; TODO: optimize this so that we calculate real cost (above) AND
            ;; heuristic cost at the same time, in O(n) instead of O(2n).
            improved-neighbor-heuristics (nodes-with-improved-costs world g-costs node neighs cost-heuristic)

            updated-g-costs (merge g-costs improved-neighbor-costs)
            updated-previous (merge previous
                                    ;; Create map of neighbors to node
                                    ;; %1 is map, %2 neighbor.
                                    (reduce #(assoc %1 %2 node) {} improved-neighbors))]
        ;; Push new neighbors into priority queue
        (recur (into pq (vec improved-neighbor-heuristics)) updated-g-costs updated-previous)))))

(defn astar
  "A* algorithm.

  Returns a map containing:

  {
  :path    [...]
  :visited [...]
  }

  Where :path is the optimal path from start to finish, and :visited are the
  nodes visited by the search."
  [world {:keys [start finish] :as setup}]
  (let [cost-fn (fn [world node] (cost-heuristic world node finish))]
    (generic-astar world setup cost-fn)))

(defn dijkstra
  "Dijkstra's classic graph algorithm.

  Returns a map containing:

  {
   :path    [...]
   :visited [...]
  }

  Where :path is the optimal path from start to finish, and :visited are the
  nodes visited by the search."
  [world setup]
  (generic-astar world setup cost))

(defn dfs
  "Depth-first search.

  Instead of terminating right away after finish is found, this DFS
  implementation exhausts the search space to find the global maxima.

  Returns a map containing:

  {
   :path    [...]
   :visited [...]
  }

  Where :path is the optimal path from start to finish, and :visited are the
  nodes visited by the search."
  [world {:keys [start finish] :as setup}]
  (loop [stack [start] ;; init stack with start node
         g-costs {}    ;; map of node to cost
         previous {}]  ;; map of node to node
    (cond
      (empty? stack)
      ;; We're done! get out of loop.
      (assoc (find-path previous world setup) :visited (map first previous))

      (= (last stack) finish)
        ;; If we popped finish, don't push its neighbors into stack, because
        ;; then we'd never finish. Instead, just pop off stack and continue
        ;; finding other paths. Goal here isn't to just find any path, but to
        ;; find the best path using DFS.
      (recur (pop stack) g-costs previous)

      :else
        ;; Else, we need to do work.
        ; - Find neighbors.
        ; - Figure out g costs from node to each of those neighbors
        ; - better-neighs = neighbors that are better through node
        ; - modify g-costs of better-neighs
        ; - modify previous of better-neighs
        ; - push better-neighs into stack
      (let [node (last stack)
            stack (pop stack)
            neighs (neighbors world node setup)
            improved-neighbor-costs (nodes-with-improved-costs world g-costs node neighs)
            improved-neighbors (keys improved-neighbor-costs)
            updated-g-costs (merge g-costs improved-neighbor-costs)
            updated-previous (merge previous
                                      ;; Create map of neighbors to node
                                      ;; %1 is map, %2 neighbor.
                                    (reduce #(assoc %1 %2 node) {} improved-neighbors))]
          ;; Push new neighbors into stack.
        (recur (into stack improved-neighbors) updated-g-costs updated-previous)))))

;; arrows could be cool for planning http://www.alanwood.net/unicode/arrows.html
