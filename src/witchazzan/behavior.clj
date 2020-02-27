;;namespace
(ns witchazzan.behavior
  (:require [witchazzan.world :as world])
  (:require [witchazzan.common :as core])
  (:require [witchazzan.comms :as comms])
  (:require [clojure.pprint :as pp])
  (:gen-class))
;;namespace
;;default handlers
(defn ignore-inbox
  [this]
  (merge this {:inbox nil}))

(defn blank-behavior [this & args] this)

(defn hourly-behavior
  "an abstraction for all objects running their code on the hour"
  [this]
  (cond (not (= (:clock @core/game-state) (:clock this)))
        (world/method (merge this {:clock (:clock @core/game-state)}) :hourly (list))
        :else this))
;;default handlers
;;helpers
(defn normalize-genes
  "prevent mutation from moving genes outside the 0-gene-max range"
  [genes]
  (zipmap (keys genes)
          (map
           #(cond (> % (core/setting "gene-max"))
                  (- % (core/setting "gene-max"))
                  (< % 0)
                  (+ % (core/setting "gene-max"))
                  :else %)
           (vals genes))))

(defn mutate-genes
  "each gene can be incremeneted, decremented or stay the same with equal chance"
  [genes]
  (zipmap (keys genes)
          (map #(+ (- (rand-int 3) 1) %) (vals genes))))

(defn rand-nth-safe
  [list]
  (cond
    (= (count list) 0)
    nil
    :else
    (rand-nth list)))

(defn find-adjacent
  "returns a list of all pieces residing in the 9 adjacent tiles to the arg"
  [object]
  (let [map (world/name->scene (:scene object)) n (:tilewidth map)]
    (filter
     #(and (world/within-n (:x %) (:x object) n) (world/within-n (:y %) (:y object) n))
     (world/scene->pieces (:scene object)))))

(defn check-starve [t]
  (cond
    (>= 0 (:energy t))
    (merge t {:delete-me true})
    :else t))

(defn teleport [this]
  "check for and apply teleports"
  (when
   (not (:teleport-debounce this))
    ((:teleport (world/name->scene (:scene this))) this)))

(defn check-px-teleport
  "creates a list of all pixels that qualify as being within range of a teleport"
  [scene]
  (let [map (world/name->scene scene)]
    (core/cmap #(dissoc % :teleport-debounce)
               (filter #(not (nil? (:teleport-debounce %)))
                       (core/cmap
                        (fn [coords]
                          (try
                            (conj (teleport
                                   (conj {:scene scene} coords)) coords)
                            (catch Exception e nil)))
     ;check every single pixel for teleports
                        (world/square-range (* (:tilewidth map) (max (:width map) (:height map)))))))))

(defn sunny?
  "so how's the weather?"
  []
  (cond (and (>= (:clock @core/game-state) 6) (< (:clock @core/game-state) 20)) true
        :else false))

(defn photosynth
  [this]
  (cond
    (sunny?)
    (merge this {:energy (+ (:energy this)
                            (* 1 (- 11 (count (find-adjacent this)))))})
    :else this))

(defn hunger
  [this]
  (cond (> (:energy this) 0)
        (merge this {:energy (- (:energy this) 1)})
        :else this))

(defn fireball-collide [this]
  (not
   ((:get-tile-walkable
     (world/name->scene (:scene this))) (world/tile-location this))))

(defn fireball-collide-players [this]
  (first
   (filter #(and
             (world/within-n (:x this) (:x %) (:tilewidth (world/name->scene (:scene this))))
             (world/within-n (:y this) (:y %) (:tilewidth (world/name->scene (:scene this))))
             (not (or (= (:id %) (:owner this)) (= (:id %) (:id this)))))
           (world/scene->pieces (:scene this)))))

(defn slime-attack
  [this]
  (let [collide (fireball-collide-players this)]
    (cond
      collide
      (merge this {:energy (inc (:energy this))
                   :outbox
                   {:mail-to (:id collide)
                    :method "hit"
                    :sender (:id this)}})
      :else this)))

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
  (let [collide-player (:id (world/method this :collide-players (list)))]
    (as-> this t
      (cond
        collide-player
        (merge t {:delete-me true
                  :outbox
                  {:mail-to collide-player
                   :method "hit"
                   :sender (:owner this)}})
        (world/method t :collide (list))
        (merge t {:delete-me true})
        :else (world/method t :move (list)))
      (merge t (teleport t)))))

(defn fireball-blue-behavior
  [this]
  (let [collide-player (:id (world/method this :collide-players (list)))]
    (as-> this t
      (cond
        collide-player
        (merge t {:delete-me true
                  :outbox
                  {:mail-to collide-player :method "teleport-rand"}})
        (world/method t :collide (list))
        (merge t {:delete-me true})
        :else (world/method t :move (list)))
      (merge t (teleport t)))))

(defn implements-identity [this]
  (cond (:identity this) (do (comms/establish-identity this) (dissoc this :identity))
        :else this))

;put this somewhere
(defn thread-debug
  "both print and return the value"
  [x]
  (pp/pprint x)
  x)

(defn walk-towards-object
  [this that speed]
  (let [angle (Math/atan2 (- (:x this) (:x that)) (- (:y this) (:y that)))]
    (merge this
           {:x (- (:x this) (* speed (Math/sin angle))) :y (- (:y this) (* speed (Math/cos angle)))})))

(defn gene-speed
  "determines the speed a creature should have based
  on speed stats and a hard max, with 1 as a minimum"
  [this]
  (+ 1 (quot
        (:speed (:genes this))
        (- (quot (core/setting "gene-max") (:max-speed this)) 1))))
;;helpers
;;implementation functions, these add prepackaged traits by responding to stats and mail
(defn implements-location-updates [this]
  (merge
   this
   (dissoc
    (apply merge (reverse (filter #(= (:method %) "location-update") (:net-inbox this))))
    :mail-to :method :message_type)))

(defn implements-blue-fire [this]
  (let [tps (filter #(= (:method %) "teleport-rand") (:inbox this))]
    (cond
      (> (count tps) 0)
      (merge this (world/find-empty-tile (:scene this)) {:force true})
      :else this)))

(defn implements-hit
  "Lose health in response to an attack"
  [this]
  (let [hits (filter #(= (:method %) "hit") (:inbox this))]
    (cond
      (> (count hits) 0)
      (merge this {:health (dec (:health this))})
      :else this)))

(defn implements-death [this]
  (cond
    (< (:health this) 1) (merge this {:delete-me true})
    :else this))

(defn implements-player-death [this]
  (cond
    (and (< (:health this) 1) (not (:dead this)))
    (do
      (comms/broadcast
       {:messageType "chat" :name "Witchazzan.core" :id -1
        :content (str "RIP " (:name this))})
      (merge
       this
       {:dead true
        :outbox
        {:mail-to "new-object"
         :type "corpse"
         :id (:id this)
         :x (:x this)
         :y (:y this)
         :scene (:scene this)
         :sprite "corpse"
         :health 1
         :behavior "blank-behavior"
         :handle-mail "corpse-inbox"}}))
    :else this))
;;implementation functions, these add prepackaged traits by responding to stats and mail
;;object behaviors
(defn carrot-hourly
  [this]
  (as-> this t
    (world/method t :photosynth (list))
    (cond (and
           (>= (:repro-chance (:genes t)) (rand-int (core/setting "gene-max")))
           (>= (:energy t) (:repro-threshold (:genes t))))
          (world/method this :reproduce (list))
          :else t)
    (check-starve t)
    (hunger t)
    (merge t (teleport t))))

(defn plant-reproduce [this]
  (let [energy (/ (:energy this) 3)
        location (world/find-empty-tile (:scene this))]
    (cond
      location
      (merge
       this
       {:energy energy
        :outbox
        (-> this
            (merge {:outbox nil :teleport-debounce nil :id nil})
            (merge {:mail-to "new-object"})
            (merge {:energy energy})
            (merge {:x (:x location) :y (:y location)})
            (merge {:genes (normalize-genes (mutate-genes (:genes this)))}))})
      :else
      this)))

(defn carrot-inbox
  [this]
  (-> this
      (implements-hit)
      (implements-blue-fire)
      (implements-death)
      (ignore-inbox)))

(defn slime-hunt
  [this]
  (as-> this t
    (cond
      (and
       (= (:scene (world/id->piece (:hunted t))) (:scene t))
       (not (= false (:active (world/id->piece (:hunted t))))))
      (walk-towards-object t (world/id->piece (:hunted t)) (gene-speed t))
      :else
      (merge t
             {:hunted
              (:id (rand-nth-safe (core/scene->players (:scene t))))}))
    (cond
      (and (nil? (:hunted t)) (not (nil? (:roost t))))
      (walk-towards-object t (:roost t) (gene-speed t))
      :else
      t)))

(defn slime-behavior
  [this]
  (->
   this
   (hourly-behavior)
   (world/method :hunt (list))
   (slime-attack)
   (merge (teleport this))))

(defn slime-hourly
  [this]
  (as-> this t
    (merge t
           {:energy (- (:energy t) (gene-speed t))
            :teleport-debounce false
            :roost (world/find-empty-tile (:scene t))})
    (check-starve t)))

(defn slime-inbox
  [this]
  (-> this
      (carrot-inbox)))

(defn corpse-inbox
  [this] (carrot-inbox this))

(defn player-behavior
  [this]
  (-> this
      (implements-identity)
      (implements-player-death)))

(defn player-inbox
  [this]
  (-> this
      (implements-location-updates)
      (implements-hit)
      (implements-blue-fire)))
;;object behaviors
