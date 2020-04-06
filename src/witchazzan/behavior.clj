;;namespace
(ns witchazzan.behavior
  (:refer witchazzan.common)
  (:require [witchazzan.comms :as comms])
  (:require [clojure.pprint :as pp])
  (:gen-class))
;;namespace
;;default handlers
(defn ignore-inbox
  [this]
  (merge this {:inbox nil}))

(defn blank-behavior [this & args] this)

(defn collide
  "Check if two objects are on the same tile"
  [one two]
  (and (= (int (:x one)) (int (:x two))) (= (int (:y one)) (int (:y two)))))

#_(defn hourly-behavior
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

#_(defn find-adjacent
  "returns a list of all pieces residing in the 9 adjacent tiles to the arg"
  [object]
  (let [map (world/name->scene (:scene object)) n 1]
    (filter
     #(and (world/within-n (:x %) (:x object) n) (world/within-n (:y %) (:y object) n))
     (world/scene->pieces (:scene object)))))

#_(defn check-starve [t]
  (cond
    (>= 0 (:energy t))
    (merge t {:delete-me true})
    :else t))

#_(defn teleport [this]
  "check for and apply teleports"
  ;TODO: fix null pointer exception on a teleport to a scene without a designated entrance
  ;use the defauly entrance, like the players do
  ;refactor me
  (let [scene (world/name->scene (:scene this))
        tp-collisions
        (map #(get (get % "data") (+ (int (:x this)) (* (:width scene) (int (:y this))))) (:teleport scene))]
    (cond
      (and
       (some #(not (= 0 %)) tp-collisions))
      (let [target (nth (:teleport scene) (.indexOf tp-collisions (apply max tp-collisions)))
            tilewidth (:tilewidth scene)
            target-obj-name (get (first (get target "properties")) "value")
            target-obj
            (core/ffilter
             #(= (get % "name") target-obj-name)
             (:objects (world/name->scene (get target "name"))))]
        (merge this
               {:x (/ (get target-obj "x") tilewidth)
                :y (/ (get target-obj "y") tilewidth)
                :scene (get target "name")}))
      :else this)))

#_(defn sunny?
  "so how's the weather?"
  []
  (cond (and (>= (:clock @core/game-state) 6) (< (:clock @core/game-state) 20)) true
        :else false))

#_(defn photosynth
  [this]
  (cond
    (and (sunny?) (not (.contains (:scene this) "Cave")))
    (merge this {:energy (+ (:energy this)
                            (* 1 (- 11 (count (find-adjacent this)))))})
    :else this))

#_(defn hunger
  [this]
  (cond (> (:energy this) 0)
        (merge this {:energy (- (:energy this) 1)})
        :else this))

#_(defn fireball-collide [this]
  (not
   ((:get-tile-walkable
     (world/name->scene (:scene this))) this)))

#_(defn fireball-collide-players [this]
  (first
   (filter #(and
             (collide this %)
             (not (or (= (:id %) (:owner this)) (= (:id %) (:id this)))))
           (world/scene->pieces (:scene this)))))

#_(defn slime-attack
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

#_(defn fireball-move
  [this]
  (let [speed (:speed this)]
    (cond
      (= "north" (:direction this)) (conj this {:y (- (:y this) speed)})
      (= "south" (:direction this)) (conj this {:y (+ (:y this) speed)})
      (= "east" (:direction this)) (conj this {:x (+ (:x this) speed)})
      :else (conj this {:x (- (:x this) speed)}))))

#_(defn fireball-behavior
  [this]
  (let [collide-player (:id (fireball-collide-players this))]
    (as-> this t
      (cond
        collide-player
        (merge t {:delete-me true
                  :outbox
                  {:mail-to collide-player
                   :method (:method this)
                   :sender (:owner this)}})
        (fireball-collide this)
        (merge t {:delete-me true})
        :else (fireball-move this))
      (teleport t))))

#_(defn implements-identity [this]
  (cond (:identity this) (do (comms/establish-identity this) (dissoc this :identity))
        :else this))

;put this somewhere
(defn thread-debug
  "both print and return the value"
  [x]
  (pp/pprint x)
  x)

#_(defn walk-towards-object
  [this that speed]
  (let [angle (Math/atan2 (- (:x this) (:x that)) (- (:y this) (:y that)))]
    (merge this
           {:x (- (:x this) (* speed (Math/sin angle))) :y (- (:y this) (* speed (Math/cos angle)))})))

#_(defn gene-speed
  "determines the speed a creature should have based
  on speed stats and a hard max, with 1 as a minimum"
  [this]
  (+ 0.1 ;minimum
     (* 0.1 ; increment
        (quot (:speed (:genes this))
              (- (quot (core/setting "gene-max") (:max-speed this)) 1)))))
;;helpers
;;implementation functions, these add prepackaged traits by responding to stats and mail
#_(defn implements-location-updates [this]
  (merge
   this
   (dissoc
    (apply merge (reverse (filter #(= (:method %) "location-update") (:net-inbox this))))
    :mail-to :method :message_type)))

#_(defn implements-blue-fire [this]
  (let [tps (filter #(= (:method %) "teleport-rand") (:inbox this))]
    (cond
      (> (count tps) 0)
      (merge this (world/find-empty-tile (:scene this)) {:force true})
      :else this)))

#_(defn implements-hit
  "Lose health in response to an attack"
  [this]
  (let [hits (filter #(= (:method %) "hit") (:inbox this))]
    (cond
      (> (count hits) 0)
      (merge this {:health (dec (:health this))})
      :else this)))

#_(defn implements-death [this]
  (cond
    (< (:health this) 1) (merge this {:delete-me true})
    :else this))

#_(defn implements-player-death [this]
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
#_(defn plant-reproduce [this]
  (let [energy (/ (:energy this) 3)
        location (world/find-empty-tile (:scene this))]
    (cond
      location
      (merge
       this
       {:energy energy
        :outbox
        (-> this
            (merge {:outbox nil :id nil})
            (merge {:mail-to "new-object"})
            (merge {:energy energy})
            (merge {:x (:x location) :y (:y location)})
            (merge {:genes (normalize-genes (mutate-genes (:genes this)))}))})
      :else
      this)))

#_(defn carrot-hourly
  [this]
  (as-> this t
    (photosynth this)
    (cond (and
           (>= (:repro-chance (:genes t)) (rand-int (core/setting "gene-max")))
           (>= (:energy t) (:repro-threshold (:genes t))))
          (plant-reproduce this)
          :else t)
    (check-starve t)
    (hunger t)
    (teleport t)))

#_(defn carrot-inbox
  [this]
  (-> this
      (implements-hit)
      (implements-blue-fire)
      (implements-death)
      (ignore-inbox)))

#_(defn slime-hunt
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
              (:id (core/rand-nth-safe (core/scene->players (:scene t))))}))
    (cond
      (and (nil? (:hunted t)) (not (nil? (:roost t))))
      (walk-towards-object t (:roost t) (gene-speed t))
      :else
      t)))

#_(defn slime-behavior
  [this]
  (->
   this
   (hourly-behavior)
   (world/method :hunt (list))
   (slime-attack)
   (teleport)))

#_(defn slime-hourly
  [this]
  (as-> this t
    (merge t
           {:energy (- (:energy t) (gene-speed t))
            :roost (world/find-empty-tile (:scene t))})
    (check-starve t)))

#_(defn slime-inbox
  [this]
  (-> this
      (carrot-inbox)))

#_(defn corpse-inbox
  [this] (carrot-inbox this))

#_(defn player-behavior
  [this]
  (-> this
      (implements-identity)
      (implements-player-death)))

#_(defn player-inbox
  [this]
  (-> this
      (implements-location-updates)
      (implements-hit)
      (implements-blue-fire)))
;;object behaviors
;;



(defprotocol game-piece
  (behavior [this]))

(defrecord carrot [id name energy]
 game-piece
 (behavior [this] (println (str name " is my carrot name")) this))

(def a-carrot (->carrot 5 "bob" 23))


(behavior a-carrot)

(swap! game-pieces #(merge % (agent (->carrot 1 "beth" 22))))
(send (first @game-pieces) (fn [this] (behavior this)))
