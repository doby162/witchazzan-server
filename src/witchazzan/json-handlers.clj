;;namespace
(ns witchazzan.core
  (:gen-class))
(declare name->scene)
(declare tile-location)
;;namespace

(defn sock->player [sock]
  (first (filter #(= (:sock %) sock) (vals (:game-pieces @game-state)))))

(defn message-player [data player]
  (try (server/send! (:sock player) (json/write-str data)) (catch Exception e)))

(defn broadcast
  "takes an n-level map and distributes it to all clients as json"
  [data players]
  (run! #(message-player data %) players))

(defn establish-identity
  "comunicates to a client which player object belongs to them"
  [player]
  (message-player {:messageType "identity" :id (:id player)
                   :name (:name player)} player))
(defn handle-chat
  "broadcasts chats as json"
  [message channel]
  (let [player (sock->player channel)]
    (broadcast  {:messageType "chat" :name (:name player) :id (:id player)
                 :content (get message "text")} (players))))

(defn handle-location-update [message channel]
  (let [new-x (get message "x") new-y (get message "y")
        sprite (get message "sprite")
        new-scene (get message "scene") new-direction (get message "direction")
        player (sock->player channel)]
    (update-game-piece (:id player) {:x new-x :y new-y :scene new-scene :direction new-direction
                                     :sprite sprite})))

(defn handle-login [message channel]
  (let [username (get message "username") password (get message "password")
        sprite (get message "sprite")
        existing-user (filter #(= username (:name %)) (vals (:game-pieces @game-state)))]
    (when (empty? existing-user) (add-game-piece {:x 0 :y 0 :type "player" :scene "openingScene"
                                                  :health 3
                                                  :defence 0 :sprite sprite
                                                  :behavior (fn [this] this)
                                                  :hit (fn
                                                         [this strength]
                                                         (update-game-piece
                                                          (:id this)
                                                          {:health
                                                           (- (:health this) (- strength (:defence this)))}))
                                                  :name username :sock channel :keys {}}))
    (when (not (empty? existing-user)) (update-game-piece (:id (first existing-user))
                                                          {:sock channel :sprite sprite}))
    (establish-identity (sock->player channel))))

(defn handle-keyboard-update [message channel]
  (let [player (sock->player channel)]
    (update-game-piece
     (:id player)
     {:keys (merge (:keys player) {(str ":" (get message "key")) (get message "state")})})))

(defn handle-command
  "this handler is a bit of a switch case inside of a switch case,
  it handles all of the text commands entered
  via the command bar on the client"
  [message channel]
  (let [player (sock->player channel)]
    (when (re-find #"^look" (get message "command"))
      (message-player {"response"
                       "You see a rhelm of unimaginable possibility."}
                      player))
    (when (re-find #"^listen" (get message "command"))
      (message-player {"response"
                       "You hear the distant chatter of a keyboard.
                     A developer is hard at work."}
                      player))))

(defn handle-fireball
  "generate a fireball object and add it to the object registry"
  [message channel]
  (let [player (sock->player channel) sprite (get message "sprite")]
    (add-game-piece {:x (:x player) :y (:y player) :type "fireball"
                     :scene (:scene player) ;standard properties
                     :direction (:direction player)
                     :sprite sprite
                     :behavior (fn [this]
                                 (cond
                                   ((:collide-players this) this)
                                   (do
                                     ((:hit ((:collide-players this) this)) ((:collide-players this) this) 1)
                                     (merge this {:delete-me true}))
                                   ((:collide this) this) (merge this {:delete-me true})
                                   :else ((:move this) this)))
                     :owner player ;attributes
                     :collide (fn [this]
                                (not
                                 ((:get-tile-walkable
                                   (name->scene (:scene this))) (tile-location this))))
                     :move (cond
                             (= "north" (get message "direction")) #(conj % {:y (dec (:y %))})
                             (= "south" (get message "direction")) #(conj % {:y (inc (:y %))})
                             (= "east" (get message "direction")) #(conj % {:x (inc (:x %))})
                             :else #(conj % {:x (dec (:x %))}))
                     :collide-players
                     (fn [this]
                       (first
                        (filter #(and
                                  (=
                                   (tile-location %)
                                   (tile-location this))
                                  (not (= (:id %) (:id (:owner this)))))
                                (scene->players (:scene this)))))})))
