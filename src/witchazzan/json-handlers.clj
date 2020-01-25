;;namespace
(ns witchazzan.core
  (:gen-class))
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
  (let [player (sock->player channel)]
    (swap!
     network-mail
     #(conj % (merge
               (apply merge (map (fn [pair] {(keyword (first pair)) (second pair)}) (seq message)))
               {:method "location-update" :mail-to (:id player)})))))

(defn handle-login [message channel]
  (let [username (get message "username") password (get message "password")
        sprite (get message "sprite")
        moving (get message "moving")
        existing-user (filter #(= username (:name %)) (vals (:game-pieces @game-state)))]
    (when (empty? existing-user)
      (add-game-piece! {:x 0 :y 0 :type "player" :scene "LoruleH8"
                        :health 3
                        :animation nil
                        :active true
                        :defence 0 :sprite sprite
                        :moving false
                        :behavior "witchazzan.core/player-behavior"
                        :handle-mail "witchazzan.core/player-inbox"
                        :name username :sock channel :keys {}}))
    (when (not (empty? existing-user))
      (update-game-piece! (:id (first existing-user))
                          {:sock channel :sprite sprite :active true}))
    (establish-identity (sock->player channel))))

; this isn't currently how the client comunicates
; (defn handle-keyboard-update [message channel]
;   (let [player (sock->player channel)]
;     (update-game-piece!
;      (:id player)
;      {:keys (merge (:keys player) {(str ":" (get message "key")) (get message "state")})})))

(defn handle-command
  "this handler is a bit of a switch case inside of a switch case,
  it handles all of the text commands entered
  via the command bar on the client"
  [message channel]
  (let [player (sock->player channel)]
    (when (re-find #"^look" (get message "command"))
      (message-player {:messageType "chat" :name "Witchazzan.core"
                       :content
                       "You see a realm of unimaginable possibility."}
                      player))
    (when (re-find #"^listen" (get message "command"))
      (message-player {:messageType "chat" :name "Witchazzan.core"
                       :content
                       "You hear the distant chatter of a keyboard.
                     A developer is hard at work."}
                      player))
    (when (re-find #"^who" (get message "command"))
      (message-player {:messageType "chat" :name "Witchazzan.core"
                       :content
                       (apply str (map #(str (:name %) " ") (players)))}
                      player))
    (when (re-find #"^debug-teleport" (get message "command"))
      (message-player {:messageType "highlight_pixels"
                       :content
                       (check-px-teleport (:scene player))}
                      player))))

(defn handle-fireball
  "generate a fireball object and add it to the object registry"
  [message channel]
  (let [player (sock->player channel) sprite (get message "sprite")]
    (add-game-piece!
     {:x (:x player) :y (:y player) :type "fireball"
      :scene (:scene player) ;standard properties
      :direction (get message "direction")
      :sprite sprite
      :behavior "witchazzan.core/fireball-behavior"
      :owner (:id player) ;attributes
      :collide "witchazzan.core/fireball-collide"
      :move "witchazzan.core/fireball-move"
      :speed 15 ; 15 is max speed for 16 px tiles w/tile collision
      :handle-mail "witchazzan.core/ignore-inbox"
      :collide-players "witchazzan.core/fireball-collide-players"})))
