;;namespace
(ns witchazzan.comms
  (:require [witchazzan.core :as core])
  (:require [org.httpkit.server :as server])
  (:require [clojure.data.json :as json])
  (:gen-class))
;;namespace

(defn message-player [data player]
  (try (server/send! (:sock player) (json/write-str data)) (catch Exception e)))

(defn broadcast
  "takes an n-level map and distributes it to all/selected clients as json"
  [data & [players]]
  (run!
   #(message-player data %)
   (cond players players :else (core/players))))

(defn establish-identity
  "comunicates to a client which player object belongs to them"
  [player]
  (message-player {:messageType "identity" :id (:id player)
                   :name (:name player)} player)
  (broadcast {:messageType "chat" :id -1 :name "Witchazzan.core"
              :content (str "Welcome, " (:name player))}))

(defn handle-chat
  "broadcasts chats as json"
  [message channel]
  (let [player (core/sock->player channel)]
    (broadcast  {:messageType "chat" :name (:name player) :id (:id player)
                 :content (get message "text")})))

(defn handle-location-update [message channel]
  (let [player (core/sock->player channel)]
    (swap!
     core/network-mail
     #(conj % (merge
               (apply merge (map (fn [pair] {(keyword (first pair)) (second pair)}) (seq message)))
               {:method "location-update" :mail-to (:id player)})))))

(defn handle-login [message channel]
  (let [username (get message "username") password (get message "password")
        sprite (get message "sprite")
        moving (get message "moving")
        existing-user (filter #(= username (:name %)) (vals (:game-pieces @core/game-state)))]
    (when (empty? existing-user)
      (swap!
       core/network-mail
       #(conj %
              {:mail-to "new-object"
               :x 0 :y 0 :type "player" :scene "LoruleH8"
               :health 3
               :animation nil
               :active true
               :defence 0 :sprite sprite
               :moving false
               :identity "true"
               :behavior "player-behavior"
               :handle-mail "player-inbox"
               :name username :sock channel})))
    (when (not (empty? existing-user))
      (swap!
       core/network-mail
       #(conj %
              {:mail-to (:id (first existing-user))
               :method "location-update"
               :identity "true"
               :dead nil ;TODO better respawn
               :health 3
              ;location updates set arbitrary values so we can ride on those coat tails
               :sock channel :sprite sprite :active true})))))

(defn handle-command
  "this handler is a bit of a switch case inside of a switch case,
  it handles all of the text commands entered
  via the command bar on the client"
  [message channel]
  (let [player (core/sock->player channel)]
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
                       (apply str (map #(str (:name %) " ") (core/players)))}
                      player))
    #_(when (re-find #"^debug-teleport" (get message "command")) ;TODO make this work
        (message-player {:messageType "highlight_pixels"
                         :content
                         (check-px-teleport (:scene player))}
                        player))))

(defn handle-fireball
  "generate a fireball object and add it to the object registry"
  [message channel]
  (let [player (core/sock->player channel) sprite (get message "sprite")]
    (when (not (:dead player)); TODO this whole thing should probably be in a standard update
      (swap!
       core/network-mail
       #(conj %
              {:mail-to "new-object"
               :x (:x player) :y (:y player) :type "fireball"
               :scene (:scene player) ;standard properties
               :direction (get message "direction")
               :sprite sprite
               :behavior
               (cond (= "joosh" (:sprite player)) "fireball-blue-behavior"
                     :else "fireball-behavior")
               :owner (:id player) ;attributes
               :collide "fireball-collide"
               :move "fireball-move"
               :speed 15 ; 15 is max speed for 16 px tiles w/tile collision
               :handle-mail "ignore-inbox"
               :collide-players "fireball-collide-players"})))))
