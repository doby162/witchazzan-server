;;namespace
(ns witchazzan.comms
  (:refer witchazzan.common)
  (:require [org.httpkit.server :as server])
  (:require [clojure.data.json :as json])
  (:gen-class))
(use '[clojure.java.shell :only [sh]])
;;namespace

(defn message-player [data player]
    (try (server/send! (:sock player) (json/write-str data)) (catch Exception e)))

(defn broadcast
    "takes an n-level map and distributes it to all/selected clients as json"
    [data & [players]]
    (run!
     #(message-player data %)
     (cond players players :else (players))))

(defn establish-identity
    "comunicates to a client which player object belongs to them"
    [player]
    (message-player {:messageType "identity" :id (:id player)
                     :name (:name player)} player)
    (broadcast {:messageType "chat" :id -1 :name "Witchazzan.core"
                :content (str "Welcome, " (:name player))}))

#_(defn handle-chat
    "broadcasts chats as json"
    [message channel]
    (let [player (sock->player channel)
          id (get message "targetPlayerId")
          audience (if id [(ffilter #(= (:id %) id) (players))] (players))]
      (broadcast  {:messageType "chat" :name (:name player) :id (:id player)
                   :content (get message "text")} audience)))

#_(defn handle-location-update [message channel]
    (let [player (sock->player channel)]
      (swap!
       network-mail
       #(conj % (merge
                 (apply merge (map (fn [pair] {(keyword (first pair)) (second pair)}) (seq message)))
                 {:method "location-update" :mail-to (:id player)})))))

#_(defn handle-login [message channel]
    (let [username (get message "username") password (get message "password")
          sprite (get message "sprite")
          moving (get message "moving")
          existing-user (filter #(= username (:name %)) (vals (:game-pieces @game-state)))]
      (when (empty? existing-user)
        (swap!
         network-mail
         #(conj %
                {:mail-to "new-object"
                 :x 0 :y 0 :type "player" :scene "LoruleH8"
                 :health 100
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
         network-mail
         #(conj %
                {:mail-to (:id (first existing-user))
                 :method "location-update"
                 :identity "true"
                 :dead nil ;TODO better respawn
                 :health 100
              ;location updates set arbitrary values so we can ride on those coat tails
                 :sock channel :sprite sprite :active true})))))

#_(defn handle-command
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
      (when (re-find #"^reload" (get message "command"))
        (require 'witchazzan.common :reload)
        (require 'witchazzan.comms :reload)
        (require 'witchazzan.world :reload)
        (require 'witchazzan.behavior :reload)
        (message-player {:messageType "chat" :name "Witchazzan.core"
                         :content "Reloading source files"} player))
      (when (re-find #"^git-pull" (get message "command"))
        (message-player {:messageType "chat" :name "Witchazzan.core"
                         :content (:out (sh "git" "pull"))} player))
      (when (re-find #"^reset" (get message "command"))
        (message-player {:messageType "chat" :name "Witchazzan.core"
                         :content "Deleting save."} player)
        (reset))
      (when (re-find #"^save-game" (get message "command"))
        (message-player {:messageType "chat" :name "Witchazzan.core"
                         :content "Saving."} player)
        (save))
      (when (re-find #"^load-game" (get message "command"))
        (message-player {:messageType "chat" :name "Witchazzan.core"
                         :content "Loading."} player)
        (load-game))))

#_(defn handle-fireball
    "generate a fireball object and add it to the object registry"
    [message channel]
    (let [player (sock->player channel) sprite (get message "sprite")]
      (when (not (:dead player)); TODO this whole thing should probably be in a standard update
        (swap!
         network-mail
         #(conj %
                {:mail-to "new-object"
                 :x (:x player) :y (:y player) :type "fireball"
                 :scene (:scene player) ;standard properties
                 :direction (get message "direction")
                 :sprite sprite
                 :behavior "fireball-behavior"
                 :method
                 (cond (= "joosh" (:sprite player)) "teleport-rand"
                       :else "hit")
                 :owner (:id player) ;attributes
                 :speed 0.3
                 :handle-mail "ignore-inbox"})))))
