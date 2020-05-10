;;namespace
(ns witchazzan.comms
  (:require [witchazzan.common :refer :all])
  (:require [witchazzan.behavior :as behavior])
  (:require [org.httpkit.server :as server])
  (:require [clojure.data.json :as json])
  (:gen-class))
(use '[clojure.java.shell :only [sh]])
;;namespace

(defn message-player [data player]
  (try (server/send! (:socket player) (json/write-str data)) (catch Exception e)))

(defn broadcast
  "takes an n-level map and distributes it to all/selected clients as json"
  [data & [players]]
  (run!
   #(message-player data @%)
   (cond players players :else (game-pieces "type" "player"))))

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
  (let [player @(first (game-pieces :socket channel))
        id (get message "targetPlayerId")
        audience (if id [(game-pieces id)] (game-pieces :type "player"))]
    (broadcast  {:messageType "chat" :name (:name player) :id (:id player)
                 :content (get message "text")} audience)))

(defn handle-location-update [message channel]
  (let [player (first (game-pieces "socket" channel))]
    (send
     player
     merge
     (apply merge (map (fn [pair] {(keyword (first pair)) (second pair)}) (seq message))))))

(defn handle-login [message channel]
  (let [username (get message "username") password (get message "password")
        id (gen-id)
        sprite (get message "sprite")
        moving (get message "moving")
        existing-user (first (game-pieces "name" username))]
;      (when (empty? existing-user)
    (behavior/add-game-piece!
     (behavior/map->player
      {:id id
       :x 0
       :y 0
       :type "player"
       :scene "LoruleH8"
       :health 100
       :active true
       :defence 0
       :sprite sprite
       :identity "true"
       :name username
       :socket channel
       :milliseconds (System/currentTimeMillis)}))
    (establish-identity @(game-pieces id)))
  #_(when (not (empty? existing-user))
      (swap!
       network-mail
       #(conj %
              {:mail-to (:id (first existing-user))
               :method "location-update"
               :identity "true"
               :dead nil ;TODO better respawn
               :health 100
              ;location updates set arbitrary values so we can ride on those coat tails
               :socket channel :sprite sprite :active true}))))
;)

(defn handle-command
  "this handler is a bit of a switch case inside of a switch case,
  it handles all of the text commands entered
  via the command bar on the client"
  [message channel]
  (let [player @(first (game-pieces "socket" channel))]
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
                       (apply str (map #(str (:name @%) ", ") (game-pieces :type "player")))}
                      player))
    (when (re-find #"^reload" (get message "command"))
      (require 'witchazzan.common :reload)
      (require 'witchazzan.comms :reload)
      (require 'witchazzan.behavior :reload)
      (require 'witchazzan.world :reload)
      (message-player {:messageType "chat" :name "Witchazzan.core"
                       :content "Reloading source files"} player))
    (when (re-find #"^git-pull" (get message "command"))
      (message-player {:messageType "chat" :name "Witchazzan.core"
                       :content (:out (sh "git" "pull"))} player))
    #_(when (re-find #"^reset" (get message "command"))
        (message-player {:messageType "chat" :name "Witchazzan.core"
                         :content "Deleting save."} player)
        (reset))
    #_(when (re-find #"^save-game" (get message "command"))
        (message-player {:messageType "chat" :name "Witchazzan.core"
                         :content "Saving."} player)
        (save))
    #_(when (re-find #"^load-game" (get message "command"))
        (message-player {:messageType "chat" :name "Witchazzan.core"
                         :content "Loading."} player)
        (load-game))))
