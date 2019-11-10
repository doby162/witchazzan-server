;;namespace
;this update: 
;establish identity used to create/load a new player instead of
;on socket connect
;alter the api of the handlers to accomodate ththis change
(ns witchazzan.core
  (:gen-class))
(declare make-object)
(declare name->scene)
(declare tile-location)
;;namespace

(defn sock->player [sock]
  (first (filter #(= (:sock @%) sock) players)))

(defn message-player [data player]
  (server/send! (:sock @player) (json/write-str data)))

(defn broadcast
  "takes an n-level map and distributes it to all clients as json"
  [data]
  (dorun (map #(message-player data %) players)))

(defn establish-identity
  "comunicates to a client which player object belongs to them"
  [player]
  (message-player {:messageType "identity" :id (:id @player)
                   :name (:name @player)} player))
(defn handle-chat [a b])
#_(defn handle-chat
    "broadcasts chats as json"
    [message channel]
    (let [player (sock->player channel)]
      (broadcast  {:messageType "chat" :name (:name @player) :id (:id @player)
                   :content (get message "text")})))

(defn handle-location-update [message channel])
#_(defn handle-location-update [message channel]
    #_(let [new-x (get message "x") new-y (get message "y")
            new-scene (get message "scene") new-direction (get message "direction")
            player (sock->player channel)]
        (swap! player #(merge % {:x new-x :y new-y :scene new-scene :direction new-direction}))))

(defn handle-login [message channel]
  (let [username (get message "username") password (get message "password")
        existing-user (filter #(= username (:name %)) (:game-pieces @game-state))]
    (when (empty? existing-user) (add-game-piece {:x 0 :y 0 :type "player" :scene "openingScene" :behavior #()
                                                  :name username :sock channel :keys {}}))
    #_(when (not (empty? existing-user)))
    #_(establish-identity (sock->player channel))))

(defn handle-keyboard-update [message channel])
#_(defn handle-keyboard-update [message channel]
    (let [player (sock->player channel)]
      (swap! player
             #(merge %
                     {:keys
                      (merge
                       (:keys %)
                       {(str ":" (get message "key")) (get message "state")})}))))

(defn handle-command [a b])
#_(defn handle-command
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

(defn handle-fireball [a b])
#_(defn handle-fireball
    "generate a fireball object and add it to the object registry"
    [message channel]
    (let [player (sock->player channel)]
      (def objects (conj
                    objects
                    (make-object (:x @player) (:y @player) "fireball" (:scene @player) ;standard properties
                                 (fn [this]
                                   (cond
                                     ((:collide (:attributes this)) this) nil
                                     :else ((:move (:attributes this)) this)))
                                 {:owner player ;attributes
                                  :collide (fn [this]
                                             (not
                                              ((:get-tile-walkable
                                                (name->scene (:scene this))) (tile-location this))))
                                  :move (cond
                                          (= "north" (get message "direction")) #(conj % {:y (dec (:y %))})
                                          (= "south" (get message "direction")) #(conj % {:y (inc (:y %))})
                                          (= "east" (get message "direction")) #(conj % {:x (inc (:x %))})
                                          :else #(conj % {:x (dec (:x %))}))
                                  :collide-wall #(not true)})))))
