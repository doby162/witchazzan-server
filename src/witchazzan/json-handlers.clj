;;namespace
(ns witchazzan.core
  (:gen-class))
(declare make-object)
(declare name->scene)
(declare tile-location)
;;namespace

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
(defn handle-chat
  "broadcasts chats as json"
  [player message]
  (broadcast  {:messageType "chat" :name (:name @player) :id (:id @player)
               :content (get message "text")}))

(defn handle-location-update [player message]
  (let [new-x (get message "x") new-y (get message "y")
        new-scene (get message "scene") new-direction (get message "direction")]
    (swap! player #(merge % {:x new-x :y new-y :scene new-scene :direction new-direction}))))

(defn handle-login [player message]
  (let [username (get message "username") password (get message "password")]
    (swap! player #(merge % {:name username}))
    (establish-identity player)))

(defn handle-keyboard-update [player message]
  (swap! player
         #(merge %
                 {:keys
                  (merge
                   (:keys %)
                   {(str ":" (get message "key")) (get message "state")})})))

(defn handle-command
  "this handler is a bit of a switch case inside of a switch case,
  it handles all of the text commands entered
  via the command bar on the client"
  [player message]
  (when (re-find #"^look" (get message "command"))
    (message-player {"response"
                     "You see a rhelm of unimaginable possibility."}
                    player))
  (when (re-find #"^listen" (get message "command"))
    (message-player {"response"
                     "You hear the distant chatter of a keyboard.
                     A developer is hard at work."}
                    player)))

(defn handle-fireball
  "generate a fireball object and add it to the object registry"
  [player message]
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
                              :collide-wall #(not true)}))))
