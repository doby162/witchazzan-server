;;namespace
(ns witchazzan.world
  (:require [witchazzan.common :refer :all])
  (:require [witchazzan.comms :as comms])
  (:require [witchazzan.behavior :as behavior])
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :as server])
  (:require [clojure.string :as str])
  (:require [compojure.core :as compojure])
  (:require [compojure.route :as route])
  (:require [crypto.password.bcrypt :as password])
  (:require [ring.middleware.params :as params])
  (:require [ring.middleware.cors :refer [wrap-cors]])
  (:require [next.jdbc :as jdbc])
  (:gen-class))
;;namespace
;;websocket infrastructure
(defn call-func-by-string
  "(call-func-by-string \"+\" [5 5]) => 10"
  [name args]
  (try
    (apply (resolve (symbol name)) args)
    (catch NullPointerException e
      (log (str "call-func-by-string failed: " name args " " e)))))

(defn socket-handler [request]
  (log "A player has entered Witchazzan!")
  (server/with-channel request channel
    (server/on-close
     channel
     (fn [data]
       ;logout
       (when (seq (game-pieces {:socket channel}))
         (send (first (game-pieces {:socket channel})) #(merge % {:active false})))))
    (server/on-receive
     channel
     (fn [data]
       (try ; checking for bad json and that a handler can be found
         (let [message (json/read-str data)
               message-type (get message "message_type")]
           (try ;checking if the function exists
             (call-func-by-string
              (str "witchazzan.comms/handle-" message-type) [(dissoc message "message_type") channel])
             (catch java.lang.NullPointerException e (log e)))
                                        ;here we are interpreting the "messasge_type" json value as
                                        ;the second half of a function name and calling that function
           )(catch java.lang.Exception e
              (log (str "invalid json: " data)) (log e)))))))

;;analysis functions
(defn ascii-graph
  [dataset]
  (let [increment 10 min 5 max (setting "gene-max")]
    (loop [i min]
      (let [num
            (get (frequencies (map #(within-n % i min) dataset)) true 0)]
        (println (- i min) "-" (- (+ increment i) min) ":" (apply str (repeatedly num #(str "#")))))
      (when (< i max) (recur (+ i 10))))))

(defn analyze-gene
  [gene population]
  (let [dataset (sort (filter #(not (nil? %)) (map #((keyword gene) (:genes @%)) population)))]
    (print "Sample size: ")
    (prn (count dataset))
    (print "Mode: ")
    (prn (first (last (sort-by second (frequencies dataset)))))
    (print "Mean: ")
    (prn (int (/ (apply + dataset) (count dataset))))
    (print "Median: ")
    (prn (nth dataset (/ (count dataset) 2)))
    (println "frequencies")
    (ascii-graph dataset)
    (print "Full dataset: ")
    (prn dataset)))
;;analysis functions

(defn json-output [data]
  {:headers {"Content-Type" "application/json; charset=utf-8" "Access-Control-Allow-Credentials" "true"}
   :body (json/write-str data)
   :status 200})

(defn nl->br [data]
  (str/replace data "\n" "<br/>"))

(defn sitemap [req]
  {:body "<a href='/api/players'> players </a><br/>
  <a href='/api/plants'> plants </a><br/>
  <a href='/api/game-pieces'> game pieces </a><br/>
  <a href='/api/settings'> settings </a><br/>
  <a href='/api/log'> log </a><br/>
  <a href='/api/scenes'> scenes </a><br/>
  <a href='/api/scenes/active'> active scenes </a><br/>
  <a href='/api/scenes/inactive'> inactive scenes </a><br/>
  <a href='/api/graph'> gene statistics for repro-threshold </a><br/>
  <a href='/api/auth'> authenticate </a><br/>
  <a href='/api/sign-up'> make an account </a><br/>
  <a href='/api/quit'> kill server </a><br/>"
   :headers {"Access-Control-Allow-Credentials" "true"}})

(use 'ring.middleware.session
     'ring.util.response)

(defn authenticate-post
  [{params :params session :session}]
  (let [name (get params "name")
        password (get params "password")
        user (jdbc/execute-one! ds ["select * from users where username= ?" name])]
    (if (and user (password/check password (:users/password user)))
      (-> (response "<a href='/api'> Auth succesful</a>")
          (assoc :session (assoc session :auth (:users/id user) :admin (:users/admin user))))
      "try again")))

(defn create-account
  [{params :form-params}]
  (let [name (get params "name")
        password (get params "password")
        user (jdbc/execute-one! ds ["select * from users where username= ?" name])]
    (if user
      "That username seems to be in use already"
      (do (jdbc/execute! ds ["insert into users (username, password, admin)
                         values(?, ?, 0)" name (password/encrypt password)])
          "Success"))))

(defn kill-api [request]
  (do
    (future (do (Thread/sleep 1000) (System/exit 0)))
    "Killing server..."))

(defn auth? [handler]
  (fn [request]
    (if (:auth (:session request))
      (handler request)
      (-> (response "Access Denied" )
          (status 401)
          (header "Access-Control-Allow-Credentials" "true")))))

(defn admin? [handler]
  (fn [request]
    (if (:admin (:session request))
      (handler request)
      (-> (response "Access Denied")
          (status 401)
          (header "Access-Control-Allow-Credentials" "true")))))

(compojure/defroutes all-routes
  (wrap-session
   (params/wrap-params
    (wrap-cors
     (compojure/routes
      (compojure/GET "/api" [] sitemap)
      (compojure/GET "/api/" [] sitemap)
      (compojure/GET "/api/auth" [] "<form method='post'> <input placeholder='username' type='text' name='name'> <input placeholder='password' type='password' name='password'><input type='submit'></form>")
      (compojure/POST "/api/sign-up" [] create-account)
      (compojure/POST "/api/auth" [] authenticate-post)
      (compojure/GET "/api/sign-up" [] "<form method='post'><input placeholder='username' type='text' name='name'><input placeholder='password' type='password' name='password'><input type='submit'></form>")
      (auth?
       (compojure/routes
        (compojure/GET "/" [] socket-handler) ; websocket connection
        (compojure/GET "/api/players" []
          (json-output (map (fn [%] (dissoc (into {} @%) :socket)) (active-pieces {:type :player}))))
        (compojure/GET "/api/plants" []
          (json-output (map (fn [%] (dissoc (into {} @%) :socket)) (active-pieces {:type :carrot}))))
        (compojure/GET "/api/game-pieces" []
          (json-output (map (fn [%] (dissoc (into {} @%) :socket)) (active-pieces))))
        (compojure/GET "/api/graph" []
          (nl->br (with-out-str (analyze-gene "repro-threshold" (active-pieces {:type :carrot})))))
        (admin?
         (compojure/routes
          (compojure/GET "/api/quit" [] kill-api)
          (compojure/GET "/api/settings" [] (json-output @settings))))
        (compojure/GET "/api/log" [] (nl->br (slurp "config/log")))
        (compojure/GET "/api/scenes" []
          (json-output (map #(dissoc (merge % {:active (boolean (scene-active (:name %)))}) :get-tile-walkable) tilemaps)))
        (compojure/GET "/api/scenes/:param" [param]
          (json-output
           (filter
            #(or (and (= param "active") (scene-active (:name %))) (and (= param "inactive") (not (scene-active (:name %)))))
            (map #(dissoc (merge % {:active (boolean (scene-active (:name %)))}) :get-tile-walkable) tilemaps))))))
      (route/not-found sitemap))
     :access-control-allow-origin #".*" :access-control-allow-methods [:get :put :post :delete :options]))))
;;websocket infrastructure
;;
;;game loop
(defn update-clients [scene]
  (comms/broadcast
   {:messageType "game-piece-list"
    :pieces (map (fn [%] (dissoc (into {} @%) :socket))
                 (active-pieces {:scene scene}))}
   (active-pieces {:type :player :scene scene})))
;;game loop
;;admin stuff
(defn log-and-clear-agents
  []
  (run!
   (fn [agent]
     (log (with-out-str (println (agent-error agent) " " @agent)))
     (behavior/delete @agent))
   (filter #(agent-error %) (game-pieces))))

(defn ten-x []
  (setting "millis-per-hour" (/ (setting "millis-per-hour") 10))
  (setting "millis-per-frame" (/ (setting "millis-per-frame") 10)))

(defn tenth-x []
  (setting "millis-per-hour" (* (setting "millis-per-hour") 10))
  (setting "millis-per-frame" (* (setting "millis-per-frame") 10)))

(defn short-day []
  (setting "millis-per-hour" 600))

(defn seed-nature []
  (run!
   (fn [scene] (behavior/spawn-carrot (merge {:scene (:name scene)} (find-empty-tile (:name scene)))))
   tilemaps))
;;admin stuff
;;loooop
(defn spawn-points
  "assumes spawn-type is both a function and a valid object name, upgrade this to take a list later"
  [type]
  (let [coord-pairs
        (filter #(:x %) ;check if valid coords were returned
                (map (fn [tilemap] ; assume one spawn of type per scene because it's easy
                       (let [properties
                             (ffilter
                              #(= (str "spawn-" type) (get % "name"))
                              (:objects tilemap))]
                         (when properties
                           {:scene (:name tilemap)
                            :x (/ (get properties "x") (:tilewidth tilemap))
                            :y (/ (get properties "y") (:tilewidth tilemap))})))
                     tilemaps))]
    (run!
     #(call-func-by-string (str "witchazzan.behavior/spawn-" type) %)
     coord-pairs)))

(defn coordinate-spawns []
  #_(when (empty? (active-pieces {:type :slime}))
      (spawn-points "slime"))
  (when (empty? (active-pieces {:type :carrot}))
    (spawn-points "carrot")))

(defn keep-time! []
  (let [old-state @game-state
        new-hour (int (mod (/ (- (System/currentTimeMillis) (:start-time @game-state))
                              (setting "millis-per-hour")) (setting "hours-per-day")))
        new-day (int (/ (/ (- (System/currentTimeMillis) (:start-time @game-state))
                           (setting "millis-per-hour")) (setting "hours-per-day")))]
    (swap! game-state merge {:last-updated (System/currentTimeMillis)})
    (when (not (= new-hour (:hour old-state)))
      (swap! game-state update-in [:hour]
             (fn [_] new-hour))
      (swap! game-state update-in [:day]
             (fn [_] new-day))
      (when (= new-hour (setting "dawn"))
        (comms/broadcast
         {:messageType "chat" :name "Witchazzan.core" :id -1
          :content (str "Dawn of day " new-day)}))
      (when (= new-hour (setting "sunset"))
        (comms/broadcast
         {:messageType "chat" :name "Witchazzan.core" :id -1
          :content (str "Night Falls")}))
      (coordinate-spawns))))

(defn game-loop [scene]
  (log scene)
  (loop []
    (try
      (run!
       (fn [game-piece]
         (send game-piece behavior/behavior))
       (active-pieces {:scene scene}))
      (Thread/sleep (setting "min-millis-per-frame"))
      (apply await (active-pieces {:scene scene}))
      (if (scene-active scene)
        (update-clients scene)
        (Thread/sleep (setting "idle-millis-per-frame")))
      (catch Exception e
        (log (str "error in " scene))
        (log e)
        (log-and-clear-agents)))
    (when (not (setting "pause")) (recur))))

(defn threadify [func] (future (func)))

(defn launch-threads []; this function will also advance all scenes by one tic when the game is paused
  (threadify
   #(loop [] (keep-time!) (Thread/sleep (setting "idle-millis-per-frame"))
          (when (not (setting "pause")) (recur))))
  (run! #(threadify (fn [] (game-loop %))) (map #(:name %) tilemaps)))

(defn pause
  "stops the game, but not the flow of time. Use justify-time before unpausing"
  []
  (swap! settings merge {:pause true}))

(defn unpause []
  (swap! settings merge {:pause false})
  (launch-threads))

(defn justify-time
  "after the simulation has been paused, run this to catch the clock up with reality"
  []
  (run! #(send % merge {:milliseconds (System/currentTimeMillis)}) (game-pieces))
  (swap!
   game-state
   #(merge % {:start-time (+ (- (System/currentTimeMillis) (:last-updated %)) (:start-time %))})))

(defn main
  [& _]
  (log "Booting...")
  (server/run-server all-routes {:port (setting "port")})
  (log (str "Running server on port " (setting "port")))
  (when (not (setting "pause"))
    (log "Not paused, running game")
    (launch-threads))
  (seed-nature))
