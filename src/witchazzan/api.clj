(ns witchazzan.api
  (:require [compojure.core :as compojure])
  (:require [compojure.route :as route])
  (:require [crypto.password.bcrypt :as password])
  (:require [ring.middleware.params :as params])
  (:require [ring.middleware.cors :refer [wrap-cors]])
  (:require [ring.middleware.cookies :refer [wrap-cookies]])
  (:require [ring.middleware.json :refer [wrap-json-body]])
  (:require [ring.util.response :refer [response]])
  (:require [next.jdbc :as jdbc])
  (:require [witchazzan.common :refer :all])
  (:require [clojure.data.json :as json])
  (:require [clojure.string :as str])
  (:require [org.httpkit.server :as server])
  (:gen-class))

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
  {:headers {"Content-Type" "application/json; charset=utf-8"}
   :body (json/write-str data)
   :status 200})

(defn nl->br [data]
  (str/replace data "\n" "<br/>"))

(defn sitemap [req]
  {:body "<a href='/api/players/active'> players </a><br/>
  <a href='/api/players/inactive'> inactive players </a><br/>
  <a href='/api/plants'> plants </a><br/>
  <a href='/api/game-pieces'> game pieces </a><br/>
  <a href='/api/settings'> settings </a><br/>
  <a href='/api/log'> log </a><br/>
  <a href='/api/scenes'> scenes </a><br/>
  <a href='/api/scenes/active'> active scenes </a><br/>
  <a href='/api/scenes/inactive'> inactive scenes </a><br/>
  <a href='/api/graph'> gene statistics for repro-threshold </a><br/>
  <a href='/api/auth'> authenticate </a><br/>
  <a href='/api/me'> account information </a><br/>
  <a href='/api/log-out'> log-out </a><br/>
  <a href='/api/sign-up'> make an account </a><br/>
  <a href='/api/quit'> kill server </a><br/>"})

(use 'ring.middleware.session
     'ring.util.response)

(defn authenticate-post
  [{params :params session :session}]
  (let [name (get params "name")
        password (get params "password")
        user (jdbc/execute-one! ds ["select * from users where username= ?" name])]
    (if (and user (password/check password (:users/password user)))
      (let [token (str (java.util.UUID/randomUUID))]
        (jdbc/execute-one! ds ["update users set token = ? where id = ?;" token (:users/id user)])
        (-> (response "<a href='/api'> Auth succesful</a>")
            (update-in [:cookies] #(merge % {"token" {:value token :max-age 2592000}}))
            (assoc :session (assoc session :auth (:users/id user) :admin (:users/admin user)))))
      "try again")))

(defn token-auth
  [handler]
  (fn [request]
    (let [token (:value (get (:cookies request) "token"))
          user (and (nil? (:auth (:session request))) token (jdbc/execute-one! ds ["select * from users where token = ?;" token]))]
      (if user
        (-> (handler (update-in request [:session] #(merge % {:auth (:users/id user) :admin (:users/admin user)})))
            (update-in [:session] #(merge % {:auth (:users/id user) :admin (:users/admin user)})))
        (handler request)))))

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
      (-> (response "Access Denied")
          (status 401)))))

(defn admin? [handler]
  (fn [request]
    (if (:admin (:session request))
      (handler request)
      (-> (response "Access Denied")
          (status 401)))))

(defn wrap-universal-headers [handler]
  (fn [request]
    (-> (handler request)
        (header "Access-Control-Allow-Credentials" "true"))))

(defn api-me [request]
  (let [data (jdbc/execute-one! ds ["select username, admin from users where id = ? ;" (:auth (:session request))])]
    (json-output
     {:username (:users/username data) :admin (:users/admin data)})))

(defn api-log-out [request]
  (jdbc/execute-one! ds ["update users set token = null where id = ?" (:auth (:session request))])
  (-> (response "<a href='/api'> de-auth succesful</a>") (assoc :session {})))

(defn player-data-get [request]
  (let [existing-row (jdbc/execute-one! ds ["select * from clientData where userId = ? and `key` = ?" (:auth (:session request)) (:key (:body request))])]
    (json-output existing-row)))

(defn player-data-post [request]
  (let [existing-row (jdbc/execute-one! ds ["select * from clientData where userId = ? and `key` = ?" (:auth (:session request)) (:key (:body request))])]
    (if existing-row
      (do
        (jdbc/execute-one! ds ["update clientData set value = ? where id = ?" (:value (:body request)) (:clientData/id existing-row)])
        (json-output {:action "updated"}))
      (do
        (jdbc/execute-one! ds ["insert into clientData (userId, `key`, value) values(?, ?, ?)" (:auth (:session request)) (:key (:body request)) (:value (:body request))])
        (json-output {:action "row added"})))))

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
              (str "witchazzan.comms/handle-" message-type) [(dissoc message "message_type") channel request])
             (catch java.lang.NullPointerException e (log e)))
                                        ;here we are interpreting the "messasge_type" json value as
                                        ;the second half of a function name and calling that function
           )(catch java.lang.Exception e
              (log (str "invalid json: " data)) (log e)))))))

(compojure/defroutes all-routes
  (wrap-session
   (params/wrap-params
    (wrap-universal-headers
     (wrap-cookies
      (token-auth
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
           (wrap-json-body ; this marks the json-in-json-out portion of tha api
            (compojure/routes
             (compojure/GET "/api/player-data" [] player-data-get)
             (compojure/POST "/api/player-data" [] player-data-post))
            {:keywords? true})
           (compojure/GET "/api/players" []
             (json-output (map (fn [%] (dissoc (into {} @%) :socket :vector)) (active-pieces {:type :player}))))
           (compojure/GET "/api/players/inactive" []
             (json-output (map (fn [%] (dissoc (into {} @%) :socket :vector)) (game-pieces {:type :player :active false}))))
           (compojure/GET "/api/players/active" []
             (json-output (map (fn [%] (dissoc (into {} @%) :socket :vector)) (active-pieces {:type :player}))))
           (compojure/GET "/api/plants" []
             (json-output (map (fn [%] (dissoc (into {} @%) :socket :vector)) (active-pieces {:type :carrot}))))
           (compojure/GET "/api/game-pieces" []
             (json-output (map (fn [%] (dissoc (into {} @%) :socket :vector)) (active-pieces))))
           (compojure/GET "/api/graph" []
             (nl->br (with-out-str (analyze-gene "repro-threshold" (active-pieces {:type :carrot})))))
           (compojure/GET "/api/log" [] (nl->br (slurp "config/log")))
           (compojure/GET "/api/scenes" []
             (json-output (map #(dissoc (merge % {:active (boolean (scene-active (:name %)))}) :get-tile-walkable) tilemaps)))
           (compojure/GET "/api/scenes/:param" [param]
             (json-output
              (filter
               #(or (and (= param "active") (scene-active (:name %))) (and (= param "inactive") (not (scene-active (:name %)))))
               (map #(dissoc (merge % {:active (boolean (scene-active (:name %)))}) :get-tile-walkable) tilemaps))))
           (compojure/GET "/api/me" [] api-me)
           (compojure/GET "/api/log-out" [] api-log-out)
           (admin?
            (compojure/routes
             (compojure/GET "/api/quit" [] kill-api)
             (compojure/GET "/api/settings" [] (json-output @settings))))))
         (route/not-found sitemap))
        :access-control-allow-origin #".*" :access-control-allow-methods [:get :put :post :delete :options])))))))
