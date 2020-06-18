(ns witchazzan.core
  (:require witchazzan.common) ;global state and access
  (:require witchazzan.behavior) ;definition of game pieces
  (:require witchazzan.comms) ; json messaging between server and client
  (:require witchazzan.world) ; main loop and general things that happen, top level
  (:require [clojure.pprint :as pp])
  (:gen-class))

;repl boilerplate
(refer 'witchazzan.common)
(refer 'witchazzan.behavior)
(refer 'witchazzan.comms)
(refer 'witchazzan.world)

(defn -main []
  (init)
  (main))
