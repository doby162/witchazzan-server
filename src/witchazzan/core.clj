(ns witchazzan.core
  (:require witchazzan.common)
  (:require witchazzan.comms)
  (:require witchazzan.world)
  (:require witchazzan.behavior)
  (:require [clojure.pprint :as pp])
  (:gen-class))

;repl boilerplate
(refer 'witchazzan.common)
(refer 'witchazzan.comms)
(refer 'witchazzan.world)
(refer 'witchazzan.behavior)

(defn -main []
  (init)
  (main))
(-main)
