(ns witchazzan.core
  (:require witchazzan.common)
  (:require witchazzan.comms)
  (:require witchazzan.behavior)
  (:require witchazzan.world)
  (:require [clojure.pprint :as pp])
  (:gen-class))

;repl boilerplate
(refer 'witchazzan.common)
(refer 'witchazzan.comms)
(refer 'witchazzan.behavior)
(refer 'witchazzan.world)

(defn -main []
  (init)
  (main))
(-main)
