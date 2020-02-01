(ns witchazzan.core
  (:gen-class))

(load-file "config/default-config.clj")
(try (load-file "config/config.clj")
     (catch Exception e
       (println "No custom configuration found at config/config.clj.
                Add settings like (setting \"port\" 1234)")))
(load-file "src/witchazzan/world.clj")
(load-file "src/witchazzan/behavior.clj")
(load-file "src/witchazzan/json-handlers.clj")

(-main)
