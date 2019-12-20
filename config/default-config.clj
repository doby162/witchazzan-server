; we're storing settings in a map, add ':index value' on a line to add settings
; retreive with (:index settings)
(def settings (atom
               {:port 8080
                :tilemap-path "../witchazzan-client/src/assets/tileMaps/"
                :tilemaps '("openingScene.json" "openingSceneRight1.json" "arena1.json" "testScene1.json")
                :frame-time 40
                :pause false
                :auto-save true
                :auto-load true
                :gene-max 255
                :milis-per-hour 60000}))
(defn setting
  ([key value]
   (swap! settings #(merge % {(keyword key) value})))
  ([key] ((keyword key) @settings)))
;add custom settings to config/config.clj with
;(setting "key" value)
;if your setup is entirly standard, no settings are required
