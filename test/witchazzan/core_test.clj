(ns witchazzan.core-test
  (:require [clojure.test :refer :all]
            [witchazzan.core :refer :all]
            [clojure.string :as str]))

(deftest a-test
  (testing "Tests should pass. That is how they work."
    (is (= 1 1))))

(deftest check-name->scene
  (is (name->scene (str/replace (first (setting "tilemaps")) #".json" ""))))

(deftest check-settings
  ;makes sure the settings file being loaded has reasonable values
  (is (integer? (setting "port")))
  (is (contains?  @settings :tilemap-path))
  (is (contains?  @settings :tilemaps))
  (is (slurp (str (setting "tilemap-path") (first (setting "tilemaps")))))
  (is (integer? (setting "millis-per-frame")))
  (is (contains? @settings :pause)))
