(defproject witchazzan "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-cloverage "1.1.2"] [lein-bikeshed "0.5.2"] [lein-cljfmt "0.6.6"] [jonase/eastwood "0.3.5"]]
  :bikeshed {:max-line-length 120}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [http-kit "2.3.0"]
                 [org.clojure/data.json "0.2.6"]]
  :main ^:skip-aot witchazzan.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :repl-options {:port 8081})
