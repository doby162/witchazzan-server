(defproject witchazzan "0.1.0-SNAPSHOT"
  :description "The server for witchazzan.space. An agent based simulation and game."
  :url "http://witchazzan.space"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-cloverage "1.1.2"] [lein-bikeshed "0.5.2"] [lein-cljfmt "0.6.7"] [jonase/eastwood "0.3.5"]]
  :bikeshed {:max-line-length 120}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [crypto-password "0.2.1"]
                 [seancorfield/next.jdbc "1.0.462"]
                 [org.mariadb.jdbc/mariadb-java-client "2.5.4"]
                 [com.clojure-goes-fast/clj-async-profiler "0.4.0"]
                 [http-kit "2.3.0"]
                 [criterium "0.4.5"]
                 [compojure "1.6.1"]
                 [org.clojure/data.json "0.2.6"]]
  :main witchazzan.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  :global-vars {*warn-on-reflection* true}
  :uberjar-name "witchazzan-server.jar"
  :jar-name "broken_jar"
  :repl-options {:port 8081 :init (-main)})
