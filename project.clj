(defproject net.clojure/monads "1.0.3-SNAPSHOT"
  :description "A protocol based implementation of monads"
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :source-paths ["src/clj", "src/cljs"]
  :test-paths ["test/clj"]
  :profiles {:dev {:dependencies [[com.birdseye-sw/buster-cljs "0.1.2"]]
                   :plugins [[lein-cljsbuild "0.3.2"]
                             [com.birdseye-sw/lein-dalap "0.1.1"]]}}
  :hooks [leiningen.dalap]

  :cljsbuild
  {:builds
   [{:source-paths ["src/cljs" "test/cljs"],
     :compiler
     {:pretty-print true,
      :target :browser,
      :output-to "resources/js/protocol_monads_browser_test.js",
      :externs ["externs/buster.js"],
      :optimizations :whitespace},
     :id "browser-test"}
    {:source-paths ["src/cljs" "test/cljs"],
     :compiler
     {:target :browser,
      :output-to "resources/js/protocol_monads_browser_optimized_test.js",
      :externs ["externs/buster.js"],
      :optimizations :advanced},
     :id "browser-test-optimized"}
    {:source-paths ["src/cljs" "test/cljs"],
     :compiler
     {:pretty-print true,
      :target :node,
      :output-to "resources/js/protocol_monads_node_test.js",
      :externs ["externs/buster.js"],
      :optimizations :whitespace},
     :id "node-test"}]}

  )
