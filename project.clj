(defproject net.clojure/monads "1.0.2"
  :description "A protocol based implementation of monads"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :source-paths ["src/clj", "src/cljs"]
  :test-paths ["test/clj"]
  :profiles {:dev {:dependencies [[com.birdseye-sw/buster-cljs "0.1.2"]]
                   :plugins [[lein-cljsbuild "0.3.2"]
                             [com.birdseye-sw/lein-dalap "0.1.1"]]}}
  :hooks [leiningen.dalap]

  :cljsbuild
  {:builds
   [{:id :browser-test
     :source-paths ["src/cljs" "test/cljs"]
     :compiler
     {:output-to "resources/js/protocol_monads_browser_test.js"
      :target :browser
      :externs ["externs/buster.js"]
      :optimizations :simple
      :pretty-print true}}
    {:id :browser-test-optimized
     :source-path ["src/cljs" "test/cljs"]
     :compiler
     {:output-to "resources/js/protocol_monads_browser_optimized_test.js"
      :target :browser
      :externs ["externs/buster.js"]
      :optimizations :advanced}}
    {:id :node-test
     :source-paths ["src/cljs" "test/cljs"]
     :compiler
     {:output-to "resources/js/protocol_monads_node_test.js"
      :target :node
      :externs ["externs/buster.js"]
      :optimizations :simple
      :pretty-print true}}]}

  )
