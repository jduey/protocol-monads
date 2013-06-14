(defproject net.clojure/monads "1.0.1"
  :description "A protocol based implementation of monads"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :source-paths ["src/clj", "src/cljs"]
  :test-paths ["test/clj"]
  :profiles {:dev {:dependencies [[com.birdseye-sw/buster-cljs "0.1.0"]]
                   :plugins [[lein-cljsbuild "0.2.9"]
                             [com.birdseye-sw/lein-dalap "0.1.0"]]}}
  :hooks [leiningen.dalap]

  :cljsbuild
  {:builds
   [{:id :dev
     :source-path "src/cljs"
     :compiler
     {:output-to "resources/js/protocol_monads_dev.js"
      :optimizations :simple
      :pretty-print true}}
    {:id :browser-test
     :source-path "test/cljs"
     :compiler
     {:output-to "resources/js/protocol_monads_browser_test.js"
      :libraries ["resources/js/protocol_monads_dev.js"]
      :externs ["externs/buster.js"]
      :optimizations :simple
      :pretty-print true}}
    {:id :browser-test-optimized
     :source-path "test/cljs"
     :compiler
     {:output-to "resources/js/protocol_monads_browser_optimized_test.js"
      :libraries ["resources/js/protocol_monads_dev.js"]
      :externs ["externs/buster.js"]
      :optimizations :advanced}}
    {:id :node-test
     :source-path "test/cljs"
     :compiler
     {:output-to "resources/js/protocol_monads_node_test.js"
      :libraries ["resources/js/protocol_monads_dev.js"]
      :externs ["externs/buster.js"]
      :optimizations :simple
      :pretty-print true}}]}

  )
