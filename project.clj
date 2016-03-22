(defproject rum-nim "0.1.0-SNAPSHOT"
  :description "rumnim is a nim environment based on cljs and rum"
  :url "https://github.com/gmp26/rumnim"
  :license {:name "MIT"
            :url "http://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.122"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [cljsjs/react "0.14.3-0"]
                 [rum "0.6.0"]
                 [secretary "1.2.3"]
                 [figwheel-sidecar "0.5.0"]
                 ]

  :plugins [[lein-cljsbuild "1.1.0"]
            ;[lein-figwheel "0.5.0-6"]
            ]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src" "script"]

              :figwheel true

              :compiler {:main nim.core
                         :asset-path "js/compiled/out"
                         :output-to "resources/public/js/compiled/nim.js"
                         :output-dir "resources/public/js/compiled/out"
                         :optimizations :none
                         :source-map true
                         :source-map-timestamp true
                         :cache-analysis true
                         :warnings  {:single-segment-namespace false }}}

             {:id "min"
              :compiler {:output-to "resources/public/js/compiled/build/nim.js"
                         :main nim.core
                         :optimizations :advanced
                         :pretty-print false
                         :warnings  {:single-segment-namespace false}}}

             {:id "debug"
              :compiler {:output-to "resources/public/js/compiled/debug/nim.js"
                         :main nim.core
                         :output-dir "resources/public/js/compiled/debug"
                         :asset-path "js/compiled/debug"
                         :optimizations :advanced
                         :pretty-print false
                         :warnings  {:single-segment-namespace false}}}]}

  :figwheel {
             ;; :http-server-root "public" ;; default and assumes "resources"
             ;; :server-port 3449 ;; default
             :css-dirs ["resources/public/css"] ;; watch and update CSS

             ;; Start an nREPL server into the running figwheel process
             ;; :nrepl-port 7888

             ;; Server Ring Handler (optional)
             ;; if you want to embed a ring handler into the figwheel http-kit
             ;; server, this is for simple ring servers, if this
             ;; doesn't work for you just run your own server :)
             ;; :ring-handler hello_world.server/handler

             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"

             ;; if you want to disable the REPL
             ;; :repl false

             ;; to configure a different figwheel logfile path
             ;; :server-logfile "tmp/logs/figwheel-logfile.log"
             })
