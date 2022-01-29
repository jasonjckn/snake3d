(require 'cemerick.pomegranate.aether)
(cemerick.pomegranate.aether/register-wagon-factory!
  "http" #(org.apache.maven.wagon.providers.http.HttpWagon.))


(defproject snake "1.0.0-SNAPSHOT"


  :dependencies [[org.clojure/clojure "1.8.0"]
                 ;;[org.clojure/clojure-contrib "1.2.0"]

                 [cantor "0.3.0"]
                 [matchure "0.10.1"]
                 [org.clojars.charles-stain/jme3-lwjgl-natives "3.0"]
                 [org.clojars.charles-stain/lwjgl "3.0"]
                 [org.clojars.jasonjckn/thread-expr "1.0.0"]
                 [org.clojure/clojure "1.8.0"]
                 [org.lwjgl/lwjgl-util "2.7.1"]
                 [org.nrepl/incomplete "0.1.0"]
                 [penumbra "0.6.1"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [slick-util "1.0.0"]

                 ]
  ;;:native-dependencies [[penumbra/lwjgl "2.4.2"]]
  :dev-dependencies [;;[native-deps "1.0.5"]
                     ]
  :main core)
