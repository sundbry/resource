(defproject ryansundberg/resource "0.1.8-SNAPSHOT"
  :description "Managed lifecycle of stateful objects with dependency determination"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :min-lein-version "2.1.3"
  :dependencies [[com.stuartsierra/dependency "0.1.1"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.6.0"]
                                  [org.clojure/tools.namespace "0.2.4"]]
                   :source-paths ["dev"]}})
