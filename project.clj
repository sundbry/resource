(defproject sundbry/resource "0.2.0-SNAPSHOT"
  :description "System resource library with automatic dependency resolution"
  :author "Ryan Sundberg <ryan.sundberg@gmail.com>"
  :url "https://github.com/ryansundberg/resource"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :min-lein-version "2.1.3"
  :main sundbry.resource
  :plugins []
  :dependencies [[com.stuartsierra/dependency "0.1.1"]
                 [sundbry/nightshell "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.6.0"]
                                  [org.clojure/tools.namespace "0.2.4"]]
                   :source-paths ["dev"]}})
