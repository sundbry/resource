(defproject sundbry/resource "0.4.0"
  :description "System resource library with automatic dependency resolution"
  :author "Ryan Sundberg <ryan.sundberg@gmail.com>"
  :url "https://github.com/sundbry/resource"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :plugins []
  :dependencies [[com.stuartsierra/dependency "0.1.1"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.6.0"]]}})
