(ns com.ryansundberg.resource-test
  (:require 
    [clojure.test :refer [deftest is]]
    [com.ryansundberg.resource :refer :all]))

(defrecord Foo [])

(defn define-simple-system
  []
  (make-system
    (->Foo)
    "Application"
    #{(make-resource (->Foo)
                     "Logger")
      (make-resource (->Foo)
                     "Database"
                     #{"Logger"})}))
      
(defn define-agent-resource
  [agent-id]
  (make-resource (->Foo)
                 (str "Agent-" agent-id)
                 #{"Logger" "Database"}))
      
(defn define-medium-system
  []
  (make-system
    (->Foo)
    "Application"
    #{(make-resource (->Foo)
                     "Logger")
      (make-resource (->Foo)
                     "Database"
                     #{"Logger"})
      (make-resource (->Foo)
                     "Agents"
                     #{}
                     (map define-agent-resource (range 1 4)))
      (make-resource (->Foo)
                     "Leaf")}))

(deftest configure-simple-system
  (let [sys (define-simple-system)
        sys-configd (configure sys)]
    (prn sys-configd)))

(deftest configure-medium-system
  (let [sys (define-medium-system)
        sys-configd (configure sys)]
    (prn sys-configd)))
