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
  []
  (make-resource (->Foo)
                 (str "Agent-" (rand-int 999))
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
                     (repeatedly 3 define-agent-resource))}))

(deftest configure-simple-system
  (let [sys (define-simple-system)
        sys-configd (configure sys)]
    (prn sys-configd)))

(deftest configure-medium-system
  (let [sys (define-medium-system)
        sys-configd (configure sys)]
    (prn sys-configd)))
