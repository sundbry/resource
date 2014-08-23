(ns resource-test
  (:require 
    [clojure.test :refer [deftest is]]
    [resource :refer :all]))

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

(deftest init-simple-system
  (let [sys (define-simple-system)
        sys-initd (initialize sys)]
    (prn sys-initd)))

(deftest init-medium-system
  (let [sys (initialize (define-medium-system))]        
    (prn sys)))

(deftest test-with-resources
  (let [sys (initialize (define-medium-system))]  
    (with-resources sys ["Database" "Agents"]
      (is (some? Database))
      (is (some? Agents)))))
