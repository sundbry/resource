(ns sundbry.resource-test
  (:refer-clojure :exclude [[name require]])
  (:require 
    [clojure.test :refer [deftest is]]
    [sundbry.resource :refer :all]))

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
    (is (instance? Foo sys-initd))))

(deftest init-medium-system
  (let [sys (initialize (define-medium-system))]        
    (is (instance? Foo sys))))

(deftest test-with-resources
  (let [sys (initialize (define-medium-system))]  
    (with-resources sys ["Database" "Agents"]
      (is (some? Database))
      (is (some? Agents)))))

(deftest test-invoke
  (let [sys (initialize (define-medium-system))
        sys2 (invoke sys (fn [x y] (comment "Visiting resource:" (name x) "param:" y) x) "P")]
    (is (= sys sys2))
    (let [sys3 (invoke sys (fn [x] (assoc x :something true)))]
      (is (not= sys sys3)))))
