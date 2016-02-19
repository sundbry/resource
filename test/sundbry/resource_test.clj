(ns sundbry.resource-test
  (:require 
    [clojure.test :refer [deftest is]]
    [sundbry.resource :as res :refer [make-system make-resource]]))

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
        sys-initd (res/initialize sys)]
    (is (instance? Foo sys-initd))))

(deftest init-medium-system
  (let [sys (res/initialize (define-medium-system))]        
    (is (instance? Foo sys))))

(deftest test-with-resources
  (let [sys (res/initialize (define-medium-system))]  
    (res/with-resources sys ["Database" "Agents"]
      (is (some? Database))
      (is (some? Agents)))))

(deftest test-invoke-visit
  (let [sys (res/initialize (define-medium-system))
        sys2 (res/invoke-visit sys (fn [x y] (comment "Visiting resource:" (name x) "param:" y) x) "P")]
    (is (= sys sys2))
    (let [sys3 (res/invoke-visit sys (fn [x] (assoc x :something true)))]
      (is (not= sys sys3)))))

(defn- count-resources
  [self]
  (apply + 1 (res/subresources self)))

(deftest test-invoke-visit-2
  (let [sys (res/initialize (define-medium-system))
        resource-count (res/invoke-visit sys count-resources)]
    (is (= 8 resource-count))))

(deftest test-invoke
  (let [sys (res/initialize (define-medium-system))
        sys2 (res/invoke sys identity)
        sys3 (res/invoke sys2 (fn [x] (assoc x :a 1)))
        sys4 (res/invoke sys3 (fn [x] (update-in x [:a] inc)))
        sys5 (res/invoke sys4 (fn [x] (update-in x [:a] dec)))
        sys6 (res/invoke sys5 (fn [x] (dissoc x :a)))
        sys7 (res/invoke-reverse sys5 (fn [x] (dissoc x :a)))
        sys8 (res/invoke sys7 identity)]
    (is (= sys sys2))
    (is (not= sys sys3))
    (is (not= sys3 sys4))
    (let [agents (res/require sys4 "Agents")
          agent-1 (res/require agents "Agent-1")
          a-1-logger (res/require agent-1 "Logger")]
      (is (= 2 (:a a-1-logger))))
    (is (= sys3 sys5))
    (is (= sys sys6))
    (is (= sys sys8))))

(deftest test-invoke-subresource
  (let [sys (res/initialize (define-medium-system))
        sys2 (res/invoke-subresource sys "Logger" #(assoc % :hello "world!"))]
    (is (= "world!" (:hello (res/require sys2 "Logger"))))))
