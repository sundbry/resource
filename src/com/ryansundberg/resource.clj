;;; @author Ryan Sundberg <ryan.sundberg@gmail.com>
;;; A library for structuring application resources similar to Stuart Sierra's Component.
(ns com.ryansundberg.resource
  (:require 
    [clojure.set :refer [difference union]]
    [com.stuartsierra.dependency :as dep]))

(defmacro enable-debugging [] true)

(defmacro debug-log [args]
  (if (enable-debugging)
    `(prn ~args)
    nil))

(defn- decorate-resource
  [instance resource-name resource-type dependency-names subresource-set]
  (merge instance
         {::type resource-type
          ::name resource-name          
          ::dependency-set (set dependency-names)
          ::external-dependency-set nil
          ::dependencies nil ; becomes dict of named dependencies
          ::subresource-set (set subresource-set)
          ::subresources nil ; becomes dict of subresources owned by this
          }))

(defn make-resource
  "Decorate a map object to become a resource"
  ([instance resource-name]
    (decorate-resource instance resource-name ::resource #{} #{}))
  ([instance resource-name dependency-names]
    (decorate-resource instance resource-name ::resource dependency-names #{}))
  ([instance resource-name dependency-names subresources]
    (decorate-resource instance resource-name ::resource dependency-names subresources)))

(defn make-system
  "Decorate a map object to become a system (top level resource)"
  ([instance resource-name]
    (decorate-resource instance resource-name ::system #{} #{}))
  ([instance resource-name subresources]
    (decorate-resource instance resource-name ::system #{} subresources)))

(defn new-resource
  "Construct a new resource"
  [resource-name]
  (make-resource {} resource-name))

(defn new-system
  "Construct a new system"
  [system-name]
  (make-system {} system-name))

(derive ::system ::resource)

(defmulti configure ::type)
(defmulti initialize ::type)

(defn- configure-deps
  [self]
  (let [self-with-subresources
        (assoc self ::subresources
               (into {} (map 
                          (fn [subresource]
                            [(::name subresource)
                             (configure subresource)])
                          (::subresource-set self))))
        sub-deps (reduce union
                         #{}
                         (map ::external-dependency-set (vals (::subresources self-with-subresources))))
        external-deps (union
                        (::dependency-set self) ; explicit dependencies
                        (difference; subresource dependences minus sublings
                          sub-deps
                          (set (keys (::subresources self-with-subresources)))))]
    (assoc self-with-subresources ::external-dependency-set external-deps)))

(defmethod configure ::resource
  [self]
  (configure-deps self))

(defmethod configure ::system
  [self]
  (let [result (configure-deps self)]
    (if (empty? (::external-dependency-set result))
      result
      (throw (ex-info "System has unmet dependencies" result)))))

(defn- add-graph-dependencies
  [dep-graph dependent-name dependency-names]
  (if-let [dependency-name (first dependency-names)]
    (recur (dep/depend dep-graph dependent-name dependency-name)
           dependent-name
           (next dependency-names))
    dep-graph))

(defn- build-dependency-graph
  [dep-graph subresources]
  (if-let [subresource (first subresources)]
    (recur
      (-> dep-graph
        (dep/depend ::parent (::name subresource)) ; add subresource as dependent of parent
        (add-graph-dependencies (::name subresource) ; add dependencies of subresource
                                (::external-dependency-set subresource)))
      (next subresources))
    dep-graph))

(defn- select-dependencies
  [resource dependency-set]
  (into {}
        (map (fn [dependency-name]
               [dependency-name
                (if-let [sibling (get (::subresources resource) dependency-name)]
                  sibling
                  (if-let [external (get (::dependencies resource) dependency-name)]
                    external
                    (throw (ex-info (str "Failed to load dependency: " dependency-name) resource))))])
             dependency-set)))
              
(defn- init-subresources
  [self subresource-order]
  (if-let [subresource-name (first subresource-order)]
    (recur
      (update-in self [::subresources subresource-name]
                 (fn [subresource]
                   (initialize
                     (assoc subresource ::dependencies
                            (select-dependencies self (::external-dependency-set subresource))))))
      (next subresource-order))
    self))

(defn- clean-initialized-resource
  [self]
  (-> self
    (dissoc ::dependency-set)
    (dissoc ::external-dependency-set)
    (dissoc ::subresource-set)))

(defn- seq-difference-helper [result-seq seq-a set-b]
  (if-let [item (first seq-a)]
    (if (contains? set-b item)
      (recur result-seq (next seq-a) set-b)
      (recur (conj result-seq item) (next seq-a) set-b))
    result-seq))
    
(defn- seq-difference [seq-a set-b]
  (seq-difference-helper [] seq-a set-b))

(defn- init-resource
  [self]
  {:pre (some? ::dependencies self)}
  (debug-log (str "Initializing " (::name self)))
  ;; all external dependencies have been injected
  (let [dep-graph (build-dependency-graph (dep/graph) (vals (::subresources self)))
        dep-order (dep/topo-sort dep-graph)
        subresource-order (seq-difference dep-order (conj (::external-dependency-set self) ::parent))]
    (-> (vary-meta self assoc ::subresource-order subresource-order)
      (init-subresources subresource-order)
      (clean-initialized-resource))))

(defmethod initialize ::resource
  [self]
  (init-resource self))

(defmethod initialize ::system
  [self]
  (init-resource (assoc self ::dependencies {})))

#_(defn invoke
  "Invoke a function on resources in dependency order"
  [root func &args]
  (apply func
         (cons (invoke-subresources root (::subresource-order (meta root)) func &args)
               args)))
