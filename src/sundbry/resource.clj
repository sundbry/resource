;;; @author Ryan Sundberg <ryan.sundberg@gmail.com>
;;; A library for structuring application resources similar to Stuart Sierra's Component.
(ns sundbry.resource
  (:refer-clojure :exclude [name require])
  (:require 
    [clojure.set :refer [difference union]]
    [com.stuartsierra.dependency :as dep]))

(defmacro ^:private enable-debugging [] false)

(defmacro ^:private debug-log [args]
  (if (enable-debugging)
    `(prn ~args)
    nil))

(defn- decorate-resource
  [instance resource-name resource-type dependency-names subresource-set]
  (doseq [dep-name (seq dependency-names)]
    (when (nil? dep-name)
      (throw (ex-info "Dependency name is nil" {:dependency-names dependency-names}))))
  (doseq [sub (seq subresource-set)]
    (when (nil? sub)
      (throw (ex-info "Subresource is nil" {:dependency-names dependency-names}))))
                
  (merge instance
         {::type resource-type
          ::name resource-name
          ::owner-full-name nil
          ::dependency-set (set dependency-names)
          ::external-dependency-set nil
          ::dependencies nil ; becomes dict of named dependencies
          ::subresource-set (set subresource-set)
          ::subresources nil ; becomes dict of subresources owned by this
          }))

; ["res1" "res2" ]
; becomes (let [res1 (require "res1") res2 (require "res2")] ...)
(defn- resource-bindings
  [self bindings resource-names]
  {:pre [(symbol? self)]}
  (if-let [resource-name (first resource-names)]
    (recur
      self
      (conj bindings (symbol resource-name) `(require ~self ~resource-name))
      (next resource-names))
    bindings))

(defmacro with-resources
  [self resource-names & forms]
  (let [self-sym (gensym)
        bindings (vec (resource-bindings self-sym [self-sym self] resource-names))]
    `(let ~bindings ~@forms)))

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

(defn acquire
  [self resource-name]
  "Return a named subresource or dependency, or nil"
  (if-let [sibling (get (::subresources self) resource-name)]
    sibling
    (if-let [dependency (get (::dependencies self) resource-name)]
      dependency
      nil)))

(defn require
  [self resource-name]
  "Return a named subresource or dependency. Throw on failure."
  (if-let [resource (acquire self resource-name)]
    resource
    (throw (ex-info (str "Failed to require resource: " resource-name) self))))

(defn name
  [self]
  (::name self))

(defn full-name
  [self]
  (if (some? (::owner-full-name self))
    (str (::owner-full-name self) "." (::name self))
    (::name self)))

(defn subresources
  [self]
  (vals (::subresources self)))

(derive ::system ::resource)

(defmulti ^:private configure ::type)
(defmulti ^:private construct ::type)

(defn- configure-deps
  "Determine my ::subresources and ::external-dependency-set"
  [self]
  (let [self-with-subresources
        (assoc self ::subresources
               (into {} (map 
                          (fn [subresource]
                            [(::name subresource)
                             (configure
                               (assoc subresource
                                      ::owner-full-name
                                      (full-name self)))])
                          (::subresource-set self))))
        sub-deps (reduce union
                         #{}
                         (map ::external-dependency-set (vals (::subresources self-with-subresources))))
        external-deps (union
                        (::dependency-set self) ; explicit dependencies
                        (difference; subresource dependences minus sublings
                          sub-deps
                          (set (keys (::subresources self-with-subresources)))))]
    (-> self-with-subresources
        (assoc ::external-dependency-set external-deps)
        (dissoc ::dependency-set)
        (dissoc ::subresource-set))))

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
  [self dependency-set]
  (into {}
        (map (fn [dependency-name]
               [dependency-name (require self dependency-name)])
             dependency-set)))
              
(defn- construct-subresources
  [self subresource-order ctor]
  (if-let [subresource-name (first subresource-order)]
    (recur
      (update-in self [::subresources subresource-name]
                 (fn [subresource]
                   (construct
                     (assoc subresource ::dependencies
                            (select-dependencies self (::external-dependency-set subresource)))
                     ctor)))
      (next subresource-order)
      ctor)
    self))

(defn- seq-difference-helper [result-seq seq-a set-b]
  (if-let [item (first seq-a)]
    (if (contains? set-b item)
      (recur result-seq (next seq-a) set-b)
      (recur (conj result-seq item) (next seq-a) set-b))
    result-seq))
    
(defn- seq-difference [seq-a set-b]
  (seq-difference-helper [] seq-a set-b))

(defn- construct-resource
  [self ctor]
  {:pre (some? ::dependencies self)}
  ;; all external dependencies have been injected
  ; (debug-log (str "Initializing " (::name self)))
  (let [self
        (if (some? (::subresource-order (meta self)))
          self
          (let [dep-graph (build-dependency-graph (dep/graph) (vals (::subresources self)))
                dep-order (dep/topo-sort dep-graph)
                subresource-order (seq-difference dep-order (conj (::external-dependency-set self) ::parent))]
            (vary-meta self assoc ::subresource-order subresource-order)))]
    (-> self
        (construct-subresources (::subresource-order (meta self)) ctor)
        (ctor))))

(declare deconstruct-resource)

(defn- deconstruct-subresources
  [self reverse-subresource-order dtor]
  (if-let [subresource-name (first reverse-subresource-order)]
    (recur
      (update-in self [::subresources subresource-name]
                 (fn [subresource]
                   (deconstruct-resource
                     (assoc subresource ::dependencies
                            (select-dependencies self (::external-dependency-set subresource)))
                     dtor)))
      (next reverse-subresource-order)
      dtor)
    self))

(defn- deconstruct-resource
  [self dtor]
  {:pre (some? ::dependencies self)}
  ;; all external dependencies have been injected
  (-> self
      (dtor)
      (deconstruct-subresources (reverse (::subresource-order (meta self))) dtor)))

(defmethod construct ::resource
  [self ctor]
  (construct-resource self ctor))

(defmethod construct ::system
  [self ctor]
  (construct-resource (assoc self ::dependencies {}) ctor))

(defn initialize
  [system]
  (construct (configure system) identity))

(defn invoke
  [self func]
  (construct self func))

(defn invoke-reverse
  [self func]
  (deconstruct-resource self func))

(defn invoke-subresource
  [self resource-name func]
  (update-in self [::subresources resource-name] func))

(defn- apply-each
  ([src-dict order func args]
    (apply-each {} src-dict order func args))
  ([dict src-dict order func args]
    ;{:post [(map? %)]}
    (if-let [k (first order)]
      (recur
        (assoc dict k (if args
                        (apply func (get src-dict k) args)
                        (func (get src-dict k))))
        src-dict
        (next order)
        func
        args)
      dict)))

(defn apply-subresources
  [self func args]
  "Apply a function on all subresources"
  (assoc self ::subresources (apply-each (::subresources self)
                                         (::subresource-order (meta self))
                                         func args)))

(defn invoke-subresources
  [self func & args]  
  "Invoke a function on all subresources"
  (apply-subresources self func args))

(defn apply-subresources-parallel
  [self func args]
  (update-in self [::subresources]
             (fn [subs]
               (into {} (pmap
                          (fn [[k v]] [k (apply func v args)])
                          subs)))))

(defn invoke-subresources-parallel
  [self func & args]
  (apply-subresources-parallel self func args))

(defn apply-subresources-reverse
  [self func args]
  "Apply a function on all subresources in reverse order"
  (assoc self ::subresources (apply-each (::subresources self)
                                         (reverse (::subresource-order (meta self)))
                                         func args)))

(defn invoke-subresources-reverse
  [self func & args]
  (apply-subresources-reverse self func args))

(defn apply-subresources-recursive
  [self func args]
  "Apply a function on all subresources, and their subresources"
  (apply func
         (if-let [subs (::subresources self)]
           (assoc self ::subresources
                  (apply-each subs (::subresource-order (meta self))
                              apply-subresources-recursive [func args]))
           self)
         args))

(defn apply-subresources-recursive-reverse
  [self func args]
  "Apply a function on all subresources, and their subresources"
  (apply func
         (if-let [subs (::subresources self)]
           (assoc self ::subresources
                  (apply-each subs (reverse (::subresource-order (meta self)))
                              apply-subresources-recursive-reverse [func args]))
           self)
         args))

(defn invoke-visit
  [root func & args]
  "Invoke a function on self and subresources recursively, in dependency order"
  ; TODO does not update dependencies, so this does not change exiting references to resources!
  (apply-subresources-recursive root func args))

(defn invoke-visit-reverse
  [root func & args]
  "Invoke a function on self and subresources recursively, in reverse dependency order"
  (apply-subresources-recursive-reverse root func args))
