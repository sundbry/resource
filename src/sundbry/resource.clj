;;; @author Ryan Sundberg <ryan.sundberg@gmail.com>
;;; A library for structuring application resources similar to Stuart Sierra's Component.
(ns sundbry.resource
  (:refer-clojure :exclude [require name])
  (:require 
    [clojure.set :refer [difference union]]
    [nightshell.redl :refer [break]]
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
  [self dependency-set]
  (into {}
        (map (fn [dependency-name]
               [dependency-name (require self dependency-name)])
             dependency-set)))
              
(defn- construct-subresources
  [self subresource-order]
  (if-let [subresource-name (first subresource-order)]
    (recur
      (update-in self [::subresources subresource-name]
                 (fn [subresource]
                   (construct
                     (assoc subresource ::dependencies
                            (select-dependencies self (::external-dependency-set subresource))))))
      (next subresource-order))
    self))

(defn- clean-constructed-resource
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

(defn- construct-resource
  [self]
  {:pre (some? ::dependencies self)}
  (debug-log (str "Initializing " (::name self)))
  ;; all external dependencies have been injected
  (let [dep-graph (build-dependency-graph (dep/graph) (vals (::subresources self)))
        dep-order (dep/topo-sort dep-graph)
        subresource-order (seq-difference dep-order (conj (::external-dependency-set self) ::parent))]
    (-> (vary-meta self assoc ::subresource-order subresource-order)
      (construct-subresources subresource-order)
      (clean-constructed-resource))))

(defmethod construct ::resource
  [self]
  (construct-resource self))

(defmethod construct ::system
  [self]
  (construct-resource (assoc self ::dependencies {})))

(defn initialize
  [system]
  (construct (configure system)))

(defn- apply-each
  ([starting-dict func args]
    (apply-each {} starting-dict func args))
  ([dict dict-rest func args]
    ;{:post [(map? %)]}
    (if-let [[k v] (first dict-rest)]
      (recur
        (assoc dict k (if args
                        (apply func v args)
                        (func v)))
        (next dict-rest)
        func
        args)
      dict)))

(defn apply-subresources
  [self func args]
  "Apply a function on all subresources"
  (assoc self ::subresources (apply-each (::subresources self) func args)))

(defn invoke-subresources
  [self func & args]  
  "Invoke a function on all subresources"
  (apply-subresources self func args))

(defn apply-subresources-recursive
  [self func args]
  "Apply a function on all subresources, and their subresources"
  (if-let [subs (::subresources self)]
    (if (empty? subs)
      (apply func (break self) (break args))
      (assoc self ::subresources
             (apply-each subs apply-subresources-recursive [func args])))
    self))

(defn invoke
  "Invoke a function on resources in dependency order"
  ; TODO update dependencies
  [root func & args]
  (let [inside (apply-subresources-recursive root func (break args))]
    (apply func inside args)))
