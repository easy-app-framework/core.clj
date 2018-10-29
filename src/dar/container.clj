(ns dar.container
  (:require [clojure.spec.alpha :as s])
  (:import (java.util HashSet ArrayList)))


(set! *warn-on-reflection* true)


(defn level? [obj] (contains? obj :main))
(defn fun?   [obj] (contains? obj :fn))
(defn const? [obj] (contains? obj :value))


(defn- walk
  ([children pre post graph main]
   (let [visited (new HashSet)
         traverse (fn traverse [g k]
                    (if (.contains visited k)
                      g
                      (let [g (pre g k)]
                        (if (reduced? g)
                          (do
                            (.add visited k)
                            (deref g))
                          (let [g (reduce traverse g (children g k))]
                            (.add visited k)
                            (post g k))))))]
     (traverse graph main)))

  ([on-cycle children pre post graph main]
    (let [visited (new HashSet)
          path (new ArrayList)
          traverse (fn traverse [g k]
                     (cond
                       (.contains visited k) g
                       (.contains path k) (do
                                            (on-cycle g k (vec path))
                                            (.add visited k)
                                            g)
                       :else (let [g (pre g k)]
                               (if (reduced? g)
                                 (do
                                   (.add visited k)
                                   (deref g))
                                 (do
                                   (.add path k)
                                   (let [g (reduce traverse g (children g k))]
                                     (.remove path (dec (.size path)))
                                     (.add visited k)
                                     (post g k)))))))]
      (traverse graph main))))


(defn- ex-cycle [path]
  (ex-info "Cycle detected" {::graph-error :cycle ::cycle path}))


(defn- throw-on-cycle [g k path]
  (throw (ex-cycle (conj path k))))


(defn- fun-deps [node]
  (concat (:pre node)
          (filter #(not= % :eval) (:args node))
          (:uses node)))


(defn- derive-level-list [graph]
  (walk (fn on-cycle [g k path]
          (when-not (level? (g k))
            (throw-on-cycle g k path)))
        (fn children [g k]
          (let [node (g k)]
            (cond
              (fun? node) (fun-deps node)
              (level? node) [(node :main)]
              :else nil)))
        (fn pre [g _] g)
        (fn post [g k]
          (if (level? (g k))
            (update g ::levels conj k)
            g))
        (assoc graph ::levels [])
        ::main-level))


(defn- dependencies [graph k]
  (let [node (graph k)]
    (cond
      (fun? node) (fun-deps node)
      (const? node) nil
      (level? node) (node ::deps)
      :else (throw (IllegalArgumentException. "Unknown node type")))))


(defn- derive-level-nodes [graph level-key]
  (let [{main :main args :args} (graph level-key)
        args-set (set args)
        g (walk throw-on-cycle
                dependencies
                (fn pre [g k]
                  (cond
                    (contains? args-set k) (reduced g)
                    (and (= level-key ::main-level) (empty? (dependencies g k))) (reduced (update g ::nodes conj k))
                    :else g))
                (fn post [{nodes ::nodes :as g} k]
                  (if (some nodes (dependencies g k))
                    (update g ::nodes conj k)
                    g))
                (assoc graph ::nodes (reduce conj #{} args))
                main)]
    (assoc-in graph [level-key ::nodes] (g ::nodes))))


(defn- derive-level-deps [graph level-key]
  (let [{main :main args :args nodes ::nodes} (graph level-key)
        args-set (set args)
        g (walk dependencies
                (fn pre [g k]
                  (cond
                    (contains? args-set k) (reduced g)
                    (contains? nodes k) g
                    (level? (g k)) g
                    :else (reduced (update g ::deps conj k))))
                (fn post [g _] g)
                (assoc graph ::deps #{})
                main)]
    (assoc-in graph [level-key ::deps] (g ::deps))))


(defn- derive-level-roots-and-shared-nodes [graph level-key]
  (let [{main :main :as level} (graph level-key)
        nodes (reduce disj (::nodes level) (:args level))
        grouped-dependencies (fn [g k]
                               (let [node (g k)
                                     strict (new ArrayList)
                                     lazy (new ArrayList)
                                     shared (new ArrayList)
                                     add-shared (fn add-shared [deps]
                                                  (doseq [d deps :when (nodes d) :let [n (g d)]]
                                                    (cond
                                                      (fun? n) (.add shared d)
                                                      (level? n) (add-shared (::deps n)))))
                                     uses? (volatile! false)]
                                 (assert (fun? node))
                                 (doseq [d (:pre node) :when (and (nodes d) (fun? (g d)))]
                                   (.add strict d))
                                 (doseq [a (:args node)]
                                   (if (= a :eval)
                                     (vreset! uses? true)
                                     (when (nodes a)
                                       (let [n (g a)]
                                         (cond
                                           (fun? n) (if (contains? (:lazy node) a)
                                                      (.add lazy a)
                                                      (.add strict a))
                                           (level? n) (add-shared (::deps n)))))))
                                 (when @uses?
                                   (add-shared (:uses node)))
                                 [strict lazy shared]))
        sorted-nodes (::nodes (walk (fn children [g k]
                                      (apply concat (grouped-dependencies g k)))
                                    (fn pre [g k] g)
                                    (fn post [g k]
                                      (update g ::nodes #(cons k %)))
                                    graph
                                    main))
        shared (atom #{})
        root-of (atom {})
        requests (atom {})
        acc-request (fn [k root kind idx]
                      (swap! requests update k (fnil conj []) [root kind idx]))
        min-request (fn [rs]
                      (reduce (fn [[root kind shared? idx] [r k i]]
                                (let [shared? (or shared? (= k :shared))]
                                  (if (< i idx)
                                    [r k shared? i]
                                    [root kind shared? idx])))
                              [nil nil false Integer/MAX_VALUE]
                              rs))]
    (doseq [[k idx] (map vector sorted-nodes (range))]
      (let [rs (@requests k)
            [min-req-root min-req-kind shared? _] (min-request rs)
            root (if (and (= min-req-kind :strict)
                          (or (= min-req-root main)
                              (= 1 (count (reduce (fn [roots req]
                                                    (conj roots (first req)))
                                                  #{}
                                                  rs)))))
                   (do
                     (when shared?
                       (swap! shared conj k))
                     min-req-root)
                   k)
            [strict lazy shared] (grouped-dependencies graph k)]
        (swap! root-of assoc k root)
        (doseq [d strict]
          (acc-request d root :strict idx))
        (doseq [d lazy]
          (acc-request d root :lazy idx))
        (doseq [d shared]
          (acc-request d root :shared idx))))
    (update graph level-key assoc ::root-of @root-of ::shared @shared)))


(defn- reduce-levels [graph f]
  (reduce f graph (graph ::levels)))


(defn- analyze [graph main args]
  (-> graph
      (assoc ::main-level {:main main :args args})
      derive-level-list
      (reduce-levels derive-level-nodes)
      (reduce-levels derive-level-deps)
      (reduce-levels derive-level-nodes)
      (reduce-levels derive-level-deps)
      (reduce-levels derive-level-roots-and-shared-nodes)))


(defn- sym [kind k])


(defn- evaluate [node-key uses self k]
  (if (contains? uses k)
    (self k)
    (throw (IllegalArgumentException. (str k " is not specified as a dependency of " node-key)))))


(defn update-app [app f & args]
  (if (instance? clojure.lang.Atom app)
    (apply swap! app f args)
    (apply f app args)))


(defn unwrap-app [app]
  (if (instance? clojure.lang.Atom app)
    @app
    app))


(defn define [app k & {:as d}]
  (update-app app assoc k d))


(defn define-level [app k main args]
  (update-app app assoc k {:main main :args args}))


(defn define-constant [app k value]
  (update-app app assoc k {:value value}))


(defn compile-app [app main args]
  (-> app
      unwrap-app
      (analyze main args)))