(ns dar.container
  (:require [clojure.spec.alpha :as s])
  (:import (java.util HashSet ArrayList)))


(set! *warn-on-reflection* true)


(defn level? [obj] (contains? obj :main))
(defn fun?   [obj] (contains? obj :fn))
(defn const? [obj] (contains? obj :value))


(defn- walk-dag [graph main children pre post s]
  (let [visited (new HashSet)
        traverse (fn traverse [s k]
                   (if (.contains visited k)
                     s
                     (do
                       (.add visited k)
                       (let [s (pre s k)]
                         (if (reduced? s)
                           (deref s)
                           (post (reduce traverse s (children graph k)) k))))))]
    (traverse s main)))


(defn- walk [graph main children pre post s]
    (let [visited (new HashSet)
          path (new ArrayList)
          traverse (fn traverse [s k]
                     (cond
                       (.contains visited k) s
                       (.contains path k) (do
                                            (.add path k)
                                            (throw (ex-info "Cycle detected" {::graph-error :cycle ::cycle (vec path)})))
                       :else (let [s (pre s k)]
                               (if (reduced? s)
                                 (do
                                   (.add visited k)
                                   (deref s))
                                 (do
                                   (.add path k)
                                   (let [s (reduce traverse s (children graph k))]
                                     (.remove path (dec (.size path)))
                                     (.add visited k)
                                     (post s k)))))))]
      (traverse s main)))


(defn- pass [s k] s)


(defn- fun-deps [{pre :pre args :args uses :uses}]
  (concat pre (filter #(not= % :eval) args) (when (and (seq uses)
                                                       (some #{:eval} args))
                                              uses)))


(defn- derive-level-list [graph]
  (assoc graph ::levels (walk graph
                              ::main-level
                              (fn children [graph k]
                                (let [node (graph k)]
                                  (cond
                                    (fun? node) (fun-deps node)
                                    (level? node) [(node :main)]
                                    :else nil)))
                              pass
                              (fn post [levels k]
                                (if (level? (graph k))
                                  (conj levels k)
                                  levels))
                              [])))


(defn- dependencies [graph k]
  (let [node (graph k)]
    (cond
      (fun? node) (fun-deps node)
      (const? node) nil
      (level? node) (::deps node)
      :else (throw (IllegalArgumentException. "Unknown node type")))))


(defn- derive-level-nodes-and-deps [graph level-key]
  (let [{main :main args :args} (graph level-key)
        seeds (set args)
        nodes (walk-dag graph
                        main
                        dependencies
                        (fn pre [nodes k]
                          (cond
                            (seeds k) (reduced nodes)
                            (and (= level-key ::main-level) (empty? (dependencies graph k))) (reduced (conj nodes k))
                            :else nodes))
                        (fn post [nodes k]
                          (if (some nodes (dependencies graph k))
                            (conj nodes k)
                            nodes))
                        seeds)
        deps (walk-dag graph
                       main
                       dependencies
                       (fn pre [deps k]
                         (cond
                           (seeds k) (reduced deps)
                           (nodes k) deps
                           :else (reduced (conj deps k))))
                       pass
                       #{})]
    (update graph level-key assoc ::nodes nodes ::deps deps)))


(defn- derive-level-roots-and-shared-nodes [graph level-key]
  (assert (fun? (graph (-> graph level-key :main))))
  (let [{main :main args :args nodes ::nodes} (graph level-key)
        nodes (reduce disj nodes args)
        grouped-dependencies (fn [graph k]
                               (let [node (graph k)
                                     strict (new ArrayList)
                                     lazy (new ArrayList)
                                     shared (new ArrayList)
                                     add-shared (fn add-shared [deps]
                                                  (doseq [d deps :when (nodes d) :let [n (graph d)]]
                                                    (cond
                                                      (fun? n) (.add shared d)
                                                      (level? n) (add-shared (::deps n)))))
                                     uses? (volatile! false)]
                                 (assert (fun? node))
                                 (doseq [d (:pre node) :when (and (nodes d) (fun? (graph d)))]
                                   (.add strict d))
                                 (doseq [a (:args node)]
                                   (if (= a :eval)
                                     (vreset! uses? true)
                                     (when (nodes a)
                                       (let [n (graph a)]
                                         (cond
                                           (fun? n) (if (contains? (:lazy node) a)
                                                      (.add lazy a)
                                                      (.add strict a))
                                           (level? n) (add-shared (::deps n)))))))
                                 (when @uses?
                                   (add-shared (:uses node)))
                                 [strict lazy shared]))
        sorted-nodes (walk-dag graph
                               main
                               (fn children [graph k]
                                 (apply concat (grouped-dependencies graph k)))
                               conj
                               pass
                               [])
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
      (reduce-levels derive-level-nodes-and-deps)
      (reduce-levels derive-level-roots-and-shared-nodes)))


(defn- sym [kind k]
  (symbol (str (name kind) "-" (name k))))


(defn- debug [x]
  (println x)
  x)


(defn- gen-root-computation [graph level-key level-node root]
  (let [{root-of ::root-of nodes ::nodes shared ::shared main :main args :args} level-node
        seeds (set args)

        gen-fun-app (fn [k {args :args lazy :lazy}]
                      `(~(sym :fn k) ~@(map (fn [arg]
                                              (cond
                                                (= arg :eval) `(fn [key]
                                                                 (if (contains? ~(sym :uses k) key)
                                                                   (~(sym :level-getter level-key) ~'state key)
                                                                   (throw (IllegalArgumentException. (str key " is not specified as a dependency of " ~k)))))
                                                (contains? lazy arg) (sym :lazy arg)
                                                :else (sym :val arg)))
                                            args)))

        gen-value-exp-pair (fn [k lazy?]
                             (let [node (graph k)
                                   val-sym (fn []
                                             (sym (if lazy? :lazy :val) k))
                                   wrap-if-lazy (fn [exp]
                                                  (if lazy?
                                                    `(fn [] ~exp)
                                                    exp))]
                               (cond
                                 (= k ::state) ['state `(new ~(sym :state-class level-key) ~'parent)]

                                 (= k ::parent) ['parent `(. ~'state ~'parent)]

                                 ;(= k ::self) ['self `(fn [k] (~(sym :level-getter level-key) ~'state k))]

                                 (= root (root-of k)) [(val-sym) (if lazy?
                                                                   `(fn [] ~(sym :val k))
                                                                   (let [app (gen-fun-app k node)]
                                                                     (if (shared k)
                                                                       `(let [v ~app]
                                                                          (set! (. ~'state ~(sym :state-field k)) v)
                                                                          v)
                                                                       app)))]

                                 (seeds k) [(val-sym) (wrap-if-lazy
                                                        (if (= root main)
                                                          (sym :val k)
                                                          `(. ~'state ~(sym :state-field k))))]

                                 (shared k) [(val-sym) (wrap-if-lazy
                                                         `(. ~'state ~(sym :state-field k)))]

                                 (nodes k) [(val-sym) (wrap-if-lazy
                                                        `(~(sym :level-getter level-key) ~'state ~k))]

                                 :else [(val-sym) (wrap-if-lazy
                                                    `(~'parent ~k))])))]

    (reduce (fn [inner-form [k lazy?]]
              `(let [~@(gen-value-exp-pair k lazy?)]
                 ~inner-form))

            (gen-fun-app root (graph root))

            (next (walk-dag graph
                            [root false]
                            (fn children [graph [k lazy?]]
                              (let [node (graph k)]
                                (cond
                                  (= k ::state) nil
                                  (= k ::parent) nil
                                  (= root (root-of k)) (when-not lazy?
                                                         (let [strict (new ArrayList)
                                                               deferred (new ArrayList)
                                                               lazy (:lazy node)]

                                                           (doseq [d (:pre node) :when (and (not (seeds d))
                                                                                            (not (and (nodes d)
                                                                                                      (not (fun? (graph d))))))]
                                                             (.add strict [d false]))

                                                           (doseq [d (:args node)]
                                                             (cond
                                                               (= d :eval) (when (= root main)
                                                                             (.add deferred [::state]))

                                                               (seeds d) (when (contains? lazy d)
                                                                           (.add deferred [d true]))

                                                               (and (nodes d)
                                                                    (level? (graph d))) (.add deferred [d (contains? lazy d)])

                                                               (not (const? (graph d))) (if (contains? lazy d)
                                                                                          (.add deferred [d true])
                                                                                          (.add strict [d false]))))

                                                           (when (shared k)
                                                             (.add deferred [::state]))

                                                           (concat deferred (reverse strict))))

                                  (and (= root main) (nodes k)) [[::state]]

                                  (and (not= root main) (not (nodes k))) [[::parent]]

                                  :else nil)))
                            conj
                            pass
                            [])))))


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