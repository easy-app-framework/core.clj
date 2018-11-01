(ns dar.container
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as st])
  (:import (java.util HashSet ArrayList HashMap)))


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
                               pass
                               (fn post [nodes k]
                                 (cons k nodes))
                               nil)
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


(defn- gen-name [kind k]
  (let [n (if (namespace k)
            (str (st/replace (namespace k) "." "-") "-" (name k))
            (name k))]
    (case kind
      :val n
      :lazy (str "lazy--" n)
      :fn (str "fn--" n)
      :root-fn (str "rfn--" n)
      :level-getter (str n "--get")
      :uses (str "uses--" n)
      :state-class (str (apply str (map st/capitalize (st/split n (re-pattern "-"))))
                        "State")
      :state-field (let [parts (st/split n (re-pattern "-"))]
                     (apply str (first parts) (map st/capitalize (next parts))))
      :state-field-ready (apply str "isReady" (map st/capitalize (st/split n (re-pattern "-"))))
      (throw (IllegalArgumentException. (str "Unknown kind " k))))))


(def ^:private ^:dynamic ^HashSet *names* nil)
(def ^:private ^:dynamic ^HashMap *bindings* nil)


(defmacro ^:private with-fresh-names [body]
  (binding [*names* (new HashSet)
            *bindings* (new HashMap)]
    (.add *names* 'state)
    (.add *names* 'k)
    (.add *names* 'self)
    (.add *names* 'parent)
    ~@body))


(defn- sym [kind k]
  (symbol (gen-name kind k)))


(defn- debug [x]
  (println x)
  x)


(defn gen-root-fn [graph level-key root]
  (let [{root-of ::root-of nodes ::nodes shared ::shared main :main args :args level-deps ::deps} (graph level-key)
        seeds (set args)

        arguments (if (= root main)
                    (let [args (map (partial sym :val) args)]
                      (if (seq level-deps)
                        (cons 'parent args)
                        args))
                    ['state])

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

        gen-name-exp-pair (fn [k lazy?]
                            (let [node (graph k)
                                  val-sym (fn []
                                            (sym (if lazy? :lazy :val) k))
                                  wrap-if-lazy (fn [exp]
                                                 (if lazy?
                                                   `(fn [] ~exp)
                                                   exp))]
                              (cond
                                (= k ::state) (do
                                                (assert (= root main))
                                                ['state `(new ~(sym :state-class level-key) ~@arguments)])

                                (= k ::parent) (do
                                                 (assert (not= root main))
                                                 ['parent `(. ~'state ~'parent)])

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
                                                   `(~'parent ~k))])))

        names-seq (walk-dag graph
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

                                                           (concat strict deferred)))

                                  (and (= root main) (nodes k)) [[::state]]

                                  (and (not= root main) (not (nodes k))) [[::parent]]

                                  :else nil)))
                            pass
                            (fn post [nodes k]
                              (cons k nodes))
                            nil)

        body (reduce (fn [inner-form [k lazy?]]
                       `(let [~@(gen-name-exp-pair k lazy?)]
                          ~inner-form))
                     (gen-fun-app root (graph root))
                     (next names-seq))]

    `(fn [~@arguments]
       ~body)))


(defn gen-level-getter [graph level-key]
  (let [{main :main args :args root-of ::root-of level-deps ::deps shared ::shared nodes ::nodes} (graph level-key)
        seeds (set args)
        cases (reduce (fn [cases k]
                        (let [conj-exp (fn [exp]
                                         (conj (conj cases k) exp))]
                          (cond
                            (or (shared k) (seeds k)) (conj-exp `(. ~'state ~(sym :state-field k)))

                            (and (not= k main) (= (root-of k) k)) (conj-exp `(if (. ~'state ~(sym :state-field-ready k))
                                                                               (. ~'state ~(sym :state-field k))
                                                                               (locking ~'state
                                                                                 (if (. ~'state ~(sym :state-field-ready k))
                                                                                   (. ~'state ~(sym :state-field k))
                                                                                   (let [v# (~(sym :root-fn k) ~'state)]
                                                                                     (set! (. ~'state ~(sym :state-field k)) v#)
                                                                                     (set! (. ~'state ~(sym :state-field-ready k)) true)
                                                                                     v#)))))

                            (level? (graph k)) (let [{args :args deps ::deps} (graph k)
                                                     args (map (partial sym :val) args)]
                                                 (conj-exp `(fn [~@args]
                                                              (~(sym :root-fn k) ~@(if (seq deps) ['self] nil) ~@args))))
                            :else cases)))
                      []
                      (sort nodes))]
    `(fn ~'self [~'state ~'k]
       (case ~'k
         ~@cases
         ~(if (seq level-deps)
            `((. ~'state ~'parent) ~'k)
            `(throw (IllegalArgumentException. (str "Unknown node " ~'k))))))))


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