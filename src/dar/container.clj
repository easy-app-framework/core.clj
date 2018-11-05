(ns dar.container
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as st]
            [clojure.pprint :as pp]
            [clojure.set :refer [union]]
            [insn.core :as insn])
  (:import (java.util HashSet ArrayList HashMap)))


(set! *warn-on-reflection* true)


(def ^:private non-reserved-keyword
  (s/and keyword? #(not= % :eval)))


(s/def :dar.container.fun-node/fn ifn?)
(s/def :dar.container.fun-node/args (s/coll-of keyword? :kind vector?))
(s/def :dar.container.fun-node/lazy (s/coll-of non-reserved-keyword :kind set?))
(s/def :dar.container.fun-node/pre (s/coll-of non-reserved-keyword :kind vector?))
(s/def :dar.container.fun-node/uses (s/coll-of non-reserved-keyword :kind set?))
(s/def ::fun-node (s/keys :req-un [:dar.container.fun-node/fn]
                          :opt-un [:dar.container.fun-node/pre
                                   :dar.container.fun-node/args
                                   :dar.container.fun-node/lazy
                                   :dar.container.fun-node/uses]))


(s/def :dar.container.level-node/main non-reserved-keyword)
(s/def :dar.container.level-node/args (s/coll-of non-reserved-keyword :kind vector?))
(s/def ::level-node (s/keys :req-un [:dar.container.level-node/main]
                            :opt-un [:dar.container.level-node/args]))


(s/def :dar.container.value-node/value any?)
(s/def ::value-node (s/keys :req-un [:dar.container.value-node/value]))


(defn- unambiguous? [node]
  (< (count (select-keys node [:fn :main :value]))
     2))


(s/def ::graph (s/map-of non-reserved-keyword (s/and map? unambiguous?
                                                     (s/or :fun   ::fun-node
                                                           :level ::level-node
                                                           :const ::value-node))))


(defn- check [arg spec arg-name]
  (when-not (s/valid? spec arg)
    (throw (ex-info (str "Invalid " arg-name) (s/explain-data spec arg)))))


(defn- level? [obj] (contains? obj :main))
(defn- fun?   [obj] (contains? obj :fn))
(defn- const? [obj] (contains? obj :value))


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
                                          (throw (ex-info "Unsupported cycle detected"
                                                          {::cycle (vec path)})))
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
  (concat pre
          (filter #(not= % :eval) args)
          (when (and (seq uses)
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


(defn- set-seeds [graph level-key]
  (update graph level-key #(assoc % ::seeds (set (:args %)))))


(defn- dependencies [graph k]
  (let [node (graph k)]
    (cond
      (nil? node) nil
      (fun? node) (fun-deps node)
      (const? node) nil
      (level? node) (::deps node)
      :else (throw (IllegalArgumentException. "Unknown node type")))))


(defn- derive-level-nodes-and-deps [graph level-key]
  (let [{main :main seeds ::seeds} (graph level-key)
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


(defn- check-all-required-nodes-are-defined [graph]
  (let [deps (-> graph ::main-level ::deps)]
    (when (seq deps)
      (throw (ex-info "Not all nodes are defined"
                      {::undefined-nodes deps}))))
  graph)


(defn- replace-levels [graph f]
  (let [levels (::levels graph)
        m (reduce (fn [m k]
                    (if-let [rep (f (graph k))]
                      (assoc m k rep)
                      m))
                  {}
                  levels)]
    (if (seq m)
      (-> graph (merge m) (assoc ::levels (filterv m levels)))
      graph)))


(defn- last-index-of [x coll]
  (last (keep-indexed (fn [i item]
                        (when (= item x)
                          i))
                      coll)))


(defn- replace-levels-with-seeded-main [{main :main args :args}]
  (when-let [idx (last-index-of main args)]
    {:value (eval
              `(fn [~@(map #(if (= % idx) 'x '_)
                           (range (count args)))]
                 ~'x))}))


(defn- replace-ambiguous-constants-with-fns [graph]
  (let [seeds (reduce (fn [seeds k]
                        (union seeds (::seeds (graph k))))
                      #{}
                      (::levels graph))]
    (reduce (fn [g [k n]]
              (if (and (const? n) (seeds k))
                (let [v (:value n)]
                  (assoc g k {:fn (fn [] v)}))
                g))
            graph
            graph)))


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
  (check graph ::graph "graph")
  (check main non-reserved-keyword "main argument")
  (check args (s/coll-of non-reserved-keyword :kind vector?) "args")
  (-> graph
      (assoc ::main-level {:main main :args args})
      derive-level-list
      (replace-levels replace-levels-with-seeded-main)
      (reduce-levels set-seeds)
      (reduce-levels derive-level-nodes-and-deps)
      check-all-required-nodes-are-defined
      replace-ambiguous-constants-with-fns
      (reduce-levels derive-level-roots-and-shared-nodes)))


(def ^:private ^:dynamic ^HashSet *names* nil)
(def ^:private ^:dynamic ^HashMap *bindings* nil)
(def ^:private ^:dynamic ^clojure.lang.Symbol *package* nil)


(defmacro ^:private with-fresh-names [& body]
  `(binding [*names* (new HashSet)
             *bindings* (new HashMap)
             *package* (gensym "dar.container.state")]
     (.add *names* "state")
     (.add *names* "k")
     (.add *names* "parent")
     (.add *names* "graph")
     ~@body))


(defn- gen-name [kind k]
  (let [n (if (vector? k)
            (st/join "--" k)
            (str k))
        n (-> n (st/replace "." "-") (st/replace "/" "-") (st/replace ":" ""))
        parts (st/split n (re-pattern "-"))]
    (case kind
      :val n
      :lazy (str "lazy--" n)
      :fn (str "fn--" n)
      :root-fn (str "rfn--" n)
      :level-getter (str n "--get")
      :uses (str "uses--" n)
      :state-class (str *package* "." (apply str (map st/capitalize parts)) "State")
      :state-field (apply str (first parts) (map st/capitalize (next parts)))
      :state-field-ready (apply str "isReady" (map st/capitalize parts))
      (throw (IllegalArgumentException. (str "Unknown kind " k))))))


(defn- sym [kind k]
  (if-let [s (.get *bindings* [kind k])]
    s
    (let [name (gen-name kind k)
          s (if (.contains *names* name)
              (gensym name)
              (symbol name))]
      (.put *bindings* [kind k] s)
      s)))


(defn- debug [x]
  (println x)
  x)


(defn- gen-root-fn [graph level-key root]
  (let [{root-of ::root-of nodes ::nodes shared ::shared main :main args :args level-deps ::deps} (graph level-key)
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
                                                ['state `(let [s# (new ~(sym :state-class level-key))]
                                                           ~@(when (seq level-deps)
                                                               [`(set! (. s# ~'parent) ~'parent)])
                                                           ~@(map (fn [a]
                                                                    `(set! (. s# ~(sym :state-field a)) ~(sym :val a)))
                                                                  args)
                                                           s#)])

                                (= k ::parent) (do
                                                 (assert (not= root main))
                                                 ['parent `(. ~'state ~'parent)])

                                (= root (root-of k)) [(val-sym) (if lazy?
                                                                  `(fn [] ~(sym :val k))
                                                                  (let [exp (gen-fun-app k node)]
                                                                    (if (shared k)
                                                                      `(let [v# ~exp]
                                                                         (set! (. ~'state ~(sym :state-field k)) v#)
                                                                         v#)
                                                                      exp)))]

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
                     (next names-seq))

        arguments (if (= root main)
                    (let [args (map (partial sym :val) args)]
                      (if (seq level-deps)
                        (cons 'parent args)
                        args))
                    ['state])]

    `(~(sym :root-fn [level-key root]) [~@arguments]
       ~body)))


(defn- gen-level-getter [graph level-key]
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
                                                                                   (let [v# (~(sym :root-fn [level-key k]) ~'state)]
                                                                                     (set! (. ~'state ~(sym :state-field k)) v#)
                                                                                     (set! (. ~'state ~(sym :state-field-ready k)) true)
                                                                                     v#)))))

                            (level? (graph k)) (let [{args :args deps ::deps} (graph k)
                                                     args (map (partial sym :val) args)]
                                                 (conj-exp `(fn [~@args]
                                                              (~(sym :root-fn [level-key k]) ~@(if (seq deps)
                                                                                                 [`(fn [k#]
                                                                                                     (~(sym :level-getter level-key) ~'state k#))]
                                                                                                 nil)
                                                                ~@args))))
                            :else cases)))
                      []
                      (sort nodes))]
    `(~(sym :level-getter level-key) [~'state ~'k]
       (case ~'k
         ~@cases
         ~(if (seq level-deps)
            `((. ~'state ~'parent) ~'k)
            `(throw (IllegalArgumentException. (str "Unknown node " ~'k))))))))


(defn- gen-spec-bindings [graph]
  (let [bindings (new ArrayList)
        add (fn [s exp]
              (.add bindings s)
              (.add bindings exp))
        nodes (reduce (fn [ns l]
                        (let [{nodes ::nodes args :args} (graph l)]
                          (reduce conj ns (reduce disj nodes args))))
                      #{}
                      (::levels graph))]

    (doseq [k (sort nodes) :let [node (graph k)]]
      (cond
        (fun? node) (do
                      (add (sym :fn k) `(:fn (~k ~'graph)))
                      (when (and (:uses node)
                                 (some #{:eval} (:args node)))
                        (add (sym :uses k) `(:fn (~k ~'graph)))))

        (const? node) (add (sym :val k) `(:value (~k ~'graph)))))

    bindings))


(defn- gen-state-classes [graph]
  (for [k (::levels graph) :let [{args :args shared ::shared deps ::deps root-of ::root-of} (graph k)]]
    (let [fields (new ArrayList)]
      (when (seq deps)
        (.add fields {:flags #{:public :volatile}
                      :name  "parent"
                      :type  'clojure.lang.IFn}))

      (doseq [f (concat args shared)]
        (.add fields {:flags #{:public :volatile}
                      :name  (name (sym :state-field f))}))

      (doseq [[n root] root-of :when (= n root)]
        (.add fields {:flags #{:public :volatile}
                      :name  (name (sym :state-field-ready root))
                      :type  :boolean})
        (.add fields {:flags #{:public}
                      :name  (name (sym :state-field root))}))

      `(insn/define {:name   '~(sym :state-class k)
                     :fields ~(vec fields)}))))


(defn- gen-app [{levels ::levels :as graph}]
  `(fn [~'graph]
     (let [~@(gen-spec-bindings graph)]
       ~@(gen-state-classes graph)
       (letfn [~@(for [level-key levels :let [{root-of ::root-of} (graph level-key)]
                       [n root] root-of :when (= n root)]
                   (gen-root-fn graph level-key root))
               ~@(for [level-key levels]
                   (gen-level-getter graph level-key))]
         ~(sym :root-fn [::main-level (-> graph ::main-level :main)])))))


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
  (let [graph (-> app unwrap-app (analyze main args))
        factory (with-fresh-names
                  (eval (gen-app graph)))]
    (factory graph)))


(defn print-code [app main args]
  (let [graph (-> app unwrap-app (analyze main args))]
    (with-fresh-names
      (pp/with-pprint-dispatch pp/code-dispatch (pp/pprint
                                                  (gen-app graph))))))