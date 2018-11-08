(ns dar.container
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as st]
            [clojure.pprint :as pp]
            [clojure.set :refer [union]]
            [insn.core :as insn])
  (:import (java.util HashSet ArrayList HashMap)
           (clojure.lang RT)))


(set! *warn-on-reflection* true)


(def ^:private non-reserved-keyword
  (s/and keyword? #(not= % :eval)))


(s/def :dar.container.fun-node/fn ifn?)
(s/def :dar.container.fun-node/args (s/coll-of keyword? :kind vector?))
(s/def :dar.container.fun-node/lazy (s/coll-of non-reserved-keyword :kind set?))
(s/def :dar.container.fun-node/pre (s/coll-of non-reserved-keyword :kind vector?))
(s/def :dar.container.fun-node/uses (s/coll-of non-reserved-keyword :kind set?))
(s/def :dar.container.fun-node/close ifn?)
(s/def ::fun-node (s/keys :req-un [:dar.container.fun-node/fn]
                          :opt-un [:dar.container.fun-node/pre
                                   :dar.container.fun-node/args
                                   :dar.container.fun-node/lazy
                                   :dar.container.fun-node/uses
                                   :dar.container.fun-node/close]))


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
      (nil? node) (throw (ex-info (str "Node " k " is not defined") {::undefined-node k}))
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


(defn- replace-levels [graph f]
  (let [levels (::levels graph)
        m (reduce (fn [m k]
                    (if-let [rep (f graph k (graph k))]
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


(defn- nth-arg [^long i ^long n]
  (assert (< i n))
  (case n
    1 identity
    2 (case i
        0 (fn [x _] x)
        1 (fn [_ x] x))
    3 (case i
        0 (fn [x _ _] x)
        1 (fn [_ x _] x)
        2 (fn [_ _ x] x))
    4 (case i
        0 (fn [x _ _ _] x)
        1 (fn [_ x _ _] x)
        2 (fn [_ _ x _] x)
        3 (fn [_ _ _ x] x))
    (fn [& args]
      (assert (= (count args) n))
      (nth args i))))


(defn- replace-levels-with-seeded-main [graph k {main :main args :args}]
  (when-let [idx (last-index-of main args)]
    {:value (nth-arg idx (count args))}))


(defn- constantly-n-args [^long n v]
  (case n
    0 (fn [] v)
    1 (fn [_] v)
    2 (fn [_ _] v)
    3 (fn [_ _ _] v)
    4 (fn [_ _ _ _] v)
    5 (fn [_ _ _ _ _] v)
    (fn [& args]
      (assert (= (count args) n))
      v)))


(defn- replace-levels-with-constant-main [graph k {main :main args :args}]
  (when (and (= k ::main-level)
             (const? (graph main)))
    {:value (constantly-n-args (count args)
                               (:value (graph main)))}))


(defn- lazy-n-args [^long n]
  (case n
    0 (fn [f] f)
    1 (fn [f]
        (fn [_] (f)))
    2 (fn [f]
        (fn [_ _] (f)))
    3 (fn [f]
        (fn [_ _ _] (f)))
    4 (fn [f]
        (fn [_ _ _ _] (f)))
    5 (fn [f]
        (fn [_ _ _ _ _] (f)))
    (fn [f]
      (fn [& args]
        (assert (= (count args) n))
        (f)))))


(defn- replace-levels-with-foreign-main [graph k {main :main nodes ::nodes args :args}]
  (when-not (nodes main)
    {:fn (lazy-n-args (count args))
     :args [main]
     :lazy #{main}}))


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
  (let [{main :main nodes ::nodes seeds ::seeds} (graph level-key)

        add-shared (fn add-shared [^ArrayList shared deps]
                     (doseq [d deps :when (nodes d) :let [n (graph d)]]
                       (cond
                         (seeds d) (.add shared d)
                         (fun? n) (.add shared d)
                         (level? n) (add-shared shared (::deps n)))))

        grouped-fun-deps (fn [graph k node]
                           (let [strict (new ArrayList)
                                 lazy (new ArrayList)
                                 shared (new ArrayList)
                                 uses? (volatile! false)]

                             (doseq [d (:pre node) :when (and (nodes d)
                                                              (not (seeds d))
                                                              (fun? (graph d)))]
                               (.add strict d))

                             (doseq [a (:args node)]
                               (if (= a :eval)
                                 (vreset! uses? true)
                                 (when (and (nodes a)
                                            (not (seeds a)))
                                   (let [n (graph a)]
                                     (cond
                                       (fun? n) (if (contains? (:lazy node) a)
                                                  (.add lazy a)
                                                  (.add strict a))
                                       (level? n) (add-shared shared (::deps n)))))))

                             (when @uses?
                               (add-shared (:uses node)))

                             [strict lazy shared]))

        grouped-dependencies (fn [graph k]
                               (let [n (graph k)]
                                 (cond
                                   (seeds k) [nil nil nil]
                                   (fun? n) (grouped-fun-deps graph k n)
                                   (level? n) (let [shared (new ArrayList)]
                                                (add-shared shared (::deps n))
                                                [nil nil shared])
                                   :else [nil nil nil])))

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
                      (reduce (fn [[root kind shared? idx] [r-root r-kind r-idx]]
                                (let [shared? (or shared? (= r-kind :shared))]
                                  (if (< r-idx idx)
                                    [r-root r-kind shared? r-idx]
                                    [root kind shared? idx])))
                              [nil nil false Integer/MAX_VALUE]
                              rs))]

    (doseq [[k idx] (map vector sorted-nodes (range)) :let [rs (@requests k)
                                                            [min-req-root min-req-kind shared? _] (min-request rs)]]
      (if (seeds k)
        (when shared?
          (swap! shared conj k))
        (let [root (if (and (= min-req-kind :strict)
                            (or (= min-req-root main)
                                (= 1 (count (reduce (fn [roots [root _ _]]
                                                      (conj roots root))
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
            (acc-request d root :shared idx)))))

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
      (reduce-levels set-seeds)
      (reduce-levels derive-level-nodes-and-deps)
      (replace-levels replace-levels-with-seeded-main)
      (replace-levels replace-levels-with-constant-main)
      (replace-levels replace-levels-with-foreign-main)
      replace-ambiguous-constants-with-fns
      (reduce-levels derive-level-roots-and-shared-nodes)))


(def ^:private ^:dynamic ^HashSet *names* nil)
(def ^:private ^:dynamic ^HashMap *bindings* nil)
(def ^:private ^:dynamic ^clojure.lang.Symbol *package* nil)


(defmacro ^:private with-fresh-names [& body]
  `(binding [*names* (new HashSet)
             *bindings* (new HashMap)
             *package* (gensym "dar.container.app")]
     (.add *names* "state")
     (.add *names* "k")
     (.add *names* "v")
     (.add *names* "parent")
     (.add *names* "graph")
     ~@body))


(defn- name-parts [n]
  (st/split (str n) (re-pattern "-")))


(defn- sym [kind k]
  (if-let [s (.get *bindings* [kind k])]
    s
    (let [n (case kind
              :val (-> (str k) (st/replace ":" "") (st/replace "." "-") (st/replace "/" "-"))
              :lazy (str "lazy--" (sym :val k))
              :fn (str "fn--" (sym :val k))
              :uses (str "uses--" (sym :val k))
              :close (str "close--" (sym :val k))
              :level-getter (str (sym :val k) "--get")
              :root-fn (str "rfn--" (sym :val (first k)) "--" (sym :val (second k)))
              :state-class (str *package* "."
                                (if (= k ::main-level)
                                  "MainLevel"
                                  (apply str (map st/capitalize (name-parts (sym :val k)))))
                                "State")
              :state-field (let [parts (name-parts (sym :val k))]
                             (apply str (first parts) (map st/capitalize (next parts))))
              :state-field-ready (apply str "isReady" (map st/capitalize (name-parts (sym :val k))))
              (throw (IllegalArgumentException. (str "Unknown kind " kind))))

          s (if (.contains *names* n)
              (gensym n)
              (symbol n))]

      (.put *bindings* [kind k] s)
      s)))


(defn- gen-level-exp [parent-key level-key {level-args :args deps ::deps main :main}]
  (let [args (map (partial sym :val) level-args)
        rfn (sym :root-fn [level-key main])]
    (if (seq deps)
      `(fn [~@args]
         (~rfn (fn [~'k]
                 (~(sym :level-getter parent-key) ~'state ~'k))
           ~@args))
      rfn)))


(defn- state-closables [graph {root-of ::root-of shared ::shared main :main seeds ::seeds}]
  (set (concat (filter (fn [k]
                         (and (not= k main)
                              (:close (graph k)) ))
                       (vals root-of))
               (filter (fn [k]
                         (and (not= (root-of k) main)
                              (not (seeds k))
                              (:close (graph k))))
                       shared))))


(defn- gen-close-state-exp
  ([k]
   `(when (. ~'state ~(sym :state-field k))
      (~(sym :close k) (. ~'state ~(sym :state-field k)))))
  ([k1 k2 & ks]
   `(try
      ~(gen-close-state-exp k1)
      (finally
        ~(apply gen-close-state-exp k2 ks)))))


(defn- gen-close-state-fn [graph level-key {main :main nodes ::nodes seeds ::seeds} closables]
  `(fn [~(vary-meta 'state assoc :tag (sym :state-class level-key))]
     ~(apply gen-close-state-exp (filter closables
                                         (walk-dag graph
                                                   main
                                                   dependencies
                                                   (fn pre [s k]
                                                     (if (and (nodes k) (not (seeds k)))
                                                       s
                                                       (reduced s)))
                                                   conj
                                                   [])))))


(defmacro let-closable [close [n exp] & body]
  `(let [~n ~exp]
     (try
       ~@body
       (finally
         (~close ~n)))))


(defn- gen-root-fn [graph level-key root]
  (let [{root-of ::root-of nodes ::nodes shared ::shared main :main args :args seeds ::seeds level-deps ::deps :as level} (graph level-key)

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

        gen-name-exp-binding (fn [k lazy?]
                               (let [node (graph k)
                                     val-sym (fn []
                                               (sym (if lazy? :lazy :val) k))
                                     wrap-if-lazy (fn [exp]
                                                    (if lazy?
                                                      `(fn [] ~exp)
                                                      exp))]
                                 (cond
                                   (= k ::state) (let [_ (assert (= root main))
                                                       b ['state `(let [~'state (new ~(sym :state-class level-key))]
                                                                    ~@(when (seq level-deps)
                                                                        [`(set! (. ~'state ~'parent) ~'parent)])
                                                                    ~@(map (fn [a]
                                                                             `(set! (. ~'state ~(sym :state-field a)) ~(sym :val a)))
                                                                           (filter shared args))
                                                                    ~'state)]]
                                                   (if (seq (state-closables graph level))
                                                     `(let-closable ~(sym :close level-key) ~b)
                                                     `(let ~b)))

                                   (= k ::parent) (do
                                                    (assert (not= root main))
                                                    `(let [~'parent (. ~'state ~'parent)]))

                                   (= root (root-of k)) (let [closable? (and (not lazy?)
                                                                             (= root main)
                                                                             (not= k main)
                                                                             (:close node))
                                                              b [(val-sym) (if lazy?
                                                                             `(fn [] ~(sym :val k))
                                                                             (let [exp (gen-fun-app k node)]
                                                                               (if (shared k)
                                                                                 `(let [~'v ~exp]
                                                                                    (set! (. ~'state ~(sym :state-field k)) ~'v)
                                                                                    ~'v)
                                                                                 exp)))]]
                                                          (if closable?
                                                            `(let-closable ~(sym :close k) ~b)
                                                            `(let ~b)))

                                   (seeds k) `(let [~(val-sym) ~(wrap-if-lazy
                                                                  (if (= root main)
                                                                    (sym :val k)
                                                                    `(. ~'state ~(sym :state-field k))))])

                                   (shared k) `(let [~(val-sym) ~(wrap-if-lazy
                                                                   `(. ~'state ~(sym :state-field k)))])

                                   (nodes k) `(let [~(val-sym) ~(wrap-if-lazy
                                                                  (if (level? node)
                                                                    (gen-level-exp level-key k node)
                                                                    `(~(sym :level-getter level-key) ~'state ~k)))])

                                   :else `(let [~(val-sym) ~(wrap-if-lazy
                                                              `(~'parent ~k))]))))

        names-seq (walk-dag graph
                            [root false]
                            (fn children [graph [k lazy?]]
                              (let [node (graph k)]
                                (cond
                                  (= k ::state) nil
                                  (= k ::parent) nil

                                  (and (= root (root-of k)) (fun? node))
                                  (when-not lazy?
                                    (let [strict (new ArrayList)
                                          deferred (new ArrayList)
                                          lazy-set (:lazy node)]

                                      (doseq [d (:pre node) :when (and (not (seeds d))
                                                                       (not (and (nodes d)
                                                                                 (not (fun? (graph d))))))]
                                        (.add strict [d false]))

                                      (doseq [d (:args node)]
                                        (cond
                                          (= d :eval) (when (= root main)
                                                        (.add deferred [::state]))

                                          (and (= root main) (seeds d)) (when (contains? lazy-set d)
                                                                          (.add deferred [d true]))

                                          (and (not= root main) (seeds d)) (.add deferred [d (contains? lazy-set d)])

                                          (and (nodes d)
                                               (level? (graph d))) (.add deferred [d (contains? lazy-set d)])

                                          (not (const? (graph d))) (if (contains? lazy-set d)
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
                       `(~@(gen-name-exp-binding k lazy?) ~inner-form))
                     (sym :val root)
                     names-seq)

        arguments (if (= root main)
                    (let [args (map (partial sym :val) args)]
                      (if (seq level-deps)
                        (cons 'parent args)
                        args))
                    ['state])]

    `(~(sym :root-fn [level-key root]) [~@arguments]
       ~body)))


(defn- gen-level-getter [graph level-key]
  (let [{main :main root-of ::root-of level-deps ::deps shared ::shared nodes ::nodes} (graph level-key)
        cases (reduce (fn [cases k]
                        (let [conj-exp (fn [exp]
                                         (conj (conj cases k) exp))]
                          (cond
                            (shared k) (conj-exp `(. ~'state ~(sym :state-field k)))

                            (and (not= k main)
                                 (= (root-of k) k)) (conj-exp `(if (. ~'state ~(sym :state-field-ready k))
                                                                 (. ~'state ~(sym :state-field k))
                                                                 (locking ~'state
                                                                   (if (. ~'state ~(sym :state-field-ready k))
                                                                     (. ~'state ~(sym :state-field k))
                                                                     (let [~'v (~(sym :root-fn [level-key k]) ~'state)]
                                                                       (set! (. ~'state ~(sym :state-field k)) ~'v)
                                                                       (set! (. ~'state ~(sym :state-field-ready k)) true)
                                                                       ~'v)))))

                            (level? (graph k)) (conj-exp (gen-level-exp level-key k (graph k)))

                            :else cases)))
                      []
                      (sort nodes))]
    `(~(sym :level-getter level-key) [~(vary-meta 'state assoc :tag (sym :state-class level-key)) ~'k]
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
                        (add (sym :uses k) `(:fn (~k ~'graph))))
                      (when (:close node)
                        (add (sym :close k) `(:close (~k ~'graph)))))

        (const? node) (add (sym :val k) `(:value (~k ~'graph)))))

    bindings))


(defn- has-state? [level]
  (or (seq (::shared level))
      (> (count (set (vals (::root-of level))))
         1)))


(defn- gen-state-classes [graph]
  (for [level-key (::levels graph)
        :let [{main :main shared ::shared deps ::deps root-of ::root-of :as level} (graph level-key)]
        :when (has-state? level)]
    (let [fields (new ArrayList)]
      (when (seq deps)
        (.add fields {:flags #{:public :volatile}
                      :name  "parent"
                      :type  'clojure.lang.IFn}))

      (doseq [f shared]
        (.add fields {:flags #{:public :volatile}
                      :name  (name (sym :state-field f))
                      :type  Object}))

      (doseq [[n root] root-of :when (and (= n root) (not= root main))]
        (.add fields {:flags #{:public :volatile}
                      :name  (name (sym :state-field-ready root))
                      :type  :boolean})
        (.add fields {:flags #{:public}
                      :name  (name (sym :state-field root))
                      :type  Object}))

      {:name   (sym :state-class level-key)
       :fields (vec fields)})))


(defn- gen-app [graph]
  `(fn [~'graph]
     (let [~@(gen-spec-bindings graph)]
       (letfn [~@(let [fns (new ArrayList)
                       add #(.add fns %)]
                   (doseq [level-key (::levels graph) :let [level (graph level-key)]]
                     (doseq [[n root] (::root-of level) :when (= n root)]
                       (add (gen-root-fn graph level-key root)))

                     (when (has-state? level)
                       (add (gen-level-getter graph level-key)))

                     (let [closables (state-closables graph level)]
                       (when (seq closables)
                         (add (gen-close-state-fn graph level-key level closables)))))
                   fns)]
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
                  (let [loader (RT/makeClassLoader)]
                    (doseq [c (gen-state-classes graph)]
                      (insn/define loader c)))
                  (eval (gen-app graph)))]
    (factory graph)))


(defn print-code [app main args]
  (let [graph (-> app unwrap-app (analyze main args))]
    (with-fresh-names
      (pp/with-pprint-dispatch pp/code-dispatch (pp/pprint
                                                  `(do
                                                     ~@(map (fn [c]
                                                              `(insn/define ~c))
                                                            (gen-state-classes graph))
                                                     ~(gen-app graph)))))))