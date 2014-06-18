(ns dar.container
  (:refer-clojure :exclude [eval promise])
  (:require [dar.async :refer :all]
            [dar.async.promise :refer :all]))

(comment
  (define :a "a")
  (define :b "b")

  (define :ab
    :args [:a :b]
    :fn #(str %1 %2))

  (<?! (eval (ns-app) :ab)) ;; => "ab"
  )

(defrecord App [fns state parent level])

(defrecord Fn [fn args pre level])

(defn start
  ([app level vals]
   (->App (:fns app) (atom vals) app level))
  ([app vals]
   (start app nil vals))
  ([app]
   (start app {})))

(defn stop! [app])

(defn- lookup [app k]
  (let [parent (:parent app)
        vals @(:state app)
        val (get vals k ::nil)]
    (if (and parent (= ::nil val))
      (recur parent k)
      val)))

(defn- find-level [app level]
  (if level
    (if-let [this (loop [app app]
                    (if (and app (= (:level app) level ))
                      app
                      (recur (:parent app))))]
      this
      app)
    app))

(declare do-eval)

(defn eval [this k]
  (let [val (lookup this k)]
    (if (= ::nil val)
      (do-eval this k)
      val)))

(defn- async-map [f coll]
  (go
    (loop [ret []
           coll coll]
      (if (seq coll)
        (recur (conj ret (<? (f (first coll))))
          (next coll))
        ret))))

(defn- eval-args [app args]
  (async-map #(if (= ::self %)
                app
                (eval app %))
    args))

(defn- do-eval [app k]
  (let [{:keys [args pre fn level] :as spec} (-> app :fns (get k))
        this (find-level app level)
        p (new-promise)
        state (get this :state)]
    (if-not spec
      (IllegalArgumentException. (str "Task " k " is not defined"))
      (do
        (swap! state #(if (= ::nil (get % k ::nil))
                        (assoc % k p)
                        %))
        (let [out (get @state k)]
          (when (identical? p out)
            (then (go
                    (when pre
                      (<? (eval-args this pre)))
                    (let [args (<? (eval-args this args))]
                      (try
                        (<? (apply fn args))
                        (catch Throwable ex
                          (throw (ex-info (str "Failed to evaluate " k)
                                   {::level (:level this)
                                    ::task k}
                                   ex))))))
              #(do
                 (swap! state assoc k %)
                 (deliver! p %))))
          out)))))

;;
;; Spec API
;;

(defn- noop [& _])

(defn define*
  ([app k v]
   (update-in app [:state] (fn [s]
                             (atom (assoc @s k v)))))
  ([app k opt-k opt-v & {:as opts}]
   (-> app
     (assoc-in [:fns k] (map->Fn (merge {:args [] :fn noop}
                                   (assoc opts opt-k opt-v))))
     (update-in [:state] #(atom @%)))))

(defn include* [app {fns :fns s :state}]
  (-> app
    (update-in :fns merge fns)
    (update-in :state #(atom (merge @% @s)))))

;;
;; Implicit per-namespace app
;;

(defn- var-get* [ns var-name]
  (let [s (symbol (name ns) (name var-name))]
    (when-let [var (find-var s)]
      (var-get var))))

(defn ns-app
  ([]
   (ns-app (ns-name *ns*)))
  ([ns]
   (var-get* ns '*dar-app*)))

(defn load-ns-app [ns]
  (require ns)
  (ns-app ns))

(defn declare-app []
  (when-not (ns-app)
    (.setDynamic (intern *ns*
                   '*dar-app*
                   (atom (->App {} (atom {}) nil :app))))))
;;
;; DSL
;;

(def ^:dynamic *get-app* (fn []
                           (declare-app)
                           (ns-app)))

(defn define [& args]
  (apply swap! (*get-app*) define* args))

(defn include [ns]
  (swap! (*get-app*) include* (if (symbol ns)
                                (load-ns-app ns)
                                ns)))

(defmacro defapp [& body]
  `(let [app# (atom (->App {} (atom {}) nil :app))]
     (binding [*get-app* (fn [] app#)]
       ~@body
       @app#)))
