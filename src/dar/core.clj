(ns dar.core
  (:refer-clojure :exclude [eval promise])
  (:require [dar.async :refer :all]
            [dar.async.promise :refer :all])
  (:import (java.lang Throwable IllegalArgumentException)))

(comment
  (define :a "a")
  (define :b "b")

  (define :ab
    :args [:a :b]
    :fn #(str %1 %2))

  (def app (make))

  (<?! (eval app :ab)) ;; => "ab"
  )

(defrecord Container [fns state parent level])

(defrecord Fn [fn args async level])

(defn start
  ([container level vals]
   (->Container (:fns container) (atom vals) container level))
  ([container vals]
   (start container nil vals))
  ([container]
   (start container {})))

(defn stop! [container])

(defn- lookup [container k]
  (let [parent (:parent container)
        vals @(:state container)
        val (get vals k ::nil)]
    (if (and parent (= ::nil val))
      (recur parent k)
      val)))

(defn- find-level [container level]
  (if (and level (not= level (:level container)))
    (recur (:parent container) level)
    container))

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

(defn- eval-args [container args]
  (async-map #(if (= ::self %)
                container
                (eval container %))
             args))

(defn- do-eval [container k]
  (let [{:keys [args fn async level] :as spec} (-> container :fns (get k))
        this (find-level container level)
        p (make-promise)
        state (get this :state)]

    (if-not spec
      (IllegalArgumentException. (str "Cell " k " is not defined"))
      (do
        (swap! state #(if (= ::nil (get % k ::nil))
                        (assoc % k p)
                        %))
        (let [out (get @state k)]
          (when (identical? p out)
            (then (go (let [args (<? (eval-args this args))]
                        (try
                          (<? (apply fn args))
                          (catch Throwable ex
                            (throw (ex-info (str "Failed to evaluate " k)
                                            {::level (:level this)
                                             ::cell k}
                                            ex))))))
                  #(do
                     (swap! state assoc k %)
                     (fulfill p %))))
          out)))))

;;
;; Spec API
;;

(defn- fn-entry? [[k v]]
  (instance? Fn v))

(defn make* [spec]
  (map->Container {:fns (into {} (filter fn-entry? spec))
                   :state (atom (into {} (filter (complement fn-entry?) spec)))
                   :level :app}))

(defn define*
  ([spec k v]
   (assoc spec k v))
  ([spec k opt-k opt-v & {:as opts}]
   (assoc spec k (map->Fn (merge {:args []}
                                 (assoc opts opt-k opt-v))))))

;;
;; Implicit per-namespace container
;;

(defn- var-get* [ns var-name]
  (let [s (symbol (name ns) (name var-name))]
    (when-let [var (find-var s)]
      (var-get var))))

(defn- get-ns-spec*
  ([]
   (get-ns-spec* (ns-name *ns*)))
  ([ns]
   (var-get* ns '*dar-core-spec*)))

(defn get-ns-spec [& args]
  (when-let [spec (apply get-ns-spec* args)]
    @spec))

(defn load-ns-spec [ns]
  (require ns)
  (get-ns-spec ns))

(defn declare-spec []
  (when-not (get-ns-spec)
    (.setDynamic (intern *ns*
                         (with-meta '*dar-core-spec* {:private true})
                         (atom {})))))
;;
;; DSL
;;

(defn make []
  (make* (or (get-ns-spec) {})))

(defn define [& args]
  (declare-spec)
  (swap! (get-ns-spec*) #(apply define* % args)))

