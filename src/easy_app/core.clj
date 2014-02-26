(ns easy-app.core
  (:refer-clojure :exclude [eval])
  (:require [clojure.core.async :as async :refer [go <!]]
            [easy-app.core.cell :as c]
            [easy-app.core.channel :refer [channel]])
  (:import (java.lang Throwable)))

(comment
  (define :a "a")
  (define :b "b")

  (define :ab
    :args [:a :b]
    :fn #(str %1 %2))

  (def app (make))

  (value app :ab) ;; => ::nil
  (<!! (eval app :ab)) ;; => "ab"
  (value app :ab) ;; => "ab"
  )

(def ^:dynamic *spec* (atom {:fns {} :vals {}}))

(defrecord Container [fns state parent level])

(defn make
  ([] (make @*spec*))
  ([{:keys [fns vals]}]
   (map->Container {:fns fns
                    :state (atom (or vals {}))
                    :level :app})))

(defn define
  ([key val]
   (swap! *spec* #(-> %
                      (assoc-in [:vals key] val)
                      (update-in [:fns] dissoc key))))

  ([key opt-key opt-val & {:as opts}]
   (let [opts (assoc opts opt-key opt-val)
         opts (merge {:args []} opts)]
     (swap! *spec* #(-> %
                        (assoc-in [:fns key] opts)
                        (update-in [:vals] dissoc key))))))

(defn value [container k]
  (let [parent (:parent container)
        vals @(:state container)
        val (get vals k ::nil)]
    (if (and parent (= ::nil val))
      (recur parent k)
      val)))

(defn find-level [container level]
  (if (and level (not= level (:level container)))
    (recur (:parent container) level)
    container))

(defn start
  ([container level vals]
   (->Container (:fns container) (atom vals) container level))
  ([container vals]
   (start container nil vals))
  ([container]
   (start container {})))

(defmacro <? [ch]
  `(let [val# (async/<! ~ch)]
     (when (instance? Throwable val#)
       (throw val#))
     val#))

(defmacro go* [& body]
  `(go (try
         ~@body
         (catch Throwable ex#
           ex#))))

(declare eval)

(defn- eval-all [container keys]
  (go* (loop [ret []
              [k & ks] keys]
         (if (nil? k)
           ret
           (recur (conj ret (<? (eval container k)))
                  ks)))))

(defn- do-eval [container k]
  (go* (let [{:keys [args fn async level] :as spec} (-> container :fns (get k))
             this (find-level container level)
             cell (c/make-cell)
             state (get this :state)]

         (when-not spec
           (throw (ex-info (str "Cell " k " is not defined")
                           {::container this})))

         (swap! state #(if (= ::nil (get % k ::nil))
                         (assoc % k cell)
                         %))

         (let [out (get @state k)]
           (when (identical? cell out)
             (let [ret (<! (go* (let [args (<? (eval-all this args))]
                                  (try
                                    (if async
                                      (<? (apply fn args))
                                      (apply fn args))
                                    (catch Throwable ex
                                      (throw (ex-info (str "Failed to evaluate " k)
                                                      {::container this
                                                       ::cell k}
                                                      ex)))))))]
               (swap! state assoc k ret)
               (c/deliver cell ret)))
           (<! (channel out))))))

(defn eval
  ([this k]
   (let [val (value this k)]
     (if (= ::nil val)
       (do-eval this k)
       (channel val))))
  ([this k cb]
   (-> (eval this k) (async/take! cb)))
  ([this k on-success on-failure]
   (eval this k #(if (instance? Throwable %)
                   (on-failure %)
                   (on-success %)))))
