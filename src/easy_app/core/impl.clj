(ns easy-app.core.impl
  (:refer-clojure :exclude [eval])
  (:require [easy-app.core.protocols :refer :all]
            [clojure.core.async :as async :refer [go <!]])
  (:import (java.lang Throwable)))

(defprotocol IChannel
  "An object convertable to core.async channel"
  (channel [this]))

(extend-protocol IChannel
  java.lang.Object
  (channel [this] (doto (async/chan)
                    (async/put! this)))
  nil
  (channel [_] (doto (async/chan)
                 (async/close!))))

(defprotocol ICell
  (put! [this val]))

(defrecord CellState [value has-value channels])

(deftype Cell [state]
  ICell
  (put! [this v]
    (swap! state assoc :value v :has-value true)
    (doseq [ch (:channels @state)]
      (async/put! ch v)))

  IChannel
  (channel [this] (let [ret (async/chan (async/dropping-buffer 1))] ;; TODO: see if we can use unbuffered channel here
                    (swap! state (fn [state]
                                   (when (:has-value state)
                                     (async/put! ret (:value state))) ;; side effects should not have any impact
                                   (if (:has-value state)
                                     state
                                     (update-in state [:channels] conj ret))))
                    ret)))

(defn make-cell []
  (Cell. (atom (CellState. nil false []))))

(def Nil :easy-app.core/nil)

(defrecord Container [fns state parent layer])

(defn lookup [{:keys [state parent]} k]
  (let [val (get @state k Nil)]
    (if (and parent (= Nil val))
      (recur parent k)
      val)))

(defn find-layer [container layer]
  (if (and layer (not= layer (:layer container)))
    (recur (:parent container) layer)
    container))

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

(defn eval-all [container keys]
  (go* (loop [ret []
              [k & ks] keys]
         (if (nil? k)
           ret
           (recur (conj ret (<? (eval container k)))
                  ks)))))

(defn eval* [container k]
  (go* (let [{:keys [args fn async layer] :as spec} (-> container :fns (get k))
             this (find-layer container layer)
             cell (make-cell)
             state (get this :state)]

         (when-not spec
           (throw (ex-info (str "Cell " k " is not defined")
                           {:easy-app.core/container this})))

         (swap! state #(if (= Nil (get % k Nil))
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
                                                      {:easy-app.core/container this
                                                       :easy-app.core/cell k}
                                                      ex)))))))]
               (swap! state assoc k ret)
               (put! out ret)))
           (<! (channel out))))))

(extend-type Container
  IContainer
  (value [this k] (lookup this k))

  (eval [this k] (let [val (lookup this k)]
                   (if (= Nil val)
                     (eval* this k)
                     (channel val))))

  (start [this layer vals] (Container. (:fns this) (atom vals) this layer)))
