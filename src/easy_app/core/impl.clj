(ns easy-app.core.impl
  (:refer-clojure :exclude [eval])
  (:require [easy-app.core.protocols :refer :all]
            [clojure.core.async :as async :refer [go go-loop <!]]))

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

(defn eval-all [container keys]
  (go-loop [ret []
           [k & ks] keys]
    (if (nil? k)
      ret
      (recur (conj ret (<! (eval container k)))
             ks))))

(defn eval* [container k]
  (let [{:keys [args fn async layer] :as spec} (-> container :fns (get k)) ;; TODO: assert that cell is defined
        this (find-layer container layer)
        cell (make-cell)
        state (get this :state)]
    (swap! state #(if (= Nil (get % k Nil))
                    (assoc % k cell)
                    %))
    (let [out (get @state k)]
      (when (identical? cell out)
        (go (let [val (apply fn (<! (eval-all this args)))]
              (swap! state assoc k val)
              (put! cell (if async
                           (<! val)
                           val)))))
      out)))

(extend-type Container
  IContainer
  (value [this k] (lookup this k))

  (eval [this k] (channel (let [val (lookup this k)]
                            (if (= Nil val)
                              (eval* this k)
                              val))))

  (start [this layer vals] (Container. (:fns this) (atom vals) this layer)))
