(ns easy-app.core
  (:require [clojure.core.async :as async :refer [>! <! go go-loop]]))

(defprotocol IContainer
  (value [this k])
  (evaluate [this k])
  (start [this layer vals])
  (stop! [this]))

(defprotocol ICell
  (put! [this ch]))

(defprotocol IChannel
  (channel [this]))

(extend-protocol IChannel
  java.lang.Object
  (channel [this] (doto (async/chan)
                    (async/put! this)))
  nil
  (channel [_] (doto (async/chan)
                 (async/close!))))

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
  (Cell. (->CellState nil false [])))

(defn- lookup [{:keys [state parent]} k]
  (let [val (get @state k ::nil)]
    (if (and parent (= ::nil val))
      (recur parent k)
      val)))

(defn- find-layer [this layer]
  (if (and layer (not= layer (:layer this)))
    (recur (:parent this) layer)
    this))

(defn- eval-all [this keys]
  (go-loop [ret []
           [k & ks] keys]
    (if (nil? k)
      ret
      (recur (conj ret (<! (evaluate this k)))
             ks))))

(defn- eval* [this k]
  (let [{:keys [deps fn async layer] :as spec} (-> this :spec k) ;; TODO: assert that cell is defined
        this (find-layer this layer)
        cell (make-cell)
        state (get this :state)]
    (swap! state #(if (= ::nil (get % k ::nil))
                    %
                    (assoc % k cell)))
    (let [out (get @state k)]
      (when (identical? cell out)
        (go (let [val (apply fn (<! (eval-all this deps)))]
              (swap! state assoc k val)
              (put! cell (if async
                           (<! val)
                           val)))))
      out)))

(defrecord Container [spec state parent layer]
  IContainer
  (value [this k] (lookup this k))

  (evaluate [this k] (channel (let [val (lookup this k)]
                                (if (= ::nil val)
                                  (eval* this k)
                                  val))))

  (start [this layer vals] (Container. spec (atom vals) this layer)))
