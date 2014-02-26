(ns easy-app.core.cell
  (:refer-clojure :exclude [deliver])
  (:require [clojure.core.async :as async]
            [easy-app.core.channel :refer :all]))

(defprotocol ICell
  (deliver [this val]))

(defrecord State [value has-value channels])

(deftype Cell [state]
  ICell
  (deliver [this v]
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
  (Cell. (atom (->State nil false []))))
