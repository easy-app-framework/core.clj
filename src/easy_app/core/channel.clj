(ns easy-app.core.channel
  (:require [clojure.core.async :as async]))

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
