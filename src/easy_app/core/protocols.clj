(ns easy-app.core.protocols
  (:refer-clojure :exclude [eval]))

(defprotocol IContainer
  (value [this k])
  (eval [this k])
  (start [this layer vals])
  (stop! [this]))

(defprotocol IDisposable
  (dispose [this]))

(extend-protocol IDisposable
  java.lang.AutoClosable
  (dispose [this] (.close this)))
