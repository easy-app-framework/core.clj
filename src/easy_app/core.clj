(ns easy-app.core
  (:refer-clojure :exclude [eval])
  (:require [easy-app.core.protocols :as p]
            [easy-app.core.impl :as impl]
            [clojure.core.async :as async]))

(def ^:dynamic *spec* (atom {:fns {} :vals {}}))

(defn make
  ([] (make @*spec*))
  ([{:keys [fns vals]}]
   (impl/->Container fns (atom (or vals {})) nil :app)))

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

(defn start
  ([app] (start app {}))
  ([app vals] (start app nil vals))
  ([app layer vals] (p/start app layer vals)))

(defn eval
  ([app key] (p/eval app key))
  ([app key val-cb]
   (-> (eval app key) (async/take! val-cb))))

(def stop! p/stop!)
(def value p/value)

(def Nil ::nil)

(comment
  (define :a "a")
  (define :b "b")

  (define :ab
    :args [:a :b]
    :fn #(str %1 %2))

  (def app (make))

  (<!! (eval app :ab)) ;; => "ab"
  (value app :ab) ;; => "ab"
  )
