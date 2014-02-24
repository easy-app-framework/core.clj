(ns easy-app.api
  (:require [easy-app.core :as core]))

(def ^:dynamic *spec* (atom {:fns {} :vals {}}))

(defn make-app
  ([] (make-app @*spec*))
  ([{:keys [fns vals]}]
   (core/->Container fns (atom (or vals {})) nil :app)))

(defn start
  ([app] (start app {}))
  ([app vals] (start app nil vals))
  ([app layer vals] (core/start app layer vals)))

(def stop! core/stop!)

(def evaluate core/evaluate)

(defn- define-fn [key & {:keys [args] :as opts}]
  ;; TODO: may be support :pre, :post actions
  ;; or replace :deps with :args in the core
  (swap! *spec* assoc-in [:fns key] (assoc opts :deps args)))

(defmacro define
  ([key val]
   `(swap! *spec* assoc-in [:vals ~key] ~val))
  ([key opt val & opts]
   `(define-fn key ~opt ~val ~@opts)))

(comment
  (define ::connection "http://hello.world")

  (define ::db
    :layer :app
    :async true
    :args [::connection]
    :fn (fn [s]
          (db/connect s)))
  )
