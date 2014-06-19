(ns dar.container
  (:refer-clojure :exclude [eval]))

(defrecord App [fns state parent level])

(defrecord Fn [fn args pre level])

(defn start
  ([app level vals]
   (->App (:fns app) (atom vals) app level))
  ([app vals]
   (start app nil vals))
  ([app]
   (start app {})))

(defn stop! [app])

(defn- lookup [app k]
  (let [parent (:parent app)
        vals @(:state app)
        val (get vals k ::nil)]
    (if (and parent (= ::nil val))
      (recur parent k)
      val)))

(defn- find-level [app level]
  (if (and level (not= level (:level app)))
    (recur (:parent app) level)
    app))

(declare do-eval)

(defn eval [this k]
  (let [val (lookup this k)]
    (if (= ::nil val)
      (do-eval this k)
      val)))

(defn- do-eval [app k]
  (let [{:keys [args pre fn level] :as spec} (-> app :fns (get k))
        this (find-level app level)
        state (get this :state)]

    (when-not spec
      (throw (js/Error. (str "Task " k " is not defined"))))

    (doseq [task pre]
      (eval this task))

    (let [ret (apply fn (doall (for [arg args]
                                 (eval this arg))))]
      (swap! state assoc k ret)
      ret)))

;;
;; Spec API
;;

(defn- noop [& _])

(defn define*
  ([app k v]
   (update-in app [:state] (fn [s]
                             (atom (assoc @s k v)))))
  ([app k opt-k opt-v & {:as opts}]
   (-> app
     (assoc-in [:fns k] (map->Fn (merge {:args [] :fn noop}
                                   (assoc opts opt-k opt-v))))
     (update-in [:state] #(atom @%)))))

(defn include* [app {fns :fns s :state}]
  (-> app
    (update-in :fns merge fns)
    (update-in :state #(atom (merge @% @s)))))

;;
;; DSL
;;

(def ^:dynamic *app* nil)

(defn define [& args]
  (apply swap! *app* define* args))

(defn include [app]
  (swap! *app* include* app))
