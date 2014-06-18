(ns dar.container)

(defrecord Container [fns state parent level])

(defrecord Fn [fn args pre level])

(defn start
  ([container level vals]
   (->Container (:fns container) (atom vals) container level))
  ([container vals]
   (start container nil vals))
  ([container]
   (start container {})))

(defn stop! [container])

(defn- lookup [container k]
  (let [parent (:parent container)
        vals @(:state container)
        val (get vals k ::nil)]
    (if (and parent (= ::nil val))
      (recur parent k)
      val)))

(defn- find-level [container level]
  (if (and level (not= level (:level container)))
    (recur (:parent container) level)
    container))

(declare do-eval)

(defn eval [this k]
  (let [val (lookup this k)]
    (if (= ::nil val)
      (do-eval this k)
      val)))

(defn- do-eval [container k]
  (let [{:keys [args pre fn level] :as spec} (-> container :fns (get k))
        this (find-level container level)
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

(def ^:dynamic *get-app* nil)

(defn define [& args]
  (apply swap! (*get-app*) define* args))

(defn include [app]
  (swap! (*get-app*) include* app))
