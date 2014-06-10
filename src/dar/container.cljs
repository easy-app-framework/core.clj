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

    (let [ret (apply fn (map #(eval this %) args))]
      (swap! state assoc k ret)
      ret)))

;;
;; Spec API
;;

(defn- fn-entry? [[k v]]
  (instance? Fn v))

(defn make* [spec]
  (map->Container {:fns (into {} (filter fn-entry? spec))
                   :state (atom (into {} (filter (complement fn-entry?) spec)))
                   :level :app}))

(defn- noop [& _])

(defn define*
  ([spec k v]
   (assoc spec k v))
  ([spec k opt-k opt-v & {:as opts}]
   (assoc spec k (map->Fn (merge {:args [] :fn noop}
                                 (assoc opts opt-k opt-v))))))
