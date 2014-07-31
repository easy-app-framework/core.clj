(ns dar.container.sync
  "Simplistic, fully sync implementation of eval function
  as a performance reference")

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

(declare do-eval do-eval-fn)

(defn evaluate [this k]
  (let [val (lookup this k)]
    (if (= ::nil val)
      (do-eval this k)
      val)))

(defn- do-eval [app k]
  (if-let [task (-> app :spec (get k))]
    (let [v (get task :value ::nil)]
      (if (= v ::nil)
        (do-eval-fn app k task)
        (do
          (swap! (:state app) assoc k v)
          v)))
    (IllegalArgumentException. (str "Task " k " is not defined"))))

(defn- do-eval-fn [container k {f :fn :keys [args pre level]}]
  (let [this (find-level container level)
        state (get this :state)]
    (let [ret (apply f (map #(evaluate this %) args))]
      (swap! state assoc k ret)
      ret)))
