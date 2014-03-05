(ns easy-app.core.sync
  "Simplistic, fully sync implementation of eval function
  as a performance reference"
  (:refer-clojure :exclude [eval]))

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
  (let [{:keys [args fn async level] :as spec} (-> container :fns (get k))
        this (find-level container level)
        state (get this :state)]

    (when-not spec
      (throw (IllegalArgumentException. (str "Cell " k " is not defined"))))

    (let [ret (apply fn (map #(eval this %) args))]
      (swap! state assoc k ret)
      ret)))
