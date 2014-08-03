(ns dar.container
  (:require [dar.async.promise :refer :all]))

(comment
  (application app)

  (define :a "a")

  (define :b "b")

  (define :ab
    :args [:a :b]
    :fn #(str %1 %2))

  (evaluate (start app) :ab) ;; => "ab"
  )

(defrecord App [spec state parent level stopped])

(defrecord Fn [fn args pre level])

(defrecord Value [value])

(defn start
  ([app level vals]
   (if (instance? App app)
     (->App (:spec app) (atom vals) app level (new-promise))
     (->App app (atom vals) nil level (new-promise))))
  ([app vals]
   (start app nil vals))
  ([app]
   (start app {})))

(defn stop! [app]
  (deliver! (:stopped app) true))

(defn- lookup [app k]
  (let [parent (:parent app)
        vals @(:state app)
        val (get vals k ::nil)]
    (if (and (= ::nil val) parent)
      (recur parent k)
      val)))

(defn- find-level [app level]
  (loop [this app]
    (cond
      (nil? level) app
      (nil? this) app
      (= (:level this) level) this
      :else (recur (:parent this)))))

(declare do-eval do-eval-fn)

(defn evaluate [this k]
  (let [val (lookup this k)]
    (if (= ::nil val)
      (do-eval this k)
      val)))

(defn- async-reduce
  ([xs] (async-reduce (fn [_ x] x) nil xs))
  ([f init xs] (async-reduce f init xs nil nil))
  ([f init xs p current]
   (if (seq xs)
     (let [x (first xs)]
       (if (delivered? x)
         (let [v (value x)]
           (if (instance? Throwable v)
             (if p (deliver! p v)  v)
             (recur f (f init v) (next xs) p current)))
         (let [current (or current (atom nil))
               p (or p (new-promise (fn [_]
                                      (when-let [x @current]
                                        (abort! x)))))]
           (reset! current x)
           (then x (fn [v]
                     (reset! current nil)
                     (if (instance? Throwable v)
                       (deliver! p v)
                       (async-reduce f (f init v) (next xs) p current))))
           p)))
     (if p
       (deliver! p init)
       init))))

(defn- do-eval [app k]
  (if-let [task (-> app :spec (get k))]
    (let [v (get task :value ::nil)]
      (if (= v ::nil)
        (do-eval-fn app k task)
        (do
          (swap! (:state app) assoc k v)
          v)))
    (IllegalArgumentException. (str "Task " k " is not defined"))))

(defmacro ^:private steps
  ([] `nil)
  ([form & rest]
   `(lazy-seq
      (cons ~form (steps ~@rest)))))

(defn- do-eval-fn [app k {f :fn :keys [args pre level close]}]
  (let [this (find-level app level)
        state (:state this)
        aborted (new-promise)
        p (new-promise (fn [_]
                         (deliver! aborted true)))]

    (swap! state #(if (= (get % k ::nil) ::nil)
                    (assoc % k p)
                    %))

    (let [out (get @state k)]
      (when (identical? p out)
        (let [arguments (object-array (count args))
              wrap-error? (atom false)
              job (async-reduce
                    (concat
                      (when (seq pre)
                        (map #(evaluate this %) pre))
                      (steps
                        (async-reduce (fn [idx v]
                                        (aset arguments idx v)
                                        (inc idx))
                          0
                          (map #(if (= % ::app)
                                  this
                                  (evaluate this %))
                            args))
                        (try
                          (reset! wrap-error? true)
                          (apply f arguments)
                          (catch Throwable e
                            e)))))]
          (then aborted (fn [_] (abort! job)))
          (then job
            (fn [v]
              (let [ret (if (and (instance? Throwable v) @wrap-error?)
                          (ex-info (str "Failed to evaluate " k)
                            {::level (:level this)
                             ::task k}
                            v)
                          (do
                            (when close
                              (then (:stopped this)
                                (fn [_]
                                  (try
                                    (close v)
                                    (catch Throwable e
                                      (println e))))))
                            v))]
                (swap! state assoc k ret)
                (deliver! out ret))))))
      (if (delivered? out)
        (value out)
        out))))

;;
;; Spec API
;;

(defn define*
  ([spec k v]
   (assoc spec k (->Value v)))
  ([spec k opt-k opt-v & {:as opts}]
   (assoc spec k (map->Fn (merge
                            {:args [] :fn (fn noop [& _])}
                            (assoc opts opt-k opt-v))))))

(defn spec-var-atom []
  (var-get
    (or
      (find-var (symbol
                  (name (ns-name *ns*))
                  (name '*dar-container-spec*)))
      (intern *ns*
        (with-meta '*dar-container-spec* {:private true})
        (atom nil)))))

(def ^:dynamic ^:private *app* nil)

(defn swap [f & args]
  (if *app*
    (apply swap! *app* f args)
    (let [v @(spec-var-atom)]
      (apply alter-var-root v f args)))
  nil)

(defn define [& args]
  (apply swap define* args))

(defn include [app]
  (swap merge app))

(defmacro with-app [app & body]
  `(binding [*app* (atom ~app)]
     ~@body
     @*app*))

(defmacro application [name]
  `(reset! (spec-var-atom)
     (def ~name {})))

(defmacro defapp [name & body]
  `(def ~name
     (with-app {}
       ~@body)))
