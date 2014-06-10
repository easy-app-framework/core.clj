(ns dar.container.macro
  "DSL for ClojureScript version")

(defmacro define [& args]
  `(do
     (when-not ~'*dar-container-spec*
       (def ~'*dar-container-spec* (atom {})))
     (swap! ~'*dar-container-spec* #(dar.container/define* % ~@args))))

(defn- spec-symbol [ns-quote]
  (symbol (name (second ns-quote)) "*dar-container-spec*"))

(defmacro include [& namespaces]
  `(do
     ~@(for [n namespaces]
         `(swap! ~'*dar-container-spec* #(merge % ~(spec-symbol n))))))

(defmacro make
  ([]
   `(dar.container/make* @~'*dar-container-spec*))
  ([ns]
   `(dar.container/make* @~(spec-symbol ns))))
