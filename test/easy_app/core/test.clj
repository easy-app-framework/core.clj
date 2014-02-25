(ns easy-app.core.test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async :refer [>!! <!!]]
            [easy-app.core :as c :refer [define]]))

(defmacro defapp [& body]
  `(binding [c/*spec* (atom {:fns {} :vals {}})]
     ~@body
     (c/make)))

(def ^:dynamic *app* nil)

(defmacro with-app [app & body]
  `(binding [*app* ~app]
     ~@body))

(defn eval [k]
  (<!! (c/eval *app* k)))

(defn value [k]
  (c/value *app* k))

(deftest basic
  (with-app (defapp
              (define :a "a")
              (define :b "b")
              (define :ab
                :args [:a :b]
                :fn str))
    (is (= (value :a) "a"))
    (is (= (value :ab) c/Nil))
    (is (= (eval :a) "a"))
    (is (= (eval :ab) "ab"))
    (is (= (value :ab) "ab"))))

(deftest async-function
  (let [ch (async/chan)]
    (with-app (defapp
                (define :a
                  :async true
                  :fn (fn [] ch))
                (define :ab
                  :args [:a]
                  :fn (fn [a]
                        (str a "b"))))
      (c/eval *app* :ab)
      (>!! ch "a")
      (is (= "ab" (eval :ab))))))

(deftest layers
  (with-app (defapp
              (define :a
                :layer :app
                :fn (fn [] "a"))
              (define :b
                :fn (fn [] "b")))
    (with-app (c/start *app*)
      (is (= "a" (eval :a)))
      (is (= "b" (eval :b))))
    (is (= "a" (value :a)))
    (is (= c/Nil (value :b)))))
