(ns easy-app.core.test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async :refer [>!! <!!]]
            [easy-app.core :as c :refer [define]])
  (:import (java.lang Exception)))

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

(deftest basic
  (with-app (defapp
              (define :a "a")
              (define :b "b")
              (define :ab
                :args [:a :b]
                :fn str))
    (is (= (eval :a) "a"))
    (is (= (eval :ab) "ab"))))

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

(deftest levels
  (let [a-times (atom 0)
        b-times (atom 0)]
    (with-app (defapp
                (define :a
                  :level :app
                  :fn (fn []
                        (swap! a-times inc)
                        "a"))
                (define :b
                  :fn (fn []
                        (swap! b-times inc)
                        "b")))
      (with-app (c/start *app*)
        (is (= "a" (eval :a)))
        (is (= "b" (eval :b))))
      (with-app (c/start *app*)
        (is (= "a" (eval :a)))
        (is (= "b" (eval :b))))
      (is (= 1 @a-times))
      (is (= 2 @b-times)))))

(deftest error-handling
  (let [ex (Exception. "hello")]
    (testing "Should catch cell errors and wrap them with relevant info"
      (with-app (defapp
                  (define :sync
                    :fn #(throw ex))

                  (define :async
                    :async true
                    :fn #(doto (async/chan)
                           (async/put! ex)))

                  (define :cell
                    :args [:async]
                    :fn (fn [_] (throw (Exception. "Should not reach here")))))

        (testing "Sync case"
          (let [res (eval :sync)]
            (is (= (ex-data res) {::c/container *app* ::c/cell :sync}))
            (is (identical? ex (.getCause res)))))

        (testing "Async case"
          (let [res (eval :async)]
            (is (= (ex-data res) {::c/container *app* ::c/cell :async}))
            (is (identical? ex (.getCause res)))))

        (testing "Should pass errors from dependencies as-is"
          (let [res (eval :cell)]
            (is (= :async (-> res ex-data ::c/cell)))))))

    (testing "Should not attempt to evaluate cells with errors twice"
      (let [a-times (atom 0)
            ab-times (atom 0)]
        (with-app (defapp
                    (define :a
                      :fn (fn []
                            (swap! a-times inc)
                            (throw ex)))
                    (define :ab
                      :args [:a]
                      :fn (fn [a]
                            (swap! ab-times inc)
                            (str a "b"))))
          (let [res1 (eval :ab)
                res2 (eval :ab)]
            (is (identical? res1 res2))
            (is (= @a-times 1))
            (is (= @ab-times 0))))))))
