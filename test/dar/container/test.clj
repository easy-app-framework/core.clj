(ns dar.container.test
  (:refer-clojure :exclude [eval promise])
  (:require [clojure.test :refer :all]
            [dar.async :refer :all]
            [dar.async.promise :refer :all]
            [dar.container :as c :refer [define defapp]]))

(def ^:dynamic *app* nil)

(defmacro with-app [app & body]
  `(binding [*app* ~app]
     ~@body))

(defn eval [k]
  (<<! (c/eval *app* k)))

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
  (let [p (new-promise)]
    (with-app (defapp
                (define :a
                  :fn (fn [] p))
                (define :ab
                  :args [:a]
                  :fn (fn [a]
                        (str a "b"))))
      (c/eval *app* :ab)
      (deliver! p "a")
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

(deftest pre-tasks
  (let [a-called (atom false)
        b-called (atom false)]
    (with-app (defapp
                (define :a :fn #(reset! a-called true))
                (define :b :fn #(reset! b-called true))
                (define :c
                  :pre [:a :b]
                  :fn (fn [] :c)))
      (is (= :c (eval :c)))
      (is @a-called)
      (is @b-called))))

(deftest error-handling
  (let [ex (Exception. "hello")]
    (testing "Should catch task errors and wrap them with relevant info"
      (with-app (defapp
                  (define :sync
                    :fn #(throw ex))

                  (define :async
                    :fn #(doto (new-promise)
                           (deliver! ex)))

                  (define :task
                    :args [:async]
                    :fn (fn [_] (throw (Exception. "Should not reach here")))))

        (testing "Sync case"
          (let [res (eval :sync)]
            (is (= (ex-data res) {::c/level :app ::c/task :sync}))
            (is (identical? ex (.getCause res)))))

        (testing "Async case"
          (let [res (eval :async)]
            (is (= (ex-data res) {::c/level :app ::c/task :async}))
            (is (identical? ex (.getCause res)))))

        (testing "Should pass errors from dependencies as-is"
          (let [res (eval :task)]
            (is (= :async (-> res ex-data ::c/task)))))))

    (testing "Should not attempt to evaluate tasks with errors twice"
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
