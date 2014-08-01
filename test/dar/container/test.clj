(ns dar.container.test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer :all]
            [dar.async.promise :refer :all]
            [dar.container :refer :all]))

(def ^:dynamic *app* nil)

(defmacro with [app & body]
  `(binding [*app* ~app]
     ~@body))

(defn eval [k]
  (evaluate *app* k))

(application simple)

(define :a "a")

(define :b "b")

(define :ab
  :args [:a :b]
  :fn str)

(deftest basic
  (with (start simple)
    (is (= (eval :a) "a"))
    (is (= (eval :ab) "ab"))))

(application async)

(define :1
  :args [:log :p1]
  :fn (fn [log p]
        (log 1)
        p))

(define :2
  :args [:log :p2 :1]
  :fn (fn [log p _]
        (log 2)
        p))

(define :12
  :args [:log :1 :2]
  :fn (fn [log v1 _]
        (log 12)))

(deftest async-tasks
  (let [log (atom [])
        write #(swap! log conj %)
        p1 (new-promise)
        p2 (new-promise)]
    (with (start async {:log write
                        :p1 p1
                        :p2 p2})
      (then (eval :12) (fn [_]
                         (write :done)))
      (is (= @log []))
      (deliver! p1 1)
      (is (= @log [1]))
      (deliver! p2 2)
      (is (= @log [1 2 12 :done])))))

(application multi-level)

(define :value 0)

(define :a
  :level :app
  :args [:a-times :value]
  :fn (fn [times v]
        (swap! times inc)
        v))

(define :b
  :args [:b-times :value]
  :fn (fn [times v]
        (swap! times inc)
        v))

(define :ab
  :args [:a :b]
  :fn +)

(deftest levels
  (let [a-times (atom 0)
        b-times (atom 0)]
    (with (start multi-level :app {:a-times a-times
                                   :b-times b-times})
      (with (start *app* {:value 1})
        (is (= (eval :ab) 1)))

      (with (start *app* {:value 2})
        (is (= (eval :ab) 2)))

      (is (= 1 @a-times))
      (is (= 2 @b-times)))))

(deftest pre-tasks
  (let [a-called (atom false)
        b-called (atom false)]
    (with (start {:a {:fn #(reset! a-called true)}
                  :b {:fn #(reset! b-called true)}
                  :c {:fn (fn [] :c)
                      :pre [:a :b]}})
      (is (= (eval :c) :c))
      (is @a-called)
      (is @b-called))))

(deftest error-handling
  (let [ex (Exception. "hello")
        p (new-promise)]
    (testing "Should catch task errors and wrap them with relevant info"
      (with (start {:sync {:fn #(throw ex)}
                    :async {:fn (fn [] p)}
                    :task {:args [:async]
                           :fn (fn [_]
                                 (print "reached!")
                                 (throw (Exception. "Should not reach here")))}})

        (testing "Sync case"
          (let [err (eval :sync)
                data (ex-data err)]
            (is (= (:dar.container/task data) :sync))
            (is (identical? (.getCause err) ex))))

        (testing "Async case"
          (let [result (eval :async)]
            (is (not (delivered? result)))
            (deliver! p ex)
            (let [err (value result)
                  data (ex-data err)]
              (is (= (:dar.container/task data) :async))
              (is (identical? (.getCause err) ex)))))

        (testing "Should pass errors from dependencies as-is"
          (let [err (eval :task)
                data (ex-data err)]
            (is (= (:dar.container/task data) :async))))))))
