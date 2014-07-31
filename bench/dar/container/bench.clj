(ns dar.container.bench
  (:require [criterium.core :as criterium]
            [dar.container :refer :all]
            [dar.container.sync :as sync]))

(application bench)

(define :a 1)
(define :b 2)
(define :c 3)
(define :d 4)
(define :e 5)

(define :ab
  :args [:a :b]
  :fn +)

(define :abcde
  :args [:a :b :c :d :e]
  :fn +)

(define :a+
  :args [:a]
  :fn inc)

(define :b+
  :args [:b]
  :fn inc)

(define :c+
  :args [:c]
  :fn inc)

(define :d+
  :args [:b]
  :fn inc)

(define :e+
  :args [:e]
  :fn inc)

(define :ab+
  :args [:a+ :b+]
  :fn +)

(define :cd+
  :args [:c+ :d+]
  :fn +)

(define :big-calc
  :args [:a+ :b+ :e+ :ab+ :cd+ :d+ :abcde]
  :fn +)

(defn simple-access [app]
  (evaluate app :a))

(defn calc-2-args [app]
  (evaluate app :ab))

(defn calc-5-args [app]
  (evaluate app :abcde))

(defn start-next-level-and-access [app]
  (evaluate (start app) :a))

(defn calc-5-args-from-prev-level [app]
  (evaluate (start app) :abcde))

(defn big-calc [app]
  (evaluate app :big-calc))

(defn sync-calc-5-args [app]
  (sync/evaluate app :abcde))

(defn sync-big-calc [app]
  (sync/evaluate app :big-calc))

(defmacro qb [fun]
  `(do
     (println)
     (println "============================================================")
     (println '~fun)
     (println "============================================================")
     (criterium/quick-bench (~fun (start bench)))
     (println)))

(defn measure-overhead [_])

(defn -main []
  (qb measure-overhead)
  (qb simple-access)
  (qb calc-2-args)
  (qb calc-5-args)
  (qb start-next-level-and-access)
  (qb calc-5-args-from-prev-level)
  (qb big-calc)
  (qb sync-big-calc)
  )
