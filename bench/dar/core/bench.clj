(ns dar.core.bench
  (:require [criterium.core :as criterium]
            [dar.async :refer :all]
            [dar.core :as co :refer [define]]
            [dar.core.sync :as sync]))

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

(def app (co/make))
(def app-state @(:state app))

(defn just-a-go-block [_]
  (go 1))

(defn simple-access [app]
  (<!! (co/eval app :a)))

(defn calc-2-args [app]
  (<!! (co/eval app :ab)))

(defn calc-5-args [app]
  (<!! (co/eval app :abcde)))

(defn start-next-level-and-access [app]
  (<!! (co/eval (co/start app) :a)))

(defn calc-5-args-from-prev-level [app]
  (<!! (co/eval (co/start app) :abcde)))

(defn big-calc [app]
  (<!! (co/eval app :big-calc)))

(defn big-calc-next-level [app]
  (<!! (co/eval (co/start app) :big-calc)))

(defn sync-calc-5-args [app]
  (sync/eval app :abcde))

(defn sync-big-calc [app]
  (sync/eval app :big-calc))

(defmacro qb [fun]
  `(do
     (println)
     (println "============================================================")
     (println '~fun)
     (println "============================================================")
     (criterium/quick-bench (~fun (assoc app :state (atom app-state))))
     (println)))

(defn -main []
  (qb just-a-go-block)
  (qb simple-access)
  (qb calc-2-args)
  (qb calc-5-args)
  (qb start-next-level-and-access)
  (qb calc-5-args-from-prev-level)
  (qb big-calc)
  (qb big-calc-next-level)
  (qb sync-calc-5-args)
  (qb sync-big-calc))
