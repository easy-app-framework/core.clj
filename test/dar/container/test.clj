(ns dar.container.test
  (:require [clojure.test :refer :all]
            [dar.container :refer :all :as c]))


(def analyze @#'dar.container/analyze)


(deftest ana-elementary-flat-graph
  (let [g (-> {}
              (define :ab
                :args [:a :b]
                :fn +)
              (define :abc
                :args [:ab :c]
                :fn +)
              (analyze :abc [:a :b :c]))]
    (is (= (-> g ::c/levels) [::c/main-level]))
    (is (= (-> g ::c/main-level ::c/nodes) #{:a :b :c :ab :abc}))
    (is (= (-> g ::c/main-level ::c/deps) #{}))
    (is (= (-> g ::c/main-level ::c/root-of) {:abc :abc
                                              :ab :abc}))))


(deftest ana-simple-2-level-graph
  (let [g (-> {}
              (define :a
                :fn (constantly 1))
              (define :ab
                :args [:a :b]
                :fn +)
              (define-constant :c 2)
              (define :ac
                :args [:a :c]
                :fn +)
              (define :ab-ac
                :args [:ab :ac]
                :fn -)
              (define-level :foo :ab-ac [:b])
              (define :main
                :args [:foo]
                :fn (fn [foo] (foo 5)))
              (analyze :main []))]
    (is (= (-> g ::c/levels) [:foo ::c/main-level]))
    (is (= (-> g :foo ::c/nodes) #{:b :ab :ab-ac}))
    (is (= (-> g :foo ::c/deps) #{:ac :a}))
    (is (= (-> g :foo ::c/root-of) {:ab-ac :ab-ac
                                    :ab :ab-ac}))
    (is (= (-> g ::c/main-level ::c/nodes) #{:a :c :ac :foo :main}))
    (is (= (-> g ::c/main-level ::c/root-of) {:main :main
                                              :ac   :ac
                                              :a    :a}))
    (is (= (-> g ::c/main-level ::c/shared) #{}))))


(defn -main []
  (run-tests 'dar.container.test))
