(ns dar.container.test
  (:require [clojure.test :refer :all]
            [clojure.pprint :as pp]
            [dar.container :refer :all :as c]))


(set! *warn-on-reflection* true)


(def analyze @#'dar.container/analyze)


(def abc-app (-> {}
                 (define :ab
                   :args [:a :b]
                   :fn +)

                 (define :abc
                   :args [:ab :c]
                   :fn +)))


(def simple-2-level-app (-> {}
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
                              :fn +)

                            (define-level :foo :ab-ac [:b])

                            (define :main
                              :args [:foo]
                              :fn (fn [foo]
                                    (+ (foo 5)
                                       (foo 1))))))


(def complex-1-level-app
  (-> {}
      (define :result :args [:abcd :xy] :fn +)
      (define :abcd :args [:b :ab] :fn +)
      (define :ab :args [:b :a] :fn +)
      (define :cd :args [:b :a] :fn +)
      (define :d :args [:abcd :ab] :fn +)
      (define :x :args [:d :cd] :fn +)
      (define :xy :args [:abcd :x] :fn +)))


(deftest ana-abc-app
  (let [g (analyze abc-app :abc [:a :b :c])]
    (is (= (-> g ::c/levels) [::c/main-level]))
    (is (= (-> g ::c/main-level ::c/nodes) #{:a :b :c :ab :abc}))
    (is (= (-> g ::c/main-level ::c/deps) #{}))
    (is (= (-> g ::c/main-level ::c/root-of) {:abc :abc
                                              :ab :abc}))))


(deftest ana-simple-2-level-app
  (let [g (analyze simple-2-level-app :main [])]
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


(defn assert-trace [app main args values expected-trace]
  (let [trace (atom [])
        app (unwrap-app app)
        app (reduce-kv (fn [g k node]
                         (if-let [f (:fn node)]
                           (let [node (assoc node :fn (fn [& args]
                                                        (let [ret (apply f args)]
                                                          (swap! trace conj (apply vector k ret (map (fn [a]
                                                                                                       (if (fn? a) :fn a))
                                                                                                     args)))
                                                          ret)))
                                 node (if-let [close (:close node)]
                                        (assoc node :close (fn [x]
                                                             (swap! trace conj [:close k x])
                                                             (close x)))
                                        node)]
                             (assoc g k node))
                           g))
                       app
                       app)
        app (compile-app app main args)
        ret (apply app values)]
    (swap! trace conj ret)
    (is (= @trace expected-trace))))


(deftest abc-trace
  (assert-trace abc-app :abc [:a :b :c] [1 2 3]
    [[:ab 3 1 2]
     [:abc 6 3 3]
     6]))


(deftest complex-1-level-app-trace
  (assert-trace complex-1-level-app :result [:a :b] [1 1]
    [[:ab 2 1 1]
     [:abcd 3 1 2]
     [:d 5 3 2]
     [:cd 2 1 1]
     [:x 7 5 2]
     [:xy 10 3 7]
     [:result 13 3 10]
     13]))


(deftest simple-2-level-app-trace
  (assert-trace simple-2-level-app :main [] []
    [[:a 1]
     [:ab 6 1 5]
     [:ac 3 1 2]
     [:ab-ac 9 6 3]
     [:ab 2 1 1]
     [:ab-ac 5 2 3]
     [:main 14 :fn]
     14]))


(deftest local-closables
  (let [app (-> {}
                (define :A
                  :fn (constantly 1)
                  :close identity)

                (define :Ab
                  :args [:A :b]
                  :fn +
                  :close identity)

                (define :Abc
                  :args [:Ab :c]
                  :fn +
                  :close identity))]
    (assert-trace app :Abc [:b :c] [1 2]
      [[:A 1]
       [:Ab 2 1 1]
       [:Abc 4 2 2]
       [:close :Ab 2]
       [:close :A 1]
       4])))


(deftest local-dummy-lazy
  (let [app (-> {}
                (define :a
                  :fn (constantly 1))

                (define :ab
                  :args [:a :b]
                  :fn +)

                (define :aba
                  :args [:a :ab]
                  :lazy #{:a}
                  :fn (fn [a ab]
                        (+ ab (a)))))]
    (assert-trace app :aba [:b] [3]
      [[:a 1]
       [:ab 4 1 3]
       [:aba 5 :fn 4]
       5])))


(deftest local-shared-lazy
  (let [nothing (-> {}
                    (define :a
                      :fn (constantly 1))

                    (define :ab
                      :args [:a :b]
                      :fn +)

                    (define :aba
                      :args [:a :ab]
                      :lazy #{:a :ab}
                      :fn (fn [a ab]
                            1)))

        something (assoc-in nothing [:aba :fn] (fn [a ab]
                                                 (+ (ab) (a) (ab) (a))))]

    (assert-trace nothing :aba [:b] [1]
      [[:aba 1 :fn :fn]
       1])

    (assert-trace something :aba [:b] [1]
      [[:a 1]
       [:ab 2 1 1]
       [:aba 6 :fn :fn]
       6])))


(defn -main []
  (run-tests 'dar.container.test))
