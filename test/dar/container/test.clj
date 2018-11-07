(ns dar.container.test
  (:require [clojure.test :refer :all]
            [dar.container :refer :all :as c]))


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
                           (assoc g k (assoc node :fn (fn [& args]
                                                        (let [ret (apply f args)]
                                                          (swap! trace conj (apply vector k ret (map (fn [a]
                                                                                                       (if (fn? a) :fn a))
                                                                                                     args)))
                                                          ret))))
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


(defn -main []
  (run-tests 'dar.container.test))
