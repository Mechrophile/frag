(ns frag.core-test
  (:require [frag.core :refer :all]
            [midje.sweet :refer :all]
            [clojure.test :refer :all]
            [plumbing.core :as p]
            [plumbing.fnk.pfnk :as pfnk]
            [schema.core :as s]
            ))

(deftest test-reactive-map
  (testing "works like a regular map"
    (testing "fetching element"
      (let [m (reactive-map {:a 1})]
        (is (= 1 (get m :a)))))
    (testing "assoc a new element"
      (let [m (-> (reactive-map {:a 1})
                  (assoc :b 2))]
        (is (= 2 (get m :b)))))
    (testing "merge with map"
      (let [m (merge (reactive-map {:a (p/fnk [b c] (+ b c))})
                     {:b 5 :c 7})]
        (is (= 12 (get m :a))))))

  (testing "can calculate elements based on other elements"
    (testing "simple"
      (let [m (reactive-map {:a 1 :b (p/fnk [a] (+ a 1))})]
        (is (= 2 (get m :b)))))
    (testing "chained"
      (let [m (reactive-map {:a 1 :b (p/fnk [a] (+ a 1)) :c (p/fnk [b] (+ b 1))})]
        (is (= 3 (get m :c))))))

  (testing "only calculates values once"
    (let [counter (atom 0)
          m (reactive-map {:a (p/fnk [] (swap! counter inc) 1)
                           :b (p/fnk [a] (+ a 1))
                           :c (p/fnk [b] (+ b 1))})]
      (is (= 1 (get m :a)))
      (is (= 2 (get m :b)))
      (is (= 3 (get m :c)))
      (is (= 1 @counter))))

  (testing "recalculates values when inputs change"
    (let [m-2 (reactive-map {:input 2
                             :square    (p/fnk [[:input  :as a]] (* a a))
                             :tesseract (p/fnk [[:square :as s]] (* s s))})]
      (is (= (* 2 2) (get m-2 :square)))
      (is (= (* 2 2 2 2) (get m-2 :tesseract)))
      (let [m-3 (assoc m-2 :input 3)]
        (is (= (* 3 3) (get m-3 :square)))
        (is (= (* 3 3 3 3) (get m-3 :tesseract))))))

  (testing "doesn't recalculate if intermediate steps didn't change"
    (let [counter (atom 0)
          m-2 (reactive-map {:input 2
                             :square    (p/fnk [[:input  :as a]] (* a a))
                             :tesseract (p/fnk [[:square :as s]]
                                               (swap! counter inc)
                                               (* s s))})]
      (is (= (* 2 2) (get m-2 :square)))
      (is (= (* 2 2 2 2) (get m-2 :tesseract)))
      (let [m--2 (assoc m-2 :input -2)]
        (is (= (* -2 -2) (get m--2 :square)))
        (is (= (* -2 -2 -2 -2) (get m--2 :tesseract)))
        (is (= 1 @counter)))))

  (testing "allows nodes to take their own previous value as input"
    (let [m1 (reactive-map {:a (p/fnk [a b] [(inc (if a (first a) 0)) b])
                            :b 1})]
      (is (= [1 1] (get m1 :a)))
      (is (= [1 1] (get m1 :a)))
      (let [m2 (assoc m1 :b 2)]
        (is (= [2 2] (get m2 :a)))
        (is (= [2 2] (get m2 :a)))
        (is (= [1 1] (get m1 :a)))))

    (testing "can create d-latches"
      (let [m (reactive-map {:latch (p/fnk [latch e d] (if e d latch))})]
        (is (nil? (:latch (assoc m :d 1 :e false))))
        (is (= 1 (:latch (assoc m :d 1 :e true))))
        (is (= 1
               (-> m
                   (assoc :e true)
                   (assoc :d 1)
                   (assoc :e false)
                   (assoc :d 0)
                   :latch))))))

  (testing "supports nested maps"
    (let [m (reactive-map (nest :nested [:input]
                                :value (p/fnk [input] (* input input))))]
      (is (= 4 (-> (assoc m :input 2) :nested :value)))
      (is (= 16
             (-> (assoc m :input 2)
                 (assoc :input 3)
                 (assoc :input 4)
                 :nested :value)))))

  (testing "constructor pulls in nested maps"
    (let [m (reactive-map :a (p/fnk [b] (* b 3))
                          {:b (p/fnk [c] (* c 5))
                           :c (p/fnk [d] (* d 7))}
                          :d (p/fnk [e] (* e 11)))
          m (assoc m :e 13)]
      (is (= (* 13 11 7 5 3) (get m :a))))))
