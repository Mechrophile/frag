(ns frag.core-test
  (:require [frag.core :refer :all]
            [midje.sweet :refer :all]
            [plumbing.core :as p]))

(facts "ReactiveMap"
  (fact "works like a regular map"
    (fact "fetching element"
      (let [m (reactive-map {:a 1})]
        (get m :a) => 1))
    (fact "assoc a new element"
      (let [m (-> (reactive-map {:a 1})
                  (assoc :b 2))]
        (get m :b) => 2)))

  (fact "can calculate elements based on other elements"
    (fact "simple"
      (let [m (reactive-map {:a 1 :b (p/fnk [a] (+ a 1))})]
        (get m :b) => 2))
    (fact "chained"
      (let [m (reactive-map {:a 1 :b (p/fnk [a] (+ a 1)) :c (p/fnk [b] (+ b 1))})]
        (get m :c) => 3)))

  (fact "only calculates values once"
    (let [counter (atom 0)
          m (reactive-map {:a (p/fnk [] (swap! counter inc) 1)
                           :b (p/fnk [a] (+ a 1))
                           :c (p/fnk [b] (+ b 1))})]
      (get m :a) => 1
      (get m :b) => 2
      (get m :c) => 3
      @counter => 1))

  (fact "recalculates values when inputs change"
    (let [m-2 (reactive-map {:input 2
                             :square    (p/fnk [[:input  :as a]] (* a a))
                             :tesseract (p/fnk [[:square :as s]] (* s s))})]
      (get m-2 :square)    => (* 2 2)
      (get m-2 :tesseract) => (* 2 2 2 2)
      (let [m-3 (assoc m-2 :input 3)]
        (get m-3 :square)    => (* 3 3)
        (get m-3 :tesseract) => (* 3 3 3 3))))

  (fact "doesn't recalculate if intermediate steps didn't change"
    (let [counter (atom 0)
          m-2 (reactive-map {:input 2
                             :square    (p/fnk [[:input  :as a]] (* a a))
                             :tesseract (p/fnk [[:square :as s]]
                                               (swap! counter inc)
                                               (* s s))})]
      (get m-2 :square)    => (* 2 2)
      (get m-2 :tesseract) => (* 2 2 2 2)
      (let [m--2 (assoc m-2 :input -2)]
        (get m--2 :square)    => (* -2 -2)
        (get m--2 :tesseract) => (* -2 -2 -2 -2)
        @counter              => 1))))



(* -2 -2)
