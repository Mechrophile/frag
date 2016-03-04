(ns frag.core-test
  (:require [frag.core :refer :all]
            [midje.sweet :refer :all]
            [plumbing.core :as p]
            [plumbing.fnk.pfnk :as pfnk]
            [schema.core :as s]))

(facts "ReactiveMap"
  (fact "works like a regular map"
    (fact "fetching element"
      (let [m (reactive-map {:a 1})]
        (get m :a) => 1))
    (fact "assoc a new element"
      (let [m (-> (reactive-map {:a 1})
                  (assoc :b 2))]
        (get m :b) => 2))
    (fact "merge with map"
      (let [m (merge (reactive-map {:a (p/fnk [b c] (+ b c))})
                     {:b 5 :c 7})]
        (get m :a) => 12)))

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
        @counter              => 1)))

  (fact "allows nodes to take their own previous value as input"
    (let [m1 (reactive-map {:a (p/fnk [a b] [(inc (if a (first a) 0)) b])
                            :b 1})]
      (get m1 :a) => [1 1]
      (get m1 :a) => [1 1]
      (let [m2 (assoc m1 :b 2)]
        (get m2 :a) => [2 2]
        (get m2 :a) => [2 2]
        (get m1 :a) => [1 1]))

    (fact "can create d-latches"
      (let [m (reactive-map {:latch (p/fnk [latch e d] (if e d latch))})]
        (:latch (assoc m :d 1 :e false)) => nil
        (:latch (assoc m :d 1 :e true))  => 1
        (-> m
            (assoc :e true)
            (assoc :d 1)
            ;;(doto (get :latch)) ;; its state changes when we observe it :|
            (assoc :e false)
            (assoc :d 0)
            :latch) => 1

        )))

  (fact "supports nested maps"
    (let [m (reactive-map (nest :nested [:input]
                                :value (p/fnk [input] (* input input))))]
      (-> (assoc m :input 2) :nested :value) => 4
      (-> (assoc m :input 2)
          (assoc :input 3)
          (assoc :input 4)
          :nested :value) => 16
      ))

  (fact "constructor pulls in nested maps"
    (let [m (reactive-map :a (p/fnk [b] (* b 3))
                          {:b (p/fnk [c] (* c 5))
                           :c (p/fnk [d] (* d 7))}
                          :d (p/fnk [e] (* e 11)))
          m (assoc m :e 13)]
      (get m :a) => (* 13 11 7 5 3))))
