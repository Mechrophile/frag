(ns frag.core-test
  (:require [frag.core :refer :all]
            [midje.sweet :refer :all]
            [plumbing.core :as p]))

(facts "about ReactiveMap"
  (fact "works like a regular map"
    (fact "fetching element"
      (let [m (reactive-map {:a 1})]
        (get m :a) => 1)))

  (fact "can calculate elements based on other elements"
    (fact "simple"
      (let [m (reactive-map {:a 1 :b (p/fnk [a] (+ a 1))})]
        (get m :b) => 2))
    (fact "chained"
      (let [m (reactive-map {:a 1 :b (p/fnk [a] (+ a 1)) :c (p/fnk [b] (+ b 1))})]
        (get m :c) => 3))))

