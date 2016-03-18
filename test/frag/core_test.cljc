(ns frag.core-test
  (:require [frag.core :refer [reactive-map nest state state-recursive]]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing run-tests]])
            [plumbing.core :as p]
            [plumbing.fnk.pfnk :as pfnk]
            [schema.core :as s]))

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
        (is (= 12 (get m :a)))))
    (testing "is seqable"
      (let [m (reactive-map {:a 1 :b 2 :c (p/fnk [a b] (+ a b))})
            s (seq m)
            expected #{[:a 1] [:b 2] [:c 3]}]
        (is (every? expected s))
        (is (every? (set s) expected))))
    (testing "keys"
      (is (= [:a :b] (keys (reactive-map {:a 1 :b (p/fnk [a] a)})))))
    (testing "contains?"
      (let [m (reactive-map {:a 1 :b (p/fnk [a] a)})]
        (is (contains? m :a))
        (is (contains? m :b))))
    (testing "into map"
      (let [m (reactive-map {:a 1 :b 2 :c (p/fnk [a b] (+ a b))})
            expected {:a 1 :b 2 :c 3}]
        (is (= expected (into {} m)))))
    (testing "prints like a map"
      (let [m (reactive-map {:a 1 :b 2 :c (p/fnk [a b] (+ a b))})
            expected "{:a 1, :b 2, :c 3}"]
        (is (= expected (pr-str m)))))
    (testing "select-keys doesn't return non-existant keys"
      (is (= {:a 1} (select-keys (reactive-map {:a 1}) [:a :b :c])))))

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
                 :nested :value))))
    (let [m (reactive-map :a (p/fnk [i] i)
                          (nest :q [] {:b (p/fnk [] 1)}))]
      (is (= 1 (get-in m [:q :b]))))

    (let [m (reactive-map :a 1
                          (nest :nested [:a]
                                :input 0
                                :value (p/fnk [input a] (* a input input)))
                          :nv (p/fnk [nested] (:value nested))
                          (nest :m [:nv]
                                :input 2
                                :value (p/fnk [nv input] (* nv (dec input))))
                          :nnv (p/fnk [m] (:value m)))]
      (is (= 0  (get m :nv)))
      (is (= 0  (get m :nnv)))
      (is (= 0  (get-in m [:nested :value])))
      (is (= 16 (get (assoc-in m [:nested :input] 4) :nv)))
      (is (= 16 (get (assoc-in m [:nested :input] 4) :nnv)))
      (is (= 16 (get-in (assoc-in m [:nested :input] 4) [:nested :value])))
      (is (= 9  (get (assoc-in m [:nested :input] 3) :nv)))
      (is (= 9  (get (assoc-in m [:nested :input] 3) :nnv)))
      (is (= 9  (get-in (assoc-in m [:nested :input] 3) [:nested :value])))))

  (testing "bug: maybe-dirty doesn't clobber dirty"
    (let [m (reactive-map :a (p/fnk [ai]   (+ (or ai 0) 0))
                          :b (p/fnk [bi a] (if bi bi a))
                          :c (p/fnk [ci b] (+ (or ci 0) b)))
          n (-> m
                (assoc :bi 2)
                (doto (get :c)) ;; c cached = 2
                (assoc :ci 1)   ;; dirties c
                (assoc :ai 3))] ;; dirties a, maybe-dirties b,c
      ;; c should be dirty and recompute, but if maybe-dirty clobbered it then
      ;; it will recompute b first, which won't change, and then mark clean
      ;; without recomputing
      (is (= (+ 1 2) (get n :c)))))

  (testing "constructor pulls in nested maps"
    (let [m (reactive-map :a (p/fnk [b] (* b 3))
                          {:b (p/fnk [c] (* c 5))
                           :c (p/fnk [d] (* d 7))}
                          :d (p/fnk [e] (* e 11)))
          m (assoc m :e 13)]
      (is (= (* 13 11 7 5 3) (get m :a)))))

  (testing "assoc'ing nested maps"
    (let [m (reactive-map :a 1
                          (nest :b [:a]
                                :x 1
                                :y (p/fnk [a x] (+ a x))))]
      (is (= 4 (-> m
                   (assoc :a 2)
                   (assoc :b {:x 2})
                   :b
                   :y))))))

(deftest state-test
  (let [m (reactive-map :a (p/fnk [b c] (+ b c))
                        :d 7
                        :f (fn [x] x)
                        :z (p/fnk [z f] (inc (or z (f 0)))))]
    (is (= {} (state m)))
    (let [n (assoc m :x 14)]
      (is (= {:x 14} (state n)))))

  (let [m (reactive-map :a (p/fnk [i] i) :i 1)]
    (is (= {} (state m)))
    (is (= {:i 2} (state (assoc m :i 2))))))

(deftest state-recursive-test
  (let [m (reactive-map :a (p/fnk [i] i)
                        (nest :q [] {:z (p/fnk [y] y)}))]
    (is (= {} (state-recursive m)))
    (is (= {:q {:y 1}} (state-recursive (assoc-in m [:q :y] 1)))))

  (let [m (reactive-map (nest :a [:i] :a (p/fnk [i j] (+ i j)) :j 2) :i 1)]
    (is (= {} (state-recursive m)))
    (is (= {:i 2} (state-recursive (assoc m :i 2))))
    (is (= {:i 2 :a {:j 3}} (state-recursive (-> (assoc m :i 2)
                                                 (assoc-in [:a :j] 3)))))))
