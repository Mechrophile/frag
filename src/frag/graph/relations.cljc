(ns frag.graph.relations
  (:require [plumbing.core :as p]
            [plumbing.map :as pm]
            [clojure.set :as set])
  (:refer-clojure :exclude [parents ancestors descendants]))

(defn- adj-map->adj-list
  "{k [v1 v2]} => [k v1] [k v2]"
  [m]
  (mapcat (fn [[k vs]] (map #(vector k %) vs)) m))

(defn- adj-list->adj-map
  "[k v1] [k v2] => {k [v1 v2]}"
  [coll]
  (reduce (fn [m [k v]] (update m k conj v)) {} coll))

(defn- reverse-adj-map
  "{k [v1 v2]} => {v1 [k] v2 [k]}"
  [m]
  (->> (adj-map->adj-list m)
       (map reverse)
       (adj-list->adj-map)))

(defn- find-descendants
  "Connect each node to all nodes reachable through its children.
  i.e., {a [b]           b [c]         c [d e f]   f [g]}
     => {a [b c d e f g] b [c d e f g] c [d e f g] f [g]}"
  [g]
  (reduce (fn [m k]
            (update m k (p/fn->> (mapcat #(cons % (get m %)))
                                 (distinct))))
          g
          (reverse (pm/topological-sort g))))

(defn- find-loops
  "Return nodes that have an edge going to themselves."
  [g]
  (->> (adj-map->adj-list g)
       (filter #(apply = %))
       (map first)))

(defrecord Relations [parents ancestors children descendants loops sources sinks])

(defn relations
  "Calculate various graph relationships given the parent relationships.
  (i.e. {k [v]} means v depends on k)."
  [parents]
  (let [children (reverse-adj-map parents)]
    (Relations.
     parents                                ;; parents
     (find-descendants parents)             ;; ancestors
     children                               ;; children
     (find-descendants children)            ;; descendants
     (find-loops parents)                   ;; loops
     (set/difference (set (keys children))  ;; sources
                     (set (keys parents)))
     (set/difference (set (keys parents))   ;; sinks
                     (set (keys children))))))

(defn parents     [rels k] (get-in rels [:parents     k]))
(defn ancestors   [rels k] (get-in rels [:ancestors   k]))
(defn children    [rels k] (get-in rels [:children    k]))
(defn descendants [rels k] (get-in rels [:descendants k]))
(defn loops       [rels]   (get-in rels [:loops]))
(defn sources     [rels]   (get-in rels [:sources]))
(defn sinks       [rels]   (get-in rels [:sinks]))
