(ns frag.core
  (:require [plumbing.core :as p]
            [plumbing.fnk.pfnk :as pfnk]
            [plumbing.map :as pm]
            [schema.core :as s]
            [clojure.set :as set]))

(defn- static-spec?
  [spec]
  (not (and (fn? spec) (:schema (meta spec)))))

(declare ->ReactiveMap)

;; ReactiveMap has three parts:
;;  - specs: how to generate a value - shared by all descendants
;;  - cache: any specs already evaluated
;;  - state: assoc'd values
;;  - dependency graph

(defn- cache
  ([] (cache {}))
  ([v] {:pre [(map? v)]}
   (atom (p/map-vals #(assoc {} :value %) v))))

(defn- cache-clone [cache] (atom @cache))

(defn- cache-dirty?            [cache k] (true? (get-in @cache [k :dirty])))
(defn- cache-maybe-dirty?      [cache k] (get-in @cache [k :dirty]))
(defn- cache-mark-dirty!       [cache k] (swap! cache assoc-in [k :dirty] true))
(defn- cache-mark-maybe-dirty! [cache k] (swap! cache assoc-in [k :dirty] :maybe))
(defn- cache-mark-clean!       [cache k] (swap! cache update k dissoc :dirty))
(defn- cache-contains-value?   [cache k] (contains? (get @cache k) :value))
(defn- cache-get               [cache k] (get-in @cache [k :value]))

(defn- cache-assoc!
  "Store value in cache and clear dirty flags"
  [cache k v]
  (swap! cache assoc k {:value v}))

(defn- cache-dissoc!
  "Remove value from cache and clear dirty flags"
  [cache k]
  (swap! cache dissoc k))

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
  [g]
  (reduce (fn [m k] (update m k (p/fn->> (mapcat #(cons % (get m %)))
                                        distinct)))
          g (reverse (pm/topological-sort g))))

(defn- find-loops
  [g]
  (->> (adj-map->adj-list g)
       (filter #(apply = %))
       (map first)))

(defn- dependency-relations
  "Given a digraph as a reversed adjacency map (i.e. {k [v]} => v->k), return
  maps of various relationships."
  [parents]
  (let [children (reverse-adj-map parents)]
    {:parents     parents
     :ancestors   (find-descendants parents)
     :children    children
     :descendants (find-descendants children)
     :loops       (find-loops parents)
     :sources     (set/difference (set (keys children)) (set (keys parents)))
     :sinks       (set/difference (set (keys parents))  (set (keys children)))}))

(defn- spec-with-deps
  [specs]
  (let [dynamic-specs (into {} (remove #(static-spec? (val %)) specs))
        dependencies (p/map-vals pfnk/input-schema-keys dynamic-specs)]
    (vary-meta specs assoc ::deps (dependency-relations dependencies))))

(defn- spec-parents     [specs k] (get-in (meta specs) [::deps :parents k]))
(defn- spec-descendants [specs k] (get-in (meta specs) [::deps :descendants k]))
(defn- spec-children    [specs k] (get-in (meta specs) [::deps :children k]))
(defn- spec-loops       [specs]   (get-in (meta specs) [::deps :loops]))
(defn- spec-sources     [specs]   (get-in (meta specs) [::deps :sources]))

(defn- rmap-recalculate [specs cache state k]
  (let [spec-fn     (get specs k)
        fetch-value #(if (cache-contains-value? cache %)
                       (cache-get cache %)
                       (get state %))
        old-value   (fetch-value k)
        new-value   (->> (spec-parents specs k)
                         (p/map-from-keys fetch-value)
                         spec-fn)]
    ;;(println "recalced" k old-value " => " new-value)
    (cache-assoc! cache k new-value)
    (when (not= new-value old-value)
      (doseq [ck (spec-children specs k)]
        (when (not= ck k)
          ;;(println "dirtying" ck)
          (cache-mark-dirty! cache ck))))))


(defn- rmap-undirty [specs cache state k]
  (when (cache-maybe-dirty? cache k)
    ;; undirty parents
    (doseq [pk (spec-parents specs k)]
      (when (not= pk k) (rmap-undirty specs cache state pk)))
    ;; dirty parents will have dirtied us
    (when (cache-dirty? cache k)
      (rmap-recalculate specs cache state k))
    (cache-mark-clean! cache k)))

(defn- rmap-get [specs cache state k not-found]
  (when (contains? specs k)
    (rmap-undirty specs cache state k))
  (if (cache-contains-value? cache k)
    (cache-get cache k)
    (get state k not-found)))

(defn- rmap-assoc [specs cache state k v]
  ;; if we have any descendants that depend on their own state, then their
  ;; current value becomes part of our new object's state
  (let [descendants (spec-descendants specs k)
        loopy-descs (set/intersection (set (spec-loops specs))
                                      (set descendants))]
    (doseq [dk loopy-descs]
      (rmap-undirty specs cache state dk))

    (let [new-cache (cache-clone cache)
          new-state (reduce (fn [s k]
                              (cache-mark-dirty! new-cache k)
                              (assoc s k (cache-get cache k)))
                            state loopy-descs)]
      (cache-dissoc! new-cache k)
      (doseq [dk descendants]             (cache-mark-maybe-dirty! new-cache dk))
      (doseq [dk (spec-children specs k)] (cache-mark-dirty!       new-cache dk))
      ;;(println "assoc" k v)
      ;;(println "loopy" loopy-descs)
      ;;(println specs new-cache (assoc new-state k v))
      (->ReactiveMap specs
                     new-cache
                     (assoc new-state k v)))))

(defn- rmap-keys [specs state]
  (distinct (mapcat keys [specs state])))

(defn- parse-specs
  [args]
  (loop [specs {}
         args args]
    (if (empty? args)
      specs
      (let [[k v & more] args]
        (cond
          (keyword? k) (recur (assoc specs k v) more)
          (map? k) (recur (merge specs k) (when v (cons v more)))
          :else (throw (new #?(:clj RuntimeException :cljs js/Error)
                            "unexpected value in key position")))))))

(defn reactive-map [& specs]
  (let [specs (parse-specs specs)
        specs-by-static (group-by #(static-spec? (val %)) specs)
        static-specs (into {} (get specs-by-static true))
        dynamic-spec-keys (map first (get specs-by-static false))
        cache (cache static-specs)]
    (doseq [k dynamic-spec-keys]
      (cache-mark-dirty! cache k))
    (->ReactiveMap (spec-with-deps specs) cache {})))

#?(:clj
   (deftype ReactiveMap [specs cache state]
     clojure.lang.ILookup
     (valAt [this k] (.valAt this k nil))
     (valAt [this k not-found]
       (rmap-get specs cache state k not-found))

     clojure.lang.IPersistentCollection
     (cons [this o] (reduce (fn [m [k v]] (assoc m k v)) this o))

     (equiv [this o]
       (and (instance? ReactiveMap o)
            (= (.specs o) specs)
            (= (.state o) state)))

     clojure.lang.IPersistentMap
     (assoc [this k v] (rmap-assoc specs cache state k v))

     java.lang.Iterable
     (iterator [this] (.iterator (seq this)))

     clojure.lang.Associative
     (containsKey [this k]
       (boolean (some #{k} (rmap-keys specs state))))
     (entryAt [this k]
       (when (contains? this k)
         (clojure.lang.MapEntry. k (get this k))))

     clojure.lang.Seqable
     (seq [this] (map #(find this %) (rmap-keys specs state))))

   :cljs
   (deftype ReactiveMap [specs cache state]
     ILookup
     (-lookup [this k] (-lookup this k nil))
     (-lookup [this k not-found] (rmap-get specs cache state k not-found))

     ICollection
     (-conj [this o] (reduce (fn [m [k v]] (assoc m k v)) this o))

     IAssociative
     (-assoc [this k v] (rmap-assoc specs cache state k v))

     ISeqable
     (-seq [this] (map #(find this %) (rmap-keys specs state)))

     IPrintWithWriter
     (-pr-writer [this writer opts]
       (print-map this pr-writer writer opts))))


(defn nest
  [k ks & spec-args]
  (let [f (fn [m] (let [self (or (get m k) (apply reactive-map spec-args))]
                   (merge self (dissoc m k))))
        input-schema (p/map-from-keys (constantly s/Any) (cons k ks))]
    {k (pfnk/fn->fnk f [input-schema ReactiveMap])}))

(defn input-keys
  [m]
  (set/union
   (set/difference (set (spec-sources (.specs m)))
                   (set (map key (filter #(static-spec? (val %)) (.specs m)))))
   (set (spec-loops (.specs m)))
   (set (keys (.state m)))))


(defn state
  [m]
  #?(:clj  (.state m)
     :cljs (.-state m)))

(defn specs
  [m]
  #?(:clj  (.specs m)
     :cljs (.-specs m)))

(defn state-recursive
  [m]
  (into
   {}
   (for [[k v] (state m)]
     (if-not (instance? ReactiveMap v)
       [k v]
       (let [inherited-keys     (spec-parents (specs m) k)
             child-state        (state-recursive v)
             non-inherited-keys (set/difference (set (keys child-state))
                                                (set inherited-keys))
             result             (select-keys child-state non-inherited-keys)]
         (when-not (empty? result)
           [k result]))))))
