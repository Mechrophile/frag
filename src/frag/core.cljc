(ns frag.core
  (:require [plumbing.core :as p]
            [plumbing.fnk.pfnk :as pfnk]
            [schema.core :as s]
            [clojure.set :as set]))

(defn- static-spec?
  [spec]
  (not (and (fn? spec) (:schema (meta spec)))))

(defn- dependent-keys
  [specs ks]
  (if (empty? ks)
    #{}
    (->> specs
         (filter (fn [[_ v]] (and (fn? v) (:schema (meta v)))))
         (filter (fn [[_ v]] (some ks (pfnk/input-schema-keys v))))
         keys set)))

(defn- dependent-keys-recursive
  [specs ks]
  (loop [cur-ks ks]
    (let [new-ks (into cur-ks (dependent-keys specs cur-ks))]
      (if (= new-ks cur-ks)
        (disj cur-ks ks)
        (recur new-ks)))))

(defn- rmap-get*
  "return [value changed? new-values new-dirty new-maybe-dirty]"
  [specs values dirty maybe-dirty k not-found]
  (let [has-value      (contains? values k)
        has-spec       (contains? specs k)
        spec           (get specs k)
        cur-value      (get values k)
        is-dirty       (get dirty k)
        is-maybe-dirty (get maybe-dirty k)]
    (cond
      (and has-value (not is-dirty) (not is-maybe-dirty))
      [cur-value false values dirty maybe-dirty]

      (and has-spec (static-spec? spec))
      [spec (if has-value (= cur-value spec) false)
       (assoc values k spec) (disj dirty k) (disj maybe-dirty k)]

      (not has-spec)
      [not-found has-value
       (dissoc values k) (disj dirty k) (disj maybe-dirty k)]

      :else
      (let [input-keys (pfnk/input-schema-keys spec)
            [inputs any-changed? new-values new-dirty new-maybe-dirty]
            (reduce
             (fn [[m c? v d md] ik]
               (if (= ik k)
                 [(assoc m k cur-value) c? v d md]
                 (let [[iv changed? nv nd nmd] (rmap-get* specs v d md ik nil)]
                   [(assoc m ik iv)
                    (or changed? c?)
                    nv nd nmd])))
             [{} false values dirty maybe-dirty]
             input-keys)
            new-value (if (and has-value (not any-changed?) (not is-dirty))
                        cur-value
                        (spec inputs))
            changed? (or (not has-value) (not= cur-value new-value))
            deps (disj (dependent-keys specs #{k}) k)
            result [new-value changed?
                    (assoc new-values k new-value)
                    (-> (disj dirty k)
                        (p/?> changed? (clojure.set/union deps)))
                    (-> (disj maybe-dirty k)
                        (p/?> changed? (clojure.set/difference deps)))]]
        ;;(println result)
        result))))

(defn- rmap-get [specs values dirty maybe-dirty k not-found]
  ;;(println "getting" k "dirty:" @dirty "/" @maybe-dirty)
  (let [[result _ new-values new-dirty new-maybe-dirty]
        (rmap-get* specs @values @dirty @maybe-dirty k not-found)]
    (reset! values new-values)
    (reset! dirty new-dirty)
    (reset! maybe-dirty new-maybe-dirty)
    result))

(defn- rmap-assoc*
  "returns
  [updated-values updated-dirty updated-maybe-dirty
   new-values new-dirty new-maybe-dirty]"
  [specs values dirty maybe-dirty k v]
  (if (= v (get values k))
    nil
    (let [dirtied       (dependent-keys specs #{k})
          maybe-dirtied (dependent-keys-recursive specs dirtied)
          redirtied     (set/intersection (set/union dirtied maybe-dirtied)
                                          (set/union dirty   maybe-dirty))
          ;; if we're dirtying any keys that were already dirty and depend on
          ;; their own value, we have to evaluate them now
          [vs d md]
          (reduce
           (fn [[vs ds md] dk]
             (let [spec (get specs dk)]
               (if (and (not (static-spec? spec))
                        (some #{dk} (pfnk/input-schema-keys spec)))
                 (do ;;(println "forcing eval of" dk
                     ;;         "because it depends on itself and" k)
                   (drop 2 (rmap-get* specs vs ds md dk nil)))
                 [vs ds md])))
           [values dirty maybe-dirty]
           redirtied)]
      [vs d md
       (assoc vs k v) (into d dirtied) (into md maybe-dirtied)])) )

(declare ->ReactiveMap)

(defn rmap-assoc
  [specs values dirty maybe-dirty k v]
  (when-let [[mv md mnd nv nd nmd]
             (rmap-assoc* specs @values @dirty @maybe-dirty k v)]
    (do (reset! values mv)
        (reset! dirty md)
        (reset! maybe-dirty nmd)
        (->ReactiveMap specs (atom nv) (atom nd) (atom nmd)))) )

#?(:clj
   (deftype ReactiveMap [specs values dirty maybe-dirty]
     clojure.lang.ILookup
     (valAt [this k] (.valAt this k nil))
     (valAt [this k not-found]
       (rmap-get specs values dirty maybe-dirty k not-found))

     clojure.lang.IPersistentCollection
     (cons [this o]
       (reduce (fn [m [k v]] (assoc m k v)) this o))

     (equiv [this o]
       (and (instance? ReactiveMap o)
            (= (.values o) values)
            (= (.dirty o) dirty)
            (= (.maybe-dirty o) maybe-dirty)))

     clojure.lang.Associative
     (assoc [this k v]
       (or (rmap-assoc specs values dirty maybe-dirty k v)
           this)))

   :cljs
   (deftype ReactiveMap [specs values dirty maybe-dirty]
     ILookup
     (-lookup [this k] (-lookup this k nil))
     (-lookup [this k not-found]
       (rmap-get specs values dirty maybe-dirty k not-found))

     ICollection
     (-conj [this o]
       (reduce (fn [m [k v]] (assoc m k v)) this o))

     IAssociative
     (-assoc [this k v]
       (or (rmap-assoc specs values dirty maybe-dirty k v)
           this))))

(defn reactive-map
  [& args]
  (if (= 1 (count args))
    (ReactiveMap. (first args) (atom {}) (atom #{}) (atom #{}))
    (let [specs (loop [specs {}
                       args args]
                  (if (empty? args)
                    specs
                    (let [[k v & more] args]
                      (cond
                        (keyword? k) (recur (assoc specs k v) more)
                        (map? k) (recur (merge specs k) (cons v more))))))]
      (ReactiveMap. specs (atom {}) (atom #{}) (atom #{})))))

(defn nest
  [k ks & spec-args]
  (let [f (fn [m] (let [self (or (get m k) (apply reactive-map spec-args))]
                   (merge self (dissoc m k))))
        input-schema (p/map-from-keys (constantly s/Any) (cons k ks))]
    {k (pfnk/fn->fnk f [input-schema s/Any])}))
