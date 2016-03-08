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
  [specs values dirty maybe-dirty input-keys k v]
  (when-let [[mv md mnd nv nd nmd]
             (rmap-assoc* specs @values @dirty @maybe-dirty k v)]
    (do (reset! values mv)
        (reset! dirty md)
        (reset! maybe-dirty nmd)
        (->ReactiveMap specs
                       (atom nv)
                       (atom nd)
                       (atom nmd)
                       (let [sv (get specs k)]
                         (if (or (not (static-spec? sv))
                                 (not= v sv))
                           (conj input-keys k)
                           (disj input-keys k)))))))

(defn rmap-keys
  [specs values]
  (distinct (concat (keys specs) (keys values))))

(defn spec-input-keys
  "Return any keys that are taken by a spec as input and not defined by another
  spec and any specs that depend on themselves. "
  [specs]
  (let [spec-keys       (-> specs keys set)
        specs-by-static (group-by #(static-spec? (val %)) specs)
        spec-input-map  (p/map-vals (comp set pfnk/input-schema-keys)
                                    (get specs-by-static false))
        all-spec-inputs (set (mapcat val spec-input-map))]
    (set/union
     (set/difference all-spec-inputs spec-keys) ;; expected in spec
     (set (keep (fn [[k v]] (when (contains? v k) k)) ;; spec that takes itself
                spec-input-map)))))

#?(:clj
   (deftype ReactiveMap [specs values dirty maybe-dirty input-keys]
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

     clojure.lang.IPersistentMap
     (assoc [this k v]
       (or (rmap-assoc specs values dirty maybe-dirty input-keys k v)
           this))

     java.lang.Iterable
     (iterator [this]
       (.iterator (seq this)))

     clojure.lang.Associative
     (containsKey [this k]
       (boolean (some #{k} (rmap-keys specs @values))))
     (entryAt [this k]
       (when (contains? this k)
         (clojure.lang.MapEntry. k (get this k))))

     clojure.lang.Seqable
     (seq [this] (for [k (rmap-keys specs @values)]
                   (clojure.lang.MapEntry. k (get this k)))))

   :cljs
   (deftype ReactiveMap [specs values dirty maybe-dirty input-keys]
     ILookup
     (-lookup [this k] (-lookup this k nil))
     (-lookup [this k not-found]
       (rmap-get specs values dirty maybe-dirty k not-found))

     ICollection
     (-conj [this o]
       (reduce (fn [m [k v]] (assoc m k v)) this o))

     IAssociative
     (-assoc [this k v]
       (or (rmap-assoc specs values dirty maybe-dirty input-keys k v)
           this))

     ISeqable
     (-seq [this] (for [k (rmap-keys specs @values)]
                    (vector k (get this k))))

     IPrintWithWriter
     (-pr-writer [this writer opts]
       (print-map this pr-writer writer opts))))

(defn parse-specs
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

(defn reactive-map
  [& args]
  (let [specs (parse-specs args)]
    (ReactiveMap. specs
                  (atom {})
                  (atom #{})
                  (atom #{})
                  (spec-input-keys specs))))

(defn nest
  [k ks & spec-args]
  (let [f (fn [m] (let [self (or (get m k) (apply reactive-map spec-args))]
                   (merge self (dissoc m k))))
        input-schema (p/map-from-keys (constantly s/Any) (cons k ks))]
    {k (pfnk/fn->fnk f [input-schema ReactiveMap])}))

(defn- rmap-specs [m]
  #?(:clj (.specs m)
     :cljs (.-specs m)))

(defn- rmap-values [m]
  #?(:clj @(.values m)
     :cljs @(.-values m)))

(defn input-keys
  "Given a ReactiveMap, return a set of keys that:
   - are expected as input in any spec
   - OR have a value but no spec
   - OR have a value and different static spec
   - OR have a spec that takes its previous value as input"
  [m]
  #?(:clj  (.input-keys m)
     :cljs (.-input-keys m)))

;; After tools.namespace reloads the namespace, ReactiveMap inside of a closure
;; refers to a different object than ReactiveMap at top level. Seems like a bug.
;; This works around it.
(defn- outputs-rmap?
  [[k v]]
  (and (not (static-spec? v))
       (= ReactiveMap (pfnk/output-schema v))))

(defn input-keys-recursive
  "Like input-keys, but recurses through any specs that return a ReactiveMap
  and take themselves as input (such as created with nest).

  NOTE: this requires evaluation of nested ReactiveMaps, which may throw an
        exception if they're expecting input that isn't available"
  [m]
  (when m
    (let [input-key-set       (input-keys m)
          nested-specs        (->> input-key-set
                                   (select-keys (rmap-specs m))
                                   (filter outputs-rmap?))
          nested-param-map    (p/map-vals pfnk/input-schema-keys nested-specs)
          non-nested-inputs   (->> (keys nested-specs)
                                   (set)
                                   (set/difference input-key-set)
                                   (map list))
          remove-fwded-inputs (fn [k v]
                                (->> (get nested-param-map k)
                                     (map list)
                                     (set)
                                     (p/<- (remove v))))
          nested-inputs       (->> (keys nested-specs)
                                   (select-keys m)
                                   (p/map-vals input-keys-recursive)
                                   (mapcat (fn [[k v]]
                                             (map (partial cons k)
                                                  (remove-fwded-inputs k v)))))]
      (set (concat nested-inputs non-nested-inputs)))))

(defn inputs
  [m]
  (select-keys m (input-keys m)))

(defn inputs-recursive
  [m]
  (reduce (fn [s p]
            (let [v (get-in m p ::not-found)]
              (if (= ::not-found v)
                s
                (assoc-in s p v))))
          {}
          (input-keys-recursive m)))
