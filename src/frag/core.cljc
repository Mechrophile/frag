(ns frag.core
  (:require [clojure.set :as set]
            [frag.graph.relations :as rel]
            [frag.graph.rgraph :as rg]
            [plumbing.core :as p]
            [plumbing.fnk.pfnk :as pfnk]
            [schema.core :as s]))

#?(:clj
   (deftype ReactiveMap [rg]
     clojure.lang.ILookup
     (valAt [this k] (.valAt this k nil))
     (valAt [this k not-found]
       (rg/get rg k not-found))

     clojure.lang.IPersistentCollection
     (cons [this o]
       (reduce (fn [m [k v]] (assoc m k v))
               this o))

     (equiv [this o]
       (and (instance? ReactiveMap o)
            (= (-> o .rg .specs) (-> this .rg .specs))
            (= (-> o .rg .state) (-> this .rg .state))))

     clojure.lang.IPersistentMap
     (assoc [this k v]
       (ReactiveMap. (rg/assoc rg k v)))

     java.lang.Iterable
     (iterator [this]
       (.iterator (seq this)))

     clojure.lang.Associative
     (containsKey [this k]
       (boolean (some #{k} (rg/keys rg))))
     (entryAt [this k]
       (when (contains? this k)
         (clojure.lang.MapEntry. k (get this k))))

     clojure.lang.Seqable
     (seq [this]
       (map #(find this %)
            (rg/keys rg))))

   :cljs
   (deftype ReactiveMap [rg]
     ILookup
     (-lookup [this k] (-lookup this k nil))
     (-lookup [this k not-found] (rg/get rg k not-found))

     ICollection
     (-conj [this o] (reduce (fn [m [k v]] (assoc m k v)) this o))

     IAssociative
     (-assoc [this k v] (ReactiveMap. (rg/assoc rg k v)))

     ISeqable
     (-seq [this] (map #(find this %) (rg/keys rg)))

     IPrintWithWriter
     (-pr-writer [this writer opts]
       (print-map this pr-writer writer opts))))

;; sometimes when reloading ns, ReactiveMap in closure doesn't update. bug?
(defn- rmap? [o]
  (instance? ReactiveMap o))

(defn reactive-map [& specs]
  (ReactiveMap. (apply rg/rgraph specs)))

(defn state
  [m]
  #?(:clj  (-> m .rg .state)
     :cljs (-> m .-rg .-state)))

(defn specs
  [m]
  #?(:clj  (-> m .rg .specs)
     :cljs (-> m .-rg .-specs)))

(defn rels
  [m]
  #?(:clj  (-> m .rg .rels)
     :cljs (-> m .-rg .-rels)))

(defn nest
  [k ks & spec-args]
  (let [f (fn [params]
            (let [old-value (get params k)
                  new-params (dissoc params k)]
              (if (rmap? old-value)
                (merge old-value new-params)
                (merge (apply reactive-map spec-args)
                       old-value new-params))))
        input-schema (p/map-from-keys (constantly s/Any) (cons k ks))]
    {k (pfnk/fn->fnk f [input-schema ReactiveMap])}))

(defn state-recursive
  [m]
  (into
   {}
   (for [[k v] (state m)]
     (if-not (rmap? v)
       [k v]
       (let [inherited-keys     (rel/parents (rels m) k)
             child-state        (state-recursive v)
             non-inherited-keys (set/difference (set (keys child-state))
                                                (set inherited-keys))
             result             (select-keys child-state non-inherited-keys)]
         (when-not (empty? result)
           [k result]))))))
