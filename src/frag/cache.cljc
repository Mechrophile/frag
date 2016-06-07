(ns frag.cache
  "A mutable map with a three-state dirty bit per key.
  Keys may contain a single value and can be marked as clean, dirty, or maybe-dirty.
  Keys may be marked clean or dirty even if they have no associated value."
  (:refer-clojure :exclude [get assoc! dissoc!])
  (:require [plumbing.core :as p]))

(defn cache
  "Create a cache, with optional default (clean) contents."
  ([] (cache {}))
  ([default-values] {:pre [(map? default-values)]}
   (atom (p/map-vals #(assoc {} :value %)
                     default-values))))

(defn clone
  "Create a new cache with the same values and dirty statuses as an existing cache."
  [cache]
  (atom @cache))

(defn status
  "Return the dirty status of a key, one of #{ :dirty :maybe-dirty :clean :missing }"
  [cache k]
  (if (contains? @cache k)
    (condp = (get-in @cache [k :dirty])
      true   :dirty
      :maybe :maybe-dirty
      nil    :clean)
    :missing))

(defn dirty?
  "Return true if k is present in cache and marked dirty."
  [cache k]
  (= :dirty (status cache k)))

(defn maybe-dirty?
  "Return true if k is present in cache and marked maybe-dirty."
  [cache k]
  (= :maybe-dirty (status cache k)))

(defn clean?
  "Return true if k is present in cache and is marked clean, i.e. it is not marked dirty or maybe-dirty."
  [cache k]
  (= :clean (status cache k)))

(defn mark-dirty!
  "Unconditionally mark a key as dirty."
  [cache k]
  (swap! cache assoc-in [k :dirty] true))

(defn maybe-dirty!
  "Mark a clean key as maybe-dirty. Does nothing if the key is already marked dirty."
  [cache k]
  (swap! cache update-in [k :dirty]
         #(or % :maybe)))

(defn mark-clean!
  "Unconditionally mark a key as clean."
  [cache k]
  (swap! cache update k dissoc :dirty))

(defn contains-value?
  "Return true if a value for the specified key is present in the cache and has a value, regardless of its dirty status."
  [cache k]
  (contains? (get-in @cache [k]) :value))

(defn get
  "Get the value of a key in the cache, regardless of its dirty status."
  ([cache k] (get cache k nil))
  ([cache k not-found]
   (get-in @cache [k :value] not-found)))

(defn assoc!
  "Store value in cache and clear dirty flags"
  [cache k v]
  (swap! cache assoc k {:value v}))

(defn dissoc!
  "Remove value from cache and clear dirty flags"
  [cache k]
  (swap! cache dissoc k))
