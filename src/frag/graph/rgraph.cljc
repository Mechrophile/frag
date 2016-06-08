(ns frag.graph.rgraph
  (:require [plumbing.core :as p]
            [plumbing.fnk.pfnk :as pfnk]
            [frag.graph.relations :as rel]
            [frag.cache :as cache]
            [clojure.set :as set]
            #?(:clj  [clojure.core :as core]
               :cljs [cljs.core :as core]))
  (:refer-clojure :exclude [get assoc keys]))

(defrecord RGraph [specs rels cache state])

(defn- recalculate [{:keys [specs rels cache state]} k]
  (let [spec-fn     (core/get specs k)
        fetch-value #(cache/get cache % (core/get state %))
        old-value   (fetch-value k)
        new-value   (->> (rel/parents rels k)
                         (p/map-from-keys fetch-value)
                         spec-fn)]
    ;;(println "recalced" k old-value " => " new-value)
    (cache/assoc! cache k new-value)
    (when (not= new-value old-value)
      (doseq [ck (rel/children rels k)]
        (when (not= ck k)
          ;;(println "dirtying" ck)
          (cache/mark-dirty! cache ck))))))


(defn- undirty [{:keys [specs rels cache state] :as rg} k]
  (when-not (cache/clean? cache k)
    ;; check parents first
    (doseq [pk (rel/parents rels k)]
      (when (not= pk k) (undirty rg pk)))

    ;; if parents were dirty, they'll promote us from maybe-dirty to (definitely-)dirty
    (when (cache/dirty? cache k)
      (recalculate rg k))

    ;; we either recalculated or were already clean
    (cache/mark-clean! cache k)))

(defn ->RGraph [specs rels cache state]
  (RGraph. specs rels cache state))

(defn get [^RGraph {:keys [specs cache state] :as rg} k not-found]
  (when (contains? specs k) (undirty rg k))
  (cache/get cache k (core/get state k not-found)))

(defn assoc [^RGraph {:keys [specs rels cache state] :as rg} k v]
  ;; if we have any descendants that depend on their own state, then their
  ;; current value becomes part of our new object's state
  (let [descendants (rel/descendants rels k)
        loopy-descs (set/intersection (set (rel/loops rels))
                                      (set descendants))]
    (doseq [dk loopy-descs]
      (undirty rg dk))

    (let [new-cache (cache/clone cache)
          new-state (reduce (fn [s k]
                              (cache/mark-dirty! new-cache k)
                              (core/assoc s k (cache/get cache k)))
                            state loopy-descs)]
      (cache/dissoc! new-cache k)
      (doseq [dk descendants]           (cache/maybe-dirty! new-cache dk))
      (doseq [dk (rel/children rels k)] (cache/mark-dirty!  new-cache dk))
      ;;(println "assoc" k v)
      ;;(println "loopy" loopy-descs)
      ;;(println specs new-cache (assoc new-state k v))
      (->RGraph specs rels new-cache (core/assoc new-state k v)))))

(defn keys [^RGraph {:keys [specs state]}]
  (distinct (mapcat core/keys [specs state])))

(defn- fnk?
  "True if x is a plumbing.core fnk."
  [x]
  (boolean
   (and (fn? x), (:schema (meta x)))))

(defn- specs->relations
  "Create a Relations record from a map of pfnks."
  [specs]
  (rel/relations (p/map-vals pfnk/input-schema-keys specs)))

(defn- parse-specs
  [args]
  (loop [specs {}
         args  args]
    (if (empty? args)
      specs
      (let [[k v & more] args]
        (cond
          (keyword? k) (recur (core/assoc specs k v) more)
          (map? k)     (recur (merge specs k) (when v (cons v more)))
          :else        (throw (new #?(:clj RuntimeException :cljs js/Error)
                                   "unexpected value in key position")))))))

(defn rgraph [& specs]
  (let [specs         (parse-specs specs)
        specs-by-fnk  (group-by #(fnk? (val %)) specs)
        static-specs  (into {} (core/get specs-by-fnk false))
        dynamic-specs (into {} (core/get specs-by-fnk true))
        cache         (cache/cache static-specs)]
    (doseq [k (core/keys dynamic-specs)]
      (cache/mark-dirty! cache k))
    (->RGraph specs (specs->relations dynamic-specs) cache {})))

(defn input-keys
  [{:keys [rels specs state]}]
  (set/union
   (set/difference (set (rel/sources rels))
                   (set (map key (remove #(fnk? (val %)) specs))))
   (set (rel/loops rels))
   (set (keys state))))
