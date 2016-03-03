(ns frag.core
  (:require [plumbing.core :as p]
            [plumbing.fnk.pfnk :as pfnk]))

(defn- rmap-get [specs values k not-found]
  (if-let [value (get @values k)]
    value
    (when-let [spec (get specs k)]
      (if (and (fn? spec) (:schema (meta spec)))
        (let [input-keys (pfnk/input-schema-keys spec)
              result (spec (p/map-from-keys #(rmap-get specs values % nil) input-keys))]
          (swap! values assoc k result)
          result)
        spec))))

(defn- dependent-keys
  [specs k]
  (loop [ks #{k}]
    (let [new-ks (->> specs
                      (filter (fn [[_ v]] (and (ifn? v) (:schema (meta v)))))
                      (filter (fn [[_ v]] (some ks (pfnk/input-schema-keys v))))
                      keys
                      set
                      (into ks))]
      (println new-ks)
      (if (= new-ks ks)
        (disj ks k)
        (recur new-ks)))))

(deftype ReactiveMap [specs values]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k not-found] (rmap-get specs values k not-found))

  clojure.lang.Associative
  (assoc [this k v]
    (ReactiveMap. specs (atom (-> (apply dissoc @values (dependent-keys specs k))
                                  (assoc k v))))))

(defn reactive-map
  [specs]
  (ReactiveMap. specs (atom {})))


