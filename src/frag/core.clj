(ns frag.core
  (:require [plumbing.core :as p]
            [plumbing.fnk.pfnk :as pfnk]))

(deftype ReactiveMap [specs]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k not-found]
    (let [r (.valAt specs k not-found)]
      (if (and (fn? r) (:schema (meta r)))
        (r specs)
        r))))




(defn reactive-map
  [specs]
  (ReactiveMap. specs))
