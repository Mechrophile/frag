(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh disable-reload!]]
            [clojure.tools.namespace.dir :as nd]
            [clojure.tools.namespace.track :as nt]
            [clojure.tools.namespace.reload :as nr]
            [clojure.test :refer [run-tests]]
            [plumbing.core :as p]
            [frag.core :as f]))

(disable-reload!)

(defonce tracker (atom (nt/tracker)))
(defonce autotester (atom nil))

(defn update-tracker! [tracker]
  (swap! tracker (comp #'clojure.tools.namespace.repl/remove-disabled
                    nd/scan)))

(defn needs-reload? [tracker]
  (or (not-empty (:clojure.tools.namespace.track/load @tracker))
      (not-empty (:clojure.tools.namespace.track/unload @tracker))))

(defn do-reload! [tracker]
  (swap! tracker nr/track-reload)
  (not (:clojure.tools.namespace.reload/error @tracker)))

(defn check-reload [tracker]
  (update-tracker! tracker)
  (when (needs-reload? tracker)
    (when (do-reload! tracker)
      (run-tests 'frag.core-test))))

(defn autotest []
  (if (and @autotester (not (future-cancelled? @autotester)))
    (do (println "Autotest stopped")
        (future-cancel @autotester))
    (reset! autotester (future (while true
                                 (check-reload tracker)
                                 (Thread/sleep 500))))))


(def complex-map
  (f/reactive-map
   :a (p/fnk [b c d] (+ b c d))
   (f/nest :bn [] :v (p/fnk [i] i))
   :b (p/fnk [bn] (:v bn))
   (f/nest :cn [:b] :v (p/fnk [i b] (+ (or i 0) (or b 0))))
   :c (p/fnk [cn] (:v cn))
   (f/nest :dn [:b :c] :v (p/fnk [i b c] (+ (or i 0)
                                            (or b 0)
                                            (or c 0))))
   :d (p/fnk [dn] (:v dn))))

