(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh disable-reload!]]
            [clojure.tools.namespace.dir :as nd]
            [clojure.tools.namespace.track :as nt]
            [clojure.tools.namespace.reload :as nr]
            [clojure.test :refer [run-tests]]))

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
