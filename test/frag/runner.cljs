(ns frag.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [frag.core-test]))

(doo-tests 'frag.core-test)
