(ns queries.gaussian
  (:require [anglican.runtime :refer :all]
            [anglican.emit :refer [defquery]]
            anglican.infcomp.core))

(anglican.infcomp.core/reset-infcomp-addressing-scheme!)

(defquery gaussian [obs]
  (let [mean (sample (normal 0 1))
        std 0.1]
    (observe (normal mean std) obs)
    mean))
