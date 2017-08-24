;; gorilla-repl.fileformat = 1

;; **
;;; # Factorial HMM
;; **

;; @@
(ns worksheets.factorial-hmm
  (:require [anglican.runtime :refer :all]
            [anglican.emit :refer [defquery]]
            [anglican.stat :as stat]
            [anglican.infcomp.zmq :as zmq]
            [anglican.inference :refer [infer]]
            [anglican.infcomp.prior :as prior]
            [gorilla-plot.core :as plt]
            [clojure.string :as str]
            [clojure.core.matrix :as m]
            anglican.smc
            anglican.infcomp.csis
            anglican.importance
            anglican.infcomp.core))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Model Definition
;; **

;; @@
(anglican.infcomp.core/reset-infcomp-addressing-scheme!)
(defquery factorial-hmm [observations init-probs trans-dists weights variance]
   												; observations is [ time sequence for y ]
  												; trans-dists is [ [two dists for feature 1] [two dists for feature 2] ... ]
  												; init-dists is [ probability of being on for feature 1, probability of being on for feature 2, ... ]
  												; weights is a vector of weight for each state if its on
  (reduce
    (fn [states obs]
      (let [state (if (empty? states)
                     (mapv #(sample (flip %)) init-probs)
                     (mapv #(sample (nth %1 %2)) trans-dists (peek states)))]
        (observe (normal (reduce + (map * weights states)) (sqrt variance)) obs)
        (conj states state)))
    []
    observations))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.factorial-hmm/factorial-hmm</span>","value":"#'worksheets.factorial-hmm/factorial-hmm"}
;; <=

;; **
;;; ## Get the data
;; **

;; @@
(let [features 20
       
      model (slurp "plots/factorial-hmm/model.csv")
      model (str/split model #"\n")
      model (mapv #(str/split % #",") model)
      model (mapv #(mapv read-string %) model)]
  (def observations (mapv read-string (str/split (slurp "plots/hmm/data_1.csv") #",")))
  (def init-probs (mapv (first model)))
  (def trans-dists {0 (discrete (second model))
                    1 (discrete (nth model 2))
                    2 (discrete (nth model 3))})
  (def obs-dists (apply hash-map (flatten (mapv vector (range 3) (mapv #(normal %1 %2) (nth model 4) (nth model 5)))))))
;; @@
