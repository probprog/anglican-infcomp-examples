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
      (let [ state (if (empty? states)
                     (mapv #(sample (discrete [(- 1 %) %])) init-probs)
                     (mapv #(sample (nth %1 %2)) trans-dists (peek states)))]
        (observe (normal (reduce + (map * weights states)) (sqrt variance)) obs)
        (conj states state)))
    []
    observations))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.factorial-hmm/factorial-hmm</span>","value":"#'worksheets.factorial-hmm/factorial-hmm"}
;; <=

;; @@
(map * [1 2 3] [4 5 6])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>18</span>","value":"18"}],"value":"(4 10 18)"}
;; <=

;; **
;;; ## Get the data
;; **

;; @@
(let [num-features 20
      model (slurp "plots/factorial-hmm/model.csv")
      model (str/split model #"\n")
      model (mapv #(str/split % #",") model)
      model (mapv #(mapv read-string %) model)]
  ;(def observations (mapv read-string (str/split (slurp "plots/factorial-hmm/data_1.csv") #",")))
  (def init-probs (first model))
  (def trans-dists {0 (discrete (second model))
                    1 (discrete (nth model 2))
                    2 (discrete (nth model 3))})
  (let [transitions (subvec model 1 (+ 1 num-features))]
    (def trans-dists (loop [i 0 trans-dists []]
                       (if (< i num-features)
                         (recur
                           (inc i)
                           (conj trans-dists [(discrete (subvec (nth transitions i) 0 2)) (discrete (subvec (nth transitions i) 2 4))]))
                         trans-dists))))
  (def weights (nth model (+ 1 num-features)))
  (def obs-variance (first (nth model (+ 2 num-features)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.factorial-hmm/obs-variance</span>","value":"#'worksheets.factorial-hmm/obs-variance"}
;; <=

;; @@
(println (type trans-dists)
(type (first trans-dists))
(type (first (first trans-dists))))
;; @@
;; ->
;;; clojure.lang.PersistentVector clojure.lang.PersistentVector anglican.runtime.discrete-distribution
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn combine-observes-fn [observes] (map #(vector (:value %)) observes))

(let [observations (repeat 10 0)]
  (first (prior/sample-observes-from-prior factorial-hmm [observations init-probs trans-dists weights obs-variance])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.factorial-hmm/combine-observes-fn</span>","value":"#'worksheets.factorial-hmm/combine-observes-fn"}
;; <=

;; @@

;; @@
