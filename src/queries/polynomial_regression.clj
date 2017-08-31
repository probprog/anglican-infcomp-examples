;; gorilla-repl.fileformat = 1

;; @@
(ns queries.polynomial-regression
(:require [anglican.runtime :refer :all]
            [anglican.emit :refer [defquery with-primitive-procedures]]
            [anglican.stat :refer [empirical-distribution collect-predicts collect-by collect-results]]
            anglican.infcomp.csis
            anglican.importance
            [anglican.infcomp.dists :refer :all]
            [anglican.infcomp.zmq :as zmq]
            [anglican.infcomp.prior :as prior]
            [anglican.inference :refer [infer]]
            [clojure.string :as str]
            anglican.infcomp.csis
            anglican.infcomp.core))

(anglican.infcomp.core/reset-infcomp-addressing-scheme!)

(defn poly [w x]
  (reduce #(-> %1 (* x) (+ %2)) (reverse w)))

(with-primitive-procedures [poly pow]
  (defquery polynomial-regression [inputs outputs]
    (let [num_inputs (observe (uniform-discrete 5 11) (count inputs))
          inputs (map #(observe (uniform-continuous -1 1) (nth inputs %)) (range num_inputs))
          
          order (sample (discrete [0.25 0.25 0.2 0.15 0.1 0.05]))
          W (repeatedly (inc order) #(sample (laplace 0 1)))]
      (map #(observe (student-t-loc-scale 4 (poly W %1) 0.1) %2) inputs outputs)
      W)))

(defn combine-observes-fn [observes]
  (let [mess (mapv :value observes)
        count_x (first mess)
        X (take count_x (rest mess))
        Y (take count_x (drop count_x (rest mess)))]
    (mapv #(vector %1 %2) X Y)))

(defn get-data [file-path]
  (let [file (map #(into [] (map read-string (str/split % #","))) (str/split (slurp file-path) #"\n"))
        x (nth file 0)
        y (nth file 1)]
    [x y]))

(def compile-args [(repeat 10 0) (repeat 10 0)])

(defn create-obs-emb-input [args]
  (let [x (first args)
        y (second args)]
    (mapv #(vector %1 %2) x y)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;queries.polynomial-regression/create-obs-emb-input</span>","value":"#'queries.polynomial-regression/create-obs-emb-input"}
;; <=
