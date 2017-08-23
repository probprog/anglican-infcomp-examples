;; gorilla-repl.fileformat = 1

;; **
;;; # Polynomial Regression
;; **

;; @@
(ns worksheets.polynomial-regression
  (:require [anglican.runtime :refer :all]
            [anglican.emit :refer [defquery with-primitive-procedures]]
            [anglican.stat :refer [empirical-distribution collect-predicts collect-by collect-results]]
            anglican.infcomp.csis
            anglican.importance
            [anglican.infcomp.dists :refer :all]
            [anglican.infcomp.zmq :as zmq]
            [anglican.infcomp.prior :as prior]
            [anglican.inference :refer [infer]]
            anglican.rmh
            anglican.smc
            anglican.infcomp.csis
            anglican.importance
            anglican.infcomp.core)
  (:use [gorilla-plot core]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Model
;; **

;; @@
(defn poly [w x]
  (reduce #(-> %1 (* x) (+ %2)) (reverse w)))

(anglican.infcomp.core/reset-infcomp-addressing-scheme!)

(with-primitive-procedures [poly]
  
  (defquery polynomial-regression [inputs outputs]
    
    (map #(observe (uniform-continuous -10 10) %) inputs) ; do some fancy observe here
    
    (let [w0 (sample "w0" (laplace 0 2))
          w1 (sample "w1" (laplace 0 2))
          w2 (sample "w2" (laplace 0 2))]
      (map #(observe (student-t-loc-scale 4 (poly [w0 w1 w2] %1) 1) %2) inputs outputs)
      [w0 w1 w2])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.polynomial-regression/polynomial-regression</span>","value":"#'worksheets.polynomial-regression/polynomial-regression"}
;; <=

;; **
;;; ## Compiling the model
;; **

;; @@
(defn combine-observes-fn [observes]
  (mapv :value observes))

(def replier (zmq/start-replier  polynomial-regression [fixed-inputs (repeat 10 0)] combine-observes-fn))
;; @@

;; **
;;; Stop the replier:
;; **

;; @@
(zmq/stop-replier replier)
;; @@

;; **
;;; ## Test the inference
;; **

;; @@
(first (infer :importance polynomial-regression [ [1 2 3 4] [1 2 3 4] ] ))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:log-weight</span>","value":":log-weight"},{"type":"html","content":"<span class='clj-double'>-57.532383857943046</span>","value":"-57.532383857943046"}],"value":"[:log-weight -57.532383857943046]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:predicts</span>","value":":predicts"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[],"value":"[]"}],"value":"[:predicts []]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:result</span>","value":":result"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.9261490566275945</span>","value":"0.9261490566275945"},{"type":"html","content":"<span class='clj-double'>-1.8734019671216908</span>","value":"-1.8734019671216908"},{"type":"html","content":"<span class='clj-double'>-2.0376262351997023</span>","value":"-2.0376262351997023"}],"value":"[0.9261490566275945 -1.8734019671216908 -2.0376262351997023]"}],"value":"[:result [0.9261490566275945 -1.8734019671216908 -2.0376262351997023]]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:anglican.state/mem</span>","value":":anglican.state/mem"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[],"value":"{}"}],"value":"[:anglican.state/mem {}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:anglican.state/store</span>","value":":anglican.state/store"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:anglican.state/store nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:anglican.state/catch-stack</span>","value":":anglican.state/catch-stack"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:anglican.state/catch-stack nil]"}],"value":"{:log-weight -57.532383857943046, :predicts [], :result [0.9261490566275945 -1.8734019671216908 -2.0376262351997023], :anglican.state/mem {}, :anglican.state/store nil, :anglican.state/catch-stack nil}"}
;; <=

;; **
;;; Plot the results:
;; **

;; @@
(let [x [1 2 3 4]
      y [1 3 7 13]
      samples (drop 100 (infer :rmh polynomial-regression [ x y ]))]
  (loop [results samples reps 200 plots []]
    (if (> reps 1)
    (let [w (:result (first results))]
      (recur (drop 10 results) (dec reps) (conj plots (list-plot (map (fn [m] (vector m (+ (get w 0) (* (get w 1) m) (* (get w 2) m m)))) (range -10 10 0.125)) :joined true :opacity 0.1)))
      )
      (apply compose plots))
    )
  )
;; @@
;; =>
;; <=

;; @@

;; @@