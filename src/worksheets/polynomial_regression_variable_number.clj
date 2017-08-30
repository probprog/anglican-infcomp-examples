;; gorilla-repl.fileformat = 1

;; **
;;; # Polynomial Regression
;; **

;; @@
(ns worksheets.polynomial-regression-variable-number
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
    (let [inputs (map #(observe (uniform-continuous -10 10) %) inputs) ; this will not work with inference algorithms other than csis (remove this line for it to work with the other inference algorithms)
          w0 (sample (laplace 0 2))
          w1 (sample (laplace 0 2))
          w2 (sample (laplace 0 2))]
      (map #(observe (student-t-loc-scale 4 (poly [w0 w1 w2] %1) 1) %2) inputs outputs)
      [w0 w1 w2])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.polynomial-regression-variable-number/polynomial-regression</span>","value":"#'worksheets.polynomial-regression-variable-number/polynomial-regression"}
;; <=

;; **
;;; ## Compiling the model
;; **

;; @@
(defn combine-observes-fn [observes]
  (vector (mapv :value observes)))

(def replier (zmq/start-replier polynomial-regression [(repeat 10 0) (repeat 10 0)] combine-observes-fn :observe-embedder-input (mapv #(vector %1 %2) (repeat 10 0) (repeat 10 0))))
;; @@

;; @@
(zmq/stop-replier replier)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;ZMQ connection terminated.&quot;</span>","value":"\"ZMQ connection terminated.\""}
;; <=

;; **
;;; ## Test the inference
;; **

;; @@
(defn getData [dataset-no]
  (let [file (map #(into [] (map read-string (str/split % #","))) (str/split (slurp (str "plots/polynomial-regression-variable-x/data_" dataset-no ".csv")) #"\n"))]
    {:in (nth file 1) :out (nth file 2)}
    ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.polynomial-regression-variable-number/getData</span>","value":"#'worksheets.polynomial-regression-variable-number/getData"}
;; <=

;; @@
(defn print-inference-results [num-particles algorithm-name dataset-no]
  (let [data (getData dataset-no)
        x (:in data)
        y (:out data)
        algorithm (case algorithm-name
                    "is" :importance
                    "smc" :smc
                    "csis" :csis)
        algorithm-options (case algorithm-name
                            "is" []
                            "smc" [:number-of-particles num-particles]
                            "csis" [:observe-embedder-input (concat x y)])
        states (take num-particles (apply infer algorithm polynomial-regression [x y] algorithm-options))
        inference-result-string (str/join
                                  "\n"
                                  (map (fn [state] (str/join "," (cons (:log-weight state) (:result state)))) states))]
    (spit (str "plots/polynomial-regression-variable-x/" algorithm-name "_" dataset-no "_" num-particles ".csv") inference-result-string)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.polynomial-regression-variable-number/print-inference-results</span>","value":"#'worksheets.polynomial-regression-variable-number/print-inference-results"}
;; <=

;; @@
(let [inputs (into [] (range 10))
      outputs (into [] (range 10))]
  (first (infer :csis polynomial-regression [inputs outputs] :observe-embedder-input (mapv #(vector %1 %2) inputs outputs)))
  )
;; @@

;; @@
(let [dataset-no 3
      particle-range [10 20 40 80 160 320 640 1280 2560]]
  (map #(print-inference-results % "csis" dataset-no) particle-range))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"(nil nil nil nil nil nil nil nil nil)"}
;; <=

;; **
;;; Plot the results:
;; **

;; @@
(let [x [1 2 3 4 5 6 7 8 9 10]
      y (into [] (map #(poly [1 1 1] %) x))]
  (println x y))
;; @@
;; ->
;;; [1 2 3 4 5 6 7 8 9 10] [3 7 13 21 31 43 57 73 91 111]
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=
