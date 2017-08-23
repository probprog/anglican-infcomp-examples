;; gorilla-repl.fileformat = 1

;; **
;;; # State Space Model
;; **

;; @@
(ns worksheets.state-space
  (:require [anglican.runtime :refer :all]
            [anglican.emit :refer [defquery]]
            [anglican.stat :as stat]
            [anglican.infcomp.zmq :as zmq]
            [anglican.inference :refer [infer]]
            [anglican.infcomp.prior :as prior]
            [gorilla-plot.core :as plt]
            [clojure.string :as str]
            anglican.smc
            anglican.infcomp.csis
            anglican.importance
            anglican.infcomp.core))
;; @@

;; **
;;; Define the state space model:
;; **

;; @@
(anglican.infcomp.core/reset-infcomp-addressing-scheme!)

(defquery state-space [observations init-dist trans-mult trans-offset trans-var obs-mult obs-offset obs-var ]
  (reduce
    (fn [states obs]
      (let [state (if (empty? states)
                    (sample init-dist)
                    (sample (normal (+ trans-offset (* trans-mult (peek states))) trans-var)))]
        (observe (normal (+ obs-offset (* obs-mult state)) obs-var) obs)
        (conj states state)))
    []
    observations))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.state-space/state-space</span>","value":"#'worksheets.state-space/state-space"}
;; <=

;; **
;;; Gather data and parameters from the csv file:
;; **

;; @@
(def test-file "3
  4
  1
  5
  2
  1
  3
  3")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.state-space/test-file</span>","value":"#'worksheets.state-space/test-file"}
;; <=

;; @@
(let [file test-file;(slurp "plots/state-space/model.csv")
      file (str/split file #"\n")
      file (map read-string file)
      
      data "3,3,3,3,3,4";(slurp "plots/state-space/data_1.csv")
      data (str/split data #",")
      data (map read-string data)]
      
  (def init-dist (normal (nth file 0) (nth file 1)))
  
  (def trans-mult (nth file 2))
  (def trans-offset (nth file 3))
  (def trans-var (nth file 4))
  
  (def obs-mult (nth file 5))
  (def obs-offset (nth file 6))
  (def obs-var (nth file 7))
  (def observations data))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.state-space/observations</span>","value":"#'worksheets.state-space/observations"}
;; <=

;; @@
(defn combine-observes-fn [observes] (map :value observes))
(def replier (zmq/start-replier state-space [observations init-dist trans-mult trans-offset trans-var obs-mult obs-offset obs-var] combine-observes-fn))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.state-space/replier</span>","value":"#'worksheets.state-space/replier"}
;; <=

;; @@
(zmq/stop-replier replier)
;; @@

;; @@
(defn print-inference-results [num-particles algorithm-name]
  (let [algorithm (case algorithm-name
                    "is" :importance
                    "smc" :smc
                    "csis" :csis)
        states (take num-particles (infer algorithm state-space [observations init-dist trans-mult trans-offset trans-var obs-mult obs-offset obs-var]))
        inference-result-string (str/join
                                  (map (fn [state] (str/join "," (cons (:log-weight state) (:result state)))) states)
                                  "\n")]
    (spit (str "plots/state-space/" algorithm-name "_1_" num-particles ".csv") inference-result-string)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.state-space/print-inference-results</span>","value":"#'worksheets.state-space/print-inference-results"}
;; <=

;; @@
(def particles-range [1 2 3 4 5])

;(map #(print-inference-results % "csis") particles-range)
(map #(print-inference-results % "is") particles-range)
(map #(print-inference-results % "smc") particles-range)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"(nil nil nil nil nil)"}
;; <=

;; @@
(prior/sample-from-prior state-space [observations init-dist trans-mult trans-offset trans-var obs-mult obs-offset obs-var])
;; @@

;; @@

;; @@
