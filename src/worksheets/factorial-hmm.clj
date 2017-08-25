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
(defquery factorial-hmm [observations init-probs trans-dists weights obs-variance]
   												; observations is [ time sequence for y ]
  												; trans-dists is [ [two dists for feature 1] [two dists for feature 2] ... ]
  												; init-dists is [ probability of being on for feature 1, probability of being on for feature 2, ... ]
  												; weights is a vector of weight for each state if its on
  (reduce
    (fn [states obs]
      (let [ state (if (empty? states)
                     (map #(sample (discrete [(- 1 %) %])) init-probs)
                     (map #(sample (nth %1 %2)) trans-dists (peek states)))]
        (observe (normal (reduce + 0 (map * weights state)) (sqrt obs-variance)) obs)
        (conj states state)))
    []
    observations))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.factorial-hmm/factorial-hmm</span>","value":"#'worksheets.factorial-hmm/factorial-hmm"}
;; <=

;; @@
(defn gather-data [ dataset ] 
	(let [num-features 20
    	  model (slurp "plots/factorial-hmm/model.csv")
    	  model (str/split model #"\n")
    	  model (mapv #(str/split % #",") model)
    	  model (mapv #(mapv read-string %) model)]
	  (def observations (mapv read-string (str/split (slurp (str "plots/factorial-hmm/data_" dataset ".csv")) #",")))
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
	  (def obs-variance (first (nth model (+ 2 num-features))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.factorial-hmm/gather-data</span>","value":"#'worksheets.factorial-hmm/gather-data"}
;; <=

;; @@
(gather-data 1)

(defn combine-observes-fn [observes] (map #(vector (:value %)) observes))

(def replier (zmq/start-replier factorial-hmm [observations init-probs trans-dists weights obs-variance] combine-observes-fn ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.factorial-hmm/replier</span>","value":"#'worksheets.factorial-hmm/replier"}
;; <=

;; @@
(zmq/stop-replier replier)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;ZMQ connection terminated.&quot;</span>","value":"\"ZMQ connection terminated.\""}
;; <=

;; @@
(defn print-inference-results [num-particles algorithm-name dataset-no]
  (gather-data dataset-no)
  (print (first observations))
  (let [algorithm (case algorithm-name
                    "is" :importance
                    "smc" :smc
                    "csis" :csis)
        algorithm-options (case algorithm-name
                            "is" []
                            "smc" [:number-of-particles num-particles]
                            "csis" [:observe-embedder-input (map vector observations)])
        states (take num-particles (apply infer algorithm factorial-hmm [observations init-probs trans-dists weights obs-variance] algorithm-options))
        inference-result-string (str/join
                                  "\n"
                                  (map (fn [state] (str/join "," (cons (:log-weight state) (flatten (:result state))))) states))]
    (spit (str "/home/shared/infcomp/17-08-24-factorial-hmm/" algorithm-name "_" dataset-no "_" num-particles ".csv") inference-result-string)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.factorial-hmm/print-inference-results</span>","value":"#'worksheets.factorial-hmm/print-inference-results"}
;; <=

;; @@
(def dataset 1)
(def particle-range [10 20 40 80 160 320 640 1280 2560 5120 10240])

(map #(print-inference-results % "smc" dataset) particle-range)
(map #(print-inference-results % "is" dataset) particle-range)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"(849.9049926315663849.9049926315663849.9049926315663849.9049926315663849.9049926315663849.9049926315663849.9049926315663849.9049926315663nil nil nil nil nil nil nil nil)"}
;; <=

;; @@
(gather-data 1)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.factorial-hmm/obs-variance</span>","value":"#'worksheets.factorial-hmm/obs-variance"}
;; <=

;; @@

;; @@
