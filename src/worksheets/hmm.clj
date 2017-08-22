;; gorilla-repl.fileformat = 1

;; @@
(ns worksheets.gaussian
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
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Define the HMM query:
;; **

;; @@
( defquery hmm
	[ observations init-dist trans-dists obs-dists ]
	( reduce
			( fn [ states obs ]
				( let [ state ( sample ( get trans-dists
											( peek states )))]
					( observe ( get obs-dists state ) obs )
					( conj states state )))
				[( sample init-dist )]
				observations ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.gaussian/hmm</span>","value":"#'worksheets.gaussian/hmm"}
;; <=

;; **
;;; Gather the parameters and data from the file:
;; **

;; @@
(let [spec (slurp "plots/hmm/model.csv")
      spec (str/split spec #"\n")
  	  spec (into [] (map #(str/split % #",") spec))
      
      init-dist (categorical (map #(vector %1 %2) (range) (map read-string (get spec 0))))
      
      trans-probs [(into [] (map read-string (get spec 1))) (into [] (map read-string (get spec 2))) (into [] (map read-string (get spec 3)))]
      trans-dists (into [] (map (fn [probs] (categorical (map #(vector %1 %2) (range) probs))) trans-probs))
      
      obs-dists (into [] (map #(normal %1 %2) (map read-string (get spec 4)) (map read-string (get spec 5))))
      
      observations (map read-string (str/split (slurp "plots/hmm/data_1.csv") #","))
      ]
  
  (def observations observations)
  (def init-dist init-dist)
  (def trans-dists trans-dists)
  (def obs-dists obs-dists)
  (def particles-range [1 10 100 1000 10000])
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.gaussian/particles-range</span>","value":"#'worksheets.gaussian/particles-range"}
;; <=

;; **
;;; Do SMC and importance sampling:
;; **

;; @@
(defn getImportanceResults [ num-particles ]
  (let [raw-results (take num-particles (infer :importance hmm [observations init-dist trans-dists obs-dists] ))
        formatted-lines (map (fn [particle] (str/join "," (cons (:log-weight particle) (:result particle))) ) raw-results)
        nicely-formatted-results (str/join "\n" formatted-lines)
        ]
    (spit (str/join ["plots/hmm/is_1_" (str num-particles) ".csv"]) nicely-formatted-results)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.gaussian/getImportanceResults</span>","value":"#'worksheets.gaussian/getImportanceResults"}
;; <=

;; @@
(map getImportanceResults particles-range)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"(nil nil nil nil nil)"}
;; <=

;; @@
(defn getSMCResults [ num-particles ]
  (let [raw-results (take num-particles (infer :smc hmm [observations init-dist trans-dists obs-dists] :number-of-particles 1000))
        formatted-lines (map (fn [particle] (str/join "," (cons (:log-weight particle) (:result particle))) ) raw-results)
        nicely-formatted-results (str/join "\n" formatted-lines)
        ]
    (spit (str/join ["plots/hmm/smc_1_" (str num-particles) ".csv"]) nicely-formatted-results)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.gaussian/getSMCResults</span>","value":"#'worksheets.gaussian/getSMCResults"}
;; <=

;; @@
(map getSMCResults particles-range)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"(nil nil nil nil nil)"}
;; <=

;; **
;;; Start the compiler:
;; **

;; @@
(defn combine-observes-fn [ observes ] (map :value observes))

(def replier (zmq/start-replier hmm [observations init-dist trans-dists obs-dists] combine-observes-fn ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.gaussian/replier</span>","value":"#'worksheets.gaussian/replier"}
;; <=

;; @@
(combine-observes-fn (prior/sample-observes-from-prior hmm [observations init-dist trans-dists obs-dists]))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>1.5936254932200566</span>","value":"1.5936254932200566"},{"type":"html","content":"<span class='clj-double'>-0.8905675819828675</span>","value":"-0.8905675819828675"},{"type":"html","content":"<span class='clj-double'>-1.4835860964772203</span>","value":"-1.4835860964772203"},{"type":"html","content":"<span class='clj-double'>1.2072729253016774</span>","value":"1.2072729253016774"},{"type":"html","content":"<span class='clj-double'>1.9199742950720333</span>","value":"1.9199742950720333"},{"type":"html","content":"<span class='clj-double'>0.24701022119150806</span>","value":"0.24701022119150806"},{"type":"html","content":"<span class='clj-double'>-0.6619913700452689</span>","value":"-0.6619913700452689"},{"type":"html","content":"<span class='clj-double'>0.4280336638442295</span>","value":"0.4280336638442295"},{"type":"html","content":"<span class='clj-double'>-1.4062279853844346</span>","value":"-1.4062279853844346"},{"type":"html","content":"<span class='clj-double'>0.16200134848318748</span>","value":"0.16200134848318748"},{"type":"html","content":"<span class='clj-double'>0.965336543350232</span>","value":"0.965336543350232"},{"type":"html","content":"<span class='clj-double'>-1.9458226342558367</span>","value":"-1.9458226342558367"},{"type":"html","content":"<span class='clj-double'>-0.3003680644459726</span>","value":"-0.3003680644459726"},{"type":"html","content":"<span class='clj-double'>0.054301685553939445</span>","value":"0.054301685553939445"},{"type":"html","content":"<span class='clj-double'>-0.7661193594731306</span>","value":"-0.7661193594731306"},{"type":"html","content":"<span class='clj-double'>-0.6115797814887327</span>","value":"-0.6115797814887327"}],"value":"(1.5936254932200566 -0.8905675819828675 -1.4835860964772203 1.2072729253016774 1.9199742950720333 0.24701022119150806 -0.6619913700452689 0.4280336638442295 -1.4062279853844346 0.16200134848318748 0.965336543350232 -1.9458226342558367 -0.3003680644459726 0.054301685553939445 -0.7661193594731306 -0.6115797814887327)"}
;; <=

;; @@
(zmq/stop-replier replier)
;; @@

;; @@
(defn getCSISResults [ num-particles ]
  (let [raw-results (take num-particles (infer :csis hmm [observations init-dist trans-dists obs-dists] ))
        formatted-lines (map (fn [particle] (str/join "," (cons (:log-weight particle) (:result particle))) ) raw-results)
        nicely-formatted-results (str/join "\n" formatted-lines)
        ]
    (spit (str/join ["plots/hmm/csis_1_" (str num-particles) ".csv"]) nicely-formatted-results)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.gaussian/getCSISResults</span>","value":"#'worksheets.gaussian/getCSISResults"}
;; <=

;; @@
(map getCSISResults particles-range)
;; @@

;; @@

;; @@
