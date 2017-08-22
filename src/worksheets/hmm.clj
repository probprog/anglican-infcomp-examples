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

;; @@
(let [spec (slurp "plots/hmm/model.csv")
      spec (str/split spec #"\n")
  	  spec (into [] (map #(str/split % #",") spec))
      
      init-dist (categorical (map #(vector %1 %2) (range) (map read-string (get spec 0))))
      trans-probs (into [] (map read-string (get spec 1)))
      trans-probs [(subvec trans-probs 0 3) (subvec trans-probs 3 6) (subvec trans-probs 6 9)]			; change this for the new data style
      trans-dists (into [] (map (fn [probs] (categorical (map #(vector %1 %2) (range) probs))) trans-probs))
      obs-dists (into [] (map #(normal %1 %2) (map read-string (get spec 2)) (map read-string (get spec 3))))
      
      observations (map read-string (str/split (slurp "plots/hmm/data_1.csv") #","))
      ]
  
  (take 10 (infer :importance hmm [ observations init-dist trans-dists obs-dists ] ))
  )
;; @@

;; @@

;; @@
