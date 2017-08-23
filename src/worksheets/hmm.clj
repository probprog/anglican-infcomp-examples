;; gorilla-repl.fileformat = 1

;; **
;;; # Hidden Markov model
;; **

;; @@
(ns worksheets.hmm
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
(anglican.infcomp.core/reset-infcomp-addressing-scheme!)
(defquery hmm [observations init-dist trans-dists obs-dists]
  (reduce
    (fn [states obs]
      (let [state (if (empty? states)
                    (sample init-dist)
                    (sample (get trans-dists (peek states))))]
        (observe (get obs-dists state) obs)
        (conj states state)))
    []
    observations))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.hmm/hmm</span>","value":"#'worksheets.hmm/hmm"}
;; <=

;; **
;;; Gather the parameters and data from the file:
;; **

;; @@
(let [model (slurp "plots/hmm/model.csv")
      model (str/split model #"\n")
      model (mapv #(str/split % #",") model)
      model (mapv #(mapv read-string %) model)]
  (def observations (mapv read-string (str/split (slurp "plots/hmm/data_1.csv") #",")))
  (def init-dist (discrete (first model)))
  (def trans-dists {0 (discrete (second model))
                    1 (discrete (nth model 2))
                    2 (discrete (nth model 3))})
  (def obs-dists (apply hash-map (flatten (mapv vector (range 3) (mapv #(normal %1 %2) (nth model 4) (nth model 5)))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.hmm/obs-dists</span>","value":"#'worksheets.hmm/obs-dists"}
;; <=

;; **
;;; Start the compiler:
;; **

;; @@
(defn combine-observes-fn [observes] (map :value observes))
(def replier (zmq/start-replier hmm [observations init-dist trans-dists obs-dists] combine-observes-fn))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.hmm/replier</span>","value":"#'worksheets.hmm/replier"}
;; <=

;; @@
(zmq/stop-replier replier)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;ZMQ connection terminated.&quot;</span>","value":"\"ZMQ connection terminated.\""}
;; <=

;; **
;;; Print inference results:
;; **

;; @@
(defn print-inference-results [num-particles algorithm-name]
  (let [algorithm (case algorithm-name
                    "is" :importance
                    "smc" :smc
                    "csis" :csis)
        algorithm-options (case algorithm-name
                            "is" []
                            "smc" [:number-of-particles num-particles]
                            "csis" [])
        states (take num-particles (apply infer algorithm hmm [observations init-dist trans-dists obs-dists] algorithm-options))
        inference-result-string (str/join
                                  "\n"
                                  (map (fn [state] (str/join "," (cons (:log-weight state) (:result state)))) states))]
    (spit (str "plots/hmm/" algorithm-name "_1_" num-particles ".csv") inference-result-string)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.hmm/print-inference-results</span>","value":"#'worksheets.hmm/print-inference-results"}
;; <=

;; @@
(def particles-range [10 100 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000])

(mapv #(print-inference-results % "csis") particles-range)
(mapv #(print-inference-results % "is") particles-range)
(mapv #(print-inference-results % "smc") particles-range)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[nil nil nil nil nil nil nil nil nil nil nil nil]"}
;; <=

;; @@

;; @@
