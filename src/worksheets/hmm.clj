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
(let [spec (slurp "plots/hmm/model.csv")
      spec (str/split spec #"\n")
      spec (into [] (map #(str/split % #",") spec))

      init-dist (categorical (map #(vector %1 %2) (range) (map read-string (get spec 0))))
      trans-probs [(into [] (map read-string (get spec 1))) (into [] (map read-string (get spec 2))) (into [] (map read-string (get spec 3)))]
      trans-dists (into [] (map (fn [probs] (categorical (map #(vector %1 %2) (range) probs))) trans-probs))
      obs-dists (into [] (map #(normal %1 %2) (map read-string (get spec 4)) (map read-string (get spec 5))))
      observations (map read-string (str/split (slurp "plots/hmm/data_1.csv") #","))]
  (def observations observations)
  (def init-dist init-dist)
  (def trans-dists trans-dists)
  (def obs-dists obs-dists)
  (def particles-range [1 2 3 4 5]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.hmm/particles-range</span>","value":"#'worksheets.hmm/particles-range"}
;; <=

;; @@
observations
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.9</span>","value":"0.9"},{"type":"html","content":"<span class='clj-double'>0.8</span>","value":"0.8"},{"type":"html","content":"<span class='clj-double'>0.7</span>","value":"0.7"},{"type":"html","content":"<span class='clj-double'>0.0</span>","value":"0.0"},{"type":"html","content":"<span class='clj-double'>-0.025</span>","value":"-0.025"},{"type":"html","content":"<span class='clj-double'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-double'>2.0</span>","value":"2.0"},{"type":"html","content":"<span class='clj-double'>0.1</span>","value":"0.1"},{"type":"html","content":"<span class='clj-double'>0.0</span>","value":"0.0"},{"type":"html","content":"<span class='clj-double'>0.13</span>","value":"0.13"},{"type":"html","content":"<span class='clj-double'>0.45</span>","value":"0.45"},{"type":"html","content":"<span class='clj-double'>6.0</span>","value":"6.0"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"},{"type":"html","content":"<span class='clj-double'>0.3</span>","value":"0.3"},{"type":"html","content":"<span class='clj-double'>-1.0</span>","value":"-1.0"},{"type":"html","content":"<span class='clj-double'>-1.0</span>","value":"-1.0"}],"value":"(0.9 0.8 0.7 0.0 -0.025 5.0 2.0 0.1 0.0 0.13 0.45 6.0 0.2 0.3 -1.0 -1.0)"}
;; <=

;; **
;;; Do SMC and importance sampling:
;; **

;; @@
(defn getImportanceResults [num-particles]
  (let [raw-results (take num-particles (infer :importance hmm [observations init-dist trans-dists obs-dists]))
        formatted-lines (map (fn [particle] (str/join "," (cons (:log-weight particle) (:result particle)))) raw-results)
        nicely-formatted-results (str/join "\n" formatted-lines)]
    (spit (str/join ["plots/hmm/is_1_" (str num-particles) ".csv"]) nicely-formatted-results)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.hmm/getImportanceResults</span>","value":"#'worksheets.hmm/getImportanceResults"}
;; <=

;; @@
(map getImportanceResults particles-range)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"(nil nil nil nil nil)"}
;; <=

;; @@
(defn getSMCResults [ num-particles ]
  (let [raw-results (take num-particles (infer :smc hmm [observations init-dist trans-dists obs-dists] :number-of-particles num-particles))
        formatted-lines (map (fn [particle] (str/join "," (cons (:log-weight particle) (:result particle))) ) raw-results)
        nicely-formatted-results (str/join "\n" formatted-lines)]
    (spit (str/join ["plots/hmm/smc_1_" (str num-particles) ".csv"]) nicely-formatted-results)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.hmm/getSMCResults</span>","value":"#'worksheets.hmm/getSMCResults"}
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
(defn combine-observes-fn [observes] (map :value observes))

(def replier (zmq/start-replier hmm [observations init-dist trans-dists obs-dists] combine-observes-fn))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.hmm/replier</span>","value":"#'worksheets.hmm/replier"}
;; <=

;; @@
(combine-observes-fn (prior/sample-observes-from-prior hmm [observations init-dist trans-dists obs-dists]))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.5544653644519333</span>","value":"0.5544653644519333"},{"type":"html","content":"<span class='clj-double'>0.9241250617946138</span>","value":"0.9241250617946138"},{"type":"html","content":"<span class='clj-double'>-0.018043199986925603</span>","value":"-0.018043199986925603"},{"type":"html","content":"<span class='clj-double'>-1.2746267085935241</span>","value":"-1.2746267085935241"},{"type":"html","content":"<span class='clj-double'>1.171554636770816</span>","value":"1.171554636770816"},{"type":"html","content":"<span class='clj-double'>1.0026099248120266</span>","value":"1.0026099248120266"},{"type":"html","content":"<span class='clj-double'>-0.5314431673708702</span>","value":"-0.5314431673708702"},{"type":"html","content":"<span class='clj-double'>-2.3164966314889757</span>","value":"-2.3164966314889757"},{"type":"html","content":"<span class='clj-double'>0.4356770569331204</span>","value":"0.4356770569331204"},{"type":"html","content":"<span class='clj-double'>1.0217609984157543</span>","value":"1.0217609984157543"},{"type":"html","content":"<span class='clj-double'>0.322195118491742</span>","value":"0.322195118491742"},{"type":"html","content":"<span class='clj-double'>-2.244892045640148</span>","value":"-2.244892045640148"},{"type":"html","content":"<span class='clj-double'>-0.28794695339796383</span>","value":"-0.28794695339796383"},{"type":"html","content":"<span class='clj-double'>0.2441162123131777</span>","value":"0.2441162123131777"},{"type":"html","content":"<span class='clj-double'>1.0673125398445473</span>","value":"1.0673125398445473"},{"type":"html","content":"<span class='clj-double'>0.567841093958255</span>","value":"0.567841093958255"}],"value":"(0.5544653644519333 0.9241250617946138 -0.018043199986925603 -1.2746267085935241 1.171554636770816 1.0026099248120266 -0.5314431673708702 -2.3164966314889757 0.4356770569331204 1.0217609984157543 0.322195118491742 -2.244892045640148 -0.28794695339796383 0.2441162123131777 1.0673125398445473 0.567841093958255)"}
;; <=

;; @@
(zmq/stop-replier replier)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;ZMQ connection terminated.&quot;</span>","value":"\"ZMQ connection terminated.\""}
;; <=

;; @@
(infer :csis hmm [observations init-dist trans-dists obs-dists])
;; @@

;; @@
(defn getCSISResults [num-particles]
  (let [raw-results (take num-particles (infer :csis hmm [observations init-dist trans-dists obs-dists] ))
        formatted-lines (map (fn [particle] (str/join "," (cons (:log-weight particle) (:result particle))) ) raw-results)
        nicely-formatted-results (str/join "\n" formatted-lines)]
    (spit (str/join ["plots/hmm/csis_1_" (str num-particles) ".csv"]) nicely-formatted-results)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.hmm/getCSISResults</span>","value":"#'worksheets.hmm/getCSISResults"}
;; <=

;; @@
(map getCSISResults particles-range)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"(nil nil nil nil nil)"}
;; <=

;; @@

;; @@
