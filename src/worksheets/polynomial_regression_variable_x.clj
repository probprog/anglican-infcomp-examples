;; gorilla-repl.fileformat = 1

;; **
;;; # Polynomial Regression
;; **

;; @@
(ns worksheets.polynomial-regression-variable-x
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
    (let [inputs inputs;(map #(observe (uniform-continuous -10 10) %) inputs) ; this will not work with inference algorithms other than csis (remove this line for it to work with the other inference algorithms)
          w0 (sample (laplace 0 2))
          w1 (sample (laplace 0 2))
          w2 (sample (laplace 0 2))]
      (map #(observe (student-t-loc-scale 4 (poly [w0 w1 w2] %1) 1) %2) inputs outputs)
      [w0 w1 w2])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.polynomial-regression-variable-x/polynomial-regression</span>","value":"#'worksheets.polynomial-regression-variable-x/polynomial-regression"}
;; <=

;; **
;;; ## Compiling the model
;; **

;; @@
(defn combine-observes-fn [observes]
  (mapv :value observes))

(def replier (zmq/start-replier polynomial-regression [(repeat 10 0) (repeat 10 0)] combine-observes-fn))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.polynomial-regression-variable-x/replier</span>","value":"#'worksheets.polynomial-regression-variable-x/replier"}
;; <=

;; **
;;; Stop the replier:
;; **

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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.polynomial-regression-variable-x/getData</span>","value":"#'worksheets.polynomial-regression-variable-x/getData"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.polynomial-regression-variable-x/print-inference-results</span>","value":"#'worksheets.polynomial-regression-variable-x/print-inference-results"}
;; <=

;; @@
(let [inputs (into [] (range 10))
      outputs (into [] (range 10))]
  (first (infer :csis polynomial-regression [inputs outputs] :observe-embedder-input (concat inputs outputs)))
  )
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:anglican.state/mem</span>","value":":anglican.state/mem"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[],"value":"{}"}],"value":"[:anglican.state/mem {}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:predicts</span>","value":":predicts"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[],"value":"[]"}],"value":"[:predicts []]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:anglican.state/catch-stack</span>","value":":anglican.state/catch-stack"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:anglican.state/catch-stack nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:anglican.infcomp.csis/samples</span>","value":":anglican.infcomp.csis/samples"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-address</span>","value":":sample-address"},{"type":"html","content":"<span class='clj-string'>&quot;S19&quot;</span>","value":"\"S19\""}],"value":"[:sample-address \"S19\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-instance</span>","value":":sample-instance"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}],"value":"[:sample-instance 0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-prior-dist</span>","value":":sample-prior-dist"},{"type":"list-like","open":"<span class='clj-record'>#anglican.runtime.laplace-distribution{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:loc</span>","value":":loc"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:loc 0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:scale</span>","value":":scale"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[:scale 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dist23708</span>","value":":dist23708"},{"type":"html","content":"<span class='clj-unkown'>#object[org.apache.commons.math3.distribution.LaplaceDistribution 0x4a6660c4 &quot;org.apache.commons.math3.distribution.LaplaceDistribution@4a6660c4&quot;]</span>","value":"#object[org.apache.commons.math3.distribution.LaplaceDistribution 0x4a6660c4 \"org.apache.commons.math3.distribution.LaplaceDistribution@4a6660c4\"]"}],"value":"[:dist23708 #object[org.apache.commons.math3.distribution.LaplaceDistribution 0x4a6660c4 \"org.apache.commons.math3.distribution.LaplaceDistribution@4a6660c4\"]]"}],"value":"(anglican.runtime/laplace 0 2)"}],"value":"[:sample-prior-dist (anglican.runtime/laplace 0 2)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:value</span>","value":":value"},{"type":"html","content":"<span class='clj-double'>0.08968971263776448</span>","value":"0.08968971263776448"}],"value":"[:value 0.08968971263776448]"}],"value":"{:sample-address \"S19\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value 0.08968971263776448}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-address</span>","value":":sample-address"},{"type":"html","content":"<span class='clj-string'>&quot;S17&quot;</span>","value":"\"S17\""}],"value":"[:sample-address \"S17\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-instance</span>","value":":sample-instance"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}],"value":"[:sample-instance 0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-prior-dist</span>","value":":sample-prior-dist"},{"type":"list-like","open":"<span class='clj-record'>#anglican.runtime.laplace-distribution{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:loc</span>","value":":loc"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:loc 0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:scale</span>","value":":scale"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[:scale 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dist23708</span>","value":":dist23708"},{"type":"html","content":"<span class='clj-unkown'>#object[org.apache.commons.math3.distribution.LaplaceDistribution 0xe415ed2 &quot;org.apache.commons.math3.distribution.LaplaceDistribution@e415ed2&quot;]</span>","value":"#object[org.apache.commons.math3.distribution.LaplaceDistribution 0xe415ed2 \"org.apache.commons.math3.distribution.LaplaceDistribution@e415ed2\"]"}],"value":"[:dist23708 #object[org.apache.commons.math3.distribution.LaplaceDistribution 0xe415ed2 \"org.apache.commons.math3.distribution.LaplaceDistribution@e415ed2\"]]"}],"value":"(anglican.runtime/laplace 0 2)"}],"value":"[:sample-prior-dist (anglican.runtime/laplace 0 2)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:value</span>","value":":value"},{"type":"html","content":"<span class='clj-double'>-0.9596916410492878</span>","value":"-0.9596916410492878"}],"value":"[:value -0.9596916410492878]"}],"value":"{:sample-address \"S17\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value -0.9596916410492878}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-address</span>","value":":sample-address"},{"type":"html","content":"<span class='clj-string'>&quot;S15&quot;</span>","value":"\"S15\""}],"value":"[:sample-address \"S15\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-instance</span>","value":":sample-instance"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}],"value":"[:sample-instance 0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-prior-dist</span>","value":":sample-prior-dist"},{"type":"list-like","open":"<span class='clj-record'>#anglican.runtime.laplace-distribution{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:loc</span>","value":":loc"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:loc 0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:scale</span>","value":":scale"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[:scale 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dist23708</span>","value":":dist23708"},{"type":"html","content":"<span class='clj-unkown'>#object[org.apache.commons.math3.distribution.LaplaceDistribution 0x1b22d04a &quot;org.apache.commons.math3.distribution.LaplaceDistribution@1b22d04a&quot;]</span>","value":"#object[org.apache.commons.math3.distribution.LaplaceDistribution 0x1b22d04a \"org.apache.commons.math3.distribution.LaplaceDistribution@1b22d04a\"]"}],"value":"[:dist23708 #object[org.apache.commons.math3.distribution.LaplaceDistribution 0x1b22d04a \"org.apache.commons.math3.distribution.LaplaceDistribution@1b22d04a\"]]"}],"value":"(anglican.runtime/laplace 0 2)"}],"value":"[:sample-prior-dist (anglican.runtime/laplace 0 2)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:value</span>","value":":value"},{"type":"html","content":"<span class='clj-double'>0.23791406404097593</span>","value":"0.23791406404097593"}],"value":"[:value 0.23791406404097593]"}],"value":"{:sample-address \"S15\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value 0.23791406404097593}"}],"value":"[{:sample-address \"S19\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value 0.08968971263776448} {:sample-address \"S17\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value -0.9596916410492878} {:sample-address \"S15\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value 0.23791406404097593}]"}],"value":"[:anglican.infcomp.csis/samples [{:sample-address \"S19\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value 0.08968971263776448} {:sample-address \"S17\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value -0.9596916410492878} {:sample-address \"S15\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value 0.23791406404097593}]]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:result</span>","value":":result"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.08968971263776448</span>","value":"0.08968971263776448"},{"type":"html","content":"<span class='clj-double'>-0.9596916410492878</span>","value":"-0.9596916410492878"},{"type":"html","content":"<span class='clj-double'>0.23791406404097593</span>","value":"0.23791406404097593"}],"value":"[0.08968971263776448 -0.9596916410492878 0.23791406404097593]"}],"value":"[:result [0.08968971263776448 -0.9596916410492878 0.23791406404097593]]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:log-weight</span>","value":":log-weight"},{"type":"html","content":"<span class='clj-double'>-35.535640650009114</span>","value":"-35.535640650009114"}],"value":"[:log-weight -35.535640650009114]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:anglican.state/store</span>","value":":anglican.state/store"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:anglican.state/store nil]"}],"value":"{:anglican.state/mem {}, :predicts [], :anglican.state/catch-stack nil, :anglican.infcomp.csis/samples [{:sample-address \"S19\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value 0.08968971263776448} {:sample-address \"S17\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value -0.9596916410492878} {:sample-address \"S15\", :sample-instance 0, :sample-prior-dist (anglican.runtime/laplace 0 2), :value 0.23791406404097593}], :result [0.08968971263776448 -0.9596916410492878 0.23791406404097593], :log-weight -35.535640650009114, :anglican.state/store nil}"}
;; <=

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
