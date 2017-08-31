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
(with-primitive-procedures [poly pow]
  (defquery polynomial-regression [inputs outputs]
    (let [num_inputs (observe (uniform-discrete 5 11) (count inputs))
          inputs (map #(observe (uniform-continuous -1 1) (nth inputs %)) (range num_inputs))
          
          order (sample (discrete [0.25 0.25 0.2 0.15 0.1 0.05]))
          W (repeatedly (inc order) #(sample (laplace 0 2)))]
      (map #(observe (student-t-loc-scale 4 (poly W %1) 1) %2) inputs outputs)
      W)))

(with-primitive-procedures [poly pow]
  (defquery polynomial-regression-basic [inputs outputs]
    (let [order (sample (discrete [0.25 0.25 0.2 0.15 0.1 0.05]))
          W (repeatedly (inc order) #(sample (laplace 0 2)))]
      (map #(observe (student-t-loc-scale 4 (poly W %1) 1) %2) inputs outputs)
      W)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.polynomial-regression-variable-number/polynomial-regression-basic</span>","value":"#'worksheets.polynomial-regression-variable-number/polynomial-regression-basic"}
;; <=

;; **
;;; ## Compiling the model
;; **

;; @@
(defn combine-observes-fn [observes]
  (let [mess (mapv :value observes)
        count_x (first mess)
        X (take count_x (rest mess))
        Y (take count_x (drop count_x (rest mess)))]
    (mapv #(vector %1 %2) X Y)))

(def replier (zmq/start-replier polynomial-regression [(repeat 10 0) (repeat 10 0)] combine-observes-fn))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.polynomial-regression-variable-number/replier</span>","value":"#'worksheets.polynomial-regression-variable-number/replier"}
;; <=

;; @@
(defn combine-observes-fn [observes]
  (let [mess (mapv :value observes)
        count_x (first mess)
        X (take count_x (rest mess))
        Y (take count_x (drop count_x (rest mess)))]
    (mapv #(vector %1 %2) X Y)))

(combine-observes-fn (prior/sample-observes-from-prior polynomial-regression [(repeat 10 0) (repeat 10 0)] ))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>-3.281977615778633</span>","value":"-3.281977615778633"},{"type":"html","content":"<span class='clj-double'>2.6451539353544424</span>","value":"2.6451539353544424"}],"value":"[-3.281977615778633 2.6451539353544424]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>-8.296309673911285</span>","value":"-8.296309673911285"},{"type":"html","content":"<span class='clj-double'>-0.2617425493956258</span>","value":"-0.2617425493956258"}],"value":"[-8.296309673911285 -0.2617425493956258]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>5.6938086459709645</span>","value":"5.6938086459709645"},{"type":"html","content":"<span class='clj-double'>-1.6612469129682648</span>","value":"-1.6612469129682648"}],"value":"[5.6938086459709645 -1.6612469129682648]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>-9.724681923027951</span>","value":"-9.724681923027951"},{"type":"html","content":"<span class='clj-double'>-1.1565323877858187</span>","value":"-1.1565323877858187"}],"value":"[-9.724681923027951 -1.1565323877858187]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>8.29546780199669</span>","value":"8.29546780199669"},{"type":"html","content":"<span class='clj-double'>-1.934407338906928</span>","value":"-1.934407338906928"}],"value":"[8.29546780199669 -1.934407338906928]"}],"value":"[[-3.281977615778633 2.6451539353544424] [-8.296309673911285 -0.2617425493956258] [5.6938086459709645 -1.6612469129682648] [-9.724681923027951 -1.1565323877858187] [8.29546780199669 -1.934407338906928]]"}
;; <=

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
(let [inputs (into [] (map #(/ % 10) (range 10)))
      weights [1 0 0 -1]
      outputs (map #(poly weights %) inputs)
      other-weights (map :result (take 10 (infer :csis polynomial-regression [inputs outputs] :observe-embedder-input (mapv #(vector %1 %2) inputs outputs))))
      _ (println other-weights)
      prediction-plots (map (fn [W] (list-plot (map (fn [x y] (vector x y)) inputs (map #(poly W %) inputs)) :joined true)) other-weights)]
  (apply compose (cons (list-plot (map #(vector %1 %2) inputs outputs)) prediction-plots))
  )
;; @@
;; ->
;;; ((0.5468282040419185 2.1106181461150486) (-0.19806984760838137 -2.813431091912248 -0.3536975168412857 6.436926997236866 0.9926847075714131) (1.1122766230188754 0.15764528909415992 0.6288875130662707) (-0.35407531556825456 4.887628838016846) (-0.02592337039711262) (-0.2089726521137024 0.2973708065491747) (-0.7311863376565966) (-0.2352193573790582 4.0385071157905355 1.364351539377647) (-0.6630681309769332 3.623523202822924 -3.030257457095615 -4.214263594236501) (1.1019006181491804 1.8208616052761815 -2.6002953223614504))
;;; 
;; <-
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"98d958ff-05f6-4eae-ba0d-4c989a7019b3","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"98d958ff-05f6-4eae-ba0d-4c989a7019b3","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"98d958ff-05f6-4eae-ba0d-4c989a7019b3","values":[{"x":0,"y":1},{"x":0.1,"y":0.999},{"x":0.2,"y":0.992},{"x":0.3,"y":0.973},{"x":0.4,"y":0.936},{"x":0.5,"y":0.875},{"x":0.6,"y":0.784},{"x":0.7,"y":0.657},{"x":0.8,"y":0.488},{"x":0.9,"y":0.271}]},{"name":"b477905b-2196-4f6e-9b03-3ced46ac3eea","values":[{"x":0,"y":0.5468282040419185},{"x":0.1,"y":0.7578900186534233},{"x":0.2,"y":0.9689518332649282},{"x":0.3,"y":1.1800136478764331},{"x":0.4,"y":1.391075462487938},{"x":0.5,"y":1.6021372770994429},{"x":0.6,"y":1.8131990917109477},{"x":0.7,"y":2.0242609063224526},{"x":0.8,"y":2.2353227209339575},{"x":0.9,"y":2.4463845355454623}]},{"name":"5fcc0e77-1cb8-4ac9-8822-4cf2c8a1971c","values":[{"x":0,"y":-0.19806984760838137},{"x":0.1,"y":-0.47641373650002505},{"x":0.2,"y":-0.7218202551544732},{"x":0.3,"y":-0.8920941766410477},{"x":0.4,"y":-0.9426578307308986},{"x":0.5,"y":-0.8265511038970053},{"x":0.6,"y":-0.49443143931417477},{"x":0.7,"y":0.10542616314095604},{"x":0.8,"y":1.027129146889924},{"x":0.9,"y":2.3271673986524335}]},{"name":"51417016-21a7-4b91-8ef3-1fc528d8cab7","values":[{"x":0,"y":1.1122766230188754},{"x":0.1,"y":1.134330027058954},{"x":0.2,"y":1.1689611813603582},{"x":0.3,"y":1.2161700859230877},{"x":0.4,"y":1.2759567407471426},{"x":0.5,"y":1.348321145832523},{"x":0.6,"y":1.4332633011792288},{"x":0.7,"y":1.53078320678726},{"x":0.8,"y":1.6408808626566165},{"x":0.9,"y":1.7635562687872985}]},{"name":"93f320cf-e716-4700-9ef8-864faff85baa","values":[{"x":0,"y":-0.35407531556825456},{"x":0.1,"y":0.13468756823343003},{"x":0.2,"y":0.6234504520351146},{"x":0.3,"y":1.1122133358367992},{"x":0.4,"y":1.6009762196384838},{"x":0.5,"y":2.089739103440168},{"x":0.6,"y":2.578501987241853},{"x":0.7,"y":3.067264871043537},{"x":0.8,"y":3.556027754845222},{"x":0.9,"y":4.044790638646907}]},{"name":"71b08082-c281-48b0-9913-3eda0f2e2c15","values":[{"x":0,"y":-0.02592337039711262},{"x":0.1,"y":-0.02592337039711262},{"x":0.2,"y":-0.02592337039711262},{"x":0.3,"y":-0.02592337039711262},{"x":0.4,"y":-0.02592337039711262},{"x":0.5,"y":-0.02592337039711262},{"x":0.6,"y":-0.02592337039711262},{"x":0.7,"y":-0.02592337039711262},{"x":0.8,"y":-0.02592337039711262},{"x":0.9,"y":-0.02592337039711262}]},{"name":"b8c6d60a-c960-4bf5-aafc-3ddd0b861d5a","values":[{"x":0,"y":-0.2089726521137024},{"x":0.1,"y":-0.1792355714587849},{"x":0.2,"y":-0.14949849080386746},{"x":0.3,"y":-0.11976141014894999},{"x":0.4,"y":-0.09002432949403251},{"x":0.5,"y":-0.06028724883911504},{"x":0.6,"y":-0.03055016818419759},{"x":0.7,"y":-8.130875292801099E-4},{"x":0.8,"y":0.02892399312563737},{"x":0.9,"y":0.05866107378055482}]},{"name":"cc8e9d95-c2e7-4795-bb22-9e0c3f28619b","values":[{"x":0,"y":-0.7311863376565966},{"x":0.1,"y":-0.7311863376565966},{"x":0.2,"y":-0.7311863376565966},{"x":0.3,"y":-0.7311863376565966},{"x":0.4,"y":-0.7311863376565966},{"x":0.5,"y":-0.7311863376565966},{"x":0.6,"y":-0.7311863376565966},{"x":0.7,"y":-0.7311863376565966},{"x":0.8,"y":-0.7311863376565966},{"x":0.9,"y":-0.7311863376565966}]},{"name":"deca513d-01d5-40c9-ae81-a8eb7d144749","values":[{"x":0,"y":-0.2352193573790582},{"x":0.1,"y":0.18227486959377182},{"x":0.2,"y":0.6270561273541548},{"x":0.3,"y":1.0991244159020905},{"x":0.4,"y":1.5984797352375795},{"x":0.5,"y":2.1251220853606214},{"x":0.6,"y":2.6790514662712157},{"x":0.7,"y":3.2602678779693632},{"x":0.8,"y":3.8687713204550644},{"x":0.9,"y":4.5045617937283176}]},{"name":"308d38b0-a07c-4467-934c-0dff5abb4fc8","values":[{"x":0,"y":-0.6630681309769332},{"x":0.1,"y":-0.33523264885983345},{"x":0.2,"y":-0.09328789745006505},{"x":0.3,"y":0.03748054168695303},{"x":0.4,"y":0.031787086985801905},{"x":0.5,"y":-0.13565384311893747},{"x":0.6,"y":-0.49012783019268424},{"x":0.7,"y":-1.0569204558008574},{"x":0.8,"y":-1.8613173015088762},{"x":0.9,"y":-2.9286039488821594}]},{"name":"e97a10ff-c958-4c72-82e6-7280a30debd0","values":[{"x":0,"y":1.1019006181491804},{"x":0.1,"y":1.257983825453184},{"x":0.2,"y":1.3620611263099587},{"x":0.3,"y":1.4141325207195043},{"x":0.4,"y":1.4141980086818209},{"x":0.5,"y":1.3622575901969085},{"x":0.6,"y":1.2583112652647672},{"x":0.7,"y":1.1023590338853968},{"x":0.8,"y":0.8944008960587972},{"x":0.9,"y":0.6344368517849686}]}],"marks":[{"type":"symbol","from":{"data":"98d958ff-05f6-4eae-ba0d-4c989a7019b3"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}},{"type":"line","from":{"data":"b477905b-2196-4f6e-9b03-3ced46ac3eea"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"5fcc0e77-1cb8-4ac9-8822-4cf2c8a1971c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"51417016-21a7-4b91-8ef3-1fc528d8cab7"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"93f320cf-e716-4700-9ef8-864faff85baa"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"71b08082-c281-48b0-9913-3eda0f2e2c15"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"b8c6d60a-c960-4bf5-aafc-3ddd0b861d5a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"cc8e9d95-c2e7-4795-bb22-9e0c3f28619b"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"deca513d-01d5-40c9-ae81-a8eb7d144749"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"308d38b0-a07c-4467-934c-0dff5abb4fc8"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"e97a10ff-c958-4c72-82e6-7280a30debd0"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"98d958ff-05f6-4eae-ba0d-4c989a7019b3\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"98d958ff-05f6-4eae-ba0d-4c989a7019b3\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"98d958ff-05f6-4eae-ba0d-4c989a7019b3\", :values ({:x 0, :y 1} {:x 1/10, :y 999/1000} {:x 1/5, :y 124/125} {:x 3/10, :y 973/1000} {:x 2/5, :y 117/125} {:x 1/2, :y 7/8} {:x 3/5, :y 98/125} {:x 7/10, :y 657/1000} {:x 4/5, :y 61/125} {:x 9/10, :y 271/1000})} {:name \"b477905b-2196-4f6e-9b03-3ced46ac3eea\", :values ({:x 0, :y 0.5468282040419185} {:x 1/10, :y 0.7578900186534233} {:x 1/5, :y 0.9689518332649282} {:x 3/10, :y 1.1800136478764331} {:x 2/5, :y 1.391075462487938} {:x 1/2, :y 1.6021372770994429} {:x 3/5, :y 1.8131990917109477} {:x 7/10, :y 2.0242609063224526} {:x 4/5, :y 2.2353227209339575} {:x 9/10, :y 2.4463845355454623})} {:name \"5fcc0e77-1cb8-4ac9-8822-4cf2c8a1971c\", :values ({:x 0, :y -0.19806984760838137} {:x 1/10, :y -0.47641373650002505} {:x 1/5, :y -0.7218202551544732} {:x 3/10, :y -0.8920941766410477} {:x 2/5, :y -0.9426578307308986} {:x 1/2, :y -0.8265511038970053} {:x 3/5, :y -0.49443143931417477} {:x 7/10, :y 0.10542616314095604} {:x 4/5, :y 1.027129146889924} {:x 9/10, :y 2.3271673986524335})} {:name \"51417016-21a7-4b91-8ef3-1fc528d8cab7\", :values ({:x 0, :y 1.1122766230188754} {:x 1/10, :y 1.134330027058954} {:x 1/5, :y 1.1689611813603582} {:x 3/10, :y 1.2161700859230877} {:x 2/5, :y 1.2759567407471426} {:x 1/2, :y 1.348321145832523} {:x 3/5, :y 1.4332633011792288} {:x 7/10, :y 1.53078320678726} {:x 4/5, :y 1.6408808626566165} {:x 9/10, :y 1.7635562687872985})} {:name \"93f320cf-e716-4700-9ef8-864faff85baa\", :values ({:x 0, :y -0.35407531556825456} {:x 1/10, :y 0.13468756823343003} {:x 1/5, :y 0.6234504520351146} {:x 3/10, :y 1.1122133358367992} {:x 2/5, :y 1.6009762196384838} {:x 1/2, :y 2.089739103440168} {:x 3/5, :y 2.578501987241853} {:x 7/10, :y 3.067264871043537} {:x 4/5, :y 3.556027754845222} {:x 9/10, :y 4.044790638646907})} {:name \"71b08082-c281-48b0-9913-3eda0f2e2c15\", :values ({:x 0, :y -0.02592337039711262} {:x 1/10, :y -0.02592337039711262} {:x 1/5, :y -0.02592337039711262} {:x 3/10, :y -0.02592337039711262} {:x 2/5, :y -0.02592337039711262} {:x 1/2, :y -0.02592337039711262} {:x 3/5, :y -0.02592337039711262} {:x 7/10, :y -0.02592337039711262} {:x 4/5, :y -0.02592337039711262} {:x 9/10, :y -0.02592337039711262})} {:name \"b8c6d60a-c960-4bf5-aafc-3ddd0b861d5a\", :values ({:x 0, :y -0.2089726521137024} {:x 1/10, :y -0.1792355714587849} {:x 1/5, :y -0.14949849080386746} {:x 3/10, :y -0.11976141014894999} {:x 2/5, :y -0.09002432949403251} {:x 1/2, :y -0.06028724883911504} {:x 3/5, :y -0.03055016818419759} {:x 7/10, :y -8.130875292801099E-4} {:x 4/5, :y 0.02892399312563737} {:x 9/10, :y 0.05866107378055482})} {:name \"cc8e9d95-c2e7-4795-bb22-9e0c3f28619b\", :values ({:x 0, :y -0.7311863376565966} {:x 1/10, :y -0.7311863376565966} {:x 1/5, :y -0.7311863376565966} {:x 3/10, :y -0.7311863376565966} {:x 2/5, :y -0.7311863376565966} {:x 1/2, :y -0.7311863376565966} {:x 3/5, :y -0.7311863376565966} {:x 7/10, :y -0.7311863376565966} {:x 4/5, :y -0.7311863376565966} {:x 9/10, :y -0.7311863376565966})} {:name \"deca513d-01d5-40c9-ae81-a8eb7d144749\", :values ({:x 0, :y -0.2352193573790582} {:x 1/10, :y 0.18227486959377182} {:x 1/5, :y 0.6270561273541548} {:x 3/10, :y 1.0991244159020905} {:x 2/5, :y 1.5984797352375795} {:x 1/2, :y 2.1251220853606214} {:x 3/5, :y 2.6790514662712157} {:x 7/10, :y 3.2602678779693632} {:x 4/5, :y 3.8687713204550644} {:x 9/10, :y 4.5045617937283176})} {:name \"308d38b0-a07c-4467-934c-0dff5abb4fc8\", :values ({:x 0, :y -0.6630681309769332} {:x 1/10, :y -0.33523264885983345} {:x 1/5, :y -0.09328789745006505} {:x 3/10, :y 0.03748054168695303} {:x 2/5, :y 0.031787086985801905} {:x 1/2, :y -0.13565384311893747} {:x 3/5, :y -0.49012783019268424} {:x 7/10, :y -1.0569204558008574} {:x 4/5, :y -1.8613173015088762} {:x 9/10, :y -2.9286039488821594})} {:name \"e97a10ff-c958-4c72-82e6-7280a30debd0\", :values ({:x 0, :y 1.1019006181491804} {:x 1/10, :y 1.257983825453184} {:x 1/5, :y 1.3620611263099587} {:x 3/10, :y 1.4141325207195043} {:x 2/5, :y 1.4141980086818209} {:x 1/2, :y 1.3622575901969085} {:x 3/5, :y 1.2583112652647672} {:x 7/10, :y 1.1023590338853968} {:x 4/5, :y 0.8944008960587972} {:x 9/10, :y 0.6344368517849686})}), :marks ({:type \"symbol\", :from {:data \"98d958ff-05f6-4eae-ba0d-4c989a7019b3\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}} {:type \"line\", :from {:data \"b477905b-2196-4f6e-9b03-3ced46ac3eea\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"5fcc0e77-1cb8-4ac9-8822-4cf2c8a1971c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"51417016-21a7-4b91-8ef3-1fc528d8cab7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"93f320cf-e716-4700-9ef8-864faff85baa\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"71b08082-c281-48b0-9913-3eda0f2e2c15\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"b8c6d60a-c960-4bf5-aafc-3ddd0b861d5a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"cc8e9d95-c2e7-4795-bb22-9e0c3f28619b\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"deca513d-01d5-40c9-ae81-a8eb7d144749\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"308d38b0-a07c-4467-934c-0dff5abb4fc8\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"e97a10ff-c958-4c72-82e6-7280a30debd0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@
(let [inputs (into [] (map #(/ % 10) (range 10)))
      weights [0 0 0 1]
      outputs (map #(poly weights %) inputs)
      other-weights (map :result (take 10 (drop 1000 (infer :rmh polynomial-regression-basic [inputs outputs]))))
      prediction-plots (map (fn [W] (list-plot (map #(poly W %) inputs) :joined true)) other-weights)]
  (apply compose (cons (list-plot outputs) prediction-plots))
  )
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"c7e77474-bb5c-450e-bf27-d187e523e8bd","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"c7e77474-bb5c-450e-bf27-d187e523e8bd","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"c7e77474-bb5c-450e-bf27-d187e523e8bd","values":[{"x":0,"y":0},{"x":1,"y":0.001},{"x":2,"y":0.008},{"x":3,"y":0.027},{"x":4,"y":0.064},{"x":5,"y":0.125},{"x":6,"y":0.216},{"x":7,"y":0.343},{"x":8,"y":0.512},{"x":9,"y":0.729}]},{"name":"fcc74be3-135c-405c-b454-52a79b1f7c19","values":[{"x":0,"y":-0.22900242450617345},{"x":1,"y":-0.13949092555804876},{"x":2,"y":-0.04997942660992408},{"x":3,"y":0.03953207233820058},{"x":4,"y":0.1290435712863253},{"x":5,"y":0.21855507023444995},{"x":6,"y":0.3080665691825746},{"x":7,"y":0.3975780681306993},{"x":8,"y":0.487089567078824},{"x":9,"y":0.5766010660269486}]},{"name":"4b937ef2-01af-4efc-a818-6d064ed738a1","values":[{"x":0,"y":-0.22900242450617345},{"x":1,"y":-0.13949092555804876},{"x":2,"y":-0.04997942660992408},{"x":3,"y":0.03953207233820058},{"x":4,"y":0.1290435712863253},{"x":5,"y":0.21855507023444995},{"x":6,"y":0.3080665691825746},{"x":7,"y":0.3975780681306993},{"x":8,"y":0.487089567078824},{"x":9,"y":0.5766010660269486}]},{"name":"11473121-e03d-473f-904d-540eeb8a9e16","values":[{"x":0,"y":-0.22900242450617345},{"x":1,"y":-0.13949092555804876},{"x":2,"y":-0.04997942660992408},{"x":3,"y":0.03953207233820058},{"x":4,"y":0.1290435712863253},{"x":5,"y":0.21855507023444995},{"x":6,"y":0.3080665691825746},{"x":7,"y":0.3975780681306993},{"x":8,"y":0.487089567078824},{"x":9,"y":0.5766010660269486}]},{"name":"39d7c3ff-7264-4f1e-a7af-6b9a9681bebf","values":[{"x":0,"y":-0.22900242450617345},{"x":1,"y":-0.13949092555804876},{"x":2,"y":-0.04997942660992408},{"x":3,"y":0.03953207233820058},{"x":4,"y":0.1290435712863253},{"x":5,"y":0.21855507023444995},{"x":6,"y":0.3080665691825746},{"x":7,"y":0.3975780681306993},{"x":8,"y":0.487089567078824},{"x":9,"y":0.5766010660269486}]},{"name":"15097d73-abf4-4c66-adf5-dd3f818c68f2","values":[{"x":0,"y":-0.22900242450617345},{"x":1,"y":-0.13949092555804876},{"x":2,"y":-0.04997942660992408},{"x":3,"y":0.03953207233820058},{"x":4,"y":0.1290435712863253},{"x":5,"y":0.21855507023444995},{"x":6,"y":0.3080665691825746},{"x":7,"y":0.3975780681306993},{"x":8,"y":0.487089567078824},{"x":9,"y":0.5766010660269486}]},{"name":"2c72166b-2f80-4017-80c8-71122e904033","values":[{"x":0,"y":-0.22900242450617345},{"x":1,"y":-0.13949092555804876},{"x":2,"y":-0.04997942660992408},{"x":3,"y":0.03953207233820058},{"x":4,"y":0.1290435712863253},{"x":5,"y":0.21855507023444995},{"x":6,"y":0.3080665691825746},{"x":7,"y":0.3975780681306993},{"x":8,"y":0.487089567078824},{"x":9,"y":0.5766010660269486}]},{"name":"30896711-ab51-4f7d-97c5-6614a5c17c3c","values":[{"x":0,"y":-0.22900242450617345},{"x":1,"y":-0.13949092555804876},{"x":2,"y":-0.04997942660992408},{"x":3,"y":0.03953207233820058},{"x":4,"y":0.1290435712863253},{"x":5,"y":0.21855507023444995},{"x":6,"y":0.3080665691825746},{"x":7,"y":0.3975780681306993},{"x":8,"y":0.487089567078824},{"x":9,"y":0.5766010660269486}]},{"name":"41b3a28f-8c3c-44b1-9415-bd2a5288eda0","values":[{"x":0,"y":-0.22900242450617345},{"x":1,"y":-0.13949092555804876},{"x":2,"y":-0.04997942660992408},{"x":3,"y":0.03953207233820058},{"x":4,"y":0.1290435712863253},{"x":5,"y":0.21855507023444995},{"x":6,"y":0.3080665691825746},{"x":7,"y":0.3975780681306993},{"x":8,"y":0.487089567078824},{"x":9,"y":0.5766010660269486}]},{"name":"99771fcb-5fde-4e2f-b5fc-e25ab047e67a","values":[{"x":0,"y":-0.22900242450617345},{"x":1,"y":-0.13949092555804876},{"x":2,"y":-0.04997942660992408},{"x":3,"y":0.03953207233820058},{"x":4,"y":0.1290435712863253},{"x":5,"y":0.21855507023444995},{"x":6,"y":0.3080665691825746},{"x":7,"y":0.3975780681306993},{"x":8,"y":0.487089567078824},{"x":9,"y":0.5766010660269486}]},{"name":"e7bdcdb0-70d6-431c-919f-5b27bc081524","values":[{"x":0,"y":-0.22900242450617345},{"x":1,"y":-0.13949092555804876},{"x":2,"y":-0.04997942660992408},{"x":3,"y":0.03953207233820058},{"x":4,"y":0.1290435712863253},{"x":5,"y":0.21855507023444995},{"x":6,"y":0.3080665691825746},{"x":7,"y":0.3975780681306993},{"x":8,"y":0.487089567078824},{"x":9,"y":0.5766010660269486}]}],"marks":[{"type":"symbol","from":{"data":"c7e77474-bb5c-450e-bf27-d187e523e8bd"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}},{"type":"line","from":{"data":"fcc74be3-135c-405c-b454-52a79b1f7c19"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"4b937ef2-01af-4efc-a818-6d064ed738a1"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"11473121-e03d-473f-904d-540eeb8a9e16"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"39d7c3ff-7264-4f1e-a7af-6b9a9681bebf"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"15097d73-abf4-4c66-adf5-dd3f818c68f2"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"2c72166b-2f80-4017-80c8-71122e904033"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"30896711-ab51-4f7d-97c5-6614a5c17c3c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"41b3a28f-8c3c-44b1-9415-bd2a5288eda0"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"99771fcb-5fde-4e2f-b5fc-e25ab047e67a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"e7bdcdb0-70d6-431c-919f-5b27bc081524"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"c7e77474-bb5c-450e-bf27-d187e523e8bd\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"c7e77474-bb5c-450e-bf27-d187e523e8bd\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"c7e77474-bb5c-450e-bf27-d187e523e8bd\", :values ({:x 0, :y 0} {:x 1, :y 1/1000} {:x 2, :y 1/125} {:x 3, :y 27/1000} {:x 4, :y 8/125} {:x 5, :y 1/8} {:x 6, :y 27/125} {:x 7, :y 343/1000} {:x 8, :y 64/125} {:x 9, :y 729/1000})} {:name \"fcc74be3-135c-405c-b454-52a79b1f7c19\", :values ({:x 0, :y -0.22900242450617345} {:x 1, :y -0.13949092555804876} {:x 2, :y -0.04997942660992408} {:x 3, :y 0.03953207233820058} {:x 4, :y 0.1290435712863253} {:x 5, :y 0.21855507023444995} {:x 6, :y 0.3080665691825746} {:x 7, :y 0.3975780681306993} {:x 8, :y 0.487089567078824} {:x 9, :y 0.5766010660269486})} {:name \"4b937ef2-01af-4efc-a818-6d064ed738a1\", :values ({:x 0, :y -0.22900242450617345} {:x 1, :y -0.13949092555804876} {:x 2, :y -0.04997942660992408} {:x 3, :y 0.03953207233820058} {:x 4, :y 0.1290435712863253} {:x 5, :y 0.21855507023444995} {:x 6, :y 0.3080665691825746} {:x 7, :y 0.3975780681306993} {:x 8, :y 0.487089567078824} {:x 9, :y 0.5766010660269486})} {:name \"11473121-e03d-473f-904d-540eeb8a9e16\", :values ({:x 0, :y -0.22900242450617345} {:x 1, :y -0.13949092555804876} {:x 2, :y -0.04997942660992408} {:x 3, :y 0.03953207233820058} {:x 4, :y 0.1290435712863253} {:x 5, :y 0.21855507023444995} {:x 6, :y 0.3080665691825746} {:x 7, :y 0.3975780681306993} {:x 8, :y 0.487089567078824} {:x 9, :y 0.5766010660269486})} {:name \"39d7c3ff-7264-4f1e-a7af-6b9a9681bebf\", :values ({:x 0, :y -0.22900242450617345} {:x 1, :y -0.13949092555804876} {:x 2, :y -0.04997942660992408} {:x 3, :y 0.03953207233820058} {:x 4, :y 0.1290435712863253} {:x 5, :y 0.21855507023444995} {:x 6, :y 0.3080665691825746} {:x 7, :y 0.3975780681306993} {:x 8, :y 0.487089567078824} {:x 9, :y 0.5766010660269486})} {:name \"15097d73-abf4-4c66-adf5-dd3f818c68f2\", :values ({:x 0, :y -0.22900242450617345} {:x 1, :y -0.13949092555804876} {:x 2, :y -0.04997942660992408} {:x 3, :y 0.03953207233820058} {:x 4, :y 0.1290435712863253} {:x 5, :y 0.21855507023444995} {:x 6, :y 0.3080665691825746} {:x 7, :y 0.3975780681306993} {:x 8, :y 0.487089567078824} {:x 9, :y 0.5766010660269486})} {:name \"2c72166b-2f80-4017-80c8-71122e904033\", :values ({:x 0, :y -0.22900242450617345} {:x 1, :y -0.13949092555804876} {:x 2, :y -0.04997942660992408} {:x 3, :y 0.03953207233820058} {:x 4, :y 0.1290435712863253} {:x 5, :y 0.21855507023444995} {:x 6, :y 0.3080665691825746} {:x 7, :y 0.3975780681306993} {:x 8, :y 0.487089567078824} {:x 9, :y 0.5766010660269486})} {:name \"30896711-ab51-4f7d-97c5-6614a5c17c3c\", :values ({:x 0, :y -0.22900242450617345} {:x 1, :y -0.13949092555804876} {:x 2, :y -0.04997942660992408} {:x 3, :y 0.03953207233820058} {:x 4, :y 0.1290435712863253} {:x 5, :y 0.21855507023444995} {:x 6, :y 0.3080665691825746} {:x 7, :y 0.3975780681306993} {:x 8, :y 0.487089567078824} {:x 9, :y 0.5766010660269486})} {:name \"41b3a28f-8c3c-44b1-9415-bd2a5288eda0\", :values ({:x 0, :y -0.22900242450617345} {:x 1, :y -0.13949092555804876} {:x 2, :y -0.04997942660992408} {:x 3, :y 0.03953207233820058} {:x 4, :y 0.1290435712863253} {:x 5, :y 0.21855507023444995} {:x 6, :y 0.3080665691825746} {:x 7, :y 0.3975780681306993} {:x 8, :y 0.487089567078824} {:x 9, :y 0.5766010660269486})} {:name \"99771fcb-5fde-4e2f-b5fc-e25ab047e67a\", :values ({:x 0, :y -0.22900242450617345} {:x 1, :y -0.13949092555804876} {:x 2, :y -0.04997942660992408} {:x 3, :y 0.03953207233820058} {:x 4, :y 0.1290435712863253} {:x 5, :y 0.21855507023444995} {:x 6, :y 0.3080665691825746} {:x 7, :y 0.3975780681306993} {:x 8, :y 0.487089567078824} {:x 9, :y 0.5766010660269486})} {:name \"e7bdcdb0-70d6-431c-919f-5b27bc081524\", :values ({:x 0, :y -0.22900242450617345} {:x 1, :y -0.13949092555804876} {:x 2, :y -0.04997942660992408} {:x 3, :y 0.03953207233820058} {:x 4, :y 0.1290435712863253} {:x 5, :y 0.21855507023444995} {:x 6, :y 0.3080665691825746} {:x 7, :y 0.3975780681306993} {:x 8, :y 0.487089567078824} {:x 9, :y 0.5766010660269486})}), :marks ({:type \"symbol\", :from {:data \"c7e77474-bb5c-450e-bf27-d187e523e8bd\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}} {:type \"line\", :from {:data \"fcc74be3-135c-405c-b454-52a79b1f7c19\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"4b937ef2-01af-4efc-a818-6d064ed738a1\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"11473121-e03d-473f-904d-540eeb8a9e16\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"39d7c3ff-7264-4f1e-a7af-6b9a9681bebf\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"15097d73-abf4-4c66-adf5-dd3f818c68f2\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"2c72166b-2f80-4017-80c8-71122e904033\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"30896711-ab51-4f7d-97c5-6614a5c17c3c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"41b3a28f-8c3c-44b1-9415-bd2a5288eda0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"99771fcb-5fde-4e2f-b5fc-e25ab047e67a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"e7bdcdb0-70d6-431c-919f-5b27bc081524\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@
(let [dataset-no 3
      particle-range [10 20 40 80 160 320 640 1280 2560]]
  (map #(print-inference-results % "csis" dataset-no) particle-range))
;; @@

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

;; @@
(let [ weights [1 -1]
       inputs (map #(/ % 10) (range -9 10 3))
       observations (map #(sample* (student-t-loc-scale 4 (poly weights %) 1)) inputs)]
  observations)

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>1.403476847896828</span>","value":"1.403476847896828"},{"type":"html","content":"<span class='clj-double'>3.2815420095077217</span>","value":"3.2815420095077217"},{"type":"html","content":"<span class='clj-double'>1.4216348887950712</span>","value":"1.4216348887950712"},{"type":"html","content":"<span class='clj-double'>2.4167549076166788</span>","value":"2.4167549076166788"},{"type":"html","content":"<span class='clj-double'>-0.19229845296558923</span>","value":"-0.19229845296558923"},{"type":"html","content":"<span class='clj-double'>1.4619539158008474</span>","value":"1.4619539158008474"},{"type":"html","content":"<span class='clj-double'>-0.6085398066178657</span>","value":"-0.6085398066178657"}],"value":"(1.403476847896828 3.2815420095077217 1.4216348887950712 2.4167549076166788 -0.19229845296558923 1.4619539158008474 -0.6085398066178657)"}
;; <=

;; @@

;; @@
