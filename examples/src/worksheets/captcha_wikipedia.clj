;; gorilla-repl.fileformat = 1

;; **
;;; # Wikipedia's Captcha
;;; 
;;; In this worksheet, we demonstrate inference compilation on a Captcha rendering generative model and use it to break Wikipedia's Captcha. More in [the paper](https://arxiv.org/abs/1610.09900).
;; **

;; @@
(ns worksheets.captcha-wikipedia
  (:require [anglican.runtime :refer :all]
            [anglican.emit :refer :all]
            [anglican.stat :as stat]
            [anglican.infcomp.zmq :as zmq]
            [anglican.inference :refer [infer]]
            [anglican.infcomp.prior :as prior]
            [gorilla-plot.core :as plt]
            [helpers.captcha :refer [levenshtein-normalized]]
            [helpers.captcha-wikipedia :refer [render render-to-file abc-dist abc-sigma letter-dict oxCaptcha]]
            [helpers.general :refer [empirical-MAP]]
            anglican.rmh
            anglican.infcomp.csis
            anglican.smc
            anglican.infcomp.core)
  (:import [robots.OxCaptcha OxCaptcha]
           [javax.imageio ImageIO]
           [java.io File]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Captcha generative model
;; **

;; @@
;; CAPTCHA Query
(anglican.infcomp.core/reset-infcomp-addressing-scheme!)
(with-primitive-procedures [render abc-dist repeatedly]
  (defquery captcha-wikipedia [baseline-image]
    (let [;; Number of letters in CAPTCHA
          num-letters (sample (uniform-discrete 8 11))
          font-size (sample (uniform-discrete 26 32))
          kerning (sample (uniform-discrete 1 3))
          letter-ids (repeatedly num-letters #(sample "letterid" (uniform-discrete 0 (count letter-dict))))
          letters (apply str (map (partial nth letter-dict) letter-ids))

          ;; Render image using renderer from ...
          rendered-image (render letters font-size kerning)]

      ;; ABC observe
      (observe (abc-dist rendered-image abc-sigma) baseline-image)

      ;; Returns
      {:letters letters
       :font-size font-size
       :kerning kerning})))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.captcha-wikipedia/captcha-wikipedia</span>","value":"#'worksheets.captcha-wikipedia/captcha-wikipedia"}
;; <=

;; **
;;; ## Train a compilation artifact
;; **

;; @@
(defn combine-observes-fn [observes]
  (:value (first observes)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.captcha-wikipedia/combine-observes-fn</span>","value":"#'worksheets.captcha-wikipedia/combine-observes-fn"}
;; <=

;; @@
(def replier (zmq/start-replier captcha-wikipedia [nil] combine-observes-fn))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.captcha-wikipedia/replier</span>","value":"#'worksheets.captcha-wikipedia/replier"}
;; <=

;; @@
(zmq/stop-replier replier)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;ZMQ connection terminated.&quot;</span>","value":"\"ZMQ connection terminated.\""}
;; <=

;; **
;;; ## Inference comparison
;;; ### Load real Wikipedia Captchas
;; **

;; @@
(def directory (clojure.java.io/file "resources/wikipedia-dataset"))
(def files (take 100 (filter #(= ".png" (apply str (take-last 4 (.getPath %))))
                             (rest (file-seq directory)))))
(def num-observes (count files))
(def observes (doall (map vec (map (fn [f] (map vec (.load oxCaptcha (.getPath f)))) files))))
(def ground-truth-letters (map (fn [f] (clojure.string/replace (.getName f) ".png" "")) files))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.captcha-wikipedia/ground-truth-letters</span>","value":"#'worksheets.captcha-wikipedia/ground-truth-letters"}
;; <=

;; **
;;; ### Load synthetic Wikipedia Captchas
;; **

;; @@
(def num-observes 100)
(def samples-from-prior (take num-observes (prior/sample-from-prior captcha-wikipedia nil)))
(def observes (map (comp combine-observes-fn :observes) samples-from-prior))
(def ground-truth-letters (map (fn [smp]
                                 (let [latents (:samples smp)
                                       letter-ids (map :value (filter #(= (:sample-address %) "letterid") latents))
                                       letters (apply str (map (partial nth letter-dict) letter-ids))]
                                   letters))
                               samples-from-prior))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.captcha-wikipedia/ground-truth-letters</span>","value":"#'worksheets.captcha-wikipedia/ground-truth-letters"}
;; <=

;; **
;;; ### Perform inference using SMC, RMH and CSIS
;; **

;; @@
;; SMC
(def num-particles 1)
(def smc-states-list (map (fn [observe]
                            (take num-particles (infer :smc captcha-wikipedia [observe] :number-of-particles num-particles)))
                          observes))
(def smc-MAP-list (map (comp empirical-MAP stat/collect-results) smc-states-list))
(time
  (doall (map (fn [smc-MAP filename] (render-to-file (:letters smc-MAP) (:font-size smc-MAP) (:kerning smc-MAP) filename))
              smc-MAP-list
              (map #(str "plots/captcha-wikipedia/" % "-smc.png") (range 1 (inc (count observes)))))))

;; RMH
(def num-iters 1)
(def rmh-states-list (map (fn [observe]
                            (take num-iters (infer :rmh captcha-wikipedia [observe])))
                          observes))
(def rmh-posterior-list (map (comp first last stat/collect-results) rmh-states-list))
(time
  (doall (map (fn [rmh-posterior filename] (render-to-file (:letters rmh-posterior) (:font-size rmh-posterior) (:kerning rmh-posterior) filename))
              rmh-posterior-list
              (map #(str "plots/captcha-wikipedia/" % "-rmh.png") (range 1 (inc (count observes)))))))

;; CSIS
(def num-particles 1)
(def csis-states-list (map (fn [observe]
                             (take num-particles (infer :csis captcha-wikipedia [observe])))
                           observes))
(def csis-MAP-list (map (comp empirical-MAP stat/collect-results) csis-states-list))
(time
  (doall (map (fn [csis-MAP filename] (render-to-file (:letters csis-MAP) (:font-size csis-MAP) (:kerning csis-MAP) filename))
              csis-MAP-list
              (map #(str "plots/captcha-wikipedia/" % "-csis.png") (range 1 (inc (count observes)))))))
;; @@
;; ->
;;; &quot;Elapsed time: 5865.437 msecs&quot;
;;; &quot;Elapsed time: 7239.303 msecs&quot;
;;; &quot;Elapsed time: 11583.766 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)"}
;; <=

;; **
;;; ### Compare accuracies
;;; #### Letters
;; **

;; @@
(def smc-letters (map :letters smc-MAP-list))
(def rmh-letters (map :letters rmh-posterior-list))
(def csis-letters (map :letters csis-MAP-list))
ground-truth-letters
smc-letters
rmh-letters
csis-letters
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;aataxaxaaa&quot;</span>","value":"\"aataxaxaaa\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttttt&quot;</span>","value":"\"tttttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;aaqzataa&quot;</span>","value":"\"aaqzataa\""},{"type":"html","content":"<span class='clj-string'>&quot;zzzczzzz&quot;</span>","value":"\"zzzczzzz\""},{"type":"html","content":"<span class='clj-string'>&quot;qtaqaapp&quot;</span>","value":"\"qtaqaapp\""},{"type":"html","content":"<span class='clj-string'>&quot;ispssssss&quot;</span>","value":"\"ispssssss\""},{"type":"html","content":"<span class='clj-string'>&quot;zttuttutz&quot;</span>","value":"\"zttuttutz\""},{"type":"html","content":"<span class='clj-string'>&quot;ttttttsjt&quot;</span>","value":"\"ttttttsjt\""},{"type":"html","content":"<span class='clj-string'>&quot;ttttacta&quot;</span>","value":"\"ttttacta\""},{"type":"html","content":"<span class='clj-string'>&quot;tsttxpptt&quot;</span>","value":"\"tsttxpptt\""},{"type":"html","content":"<span class='clj-string'>&quot;ttbptwttau&quot;</span>","value":"\"ttbptwttau\""},{"type":"html","content":"<span class='clj-string'>&quot;aauaaaia&quot;</span>","value":"\"aauaaaia\""},{"type":"html","content":"<span class='clj-string'>&quot;ptpttstttt&quot;</span>","value":"\"ptpttstttt\""},{"type":"html","content":"<span class='clj-string'>&quot;ttttaptt&quot;</span>","value":"\"ttttaptt\""},{"type":"html","content":"<span class='clj-string'>&quot;uuatauuaaa&quot;</span>","value":"\"uuatauuaaa\""},{"type":"html","content":"<span class='clj-string'>&quot;ptptttatpp&quot;</span>","value":"\"ptptttatpp\""},{"type":"html","content":"<span class='clj-string'>&quot;xtaattasa&quot;</span>","value":"\"xtaattasa\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttttt&quot;</span>","value":"\"tttttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;xzzzzzzzzi&quot;</span>","value":"\"xzzzzzzzzi\""},{"type":"html","content":"<span class='clj-string'>&quot;pxxtttvx&quot;</span>","value":"\"pxxtttvx\""},{"type":"html","content":"<span class='clj-string'>&quot;taatawata&quot;</span>","value":"\"taatawata\""},{"type":"html","content":"<span class='clj-string'>&quot;xvttuutzk&quot;</span>","value":"\"xvttuutzk\""},{"type":"html","content":"<span class='clj-string'>&quot;taktgaaat&quot;</span>","value":"\"taktgaaat\""},{"type":"html","content":"<span class='clj-string'>&quot;tistssstx&quot;</span>","value":"\"tistssstx\""},{"type":"html","content":"<span class='clj-string'>&quot;tappatsaa&quot;</span>","value":"\"tappatsaa\""},{"type":"html","content":"<span class='clj-string'>&quot;xtsttttt&quot;</span>","value":"\"xtsttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttttttt&quot;</span>","value":"\"tttttttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;tpttpptptt&quot;</span>","value":"\"tpttpptptt\""},{"type":"html","content":"<span class='clj-string'>&quot;ptlkkktpkp&quot;</span>","value":"\"ptlkkktpkp\""},{"type":"html","content":"<span class='clj-string'>&quot;sssssssss&quot;</span>","value":"\"sssssssss\""},{"type":"html","content":"<span class='clj-string'>&quot;atwakataw&quot;</span>","value":"\"atwakataw\""},{"type":"html","content":"<span class='clj-string'>&quot;xxttttxxx&quot;</span>","value":"\"xxttttxxx\""},{"type":"html","content":"<span class='clj-string'>&quot;qttzpatzz&quot;</span>","value":"\"qttzpatzz\""},{"type":"html","content":"<span class='clj-string'>&quot;stsstztstx&quot;</span>","value":"\"stsstztstx\""},{"type":"html","content":"<span class='clj-string'>&quot;itsjxtaa&quot;</span>","value":"\"itsjxtaa\""},{"type":"html","content":"<span class='clj-string'>&quot;tzftakzta&quot;</span>","value":"\"tzftakzta\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttststt&quot;</span>","value":"\"tttttststt\""},{"type":"html","content":"<span class='clj-string'>&quot;tptaptrtam&quot;</span>","value":"\"tptaptrtam\""},{"type":"html","content":"<span class='clj-string'>&quot;ttttattt&quot;</span>","value":"\"ttttattt\""},{"type":"html","content":"<span class='clj-string'>&quot;ttttwtttt&quot;</span>","value":"\"ttttwtttt\""},{"type":"html","content":"<span class='clj-string'>&quot;tdtttutaas&quot;</span>","value":"\"tdtttutaas\""},{"type":"html","content":"<span class='clj-string'>&quot;tkpkkkakpk&quot;</span>","value":"\"tkpkkkakpk\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttttlt&quot;</span>","value":"\"tttttttlt\""},{"type":"html","content":"<span class='clj-string'>&quot;twktxtkta&quot;</span>","value":"\"twktxtkta\""},{"type":"html","content":"<span class='clj-string'>&quot;utuuutuut&quot;</span>","value":"\"utuuutuut\""},{"type":"html","content":"<span class='clj-string'>&quot;tatzztza&quot;</span>","value":"\"tatzztza\""},{"type":"html","content":"<span class='clj-string'>&quot;pssasppsaa&quot;</span>","value":"\"pssasppsaa\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttatkt&quot;</span>","value":"\"tttttatkt\""},{"type":"html","content":"<span class='clj-string'>&quot;tpjtppttp&quot;</span>","value":"\"tpjtppttp\""},{"type":"html","content":"<span class='clj-string'>&quot;tttaatttt&quot;</span>","value":"\"tttaatttt\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttttxt&quot;</span>","value":"\"tttttttxt\""},{"type":"html","content":"<span class='clj-string'>&quot;atataatta&quot;</span>","value":"\"atataatta\""},{"type":"html","content":"<span class='clj-string'>&quot;ipiittitp&quot;</span>","value":"\"ipiittitp\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttttt&quot;</span>","value":"\"tttttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;atapaatt&quot;</span>","value":"\"atapaatt\""},{"type":"html","content":"<span class='clj-string'>&quot;taaaaaaata&quot;</span>","value":"\"taaaaaaata\""},{"type":"html","content":"<span class='clj-string'>&quot;xtttttatkt&quot;</span>","value":"\"xtttttatkt\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttttt&quot;</span>","value":"\"tttttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;sthtttttt&quot;</span>","value":"\"sthtttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;ttxtttttt&quot;</span>","value":"\"ttxtttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;aaaaaaaati&quot;</span>","value":"\"aaaaaaaati\""},{"type":"html","content":"<span class='clj-string'>&quot;pttittttt&quot;</span>","value":"\"pttittttt\""},{"type":"html","content":"<span class='clj-string'>&quot;xxttpttpt&quot;</span>","value":"\"xxttpttpt\""},{"type":"html","content":"<span class='clj-string'>&quot;ttkkktktk&quot;</span>","value":"\"ttkkktktk\""},{"type":"html","content":"<span class='clj-string'>&quot;tzkwstwkz&quot;</span>","value":"\"tzkwstwkz\""},{"type":"html","content":"<span class='clj-string'>&quot;aaataaaaxa&quot;</span>","value":"\"aaataaaaxa\""},{"type":"html","content":"<span class='clj-string'>&quot;tttxttta&quot;</span>","value":"\"tttxttta\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttttst&quot;</span>","value":"\"tttttttst\""},{"type":"html","content":"<span class='clj-string'>&quot;ttwaatatpt&quot;</span>","value":"\"ttwaatatpt\""},{"type":"html","content":"<span class='clj-string'>&quot;xxizxxxt&quot;</span>","value":"\"xxizxxxt\""},{"type":"html","content":"<span class='clj-string'>&quot;taataaaa&quot;</span>","value":"\"taataaaa\""},{"type":"html","content":"<span class='clj-string'>&quot;wtttmpaa&quot;</span>","value":"\"wtttmpaa\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttttttt&quot;</span>","value":"\"tttttttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;akaaatakaa&quot;</span>","value":"\"akaaatakaa\""},{"type":"html","content":"<span class='clj-string'>&quot;kaatazaxt&quot;</span>","value":"\"kaatazaxt\""},{"type":"html","content":"<span class='clj-string'>&quot;ptaaaaaka&quot;</span>","value":"\"ptaaaaaka\""},{"type":"html","content":"<span class='clj-string'>&quot;aaaataaa&quot;</span>","value":"\"aaaataaa\""},{"type":"html","content":"<span class='clj-string'>&quot;xaapaxxtsw&quot;</span>","value":"\"xaapaxxtsw\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttattsx&quot;</span>","value":"\"tttttattsx\""},{"type":"html","content":"<span class='clj-string'>&quot;taptszptt&quot;</span>","value":"\"taptszptt\""},{"type":"html","content":"<span class='clj-string'>&quot;tttxttatt&quot;</span>","value":"\"tttxttatt\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttttttt&quot;</span>","value":"\"tttttttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;ttztttttzt&quot;</span>","value":"\"ttztttttzt\""},{"type":"html","content":"<span class='clj-string'>&quot;stttsttt&quot;</span>","value":"\"stttsttt\""},{"type":"html","content":"<span class='clj-string'>&quot;xxxxtxxx&quot;</span>","value":"\"xxxxtxxx\""},{"type":"html","content":"<span class='clj-string'>&quot;ttaptatt&quot;</span>","value":"\"ttaptatt\""},{"type":"html","content":"<span class='clj-string'>&quot;skskccktsa&quot;</span>","value":"\"skskccktsa\""},{"type":"html","content":"<span class='clj-string'>&quot;spssspttss&quot;</span>","value":"\"spssspttss\""},{"type":"html","content":"<span class='clj-string'>&quot;plztlsfpp&quot;</span>","value":"\"plztlsfpp\""},{"type":"html","content":"<span class='clj-string'>&quot;atattattt&quot;</span>","value":"\"atattattt\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttttttt&quot;</span>","value":"\"tttttttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;atttataa&quot;</span>","value":"\"atttataa\""},{"type":"html","content":"<span class='clj-string'>&quot;ttptttajaa&quot;</span>","value":"\"ttptttajaa\""},{"type":"html","content":"<span class='clj-string'>&quot;jktssztja&quot;</span>","value":"\"jktssztja\""},{"type":"html","content":"<span class='clj-string'>&quot;ttttttttt&quot;</span>","value":"\"ttttttttt\""},{"type":"html","content":"<span class='clj-string'>&quot;tttttatttt&quot;</span>","value":"\"tttttatttt\""},{"type":"html","content":"<span class='clj-string'>&quot;tpttttpa&quot;</span>","value":"\"tpttttpa\""},{"type":"html","content":"<span class='clj-string'>&quot;ttttsttt&quot;</span>","value":"\"ttttsttt\""},{"type":"html","content":"<span class='clj-string'>&quot;apatsskap&quot;</span>","value":"\"apatsskap\""},{"type":"html","content":"<span class='clj-string'>&quot;ttttttttt&quot;</span>","value":"\"ttttttttt\""}],"value":"(\"aataxaxaaa\" \"tttttttt\" \"aaqzataa\" \"zzzczzzz\" \"qtaqaapp\" \"ispssssss\" \"zttuttutz\" \"ttttttsjt\" \"ttttacta\" \"tsttxpptt\" \"ttbptwttau\" \"aauaaaia\" \"ptpttstttt\" \"ttttaptt\" \"uuatauuaaa\" \"ptptttatpp\" \"xtaattasa\" \"tttttttt\" \"xzzzzzzzzi\" \"pxxtttvx\" \"taatawata\" \"xvttuutzk\" \"taktgaaat\" \"tistssstx\" \"tappatsaa\" \"xtsttttt\" \"tttttttttt\" \"tpttpptptt\" \"ptlkkktpkp\" \"sssssssss\" \"atwakataw\" \"xxttttxxx\" \"qttzpatzz\" \"stsstztstx\" \"itsjxtaa\" \"tzftakzta\" \"tttttststt\" \"tptaptrtam\" \"ttttattt\" \"ttttwtttt\" \"tdtttutaas\" \"tkpkkkakpk\" \"tttttttlt\" \"twktxtkta\" \"utuuutuut\" \"tatzztza\" \"pssasppsaa\" \"tttttatkt\" \"tpjtppttp\" \"tttaatttt\" \"tttttttxt\" \"atataatta\" \"ipiittitp\" \"tttttttt\" \"atapaatt\" \"taaaaaaata\" \"xtttttatkt\" \"tttttttt\" \"sthtttttt\" \"ttxtttttt\" \"aaaaaaaati\" \"pttittttt\" \"xxttpttpt\" \"ttkkktktk\" \"tzkwstwkz\" \"aaataaaaxa\" \"tttxttta\" \"tttttttst\" \"ttwaatatpt\" \"xxizxxxt\" \"taataaaa\" \"wtttmpaa\" \"tttttttttt\" \"akaaatakaa\" \"kaatazaxt\" \"ptaaaaaka\" \"aaaataaa\" \"xaapaxxtsw\" \"tttttattsx\" \"taptszptt\" \"tttxttatt\" \"tttttttttt\" \"ttztttttzt\" \"stttsttt\" \"xxxxtxxx\" \"ttaptatt\" \"skskccktsa\" \"spssspttss\" \"plztlsfpp\" \"atattattt\" \"tttttttttt\" \"atttataa\" \"ttptttajaa\" \"jktssztja\" \"ttttttttt\" \"tttttatttt\" \"tpttttpa\" \"ttttsttt\" \"apatsskap\" \"ttttttttt\")"}
;; <=

;; **
;;; #### Recognition rates
;; **

;; @@
(def smc-rate (* 100 (/ (count (filter identity (map = ground-truth-letters smc-letters))) (count ground-truth-letters))))
(def rmh-rate (* 100 (/ (count (filter identity (map = ground-truth-letters rmh-letters))) (count ground-truth-letters))))
(def csis-rate (* 100 (/ (count (filter identity (map = ground-truth-letters csis-letters))) (count ground-truth-letters))))
(println "SMC : " smc-rate "%\nRMH : " rmh-rate "%\nCSIS: " csis-rate "%")
;; @@
;; ->
;;; SMC :  0 %
;;; RMH :  0 %
;;; CSIS:  0 %
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; #### Levenshtein distances
;; **

;; @@
(def smc-levenshtein (mean (map levenshtein-normalized ground-truth-letters smc-letters)))
(def rmh-levenshtein (mean (map levenshtein-normalized ground-truth-letters rmh-letters)))
(def csis-levenshtein (mean (map levenshtein-normalized ground-truth-letters csis-letters)))
(println "SMC : " smc-levenshtein "\nRMH : " rmh-levenshtein "\nCSIS: " csis-levenshtein)
;; @@
;; ->
;;; SMC :  0.94430555164814 
;;; RMH :  0.9335833299160003 
;;; CSIS:  0.9531944423913956
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
