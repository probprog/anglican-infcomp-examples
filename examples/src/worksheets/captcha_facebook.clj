;; gorilla-repl.fileformat = 1

;; **
;;; # Facebook's Captcha
;;; 
;;; In this worksheet, we demonstrate inference compilation on a Captcha rendering generative model and use it to break Facebook's Captcha. More in [the paper](https://arxiv.org/abs/1610.09900).
;; **

;; @@
(ns worksheets.captcha-facebook
  (:require [anglican.runtime :refer :all]
            [anglican.emit :refer :all]
            [anglican.stat :as stat]
            [anglican.infcomp.zmq :as zmq]
            [anglican.inference :refer [infer]]
            [anglican.infcomp.prior :as prior]
            [gorilla-plot.core :as plt]
            [helpers.captcha :refer [levenshtein-normalized]]
            [helpers.captcha-facebook :refer [render render-to-file abc-dist abc-sigma letter-dict oxCaptcha]]
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
  (defquery captcha-facebook [baseline-image]
    (let [;; Number of letters in CAPTCHA
          num-letters (sample (uniform-discrete 6 8))
          font-size (sample (uniform-discrete 38 44))
          kerning (sample (uniform-discrete -2 2))
          letter-ids (repeatedly num-letters #(sample "letterid" (uniform-discrete 0 (count letter-dict))))
          letters (apply str (map (partial nth letter-dict) letter-ids))

          ;; Render image using renderer from ...
          rendered-image (render letters font-size kerning)]

      ;; ABC-style observe
      (observe (abc-dist rendered-image abc-sigma) baseline-image)

      ;; Returns
      {:letters letters
       :font-size font-size
       :kerning kerning})))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.captcha-facebook/captcha-facebook</span>","value":"#'worksheets.captcha-facebook/captcha-facebook"}
;; <=

;; **
;;; ## Train a compilation artifact
;; **

;; @@
(defn combine-observes-fn [observes]
  (:value (first observes)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.captcha-facebook/combine-observes-fn</span>","value":"#'worksheets.captcha-facebook/combine-observes-fn"}
;; <=

;; @@
(def replier (zmq/start-replier captcha-facebook [nil] combine-observes-fn))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.captcha-facebook/replier</span>","value":"#'worksheets.captcha-facebook/replier"}
;; <=

;; @@
(zmq/stop-replier replier)
;; @@

;; **
;;; ## Inference comparison
;;; ### Load real Facebook Captchas
;; **

;; @@
(def directory (clojure.java.io/file "resources/facebook-dataset"))
(def files (take 100 (filter #(= ".png" (apply str (take-last 4 (.getPath %))))
                             (rest (file-seq directory)))))
(def num-observes (count files))
(def observes (doall (map vec (map (fn [f] (map vec (.load oxCaptcha (.getPath f)))) files))))
(def ground-truth-letters (map (fn [f] (clojure.string/replace (.getName f) ".png" "")) files))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.captcha-facebook/ground-truth-letters</span>","value":"#'worksheets.captcha-facebook/ground-truth-letters"}
;; <=

;; **
;;; ### Load synthetic Facebook Captchas
;; **

;; @@
(def num-observes 100)
(def samples-from-prior (take num-observes (prior/sample-from-prior captcha-facebook nil)))
(def observes (map (comp combine-observes-fn :observes) samples-from-prior))
(def ground-truth-letters (map (fn [smp]
                                 (let [latents (:samples smp)
                                       letter-ids (map :value (filter #(= (:sample-address %) "letterid") latents))
                                       letters (apply str (map (partial nth letter-dict) letter-ids))]
                                   letters))
                               samples-from-prior))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.captcha-facebook/ground-truth-letters</span>","value":"#'worksheets.captcha-facebook/ground-truth-letters"}
;; <=

;; **
;;; ### Perform inference using SMC, RMH and CSIS
;; **

;; @@
;; SMC
(def num-particles 1)
(def smc-states-list (map (fn [observe]
                            (take num-particles (infer :smc captcha-facebook [observe] :number-of-particles num-particles)))
                          observes))
(def smc-MAP-list (map (comp empirical-MAP stat/collect-results) smc-states-list))
(time
  (doall (map (fn [smc-MAP filename] (render-to-file (:letters smc-MAP) (:font-size smc-MAP) (:kerning smc-MAP) filename))
              smc-MAP-list
              (map #(str "plots/captcha-facebook/" % "-smc.png") (range 1 (inc (count observes)))))))

;; RMH
(def num-iters 1)
(def rmh-states-list (map (fn [observe]
                            (take num-iters (infer :rmh captcha-facebook [observe])))
                          observes))
(def rmh-posterior-list (map (comp first last stat/collect-results) rmh-states-list))
(time
  (doall (map (fn [rmh-posterior filename] (render-to-file (:letters rmh-posterior) (:font-size rmh-posterior) (:kerning rmh-posterior) filename))
              rmh-posterior-list
              (map #(str "plots/captcha-facebook/" % "-rmh.png") (range 1 (inc (count observes)))))))

;; CSIS
(def num-particles 1)
(def csis-states-list (map (fn [observe]
                             (take num-particles (infer :csis captcha-facebook [observe])))
                           observes))
(def csis-MAP-list (map (comp empirical-MAP stat/collect-results) csis-states-list))
(time
  (doall (map (fn [csis-MAP filename] (render-to-file (:letters csis-MAP) (:font-size csis-MAP) (:kerning csis-MAP) filename))
              csis-MAP-list
              (map #(str "plots/captcha-facebook/" % "-csis.png") (range 1 (inc (count observes)))))))
;; @@
;; ->
;;; &quot;Elapsed time: 5978.246 msecs&quot;
;;; &quot;Elapsed time: 7475.439 msecs&quot;
;;; &quot;Elapsed time: 10553.806 msecs&quot;
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
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;eGJJLJJ&quot;</span>","value":"\"eGJJLJJ\""},{"type":"html","content":"<span class='clj-string'>&quot;B1ZBJBe&quot;</span>","value":"\"B1ZBJBe\""},{"type":"html","content":"<span class='clj-string'>&quot;obVetCC&quot;</span>","value":"\"obVetCC\""},{"type":"html","content":"<span class='clj-string'>&quot;6bVmRod&quot;</span>","value":"\"6bVmRod\""},{"type":"html","content":"<span class='clj-string'>&quot;PoBseeG&quot;</span>","value":"\"PoBseeG\""},{"type":"html","content":"<span class='clj-string'>&quot;X7JbOJJ&quot;</span>","value":"\"X7JbOJJ\""},{"type":"html","content":"<span class='clj-string'>&quot;XRRRHHJ&quot;</span>","value":"\"XRRRHHJ\""},{"type":"html","content":"<span class='clj-string'>&quot;oAV55os&quot;</span>","value":"\"oAV55os\""},{"type":"html","content":"<span class='clj-string'>&quot;DxeeVof&quot;</span>","value":"\"DxeeVof\""},{"type":"html","content":"<span class='clj-string'>&quot;JeJJeJ&quot;</span>","value":"\"JeJJeJ\""},{"type":"html","content":"<span class='clj-string'>&quot;O5i5zz5&quot;</span>","value":"\"O5i5zz5\""},{"type":"html","content":"<span class='clj-string'>&quot;BBBddd&quot;</span>","value":"\"BBBddd\""},{"type":"html","content":"<span class='clj-string'>&quot;sssNbs&quot;</span>","value":"\"sssNbs\""},{"type":"html","content":"<span class='clj-string'>&quot;zttaata&quot;</span>","value":"\"zttaata\""},{"type":"html","content":"<span class='clj-string'>&quot;5IcAD5&quot;</span>","value":"\"5IcAD5\""},{"type":"html","content":"<span class='clj-string'>&quot;xxos4PP&quot;</span>","value":"\"xxos4PP\""},{"type":"html","content":"<span class='clj-string'>&quot;e7oesss&quot;</span>","value":"\"e7oesss\""},{"type":"html","content":"<span class='clj-string'>&quot;44ojVmo&quot;</span>","value":"\"44ojVmo\""},{"type":"html","content":"<span class='clj-string'>&quot;oobHobe&quot;</span>","value":"\"oobHobe\""},{"type":"html","content":"<span class='clj-string'>&quot;RRoIRz&quot;</span>","value":"\"RRoIRz\""},{"type":"html","content":"<span class='clj-string'>&quot;Pmd4m5&quot;</span>","value":"\"Pmd4m5\""},{"type":"html","content":"<span class='clj-string'>&quot;tJJJJd5&quot;</span>","value":"\"tJJJJd5\""},{"type":"html","content":"<span class='clj-string'>&quot;oooooz&quot;</span>","value":"\"oooooz\""},{"type":"html","content":"<span class='clj-string'>&quot;DXoBDBB&quot;</span>","value":"\"DXoBDBB\""},{"type":"html","content":"<span class='clj-string'>&quot;RBeBoo&quot;</span>","value":"\"RBeBoo\""},{"type":"html","content":"<span class='clj-string'>&quot;GkmmiiB&quot;</span>","value":"\"GkmmiiB\""},{"type":"html","content":"<span class='clj-string'>&quot;DJooDA&quot;</span>","value":"\"DJooDA\""},{"type":"html","content":"<span class='clj-string'>&quot;Uo44e4&quot;</span>","value":"\"Uo44e4\""},{"type":"html","content":"<span class='clj-string'>&quot;bIRnPeI&quot;</span>","value":"\"bIRnPeI\""},{"type":"html","content":"<span class='clj-string'>&quot;osIQbpj&quot;</span>","value":"\"osIQbpj\""},{"type":"html","content":"<span class='clj-string'>&quot;zzdoRoe&quot;</span>","value":"\"zzdoRoe\""},{"type":"html","content":"<span class='clj-string'>&quot;GVrrJxH&quot;</span>","value":"\"GVrrJxH\""},{"type":"html","content":"<span class='clj-string'>&quot;mmmso7&quot;</span>","value":"\"mmmso7\""},{"type":"html","content":"<span class='clj-string'>&quot;bGsbbG&quot;</span>","value":"\"bGsbbG\""},{"type":"html","content":"<span class='clj-string'>&quot;sPmPsP&quot;</span>","value":"\"sPmPsP\""},{"type":"html","content":"<span class='clj-string'>&quot;kJsJsox&quot;</span>","value":"\"kJsJsox\""},{"type":"html","content":"<span class='clj-string'>&quot;jJmJIk&quot;</span>","value":"\"jJmJIk\""},{"type":"html","content":"<span class='clj-string'>&quot;JrrsrJs&quot;</span>","value":"\"JrrsrJs\""},{"type":"html","content":"<span class='clj-string'>&quot;sbVasJJ&quot;</span>","value":"\"sbVasJJ\""},{"type":"html","content":"<span class='clj-string'>&quot;dJdrrer&quot;</span>","value":"\"dJdrrer\""},{"type":"html","content":"<span class='clj-string'>&quot;ssH1RH&quot;</span>","value":"\"ssH1RH\""},{"type":"html","content":"<span class='clj-string'>&quot;RdodGeR&quot;</span>","value":"\"RdodGeR\""},{"type":"html","content":"<span class='clj-string'>&quot;Boseos&quot;</span>","value":"\"Boseos\""},{"type":"html","content":"<span class='clj-string'>&quot;JJJJOJJ&quot;</span>","value":"\"JJJJOJJ\""},{"type":"html","content":"<span class='clj-string'>&quot;XXJtGJJ&quot;</span>","value":"\"XXJtGJJ\""},{"type":"html","content":"<span class='clj-string'>&quot;kgPXBsP&quot;</span>","value":"\"kgPXBsP\""},{"type":"html","content":"<span class='clj-string'>&quot;BBssz7s&quot;</span>","value":"\"BBssz7s\""},{"type":"html","content":"<span class='clj-string'>&quot;sssjBds&quot;</span>","value":"\"sssjBds\""},{"type":"html","content":"<span class='clj-string'>&quot;xoVdVsd&quot;</span>","value":"\"xoVdVsd\""},{"type":"html","content":"<span class='clj-string'>&quot;jAbOJo5&quot;</span>","value":"\"jAbOJo5\""},{"type":"html","content":"<span class='clj-string'>&quot;fddDxF&quot;</span>","value":"\"fddDxF\""},{"type":"html","content":"<span class='clj-string'>&quot;O7Uooo&quot;</span>","value":"\"O7Uooo\""},{"type":"html","content":"<span class='clj-string'>&quot;iboeJie&quot;</span>","value":"\"iboeJie\""},{"type":"html","content":"<span class='clj-string'>&quot;xssoDs4&quot;</span>","value":"\"xssoDs4\""},{"type":"html","content":"<span class='clj-string'>&quot;bxmbbrV&quot;</span>","value":"\"bxmbbrV\""},{"type":"html","content":"<span class='clj-string'>&quot;dIBXsB&quot;</span>","value":"\"dIBXsB\""},{"type":"html","content":"<span class='clj-string'>&quot;Beede4e&quot;</span>","value":"\"Beede4e\""},{"type":"html","content":"<span class='clj-string'>&quot;UJJJJxU&quot;</span>","value":"\"UJJJJxU\""},{"type":"html","content":"<span class='clj-string'>&quot;ddddddd&quot;</span>","value":"\"ddddddd\""},{"type":"html","content":"<span class='clj-string'>&quot;xDxbmm&quot;</span>","value":"\"xDxbmm\""},{"type":"html","content":"<span class='clj-string'>&quot;wRxxHx&quot;</span>","value":"\"wRxxHx\""},{"type":"html","content":"<span class='clj-string'>&quot;HZXAOeg&quot;</span>","value":"\"HZXAOeg\""},{"type":"html","content":"<span class='clj-string'>&quot;bOfJdJc&quot;</span>","value":"\"bOfJdJc\""},{"type":"html","content":"<span class='clj-string'>&quot;EBBodBx&quot;</span>","value":"\"EBBodBx\""},{"type":"html","content":"<span class='clj-string'>&quot;EeaeOE1&quot;</span>","value":"\"EeaeOE1\""},{"type":"html","content":"<span class='clj-string'>&quot;fJfPDrB&quot;</span>","value":"\"fJfPDrB\""},{"type":"html","content":"<span class='clj-string'>&quot;555PoUP&quot;</span>","value":"\"555PoUP\""},{"type":"html","content":"<span class='clj-string'>&quot;orsrmm&quot;</span>","value":"\"orsrmm\""},{"type":"html","content":"<span class='clj-string'>&quot;edwJxJJ&quot;</span>","value":"\"edwJxJJ\""},{"type":"html","content":"<span class='clj-string'>&quot;oobbPsb&quot;</span>","value":"\"oobbPsb\""},{"type":"html","content":"<span class='clj-string'>&quot;JoDBRnj&quot;</span>","value":"\"JoDBRnj\""},{"type":"html","content":"<span class='clj-string'>&quot;9gfKDJJ&quot;</span>","value":"\"9gfKDJJ\""},{"type":"html","content":"<span class='clj-string'>&quot;PtdkIx&quot;</span>","value":"\"PtdkIx\""},{"type":"html","content":"<span class='clj-string'>&quot;JJXPJJd&quot;</span>","value":"\"JJXPJJd\""},{"type":"html","content":"<span class='clj-string'>&quot;a7PssxP&quot;</span>","value":"\"a7PssxP\""},{"type":"html","content":"<span class='clj-string'>&quot;BF4lFe&quot;</span>","value":"\"BF4lFe\""},{"type":"html","content":"<span class='clj-string'>&quot;iOFtCO&quot;</span>","value":"\"iOFtCO\""},{"type":"html","content":"<span class='clj-string'>&quot;oNMeoox&quot;</span>","value":"\"oNMeoox\""},{"type":"html","content":"<span class='clj-string'>&quot;xXesJex&quot;</span>","value":"\"xXesJex\""},{"type":"html","content":"<span class='clj-string'>&quot;jse5ejj&quot;</span>","value":"\"jse5ejj\""},{"type":"html","content":"<span class='clj-string'>&quot;JJwPdJJ&quot;</span>","value":"\"JJwPdJJ\""},{"type":"html","content":"<span class='clj-string'>&quot;7sdsot&quot;</span>","value":"\"7sdsot\""},{"type":"html","content":"<span class='clj-string'>&quot;IBHRMoo&quot;</span>","value":"\"IBHRMoo\""},{"type":"html","content":"<span class='clj-string'>&quot;moxsmm&quot;</span>","value":"\"moxsmm\""},{"type":"html","content":"<span class='clj-string'>&quot;PPJoPsV&quot;</span>","value":"\"PPJoPsV\""},{"type":"html","content":"<span class='clj-string'>&quot;DPxxXDm&quot;</span>","value":"\"DPxxXDm\""},{"type":"html","content":"<span class='clj-string'>&quot;55exdz5&quot;</span>","value":"\"55exdz5\""},{"type":"html","content":"<span class='clj-string'>&quot;RbbzZjj&quot;</span>","value":"\"RbbzZjj\""},{"type":"html","content":"<span class='clj-string'>&quot;tMbImIt&quot;</span>","value":"\"tMbImIt\""},{"type":"html","content":"<span class='clj-string'>&quot;dmdBdV5&quot;</span>","value":"\"dmdBdV5\""},{"type":"html","content":"<span class='clj-string'>&quot;OOOxrOa&quot;</span>","value":"\"OOOxrOa\""},{"type":"html","content":"<span class='clj-string'>&quot;dAdddBd&quot;</span>","value":"\"dAdddBd\""},{"type":"html","content":"<span class='clj-string'>&quot;AmAxoso&quot;</span>","value":"\"AmAxoso\""},{"type":"html","content":"<span class='clj-string'>&quot;DDDjDsD&quot;</span>","value":"\"DDDjDsD\""},{"type":"html","content":"<span class='clj-string'>&quot;JoJJJdJ&quot;</span>","value":"\"JoJJJdJ\""},{"type":"html","content":"<span class='clj-string'>&quot;IxxBdDB&quot;</span>","value":"\"IxxBdDB\""},{"type":"html","content":"<span class='clj-string'>&quot;oombDo&quot;</span>","value":"\"oombDo\""},{"type":"html","content":"<span class='clj-string'>&quot;bRxPPm&quot;</span>","value":"\"bRxPPm\""},{"type":"html","content":"<span class='clj-string'>&quot;PdossoX&quot;</span>","value":"\"PdossoX\""},{"type":"html","content":"<span class='clj-string'>&quot;sBssssx&quot;</span>","value":"\"sBssssx\""}],"value":"(\"eGJJLJJ\" \"B1ZBJBe\" \"obVetCC\" \"6bVmRod\" \"PoBseeG\" \"X7JbOJJ\" \"XRRRHHJ\" \"oAV55os\" \"DxeeVof\" \"JeJJeJ\" \"O5i5zz5\" \"BBBddd\" \"sssNbs\" \"zttaata\" \"5IcAD5\" \"xxos4PP\" \"e7oesss\" \"44ojVmo\" \"oobHobe\" \"RRoIRz\" \"Pmd4m5\" \"tJJJJd5\" \"oooooz\" \"DXoBDBB\" \"RBeBoo\" \"GkmmiiB\" \"DJooDA\" \"Uo44e4\" \"bIRnPeI\" \"osIQbpj\" \"zzdoRoe\" \"GVrrJxH\" \"mmmso7\" \"bGsbbG\" \"sPmPsP\" \"kJsJsox\" \"jJmJIk\" \"JrrsrJs\" \"sbVasJJ\" \"dJdrrer\" \"ssH1RH\" \"RdodGeR\" \"Boseos\" \"JJJJOJJ\" \"XXJtGJJ\" \"kgPXBsP\" \"BBssz7s\" \"sssjBds\" \"xoVdVsd\" \"jAbOJo5\" \"fddDxF\" \"O7Uooo\" \"iboeJie\" \"xssoDs4\" \"bxmbbrV\" \"dIBXsB\" \"Beede4e\" \"UJJJJxU\" \"ddddddd\" \"xDxbmm\" \"wRxxHx\" \"HZXAOeg\" \"bOfJdJc\" \"EBBodBx\" \"EeaeOE1\" \"fJfPDrB\" \"555PoUP\" \"orsrmm\" \"edwJxJJ\" \"oobbPsb\" \"JoDBRnj\" \"9gfKDJJ\" \"PtdkIx\" \"JJXPJJd\" \"a7PssxP\" \"BF4lFe\" \"iOFtCO\" \"oNMeoox\" \"xXesJex\" \"jse5ejj\" \"JJwPdJJ\" \"7sdsot\" \"IBHRMoo\" \"moxsmm\" \"PPJoPsV\" \"DPxxXDm\" \"55exdz5\" \"RbbzZjj\" \"tMbImIt\" \"dmdBdV5\" \"OOOxrOa\" \"dAdddBd\" \"AmAxoso\" \"DDDjDsD\" \"JoJJJdJ\" \"IxxBdDB\" \"oombDo\" \"bRxPPm\" \"PdossoX\" \"sBssssx\")"}
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
;;; SMC :  0.9826190483570099 
;;; RMH :  0.9821428573131562 
;;; CSIS:  0.981190477013588
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
