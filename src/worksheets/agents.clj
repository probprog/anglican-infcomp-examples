;; gorilla-repl.fileformat = 1

;; **
;;; # Agents
;; **

;; @@
(ns worksheets.agents
  (:require [anglican.runtime :refer :all]
            [anglican.emit :refer :all]
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
            anglican.infcomp.core
            [clojure.data.priority-map :refer [priority-map]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
; Dijkstra algorithm from: https://gist.github.com/ummels/86c09182dee25b142280
(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a map from nodes to their distance from start."
  [start f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(def WALL 1)
(defn get-neighbors
  "returns a list of neighbors of position"
  [position world]
  (filter (fn [[row col]]
            (and (> row 0)
                 (< row (m/row-count world))
                 (> col 0)
                 (< col (m/column-count world))
                 (not= (get-in world [row col]) WALL)))
          (map #(m/add position %) [[1 0] [0 1] [-1 0] [0 -1]])))

; https://stackoverflow.com/questions/8815772/how-can-i-find-the-index-of-the-smallest-member-of-this-vector-in-clojure
(defn argmin [v]
  (first (apply min-key second (map-indexed vector v))))

(defn policy [position desired-position world]
  (let [f #(apply hash-map (flatten (map vector (get-neighbors % world) (repeat 1))))
        distance-map (dijkstra desired-position f)
        neighbors (get-neighbors position)
        distances-of-neighbors-to-desired-position (map distance-map neighbors)]
    (nth neighbors (argmin distances-of-neighbors-to-desired-position))))

#_(defn policy [current-position desired-position world]
  (let [required-movement (map - desired-position current-position)]
    (map #(int (+ % 0.5)) (map #(/ % (reduce + required-movement)) required-movement))))

(anglican.infcomp.core/reset-infcomp-addressing-scheme!)
(with-primitive-procedures [policy m/identity-matrix]
  (defquery agents [ observed-positions world initial-position ]
    (let [desired-position [(sample (uniform-discrete 0 (count world))) (sample (uniform-discrete 0 (count (first world))))]]
      (reduce
        (fn [ positions obs ]
          (let [ current-position (peek positions)
                 step (policy current-position desired-position world) ]
            (println positions)
            (observe (mvn current-position [[1 0][0 1]]) obs)
            (println "successfully observed")
            (conj positions (map + current-position step))
            ))
        [initial-position]
        observed-positions)
      )
    )
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.agents/agents</span>","value":"#'worksheets.agents/agents"}
;; <=

;; @@
(def world [[0 0 0 0 0 0]
            [0 0 1 0 0 0]
            [0 0 1 0 0 0]
            [0 0 1 0 0 0]
            [0 0 1 0 0 0]
            [0 0 0 0 0 0]
            [0 0 0 0 0 0]] )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;worksheets.agents/world</span>","value":"#'worksheets.agents/world"}
;; <=

;; @@
(first (prior/sample-from-prior agents [ [[0 0] [0 0] [0 0] [0 0] [0 0]] world [0 0] ]))
;; @@
;; ->
;;; [[0 0]]
;;; successfully observed
;;; [[0 0] (1 0)]
;;; successfully observed
;;; [[0 0] (1 0) (2 0)]
;;; successfully observed
;;; [[0 0] (1 0) (2 0) (3 0)]
;;; successfully observed
;;; [[0 0] (1 0) (2 0) (3 0) (4 1)]
;;; successfully observed
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:log-weight</span>","value":":log-weight"},{"type":"html","content":"<span class='clj-double'>0.0</span>","value":"0.0"}],"value":"[:log-weight 0.0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:anglican.state/mem</span>","value":":anglican.state/mem"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[],"value":"{}"}],"value":"[:anglican.state/mem {}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:anglican.state/store</span>","value":":anglican.state/store"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:anglican.state/store nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:anglican.state/catch-stack</span>","value":":anglican.state/catch-stack"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[:anglican.state/catch-stack nil]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:samples</span>","value":":samples"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:time-index</span>","value":":time-index"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}],"value":"[:time-index 0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-address</span>","value":":sample-address"},{"type":"html","content":"<span class='clj-string'>&quot;S27&quot;</span>","value":"\"S27\""}],"value":"[:sample-address \"S27\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-instance</span>","value":":sample-instance"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}],"value":"[:sample-instance 0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:distribution</span>","value":":distribution"},{"type":"html","content":"<span class='clj-unkown'>#object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x2e81c86d &quot;anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@2e81c86d&quot;]</span>","value":"#object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x2e81c86d \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@2e81c86d\"]"}],"value":"[:distribution #object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x2e81c86d \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@2e81c86d\"]]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:value</span>","value":":value"},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"}],"value":"[:value 5]"}],"value":"{:time-index 0, :sample-address \"S27\", :sample-instance 0, :distribution #object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x2e81c86d \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@2e81c86d\"], :value 5}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:time-index</span>","value":":time-index"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}],"value":"[:time-index 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-address</span>","value":":sample-address"},{"type":"html","content":"<span class='clj-string'>&quot;S26&quot;</span>","value":"\"S26\""}],"value":"[:sample-address \"S26\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sample-instance</span>","value":":sample-instance"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}],"value":"[:sample-instance 0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:distribution</span>","value":":distribution"},{"type":"html","content":"<span class='clj-unkown'>#object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x75951904 &quot;anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@75951904&quot;]</span>","value":"#object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x75951904 \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@75951904\"]"}],"value":"[:distribution #object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x75951904 \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@75951904\"]]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:value</span>","value":":value"},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"}],"value":"[:value 2]"}],"value":"{:time-index 1, :sample-address \"S26\", :sample-instance 0, :distribution #object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x75951904 \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@75951904\"], :value 2}"}],"value":"[{:time-index 0, :sample-address \"S27\", :sample-instance 0, :distribution #object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x2e81c86d \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@2e81c86d\"], :value 5} {:time-index 1, :sample-address \"S26\", :sample-instance 0, :distribution #object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x75951904 \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@75951904\"], :value 2}]"}],"value":"[:samples [{:time-index 0, :sample-address \"S27\", :sample-instance 0, :distribution #object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x2e81c86d \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@2e81c86d\"], :value 5} {:time-index 1, :sample-address \"S26\", :sample-instance 0, :distribution #object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x75951904 \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@75951904\"], :value 2}]]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:observes</span>","value":":observes"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:time-index</span>","value":":time-index"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}],"value":"[:time-index 0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:observe-address</span>","value":":observe-address"},{"type":"html","content":"<span class='clj-symbol'>O19</span>","value":"O19"}],"value":"[:observe-address O19]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:observe-instance</span>","value":":observe-instance"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}],"value":"[:observe-instance 0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:value</span>","value":":value"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>-0.1677185664807136</span>","value":"-0.1677185664807136"},{"type":"html","content":"<span class='clj-double'>-1.0275189571539403</span>","value":"-1.0275189571539403"}],"value":"[-0.1677185664807136 -1.0275189571539403]"}],"value":"[:value [-0.1677185664807136 -1.0275189571539403]]"}],"value":"{:time-index 0, :observe-address O19, :observe-instance 0, :value [-0.1677185664807136 -1.0275189571539403]}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:time-index</span>","value":":time-index"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}],"value":"[:time-index 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:observe-address</span>","value":":observe-address"},{"type":"html","content":"<span class='clj-symbol'>O19</span>","value":"O19"}],"value":"[:observe-address O19]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:observe-instance</span>","value":":observe-instance"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}],"value":"[:observe-instance 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:value</span>","value":":value"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.996100928731968</span>","value":"0.996100928731968"},{"type":"html","content":"<span class='clj-double'>0.8334656972090937</span>","value":"0.8334656972090937"}],"value":"[0.996100928731968 0.8334656972090937]"}],"value":"[:value [0.996100928731968 0.8334656972090937]]"}],"value":"{:time-index 1, :observe-address O19, :observe-instance 1, :value [0.996100928731968 0.8334656972090937]}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:time-index</span>","value":":time-index"},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"}],"value":"[:time-index 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:observe-address</span>","value":":observe-address"},{"type":"html","content":"<span class='clj-symbol'>O19</span>","value":"O19"}],"value":"[:observe-address O19]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:observe-instance</span>","value":":observe-instance"},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"}],"value":"[:observe-instance 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:value</span>","value":":value"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>1.7235859090785226</span>","value":"1.7235859090785226"},{"type":"html","content":"<span class='clj-double'>0.4046510898995907</span>","value":"0.4046510898995907"}],"value":"[1.7235859090785226 0.4046510898995907]"}],"value":"[:value [1.7235859090785226 0.4046510898995907]]"}],"value":"{:time-index 2, :observe-address O19, :observe-instance 2, :value [1.7235859090785226 0.4046510898995907]}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:time-index</span>","value":":time-index"},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"}],"value":"[:time-index 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:observe-address</span>","value":":observe-address"},{"type":"html","content":"<span class='clj-symbol'>O19</span>","value":"O19"}],"value":"[:observe-address O19]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:observe-instance</span>","value":":observe-instance"},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"}],"value":"[:observe-instance 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:value</span>","value":":value"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>2.742653228270975</span>","value":"2.742653228270975"},{"type":"html","content":"<span class='clj-double'>1.0613853333946042</span>","value":"1.0613853333946042"}],"value":"[2.742653228270975 1.0613853333946042]"}],"value":"[:value [2.742653228270975 1.0613853333946042]]"}],"value":"{:time-index 3, :observe-address O19, :observe-instance 3, :value [2.742653228270975 1.0613853333946042]}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:time-index</span>","value":":time-index"},{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"}],"value":"[:time-index 4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:observe-address</span>","value":":observe-address"},{"type":"html","content":"<span class='clj-symbol'>O19</span>","value":"O19"}],"value":"[:observe-address O19]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:observe-instance</span>","value":":observe-instance"},{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"}],"value":"[:observe-instance 4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:value</span>","value":":value"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>3.5026703448230485</span>","value":"3.5026703448230485"},{"type":"html","content":"<span class='clj-double'>1.896921864494067</span>","value":"1.896921864494067"}],"value":"[3.5026703448230485 1.896921864494067]"}],"value":"[:value [3.5026703448230485 1.896921864494067]]"}],"value":"{:time-index 4, :observe-address O19, :observe-instance 4, :value [3.5026703448230485 1.896921864494067]}"}],"value":"[{:time-index 0, :observe-address O19, :observe-instance 0, :value [-0.1677185664807136 -1.0275189571539403]} {:time-index 1, :observe-address O19, :observe-instance 1, :value [0.996100928731968 0.8334656972090937]} {:time-index 2, :observe-address O19, :observe-instance 2, :value [1.7235859090785226 0.4046510898995907]} {:time-index 3, :observe-address O19, :observe-instance 3, :value [2.742653228270975 1.0613853333946042]} {:time-index 4, :observe-address O19, :observe-instance 4, :value [3.5026703448230485 1.896921864494067]}]"}],"value":"[:observes [{:time-index 0, :observe-address O19, :observe-instance 0, :value [-0.1677185664807136 -1.0275189571539403]} {:time-index 1, :observe-address O19, :observe-instance 1, :value [0.996100928731968 0.8334656972090937]} {:time-index 2, :observe-address O19, :observe-instance 2, :value [1.7235859090785226 0.4046510898995907]} {:time-index 3, :observe-address O19, :observe-instance 3, :value [2.742653228270975 1.0613853333946042]} {:time-index 4, :observe-address O19, :observe-instance 4, :value [3.5026703448230485 1.896921864494067]}]]"}],"value":"{:log-weight 0.0, :anglican.state/mem {}, :anglican.state/store nil, :anglican.state/catch-stack nil, :samples [{:time-index 0, :sample-address \"S27\", :sample-instance 0, :distribution #object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x2e81c86d \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@2e81c86d\"], :value 5} {:time-index 1, :sample-address \"S26\", :sample-instance 0, :distribution #object[anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj 0x75951904 \"anglican.infcomp.flatbuffers.uniform_discrete.UniformDiscreteClj@75951904\"], :value 2}], :observes [{:time-index 0, :observe-address O19, :observe-instance 0, :value [-0.1677185664807136 -1.0275189571539403]} {:time-index 1, :observe-address O19, :observe-instance 1, :value [0.996100928731968 0.8334656972090937]} {:time-index 2, :observe-address O19, :observe-instance 2, :value [1.7235859090785226 0.4046510898995907]} {:time-index 3, :observe-address O19, :observe-instance 3, :value [2.742653228270975 1.0613853333946042]} {:time-index 4, :observe-address O19, :observe-instance 4, :value [3.5026703448230485 1.896921864494067]}]}"}
;; <=

;; @@

;; @@
