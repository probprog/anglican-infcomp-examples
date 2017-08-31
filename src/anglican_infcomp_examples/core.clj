;; gorilla-repl.fileformat = 1

;; @@
(ns anglican-infcomp-examples.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.tools.cli :as cli]
            [clojure.string :as str]
            [anglican.infcomp.pool :as pool]
            [anglican.infcomp.zmq :as zmq]
            [anglican.inference :refer [infer]]
            [anglican.state :refer [get-result get-log-weight]]
            anglican.infcomp.csis))

(defn load-var
  "loads a variable from clojure namespace"
  [ns-name var]
  (require (symbol ns-name) :reload)
  (var-get (or (ns-resolve (symbol ns-name) (symbol var))
               (throw (Exception. (format "no such variable: %s/%s"
                                          ns-name var))))))

(def cli-options
  [["-h" "--help" "Print usage summary and exit"]
   ["-m" "--mode MODE" "Either compile or infer"
    :default :compile
    :parse-fn keyword
    :validate [#{:compile :infer}
               "The mode must be either compile or infer"]]

   ["-n" "--namespace <string>" "Clojure namespace in which query and the required variables and functions live"]
   ["-q" "--query <string>" "Name of the query"]

   ;; Compile options
   ["-t" "--compile-endpoint <endpoint>" "COMPILE: endpoint"
    :default "tcp://*:5555"]

   ["-p" "--pool" "COMPILE: Pool"]

   ["-f" "--pool-folder <string>" "COMPILE: Pool folder"
    :default "./"]

   ["-r" "--pool-traces-per-file <int>" "COMPILE: Traces per file for pool"
    :default 1000
    :parse-fn #(Integer/parseInt %)]

   ["-z" "--pool-max-folder-size <int>" "COMPILE: Maximum folder size for pool in bytes"
    :default 1000
    :parse-fn #(Integer/parseInt %)]

   ["-o" "--compile-combine-observes-fn <variable name>" "COMPILE: Name of a function to combine observes"]

   ["-s" "--compile-combine-samples-fn <variable name>" "COMPILE: Name of a function to combine samples"]

   ["-a" "--compile-query-args <variable name>" "COMPILE: Name of the variable storing the query arguments for compilation"]

   ["-x" "--compile-query-args-value <clojure value>" "COMPILE: Query arguments for compilation"
    :parse-fn edn/read-string]

   ;; Infer options
   ["-N" "--infer-number-of-samples <int>" "INFER: Number of samples to output"
    :default 1
    :parse-fn #(Integer/parseInt %)]

   ["-T" "--infer-endpoint <endpoint>" "INFER: Endpoint"
    :default "tcp://localhost:6666"]

   ["-E" "--infer-observe-embedder-input <variable name>" "INFER: Name of the variable storing the input to the observe embedder"]

   ["-Y" "--infer-observe-embedder-input-value <clojure value>" "INFER: Input to the observe embedder"
    :parse-fn edn/read-string]
   
   ["-F" "--infer-observe-embedder-input-operation <string>" "INFER: Name of a function to transform the query arguments to give the observe embedder input"]
   
   ["-P" "--infer-query-args-path <string>" "INFER: Path to the file containing the query arguments"]
   
   ["-G" "--infer-query-args-getter <variable name>" "INFER: Name of a function to extract query arguments from file"]

   ["-A" "--infer-query-args <variable name>" "INFER: Name of the variable storing the query arguments for inference"]

   ["-Z" "--infer-query-args-value <value>" "INFER: Query arguments for inference"
    :parse-fn edn/read-string]
   
   ["-O" "--infer-output-path <string>" "INFER: Path to a file to dump the output"]

   ["-d" "--debug" "debug"]])

(defn usage [summary]
  (str "Usage:

COMPILATION

Start the compilation server:

```
lein run -- \\
--mode compile \\
--namespace queries.gaussian
```

Then run the following to train the neural network:

```
python -m infcomp.compile
```

INFERENCE

Start the inference server:

```
python -m infcomp.infer
```

Then run inference:
```
lein run -- \\
--mode infer \\
--namespace queries.gaussian \\
--infer-query-args-value [2.3]
```

\n" summary))

(defn error-msg [errors]
  (str/join "\n\t" (cons "ERROR parsing the command line:" errors)))

(defn main
  "Run either the compilation or the inference for a given query."
  [& args]
  (let [{:keys [options arguments errors summary] :as parsed-options}
        (cli/parse-opts args cli-options)]
    (cond
     (:help options) (binding [*out* *err*]
                       (println (usage summary)))

     errors (binding [*out* *err*]
              (println (error-msg errors)))

     :else
     (try
       ;; Load the query.
       (let [ns-name (:namespace options)
             query-name (or (:query options) (str/replace ns-name #".+\." ""))
             query (load-var ns-name query-name)]
         (if (= (:mode options) :compile)
           ;; Compile
           (let [combine-observes-fn (if (:compile-combine-observes-fn options)
                                       (load-var ns-name (:compile-combine-observes-fn options))
                                       (fn [observes]
                                         (:value (first observes))))
                 combine-samples-fn (if (:compile-combine-samples-fn options)
                                      (load-var ns-name (:compile-combine-samples-fn options))
                                      identity)
                 query-args (if (:compile-query-args options)
                              (load-var ns-name (:compile-query-args options))
                              (:compile-query-args-value options))
                 pool (:pool options)
                 pool-folder (:pool-folder options)
                 pool-traces-per-file (:pool-traces-per-file options)
                 pool-max-folder-size (:pool-max-folder-size options)
                 endpoint (:compile-endpoint options)]
             (if pool
               (do
                 (println
                  (format (str ";; Namespace:                     %s\n"
                               ";; Query:                         %s\n"
                               ";; Mode:                          compile (pool) \n"
                               ";; Combine observes function:     %s\n"
                               ";; Combine samples function:      %s\n"
                               ";; Compile query arguments:       %s\n"
                               ";; Compile query arguments value: %s\n"
                               ";; Pool folder:                   %s\n"
                               ";; Traces per file:               %s\n"
                               ";; Maximum folder size (bytes)    %s\n")
                          ns-name
                          query-name
                          (or (:compile-combine-observes-fn options) "default: (fn [observes] (:value (first observes)))")
                          (or (:compile-combine-samples-fn options) "default: identity")
                          (or (:compile-query-args options) "default: nil")
                          (if query-args
                            (str (apply str (take 77 (str query-args))) "...")
                            "nil")
                          pool-folder
                          pool-traces-per-file
                          pool-max-folder-size))
                 (pool/start-pool query
                                  query-args
                                  combine-observes-fn
                                  pool-folder
                                  :combine-samples-fn combine-samples-fn
                                  :traces-per-file pool-traces-per-file
                                  :max-folder-size pool-max-folder-size)
                 (println (str "Started generating a pool of training data in " pool-folder)))
               (do
                 (println
                  (format (str ";; Namespace:                     %s\n"
                               ";; Query:                         %s\n"
                               ";; Mode:                          compile \n"
                               ";; Combine observes function:     %s\n"
                               ";; Combine samples function:      %s\n"
                               ";; Compile query arguments:       %s\n"
                               ";; Compile query arguments value: %s\n")
                          ns-name
                          query-name
                          (or (:compile-combine-observes-fn options) "default: (fn [observes] (:value (first observes)))")
                          (or (:compile-combine-samples-fn options) "default: identity")
                          (or (:compile-query-args options) "default: nil")
                          (if query-args
                            (str (apply str (take 77 (str query-args))) "...")
                            "nil")))
                 (zmq/start-replier query
                                    query-args
                                    combine-observes-fn
                                    :combine-samples-fn combine-samples-fn
                                    :endpoint endpoint)
                 (println (str "Compilation server started at " endpoint)))))

           ;; Infer
           (let [query-args (cond
                              (:infer-query-args-path options) ((load-var ns-name (:infer-query-args-getter options)) (:infer-query-args-path options))
                              (:infer-query-args options) (load-var ns-name (:infer-query-args options))
                              (:infer-query-args-value options) (:infer-query-args-value options)
                              :else nil)
                 
                 observe-embedder-input (cond
                              			  (:infer-observe-embedder-input options) (load-var ns-name (:infer-observe-embedder-input options))
                              			  (:infer-observe-embedder-input-value options) (:infer-observe-embedder-input-value options)
                              			  (:infer-observe-embedder-input-operation options) ((load-var ns-name (:infer-observe-embedder-input-operation options)) query-args)
                                          :else nil)
                 
                 endpoint (:infer-endpoint options)
                 num-samples (:infer-number-of-samples options)
                 states (infer :csis
                               query
                               query-args
                               :endpoint endpoint
                               :observe-embedder-input observe-embedder-input)]
             (println
              (format (str ";; Namespace:                    %s\n"
                           ";; Query:                        %s\n"
                           ";; Mode:                         infer (server: %s)\n"
                           ";; Infer query arguments:        %s\n"
                           ";; Infer query arguments value:  %s\n"
                           ";; Observe embedder input:       %s\n"
                           ";; Observe embedder input value: %s\n"
                           ";; Number of samples:            %s\n")
                      ns-name
                      query-name
                      endpoint
                      (or (:infer-query-args options) "nil")
                      (if query-args
                        (str (apply str (take 77 (str query-args))) "...")
                        "nil")
                      (or (:infer-observe-embedder-input options) "nil")
                      (if observe-embedder-input
                        (str (apply str (take 77 (str observe-embedder-input))) "...")
                        "nil")
                      num-samples))
             (if (:infer-output-path options)
               (spit (:infer-output-path options) (str/join "\n" (map #(str (str/join "," (:result %)) "," (:log-weight %)) (take num-samples states))))
               (mapv #(println (str (get-result %) "," (get-log-weight %)))
                   (take num-samples states))))))
       ;; Otherwise, could not load the query.
       (catch Exception e
         (binding [*out* *err*]
           (println e)
           (when (:debug options)
             (.printStackTrace e))))))))

(defn -main
  "invoking main from the command line"
  [& args]
  (apply main args)
  (shutdown-agents))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anglican-infcomp-examples.core/-main</span>","value":"#'anglican-infcomp-examples.core/-main"}
;; <=

;; @@

;; @@
