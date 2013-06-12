(ns nlp.narrative.chains.cloze
  "A scoring metric again implemented from the Chambers 2011 dissertation.

   An experiment produces results in the form of [score recall].

   Where score is the number of guesses it took the algorithm to predict the missing event,
   or the length of the guess list if it could not make a correct prediction.
   Recall is 1 if the algorithm predicted correctly and 0 otherwise."
  (:use [nlp.narrative.chains.util]
        [nlp.narrative.chains.core]))

(defn get-protagonist-chain-for-file [file-name]
  (let [verb-tuples (extract-verb-tuples file-name)]
    (get-protagonist verb-tuples)))

(defn run-cloze-experiment
  "Returns the score for narrative cloze on a set of files representing a single experiment.
   Results are in the format [score recall]

   file-list  : A list of files to process for the experiment. These must be in a processed
   format from the stanford parser."
  [file-list]
  (let [test-file-index (rand-int (count file-list))
        test-file (nth file-list test-file-index)
        training-files (filter #(not= % test-file) file-list)
        protagonist-chain (get-protagonist-chain-for-file test-file)
        removed-protagonist-index (rand-int (count protagonist-chain))
        removed-protagonist-tuple (assoc (nth protagonist-chain removed-protagonist-index) :coreference nil)
        removed-protagonist-chain (concat (take removed-protagonist-index protagonist-chain)
                                          (drop (+ 1 removed-protagonist-index) protagonist-chain))
        tuple-list (extract-verb-tuples-for-files training-files)
        chain-predictions (get-predictions-for-chain tuple-list removed-protagonist-chain)
        ]
    (cond
      (= 0 (count chain-predictions)) [1 0]
      :else (let [ index-of-guess (.indexOf (map first chain-predictions) removed-protagonist-tuple)]
              [(if (= -1 index-of-guess) (count chain-predictions) (+ 1 index-of-guess)) (if (= -1 index-of-guess) 0 1)]))))

(defn run-cloze-experiments [experiment-list]
  "Runs the cloze experiment for a series of experiments and aggregates the results.
   Experiments are just lists of lists of files for each experiment."
  (let [results (map run-cloze-experiment experiment-list)
        average-score (/ (reduce + (map first results)) (count results))
        average-recall (/ (reduce + (map second results)) (count results))]
    (println (str "Average Recall " average-recall))
    (println (str "Average Score " average-score))
    results))
