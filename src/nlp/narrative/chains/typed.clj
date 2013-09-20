(ns nlp.narrative.chains.typed
  "This namespace contains the typed extensions to the
   narrative chains implementation. In this case a type
   is simply a constraint on the arguments used in a chain."
  (:use [nlp.narrative.chains.core])
  (:require [nlp.narrative.chains.stats :as stats]
            [nlp.narrative.chains.util :as util]))

(defn- sim
  "Determines the similarity of two verb tuples in the context of
   a specific argument type specified by a. Lambda is a constant
   weighting factor. (Chambers found 0.8 to be optimal on his training set.)"
  [verb-tuples type-counts lambda a tuple-a tuple-b]
  (if
    (= tuple-a tuple-b) 0
    (+
     (stats/pmi verb-tuples tuple-a tuple-b)
     (*
      lambda
      (stats/log
        (get type-counts [#{tuple-a tuple-b} a] 0.5))))))

(defn- score
  "Determines an argument a (type) score with the specified chain."
  [verb-tuples type-counts lambda chain a]
  (reduce + 0
          (flatten
            (for [c (drop-last chain)]
              (for [d (util/after chain c)]
                (sim verb-tuples type-counts lambda a c d))))))

(defn chainsim'
  "Determines chain similarity for the tuple while taking arguments
   into consideration."
  [verb-tuples type-counts lambda chain tuple]
  (apply max (map #(+
                    (score verb-tuples type-counts lambda chain %)
                    (reduce + (map (partial sim verb-tuples type-counts lambda % tuple) chain)))
                  (distinct (map second (keys type-counts))))))
