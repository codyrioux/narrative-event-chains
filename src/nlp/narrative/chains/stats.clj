(ns nlp.narrative.chains.stats
  "Stats namespace implements some mathematical
   functions for performing calculations on the
   preprocessed verb-dependency pairs.
   
   pair-counts is a map of: #{tuple-a tuple-b} => times-coreferred")

(defn- log [x] (java.lang.Math/log x))

(defn p
  "Determines the probability of tuple occuring given the tuple-counts."
  [pair-counts tuple]
  (/ (reduce + 0 (map (partial pair-counts) (filter #(contains? % tuple) (keys pair-counts))))
     (reduce + 0 tuple-counts)))

(defn p2
  "The numerator in the PMI equation."
  [pair-counts tuple-a tuple-b]
  (/ (pair-counts #{tuple-a tuple-b}) (reduce + 0 (vals pair-counts))))

(defn pmi
  "Calculates the pointwise mutual information between the two provided tuples."
  [pair-counts tuple-a tuple-b]
  (log (/ (+ 1 (p2 pair-counts tuple-a tuple-b))
          (+ 1 (* (p pair-counts tuple-a) (p pair-counts tuple-b))))))
