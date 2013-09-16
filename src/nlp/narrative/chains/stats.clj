(ns nlp.narrative.chains.stats
  "Stats namespace implements some mathematical
   functions for performing calculations on the
   preprocessed verb-dependency pairs.

   coref-counts is a map of: #{tuple-a tuple-b} => times-coreferred")

(defn log [x] (java.lang.Math/log x))

(defn tuple-occurence-count
  "Determines the frequency with which a specific {:verb v :b b} occurs."
  [coref-counts tuple]
  (reduce + 0 (map coref-counts (filter #(contains? % tuple) (keys coref-counts)))))

(defn p
  "Determines the probability of tuple occuring given the tuple-counts."
  [coref-counts tuple]
  (/ (tuple-occurence-count coref-counts tuple)
     (reduce + 0 (vals coref-counts))))

(defn p2
  "The numerator in the PMI equation."
  [coref-counts tuple-a tuple-b]
  (/ (get coref-counts #{tuple-a tuple-b} 0) (reduce + 0 (vals coref-counts))))

(defn pmi
  "Calculates the pointwise mutual information between the two provided tuples."
  [coref-counts tuple-a tuple-b]
  (if (= tuple-a tuple-b) 0
    (log (/ (+ 1 (p2 coref-counts tuple-a tuple-b))
            (+ 1 (* (p coref-counts tuple-a) (p coref-counts tuple-b)))))))
