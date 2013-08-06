(ns nlp.narrative.chains.ordering
  (:require [clojure.math.combinatorics :as combo]))

(defn read-order-file
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall
      (for [line (line-seq rdr)]
        (clojure.string/split line #"\s")))))

(defn get-order-hash
  "Returns the preprocessed datastructure in the form of a map of [verb1 verb2] => count"
  []
  (reduce #(assoc %1 (vec (take 2 %2)) (read-string (nth %2 2))) {} (read-order-file "verb-pair-orders")))

(defn order
  "Given a tuple [verb1 verb2] determines and returns the proper ordering of the two verbs.
   Returns nil in the event it cannot determine a likely ordering."
  [observation-map tuple]
  (let [tuple-count (get observation-map tuple 0)
        rev [(second tuple) (first tuple)]
        rev-count (get observation-map rev 0)]
    (cond
      (and (= 0 tuple-count) (= 0 rev-count)) nil
      (>= tuple-count rev-count) tuple
      :else rev-count)))

(defn get-verb-ordering
  [observation-map verbs]
  (filter (comp not nil?) (map (partial order observation-map) (combo/combinations verbs 2))))
