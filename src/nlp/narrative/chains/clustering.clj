(ns nlp.narrative.chains.clustering
  (:use [clojure.set])
  (:require
    [nlp.narrative.chains.util :as util]
    [nlp.narrative.chains.stats :as stats]))

(defn distance
  "The clustering distance function for agglomerative clustering."
  [coref-counts clustera clusterb]
  (->>
    (for [a clustera] (for [b clusterb] (stats/pmi coref-counts a b)))
    flatten
    (reduce +)))

(defn cluster
  "Clusters the tuples into discrete chains using agglomerative clustering."
  [coref-counts threshold min-size]
  ;; Get all verbs from coref counts, make those your initial clusters
  ;;
  (loop 
    [clusters (set (doall (for [x (seq (apply union (keys coref-counts)))] #{x})))
     clusters-1 #{}]
    (cond
      (= clusters clusters-1)
      (filter #(<= min-size (count %)) clusters)
      :else
      (recur
        (map
          #(let [clusters (remove #{%} clusters)
                 closest (util/argmin (partial distance coref-counts %) clusters) ]
             (if (> (distance coref-counts % closest) threshold) (union % closest) %))
          clusters)
        clusters))))
