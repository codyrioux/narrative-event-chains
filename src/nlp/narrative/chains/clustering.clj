(ns nlp.narrative.chains.clustering)

(defn distance
  "The clustering distance function for agglomerative clustering."
  [verb-tuples clustera clusterb]
  (->>
    (for [a clustera] (for [b clusterb] (pmi verb-tuples a b)))
    (apply concat)
    (reduce +)))

(defn cluster
  "Clusters the tuples into discrete chains using agglomerative clustering."
  [verb-tuples threshold]
  (loop 
    [clusters (set (for [x verb-tuples] #{x}))
     clusters-1 #{}]
    (cond
      (= clusters clusters-1)
      clusters
      :else
      (recur
      (map
        #(let [clusters (difference clusters #{%})
               distances (zipmap (map distance % clusters) clusters)
               min-dist (reduce min (keys distances))
               closest (distances min-dist)]
           (if (< min-dist threshold) (union % closest) %))
        clusters)
        clusters))))
