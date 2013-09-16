(ns nlp.narrative.chains.util)

(defn after
  "Returns all the items in a collection after the specified item."
  [coll item]
  (loop [coll coll item item]
    (if (= item (first coll)) (rest coll) (recur (rest coll) item))))

(defn fmap
  "Map f onto the values of the map m."
  [f m]
  (into  {}  (for  [[k v] m]  [k  (f v)])))

(defn fkmap
  "Map f onto k and the values of map m."
  [f m]
  (into {} (for [[k v] m] [k (f k v)])))

(defn argmax
  "Returns the item in coll which results in the maximal value for f."
  [f coll]
  (let [results (zipmap coll (map f coll))
        max-value (apply max (vals results))
        max-args (map first (filter #(= max-value (second %)) results))]
    (first max-args)))

(defn argmin
  "Returns the item in coll which results in the minimal value for f."
  [f coll]
  (let [results (zipmap coll (map f coll))
        min-value (apply min (vals results))
        min-args (map first (filter #(= min-value (second %)) results))]
    (first min-args)))

(defn most-frequent-n  [n coll]
  "Returns the top n most frequent items in coll."
  (->> coll
       frequencies
       (sort-by val)
       reverse
       (take n)
       (map first)))
