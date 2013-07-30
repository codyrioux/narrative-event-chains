(ns nlp.narrative.chains.core
  "Core of the Narrative Chain prediction model of Nathanael Chamber's 2011 dissertation.

   Input should be preprocessed by the Stanford CoreNLP Library, see
   https://github.com/codyrioux/narrative-event-chains/wiki/Input for specifics.

   Datastructures:

   verb-tuples:
   verb-tuples are of the following form where :dependency can be :nsubj or :pobj
   coreference is a structure representing all of the representations of the same entity
   the primary concern that if (:coreference obj1) (:coreference obj2) are equal then
   the two tuples represent the same entity.
   {:verb word :dependency :nsubj :coreference (...)}
   
   chain:
   A chain should be represented by...
   A set of verb-tuples represented as they are above.
   A set of relations [t1, t2] that indicates t1 occurs strictly before t2.
  
   [[tuple0 tuple1 tuple2] [[t1, t2] [t0, t1]]]"
  (:use [clojure.set]))

(defn remove-nil-coreferences [tuple-list]
  "Removes those elements that do not corefer to anything."
  (remove #(nil? (:coreference %)) tuple-list))

(defn unique-verb-tuples [tuple-list]
  (seq (set (map #(assoc % :coreference nil) tuple-list))))

(defn unique-verb-tuples-and-coref [tuple-list]
  (seq (set tuple-list)))

(defn- count-occurences-disregard-coreference [tuple-list tuple]
  (count (filter #(= (dissoc % :coreference) (dissoc tuple :coreference)) tuple-list)))

(defn- count-occurences-regard-coreference [tuple-list tuple]
  (count (filter #(= % tuple) tuple-list)))

(defn- same-tuple? [tuple-a tuple-b]
  "Determines if the tuples are the same, regardless of coreference."
  (= (dissoc tuple-a :coreference) (dissoc tuple-b :coreference)))

(defn- coreferring? [tuple-a tuple-b]
  "Determines if the tuples are coreferring, regardless of type or dependency."
  (= (:coreference tuple-a) (:coreference tuple-b)))

(defn p [tuple-list tuple]
  "Returns the probability of a single tuple given the tuple list and specified tuple."
  (/ (count-occurences-disregard-coreference tuple-list tuple) (count tuple-list)))

(defn get-tuples-referring-to [tuple-list coref]
  "Returns only those tuples in the tuple list referring to coref."
  (filter #(= coref (:coreference %)) tuple-list))

;; Stats

(defn c [tuple-list tuple-a tuple-b]
  "Calculates the number of times verb tuples a and b were observed with a coreferring entity
   filling one of their dependencies.

   tuple-list: A collection of verb-tuples as specified in this namespace.
   tuple-a: A verb tuple as specified in this namespace.
   tuple-b A verb tuple as specified in this namespace.

   Returns: The number of times tuple a and b coreferred to the same entity."
  (let [tuple-a-occurences (filter #(and (= (:dependency %) (:dependency tuple-a)) (= (:verb %) (:verb tuple-a))) tuple-list)
        tuple-b-occurences (filter #(and (= (:dependency %) (:dependency tuple-b)) (= (:verb %) (:verb tuple-b))) tuple-list)]
    (reduce + (map #(count (get-tuples-referring-to tuple-b-occurences (:coreference %))) tuple-a-occurences))))

(defn cooccurence-count [tuple-list tuple-a]
  "Returns the total number of tuples that corefer with the provided tuple.
   Does not double count other occurences of tuple-a."
  (count (filter (partial coreferring? tuple-a) (remove #{tuple-a} tuple-list))))

(defn total-sum-c [tuple-list]
  (->>
    (unique-verb-tuples-and-coref tuple-list)
    (map (partial cooccurence-count tuple-list)) ;; Want to get a count for each verb in the unique tuple list for cooccurences with other verbs
    (reduce +)))

(def total-sum-c (memoize total-sum-c))

(defn p2 [tuple-list tuple-a tuple-b]
  "Calculates the numerator for the pmi equation. Filters the list to only those items with coreferences."
  (let [tuple-list (filter #(not (= nil (:coreference %))) tuple-list)]
    (/ (c tuple-list tuple-a tuple-b) (total-sum-c tuple-list))))

(defn logarithm [x]
  (java.lang.Math/log x))

(defn pmi [tuple-list tuple-a tuple-b]
  (let [prob (partial p tuple-list)
        tuple-list (filter #(not (= nil (:coreference %))) tuple-list)]
    (logarithm (/ (+ 1 (p2 tuple-list tuple-a tuple-b)) (+ 1 (* (prob tuple-a) (prob tuple-b)))))))

(def pmi (memoize pmi))

;; Interface

(defn chainsim
  "Takes the entire verb-tuple list, a chain, and a tuple.
   Returns the sum of PMI with each element of the chain and the specified tuple."
  [verb-tuples chain tuple-a]
  (let [pmi (partial pmi verb-tuples tuple-a)]
    (reduce + (map pmi chain))))

(defn get-predictions-for-chain [tuple-list chain]
  (let [tuple-list (remove-nil-coreferences tuple-list)
        chainsim (partial chainsim tuple-list chain)]
    (->>
      (unique-verb-tuples tuple-list)
      (map #(vec [% (chainsim %)]))
      (sort #(> (second %1) (second %2))))))

;; Clustering

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
