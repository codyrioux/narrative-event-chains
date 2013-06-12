(ns nlp.narrative.chains.core
  "Core of the Narrative Chain prediction model of Nathanael Chamber's 2011 dissertation."
  (:use
    [nlp.narrative.chains.util :only [extract-verb-tuples]]))

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

(defn c [tuple-list tuple-a tuple-b]
  "Returns the number of times tuple-a and tuple-b were observed with a coreferring entity
   filling their dependency. This is a VERY IMPORTANT equation as it will greatly effect our
   probabilities."
  (let [tuple-a-occurences (filter #(and (= (:dependency %) (:dependency tuple-a)) (= (:verb %) (:verb tuple-a))) tuple-list)
        tuple-b-occurences (filter #(and (= (:dependency %) (:dependency tuple-b)) (= (:verb %) (:verb tuple-b))) tuple-list)]
    (reduce + (map #(count (get-tuples-referring-to tuple-b-occurences (:coreference %))) tuple-a-occurences))))

(defn cooccurence-count [tuple-list tuple-a]
  "Returns the total number of tuples that corefer with the provided tuple.
   Does not double count other occurences of tuple-a."
  (count (filter (partial coreferring? tuple-a) (remove #{tuple-a} tuple-list))))

(defn total-sum-c [tuple-list]
  ""
  (->>
    (unique-verb-tuples-and-coref tuple-list)
    (map (partial cooccurence-count tuple-list)) ;; Want to get a count for each verb in the unique tuple list for cooccurences with other verbs
    (reduce +)
    ))

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

(defn chainscore [verb-tuples chain tuple-a]
  "Takes the entire verb-tuple list, a chain, and a tuple.
   Returns the sum of PMI with each element of the chain and the specified tuple."
  (let [pmi (partial pmi verb-tuples tuple-a)]
    (reduce + (map pmi chain))))

(defn get-predictions-for-chain [tuple-list chain]
  (let [tuple-list (remove-nil-coreferences tuple-list)
        chainscore (partial chainscore tuple-list chain)]
    (->>
      (unique-verb-tuples tuple-list)
      (map #(vec [% (chainscore %)]))
      (sort #(> (second %1) (second %2))))))
