(ns nlp.narrative.schemas.core
  "A narrative schema is a set of typed narrative chains.
   It models all participants in a set of events.
   In this implementation a narrative is simply modelled
   as a collection of chains: [chain1 chain2 chain3...]

   Schemas are built by starting with one desired verb
   and then adding others by maximizing narsim with the
   initial verb in a chain.

   Types can then be chosen for the chains by selecting
   the top types according to the score function.

   Ordering can be determined with the functions in the
   nlp.narrative.event.ordering namespace."
  (:use [clojure.set])
  (:require [nlp.narrative.chains.core :as chains]
            [nlp.narrative.chains.typed :as typed]
            [nlp.narrative.chains.util :as util]
            [nlp.narrative.chains.preprocessing :as prep]))

;;
;; Utility Functions
;;

(defn coref-count->verbs
  [coref-count]
  (map :verb
       (flatten (map seq (keys coref-count)))))

(defn narrative->verbs
  [narrative]
  (map :verb (flatten narrative)))

;;
;; Core Functions
;;

(def dv [:pobj :nsubj])

(defn narsim
  "Determines a similarity score for the verb v in the specified
   narrative by considering all slots at once. Base score beta
   is used to balance the decision between adding to the schema
   and splitting to a new chain.

   Verbs are added to the schema as follows:
   max narsim(N, vj)"
  [coref-counts type-count lambda beta narrative v]
  (reduce + 
          (flatten
            (for [d dv]
              (apply max
                     (map #(typed/chainsim'
                             coref-counts
                             type-count
                             lambda
                             %
                             {:verb v :dependency d})
                          narrative))))))

;;
;; WARNING
;; I was low on caffiene when writing the below code.
;; I feel it to be non idiomatic clojure code.
;; This is a prime candidate for refactoring.
;;

(defn merge-v-onto-chain
  [beta v chain]
  (map 
    #(if
       (and
         (= chain (second %))
         (< beta (nth % 2)))
       (conj chain (first %))
       chain)
    v))

(defn add-new-chains
  [beta v]
  (filter (comp not nil?)
          (map #(if (> beta (nth % 2)) [(first %)] nil) v)))

(defn add-to-nar
  [tuples type-counts lambda beta narrative v]
  (let
    [verb-chain-pairs
     (for [d dv] 
       (vec [{:verb v :dependency d}
             (util/argmax #(typed/chainsim'
                             tuples
                             type-counts
                             lambda
                             beta
                             %
                             {:verb v :dependency d})
                         narrative)
             (apply max #(typed/chainsim'
                           tuples
                           type-counts
                           lambda
                           beta
                           %
                           {:verb v :dependency d}))]))]
    (->>
      (map (partial merge-v-onto-chain beta verb-chain-pairs) narrative)
      (concat (add-new-chains beta verb-chain-pairs)))))

;;
;; Narrative Schemas Construction Functions
;;

(defn extract-narrative
  [coref-counts type-counts lambda beta verbs size v]
  (loop
    [narrative (for [d dv] [:verb v :dependency d])
     iter size]
    (cond
      (= 0 iter)
      narrative
      :else
      (recur 
        (add-to-nar
          coref-counts
          type-counts
          lambda
          beta
          narrative
          (util/argmax (partial narsim coref-counts type-counts lambda beta narrative) verbs))
        (dec iter)))))

(defn extract-narratives-of-size
  [data-dir lambda beta n size]
  (let
    [files (map #(.getPath %)  (rest  (file-seq  (clojure.java.io/file data-dir))))
     zxml-coll (map prep/get-zxml files)
     tuples-coll (map prep/extract-verb-tuples zxml-coll)
     coref-counts (apply merge-with + (map prep/create-coref-counts tuples-coll))
     type-counts (apply merge-with + (map prep/create-type-counts zxml-coll tuples-coll))
     verbs (distinct (coref-count->verbs coref-counts))
     seed-verbs (util/most-frequent-n n (coref-count->verbs coref-counts))]
    (loop
      [narratives []
       seed-verbs (util/most-frequent-n n verbs)]
      (println "Iterating...")
      (cond
        (empty? seed-verbs)
        narratives
        :else
        (let
          [next-narrative 
           (extract-narrative
             coref-counts
             type-counts
             lambda
             beta
             verbs
             size
             (first seed-verbs))]
          (recur (conj narratives next-narrative)
                 (seq (difference
                        (set seed-verbs)
                        (set (flatten (map narrative->verbs next-narrative)))))))))
    [coref-counts type-counts verbs seed-verbs]
    ))
