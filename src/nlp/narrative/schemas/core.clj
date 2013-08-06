(ns nlp.narrative.event.schemas.core
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
  (:require [nlp.narrative.event.chains.core :as chains]))

(def dv [:pobj :nsubj])

(defn narsim
  "Determines a similarity score for the verb v in the specified
   narrative by considering all slots at once. Base score beta
   is used to balance the decision between adding to the schema
   and splitting to a new chain.

   Verbs are added to the schema as follows:
   max narsim(N, vj)"
  [beta narrative v]
  (reduce + 
          (for [d dv] (max beta (apply max
                                       (map #(chains/chainsim'
                                               verb-tuples
                                               lambda
                                               %
                                               {:verb v :dependency d})
                                            narrative))))))
