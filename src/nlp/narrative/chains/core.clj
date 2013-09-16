(ns nlp.narrative.chains.core
  "Core of the Narrative Chain prediction model of Nathanael Chamber's 2011 dissertation.

   Input should be preprocessed by the Stanford CoreNLP Library, see
   https://github.com/codyrioux/narrative-event-chains/wiki/Input for specifics.

   Datas tructures:

   verb-tuples:
   verb-tuples are of the following form where :dependency can be :nsubj or :pobj
   coreference is a structure representing all of the representations of the same entity
   the primary concern that if (:coreference obj1) (:coreference obj2) are equal then
   the two tuples represent the same entity.
   {:verb word :dependency :nsubj :coreference (...)}
   
   chain:
   A chain should be represented by...
   A set of verb-tuples represented as they are above.
   A set of relations as metadata [t1, t2] that indicates t1 occurs strictly before t2.
  
   [tuple0 tuple1 tuple2]"
  (:use [clojure.set])
  (:require [nlp.narrative.chains.util :as util]
            [nlp.narrative.chains.stats :as stats]))

(defn chainsim
  "Calculates the sum of PMI (sim) with each element of the chain and the specified tuple."
  [coref-counts chain tuple]
  (reduce + (map (partial stats/pmi coref-counts tuple) chain)))
