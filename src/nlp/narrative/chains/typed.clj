(ns nlp.narrative.chains.typed
  "This namespace contains the typed extensions to the
   narrative chains implementation. In this case a type
   is simply a constraint on the arguments used in a chain."
  (:use [nlp.narrative.chains.core]))

(defn get-freq
  "This is a problem that is yet to be solved, needs to be preprocessed."
  [ta tb a]
  1)

(defn get-args
  "This is a problem that is yet to be solved, needs to be preprocessed."
  [])

(defn sim
  "Determines the similarity of two verb tuples in the context of
   a specific argument type specified by a. Lambda is a constant
   weighting factor. (Chambers found 0.8 to be optimal on his training
   set.)"
  [verb-tuples lambda a tuple-a tuple-b]
  (+
   (pmi verb-tuples tuple-a tuple-b)
   (*
    lambda
    (logarithm
      (get-freq tuple-a tuple-b a)))))

(defn score
  "Determines an argument a (type) score with the specified chain."
  [verb-tuples lambda chain a]
  (for [c chain] (for [d (util/after chain c)]
                   (sim verb-tuples lambda a c d))))

(defn chainsim'
  "Determines chain similarity for the tuple while taking arguments
   into consideration."
  [verb-tuples lambda chain tuple]
  (apply max (map #(+
             (score verb-tuples lambda chain %)
             (reduce + (map (partial sim verb-tuples lambda % tuple) chain)) )
            (get-args))))
