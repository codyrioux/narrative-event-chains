(ns nlp.narrative.chains.util
  "Utilities required for Nathanael Chamber's 2011 dissertation.
  These functions primarily support the core.clj and cloze.clj implementations."
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zx]))

(defn get-zipped-xml [filename]
  "Reads an xml file from disk and returns a zipper representing it."
  (zip/xml-zip (xml/parse (str "data/" filename))))

;;
;; Coreference Utility Functions
;;

(defn get-coreference-nodes [zipped-xml]
  "Takes a zippered version of the xml outputted by the stanford parser
  and returns the coreference nodes."
   (:content (first (zx/xml-> zipped-xml :document :coreference zip/node))))

(defn get-protagonist-old [coreference-nodes]
  "Takes a collection of coreference nodes and returns the one
  with the most children nodes. Otherwise the most mentioned entity.
  The :content of protagonist will contain a series of mentions, the content of which
  contains a vector containing: sentence, start, end, and head. The index of end is
  one word after the mention.
  
  These properties should be able to be used to explore the sentence structure
  to find the words."
  (reduce #(if (> (count (:content %1)) (count (:content %2))) %1 %2) coreference-nodes))

(defn get-protagonist [verb-tuples]
  "Takes a collection of tuples in the form used in this file, and returns the one with
  the most frequently occuring coreference."
  (let [counted-verb-tuples (group-by #(count (:coreference %)) (filter #(not (nil? %)) verb-tuples))]
    (counted-verb-tuples (apply max (keys counted-verb-tuples)))))

(defn mention-node-to-hash [mention-node]
  "Takes an xml node representing a mention from the Stanford Parser and converts it to
  a hash more useful to us."
  (let [mention (:content mention-node)
        sentence (read-string (first (:content (first (filter #(= (:tag %) :sentence) mention)))))
        start (read-string (first (:content (first (filter #(= (:tag %) :start) mention)))))
        end (read-string (first (:content (first (filter #(= (:tag %) :end) mention)))))
        head (read-string (first (:content (first (filter #(= (:tag %) :head) mention)))))]
    { :sentence-idx sentence, :start-idx start, :end-idx end, :head-idx head }))

(defn process-coreference-node [coref]
  "Takes an xml node representing the inner <coreferences> from the Stanford Parser
  and processes it into a sequence of hashes representing the coreferences." 
  (map mention-node-to-hash (:content coref)))

(defn process-coreferences [corefs]
  "Takes the :content of an xml node representing the outer <coreferences> from the
  Stanford Parser and processes the entire collection into a sequences of sequences
  or coreferences."
  (map process-coreference-node corefs))

;;
;; Dependency Utility Functions

(defn get-sentence-nodes [zipped-xml]
  "Takes a zippered version of the xml outputted by the stanford parser
  and returns the coreference nodes."
   (:content (first (zx/xml-> zipped-xml :document :sentences zip/node))))

(defn get-sentence-idx [sentence-node]
  "Given an xml node representing a sentence, this will return that sentences
  index."
  (read-string (:id (:attrs sentence-node))))

(defn get-dependencies [sentence-node]
  "Helper that takes a node representing a sentence and returns a
  collection of all dep nodes within. Ensures that each reference is only returned once."
  (seq (set (concat
    (:content (first (filter #(= (:tag %) :basic-dependencies) (:content sentence-node))))
    (:content (first (filter #(= (:tag %) :collapsed-dependencies) (:content sentence-node))))
    (:content (first (filter #(= (:tag %) :collapsed-ccprocessed-dependencies) (:content sentence-node))))))))

(defn get-verb-dependencies [sentence-node]
  "Helper that takes a node representing a sentence and returns a
  collection of all verb (:nsubj :pobj) dependencies within. This is an optimization to
  reduce the data we are working with early."
  (filter #(or (= (:type (:attrs %1)) "pobj") (= (:type (:attrs %1)) "nsubj")) (get-dependencies sentence-node)))

(defn dep-node-to-hash [sentence-idx dep-node]
  (let [deptype (:type (:attrs dep-node))
        governor-node (first (filter #(= (:tag %) :governor) (:content dep-node)))
        dependent-node (first (filter #(= (:tag %) :dependent) (:content dep-node)))
        governor (first (:content governor-node))
        governor-idx (read-string (:idx (:attrs governor-node)))
        dependent (first (:content dependent-node))
        dependent-idx (read-string (:idx (:attrs dependent-node)))]
    {:type deptype,
     :sentence-idx sentence-idx,
     :governor governor,
     :governor-idx governor-idx,
     :dependent dependent,
     :dependent-idx dependent-idx,
     :coreference nil}))

(defn process-dependencies [sentence-node]
  "Takes a sentence node and returns a collection of hashes representing
  the dep nodes contained therein."
  (let [sentence-idx (get-sentence-idx sentence-node)
        dnth (partial dep-node-to-hash sentence-idx)
        dependencies (get-verb-dependencies sentence-node)]
    (map dnth dependencies)))

(defn sentence-nodes-to-verb-dep-hashes [sentence-nodes]
 "Not completely sure..." 
    (apply concat (map process-dependencies sentence-nodes)))

(defn refers-to? [dep coref]
  "Takes a dep hash and a collection of hashes representing a single coreference group.
  Returns true if the dep refers to that group and nil otherwise."
  (some #(and 
           (= (:sentence-idx %) (:sentence-idx dep))
           (>= (:dependent-idx dep) (:start-idx %))
           (< (:dependent-idx dep) (:end-idx %)))
        coref))

(defn get-coreference [corefs dep]
  "Takes a hash representing a dependency and a collection of coreference groups
  Returns the coref collection to which it refers. nil is returned if none.
  This function has a bias for the first in the list, in the event that multiple match."
  (let [refcheck (partial refers-to? dep)]
     (first (filter refcheck corefs))))

(defn merge-corefs-onto-dependencies [corefs dep-collection]
  "This function takes a collection of coreference hashes and resolves each dependencies
  coreferring argument, placing the hash in its coreference property."
  (let [find-my-coreference (partial get-coreference corefs)]
    (map #(assoc % :coreference (find-my-coreference %)) dep-collection)))

(defn dep-node-to-tuple [dep-node]
  "This function takes the dep nodes, preferably with corefs merged onto them,
  though it is not necessary, and turns them into a tuple with the format:
  {:verb word :dependency :nsubj :coreference (...)}
  The purpose of this function is to convert them into a form workable for
  narrative event chains."
  {:verb (:governor dep-node) :dependency (keyword (:type dep-node)) :coreference (:coreference dep-node)})

(defn extract-verb-tuples [file-name]
  "Takes a file name and returns a list of tuples in the form:
  {:verb word :dependency :nsubj :coreference (...)}
  for tuples of type :nsubj and :pobj for narrative event chains."
  (let [zipped-xml (get-zipped-xml file-name)
        coreferences (process-coreferences (get-coreference-nodes zipped-xml))
        dependencies (sentence-nodes-to-verb-dep-hashes (get-sentence-nodes zipped-xml))
        ]
    (->>
      (merge-corefs-onto-dependencies coreferences dependencies)
      (map dep-node-to-tuple))))

(defn extract-verb-tuples-for-files [file-list]
  (reduce concat (map extract-verb-tuples file-list)))
