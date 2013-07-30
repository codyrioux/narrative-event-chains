(ns nlp.narrative.chains.preprocessing
  "Preprocessing required for Nathanael Chamber's 2011 dissertation.
   These functions primarily support the core.clj and cloze.clj implementations."
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zx]))
;;
;; Coreference Utility Functions
;;

(defn zxml->coref-nodes 
  "Takes a zippered version of the xml outputted by the stanford parser
   and returns the coreference nodes."
  [zipped-xml]
  (:content (first (zx/xml-> zipped-xml :document :coreference zip/node))))

(defn mention->hash
  "Takes an xml node representing a mention from the Stanford Parser and converts it to
   a hash more useful to us.
   mention-node: XML node from the input file representing a mention.
   Returns: A hash representing the mention."
  [mention-node]
  (let [mention (:content mention-node)
        sentence (read-string (first (:content (first (filter #(= (:tag %) :sentence) mention)))))
        start (read-string (first (:content (first (filter #(= (:tag %) :start) mention)))))
        end (read-string (first (:content (first (filter #(= (:tag %) :end) mention)))))
        head (read-string (first (:content (first (filter #(= (:tag %) :head) mention)))))]
    { :sentence-idx sentence, :start-idx start, :end-idx end, :head-idx head }))

(defn zxml->sentence-nodes [zipped-xml]
  "Takes a zippered version of the xml outputted by the stanford parser
   and returns the coreference nodes."
  (:content (first (zx/xml-> zipped-xml :document :sentences zip/node))))

;;
;; Dependency Utility Functions
;;

(defn- get-dependencies [sentence-node]
  "Helper that takes a node representing a sentence and returns a
   collection of all dep nodes within. Ensures that each reference is only returned once."
  (seq (set (concat
              (:content (first (filter #(= (:tag %) :basic-dependencies) (:content sentence-node))))
              (:content (first (filter #(= (:tag %) :collapsed-dependencies) (:content sentence-node))))
              (:content (first (filter #(= (:tag %) :collapsed-ccprocessed-dependencies) (:content sentence-node))))))))

(defn sentence->deps
  "Helper that takes a node representing a sentence and returns a
   collection of all verb (:nsubj :pobj) dependencies within."
  [sentence-node]
  (filter #(or (= (:type (:attrs %1)) "pobj")
               (= (:type (:attrs %1)) "nsubj"))
          (get-dependencies sentence-node)))

(defn dep->hash [sentence-idx dep-node]
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

(defn sentence->hash [sentence-node]
  "Takes a sentence node and returns a collection of hashes representing
   the dep nodes contained therein."
  (let [sentence-idx (read-string (:id (:attrs sentence-node)))
        dependencies (sentence->deps sentence-node)]
    (map (partial dep->hash sentence-idx) dependencies)))

;;
;; Coref
;;

(defn refers-to?
  "Takes a dep hash and a collection of hashes representing a single coreference group.

   dep: A dependency node as specified in this namespace.
   coref: A cofeference group.
   Returns: true if the dep refers to the specified group, nil otherwise."
  [dep coref]
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
    (map #(assoc % :coreference (get-coreference corefs %)) dep-collection))

;;
;; Processing
;;

(defn- dep-node->tuple 
  "This function takes the dep nodes, preferably with corefs merged onto them,
   though it is not necessary, and turns them into a tuple with the format:

   {:verb word :dependency :nsubj :coreference (...)}
   The purpose of this function is to convert them into a form workable for
   narrative event chains."
  [dep-node]
  {:verb (:governor dep-node) :dependency (keyword (:type dep-node)) :coreference (:coreference dep-node)})

;;
;; Interface Functions
;;

(defn extract-verb-tuples 
  "Takes a file name and returns a list of tuples in the form:
   {:verb word :dependency :nsubj :coreference (...)}
   for tuples of type :nsubj and :pobj for narrative event chains."
  [file-name]
  (let [zipped-xml (zip/xml-zip (xml/parse file-name))
        coreferences (map #(mention->hash (:content %)) (zxml->coref-nodes zipped-xml))
        dependencies (apply concat (map sentence->hash (zxml->sentence-nodes zipped-xml)))]
    (->>
      (merge-corefs-onto-dependencies coreferences dependencies)
      (map dep-node->tuple))))
