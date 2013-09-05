(ns nlp.narrative.chains.preprocessing
  "Preprocessing required for Nathanael Chamber's 2011 dissertation.
   These functions primarily support the core.clj and cloze.clj implementations."
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zx]
            [clojure.math.combinatorics :as combo]
            [nlp.narrative.chains.util :as util]))

(defn group-by-merge-meta
  "A group-by hack that merges metadata used in coreference
   in order to propogate that data to the final data structure."
  [f coll] 
  (persistent!
   (reduce
    (fn [ret x]
      (let [k (f x)
            k-meta (meta k)]
        (assoc! ret k (conj (get ret k []) x))))
    (transient {}) coll)))

;;
;; Coreference Utility Functions
;;

(defn zxml->coref-nodes 
  "Takes a zippered version of the xml outputted by the stanford parser
   and returns the coreference nodes."
  [zipped-xml]
  (:content (first (zx/xml-> zipped-xml :document :coreference zip/node))))

(defn get-lemma
  "Returns the lemma of the specified word.
   zipped-xml: The xml-zip of the entire document.
   sentence-idx: The index of the sentence in question.
   word-idx: The index of the word in question.

   Returns a string representing the lemmanized version of the word."
  [zipped-xml sentence-idx word-idx]
  (zx/xml1-> zipped-xml :document :sentences :sentence [(zx/attr= :id (str sentence-idx))]
             :tokens :token [(zx/attr= :id (str word-idx))] :lemma zx/text))

(defn is-person?
  "Determines if the NER tag is a person.
   zipped-xml: The xml-zip of the entire document.
   sentence-idx: The index of the sentence in question.
   word-idx: The index of the word in question.

   Returns a boolean determining if the token represents a person."
  [zipped-xml sentence-idx word-idx]
  (if (= "PERSON"
         (zx/xml1-> zipped-xml :document :sentences :sentence [(zx/attr= :id (str sentence-idx))]
                    :tokens :token [(zx/attr= :id (str word-idx))] :NER zx/text))
    true
    false))

(defn get-ner
  [zipped-xml sentence-idx word-idx]
  (zx/xml1-> zipped-xml :document :sentences :sentence [(zx/attr= :id (str sentence-idx))]
                    :tokens :token [(zx/attr= :id (str word-idx))] :NER zx/text))

(defn get-pos
  [zipped-xml sentence-idx word-idx]
  (zx/xml1-> zipped-xml :document :sentences :sentence [(zx/attr= :id (str sentence-idx))]
                    :tokens :token [(zx/attr= :id (str word-idx))] :NER zx/text))

(defn coref->headword 
  "Determines the headword for a provided coreference."
  [zxml coref]
  (let
    [representative (first (filter #(:representative %) coref))
     sidx (:sentence-idx representative)
     widx (:head-idx representative)]
    (with-meta {:word (get-lemma zxml sidx widx)}
               {:ner (get-ner zxml sidx widx)
                :pos (get-pos zxml sidx widx)})
    (cond
      (is-person? zxml sidx widx)
      "PERSON"
      :else
      (get-lemma zxml sidx widx))))

(defn mention->hash
  "Takes an xml node representing a mention from the Stanford Parser and converts it to
   a hash more useful to us.
   mention-node: XML node from the input file representing a mention.
   Returns: A hash representing the mention."
  [mention-node]
  (let [mention (:content mention-node)
        representative (if (= (:attrs mention-node) {:representative "true"}) true false)
        sentence (read-string (first (:content (first (filter #(= (:tag %) :sentence) mention)))))
        start (read-string (first (:content (first (filter #(= (:tag %) :start) mention)))))
        end (read-string (first (:content (first (filter #(= (:tag %) :end) mention)))))
        head (read-string (first (:content (first (filter #(= (:tag %) :head) mention)))))]
    {:sentence-idx sentence, :start-idx start, :end-idx end, :head-idx head :representative representative}))

(defn zxml->sentence-nodes [zipped-xml]
  "Takes a zippered version of the xml outputted by the stanford parser
   and returns the coreference nodes."
  (:content (first (zx/xml-> zipped-xml :document :sentences zip/node))))

(defn process-coreference-node  [coref]
  "Takes an xml node representing the inner <coreferences> from the Stanford Parser
   and processes it into a sequence of hashes representing the coreferences."
  (map mention->hash (:content coref)))

(defn process-coreferences [corefs]
  "Takes the :content of an xml node representing the outer <coreferences> from the
   Stanford Parser and processes the entire collection into a sequences of sequences
   or coreferences."
  (map process-coreference-node corefs))

;;
;; Dependency Utility Functions
;;

(defn get-dependencies [sentence-node]
  "Helper that takes a node representing a sentence and returns a
   collection of all dep nodes within. Ensures that each reference is only returned once."
  (seq (set (apply concat (map :content (filter #(= (:tag %) :dependencies) (:content sentence-node)))))))

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

(defn get-zxml [file] (zip/xml-zip (xml/parse file)))

(defn extract-verb-tuples 
  "Takes a file name and returns a list of tuples in the form:
   {:verb word :dependency :nsubj :coreference (...)}
   for tuples of type :nsubj and :pobj for narrative event chains."
  [zipped-xml]
  (let [coreferences (process-coreferences (zxml->coref-nodes zipped-xml))
        dependencies (apply concat (map sentence->hash (zxml->sentence-nodes zipped-xml)))]
    (->>
      (merge-corefs-onto-dependencies coreferences dependencies)
      (map dep-node->tuple))))

(defn create-coref-counts
  "Converts the output of extract-verb-tuples to a map of
   #{{:verb v1 :dependency dep1} {:verb v2 :dependency dep2}} => count
   where count is the number of times these verbs/dependency pairs were observed
   coreferring."
  [tuples]
  (->>
    (filter (comp not nil? :coreference) tuples)
    (group-by :coreference)
    (vals)
    (map #(map (fn [x] (dissoc x :coreference)) %))
    (map #(combo/combinations % 2))
    (apply concat)
    (map set)
    (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {}))) 

(defn collect-meta
  [coll]
  (let
    []
    (loop
      [pos []
       ner []
       coll (map #(comp meta :coreference) coll)]
      (cond (empty? coll)
      {:ner ner :pos pos}
      :else
      (recur
        (conj pos (:pos (first coll)))
        (conj ner (:ner (first coll)))
        (rest coll))))))

(defn create-type-counts
  "Converts the output of extract-verb-tuples to a map of
   [#{{:verb v1 :dependency dep1} {:verb v2 :dependency dep2}} headword] => count"
  [zxml tuples]
  (let
    [headword-to-vs
     (->>
       (filter (comp not nil? :coreference) tuples)
       (map #(assoc % :coreference (coref->headword zxml (:coreference %))))
       (group-by (comp :word :coreference)))
     headword-to-meta (util/fmap collect-meta headword-to-vs)]
    (->>
      (util/fmap #(map (fn [x] (dissoc x :coreference)) %))
      (util/fmap #(combo/combinations % 2))
      (util/fmap #(map set %))
      (util/fkmap (fn [k v] (map #(vec [% k]) v)))
      vals
      (apply concat)
      frequencies
      (map #(vec [first % (with-meta (second %) (get headword-to-meta (second %)))])))))
