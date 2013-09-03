(ns nlp.narrative.schemas.experiment
  (:require
    [nlp.narrative.schemas.core :as schemas]
    [nlp.narrative.chains.preprocessing :as preprocessing]))

(defn experiment
  [target-dir]
  (let
    [files (map #(.getPath %)  (rest  (file-seq  (clojure.java.io/file doc-path))))
     cr 1
     tc 1]

    )
  )
