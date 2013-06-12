# narrative-event-chains

A Clojure library designed to implement the software components in Nathanael Chambers 2011 dissertation.

## Usage

First and foremost your data must be processed using the Stanford CoreNLP library. The easiest
way to do so is to download the stanford corenlp package from their website and run the following
snippet:

```bash
java -cp stanford-corenlp-1.3.4.jar:stanford-corenlp-1.3.4-models.jar:xom.jar:joda-time.jar:jollyday.jar \
-Xmx3g edu.stanford.nlp.pipeline.StanfordCoreNLP -annotators tokenize,ssplit,pos,lemma,ner,parse,dcoref -file input.txt
```

Then you can run experiments at the clojure repl using:

```clojure
(use 'nlp.narrative.chains.cloze :reload-all)
(run-cloze-experiments [file1, file2, file3])
```

## License

Copyright © 2013 Cody Rioux

Distributed under the Eclipse Public License, the same as Clojure.
