# HC SVNT DRACONES
A warning to all those who would proceed. This library is an atrocity, being my first serious Clojure project and Clojure being my first lisp this thing needs a major refactoring. In all likelihood it wouldn't even be up here were it not the only implementation of narrative event chains & schemas online. If you actually intend to use this library from Clojure or Java please message me, it'll motivate me to fix this thing up.

You've been warned...

# narrative-event-chains

A Clojure library designed to implement the software components in Nathanael Chambers 2011 dissertation.

This code is NOT currently ready for production usage. Not by a long shot.

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

To actually utilize the library for your own purposes beyond running cloze experiments is currently a bit complicated.
This was written when I was quite new at Clojure, as is evident from the code. It will require substantial cleanup
before it is considered ready to be published as a library.

## License

Copyright © 2013 Cody Rioux

Distributed under the Eclipse Public License, the same as Clojure.
