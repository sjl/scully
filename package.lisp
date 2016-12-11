(defpackage :scully.gdl
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :temperance
    :trivia
    :named-readtables
    :scully.quickutils)
  (:export
    :gensym-ggp
    :read-gdl
    :load-rules
    :dump-gdl
    :normalize-rules
    :bare-term
    :negationp
    :term-predicate
    :term<
    :rule-head
    :rule-body
    :rule-predicate
    :rule-head=))


(defpackage :scully.graphviz
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :trivia
    :trivialib.bdd
    :scully.quickutils))

(defpackage :scully.zdd
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :hamt
    :trivia
    :trivialib.bdd
    :scully.quickutils)
  (:export)
  (:shadowing-import-from :hamt
    :hash-set))

(defpackage :scully.rule-trees
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :trivia
    :named-readtables
    :scully.gdl
    :scully.quickutils)
  (:export))

(defpackage :scully.terms
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :trivia
    :named-readtables
    :scully.gdl
    :scully.quickutils)
  (:export
    :integerize-rules
    :stratify-layer))

(defpackage :scully.logic
  (:use
    :cl
    :losh
    :iterate
    :trivia
    :cl-arrows
    :scully.quickutils))


(defpackage :scully.reasoners.prolog
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :temperance
    :scully.quickutils)
  (:export
    :prolog-reasoner
    :make-prolog-reasoner
    :load-rules
    :initial-state
    :terminalp
    :next-state
    :legal-moves-for
    :percepts-for
    :roles
    ))

(defpackage :scully.grounders.prolog
  (:use
    :cl
    :losh
    :iterate
    :optima
    :cl-arrows
    :temperance
    :scully.quickutils)
  (:export
    ))

(defpackage :scully.grounders.fluxplayer
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :smug
    :scully.quickutils)
  (:export
    :ground-gdl-file
    :ground-gdl-string))

(defpackage :scully.players.random
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :scully.quickutils
    :scully.reasoners.prolog)
  (:export
    ))

(defpackage :scully.players.random-ii
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :scully.quickutils
    :scully.reasoners.prolog)
  (:export
    ))
