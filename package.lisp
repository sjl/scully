(defpackage :scully.gdl
  (:use
    :cl
    :losh
    :iterate
    :temperance
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
    :rule-head=)
  (:import-from :trivia
    :defun-ematch
    :defun-match))


(defpackage :scully.graphviz
  (:use
    :cl
    :losh
    :iterate
    :trivia
    :trivialib.bdd
    :scully.quickutils)
  (:shadowing-import-from :losh
    :<>))

(defpackage :scully.zdd
  (:use
    :cl
    :losh
    :iterate
    :hamt
    :trivia
    :trivialib.bdd
    :scully.quickutils)
  (:export
    :node
    :sink
    :content
    :with-zdd
    :zdd-node
    :zdd-empty-p
    :zdd-unit-p
    :zdd-count
    :zdd-size
    :zdd-random-member
    :zdd-set
    :zdd-union
    :zdd-intersection
    :zdd-join
    :zdd-meet
    :zdd-family
    :zdd-keep-supersets-of
    :zdd-remove-supersets-of
    :zdd-keep-avoiders-of
    :zdd-match)
  (:shadowing-import-from :hamt
    :hash-set)
  (:shadowing-import-from :losh
    :<>))

(defpackage :scully.rule-trees
  (:use
    :cl
    :losh
    :iterate
    :named-readtables
    :scully.gdl
    :scully.quickutils)
  (:export))

(defpackage :scully.terms
  (:use
    :cl
    :losh
    :iterate
    :named-readtables
    :scully.gdl
    :scully.quickutils)
  (:export
    :integerize-rules
    :stratify-layer)
  (:import-from :trivia
    :match))


(defpackage :scully.reasoners.prolog
  (:use
    :cl
    :losh
    :iterate
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

(defpackage :scully.reasoners.zdd
  (:use
    :cl
    :losh
    :trivia
    :iterate
    :named-readtables
    :scully.zdd
    :scully.quickutils)
  (:shadowing-import-from :losh
    :<>))


(defpackage :scully.grounders.prolog
  (:use
    :cl
    :losh
    :iterate
    :optima
    :temperance
    :scully.quickutils)
  (:export
    ))

(defpackage :scully.grounders.fluxplayer
  (:use
    :cl
    :losh
    :iterate
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
    :scully.quickutils
    :scully.reasoners.prolog)
  (:export
    ))

(defpackage :scully.players.random-ii
  (:use
    :cl
    :losh
    :iterate
    :scully.quickutils
    :scully.reasoners.prolog)
  (:export
    ))
