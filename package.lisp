(defpackage :scully.gdl
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :temperance
    :scully.quickutils)
  (:export
    :read-gdl
    :load-rules
    :redump-gdl))


(defpackage :scully.dag
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :trivia
    :scully.quickutils)
  (:export))

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
  (:export))

(defpackage :scully.terms
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :trivia
    :named-readtables
    :scully.quickutils)
  (:export))

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
