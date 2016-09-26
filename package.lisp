(defpackage #:scully.gdl
  (:use
    #:cl
    #:losh
    #:iterate
    #:cl-arrows
    #:temperance
    #:scully.quickutils)
  (:export
    #:read-gdl
    #:load-rules
    ))

(defpackage #:scully.reasoners.prolog
  (:use
    #:cl
    #:losh
    #:iterate
    #:cl-arrows
    #:temperance
    #:scully.quickutils)
  (:export
    #:prolog-reasoner
    #:make-prolog-reasoner
    #:load-rules
    #:initial-state
    #:terminalp
    #:next-state
    #:legal-moves-for
    #:percepts-for
    #:roles
    ))

(defpackage #:scully.grounders.prolog
  (:use
    #:cl
    #:losh
    #:iterate
    #:trivia
    #:cl-arrows
    #:temperance
    #:scully.quickutils)
  (:export
    ))

(defpackage #:scully.players.random
  (:use
    #:cl
    #:losh
    #:iterate
    #:cl-arrows
    #:scully.quickutils
    #:scully.reasoners.prolog)
  (:export
    ))

(defpackage #:scully.players.random-ii
  (:use
    #:cl
    #:losh
    #:iterate
    #:cl-arrows
    #:scully.quickutils
    #:scully.reasoners.prolog)
  (:export
    ))
