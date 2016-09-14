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
    #:legal-moves-for))

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
