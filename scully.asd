(asdf:defsystem :scully
  :name "scully"
  :description "A General Game Player for incomplete-information games"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:iterate
               :losh
               :cl-digraph
               :cl-digraph.dot
               :temperance
               :hunchentoot
               :smug
               :named-readtables
               :fare-quasiquote
               :fare-quasiquote-readtable
               :cl-dot
               :cl-algebraic-data-type
               :cl-arrows
               :cl-ggp
               :cl-hamt
               :trivia
               :trivia.quasiquote
               :trivialib.bdd)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "gdl")
                             (:file "terms")
                             (:file "rule-trees")
                             (:file "zdd")
                             (:file "graphviz")
                             (:module "reasoners" :serial t
                              :components ((:file "prolog")))
                             (:module "grounders" :serial t
                              :components ((:file "prolog")
                                           (:file "fluxplayer")))
                             (:module "players" :serial t
                              :components ((:file "random")
                                           (:file "random-ii")))))))

