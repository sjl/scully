(asdf:defsystem :scully
  :name "scully"
  :description "A General Game Player for incomplete-information games"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:iterate
               :losh
               :temperance
               :hunchentoot
               :optima
               :smug
               :cl-dot
               :cl-algebraic-data-type
               :cl-arrows
               :cl-ggp
               :cffi)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "gdl")
                             (:file "rule-trees")
                             (:file "cudd")
                             (:file "zdd")
                             (:module "reasoners" :serial t
                              :components ((:file "prolog")))
                             (:module "grounders" :serial t
                              :components ((:file "prolog")
                                           (:file "fluxplayer")))
                             (:module "players" :serial t
                              :components ((:file "random")
                                           (:file "random-ii")))))))

