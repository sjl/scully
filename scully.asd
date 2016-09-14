(asdf:defsystem #:scully
  :name "scully"
  :description "A General Game Player for incomplete-information games"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (#:iterate
               #:losh
               #:temperance
               #:hunchentoot
               #:cl-arrows
               #:cl-ggp)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:module "reasoners" :serial t
                              :components ((:file "prolog")))
                             (:module "players" :serial t
                              :components ((:file "random")))))))

