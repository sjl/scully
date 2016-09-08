(asdf:defsystem #:scully
  :name "scully"
  :description "A General Game Player for incomplete-information games"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (#:iterate
               #:losh
               #:cl-arrows
               #:cl-ggp
               #:cl-conspack
               #:usocket)

  :serial t
  :components ((:module "vendor"
                :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src"
                :serial t
                :components ((:module "brains"
                              :serial nil
                              :components ((:file "random")))
                             (:file "player")))))

