(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :ensure-gethash
               :ensure-list
               :map-product
               :mkstr
               :once-only
               :rcurry
               :set-equal
               :with-gensyms
               :with-output-to-file

               )
  :package "SCULLY.QUICKUTILS")
