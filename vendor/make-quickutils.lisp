(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :once-only
               :with-gensyms
               :map-product
               :curry
               :rcurry
               :set-equal

               )
  :package "SCULLY.QUICKUTILS")
