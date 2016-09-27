(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :ensure-gethash
               :ensure-list
               :map-product
               :once-only
               :rcurry
               :set-equal
               :with-gensyms

               )
  :package "SCULLY.QUICKUTILS")
