(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :copy-hash-table
               :curry
               :ensure-boolean
               :ensure-gethash
               :ensure-list
               :hash-table-keys
               :map-product
               :mkstr
               :once-only
               :rcurry
               :set-equal
               :with-gensyms
               :with-output-to-file

               )
  :package "SCULLY.QUICKUTILS")
