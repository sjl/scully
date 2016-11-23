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
               :flatten-once
               :hash-table-keys
               :hash-table-values
               :map-product
               :mkstr
               :once-only
               :rcurry
               :set-equal
               :with-gensyms
               :with-output-to-file
               :write-string-into-file

               )
  :package "SCULLY.QUICKUTILS")
