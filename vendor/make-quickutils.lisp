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
               :extremum
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
               :yes-no

               )
  :package "SCULLY.QUICKUTILS")
