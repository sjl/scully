(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :with-gensyms
               :once-only

               )
  :package "SCULLY.QUICKUTILS")
