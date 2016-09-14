(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :once-only
               :with-gensyms

               )
  :package "SCULLY.QUICKUTILS")
