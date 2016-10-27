(in-package :scully.zdd)

;;;; Library ------------------------------------------------------------------
(define-foreign-library cudd
  (:darwin "./build/libcudd.dylib"))

(use-foreign-library cudd)


