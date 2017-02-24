(in-package :scully.gdl)
(in-readtable :fare-quasiquote)


;;;; Utils --------------------------------------------------------------------
(defvar *ggp-gensym-counter* 0)
(defun gensym-ggp ()
  "Return a unique symbol in the `ggp-rules` package."
  (values (intern (mkstr 'rule- (incf *ggp-gensym-counter*))
                  (find-package :ggp-rules))))


;;;; Files --------------------------------------------------------------------
(defun read-gdl (filename)
  "Read GDL from the given file"
  (let ((*package* (find-package :ggp-rules)))
    (with-open-file (stream filename)
      (loop
        :with done = (gensym)
        :for form = (read stream nil done)
        :while (not (eq form done))
        :collect form))))

(defun dump-gdl (rules &optional stream)
  (let ((*package* (find-package :ggp-rules)))
    (format stream "~(~{~S~%~}~)" rules)))


;;;; Temperance ---------------------------------------------------------------
(defun load-rules (database rules)
  (push-logic-frame-with database
    (mapc (lambda (rule)
            (if (and (consp rule)
                     (eq (car rule) 'ggp-rules::<=))
              (apply #'invoke-rule database (cdr rule))
              (invoke-fact database rule)))
          rules)))


;;;; Normalization ------------------------------------------------------------
;;; Normalization takes a set of clauses from raw GDL format and turns them into
;;; friendlier Lispy clauses of the form:
;;;
;;;     (head . body)
;;;
;;; * (<= head . body) becomes (head . body)
;;; * (fact) becomes ((fact)), i.e. ((fact) . nil)
;;; * Nullary predicates like terminal have their parens added back.
;;;
;;; So something like (<= terminal (true foo) (not bar)) would become:
;;;
;;;   ((terminal)
;;;    (true foo)
;;;    (not (bar)))
(defun-match normalize-term (term)
  (`(ggp-rules::not ,body) `(ggp-rules::not ,(normalize-term body)))
  (`(,_ ,@_) term)
  (sym `(,sym)))

(defun-match normalize-rule (rule)
  (`(ggp-rules::<= ,head ,@body)
   `(,(normalize-term head) ,@(mapcar #'normalize-term body)))
  (fact `(,(normalize-term fact))))

(defun normalize-rules (gdl-rules)
  (mapcar #'normalize-rule gdl-rules))


;;;; Rule Data Access ---------------------------------------------------------
(defun-match bare-term (term)
  (`(ggp-rules::not ,x) x)
  (_ term))

(defun-match negationp (term)
  (`(ggp-rules::not ,_) t)
  (_ nil))

(defun-ematch term-predicate (term)
  (`(ggp-rules::not (,predicate ,@_)) predicate)
  (`(,predicate ,@_) predicate))


(defun-ematch rule-head (rule)
  (`(,head ,@_) head))

(defun-ematch rule-body (rule)
  (`(,_ ,@body) body))

(defun rule-predicate (rule)
  (term-predicate (rule-head rule)))

(defun rule-head= (rule term &optional (predicate #'=))
  (funcall predicate (rule-head rule) term))


