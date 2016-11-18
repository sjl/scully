(in-package :scully.terms)
(in-readtable :fare-quasiquote)


;;;; Overview -----------------------------------------------------------------
;;; We start with a set of grounded rules like: ((next bar) x y (true foo)).
;;;
;;; We need to turn map each term to a unique integer, making sure that they end
;;; up ordered how we want them.
;;;
;;; Our desired ordering has a few "layers":
;;;
;;; Base: (true ...)
;;; Does: (does ...)
;;; Possible: (legal ...)
;;;           terminal
;;;           (goal ...)
;;;           anything that depends on only these or lower
;;; Happens: (next ...)
;;;          (sees ...)
;;;           anything that depends on only these or lower
;;;
;;; Within each layer the ordering is arbitrary, EXCEPT that if a rule X that
;;; relies on the negation of a rule Y, then Y must come before X.


;;;; Utils --------------------------------------------------------------------
(defun-match bare-term (term)
  (`(ggp-rules::not ,x) x)
  (x x))

(defun-match negationp (term)
  (`(ggp-rules::not ,_) t)
  (_ nil))


(defun-match normalize-term (term)
  (`(ggp-rules::not ,body) `(ggp-rules::not ,(normalize-term body)))
  (`(,_) term)
  (`(,head ,@body) (cons head (mapcar #'normalize-term body)))
  (sym `(,sym)))

(defun-match normalize-term (term)
  (`(ggp-rules::not ,body) `(ggp-rules::not ,(normalize-term body)))
  (`(,constant) constant)
  (`(,head ,@body) (cons head (mapcar #'normalize-term body)))
  (`,constant constant))

(defun-match normalize-rule (rule)
  (`(ggp-rules::<= ,head ,@body) `(,(normalize-term head)
                                   ,@(mapcar #'normalize-term body)))
  (fact `(,(normalize-term fact))))

(defun normalize-rules (rules)
  (mapcar #'normalize-rule rules))


;;;; Dependency Graph ---------------------------------------------------------
(defun build-dependency-graph (rules &key negations-only)
  (let ((graph (digraph:make-digraph :test #'equal)))
    (labels
        ((mark-dependency (head dep)
           (digraph:insert-vertex graph head)
           (digraph:insert-vertex graph dep)
           (digraph:insert-edge graph head dep))
         (mark-dependencies (head body)
           (iterate (for b :in body)
                    (when (or (negationp b)
                              (not negations-only))
                      (mark-dependency head (bare-term b))))))
      (iterate (for rule :in rules)
               (for (head . body) = (ensure-list rule))
               (mark-dependencies head body)))
    graph))


;;;; Layer Partitioning -------------------------------------------------------
(defun mark (layers layer term)
  (setf (gethash term layers) layer)
  (pushnew term (gethash layer layers) :test #'equal))


(defun extract-simple (predicates layer layers terms)
  (iterate (for term :in terms)
           (if (member (car (ensure-list term)) predicates)
             (mark layers layer term)
             (collect term))))


(defun extract-base (layers terms)
  (let ((terms (extract-simple '(ggp-rules::true
                                 ggp-rules::role)
                               :base layers terms)))
    (iterate (for term :in terms)
             (match term
               (`(ggp-rules::init ,contents)
                (mark layers :base `(ggp-rules::true ,contents))
                (mark layers :base term))
               (_ (collect term))))))

(defun extract-does (layers terms)
  (extract-simple '(ggp-rules::does) :does layers terms))


(defun extract-possible% (layers dependencies terms)
  (labels ((find-dependencies (term)
             (mapcar (rcurry #'gethash layers)
                     (digraph:successors dependencies term)))
           (find-eligible (terms)
             (iterate (for term :in terms)
                      (for deps = (find-dependencies term))
                      (for unmet = (set-difference deps '(:base :possible)))
                      (when (null unmet)
                        (collect term)))))
    (iterate
      (with remaining = terms)
      (for next = (find-eligible remaining))
      (while next)
      (mapcar (curry #'mark layers :possible) next)
      (zapf remaining (set-difference % next :test #'equal))
      (finally (return remaining)))))

(defun extract-possible (layers dependencies terms)
  (-<> terms
    (extract-simple '(ggp-rules::legal
                      ggp-rules::goal
                      ggp-rules::terminal)
                    :possible layers <>)
    (extract-possible% layers dependencies <>)))


(defun extract-early-happens (layers terms)
  (extract-simple '(ggp-rules::sees
                    ggp-rules::next)
                  :happens layers terms))

(defun extract-final-happens (layers terms)
  (mapcar (curry #'mark layers :happens) terms)
  nil)


(defun partition-rules (dependencies rules)
  (let* ((terms (-<> rules
                  flatten-once
                  (mapcar #'bare-term <>)
                  (remove-duplicates <> :test #'equal)))
         (layers (make-hash-table :test #'equal)))
    (-<> terms
      (extract-base layers <>)
      (extract-does layers <>)
      (extract-early-happens layers <>) ; ugh
      (extract-possible layers dependencies <>)
      (extract-final-happens layers <>))
    layers))


;;;; Intra-Layer Ordering -----------------------------------------------------
(defun sort-layer (negation-dependencies terms)
  (let ((layer (digraph:make-digraph :test #'equal)))
    (flet ((add-dependencies (term)
             (iterate
               (for dep :in (digraph:successors negation-dependencies term))
               (when (digraph:contains-vertex-p layer dep)
                 (digraph:insert-edge layer term dep)))))
      (mapc (curry #'digraph:insert-vertex layer) terms)
      (mapc #'add-dependencies terms))
    ;; todo: fix the roots/cycles issue in cl-digraph
    (digraph:topological-sort layer)))

(defun order-predicates (rules)
  (let* ((dependencies (build-dependency-graph rules))
         (negation-dependencies (build-dependency-graph rules :negations-only t))
         (layers (partition-rules dependencies rules)))
    (let ((base (gethash :base layers))
          (does (gethash :does layers))
          (possible (sort-layer negation-dependencies (gethash :possible layers)))
          (happens (sort-layer negation-dependencies (gethash :happens layers))))
      ; (pr :base)
      ; (pr base)
      ; (terpri)
      ; (pr :does)
      ; (pr does)
      ; (terpri)
      ; (pr :possible)
      ; (pr possible)
      ; (terpri)
      ; (pr :happens)
      ; (pr happens)
      ; (terpri)
      (values (append base possible does happens)
              layers))))


;;;; API ----------------------------------------------------------------------
(defun integerize-term (term->number term)
  (match term
    (`(ggp-rules::not ,body)
     `(ggp-rules::not ,(gethash body term->number)))
    (_ (gethash term term->number))))

(defun integerize-rule (term->number rule)
  (mapcar (curry #'integerize-term term->number) rule))

(defun integerize-rules (rules)
  (let ((rules (normalize-rules rules))
        (term->number (make-hash-table :test #'equal))
        (number->term (make-hash-table))
        (rule-layers (make-hash-table)))
    (multiple-value-bind (terms layers)
        (order-predicates rules)
      (iterate (for i :from 0)
               (for term :in terms)
               (setf (gethash i number->term) term
                     (gethash term term->number) i))
      (iterate (for rule :in rules)
               (for head = (first rule))
               (for layer = (gethash head layers))
               (push (integerize-rule term->number rule)
                     (gethash layer rule-layers))))
    (list term->number number->term rule-layers)))


;;;; Scratch ------------------------------------------------------------------

#+no (-<> scully.zdd::*rules*
  (integerize-rules <>)
  ; (never <>)
  ; (map nil #'print-hash-table <>)
  )
