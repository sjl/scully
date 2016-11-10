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
  (`(not ,x) x)
  (x x))

(defun-match negationp (term)
  (`(not ,_) t)
  (_ nil))


(defun-match normalize-term (term)
  (`(not ,body) `(not ,(normalize-term body)))
  (`(,_) term)
  (`(,head ,@body) (cons head (mapcar #'normalize-term body)))
  (sym `(,sym)))

(defun normalize-rule (rule)
  (mapcar #'normalize-term (ensure-list rule)))

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
           (if (member (car term) predicates)
             (mark layers layer term)
             (collect term))))


(defun extract-base (layers terms)
  (extract-simple '(true) :base layers terms))

(defun extract-does (layers terms)
  (extract-simple '(does) :does layers terms))


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
    (extract-simple '(legal goal terminal) :possible layers <>)
    (extract-possible% layers dependencies <>)))


(defun extract-happens (layers terms)
  (mapcar (curry #'mark layers :happens) terms)
  nil)


(defun partition-rules (dependencies rules)
  (let* ((terms (-<> rules
                  flatten-once
                  (mapcar #'bare-term <>)))
         (layers (make-hash-table :test #'equal)))
    (-<> terms
      (extract-base layers <>)
      (extract-does layers <>)
      (extract-possible layers dependencies <>)
      (extract-happens layers <>))
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

(defun order-rules (rules)
  (let* ((rules (normalize-rules rules))
         (dependencies (build-dependency-graph rules))
         (negation-dependencies (build-dependency-graph rules :negations-only t))
         (layers (partition-rules dependencies rules)))
    (let ((base (gethash :base layers))
          (does (gethash :does layers))
          (possible (sort-layer negation-dependencies (gethash :possible layers)))
          (happens (sort-layer negation-dependencies (gethash :happens layers))))
      (pr :base base)
      (pr :does does)
      (pr :possible possible)
      (pr :happens happens)
      (append base possible does happens))))


;;;; Scratch ------------------------------------------------------------------

(order-rules '(
               (foo (true something))
               (bar (true (something))
                    (does x)
                    )
               ))
