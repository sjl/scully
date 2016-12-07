(in-package :scully.logic)


(defparameter *rules*
  (scully.gdl::read-gdl "gdl/tictactoe-grounded.gdl")
  ; (scully.gdl::read-gdl "gdl/hanoi-grounded.gdl")
  ; (scully.gdl::read-gdl "gdl/8puzzle-grounded.gdl")
  ; (scully.gdl::read-gdl "gdl/roshambo2-grounded.gdl")
  )


(defun make-stratum-rule-trees (stratum)
  (-<> stratum
    (group-by #'car <>)
    hash-table-values
    (mapcar #'scully.rule-trees::make-rule-tree <>)))

(defun make-rule-forests (rules)
  "Turn a set of grounded GDL rules into rule forests and mapping tables.

  A rule forest is a collection of individual rule trees in a single layer,
  stratified as necessary:

    POSSIBLE: (STRATUM-1 STRATUM-2 ...)
    HAPPENS:  (STRATUM-1 STRATUM-2 ...)
                 ||       ||
                 ||       \/
                 ||     (rule-tree-1 rule-tree-2 ...)
                 \/
         (rule-tree-1 rule-tree-2 ...)

  Returns a list of:

  * The :possible layer's rule forest.
  * The :happens layer's rule forest.
  * The term->number hash table.
  * The number->term hash table.

  "
  (destructuring-bind (term->number number->term rule-layers)
      (-> rules
        scully.gdl::normalize-rules
        scully.terms::integerize-rules)
    (flet ((make-forest (layer)
             (-<> rule-layers
               (gethash layer <>)
               scully.terms::stratify-layer
               (mapcar #'make-stratum-rule-trees <>))))
      (list (make-forest :possible)
            (make-forest :happens)
            term->number
            number->term))))

; (make-rule-forest *rules*)


; (defun apply-rule-tree (zdd rule-tree head-bound)
;   "Apply the logical rules in `rule-tree` to the sets in `zdd`.

;   `zdd` is assumed to contain sets of logical axioms.  This function will update
;   each of these sets to add any rule heads derivable from the axioms in the set.

;   "
;   (recursively ((zdd zdd)
;                 (rule-tree rule-tree))
;     (ematch* (zdd rule-tree)
;       ;; If Z = ∅ there are no sets to cons heads onto, bail.
;       (((sink nil) _) zdd)

;       ;; If R = ∅ or {∅} we've bottomed out of the rule tree and there are no
;       ;; heads to cons, we're done.
;       ((_ (sink)) zdd)

;       ;; If we've passed the head boundary on the rule tree side then we're done
;       ;; filtering and just need to cons in all the heads.
;       ((_ (guard (node var _ _)
;                  (>= var head-bound)))
;        (zdd-join zdd rule-tree))

;       ;; If Z = {∅} we might have some heads we need to cons later in the rule
;       ;; tree, so recur down the lo side of it.
;       (((sink t) (node _ _ lo))
;        (recur zdd lo))

;       ;; Otherwise we need to filter.
;       (((node var-z hi-z lo-z) (node var-r hi-r lo-r))
;        (cond
;          ((= var-z var-r) (zdd-node var-z
;                                     (recur hi-z hi-r)
;                                     (recur lo-z lo-r)))
;          ((< var-z var-r) (zdd-node var-z
;                                     (recur hi-z rule-tree)
;                                     (recur lo-z rule-tree)))
;          ((> var-z var-r) (recur zdd lo-r)))))))




;;;; PLAN
;;;
;;; 1. Receive GDL from server
;;; 2. Ground it
;;; 3. Integerize the ground GDL
;;; 4. Find initial state
;;; 5. Build rule trees for integerized rules
;;; 6. ...
