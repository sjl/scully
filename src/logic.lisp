(in-package :scully.logic)


(defparameter *rules*
  ; (scully.gdl::read-gdl "gdl/tictactoe-grounded.gdl")
  ; (scully.gdl::read-gdl "gdl/hanoi-grounded.gdl")
  ; (scully.gdl::read-gdl "gdl/8puzzle-grounded.gdl")
  (scully.gdl::read-gdl "gdl/roshambo2-grounded.gdl")
  )


(defun make-stratum-rule-trees (stratum)
  (-<> stratum
    (group-by #'car <>)
    hash-table-values
    (mapcar #'scully.rule-trees::make-rule-tree <>)))


(setf *print-length* 10
      *print-depth* 5)

(defun make-rule-forest (rules)
  (destructuring-bind (term->number number->term rule-layers)
    (scully.terms::integerize-rules rules)
  (flet ((draw (rt)
           (scully.graphviz::draw-rule-tree
             rt :label-fn (lambda (n)
                            (gethash n number->term)))
           (break)
           ))
    (print-hash-table rule-layers)
    (-<> rule-layers
      (gethash :possible <>)
      scully.terms::stratify-layer
      (nth 0 <>)
      (make-stratum-rule-trees <>)
      (map nil #'draw <>)
      ; (map nil #'pr <>)
      ; (mapcar (curry #'group-by #'car) <>)
      ; (map nil #'print-hash-table <>)
      ; (hash-table-values <>)
      ; (map nil (lambda (rule)
      ;            (-<> rule
      ;              (scully.rule-trees::make-rule-tree <>)
      ;              )
      ;            (break))
      ;      <>)
      )))
  )

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
