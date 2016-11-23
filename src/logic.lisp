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

(make-rule-forest *rules*)




;;;; PLAN
;;;
;;; 1. Receive GDL from server
;;; 2. Ground it
;;; 3. Integerize the ground GDL
;;; 4. Find initial state
;;; 5. Build rule trees for integerized rules
;;; 6. ...
