(in-package :scully.logic)


(defparameter *rules*
  (scully.gdl::read-gdl "gdl/tictactoe-grounded.gdl"))


(defun make-stratum-rule-trees (stratum)
  (-<> stratum
    (group-by #'car <>)
    hash-table-values
    (mapcar #'scully.rule-trees::make-rule-tree <>)))


; (setf *print-length* 10000)
(destructuring-bind (term->number number->term rule-layers)
    (scully.terms::integerize-rules *rules*)
  ; (let ((*print-length* 1000))
  ;   (print-hash-table number->term))
  ; (print-hash-table rule-layers)
  (flet ((draw (rt)
           (scully.graphviz::draw-rule-tree
             rt :label-fn (lambda (n)
                            (gethash n number->term)))
           (break)))
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
