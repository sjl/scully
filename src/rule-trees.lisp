(in-package #:scully.rule-trees)


(adt:defdata rule-tree
  (node t rule-tree rule-tree list)
  (leaf list))


(defun rule-head (rule)
  (first rule))

(defun rule-body (rule)
  (rest rule))

(defun rule-first-body (rule)
  (first (rule-body rule)))

(defun rule-empty-p (rule)
  (null (rule-body rule)))


(defun negationp (term)
  (and (consp term) (eql 'not (first term))))

(defun bare-term (term)
  (if (negationp term)
    (second term)
    term))


(defun %term< (t1 t2)
  ;; symbol < number < cons
  (ensure-boolean
    (etypecase t1
      (symbol (etypecase t2
                (symbol (string< (symbol-name t1) (symbol-name t2)))
                (number t)
                (cons t)))
      (number (etypecase t2
                (symbol nil)
                (number (< t1 t2))
                (cons t)))
      (cons (etypecase t2
              (symbol nil)
              (number nil)
              (cons (cond
                      ((term< (car t1) (car t2)) t)
                      ((term< (car t2) (car t1)) nil)
                      (t (term< (cdr t1) (cdr t2))))))))))
(defun term< (t1 t2)
  ;; symbol < number < cons
  (%term< (bare-term t1) (bare-term t2)))


(defun sort-body (rule)
  (destructuring-bind (head . body) rule
    (list* head (sort body #'term<))))

(defun drop-first (rule)
  (destructuring-bind (head . body) rule
    (list* head (rest body))))

(defun find-smallest-body-term (rules)
  (-<> rules
    (mapcar #'rule-first-body <>)
    (sort <> #'term<)
    (first <>)))

(defun partition-rules (rules)
  (let ((element (bare-term (find-smallest-body-term rules))))
    (labels
        ((rule-requires (rule)
           (equal (rule-first-body rule) element))
         (rule-disallows (rule)
           (equal (rule-first-body rule) `(not ,element)))
         (rule-ignores (rule)
           (not (or (rule-requires rule)
                    (rule-disallows rule)))))
      (values element
              (remove-if-not #'rule-disallows rules)
              (remove-if-not #'rule-requires rules)
              (remove-if-not #'rule-ignores rules)))))


(defun make-rule-tree (rules)
  (recursively ((rules (mapcar #'sort-body rules)))
    (let* ((heads (-<> rules
                    (remove-if-not #'rule-empty-p <>)
                    (mapcar #'rule-head <>)
                    (remove-duplicates <> :test #'equal)))
           (next-rules (remove-if (lambda (rule)
                                    (member (rule-head rule) heads :test #'equal))
                                  rules)))
      (if (null next-rules)
        (leaf heads)
        (multiple-value-bind (term low high both)
            (partition-rules next-rules)
          (node term
                (recur (append (mapcar #'drop-first low) both))
                (recur (append (mapcar #'drop-first high) both))
                heads))))))


;;;; GraphViz
(setf cl-dot:*dot-path* "/usr/local/bin/dot")

(defmethod attrs (object &rest attributes)
  (make-instance 'cl-dot:attributed
                 :object object
                 :attributes attributes))

(defmethod cl-dot:graph-object-node ((graph (eql 'rule-tree))
                                     (object rule-tree))
  (make-instance 'cl-dot:node
                 :attributes (adt:match rule-tree object
                               ((leaf heads)
                                `(:label ,(format nil "+~S" heads)
                                  :shape :ellipse))
                               ((node term _ _ heads)
                                `(:label ,(format nil "~S~A" term
                                                  (if heads
                                                    (format nil "~%+~S" heads)
                                                    ""))
                                  :shape :box)))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'rule-tree))
                                          (object rule-tree))
  (adt:match rule-tree object
    ((leaf _) nil)
    ((node _ low high _)
     (list (attrs high :style :solid)
           (attrs low :style :dashed)))))

(defun draw-rule-tree (tree &optional (filename "tree.png"))
  (cl-dot:dot-graph
    (cl-dot:generate-graph-from-roots 'rule-tree (list tree))
    filename
    :format :png))


;;;; Scratch
(defparameter *rules* '(
                        (x (not b) a)
                        (x a c)
                        (y c)
                        (z b d)
                        (z d (not c))
                        ))
(defparameter *rules* '(
                        (x (not b) a)
                        (x a c)
                        (y (true dogs))
                        (z d b)
                        (z (not c) d)
                        ))

(draw-rule-tree (make-rule-tree *rules*))
