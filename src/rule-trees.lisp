(in-package :scully.rule-trees)
(in-readtable :fare-quasiquote)

;;;; Rule Trees ---------------------------------------------------------------
(adt:defdata rule-tree
  (node t rule-tree rule-tree)
  (top t)
  bottom)


(defun rule-head (rule)
  (first rule))

(defun rule-body (rule)
  (rest rule))

(defun-match bare-term (term)
  (`(ggp-rules::not ,contents) contents)
  (_ term))

(defun term< (a b)
  (< (bare-term a) (bare-term b)))


(defun find-smallest-body-term (bodies)
  (-<> bodies
    (mapcar #'first <>)
    (sort <> #'term<)
    (first <>)))

(defun partition (bodies)
  (let ((element (bare-term (find-smallest-body-term bodies))))
    (labels
        ((requires (body)
           (equal (first body) element))
         (disallows (body)
           (equal (first body) `(ggp-rules::not ,element)))
         (ignores (body)
           (not (or (requires body)
                    (disallows body)))))
      (values element
              (remove-if-not #'disallows bodies)
              (remove-if-not #'requires bodies)
              (remove-if-not #'ignores bodies)))))


(defun make-node (cache term hi lo)
  (if (eql hi lo)
    hi
    (ensure-gethash (list term hi lo) cache
                    (node term hi lo))))

(defun make-rule-tree (rules)
  (let* ((head (rule-head (first rules)))
         (top (top head))
         (cache (make-hash-table :test #'equal)))
    (recursively ((bodies (-<> rules
                            (mapcar #'rule-body <>)
                            (mapcar (rcurry #'sort #'term<) <>))))
      (cond
        ((null bodies) bottom)
        ((some #'null bodies) top)
        (t (multiple-value-bind (term disallows requires ignores)
               (partition bodies)
             (make-node cache
                        term
                        (recur (append (mapcar #'rest requires) ignores))
                        (recur (append (mapcar #'rest disallows) ignores)))))))))


;;;; GraphViz -----------------------------------------------------------------
(setf cl-dot:*dot-path* "/usr/local/bin/dot")

(defun attrs (object &rest attributes)
  (make-instance 'cl-dot:attributed
    :object object
    :attributes attributes))


(defmethod cl-dot:graph-object-node ((graph (eql 'rule-tree)) (object rule-tree))
  (make-instance 'cl-dot:node
    :attributes (adt:match rule-tree object
                  ((node term _ _) `(:label ,(aesthetic-string term)
                                     :shape :circle))

                  (bottom `(:label "⊥"
                            :shape :square))

                  ((top term) `(:label ,(aesthetic-string term)
                                :shape :rectangle)))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'rule-tree))
                                          (object rule-tree))
  (adt:match rule-tree object
    ((node _ hi lo) (list (attrs hi :style :solid)
                          (attrs lo :style :dashed)))
    ((top _) nil)
    (bottom nil)))


(defun draw (rule-tree &key (filename "rule-tree.png"))
  (cl-dot:dot-graph
    (cl-dot:generate-graph-from-roots 'rule-tree (list rule-tree))
    filename
    :format :png)
  rule-tree)


;;;; Scratch ------------------------------------------------------------------
(defparameter *rule* '(
                       (1000 1 2 (ggp-rules::not 3))
                       (1000 4 2 3 15)
                       (1000 (ggp-rules::not 19) 18)
                       (1000 19 17)
                       ))

(-<> *rule* make-rule-tree draw)
