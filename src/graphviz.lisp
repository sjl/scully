(in-package :scully.graphviz)

(setf cl-dot:*dot-path* "/usr/local/bin/dot")

;;;; Utils --------------------------------------------------------------------
(defun attrs (object &rest attributes)
  (make-instance 'cl-dot:attributed
    :object object
    :attributes attributes))


;;;; Rule Trees ---------------------------------------------------------------
(defparameter *rt-label-fn* #'identity)

(defmethod cl-dot:graph-object-node ((graph (eql 'rule-tree))
                                     (object scully.rule-trees::rule-tree))
  (make-instance 'cl-dot:node
    :attributes (adt:match scully.rule-trees::rule-tree object
                  ((scully.rule-trees::node term _ _)
                   `(:label ,(aesthetic-string (funcall *rt-label-fn* term))
                     :shape :ellipse))

                  (scully.rule-trees::bottom
                    `(:label "⊥"
                      :shape :square))

                  ((scully.rule-trees::top term)
                   `(:label ,(aesthetic-string (funcall *rt-label-fn* term))
                     :shape :rectangle)))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'rule-tree))
                                          (object scully.rule-trees::rule-tree))
  (adt:match scully.rule-trees::rule-tree object
    ((scully.rule-trees::node _ hi lo) (list (attrs hi :style :solid)
                                             (attrs lo :style :dashed)))
    ((scully.rule-trees::top _) nil)
    (scully.rule-trees::bottom nil)))


(defun draw-rule-tree (rule-tree &key
                       (filename "rule-tree.png")
                       (label-fn #'identity))
  (let ((*rt-label-fn* label-fn))
    (cl-dot:dot-graph
      (cl-dot:generate-graph-from-roots 'rule-tree (list rule-tree)
                                        ;; '(:dpi 300)
                                        )
      filename
      :format :png))
  rule-tree)


;;;; ZDDs ---------------------------------------------------------------------
(defparameter *draw-unique-sinks* nil)
(defparameter *draw-unique-nodes* nil)
(defparameter *draw-hex-p* #'no)
(defparameter *draw-label-fn* #'identity)


(defun sink-attrs (val)
  `(:label ,(if val "⊤" "⊥")
    :shape :square
    :style :filled
    :fillcolor "#fafafa"
    :color "#aaaaaa"
    :fontsize 22
    :width 0.05))


(defmethod cl-dot:graph-object-node ((graph (eql 'zdd))
                                     (object scully.zdd::node))
  (make-instance 'cl-dot:node
    :attributes (ematch object
                  ((scully.zdd::node v)
                   `(:label ,(funcall *draw-label-fn* v)
                     :shape ,(if (funcall *draw-hex-p* v)
                               :hexagon
                               :rectangle))))))

(defmethod cl-dot:graph-object-node ((graph (eql 'zdd))
                                     (object cons))
  (cl-dot:graph-object-node graph (car object)))

(defmethod cl-dot:graph-object-node ((graph (eql 'zdd))
                                     (object scully.zdd::leaf))
  (make-instance 'cl-dot:node
    :attributes (ematch object ((scully.zdd::sink c) (sink-attrs c)))))


(defun wrap-node (object)
  (ematch object
    ((scully.zdd::sink) (if *draw-unique-sinks* object (cons object nil)))
    ((scully.zdd::node) (if *draw-unique-nodes* object (cons object nil)))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'zdd))
                                          (object t))
  (ematch object
    ((cons object _)
     (cl-dot:graph-object-points-to graph object))
    ((scully.zdd::sink _)
     '())
    ((scully.zdd::node _ hi lo)
     (list (attrs (wrap-node hi) :style :solid)
           (attrs (wrap-node lo) :style :dashed)))))


(defun draw-zdd (zdd &key
                 (filename "zdd.png")
                 (unique-sinks nil)
                 (unique-nodes t)
                 (hexp #'no)
                 (label-fn #'identity))
  (let ((*draw-unique-sinks* unique-sinks)
        (*draw-unique-nodes* unique-nodes)
        (*draw-hex-p* hexp)
        (*draw-label-fn* label-fn))
    (cl-dot:dot-graph
      (cl-dot:generate-graph-from-roots 'zdd (list (wrap-node zdd)))
      filename
      :format :png))
  zdd)

