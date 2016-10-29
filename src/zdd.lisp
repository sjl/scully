(in-package :scully.zdd)

(defun gcprint (thing &rest args)
  (let ((*print-circle* t))
    (apply #'print
           (prog1 thing
                  (tg:gc :full t :verbose t))
           args)))

(setf cl-dot:*dot-path* "/usr/local/bin/dot")

(defmethod attrs (object &rest attributes)
  (make-instance 'cl-dot:attributed
                 :object object
                 :attributes attributes))

(defmethod cl-dot:graph-object-node ((graph (eql 'zdd))
                                     (object leaf))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(trivialib.bdd::leaf-content object)
                               :shape :ellipse)))

(defmethod cl-dot:graph-object-node ((graph (eql 'zdd))
                                     (object node))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(trivialib.bdd::node-variable object)
                               :shape :box)))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'zdd))
                                          (object node))
  (list (attrs (trivialib.bdd::node-hi object) :style :solid)
        (attrs (trivialib.bdd::node-lo object) :style :dashed)))

(defun draw (zdd &optional (filename "zdd.png"))
  (cl-dot:dot-graph
    (cl-dot:generate-graph-from-roots 'zdd (list zdd))
    filename
    :format :png))


(with-odd-context (:operation #'zdd-apply)
  (draw (gcprint (make-family '((4)  ())))))
