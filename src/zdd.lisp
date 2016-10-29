(in-package :scully.zdd)

; (defctype dd-halfword :uint32)

; (defcstruct dd-children
;   (true :pointer)
;   (else :pointer))

; (defcunion dd-node-guts
;   (value :double)
;   (kids (:struct dd-children)))

; (defcstruct dd-node
;   (index dd-halfword)
;   (ref dd-halfword)
;   (next (:pointer (:struct dd-node)))
;   (type (:union dd-node-guts)))


; (defparameter *m*
;   (cudd-init 0
;              0
;              scully.cudd:+cudd-unique-slots+
;              scully.cudd:+cudd-cache-slots+
;              0))

; (defun ref (node)
;   (cudd-ref node)
;   node)

; (defun deref (&rest nodes)
;   (iterate (for node :in nodes)
;            (cudd-recursive-deref-zdd *m* node))
;   (values))

; (defun var (index)
;   (let ((node (cudd-zdd-ith-var *m* index)))
;     (ref node)
;     node))

; (defun draw (node &optional (filename "zdd.dot"))
;   (format t "Cover~%")
;   (cudd-zdd-print-cover *m* node)
;   (format t "~%Node Count: ~A~%"
;           (cudd-zdd-read-node-count *m*))
;   (format t "~%Minterm~%")
;   (cudd-zdd-print-minterm *m* node)
;   (format t "~%Debug~%")
;   (cudd-zdd-print-debug *m* node 0 10)
;   (format t "~%Table~%")
;   (cudd-zdd-print-subtable *m*)
;   (let ((file (foreign-funcall "fopen" :string filename :string "w" :pointer)))
;     (unwind-protect
;         (let ((node-array (foreign-alloc :pointer :initial-element node)))
;           (cudd-zdd-dump-dot *m* 1 node-array (null-pointer) (null-pointer) file)
;           (foreign-free node-array))
;       (foreign-funcall "fclose" :pointer file))))


; (defun test ()
;   (let* ((a (var 0))
;          (b (var 1))
;          (result (ref (cudd-zdd-union *m* a b))))
;     (deref a b)
;     (draw result)
;     (deref result)))

; (defun test ()
;   (let* ((a (var 1)))
;     (draw a)
;     (deref a)))

; (test)
; (draw (cudd-zdd-ith-var *m* 0))

; (draw (cudd-read-zdd-one *m* 0))

; (draw (cudd-read-zero *m*))
; (test)


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
