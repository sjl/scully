(in-package :scully.zdd)

(defpattern leaf (&optional content)
  `(structure leaf :content ,content))


;;;; Utils --------------------------------------------------------------------
(defun gcprint (thing &rest args)
  (let ((*print-circle* t))
    (apply #'print
           (prog1 thing
                  (tg:gc :full t :verbose t))
           args)))


;;;; GraphViz -----------------------------------------------------------------
(setf cl-dot:*dot-path* "/usr/local/bin/dot")

(defun attrs (object &rest attributes)
  (make-instance 'cl-dot:attributed
    :object object
    :attributes attributes))

(defmethod cl-dot:graph-object-node ((graph (eql 'zdd))
                                     (object t))
  (make-instance 'cl-dot:node
    :attributes (ematch object
                  ((leaf c) `(:label ,c :shape :ellipse))
                  ((node v) `(:label ,v :shape :box)))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'zdd))
                                          (object t))
  (ematch object
    ((leaf) '())
    ((node _ hi lo)
     (list (attrs hi :style :solid)
           (attrs lo :style :dashed)))))

(defun draw (zdd &optional (filename "zdd.png"))
  (cl-dot:dot-graph
    (cl-dot:generate-graph-from-roots 'zdd (list zdd))
    filename
    :format :png)
  zdd)


;;;; ZDDs ---------------------------------------------------------------------
(defparameter *cache* (tg:make-weak-hash-table :weakness :value :test #'equalp))


(defmacro with-zdd (&body body)
  `(with-odd-context (:operation #'zdd-apply :node-cache *cache*)
    ,@body))

(defun enumerate (zdd)
  (ematch zdd
    ((leaf nil) nil)
    ((leaf t) (list nil))
    ((node variable hi lo)
     (append (mapcar (curry #'cons variable) (enumerate hi))
             (enumerate lo)))))

(defun unit-patch (z)
  (ematch z
    ((leaf t) z)
    ((leaf nil) (leaf t))
    ((node variable hi lo)
     (zdd-node variable hi (unit-patch lo)))))


(defun zdd-set (elements)
  (make-set elements))


(defun zdd-union (a b)
  (ematch* (a b)
    (((node) (leaf))
     (zdd-union b a))

    (((leaf nil) b)
     b)

    (((leaf t) b)
     (unit-patch b))

    (((node var-a hi-a lo-a)
      (node var-b hi-b lo-b))
     (cond
       ((< var-a var-b)
        (zdd-node var-a
                  hi-a
                  (zdd-union lo-a b)))
       ((> var-a var-b)
        (zdd-node var-b
                  hi-b
                  (zdd-union lo-b a)))
       ((= var-a var-b)
        (zdd-node var-a
                  (zdd-union hi-a hi-b)
                  (zdd-union lo-a lo-b)))))))


(defun zdd-family (&rest sets)
  (reduce #'zdd-union (mapcar #'zdd-set sets)))



;;;; Scratch ------------------------------------------------------------------
(with-zdd
  (enumerate (gcprint (draw (zdd-union (zdd-family '(1 2 4) '(3 4))
                                       (zdd-set '(1 2)))))))
