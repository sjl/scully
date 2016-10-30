(in-package :scully.zdd)


;;;; Utils --------------------------------------------------------------------
(defun gcprint (thing &rest args)
  (let ((*print-circle* t))
    (apply #'print
           (prog1 thing
                  (tg:gc :full t :verbose t))
           args)))

(defpattern leaf (&optional content)
  `(structure leaf :content ,content))


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


(defun zdd-union% (a b)
  (ematch* (a b)
    (((node) (leaf)) (zdd-union% b a))

    (((leaf nil) b) b)
    (((leaf t) b) (unit-patch b))

    (((node var-a hi-a lo-a)
      (node var-b hi-b lo-b))
     (cond
       ((< var-a var-b) (zdd-node var-a hi-a (zdd-union% lo-a b)))
       ((> var-a var-b) (zdd-node var-b hi-b (zdd-union% lo-b a)))
       ((= var-a var-b) (zdd-node var-a
                                  (zdd-union% hi-a hi-b)
                                  (zdd-union% lo-a lo-b)))))))
(defun zdd-union (&rest zdds)
  (if zdds
    (reduce #'zdd-union% zdds)
    (leaf nil)))

(defun zdd-intersection% (a b)
  (ematch* (a b)
    (((node) (leaf)) (zdd-intersection% b a))

    (((leaf nil) _) (leaf nil))
    ((_ (leaf nil)) (leaf nil))

    (((leaf t) (leaf _)) b)
    (((leaf t) (node _ _ lo)) (zdd-intersection% a lo))

    (((node var-a hi-a lo-a)
      (node var-b hi-b lo-b))
     (cond
       ((< var-a var-b) (zdd-intersection% lo-a b))
       ((> var-a var-b) (zdd-intersection% lo-b a))
       ((= var-a var-b) (zdd-node var-a
                                  (zdd-intersection% hi-a hi-b)
                                  (zdd-intersection% lo-a lo-b)))))))

(defun zdd-intersection (&rest zdds)
  (if zdds
    (reduce #'zdd-intersection% zdds)
    (leaf nil)))

(defun zdd-join% (a b)
  (ematch* (a b)
    (((leaf nil) _) (leaf nil))
    ((_ (leaf nil)) (leaf nil))

    (((leaf t) b) b)
    ((a (leaf t)) a)

    (((node var-a hi-a lo-a)
      (node var-b hi-b lo-b))
     (cond
       ((< var-a var-b) (zdd-node var-a
                                  (zdd-join% hi-a b)
                                  (zdd-join% lo-a b)))
       ((> var-a var-b) (zdd-node var-b
                                  (zdd-join% hi-b a)
                                  (zdd-join% lo-b a)))
       ((= var-a var-b) (zdd-node var-a
                                  (zdd-union (zdd-join% hi-a lo-b)
                                             (zdd-join% lo-a hi-b)
                                             (zdd-join% hi-a hi-b))
                                  (zdd-join% lo-a lo-b)))))))

(defun zdd-join (&rest zdds)
  (if zdds
    (reduce #'zdd-join% zdds)
    (leaf nil)))


(defun zdd-family (&rest sets)
  (reduce #'zdd-union (mapcar #'zdd-set sets)))


;;;; Scratch ------------------------------------------------------------------
(with-zdd
  (enumerate (zdd-union (zdd-set '(1 2))
                        (zdd-set '(1 2))
                        (zdd-set '(1))
                        (zdd-family '(2) '(1 2) '(3)))))

(with-zdd
  (enumerate (zdd-intersection (zdd-family '(1) '(1 2) '(3) )
                               (zdd-family '(2 3 4) '(1 4) nil '(1 2))
                               (zdd-family nil '(1 2)))))


(with-zdd
  (enumerate
    (zdd-join (zdd-family '(1 2) '(7 8) '())
              (zdd-family '(1 5 9) nil)
              (zdd-set '(1))
              )))
