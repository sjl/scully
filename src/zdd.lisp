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
                  ((leaf c) `(:label ,(if c "⊤" "⊥") :shape :square))
                  ((node v) `(:label ,v :shape :circle)))))

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
(defparameter *cache*
  (tg:make-weak-hash-table :weakness :value :test #'equalp))


(defmacro with-zdd (&body body)
  `(with-odd-context (:operation #'zdd-apply :node-cache *cache*)
    ,@body))

(defun enumerate (zdd)
  "Return a list of all members of `zdd`."
  (ematch zdd
    ((leaf nil) nil)
    ((leaf t) (list nil))
    ((node variable hi lo)
     (append (mapcar (curry #'cons variable) (enumerate hi))
             (enumerate lo)))))

(defun print-enumerated (zdd)
  (pr (enumerate zdd))
  zdd)


(defun zdd-count (zdd)
  "Return the number of members of `zdd`."
  (ematch zdd
    ((leaf nil) 0)
    ((leaf t) 1)
    ((node _ hi lo) (+ (zdd-count hi)
                       (zdd-count lo)))))

(defun zdd-size (zdd)
  "Return the number of unique nodes in `zdd`."
  (let ((seen (make-hash-table :test 'eq)))
    (recursively ((zdd zdd))
      (ematch zdd
        ((leaf) (setf (gethash zdd seen) t))
        ((node _ hi lo)
         (when (not (gethash zdd seen))
           (setf (gethash zdd seen) t)
           (recur lo)
           (recur hi)))))
    (hash-table-count seen)))

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

(defun zdd-meet% (a b)
  (ematch* (a b)
    (((leaf nil) _) (leaf nil))
    ((_ (leaf nil)) (leaf nil))

    (((leaf t) _) (leaf t))
    ((_ (leaf t)) (leaf t))

    (((node var-a hi-a lo-a)
      (node var-b hi-b lo-b))
     (cond
       ((< var-a var-b) (zdd-union (zdd-meet% hi-a b)
                                   (zdd-meet% lo-a b)))
       ((> var-a var-b) (zdd-union (zdd-meet% hi-b a)
                                   (zdd-meet% lo-b a)))
       ((= var-a var-b) (zdd-node var-a
                                  (zdd-meet% hi-a hi-b)
                                  (zdd-union (zdd-meet% hi-a lo-b)
                                             (zdd-meet% lo-a hi-b)
                                             (zdd-meet% lo-a lo-b))))))))

(defun zdd-meet (&rest zdds)
  (if zdds
    (reduce #'zdd-meet% zdds)
    (leaf nil)))


(defun zdd-keep-supersets-of% (zdd set)
  (ematch* (zdd set)
    ((_ nil) zdd)
    (((leaf) _) (leaf nil))
    (((node var hi lo) (list* el remaining))
     (cond
       ((= var el) (zdd-node var
                             (zdd-keep-supersets-of% hi remaining)
                             (leaf nil)))
       ((< var el) (zdd-node var
                             (zdd-keep-supersets-of% hi set)
                             (zdd-keep-supersets-of% lo set)))
       ((> var el) (leaf nil))))))

(defun zdd-keep-supersets-of (zdd set)
  (zdd-keep-supersets-of% zdd (sort set #'<)))


(defun zdd-remove-supersets-of% (zdd set)
  (ematch* (zdd set)
    ((_ nil) (leaf nil))
    (((leaf) _) zdd)
    (((node var hi lo) (list* el remaining))
     (cond
       ((= var el) (zdd-node var
                             (zdd-remove-supersets-of% hi remaining)
                             lo))
       ((< var el) (zdd-node var
                             (zdd-remove-supersets-of% hi set)
                             (zdd-remove-supersets-of% lo set)))
       ((> var el) zdd)))))

(defun zdd-remove-supersets-of (zdd set)
  (zdd-remove-supersets-of% zdd (sort set #'<)))


(defun zdd-keep-avoiders-of% (zdd set)
  (ematch* (zdd set)
    ((_ nil) zdd)
    (((leaf) _) zdd)
    (((node var hi lo) (list* el remaining))
     (cond
       ((= var el) (zdd-keep-avoiders-of% lo remaining))
       ((< var el) (zdd-node var
                             (zdd-keep-avoiders-of% hi set)
                             (zdd-keep-avoiders-of% lo set)))
       ((> var el) (zdd-keep-avoiders-of% zdd remaining))))))

(defun zdd-keep-avoiders-of (zdd set)
  (zdd-keep-avoiders-of% zdd (sort set #'<)))


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
    ))

(with-zdd
  (enumerate
    (draw (zdd-meet (zdd-family '(1 2) '(1 6))
              (zdd-family '(2))))))


(with-zdd
  (-<> (zdd-join (zdd-family '(1 2) '(7 8) '())
                 (zdd-family '(1 5 9) nil)
                 (zdd-set '(1)))
    (print-enumerated <>)
    (zdd-keep-avoiders-of <> '(2 7))
    (print-enumerated <>)
    (draw <>)
    (zdd-size <>)
    )
  )
