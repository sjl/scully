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

(defun never (val)
  (declare (ignore val))
  (values))

(defun print-through (function val)
  (pr (funcall function val))
  val)

(defun mapprint-through (function val)
  (mapc #'pr (funcall function val))
  val)


;;;; GraphViz -----------------------------------------------------------------
(setf cl-dot:*dot-path* "/usr/local/bin/dot")
(defparameter *draw-unique-sinks* nil)

(defun attrs (object &rest attributes)
  (make-instance 'cl-dot:attributed
    :object object
    :attributes attributes))

(defmethod cl-dot:graph-object-node ((graph (eql 'zdd))
                                     (object node))
  (make-instance 'cl-dot:node
    :attributes (ematch object
                  ((node v) `(:label ,v :shape :circle)))))

(defun sink-attrs (val)
  `(:label ,(if val "⊤" "⊥")
    :shape :square
    :style :filled
    :fillcolor "#fafafa"
    :color "#aaaaaa"
    :fontsize 22
    :width 0.05
    ))

(defmethod cl-dot:graph-object-node ((graph (eql 'zdd))
                                     (object cons))
  (make-instance 'cl-dot:node
    :attributes (ematch (car object) ((leaf c) (sink-attrs c)))))

(defmethod cl-dot:graph-object-node ((graph (eql 'zdd))
                                     (object leaf))
  (make-instance 'cl-dot:node
    :attributes (ematch object ((leaf c) (sink-attrs c)))))

(defun wrap-node (object)
  (if *draw-unique-sinks*
    object
    (ematch object
      ((leaf) (cons object nil))
      ((node) object))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'zdd))
                                          (object t))
  (ematch object
    ((leaf _) '())
    ((cons (leaf) _) '())
    ((node _ hi lo)
     (list (attrs (wrap-node hi) :style :solid)
           (attrs (wrap-node lo) :style :dashed)))))

(defun draw (zdd &optional (filename "zdd.png"))
  (cl-dot:dot-graph
    (cl-dot:generate-graph-from-roots 'zdd (list (wrap-node zdd)))
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


;;;; Rule Trees ---------------------------------------------------------------
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

(defun term< (t1 t2)
  (< (bare-term t1) (bare-term t2)))


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
  (recursively ((rules (mapcar #'sort-body rules))
                (accumulated-heads nil))
    (let* ((heads (-<> rules
                    (remove-if-not #'rule-empty-p <>)
                    (mapcar #'rule-head <>)
                    (remove-duplicates <> :test #'equal)
                    (union accumulated-heads <> :test #'equal))) ; slow
           (next-rules (remove-if
                         (lambda (rule)
                           (member (rule-head rule) heads :test #'equal))
                         rules)))
      (if (null next-rules)
        (zdd-set heads)
        (multiple-value-bind (term low high both)
            (partition-rules next-rules)
            (zdd-node term
                (recur (append (mapcar #'drop-first high) both) heads)
                (recur (append (mapcar #'drop-first low) both) heads)))))))


(defun apply-rule-tree (zdd rule-tree head-bound)
  (recursively ((zdd zdd)
                (rule-tree rule-tree))
    (ematch* (zdd rule-tree)
      ;; If Z = ∅ there are no sets to cons heads onto, bail.
      (((leaf nil) _) zdd)

      ;; If R = ∅ or {∅} we've bottomed out of the rule tree and there are no
      ;; heads to cons, we're done.
      ((_ (leaf)) zdd)

      ;; If we've passed the head boundary on the rule tree side then we're done
      ;; filtering and just need to cons in all the heads.
      ((_ (guard (node var _ _)
                 (>= var head-bound)))
       (zdd-join zdd rule-tree))

      ;; If Z = {∅} we might have some heads we need to cons later in the rule
      ;; tree, so recur down the lo side of it.
      (((leaf t) (node _ _ lo))
       (recur zdd lo))

      ;; Otherwise we need to filter.
      (((node var-z hi-z lo-z) (node var-r hi-r lo-r))
       (cond
         ((= var-z var-r) (zdd-node var-z
                                    (recur hi-z hi-r)
                                    (recur lo-z lo-r)))
         ((< var-z var-r) (zdd-node var-z
                                    (recur hi-z rule-tree)
                                    (recur lo-z rule-tree)))
         ((> var-z var-r) (recur zdd lo-r)))))))


;;;; Scratch ------------------------------------------------------------------
(let ((*draw-unique-sinks* nil))
  (with-zdd
    (-<> (zdd-join (zdd-family '(1 2) '(7 8) '())
                   (zdd-family '(1 5 9) nil)
                   (zdd-set '(1)))
      (print-enumerated <>)
      ; (zdd-keep-avoiders-of <> '(2 7))
      (print-enumerated <>)
      (draw <>)
      (zdd-size <>)
      )))

(defparameter *rules* '(
                        (1001 (not 2) 1)
                        (1001 1 3)
                        (1002 3)
                        (1003 4 2)
                        (1003 (not 3) 4)
                        (1004 1 2 3 (not 4))
                        (1005 (not 2) (not 3))
                        (1006 4 5)
                        (1006 2)
                        ))

(defparameter *state* '(
                        (1 3)
                        (1 2)
                        (2 4 5)
                        ()
                        (1 2 4 7)
                        )
  )

(let ((*draw-unique-sinks* t))
  (with-zdd
    (-<> (make-rule-tree *rules*)
      ; (print-enumerated <>)
      ; (zdd-keep-avoiders-of <> '(2 7))
      (mapprint-through #'enumerate <>)
      (print-through #'zdd-count <>)
      (print-through #'zdd-size <>)
      (draw <>)
      ; (zdd-size <>)
      (never)
      )
    (pr '--------------)
    (-<> (apply #'zdd-family *state*)
      (mapprint-through #'enumerate <>)
      (print-through #'zdd-count <>)
      (print-through #'zdd-size <>)
      ; (draw <>)
      ; (zdd-size <>)
      (never)
      )
    (pr '--------------)
    (-<> (apply-rule-tree (apply #'zdd-family *state*)
                          (make-rule-tree *rules*)
                          100)
      (mapprint-through #'enumerate <>)
      (print-through #'zdd-count <>)
      (print-through #'zdd-size <>)
      ; (draw <>)
      ; (zdd-size <>)
      (never)
      )
    ))
