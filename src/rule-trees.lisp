(in-package :scully.rule-trees)
(in-readtable :fare-quasiquote)

;;;; Rule Trees ---------------------------------------------------------------
(defun abs< (x y)
  (declare (type fixnum x y))
  (< (abs x) (abs y)))

(adt:defdata rule-tree
  (node t rule-tree rule-tree)
  (top t)
  bottom)

(defun rule-tree-hi (tree)
  (adt:match rule-tree tree
    ((node _ hi _) hi)
    (_ (error "No hi for rule tree ~S" tree))))

(defun rule-tree-lo (tree)
  (adt:match rule-tree tree
    ((node _ _ lo) lo)
    (_ (error "No lo for rule tree ~S" tree))))


(defun find-smallest-body-term (bodies)
  "Find the smallest body term in `bodies`.

  Each body in `bodies` must already be sorted.  No body should be empty.

  "
  (iterate (for body :in bodies)
           (for term = (the fixnum (first body)))
           (minimizing (abs term))))

(defun partition (bodies)
  "Partition `bodies` into exclusive groups based on the smallest element.

  `bodies` must each be already sorted.

  Four values will be returned:

  1. The smallest element in any body.
  2. All bodies that DISALLOW that element.
  3. All bodies that REQUIRE that element.
  4. All bodies that DON'T CARE about that element.

  "
  (iterate
    (with element = (the fixnum (find-smallest-body-term bodies)))
    (with negation = (the fixnum (- element)))
    (for body :in bodies)
    (for term = (the fixnum (first body)))
    (cond
      ((= term element) (collect body :into requires))
      ((= term negation) (collect body :into disallows))
      (t (collect body :into ignores)))
    (finally (return (values element disallows requires ignores)))))


(defun make-node (cache term hi lo)
  (if (eql hi lo)
    hi
    (ensure-gethash (list term hi lo) cache
                    (node term hi lo))))

(defun sort-body (body)
  (sort body #'abs<))

(defun make-rule-tree (rules)
  "Make a rule tree for `rules`.

  All rules must have the same head (this is not checked).  Bodies do not need
  to be sorted.

  "
  (let* ((head (rule-head (first rules)))
         (top (top head))
         (cache (make-hash-table :test #'equal)))
    ;; (pr head)
    (recursively ((bodies (mapcar (compose #'sort-body #'rule-body) rules)))
      (cond
        ((null bodies) bottom)
        ((some #'null bodies) top)
        (t (multiple-value-bind (term disallows requires ignores)
               (partition bodies)
             (make-node cache
                        term
                        (recur (append (mapcar #'rest requires) ignores))
                        (recur (append (mapcar #'rest disallows) ignores)))))))))


(defun rule-tree-size (tree)
  (adt:match rule-tree tree
    (bottom 1)
    ((top _) 1)
    ((node _ hi lo) (+ 1
                       (rule-tree-size hi)
                       (rule-tree-size lo)))))

(defun head (tree)
  (adt:match rule-tree tree
    (bottom nil)
    ((top term) term)
    ((node _ hi lo) (or (head hi) (head lo)))))


;;;; Scratch ------------------------------------------------------------------
