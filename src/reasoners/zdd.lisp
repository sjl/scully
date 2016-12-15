(in-package :scully.reasoners.zdd)

(defparameter *reasoner* nil)


;;;; Utils --------------------------------------------------------------------
(defmacro defclass* (name-and-options direct-superclasses slots &rest options)
  (flet ((slot-definition (conc-name slot)
           (destructuring-bind (name &key
                                     type
                                     documentation
                                     initform
                                     (accessor (symb conc-name name))
                                     (initarg (intern (symbol-name name) :keyword)))
               (ensure-list slot)
             `(,name
               :initarg ,initarg
               :accessor ,accessor
               ,@(when initform `(:initform ,initform))
               ,@(when type `(:type ,type))
               ,@(when documentation `(:documentation ,documentation))))))
    (destructuring-bind (name &key (conc-name (symb name '-)))
        (ensure-list name-and-options)
      `(defclass ,name ,direct-superclasses
        ,(mapcar (curry #'slot-definition conc-name) slots)
        ,@options))))


(defun find-ggp-symbol (atom)
  (if (symbolp atom)
    (values (intern (symbol-name atom)
                    (find-package :ggp-rules)))
    atom))

(defun make-iset (reasoner contents)
  ; (print-hash-table (zr-term->number reasoner))
  (zdd-set (mapcar (curry #'term-to-number reasoner)
                   (map-tree #'find-ggp-symbol contents))))


;;;; Rule Forests -------------------------------------------------------------
(defclass* (rule-forest :conc-name rf-) ()
  (strata
   upper-bound
   lower-bound))


(defun make-forest-with (old-forest strata)
  (make-instance 'rule-forest
    :strata strata
    :upper-bound (rf-upper-bound old-forest)
    :lower-bound (rf-lower-bound old-forest)))

(defun forest-empty-p (forest)
  (null (rf-strata forest)))


(defun find-bound (predicate layer)
  (extremum (mapcar #'scully.gdl::rule-head layer) predicate))

(defun find-lower-bound (layer)
  (find-bound #'< layer))

(defun find-upper-bound (layer)
  (find-bound #'> layer))


;;;; Reasoner -----------------------------------------------------------------
(defclass* (zdd-reasoner :conc-name zr-) ()
  (rules
   roles
   term->number
   number->term
   initial-zdd
   legal-zdd
   goal-zdd
   terminal-zdd
   possible-forest
   happens-forest))


(defun find-initial-state (rules term->number)
  (-<> rules
    (mapcan (lambda-match
              ((list (list* 'ggp-rules::init body))
               `((ggp-rules::true ,@body))))
            <>)
    (mapcar (lambda (term) (gethash term term->number)) <>)))

(defun find-roles (rules)
  (mapcan (lambda-match
            ((list (list 'ggp-rules::role r))
             (list r)))
          rules))

(defun make-predicate-zdd (predicate term->number)
  (-<> term->number
    hash-table-alist
    (remove-if-not (lambda (rule)
                     (eql predicate (first (first rule))))
                   <>)
    (mapcar #'cdr <>)
    (zdd-set <>)))

(defun make-stratum-rule-trees (stratum)
  (-<> stratum
    (group-by #'car <>)
    hash-table-values
    (mapcar #'scully.rule-trees::make-rule-tree <>)))

(defun make-rule-forest (rule-layers layer)
  (let ((rules (gethash layer rule-layers)))
    (make-instance 'rule-forest
      :strata (-<> rules
                scully.terms::stratify-layer
                (mapcar #'make-stratum-rule-trees <>))
      :upper-bound (find-upper-bound rules)
      :lower-bound (find-lower-bound rules))))


(defun make-zdd-reasoner (rules)
  "Turn a set of grounded GDL rules into a logic manager.

  A rule forest is a collection of individual rule trees in a single layer,
  stratified as necessary:

    POSSIBLE: (STRATUM-1 STRATUM-2 ...)
    HAPPENS:  (STRATUM-1 STRATUM-2 ...)
                 ||       ||
                 ||       \/
                 ||     (rule-tree-1 rule-tree-2 ...)
                 \/
         (rule-tree-1 rule-tree-2 ...)

  "
  (let ((rules (scully.gdl::normalize-rules rules)))
    (destructuring-bind (term->number number->term rule-layers)
        (scully.terms::integerize-rules rules)
      (with-zdd
        (make-instance 'zdd-reasoner
          :rules rules
          :roles (find-roles rules)
          :possible-forest (make-rule-forest rule-layers :possible)
          :happens-forest (make-rule-forest rule-layers :happens)
          :initial-zdd (zdd-set (find-initial-state rules term->number))
          :legal-zdd (make-predicate-zdd 'ggp-rules::legal term->number)
          :goal-zdd (make-predicate-zdd 'ggp-rules::goal term->number)
          :terminal-zdd (make-predicate-zdd 'ggp-rules::terminal term->number)
          :term->number term->number
          :number->term number->term)))))


;;;; Basic API ----------------------------------------------------------------
(defun number-to-term (reasoner number)
  (gethash number (zr-number->term reasoner)))

(defun term-to-number (reasoner term)
  (gethash term (zr-term->number reasoner)))


(defun initial-iset (reasoner)
  "Return the initial information set of the game."
  (zr-initial-zdd reasoner))

(defun rand-state (reasoner iset)
  "Select a random member of the given information set."
  (mapcar (curry #'number-to-term reasoner)
          (zdd-random-member iset)))

(defun terminalp (reasoner iset)
  "Return whether the given information set is a terminal state."
  (-<> iset
    (zdd-meet <> (zr-terminal-zdd reasoner))
    zdd-unit-p
    not))

(defun roles (reasoner)
  (zr-roles reasoner))


;;;; Drawing ------------------------------------------------------------------
(defun label (reasoner n)
  (let ((*package* (find-package :ggp-rules)))
    (-<> n
      (number-to-term (if (eq t reasoner)
                        *reasoner*
                        reasoner)
                      <>)
      (structural-string <>))))

(defun draw-zdd (reasoner zdd)
  (scully.graphviz::draw-zdd zdd :label-fn (curry #'label reasoner)))

(defun draw-rule-tree (reasoner rule-tree)
  (scully.graphviz::draw-rule-tree rule-tree :label-fn (curry #'label reasoner)))


;;;; Logic Application --------------------------------------------------------
;;;; Utils
(defun tree-to-result (tree)
  (adt:match scully.rule-trees::rule-tree tree
    ((scully.rule-trees::top head) (values nil head))
    (scully.rule-trees::bottom (values nil nil))
    ((scully.rule-trees::node _ _ _) (values tree nil))))


(defun process-stratum (function stratum)
  "Process the stratum with `function`.

  Two values will be returned:

  1. The new stratum (possibly NIL).
  2. Any new heads to add (possible NIL).

  "
  (iterate
    (for tree :in stratum)
    (for (values new-tree new-head) = (funcall function tree))
    (when new-tree (collect new-tree :into new-stratum))
    (when new-head (collect new-head :into new-heads))
    (finally (return (values new-stratum new-heads)))))

(defun process-forest (function forest)
  "Process the rule forest with `function`.

  Two values will be returned:

  1. The new forest (possibly NIL).
  2. Any new heads to add (possible NIL).

  "
  (iterate
    (for stratum :in (rf-strata forest))
    (for (values new-stratum new-heads) = (process-stratum function stratum))
    (when new-stratum (collect new-stratum :into new-strata))
    (appending new-heads :into heads)
    (finally (return (values (make-forest-with forest new-strata) heads)))))


;;;; Phase 1: Information Set Traversal
(defun advance-tree (tree term)
  "Advance the rule tree up to (but not beyond) `term`.

  Two values will be returned:

  1. Either the resulting rule tree, or NIL if it was advanced down to a sink.
  2. The new head if it was advanced down to a TOP sink, or NIL otherwise.

  "
  (adt:match scully.rule-trees::rule-tree tree
    ((scully.rule-trees::node term% _ lo)
     (if (< term% term)
       (advance-tree lo term)
       (tree-to-result tree)))
    (_ (tree-to-result tree))))

(defun advance-forest (forest term)
  "Advance the rule forest up to (but not beyond) `term`.

  Two values will be returned:

  1. The new forest (possibly NIL).
  2. Any new heads to add (possible NIL).

  "
  (process-forest (rcurry #'advance-tree term) forest))


(defun split-tree-hi (tree term)
  (adt:match scully.rule-trees::rule-tree tree
    ((scully.rule-trees::node term% hi _)
     (if (= term% term)
       (tree-to-result hi)
       (tree-to-result tree)))
    (_ (error "Cannot split rule tree: ~S" tree))))

(defun split-tree-lo (tree term)
  (adt:match scully.rule-trees::rule-tree tree
    ((scully.rule-trees::node term% _ lo)
     (if (= term% term)
       (tree-to-result lo)
       (tree-to-result tree)))
    (_ (error "Cannot split rule tree: ~S" tree))))


(defun split-forest-hi (forest term)
  (process-forest (rcurry #'split-tree-hi term) forest))

(defun split-forest-lo (forest term)
  (process-forest (rcurry #'split-tree-lo term) forest))


(defun traverse-iset (iset forest)
  "Walk down the information set and rule forest in parallel."
  (recursively ((iset iset)
                (forest forest)
                (heads '()))
    (ematch iset
      ;; If we hit an empty sink we're out of sets to ever cons the heads onto,
      ;; so we can just bail immediately.
      ((sink nil) iset)

      ;; If we hit a unit sink we're done with the state-walking portion of this
      ;; algorithm and can move on the the fixed-pointing of the heads.
      ((sink t) (finalize-heads forest heads))

      ;; Otherwise we need to build a new ZDD node with the recursive results.
      ((node term hi lo)
       (multiple-value-bind*
           (((forest advanced-heads) (advance-forest forest term))
            ((forest-hi hi-heads) (split-forest-hi forest term))
            ((forest-lo lo-heads) (split-forest-lo forest term)))
         (zdd-node
           term
           (recur hi forest-hi (append heads advanced-heads hi-heads))
           (recur lo forest-lo (append heads advanced-heads lo-heads))))))))


;;;; Phase 2: Head Finalization
(defun walk-tree-positive (rule-tree heads)
  ; (pr "Walking positive rule tree with heads"
  ;     (mapcar (curry #'number-to-term *reasoner*) heads))
  ; (draw-rule-tree t rule-tree)
  ; (break)
  (adt:match scully.rule-trees::rule-tree rule-tree
    ((scully.rule-trees::node term hi _)
     (if (member term heads)
       (walk-tree-positive hi heads)
       (tree-to-result rule-tree)))
    (_ (tree-to-result rule-tree))))

(defun walk-tree-negative (rule-tree heads)
  ; (pr "Walking negative rule tree with heads"
  ;     (mapcar (curry #'number-to-term *reasoner*) heads))
  ; (draw-rule-tree t rule-tree)
  ; (break)
  (adt:match scully.rule-trees::rule-tree rule-tree
    ((scully.rule-trees::node term hi lo)
     (if (member term heads)
       (walk-tree-negative hi heads)
       (walk-tree-negative lo heads)))
    (_ (tree-to-result rule-tree))))


(defun walk-stratum-positive (stratum heads)
  (iterate
    ; (pr "Beginning stratum walk, starthing with heads" heads)
    (for (values new-stratum new-heads) =
         (process-stratum (rcurry #'walk-tree-positive heads) stratum))
    ; (pr "Found new heads:" new-heads)
    ; (break)
    (appending new-heads :into all-new-heads)
    (setf stratum new-stratum
          heads (append heads new-heads))
    (while new-heads)
    (finally (return (values stratum all-new-heads)))))

(defun walk-stratum-negative (stratum heads)
  (process-stratum (rcurry #'walk-tree-negative heads) stratum))


(defun finalize-heads (forest heads)
  "Finalize the set of heads to add and return the appropriate ZDD."
  (multiple-value-bind (f h) (advance-forest forest (rf-lower-bound forest))
    (setf heads (append heads h)
          forest f))
  (iterate
    (for stratum :in (rf-strata forest))
    (multiple-value-bind (s h) (walk-stratum-positive stratum heads)
      (setf heads (append heads h)
            stratum s))
    (multiple-value-bind (s h) (walk-stratum-negative stratum heads)
      (setf heads (append heads h)
            stratum s))
    (finally (return (zdd-set heads)))))


;;;; API
(defun apply-rule-forest (reasoner iset forest)
  "Apply `forest` to the given information set for `reasoner`."
  (with-zdd
    (let ((*reasoner* reasoner))
      (traverse-iset iset forest))))


;;;; Scratch ------------------------------------------------------------------
(defparameter *rules*
  (scully.gdl::read-gdl "gdl/tictactoe-grounded.gdl")
  ; (scully.gdl::read-gdl "gdl/hanoi-grounded.gdl")
  ; (scully.gdl::read-gdl "gdl/8puzzle-grounded.gdl")
  ; (scully.gdl::read-gdl "gdl/roshambo2-grounded.gdl")
  )
(defparameter *l* (make-zdd-reasoner *rules*))
(defparameter *i* (initial-iset *l*))
(defparameter *j* (initial-iset *l*))

(with-zdd
  (-<> *l*
    (make-iset '(
                 (true (control oplayer))
                 (true (cell 1 1 o)) (true (cell 1 2 x)) (true (cell 1 3 o))
                 (true (cell 2 1 x)) (true (cell 2 2 o)) (true (cell 2 3 o))
                 (true (cell 3 1 x)) (true (cell 3 2 x)) (true (cell 3 3 x))
                 ))
    (apply-rule-forest *l* <> (zr-possible-forest *l*))
    (draw-zdd *l* <>)
    ))

(with-zdd
  (-<>
      (zdd-union (make-iset *l* '(
                                  (true (control xplayer))
                                  (true (cell 1 1 o)) (true (cell 1 2 x)) (true (cell 1 3 B))
                                  (true (cell 2 1 x)) (true (cell 2 2 o)) (true (cell 2 3 o))
                                  (true (cell 3 1 x)) (true (cell 3 2 o)) (true (cell 3 3 x))
                                  ))
                 (make-iset *l* '(
                                  (true (control xplayer))
                                  (true (cell 1 1 o)) (true (cell 1 2 B)) (true (cell 1 3 x))
                                  (true (cell 2 1 x)) (true (cell 2 2 o)) (true (cell 2 3 o))
                                  (true (cell 3 1 x)) (true (cell 3 2 o)) (true (cell 3 3 x))
                                  ))
                 (make-iset *l* '(
                                  (true (control xplayer))
                                  (true (cell 1 1 o)) (true (cell 1 2 x)) (true (cell 1 3 x))
                                  (true (cell 2 1 x)) (true (cell 2 2 o)) (true (cell 2 3 o))
                                  (true (cell 3 1 x)) (true (cell 3 2 o)) (true (cell 3 3 B))
                                  )))
    (apply-rule-forest *l* <> (zr-possible-forest *l*))
    (draw-zdd *l* <>)))
