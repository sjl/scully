(in-package :scully.reasoners.zdd)
(in-readtable :fare-quasiquote)

(defparameter *reasoner* nil)


;;;; Utils --------------------------------------------------------------------
(defun find-ggp-symbol (atom)
  (if (symbolp atom)
    (values (intern (symbol-name atom)
                    (find-package :ggp-rules)))
    atom))

(defun make-iset (reasoner contents)
  ; (print-hash-table (zr-term->number reasoner))
  (zdd-set (mapcar (curry #'term-to-number reasoner)
                   (map-tree #'find-ggp-symbol contents))))


;;;; Strata -------------------------------------------------------------------
(defclass* stratum ()
  (rule-trees lower-bound upper-bound))


(defun make-stratum (rule-trees lower-bound upper-bound)
  (make-instance 'stratum
    :rule-trees rule-trees
    :lower-bound lower-bound
    :upper-bound upper-bound))

(defun update-stratum-with (old-stratum new-rule-trees)
  (make-stratum new-rule-trees
                (stratum-lower-bound old-stratum)
                (stratum-upper-bound old-stratum)))

(defmethod print-object ((o stratum) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "with ~D rule~:P (~D-~D)"
            (length (stratum-rule-trees o))
            (stratum-lower-bound o)
            (stratum-upper-bound o))))


(defun find-stratum-bounds (rules)
  (extrema #'< (mapcar #'scully.gdl::rule-head rules)))

(defun build-stratum-rule-trees (rules)
  (-<> rules
    (group-by #'scully.gdl::rule-head <>)
    hash-table-values
    (mapcar #'scully.rule-trees::make-rule-tree <>)))

(defun build-stratum (rules)
  (multiple-value-call #'make-stratum
    (build-stratum-rule-trees rules)
    (find-stratum-bounds rules)))


;;;; Rule Forests -------------------------------------------------------------
(defclass* (rule-forest :conc-name rf-) ()
  (strata))

(defun make-rule-forest (strata)
  (make-instance 'rule-forest :strata strata))

(defun forest-empty-p (forest)
  (null (rf-strata forest)))

(defun build-rule-forest (strata-list)
  (make-rule-forest (mapcar #'build-stratum strata-list)))


;;;; Universes ----------------------------------------------------------------
(defun make-universe (predicate term->number)
  (let ((universe (make-array (hash-table-count term->number)
                    :initial-element nil)))
    (iterate (for (term number) :in-hashtable term->number)
             (when (funcall predicate term)
               (setf (aref universe number) t)))
    universe))


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
   next-zdd
   percept-universes
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

(defun make-predicate-zdd (predicate-prefix term->number)
  "Make a ZDD with a single member: the set of all terms for a single predicate.

  For example:

    (make-predicate-zdd '(ggp-rules::legal) ...)
    (make-predicate-zdd '(ggp-rules::true) ...)
    (make-predicate-zdd '(ggp-rules::sees ggp-rules::white) ...)

  "
  (let ((prefix-length (length predicate-prefix)))
    (-<> term->number
      hash-table-alist
      (mapcar (lambda (rule-mapping)
                (destructuring-bind (term . number) rule-mapping
                  (if (equal predicate-prefix
                             (take prefix-length term))
                    number
                    nil)))
              <>)
      (remove nil <>)
      (zdd-set <>))))


(defun make-zdd-reasoner (rules)
  "Turn a set of grounded GDL rules into a ZDD-based reasoner.

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
  (let* ((rules (scully.gdl::normalize-rules rules))
         (roles (find-roles rules)))
    (destructuring-bind (term->number number->term possible happens)
        (scully.terms::integerize-rules rules)
      (with-zdd
        (make-instance 'zdd-reasoner
          :rules rules
          :roles roles
          :possible-forest (build-rule-forest possible)
          :happens-forest (build-rule-forest happens)
          :initial-zdd (zdd-set (find-initial-state rules term->number))
          :legal-zdd (make-predicate-zdd '(ggp-rules::legal) term->number)
          :goal-zdd (make-predicate-zdd '(ggp-rules::goal) term->number)
          :terminal-zdd (make-predicate-zdd '(ggp-rules::terminal) term->number)
          :next-zdd (make-predicate-zdd '(ggp-rules::next) term->number)
          :percept-universes
          (iterate
            (for role :in roles)
            (collect-hash (role (make-universe
                                  (lambda (term)
                                    (equal (take 2 term)
                                           `(ggp-rules::sees ,role)))
                                  term->number))))
          :term->number term->number
          :number->term number->term)))))


;;;; State Conversion ---------------------------------------------------------
(defun convert-next-to-true (reasoner zdd)
  (recursively ((z zdd))
    (ematch z
      ((sink nil) (sink nil))
      ((sink t) (sink t))
      ((node n hi lo)
       (ematch (number-to-term reasoner n)
         (`(ggp-rules::next ,body)
          (zdd-node (term-to-number reasoner `(ggp-rules::true ,body))
                    (recur hi)
                    (recur lo))))))))


;;;; Basic API ----------------------------------------------------------------
(defun number-to-term (reasoner number)
  (gethash number (zr-number->term reasoner)))

(defun term-to-number (reasoner term)
  (gethash term (zr-term->number reasoner)))


(defun iset-to-list (reasoner iset)
  (map-tree (curry #'number-to-term reasoner)
            (scully.zdd::enumerate iset)))

(defun dump-iset (reasoner iset)
  (iterate (for i :from 0)
           (for state :in (iset-to-list reasoner iset))
           (format t "STATE ~D:~%~{    ~S~%~}~2%" i state))
  iset)


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
    (for tree :in (stratum-rule-trees stratum))
    (for (values new-tree new-head) = (funcall function tree))
    (when new-tree (collect new-tree :into new-trees))
    (when new-head (collect new-head :into new-heads))
    (finally (return (values (update-stratum-with stratum new-trees)
                             new-heads)))))

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
    (finally (return (values (make-rule-forest new-strata) heads)))))


;;;; Phase 1: Information Set Traversal
(defun advance-tree (tree term heads)
  "Advance the rule tree up to (but not beyond) `term`.

  Two values will be returned:

  1. Either the resulting rule tree, or NIL if it was advanced down to a sink.
  2. The new head if it was advanced down to a TOP sink, or NIL otherwise.

  "
  (adt:match scully.rule-trees::rule-tree tree
    ((scully.rule-trees::node term% hi lo)
     (if (< term% term)
       (if (member term% heads)
         (advance-tree hi term heads)
         (advance-tree lo term heads))
       (tree-to-result tree)))
    (_ (tree-to-result tree))))

(defun advance-stratum (stratum term &optional heads)
  "Advance the stratum up to (but not beyond) `term`.

  Two values will be returned:

  1. The new stratum (possibly NIL).
  2. Any new heads to add (possible NIL).

  "
  (process-stratum (rcurry #'advance-tree term heads) stratum))

(defun advance-forest (forest term &optional heads)
  "Advance the rule forest up to (but not beyond) `term`.

  Two values will be returned:

  1. The new forest (possibly NIL).
  2. Any new heads to add (possible NIL).

  "
  (process-forest (rcurry #'advance-tree term heads) forest))


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
  ;; At this point we need to see if this rule tree can be applied to the
  ;; current heads.  This function is called in a fixed-point style, and it may
  ;; take multiple iterations for the set of heads to add to stabilize.
  ;;
  ;; This function is called after the trees have been advanced to the lower
  ;; bound of the stratum.  Because we stratified the negation dependencies,
  ;; this means that the only things left at this point are positive terms.
  (recursively ((tree rule-tree))
    (adt:match scully.rule-trees::rule-tree tree
      ;; If we're at a normal node, check if it's in the heads we've added so
      ;; far and recur down the appropriate leg.
      ((scully.rule-trees::node term hi lo)
       (if (member term heads)
         (recur hi)
         (recur lo)))
      ;; If we hit bottom, it just means we can't add this head *yet*.  Return
      ;; the original rule tree.
      ((scully.rule-trees::bottom)
       (tree-to-result rule-tree))
      ;; If we hit top we can add the head.
      ((scully.rule-trees::top _)
       (tree-to-result tree)))))

(defun walk-stratum-positive (stratum heads)
  (iterate
    (for (values new-stratum new-heads) =
         (process-stratum (rcurry #'walk-tree-positive heads) stratum))
    (appending new-heads :into all-new-heads)
    (setf stratum new-stratum
          heads (append heads new-heads))
    (while new-heads)
    (finally (return (values stratum all-new-heads)))))


(defun finalize-heads (forest heads)
  "Finalize the set of heads to add and return the appropriate ZDD."
  (declare (optimize (debug 3) (speed 0)))
  (iterate
    (for stratum :in (rf-strata forest))
    (for lower-bound = (stratum-lower-bound stratum))
    (multiple-value-bind (s h) (advance-stratum stratum lower-bound heads)
      (setf heads (append heads h)
            stratum s))

    ; (pr '--------------------------------)
    ; (pr stratum)
    ; (pr lower-bound)
    ; (pr heads)
    ; (map nil (lambda (rt)
    ;            (draw-rule-tree *r* rt)
    ;            (break))
    ;      (stratum-rule-trees stratum))

    (multiple-value-bind (s h) (walk-stratum-positive stratum heads)
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
  (scully.gdl::read-gdl "gdl/pennies-grounded.gdl"))

; (-<> *rules*
;   (scully.gdl::normalize-rules <>)
;   (scully.terms::integerize-rules <>)
;   ; (nth 2 <>)
;   ; (make-rule-forest <>)
;   ; (scully.terms::print-strata <>)
;   ; (no <>)
;   ; (rest <>)
;   ; (map nil #'print-hash-table <>)
;   )



(defparameter *r* (make-zdd-reasoner *rules*))
; (defparameter *i* (initial-iset *r*))
; (defparameter *j* (initial-iset *r*))

; (with-zdd
;   (-<> *r*
;     (make-iset <>
;                '(
;                  (true (control xplayer))
;                  (true (cell 1 1 B)) (true (cell 1 2 x)) (true (cell 1 3 o))
;                  (true (cell 2 1 B)) (true (cell 2 2 o)) (true (cell 2 3 o))
;                  (true (cell 3 1 x)) (true (cell 3 2 x)) (true (cell 3 3 x))
;                  ))
;     (apply-rule-forest *r* <> (zr-possible-forest *r*))
;     (draw-zdd *r* <>)
;     (dump-iset *r* <>)
;     (no <>)
;     ))


(defun test ()
  (with-zdd
    (-<>
        (zdd-union
          (make-iset *r* '(
                           (true (control oplayer))
                           (true (cell 1 1 o)) (true (cell 1 2 x)) (true (cell 1 3 x))
                           (true (cell 2 1 x)) (true (cell 2 2 o)) (true (cell 2 3 b))
                           (true (cell 3 1 x)) (true (cell 3 2 o)) (true (cell 3 3 b))
                           ))
          (make-iset *r* '(
                           (true (control oplayer))
                           (true (cell 1 1 o)) (true (cell 1 2 x)) (true (cell 1 3 x))
                           (true (cell 2 1 x)) (true (cell 2 2 b)) (true (cell 2 3 o))
                           (true (cell 3 1 x)) (true (cell 3 2 o)) (true (cell 3 3 b))
                           )))
      (apply-rule-forest *r* <> (zr-possible-forest *r*))
      (zdd-join <> (make-iset *r* '((does oplayer (mark 3 3))
                                    (does xplayer noop))))
      (apply-rule-forest *r* <> (zr-happens-forest *r*))
      (zdd-meet <> (zr-next-zdd *r*))
      (dump-iset *r* <>)
      (convert-next-to-true *r* <>)
      (dump-iset *r* <>)
      (no <>)
      ; (draw-zdd *r* <>)
      )))
