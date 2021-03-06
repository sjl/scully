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
  (let ((universe (make-array (1+ (hash-table-count term->number))
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
   legal-zdds
   goal-zdds
   terminal-zdd
   next-zdd
   percept-universes
   does-universes
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
  (let* ((rules (-<> rules
                  scully.gdl::normalize-rules
                  scully.gdl::split-rules
                  ))
         (roles (find-roles rules)))
    (destructuring-bind (term->number number->term possible happens)
        (scully.terms::integerize-rules rules)
      ;; (print-hash-table number->term)
      (with-zdd
        (make-instance 'zdd-reasoner
          :rules rules
          :roles roles
          :possible-forest (build-rule-forest possible)
          :happens-forest (build-rule-forest happens)
          :initial-zdd (zdd-set (find-initial-state rules term->number))
          :legal-zdds (iterate
                        (for role :in roles)
                        (collect-hash
                          (role (make-predicate-zdd `(ggp-rules::legal ,role)
                                                    term->number))))
          :goal-zdds (iterate
                       (for role :in roles)
                       (collect-hash
                         (role (make-predicate-zdd `(ggp-rules::goal ,role)
                                                   term->number))))
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
          :does-universes
          (iterate
            (for role :in roles)
            (collect-hash (role (make-universe
                                  (lambda (term)
                                    (equal (take 2 term)
                                           `(ggp-rules::does ,role)))
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


;;;; Sprouting ----------------------------------------------------------------
(defun build-role-move-zdd (next-zdd role-moves)
  (reduce (lambda (prev move)
            (zdd-node move next-zdd prev))
          (sort role-moves #'>)
          :initial-value (sink nil)))

(defun sprout-extend% (legal-moves-by-role)
  (reduce #'build-role-move-zdd legal-moves-by-role :initial-value (sink t)))

(defun sprout-extend (reasoner legal-moves role chosen-move)
  (sprout-extend%
    (-<> legal-moves
      (group-by #'second <>) ; go role by role
      (progn (setf (gethash role <>)
                   (list `(ggp-rules::does ,role ,chosen-move)))
             <>)
      hash-table-values
      (sort <> #'scully.terms::symbol< ; sort by role
            :key (lambda (moves)
                   (second (first moves))))
      nreverse ; go bottom up
      (mapcar (lambda (role-moves) ; convert to integers
                (mapcar (curry #'term-to-number reasoner) role-moves))
              <>))))


(defun sprout-traverse (reasoner iset role chosen-move)
  (recursively ((z iset)
                (legal-moves '()))
    (ematch z
      ;; If we hit the empty sink, just bail, there's nothing to add on to.
      ((sink nil) (sink nil))
      ;; If we hit the unit sink we're ready to sprout off the `does`es.
      ((sink t) (sprout-extend reasoner legal-moves role chosen-move))
      ;; Otherwise we're at a node.
      ((node n hi lo)
       (match (number-to-term reasoner n)
         ;; If the term is a legal move, we add it into the list when recuring
         ;; down the hi branch.
         (`(ggp-rules::legal ,role ,move)
          (zdd-node n
                    (recur hi (cons `(ggp-rules::does ,role ,move) legal-moves))
                    (recur lo legal-moves)))
         ;; Otherwise we just recur down both.
         (_ (zdd-node n (recur hi legal-moves) (recur lo legal-moves))))))))

(defun sprout (reasoner iset role chosen-move)
  "Sprout off child states for each state in `iset` for all legal moves."
  ;; Given an information set, we want to compute a new information set with all
  ;; possible combinations of `does` added, which we'll narrow down later once
  ;; we get the percepts back from the server.
  ;;
  ;; This is going to happen right after we calculate the possible layer, and
  ;; will result in the appropriate things in the does layer being added.
  ;;
  ;; To do this we'll traverse the ZDD recursively, accumulating a list of all
  ;; legal moves for each player as we go.  Once we hit a sink we'll tack on
  ;; a child ZDD of all the possible combos.
  (sprout-traverse reasoner iset role chosen-move))


;;;; Basic API ----------------------------------------------------------------
(defun number-to-term (reasoner number)
  (gethash number (zr-number->term reasoner)))

(defun term-to-number (reasoner term)
  (gethash term (zr-term->number reasoner)))


(defun iset-to-list (reasoner iset)
  (let ((contents (scully.zdd::zdd-enumerate iset)))
    (if (null contents)
      nil
      (map-tree (curry #'number-to-term reasoner) contents))))

(defun dump-iset (reasoner iset)
  (iterate (for i :from 1)
           (for state :in (iset-to-list reasoner iset))
           (let ((*package* (find-package :ggp-rules)))
             (format t "STATE ~D:~%~{    ~S~%~}~2%" i state)))
  iset)

(defun dump-rule-tree (reasoner term)
  (draw-rule-tree
    reasoner
    (iterate
      finder
      (with target = (term-to-number reasoner term))
      (for nf :from 0)
      (for forest :in (list (zr-possible-forest reasoner)
                            (zr-happens-forest reasoner)))
      (iterate (for stratum :in (rf-strata forest))
               (for ns :from 0)
               (iterate (for rule :in (stratum-rule-trees stratum))
                        (when (= target (scully.rule-trees::head rule))
                          (pr 'forest nf 'stratum ns)
                          (return-from finder rule)))))))


(defun initial-iset (reasoner)
  "Return the initial information set of the game."
  (zr-initial-zdd reasoner))


(defun terminalp (reasoner iset)
  "Return whether the given information set is a terminal state."
  (-<> iset
    (zdd-meet <> (zr-terminal-zdd reasoner))
    zdd-unit-p
    not))

(defun legal-moves-for (reasoner iset role)
  (-<> iset
    (zdd-meet <> (gethash role (zr-legal-zdds reasoner)))
    zdd-arbitrary-member
    (mapcar (curry #'number-to-term reasoner) <>)
    (mapcar #'third <>)))

(defun goal-values-for (reasoner iset role)
  (-<> iset
    (zdd-meet <> (gethash role (zr-goal-zdds reasoner)))
    enumerate
    (mapcar #'first <>)
    remove-duplicates
    (mapcar (curry #'number-to-term reasoner) <>)
    (mapcar #'third <>)))


(defun roles (reasoner)
  (zr-roles reasoner))


(defun filter-iset-for-percepts (reasoner iset role percepts)
  (let* ((universe (gethash role (zr-percept-universes reasoner)))
         (full-percepts (iterate (for p :in percepts)
                                 (collect `(ggp-rules::sees ,role ,p))))
         (percepts (mapcar (curry #'term-to-number reasoner) full-percepts)))
    (zdd-match iset percepts universe)))

(defun filter-iset-for-move (reasoner iset role move)
  (let ((universe (gethash role (zr-does-universes reasoner)))
        (moves (list (term-to-number reasoner `(ggp-rules::does ,role ,move)))))
    (zdd-match iset moves universe)))


(defun compute-next-iset (reasoner iset)
  (-<> iset
    (zdd-meet <> (zr-next-zdd reasoner))
    (convert-next-to-true reasoner <>)))


(defun apply-possible (reasoner iset)
  (apply-rule-forest reasoner iset (zr-possible-forest reasoner)))

(defun apply-happens (reasoner iset)
  (apply-rule-forest reasoner iset (zr-happens-forest reasoner)))


;;;; Drawing ------------------------------------------------------------------
(defun label (reasoner n)
  (let ((*package* (find-package :ggp-rules)))
    (-<> n
      (number-to-term (if (eq t reasoner)
                        *reasoner*
                        reasoner)
                      <>)
      ;; (format nil "~D ~S" n <>)
      (format nil "~S" <>))))

(defun draw-zdd (reasoner zdd)
  (scully.graphviz::draw-zdd zdd :label-fn (curry #'label reasoner)))

(defun draw-rule-tree (reasoner rule-tree &optional (filename "rule-tree.png"))
  (scully.graphviz::draw-rule-tree rule-tree
                                   :label-fn (curry #'label reasoner)
                                   :filename filename))


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


;;;; Stats --------------------------------------------------------------------
(defun rule-forest-size (forest)
  (iterate
    (for stratum :in (rf-strata forest))
    (appending (mapcar #'scully.rule-trees::rule-tree-size
                       (stratum-rule-trees stratum)))))

(defun reasoner-rule-tree-sizes (reasoner)
  (append (rule-forest-size (zr-possible-forest reasoner))
          (rule-forest-size (zr-happens-forest reasoner))))


;;;; Scratch ------------------------------------------------------------------
(defparameter *i* nil)
(defparameter *r* nil)

; Data columns: X Min 1stQuartile Median 3rdQuartile Max BoxWidth Titles

(defun ground-gdl-size (gdl)
  (iterate (for rule :in gdl)
           (summing (if (and (consp rule)
                             (equal (first rule) 'ggp-rules::<=))
                      (1- (length rule))
                      1))))

(defun run-test (game-name shuffle?)
  (let* ((scully.terms::*shuffle-variables* shuffle?)
         (gdl (scully.gdl::read-gdl (format nil "gdl/~(~A~)-grounded.gdl" game-name)))
         (gdl-size (ground-gdl-size gdl))
         (r (make-zdd-reasoner gdl))
         (sizes (reasoner-rule-tree-sizes r)))
    (values sizes gdl-size)))


(defun percentile (sorted-numbers p)
  (nth (truncate (* (/ p 100) (length sorted-numbers)))
       sorted-numbers))

(defun percentiles (numbers)
  (let ((sorted (sort numbers #'<)))
    (values (percentile sorted 5)
            (percentile sorted 25)
            (percentile sorted 50)
            (percentile sorted 75)
            (percentile sorted 95))))


(defun run-shuffle-test (x game-name &optional (iterations 10))
  (with-open-file (data "data-shuffling-rule-trees"
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (let* ((unshuffled-size (apply #'+ (run-test game-name nil)))
           (results (iterate
                      (for i :from 1 :to iterations)
                      (princ i) (princ #\space) (finish-output)
                      (for shuffled-size = (apply #'+ (run-test game-name t)))
                      (collect (/ shuffled-size unshuffled-size 1.0)))))
      (fresh-line)
      (multiple-value-bind (p5 p25 p50 p75 p95)
          (percentiles results)
        (format data "~D ~,5F ~,5F ~,5F ~,5F ~,5F 0.4 ~A~%"
                x p5 p25 p50 p75 p95 game-name)))))

(defun run-shuffle-tests (iterations)
  (iterate
    (for game :in '(montyhall meier mastermind448 transit vis_pacman3p latenttictactoe stratego))
    (pr game)
    (for x :from 1)
    (run-shuffle-test x game iterations)))


(defun run-basic-test (game-name)
  (with-open-file (data "data-rule-tree-sizes"
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (multiple-value-bind (sizes gdl-size)
        (run-test game-name nil)
      (format data "~A ~D ~D ~D~%"
              game-name
              gdl-size
              (length sizes)
              (apply #'+ sizes)))))

(defun run-basic-tests ()
  (iterate
    (for game :in '(montyhall meier mastermind448 transit vis_pacman3p latenttictactoe stratego))
    (pr game)
    (run-basic-test game)))

;; (run-shuffle-tests 50)
;; (run-basic-tests)
