(in-package :scully.terms)
(in-readtable :fare-quasiquote)

;;;; Overview -----------------------------------------------------------------
;;; We start with a set of grounded rules like: ((next bar) x y (true foo)).
;;;
;;; We need to turn map each term to a unique integer, making sure that they end
;;; up ordered how we want them.
;;;
;;; Our desired ordering has a few "layers":
;;;
;;; Base: (true ...)
;;; Does: (does ...)
;;; Possible: (legal ...)
;;;           terminal
;;;           (goal ...)
;;;           anything that depends on only these or lower
;;; Happens: (next ...)
;;;          (sees ...)
;;;           anything that depends on only these or lower
;;;
;;; Within each layer the ordering is arbitrary, EXCEPT that if a rule X that
;;; relies on the negation of a rule Y, then Y must come before X.


;;;; Dependency Graph ---------------------------------------------------------
(defun build-dependency-graph (rules &key includep)
  "Build a dependency graph of the given `rules`.

  All rule heads will be included as vertices.

  A head will have a dependency on each of its body terms for which
  `(funcall includep term)` returns `t`.  If `includep` is `nil` all
  dependencies will be included.

  Only body terms upon which there is a dependency will be included in the graph
  -- if a body term is discarded by `includep` there will be no vertex for it.

  "
  (let ((graph (digraph:make-digraph :test #'equal)))
    (labels
        ((mark-dependency (head dep)
           (digraph:insert-vertex graph dep)
           (digraph:insert-edge graph head dep))
         (mark-dependencies (head body)
           (digraph:insert-vertex graph head)
           (iterate (for b :in body)
                    (when (or (null includep)
                              (funcall includep b))
                      (mark-dependency head (bare-term b))))))
      (iterate (for (head . body) :in rules)
               (mark-dependencies head body)))
    graph))


;;;; Layer Partitioning -------------------------------------------------------
;;; We want to partition the terms of the rules into layers.  The result will be
;;; a hash table containing two types of entries, for convenience:
;;;
;;;   term          -> layer keyword
;;;   layer keyword -> list of terms in the layer

(defun mark (layers layer term)
  (setf (gethash term layers) layer)
  (pushnew term (gethash layer layers) :test #'equal))


(defun extract-simple (predicates layer layers terms)
  "Extract simple terms for a given `layer` from `terms`.

  Extract the terms with predicates in `predicates` and mark them appropriately
  in the `layers` hash table.

  Returns a list of remaining terms.

  "
  (iterate (for term :in terms)
           (if (member (term-predicate term) predicates)
             (mark layers layer term)
             (collect term))))


(defun extract-base (layers terms)
  (let ((terms (extract-simple '(ggp-rules::true
                                 ggp-rules::role)
                               :base layers terms)))
    ;; In addition to the simple things, we need to make sure we've got
    ;; a corresponding `(true *)` term for any `(init *)` term.
    (iterate (for term :in terms)
             (match term
               (`(ggp-rules::init ,contents)
                (mark layers :base `(ggp-rules::true ,contents))
                (mark layers :base term))
               (_ (collect term))))))

(defun extract-does (layers terms)
  (extract-simple '(ggp-rules::does) :does layers terms))


(defun extract-possible% (layers dependencies terms)
  (labels ((find-dependencies (term)
             "Return the layers of each of `term`s dependencies."
             (mapcar (rcurry #'gethash layers)
                     (digraph:successors dependencies term)))
           (find-eligible (terms)
             "Find terms that depend only on things in `:base`/`:possible`."
             (iterate (for term :in terms)
                      (for unmet = (set-difference (find-dependencies term)
                                                   '(:base :possible)))
                      (when (null unmet)
                        (collect term)))))
    (iterate
      (with remaining = terms)
      (for next = (find-eligible remaining))
      (while next)
      (mapcar (curry #'mark layers :possible) next)
      (zapf remaining (set-difference % next :test #'equal))
      (finally (return remaining)))))

(defun extract-possible (layers dependencies terms)
  ;; At this point we've got the :base and :does layers finished.  We then
  ;; extract the simple things for the :possible layer.
  ;;
  ;; Once we've done this, rules that depend on ONLY things in the
  ;; :base/:possible layers can also be extracted.
  (-<> terms
    (extract-simple '(ggp-rules::legal
                      ggp-rules::goal
                      ggp-rules::terminal)
                    :possible layers <>)
    (extract-possible% layers dependencies <>)))


(defun extract-early-happens (layers terms)
  ;; We need to extract these early because we don't want them to get included
  ;; in the `:possible` layer if they don't depend on anything.
  (extract-simple '(ggp-rules::sees
                    ggp-rules::next)
                  :happens layers terms))

(defun extract-final-happens (layers terms)
  ;; Everything left at the end must be in the `:happens` layer.
  (mapcar (curry #'mark layers :happens) terms)
  nil)


(defun partition-rules (dependencies rules)
  (let* ((terms (-<> rules
                  flatten-once
                  (mapcar #'bare-term <>)
                  (remove-duplicates <> :test #'equal)))
         (layers (make-hash-table :test #'equal)))
    (-<> terms
      (extract-base layers <>)
      (extract-does layers <>)
      (extract-early-happens layers <>) ; ugh
      (extract-possible layers dependencies <>)
      (extract-final-happens layers <>))
    layers))


;;;; Intra-Layer Ordering -----------------------------------------------------
(defun sort-layer (negation-dependencies terms)
  ;; We sort a layer by creating a digraph of only the terms in that layer,
  ;; adding all negation dependencies between them, and topologically sorting.
  (let ((layer (digraph:make-digraph :test #'equal)))
    (flet ((add-dependencies (term)
             (iterate
               (for dep :in (digraph:successors negation-dependencies term))
               ;; We only care about dependencies where both the head and body
               ;; are in THIS layer -- we don't care about a dependency on an
               ;; earlier layer.
               (when (digraph:contains-vertex-p layer dep)
                 (digraph:insert-edge layer term dep)))))
      (mapc (curry #'digraph:insert-vertex layer) terms)
      (mapc #'add-dependencies terms))
    ;; todo: fix the roots/cycles issue in cl-digraph
    (digraph:topological-sort layer)))

(defun order-terms (rules)
  "Find a linear ordering of all terms in `rules`.

  Returns two values: a list of the terms, in order, and the final layer hash
  table.

  "
  (let* ((dependencies (build-dependency-graph rules))
         (negation-dependencies (build-dependency-graph rules
                                                        :includep #'negationp))
         (layers (partition-rules dependencies rules)))
    (let ((base (gethash :base layers))
          (does (gethash :does layers))
          (possible (sort-layer negation-dependencies (gethash :possible layers)))
          (happens (sort-layer negation-dependencies (gethash :happens layers))))
      ;; base < possible < does < happens
      (values (append base possible does happens)
              layers))))


;;;; Integerization -----------------------------------------------------------
(defun integerize-term (term->number term)
  (match term
    (`(ggp-rules::not ,body)
     `(ggp-rules::not ,(gethash body term->number)))
    (_ (gethash term term->number))))

(defun integerize-rule (term->number rule)
  (mapcar (curry #'integerize-term term->number) rule))

(defun integerize-rules (rules)
  "Integerize `rules`.

  `rules` should be a (normalized) list of rules.

  A list of 3 hash tables will be returned:

    (term->number number->term rule-layers)

  "
  (let ((term->number (make-hash-table :test #'equal))
        (number->term (make-hash-table))
        (rule-layers (make-hash-table)))
    (multiple-value-bind (terms layers)
        (order-terms rules)
      (iterate (for i :from 0)
               (for term :in terms)
               (setf (gethash i number->term) term
                     (gethash term term->number) i))
      (iterate (for rule :in rules)
               (for layer = (gethash (rule-head rule) layers))
               (push (integerize-rule term->number rule)
                     (gethash layer rule-layers))))
    (list term->number number->term rule-layers)))


;;;; Stratification -----------------------------------------------------------
(defun build-single-layer-dependency-graph (rules)
  (let* ((layer-heads (remove-duplicates (mapcar #'rule-head rules))))
    (build-dependency-graph
      rules
      :includep (lambda (b)
                  (and (negationp b)
                       (member (bare-term b) layer-heads))))))

(defun stratify-layer (rules)
  "Stratify a single layer of rules into a list of strata."
  (iterate
    (with dependencies = (build-single-layer-dependency-graph rules))
    ; (initially (digraph.dot:draw dependencies))
    (with remaining = rules)
    (until (null remaining))

    (for next-heads = (digraph:leafs dependencies))
    (when (null next-heads)
      (error "Cycle in negations detected!"))

    (for stratum = (remove-if-not (rcurry #'member next-heads)
                                  remaining
                                  :key #'rule-head))
    (collect stratum)

    (setf remaining (set-difference remaining stratum))
    (mapc (curry #'digraph:remove-vertex dependencies) next-heads)))

