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
  (pushnew term (gethash layer layers) :test #'equal)
  (values))


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
  (prog1
      (extract-simple '(ggp-rules::does) :does layers terms)
    ;; In addition to the simple things, we need to make sure we've got
    ;; a corresponding `(does *)` term for any `(legal *)` term.
    (iterate (for term :in terms)
             (match term
               (`(ggp-rules::legal ,@contents)
                (mark layers :does `(ggp-rules::does ,@contents)))
               (_ (collect term))))))


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


;;;; Stratification -----------------------------------------------------------
(defun build-single-layer-dependency-graph (rules)
  (let* ((layer-heads (remove-duplicates (mapcar #'rule-head rules)
                                         :test #'equal)))
    (build-dependency-graph
      rules
      :includep (lambda (body-term)
                  (and (negationp body-term)
                       (member (bare-term body-term) layer-heads
                               :test #'equal))))))

(defun stratify-layer (rules)
  "Stratify a single layer of rules into a list of strata."
  (iterate
    (with dependencies = (build-single-layer-dependency-graph rules))
    (with remaining = rules)
    (until (null remaining))

    (for next-heads = (digraph:leafs dependencies))
    (when (null next-heads)
      (error "Cycle in negations detected!"))

    (for stratum = (remove-if-not (lambda (head)
                                    (member head next-heads :test #'equal))
                                  remaining
                                  :key #'rule-head))
    (collect stratum)

    (setf remaining (set-difference remaining stratum :test #'equal))

    (dolist (head next-heads)
      (digraph:remove-vertex dependencies head))))


;;;; Intra-Layer Ordering -----------------------------------------------------
(defun sort-and-flatten-strata (strata)
  "Take `strata` and turn it into a sorted list of rule heads."
  (flet ((heads-in-stratum (stratum)
           (-<> stratum
             (mapcar #'rule-head <>)
             (remove-duplicates <> :test #'equal))))
    (-<> strata
      (mapcar #'heads-in-stratum <>)
      (flatten-once <>))))

(defun extract-rules-for-layer (layers rules layer-key)
  "Return all rules for the given layer.

  `layers` should be a table of terms to layer keys.
  `rules` should be the list of all rules.

  "
  (iterate (for head :in (gethash layer-key layers))
           (for matching-rules = (remove-if-not (curry #'equal head) rules
                                                :key #'rule-head))
           (appending matching-rules)))

(defun order-layer (layer-terms layer-strata)
  "Return a list of all terms in the layer in the proper order."
  (let* ((strata-terms (sort-and-flatten-strata layer-strata))
         (leftovers (set-difference layer-terms strata-terms :test #'equal)))
    (append leftovers strata-terms)))


(defun sort-base-layer (base-terms happens-terms)
  "Return a fresh list of the base terms, sorted correctly."
  ;; The base layer needs to be sorted in the same order as the `next` terms
  ;; appear in the `happens` layer, because we want to be able to convert a ZDD
  ;; of `(next ...)` terms into the next information set in a single pass.
  (sort (copy-seq base-terms) #'<
        :key (trivia:lambda-match
               (`(ggp-rules::true ,body)
                (or (position `(ggp-rules::next ,body) happens-terms
                              :test #'equal)
                    -1))
               (_ -2))))


(defun order-terms (rules)
  "Find a linear ordering of all terms in `rules`.

  Returns three values:

  * A list of all terms, in order
  * A list of the `:possible` strata
  * A list of the `:happens` strata

  "
  ;; Start by partitioning the terms into the layers.
  (let* ((dependencies (build-dependency-graph rules))
         (layers (partition-rules dependencies rules)))
    ;; Then we need to stratify the possible & happens layers.
    (let* ((possible-rules (extract-rules-for-layer layers rules :possible))
           (happens-rules (extract-rules-for-layer layers rules :happens))
           (possible-strata (stratify-layer possible-rules))
           (happens-strata (stratify-layer happens-rules)))
      ;; We order the individual layers.
      (let* ((possible-terms (order-layer (gethash :possible layers) possible-strata))
             (happens-terms (order-layer (gethash :happens layers) happens-strata))
             (base-terms (sort-base-layer (gethash :base layers) happens-terms))
             (does-terms (gethash :does layers)))
        ;; And finally we concatenate the layer orderings into one bigass order:
        ;; base < possible < does < happens
        (values (append base-terms possible-terms does-terms happens-terms)
                possible-strata
                happens-strata)))))


;;;; Integerization -----------------------------------------------------------
(defun integerize-term (term->number term)
  (match term
    (`(ggp-rules::not ,body)
     `(ggp-rules::not ,(gethash body term->number)))
    (_ (gethash term term->number))))

(defun integerize-rule (term->number rule)
  (mapcar (curry #'integerize-term term->number) rule))

(defun integerize-stratum (term->number stratum)
  (mapcar (curry #'integerize-rule term->number) stratum))


(defun integerize-rules (rules)
  "Integerize `rules`.

  `rules` should be a (normalized) list of rules.

  A list of 4 values will be returned:

  * The term->number hash table
  * The number->term hash table
  * A list of the `:possible` strata
  * A list of the `:happens` strata

  "
  (let ((term->number (make-hash-table :test #'equal))
        (number->term (make-hash-table)))
    (multiple-value-bind (terms possible happens)
        (order-terms rules)
      ;; Generate the mapping tables
      (iterate (for i :from 0)
               (for term :in terms)
               (setf (gethash i number->term) term
                     (gethash term term->number) i))
      ;; Return the tables and the integerized rules
      (list term->number
            number->term
            (mapcar (curry #'integerize-stratum term->number) possible)
            (mapcar (curry #'integerize-stratum term->number) happens)))))


;;;; Scratch ------------------------------------------------------------------
(defparameter *rules*
  '(
    (ggp-rules::<= x (ggp-rules::true a))
    (ggp-rules::<= x (ggp-rules::true b))
    (ggp-rules::<= (ggp-rules::next a)
                   (ggp-rules::true dangus))
    (ggp-rules::<= z (ggp-rules::does c x))
    (ggp-rules::<= (ggp-rules::next b)
                   (ggp-rules::not z))
    ))

(defun print-strata (strata)
  (iterate (for i :from 0)
           (for stratum :in strata)
           (format t "STRATUM ~D:~%~{    ~S~%~}~2%"
                   i stratum)))

(-<> *rules*
  (normalize-rules <>)
  (integerize-rules <>)
  ; (nth 2 <>)
  ; (print-strata <>)
  (no <>)
  ; (rest <>)
  ; (map nil #'print-hash-table <>)
  )
