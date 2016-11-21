(in-package :scully.zdd)


;;;; Utils --------------------------------------------------------------------
(defun gcprint (thing &rest args)
  (let ((*print-circle* t))
    (apply #'print
           (prog1 thing
                  (tg:gc :full t :verbose t))
           args)))

(defun never (val)
  (declare (ignore val))
  (values))

(defun print-through (function-or-object val)
  (if (functionp function-or-object)
    (pr (funcall function-or-object val))
    (pr function-or-object))
  val)

(defun mapprint-through (function val)
  "Calling `function` on each item in `val` and print the result, return `val`."
  (mapc #'pr (funcall function val))
  val)


;;;; Bullshit -----------------------------------------------------------------
;;; The BDD lib defines a pattern for `node` but not for `leaf`.  It's awkward
;;; to have two different syntaxes.  But if we define a pattern for `leaf` and
;;; then try to reload the BDD lib it will explode, because the lib uses the
;;; second syntax!  So basically we'll just rename "leaf" to "sink" and get on
;;; with our lives.  Christ.
(defpattern sink (&optional content)
  `(structure leaf :content ,content))

(defun sink (thing)
  (leaf thing))

(deftype sink () 'leaf)


;;;; ZDDs ---------------------------------------------------------------------
(defparameter *cache*
  (tg:make-weak-hash-table :weakness :value :test #'equalp))


(defmacro with-zdd (&body body)
  "Execute `body` with the ZDD settings properly initialized."
  `(with-odd-context (:operation #'zdd-apply :node-cache *cache*)
    ,@body))


(defun enumerate (zdd)
  "Return a list of all members of `zdd`."
  (ematch zdd
    ((sink nil) nil)
    ((sink t) (list nil))
    ((node variable hi lo)
     (append (mapcar (curry #'cons variable) (enumerate hi))
             (enumerate lo)))))


(defun zdd-count (zdd)
  "Return the number of members of `zdd`."
  (ematch zdd
    ((sink nil) 0)
    ((sink t) 1)
    ((node _ hi lo) (+ (zdd-count hi)
                       (zdd-count lo)))))

(defun zdd-size (zdd)
  "Return the number of unique nodes in `zdd`."
  (let ((seen (make-hash-table :test 'eq)))
    (recursively ((zdd zdd))
      (ematch zdd
        ((sink) (setf (gethash zdd seen) t))
        ((node _ hi lo)
         (when (not (gethash zdd seen))
           (setf (gethash zdd seen) t)
           (recur lo)
           (recur hi)))))
    (hash-table-count seen)))


(defun pick-random (a a-weight b b-weight)
  (if (< (random (+ a-weight b-weight))
         a-weight)
    a
    b))

(defun zdd-random-member (zdd)
  "Select a random member of `zdd`."
  (ematch zdd
    ((sink nil) (error "No elements to choose from!"))
    ((sink t) nil)
    ((node var hi lo)
     (let ((hi-weight (zdd-count hi)) ; todo: memoize these
           (lo-weight (zdd-count lo)))
       (if (< (random (+ lo-weight hi-weight))
              lo-weight)
         (zdd-random-member lo)
         (cons var (zdd-random-member hi)))))))


(defun unit-patch (zdd)
  "Ensure the empty set is a member of `zdd`."
  (ematch zdd
    ((sink t) zdd)
    ((sink nil) (sink t))
    ((node variable hi lo)
     (zdd-node variable hi (unit-patch lo)))))


(defun zdd-set (elements)
  "Return a ZDD with a single member (which contains `elements`)."
  (make-set elements))


(defun zdd-union% (a b)
  (ematch* (a b)
    (((node) (sink)) (zdd-union% b a))

    (((sink nil) b) b)
    (((sink t) b) (unit-patch b))

    (((node var-a hi-a lo-a)
      (node var-b hi-b lo-b))
     (cond
       ((< var-a var-b) (zdd-node var-a hi-a (zdd-union% lo-a b)))
       ((> var-a var-b) (zdd-node var-b hi-b (zdd-union% lo-b a)))
       ((= var-a var-b) (zdd-node var-a
                                  (zdd-union% hi-a hi-b)
                                  (zdd-union% lo-a lo-b)))))))
(defun zdd-union (&rest zdds)
  "Return the union of ZDDs: {α | α ∈ Z₁ or α ∈ Z₂}."
  (if zdds
    (reduce #'zdd-union% zdds)
    (sink nil)))

(defun zdd-intersection% (a b)
  (ematch* (a b)
    (((node) (sink)) (zdd-intersection% b a))

    (((sink nil) _) (sink nil))
    ((_ (sink nil)) (sink nil))

    (((sink t) (sink _)) b)
    (((sink t) (node _ _ lo)) (zdd-intersection% a lo))

    (((node var-a hi-a lo-a)
      (node var-b hi-b lo-b))
     (cond
       ((< var-a var-b) (zdd-intersection% lo-a b))
       ((> var-a var-b) (zdd-intersection% lo-b a))
       ((= var-a var-b) (zdd-node var-a
                                  (zdd-intersection% hi-a hi-b)
                                  (zdd-intersection% lo-a lo-b)))))))

(defun zdd-intersection (&rest zdds)
  "Return the intersection of ZDDs: {α | α ∈ Z₁ and α ∈ Z₂}."
  (if zdds
    (reduce #'zdd-intersection% zdds)
    (sink nil)))

(defun zdd-join% (a b)
  (ematch* (a b)
    (((sink nil) _) (sink nil))
    ((_ (sink nil)) (sink nil))

    (((sink t) b) b)
    ((a (sink t)) a)

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
  "Return the relational join of ZDDs: {α ∪ β | α ∈ Z₁ and β ∈ Z₂}."
  (if zdds
    (reduce #'zdd-join% zdds)
    (sink nil)))

(defun zdd-meet% (a b)
  (ematch* (a b)
    (((sink nil) _) (sink nil))
    ((_ (sink nil)) (sink nil))

    (((sink t) _) (sink t))
    ((_ (sink t)) (sink t))

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
  "Return the relational meet of ZDDs: {α ∩ β | α ∈ Z₁ and β ∈ Z₂}."
  (if zdds
    (reduce #'zdd-meet% zdds)
    (sink nil)))


(defun zdd-family (&rest sets)
  "Return a ZDD that contains each of the given `sets` as members."
  (reduce #'zdd-union (mapcar #'zdd-set sets)))


(defun zdd-keep-supersets-of% (zdd set)
  (ematch* (zdd set)
    ((_ nil) zdd)
    (((sink) _) (sink nil))
    (((node var hi lo) (list* el remaining))
     (cond
       ((= var el) (zdd-node var
                             (zdd-keep-supersets-of% hi remaining)
                             (sink nil)))
       ((< var el) (zdd-node var
                             (zdd-keep-supersets-of% hi set)
                             (zdd-keep-supersets-of% lo set)))
       ((> var el) (sink nil))))))

(defun zdd-keep-supersets-of (zdd set)
  "Return a ZDD of all supersets of `set` in `zdd`: {α | α ∈ Z and α ⊇ S}."
  (zdd-keep-supersets-of% zdd (sort set #'<)))


(defun zdd-remove-supersets-of% (zdd set)
  (ematch* (zdd set)
    ((_ nil) (sink nil))
    (((sink) _) zdd)
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
  "Return a ZDD of all non-supersets of `set` in `zdd`: {α | α ∈ Z and α ⊉ S}."
  (zdd-remove-supersets-of% zdd (sort set #'<)))


(defun zdd-keep-avoiders-of% (zdd set)
  (ematch* (zdd set)
    ((_ nil) zdd)
    (((sink) _) zdd)
    (((node var hi lo) (list* el remaining))
     (cond
       ((= var el) (zdd-keep-avoiders-of% lo remaining))
       ((< var el) (zdd-node var
                             (zdd-keep-avoiders-of% hi set)
                             (zdd-keep-avoiders-of% lo set)))
       ((> var el) (zdd-keep-avoiders-of% zdd remaining))))))

(defun zdd-keep-avoiders-of (zdd set)
  "Return a ZDD of members of `zdd` avoiding `set`: {α | α ∈ Z and α ∩ S = ø}."
  (zdd-keep-avoiders-of% zdd (sort set #'<)))


(defun zdd-match% (zdd set lower-bound upper-bound)
  (recursively ((zdd zdd) (set set))
    (ematch zdd
      ;; If Z = ∅, there are no candidates for matching.
      ((sink nil) (sink nil))

      ;; If Z = {∅}, the only set ∅ can match is the empty set.
      ((sink t) (if set
                  (sink nil)
                  (sink t)))

      ;; Otherwise Z is a real node.
      ((node var hi lo)
       (cond
         ;; If we're below the lower bound of the universe, just recur down.
         ((< var lower-bound) (zdd-node var
                                        (recur hi set)
                                        (recur lo set)))

         ;; If we're above the upper bound of the universe, we're never gonna
         ;; see anything more we might need to match.
         ;;
         ;; If our target set is empty, that's perfect.  But if it's NOT empty,
         ;; we're never gonna satisfy it.
         ((> var upper-bound) (if set
                                (sink nil)
                                zdd))

         ;; Otherwise Z's var is within the universe.
         (t (ematch set
              ;; If our target is empty, only the lo branch of Z can possibly
              ;; match.
              (nil (recur lo set))

              ;; Otherwise we've got a target element.  Almost there!
              ((list* element remaining)
               (cond
                 ;; If we're below the target element, we recur down the lo
                 ;; branch because the hi branch contains something unwanted.
                 ((< var element) (recur lo set))
                 ;; If we're above the target element, we can never match.
                 ((> var element) (sink nil))
                 ;; Otherwise, we recur down the hi branch with the rest of our
                 ;; target (the lo branch is always missing this element).
                 ((= var element) (zdd-node var
                                            (recur hi remaining)
                                            ;        jeeeeeeeesus
                                            (sink nil))))))))))))

(defun zdd-match (zdd set lower-bound upper-bound)
  "Return a ZDD of members that exactly match `set` within the universe.

  {α | α ∈ Z and α ∩ U = S}

  "
  (zdd-match% zdd (sort set #'<) lower-bound upper-bound))


(defun apply-rule-tree (zdd rule-tree head-bound)
  "Apply the logical rules in `rule-tree` to the sets in `zdd`.

  `zdd` is assumed to contain sets of logical axioms.  This function will update
  each of these sets to add any rule heads derivable from the axioms in the set.

  "
  (recursively ((zdd zdd)
                (rule-tree rule-tree))
    (ematch* (zdd rule-tree)
      ;; If Z = ∅ there are no sets to cons heads onto, bail.
      (((sink nil) _) zdd)

      ;; If R = ∅ or {∅} we've bottomed out of the rule tree and there are no
      ;; heads to cons, we're done.
      ((_ (sink)) zdd)

      ;; If we've passed the head boundary on the rule tree side then we're done
      ;; filtering and just need to cons in all the heads.
      ((_ (guard (node var _ _)
                 (>= var head-bound)))
       (zdd-join zdd rule-tree))

      ;; If Z = {∅} we might have some heads we need to cons later in the rule
      ;; tree, so recur down the lo side of it.
      (((sink t) (node _ _ lo))
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

; (destructuring-bind (term->number number->term layers)
;     (scully.terms::integerize-rules *rules*)
;   ; (print-hash-table layers)
;   (with-zdd
;     (-<> (gethash :happens layers)
;       ; (mapprint-through #'pr <>)
;       (make-rule-tree <>)
;       ; (draw <> :unique-sinks nil :unique-nodes t
;       ;       :label-fn (lambda (n)
;       ;                   (aesthetic-string (gethash n number->term))))
;       ; (print-through #'zdd-size <>)
;       (never <>))))
