(in-package :scully.zdd)


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


(defun zdd-enumerate (zdd)
  "Return a list of all members of `zdd`."
  (ematch zdd
    ((sink nil) nil)
    ((sink t) (list nil))
    ((node variable hi lo)
     (append (mapcar (curry #'cons variable) (zdd-enumerate hi))
             (zdd-enumerate lo)))))


(defun zdd-empty-p (zdd)
  (ematch zdd
    ((sink nil) t)
    ((sink t) nil)
    ((node _ _ _) nil)))

(defun zdd-unit-p (zdd)
  (ematch zdd
    ((sink nil) nil)
    ((sink t) t)
    ((node _ _ _) nil)))

(defun zdd-count (zdd)
  "Return the number of members of `zdd`."
  (ematch zdd
    ((sink nil) 0)
    ((sink t) 1)
    ((node _ hi lo) (+ (zdd-count hi)
                       (zdd-count lo)))))

(defun zdd-node-count (zdd)
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


(defun-ematch* zdd-union% (a b)
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
                                (zdd-union% lo-a lo-b))))))
(defun zdd-union (&rest zdds)
  "Return the union of ZDDs: {α | α ∈ Z₁ or α ∈ Z₂}."
  (if zdds
    (reduce #'zdd-union% zdds)
    (sink nil)))


(defun-ematch* zdd-intersection% (a b)
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
                                (zdd-intersection% lo-a lo-b))))))

(defun zdd-intersection (&rest zdds)
  "Return the intersection of ZDDs: {α | α ∈ Z₁ and α ∈ Z₂}."
  (if zdds
    (reduce #'zdd-intersection% zdds)
    (sink nil)))


(defun-ematch* zdd-join% (a b)
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
                                (zdd-join% lo-a lo-b))))))

(defun zdd-join (&rest zdds)
  "Return the relational join of ZDDs: {α ∪ β | α ∈ Z₁ and β ∈ Z₂}."
  (if zdds
    (reduce #'zdd-join% zdds)
    (sink nil)))


(defun-ematch* zdd-meet% (a b)
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
                                           (zdd-meet% lo-a lo-b)))))))

(defun zdd-meet (&rest zdds)
  "Return the relational meet of ZDDs: {α ∩ β | α ∈ Z₁ and β ∈ Z₂}."
  (if zdds
    (reduce #'zdd-meet% zdds)
    (sink nil)))


(defun zdd-family (&rest sets)
  "Return a ZDD that contains each of the given `sets` as members."
  (reduce #'zdd-union (mapcar #'zdd-set sets)))


(defun-ematch* zdd-keep-supersets-of% (zdd set)
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
     ((> var el) (sink nil)))))

(defun zdd-keep-supersets-of (zdd set)
  "Return a ZDD of all supersets of `set` in `zdd`: {α | α ∈ Z and α ⊇ S}."
  (zdd-keep-supersets-of% zdd (sort set #'<)))


(defun-ematch* zdd-remove-supersets-of% (zdd set)
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
     ((> var el) zdd))))

(defun zdd-remove-supersets-of (zdd set)
  "Return a ZDD of all non-supersets of `set` in `zdd`: {α | α ∈ Z and α ⊉ S}."
  (zdd-remove-supersets-of% zdd (sort set #'<)))


(defun-ematch* zdd-keep-avoiders-of% (zdd set)
  ((_ nil) zdd)
  (((sink) _) zdd)
  (((node var hi lo) (list* el remaining))
   (cond
     ((= var el) (zdd-keep-avoiders-of% lo remaining))
     ((< var el) (zdd-node var
                           (zdd-keep-avoiders-of% hi set)
                           (zdd-keep-avoiders-of% lo set)))
     ((> var el) (zdd-keep-avoiders-of% zdd remaining)))))

(defun zdd-keep-avoiders-of (zdd set)
  "Return a ZDD of members of `zdd` avoiding `set`: {α | α ∈ Z and α ∩ S = ø}."
  (zdd-keep-avoiders-of% zdd (sort set #'<)))


(defun zdd-match% (zdd set universe)
  (recursively ((zdd zdd) (set set))
    (ematch zdd
      ;; If Z = ∅, there are no candidates for matching.
      ((sink nil) (sink nil))

      ;; If Z = {∅}, the only set ∅ can match is the empty set.
      ((sink t) (if (null set)
                  (sink t)
                  (sink nil)))

      ;; Otherwise Z is a real node.
      ((node var hi lo)
       (if (not (aref universe var))
         ;; If this node is not in the universe, we don't care about it at all.
         ;; Recur down both branches.
         (zdd-node var
                   (recur hi set)
                   (recur lo set))

         ;; Otherwise this node is in the universe.  Is it in the set we're
         ;; looking for?
         (ematch set
           ;; If our target is empty, only the lo branch of Z can ever match.
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
                                         ;        jeeeeeeesus
                                         (sink nil)))))))))))

(defun zdd-match (zdd set universe)
  "Return a ZDD of members that exactly match `set` within the universe.

  {α | α ∈ Z and α ∩ U = S}

  `universe` should be an array of booleans, one per possible term.

  Every element to match in `set` should be a member of the universe.  This is
  not checked.  `set` does not need to be sorted beforehand.

  "
  (zdd-match% zdd (sort (copy-list set) #'<) universe))


;;;; Scratch ------------------------------------------------------------------
