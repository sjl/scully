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

(defun print-through (function val)
  (pr (funcall function val))
  val)

(defun mapprint-through (function val)
  (mapc #'pr (funcall function val))
  val)

(defun line (x)
  (declare (ignore x))
  '----------------------------------------)


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


;;;; GraphViz -----------------------------------------------------------------
(setf cl-dot:*dot-path* "/usr/local/bin/dot")
(defparameter *draw-unique-sinks* nil)
(defparameter *draw-unique-nodes* nil)
(defparameter *draw-hex-p* #'never)

(defun attrs (object &rest attributes)
  (make-instance 'cl-dot:attributed
    :object object
    :attributes attributes))


(defun sink-attrs (val)
  `(:label ,(if val "⊤" "⊥")
    :shape :square
    :style :filled
    :fillcolor "#fafafa"
    :color "#aaaaaa"
    :fontsize 22
    :width 0.05))

(defmethod cl-dot:graph-object-node ((graph (eql 'zdd)) (object node))
  (make-instance 'cl-dot:node
    :attributes (ematch object
                  ((node v) `(:label ,v
                              :shape ,(if (funcall *draw-hex-p* v)
                                        :hexagon
                                        :circle))))))

(defmethod cl-dot:graph-object-node ((graph (eql 'zdd)) (object cons))
  (cl-dot:graph-object-node graph (car object)))

(defmethod cl-dot:graph-object-node ((graph (eql 'zdd)) (object leaf))
  (make-instance 'cl-dot:node
    :attributes (ematch object ((sink c) (sink-attrs c)))))


(defun wrap-node (object)
  (ematch object
    ((sink) (if *draw-unique-sinks* object (cons object nil)))
    ((node) (if *draw-unique-nodes* object (cons object nil)))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'zdd))
                                          (object t))
  (ematch object
    ((cons object _)
     (cl-dot:graph-object-points-to graph object))
    ((sink _)
     '())
    ((node _ hi lo)
     (list (attrs (wrap-node hi) :style :solid)
           (attrs (wrap-node lo) :style :dashed)))))


(defun draw (zdd &key
             (filename "zdd.png")
             (unique-sinks nil)
             (unique-nodes t)
             (hexp #'never))
  (let ((*draw-unique-sinks* unique-sinks)
        (*draw-unique-nodes* unique-nodes)
        (*draw-hex-p* hexp))
    (cl-dot:dot-graph
      (cl-dot:generate-graph-from-roots 'zdd (list (wrap-node zdd)))
      filename
      :format :png))
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

(defun unit-patch (z)
  (ematch z
    ((sink t) z)
    ((sink nil) (sink t))
    ((node variable hi lo)
     (zdd-node variable hi (unit-patch lo)))))


(defun zdd-set (elements)
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
  (if zdds
    (reduce #'zdd-meet% zdds)
    (sink nil)))


(defun zdd-family (&rest sets)
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
  (zdd-match% zdd (sort set #'<) lower-bound upper-bound))


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
(with-zdd
  (-<> (zdd-join (zdd-family '(1 2) '(7 8) '())
                 (zdd-family '(1 5 9) nil)
                 (zdd-set '(1)))
    (print-through #'enumerate <>)
    (zdd-keep-avoiders-of <> '(2 7))
    (print-through #'enumerate <>)
    (draw <>)
    (zdd-size <>)
    ))


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

(with-zdd
  (-<> (make-rule-tree *rules*)
    ; (print-enumerated <>)
    ; (zdd-keep-avoiders-of <> '(2 7))
    (mapprint-through #'enumerate <>)
    (print-through #'zdd-count <>)
    (print-through #'zdd-size <>)
    (draw <> :unique-sinks t :unique-nodes t :hexp (curry #'<= 1000))
    ; (zdd-size <>)
    (never)
    )
  ; (pr '--------------)
  ; (-<> (apply #'zdd-family *state*)
  ;   (mapprint-through #'enumerate <>)
  ;   (print-through #'zdd-count <>)
  ;   (print-through #'zdd-size <>)
  ;   ; (draw <>)
  ;   ; (zdd-size <>)
  ;   (never)
  ;   )
  ; (pr '--------------)
  ; (-<> (apply-rule-tree (apply #'zdd-family *state*)
  ;                       (make-rule-tree *rules*)
  ;                       100)
  ;   (mapprint-through #'enumerate <>)
  ;   (print-through #'zdd-count <>)
  ;   (print-through #'zdd-size <>)
  ;   ; (draw <>)
  ;   ; (zdd-size <>)
  ;   (never)
  ;   )
  )


(defun test ()
  (with-zdd
    (print-hash-table
      (frequencies
        (iterate (repeat 10000)
                 (collect (zdd-random-member
                            (zdd-family
                              '(1 2 3)
                              '(2)
                              '(1 3)
                              '(1 5)
                              '(5)))))
        :test #'equal))))


(with-zdd
  (-<> (zdd-family
         '(1 2 100 200 6000)
         '(100 200 300)
         '(99 100 200 300)
         '(1 9900)
         '()
         '(1 2 1001)
         )
    (mapprint-through #'enumerate <>)
    (print-through #'line <>)
    (zdd-match <> '() 100 999)
    (mapprint-through #'enumerate <>)
    (draw <> :hexp (lambda (v) (>= 999 v 100)))
    (never <>)
    ))
