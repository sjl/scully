(in-package :scully.logic)

(defparameter *rules*
  (scully.gdl::read-gdl "gdl/tictactoe-grounded.gdl")
  ; (scully.gdl::read-gdl "gdl/hanoi-grounded.gdl")
  ; (scully.gdl::read-gdl "gdl/8puzzle-grounded.gdl")
  ; (scully.gdl::read-gdl "gdl/roshambo2-grounded.gdl")
  )


(defun slot-definition (conc-name slot)
  (destructuring-bind (name &key
                            type
                            documentation
                            (accessor (symb conc-name name))
                            (initarg (intern (symbol-name name) :keyword)))
      (ensure-list slot)
    `(,name :initarg ,initarg :accessor ,accessor
            ,@(when type `(:type ,type))
            ,@(when documentation `(:documentation ,documentation)))))

(defmacro defclass* (name-and-options direct-superclasses slots &rest options)
  (destructuring-bind (name &key (conc-name (symb name '-)))
      (ensure-list name-and-options)
    `(defclass ,name ,direct-superclasses
       ,(mapcar (curry #'slot-definition conc-name) slots)
       ,@options)))

(defclass* (logic-manager :conc-name lm-) ()
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
    (scully.zdd::zdd-set <>)))

(defun make-stratum-rule-trees (stratum)
  (-<> stratum
    (group-by #'car <>)
    hash-table-values
    (mapcar #'scully.rule-trees::make-rule-tree <>)))


(defun make-logic-manager (rules)
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
      (flet ((make-forest (layer)
               (-<> rule-layers
                 (gethash layer <>)
                 scully.terms::stratify-layer
                 (mapcar #'make-stratum-rule-trees <>))))
        (scully.zdd::with-zdd
          (make-instance 'logic-manager
            :rules rules
            :roles (find-roles rules)
            :possible-forest (make-forest :possible)
            :happens-forest (make-forest :happens)
            :initial-zdd (scully.zdd::zdd-set (find-initial-state rules term->number))
            :legal-zdd (make-predicate-zdd 'ggp-rules::legal term->number)
            :goal-zdd (make-predicate-zdd 'ggp-rules::goal term->number)
            :terminal-zdd (make-predicate-zdd 'ggp-rules::terminal term->number)
            :term->number term->number
            :number->term number->term))))))


(defun initial-iset (logic-manager)
  "Return the initial information set of the game."
  (lm-initial-zdd logic-manager))

(defun number-to-term (logic-manager number)
  (gethash number (lm-number->term logic-manager)))

(defun term-to-number (logic-manager term)
  (gethash term (lm-term->number logic-manager)))

(defun rand-state (logic-manager iset)
  "Select a random member of the given information set."
  (mapcar (curry #'number-to-term logic-manager)
          (scully.zdd::zdd-random-member iset)))

(defun terminalp (logic-manager iset)
  "Return whether the given information set is a terminal state."
  (-<> iset
    (scully.zdd::zdd-meet <> (lm-terminal-zdd logic-manager))
    scully.zdd::zdd-unit-p
    not))

(defun draw-zdd (logic-manager zdd)
  (flet ((label (n)
           (let ((*package* (find-package :ggp-rules)))
             (-<> n
               (number-to-term logic-manager <>)
               (structural-string <>)))))
    (scully.graphviz::draw-zdd zdd :label-fn #'label)))


(defparameter *l* (make-logic-manager *rules*))


; (defun apply-rule-tree (zdd rule-tree head-bound)
;   "Apply the logical rules in `rule-tree` to the sets in `zdd`.

;   `zdd` is assumed to contain sets of logical axioms.  This function will update
;   each of these sets to add any rule heads derivable from the axioms in the set.

;   "
;   (recursively ((zdd zdd)
;                 (rule-tree rule-tree))
;     (ematch* (zdd rule-tree)
;       ;; If Z = ∅ there are no sets to cons heads onto, bail.
;       (((sink nil) _) zdd)

;       ;; If R = ∅ or {∅} we've bottomed out of the rule tree and there are no
;       ;; heads to cons, we're done.
;       ((_ (sink)) zdd)

;       ;; If we've passed the head boundary on the rule tree side then we're done
;       ;; filtering and just need to cons in all the heads.
;       ((_ (guard (node var _ _)
;                  (>= var head-bound)))
;        (zdd-join zdd rule-tree))

;       ;; If Z = {∅} we might have some heads we need to cons later in the rule
;       ;; tree, so recur down the lo side of it.
;       (((sink t) (node _ _ lo))
;        (recur zdd lo))

;       ;; Otherwise we need to filter.
;       (((node var-z hi-z lo-z) (node var-r hi-r lo-r))
;        (cond
;          ((= var-z var-r) (zdd-node var-z
;                                     (recur hi-z hi-r)
;                                     (recur lo-z lo-r)))
;          ((< var-z var-r) (zdd-node var-z
;                                     (recur hi-z rule-tree)
;                                     (recur lo-z rule-tree)))
;          ((> var-z var-r) (recur zdd lo-r)))))))




;;;; PLAN
;;;
;;; 1. Receive GDL from server
;;; 2. Ground it
;;; 3. Integerize the ground GDL
;;; 4. Find initial state
;;; 5. Build rule trees for integerized rules
;;; 6. ...
