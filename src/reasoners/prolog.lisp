(in-package :scully.reasoners.prolog)


;;;; Brute-Force Prolog Reasoner
;;; This is the slow, naive way to play.  It's here as a reference point.


;;;; Reasoner -----------------------------------------------------------------
(defun load-gdl-preamble (db)
  (push-logic-frame-with db
    (rule db (ggp-rules::not ?x) (call ?x) ! fail)
    (fact db (ggp-rules::not ?x))

    (rule db (ggp-rules::or ?x ?y) (call ?x))
    (rule db (ggp-rules::or ?x ?y) (call ?y))

    (rule db (ggp-rules::and ?x ?y) (call ?x) (call ?y))

    (rule db (ggp-rules::distinct ?x ?x) ! fail)
    (fact db (ggp-rules::distinct ?x ?y))))

(defun make-reasoner-database ()
  (let ((db (make-database)))
    (load-gdl-preamble db)
    db))


(defclass prolog-reasoner ()
  ((database :initform (make-reasoner-database) :reader pr-database)
   (current-state :initform nil :accessor pr-state)
   (current-moves :initform nil :accessor pr-moves)))

(defun make-prolog-reasoner ()
  (make-instance 'prolog-reasoner))


;;;; GDL Cleaning -------------------------------------------------------------
;;; Some GDL authors use (or x y) and (and x y) in their game descriptions, even
;;; though it's not part of the GDL "spec".  Worse still, some use n-ary
;;; versions of those predicates, because fuck you.  So we'll do a quick pass
;;; over the GDL to clean up these bugs.

(defun clean-or (gdl)
  (destructuring-bind (or . arguments)
      gdl
    (case (length arguments)
      (1 (first arguments))
      (2 gdl)
      (t (list or (first arguments)
               (clean-or (cons or (rest arguments))))))))

(defun clean-and (gdl)
  (destructuring-bind (and . arguments)
      gdl
    (case (length arguments)
      (1 (first arguments))
      (2 gdl)
      (t (list and (first arguments)
               (clean-and (cons and (rest arguments))))))))

(defun clean-gdl (gdl)
  (if (consp gdl)
    (case (car gdl)
      (ggp-rules::or (clean-or gdl))
      (ggp-rules::and (clean-and gdl))
      (t (cons (clean-gdl (car gdl))
               (clean-gdl (cdr gdl)))))
    gdl))


;;;; State Normalization ------------------------------------------------------
(defun dedupe-state (state)
  (iterate (for fact :in state)
           (for prev :previous fact)
           (when (not (equal fact prev))
             (collect fact))))

(defun fact-slow< (a b)
  ;; numbers < symbols < conses
  (etypecase a
    (number (typecase b
              (number (< a b))
              (t t)))
    (symbol (etypecase b
              (number nil)
              (cons t)
              (symbol (string< (symbol-name a) (symbol-name b)))))
    (cons (typecase b
            (cons (cond
                    ((fact-slow< (car a) (car b)) t)
                    ((fact-slow< (car b) (car a)) nil)
                    (t (fact-slow< (cdr a) (cdr b)))))
            (t nil)))))

(defun fact< (a b)
  (if (eql a b)
    nil
    (let ((ha (sxhash a))
          (hb (sxhash b)))
      (if (= ha hb)
        (fact-slow< a b)
        (< ha hb)))))

(defun sort-state (state)
  (sort state #'fact<))

(defun normalize-state (state)
  (dedupe-state (sort-state state)))


;;;; Ugly State Management ----------------------------------------------------
(defun apply-state (reasoner state)
  (let ((db (pr-database reasoner)))
    (push-logic-frame-with db
      (loop :for fact :in state
            :do (invoke-fact db `(ggp-rules::true ,fact)))))
  (setf (pr-state reasoner) state))

(defun apply-moves (reasoner moves)
  (let ((db (pr-database reasoner)))
    (push-logic-frame-with db
      (loop :for (role . action) :in moves
            :do (invoke-fact db `(ggp-rules::does ,role ,action)))))
  (setf (pr-moves reasoner) moves))


(defun clear-state (reasoner)
  (pop-logic-frame (pr-database reasoner))
  (setf (pr-state reasoner) nil))

(defun clear-moves (reasoner)
  (pop-logic-frame (pr-database reasoner))
  (setf (pr-moves reasoner) nil))


(defun ensure-state (reasoner state)
  (when (not (eql state (pr-state reasoner)))
    (when (not (null (pr-moves reasoner)))
      (clear-moves reasoner))
    (when (not (null (pr-state reasoner)))
      (clear-state reasoner))
    (apply-state reasoner state)))

(defun ensure-moves (reasoner moves)
  (when (not (eql moves (pr-moves reasoner)))
    (when (not (null (pr-moves reasoner)))
      (clear-moves reasoner))
    (apply-moves reasoner moves)))


;;;; API ----------------------------------------------------------------------
(defun load-rules (reasoner rules)
  (scully.gdl:load-rules (pr-database reasoner)
                         (clean-gdl rules)))

(defun initial-state (reasoner)
  (normalize-state
    (query-map (pr-database reasoner)
               (lambda (r) (getf r '?what))
               (ggp-rules::init ?what))))

(defun terminalp (reasoner)
  (prove (pr-database reasoner) ggp-rules::terminal))

(defun next-state (reasoner state moves)
  (ensure-state reasoner state)
  (ensure-moves reasoner moves)
  (normalize-state
    (query-map (pr-database reasoner)
               (lambda (r) (getf r '?what))
               (ggp-rules::next ?what))))

(defun legal-moves-for (reasoner role state)
  (ensure-state reasoner state)
  (remove-duplicates
    (invoke-query-map (pr-database reasoner)
                      (lambda (r) (getf r '?action))
                      `(ggp-rules::legal ,role ?action))
    :test #'equal))

(defun percepts-for (reasoner role state moves)
  (ensure-state reasoner state)
  (ensure-moves reasoner moves)
  (remove-duplicates
    (invoke-query-map (pr-database reasoner)
                      (lambda (r) (getf r '?what))
                      `(ggp-rules::sees ,role ?what))
    :test #'equal))

(defun roles (reasoner)
  (query-map (pr-database reasoner)
             (lambda (r) (getf r '?role))
             (ggp-rules::role ?role)))
