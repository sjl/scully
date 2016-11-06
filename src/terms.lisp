(in-package :scully.terms)
(in-readtable :fare-quasiquote)


;;;; Overview
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


;;;; Ordering -----------------------------------------------------------------
(defun-match bare-term (term)
  (`(not ,x) x)
  (x x))

(defun-match negationp (term)
  (`(not ,_) t)
  (_ nil))


(defun content< (c1 c2)
  nil
  )

(defun-match* same-predicate-p (t1 t2)
  ((`(,h1 ,@_)
    `(,h2 ,@_))
   (equal h1 h2))
  ((_ _) nil))


(defun-ematch layer-id (l)
  (:base 0)
  (:does 1)
  (:possible 2)
  (:happens 3))

(defun-match layer (term)
  (`(,head ,@_) (case head
                  (true :base)
                  (does :does)
                  ((legal goal) :possible)
                  ((next sees) :happening)))
  ('terminal :possible)
  (_ (error "Unknown layer for ~S" term)))

(defun layer< (l1 l2)
  (< (layer-id l1)
     (layer-id l2)))

(defun layer= (l1 l2)
  (eql l1 l2))


(defun term< (t1 t2)
  (let ((l1 (layer t1))
        (l2 (layer t2)))
    (if (not (layer= l1 l2))
      (layer< l1 l2)

      )))



(defun find-vertex (graph element)
  ; please kill me
  (cl-graph:search-for-vertex graph element
                              :test #'equal
                              :key #'cl-graph::value ; because fuck you
                              :error-if-not-found? nil))

(defun build-layer-graph (rules)
  (let ((graph (cl-graph:make-graph 'cl-graph:graph-container
                                    :default-edge-type :directed
                                    :test #'equal)))
    (map nil (curry #'cl-graph:add-vertex graph)
         (remove-duplicates (mapcar #'first rules) :test #'equal))
    (iterate
      (for (head . body) :in rules)
      (for vhead = (find-vertex graph head))
      (iterate (for term :in body)
               (when (negationp term)
                 (let ((vdep (find-vertex graph (bare-term term))))
                   (cl-graph:add-edge-between-vertexes graph vhead vdep)))))
    graph))

(defun order-rules (rules)
  (-<> rules
    build-layer-graph
    cl-graph:topological-sort
    (mapcar #'cl-graph::value <>))) ; eat shit



(defparameter *g*
  (build-layer-graph (list
                       '((t foo) a)
                       '((t bar) a (not (t foo)))
                       '((t baz) a (not (t bar)))
                       )))

(cl-graph:vertexes *g*)
(cl-graph:edges *g*)
(*g*)

(order-rules (list
  '((t foo) a)
  '((t bar) a (not (t foo)))
  '((t baz) a (not (t bar)))
  ))


(defparameter *g*
  (cl-graph:make-graph 'cl-graph:graph-container
                       :default-edge-type :directed))
(cl-graph:vertexes *g*)
(cl-graph:add-vertex *g* (list 1))
(cl-graph:add-vertex *g* (list 1))
