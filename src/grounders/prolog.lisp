(in-package :scully.grounders.prolog)


;;;; Utils
(defun fixed-point (function data &key (test 'eql))
  "Find the fixed point of `function`, starting with `data`."
  (let ((next (funcall function data)))
    (if (funcall test data next)
      data
      (fixed-point function next :test test))))

(defun gensyms (n prefix)
  (iterate (repeat n) (collect (gensym prefix))))


;;;; Sanitization
(defun clause-is-p (clause functor-name)
  (and (consp clause)
       (eql (first clause) functor-name)))

(defun clause-is-not-p (clause)
  (clause-is-p clause 'ggp-rules::not))

(defun clause-is-distinct-p (clause)
  (clause-is-p clause 'ggp-rules::distinct))

(defun clause-is-and-p (clause)
  (clause-is-p clause 'ggp-rules::and))

(defun clause-is-or-p (clause)
  (clause-is-p clause 'ggp-rules::or))


(defun split-ors (rule)
  (labels ((split (body)
             ;; take the body of a clause and return a list of the bodies that
             ;; result after splitting up any `(or ...)`s inside it.
             (match body
               (nil (list nil))

               ((list* (list* 'ggp-rules::or args) remaining)
                (mapcan (lambda (arg)
                          (mapcar (curry #'cons arg)
                                  (split remaining)))
                        args))

               ((list* other remaining)
                (mapcar (curry #'cons other) (split remaining))))))
    (destructuring-bind (head . body) rule
      (mapcar (curry #'cons head) (split body)))))

(defun strip-ands (rule)
  (labels ((flatten-ands (body)
             (match body
               (nil nil)
               ((list* first-clause remaining)
                (append (if (clause-is-and-p first-clause)
                          (flatten-ands (rest first-clause))
                          (list first-clause))
                        (flatten-ands remaining))))))
    (destructuring-bind (head . body) rule
      (cons head (flatten-ands body)))))

(defun strip-nots (rule)
  (destructuring-bind (head . body) rule
    (cons head (remove-if #'clause-is-not-p body))))

(defun strip-distincts (rule)
  (destructuring-bind (head . body) rule
    (cons head (remove-if #'clause-is-distinct-p body))))


(defun sanitize-rule (rule)
  (match rule
    ((list* 'ggp-rules::<= contents)
     (->> contents
       split-ors
       (mapcar #'strip-ands)
       (mapcar #'strip-nots)
       (mapcar #'strip-distincts)
       (mapcar (curry #'cons 'ggp-rules::<=))))
    (fact (list fact))))

(defun sanitize-rules (rules)
  (mapcan #'sanitize-rule rules))


;;;; Fluents
(defun find-initial-state (database)
  (query-map database
             (lambda (result)
               `(ggp-rules::true ,(getf result '?what)))
             (ggp-rules::init ?what)))

(defun find-trues (database)
  (query-map database
             (lambda (result)
               `(ggp-rules::true ,(getf result '?what)))
             (ggp-rules::next ?what)))

(defun find-moves (database)
  (query-map database
             (lambda (result)
               `(ggp-rules::does
                 ,(getf result '?role)
                 ,(getf result '?move)))
             (ggp-rules::legal ?role ?move)))

(defun push-fluents (database fluents)
  (push-logic-frame-with database
    (map nil (curry #'invoke-fact database) fluents)))

(defun pop-fluents (database)
  (pop-logic-frame database))

(defun find-more-fluents (database fluents)
  (push-fluents database fluents)
  (prog1
      (-> fluents
        (union (find-moves database) :test #'equal)
        (union (find-trues database) :test #'equal)) 
    (pop-fluents database)))

(defun ground-fluents (rules)
  (let ((database (make-database)))
    (scully.gdl:load-rules database rules)
    (fixed-point (curry #'find-more-fluents database)
                 (find-initial-state database)
                 :test (rcurry #'set-equal :test #'equal))))


;;;; Axioms
(defun find-functor (rule)
  (ematch rule
    ((list* 'ggp-rules::<= (list* functor arguments) _)
     (cons functor (length arguments)))

    ((list* 'ggp-rules::<= bare-functor _)
     (cons bare-functor 0))

    ((list* functor arguments)
     (cons functor (length arguments)))))

(defun find-axioms (rules)
  (-<> rules
    (mapcar #'find-functor <>)
    (remove-duplicates <> :test #'equal)))

(defun ground-single-axiom (database functor arity)
  (let ((vars (gensyms arity "?")))
    (remove-duplicates
      (invoke-query-map database
                        (lambda (result)
                          (if (zerop arity)
                            functor
                            `(,functor ,@(mapcar (curry #'getf result) vars))))
                        `(,functor ,@vars))
      :test #'equal)))

(defun find-all-axioms (database functors)
  (iterate (for (functor . arity) :in functors)
           (unioning (ground-single-axiom database functor arity)
                     :test #'equal)))

(defun ground-axioms (rules grounded-fluents)
  (let ((database (make-database)))
    (scully.gdl:load-rules database rules)
    (push-fluents database grounded-fluents)
    (find-all-axioms database (find-axioms rules))))


;;;; API
(defun ground-rules (rules)
  (let* ((rules (sanitize-rules rules))
         (fluents (ground-fluents rules))
         (axioms (ground-axioms rules fluents)))
    fluents
    axioms))


; (map nil #'print (ground-rules (scully.gdl:read-gdl "gdl/buttons.gdl")))
