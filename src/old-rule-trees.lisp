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
  (and (consp term) (eql 'ggp-rules::not (first term))))

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
           (equal (rule-first-body rule) `(ggp-rules::not ,element)))
         (rule-ignores (rule)
           (not (or (rule-requires rule)
                    (rule-disallows rule)))))
      (values element
              (remove-if-not #'rule-disallows rules)
              (remove-if-not #'rule-requires rules)
              (remove-if-not #'rule-ignores rules)))))


(defmethod print-object ((set hash-set) stream)
  (print-unreadable-object (set stream :type t :identity nil)
    (prin1 (set->list set) stream)))

(defun hash-set= (s1 s2)
  (zerop (set-size (set-symmetric-diff s1 s2))))

(defun rule-head-in (set rule)
  (set-lookup set (rule-head rule)))

(defun collapse-positive-heads (rules-and-heads)
  (destructuring-bind (rules heads) rules-and-heads
    (flet ((update-rule (rule)
             (cons (rule-head rule)
                   (remove-if (curry #'set-lookup heads)
                              (rule-body rule)))))
      (let* ((new-rules (set-map #'update-rule rules))
             (new-heads (-<> new-rules
                          (set-filter #'rule-empty-p <>)
                          (set-map #'rule-head <>))))
        (list (set-filter (complement (curry #'rule-head-in new-heads))
                          new-rules)
              (set-union heads new-heads))))))

(defun find-strictly-negative-rules (rules)
  (set-filter (lambda (rule)
                (every #'negationp (rule-body rule)))
              rules))

(defun collapse-negative-heads (rules-and-heads)
  (destructuring-bind (rules heads) rules-and-heads
    (if (zerop (set-size rules))
      (list rules heads)
      (labels ((negation-satisfied-p (negation)
                 (not (set-lookup heads (bare-term negation))))
               (rule-satisfied-p (rule)
                 (every #'negation-satisfied-p (rule-body rule)))
               (smallest-head ()
                 (-<> (set->list rules)
                   (mapcar #'rule-head <>)
                   (sort <> #'term<)
                   (first <>)))
               (rules-with-head (head)
                 (set-filter (lambda (rule) (eql head (rule-head rule)))
                             rules)))
        (let* ((next (smallest-head))
               (candidates (rules-with-head next)))
          (list (set-diff rules candidates)
                (if (some #'rule-satisfied-p (set->list candidates))
                  (set-insert heads next)
                  heads)))))))


(defun make-rule-tree (rules)
  "Create a rule tree ZDD from the given logical `rules`.

  `rules` should be a list of one layer-worth of rules, each of the form:
  `(head-term &rest body-terms)`

  Each head term should be a single variable.
  Each body term should be either a single variable or `(not variable)`.

  Rules and bodies do not need to be sorted beforehand.

  "
  (recursively ((rules (mapcar #'sort-body rules))
                (accumulated-heads nil))
    (let* ((heads (-<> rules
                    (remove-if-not #'rule-empty-p <>)
                    (mapcar #'rule-head <>)
                    (remove-duplicates <> :test #'=)
                    (union accumulated-heads <> :test #'=))) ; slow
           (next-rules (remove-if
                         (lambda (rule)
                           (member (rule-head rule) heads :test #'equal))
                         rules)))
      (if (null next-rules)
        (zdd-set heads)
        (multiple-value-bind (term low high both)
            (partition-rules next-rules)
          ; (pr :rules rules)
          ; (pr :acch accumulated-heads)
          ; (pr :heads heads)
          ; (pr :next-rules next-rules)
          ; (pr :term term)
          ; (pr :low low)
          ; (pr :high high)
          ; (pr :both both)
          ; (break)
          (zdd-node term
                    (recur (append (mapcar #'drop-first high) both) heads)
                    (recur (append (mapcar #'drop-first low) both) heads)))))))

