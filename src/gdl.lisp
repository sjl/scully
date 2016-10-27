(in-package :scully.gdl)

(defun read-gdl (filename)
  (let ((*package* (find-package :ggp-rules)))
    (with-open-file (stream filename)
      (loop
        :with done = (gensym)
        :for form = (read stream nil done)
        :while (not (eq form done))
        :collect form))))

(defun load-rules (database rules)
  (push-logic-frame-with database
    (mapc (lambda (rule)
            (if (and (consp rule)
                     (eq (car rule) 'ggp-rules::<=))
              (apply #'invoke-rule database (cdr rule))
              (invoke-fact database rule)))
          rules)))

(defun redump-gdl (rules)
  (let ((*package* (find-package :ggp-rules)))
    (format nil "~(~{~S~%~}~)" rules)))
