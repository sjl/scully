(in-package :scully.gdl)

(defvar *ggp-gensym-counter* 0)

(defun gensym-ggp ()
  "Return a unique symbol in the `ggp-rules` package."
  (values (intern (mkstr 'rule- (incf *ggp-gensym-counter*))
                  (find-package :ggp-rules))))


(defun read-gdl (filename)
  "Read GDL from the given file"
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

(defun dump-gdl (rules)
  (let ((*package* (find-package :ggp-rules)))
    (format nil "~(~{~S~%~}~)" rules)))
