(in-package :scully.grounders.fluxplayer)


;;;; Data ---------------------------------------------------------------------
(defstruct index-entry id term)
(defstruct rule id positive negative)


;;;; Parsing ------------------------------------------------------------------
(defparameter *whitespace* '(#\space #\tab))

(defun .whitespace (&optional result-type)
  (.first (.map result-type (.is 'member *whitespace*))))

(defun .whitespace-and (parser)
  (.progn (.whitespace) parser))


(defun .digit (&optional (radix 10))
  (.is (rcurry #'digit-char-p radix)))

(defun .positive-integer (&optional (radix 10))
  (.let* ((digits (.first (.map 'string (.digit radix)))))
    (.identity (parse-integer digits :radix radix))))


(defun .eof ()
  (.or (.not (.item))
       (.fail)))


(defun .actual-line ()
  ;; zero or more chars, followed by a newline
  (.let* ((contents (.optional
                      (.first (.map 'string (.is-not #'char= #\newline)))))
          (_ (.char= #\newline)))
    (.identity (or contents ""))))

(defun .final-non-terminated-line ()
  ;; one or more chars at eof without a trailing newline
  (.let* ((contents (.first (.map 'string (.is-not #'char= #\newline))))
          (_ (.eof)))
    (.identity contents)))

(defun .line ()
  (.or (.actual-line) (.final-non-terminated-line)))


(defun .repeat (n parser)
  (if (zerop n)
    (.identity nil)
    (.let* ((el parser)
            (rest (.repeat (1- n) parser)))
      (.identity (cons el rest)))))


(defun read-gdl-term (string)
  (let ((*package* (find-package :ggp-rules)))
    (read-from-string string)))

(defun .index-line ()
  (.let* ((id (.positive-integer))
          (_ (.whitespace))
          (term (.line)))
    (.identity (make-index-entry :id id
                                 :term (read-gdl-term term)))))

(defun .rule-line ()
  (.let* ((_ (.positive-integer)) ; type, not used here
          (id (.whitespace-and (.positive-integer)))
          (term-count (.whitespace-and (.positive-integer)))
          (negative-term-count (.whitespace-and (.positive-integer)))
          (negative-terms (.repeat negative-term-count
                                   (.whitespace-and (.positive-integer))))
          (positive-terms (.repeat (- term-count negative-term-count)
                                   (.whitespace-and (.positive-integer)))))
    (.identity (make-rule :id id
                          :positive positive-terms
                          :negative negative-terms))))


(defun parse-raw-grounded (raw)
  (with-input-from-string (s raw)
    (iterate
      (with in-index = nil)
      (for line :in-stream s :using #'read-line)
      (cond
        ((string= "0" line) (setf in-index t))
        (in-index (collect (parse (.index-line) line) :into index))
        (t (collect (parse (.rule-line) line) :into rules)))
      (finally (return (list rules index))))))


;;;; Rebuilding ---------------------------------------------------------------
(defun rebuild-rules (rule-entries index-entries)
  (let ((index (make-hash-table)))
    (iterate (for entry :in index-entries)
             (setf (gethash (index-entry-id entry) index)
                   (index-entry-term entry)))
    (flet ((get-rule (id)
             (ensure-gethash id index (scully.gdl:gensym-ggp)))
           (useless-rule-p (has-name pos neg)
             (and (not has-name)
                  (null pos)
                  (null neg))))
      (iterate
        (for entry :in rule-entries)
        (for (values rule has-name) = (get-rule (rule-id entry)))
        (for pos = (mapcar #'get-rule (rule-positive entry)))
        (for neg = (mapcar #'get-rule (rule-negative entry)))
        (unless (useless-rule-p has-name pos neg)
          (collect (if (or pos neg)
                     `(ggp-rules::<= ,rule
                        ,@pos
                        ,@(mapcar (curry #'list 'ggp-rules::not) neg))
                     (ensure-list rule))))))))


;;;; Fluxplayer ---------------------------------------------------------------
(defun ground-with-fluxplayer (string filename)
  (uiop/run-program:run-program
    `("/Users/sjl/src/fluxplayer/trunk/src/groundgdl.sh" ,filename "-")
    :force-shell nil
    :input (make-string-input-stream string)
    :output :string))


;;;; API ----------------------------------------------------------------------
(defun ground-gdl-string (string)
  (-<> (ground-with-fluxplayer string "-")
    (parse-raw-grounded <>)
    (apply #'rebuild-rules <>)))

(defun ground-gdl-file (filename)
  (-<> (ground-with-fluxplayer "" filename)
    (parse-raw-grounded <>)
    (apply #'rebuild-rules <>)))


(defun dump-grounded (filename)
  (let ((grounded (ground-gdl-file (mkstr "gdl/" filename ".gdl"))))
    (write-string-into-file (scully.gdl:dump-gdl grounded)
                            (mkstr "gdl/" filename "-grounded.gdl")
                            :if-exists :supersede))
  'ok)


;; (dump-grounded "buttons")
;; (dump-grounded "8puzzle")
;; (dump-grounded "tictactoe")
;; (dump-grounded "stratego")
;; (dump-grounded "small_dominion")
;; (time (dump-grounded "meier"))
