(in-package #:scully.grounders.fluxplayer)


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


(defun .index-line ()
  (.let* ((id (.positive-integer))
          (_ (.whitespace))
          (term (.line)))
    (.identity (make-index-entry :id id :term (read-from-string term)))))

(defun .rule-line ()
  (.let* ((_ (.positive-integer)) ; type, not used here
          (id (.whitespace-and (.positive-integer)))
          (term-count (.whitespace-and (.positive-integer)))
          (negative-term-count (.whitespace-and (.positive-integer)))
          (positive-terms (.repeat (- term-count negative-term-count)
                                   (.whitespace-and (.positive-integer))))
          (negative-terms (.repeat negative-term-count
                                   (.whitespace-and (.positive-integer))))
          (_ (.char= #\newline)))
    (.identity (make-rule :id id
                          :positive positive-terms
                          :negative negative-terms))))

(defun .delimiter-line ()
  (.progn (.char= #\0)
          (.or (.char= #\newline)
               (.eof))
          (.identity nil)))

(defun .grounded-gdl ()
  (.let* ((rules (.first (.map 'list (.rule-line))))
          (_ (.delimiter-line))
          (index (.first (.map 'list (.index-line))))
          (_ (.delimiter-line))
          )
    (.identity (list :rules rules :index index))))


(defun parse-raw-grounded (raw)
  (values (parse (.grounded-gdl) raw)))


;;;; API ----------------------------------------------------------------------
(defun ground-raw (filename)
  (uiop/run-program:run-program
    `("/Users/sjl/src/fluxplayer/trunk/src/groundgdl.sh" ,filename "-")
    :force-shell nil
    :output :string))

(defun ground-gdl (filename)
  (parse-raw-grounded (ground-raw filename)))


; (ground-gdl "gdl/buttons.gdl")
