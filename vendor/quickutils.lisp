;;;; This file was automatically generated by Quickutil.
;;;; See http://quickutil.org for details.

;;;; To regenerate:
;;;; (qtlc:save-utils-as "quickutils.lisp" :utilities '(:COMPOSE :COPY-HASH-TABLE :CURRY :ENSURE-BOOLEAN :ENSURE-GETHASH :ENSURE-LIST :HASH-TABLE-KEYS :MAP-PRODUCT :MKSTR :ONCE-ONLY :RCURRY :SET-EQUAL :WITH-GENSYMS :WITH-OUTPUT-TO-FILE) :ensure-package T :package "SCULLY.QUICKUTILS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "SCULLY.QUICKUTILS")
    (defpackage "SCULLY.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use #:cl))))

(in-package "SCULLY.QUICKUTILS")

(when (boundp '*utilities*)
  (setf *utilities* (union *utilities* '(:MAKE-GENSYM-LIST :ENSURE-FUNCTION
                                         :COMPOSE :COPY-HASH-TABLE :CURRY
                                         :ENSURE-BOOLEAN :ENSURE-GETHASH
                                         :ENSURE-LIST :MAPHASH-KEYS
                                         :HASH-TABLE-KEYS :MAPPEND :MAP-PRODUCT
                                         :MKSTR :ONCE-ONLY :RCURRY :SET-EQUAL
                                         :STRING-DESIGNATOR :WITH-GENSYMS
                                         :WITH-OPEN-FILE* :WITH-OUTPUT-TO-FILE))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-gensym-list (length &optional (x "G"))
    "Returns a list of `length` gensyms, each generated as if with a call to `make-gensym`,
using the second (optional, defaulting to `\"G\"`) argument."
    (let ((g (if (typep x '(integer 0)) x (string x))))
      (loop repeat length
            collect (gensym g))))
  )                                        ; eval-when
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; To propagate return type and allow the compiler to eliminate the IF when
  ;;; it is known if the argument is function or not.
  (declaim (inline ensure-function))

  (declaim (ftype (function (t) (values function &optional))
                  ensure-function))
  (defun ensure-function (function-designator)
    "Returns the function designated by `function-designator`:
if `function-designator` is a function, it is returned, otherwise
it must be a function name and its `fdefinition` is returned."
    (if (functionp function-designator)
        function-designator
        (fdefinition function-designator)))
  )                                        ; eval-when

  (defun compose (function &rest more-functions)
    "Returns a function composed of `function` and `more-functions` that applies its ;
arguments to to each in turn, starting from the rightmost of `more-functions`,
and then calling the next one with the primary value of the last."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (reduce (lambda (f g)
              (let ((f (ensure-function f))
                    (g (ensure-function g)))
                (lambda (&rest arguments)
                  (declare (dynamic-extent arguments))
                  (funcall f (apply g arguments)))))
            more-functions
            :initial-value function))

  (define-compiler-macro compose (function &rest more-functions)
    (labels ((compose-1 (funs)
               (if (cdr funs)
                   `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                   `(apply ,(car funs) arguments))))
      (let* ((args (cons function more-functions))
             (funs (make-gensym-list (length args) "COMPOSE")))
        `(let ,(loop for f in funs for arg in args
                     collect `(,f (ensure-function ,arg)))
           (declare (optimize (speed 3) (safety 1) (debug 1)))
           (lambda (&rest arguments)
             (declare (dynamic-extent arguments))
             ,(compose-1 funs))))))
  

  (defun copy-hash-table (table &key key test size
                                     rehash-size rehash-threshold)
    "Returns a copy of hash table `table`, with the same keys and values
as the `table`. The copy has the same properties as the original, unless
overridden by the keyword arguments.

Before each of the original values is set into the new hash-table, `key`
is invoked on the value. As `key` defaults to `cl:identity`, a shallow
copy is returned by default."
    (setf key (or key 'identity))
    (setf test (or test (hash-table-test table)))
    (setf size (or size (hash-table-size table)))
    (setf rehash-size (or rehash-size (hash-table-rehash-size table)))
    (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold table)))
    (let ((copy (make-hash-table :test test :size size
                                 :rehash-size rehash-size
                                 :rehash-threshold rehash-threshold)))
      (maphash (lambda (k v)
                 (setf (gethash k copy) (funcall key v)))
               table)
      copy))
  

  (defun curry (function &rest arguments)
    "Returns a function that applies `arguments` and the arguments
it is called with to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        ;; Using M-V-C we don't need to append the arguments.
        (multiple-value-call fn (values-list arguments) (values-list more)))))

  (define-compiler-macro curry (function &rest arguments)
    (let ((curries (make-gensym-list (length arguments) "CURRY"))
          (fun (gensym "FUN")))
      `(let ((,fun (ensure-function ,function))
             ,@(mapcar #'list curries arguments))
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest more)
           (apply ,fun ,@curries more)))))
  

  (defun ensure-boolean (x)
    "Convert `x` into a Boolean value."
    (and x t))
  

  (defmacro ensure-gethash (key hash-table &optional default)
    "Like `gethash`, but if `key` is not found in the `hash-table` saves the `default`
under key before returning it. Secondary return value is true if key was
already in the table."
    `(multiple-value-bind (value ok) (gethash ,key ,hash-table)
       (if ok
           (values value ok)
           (values (setf (gethash ,key ,hash-table) ,default) nil))))
  

  (defun ensure-list (list)
    "If `list` is a list, it is returned. Otherwise returns the list designated by `list`."
    (if (listp list)
        list
        (list list)))
  

  (declaim (inline maphash-keys))
  (defun maphash-keys (function table)
    "Like `maphash`, but calls `function` with each key in the hash table `table`."
    (maphash (lambda (k v)
               (declare (ignore v))
               (funcall function k))
             table))
  

  (defun hash-table-keys (table)
    "Returns a list containing the keys of hash table `table`."
    (let ((keys nil))
      (maphash-keys (lambda (k)
                      (push k keys))
                    table)
      keys))
  

  (defun mappend (function &rest lists)
    "Applies `function` to respective element(s) of each `list`, appending all the
all the result list to a single list. `function` must return a list."
    (loop for results in (apply #'mapcar function lists)
          append results))
  

  (defun map-product (function list &rest more-lists)
    "Returns a list containing the results of calling `function` with one argument
from `list`, and one from each of `more-lists` for each combination of arguments.
In other words, returns the product of `list` and `more-lists` using `function`.

Example:

    (map-product 'list '(1 2) '(3 4) '(5 6))
     => ((1 3 5) (1 3 6) (1 4 5) (1 4 6)
         (2 3 5) (2 3 6) (2 4 5) (2 4 6))"
    (labels ((%map-product (f lists)
               (let ((more (cdr lists))
                     (one (car lists)))
                 (if (not more)
                     (mapcar f one)
                     (mappend (lambda (x)
                                (%map-product (curry f x) more))
                              one)))))
      (%map-product (ensure-function function) (cons list more-lists))))
  

  (defun mkstr (&rest args)
    "Receives any number of objects (string, symbol, keyword, char, number), extracts all printed representations, and concatenates them all into one string.

Extracted from _On Lisp_, chapter 4."
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))
  

  (defmacro once-only (specs &body forms)
    "Evaluates `forms` with symbols specified in `specs` rebound to temporary
variables, ensuring that each initform is evaluated only once.

Each of `specs` must either be a symbol naming the variable to be rebound, or of
the form:

    (symbol initform)

Bare symbols in `specs` are equivalent to

    (symbol symbol)

Example:

    (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
      (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
    (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
          (names-and-forms (mapcar (lambda (spec)
                                     (etypecase spec
                                       (list
                                        (destructuring-bind (name form) spec
                                          (cons name form)))
                                       (symbol
                                        (cons spec spec))))
                                   specs)))
      ;; bind in user-macro
      `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
              gensyms names-and-forms)
         ;; bind in final expansion
         `(let (,,@(mapcar (lambda (g n)
                             ``(,,g ,,(cdr n)))
                           gensyms names-and-forms))
            ;; bind in user-macro
            ,(let ,(mapcar (lambda (n g) (list (car n) g))
                    names-and-forms gensyms)
               ,@forms)))))
  

  (defun rcurry (function &rest arguments)
    "Returns a function that applies the arguments it is called
with and `arguments` to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        (multiple-value-call fn (values-list more) (values-list arguments)))))
  

  (defun set-equal (list1 list2 &key (test #'eql) (key nil keyp))
    "Returns true if every element of `list1` matches some element of `list2` and
every element of `list2` matches some element of `list1`. Otherwise returns false."
    (let ((keylist1 (if keyp (mapcar key list1) list1))
          (keylist2 (if keyp (mapcar key list2) list2)))
      (and (dolist (elt keylist1 t)
             (or (member elt keylist2 :test test)
                 (return nil)))
           (dolist (elt keylist2 t)
             (or (member elt keylist1 :test test)
                 (return nil))))))
  

  (deftype string-designator ()
    "A string designator type. A string designator is either a string, a symbol,
or a character."
    `(or symbol string character))
  

  (defmacro with-gensyms (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(let ,(mapcar (lambda (name)
                     (multiple-value-bind (symbol string)
                         (etypecase name
                           (symbol
                            (values name (symbol-name name)))
                           ((cons symbol (cons string-designator null))
                            (values (first name) (string (second name)))))
                       `(,symbol (gensym ,string))))
            names)
       ,@forms))

  (defmacro with-unique-names (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(with-gensyms ,names ,@forms))
  

  (defmacro with-open-file* ((stream filespec &key direction element-type
                                                   if-exists if-does-not-exist external-format)
                             &body body)
    "Just like `with-open-file`, but `nil` values in the keyword arguments mean to use
the default value specified for `open`."
    (once-only (direction element-type if-exists if-does-not-exist external-format)
      `(with-open-stream
           (,stream (apply #'open ,filespec
                           (append
                            (when ,direction
                              (list :direction ,direction))
                            (when ,element-type
                              (list :element-type ,element-type))
                            (when ,if-exists
                              (list :if-exists ,if-exists))
                            (when ,if-does-not-exist
                              (list :if-does-not-exist ,if-does-not-exist))
                            (when ,external-format
                              (list :external-format ,external-format)))))
         ,@body)))
  

  (defmacro with-output-to-file ((stream-name file-name &rest args
                                                        &key (direction nil direction-p)
                                                        &allow-other-keys)
                                 &body body)
    "Evaluate `body` with `stream-name` to an output stream on the file
`file-name`. `args` is sent as is to the call to `open` except `external-format`,
which is only sent to `with-open-file` when it's not `nil`."
    (declare (ignore direction))
    (when direction-p
      (error "Can't specifiy :DIRECTION for WITH-OUTPUT-TO-FILE."))
    `(with-open-file* (,stream-name ,file-name :direction :output ,@args)
       ,@body))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(compose copy-hash-table curry ensure-boolean ensure-gethash
            ensure-list hash-table-keys map-product mkstr once-only rcurry
            set-equal with-gensyms with-unique-names with-output-to-file)))

;;;; END OF quickutils.lisp ;;;;
