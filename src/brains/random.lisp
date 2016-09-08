(in-package #:scully.brains.random)


;;;; Data ---------------------------------------------------------------------
(defparameter *brain* nil)
(defparameter *server* nil)


;;;; Random Brain -------------------------------------------------------------
(defclass random-brain ()
  ((role :type symbol :accessor brain-role)
   (database :accessor brain-database)
   (state :accessor brain-state)))


(defun start-game (brain rules role timeout)
  (setf (brain-role brain) role
        (brain-database brain) (make-database))
  (with-database (brain-database brain)
    (load-gdl-preamble)
    (push-logic-frame-with
      (load-rules rules))
    (setf (brain-state brain) (initial-state))))

(defun stop-game (brain)
  (setf (brain-state brain) nil
        (brain-database brain) nil
        (brain-role brain) nil))

(defun update-game (brain moves)
  (when moves
    (with-database (brain-database brain)
      (apply-state (brain-state brain))
      (apply-moves moves)
      (setf (brain-state brain) (next-state))
      (clear-moves)
      (clear-state))))

(defun random-nth (list)
  (nth (random (length list)) list))

(defun select-move (brain timeout)
  (with-database (brain-database brain)
    (prog2
      (apply-state (brain-state brain))
      (cdr (random-nth (legal-moves-for (brain-role brain))))
      (clear-state))))


;;;; Sockets ------------------------------------------------------------------
(defun read-message (stream)
  (conspack:decode-stream stream))

(defun write-message (out stream &rest message)
  (format out "writing to stream... ~S~%" message)
  (conspack:encode message :stream stream)
  (finish-output stream))

(defun handle (stream brain out)
  (loop
    (destructuring-bind (tag . arguments)
        (read-message stream)
      (case tag
        (:quit (return-from handle))
        (:double (write-message out stream (* 2 (car arguments))))
        (t (format out "Unknown message: ~S ~S~%" tag arguments))))))

(defun run-brain-server (brain port)
  (setf *server*
        (usocket:socket-server
          "127.0.0.1" port
          'handle (list brain *standard-output*)
          :in-new-thread t
          :protocol :stream
          :element-type '(unsigned-byte 8)
          :reuse-address t)))

(defun run-brain (port)
  (let ((brain (make-instance 'random-brain)))
    (setf *brain* brain)
    (run-brain-server brain port)))
