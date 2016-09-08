(in-package #:scully.player)


;;;; Brain Connections --------------------------------------------------------
(defstruct (brain (:constructor %make-brain (name stream socket)))
  name stream socket)


(defun make-brain (name port)
  (let ((socket (usocket:socket-connect "127.0.0.1" port
                                        :protocol :stream
                                        :element-type '(unsigned-byte 8))))
    (%make-brain name (usocket:socket-stream socket) socket)))


(defun brain-send (brain tag &rest arguments)
  (conspack:encode (list* tag arguments)
                   :stream (brain-stream brain))
  (finish-output (brain-stream brain))
  (values))

(defun brain-read (brain)
  (values (conspack:decode-stream (brain-stream brain))))


;;;; Player -------------------------------------------------------------------
(defclass scully-player (ggp:ggp-player)
  ((brains :accessor player-brains :initarg :brains)))

(defun make-player ()
  (make-instance 'scully-player
                 :name "Scully"
                 :port 5000
                 :brains (list (make-brain :random 5001))))


(defun broadcast-to-brains (player tag &rest arguments)
  (iterate (for brain :in (player-brains player))
           (apply #'brain-send brain tag arguments))
  (values))

(defun gather-responses (player timeout)
  (iterate
    (with remaining = (player-brains player))
    (with results = nil)
    (when (null remaining)
      (return results))
    (for (values ready remaining-time) = (usocket:wait-for-input
                                           (mapcar #'brain-socket remaining)
                                           :timeout timeout
                                           :ready-only t))
    (if (null remaining-time)
      (return results)
      (progn
        (setf timeout remaining-time)
        (iterate
          (for brain :in (copy-list remaining))
          (when (member (brain-socket brain) ready)
            (push (cons (brain-name brain) (brain-read brain)) results)
            (setf remaining (remove brain remaining))))))))

(defun select-move-response (responses)
  (let ((resp-random (assoc :random responses)))
    (cdr resp-random)))


(defmethod ggp:player-start-game ((player scully-player) rules role timeout)
  (broadcast-to-brains player :start rules role timeout))

(defmethod ggp:player-stop-game ((player scully-player))
  (broadcast-to-brains player :stop)
  (broadcast-to-brains player :quit))

(defmethod ggp:player-update-game ((player scully-player) moves)
  (broadcast-to-brains player :update moves))

(defmethod ggp:player-select-move ((player scully-player) timeout)
  (broadcast-to-brains player :start-thinking)
  (let ((seconds (* internal-time-units-per-second
                    (- timeout (get-internal-real-time)))))
    (sleep (- seconds 2))
    (broadcast-to-brains player :request-move)
    (select-move-response (gather-responses player (- seconds 1)))))


(defvar *player* nil)

(defun start-player ()
  (setf *player* (make-player))
  (ggp:start-player *player*)
  *player*)

; (ggp:start-player *player*)
; (ggp:kill-player *player*)
