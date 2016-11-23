(in-package :scully.players.random)


;;;; Random Player ------------------------------------------------------------
(defclass random-player (ggp:ggp-player)
  ((role :type symbol :accessor rp-role)
   (reasoner :accessor rp-reasoner)
   (state :accessor rp-state)))

(define-with-macro (random-player :conc-name rp)
  role reasoner state)


(defmethod ggp:player-start-game ((player random-player) rules role timeout)
  (let ((reasoner (make-prolog-reasoner)))
    (load-rules reasoner (-> rules
                           scully.gdl:dump-gdl
                           scully.grounders.fluxplayer:ground-gdl-string))
    (setf (rp-role player) role
          (rp-reasoner player) reasoner
          (rp-state player) (initial-state reasoner))))

(defmethod ggp:player-stop-game ((player random-player))
  (setf (rp-state player) nil
        (rp-reasoner player) nil
        (rp-role player) nil))

(defmethod ggp:player-update-game ((player random-player) moves)
  (when moves
    (with-random-player (player)
      (setf state (next-state reasoner state moves)))))

(defmethod ggp:player-select-move ((player random-player) timeout)
  (with-random-player (player)
    (random-elt (legal-moves-for reasoner role state))))


;;;; Run ----------------------------------------------------------------------
(setf hunchentoot:*default-connection-timeout* nil) ; its_fine

(defvar *random-player* (make-instance 'random-player
                                       :name "Scully-Random"
                                       :port 5001))

; (ggp:start-player *random-player* :server :hunchentoot)
; (ggp:kill-player *random-player*)
