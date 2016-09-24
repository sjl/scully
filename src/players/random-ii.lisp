(in-package #:scully.players.random-ii)


;;;; Random Incomplete-Information Player -------------------------------------
(defclass random-ii-player (ggp:ggp-player)
  ((role :type symbol :accessor rp-role)
   (reasoner :accessor rp-reasoner)
   (information-set :accessor rp-information-set)))

(define-with-macro (random-ii-player :conc-name rp)
  role reasoner information-set)


(defun percepts-match-p (player state moves percepts)
  (set-equal percepts
             (percepts-for (rp-reasoner player) (rp-role player) state moves)
             :test #'equal))

(defun get-possible-moves (player state move)
  (let* ((reasoner (rp-reasoner player))
         (our-role (rp-role player))
         (other-roles (remove our-role (roles reasoner)))
         (other-moves (mapcar (lambda (role)
                                (mapcar (curry #'cons role)
                                        (legal-moves-for reasoner role state)))
                              other-roles)))
    (apply #'map-product #'list
           (list (cons our-role move))
           other-moves)))

(defun get-next-states (player state move percepts)
  (-<> (get-possible-moves player state move)
    (mapcar (lambda (moves)
              (when (percepts-match-p player state moves percepts)
                (next-state (rp-reasoner player) state moves)))
            <>)
    (remove nil <>)
    (remove-duplicates <> :test #'equal)))

(defun get-next-information-set (player move percepts)
  (iterate (for state :in (rp-information-set player))
           (unioning (get-next-states player state move percepts)
                     :test #'equal)))


(defmethod ggp:player-start-game ((player random-ii-player) rules role timeout)
  (let ((reasoner (make-prolog-reasoner)))
    (load-rules reasoner rules)
    (setf (rp-role player) role
          (rp-reasoner player) reasoner
          (rp-information-set player) (list (initial-state reasoner)))))

(defmethod ggp:player-stop-game ((player random-ii-player))
  (setf (rp-role player) nil
        (rp-reasoner player) nil
        (rp-information-set player) nil))

(defmethod ggp:player-update-game-ii ((player random-ii-player) move percepts)
  (when move
    (setf (rp-information-set player)
          (get-next-information-set player move percepts))))

(defmethod ggp:player-select-move ((player random-ii-player) timeout)
  (with-random-ii-player (player)
    (format t "Information set size: ~D~%" (length information-set))
    ; (let ((*package* (find-package :ggp-rules)))
    ;   (iterate (for state :in information-set)
    ;            (format t "    ~S~%" state)))
    (format t "Selecting move...~%")
    (random-elt (legal-moves-for reasoner role (first information-set)))))


;;;; Run ----------------------------------------------------------------------
(setf hunchentoot:*default-connection-timeout* nil) ; its_fine

(defvar *player* (make-instance 'random-ii-player
                                :name "Scully-Random-II"
                                :port 5002))

; (ggp:start-player *player* :server :hunchentoot :use-thread t)
; (ggp:kill-player *player*)
