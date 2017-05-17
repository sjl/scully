(in-package :scully.players.random-ii)

(defvar *data-file* nil)


;;;; Random Incomplete-Information Player -------------------------------------
(defclass random-ii-player (ggp:ggp-player)
  ((role :type symbol :accessor rp-role)
   (reasoner :accessor rp-reasoner)
   (information-set :accessor rp-information-set)
   (turn :initform 0 :accessor rp-turn)))

(define-with-macro (random-ii-player :conc-name rp)
  role reasoner information-set turn)


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
  (setf *data-file* (open "data-prolog" :direction :output :if-exists :append))
  ;; (format *data-file* "turn,information set size,cons/symbol count~%")
  (let ((reasoner (make-prolog-reasoner)))
    (load-rules reasoner rules)
    (setf (rp-role player) role
          (rp-turn player) 0
          (rp-reasoner player) reasoner)))

(defmethod ggp:player-stop-game ((player random-ii-player))
  (finish-output *data-file*)
  (close *data-file*)
  (setf (rp-role player) nil
        (rp-reasoner player) nil
        (rp-information-set player) nil))

(defmethod ggp:player-update-game-ii ((player random-ii-player) move percepts)
  (format t "~2%=====================================~%")
  (with-random-ii-player (player)
    (setf information-set
          (if move
            (get-next-information-set player move percepts)
            (list (initial-state reasoner))))
    (format *data-file* "~D,~D,~D~%"
            turn
            (length information-set)
            (information-set-objects information-set))))

(defun information-set-objects (iset)
  (apply #'+ (mapcar #'length iset)))

(defmethod ggp:player-select-move ((player random-ii-player) timeout)
  (format t "Selecting move...~%")
  (with-random-ii-player (player)
    (format t "Information set size: ~D~%" (length information-set))
    (format t "Information set object count ~D~%"
            (information-set-objects information-set))
    (random-elt (legal-moves-for reasoner role (first information-set)))))


;;;; Run ----------------------------------------------------------------------
(setf hunchentoot:*default-connection-timeout* nil) ; its_fine

(defvar *player* (make-instance 'random-ii-player
                                :name "Scully-Random-II"
                                :port 5002))

;; (ggp:start-player *player* :server :hunchentoot :use-thread t)
;; (ggp:kill-player *player*)
