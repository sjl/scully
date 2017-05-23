(in-package :scully.players.random-ii)

(defvar *data-file* nil)
(defvar *current-game* nil)
(defvar *run* 0.0)
(defvar *gc* 0.0)


;;;; Random Incomplete-Information Player -------------------------------------
(defclass random-ii-player (ggp:ggp-player)
  ((role :type symbol :accessor rp-role)
   (reasoner :accessor rp-reasoner)
   (information-set :accessor rp-information-set)
   (turn :initform 0 :accessor rp-turn)
   (game-number :initform 0 :accessor rp-game-number)))

(define-with-macro (random-ii-player :conc-name rp)
  role reasoner information-set turn game-number)


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


(defun information-set-objects (iset)
  (apply #'+ (mapcar #'length iset)))

(defun dump-iset (iset)
  (iterate (for state :in iset)
           (for i :from 1)
           (format t "~%State ~D~%" i)
           (iterate (for term :in state)
                    (format t "    ~S~%" term))))


(defmethod ggp:player-start-game ((player random-ii-player) rules role timeout)
  (setf *data-file* (open "data-prolog" :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
        *run* 0.0
        *gc* 0.0)
  (sb-ext:gc :full t)
  ;; (format *data-file* "turn,information set size,cons/symbol count~%")
  (let ((reasoner (make-prolog-reasoner)))
    (incf (rp-game-number player))
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
    (incf turn)
    (setf information-set (scully.gdl:time-it
                            (*run* *gc*)
                            (if move
                              (get-next-information-set player move percepts)
                              (list (initial-state reasoner)))))
    (format *data-file* "~A,~D,~D,~D,~D,~,4F,~,4F~%"
            *current-game*
            game-number
            turn
            (length information-set)
            (information-set-objects information-set)
            *run*
            *gc*)))


(defmethod ggp:player-select-move ((player random-ii-player) timeout)
  (format t "Selecting move...~%")
  (with-random-ii-player (player)
    (format t "Information set size: ~D~%" (length information-set))
    (format t "Information set object count ~D~%"
            (information-set-objects information-set))
    (-<> information-set
      first
      (legal-moves-for reasoner role <>)
      pr
      scully.gdl:sort-moves
      first)))


;;;; Run ----------------------------------------------------------------------
(setf hunchentoot:*default-connection-timeout* nil) ; its_fine

(defvar *player* (make-instance 'random-ii-player
                                :name "Scully-Random-II"
                                :port 5002))

(setf *current-game* 'mastermind448)

;; (ggp:start-player *player* :server :hunchentoot :use-thread t)
;; (ggp:kill-player *player*)
