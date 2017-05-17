(in-package :scully.players.random-zdd)

(defvar *data-file* nil)


;;;; Random Incomplete-Information Player -------------------------------------
(defclass random-zdd-player (ggp:ggp-player)
  ((role :type symbol :accessor rp-role)
   (reasoner :accessor rp-reasoner)
   (information-set :accessor rp-iset)
   (turn :initform 0 :accessor rp-turn)))

(define-with-macro (random-zdd-player :conc-name rp)
  role reasoner iset turn)


(defmethod ggp:player-start-game ((player random-zdd-player) rules role timeout)
  (setf *data-file* (open "data-zdd" :direction :output :if-exists :append))
  ;; (format *data-file* "turn,information set size,zdd node count,max node count~%")
  (scully.zdd::with-zdd
    (let* ((grounded-rules (-<> rules
                             (with-output-to-string (s)
                               (dump-gdl <> s))
                             (ground-gdl-string <>)))
           (reasoner (make-zdd-reasoner grounded-rules)))
      (setf (rp-role player) role
            (rp-turn player) 0
            (rp-reasoner player) reasoner)
      t)))

(defmethod ggp:player-stop-game ((player random-zdd-player))
  (format t "Done~%")
  (finish-output *data-file*)
  (close *data-file*)
  (scully.zdd::with-zdd
    (with-random-zdd-player (player)
      (setf role nil
            reasoner nil
            iset nil))))

(defun information-set-objects (iset)
  (apply #'+ (mapcar #'length iset)))

(defmethod ggp:player-update-game-ii ((player random-zdd-player) move percepts)
  (incf (rp-turn player))
  (format t "~2%=====================================~%")
  (pr percepts)

  (let ((max-size 0))
    (scully.zdd::with-zdd
      (with-random-zdd-player (player)
        (setf iset
              (if move
                (-<> iset
                  (progn
                    (format t "Information set size: ~D states, ~D ZDD nodes~%"
                            (scully.zdd:zdd-count <>)
                            (scully.zdd:zdd-node-count <>))
                    (format t "      Iset cons size: ~D things~%"
                            (information-set-objects (scully.zdd::zdd-enumerate <>)))
                    <>)
                  (sprout reasoner <> (rp-role player) move)
                  (progn (format t "After sprouting size: ~D states~%"
                                 (scully.zdd:zdd-count <>))
                         <>)
                  (apply-happens reasoner <>)
                  (progn
                    (setf max-size (scully.zdd:zdd-node-count <>))
                    (format t "          Max size: ~D ZDD nodes~%" max-size)
                    <>)
                  (filter-iset-for-percepts reasoner <> role percepts)
                  (progn (format t "After filtering size: ~D states~%"
                                 (scully.zdd:zdd-count <>))
                         <>)
                  (compute-next-iset reasoner <>)
                  ;; (progn (dump-iset reasoner <>)
                  ;;        <>)
                  (apply-possible reasoner <>))
                (apply-possible reasoner (initial-iset reasoner))))
        (let ((object-size (information-set-objects (scully.zdd::zdd-enumerate iset))))
          (format *data-file* "~D,~D,~D,~D,~D~%" turn
                  (scully.zdd:zdd-count iset)
                  (scully.zdd:zdd-node-count iset)
                  max-size
                  object-size))))))

(defmethod ggp:player-select-move ((player random-zdd-player) timeout)
  (scully.zdd::with-zdd
    (format t "Selecting move...~%")
    (with-random-zdd-player (player)
      ;; (format t "CURRENT ISET:~%")
      ;; (dump-iset reasoner iset)
      ;; (format t "LEGAL MOVES:~%")
      ;; (pr (legal-moves-for reasoner iset role))
      (random-elt (legal-moves-for reasoner iset role)))))


;;;; Run ----------------------------------------------------------------------
(setf hunchentoot:*default-connection-timeout* nil) ; its_fine

(defvar *player* (make-instance 'random-zdd-player
                                :name "Scully-Random-ZDD"
                                :port 5003))

;; (ggp:start-player *player* :server :hunchentoot :use-thread t)
;; (ggp:kill-player *player*)
