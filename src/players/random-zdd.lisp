(in-package :scully.players.random-zdd)


;;;; Random Incomplete-Information Player -------------------------------------
(defclass random-zdd-player (ggp:ggp-player)
  ((role :type symbol :accessor rp-role)
   (reasoner :accessor rp-reasoner)
   (information-set :accessor rp-iset)))

(define-with-macro (random-zdd-player :conc-name rp)
  role reasoner iset)


(defmethod ggp:player-start-game ((player random-zdd-player) rules role timeout)
  (scully.zdd::with-zdd
    (let* ((grounded-rules (-<> rules
                             (with-output-to-string (s)
                               (dump-gdl <> s))
                             (ground-gdl-string <>)))
           (reasoner (make-zdd-reasoner grounded-rules)))
      (setf (rp-role player) role
            (rp-reasoner player) reasoner)
      t)))

(defmethod ggp:player-stop-game ((player random-zdd-player))
  (scully.zdd::with-zdd
    (with-random-zdd-player (player)
      (setf role nil
            reasoner nil
            iset nil))))

(defmethod ggp:player-update-game-ii ((player random-zdd-player) move percepts)
  (format t "~2%=====================================~%")
  (scully.zdd::with-zdd
    (with-random-zdd-player (player)
      (setf iset
            (if move
              (-<> iset
                (progn (format t "Information set size: ~D states, ~D ZDD nodes~%"
                               (scully.zdd:zdd-count <>)
                               (scully.zdd:zdd-node-count <>))
                       <>)
                (sprout reasoner <> (rp-role player) move)
                (apply-happens reasoner <>)
                (progn
                  (format t "        Max size: ~D ZDD nodes~%"
                          (scully.zdd:zdd-node-count <>))
                  <>)
                (filter-iset-for-percepts reasoner <> role percepts)
                (compute-next-iset reasoner <>)
                (apply-possible reasoner <>))
              (apply-possible reasoner (initial-iset reasoner)))))))

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

(ggp:start-player *player* :server :hunchentoot :use-thread t)
;; (ggp:kill-player *player*)
(slot-value *player* 'ggp::request-lock)
