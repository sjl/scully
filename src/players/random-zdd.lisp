(in-package :scully.players.random-zdd)

(defparameter *current-game* 'stratego)
(defvar *prev-time* 0.0)
(defvar *prev-gc* 0.0)
(defvar *run* 0)
(defvar *gc* 0)


;;;; Random Incomplete-Information Player -------------------------------------
(defclass random-zdd-player (ggp:ggp-player)
  ((role :type symbol :accessor rp-role)
   (reasoner :accessor rp-reasoner)
   (information-set :accessor rp-iset)
   (turn :initform 0 :accessor rp-turn)
   (game :initform 0 :accessor rp-game)))

(define-with-macro (random-zdd-player :conc-name rp)
  role reasoner iset turn game)


(defmethod ggp:player-start-game ((player random-zdd-player) rules role timeout)
  (incf (rp-game player))
  (sb-ext:gc :full t)
  (setf *prev-time* 0.0
        *prev-gc* 0.0)
  (scully.zdd::with-zdd
    (let* ((grounded-rules (-<> rules
                             (with-output-to-string (s)
                               (dump-gdl <> s))
                             (ground-gdl-string <>)))
           (reasoner (make-zdd-reasoner grounded-rules)))
      (when (null grounded-rules)
        (error "Could not ground rules.  Are they valid GDL?"))
      (setf (rp-role player) role
            (rp-turn player) 0
            (rp-reasoner player) reasoner)
      t)))

(defmethod ggp:player-stop-game ((player random-zdd-player))
  (format t "Done~%")
  (scully.zdd::with-zdd
    (with-random-zdd-player (player)
      (setf role nil
            reasoner nil
            iset nil))))


(defun information-set-objects (iset)
  (apply #'+ (mapcar #'length iset)))

(defun debug-log (obj &rest args)
  (apply #'format t args)
  (fresh-line)
  (finish-output)
  obj)

(defun log-iset-data (data-file game state-count node-count object-size)
  (with-open-file (data data-file :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (format data "~A ~D ~D ~D~%"
            game
            state-count
            node-count
            object-size)
    (finish-output data)))

(defmethod ggp:player-update-game-ii ((player random-zdd-player) move percepts)
  (incf (rp-turn player))
  (format t "~2%=====================================~%")
  (pr percepts)

  (let ((state-count 0)
        (node-count 0)
        (max-node-count 0)
        (object-size 0)
        (elapsed 0.0)
        (gc 0.0))
    (scully.zdd::with-zdd
      (with-random-zdd-player (player)
        (format t "Computing next information set...~%")
        (setf
          iset (time-it (*run* *gc*)
                 (if move
                   (-<> iset
                     ;; (debug-log <> "  Sprouting...")
                     (sprout reasoner <> (rp-role player) move)
                     ;; (progn (format t "After sprouting size: ~D states~%"
                     ;;                (scully.zdd:zdd-count <>))
                     ;;        <>)
                     ;; (debug-log <> "  Happens...")
                     ;; (let ((*trace-output* *standard-output*))
                     ;;   (start-profiling :mode :alloc)
                     ;;   (prog1 (time (apply-happens reasoner <>))
                     ;;     (stop-profiling)
                     ;;     (break)))
                     ;; (let ((*trace-output* *standard-output*))
                     ;;   (time (apply-happens reasoner <>)))
                     (apply-happens reasoner <>)
                     ;; (progn (setf max-node-count (scully.zdd:zdd-node-count <>))
                     ;;        <>)
                     ;; (debug-log <> "  Filtering percepts...")
                     (filter-iset-for-percepts reasoner <> role percepts)
                     ;; (debug-log <> "  Computing next...")
                     (compute-next-iset reasoner <>)
                     ;; (progn (dump-iset reasoner <>) <>)
                     )
                   (initial-iset reasoner)))
          elapsed (+ *prev-time* *run*)
          gc (+ *prev-gc* *run*))
        ;; (dump-iset reasoner iset)
        ;; (break)
        (debug-log iset "  Counting nodes...")
        (setf state-count (scully.zdd:zdd-count iset)
              node-count (scully.zdd:zdd-node-count iset)
              object-size (information-set-objects (scully.zdd::zdd-enumerate iset)))
        (format t "Information set size: ~D states, ~D ZDD nodes~%" state-count node-count)
        (format t "      Iset cons size: ~D conses~%" object-size)
        (format t "       Max iset size: ~D ZDD nodes~%" max-node-count)
        (log-iset-data "data-iset-sizes"
                       *current-game* state-count node-count object-size)
        ;; (format *data-file* "~A,~D,~D,~D,~D,~D,~D,~,4F,~,4F~%"
        ;;         *current-game*
        ;;         game
        ;;         turn
        ;;         state-count
        ;;         node-count
        ;;         max-node-count
        ;;         object-size
        ;;         elapsed
        ;;         gc)
        ))))

(defmethod ggp:player-select-move ((player random-zdd-player) timeout)
  (scully.zdd::with-zdd
    (format t "Selecting move...~%")
    (with-random-zdd-player (player)
      (debug-log iset "  Applying possible...")
      (setf iset (time-it (*run* *gc*) (apply-possible reasoner iset))
            *prev-time* *run*
            *prev-gc* *gc*)
      (debug-log iset "  Calculating moves...")
      ;; (format t "CURRENT ISET:~%")
      ;; (dump-iset reasoner iset)
      ;; (format t "LEGAL MOVES:~%")
      ;; (pr (legal-moves-for reasoner iset role))
      (pr (random-elt (scully.gdl::sort-moves (legal-moves-for reasoner iset role)))))))


;;;; Run ----------------------------------------------------------------------
(setf hunchentoot:*default-connection-timeout* nil) ; its_fine

(defvar *player* (make-instance 'random-zdd-player
                   :name "Scully-Random-ZDD"
                   :port 5003))

;; (setf *current-game* 'latenttictactoe)
;; (setf scully.terms::*shuffle-variables* nil)

;; (ggp:start-player *player* :server :hunchentoot :use-thread t)
;; (ggp:kill-player *player*)
;; (sb-ext:gc :full t)
