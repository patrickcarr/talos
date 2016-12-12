;;; Rulekit program to solve the Tower of Hanoi problem.
;;; PARMENIDES version.

(setq *CR-STRATEGY* *MEA*)
(defvar *VERBOSE-HANOI* T)

(literalize move (extendable nil)
  stack NIL
  from NIL
  to NIL)

(literalize on (extendable nil)
  disk NIL
  peg NIL)

(RULE break-down-problem
  :LHS ((move (LABEL =c3) :stack =stack :from =from :to =to
	      (CHECK (listp =stack)))   ;; Must be a 'non-trivial' move
	(on :disk =d :peg =from (CHECK (subsetp =stack =d))))
  :RHS (($remove =c3)
	($make 'move :stack (allbutlast =stack) :from (otherpeg =from =to) :to =to)
	($make 'move :stack (car (last =stack)) :from =from :to =to)
	($make 'move :stack (allbutlast =stack) :from =from :to (otherpeg =from =to))
 ))

(RULE move-disk
  :LHS ((move (LABEL =c1) :stack =stack :from =from :to =to)
	(on (LABEL =c2) :disk =d :peg =from (CHECK (eq =stack (car =d))))
	(on (LABEL =c3) :disk =x :peg =to
	    (CHECK (or (null =x) (> (car =x) =stack)))))
  ;;EXTRA-TESTS (= 3 3)
  :RHS (($remove =c1)
	($modify =c3 :disk (cons =stack =x))    ;;and put it onto the other stack.
	($modify =c2 :disk (cdr =d))            ;;take that disk off the stack
	(if *VERBOSE-HANOI*
 	  (format T "Moving disk ~A to top of peg ~A~%" =stack =to))))

(defun begin ()
  (solve-n :n 3))

(defun solve-n (&key (n 3) (cycles 12))
  (let ((initial-list (iota-list n)))
    (set-wm
      (on :peg 1 :disk initial-list)
      (on :peg 2 :disk NIL)
      (on :peg 3 :disk NIL)
      (move :stack initial-list :from 1 :to 3))
    (run cycles)))

;;; Returns a list from 1 to n.
(defun iota-list (n)
  (let (res)
       (dotimes (i (1+ n))
	 (push i res))
       (cdr (nreverse res))))

;;; Utility functions for Tower-of-Hanoi
(defun allbutlast (a)
  (let ((a (nreverse (cdr (reverse a)))))
    (if (null (cdr a)) (car a) a)))

(defun otherpeg (a b)
  (car (delete a (delete b (copy-list '(1 2 3))))))
