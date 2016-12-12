;;; Rulekit program to solve the Tower of Hanoi problem.
;;; PARMENIDES version.

(setq *CR-STRATEGY* *MEA*)

(literalize move (extendable nil)
  stack ()
  from ()
  to ())

(literalize on (extendable nil)
  disk ()
  peg ())

(RULE break-down-problem
  :LHS ((move (label =c3) :stack =stack :from =from :to =to
	      (CHECK (and (listp =stack) (cdr =stack))))
	               ;;make sure there are at least two disks on the peg
	(on :disk =d :peg =from (CHECK (subsetp =stack =d))))
  :RHS (($remove =c3)
	($make 'move :stack (allbutlast =stack) :from (otherpeg =from =to) :to =to)
	($make 'move :stack (car (last =stack)) :from =from :to =to)
	($make 'move :stack (allbutlast =stack) :from  =from :to (otherpeg =from =to))
 ))

(RULE move-disk
  :LHS ((move (LABEL =c1) :stack =stack :from =from :to =to
	      (CHECK (and =stack (atom =stack))))    ;;if only one disk is on peg
	(on (LABEL =c2) :disk =d :peg =from (CHECK (and =d (= =stack (car =d)))))
	(on (LABEL =c3) :disk =x :peg =to
	    (CHECK (or (null =x) (> (car =x) =stack)))))
  ;EXTRA-TESTS (= 3 3)
  :RHS (($remove =c1)
	($modify =c3 :disk (cons =stack =x))    ;;and put it onto the other stack.
	($modify =c2 :disk (cdr =d))            ;;take that disk off the stack
	(format t "Moving disk ~A to top of peg ~A" =stack =to)))

(defun begin ()
  (start
   (on :peg 1 :disk '(1 2 3))
   (on :peg 2 :disk NIL)
   (on :peg 3 :disk NIL)
   (move :stack '(1 2 3) :from 1 :to 3)))

;;Utility functions for tower-of-Hanoi
(defun allbutlast (a)
  (let ((a (reverse (cdr (reverse a)))))
    (if (null (cdr a)) (car a) a)))

(defun otherpeg (a b)
  (car (delete a (delete b (copy-list '(1 2 3))))))
