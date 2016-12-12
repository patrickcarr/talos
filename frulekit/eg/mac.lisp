;;; A macro Rulekit rule for algebra  March 19, 1986 
;;; and an iterative rule based on the macro rule, written by Pat Chang.
;;; Modified to work with FRulekit 11-Feb-87 PHS.

;;; Requires: goalmon, algaux, agenda

;;; Questions: why isn't select-var+, - here? solve-for-unknown-n & dec-solve?

;;(load "/ml/usr/pshell/frulekit/agenda.sfasl")
(literalize equation ()
  name T
  formula T
  quantity T
  elements T
  selected T)

(literalize var ()
  name T
  selected T
  result T
  desired-unknown T)

(literalize count ()
  eqns T
  vars T
  iters T)

(literalize iterate ()
  flag T)

(RULE mac-solve                                ;; not iterative
    :LHS ((current-goal :name solve-unknown)
	  (var :name =u :desired-unknown :yes)
;	  (iterate :flag nil)   ;to be inserted by mod-mac-lhs
	  (equation  :name =e1 :elements =es1)
	  (equation  :name =e2 :elements =es2 (CHECK  (not (equal =e1 =e2))))
	  (var :name =v1 (LABEL =v)
	       (CHECK (not (equal =v1 =u)))
	       (CHECK (member =v1 =es1))
	       (CHECK (member =v1 =es2)))
	  (count :eqns =:nume :vars =:numv (LABEL =c)))
;**change all =numv to =:numv.  Similarly =nume

    :RHS  (($modify =v :selected :yes)  ;temp
	   (r-agenda rhs)))

(setq rhs '((b1 m-var-on-lhs+ m-var-on-lhs-)  ;; rule that returns halt is
	    (b2 m-replace- m-replace+)))      ;; always the 1st rule in a
					      ;; bucket

(RULE m-var-on-lhs+
  :LHS ((var :name =v1 :selected :yes)              ;; temp
	(equation :name =e1 (LABEL =e) :quantity =v1))
  
  :RHS (($modify =e :selected :yes)
	:bucket-halt))

(RULE m-var-on-lhs-
  :LHS ((var :name =v1 :selected :yes)              ;; temp
	(<ABS> (equation :name =e1 :quantity =v1))
	(equation (LABEL =e) :name =e2 :quantity =q :formula =f :elements =es2
		  (CHECK (member =v1 =es2))))
  
  :RHS  (($modify  =e
		   :quantity =v1
		   :formula (rearrange =f =q =es2 =v1))))

(RULE m-replace-
  :LHS ((count :eqns =:nume :vars =:numv (LABEL =c))
	(var :name =v1 (LABEL =v) :selected :yes)               ;; temporary
	(equation :name =e1 :selected :yes :quantity =v1 (LABEL =e))
	(<ABS> (equation :name =e2  :elements =es2
			 (CHECK (not (equal =e1 =e2)))
			 (CHECK (memq =v1 =es2)))))
  
  :RHS (($modify =c :eqns (- =:nume 1) :vars (- =:numv 1))
	($remove =v)
	($remove =e)
	:bucket-halt))

(RULE m-replace+
  :LHS ((var :name =v1 (LABEL =v) :selected :yes)               ;; temporary
	(equation :name =e1 :selected :yes :elements =es1
		  :quantity =v1  :formula =f1); new equation has been returned
	(equation :name =e2 :formula =rhs (LABEL =e)
		  :quantity =lhs :elements =es2
		  (CHECK (not (equal =e1 =e2)))
		  (CHECK (memq =v1 =es2)))) ; var is in equation =e2
  
  :RHS (($modify =e
		 :quantity (subst =f1 ; new
				  =v1 ; old
				  =lhs) ; tree
		 :formula (subst  =f1  =v1 =rhs)
		 :elements (delete =v1 (union =es1 =es2) :test 'equal))))


(setq !!control 'bucket)

; iterative rule:
(RULE iter-solve
    :LHS ((current-goal :name solve-unknown)
	  (var :name =u :desired-unknown :yes)
	  (iterate :flag yes (LABEL =i))                 ;; temp
	  (count :eqns =:nume :vars =:numv (CHECK (= =:nume =:numv))
		 (LABEL =c)))
    
    :RHS (($modify =i :flag 'nil) (r-cycle mac-bucket)))

(setq mac-bucket (list 'mac-solve)) ; mac-bucket holds the mac-solve rule

;;; If unknown is the only var in an equation and there are no other
;;; equations containing unknown, then solve for unknown
(RULE solve-for-unknown-1
    :LHS ((count :eqns 1 :vars 1)
	  (var :name =v1 :desired-unknown :yes (LABEL =u))
	  (equation :name =e1 :quantity =q :formula =f :elements =el
		    (LABEL =e) :elements =es :quantity =lhs
		    (CHECK (member =v1 =es))
;	      (CHECK (not (equal =v1 =lhs)))
		    (CHECK (not (cdr =es))))  ; unknown is the only var
	  (<ABS> (equation :name  =e2 :elements =es2
			   (CHECK (not (equal =e1 =e2)))
			   (CHECK (member =v1 =es2))))
	  (current-goal :name solve-unknown))

    :RHS ((setq algres (rearrange =f =q =el =v1))
	  ($modify =e
		   :selected :yes
		   :quantity =v1
		   :formula algres)
	  ($modify =u :result algres)
	  (format t "The answer is ~S ~%" algres)
	  (pop-success)
	  algres))

(RULE done
    :LHS ((current-goal :name top :status successful))
    :RHS ((format t "Done ~%")
	  (halt)))


;;; FULLMEMBER
(defun fullmember (atm s-exp)		;; T iff atm is in s-exp at any depth
  (cond ((null s-exp) nil)
	((or (atom s-exp) (rk-variablep s-exp))  ;; atm can be Rulekit variables
	 (equal atm s-exp))
	(T (or (fullmember atm (car s-exp))
	       (fullmember atm (cdr s-exp))))))


(defun begin ()
  (clear-net)
  (setq *CYCLE* 0)
  (init-goalmon)
  (spawn-subgoals '(solve-unknown))
  (CONT (var :name :z)
	(var :name :y)
	(var :name :x :desired-unknown :yes)
	(iterate :flag nil)                  ;temp: set to 'yes for iter
	(count :eqns 3 :vars 3)
	(equation :name :eqn-2 :quantity :z :formula '(- 10 (* 3  :x))
		  :elements '( :z  :x))
	(equation :name :eqn-3 :quantity :z :formula '(/ (* 4 :y) 3)
		  :elements '( :y  :z))	  
	(equation :name :eqn-1 :quantity 10 :formula '(+ (*  :x 2) (* :y 2))
		  :elements '( :x  :y))))
