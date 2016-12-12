
;;;; 
;;;; Rulekit algebra rules for Fermi.
;;;;

(use-package '(frulekit parmenides))

;;; HISTORY:
;;;
;;; 26-June-89.  Pete	Converted the goal management stuff to make its actions
;;; 	more apparant to learning algorithms.  Changed (pop-success) to:
;;; 	($modify =goal :name (pop-success-name)), and changed (choose-subgoal)
;;; 	to:  ($modify =goal (choose-subgoal-name (find-new-goal =subs) 'result))
;;;
;;; 18-July-86.  Pete	Converted to the new FRulekit syntax.
;;;
;;; 9-Mar-86.  Pat Cheng.  Created.
;;; 

;;; Question: what does this system do when a variable's value becomes known?
;;; (i.e., not a variable any more).  Answer: i guess you can notate its
;;; value by adding an equation with the varname on the lhs and value on
;;; the rhs.

(literalize equation ()
  :name T
  :formula T
  :quantity T
  :elements T
  :selected T)

(literalize var ()
  :name T
  :selected NIL
  :result NIL
  :desired-unknown NIL)

(literalize count ()
  :eqns T
  :vars T
  :iters T )

(literalize newvar ()
  :name NIL
  :value NIL
  :elements NIL
  :class NIL)

(literalize elaboration ()
  :class NIL
  :value NIL
  :slot NIL)

;; ex equation:  (equation :name eqn-1 :quantity :x 
;;		      :formula (* 2 :y) :elements (:x :y))
;; :quantity should have a single element, either a number or a var
;; :formula has a list that is an algebraic expression
;; variables should be keywords  (WHY?)

;;; Added 23-Apr-89.  With the revised Rule-Fermi, its actions are opened
;;; up so that learning programs (composing, iterative macro-ops) can inspect
;;; Fermi's actions.  Every time a puller applies, for every variable, it
;;; asserts its value into working memory.  If the value is known, then
;;; this rule makes an equation out of it.  Otherwise, the next rule makes
;;; a subgoal to find that value.
#|  This is obsolete and replaced by make-equation-from-known-result.
    From now on, names of variables in formulas are always bound to
    frames.  If the result is known, then make-equation-from-known-result
    will fire,  stating that the value of that variable is the value of the
    result slot.  If it isn't known, then make-subgoal-and-template-wme
    will fire.
(RULE make-equation-from-known-value
  :LHS
   ((newvar :name =name :value =value :elements =elements
	    (CHECK =value) (CHECK (not (wmep =value)))
	    (LABEL =newvar))
    (<ABS> (equation :quantity =name)))
  :RHS
   (($make 'equation :name (gentemp "EQN") :quantity =name
	   :formula =value :elements =elements)
    (inc-fermi-formulas =name)
    ($remove =newvar)
    (algebra-readyp)))|#

(RULE make-var-if-not-already-made
  :LHS
   ((newvar :name =name :value =value :elements =elements
	    (LABEL =newvar))
    (<ABS> (var :name =name)))
  :RHS
   (($make 'var :name =name)
    (inc-fermi-elements =name)))

(RULE cleanup-newvar
  :LHS
   ((newvar (LABEL =newvar)))
  :RHS
   (($remove =newvar)))

(RULE make-subgoal-from-unknown-value-elaborate
  :LHS
   ((newvar :name =name :value =value :class =class (CHECK (null =value))
	    (LABEL =newvar))
    (elaboration :class =class :value =evalue :slot =slot)
    (current-goal :subgoals =subgoals (LABEL =goal))
    (<ABS> (equation :quantity =name)))
  :RHS
   ((make-subgoal *CURRENT-GOAL* =name 'result)
    ($modify =goal :subgoals (cons
			      (get (make-subgoal *CURRENT-GOAL* =name 'result)
				   'cr-goal) =subgoals))
    (setf (car (cr-goal-wmes *CURRENT-GOAL*)) (ith-wme 1 'current-goal))
    ($remove =newvar)
    ($make-named =class =name =slot =evalue)))

#|
(RULE make-subgoal-from-unknown-value-no-elab
  :LHS
   ((newvar :name =name :value =value :class =class (CHECK (null =value))
	    (LABEL =newvar))
    (current-goal :subgoals =subgoals (LABEL =goal))
    (<ABS> (elaboration :class =class))
    (<ABS> (equation :quantity =name)))
  :RHS
   ((make-subgoal *CURRENT-GOAL* =name 'result)
    ($modify =goal :subgoals (cons
			      (get (make-subgoal *CURRENT-GOAL* =name 'result)
				   'cr-goal) =subgoals))
    (setf (car (cr-goal-wmes *CURRENT-GOAL*)) (ith-wme 1 'current-goal))
    ($remove =newvar)
    ($make-named =class =name =slot =evalue)))
|#

(RULE make-subgoal-and-template-wme
  :LHS
   ((newvar :name =name :value =value :class =class (CHECK (wmep =value))
	    (LABEL =newvar) (CHECK (not (get-slot =value :result))))
    (current-goal :subgoals =subgoals (LABEL =goal))
;;    (<ABS> (equation :quantity =name))
    )
  :RHS
   ((make-subgoal *CURRENT-GOAL* =name 'result)
    ($modify =goal :subgoals (cons
			      (get (make-subgoal *CURRENT-GOAL* =name 'result)
				   'cr-goal) =subgoals))
    (if (cr-goal-wmes *CURRENT-GOAL*)
	(setf (car (cr-goal-wmes *CURRENT-GOAL*)) (ith-wme 1 'current-goal)))
    ($remove =newvar)))

#|
(RULE pop-failure-when-goal-recurs
  :LHS
   ((newvar :name =name :value =value :class =class (CHECK (wmep =value))
	    (LABEL =newvar) (CHECK (not (get-slot =value :result)))
	    (CHECK (current-super-goal-p =name)))
    (current-goal :subgoals =subgoals (LABEL =goal)))
  :RHS
   ((format T "Popping goal ~S with failure since it recurs~%" =name)
    ($remove =newvar)
    (pop-failure)))
|#

;;; One way that the rules could detect circular goals is if there is
;;; a newvar without a known value but an equation for that var, then this
;;; must be a recurring goal.

;;; From now on, names of variables in formulas are always bound to
;;; frames.  If the result is known, then make-equation-from-known-result
;;; will fire,  stating that the value of that variable is the value of the
;;; result slot.  If it isn't known, then make-subgoal-and-template-wme
;;; will fire.
(RULE make-equation-from-known-result
  :LHS
   ((newvar :name =name :value =value :class =class :elements =elements
	    (CHECK (wmep =value)) (CHECK (get-slot =value :result))
	    (LABEL =newvar))
    (current-goal :subgoals =subgoals (LABEL =goal))
    (<ABS> (equation :quantity =name)))
  :RHS
   (($make 'equation :name (gentemp "EQN") :quantity =name
	   :formula (get-slot =value :result) :elements =elements)
    (inc-fermi-formulas =name)
    ($remove =newvar)
    (algebra-readyp)))

;;   ((make-subgoal *CURRENT-GOAL* =name 'result)
;;    ($modify =goal :subgoals (cons
;;			      (get (make-subgoal *CURRENT-GOAL* =name 'result)
;;				   'cr-goal) =subgoals))
;;    (if (cr-goal-wmes *CURRENT-GOAL*)
;;	(setf (car (cr-goal-wmes *CURRENT-GOAL*)) (ith-wme 1 'current-goal)))
;;    ($remove =newvar))

(defun some-goal-new (subgoals)
  (some #'(lambda (goal)
	    (eq (cr-goal-status goal) 'new))
	  subgoals))

(defun find-new-goal (subgoals)
  (cr-goal-name (some #'(lambda (goal)
			  (if (eq (cr-goal-status goal) 'new)) goal)
		      subgoals)))


;;; Added 29-May-89: If the current goal has unfulfilled subgoals, then choose
;;; one of the subgoals as the new current goal.
(RULE choose-subgoal
  :LHS
   ((current-goal :subgoals =subs (LABEL =goal)
      		  	    	  (CHECK (some-goal-new =subs))))
  :RHS
   (($modify =goal :name (choose-subgoal-name (find-new-goal =subs) 'result)
		   :subgoals (cr-goal-subgoals *CURRENT-GOAL*)
		   :id (cr-goal-id *CURRENT-GOAL*)
		   :status (cr-goal-status *CURRENT-GOAL*)
		   :want (cr-goal-want *CURRENT-GOAL*)
		   )
    (setf (cr-goal-wmes *CURRENT-GOAL*)
	  (list (ith-wme 1 'current-goal)))))

#|(RULE pop-failure
 :LHS
  ((current-goal :name =name (LABEL =goal)
		 (CHECK (current-super-goal-p =name))))
 :RHS
  ((format T "Popping goal ~S with failure since it recurs~%" =name)
;;   ($remove =goal)
   (pop-failure)))
|#
;;; If the current goal wants to find some value, and that value has been
;;; found (in the form of an equation), then pop with success.
(RULE pop-success
  :LHS
   ((current-goal :name =name :subgoals =subs (LABEL =goal)
		  (CHECK (not (some-goal-new =subs)))
		  (CHECK (not (current-super-goal-p =name))))
    (equation :quantity =name :formula =formula (CHECK =formula)))
  :RHS
  (($modify =goal :name (pop-success-name)
	    :subgoals (cr-goal-subgoals *CURRENT-GOAL*)
	    :id (cr-goal-id *CURRENT-GOAL*)
	    :status (cr-goal-status *CURRENT-GOAL*)
	    :want (cr-goal-want *CURRENT-GOAL*)
)))

;;; If we haven't yet found a quantity for the current goal, and the
;;; current goal has no subgoals, then pop with failure.
;;; Old version
#|
(RULE pop-failure
  :LHS
   ((current-goal :name =name :subgoals NIL)
    (equation :quantity =name :formula NIL))
  :RHS
  ((pop-failure)))
|#
    

;;; If the goal is to solve for unknown, and there are at least 2 equations,
;;; set up subgoal to reduce the number of equations and unknowns
(RULE start-solving-for-unknown
    :LHS ((var :desired-unknown :yes) 
	  (<ABS> (count))
	  (current-goal :name solve-for-unknown (LABEL =goal)))
    :RHS (($make 'count :eqns *NUM-FORMULAS* :vars *NUM-ELEMENTS*)
	  ($remove =goal)
	  (spawn-subgoals '(reduce-eqns&unkns solve-for-unknown))))

(RULE solve-for-unknown-n
    :LHS ((var :desired-unknown :yes) 
	  (count :eqns =numeqns (CHECK (> =numeqns 1)))
	  (current-goal :name solve-for-unknown (LABEL =goal)))
    :RHS (($remove =goal)
	  (spawn-subgoals '(reduce-eqns&unkns solve-for-unknown))))

;;; Used to be called dec-solve
(RULE reduce-eqns&unkns
    :LHS ((var :desired-unknown :yes)
	  (count :eqns =numeqns (CHECK (> =numeqns 1)))
	  (current-goal :name reduce-eqns&unkns (LABEL =goal)))
    :RHS (($remove =goal)
	  (spawn-subgoals '(select-var  var-on-lhs  new-set)))) ;implicit AND

;;; If variable is not equal to unknown, and appears in at least two equations,
;;; mark variable as selected
;;; Used to be called select-var+
(RULE select-var
    :LHS ((var :name =u :desired-unknown :yes)
	  (equation :name =e1 :elements =es1)
	  (var (LABEL =v) :name =v1
	       (CHECK (not (eq =v1 =u)))
	       (CHECK (memq =v1 =es1)))
	  (equation :name =e2 :elements =es2
		    (CHECK (not (equal =e1 =e2)))
		    (CHECK (memq =v1 =es2)))
	  (current-goal :name select-var (LABEL =goal)))
    :RHS (($modify =v :selected :yes)
	  ($modify =goal :name (pop-success-name)
		   :subgoals (cr-goal-subgoals *CURRENT-GOAL*)
		   :id (cr-goal-id *CURRENT-GOAL*)
		   :status (cr-goal-status *CURRENT-GOAL*)
		   :want (cr-goal-want *CURRENT-GOAL*)
)))


;;; If there does not exist any variable that appears in at least 2 eqns
;;; then pop failure.
;;; This rule is not right.  Maybe it works for the trivial case of
;;; 2 eqns but not in the general case.  Maybe we really need absence
;;; tests of conjunctions.
#|
(RULE select-var-  		
    :LHS ((current-goal :name select-var) 
	  (var :name =u :desired-unknown :yes)
	  (equation (LABEL =e) :name =e1 :elements =es1)
	  (equation :name =e2 :elements =es2   ; var is not in
		     (CHECK (not (equal =e1 =e2)))) ; any other eqn 
	  (<ABS> (var (LABEL =v) :name =v1
		      (CHECK (not (equal =v1 =u))) ; var is not d-unknown
		      (CHECK (member =v1 =es1))
		      (CHECK (member =v1 =es2)))))

    :RHS ((pop-failure)))
|#


;;; If the selected variable is on the LHS of an equation,
;;; then select equation.
;;; Used to be called var-on-lhs+
(RULE select-eqn
  :LHS ((var :name =v1 :selected :yes)
	(equation (LABEL =e) :name =e1 :quantity =v1)
	(current-goal :name var-on-lhs (LABEL =goal)))
  :RHS (($modify =e :selected :yes)
	($modify =goal :name (pop-success-name)
		 :subgoals (cr-goal-subgoals *CURRENT-GOAL*)
		 :id (cr-goal-id *CURRENT-GOAL*)
		 :status (cr-goal-status *CURRENT-GOAL*)
		 :want (cr-goal-want *CURRENT-GOAL*)
)))

;;; If there is no equation in which the selected var is on the LHS
;;; but there is an equation that contains the selected var,
;;; rearrange equation so that var only is on the lhs.
;;; Used to be called var-on-lhs-
(RULE rearrange-eqn
  :LHS ((var :name =v1 :selected :yes)
	(<ABS> (equation :name =e1 :quantity =v1))
	(equation (LABEL =e) :name =e2 :quantity =q :formula =f :elements =es2
		  (CHECK (member =v1 =es2)))
	(current-goal :name var-on-lhs))
  :RHS (($modify =e
	  :quantity =v1
	  :formula (rearrange =f =q =es2 =v1))))

                     
;;; If the selected var is in an eqn (=e) other than the selected eqn,
;;; substitute occurrences of the selected var in the eqn (=e) with the 
;;; RHS of the selected equation.
;;; Used to be called replace+
(RULE substitute
      :LHS ((equation :name =e1 :selected :yes :elements =es1
		      :quantity =v1  :formula =f1); new equation has been returned
	    (var :name =v1 :selected :yes)
	    (equation :name =e2 :formula =rhs (LABEL =e)
		      :quantity =lhs :elements =es2
		      (CHECK (not (equal =e1 =e2)))
		      (CHECK (memq =v1 =es2)))    ;; var is in equation =e2
	    (current-goal :name new-set))
      :RHS (($modify =e
		     :quantity (subst =f1 ; new
				      =v1 ; old
				      =lhs) ; tree
		     :formula (subst  =f1  =v1 =rhs)
		     :elements (remove =v1 (union =es1 =es2) :test 'equal))))

;;; If all occurrences of =v1 have been replaced, remove =v1 and =e1;  new set
;;; has been created
;;; Used to be called replace-
(RULE eliminate
    :LHS ((count :eqns =nume :vars =numv (LABEL =c))
	  (equation :name =e1 :selected :yes :quantity =v1 (LABEL =e))
	  (var :name =v1 :selected :yes (LABEL =v))
	  (<ABS> (equation :name =e2  :elements =es2
		     (CHECK (not (equal =e1 =e2)))
		     (CHECK (member =v1 =es2))))
	  (current-goal :name new-set (LABEL =goal)))
    
    :RHS (($remove =v)
	  ($remove =e)
	  ($modify =c :eqns (- =nume 1) :vars (- =numv 1))
	  ($modify =goal :name (pop-success-name)
		   :subgoals (cr-goal-subgoals *CURRENT-GOAL*)
		   :id (cr-goal-id *CURRENT-GOAL*)
		   :status (cr-goal-status *CURRENT-GOAL*)
		   :want (cr-goal-want *CURRENT-GOAL*)
)))

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
	  (current-goal :name solve-for-unknown (LABEL =goal)))

    :RHS ((setq algres (rearrange =f =q =el =v1))
	  ($modify =e
		   :selected :yes
		   :quantity =v1
		   :formula algres)
	  ($modify =u :result algres)
	  (format T "The answer is ~S ~%" algres)
	  ($modify =goal :name (pop-success-name)
		   :subgoals (cr-goal-subgoals *CURRENT-GOAL*)
		   :id (cr-goal-id *CURRENT-GOAL*)
		   :status (cr-goal-status *CURRENT-GOAL*)
		   :want (cr-goal-want *CURRENT-GOAL*)
)
	  algres))

;(RULE unknown-solved
;    :LHS ((current-goal :name solve-for-unknown)
;	  (equation :name =e1 :selected :yes :formula =rhs 
;	      :quantity =v1 :elements =es 
;	      (CHECK (not (cdr =es))))
;	  (var :name =v1 (LABEL =u) :desired-unknown :yes :result nil))

;    :RHS (($modify =u result =rhs) ))	


; if desired unknown has a non-nil result, then pop success
;(RULE check-solve-for-unknown
;    :LHS ((current-goal :name solve-for-unknown)
;	  (var :desired-unknown :yes :result =r (CHECK =r)))
;    :RHS ((format t "The answer is ~A!" =r)	  
;	  (pop-success)))

;;; Perhaps this should be part of the goal monitor.
(RULE done
  :LHS ((current-goal :name top :status successful))
  :RHS ((format T "Done ~%")
	(halt)))


;;; Should return 2 (= x).
(defun test-alg (&optional (cycles 999))
;;  (clear-net)
  (set-wm (var :name :z)
	  (var :name :y)
	  (var :name :x :desired-unknown :yes)
	  (count :eqns 3 :vars 3)
	  (equation :name :eqn-2 :quantity :z :formula '(- 10 (* 3  :x))
		    :elements '( :z  :x))
	  (equation :name :eqn-3 :quantity :z :formula '(\/ (* 4 :y) 3)
		    :elements '( :y  :z))	  
	  (equation :name :eqn-1 :quantity 10 :formula '(+ (*  :x 2) (* :y 2))
		    :elements '(:x :y)))
  (init-goalmon)
  (spawn-subgoals '(solve-for-unknown))
  (run cycles))


;;; New interface stuff for RFermi.

(defvar *NUM-FORMULAS* 0)
(defvar *NUM-ELEMENTS* 0)
(defvar *ELEMENTS* nil)

(defun init-algebra ()
  (setq *NUM-FORMULAS* 0)
  (setq *NUM-ELEMENTS* 0)
  (setq *ELEMENTS* nil)
  (format T "Algebra initialized~%"))

;;; If #formulas >= #variables, start solving and return either the
;;; result or :need-more, else return :need-more.
;;; This may need something more, to specify where to put the answer
;;; and/or where to get it.
;;; For now the algebra just returns the answer as a top-level result, and puts
;;; the answer the desired variable's result slot.
(defun algebra-readyp ()
  (cond ((>= *NUM-FORMULAS* *NUM-ELEMENTS*)
	 (format T "The algebra will now try to solve the system of equations~%")
	 (spawn-subgoals '(solve-for-unknown))	     ;;Top goal of algebra system
	 ($make 'count :eqns *NUM-FORMULAS* :vars *NUM-ELEMENTS*))
	(T :need-more)))

(defun add-desired-unknown (varname)
  (when (not (memq varname *ELEMENTS*))
    ($make 'var :name varname :desired-unknown :yes)
    (push varname *ELEMENTS*)
    (incf *NUM-ELEMENTS*)))

(defun add-element-to-algebra (varname)
  (when (not (memq varname *ELEMENTS*))
    ($make 'var :name varname)
    (inc-fermi-elements varname)))

(defun inc-fermi-elements (varname)
  (push varname *ELEMENTS*)
  (incf *NUM-ELEMENTS*))

;;; varname should already be in the elements list.
(defun add-equation-to-algebra (varname vvalue elements)
  ($make 'equation :name (gentemp "EQN") :quantity varname :formula vvalue
	 :elements elements)
  (if (fermi-valuep vvalue)
      (incf *NUM-FORMULAS*)))

(defun inc-fermi-formulas (vvalue)
  (if (fermi-valuep vvalue)
      (incf *NUM-FORMULAS*)))

;	  (equation :name :test :quantity 8
;		    :formula '(+ (* 2 (\/ (- 10 (* :y 2)) 2)) :y)
;		    :elements '(:y))))

;works when given as single eqn
;	   (equation :name :eqn-zero :quantity 6 :formula '(* 3 (- 7 :x))
;	      :elements '(:x))))

;for testing with 2 eqns.
;	  (equation :name :eqn-4 :quantity 8 :formula '(+ (* 2 :x) :y)
;		    :elements '(:x :y))
;	  (equation :name :eqn-1 :quantity 10 :formula '(+ (*  :x 2) (* :y 2))
;		    :elements '( :x  :y))))

;for testing with 3 eqns together with eqn-1
;(equation :name :eqn-2 :quantity :z :formula '(- 10 (* 3  :x))
;	      :elements '( :z  :x))
; (equation :name :eqn-3 :quantity :y :formula '(\/ 12 :z)
;      :elements '( :y  :z))	  



;(setq eqn-t ($make  'equation :name :test :quantity 8
;		    :formula '(+ (* 2 (\/ (- 10 (* :y 2)) 2)) :y)
;		    :elements '(:y)))


;;; Tests it on a Fermi example of 6 equations from pressure-drop pullers.
;;; Should return 19.6.
(defun test-alg2 (&optional (cycles 999))
  (clear-net)
  (set-wm
   (var :name 'pressure-drop :desired-unknown :yes)
   (var :name 'dens)
   (var :name 'accel)
   (var :name 'height)
   (var :name 'initial)
   (var :name 'final)
   (count :eqns 6 :vars 6)
   (equation :name :EQN4386 :FORMULA '(* DENS ACCEL HEIGHT) :QUANTITY 'PRESSURE-DROP :ELEMENTS '(PRESSURE-DROP HEIGHT ACCEL DENS))
   (equation :name :EQN4398 :FORMULA 1.0 :QUANTITY 'DENS :ELEMENTS '(DENS))
   (equation :name :eqn4400 :formula 9.8 :quantity 'accel :elements '(accel))
   (equation :name :EQN4410 :FORMULA '(- INITIAL FINAL) :QUANTITY 'HEIGHT :ELEMENTS '(HEIGHT FINAL INITIAL))
   (equation :name :EQN4433 :FORMULA 3.0 :QUANTITY 'INITIAL :ELEMENTS '(INITIAL))
   (equation :name :eqn4443 :formula 1.0 :quantity 'final
	     :elements '(final))
)
  (init-goalmon)
  (spawn-subgoals '(solve-for-unknown))
  (run cycles))

;;; 16 eqns and 16 unknowns
(defun test-alg3 (&optional (cycles 999))
  (clear-net)
  (set-wm
   (var :name 'pressure-drop :desired-unknown :yes)
   (var :name 'dens)
   (var :name 'accel)
   (var :name 'height)
   (var :name 'initial)
   (var :name 'final)
   (var :name 'final1) (var :name 'final2) (var :name 'final3)
   (var :name 'final4) (var :name 'final5) (var :name 'final6)
   (var :name 'final7) (var :name 'final8) (var :name 'final9)
   (var :name 'final10)
   (count :eqns 16 :vars 16)
   (equation :name :EQN4386 :FORMULA '(* DENS ACCEL HEIGHT) :QUANTITY 'PRESSURE-DROP :ELEMENTS '(PRESSURE-DROP HEIGHT ACCEL DENS))
   (equation :name :EQN4398 :FORMULA 1.0 :QUANTITY 'DENS :ELEMENTS '(DENS))
   (equation :name :eqn4400 :formula 9.8 :quantity 'accel :elements '(accel))
   (equation :name :EQN4410 :FORMULA '(- INITIAL FINAL) :QUANTITY 'HEIGHT :ELEMENTS '(HEIGHT FINAL INITIAL))
   (equation :name :EQN4433 :FORMULA 3.0 :QUANTITY 'INITIAL :ELEMENTS '(INITIAL))
   (equation :name :eqn4443 :formula 'final1 :quantity 'final
	     :elements '(final final1))
   (equation :name :eqn4444 :formula 'final2 :quantity 'final1
	     :elements '(final1 final2))
   (equation :name :eqn4445 :formula 'final3 :quantity 'final2
	     :elements '(final2 final3))
   (equation :name :eqn4446 :formula 'final4 :quantity 'final3
	     :elements '(final3 final4))
   (equation :name :eqn4447 :formula 'final5 :quantity 'final4
	     :elements '(final4 final5))
   (equation :name :eqn4448 :formula 'final6 :quantity 'final5
	     :elements '(final5 final6))
   (equation :name :eqn4449 :formula 'final7 :quantity 'final6
	     :elements '(final6 final7))
   (equation :name :eqn4450 :formula 'final8 :quantity 'final7
	     :elements '(final7 final8))
   (equation :name :eqn4451 :formula 'final9 :quantity 'final8
	     :elements '(final8 final9))
   (equation :name :eqn4452 :formula 'final10 :quantity 'final9
	     :elements '(final9 final10))
   (equation :name :eqn4453 :formula 1.0 :quantity 'final10
	     :elements '(final10))
)
  (init-goalmon)
  (spawn-subgoals '(solve-for-unknown))
  (run cycles))

;;; 32 equations
(defun test-alg4 (&optional (cycles 999))
  (clear-net)
  (set-wm
   (var :name 'pressure-drop :desired-unknown :yes)
   (var :name 'dens)
   (var :name 'accel)
   (var :name 'height)
   (var :name 'initial)
   (var :name 'final)
   (var :name 'final1) (var :name 'final2) (var :name 'final3)
   (var :name 'final4) (var :name 'final5) (var :name 'final6)
   (var :name 'final7) (var :name 'final8) (var :name 'final9)
   (var :name 'final10)
   (var :name 'final11) (var :name 'final12) (var :name 'final13)
   (var :name 'final14) (var :name 'final15) (var :name 'final16)
   (var :name 'final17) (var :name 'final18) (var :name 'final19)
   (var :name 'final20)
   (var :name 'final21) (var :name 'final22) (var :name 'final23)
   (var :name 'final24) (var :name 'final25) (var :name 'final26)
   (count :eqns 32 :vars 32)
   (equation :name :EQN4386 :FORMULA '(* DENS ACCEL HEIGHT) :QUANTITY 'PRESSURE-DROP :ELEMENTS '(PRESSURE-DROP HEIGHT ACCEL DENS))
   (equation :name :EQN4398 :FORMULA 1.0 :QUANTITY 'DENS :ELEMENTS '(DENS))
   (equation :name :eqn4400 :formula 9.8 :quantity 'accel :elements '(accel))
   (equation :name :EQN4410 :FORMULA '(- INITIAL FINAL) :QUANTITY 'HEIGHT :ELEMENTS '(HEIGHT FINAL INITIAL))
   (equation :name :EQN4433 :FORMULA 3.0 :QUANTITY 'INITIAL :ELEMENTS '(INITIAL))
   (equation :name :eqn4443 :formula 'final1 :quantity 'final
	     :elements '(final final1))
   (equation :name :eqn4444 :formula 'final2 :quantity 'final1
	     :elements '(final1 final2))
   (equation :name :eqn4445 :formula 'final3 :quantity 'final2
	     :elements '(final2 final3))
   (equation :name :eqn4446 :formula 'final4 :quantity 'final3
	     :elements '(final3 final4))
   (equation :name :eqn4447 :formula 'final5 :quantity 'final4
	     :elements '(final4 final5))
   (equation :name :eqn4448 :formula 'final6 :quantity 'final5
	     :elements '(final5 final6))
   (equation :name :eqn4449 :formula 'final7 :quantity 'final6
	     :elements '(final6 final7))
   (equation :name :eqn4450 :formula 'final8 :quantity 'final7
	     :elements '(final7 final8))
   (equation :name :eqn4451 :formula 'final9 :quantity 'final8
	     :elements '(final8 final9))
   (equation :name :eqn4452 :formula 'final10 :quantity 'final9
	     :elements '(final9 final10))
   (equation :name :eqn4443 :formula 'final11 :quantity 'final10
	     :elements '(final10 final11))
   (equation :name :eqn4444 :formula 'final12 :quantity 'final11
	     :elements '(final11 final12))
   (equation :name :eqn4445 :formula 'final13 :quantity 'final12
	     :elements '(final12 final13))
   (equation :name :eqn4446 :formula 'final14 :quantity 'final13
	     :elements '(final13 final14))
   (equation :name :eqn4447 :formula 'final15 :quantity 'final14
	     :elements '(final14 final15))
   (equation :name :eqn4448 :formula 'final16 :quantity 'final15
	     :elements '(final15 final16))
   (equation :name :eqn4449 :formula 'final17 :quantity 'final16
	     :elements '(final16 final17))
   (equation :name :eqn4450 :formula 'final18 :quantity 'final17
	     :elements '(final17 final18))
   (equation :name :eqn4451 :formula 'final19 :quantity 'final18
	     :elements '(final18 final19))
   (equation :name :eqn4452 :formula 'final20 :quantity 'final19
	     :elements '(final19 final20))
   (equation :name :eqn4443 :formula 'final21 :quantity 'final20
	     :elements '(final20 final21))
   (equation :name :eqn4444 :formula 'final22 :quantity 'final21
	     :elements '(final21 final22))
   (equation :name :eqn4445 :formula 'final23 :quantity 'final22
	     :elements '(final22 final23))
   (equation :name :eqn4446 :formula 'final24 :quantity 'final23
	     :elements '(final23 final24))
   (equation :name :eqn4447 :formula 'final25 :quantity 'final24
	     :elements '(final24 final25))
   (equation :name :eqn4448 :formula 'final26 :quantity 'final25
	     :elements '(final25 final26))
   (equation :name :eqn4453 :formula 1.0 :quantity 'final26
	     :elements '(final26))
)
  (init-goalmon)
  (spawn-subgoals '(solve-for-unknown))
  (run cycles))
