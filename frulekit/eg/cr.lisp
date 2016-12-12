;;; ------------------------------------------------------------------
;;; Conflict Set Hacks, used by Hans Tallis
;;; ------------------------------------------------------------------
;;; Give certain rules priority
;;; The order:
;;; 	critic
;;; 	prune beneath satisfied goals
;;; 	mark unsatisfied goals required for firing last action
;;;	trigger a satisfied spawn-action context  ??? nec. ???
;;; 	all the others
;;;	satisfied-goal-becomes-unsatisfied

;;------------------------------
(defvar criticize-stage '(keep-track-of-last-action-fired
			     critic-sat-usable
			     critic-sat-unusable-En
			     critic-sat-unusable-NotEn
			     critic-sat-unusable-Then
			     critic-sat-unusable-If
			     critic-unsat-undim
			     critic-unsat-dim
			     critic-triggered
			     remove-critic-mark
			     critic-mark-Then
			     critic-mark-If
			     critic-mark-En
			     critic-mark-NotEn
			     critic-action-mark-If-Then
			     critic-action-mark-En
			     critic-action-mark-NotEn
			))
(defun criticize-stagep (r1) (not (not (member r1 criticize-stage))))

;;; The effect of this filter is that, if there are any criticize-stage
;;; rules in the conflict set, then they will have precedence over the
;;; non-criticize-stage rules.
(def-cr-strategy my-criticize-priority
    (lambda (ins)
	    (criticize-stagep (rk-rule-pname (instant-prod ins))))
    id2)

;;------------------------------
(defvar prune-stage '(surprise-satisfy-goal
			change-newly-satisfied-to-satisfied
			change-unsatisfied-to-fresh
			create-prune-satisfaction-context
			mark-root-subgoals-context
			mark-pruned-spawn-action-context
			recurse-prune-to-child-subgoals
			mark-pruned-expand-subgoal-context
			remove-prune-satisfaction-context
			kill-them-all kill-orphan
			stuck-in-a-rut
			))
(defun prune-stagep (r1) (not (not (member r1 prune-stage))))
(def-cr-strategy my-prune-priority
    (lambda (ins)
	    (prune-stagep (rk-rule-pname (instant-prod ins))))
    id2)

;;------------------------------
(defvar unsatisfied-stage '(satisfied-firing-subgoal-becomes-unsatisfied
			))
(defun unsatisfied-stagep (r1) (not (not (member r1 unsatisfied-stage))))
(def-cr-strategy my-unsatisfied-priority
    (lambda (ins)
	    (unsatisfied-stagep (rk-rule-pname (instant-prod ins))))
    id2)

;;------------------------------
(defvar trigger-stage '(make-spawned-action-context-triggered
			))
(defun trigger-stagep (r1) (not (not (member r1 trigger-stage))))
(def-cr-strategy my-trigger-priority
    (lambda (ins)
	    (trigger-stagep (rk-rule-pname (instant-prod ins))))
    id2)

;;------------------------------
(defvar satisfied-goal-becomes-unsatisfied-stage '(satisfied-goal-becomes-unsatisfied
			))
(defun satisfied-goal-becomes-unsatisfied-stagep (r1) (not (not (member r1 satisfied-goal-becomes-unsatisfied-stage))))
(def-cr-strategy my-satisfied-goal-becomes-unsatisfied-priority
    (lambda (ins)
	    (satisfied-goal-becomes-unsatisfied-stagep (rk-rule-pname (instant-prod ins))))
    id2)

;;------------------------------
(defvar complacency-stage '(everybody-is-too-near
			))
(defun complacency-stagep (r1) (not (not (member r1 complacency-stage))))
(def-cr-strategy my-complacency-priority
    (lambda (ins)
	    (complacency-stagep (rk-rule-pname (instant-prod ins))))
    id2)

;;------------------------------
(defvar not-all-others-stage (append
				   criticize-stage
				   prune-stage
				   unsatisfied-stage
				   trigger-stage
				   satisfied-goal-becomes-unsatisfied-stage
				   complacency-stage
				   ))
(defun all-others-stagep (r1) (not (not (not (member r1 not-all-others-stage)))))
(def-cr-strategy my-all-others-priority
    (lambda (ins)
	    (all-others-stagep (rk-rule-pname (instant-prod ins))))
    id2)

;;; a is whether the currently contending instantiation passed the test.
;;; b is whether the current 'best' instantiation passed the current test.
;;; Return T iff a is true and b isn't.
(defun id2 (a b)
  (and a (not b)))

;;; randomly pick one of the actions if they're first on the list
;;; note:  this is not a def-cr-strategy; rather, it takes and returns a
;;; conflict set, instead of defining a filter
(defun my-action-instancep (instance)
    (eq 'create-spawn-an-action-context (rk-rule-pname (instant-prod instance))))
;(defmacro plus (&rest l) `(+ ,@l))
(defun randomly-pick-an-action (cs)
    (let (action-count)
	 (setq action-count (sloop for ins in cs
				   while (my-action-instancep ins)
				   sum 1))
	 (if (< 0 action-count)
	     `(,(nth (random action-count) cs))
	     cs)))

(setq *cr-strategy* `(my-criticize-priority
			 my-prune-priority
			 my-unsatisfied-priority
			 my-trigger-priority
			 my-all-others-priority
			 my-satisfied-goal-becomes-unsatisfied-priority
			 my-complacency-priority
			 ;,@*MEA*
			 narrow-down-mea
			 narrow-down-lex
			 randomly-pick-an-action ;my addition
			 num-tests
			 num-checks
			 ))

