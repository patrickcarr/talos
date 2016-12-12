;;; Simple Monkey and Bananas in FRulekit
;;; This version demonstrates nested WMEs.  The state slot of the goal WME
;;; is a pointer to an actual state wme.  want-to-move-to-them creates this
;;; and move-to-them tests for this.

(literalize goal (extendable nil)
  state NIL)

;;Generic state of something.  Tells where something is, and other info.
(literalize stateof (extendable nil)
  what NIL
  where NIL
  other NIL)


(RULE want-to-move-to-them
  :LHS ((stateof :what bananas :where =w :other yummy (LABEL =state))
	(<ABS> (stateof :what me :where =w)))
  :RHS (($make 'goal :state =state)))

(RULE move-to-them
  :LHS (
	(goal (LABEL =goal) :state (stateof :what bananas :where =ban))
	(stateof :what me :where =w (LABEL =me) (CHECK (<> =w =ban))))
  :RHS (($modify =me :where =ban)))	;;just hyperspace to the bananas

(RULE eat-them
  :LHS (
        (goal (LABEL =goal)
	      :state (stateof :what bananas :where =w :other yummy))
	(<ABS> (stateof :what me :where =x (CHECK (<> =w =x)))))
  :RHS (($remove =stateof =goal)))	;;FRulekit binds =stateof to the nested wme

(defun begin ()
  (start
   (stateof :what 'me :where 'corner :other 'hungry)
   (stateof :what 'bananas :where 'ceiling :other 'yummy)))
