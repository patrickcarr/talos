;;; FRulekit example of nesting with CHECKS after the nested
;;; wme.

(literalize wme1 ()
  a NIL
  child NIL)

(RULE find-child
  :LHS ((wme1 (LABEL =wme1) :a =a1 :child (wme1 :a =a2 (LABEL =wme2))
 	      (CHECK (> =a1 =a2))))
  :RHS ((format T "Found parent: ") (pp-wme =wme1)
	(format T "With child ") (pp-wme =wme2)))

(defun begin ()
  ($make 'wme1 :a 2 :child ($make 'wme1 :a 1)))
