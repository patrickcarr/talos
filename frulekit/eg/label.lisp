;;; Demonstrates the ability of FRulekit to do label test after the var has
;;; been bound.

(literalize person ()
  age T
  child T)

(literalize boy ()
  walks T)

(RULE have-baby
  :LHS ((person :age 10 (LABEL =p))
	(boy :walks t (LABEL =b)))
  :RHS (($modify =p :age 11 :child =b)))

(RULE check-baby
  :LHS ((person :child =c)
	(boy (LABEL =c)))
  :RHS ((format t "Found person with child ~%")))

(defun begin ()
  (start
   (person :age 10)
   (boy :walks t)))
