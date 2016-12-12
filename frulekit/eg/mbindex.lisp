;;; This is an example of using the FRulekit BIND command on the LHS.

(literalize table ()
  weight NIL
  max NIL)

(literalize chair ()
  weight NIL
  of-table NIL)

(RULE compute-weight
  :LHS ((chair :weight =lw :of-table
	       (table :weight =w (MBIND (=total =frac) (floor (+ =lw =w)))
		      (CHECK (> =total 10)) :max =total))
	(chair :weight =total (MBIND (=total2) (* =total 2))))
  :RHS ((format t "Table wt. = ~A, frac = ~A, total = ~A, total2 = ~A~%"
		=w =frac =total =total2)))

(defun begin ()
  (start
   (chair :weight 5 :of-table ($make 'table :weight 7.5 :max 12))
   (chair :weight 12)))


(literalize wme1 ()
  time T)

(literalize wme2 ()
  here T)


(RULE test2 
  :LHS ((wme2 :here T)
	(wme1 (LABEL =wme1) :time =time
	      (MBIND (=next =frac) (floor (+ =time 1.2))))
	(wme1 (LABEL =wme2) :time =next))
  :RHS (($modify =wme1 :time =next)
	(format T "next, frac are: ~S, ~S" =next =frac)))


(RULE test3 
  :LHS ((wme2 :here T)
	(wme1 (LABEL =wme1) :time =time
	      (MBIND (=next =frac) (floor (+ =time 1.1))))
	(wme1 (LABEL =wme2) :time =foo
	      (CHECK (= =next (+ =foo 1)))
	      (CHECK (<> =wme1 =wme2))))
  :RHS (($modify =wme1 :time =next)
	(format T "next, frac are: ~S, ~S" =next =frac)))


(defun begin2 ()
  (setq *PAUSE-EVERY-CYCLE* T)
  (start (wme2) (wme1 :time 10) (wme1 :time 10) (wme1 :time 11)))

;;; Causes test2 and test3 to fire alternately.
