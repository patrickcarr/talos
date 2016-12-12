;;; This is an example of using the FRulekit BIND command on the LHS.

(literalize table ()
  weight NIL
  max NIL)

(literalize chair ()
  weight NIL
  of-table NIL)

(RULE compute-weight
  :LHS ((chair :weight =lw :of-table
	       (table :weight =w (BIND =total (+ =lw =w))
		      (CHECK (> =total 10)) :max =total))
	(chair :weight =total (BIND =total2 (* =total 2))))
  :RHS ((format t "Table wt. = ~A, chair wt. = ~A, total = ~A, total2 = ~A~%"
		=w =lw =total =total2)))

(defun begin ()
  (start
   (chair :weight 5 :of-table ($make 'table :weight 7 :max 12))
   (chair :weight 12)))


(literalize wme1 ()
  time T)


(RULE test2 
  :LHS ((wme1 (LABEL =wme1) :time =time
	      (BIND =next (+ =time 1)))
	(wme1 (LABEL =wme2) :time =next))
  :RHS (($modify =wme1 :time =next)))


(RULE test3 
  :LHS ((wme1 (LABEL =wme1) :time =time
	      (BIND =next (+ =time 1)))
	(wme1 (LABEL =wme2) :time =foo
	      (CHECK (= =next (+ =foo 1)))
	      (CHECK (<> =wme1 =wme2))))
  :RHS (($modify =wme1 :time =next)))

(defun begin2 ()
  (setq *PAUSE-EVERY-CYCLE* T)
  (start (wme1 :time 10) (wme1 :time 10) (wme1 :time 11)))

;;; Causes test2 and test3 to fire alternately.
