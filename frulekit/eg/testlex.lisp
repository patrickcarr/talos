;;; Test of the LEX c.r. strategy

(literalize c1 ()
  a nil)

(literalize c2 ()
  b nil)

(literalize c3 ()
  c nil)

(RULE r1
 :LHS ((c1 :a 1)
       (c2 :b 2))
 :RHS ((format T "r1")))

(RULE r2
 :LHS ((c1 :a 1)
       (c2 :b 2)
       (c3 :c 3))
 :RHS ((format T "r2")))

(defun begin ()
  (start
   (c3 :c 3)
   (c1 :a 1)
   (c2 :b 2)))

(setq *CR-STRATEGY* *LEX*)
