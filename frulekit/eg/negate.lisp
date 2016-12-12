(literalize t1 ()
  a nil)

(literalize t2 ()
  b nil)

(RULE r1
 :LHS ((t1 :a 1)
       (<abs> (t1 :a 2))
       (<abs> (t1 :a 3)))
 :rhs ((format T "Match r1 ~%")))

(RULE r2
 :LHS ((t2 :b 1)
       (<abs> (t2 :b 2)))
 :rhs ((format T "Match r2 ~%")))
