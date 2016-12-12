(literalize seen-object nil name "default")
(literalize touching-object nil obj1 nil obj2 nil)

(rule explore
    :lhs (
	  (seen-object :name "o")
	  (<ABS> (touching-object))
	 )
    :rhs (
	  (move-forward-2D 100.0 1)
	 )
)
(rule back-away-from-wall
    :lhs (
	  (touching-object)
	 )
    :rhs (
	  (move-backward-2D 20.0 1)
	  (turn-right 1 10.0)
	 )
)
