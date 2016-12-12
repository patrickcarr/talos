;;; Test case for FRulekit shared memory.

(literalize c1 ()
  s1 ()
  s2 ())

(RULE r1
  :LHS ((c1 :s1 1)
	(c1 :s1 2)
	(<abs> (c1 :s1 3)))
  :RHS ((format T "r1")))

(RULE r2
  :LHS ((c1 :s1 1)
	(c1 :s1 2)
	(<abs> (c1 :s1 4)))
  :RHS ((format T "r2")))


(defun begin ()
  ($make 'c1 :s1 4)
  ($make 'c1 :s1 1)
  ($make 'c1 :s1 2))
;;  ($make 'c1 :s1 3)
;;; This should cause the match count of the token in the 4 node
;;; to go to 0.  Thus adding then removing a 3 wme should cause r2
;;; to mis-fire.

;;; This will cause r1 to be added to the conflict set even though there
;;; is an inhibiting 3 wme if sharing isn't done correctly.
(defun testit ()
  (clear-net)
  ($make 'c1 :s1 3)
  ($make 'c1 :s1 4)
  ($make 'c1 :s1 2)
  ($make 'c1 :s1 1)
  ($remove (ith-wme 3))	     ;;the match count will be screwed up here.
  ($remove ($make 'c1 :s1 3)))
