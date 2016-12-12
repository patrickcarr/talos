;;; Tests to make sure that rules with two shared alpha tests are handled
;;; correctly.

(use-package 'frk)

(literalize type1 ()
  :value nil)

(literalize type2 ()
  :value nil)

(literalize type3 ()
  :value nil)

(RULE match1
 :LHS
  ((type1 :value =v)
   (type2 :value =w)
   (type2 :value =w)
   (type3 :value =x)
   (type3 :value 3))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match2
 :LHS
  ((type1 :value =v)
   (type2 :value =w)
   (type3 :value =x)
   (type2 :value =w)
   (type3 :value 3))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match3
 :LHS
  ((type1 :value =v)
   (type2 :value =w)
   (type3 :value =x)
   (type3 :value 3)
   (type2 :value =w))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match4
 :LHS
  ((type1 :value =v)
   (type3 :value =x)
   (type2 :value =w)
   (type3 :value 3)
   (type2 :value =w))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match5
 :LHS
  ((type1 :value =v)
   (type3 :value =x)
   (type2 :value =w)
   (type2 :value =w)
   (type3 :value 3))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))


(RULE match6
 :LHS
  ((type1 :value =v)
   (type3 :value =x)
   (type3 :value 3)
   (type2 :value =w)
   (type2 :value =w))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

;;; 1 in 2nd pos.

(RULE match1-2
 :LHS
  ((type2 :value =w)
   (type1 :value =v)
   (type2 :value =w)
   (type3 :value =x)
   (type3 :value 3))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match2-2
 :LHS
  ((type2 :value =w)
   (type1 :value =v)
   (type3 :value =x)
   (type2 :value =w)
   (type3 :value 3))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match3-2
 :LHS
  ((type2 :value =w)
   (type1 :value =v)
   (type3 :value =x)
   (type3 :value 3)
   (type2 :value =w))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match4-2
 :LHS
  ((type3 :value =x)
   (type1 :value =v)
   (type2 :value =w)
   (type3 :value 3)
   (type2 :value =w))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match5-2
 :LHS
  ((type3 :value =x)
   (type1 :value =v)
   (type2 :value =w)
   (type2 :value =w)
   (type3 :value 3))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))


(RULE match6-2
 :LHS
  ((type3 :value =x)
   (type1 :value =v)
   (type3 :value 3)
   (type2 :value =w)
   (type2 :value =w))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

;;; 1 in 3rd pos:

(RULE match1-3
 :LHS
  ((type2 :value =w)
   (type2 :value =w)
   (type1 :value =v)
   (type3 :value =x)
   (type3 :value 3))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match2-3
 :LHS
  ((type2 :value =w)
   (type3 :value =x)
   (type1 :value =v)
   (type2 :value =w)
   (type3 :value 3))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match3-3
 :LHS
  ((type2 :value =w)
   (type3 :value =x)
   (type1 :value =v)
   (type3 :value 3)
   (type2 :value =w))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match4-3
 :LHS
  ((type3 :value =x)
   (type2 :value =w)
   (type1 :value =v)
   (type3 :value 3)
   (type2 :value =w))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))

(RULE match5-3
 :LHS
  ((type3 :value =x)
   (type2 :value =w)
   (type1 :value =v)
   (type2 :value =w)
   (type3 :value 3))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))


(RULE match6-3
 :LHS
  ((type3 :value =x)
   (type3 :value 3)
   (type1 :value =v)
   (type2 :value =w)
   (type2 :value =w))
  :RHS
  ((format T "=v, =w, =x are: ~A, ~A, ~A~%" =v =w =x)))



(defun begin ()
  (set-wm
   (type1 :value 1)
   (type3 :value 3)))
