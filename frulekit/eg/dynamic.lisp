;;; Example of dynamic slot specification in FRulekit.

(literalize context ()
  slots NIL
  cur-slot NIL)

(literalize data (propagate T)
  a (value nil)
  b NIL
  c NIL)

(RULE print-slot
  :LHS ((context :cur-slot =c :slots =s (CHECK =c) (LABEL =context))
	(data [=c] =val [a value] =a (CHECK (> =val 0))))
  :RHS ((format T "The ~A slot of data is: ~A~%" =c =val)
	(format T "The value of the A slot is: ~A~%" =a)
	($modify =context :cur-slot (cadr =s) :slots (cdr =s))))


;;; Causes FRulekit to print the value of each of the slots :a, :b, and :c.
(defun begin ()
  (start
   (context :slots '(a b c) :cur-slot 'a)
   (data :a '(value 1) :b 2 :c 3)))
