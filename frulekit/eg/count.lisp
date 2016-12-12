;;; Agenda example.
;; Converted to Rulekit productions Jan. 23 1986

(setq *PAUSE-EVERY-CYCLE* t)

(literalize anumber ()
  value NIL)

(rule rule1
  :LHS ((anumber :value =n))
  :RHS ((format t "Number: ~D ~%" =n))
  :BUCKET-ADD ((bucket-2 *last)))

(rule rule1a
  :LHS ((anumber :value =n))
  :RHS ((format t "Number: ~D ~%" =n)))

(rule rule2
  :LHS ((anumber :value =n (CHECK (> =n 10)) (LABEL =c1)))
  :RHS (($modify =c1 :value (+ =n 2))
        (format t "Big Number: ~D ~%" =n))
  :BUCKET-DEL (bucket-1))

(rule rule3
  :LHS ((anumber :value =n (CHECK (> =n 20))))
  :RHS (:halt))


(setq !trace-test t)
(setq !trace-act t)
(setq !trace-agenda t)
(setq !!control :linear)

(setq !rules '(rule1 rule2 rule3))
;;my-agenda will cause it to count from 12 to 20.
(setq my-agenda
      '((bucket-1 rule1 rule1a)
	(bucket-2 rule2)
	(bucket-3 rule3)))

(set-wm (anumber :value 12))
