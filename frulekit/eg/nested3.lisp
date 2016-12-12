(RULE r1
  :LHS ((time :value =now)
	(context :instantiation =instantiation
		 (BIND =num (length =instantiation))
		 :space (a-space :operator (an-operator :types =types
							:num-of-params =num-of-params)
				 :+examples =+examples)
 		 :cluster (a-cluster :s-constraints =s-constraints)
		 (CHECK (< =num =num-of-params))
		 (BIND =class (nth =num =types)))
	(=!WME (LABEL =wme)
	       (CHECK (eq (frame-class =wme) =class))
	       (CHECK (parameter-satisfies-complex =wme =num =s-constraints)))
	(a-heuristic (LABEL =a-heuristic)
		     :name vote-for-parameter-based-on-urgency-and-minimal-distance-s-constraints
		     :strength =strength))
  :RHS (($make-ballot :time =now :status 'posted :scope =class :item =wme
		      :source =a-heuristic
		      :vote (* (- 1.0
				  (abs (- =urgency
					  (minimal-distance
					   =wme =num =+examples))))
			       =strength))))

(RULE r2
  :LHS ((time :value =now)
	(context :instantiation =instantiation
		 (BIND =num (length =instantiation))
		 :space (a-space :operator (an-operator :types =types
							:num-of-params =num-of-params)
				 :+examples =+examples)
		 (CHECK (< =num =num-of-params))
		 (BIND =class (nth =num =types))
 		 :cluster (a-cluster :s-constraints =s-constraints))
	(=!WME (LABEL =wme)
	       (CHECK (eq (frame-class =wme) =class))
	       (CHECK (parameter-satisfies-complex =wme =num =s-constraints)))
	(a-heuristic (LABEL =a-heuristic)
		     :name vote-for-parameter-based-on-urgency-and-minimal-distance-s-constraints
		     :strength =strength))
  :RHS (($make-ballot :time =now :status 'posted :scope =class :item =wme
		      :source =a-heuristic
		      :vote (* (- 1.0
				  (abs (- =urgency
					  (minimal-distance
					   =wme =num =+examples))))
			       =strength))))
