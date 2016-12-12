(defun testify22 (class slot facet0 thing)
  (let* ((facet0 (if facet0 (assure-keyword facet0)))
	 (slot (assure-current slot))
	 (facet (or facet0 (if (facetedp class slot) :value))))
    (cond ((eq facet :value)
	   (list (smash class "-" slot ".VALUE") thing))
	  ((null facet)
	   (list (smash class "-" slot) thing))
	  (T
	   (multiple-value-bind (slotnum facetnum)
				(pa-get-snf-nums
				 class (assure-keyword slot) facet)
	     (cond ((and facet (not (and slotnum facetnum)))
		    (ml-cerror :go-on :slot-facet-not-in-frame
			       slot facet class))
		   ((and (not facet) (not slotnum))
		    (ml-cerror :go-on :no-slot-in-class slot class))
		   (T
		    `(aref (aref ,thing ,slotnum) ,facetnum))))))))
