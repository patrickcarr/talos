;;; Sample use of FRulekit literalize.  The contents of this file represents
;;; research under way and is the property of Klaus Gross.

(defun update-history ()
  (declare (special framename slotname facetname newval))
  (if (and 
       ;; new value isn't the same as the old value
       (not (eq newval (get-value framename slotname)))
       ;; history is a facet of the slot
       (memq 'history (get-facet-names framename slotname))
       ;; havent't already updated the history for this time period
       (not (= (car (get-facet framename slotname 'history))
	       (- *now* 1))))
      (add-to-facet
       framename
       slotname
       'history
       (list (- *now* 1) (get-facet framename slotname 'history)))))

(literalize time ()
  value nil)

(literalize a-slot ()
  name nil                  ; slot name
  type nil                  ; number or symbol
  max nil                   ; maximum value of slot seen if type is number
  min nil                   ; minimum value of slot seen if type is number
  values nil                ; values seen if type is symbol
  )

(literalize an-object (pre-if-set (update-history))
  location (value nil history nil)
  rotation (value nil history nil)
  enclosure (value nil history nil)
  mass (value nil history nil)
  )

(literalize a-complex-object (is-a (an-object))
  parts nil
  )

(literalize a-primitive-object (is-a (an-object) pre-if-set (update-history))
  inside-color (value nil history nil)
  outside-color (value nil history nil)
  inside-surface-texture (value nil history nil)
  outside-surface-texture (value nil history nil)
  taste (value nil history nil)
  temperature (value nil history nil)
  )

(literalize a-circle (is-a (a-primitive-object) pre-if-set (update-history))
  normal (value nil history nil)
  radius (value nil history nil)
  )

(literalize a-cylinder (is-a (a-primitive-object) pre-if-set (update-history))
  axis (value nil history nil)
  height (value nil history nil)
  radius (value nil history nil)
  )

(literalize a-polygon (is-a (a-primitive-object) pre-if-set (update-history))
  normal (value nil history nil)
  vertices (value nil history nil)
  )

(literalize a-sphere (is-a (a-primitive-object) pre-if-set (update-history))
  radius (value nil history nil)
  )

(literalize a-physiological-state (pre-if-set (update-history))
  state nil                 ; state name (i.e. energy or health or ...)
  minimum nil               ; minimum value that state can obtain
  maximum nil               ; maximum value that state can obtain
  optimum-min nil           ; minimum value that state can obtain without goal
  optimum-max nil           ; maximum value that state can obtain without goal
  value (value nil history nil)
  )

(literalize a-goal ()
  time nil                  ; time of goal creation
  goal nil                  ; list of desired effects
  urgency nil               ; urgency of the goal
  )

(literalize a-space ()
  name nil                  ; name of the space
  operator nil              ; operator of the space (wme ptr)
  effect nil                ; effect of applying op to an instance of the space
  +examples nil             ; instances that have the effect
  -examples nil             ; instances that don't have the effect
  not-covered nil           ; instances not-covered in children
  evaluation 0              ; evaluation of the clustering
  clustering nil            ; clustering of space
  )

(literalize a-cluster ()
  space nil                 ; space of hypothesis (wme ptr)
  operator nil              ; operator of the space (wme ptr)
  effect nil                ; effect of applying op to an instance of the space
  number nil                ; the # of the cluster in the parent clustering
  depth nil                 ; the depth of the cluster
  s-constraints nil         ; s-cluster of all parents plus s-cluster
  g-constraints nil         ; g-cluster of all parents plus g-cluster
  s-cluster nil             ; specialized cluster
  g-cluster nil             ; generalized cluster
  +examples nil             ; positive examples
  -examples nil             ; negative examples
  not-covered nil           ; events not covered in children
  evaluation 0              ; evaluation of the clustering
  clustering nil            ; clustering of s-cluster
  specialization-of nil     ; pointer to parent
  )

(def-frame specialization-of (is-a (relation) propagate nil)
  combination-type first
  slots-inherited (value '(space operator effect
			    (s-constraints append) (g-constraints append)))
  )

(literalize update-clustering ()
  item nil                  ; the item whose clustering needs to be updated
  event nil                 ; the event used to update the clustering
  )

(literalize delete-clusters-with-parent ()
  parent nil                ; the parent
  )
 
(literalize an-operator ()
  name nil                  ; name of the function which is the operator
  parameters nil
  types nil                 ; the types of the parameters  
  number-of-parameters nil  ; number of parameters that function has
  )

(literalize a-heuristic (pre-if-set (update-history))
  strength (value nil history nil) ; 0...n
  name nil                  ; name of the heuristic
  )

(literalize context ()
  time nil                  ; time the context applies
  goal nil                  ; ptr to goal the context applies to
  space nil                 ; ptr to space the context applies to
  cluster nil               ; ptr to cluster the context applies to
  instantiation nil         ; instantiation of hypothesis
  event nil                 ; symbol representing instantiation
  effect nil                ; effect that was actually achieved
  )

(literalize ballot ()
  time nil                  ; time the ballot was created
  status nil                ; posted or counted
  scope nil                 ; a-goal, a-space, a-cluster, or a type
  item nil                  ; ptr to item
  position nil              ; if item is a type then position
  source nil                ; ptr to heuristic that posted ballot
  vote 0                    ; amount of vote
  )

(literalize total ()
  time nil                  ; time of counting
  scope nil                 ; a-goal, a-space, a-cluster, or a type
  item nil                  ; ptr to item
  position nil              ; if item is a type then position
  votes 0                   ; total vote
  )

(make-a-heuristic
 nil
 :name 'vote-for-goal-of-last-context
 :strength '(value 100 history nil))

(make-a-heuristic
 nil
 :name 'vote-for-goal-based-on-urgency
 :strength '(value 100.0 history nil))

(make-a-heuristic
 nil
 :name 'vote-for-space-of-last-context
 :strength '(value 100.0 history nil))

(make-a-heuristic
 nil
 :name 'vote-for-space-based-on-urgency-and-evaluation
 :strength '(value 100.0 history nil))

(make-a-heuristic
 nil
 :name 'vote-for-space-based-on-urgency-and-number-of-examples
 :strength '(value 100.0 history nil))

(make-a-heuristic
 nil
 :name 'vote-for-cluster-of-last-context
 :strength '(value 100.0 history nil))

(make-a-heuristic
 nil
 :name 'vote-for-cluster-based-on-urgency-and-evaluation)
 :strength '(value 100.0 history nil))

(make-a-heuristic
 nil
 :name 'vote-for-cluster-based-on-urgency-and-number-of-examples
 :strength '(value 100.0 history nil))

(make-a-heuristic
 nil
 :name 'vote-for-parameter-based-on-urgency-and-minimal-distance
 :strength '(value 100.0 history nil))

(make-a-heuristic
 nil
 :name 'vote-for-parameter-based-on-urgency-and-minimal-distance-s-constraint
 :strength '(value 100.0 history nil))

(make-a-heuristic
 nil
 :name 'vote-for-parameter-based-on-urgency-and-minimal-distance-g-constraint
 :strength '(value 100.0 history nil))
     
(make-a-slot nil :name 'location :type 'point)
(make-a-slot nil :name 'rotation :type 'point)
(make-a-slot nil :name 'enclosure :type 'number)
(make-a-slot nil :name 'mass :type 'number)
(make-a-slot nil :name 'parts :type 'list)
(make-a-slot nil :name 'inside-color :type 'point)
(make-a-slot nil :name 'outside-color :type 'point)
(make-a-slot nil :name 'taste :type 'number)
(make-a-slot nil :name 'temperature :type 'number)
(make-a-slot nil :name 'normal :type 'point)
(make-a-slot nil :name 'radius :type 'number)
(make-a-slot nil :name 'axis :type 'point)
(make-a-slot nil :name 'height :type 'number)
(make-a-slot nil :name 'vertices :type 'list)

(make-a-physiological-state
 nil
 :state 'energy
 :minimum 0
 :maximum 100
 :optimum-min 85
 :optimum-max 90
 :value '(value 90 history nil))

(make-a-physiological-state
 nil
 :state 'health
 :minimum 0
 :maximum 100
 :optimum-min 85
 :optimum-max 90
 :value '(value 90 history nil))

(make-an-operator
 nil
 :name 'eat
 :parameters '(an-object)
 :number-of-parameters 1)
