;;; based on [cad]/usr/jgc/lisp/rulekit.l   --  jgc   Aug 6, 1985  --
;;; LastEditDate of franz code = Sun Sep 29 19:26:51 1985

;;; Copyright (c) 1985, 1988 Carnegie Mellon University.
;;; To obtain a copy of this software, please contact:
;;;	Peter Shell
;;;	School of Computer Science
;;;	Carnegie Mellon University
;;;	Pittsburgh, PA  15213

;;; Converted to CommonLisp by Peter Shell,  starting Oct. 15 1985
;;; Exists on the ML vax as: /usr/pshell/frulekit/agenda.lisp
;;; This version is for the CommonLisp FRulekit.

;;; 20-Mar-87: Added inscount field to bucket, and instants and bucket field
;;; to rule, for faster agenda execution.  Modified the r-agenda algorithm
;;; slightly to make agenda execution faster.  See frulekit/notes/fasteragenda
;;; for details.
;;; Oct. 20 1986:  Bucket add and bucket delete (fields :bucket-add and
;;; :bucket-del) added.  Representation of Agenda changed to a list of
;;; bucket names.  Lists of rule names can be gotten from a bucket name
;;; by evaluating the bucket name or calling get-bucket on the bucket name.
;;; (make-bucket bucket-name contents) defines a bucket but doesn't put
;;; it into the Agenda.
;;; Jan. 86: added functions add-rules, delete-rules, add-buckets, and
;;; delete-buckets, add-new-bucket.
;;; Implemented support for :*next bucket position.
;;; Added goto-top-of-bucket and goto-top-of-agenda.

; Still to do:
;  1. Change remaining progs to 'do' statements.
;  6. Writing a 'tester' that would run FRuleKit (and for that matter
;     FrameKit) through a library of examples and expected results in
;     order to insure that code is working correctly before continues to
;     do so as changes and additions are made.
;     

;;;; -------------------------------------------------------------------- ;;;
;;;; PACKAGE STUFF							  ;;;
;;;;

(in-package "FRULEKIT" :use '("LISP" "PARMENIDES"))

;;; User-accessible agenda functions, macros and variables documented in the
;;; user's manual.  The core FRulekit symbols are exported from build.lisp.
(export '(		;; FUNCTIONS & MACROS
          R-linear R-cycle R-priority R-agenda Create-new-agenda
	  Cont-agenda Add-rule Add-rules Make-bucket Add-buckets
	  Add-bucket Add-new-bucket Delete-rules Delete-buckets
	  Delete-bucket Get-bucket Compile-extra-tests
	  Rk-rule-add Rk-rule-del Rk-rule-bucket-add
	  Rk-rule-bucket-del Rk-rule-ktest Rk-rule-bucket Rk-rule-instants

    ;; VARIABLES
	  !trace-test !trace-act !trace-agenda !!control *AGENDA*
	 ))

(load-messages (format NIL "~Aag-messages.~A" *FR-PATHNAME* *LANGUAGE*))

(proclaim '(special
	    !count-act          ;; counts rules fired
	    !count-test         ;; counts rules tested
	    !time
	    !value
	    !test
	    !!control           ;; for agenda control structure -- see below
            !trace-agenda       ;; for tracing all adds & deletes to agenda
            !trace-act  	;; for rule tracing (all rules fired)
	    !trace-test  	;; for rule tracing (all rules tested)
	    *CURRENT-RULE-NAME* ;; current rule that's being tested, for inter.
	    *RULE-NAMES*
	    bucket-ptr
	    *SWITCHES*))

(defvar *AGENDA* nil)  ;;Holds the current agenda, a list of bucket names.
		   ;;This allows deleting and adding of buckets to the agenda.
(eval-when (load eval)
  (if (not *AGENDA-LOADED*)
      (nconc *SWITCHES* '(!!control !trace-agenda !trace-act !trace-bucket
				    !count-act !count-test))))
(setq *AGENDA-LOADED* T)
(defvar agenda-ptr nil)

(eval-when (load eval compile)
  (defstruct rk-rule
    pnode	;; a back-pointer to the prod. node that points to it
    rhs      	;;text of the rhs
    lhs      	;;text of the lhs
    beliefs	;;text of the belief RHS
    left-access
    (pname NIL :type atom)
    extra-test	;; Was called test.  Lisp expression that must be true in
		;; addition to the working memory (LHS) tests.  Should not contain 
		;; any tests referring to the variables in the LHS; those types of
		;; tests go in the LHS.  Default is current-rule-in-conflict-set-p.
    num-tests	;; (num-alha&beta-tests . num-check-vars) for each conde.
    (inscount 0 :type integer)	;; the number of instantions of this rule in the conflict set
    (break NIL :type atom)	;; flag saying when to break relative to firing.
    disj-nodes	;; points to top&bottom disjunctive nodes (if any).
    add		;; ((bucket-1 pos-1 . ruls-1) (bucket-2 pos-2 . ruls-2) ...) 
		;; Adds the set of rules to each corresponding bucket at the 
		;; specified position in the bucket iff the rule fires.
    del		;; Deletes ruls-i from bucket-i in the agenda.
    ktest 	;; Lisp code, exectuted iff rule-test evals to nil (yes, nil).
		;; ONLY APPLICABLE WHEN AGENDA IS USED!
    bucket-add	;; ((bucket-name-1 <agenda-pos-1>) (bucket-name-2 <agenda-pos-2>))
		;; adds the given buckets to the agenda.
    bucket-del 	;; (bucket-name-1 bucket-name-2 ...) deletes the given buckets.
    bucket	;; Added 20-Mar-87.  Improper list of bucket(s) that it's in.
    instants	;; Added 20-Mar-87.  List of active instantiations of rule.
    		;; Now we can do c.r. only on the instants of the rule.
    ))

;;;  test:, ktest:, action:, add:, del: makes commonlisp look for a package
;;;  since commonlisp sees <symbol>: as a reference to a package.
;;;  So the rule was changed to a commonlisp structure (see below).
;;;  This means that all the slot names in the lexicon can't have colons after
;;;  them.

(defun init-agenda ()
;;   (setq *CR-STRATEGY* (cons 'AGENDA *MEA*))	;;Agenda strategy defined below
  (setq !trace-test T)
  (setq !trace-act T)
  (setq !trace-agenda T)
  (setq !!control :bucket)   ;; this is the most common setting.
;; Initialize these here for non-agenda control structures
  (setq !count-test 0)
  (setq !count-act 0))

(eval-when (load eval) (init-agenda))

;;; A rulekit bucket, which contains a list of rules.
(eval-when (eval load compile)
  (defstruct (bucket (:print-function bucket-printer)
		     (:constructor make-bucket0))
    contents
    inscount))	;;Inscount slot added 20-Mar-87 for faster agenda execution.


;;;; Inscount bookkeeping, for inter.lisp.

;;; rule is a rk-rule structure.
(defun agenda-add-instant (rule instant)
  (push instant (rk-rule-instants rule))
  (dolist (bucket (rk-rule-bucket rule))
    (incf (bucket-inscount bucket))))

(defun agenda-delete-instant (rule instant)
  (setf (rk-rule-instants rule)
	(delete instant (rk-rule-instants rule) :test #'eq))
  (dolist (bucket (rk-rule-bucket rule))
    (decf (bucket-inscount bucket))))

(defun agenda-delete-instants-of (rule)
  (let ((inscount (rk-rule-inscount rule)))
    (setf (rk-rule-inscount rule) 0)
    (setf (rk-rule-instants rule) nil)
    (dolist (bucket (rk-rule-bucket rule))
      (decf (bucket-inscount bucket) inscount))))

(defun agenda-clear-rules (rule-names)
  (dolist (rname rule-names)
    (let ((prod (get rname 'prod)))
      (setf (rk-rule-inscount prod) 0)
      (setf (rk-rule-instants prod) nil))))

(defun agenda-clear-buckets (bucket-names)
  (dolist (bname bucket-names)
    (let ((bucket (get bname 'bucket)))
      (setf (bucket-inscount bucket) 0))))


; Utility macros & functions.  These obviate the need for macros.l & general.l
;

;;; (funl (args) body)  => (function (lambda (args) body))
(defmacro funl (var body)
  `(function (lambda ,var ,body)))

;;; Given the name of the rule return the add list (rules to add if it fires).
(defmacro rk-add (rul)
  `(rk-rule-add (get ,rul 'prod)))

(defmacro rk-del (rul)
  `(rk-rule-del (get ,rul 'prod)))

(defmacro rk-extra-test (rul)
  `(rk-rule-extra-test (get ,rul 'prod)))

(defmacro rk-ktest (rul)
  `(rk-rule-ktest (get ,rul 'prod)))

(defmacro rk-bucket-del (rul)
  `(rk-rule-bucket-del (get ,rul 'prod)))

(defmacro rk-bucket-add (rul)
  `(rk-rule-bucket-add (get ,rul 'prod)))

(defun compile-extra-tests ()
  (mapc #'compile-extra-test *RULE-NAMES*))

(defun compile-extra-test (rname)
  (let ((rule (get rname 'prod)))
    (and (not (compiled-function-p (rk-rule-extra-test rule)))
	 (setf (rk-rule-extra-test rule)
	       (compile nil (rk-rule-extra-test rule))))))

;;; General low-level functions:
(defun r-test (rul)
  (setq *CURRENT-RULE-NAME* rul)    ;;for in-conflict-set-p in inter.
  (let ((loc-test (apply (rk-extra-test rul) nil)))
    (and loc-test (setq !test loc-test))
    (and !trace-test (ml-format T :rule-tested rul loc-test))
    (setq !count-test (1+ !count-test))
    loc-test))

;;; perform-cr, refract and fire-chosen-instant from inter.lisp.
(defun r-act (rul)
  (declare (special num-cycles))
  (inter-inc-cycle)
  (perform-cr (rk-rule-instants (get rul 'prod)))
  (refract)
  (setq !value (fire-chosen-instant))
;;;   (setq !value (run 1))   ;;in inter.lisp.
  (and !trace-act (ml-format T :rule-applied rul !value))
  (incf !count-act)
  (and num-cycles (decf num-cycles))
  !value)

;;; FRulekit conflict-resolution strategy-defining macro.  Inter must
;;; be loaded first.
(def-cr-strategy agenda
  (lambda (ins)
    (if (and *CURRENT-RULE-NAME*
	     (eq (rk-rule-pname (instant-prod ins)) *CURRENT-RULE-NAME*))
	1
	0))
  >)

(defun r-apply (rul)
  (and
   (r-test rul)
   (r-act rul)))

; Linear (one-pass) control strategy. Halts after all rules were tried
; once, or one rule returns an explicit halt.
(defun r-linear (ruls &optional num-cycles)
  (declare (special num-cycles))
  (setq !test nil)
  (do ((ruls ruls (cdr ruls)))
      ((null ruls) nil)
      (cond ((eq :halt (r-apply (car ruls)))
	     (return :halt)))))


; Cyclic (multi-pass) linear control strategy.  Halts if ALL rules do
; do not fire or if one rule returns an explicit halt.  When one
; rule fires, it goes on to the next rule in ruls.
(defun r-cycle (ruls &optional num-cycles)
  (declare (special num-cycles))
   (setq !test nil)
   (prog (ptr)
         (setq ptr ruls)
      lp (cond ((null ptr)
                (cond (!test (setq ptr ruls)
		             (setq !test nil)
			     (go lp))
		      (t (return nil))))
	       ((eq :halt (r-apply (car ptr)))
                (return :halt)))
         (setq ptr (cdr ptr))
	 (go lp)))

; PSG-style priority cycle. Tests rules in set in given order, and returns 
; to the first rule if any rule fires.  Halts on explitict 'halt action.
(defun r-priority (ruls &optional num-cycles)
  (declare (special num-cycles))
   (setq !test nil)
   (prog (ptr)
         (setq ptr ruls)
      lp (cond ((null ptr) (return nil))
	       ((eq :halt (r-apply (car ptr)))
                (return :halt))
	       (!test (setq !test nil)
	              (setq ptr ruls)
		      (go lp))
	       (t (setq ptr (cdr ptr))
	          (go lp)))))


;;; Agenda structure:    [changed Oct. 20 1985 PShell]
;;; (name-1 name-2 ... name-N)
;;; Bucket content: (rule1 rule2 ... ruleN)
;;; A bucket is accessed by calling (get-bucket <name>)
;;; buckets are made with make-bucket: (make-bucket <name> <contents>)
;;; buckets are activated into the current agenda by: (activate-bucket <name>)
;;; buckets are deleted by calling: (delete-bucket <name>)

; If the action of a rule returns the KEYWORD :halt the agenda stops,
;  else if it returns :recycle computation goes to the first rule in the
;     first bucket (top of the agenda),
;  else the action is dictated by the setting of !!control (see below).
;
; If !!control = :priority, upon firing any rule it goes back to
;  testing the first rule of the first bucket.  When none of the rules
;  in the bucket fire it goes on to the first rule in the next bucket.
; If !!control = :bucket-priority, upon firing any rule it goes back to
;  testing the first rule in the current bucket.  When none of the rules in
;  the bucket fire it checks whether any rule in the bucket had fired, 
;  and if so, it goes to first rule in the agenda, else to the
;  first rule in the next bucket.

; If !!control = :bucket, upon firing  any rule it goes back to 
;  testing the first rule of the current bucket.
; If !!control = :linear, upon firing any rule it continues on
;  to the next rule in the current bucket; or, if there are none left,
;  it continues to the first rule in the next bucket.
; If !!control = :linear-cycle, then control is linear with respect to the
;  agenda but cyclic inside the buckets.  FRulekit
;  iterates through the buckets one after the other.  At the end of a bucket,
;  if any rule had fired in that bucket, then control is returned to the
;  beginning of that bucket again.  Otherwise, the next bucket is chosen.

;
; Under all circumstances, if a rule does not fire, the next rule in
;  the bucket is tested.  If the end of the agenda is reached, 
;  the agenda stops, printing run-time statistics.
;

(defvar !trace-agenda nil)
(defvar !trace-bucket nil)

(defun activate-bucket (bname)
  (push bname *AGENDA*))

;;; Takes an alist containing an agenda.  First initializes agenda.  Then adds
;;; each bucket to the bucket hash table, and activates the bucket in the
;;; current agenda.  Normally used once per task run.
;;; Since every modification to a bucket modifies the bucket hash table, this
;;; will have to be called again if you want to re-run the task.
(defun create-new-agenda (buckets)
  (setq *AGENDA* nil)
  (mapc #'(lambda (bucket)
	    (make-bucket (car bucket) (cdr bucket))
	    (activate-bucket (car bucket)))
	buckets)
  (setq *AGENDA* (nreverse *AGENDA*)))


(defun bucket-printer (bucket stream depth)
  (declare (ignore depth))
  (ml-format stream :contents-1 (bucket-contents bucket))
  (ml-format stream :instantiations-1 (bucket-inscount bucket)))


(defmacro check-ruleness (rname bname)
  `(if (not (get ,rname 'prod))
      (ml-cerror :go-on :rule-in-bucket-not-defined ,rname ,bname)))

;;; makes a bucket named BNAME and with contents CONTENTS.
;;; DOESN'T add it to the current agenda.
(defun make-bucket (bname contents)
  (dolist (rname contents)
    (check-ruleness rname bname))
  (let ((bucket (make-bucket0
		 :contents contents
		 :inscount (compute-inscount contents))))
    (if (not (zerop (bucket-inscount bucket)))
	(ml-format T :instantiations-2 (bucket-inscount bucket) bname))
    (dolist (rname contents)
      (pushnew bucket
		  (rk-rule-bucket (get rname 'prod))))
    (putprop bname bucket 'bucket)
    bucket))

;;; Returns the total of all the inscounts of the given rule names.
(defun compute-inscount (rnames)
  (let ((total 0))
    (dolist (rname rnames)
      (let ((prod (get rname 'prod)))
	(if prod (incf total (rk-rule-inscount prod)))))
    total))

(defun modify-bucket (bname contents)
  (setf (bucket-contents (get bname 'bucket)) contents))

(defun get-bucket2 (bname)
  (let ((bucket (get bname 'bucket)))
    (cond (bucket (bucket-contents bucket))
	  (T (make-bucket bname NIL)
	     NIL))))


;;; returns the list of rules given the bucket name
(eval-when (load eval compile)
  (defmacro get-bucket (bname)
    `(bucket-contents (get ,bname 'bucket))))

 
(defun delete-bucket (bname)
  (setq *AGENDA* (delete bname *AGENDA*)))

;;; Top level agenda control fn.
(defun r-agenda (agenda &optional num-cycles)
  (declare (special num-cycles))
  (create-new-agenda agenda)
  (cont-agenda num-cycles))


;;; Uses the global variable *AGENDA*
(defun cont-agenda (&optional num-cycles)
  (declare (special num-cycles))	;; needs to be available to r-act.
  (prog (!count-act !count-test
           bucket-test-flg)
         (setq agenda-ptr *AGENDA*)
	 (setq !count-act 0)
	 (setq !count-test 0)
	 (setq !time (get-internal-run-time))
     lp1 (cond ((or (null agenda-ptr)
		    (and num-cycles (zerop num-cycles)))
		(pr-stat)
		(setq *INSTANT* *TOP-LEVEL-INSTANT*)
		(return nil)))
         (if !trace-agenda (ml-format T :agenda-ptr agenda-ptr))
	 (setq bucket-ptr (get-bucket (car agenda-ptr)))
	 (setq bucket-test-flg nil)
     lp2 (cond ((and num-cycles (zerop num-cycles))
		(pr-stat)
		(setq *INSTANT* *TOP-LEVEL-INSTANT*)
		(return nil)))
	(if !trace-bucket (ml-format T :bucket-ptr bucket-ptr))
	(setq !test nil)
	(setq !value nil)
	(cond ((or (null bucket-ptr)
		   (zerop (bucket-inscount (get (car agenda-ptr) 'bucket))))
	       (setq agenda-ptr
		     (cond ((and (eq !!control :linear-cycle) bucket-test-flg)
			    agenda-ptr) 
			   (bucket-test-flg *AGENDA*)
			   (T (cdr agenda-ptr))))
	       (go lp1))  ;;each go lp1 means break out of lp2 loop
	      ((eq :halt (r-agenda-apply (car bucket-ptr)
					 *AGENDA* agenda-ptr))
	       (pr-stat)
	       (setq *INSTANT* *TOP-LEVEL-INSTANT*)
	       (return :halt))
	      ((eq !value :bucket-halt)
	       (setq agenda-ptr (cdr agenda-ptr))
	       (go lp1))
	      ((eq !value :recycle)
	       (setq agenda-ptr *AGENDA*)
	       (go lp1))
	      (!test 
	       (ecase !!control 
		 (:priority (goto-top-of-agenda)
			   (go lp1))
		 (:bucket-priority (goto-top-of-bucket)
				  (setq bucket-test-flg T)
				  (go lp2))
		 (:bucket (goto-top-of-bucket)
			 (go lp2))
		 (:linear-cycle (setq bucket-test-flg T)
				(go lp2))
		 (:linear (setq bucket-ptr (cdr bucket-ptr))
			 (go lp2))))
	      (T (setq bucket-ptr (cdr bucket-ptr))
		 (go lp2)))))

(defun goto-top-of-bucket ()
  (setq bucket-ptr
    (get-bucket (car agenda-ptr))))

(defun goto-top-of-agenda ()
  (setf agenda-ptr *AGENDA*))


(defun pr-stat ()    ;; print statistics (rules tested & fired)
  (ml-format T :rules-tested
      !count-test !count-act (/ (+ (- (get-internal-run-time) !time) 30) 60)))

; Applies a single rule from the agenda: 
;   rul = rule name
;   agenda = pointer to the top of the agenda
;   agenda-ptr = pointer the current bucket in the agenda
(defun r-agenda-apply (rul agenda agenda-ptr)
  (declare (ignore agenda))
  (cond ((r-test rul)
	 (r-act rul)
	 (r-add rul)
	 (r-del rul)
	 (r-bucket-add rul)
	 (r-bucket-del rul)
	 !value)
	((r-ktest rul)
	 (r-rem rul agenda-ptr))))

(defun r-add (rul)
  (and (rk-add rul)
       (add-rules (rk-add rul))))

(defun add-rules (addlst)    ;; Adds rules to specified buckets
  (dolist (rulespec addlst)
    (add-rule (car rulespec) (cadr rulespec) (cddr rulespec))))

(defun add-rule (bspec bpos rnames)    ;; Adds rules to a specified bucket
  (and !trace-agenda rnames
       (ml-format T :adding-to-bucket rnames bspec))
  (let* ((bname (old-agenda-pos-find bspec *AGENDA* agenda-ptr))
	 (bcontents (get-bucket2 bname))
	 (bucket (get bname 'bucket)))
    (dolist (rname rnames)
      (check-ruleness rname bname))
    (cond ((atom rnames)
	   (ml-cerror :go-on :rule-list rnames))
	  (bcontents
	   (cond ((eq bpos :*first)
		  (modify-bucket bname (append rnames bcontents)))
		 ((eq bpos :*last)
		  (modify-bucket bname (nconc bcontents rnames)))
		 ((eq bpos :*next)
		  (setf (cdr bucket-ptr) (append rnames (cdr bucket-ptr))))
		 ((and (consp bpos) (eq (car bpos) :*after))
		  (splice-after rnames bcontents (cadr bpos)))
		 (T
		  (ml-error :illegal-bucket-position bpos))))
	  (T (modify-bucket bname rnames)))
    (dolist (rname rnames)
      (pushnew bucket
		  (rk-rule-bucket (get rname 'prod))))
    (incf (bucket-inscount bucket)
	  (compute-inscount rnames))))

;;; Splice rnames into bcontents after whichrule, which is assumed to be
;;; in bcontents.
(defun splice-after (rnames bcontents whichrule)
  (let ((find (member whichrule bcontents)))
    (cond ((not find)
	   (ml-error :rule-not-in-bucket whichrule))
	  (T
	   (nconc rnames (cdr find))
	   (setf (cdr find) rnames)))))

(defun r-del (rul)
  (and (rk-del rul)
       (delete-rules (rk-del rul))))

(defun delete-rules (delst)  ;; Deletes rules from specified buckets
  (declare (special delst))
  (prog (agenda-pos rulst bucket)
	(and !trace-agenda delst
	     (ml-format T :deleting delst))
     lp (or delst (return nil))
	(setq bucket (old-agenda-pos-find (caar delst) *AGENDA* agenda-ptr))
	(cond ((null (setq agenda-pos 
			   (agenda-pos-find (caar delst) *AGENDA* agenda-ptr)))
	       (ml-format T :illegal-agenda-pos (car delst))
	       (return nil))
	      ((eq (setq rulst (cdar delst)) :*ALL)
	       (modify-bucket bucket nil)
	       (setf (bucket-inscount bucket) 0))
	      (T (mapc (funl (r)
			     (modify-bucket bucket
					    (delete r agenda-pos)))
		       rulst)
		 (dolist (rname rulst)
		   (setf (rk-rule-bucket (get rname 'prod))
			 (delete (get bucket 'bucket)
				 (rk-rule-bucket (get rname 'prod)))))
		 (decf (bucket-inscount (get bucket 'bucket))
		       (compute-inscount rulst))))
	(setq delst (cdr delst))
	(go lp)))


;;; Returns the list of rules associated with the indicated bucket.
(defun agenda-pos-find (br agenda agenda-ptr)      ;;Aux fn to r-add & r-del
  (get-bucket (old-agenda-pos-find br agenda agenda-ptr)))


(defun add-new-bucket (name where rules)
  (make-bucket name rules)
  (add-bucket name where))

;;; Inserts the given buckets AFTER the bucket indicated by the position
;;; (next, current etc).
;;; Example: ((bucket-1 :*next) (bucket-3 :*current))
(defun r-bucket-add (rule)
  (let ((bucket-specs (rk-bucket-add rule)))
    (add-buckets bucket-specs)))

(defun add-buckets (bucket-specs)
  (dolist (bucket-spec bucket-specs)
    (add-bucket (car bucket-spec) (cadr bucket-spec))))

;;; For elegance sake.  spec is :*next, *current, :*previous, :*first, :*last or
;;; the bucket name to add name after.
(defun add-bucket (name spec)
  (let ((bucket (get name 'bucket)))
    (if bucket
	(setf (bucket-inscount bucket)
	      (compute-inscount (bucket-contents bucket)))))
  (and !trace-agenda spec
       (ml-format T :adding-bucket name spec))
  (let ((agenda-pos (agenda-find spec *AGENDA* agenda-ptr)))
    (if (null agenda-pos)
	(if (memq spec '(:*current :*next :*previous :*first :*last))
	    (setq *AGENDA* (nconc *AGENDA* (list name)))
	    (ml-format T :agenda-pos-not-found spec))
	(setf (cdr agenda-pos)
	      (cons name (cdr agenda-pos))))))

;;; Example: (bucket-1 bucket-2)
(defun r-bucket-del (rule)
  (let ((bucket-names (rk-bucket-del rule)))
    (delete-buckets bucket-names)))

(defun delete-buckets (bucket-names)
  (and !trace-agenda bucket-names
       (ml-format T :deleting-buckets bucket-names))
  (mapc #'(lambda (bucket)
	    (setq *AGENDA*
		  (delete bucket *AGENDA*)))
	bucket-names))


;;; Like agenda-pos-find but doesn't take the car.
(defun agenda-find (br agenda agenda-ptr)  ;; Aux fn to bucket-add & bucket-del
  (case br
    (:*next (cdr agenda-ptr))
    (:*current agenda-ptr)
    (:*last (last agenda-ptr))
    (:*first agenda)
    (otherwise (memq br agenda))))

;;; Almost an exact copy of the old agenda-find-pos, but due to the new
;;; data structure, it now returns just the name of the bucket, instead
;;; of a list containing the name and the list of rules.
(defun old-agenda-pos-find (br agenda agenda-ptr)
   (case br
	(:*next (cadr agenda-ptr))
	(:*current (car agenda-ptr))
	(:*last (car (last agenda-ptr)))
	(:*first (car agenda))
	(otherwise br)))

(defun r-ktest (rul)
   (eval (rk-ktest rul)))

(defun r-rem (rul agenda-ptr)
  (let* ((bname (car agenda-ptr))
	 (bucket (get bname 'bucket)))
    (and !trace-agenda
	 (ml-format T :rule-removing rul bname))
    (modify-bucket (car agenda-ptr)
		   (delete rul bucket))
    (if (get rul 'prod)
	(decf (bucket-inscount bucket)
	      (rk-rule-inscount (get rul 'prod))))))
