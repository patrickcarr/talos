;;; -*- Mode:Lisp; Package:FRulekit -*-

;;; Based on Rulekit build.slisp of 9-May-1986. This is the Parmenides version,
;;; started 11-June-1986. Original started 13-Nov-85.
;;; Copyright (c) 1985, 1988 Peter Shell and Carnegie Mellon University.
;;; To obtain a copy of this software, please contact:
;;;	Peter Shell
;;;	Computer Science Dept.
;;;	Carnegie Mellon University
;;;	Pittsburgh, PA  15213

;;; NOTE: parmenides.lisp must be loaded before this module is loaded or
;;; compiled, since it contains necessary macros, defstructs and def-frames.

;;;; -------------------------------------------------------------------- ;;;
;;;; PACKAGE STUFF							  ;;;
;;;;

(in-package "FRULEKIT" :nicknames '("FRK") :use '("LISP" "PARMENIDES"))
;;; NOTE: the nickname "FR" is purposely not used since it is used for Framekit
;;; BUILD's exportable symbols are exported from INTER since I group all
;;; of BUILD and INTER's symbols in one place.

#+Allegro (import 'excl::putprop)

;;; Directory in which the FRulekit multi-lingual messages reside.
(defvar *FR-PATHNAME* "/afs/cs/usr/pshell/frulekit/")
(load-messages (format NIL "~Afr-messages.~A" *FR-PATHNAME* *LANGUAGE*))

(defun frk-define-language (language)
  (load-messages (format NIL "~Afr-messages.~A" *FR-PATHNAME* language))
  (define-language language))		;; Tell parmenides about it

;;; This module is for compiling FRulekit rules into the Rete net.
(proclaim '(special
	    *TOP-RETE-NODE* *RETE-VAR-TO-LEFT-TEST* *RETE-VAR-TO-RIGHT-TEST*
	    *BETA-TEST* *PRED-TAGS* *NUM-ORS* *AREF-FN-NAMES*   ;; Parmenides
	    *RETE-TEST-HASH* *RETE-N-NODE-HASH* *PRINT-DEPTH* *CONFLICT-SET*
	    *RULE-NAMES* *NEWNODE* *NEWNODES* *CHECKVARS* *MAX-BACK* *FIRST-BIND*
	    *BIND-VARS* *CVARS* *CHECKS-BETAS* *RK-VARP* *FORMAT* *TRACE-SHARE*
	    *OLDBETA* *BUILDING* *NUM-BETAS* *NUM-ALPHAS* *AGENDA-LOADED*
	    *BIND-ACCESS* *ALPHA-LISP-TEST* *BIND-COUNT* *NUM-BINDS* *BIND-NUMS*
	    *CYCLE* *MODCYCLE* *RIGHT-DISJ-LIST* *LEFT-DISJ-LIST* *IN-DISJUNCT*
	    *ADDED-INSTANTS* *REMOVED-INSTANTS* *TYPE-LENGTH* *BIND-VARS-DISJ*
	    *DONT-WARN-LISP-CHECKS* *FIRED-INSTANTS*))

;;; Trace data structures.
(proclaim '(simple-vector
	    *PROD-MATCHES* *PROD-UNMATCHES* *PROD-FIRINGS*
	    *ADDITIONS* *DELETIONS* *MODIFIES*
	    *REFRACTIONS* *NON-DELETIONS*))

(defvar *MAX-BACK* 50)
(defvar *NAME-NODES* NIL)	;; Controls whether FRK gives names to rete nodes
(defvar *RULEKIT-TERSE* NIL)
(defvar *DONT-WARN-LISP-CHECKS* NIL)	;; If true, then don't warn about
					;; lisp checks not checking FRK vars.

(defvar *PROD-FIRINGS* (make-array *MAX-BACK* :initial-element NIL))
(defvar *PROD-MATCHES* (make-array *MAX-BACK* :initial-element NIL))
(defvar *PROD-UNMATCHES* (make-array *MAX-BACK* :initial-element NIL))
(defvar *ADDITIONS* (make-array *MAX-BACK* :initial-element NIL))
(defvar *DELETIONS* (make-array *MAX-BACK* :initial-element NIL))
(defvar *MODIFIES* (make-array *MAX-BACK* :initial-element NIL))
(defvar *REFRACTIONS* (make-array *MAX-BACK* :initial-element NIL))
(defvar *NON-DELETIONS* (make-array *MAX-BACK* :initial-element NIL))
(defvar *BIND-COUNT* '(make-array NIL))

(proclaim '(bit-vector *NODETYPE* *ALPHA-NODETYPE*))
(defvar *TYPE-LENGTH* 8)	;; Number of bits in the type field
(defvar *NODETYPE*
  (make-array *TYPE-LENGTH* :element-type 'bit :initial-element 0))
(defvar *ALPHA-NODETYPE*
  (make-array *TYPE-LENGTH* :element-type 'bit :initial-element 0))

;;; These are the bit positions of the various types of rete nodes.
;;; If something is not an alpha node then it's a beta node.
;;; Not all combinations of bits are possible.  E.g., if something is a
;;; T node then it's not an ALPHA, NOT or PROD node.
;;; Nodes can't be BLOCKED and ACTIVE at the same time, but they can be not
;;; BLOCKED and not ACTIVE.
(defvar *ALPHABIT*	0)
(defvar *NOTBIT*	1)	;; Since NOT nodes and T nodes always occupy 
(defvar *TNODE-BIT*  *NOTBIT*)	;; different places in Rete, they share a bit.
(defvar *BINDBIT*	2)
(defvar *ORBIT*		3)
(defvar *PARENT-BIT*	4)
(defvar *TOP-DISJ-BIT*	5)
(defvar *ACTIVE-BIT*	6)
(defvar *BLOCKED-BIT*	7)

(proclaim '(bit-vector *TNODE-TYPE* *NNODE-TYPE*))
(defvar *TNODE-TYPE*
  (make-array *TYPE-LENGTH* :element-type 'bit :initial-element 0))
(setf (sbit *TNODE-TYPE* *TNODE-BIT*) 1)

(defvar *NNODE-TYPE*
  (make-array *TYPE-LENGTH* :element-type 'bit :initial-element 0))
(setf (sbit *NNODE-TYPE* *TNODE-BIT*) 0)
(setf (sbit *ALPHA-NODETYPE* *ALPHABIT*) 1)

(defvar *LEFT-BRACKET* #\[)
(defvar *RIGHT-BRACKET* #\])

;;;Dec lisp erroneously evaluates printer names in defstruct.
#+DEC (dolist (pname
	       '(rete-node-printer rk-rule-printer wme-printer
		 instant-printer bucket-printer cr-goal-printer))
	(set pname pname))

;;; A hack around the Kyoto carriage-return bug:
#+KCL (defvar *FORMAT* (symbol-function 'format))
#+KCL (defun format (stream string &rest vars)
	(terpri)
	(apply *FORMAT* `(,stream ,string ,@vars)))


;;; RETE-VAR-TO-TEST is a hash table which maps from user variable names to
;;; the lisp test that access them from the input to the node.
;;; re-initialized for each production.
;;; RETE-TEST-HASH is for pointers to tests so common T-nodes can be shared.
;;; RETE-N-NODE-HASH is the analagous hash table for any class tests, and is
;;; disjoint from RETE-TEST-HASH.

(eval-when (eval load compile)
  (defstruct (rete-node (:print-function rete-node-printer))
   test		;;The test for any node - slot-access fn, variable binding
    		;; consisteny, or class type.  Implemented as a CL fn object.
   left-mem	;;Stores WMe's that arrive from the left
   right-mem	;;Stores WMe's that arrive from the right
   output-mem	;;Stores WME's that matched and go to its output nodes.
   (type #*00000000 :type bit-vector)
   left-input	;;The left input is either an alpha node or a beta node
   right-input	;;The right input is always an alpha node, or NIL if this node
                ;; is itself an alpha node.
   left-output	;;Pointer to list of nodes that take input from it, iff alpha
   right-output	;;Pointer to list of nodes that take input, goes to beta
   pnode-prod   ;;Name of the production which should fire if node is reached.
   slots	;;Slots at or under node.  For smart-modify.
   function)	;;The function-object form of the test.

;;; Shared memory structure.  output-mem, right-mem and left-mem all point to it.
  (defstruct (shared (:type vector))
    contents)

  (defmacro my-nconc (struct list)
    `(if ,struct
	 (nconc ,struct ,list)
	 (setf ,struct ,list)))

  (if (not (fboundp 'memq))
      (defmacro memq (a l)
	(let ((tempvar (gentemp "X")))
	  `(member-if #'(lambda (,tempvar) (eq ,a ,tempvar)) ,l))))

  (defmacro excise (rname &optional excise-all)
    `(excise-rule (get ',rname 'prod) ',rname ',excise-all))

  (defmacro turn-on (bit-vector bit)
    `(setf (sbit ,bit-vector ,bit) 1))

  (defmacro turn-off (bit-vector bit)
    `(setf (sbit ,bit-vector ,bit) 0))

;;; node-type macros  
  (defmacro t-or-nnode-p (node)
    `(eq (rete-node-left-input ,node) *TOP-RETE-NODE*))
  
  (defmacro tnodep (node)
    `(and (t-or-nnode-p ,node)
	  (not (zerop (sbit (rete-node-type ,node) *TNODE-BIT*)))))
  
  (defmacro nnodep (node)
    `(and (t-or-nnode-p ,node)
	  (zerop (sbit (rete-node-type ,node) *TNODE-BIT*))))

  (defmacro alphap (node)
    `(not (zerop (sbit (rete-node-type ,node) *ALPHABIT*))))

  (defmacro alpha-type-p (type)
    `(not (zerop (sbit ,type *ALPHABIT*))))

  (defmacro betap (node)
    `(zerop (sbit (rete-node-type ,node) *ALPHABIT*)))
  
  (defmacro beta-type-p (type)
    `(zerop (sbit ,type *ALPHABIT*)))
  
  (defmacro pnodep (node)
    `(rete-node-pnode-prod ,node))

  (defmacro absnodep (node)
    `(not (zerop (sbit (rete-node-type ,node) *NOTBIT*))))
  
  (defmacro bindp (node)
    `(not (zerop (sbit (rete-node-type ,node) *BINDBIT*))))
  
  (defmacro bind-type-p (type)
    `(not (zerop (sbit ,type *BINDBIT*))))

  (defmacro not-bind-type-p (type)
    `(zerop (sbit ,type *BINDBIT*)))
  
  (defmacro disj-type-p (type)
    `(not (zerop (sbit ,type *ORBIT*))))

  (defmacro disj-node-p (node)
    `(not (zerop (sbit (rete-node-type ,node) *ORBIT*))))

  (defmacro blocked-type-p (type)
    `(not (zerop (sbit ,type *BLOCKED-BIT*))))

  (defmacro not-blocked-type-p (type)
    `(zerop (sbit ,type *BLOCKED-BIT*)))

  (defmacro blocked-node-p (node)
    `(not (zerop (sbit (rete-node-type ,node) *BLOCKED-BIT*))))

  (defmacro not-blocked-node-p (node)
    `(zerop (sbit (rete-node-type ,node) *BLOCKED-BIT*)))

  (defmacro active-type-p (type)
    `(not (zerop (sbit ,type *ACTIVE-BIT*))))

  (defmacro active-node-p (node)
    `(not (zerop (sbit (rete-node-type ,node) *ACTIVE-BIT*))))

  (defmacro not-active-node-p (node)
    `(zerop (sbit (rete-node-type ,node) *ACTIVE-BIT*)))

;;; T iff node is the highest disjunctive nodes (but not the parent).
  (defmacro top-disjunctive-node-p (node)
    `(not (zerop (sbit (rete-node-type ,node) *TOP-DISJ-BIT*))))

  (defmacro top-disjunctive-type-p (type)
    `(not (zerop (sbit ,type *TOP-DISJ-BIT*))))

;;; T iff node is the parent of disjunctive nodes.
  (defmacro parent-disjunctive-node-p (node)
    `(not (zerop (sbit (rete-node-type ,node) *PARENT-BIT*))))

  (defmacro parent-disjunctive-type-p (type)
    `(not (zerop (sbit ,type *PARENT-BIT*))))

;;; An easy way to handle disjunctive nodes.  Sometimes nodes can be plural
;;; (disjunctive), or singluar.
;;; Does nothing if structure is NIL.
  (defmacro do-list-or-atom ((var structure) &rest body)
    `(if (consp ,structure)
	 (dolist (,var ,structure) ,@body)
	 (if ,structure
	     (let ((,var ,structure))
	       ,@body))))

;;; A FRulekit rule which may be placed in an Frulekit agenda or rete net.
;;; The agenda slots are defined in the agenda module.
  (defstruct rk-rule
    pnode	;;keep a back-pointer to the production node that points to it
    rhs      	;;text of the RHS
    lhs      	;;text of the lhs
    beliefs	;;text of the belief RHS
    left-access	;;a-list of (var . access) pairs telling for each variable how
  		;;to get its value from the pnode.
    (pname NIL :type atom)
    extra-test	;; Was called test.  Lisp expression that must be true in
                ;; addition to the working memory (LHS) tests.  Should not
		;; contain any tests referring to the variables in the LHS;
		;; those types of tests go in the LHS.  Default is
		;; current-rule-in-conflict-set-p.
    num-tests	;; (num-alha&beta-tests . num-check-vars) for each conde.
    (inscount 0 :type integer)	;; #  instant.s of this rule in conflict set
    (break NIL :type atom)	;; Flag saying when to pause this rule.
		;; Possible values are  :BEFORE, :AFTER, :ALWAYS, or NIL.
    disj-nodes	;; points to disjunctive-nodes structure, below (if any).
    rhs-fn	;; For the function version of rhs
    beliefs-fn	;; and beliefs.
)

  (defstruct disj-nodes	;; Disjunctive rules point to these structures.
    parent		;; List of rete node above disjunctive nodes for rule.
    bottom)		;; List of bottom disjunctive nodes, lowest first.

  (defmacro rk-format (stream msgname &rest args)
    `(if (not *RULEKIT-TERSE*)
	 (ml-format ,stream ,msgname ,@args)))
)	;; End of (eval-when (eval load compile))

(defun rete-node-and-not-disj-node-p (thing)
  (and (not (listp thing))
       (not (disj-node-p thing))))

(defun rete-node-and-disj-node-p (thing)
  (and (not (listp thing))
       (disj-node-p thing)))

(defun maybe-print (val msg stream)
  (if val (format stream msg val)))

(defun rk-rule-printer (prod stream depth)
  (declare (ignore depth))
  (let ((*PRINT-PRETTY* NIL))	;;Because the Slisp pretty-printer is screwed up.
    (terpri)
    (format stream "~%Rule ~A:~%" (rk-rule-pname prod))
    (princ "LHS: ") (terpri)
    (mapc #'(lambda (condition)
	      (princ "   ") (pprint condition))
	  (rk-rule-lhs prod))
    (format T "~%RHS:~%")
    (dolist (action (rk-rule-rhs prod))
      (format T "   ~A~%" action))
    (princ "left-access: ") (princ (rk-rule-left-access prod)) (terpri)
 ))

;;; User rule pretty-printer
(defmacro pp-rule (rname)
  (let ((rule (get rname 'prod))
	(*PRINT-PRETTY* T))
    (cond ((not rule)
	   (ml-format T :no-such-rule rname))
	  (T
	   (format T "Rule ~A:~%" rname)
	   (if (rk-rule-break rule)
	       (format T "Breakpoint = ~S~%" (rk-rule-break rule)))
	   (princ " :LHS ") (terpri)
	   (dolist (condition (rk-rule-lhs rule))
	     (princ "   ")
	     (prin1 condition) (terpri))
	   (and (rk-rule-extra-test rule)
		(not (equal (rk-rule-extra-test rule)
			    '(lambda ()
			       (current-rule-in-conflict-set-p))))
		(format T ":Extra-test  ~A ~%" (rk-rule-extra-test rule)))
	   (pp-rule-actions (rk-rule-beliefs rule) " :BELIEFS ")
	   (pp-rule-actions (rk-rule-rhs rule) " :RHS ")
	   (when *AGENDA-LOADED*
	     (maybe-print (rk-rule-add rule) ":Add    ~A ~%" t)
	     (maybe-print (rk-rule-del rule) ":Del    ~A ~%" t)
	     (maybe-print (rk-rule-ktest rule) ":KTest  ~A ~%" t)
	     (maybe-print (rk-rule-bucket-add rule) ":Bucket-add  ~A ~%" t)
	     (maybe-print (rk-rule-bucket-del rule) ":Bucket-del  ~A ~%" t))
	   T))))

(defun pp-rule-actions (actions name)
  (when actions
    (princ name) (terpri)
    (cond ((compiled-function-p actions)
	   (format T "  ~S~%" actions))
	  (T
	   (format T "  (~S ~S" (car actions) (cadr actions))    ;;(lambda ())
		   (dolist (action (cddr actions))
		     (format T "~%    ~S" action))
		   (format T ")~%")))))

(defvar *RULE-NAMES* NIL)
(defvar *TOP-RETE-NODE*
  (make-rete-node :test (function (lambda (X) X))
		  :function (function (lambda (X) X))
		  :left-mem NIL :right-mem NIL))
(defvar *RETE-VAR-TO-LEFT-TEST* (make-hash-table :size 47))	;; beta
(defvar *RETE-VAR-TO-RIGHT-TEST* (make-hash-table :size 47))	;; alpha
(defvar *RIGHT-DISJ-LIST* NIL)
(defvar *LEFT-DISJ-LIST* NIL)
(defvar *RETE-TEST-HASH* (make-hash-table :size 53))	;; T- & N- nodes
(defvar *RETE-N-NODE-HASH* (make-hash-table :size 53))
(defvar *AGENDA-LOADED* NIL)	;;T only when the agenda module is loaded.
(defvar *BIND-VARS* NIL)	;;Names of vars in bind commands for each prod.
(defvar *BIND-VARS-DISJ* NIL)	;;Names of vars in disj bind commands.
(defvar *BIND-NUMS* NIL)	;;Number (pos. in array) of each bind var.
(defvar *PRED-TAGS* NIL)	;;Tag certain RK vars with predicates.

(defun re-init-trace ()
  (fill *PROD-FIRINGS* NIL)
  (fill *PROD-MATCHES* NIL)
  (fill *PROD-UNMATCHES* NIL)
  (fill *ADDITIONS* NIL)
  (fill *DELETIONS* NIL)
  (fill *MODIFIES* NIL)
  (fill *REFRACTIONS* NIL)
  (fill *NON-DELETIONS* NIL))

;;;; Variable access (and other) macros
(eval-when (eval load compile)

;;; Inverse of record-left-test-of-var
  (defmacro get-left-test (var)
    `(if *IN-DISJUNCT*
	 (or (getf *LEFT-DISJ-LIST* ,var)
	     (gethash ,var *RETE-VAR-TO-LEFT-TEST*))
	 (gethash ,var *RETE-VAR-TO-LEFT-TEST*)))

;;; (Almost) inverse of record-right-test-of-var
  (defmacro get-right-test (var)
    `(if *IN-DISJUNCT*
	 (getf *RIGHT-DISJ-LIST* ,var)
	 (gethash ,var *RETE-VAR-TO-RIGHT-TEST*)))

  (defmacro record-left-test-of-var (var test)
    `(if *IN-DISJUNCT*
	 (setf (getf *LEFT-DISJ-LIST* ,var) ,test)
	 (setf (gethash ,var *RETE-VAR-TO-LEFT-TEST*) ,test)))

;;; Must check: if var already has a right test, then there is more than one
;;; occurance of the var in one conde (intra-condition testing).  Thus each
;;; var has a list of tests associated with it.
  (defmacro record-right-test-of-var (var test)
    `(if *IN-DISJUNCT*
	 (push ,test (getf *RIGHT-DISJ-LIST* ,var))
	 (push ,test (gethash ,var *RETE-VAR-TO-RIGHT-TEST*))))

  (defmacro get-old-left-test (var)
    `(gethash ,var *RETE-VAR-TO-LEFT-TEST*))

  (defmacro record-old-left-test (var test)
    `(setf (gethash ,var *RETE-VAR-TO-LEFT-TEST*) ,test))

  (defmacro clear-right-test (var)
    `(if *IN-DISJUNCT*
	 (setf (getf *RIGHT-DISJ-LIST* ,var)
	       (cdr (getf *RIGHT-DISJ-LIST* ,var)))
	 (setf (gethash ,var *RETE-VAR-TO-RIGHT-TEST*)
	       (cdr (get-right-test ,var)))))

  (defmacro clear-all-right-tests (var)
    `(if *IN-DISJUNCT*
	 (remf *RIGHT-DISJ-LIST* ,var)
	 (remhash ,var *RETE-VAR-TO-RIGHT-TEST*)))

  (defmacro clear-left-test (var)
    `(if *IN-DISJUNCT*
	 (remf *LEFT-DISJ-LIST* ,var)
	 (remhash ,var *RETE-VAR-TO-LEFT-TEST*)))

  (defmacro map-rete-vars (form)
    `(if *IN-DISJUNCT*
	 (do ((left-plist *LEFT-DISJ-LIST* (cddr left-plist)))
	     ((null left-plist) T)
	   (funcall ,form (car left-plist) (cadr left-plist)))
	 (maphash ,form *RETE-VAR-TO-LEFT-TEST*)))

  (defmacro maybe-clear-rights (absp vars)
    `(if ,absp (clear-rights ,vars)))
  
  (defmacro clear-rights (vars)
    `(dolist (var ,vars)
       (clear-right-test var)))
  
  (defmacro clear-lefts (vars)
    `(dolist (var ,vars)
       (clear-left-test var)))
  
  (defmacro maybe-push-var (varname list)
    `(and (not (memq ,varname *BIND-VARS*))
	  (not (memq ,varname *BIND-VARS-DISJ*))
	  (not (memq ,varname ,list))
	  (push ,varname ,list)))

  (defmacro add-beta-test (access)
    `(if *OLDBETA*
	 (push ,access *BETA-TEST*)
	 (push ,access *ALPHA-LISP-TEST*)))

  (defmacro process-bind-var-test (varname betap)
    `(if ,betap
	 (record-or-find-left-bind-access ,varname)
	 (record-or-find-right-bind-access ,varname)))

  (defmacro process-mbind-var-tests (varnames betap)
    `(if ,betap
	 (dolist (varname ,varnames)
	   (record-or-find-left-bind-access varname))
	 (dolist (varname ,varnames)
	   (record-or-find-right-bind-access varname))))

  (defmacro bind-xlate (betap)
    `(if ,betap 'left 'right))

  (defmacro store-bind-num-for-var (varname number)
    `(progn
       (setf (getf *BIND-NUMS* ,varname) ,number)))
;;       (record-global-and-disj-left-test-of-var varname
;;						(make-left-bind-access))

  (defmacro get-bind-num-for-var (varname)
    `(getf *BIND-NUMS* ,varname))

  (defmacro label-cmd-p (cmd)
    `(and (atom ,cmd)
	  (string= (symbol-name ,cmd) "LABEL")))

  (defmacro check-cmd-p (cmd)
    `(and (atom ,cmd)
	  (string= (symbol-name ,cmd) "CHECK")))

  (defmacro bind-cmd-p (cmd)
    `(and (atom ,cmd)
	  (string= (symbol-name ,cmd) "BIND")))

  (defmacro mbind-cmd-p (cmd)
    `(and (atom ,cmd)
	  (string= (symbol-name ,cmd) "MBIND")))

  (defmacro or-cmd-p (cmd)
    `(and (atom ,cmd)
	  (string= (symbol-name ,cmd) "<OR>")))

  (defmacro abs-cmd-p (cmd)
    `(and (atom ,cmd)
	  (string= (symbol-name ,cmd) "<ABS>")))

;;; The beta-position attribute defines whether a CHECK should be placed in
;;; a beta node if it contains that symbol.  Thus, defining a symbol (e.g.,
;;; a lisp variable) as a beta-var says that it may change without the status
;;; of the WME that it's in changing (since if it was in the alpha test and
;;; it changed but the WME didn't, then it wouldn't get re-checked).
  (defmacro def-beta-var (varname)
    `(setf (get ,varname :beta-position) T))

  (defmacro def-alpha-var (varname)
    `(remprop ,varname :beta-position))

  (defmacro beta-position-p (varname)
    `(and (symbolp ,varname)
	  (get ,varname :beta-position)))

;;; Primitive facility for keeping track of which predicate should be used
;;; for equality testing of vars.  Currently only used to make label wmes
;;; use 'eq' for testing since using the default, 'equal', is slower
;;; and can result in an infinite loop.  (Added 22-June-87)
  (defmacro tag-var (varname tag)
    `(setf (getf *PRED-TAGS* ,varname) ,tag))

  (defmacro get-tag (varname)
    `(getf *PRED-TAGS* ,varname))
)

(defun print-frulekit-herald ()
  (format T "FRulekit Copyright (c) 1985, 1990 Carnegie Mellon.~%"))

;;; Initialize the FRulekit net
(defun init-rete ()
  (init-reader)
  (re-init-trace)
  (dolist (rname *RULE-NAMES*)
    (remprop rname 'defined))
  (setq *BETA-TEST* NIL
	*RULE-NAMES* NIL
	*ADDED-INSTANTS* NIL
	*REMOVED-INSTANTS* NIL)
  (setf (rete-node-right-output *TOP-RETE-NODE*) NIL)
  (clrhash *RETE-TEST-HASH*)
  (clrhash *RETE-N-NODE-HASH*)
  (setq *CONFLICT-SET* NIL
	*FIRED-INSTANTS* NIL
	*PRINT-DEPTH* 0	;; kluge for the rete-node-printer, since the
			   	;; commonlisp printer is brain-damaged.
	*NUM-BETAS* 0		;; number of beta tests, for specificity
	*NUM-ALPHAS* 0		;; number of alpha tests, for specificity
	*TRACE-SHARE* NIL
	*BUILDING* NIL
	*CHECKVARS* NIL
	*PRED-TAGS* NIL
	*CYCLE* 0
	*MODCYCLE* 0)
  (rk-format T :build-initialized))

;;; These have to be read before anything else.
;;; New syntax addition (8-July-86): =!x expands to (any x), which really means
;;; =x (CHECK (isa-instance =x 'x)).
(defun read-equal (stream char)
  (declare (ignore char))
  (let ((next-char (peek-char NIL stream)))		
    (cond ((equal next-char #\SPACE)
	   '\=)
	  ((char= next-char #\!)
	   (read-char stream)	;;strip away the "!"
	   (list 'any (read stream NIL :eof t)))
	  ((char= next-char #\<)
	   (read-char stream)
	   (list 'anysub (read stream NIL :eof t)))
	  (T
	   (list 'var (read stream NIL :eof t))))))

(defun single-char-syntax (stream char)
  (declare (ignore stream))
  char)

(defun init-reader ()
  (set-macro-character #\=  #'read-equal T)   ;;so =x will read (var x).
  (set-macro-character #\[ #'single-char-syntax NIL)
  (set-macro-character #\] #'single-char-syntax NIL)
)

;;; For Lisps with the XP pretty-printing system, this causes them to print
;;; (VAR X) as =X,  (ANY X) as =!X,  and (ANYSUB X) as =<X.
(when (find-package 'xp)
  (pushnew :xp *FEATURES*))

;;; The define-print-dispatches must be done using #+:XP so that the reader
;;; won't try to read any reference to the xp: package if there is no such
;;; package.
#| commented out since cmucl seems to have a different kind of xp which
   doesn't include define-print-dispatch.
#+:XP
(progn
  (xp:define-print-dispatch (cons (member var)) ((:priority 10)) (xp list)
			    (if (and (consp (cdr list)) (null (cddr list)))
				(funcall (xp::fs "XP" "=~S") xp (cadr list))
				(xp:fill-style xp list)))

  (xp:define-print-dispatch (cons (member any)) ((:priority 10)) (xp list)
			    (if (and (consp (cdr list)) (null (cddr list)))
				(funcall (xp::fs "XP" "=!~S") xp (cadr list))
				(xp:fill-style xp list)))

  (xp:define-print-dispatch (cons (member anysub)) ((:priority 10)) (xp list)
			    (if (and (consp (cdr list)) (null (cddr list)))
				(funcall (xp::fs "XP" "=<~S") xp (cadr list))
				(xp:fill-style xp list)))
) |#


(eval-when (load eval)
  (init-rete)
  (print-frulekit-herald))


;;; Defines the rule structure then compiles the LHS of the rule.
(defmacro rule (rname &rest slots)
  `(progn
    (cond ((get ',rname 'defined)
	   (excise ,rname)
	   (push ',rname *RULE-NAMES*)
	   (rk-format T :recompiling-rule ',rname))
	  (T
	   (rk-format T :compiling-rule ',rname)
	   (push ',rname *RULE-NAMES*)))
    (putprop ',rname T 'defined)
    (let ((newrule
	   (apply #'make-rk-rule ',slots)))
      (push NIL (rk-rule-rhs newrule))	;;The RHS was made a lambda (26-Mar-87)
      (push 'lambda (rk-rule-rhs newrule))
      (setf (rk-rule-rhs-fn newrule) (eval `(function ,(rk-rule-rhs newrule))))
      (push NIL (rk-rule-beliefs newrule))
      (push 'lambda (rk-rule-beliefs newrule))
      (setf (rk-rule-beliefs-fn newrule)
	    (eval `(function ,(rk-rule-beliefs newrule))))
      (setf (rk-rule-inscount newrule) 0)
      (add-extra-test newrule)
      (setf (rk-rule-pname newrule) ',rname)
      (putprop ',rname newrule 'prod)
      (setf (rk-rule-lhs newrule)
	    (preprocess-conds (copy-tree (rk-rule-lhs newrule))))
      (compile-prod ',rname (rk-rule-lhs newrule) newrule))))

;;; For the agenda module.
(defun add-extra-test (newrule)
  (if (not (rk-rule-extra-test newrule))
      (setf (rk-rule-extra-test newrule)
	    '(lambda () (current-rule-in-conflict-set-p)))
      (if (rk-rule-lhs newrule)    ;;doesn't make sense to check c.s. if no LHS
	  (setf (rk-rule-extra-test newrule)
		`(lambda ()
		   (and (current-rule-in-conflict-set-p)
			,(rk-rule-extra-test newrule)))))))

(defun re-rule (rname)
  (let ((newrule (get rname 'prod)))
    (compile-prod rname (rk-rule-lhs newrule) newrule)))

;;; Non-macro version of RULE.
(defun rule* (rname lhs rhs)
  (cond ((get rname 'defined)
	 (eval `(excise ,rname))
	 (push rname *RULE-NAMES*)
	 (rk-format T :recompiling-rule rname))
      (T
       (rk-format T :compiling-rule rname)
       (push rname *RULE-NAMES*)))
  (putprop rname T 'defined)
  (let ((newrule
	 (make-rk-rule :LHS lhs :RHS rhs)))
    (push NIL (rk-rule-rhs newrule))   ;; The RHS was made a lambda (26-Mar-87)
    (push 'lambda (rk-rule-rhs newrule))
    (setf (rk-rule-rhs-fn newrule) (eval `(function ,(rk-rule-rhs newrule))))
    (push NIL (rk-rule-beliefs newrule))
    (push 'lambda (rk-rule-beliefs newrule))
    (setf (rk-rule-beliefs-fn newrule)
	  (eval `(function ,(rk-rule-beliefs newrule))))
    (setf (rk-rule-inscount newrule) 0)
    (add-extra-test newrule)
    (setf (rk-rule-pname newrule) rname)
    (putprop rname newrule 'prod)
    (setf (rk-rule-lhs newrule) (preprocess-conds (rk-rule-lhs newrule)))
    (compile-prod rname (rk-rule-lhs newrule) newrule)
    rname))

(defun preprocess-conds (condes)
  (declare (special condes))
  (let ((*NEW-LABEL-NAMES* NIL))
    (declare (special *NEW-LABEL-NAMES*))
    (twiddle-absence-after-or condes)
    (mapcan #'preprocess-cond condes)))

;;; Since absence nodes can't join disjunct CE's, we move the absence test
;;; to the disjunct by putting a copy of each absence test after the <OR>
;;; into the <OR> statement.
(defun twiddle-absence-after-or (ces)
  (let ((or-clause NIL))
    (loop
      (setq or-clause (member-if #'(lambda (ce) (or-cmd-p (car ce)))
				 ces))
      (cond ((and or-clause (abs-cmd-p (car (cadr or-clause))))
	     (splice-in-absence-test or-clause)
	     (setq ces or-clause))
	    (T (setq ces (cdr ces))))
      (if (null ces) (return T)))))

(defun splice-in-absence-test (or-clause)
  (let ((absence-test (cadr or-clause)))
    (dolist (disjunct (cdar or-clause))
      (nconc disjunct (list absence-test)))
    (setf (cdr or-clause) (cddr or-clause))))


;;; Does everything RULE does except doesn't push it through the net.
;;; Usually for modifying the RHS without having to mess with the net.
;;; Caution: if you modify the LHS then you must call RULE or BUILD-RULE;
;;; otherwise, it won't have any effect!
(defmacro modify-rule (rname &rest slots)
  `(let ((newrule
	  (make-rk-rule ,@slots)))
     (setf (rk-rule-pname newrule) ',rname)
     (putprop ',rname newrule 'prod)))

;;; FIX!
(defun modify-rk-rule (rule slots)
  (do ((slot slots (cddr slot)))
      ((null slot) t)
    (set-facet rule (car slot)
	       :value (eval (cadr slot)))))

(defun compile-prod (pname conds prod)
  (init-prod pname prod)
  (init-rk-compiler)
  (compile-conds conds NIL pname prod)
  (setf (get pname 'backpointers) (nreverse (get pname 'backpointers)))
  (when (> *NUM-BINDS* -1)
    (setf (cadr *BIND-COUNT*) (1+ *NUM-BINDS*))		;; *
    (setq *BIND-COUNT* (copy-list `(make-array NIL))))
  (if conds
      (make-it-a-pnode *OLDBETA* prod))
  pname)


;; * Modifiying the *BIND-COUNT* modifies the code which makes the array
;; which holds the bind vars, to be exactly as big as how many bind vars
;; were actually used.

(defun init-prod (pname prod)
  (setf (rk-rule-pnode prod) NIL)
  (setf (rk-rule-num-tests prod) (copy-list '(0 . 0)))	;;initialize num tests.
  (setf (get pname 'backpointers) NIL))

(defun init-rk-compiler ()
  (clrhash *RETE-VAR-TO-LEFT-TEST*)
  (clrhash *RETE-VAR-TO-RIGHT-TEST*)
  (setq *IN-DISJUNCT* NIL
	*LEFT-DISJ-LIST* NIL
	*RIGHT-DISJ-LIST* NIL
	*BIND-VARS-DISJ* NIL
	*BIND-NUMS* NIL
	*NEWNODES* NIL
	*OLDBETA* NIL
	*BIND-ACCESS* *AREF-FN-NAMES*	;;From Parmenides
	*BIND-VARS* NIL
	*FIRST-BIND* NIL
	*NUM-BINDS* -1
	*PRED-TAGS* NIL))


;;; Return a list containing all the conds that cond expands to.
;;; A syntax of nested condition elements is now allowed: e.g.,
;;; (boy :likes (girl :likes dog) :has sex)
;;; expands to: ((boy :likes =girl :has sex) (girl (LABEL =girl) :likes dog))
(defun preprocess-cond (conde)
  (declare (special last-nest))
  (setq last-nest NIL)
  (preprocess-cond0 conde))

(defun preprocess-cond0 (conde)
  (declare (special last-nest))
  (let ((origconde (list conde))
	(sname (cadr conde))
	(conde (cddr conde))
	carconde)
    (setq last-nest origconde)
    (loop
     (if (and (null conde) (null sname)) (return origconde))
     (cond ((listp sname)
	    (cond ((and (symbolp (car sname))
			(member (symbol-name (car sname))
				'("CHECK" "BIND" "MBIND")
				:test #'string=)
			(not (eq last-nest origconde)))		;; Nested from
		   (nconc (car last-nest) (list sname))		;; last time
		   (delete sname (car origconde))))
	    (setq sname (car conde) conde (cdr conde)))
	   ((eq sname *LEFT-BRACKET*)			;;skip over bracket
	    (setq sname (cdr conde))
	    (if (eq (car sname) *RIGHT-BRACKET*)
		(setq conde (cdr sname) sname (car sname))
		(setq conde (cddr sname) sname (cadr sname))))
	   (T
	    (setq carconde (car conde))
	    (when (and (consp carconde) (not (rk-variablep carconde))
		       (not (rk-anyp carconde))		;; Nested wme match
		       (not (rk-anysubp carconde)))
	      (let ((lname (maybe-labelize carconde)))
		(nconc origconde (preprocess-cond0 carconde))
		(rplaca conde (maybe-add-var (car carconde) lname))))
	    (setq sname (cadr conde))
	    (setq conde (cddr conde)))))
    origconde))

;;; If the given conde has a label statement, then return that label,
;;; else return NIL.
(defun label-name (carconde)
  (cadr
   (cadar (member-if
	   #'(lambda (x) (and (listp x) (label-cmd-p (car x)))) carconde))))

;;; Returns the label name if found in the given conde; otherwise, generates
;;; one.
(defun maybe-labelize (carconde)
  (declare (special condes))
  (or (label-name carconde)
      (let ((label-name (find-unique-label-name (car carconde) condes)))
	(push `(LABEL (VAR ,label-name)) (cdr carconde))
	label-name)))

(defun find-unique-label-name (olabel-name condes)
  (declare (special *NEW-LABEL-NAMES*))
  (let ((label-name (if (listp olabel-name) (cadr olabel-name) olabel-name)))
    (cond ((and (not (member label-name *NEW-LABEL-NAMES*))
		(not (label-in-ces-p label-name condes)))
	   (push label-name *NEW-LABEL-NAMES*)
	   label-name)
	  (T
	   (do* ((i 1 (1+ i))
		 (new-name (smash label-name i) (smash label-name i)))
		((and (not (member new-name *NEW-LABEL-NAMES*))
		      (not (label-in-ces-p new-name condes)))
		 (push new-name *NEW-LABEL-NAMES*)
		 new-name))))))

(defun label-in-ces-p (label-name condes)
  (dolist (ce condes NIL)
    (if (eq (label-name ce) label-name) (return T))))

(defun find-varname (carconde)
  (if (or (rk-anyp carconde) (rk-anysubp carconde))
      (cadr carconde)
      carconde))

(defun maybe-add-var (carconde lname)
  `(var ,(or lname (find-varname carconde))))

;;; It used to be this but it caused incorrect tests:
;;;   (if (not (or (rk-anyp carconde) (rk-anysubp carconde)))
;;;       `(var ,(or lname carconde))
;;;       carconde)

(defun count-ors (conds)
  (let ((num-ors 0))
    (dolist (cond conds)
      (if (or-cmd-p (car cond)) (incf num-ors)))
    num-ors))

;;; Compile a list of tests from a single CE, into the rete network.
;;; Don't care about the local vars that the absence test uses, (except that we
;;; must clear them from the right and left access), and we DON'T want it to
;;; change the status of where the positive vars are.
(defun compile-conds (conds allvars pname prod)
  (if (abs-cmd-p (caar conds))
      (ml-cerror :continue-compilation :first-not-abs))
  (setq *NUM-ORS* (count-ors conds))
  (let ((oldoldbeta NIL))
    (do ((conde conds (cdr conde)))
	((null conde) T)
      (fill *NODETYPE* 0)
      (cond ((abs-cmd-p (caar conde))
	     (cond ((not (cddar conde))	;;Negation test of just one conde.
		    (compile-neg-node
		     (cadar conde) pname prod NIL (or-cmd-p (caadr conde))))
		   (T (handle-negated-conj (cdar conde) allvars pname prod))))
	    ((or-cmd-p (caar conde))
	     (setq allvars
		   (nunion allvars (copy-list (compile-or-clause
					       allvars (cdar conde)
					       pname prod oldoldbeta)))))
	    (T
	     (setq allvars (nunion
			    allvars (copy-list
				     (compile-one (car conde) pname *NODETYPE*
						  prod NIL
						  (or-cmd-p (caadr conde))))))
	     (update-tests allvars oldoldbeta)
	     (update-bindvars oldoldbeta))
	    ) ;; setq of allvars used to be above the let*.
      (setq oldoldbeta T)
      (if (and *BUILDING* *NEWNODE*) (pushnew *NEWNODE* *NEWNODES*)))))

;;; Added 18-Apr-88. Only need to worry about using special BIND cmds to bind
;;; variables if a new variable is bound in more than one disjunct.  Otherwise,
;;; simply don't update the access expression for the variables referred to in
;;; the disjuncts.  This way they will refer to CE's outside the disjunct.
;;; Clause is a list of disjuncts.  Each disjunct is either a single CE, or
;;; a list of CE's.
;;; Shared-mem: each of the last (lowest) disjuncts share the same output-mem.
;;; Returns new variables in disjuncts.
(defun compile-or-clause (prev-vars clause pname prod oldoldbeta)
  (if (not (rk-rule-disj-nodes prod))
      (setf (rk-rule-disj-nodes prod)
	    (make-disj-nodes :parent (list *OLDBETA*)))
      (push *OLDBETA* (disj-nodes-parent (rk-rule-disj-nodes prod))))
  (turn-on (rete-node-type *OLDBETA*) *PARENT-BIT*)
  (let* ((*IN-DISJUNCT* T)
	 (*FIRST-BIND* (eq *BIND-ACCESS* *AREF-FN-NAMES*))
	 (disjuncts (mapcar #'identity clause))
	 (left-access-lists (make-sequence 'list (length clause)
					   :initial-element NIL))
	 (bind-access-lists (make-sequence 'list (length clause)
					   :initial-element NIL))
	 (backpointers (make-sequence 'list (length clause)
				      :initial-element NIL))
	 (old-betas (make-sequence 'list (length clause)
				   :initial-element *OLDBETA*))
	 (pos-node NIL))
    (loop
      (setq pos-node NIL)
      (do ((disjunct disjuncts (cdr disjunct))
	   (left-list left-access-lists (cdr left-list))
	   (bind-list bind-access-lists (cdr bind-list))
	   (old-beta old-betas (cdr old-beta))
	   (backpointer backpointers (cdr backpointer)))
	  ((null disjunct) T)
	(setq *LEFT-DISJ-LIST* (car left-list)
	      *BIND-VARS-DISJ* (car bind-list)
	      *OLDBETA* (car old-beta))
	(loop	    ;; Compile another disjunct until a positive one is reached
	  (if (not (caar disjunct)) (return T))
	  (fill *NODETYPE* 0)
	  (turn-on *NODETYPE* *ORBIT*)
	  (setq *RIGHT-DISJ-LIST* NIL)
	  (let ((vars (compile-pos-or-neg
		       (caar disjunct) pname *NODETYPE* prod)))
	    (push *OLDBETA* (car backpointer))
	    (setq oldoldbeta T)
	    (if (and *BUILDING* *NEWNODE*) (push *NEWNODE* *NEWNODES*))
	    (when (not (realabs *NODETYPE*))
	      (update-left-disj-tests)
	      (update-tests vars oldoldbeta)
	      (update-bindvars oldoldbeta)	;; Update disj bindvars later
	      (setq pos-node T)
	      (return T))			;; From inner 'loop'.
	    (setf (car disjunct) (cdr (car disjunct)))))
	(setf (car left-list) *LEFT-DISJ-LIST*)
	(setf (car bind-list) *BIND-VARS-DISJ*)
	(setf (car old-beta) *OLDBETA*))
      (if pos-node (bump-prev-vars prev-vars))
      (do ((disjunct disjuncts (cdr disjunct)))
	  ((null disjunct) T)
	(setf (car disjunct) (cdr (car disjunct))))
;; It should really be the case that all disjuncts become null at the same
;; time.  Otherwise, if there are CE's after the disjunct then the access
;; for disjunct variables will not be correct for every disjunct.
      (when (every #'null disjuncts)
	(setq *IN-DISJUNCT* NIL)
	(do ((backpointer backpointers (cdr backpointer)))	;; For 'whynot'
	    ((null backpointer)			      ;; and 'matches' modules.
	     (push backpointers (get pname 'backpointers)))
	  (setf (car backpointer)
		(nreverse (car backpointer))))
	(setq *OLDBETA* old-betas)
	(dolist (top-disj (rete-node-right-output
			   (car (disj-nodes-parent (rk-rule-disj-nodes prod)))))
	  (turn-on (rete-node-type top-disj) *TOP-DISJ-BIT*))
	(push *OLDBETA* (disj-nodes-bottom (rk-rule-disj-nodes prod)))
	(update-disj-bindvars bind-access-lists oldoldbeta)
	(decf *NUM-ORS*)
	(return (transfer-new-left-tests	;; From outer 'do' loop.
		 left-access-lists
		 (rete-node-right-output
		  (car (disj-nodes-parent (rk-rule-disj-nodes prod))))))))))
;; Car of Disj-nodes-bottom is set here to *OLDBETA*, which is the list of
;; bottom nodes.  If there is a node under this one, then it will be set to
;; that; otherwise, it will be this list.

(defun compile-pos-or-neg (ce pname nodetype prod)
  (cond ((abs-cmd-p (car ce))
	 (compile-neg-node (cadr ce) pname prod :disjunctive NIL))
	(T
	 (if (or-cmd-p (car ce))
	     (ml-cerror :ignore-conde :not-nested-or)
	     (compile-one ce pname nodetype prod :disjunctive NIL)))))


;;; New algorithm for variables which would solve many variable-expression
;;; problems and not require any BIND's:
;;; When done compiling a set of disjuncts, examine all of the variables used
;;; in those disjuncts.  For each variable whose access is not the same for
;;; all disjuncts (i.e., if it has different accesses in different disjuncts,
;;; or doesn't have an access in some disjuncts), make its left access be
;;; dependent on which disjunct the token came from.  The tokens always know
;;; which disjunct they came from since they are marked after disjunctive
;;; nodes. For example, if a variable had the value (exp-1) in disj-1 and
;;; (exp-2) in disj-2, then its value would be:
;;; (if (eq (marked-token-top-disj token) <disj-node-1>) (exp-1) (exp-2)).
;;; The exp-plist is a plist of varnames associated with a list of:
;;; (node . expression), the expression being the value of the var if the
;;; node is the top disjunctive node that the token came through.  If there
;;; is an expression for every disjunct and it's the same, then that can be
;;; reduced to just that expression.  Otherwise, the expression for the var
;;; will be turned into a conditional.
(defun transfer-new-left-tests (left-access-lists top-nodes)
  (let ((exp-plist NIL)		;; Plist of var & multiple expressions
	(num-disjuncts (length top-nodes))
	(vars NIL))
    (do ((top-node top-nodes (cdr top-node))
	 (left-accesses left-access-lists (cdr left-accesses)))
	((null left-accesses)
	 (form-permanent-access exp-plist num-disjuncts)
	 vars)
      (do* ((left-access (car left-accesses) (cddr left-access))
	    (final-exp (getf exp-plist (car left-access))
		       (getf exp-plist (car left-access))))
	   ((null left-access) T)
	(cond (final-exp (nconc final-exp (list (cons (car top-node)
						      (cadr left-access)))))
	      (T
	       (push (car left-access) vars)
	       (setf (getf exp-plist (car left-access))
		     (list (cons (car top-node) (cadr left-access))))))))))

;;; Forms (possibly conditional) expressions for each of the variables in
;;; exp-plist.
(defun form-permanent-access (exp-plist num-disjs)
  (do ((var-exp exp-plist (cddr var-exp)))
      ((null var-exp) T)
    (record-old-left-test
     (car var-exp)
     (make-conditional-expression (car var-exp) (cadr var-exp) num-disjs))))

(defun make-conditional-expression (var exps num-disjuncts)
  (cond ((and (= (length exps) num-disjuncts)
	      (all-expressions-the-same exps))
	 (cdr (car exps)))
	(T
	 (let ((default (get-old-left-test var)))
	   (append '(cond)
		   (mapcar
		    #'(lambda (disj)
			`((eq (nth ,*NUM-ORS* (marked-token-top-disj left))
			      ',(car disj))
			  ,(cdr disj)))
		    exps)
		   (if (and default (< (length exps) num-disjuncts))
		       `((T ,default))))))))

;;; Returns T iff all the expressions in exps are the same.
(defun all-expressions-the-same (exps)
  (do ((exp exps next-exp)
       (next-exp (cdr exps) (cdr next-exp)))
      ((null next-exp) T)
    (if (not (equal (cdar exp) (cdar next-exp)))
	(return NIL))))


;;; After done compiling the tests in the disjuncts, transfer the access
;;; for all the variables in those disjuncts to the permanent hash table
;;; access.  This needs to be improved.  If the same variable in different
;;; disjuncts has a different expression, then we need to make a BIND so
;;; that its value is stored in the same place and we can use the same
;;; expression for its value no matter which disjunct was true.
;;; If there is already a permanent left access for a disjunctive var, then
;;; we don't change its access to the one in the disjunct since the rule
;;; can check for that var after the disjunction, and some disjuncts may
;;; not bind that var.  Thus, we usually think of disjunct variables as
;;; temporary.  However, the user should be allowed to access these variables
;;; if the proper disjunct was true.
;;; This routine returns the new variables in the disjuncts which will be
;;; referred to after the disjunct.
#|
(defun old-transfer-new-left-tests (left-access-lists)
  (let ((used-vars NIL))
    (dolist (left-access-list left-access-lists used-vars)
      (do ((left-plist left-access-list (cddr left-plist)))
	  ((null left-plist) T)
	(when (not (get-old-left-test (car left-plist)))
	  (record-old-left-test (car left-plist) (cadr left-plist))
	  (push (car left-plist) used-vars))))))|#

;;; Bump the left access of each of VARS by splicing in a cdr.
(defun bump-prev-vars (vars)
  (dolist (var vars)
    (let ((left-test (get-old-left-test var)))
      (if left-test
	  (record-old-left-test var (splice-in-cdr left-test))))))

(defun clear-old-and-return-new-right-vars (right-vars)
  (let ((new-r-vars NIL))
    (dolist (var right-vars (nreverse new-r-vars))
      (cond ((get-left-test var)
	     (clear-right-test var))
	    (T (push var new-r-vars))))))

(defun compile-neg-node (conde pname prod disj next-ce-or)
  (turn-on *NODETYPE* *NOTBIT*)		;; A <NOT> node
  (compile-one conde pname *NODETYPE* prod disj next-ce-or)
  NIL)


(defun handle-negated-conj (conde allvars pname prod)
  (declare (ignore conde allvars prod))
  (ml-cerror :ignore-negation :not-negated-conjunctions pname))

(defun realabs (abs)
  (not (zerop (sbit abs *NOTBIT*))))

(defun realabs-node (node)
  (not (zerop (sbit (rete-node-type node) *NOTBIT*))))

(defun anyabs (abs)
  (not (zerop (sbit abs *NOTBIT*))))
;;  (or (eq abs 'NOT)
;;      (eq abs 'NOTBIND)
;;      (eq abs 'PRENOT))

;;; Compiles one condition element.  Returns the list of variables used in that
;;; condition.  Note that this can be called with an absence test (with the ABS
;;; stripped off) and the cond-vars returned can just be blown off.
;;; Each condition has an alpha node, a beta node, and either a Tnode or an
;;; N-node, depending on whether the class test is an "any" test.
(defun compile-one (conde pname nodetype prod disj-node next-or)
  (setq *NEWNODE* NIL *NUM-BETAS* 0 *NUM-ALPHAS* 0 *BETA-TEST* NIL
	*CHECKVARS* NIL *ALPHA-LISP-TEST* NIL
	*NODETYPE* nodetype)	;;possibly changed by make-slot-tests
  (let* ((t-node (get-t-or-n-node (car conde)))
	 (class (find-wme-class (car conde)))
	 (cond-vars (make-slot-tests class (cdr conde) *OLDBETA*))
	 (newand NIL)
	 (slots (slots-in (cdr conde)))
	 (slot-test (make-final-alpha-test))
	 (alpha-node (make-n-link-or-find-alpha slot-test t-node slots next-or)))
    (check-slots-legality slots class)
    (cond (*OLDBETA*	   ;;beta nodes - a beta must have already been made
	   (setq newand (make-or-find-beta (construct-beta-test) *OLDBETA*
					   alpha-node *NODETYPE*))
	   (when (consp *OLDBETA*)	    ;; If previous node was disjunctive
	     (setf (car (disj-nodes-bottom (rk-rule-disj-nodes prod))) newand)
	     (if (not (realabs nodetype))	 ;; Tag last beta as not shared
		 (setf (rete-node-left-mem newand) :NOT-SHARED)))
	   (if (realabs nodetype) (clear-rights cond-vars))
	   (setq *OLDBETA* newand))
	  (T				    ;;Else it's the first alpha.
	   (setq *OLDBETA* alpha-node)))
    (if (not disj-node)
	(push *OLDBETA* (get pname 'backpointers)))	;;For the WHYNOT module
    (incf (car (rk-rule-num-tests prod))
	  (+ *NUM-BETAS* *NUM-ALPHAS*))
    (incf (cdr (rk-rule-num-tests prod))
	  (length *CHECKVARS*))		    ;;For specificity module (21-Sep-86)
    cond-vars))

(defun find-wme-class (carconde)
  (let ((class
	 (if (atom carconde) carconde
	     (cadr carconde))))
    (check-class class)
    class))

(defun check-class (class)
  (cond ((not (classp class))
	 (ml-cerror :keep-compiling :not-a-class class))))

(defun check-slots-legality (slots class)
  (let ((slots-of-class (get-slot-names class)))
    (dolist (slot slots)
      (check-slot-legality slot slots-of-class class))))

(defun check-slot-legality (slot slots-of-class class)
  (when (not (memq slot slots-of-class))
    (if (memq slot '(:LABEL :CHECK))
	(ml-format T :obsolete slot)
	(ml-format T :slot-not-defined slot class))))

;;; Used by Pat Cheng's learning module and by check-slots-legality.
;;; Returns a list containing the name of every slot in conds.
(defun slots-in (conds)
  (let (res cond)
    (loop
     (setq cond (car conds))
     (cond ((listp cond)
	    (setq conds (cdr conds)))
	   ((eq cond *LEFT-BRACKET*)
	    (if (atom (cadr conds))
		(push (assure-keyword (cadr conds)) res))
	    (setq conds (cdddr conds))
	    (if (eq (car conds) *RIGHT-BRACKET*)
		(setq conds (cddr conds))
		(setq conds (cdr conds))))
	   (T
	    (push (assure-keyword cond) res)
	    (setq conds (cddr conds))))
     (if (null conds) (return res)))))

;;; Used only by make-slot-tests.
(eval-when (load eval compile)
  (defmacro process-value (value facet make-exp)
    `(cond ((rk-anyp ,value)
	    (setq varname (cadr ,value))
	    (process-lhs-var varname ,make-exp)
	    (push (subst-vars `(isa-instance (var ,varname) ',varname))
		  *ALPHA-LISP-TEST*))
	   ((rk-anysubp ,value)
	    (setq varname (cadr ,value))
	    (process-lhs-var varname ,make-exp)
	    (push (subst-vars `(isa-or-instance (var ,varname) ',varname))
		  *ALPHA-LISP-TEST*))
	   ((rk-variablep ,value)
	    (setq varname (cadr ,value))
	    (process-lhs-var varname ,make-exp))
	   ((beta-position-p ,value)
	    (if (not *OLDBETA*)
		(ml-cerror :ignore-lisp-var :cant-put ,value)
		(add-beta-test (list 'equal ,make-exp ,value))))
	   (T
	    (incf *NUM-ALPHAS*)
	    (push (make-alpha-test class slot ,facet ,value)
		  *ALPHA-LISP-TEST*)))))

;;was pushed on constant-tests

(defun make-dyn-slot-bind (value-exp varname betap)
  (let ((v1 (gentemp "VAL")) (v2 (gentemp "VAL")))
    (add-beta-test
     `(multiple-value-bind (,v1 ,v2) ,value-exp
	(and ,v2 ,(make-var-binding v1 betap varname))))
    (process-bind-var-test varname betap)))


(defmacro add-bind-vars (varnames)
  `(if *IN-DISJUNCT*
	(setq *BIND-VARS-DISJ* (nconc *BIND-VARS-DISJ* ,varnames))
	(setq *BIND-VARS* (nconc *BIND-VARS* ,varnames))))

(defmacro add-bind-var (varname)
  `(if *IN-DISJUNCT*
       (push ,varname *BIND-VARS-DISJ*)
       (push ,varname *BIND-VARS*)))

;;; Input invariant: no variable should have a right-test associated with
;;; it.  rete-var-to-right-test should be clear.
;;; Makes the appropriate alpha tests and puts them into one big conjunction
;;; in the given alpha node, then returns the list of variable names
;;; associated with the beta tests.
;;; Modified 21-Feb. 1986: now returns (test . vars) instead of just vars.
;;; Doesn't take a node as an argument, so doesn't change the node.
;;; Modified 11-Mar-86: the test now includes the lisp test iff betap is false.
;;; Added 14-Feb-87: facet is NIL if not specified, and indicates to lower-
;;; level testify functions to use value if the slot is faceted; else just
;;; access the slot instead of a facet in the slot.
(defun make-slot-tests (class conds betap)    ;;conds = 1 lhs test
  (let (slot value carconds varname vars)
    (loop
      (if (null conds)
	  (return vars))
      (setq carconds (car conds))
      (cond ((listp carconds)		;;special directive like label or check
	     (setq slot (car carconds)
		   value (cadr carconds)
		   conds (cdr conds))
	     (cond ((check-cmd-p slot)
		    (let ((newtest (subst-vars value)))
		      (if *CHECKS-BETAS*
			  (add-beta-test newtest)
			  (push newtest *ALPHA-LISP-TEST*))))
		   ((label-cmd-p slot)
		    (setq varname (cadr value))
		    (maybe-push-var varname vars)
		    (tag-var varname 'eq)
		    (process-lhs-var varname (make-right-conde-access)))
		   ((bind-cmd-p slot)
		    (turn-on *NODETYPE* *BINDBIT*)
		    (setq varname (cadr value))
		    (when (check-bind-var-legality varname)
		      (add-beta-test
		       (make-var-binding (subst-vars (caddr carconds)) betap
					 varname))
		      (process-bind-var-test varname betap)
		      (add-bind-var varname)))
		   ((mbind-cmd-p slot)
		    (turn-on *NODETYPE* *BINDBIT*)
		    (let ((varnames (mapcar #'cadr value)))
		      (when (check-mbind-var-legality varnames)
			(add-beta-test
			 (make-mvar-bindings
			  (length varnames) (subst-vars (caddr carconds))
			  betap varnames))
			(process-mbind-var-tests varnames betap) 
			(add-bind-vars varnames))))
		   (T (ml-cerror :ignore-command :illegal-special-cmd
				 carconds slot))))
	    ((eq carconds *LEFT-BRACKET*)	;; Dynamic slot specification
	     (setq conds (cdr conds))
	     (setq slot (car conds))
	     (setq conds (cdr conds))
	     (let* ((facet
		     (if (eq (car conds) *RIGHT-BRACKET*) NIL
			 (prog1 (car conds)
			   (setq conds (cdr conds)))))
		    (val (cadr conds)))
	       (if (not (eq (car conds) *RIGHT-BRACKET*))
		   (ml-cerror :keep-compiling :expected-closing (car conds)))
	       (setq conds (cddr conds))
	       (cond ((and (symbolp slot) (symbolp facet))
		      (process-value
		       val facet (make-right-slot-access2 class slot facet))
		      (when varname
			(maybe-push-var varname vars)))
		     (T
		      (let* ((value-exp
			      (make-dynamic-access slot facet))
			     (varname (if (listp val) (cadr val)))
			     (test-val (if (or (symbolp val) (numberp val)) val
					   (and varname (get-access2 varname)))))
			(cond ((or test-val (null varname))
			       (incf *NUM-BETAS*)
			       (add-beta-test
				(make-equality-test value-exp test-val)))
			      ((check-bind-var-legality varname)
			       (make-dyn-slot-bind value-exp varname betap)
			       (add-bind-var varname))))))))
	    (T
	     (setq slot carconds
		   value (cadr conds)
		   conds (cddr conds)
		   varname NIL)
	     (process-value value NIL (make-right-slot-access class slot))
	     (when varname
	       (maybe-push-var varname vars)))))))


;;; Return T iff it's legal or user decides to continue.
(defun check-bind-var-legality (varname)
  (cond ((null *BIND-ACCESS*)
	 (ml-cerror :ignore-bind :only-16-bind varname)
	 NIL)
	((bind-get-access2 varname)
	 (ml-cerror :ignore-bind :illegal-bind varname)
	 NIL)
	((or (memq varname *BIND-VARS*)
	     (memq varname *BIND-VARS-DISJ*))
	 (ml-cerror :ignore-bind :illegal-bind varname)
	 NIL)
	(T T)))

(defun check-mbind-var-legality (varnames)
  (if (every #'check-bind-var-legality varnames)
    (if (> (+ *NUM-BINDS* (length varnames)) 15)
	(ml-cerror :ignore-mbind :only-16-bind varnames)
	T)
      T))


(defun link-in-and (beta newand alpha)
  (right-link-in beta newand)
  (left-link-in alpha newand))

;;; Added 2-Sep-86.  If the carconde is just a class name, then get a
;;; t-node, else if it's of the form: (list 'any classname), then get
;;; an N-node and make the appropriate structures.
(defun get-t-or-n-node (carconde)
  (if (atom carconde)
      (make-n-link-or-find-tnode carconde)
      (make-n-link-or-find-nnode (cadr carconde))))

(defun make-n-link-or-find-tnode (class)
  (let* ((test (make-class-test class))
	 (findnode (gethash class *RETE-TEST-HASH*)))
    (cond (findnode
	   findnode)
	  (T
	   (let ((tnode
		  (setf (gethash class *RETE-TEST-HASH*)
			(make-rete-node
			 :left-input *TOP-RETE-NODE*
			 :test test
			 :type *TNODE-TYPE*
			 :output-mem (make-shared :contents NIL)))))
	     (add-right-output *TOP-RETE-NODE* tnode)
	     (setq *NEWNODE* tnode))))))

(defun make-n-link-or-find-nnode (class)
  (let* ((test (make-class-test class))
	 (findnode (gethash class *RETE-N-NODE-HASH*)))
    (cond ((and findnode (eq class (rete-node-test findnode)))
	   findnode)
	  (T
	   (let* ((nnode
		   (setf (gethash class *RETE-N-NODE-HASH*)
			 (make-rete-node
			  :left-input *TOP-RETE-NODE*
			  :left-output findnode
			  :test test
			  :type *NNODE-TYPE*
			  :output-mem (make-shared :contents NIL)))))
	     (dolist (subclass (inverse-isas class))
	       (revise-nnode-pointers findnode nnode subclass))
	     (add-right-output *TOP-RETE-NODE* nnode)
	     (setq *NEWNODE* nnode))))))

(defun revise-nnode-pointers (oldnode nnode class)
  (let ((classnode (gethash class *RETE-N-NODE-HASH*)))
    (cond ((or (null classnode) (eq classnode oldnode))
	   (setf (gethash class *RETE-N-NODE-HASH*) nnode)
	   (dolist (subclass (inverse-isas class))
	     (revise-nnode-pointers oldnode nnode subclass)))
	  (T (if (not (eq nnode classnode))
		 (setf (rete-node-left-output classnode) nnode))))))


;;; To comply with ClTl-2, all of the tests are now function objects
;;; instead of (lambda ...) forms as before.  They get stored in the
;;; :function slot of the rete node, whereas the lambda form remains
;;; in the test slot, since we still need to access the original lambda
;;; form while building new rules.

;;; Alpha node sharing.  If there is already an alpha node coming out of
;;; tnode with the same test as arg. test, then return that alpha node; else
;;; make a new one and link it in to the tnode.
(defun make-n-link-or-find-alpha (test tnode slots next-ce-or)
  (let ((find-alpha
	 (if (not (or *IN-DISJUNCT* next-ce-or))
	     (car (member test (rete-node-right-output tnode)
			  :test #'(lambda (test anode)
				    (and
				     (equal test (rete-node-test anode))
				     (equal (rete-node-type anode)
					    *ALPHA-NODETYPE*))))))))
    (cond (find-alpha
	   (if *TRACE-SHARE*
	       (ml-format T :sharing-alpha-test test))
	   find-alpha)
	  (T
	   (let ((new-alpha (make-template-alpha)))
	     (right-link-in tnode new-alpha)
	     (setf (rete-node-test new-alpha) test)
	     (if (functionp test)
		(setf (rete-node-function new-alpha) test)
		(setf (rete-node-function new-alpha) (eval `(function ,test))))
	     (setf (rete-node-slots new-alpha)
		   (nunion (copy-list slots) (rete-node-slots new-alpha)))
	     (if *BUILDING*
		 (if (and (not *NEWNODE*)
			  (not (memq tnode *NEWNODES*)))
		     (setq *NEWNODE* new-alpha)))
	     new-alpha)))))

(defun make-or-find-beta (test oldbeta alpha-node mytype)
  (let* ((find-beta
	  (if
	   (not (consp oldbeta))	;; We don't share <OR> nodes right now
	   (car (member test (rete-node-right-output oldbeta)
			:test #'(lambda (test bnode)
				  (and (eq (rete-node-right-input bnode)
					   alpha-node)
				       (equal (rete-node-type bnode) mytype)
				       (equal test (rete-node-test bnode)))))))))
    (cond (find-beta
	   (if *TRACE-SHARE*
	       (ml-format T :sharing-beta-test test))
	   find-beta)
	  (T
	   (let ((new-beta (make-template-beta mytype)))
	     (setf (rete-node-test new-beta) test)
	     (if (functionp test)
		 (setf (rete-node-function new-beta) test)
		 (setf (rete-node-function new-beta) (eval `(function ,test))))
	     (link-in-and oldbeta new-beta alpha-node)
	     (if *BUILDING* (if (not *NEWNODE*) (setq *NEWNODE* new-beta)))
	     new-beta)))))


;;; Added 5-Feb-87 for dynamic slot spec tests, which may have constants
;;; in their slot specs, in which case they are quoted and not warned
;;; against.
(defun dyn-subst-vars (conde)
  (if (symbolp conde)
      `',conde
      (subst-vars conde)))

;;; For lisp tests.  Tree substitution of (VAR <var>) with the access function
;;; for <var>.  Added 4-Feb-87: save the new vars tested in the code in
;;; *CHECKVARS*.
(defun subst-vars (conde)
  (setq *rk-varp* NIL)	;;*rk-varp* T after subst-vars0 iff there is a FRulekit
  		  	;;variable in the conde.
  (setq *CVARS* NIL)	;;Check varnames.
  (setq *CHECKS-BETAS* NIL)	;; T iff the test checks any beta vars.
  (let ((new-conde (subst-vars0 conde)))
    (if (and (not *rk-varp*)
	     (not *DONT-WARN-LISP-CHECKS*))
      (rk-format T :lisp-check-any-variables conde))
    (setq *CHECKVARS* (nunion *CHECKVARS* *CVARS*))
    new-conde))

;;; Does lots of nasty consing.
(defun subst-vars0 (conde)
  (cond ((atom conde)
	 (if (beta-position-p conde)
	     (setq *CHECKS-BETAS* T))
	 conde)
	((rk-variablep conde)
	 (setq *rk-varp* T)
	 (pushnew (cadr conde) *CVARS*)
	 (get-access (cadr conde)))
	(T (cons (subst-vars0 (car conde))
	     	 (subst-vars0 (cdr conde))))))

(defun get-access (varname)
  (let ((val (get-access2 varname)))
    (if val val
	(and (ml-cerror :keep-compiling :var-no-binding-1 varname) NIL))))

(defun bind-get-access2 (varname)
  (if *IN-DISJUNCT*
      (getf *LEFT-DISJ-LIST* varname)
      (get-left-test varname)))

;;; This is wrong.
(defun dyn-var-get-access2 (varname)
  (if *IN-DISJUNCT*
      (or (getf *LEFT-DISJ-LIST* varname)
	  (getf *RIGHT-DISJ-LIST* varname))
      (get-access2 varname)))

(defun get-access2 (varname)
  (or (car (get-right-test varname))
      (progn
	(setq *CHECKS-BETAS* T)
	(get-left-test varname))))

(defun add-right-output (node onode)
  (my-nconc (rete-node-right-output node) (list onode)))


;;;; Tests

;;; Modified 14-Mar-1988 to correctly order tests.
(defun process-lhs-var (varname r-test)
  (let ((access (get-right-test varname))
	(pred (or (get-tag varname) 'equal)))
    (cond (access		;; make intra-CE consistency test
	   (incf *NUM-ALPHAS*)
	   (push `(,pred ,(car access) ,r-test) *ALPHA-LISP-TEST*))
	  (T
	   (setq access (get-left-test varname))
	   (when access
	     (incf *NUM-BETAS*)
	     (add-beta-test `(,pred ,access ,r-test))))))
  (record-right-test-of-var varname r-test))

;;; Modified 14-Mar-88.
(defun make-final-beta-test ()
  (if *BETA-TEST*
      `(lambda (left right)
	 ,(cond ((cdr *BETA-TEST*)
		 (cons 'and *BETA-TEST*))
		(T (car *BETA-TEST*))))
      #'(lambda (x y) (declare (ignore x y)) T)
))

;;; Invariant: there is some var in the vars list that has both a right and
;;; left node.
;;; For each var that has both a left and right node, add the equality test
;;; to the list of tests for this beta node.  Then delete the right-node
;;; pointer and make the left node pointer be the (car of) old right node
;;; pointer.  Add a cdr to every test of the left node which doesn't have
;;; a right node.
(defun construct-beta-test ()
  (setq *BETA-TEST* (nreverse *BETA-TEST*))
  (make-final-beta-test))

(defun make-simple-beta-test (vars)
  (do* ((vars vars (cdr vars))
	(var (car vars) (car vars))
	(right-access (get-right-test var) (get-right-test var))
	(left-access (get-left-test var) (get-left-test var))
	(pred (get-tag var) (get-tag var))
	(tests (and right-access left-access
		    (list (make-and-test left-access right-access pred)))
	       (if (and right-access left-access)
		   (cons (make-and-test left-access right-access pred)
			 tests)
		   tests)))
       ((null vars)
	tests)))

(defun make-final-alpha-test ()
  (make-final-alpha-test0
   (nreverse *ALPHA-LISP-TEST*)))

(defvar *ALPHA-TRUE-TEST*
  #'(lambda (x) (declare (ignore x)) T))

(defun make-final-alpha-test0 (tests)
  (if tests
      `(lambda (right)
	 ,(cond ((cdr tests)
		 (cons 'and tests))
		(tests (car tests))  ;;don't need
		(T T)))
      *ALPHA-TRUE-TEST*))
;;      #'identity

;;; Assumes at least one right-access and possibly no left accesses.
(defun make-and-test (left-access right-access pred)
  (let ((predicate (or pred 'equal)))
    (and left-access
	 `(,predicate ,(car right-access) ,left-access))))

;;; Added 25-Sep-86.  Intra tests are seperated from 'AND' tests since they
;;; can go on the alpha nodes.
;;; Multiple right-accesses are for intra-element testing.
(defun make-intra-tests (vars)
  (mapcan #'(lambda (var)
	      (let* ((right-access (get-right-test var))
		     (car-right (car right-access)))
		(mapcar #'(lambda (acc)
			    `(equal ,car-right ,acc))
			(cdr right-access))))
	  vars))


;;;; BIND commands

;;; Binding stuff added 29-Jan-87.  Supports the BIND LHS command.
;;; Returns <var-bind-access>
;;; Have to change this so that, if the var is in a disjunct and that var
;;; has been BIND'ed by a sibling disjunct, then make this var have the
;;; same access as the original one.  Need to pass in varname.
(defun make-var-binding (test betap varname)
  (if (or (eq *BIND-ACCESS* *AREF-FN-NAMES*)
	  (and *FIRST-BIND* (not *BIND-VARS-DISJ*)))
      (list 'progn
	    (make-initial-bind-test-of-conde betap)
	    (make-bind-for-var test betap varname) T)
      (list 'progn
	    (make-bind-for-var test betap varname) T)))

;;; MBIND implemented 25-Feb-87.
(defun make-mvar-bindings (num-binds test betap varnames)
  (if (or (eq *BIND-ACCESS* *AREF-FN-NAMES*)
	  (and *FIRST-BIND* (not *BIND-VARS-DISJ*)))
      (list 'progn
	    (make-initial-bind-test-of-conde betap)
	    (make-bind-for-mvars num-binds test betap varnames) T)
      (list 'progn
	    (make-bind-for-mvars num-binds test betap varnames) T)))

;;; Ensures that it returns T since it's in a test of a rete node.
;;; Took out (if (not (token-bindval ,(bind-xlate betap))) ...)
;;; since now (most) beta tests don't re-eval the test when deleting.
(defun make-initial-bind-test-of-conde (betap)
  `(setf (token-bindval ,(bind-xlate betap)) ,*BIND-COUNT*))

;;; Access is the access fn for getting the val of the bound var.
;;; Makes it much more efficient to check to make sure that the value
;;; hasn't already been assigned, since when removing tokens, it executes
;;; the test again.
;;; took out:  (if (not (,access (token-bindval ,(bind-xlate betap)))) ...)
(defun make-bind-for-var (code betap varname)
  (let ((bind-num (or (get-bind-num-for-var varname)
		      (prog1	;;??????
			(incf *NUM-BINDS*)
			(setq *BIND-ACCESS* (cdr *BIND-ACCESS*))
			(store-bind-num-for-var varname *NUM-BINDS*)))))
    `(setf (svref (token-bindval ,(bind-xlate betap)) ,bind-num) ,code)))

(defun make-bind-for-mvars (num-binds code betap varnames)
  (let* (lvars-res
	 setfs-res
	 (lvars (dotimes (i num-binds (nreverse lvars-res))
		  (push (gentemp "VAR") lvars-res)))
	 (setfs (do ((var lvars (cdr var))
		     (varname varnames (cdr varnames)))
		    ((null var) setfs-res)
		  (push (make-bind-for-var (car var) betap (car varname))
			setfs-res))))
    `(multiple-value-bind ,lvars ,code ,.setfs)))

;;; Associates test with var in the global var table and in the disjunctive one.
(defun record-global-and-disj-left-test-of-var (var test)
  (setf (getf *LEFT-DISJ-LIST* var) test))

(defun record-or-find-left-bind-access (varname)
  (or (get-left-test varname)		;; In the case of a bind in a disj. CE
      (progn
	(record-left-test-of-var
	 varname (or (get-old-bind-access varname 'left)
		     (progn
		       (store-bind-num-for-var varname *NUM-BINDS*)
		       (make-left-bind-access)))))))

(defun record-or-find-right-bind-access (varname)
  (or (get-right-test varname)
      (progn
	(record-right-test-of-var
	 varname (or (get-old-bind-access varname 'right)
		     (progn
		       (store-bind-num-for-var varname *NUM-BINDS*)
		       (make-right-bind-access)))))))

(defun get-old-bind-access (varname side)
  (let ((bind-num (get-bind-num-for-var varname)))
    (when bind-num
      (push varname *BIND-VARS-DISJ*)
      `(,(nth bind-num *AREF-FN-NAMES*) (token-bindval ,side)))))

;;; Want left bind accesses whenever betap is true since bodies of BIND
;;; commands may reference leftward nodes (vars bound in previous CEs).
(defun make-left-bind-access ()
  (prog1
   `(,(car *BIND-ACCESS*) (token-bindval left))
   (setq *BIND-ACCESS* (cdr *BIND-ACCESS*))))

(defun make-right-bind-access ()
  (prog1
   `(,(car *BIND-ACCESS*) (token-bindval right))
   (setq *BIND-ACCESS* (cdr *BIND-ACCESS*))))

(defun update-left-disj-tests ()
  (do ((left-test (cdr *LEFT-DISJ-LIST*) (cddr left-test)))
      ((null left-test) T)
    (setf (car left-test)
	  (splice-in-cdr (car left-test)))))

;;; Move right tests to left tests, and update left tests by adding a
;;; CDR to their access.
(defun update-tests (vars betap)
  (do* ((vars vars (cdr vars))
	(var (car vars) (car vars))
	(right-test (car (get-right-test var)) (car (get-right-test var)))
	(left-test  (get-left-test var)  (get-left-test var)))
       ((null vars) T)
    (cond (right-test	      ;; This means it's in the most recent alpha node
	   (record-left-test-of-var
	    var (subst 'left 'right (maybe-splice-car-beta right-test betap)))
	   (clear-all-right-tests var))
	  (left-test
	   (record-left-test-of-var var (splice-in-cdr left-test))))))

(defun update-disj-bindvars (bind-access-lists oldoldbeta)
  (let ((bind-vars (reduce #'union bind-access-lists)))
    (dolist (var bind-vars)
      (if (not oldoldbeta)
	  (let ((old-test (getf *LEFT-DISJ-LIST* var)))
	    (record-left-test-of-var var old-test))))))      

;;; Change any/all bindvars that have right access, to have left accesses.
;;; For now this can only happen with the first conde.
(defun update-bindvars (oldbeta)
  (update-bindvars0 *BIND-VARS* oldbeta))

(defun update-bindvars0 (vars oldbeta)
  (dolist (var vars)
    (if (not oldbeta)
	(let ((right-test (get-right-test var)))
	  (record-left-test-of-var var (subst 'left 'right (car right-test)))))
    (clear-all-right-tests var)))


;;; Return the new slot access function gotten by adding the FN to the old
;;; slot access function.  Usually fn is either car or cdr.
(defun splice-in (fn slot-access)
  `(,(car slot-access)
    (,fn ,@(cdr slot-access))))

(defun splice-in-car (slot-access)
  (splice-in 'car slot-access))
;;; (foo (token-contents x)) ==> (foo (car (token-contents x)))

(defun maybe-splice-car-beta (slot-access betap)
  (if betap (maybe-splice-in-car slot-access) slot-access))

(defun maybe-splice-in-car (slot-access)
  (if (and *OLDBETA* (not (eq (car slot-access) 'car)))
      (if (and (listp (cadr slot-access))
	       (eq (caadr slot-access) 'car))
	  slot-access		;; don't splice in redundant car's
      (splice-in-car-cdr 'car slot-access))
      slot-access))

(defun splice-in-cdr (slot-access)
  (splice-in-car-cdr 'cdr slot-access))

(defun next-code-portion (code)
  (if (listp (car code)) (car code) code))

;;; Re-written to be more general: puts <fn> before the occurrence of
;;; 'token-contents in <slot-access>.
(defun splice-in-car-cdr (fn slot-access)
  (if (atom slot-access) slot-access
      (cond ((eq (car slot-access) 'token-contents)
	     (list fn slot-access))
	    (T
	     (cons (splice-in-car-cdr fn (car slot-access))
		   (splice-in-car-cdr fn (cdr slot-access)))))))

;;; (foo (token-contents (car x))) ==> (foo (token-content (car (cdr x))))
;;; (foo (car (token-contents x))) ==> (foo (car (cdr (token-contents x))))


;;;;;;;;;;;;;;;  Bookkeeping functions

;;; If upper is a list of nodes (disjuncts), then link each of them into lower
;;; and tag lower as disjunctive too.
(defun right-link-in (upper lower)
  (cond ((consp upper)
;;	 (turn-on (rete-node-type lower) *ORBIT*)
	 (dolist (one-upper upper)
	   (my-nconc (rete-node-right-output one-upper) (list lower)))
	 (setf (rete-node-left-input lower) upper)
	 (setf (rete-node-left-mem lower)
	       (if (anyabs (rete-node-type lower)) NIL	      ;;NOTs have their
		   (rete-node-output-mem (car upper)))))      ;;own left memory
	(T
	 (right-link-in-one upper lower))))

(defun right-link-in-one (upper lower)
  (my-nconc (rete-node-right-output upper) (list lower))
  (setf (rete-node-left-input lower) upper)
  (setf (rete-node-left-mem lower)
	(if (anyabs (rete-node-type lower)) NIL	   ;;NOTs have their own memory
	    (rete-node-output-mem upper))))	 ;;no longer leaf node for prod

(defun left-link-in (alpha newand)
  (my-nconc (rete-node-left-output alpha) (list newand))
  (setf (rete-node-right-input newand) alpha)
  (setf (rete-node-right-mem newand)
	(rete-node-output-mem alpha)))


;;; It's more efficient to check for equality of the name of the class than
;;; to do a class-p check.
(defun make-class-test (class)
  class)

(defun make-lambda (var fn)
  (eval `(function (lambda (,var)
	   ,fn))))

;;; Make node a production node by storing on its right and left-access
;;; field an alist describing for every variable, how to get the value.
(defun make-it-a-pnode (nodes prod)
  (let ((pname (rk-rule-pname prod))
	(one-node (if (consp nodes) (car nodes) nodes)))
    (if (and (rete-node-pnode-prod one-node)
	     (not (and (eq (car (rete-node-pnode-prod one-node)) prod)
		       (not (cdr (rete-node-pnode-prod one-node))))))
	(rk-format T :isomorphic pname
		   (rk-rule-pname (car (rete-node-pnode-prod one-node)))))
    (if (consp nodes)
	(dolist (node nodes)
	  (pushnew prod (rete-node-pnode-prod node)))
	(pushnew prod (rete-node-pnode-prod nodes)))
    (let ((left-pvars NIL))
      (map-rete-vars #'(lambda (var access-fn)
			 (push (cons var (make-lambda 'left access-fn))
			       left-pvars)))
      (setf (rk-rule-left-access prod) left-pvars))
    (setf (rk-rule-pnode prod) nodes)))


(defun make-right-slot-access (class slot)
  `(,(testify class slot)
    (token-contents right)))
;;; Keep it in list form for now since we will need to twiddle it as it goes
;;; down the net.

;;; Added 4-Feb-87 to support variable slot specification in the LHS.
;;; If the given slot and facet names are static (i.e., constants), then
;;; the value access is simply (<frame-slot-.facet> <wme>); else it is
;;; (get-facet <wme> <slot-expression> <facet-expression>)
(defun make-dynamic-access (slot facet)
  (let ((right-acc '(token-contents right)))
    (if facet
	`(get-instance-facet
	  ,right-acc ,(dyn-subst-vars slot) ,(dyn-subst-vars facet))
	`(get-atomic-value ,(dyn-subst-vars slot) ,right-acc))))
;; get-atomic-value dynamically gets the slot value if the slot in the frame
;; is not faceted, else gets the value facet of specified slot.

(defun make-right-slot-access2 (class slot facet)
  (testify2 class slot facet '(token-contents right)))

(defun make-right-conde-access ()
  '(token-contents right))

(defun make-alpha-test (class slot facet value)
  (make-equality-test (testify2 class slot facet '(token-contents right))
		      value))

(defun make-equality-test (access-exp value)
  (cond
   ((constantp value)
    `(eq ,access-exp ,value))
   ((symbolp value)
    `(eq ,access-exp ',value))
   ((numberp value)
    `(= ,access-exp ,value))
   ((stringp value)
    `(string= ,access-exp ,value))
   ((characterp value)
    `(char= ,access-exp ,value))
   (T
    `(equal ,access-exp ',value))))


;;; Returns a template alpha node (with no test).
(defun make-template-alpha ()
  (let ((node (make-rete-node
	       :type (make-array *TYPE-LENGTH*
				 :element-type 'bit
				 :initial-contents *ALPHA-NODETYPE*)
	       :output-mem (make-shared :contents NIL))))
    (turn-off (rete-node-type node) *NOTBIT*)
    (if (bind-type-p *NODETYPE*)
	(turn-on (rete-node-type node) *BINDBIT*))
    node))

(defun make-template-beta (type)
  (let ((node (make-rete-node
	       :output-mem (make-shared :contents NIL)
	       :type (make-array
		      *TYPE-LENGTH* :element-type 'bit :initial-contents type))))
    (turn-off (rete-node-type node) *ALPHABIT*)
    node))

;; '=' is a read macro which expands into (var . x).
(defun rk-variablep (value)
  (and (listp value)
       (symbolp (car value))
       (string= (symbol-name (car value)) "VAR")))

;;; =!x expands into (ANY x)
(defun rk-anyp (value)
  (and (listp value)
       (symbolp (car value))
       (string= (symbol-name (car value)) "ANY")))

;;; =<x expands into (ANYSUB x)
(defun rk-anysubp (value)
  (and (listp value)
       (symbolp (car value))
       (string= (symbol-name (car value)) "ANYSUB")))

;;; Smash is imported from parmenides.lisp.
;;; (class slot) ==> (class-slot.value) or (class-slot).
(defun testify (class slot)
  (if (facetedp class slot)
      (smash class "-" slot ".VALUE")
      (smash class "-" slot)))

;;; Uses Parmenides fn pa-get-snf-nums.
(defun testify2 (class slot facet0 thing)
  (let* ((facet1 (if facet0 (assure-keyword facet0)))
	 (slot (assure-current slot))
	 (facet (or facet1 (if (facetedp class slot) :value))))
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

;;;;;; Highest node code - for smart-modify in inter.lisp.

;;; Return T iff given node contains the given slot.
(defun node-contains-slot (node slot)
  (memq slot (rete-node-slots node)))

;;; Return the highest alpha node which refers to slot in the given class
(defun highest-alpha-nodes (class slot)
  (getf (get class 'high-node-plist) slot))

(defun add-highest-alpha (class slot node)
  (setf (getf (get class 'high-node-plist) slot)
	(push node (getf (get class 'high-node-plist) slot))))

(defun rtest (rname)
  (rk-rule-left-access (get rname 'prod)))


;;;;  Excise [The excise macro appears at the top of this file]

(defun excise-rule (rule rname &optional (excise-all NIL))
  (if (null rule)
      (rk-format T :no-such-rule rule)
      (let ((pnode (rk-rule-pnode rule)))
	(setq *RULE-NAMES* (delete rname *RULE-NAMES*))
	(delete-instants-of-rule rule)	;;In inter.lisp
	(remprop rname 'prod)
	(remprop rname 'defined)
	(setf (rk-rule-pnode rule) NIL)
	(if (listp pnode)
	    (dolist (one-pnode pnode)
	      (setf (rete-node-pnode-prod one-pnode)
		    (delete rule (rete-node-pnode-prod one-pnode))))
	    (setf (rete-node-pnode-prod pnode)
		  (delete rule (rete-node-pnode-prod pnode))))
	(cond
	 ((not pnode)
	  (rk-format T :rule-not-completely-compiled))
	 ((and (not excise-all)
	       (if (consp pnode)
		   (some #'(lambda (a-pnode)
			     (cdr (rete-node-pnode-prod a-pnode)))
			 pnode)
		   (cdr (rete-node-pnode-prod pnode))))
	  (ml-format T :not-completely-excised))
	 (T
	  (if (consp pnode)
	      (dolist (a-pnode pnode)
		(start-deletion a-pnode))
	      (start-deletion pnode))
	  (rk-format T :excised-rule rname))))))

(defun start-deletion (pnode)
  (let ((left-input (rete-node-left-input pnode))
	(right-input (rete-node-right-input pnode)))
    (if (not (or (rete-node-left-output pnode)
		 (rete-node-right-output pnode)))
	(delete-node pnode))
    (excise-at-node left-input)
    (excise-at-node right-input)))

(defun excise-at-node (nodes)
  (do-list-or-atom (node nodes)
    (when (deletable node)
      (let ((linput (rete-node-left-input node))
	    (rinput (rete-node-right-input node)))
	(delete-node node)
	(excise-at-node linput)
	(excise-at-node rinput)))))

(defun deletable (node)
  (not (or (rete-node-left-output node)
	   (rete-node-right-output node)
	   (pnodep node))))

(defun delete-node (node)
  (do-list-or-atom (left-in (rete-node-left-input node))
    (when (eq left-in *TOP-RETE-NODE*)
      (remhash (rete-node-test node) *RETE-TEST-HASH*)
      (setf (gethash (rete-node-test node) *RETE-N-NODE-HASH*)
	    (rete-node-left-output node)))
    (setf (rete-node-right-output left-in)
	  (delete node (rete-node-right-output left-in))))
  (do-list-or-atom (right-in (rete-node-right-input node))
    (if (eq right-in *TOP-RETE-NODE*)
	(remhash (rete-node-test node) *RETE-TEST-HASH*)
      (setf (gethash (rete-node-test node) *RETE-N-NODE-HASH*)
	    (rete-node-left-output node)))
    (setf (rete-node-left-output right-in)
	  (delete node (rete-node-left-output right-in))))
  (dolist (left-out (rete-node-left-output node))
    (setf (rete-node-right-input left-out) NIL))
  (dolist (right-out (rete-node-right-output node))
    (setf (rete-node-left-input right-out) NIL))
  (if (null (cdr (rete-node-pnode-prod node)))
      (setf (rete-node-pnode-prod node) NIL))
  (setf (rete-node-left-output node) NIL)
  (setf (rete-node-left-input node) NIL)
  (setf (rete-node-right-output node) NIL)
  (setf (rete-node-right-input node) NIL))
