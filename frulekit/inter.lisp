;;; -*- Mode:Lisp; Package:FRulekit -*-

;;; Based on rulekit/inter.slisp of 13-May-1986.  This is the Parmenides
;;; version, started 11-June-1986.  Original started 17-Nov-85.
;;; Copyright (c) 1985, 1988 Peter Shell and Carnegie Mellon University.
;;; To obtain a copy of this software, please contact:
;;;	Peter Shell
;;;	Center for Machine Translation
;;;	Carnegie Mellon University
;;;     5000 Forbes Ave.
;;;	Pittsburgh, PA  15213

;;;; -------------------------------------------------------------------- ;;;
;;;; PACKAGE STUFF							  ;;;
;;;;

(in-package "FRULEKIT" :nicknames '("FRK") :use '("LISP" "PARMENIDES"))
;;; NOTE: the nickname "FR" is purposely not used since it is used for Framekit


;;; User-accessible functions, macros and variables documented in the user's
;;; manual.
;;; Symbols exported by optional modules AGENDA and TRACE are exported from
;;; those modules, but symbols exports from module BUILD are exported from
;;; here since build and inter must both be loaded to run, and it is more
;;; natural to group all the symbols from build and inter in one place.
(export '(
     ;; FUNCTIONS & MACROS
	  Rule Rule* Build-rule Build-rule* Pp-rule Pp-instant Ith-wme
	  Pp-wme Pp-wmes Pp-ith-wme Pp-wm Save-wme Save-ith-wme Save-wm Wm
	  Set-wm Add-to-wm Def-cr-strategy Def-complex-cr-strategy
	  Pr-switches Re-init-trace $Disj $Disj-nums Start Literalize
	  $Make $Fast-make $Make-named $Remove $Remove-keep
	  $Modify $Modify-demons $Modify-in-place
	  Excise Add-frame Whynot Matches D-in-instants Halt Rbreak Cont Run
	  Back Init-rete Clear-net Compile-nodes Compile-rhses Compile-all
	  Inhibit-rules Uninhibit-rules $modify-num VAR <> End-rhs
	  Def-inverse-action Var-in-instant In-Wm-P

     ;; FRAME AND STRUCTURE ACCESS
	  Rk-rule-lhs Rk-rule-rhs  Rk-rule-beliefs Rk-rule-pname
	  Rk-rule-extra-test Rk-rule-num-tests WME
	  Wme-%time Wme-%created Wme-%class
	  Token-contents Token-match-count Token-bindval
	  Instant-token Instant-prod Instant-sortedwmes

     ;; GLOBAL VARIABLES
	  *CR-STRATEGY* *MEA* *LEX* *FULL-CR* narrow-down-mea narrow-down-lex
	  narrow-down-test-class num-tests num-checks narrow-down-wme-class
	  *RULE-NAMES* *RECORD-LEVEL* *MAX-BACK* *PAUSE-EVERY-CYCLE*
	  *CONFLICT-SET* *INSTANT* *TRACE-FIRE* *TRACE-MATCH*
	  *TRACE-UNMATCH* *TRACE-ADD* *TRACE-DELETE* *TRACE-CR*
	  *TRACE-CYCLE* *TRACE-MODIFY* *RULEKIT-TERSE* *REFRACT*
	  *FULL-WMEPRINT* *NUMBER-WMES* *AGENDA-LOADED* *CYCLE*
	  ))

(export '(   	      ;; Programmer's symbols (not documented - some should be)
  	  Rk-rule-pnode Rk-rule-left-access Rk-rule-inscount Rk-rule-break
	  reverse-improper rk-variablep wmep def-beta-var
	  *NON-DELETIONS* *AGENDA* *DONT-WARN-LISP-CHECKS* *NO-LITERALIZE*
	 ))

#+Allegro (import 'excl::putprop)


;;; Module for dynamically interpreting the FRulekit rete net made by
;;; build.lisp.  main functions:
;;; (1) Literalize.
;;; (2) ($make <pattern>) -- makes a wme by pushing it through the net, updating
;;; the conflict set if necessary.
;;; (3) ($remove <wmes>) removes wmes by pushing the negation of it through
;;; the net, updating the conflict set if necessary.
;;; (4) ($modify <wme>).  see frulekit.PS.
;;; Note: build and Parmenides must be loaded before running or compiling this.

(eval-when (compile)
  (if (equal (lisp-implementation-type) "Kyoto Common Lisp on MV")
      (load "/usr/pshell/frulekit/build.o")))

#+LUCID (proclaim '(special lucid::*debugger-bindings*))
#+LUCID (pushnew '(*readtable*) lucid::*debugger-bindings*)

(proclaim '(type list *PRINTED-NODES* *CONFLICT-SET* *SWITCHES*
		      *ADDED-INSTANTS* *REMOVED-INSTANTS* *FIRED-INSTANTS*))

(defvar *INSTANT*)          ;;current instantiation chosen from *CONFLICT-SET*
(defvar *MEA* '(narrow-down-mea narrow-down-lex num-tests num-checks))
(defvar *LEX* (cdr *MEA*))
(defvar *FULL-CR*
  '(narrow-down-mea narrow-down-lex num-tests num-checks narrow-down-wme-class
		    narrow-down-test-class))
(defvar *CR-STRATEGY* *MEA*)    ;;Ordered list of conflict-resolution strategies.
(defvar *TRACE-FIRE* T)         ;;Flag that says to print out what prod is firing
(defvar *TRACE-MATCH* NIL)
(defvar *TRACE-UNMATCH* NIL)
(defvar *TRACE-ADD* T)
(defvar *TRACE-DELETE* T)
(defvar *TRACE-MODIFY* T)
(defvar *TRACE-CR* NIL)		;;Trace conflict resolver.
(defvar *TRACE-NODE-TEST* NIL)
(defvar *TRACE-CYCLE* T)
(defvar *PAUSE-EVERY-CYCLE* NIL)
(defvar *CYCLE* 0)
(defvar *MAX-BACK* 50)		;;Maximum number of cycles to record for back.
(defvar *MODCYCLE* 0)		;;*CYCLE* mod *MAXBACK*
(defvar *INTRA-CYCLE-TIME* 0)
(defvar *CURRENT-RULE-NAME*)    ;;Name of rule that agenda is currently testing
(defvar *SWITCHES*)             ;;List of switch names.
(defvar *NODES-TRAVERSED*)
(defvar *RECORD-LEVEL* 1)	;;0 = none, 1 = for back, 2 = for trace.lisp.
(defvar *CONFLICT-SET*)
(defvar *REFRACT* T)
(defvar *TOP-LEVEL-INSTANT*)	;;Dummy top level instantiation
(defvar *RETE-TEST-HASH*)	;;Build data structure used by find-tnode.
(defvar *RETE-N-NODE-HASH*)	;;Build data structure used by find-nnode.
(defvar *NEWNODES*)		;;From build.lisp; for the build-rule command.
(defvar *FIND-TOKEN* NIL)
(defvar *RESULT*)		;;So FRulekit can return a value.
(defvar *VAR-BINDING-HASH*
  (make-hash-table :size 43))	;;Hash table for var bindings.
(defvar *PRINTING-WME* NIL)	;;Flag for wme printer.
(defvar *PRINTED-NODES*)
(defvar mystream)
(defvar *BACKING-UP* NIL)	;;T while FRulekit is backing up.
(defvar *NUMBER-WMES* NIL)
(defvar *AGENDA-LOADED* NIL)	;;T only when the agenda module is loaded.
(defvar *COMPILING-RHS* NIL)	;;Non-nil only while the RHS is being compiled.
(defvar *ADDED-INSTANTS* NIL)	;;List of instantiations which were added or
(defvar *REMOVED-INSTANTS* NIL) ;;removed and could block or unblock nodes.
(defvar *PERFORMING-BLOCKING* NIL)   ;;Flag for perform-blocking-and-unblocking
(defvar *FIRED-INSTANTS* NIL)	;; List of fired instants, for RMS module.
(defvar *NO-LITERALIZE* NIL)	;; If T then the user doesn't use literalize
				;; for classes (def-frame instead), so we
				;; can't do c-r.

(proclaim '(special *PROD-MATCHES* *PROD-UNMATCHES* *TOKEN-POSITIVE* *AGENDA*))

;;; trace package data structures.
(proclaim '(simple-vector
	    *PROD-MATCHES* *PROD-UNMATCHES* *PROD-FIRINGS* *ADDITIONS*
	    *DELETIONS* *MODIFIES* *REFRACTIONS* *NON-DELETIONS*))

(defvar *PROD-FIRINGS* (make-array *MAX-BACK* :initial-element NIL))
(defvar *ADDITIONS* (make-array *MAX-BACK* :initial-element NIL))
(defvar *DELETIONS* (make-array *MAX-BACK* :initial-element NIL))
(defvar *MODIFIES* (make-array *MAX-BACK* :initial-element NIL))
(defvar *REFRACTIONS* (make-array *MAX-BACK* :initial-element NIL))
(defvar *NON-DELETIONS* (make-array *MAX-BACK* :initial-element NIL))

(defun change-max-back (newvalue)
  (setq *MODIFIES* (make-array newvalue :initial-element nil))
  (setq *REFRACTIONS* (make-array newvalue :initial-element nil))
  (setq *NON-DELETIONS* (make-array newvalue :initial-element nil))
  (setq *PROD-FIRINGS* (make-array newvalue :initial-element nil))
  (setq *PROD-MATCHES* (make-array newvalue :initial-element nil))
  (setq *PROD-UNMATCHES* (make-array newvalue :initial-element nil))
  (setq *ADDITIONS* (make-array newvalue :initial-element nil))
  (setq *DELETIONS* (make-array newvalue :initial-element nil))
  (setq *MAX-BACK* newvalue))


(defvar *TIME-FRULEKIT* NIL)	;; If T, then time FRulekit functions
(defvar *START-MODIFY* 0)	;; For timing the modify function
(defvar *TOTAL-TIME* 0)		;; For accumulating time spent on FRulekit fns.

(defun init-time ()
  (setq *TOTAL-TIME* 0))

(defun report-time ()
  (let ((seconds-taken (/ *TOTAL-TIME* internal-time-units-per-second)))
    (ml-format T :seconds seconds-taken)
    seconds-taken))

(defun pr-switches ()
  (dolist (switch *SWITCHES*)
    (format T "~%~19A ~A" switch (symbol-value switch)))
  T)


;;;; Structures and Frames
(eval-when (compile load eval)
  (defstruct (token (:type vector))
    contents				;; pointer to wme(s) that define it
    (match-count 0 :type integer)  ;; # of right tokens it matches, for negations
    bindval	;; stores user-specified dynamic variable bindings (BIND cmd).
  )

;;; This type of token is used for disjunctive nodes to mark the (top) disjunct
;;; through which the token passed.
  (defstruct (marked-token (:type vector))
    contents				;; pointer to wme(s) that define it
    (match-count 0 :type integer)  ;; # of right tokens it matches, for negations
    bindval	;; stores user-specified dynamic variable bindings (BIND cmd).
    top-disj)
  
;;; Structure for an instantiation
  (defstruct (instant (:print-function instant-printer))
    token         ;;contains the wmes
    prod          ;;the production that made it
    sortedwmes    ;;wmes sorted by creation time.  used in conflict resolution.
  )
)

;;; This copies tokens or marked-tokens.  They are both of length 4 since
;;; marked-tokens are vectors and tokens aren't.
;;; The constant 4 is in this routine because the CMU CLisp compiler writes
;;; MUCH more efficient code when it knows the array lengths.
(defun copy-token-or-marked (a)
  (let* ((length (car (array-dimensions a)))
	 (newv (if (= length 4) (get-free-frame 4) (get-free-frame 3))))
    (dotimes (i length)
      (setf (svref newv i) (svref a i)))
    newv))

(defvar *MARKED-TOKEN* (make-marked-token))

;;; FRulekit working memory elements are frames.  To define a class which
;;; is-a wme, use literalize.  FRulekit doesn't propagate its classes by
;;; default since it's inefficient.
(eval-when (compile)
 (def-frame wme (setable :setf propagate NIL pre-if-set (pre-modify)
			  post-if-set (post-modify) cache :*ALL*)
  %time 0	  ;; time tag, saying when it was created
  %created TOP	  ;; pointer to production that added/deleted it
  %prod-matches NIL
  %prod-firings NIL))
;;; Last two slots for the trace package, which is why this needs to be
;;; eval'ed in the compiler's environment, since some code in this file accesses
;;; the trace slots but only when trace is loaded.

;;; Usually only these 2 slots are needed in a WME.  The WME is re-defined
;;; in trace.lisp to include the other 4 slots.
(eval-when (load eval)
  (def-frame wme (setable :setf propagate NIL pre-if-set (pre-modify)
			   post-if-set (post-modify) cache :*ALL*)
   %time 0		  ;; time tag, saying when it was created
   %created TOP))	  ;; pointer to production that added/deleted it



;;;; Macros
;;; Returns T iff token is "positive" [its tag indicates it should be added].
(eval-when (compile load eval)
  (defmacro beta-test-p (left-mem right-mem node)
    `(funcall (rete-node-function ,node)
	      ,left-mem
	      ,right-mem))
  
  (defmacro fast-del (contents list test)
    `(do ((prevvar NIL itervar)
	  (itervar ,list (cdr itervar)))
	 ((null itervar) ,list)
       (when (,test ,contents (car itervar))
	 (cond ((eq itervar ,list)
		(setf ,list (cdr itervar)))
	       (T (setf (cdr prevvar) (cdr itervar))
		  (setq itervar prevvar))))))
  
;;; What if last test is absence test and deleting a wme causes the rule
;;; to match?  See change-cs.
  (defmacro neg-send-right (token node)
    `(let ((new-token (maybe-new-token ,token ,node)))
       (if (and (pnodep ,node)
		(not-blocked-node-p ,node))
	   (change-cs new-token ,node))
       (send-right new-token ,node NIL)))

;;; Assuming that there is only one left-output (i.e. no pre-abs node sharing).
;;; when a token goes left (comes from the right) into a negation node.
  (defmacro left-beta-push-neg (tokens node)
    `(if *TOKEN-POSITIVE*
	 (neg-add-from-right ,tokens ,node)
	 (neg-del-from-right ,tokens ,node)))

  (defmacro right-beta-push-neg (token node)
    `(if *TOKEN-POSITIVE*
	 (neg-add-from-left ,token ,node)
	 (neg-del-from-left ,token ,node)))

  (defmacro right-beta-push-one (tokens right-mem node)
    `(if (beta-test-p ,tokens ,right-mem ,node)
	 (let ((newtoken (new-token ,right-mem ,tokens ,node)))
	   (if (pnodep ,node)
	       (change-cs newtoken ,node))
	   (send-right newtoken ,node ,tokens))))
  )

;;; A little faster than beta-test-p
(defun new-beta-test-p (left-mem right-mem test)
  (declare (optimize (speed 3) (safety 0)))
  (funcall test
     left-mem
     right-mem))

(defmacro queue-added-instant (instant)
  `(pushnew ,instant *ADDED-INSTANTS*
	    :test #'(lambda (newins ins) (eq (instant-prod newins)
					     (instant-prod ins)))))
(defmacro queue-removed-instant (instant)
  `(pushnew ,instant *REMOVED-INSTANTS*
	    :test #'(lambda (newins ins) (eq (instant-prod newins)
					     (instant-prod ins)))))

(defun num-tokens-of-class (class)
  (let* ((node (or (gethash class *RETE-TEST-HASH*)
		   (gethash class *RETE-N-NODE-HASH*))))
    (length
     (shared-contents (rete-node-output-mem node)))))

(defmacro num-tokens (wme)
  `(num-tokens-of-class (wme-%class ,wme)))

;;; New shared memory data structure added 15-Mar-86.
(defmacro rete-shared-left (node)
  `(shared-contents (rete-node-left-mem ,node)))

(defmacro rete-shared-right (node)
  `(shared-contents (rete-node-right-mem ,node)))

(defmacro rete-shared-output (node)
  `(shared-contents (rete-node-output-mem ,node)))

(defmacro rete-generic-left (node)
  `(if (or (eq (rete-node-left-mem ,node) :NOT-SHARED)
	   (anyabs (rete-node-type ,node)))
       (rete-node-left-mem ,node)
       (shared-contents (rete-node-left-mem ,node))))

;;; Not used anymore.
(defmacro rete-generic-right (node)
  `(if (anyabs (rete-node-type ,node))
       (rete-node-right-mem ,node)
       (shared-contents (rete-node-right-mem ,node))))

(defmacro active-or-blocked (node)
  `(let ((type (rete-node-type ,node)))
     (not (and (zerop (sbit type *BLOCKED-BIT*))
	       (zerop (sbit type *ACTIVE-BIT*))))))

(defmacro do-token-left (node token)
  `(if (not *TOKEN-POSITIVE*)
       (setf (rete-node-left-mem ,node)
	     (fast-del ,token (rete-node-left-mem ,node) eq))))

;;; Push a token which comes from the left, through the given beta node.
;;; Left-mem is the left memory, which is a list of plural tokens.
(defmacro left-beta-push-one (tokens left-mem node)
  `(if (beta-test-p ,left-mem ,tokens ,node)
       (let ((newtoken (new-token ,tokens ,left-mem ,node)))
	 (if (pnodep ,node) (change-cs newtoken ,node))
	 (send-right newtoken ,node ,tokens))))

(defmacro doleft-mem (left-mem node token)
  `(cond ((eq (rete-node-left-mem ,node) :NOT-SHARED)
	  (if (active-or-blocked (car (rete-node-left-input ,node)))
	      (dolist (left-input (rete-node-left-input ,node))
		(when (not (blocked-node-p left-input))
		  (dolist (,left-mem (rete-shared-output left-input))
		    (left-beta-push-one ,token ,left-mem ,node))
		  (return T)))
	      (do ((left-input (rete-node-left-input ,node) (cdr left-input)))
		  ((null left-input) T)
		(when (blocked-node-p (car left-input))
		  (store-partial-mem (rete-node-right-input ,node)
				     ,token left-input)
		  (return T))
		(dolist (,left-mem (rete-shared-output (car left-input)))
		  (left-beta-push-one ,token ,left-mem ,node)))))
	 (T
	  (dolist (,left-mem (rete-shared-left ,node))
	    (left-beta-push-one ,token ,left-mem ,node)))))

(defmacro get-bottom-disj-nodes (prod)
  `(let ((disj-nodes (rk-rule-disj-nodes ,prod)))
     (if disj-nodes (disj-nodes-bottom disj-nodes))))

(defmacro get-one-bottom-disj-node (prod)
  `(let* ((disj-nodes (rk-rule-disj-nodes ,prod))
	  (bottom (if disj-nodes (disj-nodes-bottom disj-nodes))))
     (if (consp bottom) (car bottom) bottom)))

(defmacro some-blocked-bottom-node-p (prod)
  `(let* ((disj-nodes (rk-rule-disj-nodes ,prod))
	  (bottom (if disj-nodes (disj-nodes-bottom disj-nodes))))
     (some #'(lambda (one-bottom)
	       (if (consp one-bottom)
		   (some #'(lambda (node) (blocked-node-p node)) one-bottom)
		   (blocked-node-p one-bottom)))
	   bottom)))

(defmacro get-parent-disj-nodes (prod)
  `(let ((disj-nodes (rk-rule-disj-nodes ,prod)))
     (if disj-nodes (disj-nodes-parent disj-nodes))))

;;; N is origin 1.  parents are in reverse order.
;;; Returns NIL if there is no <n>th parent disjunctive node.
(defmacro get-nth-parent-disj-node (prod n)
  `(let* ((disj-nodes (rk-rule-disj-nodes ,prod))
	  (pos (if disj-nodes (- (length (disj-nodes-parent disj-nodes)) ,n))))
     (if (and pos (> pos -1))
	 (nth pos (disj-nodes-parent disj-nodes)))))

(defmacro get-nth-top-disj (token n)
  `(let ((top-disj-nodes (marked-token-top-disj ,token)))
     (if top-disj-nodes (nth (- (length top-disj-nodes) ,n)
			     top-disj-nodes))))

;;; Top-level way to set wm to a list of wmes w/out starting.  Useful to agenda.
(defmacro set-wm (&rest wmes)
  `(progn
    (clear-net)
    ,@(mapcar #'(lambda (wme)
	       `($make ',(car wme) ,@(cdr wme)))
	       wmes)))

;;; Top-level way to add things to wm without starting.  useful to agenda.
(defmacro add-to-wm (&rest wmes)
  `(progn
    (init-instant)
    ,@(mapcar #'(lambda (wme)
	       `($make ',(car wme) ,@(cdr wme)))
	       wmes)))

;;; Top-level ops command.
;;; Start by adding the given list of wmes.  clears working memory.
;;; FIXUP!
(defmacro start (&rest wmes)
  `(progn
    (clear-net)
    ,@(mapcar #'(lambda (wme)
		  `($make ',(car wme) ,@(cdr wme)))
	      wmes)
    (main-rk-loop 100)))

;;; continue.  just like start except doesn't clear working memory.
(defmacro cont (&rest wmes)
  `(progn
    (init-instant)
    ,@(mapcar #'(lambda (wme)
	       `($make ',(car wme) ,@(cdr wme)))
	      wmes)
    (main-rk-loop 100)))

;;; Added 7-Oct-1987.  Returns T iff the given arg is both a frame instance
;;; and its class is-a WME.
(defun wmep (obj)
  (and (framep obj)
       (isa-p (pa-class-of obj) 'WME)))

;;; Returns T iff the given wme is in Working Memory.  This is tested by
;;; looking in the TNode for the wme's class.
(defun in-wm-p (wme)
  (if (framep wme)
      (let ((tnode (or (find-tnode wme)  (find-nnode wme))))
	(if tnode
	 (member wme (rete-shared-output tnode)
		 :test #'(lambda (wme token)
			   (eq (token-contents token) wme)))))))

;;; Has to be done after (defstruct instant).
(defvar *TOP-LEVEL-INSTANT*
  (make-instant :prod (make-rk-rule :pname 'top-level :inscount 0)))



;;;; MIP MODULE, implemented by Daniel Borrajo

;;; Macros

(eval-when (load eval compile)
  
;;
;; Sometimes it is useful to return the wme instead of T or NIL
  (defmacro find-output-token (wme node)
    (let ((token-name (gensym "T")))
      `(find-if #'(lambda (,token-name)
		    (eq (token-contents ,token-name) ,wme))
		(what-has-output-mem ,node))))
  
  (defmacro find-joined-output-token (left-wmes right-wmes node)
    (let ((token-name (gensym "T")))
      `(find-if #'(lambda (,token-name)
		    (let ((contents (token-contents ,token-name)))
		      (and (eq (cdr contents) ,left-wmes)
			   (eq (car contents) ,right-wmes))))
		(shared-contents (rete-node-output-mem ,node)))))
  
  (defmacro find-output-token-left (input-token node)
    (let ((token-name (gensym "T")))
      `(find-if #'(lambda (,token-name)
		    (eq (token-contents ,input-token)
			(token-contents ,token-name)))
		(what-has-output-mem ,node))))

  (defmacro what-has-output-mem (node)
    `(let ((output-mem (rete-node-output-mem ,node)))
       (if (consp output-mem)
	   output-mem
	   (shared-contents output-mem))))
  
  (defmacro mip-left-beta-push-one (was right-token left-token node)
    `(let ((newtoken (if ,was ,was
			 (new-token ,right-token ,left-token ,node))))
       (if (pnodep ,node) (change-cs newtoken ,node))
       (send-right newtoken ,node NIL)))

  (defmacro make-freelist-frame (class size &rest slots)
    (let ((frame-name (gentemp "F-")))
      `(progn
	 (let ((,frame-name (get-free-frame ,size)))
	   ,@(make-slot-setfs class slots frame-name)
	   ,frame-name))))

  (defun make-slot-setfs (class slots frame-name)
    (do ((slots slots (cddr slots))
	 (res NIL
	      (push
	       `(setf (,(smash class "-" (car slots)) ,frame-name) ,(cadr slots))
	       res)))
	((null slots) res))))

;;;
;;; Top level call to modify-in-place. Copy-of is the copy of old-wme without
;;; the changes, while old-wme is the changed wme.
;;;

(defun $modify-in-place (old-wme &rest newslots)
  (when *TIME-FRULEKIT*
    (setq *START-MODIFY* (get-internal-run-time)))
  (setq old-wme (assure-frame old-wme))
  (let ((copy-of (fast-copy-frame old-wme)))
    (modify-frame old-wme newslots)
    (when (> *RECORD-LEVEL* 0)
      (push (cons old-wme copy-of) (svref *MODIFIES* *MODCYCLE*)))
    (let ((*PERFORMING-BLOCKING* :no-unblocking))
      ($mip old-wme copy-of)
      (when *TIME-FRULEKIT*
	(incf *TOTAL-TIME* (- (get-internal-run-time) *START-MODIFY*))))
    old-wme))

;;; 
;;; Main miping function that handles the top level of the rete net
;;;
(defun $mip (wme old-wme)
  (setf (wme-%time wme) (+ *CYCLE* *INTRA-CYCLE-TIME*))
  (incf *INTRA-CYCLE-TIME* (/ 1.0 1000))
  (when *TRACE-MODIFY*
    (ml-format T :mip-wme)
    (pp-wme wme))
  (let ((tnode (find-tnode wme))
	(nnode (find-nnode wme)))
    (cond ((and (not tnode) (not nnode))
	   (rk-format T :no-rules-class (wme-%class wme))
	   wme)
	  (T
	   (let ((old-tnode-token (if tnode (find-output-token wme
							       tnode)))
		 (old-nnode-token (if nnode (find-output-token wme
							       nnode))))
	     (if tnode
		 (mip-tnode-push-through old-tnode-token wme tnode old-wme))
	     (mip-nnode-push-through old-nnode-token wme nnode old-wme))
	   (perform-blocking-and-unblocking))))
  wme)


(defun mip-tnode-push-through (token wme node old-wme)
  (dolist (right-node (rete-node-right-output node))
    (mip-alpha token wme right-node old-wme)))

(defun mip-nnode-push-through (token wme node old-wme)
  (when node
    (mip-tnode-push-through token wme node old-wme)
    (mip-nnode-push-through token wme (rete-node-left-output node) old-wme)))

;;; In case the token is NIL it will do nothing. this is the case when the first
;;; thing that it is done is a $modify-in-place.

(defun mip-alpha (token wme node old-wme)
  (if token
      (case-table (find-output-token wme node)
		  (alpha-test-p token node)
		  nil token node old-wme)))

(defun case-table (was is right-token left-token node old-wme)
  (cond ((and was is)
	 (dolist (right-node (rete-node-right-output node))
	   (mip-beta-left was right-node old-wme))
	 (dolist (left-node (rete-node-left-output node))
	   (mip-beta-right was left-node old-wme)))
	((or was is)
	 (let ((*TOKEN-POSITIVE* is))
	   (if (alphap node)
	       (alpha-add-through left-token node)
	       (if (absnodep node)
		   (neg-send-right left-token node)
		   (mip-left-beta-push-one was right-token left-token node)))))
	(T nil)))
 
(defun mip-beta-right (token node old-wme)
  (if (absnodep node)
      (dolist (left-token (rete-node-left-mem node))
	(mip-negative-right left-token token node old-wme))
      (dolist (left-token (rete-shared-left node))
	(case-table (find-joined-output-token
	             (token-contents left-token)
                     (token-contents token)
                     node)
		    (beta-test-p left-token token node)
		    token left-token node old-wme))))

(defun mip-beta-left (token node old-wme)
  (if (absnodep node)
      (mip-negative-left token node old-wme)
      (dolist (right-token (rete-shared-right node))
	(case-table (find-joined-output-token
                     (token-contents token)
                     (token-contents right-token)
                     node)
		    (beta-test-p token right-token node)
		    right-token token node old-wme))))

(defun mip-negative-left (token node old-wme)
  (case-table (find-output-token-left token node)
	      (= (recompute-new-match-count-left token node) 0)
	      nil token node old-wme))


(defun mip-negative-right (left-token right-token node old-wme)
  (case-table (find-output-token-left left-token node)
	      (= (recompute-new-match-count-right left-token right-token
						  node old-wme
						  (token-contents right-token))
		 0)
	      nil left-token node old-wme))

(defun recompute-new-match-count-right (ltoken rtoken node old-wme wme)
  (let ((is (beta-test-p ltoken rtoken node)))
    (setf (token-contents rtoken) old-wme)
    (let ((was (beta-test-p ltoken rtoken node)))
      (cond ((and was (not is))
	     (decf (token-match-count ltoken)))
	    ((and (not was) is)
	     (incf (token-match-count ltoken)))
	    (t nil)))
    (setf (token-contents rtoken) wme)
    (token-match-count ltoken)))

(defun recompute-new-match-count-left (token node)
  (let ((num 0))
    (setf (token-match-count token)
	  (dolist (rtoken (rete-shared-right node) num)
	    (if (beta-test-p token rtoken node) (incf num))))))


;;;;;;;;; Code
(defun init-inter ()
  (re-init-trace)
  (init-time)
  (setq *CR-STRATEGY* *MEA*)  ;;fn which returns an instant given a conflict set.
  (setq *SWITCHES* '(*TRACE-MATCH* *TRACE-UNMATCH* *TRACE-ADD* *TRACE-DELETE*
		     *TRACE-MODIFY* *TRACE-FIRE* *TRACE-CYCLE* *TRACE-CR*
		     *PAUSE-EVERY-CYCLE* *CR-STRATEGY* *REFRACT* *RECORD-LEVEL*
		     *MAX-BACK* *FULL-WMEPRINT* *NUMBER-WMES* *AGENDA-LOADED*
		     *RULEKIT-TERSE*))
  (setq *INSTANT* *TOP-LEVEL-INSTANT*)
  (clrhash *VAR-BINDING-HASH*)
  (setq *CYCLE* 0)
  (setq *MODCYCLE* 0)
  (setq *BACKING-UP* NIL)
  (rk-format T :interpreter-initialized))

;;; Literalize now has the same syntax, and almost the same semantics, as
;;; Def-frame.  Example: (literalize goal () :parent NO :action run)
(defmacro literalize (classname cplist &rest plist)
  (cond ((correct-common-p classname)
	 (ml-cerror :ignore-literalize :reserved-clisp-type classname)
	 NIL)
	(T
	 (keywordize-cplist cplist)
	 (let ((isas (getf cplist :is-a)))
	   (if (listp isas)
	       (if (not (some #'(lambda (parent) (isa-p parent 'wme)) isas))
		   (setf (getf cplist :is-a) (nconc isas '(wme))))
	       (if (not (isa-p isas 'wme))
		   (setf (getf cplist :is-a) (list isas 'wme)))))
	 (if (not (memq :setable cplist))	  ;;over-ride inheritance of WME
	     (setq cplist (nconc cplist (copy-list '(:setable NIL)))))
	 `(progn
	   (def-frame ,classname ,cplist ,@plist)
	   (make-wmesnames ',classname)))))

(defun make-wmesnames (classname)
  (let ((snames
	 (remove-trace-snames (get-slot-names classname))))
    (putprop classname snames :wmesnames)))

;;; Remove the slot names that the trace package uses from the given list.
;;; This is done so that a list of the printable slots is associated with the
;;; frame, as well as the whole list of slot names.
(defun remove-trace-snames (snames)
  (nthcdr (length (get-slot-names 'wme)) snames))


(defun run (&optional (cycles *MAX-BACK*))
  (init-instant)
  (main-rk-loop cycles))

(defun init-instant ()
  (if (null *INSTANT*)
      (setq *INSTANT*
	    *TOP-LEVEL-INSTANT*)))

(defun halt ()
  (cond ((kyoto-lisp-p)
	 (rk-format T :halt-r))
	((cmu-common-lisp-p)
	 (rk-format T :halt-go))
	((dec-lisp-p)
	 (rk-format T :halt-cont))
	((xerox-p)
	 (rk-format T :halt-ok))
	(T (rk-format T :halting)))
  (break))

;;; Added 27-Mar.  Allows breakpointing of rules.  Possible values for
;;; breakpointing are :BEFORE, :AFTER, :ALWAYS, and NIL.
(defmacro rbreak (rname &optional (val :BEFORE))
  `(rbreak0 ',rname ',val))

(defun rbreak0 (rname val)
  (if (not (memq val '(:BEFORE :AFTER :ALWAYS NIL)))
      (ml-format T :legal-breakpoint-values)
      (let ((rule (get rname 'prod)))
	(if (not rule)
	    (ml-format T :no-such-rule rname)
	    (setf (rk-rule-break rule) val))))
  val)


;;; Creates the Frulekit form of a wme given the raw contents in slots-values,
;;; then pushes it through the net.
;;; Assumes that *INSTANT* contains the name of the prod. which created struct.
;;; similar syntax to ops5:  ($make <class> . (slot value)*)
;;; ex: ($make 'goal :parent 'dine :action 'eat-food)    where goal is a class
;;; ex: ($make 'polygon :size-of 10 :perimeter 5)    established by literalize
;;; Returns the new wme it makes.
(defun $make (class &rest slots-values)
  (when *TIME-FRULEKIT*
    (setq *START-MODIFY* (get-internal-run-time)))
  (let ((new-wme
	 ($add (make-frame0		;;Parmenides function.
		class NIL
		`(,@slots-values)))))
    (when *TIME-FRULEKIT*
      (incf *TOTAL-TIME* (- (get-internal-run-time) *START-MODIFY*)))
    new-wme))

(defun $fast-make (class &rest slots-values)
  (when *TIME-FRULEKIT*
    (setq *START-MODIFY* (get-internal-run-time)))
  (let ((new-wme
	 ($add (fast-make-frame0		;;Parmenides function.
		class NIL
		`(,@slots-values)))))
    (when *TIME-FRULEKIT*
      (incf *TOTAL-TIME* (- (get-internal-run-time) *START-MODIFY*)))
    new-wme))

;;; Added 18-Aug-87.  Like $make but also takes a name argument.
(defun $make-named (class name &rest slots-values)
  (when *TIME-FRULEKIT*
    (setq *START-MODIFY* (get-internal-run-time)))
  (let ((new-wme
	 ($add (make-frame0		;;Parmenides function.
		class name
		`(,@slots-values)))))
    (when *TIME-FRULEKIT*
      (incf *TOTAL-TIME* (- (get-internal-run-time) *START-MODIFY*)))
    new-wme))

;;; Adds a frame which was already made, to WM.  Written 6-Sep-86.
(defun add-frame (frame)
  (when *TIME-FRULEKIT*
    (setq *START-MODIFY* (get-internal-run-time)))
  (let ((new-wme ($add (assure-frame frame))))
    (when *TIME-FRULEKIT*
      (incf *TOTAL-TIME* (- (get-internal-run-time) *START-MODIFY*)))
    new-wme))

;;; If <iterations> is :FOREVER then run until the conflict set is empty;
;;; else limit it to <iterations> cycle.
;;; Returns the value of the last production fired.
;;; Would be more elegant with flet, but most lisps don't handle it very well.
(defun main-rk-loop (iterations)
  (setq *BACKING-UP* NIL *RESULT* NIL *COMPILING-RHS* NIL)
  (if (eq iterations :FOREVER)
      (loop
       (inter-inc-cycle)
       (perform-cr *CONFLICT-SET*)          ;;pick *INSTANT* from *CONFLICT-SET*
       (refract)
       (cond ((null *INSTANT*)
	      (return))
	     (T
	      (setq *RESULT* (fire-chosen-instant)))))
      (dotimes (i iterations)
	(inter-inc-cycle)
	(perform-cr *CONFLICT-SET*)
	(refract)
	(cond ((null *INSTANT*)
	       (return))
	      (T
	       (setq *RESULT* (fire-chosen-instant))))
	))
  (cond ((null *INSTANT*)
	 (setf (svref *ADDITIONS* *MODCYCLE*) NIL)
	 (setf (svref *DELETIONS* *MODCYCLE*) NIL)
	 (setf (svref *MODIFIES* *MODCYCLE*) NIL)
	 (setf (svref *REFRACTIONS* *MODCYCLE*) NIL)
	 (rk-format T :no-productions)
	 (setq *INSTANT* *TOP-LEVEL-INSTANT*)))
  *RESULT*)


(defun inter-inc-cycle ()
  (incf *CYCLE*)
  (if (> *RECORD-LEVEL* 0)
      (setf *MODCYCLE* (mod *CYCLE* *MAX-BACK*)))
  (setq *INTRA-CYCLE-TIME* 0.0)
  (if *TRACE-CYCLE* (ml-format T :cycle *CYCLE*)))

(defun refract ()
  (when (and *REFRACT* *INSTANT*)
    (setf *CONFLICT-SET* (fast-del *INSTANT* *CONFLICT-SET* eq))
    (if (> *RECORD-LEVEL* 0)
	(setf (svref *REFRACTIONS* *MODCYCLE*) *INSTANT*))
    (let ((prod (instant-prod *INSTANT*)))
      (decf (rk-rule-inscount prod))
      (when (and (rk-rule-disj-nodes prod)
		 (= (rk-rule-inscount prod) 0)
		 (some-blocked-bottom-node-p prod))
	(queue-removed-instant *INSTANT*)
	(perform-blocking-and-unblocking))
      (when *AGENDA-LOADED*
	(agenda-delete-instant prod *INSTANT*)))))

;;; For the agenda module.
(defmacro current-rule-in-conflict-set-p ()
  `(in-conflict-set-p ',*CURRENT-RULE-NAME*))

;;; For the Agenda module.
(defun in-conflict-set-p (rulename)
  (> (rk-rule-inscount (get rulename 'prod)) 0))

;;; General conflict-resolution-strategy definer.
;;; Extractor: should return a value given an instantiation.
;;; Comparer: given two values, return T iff the first one is preferred.
(eval-when (load eval compile)
  (defmacro def-cr-strategy (fname extractor comparer)
    `(defun ,fname (cs)
       (do* ((cands cs (cdr cands))
	     (value (,extractor (car cands))
		    (,extractor (car cands)))
	     (highest value highest)
	     (winners (list (car cands))
		      (cond ((,comparer value highest)
			     (setf highest value)
			     (list (car cands)))
			    ((eq value highest)
			     (cons (car cands) winners))
			    (T winners))))
	    ((null (cdr cands)) (or winners cs)))))

;;; ces must always correspond one-to-one with cs!
  (defmacro def-ce-cr-strategy (fname extractor comparer)
    `(defun ,fname (cs ces)
       (do* ((cands cs (cdr cands))
	     (ces ces (cdr ces))
	     (value (and (car ces) (,extractor (caar ces)))
		    (and (car ces) (,extractor (caar ces))))
	     (highest value highest)
	     (winning-ces (list (car ces)) winning-ces)
	     (winners (list (car cands))
		      (cond ((null value) winners)
			    ((or (null highest) (,comparer value highest))
			     (setf highest value)
			     (setf winning-ces (list (car ces)))
			     (list (car cands)))
			    ((eq value highest)
			     (push (car ces) winning-ces)
			     (cons (car cands) winners))
			    (T winners))))
	    ((null (cdr cands))
	     (if (null highest) NIL
		 (cons winners winning-ces))))))

  (defmacro def-complex-cr-strategy (fname extractor comparer)
    (let ((fname2 (intern (concatenate 'string (symbol-name fname) "-WMES"))))
      `(progn
	(def-ce-cr-strategy ,fname2 ,extractor ,comparer)
	(defun ,fname (cs)
	  (do ((ces NIL (update-ces ces))
	       (old-candidates cs candidates)
	       (candidates (cons cs (mapcar #'instant-sortedwmes cs))
			   (,fname2 candidates ces)))
	      ((null (car candidates)) old-candidates)
	    (setq ces (cdr candidates) candidates (car candidates))
	    (cond ((null (cdr candidates))        ;;if it's narrowed down to one
		   (return candidates)))))))))		  ;;then return that one

(defun update-ces (ces)
  (do ((ce ces (cdr ce)))
      ((null ce) ces)
    (setf (car ce) (cdr (car ce)))))


;;; (def-complex-cr-strategy narrow-down-wme-class wme-%class isa-parent)
;;; Defined below, since the standard def-complex-cr-strategy looks through
;;; the instant-sortedwmes list, but narrow-down-wme-class has to iterate
;;; through the list of WMEs as they matched the rule.

(Defun Narrow-Down-Wme-Class (Cs)
  (Do ((Old-Candidates Cs Candidates)
       (Candidates (Narrow-Down-Wme-Class-Wmes
		    cs
		    (Mapcar #'(Lambda (Ins) (reverse-improper
					     (Token-Contents (Instant-Token
							      Ins))))
			    Cs))
		   (Narrow-Down-Wme-Class-Wmes (car Candidates)
					       (cdr candidates))))
      ((null (Car Candidates)) (car Old-Candidates))
    (update-ces (cdr candidates))
    (Cond ((Null (Cdar Candidates))
	   (Return (car Candidates))))))

(Defun Narrow-Down-Wme-Class-Wmes (Cs Ces)
  (Do*
      ((Cands Cs (Cdr Cands))
       (Ces Ces (Cdr Ces))
       (Value (And (Car Ces) (Wme-%Class (Caar Ces)))
	      (And (Car Ces) (Wme-%Class (Caar Ces))))
       (Highest Value Highest)
       (Winning-Ces (List (Car Ces)) Winning-Ces)
       (Winners (List (Car Cands))
		(Cond ((Null Value) Winners)
		      ((Or (Null Highest) (Isa-Parent Value Highest))
		       (Setf Highest Value) (Setf Winning-Ces (List (Car Ces)))
		       (List (Car Cands)))
		      ((not (isa-parent highest value))
		       (Push (Car Ces) Winning-Ces)
		       (Cons (Car Cands) Winners))
		      (T Winners))))
      ((Null (Cdr Cands))
       (If (Null Highest) NIL (Cons Winners Winning-Ces)))))


;;; Returns the last elt of an improper list.
(defun ilast (ilist)
  (if (consp ilist)
      (cdr (last ilist))
      ilist))


(def-cr-strategy narrow-down-mea
  (lambda (ins)
    (wme-%time
     (ilast (token-contents (instant-token ins)))))
  >)

;;; A specificity strategy.  See specificity.mss or .doc for more details.
(def-cr-strategy num-tests
  (lambda (ins) (car (rk-rule-num-tests (instant-prod ins))))
  >)

(def-cr-strategy num-checks
  (lambda (ins) (cdr (rk-rule-num-tests (instant-prod ins))))
  >)

(def-complex-cr-strategy narrow-down-wme-class wme-%class isa-parent)

;;(def-cr-strategy narrow-down-test-class instant-prod tests-more-specific-p)
;; defined below

(def-complex-cr-strategy narrow-down-lex
  (lambda (wme) (floor (wme-%time wme)))
  >)
(def-complex-cr-strategy narrow-down-lex-ordered wme-%time >)

(defun isa-parent (class1 class2)
  (and (not (eq class1 class2))
       (isa-p class2 class1)))

(defun tests-more-specific-p (prod1 prod2)
  (if (not (eq prod1 prod2))
      (do ((lhs1 (rk-rule-lhs prod1) (cdr lhs1))
	   (lhs2 (rk-rule-lhs prod2) (cdr lhs2)))
	  ((or (null lhs1) (null lhs2))
	   (if (null lhs1) NIL T))
	(when (and (not (member (caar lhs1) '(<ABS> <OR>)))
		   (not (member (caar lhs2) '(<ABS> <OR>))))
	  (let ((class1 (if (atom (caar lhs1)) (caar lhs1) (cadr (caar lhs1))))
		(class2 (if (atom (caar lhs2)) (caar lhs2) (cadr (caar lhs2)))))
	    (if (and (not (eq class1 class2)) (isa-p class1 class2))
		(return T)))))))

(defun narrow-down-test-class (cs)
  (Do*
      ((Cands Cs (Cdr Cands))
       (Value (Instant-Prod (Car Cands)) (Instant-Prod (Car Cands)))
       (Highest Value Highest)
       (Winners (List (Car Cands))
		(Cond
		 ((Tests-More-Specific-P Value Highest) (Setf Highest Value)
		  (List (Car Cands)))
		 ((Or (Eq Value Highest)
		      (Not (Tests-More-Specific-P Highest Value)))
		  (Cons (Car Cands) Winners))
		 (T Winners))))
      ((Null (Cdr Cands)) (Or Winners Cs))))

(defun perform-cr (conflict-set)
  (setq *INSTANT*
	(let ((cs conflict-set))
	  (do ((cr-strategy *CR-STRATEGY* (cdr cr-strategy)))
	      ((null cr-strategy)
	       (when (and (cdr cs) *TRACE-CR*)
		 (ml-format T :no-cr-strategies))
	       (car cs))
	    (if (null (cdr cs))
		(return (car cs)))
	    (when *TRACE-CR*
	      (ml-format T :cr-strategy (car cr-strategy)))
	    (setq cs (funcall (car cr-strategy) cs))))))

;;; Conflict resolution *MEA*: given the conflict set, return the
;;; instantiation which will make the system perform means-ends-analysis.
;;; (1) refract
;;; (2) recency of first cond. elt
;;; (3) recency of remaining cond. elts
;;; (4) tests count
;;; (5) lisp checks count
;;; (6) arbitrary

;;; *LEX* is like *MEA* except step (2) is taken out.


(defmacro print-disjunct-num (instant msgname)
  `(let ((parents (get-parent-disj-nodes (instant-prod ,instant))))
     (do* ((parent parents (cdr parent))
	   (parent-num (length parent) (1- parent-num))
	   (top-disj (if parent (marked-token-top-disj (instant-token ,instant)))
		     (cdr top-disj)))
	  ((null parent) T)
       (let* ((siblings (if parent (rete-node-right-output (car parent)))))
	 (when siblings
	   (ml-format T ,msgname
		   (1+ (position (car top-disj) siblings)))
	   (if (cdr parents)
	       (ml-format T :in-the parent-num))
	   (terpri))))))

(defmacro end-rhs ()
  `(throw 'fire-chosen-instant))

;;; Since var is defined to get the value of the var from the hashtable,
;;; all we really have to do is evaluate each form in the rhs of the winner.
;;; Returns the value of the last action performed by the rule.
(defun fire-chosen-instant ()
  (let* ((prod (instant-prod *INSTANT*))
	 (pname (rk-rule-pname prod))
	 (breakval (rk-rule-break prod))
	 res)
    (when (and (rk-rule-beliefs prod)
	       (not (equal (rk-rule-beliefs prod) '(LAMBDA NIL))))
      (push *INSTANT* *FIRED-INSTANTS*))
    (when *TRACE-FIRE*
      (ml-format T :firing-production pname)
      (print-disjunct-num *INSTANT* :is-disjunct)
      (cond ((eq *TRACE-FIRE* 'values)
	     (ml-format T :with-values)
	     (show-var-vals *INSTANT*))
	    (T (terpri))))
    (when (> *RECORD-LEVEL* 0)
      (push *INSTANT* (svref *PROD-FIRINGS* *MODCYCLE*))
      (if (> *RECORD-LEVEL* 1)
	  (store-fired *INSTANT*)))
    (push-rhs-var-bindings *INSTANT* prod)
    (when (or (eq breakval :BEFORE) (eq breakval :ALWAYS))
      (ml-format T :breakpoint-before pname)
      (halt))
    (setq *COMPILING-RHS* NIL)
    (catch 'fire-chosen-instant
      (if (rk-rule-beliefs-fn prod)
	  (setq res (funcall (rk-rule-beliefs-fn prod))))
      (if (rk-rule-rhs-fn prod)
	  (setq res (funcall (rk-rule-rhs-fn prod)))))
    (setq *INSTANT* *TOP-LEVEL-INSTANT*)
    (cond ((or (eq breakval :AFTER) (eq breakval :ALWAYS))
	   (ml-format T :breakpoint-after pname)
	   (halt))
	  (*PAUSE-EVERY-CYCLE* (halt)))
    (pop-rhs-var-bindings prod)
    res))


;;;;;;;;; add & remove
;;; add wme by pushing it through the net, updating the c.s. if necessary.
(defun $add (wme)
  (when (not *NO-LITERALIZE*)
    (if (not *BACKING-UP*) (setf (wme-%created wme) *INSTANT*))
    (setf (wme-%time wme) (+ *CYCLE* *INTRA-CYCLE-TIME*))
    (incf *INTRA-CYCLE-TIME* (/ 1.0 1000)))
  (when *TRACE-ADD*
    (ml-format T :adding-wme)
    (pp-wme wme))
  (if (> *RECORD-LEVEL* 0)
      (push wme (svref *ADDITIONS* *MODCYCLE*)))
  (let ((tnode (find-tnode wme))
	(nnode (find-nnode wme)))
    (cond ((and (not tnode) (not nnode))
	   (rk-format T :no-rules-class (wme-%class wme))
	   wme)
	  (T
	   (let ((token
		  (make-freelist-frame token 3
		   :contents wme
		   :match-count 0))
		 (*TOKEN-POSITIVE* T))
	     (let ((*PERFORMING-BLOCKING* :NO-UNBLOCKING))
	       (if tnode (tnode-push-through token tnode))
	       (push-at-nnode nnode token))
	     (perform-blocking-and-unblocking)))))
  wme)

;;; Added 2-Sep-86 to support any-class tests. (e.g., (any person)
;;; would match a boy.)
(defun push-at-nnode (node token)
  (when node
    (tnode-push-through token node)
    (push-at-nnode (rete-node-left-output node) token)))

;;; Remove wme by pushing the "negation" of it through the net, updating
;;; the C.S. if necessary.
;;; Added new-wme argument 22-Nov-1987.  If it's NIL then just remove-frame
;;; the old wme.  If it's NIL, then replace-frame.  If it's :neither then do
;;; neither.  The pre-modify demon uses this feature.
(defun $remove-wme (wme new-wme)
  (let ((tnode (and wme (find-tnode wme)))
	(nnode (and wme (find-nnode wme))))
    (cond ((not wme)
	   (ml-cerror :ignore-remove :non-variable))
	  ((not (memq 'wme (isas (frame-class wme))))
	   (ml-cerror :ignore-remove :var-not-wme wme))
	  (T
	   (if (> *RECORD-LEVEL* 0)
	       (push wme (svref *DELETIONS* *MODCYCLE*)))
	   (cond ((and (not tnode) (not nnode))
		  (when (not *RULEKIT-TERSE*)
		    (ml-format T :remove-no-productions)
		    (pp-wme wme) (princ (string *RIGHT-BRACKET*))))
	   (T
	    (let* ((token
		    (make-freelist-frame token 3
		     :contents wme
		     :match-count 0))		;;means remove it
		   (*TOKEN-POSITIVE* NIL))
	      (when *TRACE-DELETE*
		(ml-format T :removing-wme)
		(pp-wme wme))
	      (if tnode (tnode-push-through token tnode))
	      (push-at-nnode nnode token)
	      (release-frame token))))
	   (cond ((null new-wme)
		  (if (= *RECORD-LEVEL* 0) (remove-frame wme)))	  ;;PA function
		 ((not (eq new-wme :neither))
		  (replace-frame wme new-wme)))		;;Parmenides function
))))

;;; Removes those wmes indicated by the labels.
;;; example: ($remove =goal =parent), where =goal and =parent are bound
;;; to wmes that match condes.
(defun $remove (&rest wmevars)
  (when *TIME-FRULEKIT*
    (setq *START-MODIFY* (get-internal-run-time)))
  (let ((*PERFORMING-BLOCKING* :NO-UNBLOCKING))
    (dolist (wme wmevars)
      ($remove-wme wme NIL)))
  (perform-blocking-and-unblocking)
  (when *TIME-FRULEKIT*
    (incf *TOTAL-TIME* (- (get-internal-run-time) *START-MODIFY*)))
  T)

;;; Just like $remove except doesn't delete the PA frame wme.
(defun $remove-keep (&rest wmevars)
  (when *TIME-FRULEKIT*
    (setq *START-MODIFY* (get-internal-run-time)))
  (let ((*PERFORMING-BLOCKING* :NO-UNBLOCKING))
    (dolist (wme wmevars)
      ($remove-wme wme :neither)))
  (perform-blocking-and-unblocking)
  (when *TIME-FRULEKIT*
    (incf *TOTAL-TIME* (- (get-internal-run-time) *START-MODIFY*)))
  T)

;;;; Modify tracing mini-module: store-old, get-old, rem-old ;;;;

;;; For each cycle, (svref *MODIFIES* <cycle>) is a list of (wme . <modified>)
;;; where <modified> is the old wme.
(defun store-old (wme newslots)
  (push (cons wme (fast-copy-frame wme newslots)) (svref *MODIFIES* *MODCYCLE*)))

(defun $modify (wme &rest newslots)
  (when *TIME-FRULEKIT*
    (setq *START-MODIFY* (get-internal-run-time)))
  (setq wme (assure-frame wme))
  (let* ((oldwme wme)
	 (newwme oldwme))
    (case *RECORD-LEVEL*
      (1
       (store-old oldwme newslots))
;;       (setq newwme (fast-copy-frame oldwme))
;;       (push (cons oldwme newwme) (svref *MODIFIES* *MODCYCLE*))
      (2
       (setq newwme (fast-copy-frame oldwme))
       (push (cons oldwme newwme) (svref *MODIFIES* *MODCYCLE*))))
    (let ((*PERFORMING-BLOCKING* :no-unblocking))
      ($remove-wme oldwme (if (> *RECORD-LEVEL* 0) newwme :neither)))
    (modify-frame newwme newslots)	;;Parm modify-frame changes desired slots
;;    (when (not (eq newwme oldwme))
;;      (pop-rk-var wme)		  ;;For when  there are 2 modifies on
;;      (set-rk-var (cadr wme) newwme))	  ;;the same label in a row.
;;    (push newwme (get (frame-class newwme) :instances))	;;sort of weird
    ($add newwme)
    (when *TIME-FRULEKIT*
      (incf *TOTAL-TIME* (- (get-internal-run-time) *START-MODIFY*)))
    newwme))

;;; Exactly like $modify except calls demons.
(defun $modify-demons (wme &rest newslots)
  (setq wme (assure-frame wme))
  (let* ((oldwme wme)
	 (newwme oldwme))
    (case *RECORD-LEVEL*
      (1
       (store-old oldwme newslots))
      (2
       (setq newwme (fast-copy-frame oldwme))
       (push (cons oldwme newwme) (svref *MODIFIES* *MODCYCLE*))))
    ($remove-wme oldwme (if (> *RECORD-LEVEL* 0) newwme :neither))
    (modify-frame-demons newwme newslots)	;;Parmenides modify-frame-demons
;;    (push newwme (get (frame-class newwme) :instances))	 ;;sort of weird
    ($add newwme)))

;;; Pre-modify is the pre-if-set demon for all Parmenides wmes.
;;; This doesn't completely work for record-level 2 since in set-facet-demons,
;;; Parmenides modifies the facet on frame, whereas we really want it to
;;; modify newwme.
(defun pre-modify ()
  (declare (special frame *modify-demon-queue*))
  (if (and (wmep frame)
	   (frame-instance-p frame)
	   (in-wm-p frame)
	   (not (member-if
		 #'(lambda (new-wme)
		     (if (atom new-wme) (eq new-wme frame)
			 (eq (car new-wme) frame)))
		 *modify-demon-queue*)))
      (let* ((oldwme frame)
	     (newwme oldwme)
	     otherwme)
	(when (> *RECORD-LEVEL* 0)
	  (setq otherwme (fast-copy-frame oldwme))
	  (push (cons oldwme otherwme) (svref *MODIFIES* *MODCYCLE*))
	  (if (= *RECORD-LEVEL* 2) (setq newwme otherwme)))
	($remove-wme oldwme (if (= *RECORD-LEVEL* 0) :neither newwme))
	(if (= *RECORD-LEVEL* 2)
	    (push (cons oldwme newwme) *modify-demon-queue*)
	    (push newwme *modify-demon-queue*)))))

;;; post-modify is the post-if-set demon for all FRulekit wmes.
;;; The post-modify demon works by calling $add on all the newwme queue'd in
;;; the *modify-demon-queue* by the pre-modify demon.  It is done this way
;;; to handle the case where the pre-modify and post-modify demons are
;;; called recursively (more than once).  This can cause the pre-modify demon
;;; to be called 2 or more times before the post-modify demon is called.
(defun post-modify ()
  (declare (special *modify-demon-queue*))
  (dolist (new-wme *modify-demon-queue*)
    (if (consp new-wme)
	($add (cdr new-wme))
	($add new-wme)))
  (setq *modify-demon-queue* NIL))


;;; More efficient modify.  Finds the highest node in the rete net
;;; that is affected by modifying the given slots, then starts the addition
;;; and deletion from there, rather from the top node.
;;; build.lisp has to do its part in keeping track of the highest-up node
;;; in the net that refers to a given slot in a given class.  The function
;;; highest-alpha-node in build returns this node.
(defmacro $smart-modify (wmevar &rest newslots)
  (let* ((oldwme (eval wmevar))
	 (newwme (if (> *RECORD-LEVEL* 0)
		     (fast-copy-frame oldwme)
		     oldwme))
	 (class (wme-%class oldwme))
	 (tnode (gethash class *RETE-TEST-HASH*))
	 (slots (slots-in newslots)))
    (when *TRACE-MODIFY*
      (ml-format T :modifying-wme)
      (pp-wme oldwme)
      (ml-format T :new-slots newslots))
    (modify-frame newwme newslots)
    (dolist (alpha (rete-node-right-output tnode))
      (if (node-contains-a-slot alpha slots)
	  (alpha-modify-node oldwme newwme alpha)))
    T))

(defun find-tnode (wme)
  (gethash (wme-%class wme) *RETE-TEST-HASH*))

(defun find-nnode (wme)
  (gethash (wme-%class wme) *RETE-N-NODE-HASH*))

;;; Return t iff the given node contains at least one of the given slots
(defun node-contains-a-slot (node slots)
  (some #'(lambda (slot)
	    (node-contains-slot node slot))	;;from build.lisp
	slots))

(defmacro $modify-num (wme &rest newslots)
  (let* ((oldwme (wme-num wme))
	 (newwme (if (> *RECORD-LEVEL* 0)
		     (fast-copy-frame oldwme)
		     oldwme)))
    ($remove-wme oldwme newwme)
    (modify-frame newwme newslots)
    `($add ',newwme)))


;;; given the number of the condition element, return the corresponding wme.
(defun wme-num (n)
  (nth (1- n)
       (reverse-improper (token-contents (instant-token *INSTANT*)))))


;;;; Node updating
;;; Precondition: token belongs at this tnode.
(defun tnode-push-through (token tnode)
  (do-token token tnode)
  (dolist (right-out (rete-node-right-output tnode))   ;;right-output from tnodes
    (alpha-push-through token right-out)))	       ;;always go to alphas.

;;; Added 17-Sep-86 to support smart-modify.  Since modify is not currently
;;; being done at beta nodes, this must still pass the token to the betas
;;; when the oldtruthval and newtruthval are both true.
(defun alpha-modify-node (oldwme newwme node)
  (let* ((oldtoken (find-token oldwme node))
	 (oldtruthval (= (token-match-count oldtoken) 1))
	 (newtoken (make-token :contents newwme))
	 (newtruthval (alpha-test-p newtoken node))
	 (*TOKEN-POSITIVE* T))
    (cond (oldtruthval
	   (let ((*TOKEN-POSITIVE* NIL))
	     (cond (newtruthval		;eventually should be beta-modify-nodes
		    (alpha-add-through oldtoken node)
		    (alpha-add-through newtoken node))
		   (T
		    (alpha-add-through oldtoken node)))))
	  (T
	   (cond (newtruthval
		  (alpha-add-through newtoken node)))))))

(defun find-token (wme alphanode)
  (car (member-if #'(lambda (token) (eq (token-contents token) wme))
		  (shared-contents (rete-node-left-mem alphanode)))))

;;; For un-refract-instant.
;;; Modified 23-Jun-87 for new memory structure.
(defun find-token-out (wmes node)
  (if (not node)
      (ml-cerror :go-on :rete-changed)
      (car (member-if #'(lambda (token) (equal (token-contents token) wmes))
		      (rete-shared-output node)))))

;;; Alpha-Test-P now updates the match-count slot.
;;; If we do a mem check instead of alpha test, it wouldn't re-evaluate
;;; the binds or checks, but in general this seems more expensive.
(defun alpha-push-through (token node)
  (cond ((alpha-test-p token node)
	 (if (bindp node)
	     (alpha-add-through (copy-token token) node)
	     (alpha-add-through token node)))))

(defun alpha-add-through (token node)
  (let ((outmem (rete-node-output-mem node)))
    (cond ((consp outmem)		;; Means it leads to a blocked disjunct
	   (cond ((blocked-node-p node)	;; Means it's an 'inner' disjunct.
		  (when (not *TOKEN-POSITIVE*)
		    (dolist (left-out (rete-node-left-output node))
		      (left-beta-push-through token left-out)))
		  (do-new-old-disj-token token outmem)
		  (when (not *TOKEN-POSITIVE*)
		    (dolist (right-out (rete-node-right-output node))
		      (right-beta-push-through token right-out))))
		 (T
		  (dolist (left-out (rete-node-left-output node))
		    (left-beta-push-through token left-out))
		  (do-new-all-disj-token token outmem)	  ;; For the last alpha
		  (dolist (right-out (rete-node-right-output node))
		    (right-beta-push-through token right-out)))))
	  (T
	   (dolist (left-out (rete-node-left-output node))
	     (left-beta-push-through token left-out))
	   (maybe-do-new-all-disj-token token (rete-node-output-mem node) node)
;;>	   (do-token token node)
	   (dolist (right-out (rete-node-right-output node))	;;All output is
	     (right-beta-push-through token right-out))		;;to betas.
	   ;;(frbreak "in alpha-add-through before left-beta-push-through")
	   ;;(frbreak "in alpha-add-through after left-beta-push-through")
	   )))
  (if (pnodep node) (change-cs token node)))



;;; Token has passed the test at node.
;;; If the node sits above blocked disjunctive nodes, then add the token
;;; to both output memories, and only send the token to the ACTIVE disjunct.
;;; If the node sits above a negative blocked disjunct, and we are deleting
;;; it, then remove it from that absence node's left-memory (since absence
;;; nodes have seperate memories from their parents).
(defun send-right (token node parent-token)
  (declare (ignore parent-token))
  (let ((outmem (rete-node-output-mem node)))
    (cond ((parent-disjunctive-node-p node)
	   (cond ((consp outmem)	   ;; Means there are blocked disjuncts
		  (do-new-all-disj-token token outmem)
		  (if *TOKEN-POSITIVE*	   ;; Additions are blocked
		      (dolist (disjunct (rete-node-right-output node))
			(if (active-node-p disjunct)
			    (right-beta-push-through token disjunct)))
		      (dolist (disjunct (rete-node-right-output node))
			(right-beta-push-through token disjunct))))
		 (T
		  (do-token token node)
		  (do ((disjunct (rete-node-right-output node) (cdr disjunct)))
		      ((null disjunct) T)
		    (when (blocked-node-p (car disjunct))
		      (store-partial-mem node token disjunct)
		      (return T))
		    (right-beta-push-through token (car disjunct))))))
	  ((consp outmem)
	   (do-new-old-disj-token token outmem))
	  (T
	   (do-token token node)
;;	   (if (not (blocked-node-p node))
	   (dolist (right-out (rete-node-right-output node))
	     (right-beta-push-through token right-out))))))

;;; Token comes left from right.
(defun left-beta-push-through (token betanode)
  (let ((type (rete-node-type betanode)))
    (cond ((realabs type)
	   (left-beta-push-neg token betanode))
	  ((beta-type-p type)
	   (if (not-bind-type-p type)
	       (left-beta-push-pos token betanode)
	       (left-beta-push-pos (copy-token-or-marked token) betanode)))
	  (T
	   (ml-error :unknown-type (rete-node-type betanode))))))


;;; When a token gets added by going left into node (comes from the right).
;;; Modified 2-Feb-87 to do a mem test in the output-mem if tokens is
;;; negative.  This makes it more efficient and doesn't re-test when
;;; deleting, which takes extra time when there are lisp CHECKS or BIND
;;; side-effects in the tests.
(defun left-beta-push-pos (rtoken node)
  (if *TOKEN-POSITIVE*
      (progn ;;(frbreak "before doleft-mem")
	(doleft-mem left-mem node rtoken)
	;;(frbreak "after doleft-mem")
	)
      (let ((outmem (rete-node-output-mem node)))
	(cond ((consp outmem)			;; Means it leads to disj nodes
;;	       (setq *outmem* outmem) (frbreak "left-beta-push-pos")
	       (right-delete-token rtoken (car outmem))
	       (right-delete-token-push rtoken (cadr outmem) node)
	       ;;(frbreak "in left-beta-push-pos before right-beta-push-thru")
	       (when (and (caddr outmem)	;; Delete from partial mem.
			  (eq (car (token-contents (car (cddr outmem))))
			      (token-contents rtoken)))
		 (setf (cddr outmem) NIL)))
	      (T
	       ;;(frbreak "before right-delete-token-push")
	       (right-delete-token-push rtoken outmem node)))
	       ;;(frbreak "after right-delete-token-push")
)))
;;	(release-frame rtoken)

;;; Negation case (2)
(defun neg-add-from-right (tokens node)
  (let ((*TOKEN-POSITIVE* NIL))
    (dolist (ltoken (rete-node-left-mem node))
      (when (beta-test-p ltoken tokens node)
	(incf (token-match-count ltoken))
	(if (= (token-match-count ltoken) 1)		;;If ltoken becomes 1
	    (neg-send-right ltoken node))))))

;;; Negation case (4)
;;; In order to test by searching instead of calling the beta test, we
;;; would need an additional pointer on each token to its parents, so
;;; that we may decrement the match count.  Since we are not doing this,
;;; putting BIND commands in absence tests will re-evaluate the bind.
(defun neg-del-from-right (token node)
  (let ((*TOKEN-POSITIVE* T))
    (dolist (ltoken (rete-node-left-mem node))
      (when (beta-test-p ltoken token node)
	(decf (token-match-count ltoken))
	(if (and (zerop (token-match-count ltoken))
		 (not (blocked-node-p node)))
	    (neg-send-right ltoken node))))))


;;; Negation case (1)
(defun neg-add-from-left (token node)
  (push token (rete-node-left-mem node))
  (neg-match-from-left token node 0))

;;; Tokens coming from the left.
(defun right-beta-push-through (tokens node)
  (let ((type (rete-node-type node)))
    (cond ((realabs type)
	   (right-beta-push-neg (copy-token-or-marked tokens) node))
	  ((beta-type-p type)
	   (if (not-bind-type-p type)
	       (right-beta-push-pos tokens node)
	       (right-beta-push-pos (copy-token-or-marked tokens) node)))
	  (T
	   (ml-error :illegal-node-type (rete-node-type node))))))

;;; When tokens go right into beta nodes they are plural
;;; (their contents are lists)
(defun right-beta-push-pos (ltokens node)
;;  (setq *ltokens* ltokens *node* node)
;;  (frbreak "in right-beta-push-pos")
  (if *TOKEN-POSITIVE*
      (dolist (right-mem (rete-shared-right node))
	(right-beta-push-one ltokens right-mem node))
      (let ((outmem (rete-node-output-mem node)))
	(setq *FIND-TOKEN* NIL)
	(cond ((consp outmem)		;; Indicates it leads to disj nodes
;;	       (setq *outmem* outmem) (frbreak "left-beta-push-through")
	       (left-delete-token ltokens (car outmem))
	       (left-delete-token-push ltokens (cadr outmem) node)
	       ;;(frbreak "in right-beta-push-pos before right-beta-push-thru")
	       (when (and (caddr outmem)	;; Delete from partial mem.
			  (eq (cdr (token-contents (car (cddr outmem))))
			      (token-contents ltokens)))
		 (setf (cddr outmem) NIL)))
	      (T
	       ;;(frbreak "before left-delete-token-push")
	       (left-delete-token-push ltokens outmem node)
	       ;;(frbreak "after left-delete-token-push")
	       ))
)))
;;;	(release-frame ltokens)

(defun neg-match-from-left (token node num)
  (setf (token-match-count token)
	(dolist (rtoken (rete-shared-right node) num)
	  (if (beta-test-p token rtoken node) (incf num))))
  (when (zerop num)			   ;; If it matches nothing on the
    (neg-send-right			   ;; right then send the token down.
     (maybe-new-token token node) node)))
;;; (maybe-new-token added 9-Jun-87)

;;; Negation case (3)
(defun neg-del-from-left (token node)
  (setq *FIND-TOKEN* NIL)
  (setf (rete-node-left-mem node)
	(delete-token-from-not-save token (rete-node-left-mem node)))
;;(frbreak "after delete-token-from-not-save")
  (when *FIND-TOKEN*
    (if (zerop (token-match-count *FIND-TOKEN*))   ;; If it was successful before
	(neg-send-right				   ;; then send deletion down
	 token node))))


;;; Push all the WMEs of the class corr. to the tnode, through the given tnode.
;;; Assumes that all frames under that class should go into WM.
(defun push-all (tnode)
  (let* ((class (rete-node-test tnode))		;;(cadr (caddr (caddr <tnode>)))
	 (class-frame (frame class)))
    (dolist (wme (instances-of class))
      (if (and (eq class (wme-%class wme))
	       (not (eq wme class-frame)))
	  (add-wme-at-tnode tnode wme)))))

(defun add-wme-at-tnode (node wme)
  (and (> *RECORD-LEVEL* 0)
       (push wme (svref *ADDITIONS* *MODCYCLE*)))
  (let ((token
	 (make-token
	  :contents wme
	  :match-count 0)))
    (tnode-push-through token node))
  wme)

;;; Push all the tokens above the given alpha node through it.
(defun push-to-alpha (alpha)
  (dolist (token (rete-shared-left alpha))	       ;;out-mem of above tnode
    (alpha-push-through token alpha)))

(defun push-to-beta (newbeta)
  (dolist (l-tokens (rete-generic-left newbeta))
    (right-beta-push-through l-tokens newbeta)))


;;;; Disjunctive node support, added 22-Apr-88
;;;; Based on frulekit/disj/algorithm.mss

;;; This terminates because unblocking a disjunct never causes that disjunct
;;; to be unblocked again.  Blocking disjuncts never cause any disjunct to
;;; be blocked or unblocked (since they add no instantiations).
(defun perform-blocking-and-unblocking ()
  (when (not (eq *PERFORMING-BLOCKING* :ALL))
    (loop
      (let ((removed-instants *REMOVED-INSTANTS*)
	    (added-instants *ADDED-INSTANTS*)
	    (old-performing-blocking *PERFORMING-BLOCKING*)
	    (*PERFORMING-BLOCKING* :ALL))
	(setq *ADDED-INSTANTS* NIL)
	(dolist (instant added-instants)
	  (if (> (rk-rule-inscount (instant-prod instant)) 0)
	      (block-all-siblings
	       (marked-token-top-disj (instant-token instant)))))
	(when (not (eq old-performing-blocking :NO-UNBLOCKING))
	  (setq *REMOVED-INSTANTS* NIL)
	  (dolist (instant removed-instants)
	    (when (zerop (rk-rule-inscount (instant-prod instant)))
	      (unblock-bottom-disj-nodes
	       (get-bottom-disj-nodes (instant-prod instant)))
	      (reset-parent-disj-nodes
	       (get-parent-disj-nodes (instant-prod instant))))))
	)
      (if (and (null *ADDED-INSTANTS*)
	       (or (null *REMOVED-INSTANTS*)
		   (eq *PERFORMING-BLOCKING* :NO-UNBLOCKING)))
	  (return T)))))

;;; (1) Block all siblings of a disjunctive node when an instantiation
;;; containing that disjunctive node is added to the conflict set.
(defun block-all-siblings (disj-nodes)
  (dolist (disj-node disj-nodes)
    (let ((type (rete-node-type disj-node)))
      (when (not (active-type-p type))
	(turn-on type *ACTIVE-BIT*)
	(let* ((parent (rete-node-left-input disj-node)))
	  (three-split-output-mem parent)
	  (dolist (sibling (rete-node-right-output parent))
	    (if (not (active-node-p sibling))
		(block-disj-node sibling)
		(mark-as-active sibling))))))))

(defun mark-as-active (node)
  (let ((type (rete-node-type node)))
    (turn-on type *ACTIVE-BIT*)
    (if (disj-type-p type)
	(dolist (child (rete-node-right-output node))
	  (mark-as-active child)))))

(defun block-top-disj-beta-node (node)
  (turn-on (rete-node-type node) *BLOCKED-BIT*)
  (block-disj-node node))

;;; Mark the given disjunctive node, as well as all its direct disjunctive
;;; descendants, as BLOCKED.
;;; The output-mem of the alphas going into the blocked disjuncts are changed
;;; to (<NEW-tokens> . <OLD-output-mem>).  All tokens coming out of
;;; these alphas are put into the temporary <NEW-tokens> list, and are
;;; not pushed further into output memory.  When a node is unblocked, the
;;; <NEW-tokens> are nconc'ed into <old-output-mem> and the <output-mem>
;;; is restored to normal.
(defun block-disj-node (node)
  (let ((right-alpha (rete-node-right-input node))
	(type (rete-node-type node)))
    (cond ((rete-node-right-output node)
	   (cond ((disj-type-p type)
		  (when (not (blocked-type-p type))		;; Avoid
		    (turn-on type *BLOCKED-BIT*)		;; duplication
		    (turn-on (rete-node-type right-alpha) *BLOCKED-BIT*)
		    (setf (rete-node-right-mem node)
			  (split-output-mem right-alpha))
		    (dolist (child (rete-node-right-output node))
		      (block-disj-node child))))
		 (T		;; Last node
		  (turn-on type *BLOCKED-BIT*)
		  (three-split-output-mem right-alpha))))
	  ((consp (rete-node-left-input node))		;; Indicates a 'join'
	   (turn-on type *BLOCKED-BIT*)
	   (three-split-output-mem right-alpha)
	   (if (realabs-node node) (split-output-mem node)
	       (if (eq (rete-node-left-mem node) :NOT-SHARED)
		   (setf (rete-node-left-mem node)
			 (rete-node-output-mem
			  (car (member-if #'(lambda (left-in)
					      (not (blocked-node-p left-in)))
					  (rete-node-left-input node))))))))
	  (T
	   (turn-on type *BLOCKED-BIT*)
	   (turn-on (rete-node-type right-alpha) *BLOCKED-BIT*)
	   (if (realabs-node node) (split-output-mem node))
	   (setf (rete-node-right-mem node)
		 (split-output-mem right-alpha))))))

;;; Store the given token, starting with left-input, as a partial-new token,
;;; in the output-mem of the alpha going into the given node.
(defun store-partial-mem (node token left-input)
  (setf (cddr (rete-node-output-mem node))
	(cons token left-input)))

;;; For alpha nodes, the format is: (NEW . OLD).
;;; For the top beta and the last alpha, the format is: (NEW ALL . Partial).
;;; The beta above the disjunctive nodes point (output-mem) to the whole list.
;;; The ACTIVE disjunctive beta points to the ALL list.
;;; The top BLOCKED betas point (left) to the ALL list.
;;; The top BLOCKED betas point (right) to the NEW list.
;;; The last positive beta points to the ALL alpha list.
;;; The last negative beta points to (NEW . ALL)
;;; Returns the new shared memory structure.
;;; Partial has the form: (token . node), and may be NIL.
(defun split-output-mem (node)
  (let ((new-shared (make-shared)))
    (setf (rete-node-output-mem node)
	  (cons new-shared (rete-node-output-mem node)))
    new-shared))

;;; Splits output-memories for nodes (top beta and bottom alphas) which
;;; have the format: (NEW ALL . Partial).  The NIL below is a place-holder
;;; for the Partially-new token which will be added later.
;;; Partial has the form: (token . node), and may be NIL.
(defun three-split-output-mem (node)
  (let ((new-shared (make-shared)))
    (setf (rete-node-output-mem node)
	  (list* new-shared (rete-node-output-mem node) NIL))
    new-shared))

;;; Restore the output mem of a node to how it was before it was split by
;;; split-output-mem.
(defun merge-output-mem (node)
  (let ((shared (cdr (rete-node-output-mem node))))
    (setf (shared-contents shared)
	  (nconc (shared-contents (car (rete-node-output-mem node)))
		 (shared-contents shared)))
    (setf (rete-node-output-mem node)
	  shared)))


;;; Step (3).
;;; Reset the output-mem of the given top beta node from (NEW ALL . Partial)
;;; to ALL.
(defun reset-parent-disj-nodes (nodes)
  (dolist (parent nodes)
    (setf (rete-node-output-mem parent)
	  (cadr (rete-node-output-mem parent)))))

;;; Step (3) and (4).
;;; Node is assumed to be the lowest beta disjunct node.
;;; Match NEW alphas against ALL beta memories which used to blocked.
;;; Parents is the list of left-inputs.  Node may be NIL, which means that
;;; parents are the bottom nodes for the productions.
(defun unblock-bottom-disj-nodes (bottom-nodes)
  (dolist (bottom bottom-nodes)
    (unblock-bottom-disj-node bottom)))

(defun unblock-bottom-disj-node (bottom)
  (let* ((node (if (not (consp bottom)) bottom))
	 (parents (if (consp bottom) bottom (rete-node-left-input node)))
	 (*TOKEN-POSITIVE* T))
    (if node (turn-off (rete-node-type node) *BLOCKED-BIT*))
    (dolist (parent parents)	       ;; Reverse blocked status
      (setf (sbit (rete-node-type parent) *BLOCKED-BIT*)
	    (1- (sbit (rete-node-type parent) *BLOCKED-BIT*))))
;;(frbreak "~%Unblocking bottom disj-node... ")
    (if node
	(let ((right-mem (rete-node-output-mem (rete-node-right-input node))))
	  (dolist (new-alpha (shared-contents (car right-mem)))
	    (left-beta-push-through new-alpha node))
	  (if (cddr right-mem)
	      (match-partial-new (cddr right-mem) node))
	  (cond ((realabs-node node)
		 (dolist (new-token (shared-contents
				     (car (rete-node-output-mem node))))
		   (change-cs new-token node))
		 (setf (rete-node-output-mem node)
		       (cdr (rete-node-output-mem node))))
		(T
		 (setf (rete-node-left-mem node) :NOT-SHARED))))
	(dolist (parent parents)
	  (when (and (not-active-node-p parent) (realabs-node parent))
	    (dolist (new-token
		     (shared-contents (car (rete-node-output-mem parent))))
	      (change-cs new-token parent))
	    (setf (rete-node-output-mem parent)
		  (cdr (rete-node-output-mem parent))))))
    (dolist (parent parents)
      (turn-off (rete-node-type parent) *BLOCKED-BIT*))
;;    (frbreak "~%AFTER matching bottom-blocked-alpha-mems... ")
    (if node (reset-last-alpha-mem node))
    (dolist (parent parents)
      (let ((p-type (rete-node-type parent)))
	(cond ((active-type-p p-type)
	       (if node (turn-off-actives node) (turn-off-actives parent)))
	      (T
	       (unblock-disj-node parent)))))))

(defun turn-off-actives (node)
  (let ((type (rete-node-type node)))
    (when (active-type-p type)
      (turn-off type *ACTIVE-BIT*)
      (if (consp (rete-node-left-input node))
	  (dolist (parent (rete-node-left-input node))
	    (turn-off-actives parent))
	  (turn-off-actives (rete-node-left-input node))))))

(defmacro unblock-neg-node-tokens (node)
  `(when (realabs-node ,node)
     (dolist (left-token (rete-node-left-mem ,node))
       (if (zerop (token-match-count left-token))
	   (neg-send-right left-token ,node)))))

(defun unblock-disj-node (node)
  (let* ((parent (rete-node-left-input node))
	 (p-type (rete-node-type parent))
	 (right-alpha (rete-node-right-input node)))
    (unblock-neg-node-tokens node)
    (turn-off (rete-node-type right-alpha) *BLOCKED-BIT*)
    (cond ((disj-type-p p-type)
;;(frbreak "~%Before match-blocked-alpha-mems... ")
	   (match-blocked-alpha-mems node)
;;(frbreak "~%After match-blocked-alpha-mems... ")
	   (merge-output-mem right-alpha)
	   (setf (rete-node-right-mem node)
		 (rete-node-output-mem right-alpha))
	   (turn-off p-type *BLOCKED-BIT*)
	   (unblock-disj-node parent))
	  ((consp (rete-node-output-mem (rete-node-right-input node)))
;;>	  (T)			;; In this case it's the top beta
;;(frbreak "~%Before match-top-blocked-beta-mems... ")
	   (match-top-blocked-beta-mems node)
;;(frbreak "~%After match-top-blocked-beta-mems... ")
	   (merge-output-mem right-alpha)
	   (setf (rete-node-right-mem node)
		 (rete-node-output-mem right-alpha))))))

;;; 4.2: try to match NEW (blocked) alpha-mems against ALL beta mems.
(defun match-blocked-alpha-mems (betanode)
  (let ((*TOKEN-POSITIVE* T))
    (dolist (new-alphamem (rete-shared-right betanode))
      (left-beta-push-through new-alphamem betanode))))

;;; For the top blocked disjunctive node, we need to unblock both the
;;; new betas and the new alphas:
;;; First, match ALL betas with NEW alphas; (match-blocked-alpha-mems)
;;; Then,  match NEW betas with OLD alphas. (match-top-blocked-[pos,neg]mems)
;;; Special consideration is made for top absence nodes.  We need to compute
;;; the NEW list by copying all the new tokens of the parent, and also to
;;; augment the ALL list by adding these new tokens into the ALL list.
;;; The new alphas are matched by calling neg-add-from-right on these alphas,
;;; and the new betas area matched by calling neg-add-from-left on these betas.
(defun match-top-blocked-beta-mems (node)
  (cond ((realabs (rete-node-type node))
	 (let* ((neg-node-new (compute-neg-new node))
		(neg-node-all (nconc (rete-node-left-mem node) neg-node-new))
		(old-alphamems (shared-contents (cdr (rete-node-output-mem
						      (rete-node-right-input
						       node)))))
		(new-alphamems (rete-shared-right node))
		(*TOKEN-POSITIVE* T))
	   (setf (rete-node-left-mem node) neg-node-all)
	   (dolist (new-alpha new-alphamems)
	     (neg-add-from-right new-alpha node))
	   (setf (rete-shared-right node) old-alphamems)
	   (dolist (new-left-token neg-node-new)
	     (neg-match-from-left
	      new-left-token node (token-match-count new-left-token)))
	   (setf (rete-shared-right node) new-alphamems)
	   (setf (rete-node-left-mem node) neg-node-all)))
	(T
	 (match-blocked-alpha-mems node)
	 (match-top-blocked-pos-mems node))))

;;	   (setf (rete-node-left-mem node) neg-node-new)
;;	   (dolist (old-alphamem old-alphamems)
;;	     (neg-add-from-right old-alphamem node))

(defun compute-neg-new (node)
  (let* ((parent-mem (rete-node-output-mem (rete-node-left-input node)))
	 (partial-token-and-nodes (cddr parent-mem))
	 (new-tokens
	  (mapcar #'copy-token-or-marked		;; Compute NEW list
		  (shared-contents (car parent-mem)))))
    (if (memq node (cdr partial-token-and-nodes))
	(let ((token (car partial-token-and-nodes)))
	  (push (copy-token-or-marked token) new-tokens)))
    new-tokens))

;;; Match NEW beta-mems in the first blocked beta node, with OLD alpha mems.
;;; Then reset the left memory of this beta node to be the ALL list (the cadr
;;; of the output-mem of its parent beta node).
(defun match-top-blocked-pos-mems (betanode)
  (let ((parent-mem (rete-node-output-mem (rete-node-left-input betanode)))
	(old-alphamems (shared-contents (cdr (rete-node-output-mem
					      (rete-node-right-input betanode))))))
    (setf (rete-node-left-mem betanode) (car parent-mem))	;; NEW tokens
    (dolist (old-alphamem old-alphamems)
      (left-beta-push-through old-alphamem betanode))
    (let ((partial-token-and-nodes (cddr parent-mem)))
      (when (memq betanode (cdr partial-token-and-nodes))
	(let* ((token (car partial-token-and-nodes))
	       (shared (make-shared :contents (list token))))
	  (setf (rete-node-left-mem betanode) shared)
	  (dolist (old-alphamem old-alphamems)
	    (left-beta-push-through old-alphamem betanode)))))
    (setf (rete-node-left-mem betanode)		;; Reset mem to original
	  (cadr (rete-node-output-mem (rete-node-left-input betanode))))))

;;; In negative top-blocked nodes, the left-mem is (NEW . ALL), but these
;;; lists are not eq to the output-mems of the parents, so things must be
;;; done differently.
(defun match-top-blocked-neg-mems (betanode)
  (let* ((parent-mem (rete-node-output-mem (rete-node-left-input betanode)))
	 (old-alphamems (shared-contents (cdr (rete-node-output-mem
					       (rete-node-right-input
						betanode)))))
	 (partial-token-and-nodes (cddr parent-mem)))
    (if (memq betanode (cdr partial-token-and-nodes))
	(let ((token (car partial-token-and-nodes)))
	  (push (copy-token-or-marked token) (rete-node-left-mem betanode))))
    (dolist (old-alphamem old-alphamems)
      (neg-add-from-right old-alphamem betanode))))

(defun match-bottom-blocked-alpha-mems (betanode parent-disj alpha-mems)
  (dolist (new-alpha-mem (shared-contents alpha-mems))
    (left-beta-push-disj new-alpha-mem betanode
			 (rete-node-output-mem parent-disj))))

;;; If I decide to not queue block requests, then this will have to
;;; check to see if each input beta node is blocked.
;;; This is styled after left-beta-push-through.
(defun match-partial-new (partial-token-and-node betanode)
  (let ((type (rete-node-type betanode))
	(token (car partial-token-and-node))
	(first-left-input (cdr partial-token-and-node)))
    (cond ((realabs type)
	   (ml-error :bottom-not-negative))
	  ((beta-type-p type)
	   (if (not-bind-type-p type)
	       (match-partial-pos token betanode first-left-input)
	       (match-partial-pos (copy-token-or-marked
				   token) betanode first-left-input)))
	  (T
	   (ml-error :inappropriate-type (rete-node-type betanode))))))

(defun match-partial-pos (token betanode first-left-input)
  (do ((left-input first-left-input (cdr left-input)))
      ((null left-input) T)
;;    (when (blocked-node-p (car left-input))
;;      (store-partial-mem node token left-input)
;;      (return T))
    (dolist (left-mem (rete-shared-output (car left-input)))
      (left-beta-push-one token left-mem betanode))))

;;; Reset the output memory of the last alpha from (NEW ALL . Partial) to
;;; just the ALL list.
(defun reset-last-alpha-mem (betanode)
  (let ((alphanode (rete-node-right-input betanode)))
    (setf (rete-node-output-mem alphanode)
	  (cadr (rete-node-output-mem alphanode)))))

(defun left-beta-push-disj (rtoken node shared)
  (dolist (left-mem (shared-contents shared))
    (left-beta-push-one rtoken left-mem node)))


;;; Returns T iff the dis-num'th disjunct is true in *INSTANT*.
;;; Origin 1.
(defmacro $disj (dis-num &optional (or-num 1))
  `(let* ((parent (get-nth-parent-disj-node (instant-prod *INSTANT*) ,or-num))
	  (siblings (if parent (rete-node-right-output parent)))
	  (disjunct (if (> ,dis-num 0) (nth (1- ,dis-num) siblings))))
     (cond
      ((not parent)
       (rk-format T :no-or ,or-num
		  (rk-rule-pname (instant-prod *INSTANT*))))
      ((not disjunct)
       (rk-format T :no-disjunct
		  ,dis-num ,or-num
		  (rk-rule-pname (instant-prod *INSTANT*))))
      (T
       (eq disjunct (get-nth-top-disj (instant-token *INSTANT*) ,or-num))))))

(defmacro $disj-nums ()
  `(do* ((parent (get-parent-disj-nodes (instant-prod *INSTANT*)) (cdr parent))
	 (top-disj (marked-token-top-disj (instant-token *INSTANT*))
		   (cdr top-disj))
	 (res NIL res))
	((null parent) res)
     (let* ((siblings (if parent (rete-node-right-output (car parent))))
	    (disj-num (position (car top-disj) siblings)))
       (cond (disj-num (push (1+ disj-num) res))
	     (T
	      (ml-cerror :return-nil :not-in-rhs)
	      (return NIL))))))


;;; Finished 18-Mar-1986.
(defmacro build-rule (rname &rest slots)
  (setq *BUILDING* T)
  (eval `(RULE ,rname ,@slots))		;;Now we have *NEWNODES*
  (rk-format T :building)
  (let ((*TOKEN-POSITIVE* T))
    (push-newnodes))
  (setq *BUILDING* NIL)
  T)

;;; Non-macro form of build-rule. (added 4-Aug-86)
(defun build-rule* (rname lhs rhs)
  (setq *BUILDING* T)
  (rule* rname lhs rhs)
  (rk-format T :building)
  (let ((*TOKEN-POSITIVE* T))
    (push-newnodes))
  (setq *BUILDING* NIL)
  T)

(defun push-newnodes ()
  (dolist (newnode *NEWNODES*)
    (cond ((T-Or-Nnode-P newnode)
	   (push-all newnode))
	  ((ALPHAP newnode)
	   (push-to-alpha newnode))
	  ((BETAP newnode)
	   (push-to-beta newnode))
	  ((ABSNODEP newnode)
	   (push-to-beta newnode)))))


;;; Stops the rules from firing by taking their names out of the pnodeprod
;;; slot of the appropriate rete node (doesn't actually change the rete net -
;;; see excise-rule in build for that).
(defmacro inhibit-rules (&rest rnames)
  (mapc #'inhibit-rule rnames)
  T)

(defun inhibit-rule (rname)
  (let ((prod (get rname 'prod)))
    (cond (prod
	   (let ((pnode (rk-rule-pnode prod)))
	     (when pnode
	       (rk-format T :inhibiting-rule rname)
	       (delete-instants-of-rule prod)
	       (setf (rete-node-pnode-prod pnode)
		     (fast-del prod (rete-node-pnode-prod pnode) eq)))))
	  (T (ml-format T :no-such-rule rname)))))

(defmacro uninhibit-rules (&rest rnames)
  (mapc #'uninhibit-rule rnames)
  T)

(defun uninhibit-rule (rname)
  (let ((prod (get rname 'prod)))
    (cond (prod
	   (let ((pnode (rk-rule-pnode prod))
		 (*TOKEN-POSITIVE* T))
	     (rk-format T :uninhibiting-rule rname)
	     (do ((token (rete-node-output-mem pnode) (cdr token)))
		 ((atom token) (do-instant token prod))
	       (do-instant (car token) prod))
	     (if (not (rete-node-pnode-prod pnode))
		 (setf (rete-node-pnode-prod pnode) (list prod))
		 (push prod (rete-node-pnode-prod pnode)))))
	  (T (ml-format T :no-such-rule rname)))))


;;; Token operations

;;; Done this way because it's much faster than using make-token.
(defun token-cons (tokens ptoken)
  (let ((newt (copy-token-or-marked ptoken)))
    (setf (token-contents newt)
	  (cons (token-contents tokens) (token-contents ptoken)))
    (setf (token-match-count newt) 0)
    (setf (token-bindval newt)
	  (or (token-bindval ptoken) (token-bindval tokens)))
    newt))

;;; Makes a marked token if we're at a top disjunctive node; else makes a
;;; regular token.
(defun new-token (token ptoken node)
  (cond ((top-disjunctive-node-p node)
	 (let ((newt (copy-marked-token *MARKED-TOKEN*)))
	   (setf (marked-token-top-disj newt)
		 (if (> (length ptoken) 3)
		     (cons node (marked-token-top-disj ptoken))
		     (list node)))		 
	   (setf (marked-token-contents newt)
		 (cons (token-contents token) (token-contents ptoken)))
	   (setf (marked-token-match-count newt) 0)
	   (setf (marked-token-bindval newt)
		 (or (token-bindval ptoken) (token-bindval token)))
	   newt))
	(T (token-cons token ptoken))))

;;; If the given absence node is a top disjunctive node, then we need to make
;;; the token a marked shared-token.
(defun maybe-new-token (token node)
  (cond ((top-disjunctive-node-p node)
	 (let ((newt (copy-marked-token *MARKED-TOKEN*)))
	   (setf (marked-token-top-disj newt)
		 (if (> (length token) 3)
		     (cons node (marked-token-top-disj token))
		     (list node)))
	   (setf (marked-token-contents newt)
		 (token-contents token))
	   (setf (marked-token-match-count newt) 0)
	   (setf (marked-token-bindval newt)
		 (token-bindval token))
	   newt))
	(T token)))

;;; Conflict set operations

(defun change-cs (token node)
  (dolist (prod (rete-node-pnode-prod node))
    (do-instant token prod)))


;;; While backing up, don't add instantiations that weren't in the conflict
;;; set on that cycle (this is noted in *NON-DELETIONS*).
(defun do-instant (token prod)
  (if *TOKEN-POSITIVE*
      (if (not (and *BACKING-UP*
		    (member-if
		     #'(lambda (x)
			 (equal (token-contents x) (token-contents token)))
		     (svref *NON-DELETIONS* *MODCYCLE*))))
	  (let ((instant
		 (make-instant
		  :token token
		  :prod prod
		  :sortedwmes
		  (if *NO-LITERALIZE*
		      (copy-improper-list (token-contents token))
		      (sort (copy-improper-list (token-contents token))
			    #'> :key #'wme-%time)))))
	    (push instant *CONFLICT-SET*)
	    (incf (rk-rule-inscount prod))
	    (when (and (= (rk-rule-inscount prod) 1)
		       (get-bottom-disj-nodes prod))
	      (queue-added-instant instant)
	      (perform-blocking-and-unblocking))
	    (when *AGENDA-LOADED*
	      (agenda-add-instant prod instant))
	    (when *TRACE-MATCH*
	      (ml-format T :added-to-cs instant)
	      (print-disjunct-num instant :is-disjunct))
	    (when (> *RECORD-LEVEL* 1)		;; Back doesn't need this
	      (push instant (svref *PROD-MATCHES* *MODCYCLE*))
	      (store-matched instant))))
      (delete-instant token prod)))

(defun delete-instant (token prod)
  (when (rk-rule-beliefs prod)
    (un-believe-rule prod token))
  (let ((wmes (token-contents token)))
    (do ((prev NIL instants)
	 (instants *CONFLICT-SET* (cdr instants))
	 (instant (car *CONFLICT-SET*) (cadr instants)))
	((null instants)
	 (if (> *RECORD-LEVEL* 0)
	     (push token (svref *NON-DELETIONS* *MODCYCLE*))))
      (when (and (eq prod (instant-prod instant))
		 (eq wmes (token-contents (instant-token instant))))
	(decf (rk-rule-inscount prod))
	(when *AGENDA-LOADED*
	  (agenda-delete-instant prod instant))
	(when *TRACE-UNMATCH*
	  (ml-format T :removed-from-cs instant)
	  (print-disjunct-num instant :was-disjunct))
	(if (null prev) (setf *CONFLICT-SET* (cdr *CONFLICT-SET*))
	    (setf (cdr prev) (cddr prev)))
	(when (> *RECORD-LEVEL* 1)
	  (push instant (svref *PROD-UNMATCHES* *MODCYCLE*)))
	(when (and (rk-rule-disj-nodes prod)
		   (= (rk-rule-inscount prod) 0)
		   (some-blocked-bottom-node-p prod))
	  (queue-removed-instant instant)
	  (perform-blocking-and-unblocking))
	(return T)))))

(defun un-believe-rule (prod token)
  (let ((instant (find-fired-instant prod token)))
    (when instant	;; If it's NIL, then it hasn't fired.
      (setq *FIRED-INSTANTS* (delete instant *FIRED-INSTANTS* :count 1))
      (dolist (belief (cddr (rk-rule-beliefs prod)))	;; skip 'lambda ()'
	(undo-belief belief instant)))))

(defun undo-belief (action instant)
  (let ((undo-action (get-undo-action (car action))))
    (if undo-action
	(funcall undo-action action instant))))

(defun get-undo-action (action)
  (get action :undo-action))

(eval-when (load eval compile)
  (defmacro def-inverse-action (action inv-action)
    `(setf (get ',action :undo-action) ,inv-action)))

(def-inverse-action $remove-keep
		    #'(lambda (action instant)
			(add-frame (var-in-instant (cadr action) instant))))


(defun find-fired-instant (prod token)
  (car (member-if #'(lambda (instant)
		      (and (eq prod (instant-prod instant))
			   (eq (token-contents token)
			       (token-contents (instant-token instant)))))
		  *FIRED-INSTANTS*)))

;;; For excise-rule and inhibit-rule (added 29-Mar-87).
(defun delete-instants-of-rule (prod)
  (when (rk-rule-pnode prod)
    (when (and (rk-rule-disj-nodes prod)
	       (not (= (rk-rule-inscount prod) 0))
	       (some-blocked-bottom-node-p prod))
      (setf (rk-rule-inscount prod) 0)
      (let ((instant (car (member-if #'(lambda (ins)
					 (eq (instant-prod ins) prod))
				     *CONFLICT-SET*))))
	(when instant
	  (queue-removed-instant instant)
	  (perform-blocking-and-unblocking))))
    (let ((pnode (rk-rule-pnode prod)))
      (cond ((consp pnode)	;; In this case it's the last disjunctive node
	     (dolist (a-pnode pnode)
	       (dolist (token (shared-contents (rete-node-output-mem a-pnode)))
		 (un-believe-rule prod token))))
	    (T
	     (dolist (token (shared-contents (rete-node-output-mem pnode)))
	       (un-believe-rule prod token))))))
  (setq *CONFLICT-SET*
	(delete-if #'(lambda (ins) (eq (instant-prod ins) prod))
		   *CONFLICT-SET*))
  (when *AGENDA-LOADED*
    (agenda-delete-instants-of prod)))

;;; Ex: (a b . c) ==> (a b c)
(defun copy-improper-list (l)
      (do ((l l (cdr l))
	   (res NIL (cons (car l) res)))
	  ((atom l) (nreverse (cons l res)))))

(defun reverse-improper (l)
  (do ((l l (cdr l))
       (res NIL (cons (car l) res)))
      ((atom l) (cons l res))))


(defun show-var-vals (&optional (instant *INSTANT*))
  (let ((tokens (instant-token instant)))
    (dolist (var-access (rk-rule-left-access (instant-prod instant)))
      (and (cdr var-access)
	   (format T "~%Var ~A: ~A" (car var-access)
		   (funcall (cdr var-access) tokens)))))
  (terpri))

;;; returns (values <var-value> <boundp>)
(defun rk-var-status (varname)
  (multiple-value-bind (vals knownp)
		       (gethash varname *VAR-BINDING-HASH*)
    (values (car vals) knownp)))

;;; Returns the most recent value of the given variable name, usually (but not
;;; necessarily) in the instantiation which is currently firing.
(defmacro var (varname)
  (when *COMPILING-RHS*
      (if (not (assoc varname (rk-rule-left-access *COMPILING-RHS*)))
	  (ml-cerror :go-on :var-no-binding
		     varname (rk-rule-pname *COMPILING-RHS*))))
  `(car (gethash ',varname *VAR-BINDING-HASH*)))

;;; So that you can access the dynamic value of a variable from the LHS of
;;; a rule.
(defmacro oldvar (varname)
  (multiple-value-bind (x y) (gethash varname *VAR-BINDING-HASH*)
    (declare (ignore x))
    (if (not y)
	(ml-cerror :go-on :var-no-binding-1 varname))
    `(car (gethash ',varname *VAR-BINDING-HASH*))))

;;; Gives the variables from the given instantiation dynamic bindings by
;;; pushing their values onto a list of values they have in the var-binding
;;; hash table, which VAR uses to get the value.
;;; Modified 26-Mar-87 to go directly from the token in the instant,
;;; thus bypassing the intermediate varalist.  More efficient when instantiations
;;; are refracted, which is the most common thing.
(defun push-rhs-var-bindings (instant prod)
  (let ((tokens (instant-token instant)))
    (dolist (var-fn (rk-rule-left-access prod))
      (push (funcall (cdr var-fn) tokens)
	    (gethash (car var-fn) *VAR-BINDING-HASH*)))))

(defun pop-rhs-var-bindings (prod)
  (dolist (var-fn (rk-rule-left-access prod))
    (pop (gethash (car var-fn) *VAR-BINDING-HASH*))))

(defun assure-varname (var)
  (if (consp var) (cadr var) var))

;;; Also modified 26-Mar-87
(defun var-in-instant (varname instant)
  (let ((function (cdr (assoc (assure-varname varname)
			      (rk-rule-left-access (instant-prod instant))))))
    (if function
	(funcall function (instant-token instant))
	(ml-format T :no-such-variable varname))))

(defun set-rk-var (varname val)
  (push val (gethash varname *VAR-BINDING-HASH*)))

(defun pop-rk-var (varname)
  (pop (gethash varname *VAR-BINDING-HASH*)))

;;; Inefficient but only used in debugging commands.
(defun var-names-of (instant)
  (mapcar #'car (rk-rule-left-access (instant-prod instant))))

(defun instant-wmes-of (instant)
  (token-contents (instant-token instant)))

(defun prod-of (instant)
  (instant-prod instant))


;;; If the test returns true, then store the result of the test, along with the
;;; token that instantiated that test, in the memory slot of the node, and
;;; return t, else return NIL.
;;; Alphas have only one token in the instant field, whereas betas may have many.
(defun alpha-test-p (token node)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq (rete-node-function node) *ALPHA-TRUE-TEST*)
	 T)
	(T (funcall (rete-node-function node) token))))
;;	 (if *TRACE-NODE-TEST*
;;	     (ml-format T :token-tested-t token node))

;;; Alphas are always input from the left.
;;; Add the token if it's a positive one; otherwise delete it.
(defun do-token (token node)
  (cond (*TOKEN-POSITIVE*
	 (push token (rete-shared-output node)))
	(T
	 (delete-token token (rete-node-output-mem node)))))

;;; Does a token up at the beta node above blocked disjuncts, and for the
;;; last alpha node.  These memories are structured: (NEW ALL . Partial).
;;; Partial has the form: (token . node).
(defun do-new-all-disj-token (token outmem)
  (cond (*TOKEN-POSITIVE*
	 (push token (shared-contents (car outmem)))
	 (push token (shared-contents (cadr outmem))))
	(T
	 (delete-token token (car outmem))
	 (delete-token token (cadr outmem))
	 (if (and (cddr outmem)
		  (eq (token-contents (car (cddr outmem)))
		      (token-contents token)))
	     (setf (cddr outmem) NIL))
;;	 (release-frame token)
	 )))

(defun maybe-do-new-all-disj-token (token outmem node)
  (if (consp outmem)
      (do-new-all-disj-token token outmem)
      (do-token token node)))

;;; Does a token for the last blocked disjunct
;;; For (NEW . OLD) memory nodes, you don't add to the OLD list, but you
;;; do delete from it (since deletions can't be queued, and probably
;;; wouldn't make things more efficient anyway - you don't reverse deletions
;;; the way you can reverse an addition by deleting it).
(defun do-new-old-disj-token (token outmem)
  (cond (*TOKEN-POSITIVE*
	 (push token (shared-contents (car outmem))))
	(T
	 (delete-token token (car outmem))
	 (delete-token token (cdr outmem))
;;	 (release-frame token)
	 )))

;;; Token(s) coming from the left.
(defun delete-token (tokens shared)
  (setf (shared-contents shared)
	(fast-del (token-contents tokens) (shared-contents shared)
		  (lambda (wme ntoken)
		    (eq wme (token-contents ntoken))))))

(defun right-delete-token (tokens shared)
  (setf (shared-contents shared)
	(fast-del (token-contents tokens) (shared-contents shared)
		  (lambda (wme ntoken)
		    (eq wme (car (token-contents ntoken)))))))

(defun right-delete-token-push (tokens shared node)
  (setf (shared-contents shared)
	(fast-del (token-contents tokens) (shared-contents shared)
		  (lambda (wme ntoken)
		    (when (eq wme (car (token-contents ntoken)))
		      (if (pnodep node) (change-cs ntoken node))
		      (dolist (right-out (rete-node-right-output node))
			(right-beta-push-through ntoken right-out))
		      T)))))

(defun left-delete-token (tokens shared)
  (setf (shared-contents shared)
	(fast-del (token-contents tokens) (shared-contents shared)
		  (lambda (wme ntoken)
		    (eq wme (cdr (token-contents ntoken)))))))

(defun left-delete-token-push (tokens shared node)
  (setf (shared-contents shared)
	(fast-del (token-contents tokens) (shared-contents shared)
		  (lambda (wme ntoken)
		    (when (eq wme (cdr (token-contents ntoken)))
		      (if (pnodep node) (change-cs ntoken node))
		      (dolist (right-out (rete-node-right-output node))
			(right-beta-push-through ntoken right-out))
		      T)))))

;;; Added 23-Jun-87.  For deleting WMEs from memories of NOT nodes.
(defun delete-token-from-not (token mem)
  (fast-del (token-contents token) mem
	    (lambda (wme ntoken)
	      (eq wme (token-contents ntoken)))))

(defun delete-token-from-not-save (token mem)
  (fast-del (token-contents token) mem
	    (lambda (wme ntoken)
	      (and (eq wme (token-contents ntoken))
		   (setq *FIND-TOKEN* ntoken)))))


(defun compile-all ()
  (compile-left-accesses)
  (compile-rhses)
  (compile-nodes)
  T)

;;; Added 26-Mar-87
(defun compile-rhses ()
  (dolist (rname *RULE-NAMES*)
    (let ((rule (get rname 'prod)))
      (compile-rhs rule)
      (compile-beliefs rule))))

(defun compile-rhs (rule)
  (let ((rhs (and rule (rk-rule-rhs rule))))
    (cond ((null rhs)
	   (ml-cerror :go-on :no-rhs-to-compile (rk-rule-pname rule)))
	  ((not (compiled-function-p (rk-rule-rhs-fn rule)))
	   (setq *COMPILING-RHS* rule)
	   (setf (rk-rule-rhs-fn rule)
		 (compile NIL rhs))
	   (setq *COMPILING-RHS* NIL)))))

(defun compile-beliefs (rule)
  (let ((rhs (and rule (rk-rule-beliefs rule))))
    (cond ((null rhs)
	   T)
	  ((not (compiled-function-p (rk-rule-beliefs-fn rule)))
	   (setq *COMPILING-RHS* rule)
	   (setf (rk-rule-beliefs-fn rule) (compile NIL rhs))
	   (setq *COMPILING-RHS* NIL)))))


(defun compile-left-accesses ()
  (dolist (rname *RULE-NAMES*)
    (dolist (left-access (rk-rule-left-access (get rname 'prod)))
#+cltl1 (if (not (compiled-function-p (cdr left-access)))
	    (setf (cdr left-access) (compile NIL (cdr left-access))))
#+cltl2 (if (not (compiled-function-p (cdr left-access)))
	    (setf (cdr left-access)
		  (compile NIL (function-lambda-expression (cdr left-access))))
    ))))

(defun compile-nodes ()
  (setq *NODES-TRAVERSED* NIL)
  (compile-node *TOP-RETE-NODE*))

(defun compile-node (node)
  (when (not (memq node *NODES-TRAVERSED*))
    (maybe-compile node (rete-node-test node))
    (push node *NODES-TRAVERSED*)
    (if (listp (rete-node-left-output node))
	(dolist (subnode (rete-node-left-output node))
	  (compile-node subnode)))
    (if (listp (rete-node-right-output node))
	(dolist (subnode (rete-node-right-output node))
	  (compile-node subnode)))))

(defun maybe-compile (node test)
  (if (and (not (compiled-function-p test))
	   (not (atom test)))
      (setf (rete-node-test node)
	    (compile NIL test))))

;;; Reset the state of the interpreter to no declarative memory and therefore
;;; no matched rules.
(defun clear-net ()
  (declare (special *modify-demon-queue*))
  (setq *modify-demon-queue* NIL)	;; used by pre-modify & post-modify
  (re-init-trace)	;;from build.lisp
  (setq *CYCLE* 0
	*MODCYCLE* 0
	*ADDED-INSTANTS* NIL
	*REMOVED-INSTANTS* NIL)
  (clrhash *VAR-BINDING-HASH*)
;;  (dolist (class (inverse-isas 'wme))
;;    (setf (get class :instances) NIL))
  (when *AGENDA-LOADED*
    (agenda-clear-rules *RULE-NAMES*)
    (agenda-clear-buckets *AGENDA*))
  (dolist (instant *CONFLICT-SET*)
    (setf (rk-rule-inscount (instant-prod instant)) 0))
  (setq *CONFLICT-SET* NIL
	*FIRED-INSTANTS* NIL
	*performing-blocking* NIL
	*REMOVED-INSTANTS* NIL
	*ADDED-INSTANTS* NIL)
  (setq *INSTANT* *TOP-LEVEL-INSTANT*)
  (dolist (node (rete-node-right-output *top-rete-node*))
    (clear-net0 node))
  T)

;;; Clears the memory from all the nodes under node
;;; Resets all disjunctive nodes to not blocked state and not active state.
(defun clear-net0 (&optional (node *TOP-RETE-NODE*))
  (let ((type (rete-node-type node)))
    (when (or (blocked-type-p type) (active-type-p type)
	      (consp (rete-node-output-mem node)))
      (turn-off type *BLOCKED-BIT*)
      (turn-off type *ACTIVE-BIT*)
      (if (consp (rete-node-output-mem node))
	(setf (rete-node-output-mem node)
	      (car (rete-node-output-mem node))))
;;      (if (alpha-type-p type)
;;	(dolist (output (rete-node-left-output node))
;;	  (setf (rete-node-right-mem output)
;;		(rete-node-output-mem node))))
      (when (and (beta-type-p type) (not (t-or-nnode-p node)))
	(setf (rete-node-right-mem node)
	      (rete-node-output-mem (rete-node-right-input node)))
	(if (consp (rete-node-right-mem node))
	    (setf (rete-node-right-mem node)
		  (car (rete-node-right-mem node))))
	(if (and (not (realabs type))
		 (not (eq (rete-node-left-mem node) :NOT-SHARED)))
	    (setf (rete-node-left-mem node)
		  (rete-node-output-mem
		   (if (consp (rete-node-left-input node))
		       (car (rete-node-left-input node))
		       (rete-node-left-input node)))))))
    (when (and (anyabs type) (not (t-or-nnode-p node)))
      (if (not (eq (rete-node-left-mem node) :NOT-SHARED))
	  (setf (rete-node-left-mem node) NIL))
      (setf (rete-shared-right node) NIL))
    (when (rete-node-output-mem node)
      (setf (rete-shared-output node) NIL)
      (if (consp (rete-node-left-output node))
	  (dolist (subnode (rete-node-left-output node))
	    (clear-net0 subnode)))
      (dolist (subnode (rete-node-right-output node))
	(clear-net0 subnode)))
    T))



;;; Back.  The inverse of Run.  Backs the system up to previous states.
;;; Record-level must be at least 1 to do this.
(defun back (&optional (cycles 1))
  (cond ((> cycles *MAX-BACK*)
	 (ml-format T :only-stored *MAX-BACK*))
	((< *RECORD-LEVEL* 1)
	 (ml-format T :record-level *RECORD-LEVEL*))
	(T
	 (let ((*RECORD-LEVEL* 0)	;;Don't record while backing up
	       (*BACKING-UP* T))
	   (init-instant)
	   (dotimes (i cycles)
	     (back-one)
	     (un-refract-instant)
	     (when (and (not (svref *PROD-FIRINGS* *MODCYCLE*))
			(not (svref *PROD-FIRINGS*
				    (mod (1- *MODCYCLE*) *MAX-BACK*))))
	       (ml-format T :no-firings-before)
	       (return T))
	     (decf *CYCLE*)
	     (setq *MODCYCLE* (mod *CYCLE* *MAX-BACK*))
	     (if *TRACE-CYCLE* (ml-format T :cycle *CYCLE*)))))))


;;; Moves the system back one.  Removes all makes and makes all removes
;;; done on the current cycle number.
(defun back-one ()
  (let ((makes (svref *ADDITIONS* *MODCYCLE*))
	(removes (svref *DELETIONS* *MODCYCLE*))
	(modifies (svref *MODIFIES* *MODCYCLE*)))
    (setq *INSTANT* (car (svref *PROD-FIRINGS* *MODCYCLE*)))
    (setf (svref *ADDITIONS* *MODCYCLE*) NIL)
    (setf (svref *DELETIONS* *MODCYCLE*) NIL)
    (setf (svref *MODIFIES* *MODCYCLE*) NIL)
    (dolist (make makes)
      (let ((*PERFORMING-BLOCKING* :NO-UNBLOCKING))
	($remove-wme make NIL))
      (perform-blocking-and-unblocking))
    (dolist (modify modifies)
      (dup-array (cdr modify) (car modify)))	;; Courtesy of Parmenides
    (dolist (remove removes)
      ($add remove))))

;;; Find-token needs to be called to find the token stored in the pnode.
;;; We want to store the token in the pnode since so delete-instant can
;;; always do an eq test when trying to delete instantiations.
(defun un-refract-instant ()
  (let ((instant (svref *REFRACTIONS* *MODCYCLE*)))
    (when instant
      (setf (svref *REFRACTIONS* *MODCYCLE*) NIL)
      (let* ((token (instant-token instant))
	     (real-token (find-token-out (token-contents token)
					 (rk-rule-pnode (instant-prod instant))))
	     (rule (instant-prod *INSTANT*)))
	(setf (instant-token instant) real-token)
	(incf (rk-rule-inscount rule))
	(when (and (rk-rule-disj-nodes rule)
		   (= (rk-rule-inscount rule) 1))
	  (queue-added-instant instant)
	  (perform-blocking-and-unblocking))
	(when *AGENDA-LOADED*
	  (agenda-add-instant rule *INSTANT*))
	(push instant *CONFLICT-SET*)))))


;;; Printing and Reading utilities
(defvar *RETEPRINT* T)		;;controls how much of the rete it prints.
(defvar *FULL-WMEPRINT* NIL)		;;controls nested wme printing.

;;; Added 16-Mar-1988.  Writes wme, in FRulekit-readable form, to stream.
(defun save-wme (wme &optional (stream *STANDARD-OUTPUT*) (all-slots NIL))
  (cond ((wmep wme)
	 (if (symbolp wme)
	     (save-wme-instance (frame wme) wme stream all-slots)
	     (save-wme-instance  wme NIL stream all-slots)))
	(T (ml-format T :no-such-wme))))

;;; Pretty-Prints wme instances (arrays)
;;; wme must be an array, but not necessarily a frame
(defun save-wme-instance (wme name stream all-slots)
  (if (null wme)
      (ml-format stream :no-such-wme)
      (let ((snames (get (wme-%class wme) :wmesnames)))
	(if (not snames)		;;then it's not a FRulekit wme
	    (princ wme stream)
	    (write-frame wme name snames '$make-named stream
			 :savep T
			 :all-slots-p all-slots)))))

(defun pp-wme (wme &optional (stream *standard-output*))
  (if (framep wme)
      (let ((name (if (symbolp wme) wme))
	    (wme (assure-frame wme)))
	(setq *PRINTING-WME* T)
	(format stream "\[~S " (wme-%class wme))
	(if name (format stream "'~S " name))
	(when (wmep wme) (ml-format stream :time (floor (wme-%time wme))))
	(let ((wmenum 0))
	  (dolist (sname (or (get (frame-class wme) :wmesnames)
			     (cdr (get-slot-names (frame-class wme)))))
	    (incf wmenum)
	    (when (> wmenum 3) (format stream "~%") (setq wmenum 0))
	    (format stream " ~A:" sname)
	    (pp-wme-val (frk-generic-value wme sname) stream)))
	(when (wmep wme) (ml-format stream :creator)
	      (instant-printer (get-slot wme :%created) stream 0))
	(format stream "\]~%")
	(setq *PRINTING-WME* NIL)
	T)
      (ml-format T :no-such-wme)))

;;; A little bit slower than the PA get-generic-value, but more accurate.
(defun frk-generic-value (wme sname)
  (if (facetedp wme sname)
      (get-value wme sname)
      (get-slot wme sname)))

(defun pp-wme-val-list (stream val)
  (format stream "\(")
  (when (car val)
    (pp-wme-val (car val) stream)
    (do ((val (cdr val) (cdr val)))
	((atom val) (when val (princ " . " stream) (pp-wme-val val stream)))
      (princ " " stream)
      (pp-wme-val (car val) stream)))
  (format stream "\)"))

;;; Helper function for pp-wme
(defun pp-wme-val (val stream)
  (cond ((arrayp val)
	 (cond ((stringp val)
		(format stream "~S" val))
	       ((isa-instance val 'wme)
		(cond (*FULL-WMEPRINT*
		       (let ((*PRINT-CIRCLE* T))
			 (format stream " WME->~% ")
			 (pp-wme val stream)))
		      (T (format stream "{WME ~S}"
				 (wme-%class val)))))
	       ((framep val)
		(cond (*FULL-WMEPRINT*
		       (let ((*PRINT-CIRCLE* T))
			 (format stream " FRAME->~% ")
			 (pp-frame val :stream stream)))
		      (T (format stream "{FRAME ~S}"
				 (frame-class val)))))
	       (T (format stream "~S" (aref val 0)))))
	((listp val)
	 (pp-wme-val-list stream val))
	(T (format stream "~S" val))))

(defun pp-wmes (wmes &optional (stream *standard-output*))
  (terpri stream)
  (dolist (wme wmes)
    (pp-wme wme stream)))

(defun pp-wm (&optional (class NIL) (stream *standard-output*))
  (deal-with-wm class NIL NIL stream))

(defun pp-ith-wme (ith &optional (class NIL) (stream *standard-output*))
  (deal-with-wm class ith NIL stream))

(defun save-wm (&optional (class NIL) (stream *standard-output*))
  (deal-with-wm class NIL :save stream))

(defun save-ith-wme (ith &optional (class NIL) (stream *standard-output*))
  (deal-with-wm class ith :save stream))

(defun wm (&optional (class NIL) (stream *standard-output*))
  (deal-with-wm class NIL :return stream))

(defun ith-wme (ith &optional (class NIL) (stream *standard-output*))
  (deal-with-wm class ith :return stream))

(defun deal-with-wm (class ith return stream)
  (declare (special ith return stream))
  (let ((pos 1) res)
    (declare (special pos res))
    (cond (class
	   (deal-with-wmes-of-class class) res)
	  (T (dolist (node (rete-node-right-output *TOP-RETE-NODE*) res)
	       (if (tnodep node)
		   (deal-with-wmes-in-node node)
		   (deal-with-wmes-in-nnode node)))))))

;;; May result in some wmes being printed twice if there are wmes which are
;;; a member of two WME classes, both of which are tested through ANY tests.
(defun deal-with-wmes-in-nnode (node)
  (when node
    (if (or (not (gethash (rete-node-test node) *RETE-TEST-HASH*))
	    (null (rete-node-left-output node)))
	(dolist (token (rete-shared-output node))
	  (let ((wme (token-contents token)))
	    (if T;; (not (find-tnode wme))
		(deal-with-wme wme))))
	(deal-with-wmes-in-nnode (rete-node-left-output node)))))

(defun deal-with-wmes-in-node (node)
  (dolist (token (rete-shared-output node))
    (deal-with-wme (token-contents token))))

(defun deal-with-wme (wme)
  (declare (special ith pos return res stream))
  (cond (ith
	 (if (= pos ith)
	     (if (eq return :return)
		 (setq res wme)
		 (pp-num-wme wme pos stream return))))
	(T
	 (if (eq return :return)
	     (push wme res)
	     (pp-num-wme wme pos stream return))))
  (incf pos))

(defun pp-num-wme (wme pos stream return)
  (if *NUMBER-WMES* (format stream "~S: " pos))
  (cond ((eq return :save)
	 (save-wme wme stream)
	 (terpri stream))
	(T (pp-wme wme stream))))


;;; Because you can't specify print-funtions for arrays, and wmes are implemented
;;; as frames, which are implemented as arrays, instants are printed by
;;; the standard printer, which calls this printer, which has been modified to
;;; only print the rule name.
(defun instant-printer (instant stream depth)
  (declare (ignore depth))	;;depth doesn't work
  (cond ((symbolp instant)
	 (format stream " <~A>" instant))
	(instant
	 (format stream " ~A" (rk-rule-pname (instant-prod instant))))))

(defun print-instant (instant stream depth)
  (if *PRINTING-WME* (instant-printer instant stream depth)
      (let ((*standard-output* *terminal-io*)
	    (stream *standard-output*)
	    (*print-pretty* NIL))
	(when instant
	  (ml-format stream :instantiation
		  (rk-rule-pname (instant-prod instant)))
	  (print-disjunct-num instant :is-disjunct)
	  (ml-format stream :matched-wme)
	  (when (instant-token instant)
	    (format stream "~%  ")
	    (dolist (wme (reverse-improper (instant-wmes-of instant)))
	      (pp-wme wme stream)
	      (format stream "  ")))))))


(defun pp-instant (instant)
  (print-instant instant T 0))

;;; For marked-tokens.  Since marked-tokens point to rete nodes and rete
;;; nodes point to token (through memories), we mustn't print the rete node
;;; when printing the token (or an infinite loop will occur).
(defun token-printer (token stream depth)
  (declare (ignore depth))
  (format stream "~%   Contents: ~A" (token-contents token))
  (format stream "~%   Match-count: ~A" (token-match-count token))
  (if (token-bindval token)
      (format stream "~%   Bindval: ~A~%" (token-bindval token))))


(defun rete-node-printer (node stream depth)
  (declare (ignore depth))
  (setq *PRINT-DEPTH* 0)
;;  (setq *standard-output* *terminal-io*)
;;  (setq stream *standard-output*)
;;Just use the terminal-io stream because slisp re-binds *standard-output*
;;before it calls my print function to some temporary stream which it then
;;mis-manages and causes it to be printed incorrectly  (this only happens
;;after a lot of i/o has been done thru the user-defined print function).
  (if (eql *PRINT-DEPTH* 0) (setq *PRINTED-NODES* NIL))
  (if (eq *RETEPRINT* T)
      (let ((*RETEPRINT* NIL))		;; to avoid printing many at a time
	(shownode node stream))))

(defun shownode (node stream)
  (setf mystream stream)
  (dotimes (i (* *PRINT-DEPTH* 4)) (princ ">" stream))
  (ml-format stream :rete-node)
  (dotimes (i (* *PRINT-DEPTH* 4)) (princ ">"))
  (when (rete-node-pnode-prod node)
    (format stream " Pnodes:")
    (dolist (prod (rete-node-pnode-prod node))
      (format stream " ~A" (rk-rule-pname prod)))
    (terpri stream))
  (dotimes (i (* *PRINT-DEPTH* 4)) (princ ">" stream))
  (format stream " Test: ~%")
  (let ((*print-pretty* T))
    (princ "   " stream)
    (princ (rete-node-test node) stream) (terpri stream))
  (dotimes (i (* *PRINT-DEPTH* 4)) (princ ">" stream))
  (format stream " Function: ~%")
  (let ((*print-pretty* T))
    (princ "   " stream)
    (princ (rete-node-function node) stream) (terpri stream))
  (dotimes (i (* *PRINT-DEPTH* 4)) (princ ">" stream))
  (ml-format stream :type)
  (pp-type (rete-node-type node) node stream)
  (dotimes (i (* *PRINT-DEPTH* 4)) (princ ">" stream)) (terpri stream)
  (when NIL (rete-node-left-mem node)
    (ml-format stream :left-memory (rete-node-left-mem node))
    (dotimes (i (* *PRINT-DEPTH* 4)) (princ ">" stream)))
  (when  NIL (rete-node-right-mem node)
    (ml-format stream :right-memory (rete-node-right-mem node))
    (dotimes (i (* *PRINT-DEPTH* 4)) (princ ">" stream)))
  (when  NIL (rete-node-output-mem node)
    (ml-format stream :output-memory (rete-node-output-mem node))
    (dotimes (i (* *PRINT-DEPTH* 4)) (princ ">" stream)))
  (when (rete-node-slots node)
    (format stream " Slots: ~A~%" (rete-node-slots node))
    (dotimes (i (* *PRINT-DEPTH* 4)) (princ ">" stream)))
  T)

(defun pp-type (type node stream)
  (cond ((eq node *TOP-RETE-NODE*)
	 (format stream " TOP"))
	(T
	 (if (not (zerop (sbit type *ALPHABIT*))) (format stream " ALPHA"))
	 (if (and (zerop (sbit type *ALPHABIT*)) (not (tnodep node)))
	     (format stream " BETA"))
	 (if (not (zerop (sbit type *NOTBIT*))) (format stream " NOT"))
	 (if (tnodep node) (format stream " T-node"))
	 (if (not (zerop (sbit type *BINDBIT*))) (format stream " BIND"))
	 (if (not (zerop (sbit type *ORBIT*))) (format stream " <OR>"))
	 (if (top-disjunctive-node-p node)
	     (format stream " Top-Disj"))
	 (if (parent-disjunctive-node-p node)
	     (format stream " Parent-Disj"))
	 (if (not (zerop (sbit type *BLOCKED-BIT*)))
	     (format stream " BLOCKED"))
	 (if (not (zerop (sbit type *ACTIVE-BIT*)))
	     (format stream " ACTIVE")))))

;;; OPS-5 type predicates
;;; Don't even try to use <= or >= since the = read macro messes them up.
(defun <> (a b)
  (not (equal a b)))


;;; --------------------  WHYNOT Explanation module -------------------- ;;;
;;; Say, (whynot <pnames>) to get an explanation for why the given production
;;; names didn't fire.  Currently it only explains why rules didn't match, and
;;; doesn't explain the C.R. strategy.

(defmacro whynot (&rest pnames)
  (dolist (pname pnames)
    (explain pname)
    (terpri)))

(defun explain (pname)
  (let ((prod (get pname 'prod)))
    (cond ((not prod)
	   (ml-format T :no-such-rule pname))
	  ((inter-in-conflict-set-p (get pname 'prod))
	   (why-best-of *CONFLICT-SET* pname))	;;explain c.r. strategy
	  (T (www pname)))))			;;expain why it didn't match.

;;; Explain why the given production with name pname didn't fire, even though
;;; it matched.  This amounts to explaining the c.r. strategy.
(defun why-best-of (instants pname)
  (declare (ignore instants))
  (ml-format T :rule-wont-fire pname))

;;; Explain Which condition Was Wrong in the given rule.
(defun www (pname)
  (let ((nodes (get pname 'backpointers)))	;;Forgy's idea to keep these
    (cond
     ((null nodes)
      (ml-format T :rule-not-compiled pname))
     (T
      (do* ((node nodes (cdr node))
	    (ce 1 (1+ ce))
	    (mii (rete-generic-left (car nodes))
		 (and node (rete-shared-right (car node)))))
	   ((null node)
	    (ml-format T :rule-matched pname))
	(cond ((not mii)
	       (ml-format T :conde ce pname)
	       (format T "[i.e.: ~S]~%"
		       (nth (1- ce) (rk-rule-lhs (get pname 'prod))))
	       (ml-format T :not-match)
	       (return ce))
	      ((not (rete-shared-output (car node)))
	       (ml-format T :conde ce pname)
	       (format T "[i.e.: ~S]~%"
		       (nth (1- ce) (rk-rule-lhs (get pname 'prod))))
	       (ml-format T :match-but-var-binding)
	       (ml-format T :check-problem)
	       (return ce))))))))

(defun inter-in-conflict-set-p (prod)
  (member-if #'(lambda (ins)
		 (eq (instant-prod ins) prod))
	     *CONFLICT-SET*))

;;;; Matches module
(defmacro matches (pname &rest args)
  `(matches0 ',pname ,@args))

(defun matches0 (pname &key max (stream *STANDARD-OUTPUT*))
  (declare (special stream))
  (let ((nodes (get pname 'backpointers))
	ith return res)
    (declare (special ith return res))
    (cond
     ((null nodes)
      (ml-format stream :rule-not-compiled pname))
     ((null (cdr nodes))
      (ml-format stream :conde-1 1)
      (deal-with-tokens (rete-node-output-mem (car nodes))))
     (T
      (format stream "~%")
      (print-left-nums 1 stream)
      (deal-with-tokens (get-tokens (rete-node-output-mem (car nodes))))
      (print-matches (cdr nodes) 2 max stream)))))
;;      (print-left-nums (length nodes) stream)
;;    (deal-with-tokens (rete-node-output-mem (car (last nodes))))))))

(defun print-matches (nodes ce max stream)
  (do ((nodes nodes (cdr nodes))
       (ce ce (1+ ce)))
      (NIL)
    (cond ((consp (car nodes))		;; Disjunctive CE
	   (do ((cenum 1 (+ cenum 1))
		(disjunct (car nodes) (cdr disjunct)))
	       ((null disjunct)
		(format stream "~%"))
	     (ml-format stream :disjunctive (+ ce (/ cenum 10.0)))
	     (print-matches (car disjunct) (+ ce (/ cenum 10.0)) max stream)))
	  (T
	   (if (= ce (round ce))
	       (ml-format stream :conde-1 ce)
	       (ml-format stream :conde-2 ce))
	   (deal-with-tokens (rete-node-right-mem (car nodes)))
	   (let ((real-tokens (get-tokens (rete-node-output-mem (car nodes)))))
	     (when (shared-contents real-tokens)
	       (format stream "~%")
	       (print-left-nums ce stream)
	       (deal-with-tokens real-tokens)))))
    (when (or (null (cdr nodes)) (and max (<= max (1+ ce))))
      (terpri stream)
      (return T))))

(defun get-tokens (memory)
  (if (consp memory)
      (setq memory (cdr memory)))
  (if (consp memory)
      (setq memory (car memory)))
  memory)

(defun deal-with-tokens (tokens)
  (declare (special stream))
  (let ((pos 1))
    (declare (special pos))
    (dolist (contents (shared-contents tokens))
      (do ((contents (token-contents contents) (cdr contents)))
	  ((atom contents)
	   (deal-with-wme contents) T)
	(deal-with-wme (car contents)))
      (terpri stream))))

(defun print-left-nums (ce stream)
  (cond	((= ce 1)
	 (ml-format stream :conde-1 ce))
	(T
	 (format stream "~A" #\()
	 (if (= ce (round ce))
	     (dotimes (i (1- ce))
	       (format stream "~S & " (1+ i)))
	     (dotimes (i (1- (round ce)))
	       (format stream "~2,1F & " (+ (mod ce 1) 1 i))))
	 (if (= ce (round ce))
	     (format stream "~S~A:~%" ce #\))
	     (format stream "~2,1F~A:~%" ce #\))))))


;;;; rk-frame-and-slot-of.  Added 4-Sep-86.  Moved here from trace 12-Aug-1987.
;;;  Returns (values frame-of slot-of) the given varname in the given
;;; instantiation.  I.e., the first wme class and slot names that the
;;; given var appeared in.  slot-name is interned in the current package.
;;; Useful for debugging rules.
;;; Modified 18-Aug-87 to return the frame and not the class.
;;; rk-class-and-slot-of now returns the class and slot.
(defun rk-frame-and-slot-of (varname instant)
  (let ((wmes (instant-wmes-of instant)))
    (multiple-value-bind (condenum sname)
			 (conde-number varname instant)
      (cond ((null condenum)
	     (ml-cerror :return-nil-nil :var-not-in-production varname
		     (rk-rule-pname (instant-prod instant)))
	     (values NIL NIL))
	    (T
	     (values (nth (- (length wmes) condenum) wmes)
		     (assure-current sname)))))))

(defun rk-class-and-slot-of (varname instant)
  (multiple-value-bind (frame slot)
		       (rk-frame-and-slot-of varname instant)
    (values (frame-class frame) slot)))

;;; Returns (values condenumber sname).  condenumber is the origin-0 conde
;;; position of varname in the condition elements in the given instantiation.
;;; sname is the name of the slot corresponding to the varname in that conde.
(defun conde-number (varname instant)
  (let* (sname lastsname
	 (pos
	  (position-if
	   #'(lambda (conde)
	       (member-if #'(lambda (value)
			      (prog1
			       (and (rk-variablep value)
				    (eq varname (cadr value))
				    (setq sname lastsname))
			       (setq lastsname value)))
			  (cdr conde)))
	   (rk-rule-lhs (instant-prod instant)))))
    (values pos sname)))


;;; Finds the differences between different instantiations of the same rule
;;; name.
(defun d-in-instants (rname)
  (let ((cs (instants-of-rule (get rname 'prod))))
    (cond ((null cs)
	   (ml-format T :no-instantiations rname))
	  ((null (cdr cs))
	   (ml-format T :1-instantiations rname))
	  (T
	   (dolist (varname (var-names-of (car cs)))
	     (let ((firstval (var-in-instant varname (car cs)))
		   (insnum 1))
	       (dolist (ins (cdr cs))
		 (incf insnum)
		 (when (not (eq firstval (var-in-instant varname ins)))
		   (ml-cerror :try-next :var-different varname insnum)))))))))

;;; Compares the list of instantiations in cs with each other.
(defun c-instants (cs)
  (dolist (varname (var-names-of (car cs)))
    (let ((firstval (var-in-instant varname (car cs)))
	  (insnum 1))
      (dolist (ins (cdr cs))
	(incf insnum)
	(when (not (eq firstval (var-in-instant varname ins)))
	  (ml-cerror :try-next :var-different varname insnum))))))

(defun instants-of-rule (prod)
  (do ((ins *CONFLICT-SET* (cdr ins))
       (res NIL (if (eq (instant-prod (car ins)) prod)
		    (cons (car ins) res)
		    res)))
      ((null ins) res)))

(defun pp-wmes-of-class (class)
  (pp-wm class))

;;; Returns the wmes of the given class.
(defun deal-with-wmes-of-class (class)
  (let ((tnode (gethash class *RETE-TEST-HASH*))
	(nnode (gethash class *RETE-N-NODE-HASH*))
	res)
    (cond ((and (not tnode) (not nnode))
	   (if (not (get class :classp))
	       (ml-format T :not-wme-class class)
	       (ml-format T :no-rules-try-instances-of class)))
	  (T
	   (if tnode
	       (dolist (token (rete-shared-output tnode))
		 (deal-with-wme (token-contents token)))
	       (if nnode
		   (dolist (token (rete-shared-output nnode))
		     (if (eq (wme-%class (token-contents token)) class)
			 (deal-with-wme (token-contents token))))))
	   res))))

(defun verbose ()
  (setq *TRACE-ADD* T *TRACE-DELETE* T *TRACE-FIRE* T *TRACE-CYCLE* T
	*RULEKIT-TERSE* NIL))

(defun silent ()
  (setq *TRACE-ADD* NIL *TRACE-DELETE* NIL *TRACE-MATCH* NIL *TRACE-UNMATCH* NIL
	*TRACE-NODE-TEST* NIL)
  (setq
   *TRACE-FIRE* NIL *TRACE-CYCLE* NIL *FULL-WMEPRINT* NIL *RULEKIT-TERSE* T))


;;; Implementation-unspecific things.

;;; Returns T iff a is a pre-defined commonlisp type, in spice, symbolics or
;;; kyoto.  Each of these lisps has their own weird (wrong) behaviour
;;; with subtypep.
(defun correct-common-p (a)
  (cond ((kyoto-lisp-p)
	 (and (not (subtypep a 'wme))
	      (cadr (multiple-value-list
		     (subtypep a NIL)))))	;;Is Kyoto lisp sure?
	((cmu-common-lisp-p)
	 (and (not (subtypep a 'wme))
	      (subtypep a 'common)))))


#-(or :cmu lucid symbolics)
    (defmacro grindef (fname)
      (let ((*print-pretty* T))
	(if (not (fboundp fname))
	    (ml-format T :not-defined)
	    `(symbol-function ',fname))))

(init-inter)
