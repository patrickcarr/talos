;;; Rulekit TRACE package by Peter Shell.  Started 27-Jan-86. 

;;; Copyright (c) 1985, 1988 Carnegie Mellon University.
;;; To obtain a copy of this software, please contact:
;;;	Peter Shell
;;;	School of Computer Science
;;;	Carnegie Mellon University
;;;	Pittsburgh, PA  15213

;;;; -------------------------------------------------------------------- ;;;
;;;; PACKAGE STUFF							  ;;;
;;;;

(in-package "FRULEKIT" :use '("LISP" "PARMENIDES"))
;;; User-accessible trace functions, macros and variables documented in the
;;; user's manual.  The core FRulekit symbols are exported from build.lisp.

(export '(		;; FUNCTIONS & MACROS
  	   var-names-of var-in-instant instant-wmes-of prod-of
	   give-additions-on-cycle give-deletions-on-cycle give-matches
	   give-firings give-next-matches give-next-firings
	   give-matches-on-cycle give-firings-on-cycle give-named-matches
	   give-un-matches-on-cycle
	   give-named-firings lhs-of rhs-of prod-responsible-for
	   possible-prods-responsible-for in-wmp give-instant give-negs
	   rule-rhs-of

	;; Patty Cheng's additions
	   Give-firing-on-cycle Give-linear-trace Give-prod-name

	;; Def-frame accessors
	   Wme-%matches Wme-%firings Wme-%label
	 ))

(load-messages (format NIL "~Atr-messages.~A" *FR-PATHNAME* *LANGUAGE*))

;;; FIX: give-firing-on-cycle off by one
(proclaim '(simple-vector
	    *PROD-MATCHES*	;;List of instantiations added on cycle N.
	    *PROD-UNMATCHES*	;;List of instantiations removed on cycle N.
	    *PROD-FIRINGS*	;;List of fired instantiations for cycle N.
	    *ADDITIONS*
	    *DELETIONS*))
(proclaim '(special
	    *RECORD-LEVEL*	;;On iff the trace package is active.
	    *MAX-BACK*))

(defvar *MAX-BACK* 50)

(eval-when (load eval compile)
  (def-frame wme (setable :setf propagate NIL pre-if-set (pre-modify)
			   post-if-set (post-modify) cache :*ALL*)
  %time 0	  ;; time tag, saying when it was created
  %created TOP	  ;; pointer to production that added/deleted it
  %prod-matches T  ;;pointer to instantiation which wme is part of
  %prod-firings T  ;;pointer to fired instantiations which wme is a part of
  %label T	  ;;label of the corresponding conditon element that matched
   		  ;;the original wme.  Only applicable when the trace package
   		  ;;is loaded and the WME is made by a remove or modify.
 ))

;;Should only have to be called once.  re-init-trace is for re-initiailizing.
(defun init-trace ()
  (setf *RECORD-LEVEL* 2)		;; User probaly wants tracing activated.
  (defvar *PROD-MATCHES* (make-array *MAX-BACK* :initial-element nil))
  (defvar *PROD-UNMATCHES* (make-array *MAX-BACK* :initial-element nil))
  (ml-format T :tracing-activated)
)

(init-trace)

;;It saves conses to fill instead of do make-array.
(defun trace-re-init-trace ()
  (fill *PROD-MATCHES* nil)
  (fill *PROD-FIRINGS* nil)
  (fill *PROD-UNMATCHES* nil)
  T)

;;; ----------------- Tracing Productions ------------------ ;;;
;;All of the functions below assume that ?wme is a Rulekit wme, that is,

;;; a wme structure created by Rulekit.
;;; Used by functions which take a ?wme as input, and dispatches depending
;;; on whether the given ?wme is positive or negative.
(eval-when (load eval compile)
  (defmacro process-?wme (?wme fn)
    `(if (positive-pattern ,?wme)
	 (,fn (cdr ,?wme))
	 (if (negative-pattern ,?wme)
	     (ml-format T :only-positives-wmes ',fn)
	     (ml-error :wme-list ',fn ',?wme)))))


;;; Every time a production matches, it adds that match to the list of matches
;;; for every wme involved in the match.  Doesn't work with negative wmes.
;;; Returns a list of production instantiations which were placed
;;; in the conflict set because ?WME helped them to match.
(defun give-matches (?wme)
  (process-?wme ?wme wme-%prod-matches))

;;Every time a production fires, it adds itself to the list of prod-firings
;;for wme which helped it to match (except negative wmes).
;;; Returns a list of production instantiations which ?WME helped
;;; to match and which fired.
(defun give-firings (?wme)
  (process-?wme ?wme wme-%prod-firings))

(eval-when (load eval compile)

;;; Given a wme, return the list of instantiations which were created
;;; on the very next cycle after ?WME was added.
  (defmacro give-next-matches (?wme)
    (process-?wme ?wme matches-on-next-cycle))

;;; Given a wme, return the list of firings which were created on the
;;; very next cycle after ?WME was added.
  (defmacro give-next-firings (?wme)
    (process-?wme ?wme firings-on-next-cycle))

  (defmacro give-matches-on-cycle (cycle)
    `(svref *PROD-MATCHES* ,cycle))

  (defmacro give-un-matches-on-cycle (cycle)
    `(svref *PROD-UNMATCHES* ,cycle))

  (defmacro give-firings-on-cycle (cycle)
    `(svref *PROD-FIRINGS* ,cycle)))


(defun matches-on-next-cycle (wme)
  (svref *PROD-MATCHES* (1+ (wme-%time wme))))

(defun firings-on-next-cycle (wme)
  (svref *PROD-FIRINGS* (1+ (wme-%time wme))))

(defun give-named-matches (pname cycle)
  (mapcan #'(lambda (ins)
	      (and (eq pname (rk-rule-pname (instant-prod ins)))
		   (list cycle ins)))
	  (svref *PROD-MATCHES* cycle)))

;;; Firings are of the form (cycle-number instantiation)
(defun give-named-firings (pname cycle)
  (mapcan #'(lambda (ins)
	      (and (eq pname (rk-rule-pname (instant-prod ins)))
		   (list cycle ins)))
	  (svref *PROD-FIRINGS* cycle)))


(defun instantiated-lhs-of (instant)
  (instant-sortedwmes instant))

(defun lhs-of (pname)
  (rk-rule-lhs (get pname 'prod)))

;;; Modifed 26-Mar-87 to take the cddr since the car & cadr are lambda and nil.
(defun rhs-of (pname)
  (cddr (rk-rule-rhs (get pname 'prod))))

(defmacro rule-rhs-of (prod)
  `(cddr (rk-rule-rhs ,prod)))

;;; Returns those productions which the given conde
;;; partially matches.  Does this by looking at the Tnodes and alpha nodes.
(defun give-productions (&rest pattern)
  (if (negative-conde pattern)
      (find-matches-of-pattern (cadr pattern) nil)
      (find-matches-of-pattern (cadr pattern) t)))

;;; pos is T iff it's an positive pattern (not negated).
(defun find-matches-of-pattern (pattern pos)
  `(find-matches-of
    (make-token
     :contents
      (,(intern (concatenate 'string "MAKE-" (symbol-name (caar pattern))))
       :%time *CYCLE*
       :%created *INSTANT*
       :%class ',(caar pattern)
       ,@(cdar pattern))
     :tag ,pos)))	;;;; WON'T WORK

;;Find the nodes that token matches by testing the token against the T nodes
;;and any alpha nodes coming out of the T nodes.
(defun find-matches-of (token)
  (let ((tnodes
	 (mapcan
	  #'(lambda (tnode)
	      (and (alpha-test-p token tnode)	;;; WON'T WORK
		   (list tnode)))
	  (rete-node-right-output *TOP-RETE-NODE*))))
    (mapcan #'(lambda (node)
		(find-alpha-matches node token))
	    tnodes)))

;;; This won't work.  Re-implement by searching from the alpha node,
;;; through the net, to all the pnodes.
(defun find-alpha-matches (tnode token)
  (if (pnodep tnode) (list tnode)
      (mapcan #'(lambda (anode)
		  (and (alpha-test-p token anode)
		       (list (rete-node-name anode))))	;;;; WON'T WORK
	      (rete-node-right-output tnode))))


;;;  IMPLEMENTATION HELPER FUNCTIONS (ya gotta have 'em)
(defun negative-conde (conde)
  (eq (car conde) 'ABS))

(defun negative-pattern (pattern)
  (eq (car pattern) 'ABS))

(defun positive-pattern (pattern)
  (eq (car pattern) 'POS))


(defun store-matched (instant)
  (dolist (wme (instant-sortedwmes instant))
    (push instant (wme-%prod-matches wme))))

(defun store-fired (instant)
  (dolist (wme (instant-sortedwmes instant))
    (push instant (wme-%prod-firings wme))))


;;; ----------------- WME Creation ------------------ ;;;

;;Returns the prod. firing responsible for WME being created.
;;Only works for positive wmes.
(defun prod-responsible-for (?wme)
  (process-?wme ?wme wme-%created))

(defun possible-prods-responsible-for (?wme)
  (declare (ignore ?wme))
  (ml-error :not-implemented))


;;;; in-wmp
;;; Optimize this to use the Tnodes and Alpha nodes in the rete net,
;;; since if there is a node already made with the same test which
;;; this function would make, then it stores all the WMEs that passed
;;; the test.
;;; Returns a list of WMEs which match conde.  O[length(WM)].
(defun in-wmp (conde)
  (let ((test-fn (make-test-fn conde)))		;;from build.slisp.
    (mapcan #'(lambda (wme)
		(and (funcall test-fn wme)
		     (list wme)))
	      (wm (car conde)))))	;;returns all WMEs of class (car conde).

;;; This used to be in build but belongs here.
(defun make-test-fn (conde)
  (if (eq (car conde) 'ABS)
      (make-negative-test-fn (cadr conde))
      (make-positive-test-fn conde)))

(defun make-positive-test-fn (conde)
  `(lambda (x)
     (and
      ,@(make-alpha-tests (car conde) (cdr conde)))))

(defun make-negative-test-fn (conde)
  `(lambda (x)
     (not
      (and 
       ,@(make-alpha-tests (car conde) (cdr conde))))))

(defun make-alpha-tests (class conds)
  (do* ((conds conds (cddr conds))
	(test (maybe-make-test class conds) (maybe-make-test class conds))
	(res (and test (list test))
	     (if test (push test res) res)))
       ((null (cddr conds)) res)))

(defun maybe-make-test (class conds)
  (cond ((rk-variablep (cadr conds))
	 (ml-format T :ignoring-test (car conds) (cadr conds))
	 nil)
	(T (make-simple-alpha-test class (car conds) (cadr conds)))))

(defun make-simple-alpha-test (class slot value)
  (cond
   ((null slot) nil)
   ((numberp value)
    `(= (,(testify class slot) x)
	,value))
   ((constantp value)
    `(eq (,(testify class slot) x)
	 ,value))
   ((symbolp value)
    `(eq (,(testify class slot) x)
	 ',value))
   (T
    `(equal (,(testify class slot) x)
	    ',value))))


(eval-when (load eval compile)
  (defmacro give-additions-on-cycle (n)
    `(svref *ADDITIONS* ,n))

  (defmacro give-deletions-on-cycle (n)
    `(svref *DELETIONS* ,n)))

;;Firing is of the form (time pname), like what prod-responsible-for returns.
;;since firings are instantiations, this is useless.
;;(defun give-instant (firing)
;;;  (car (member (cadr firing)
;;;	       (svref *PROD-FIRINGS* (car firing)))))
(defun nth-pos-conde (num prod)
  (do* ((lhs (rk-rule-lhs prod) (cdr lhs))
	(i 1 (if (not (eq (car lhs) '<ABS>))
		 (1+ i) i)))
       ((= i num) (car lhs))
    (if (null lhs) (ml-error :only-condes i (rk-rule-pname prod) num))))


;;; -----------------  Additional trace code by Pat Cheng  ------------------ ;;;
;;; Some of this needs to be updated to work with the new FRulekit.

;;; returns the instantiation (for non-parallel systems) on cycle n
(eval-when (load eval compile)
  (defmacro give-firing-on-cycle (n)
    `(car (give-firings-on-cycle ,n))))

;;; returns firings (i.e. instantiations) from cycle "from" up to
;;; and including cycle "to"
(defun give-linear-trace (from to)
  (let (ans)
    (dotimes (k (+ 1 (- to from)) (nreverse ans))
      (setq ans (cons (give-firing-on-cycle (+ from k)) ans)))))

;;; returns the name of the production fired on cycle n
(defun give-prod-name (n)
  (rk-rule-pname (prod-of (give-firing-on-cycle n))))

;;; returns the first wme of a particular class in an instantiation
(defun give-wme-of-class (class inst)
  (some #'(lambda (wme) (if (eq class (wme-%class wme)) wme))
	(instant-wmes-of inst)))

;;; returns a list of all wmes of a particular class in an instantiation
(defun give-all-wmes-of-class (class inst)
  (mapcan #'(lambda (wme) (and (eq class (wme-%class wme))
			       (list wme)))
	  (instant-wmes-of inst)))
 
;;; returns a list of the 1st wme of a particular class in a list
;;; of instantiations (e.g. the list returned by give-linear-trace)
(defun give-trace-of-class (class lst)
  (mapcar #'(lambda (inst) (give-wme-of-class class inst)) lst))

;;; returns a list of lists of all the wmes of a particular class in a list
;;; of instantiations
(defun give-wmes-of-class-in-trace (class lst)
  (mapcar #'(lambda (inst) (give-all-wmes-of-class class inst)) lst))

;;; returns the names of the current-goal of a list of instantiations
(defun give-goals-of (lst)
  (mapcar #'current-goal-name (give-trace-of-class 'current-goal lst)))

;;; returns a list of the lhs of a list of instantiations
(defun give-trace-of-lhs (lst)
  (mapcar #'(lambda (inst) (rk-rule-lhs (prod-of inst))) lst))

;;; returns a list of the rhs of a list of instantiations
(defun give-trace-of-rhs (lst)
  (mapcar #'(lambda (inst) (rule-rhs-of (prod-of inst))) lst))

;;; Given a plist (eg a CONDE without the class), returns a list of variables in 
;;; the plist at any depth
(defun all-vars-in-list (lst)  
  (cond ((null lst) nil)
	((atom lst) nil)
	((rk-variablep lst) (list (cadr lst)))
	(T (nconc  (all-vars-in-list (car lst))
		   (all-vars-in-list (cdr lst))))))


;;; Given a plist (eg a CONDE without the class), returns a list of variables
;;; in the slots of the plist, excluding variables in CHECKs, BINDs and [slot].
;;; Should this include the vars in LABEL commands?
;;; Revised 29-June-87 to return only the var name and not wrap it around (var ..).
(defun vars-in-list (conds)
  (let (res cond)
    (loop
     (setq cond (car conds))
     (cond ((listp cond)
	    (setq conds (cdr conds)))
	   ((eq cond *LEFT-BRACKET*)
	    (setq conds (cdddr conds))
	    (if (eq (car conds) *RIGHT-BRACKET*)
		(setq conds (cdr conds))))
	   (T
	    (setq conds (cdr conds))
	    (if (rk-variablep (car conds))
		(push (cadr (car conds)) res))
	    (setq conds (cdr conds))))
     (if (null conds) (return res)))))
