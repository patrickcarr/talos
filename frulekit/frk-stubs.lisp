;;; NOTE: This file is being edited by Peter Shell at Thu Feb  6 16:12

;;; Before editing this file, sign it out by putting your name and
;;; the date at the top of this file, then write it out to disk.

;;;;								;;;;
;;;; 	     FRULEKIT STUB FILE FOR THE FLAGGER MODULE		;;;;
;;;;								;;;;

(in-package "FRULEKIT" :nicknames '("FRK") :use '("LISP" "PARMENIDES"))

;;; For the version of CRESUS which does not use the Flagger,
;;; we can make CRESUS much more efficient by not loading FRulekit,
;;; since we only use FRulekit when we use the Flagger module.
;;; Thus, this file provides the stubs for the FRulekit functions
;;; that are used.
;;;
;;; Those functions are:
;;; $fast-make, $make, $modify, $remove-keep, literalize, run, rule,
;;; add-frame, clear-net, init-rete
;;;
;;; Since we are retaining all Parmenides functionality, and some of
;;; these stub functions call Parmenides functions, the stub functions
;;; can't just do nothing - they have to call the appropriate Parmenides
;;; functions.  For example, 'literalize' without the FRulekit functionality
;;; is the same as 'def-frame'.

(export '($Fast-Make $Make $Modify $Remove-Keep Literalize Run
	  Rule Add-Frame Clear-Net Init-Rete In-Wm-P <>
	  *TRACE-ADD* *TRACE-DELETE* *TRACE-CYCLE* *TRACE-FIRE*
	  *RULEKIT-TERSE* *RECORD-LEVEL* *CONFLICT-SET* def-inverse-action))

(defvar *CONFLICT-SET* NIL)
(defvar *TRACE-ADD* NIL)
(defvar *TRACE-DELETE* NIL)
(defvar *TRACE-CYCLE* NIL)
(defvar *TRACE-FIRE* NIL)
(defvar *RULEKIT-TERSE* T)
(defvar *RECORD-LEVEL* 0)


(def-frame* 'wme () ())

;;; $Fast-Make
(defun $fast-make (class &rest slots-values)
  (make-frame0		;;Parmenides function.
   class NIL
   `(,@slots-values)))

;;; $Make
(defun $make (class &rest slots-values)
  (make-frame0		;;Parmenides function.
   class NIL
   `(,@slots-values)))

;;; $Modify
(defun $modify (wme &rest newslots)
  (setq wme (assure-frame wme))
  (modify-frame wme newslots)	;;Parm modify-frame changes desired slots
  wme)


;;; $Remove-keep
(defun $remove-keep (&rest wmevars)
  (declare (ignore wmevars))
  T)


;;; Literalize
(defmacro literalize (classname cplist &rest plist)
  (keywordize-cplist cplist)
;;  (if (not (memq :setable cplist))	  ;;over-ride inheritance of WME
;;      (setq cplist (nconc cplist (copy-list '(:setable NIL)))))
  `(def-frame ,classname ,cplist ,@plist))

;;; This isn't called directly by user code but compiled code which
;;; contain literalizes have this function compiled into them since
;;; the full Literalize does make-wmesnames.  So providing this fn
;;; avoids having to re-compile all code when using this stub module.
(defun make-wmesnames (classname)
  (declare (ignore classname))
  NIL)

;;; Run
(defun run (&optional (cycles))
  (declare (ignore cycles))
  T)

;;; Rule 
(defmacro rule (rname &rest slots)
  (declare (ignore slots))
  `(progn
    (format T "~%[Ignoring rule ~S since FRulekit is not loaded.]~%"
     ',rname)
    T))

(defmacro def-inverse-action (action inv-action)
  `(setf (get ',action :undo-action) ,inv-action))

;;; Add-Frame
(defun add-frame (frame)
  frame)

;;; Clear-Net
(defun clear-net ()
  T)

;;; Init-Rete
(defun init-rete ()
  T)

(defun in-wm-p (frame)
  (declare (ignore frame))
  T)

(defun <> (a b)
  (not (equal a b)))

(defun pre-modify ()
  )

(defun post-modify ()
  )
