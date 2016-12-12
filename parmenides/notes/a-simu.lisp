;;Received: from A.GP.CS.CMU.EDU by ML.RI.CMU.EDU; 29 Apr 87 10:51:18 EDT
;;Date: Wed, 29 Apr 87 10:50:45 EDT
;;From: Guojun.Zhang@A.GP.CS.CMU.EDU
;;To: pshell@ml

; 	A-simu.lisp is a Parmenides simulation implementing the agenda model. 
; 	This simulation consists of two types of frames (process and agenda)
; and a set of demons which trigger the interaction between the frames and
; approach proper control with help of agandas.

; 	Problems
;  -------------------------------------------------------------------
; 1) There are bugs with command SET-FACET-DEMONS.
; 1) A demon does not seem to be able to trigger more than one successor
;    frames at the same time. Use of agendas might be helpful.
; 1) There are bugs with function INVERSE-ISAS.

; 	create an agenda frame
;  -------------------------------------------------------------------
(def-frame agenda ()
  prcs-list (value '(INPUT ARCHPLAN SPACER  HIRISE-1 SPEX-1 ANALYZER SPEX-2
	     FOOTER-1 FOOTER-2 PLANEX-1 PLANEX-2 PLANEX-3) post-if-set 
             '(**prcs-order))
  completed-prcs ()
  current-prcs ()
  why-fire ())

; 	create process frames
;  -------------------------------------------------------------------
(def-frame INPUT (propagate t cache (pass))
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(Input N) post-if-set '(**c-output))
  output (value '(BldgPrmtr N) post-if-set '(**c-switch))
  pass (value nil))

(def-frame ARCHPLAN (is-a (INPUT)) 
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(BldgPrmtr N) post-if-set '(**c-output))
  output (value '(GlobalFunPlan N Grid N)post-if-set '(**c-switch)))

(def-frame SPACER (is-a (ARCHPLAN)) 
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(GlobalFunPlan N Grid N) post-if-set '(**c-output))
  output (value '(LocalFunPlan N) post-if-set '(**c-switch)))

(def-frame HIRISE-1 (is-a (ARCHPLAN)) 
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(Grid N) post-if-set '(**c-output))
  output (value '(StructPlan N) post-if-set '(**c-switch)))

(def-frame SPEX-1 (is-a (HIRISE-1)) 
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(StructPlan N) post-if-set '(**c-output))
  output (value '(StructPrelDsgn N) post-if-set '(**c-switch)))

(def-frame ANALYZER (is-a (SPEX-1)) 
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(StructPrelDsgn N) post-if-set '(**c-output))
  output (value '(StructAnalysis N) post-if-set '(**c-switch)))

(def-frame SPEX-2 (is-a (ANALYZER)) 
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(StructAnalysis N) post-if-set '(**c-output))
  output (value '(StructFinalDsgn N) post-if-set '(**c-switch)))

(def-frame FOOTER-1 (is-a (SPEX-1)) 
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(FoundPlan N) post-if-set '(**c-output)) 	 
  output (value '(FoundDesign N) post-if-set '(**c-switch)))

(def-frame FOOTER-2 (is-a (FOOTER-1)) 
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(FoundPlan N) post-if-set '(**c-output))
  output (value '(FoundDesign N) post-if-set '(**c-switch)))

(def-frame PLANEX-1 (is-a (SPEX-2 FOOTER-2)) 
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(StructFinalDsgn N FoundDesign N) post-if-set '(**c-output))
  output (value '(ConstrAct N) post-if-set '(**c-switch)))

(def-frame PLANEX-2 (is-a (PLANEX-1)) 
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(ConstrAct N) post-if-set '(**c-output))
  output (value '(ConstrTech N) post-if-set '(**c-switch)))

(def-frame PLANEX-3 (is-a (PLANEX-2)) 
  status (value 'N switch 0 post-if-set '(**c-input))
  input (value '(ConstrTech N) post-if-set '(**c-output))
  output (value '(ConstrSchedule N) post-if-set '(**c-switch)))

; 	user defined functions
;  -------------------------------------------------------------------

; If the value of switch facet of the status slot in a process frame is
; changed either from 0 to 1 or from 1 to 0, **c-input is triggered to change
; the input messages' values into P
(defun **c-input ()
  (declare (special frame))
    (let ((old-in (get-facet frame 'input 'value)))
      (set-facet-demons frame 'input 'value (all-P-out old-in))))

; If all messages in the input slot of the current process frame have value
; P, **c-output is triggered to modify all output messages' values into P 
(defun **c-output ()
  (declare (special frame))
    (let ((in (get-facet frame 'input 'value))
	  (out (get-facet frame 'output 'value)))
      (if (all-P-in in)
        (set-facet-demons frame 'output 'value (all-P-out out)))))

(defun all-P-in (x)
  (cond ((null x) t)
	((eq (cadr x) 'P) (all-P-in (cddr x)))
        (t nil)))

(defun all-P-out (x)
  (cond ((null x) nil)
	((eq (cadr x) 'P)
         (append (list (car x) (cadr x)) (all-P-out (cddr x))))
	((not (eq (cadr x) 'P)) 
         (append (list (car x) 'P) (all-P-out (cddr x))))
        (t (format t "#### there is a bug ####~%"))))
;         (format t "** changed ~S into P **~%" (cadr x)))

; If all messages in the output slot of the current process frame have value
; P, **c-switch is triggered to modify the value of switch facet of status
; slot of related process frames from 0 to 1 or from 1 to 0
(defun **c-switch ()
  (declare (special frame))
    (let ((out (get-facet frame 'output 'value))
          (new-frames (inverse-isas frame)))
      (if (all-P-in out)
      (format t "#### **c-switch worked ####~%")
      (c-switch new-frames))))

(defun c-switch (x)
  (cond ((null x) nil)
        (t (0-1 (car x)) 
      (format t "#### c-switch worked ####~%")
(c-switch (cdr x)))))

(defun 0-1 (x)
  (cond ((eq (get-facet x 'status 'switch) 0)
         (set-facet-demons x 'status 'switch 1))
        (t (set-facet-demons x 'status 'switch 0))))

; utility functions
;  -------------------------------------------------------------------
(defun xx (x)
  (cond ((null x) nil 'hi)
        (t (print (car x))
      (format t "#### xx worked ####~%")
 (xx (cdr x)))))


