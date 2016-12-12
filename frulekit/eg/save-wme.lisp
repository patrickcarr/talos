#|
Received: from K.GP.CS.CMU.EDU by ML.RI.CMU.EDU;  7 Apr 88 13:07:12 EDT
Received: from K.GP.CS.CMU.EDU by K.GP.CS.CMU.EDU;  7 Apr 88 13:05:01 EDT
To: pshell@ml
Subject: Serious bug in SAVE-WME
Date: Thu, 07 Apr 88 13:04:54 EDT
Message-ID: <2448.576435894@K.GP.CS.CMU.EDU>
From: Paul.Birkel@K.GP.CS.CMU.EDU
Status: R


Peter;

	Load the following into a clean environment:

****************************************************************
|#
(literalize patient (:cache *ALL*)
  name nil
  patient-id nil
  database (:value nil)
  patient-name (:last nil :first nil)
  demographics (:birthdate nil :sex nil :race nil)
  date-in-first-week (:value nil)
  date-in-last-week (:value nil)
  status (:value nil)
  loaded (:value nil))

(literalize event (:cache *ALL*)
  name nil
  patient-id nil
  when (:value 0 :date nil :time 0)
  meal (:value nil)
  bg (:value nil :hyperglycemia nil :hypoglycemia nil)
  urine (:sugar nil :acetone nil)
  longact (:value nil)
  dose (:value nil)
  mod (:value nil :original nil :diff-explained-by nil)
  comment
    (:value nil :duration nil :state nil
	    :parsed nil
	    :explain-missing-bg nil :explain-extra-bg nil
	    :reaction-degree nil :treated-reaction 'unknown
	    :explain-dose-change nil :dose-type 'regular :dose-adjustment 0
	    :explain-calorie-change nil :meal-timing nil
	    :activity-type nil :activity-degree nil
	    :calorie-type nil :calorie-degree nil :high-glycemic-index nil
	    :physiology-type nil :physiology-degree nil
	    :emotion-type nil :emotion-degree nil)
  reg-mod
    (:value nil :evaluated nil
	    :summary-list nil :plausible-list nil
	    :rebound nil
	    :increased-activity nil
	    :increased-activity-delayed nil :increased-activity-concurrent nil
	    :decreased-activity nil
	    :decreased-activity-delayed nil :decreased-activity-concurrent nil
	    :increased-calorie nil :increased-calorie-delayed nil
	    :decreased-calorie nil
	    :immediate-ketonuria nil :preceding-ketonuria nil
	    :illness nil :illness-concurrent nil
	    :menses nil :menses-concurrent nil
	    :anger nil :anger-concurrent nil
	    :joy nil :joy-concurrent nil
	    :excitement nil :excitement-concurrent nil
	    :grief nil :grief-concurrent nil
	    :anxiety nil :anxiety-concurrent nil
	    :snacking nil :admin-error nil
	    :asleep nil :unknown nil)
  LA-mod
    (:value nil :evaluated nil
	    :summary-list nil :plausible-list nil
	    :increased-activity nil
	    :decreased-activity nil
	    :snacking nil
	    :misc nil :unknown nil)
  calorie-mod
    (:value nil :evaluated nil
	    :summary-list nil :plausible-list nil
	    :reaction nil
	    :increased-activity nil)
  problems (:value nil))

(def-frame treatable-event (:is-a (event) :cache *ALL*)
  prev-meal-event (:value nil)
  next-meal-event (:value nil)
  reg-mod (:value 0)
  LA-mod (:value 0))

(literalize meal-event (:is-a (treatable-event) :cache *ALL*)
  pre-prev-meal-events (:value nil)
  pre-meal-events (:value nil)
  post-meal-events (:value nil)
  ;
  prev-day-same-meal (:value nil)
  next-day-same-meal (:value nil)
  prev-week-same-meal (:value nil)
  next-week-same-meal (:value nil))

($make-named 'patient 'patient-6 :name 'patient-6 :patient-id 6
   :patient-name '(:last Reed :first Valerie)
   :demographics '(:birthdate Oct-17-62 :sex F :race W)
   :date-in-first-week '(:value Feb-23-87)
   :date-in-last-week  '(:value Mar-20-87)
   :loaded '(:value t))

($make-named 'meal-event 'event-545 :name 'event-545 :patient-id 6
   :when '(:value 2750072400 :date Feb-23-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 112)
   :urine '(:sugar 2% :acetone Neg)
   :longact '(:value |1.1 CSI|)
   :dose '(:value 5) :mod '(:value 0)
   :comment '(:value (no milk #\; sinus headache #\; busy around house)
              :activity-type increased
              :activity-degree mild
              :calorie-type decreased
              :calorie-degree mild
              :physiology-type illness
              :physiology-degree mild)
   :pre-prev-meal-events '(:value nil)
   :prev-meal-event '(:value nil) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-546)
   :prev-day-same-meal '(:value nil) :next-day-same-meal '(:value event-549)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-576))
#|

****************************************************************

	Big suckers, huh?

	Anyway, now try (save-wme 'patient-6): this does the right thing
	(i.e. it generates a ($make-named ......) and returns NIL.

	Now try (save-wme 'event-545): this generates the array implementing
	the frame, and then returns the same!

	For what it's worth, in a larger context of code having loaded
	rules first, the former complains that 'patient-6 isn't a WME,
	yet (isas 'patient-6) says it is. And when the stream is a file
	(save-wme 'event-545) generates a (make-frame .....)!

	Maybe if you fix the underlying (save-wme) problem the "isn't a WME"
	will go away?

paul

|#
