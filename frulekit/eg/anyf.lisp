;;; Rulekit program to demonstrate the new =! feature, which gives the user
;;; a way to say a certain WME is isa-instance another class.
;;; For example, =!person expands to =person (CHECK (isa-instance
;;; =person 'person))

(literalize person () (extendable nil cache (walks))
  walks (value 'yes)
  child ())

(literalize boy (is-a (person) extendable nil)
  age ()
  weight ())

(literalize punk (is-a (boy) extendable nil)
  band ())

#|
(RULE find-person
  :LHS ((=!person :walks yes (LABEL =p))
	(=!person :walks yes (LABEL =p1) (CHECK (<> =p =p1))))
  :RHS ((format T "Found any person:")
	(pp-wme =p)))
|#

(RULE find-walking-boy
  :LHS ((boy :age =a :walks yes (LABEL =b)))
  :RHS ((format T "Found walking boy ")
	(pp-wme =b)
	($make 'person :child `(value ,=b))))

;;Note that =!person matches a child since child isa person (by the
;;literalize).
(RULE find-person-with-child
  :LHS ((person :child =!person (LABEL =p))
	(boy :age =a :weight =a)
	(boy :age =a))
  :RHS ((format T "Found parent: ")
	(pp-wme =p)
	(format t "With child: ")
	(pp-wme =person)
	($remove =p)))

(defun begin ()
  (start
   (boy :age '(value 10) :weight '(value 10))))
