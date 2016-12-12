;;; FRulekit program to demonstrate the propagation ability of PARMENIDES
;;; together with the pre-set and post-set demons.

(use-package 'parmenides)
(use-package 'frulekit)

;;; FRulekit by default makes classes not propagatable.
(literalize person (:propagate T :cache :*ALL*)
  :walks (:value 'yes))

(literalize boy (is-a (person))
  :age (:value 'yes))

(defun init-testnet ()
  (set-facet 'person :walks :value 'yes))

(RULE find-walking-boy
  :LHS (
   (boy :age =a :walks yes (LABEL =b)))
  :RHS (
   (format T "Found walking boy ")
   (pp-wme =b)
   (set-facet-demons 'person 'walks 'value 'no)))

(RULE find-non-walking-boy
  :LHS (
   (boy :walks no (LABEL =b)))
  :RHS (
   (format T "Found non-walking boy ")
   (pp-wme =b)
   ($remove =b)))

(defun begin ()
  (init-testnet)
  (start
   (boy :age '(:value 10))))
