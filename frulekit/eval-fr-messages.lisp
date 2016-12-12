(ml-format T :no-such-rule 'rname)
(ml-format T :obsolete 'slot)
(ml-format T :slot-not-defined 'slot 'class)
(ml-format T :sharing-alpha-test 'test)
(ml-format T :sharing-beta-test 'test)
(ml-format T :not-completely-excised)
(ml-format T :seconds 10)
(ml-format T :legal-breakpoint-values)
(ml-format T :no-such-rule 'rname)
(ml-format T :cycle *CYCLE*)
(ml-format T :no-cr-strategies)
(ml-format T :cr-strategy 'cr-strategy)
(ml-format T :in-the 10)
(ml-format T :firing-production 'pname)
(ml-format T :with-values)
(ml-format T :breakpoint-before 'pname)
(ml-format T :breakpoint-after 'pname)
(ml-format T :adding-wme)
(ml-format T :remove-no-productions)
(ml-format T :removing-wme)
(ml-format T :modifying-wme)
(ml-format T :new-slots 'newslots)
(ml-format T :no-such-rule 'rname)
(ml-format T :no-such-rule 'rname)
(ml-format T :added-to-cs 'instant)
(ml-format T :removed-from-cs 'instant)
(ml-format T :token-tested-t 'token 'node)
(ml-format T :only-stored *MAX-BACK*)
(ml-format T :record-level *RECORD-LEVEL*)
(ml-format T :no-firings-before)
(ml-format T :cycle *CYCLE*)
(ml-format T :no-such-wme)
(ml-format t :no-such-wme)
(ml-format t :time 10)
(ml-format t :creator)
(ml-format T :no-such-wme)
(ml-format t :instantiation 'pname)
(ml-format t :matched-wme)
(ml-format t :rete-node)
(ml-format t :type)
(ml-format t :left-memory 'left-memory)
(ml-format t :right-memory 'right-memory)
(ml-format t :output-memory 'output-memory)
(ml-format T :no-such-rule 'pname)
(ml-format T :rule-wont-fire 'pname)
(ml-format T :rule-not-compiled 'pname)
(ml-format T :rule-matched 'pname)
(ml-format T :conde 10 'pname)
(ml-format T :not-match)
(ml-format T :conde 10 'pname)
(ml-format T :match-but-var-binding)
(ml-format T :check-problem)
(ml-format t :rule-not-compiled 'pname)
(ml-format t :conde-1 1)
(ml-format t :disjunctive 10)
(ml-format t :conde-1 9)
(ml-format t :conde-2 8)
(ml-format t :conde-1 'ce)
(ml-format T :no-instantiations 'rname)
(ml-format T :1-instantiations 'rname)
(ml-format T :not-wme-class 'class)
(ml-format T :no-rules-try-instances-of 'class)
(ml-format T :not-defined)
(ml-format T :rule-tested 'rul 'loc-test)
(ml-format T :rule-applied 'rul '!value)
(ml-format t :contents-1 'contents)
(ml-format t :instantiations-1 10)
(ml-format T :instantiations-2 10 'bname)
(ml-format T :agenda-ptr 'agenda-ptr)
(ml-format T :bucket-ptr 'bucket-ptr)
(ml-format T :rules-tested
      '!count-test '!count-act 10)
(ml-format T :adding-to-bucket 'rnames 'bspec)
(ml-format T :deleting 'delst)
(ml-format T :illegal-agenda-pos 10)
(ml-format T :adding-bucket 'name 'spec)
(ml-format T :agenda-pos-not-found 'spec)
(ml-format T :deleting-buckets 'bucket-names)
(ml-format T :rule-removing 'rul 'bname)
(ml-format T :tracing-activated)
(ml-format T :only-positives-wmes 'fn)
(ml-format T :ignoring-test 'test 'test1)
(frk::rk-format T :build-initialized)
(frk::rk-format T :recompiling-rule 'rname)
(frk::rk-format T :compiling-rule 'rname)
(frk::rk-format T :recompiling-rule 'rname)
(frk::rk-format T :compiling-rule 'rname)
(frk::rk-format T :lisp-check-any-variables 'conde)
(frk::rk-format T :isomorphic 'pname
		   'pname1)
(frk::rk-format T :no-such-rule 'rule)
(frk::rk-format T :rule-not-completely-compiled)
(frk::rk-format T :excised-rule 'rname)
(frk::rk-format T :interpreter-initialized)
(frk::rk-format T :halt-r)
(frk::rk-format T :halt-go)
(frk::rk-format T :halt-cont)
(frk::rk-format T :halt-ok)
(frk::rk-format T :halting)
(frk::rk-format T :no-productions)
(frk::rk-format T :no-rules-class 'class)
(frk::rk-format T :no-or 3 'pname)
(frk::rk-format T :no-disjunct 3 2 'pname)
(frk::rk-format T :building)
(frk::rk-format T :building)
(frk::rk-format T :inhibiting-rule 'rname)
(frk::rk-format T :uninhibiting-rule 'rname)
(ml-cerror :continue-compilation :first-not-abs)
(ml-cerror :ignore-conde :not-nested-or)
(ml-cerror :ignore-negation :not-negated-conjunctions 'pname)
(ml-cerror :keep-compiling :not-a-class 'class)
(ml-cerror :ignore-lisp-var :cant-put 'value)
(ml-cerror :ignore-command :illegal-special-cmd
				 'carconds 'slot)
(ml-cerror :keep-compiling :expected-closing 'conde)
(ml-cerror :ignore-bind :only-16-bind 'varname)
(ml-cerror :ignore-bind :illegal-bind 'varname)
(ml-cerror :ignore-bind :illegal-bind 'varname)
(ml-cerror :ignore-mbind :only-16-bind 'varnames)
(ml-cerror :keep-compiling :var-no-binding-1 'varname)
(ml-cerror :go-on :slot-facet-not-in-frame
		 'slot 'facet 'class)
(ml-cerror :go-on :no-slot-in-class 'slot 'class)
(ml-cerror :ignore-literalize :reserved-clisp-type 'classname)
(ml-cerror :ignore-remove :non-variable)
(ml-cerror :ignore-remove :var-not-wme 'wme)
(ml-cerror :go-on :rete-changed)
(ml-cerror :return-nil :not-in-rhs)
(ml-cerror :go-on :var-no-binding
		     'varname 'pname)
(ml-cerror :go-on :var-no-binding-1 'varname)
(ml-cerror :go-on :no-rhs-to-compile 'pname)
(ml-cerror :return-nil-nil :var-not-in-production 'varname
		     'pname)
(ml-cerror :try-next :var-different 'varname 2)
(ml-cerror :go-on :rule-in-bucket-not-defined 'rname 'bname)
(ml-cerror :go-on :rule-list 'rnames)
(ml-error :unknown-type 'type)
(ml-error :illegal-node-type 'type)
(ml-error :bottom-not-negative)
(ml-error :inappropriate-type 'type)
(ml-error :illegal-bucket-position 'bpos)
(ml-error :rule-not-in-bucket 'whichrule)
(ml-error :wme-list 'fn '?wme)
(ml-error :not-implemented)
(ml-error :only-condes 'i 'pname 'num)
