@device(dover)
@make(article)
@section[Trace Package]

  The important trace flag is *RECORD-LEVEL*, which determines whether
Rulekit should be tracing itself while it runs.  Since tracing slows down
the system slightly, it is by default set to 1 (only enough tracing for
the @b(back) function),
but is set to 2 when the trace package (trace.slisp) is loaded. @b(RE-INIT-TRACE)
may be called to re-initialize.

@subsection(Purpose)

  There are two different uses for the trace module.  The first is that the
trace functions described here can be used as tools for learning
and analogy algorithms.
The knowledge compilation techniques
composition and proceduralization, as well as goal-subsumption, analogy
and generalization, will be possible using this package and Rulekit.
The second use of a trace package is for user debugging.  Many of the trace
functions that are used for learning are also useful for debugging
productions.  While the main goal of this package is to provide tools for
implementors of learning algorithms, some of the trace functions will be
used for writing debugging functions.

@subsection(Data Types)

@b(WME.)  Working Memory Element.

Rulekit provides three useful predefined slots for WMEs:
@begin(itemize)

@b(%time)@*
  Accessed by calling @b(wme-%time), this stores the time tag of when the
WME was created.

@b(%created)@*
  Accessed by @b(wme-%created), this stores a pointer to the instantiation
of the production which created the WME (see @i{instantiations}, below).

@b(%class)@*
  Accessed by @b(wme-%class), this is a symbol which is the name of the
class of the WME.
@end(itemize)

Other slots, defined by literalize, are accessed in a similar way.

@b(?WME.)  A negated or positive Working Memory Element.
A negated WME can mean the absence of a WME in some
contexts, or the deletion of a WME from WM in others, depending on what
function is being applied to it.
A ?WME is a potted-pair whose car is either ABS or POS, and whose cdr is
a WME data structure.

@b(WMES.)  Plural Working Memory Elements.  These are just lists of WMEs.
Note: this is the convention for all pluralized data types.

@b(PATTERN.)  These are like WMEs except they are lists and
may contain variables instead of constants in their value field. Variables
are preceded by an '=' sign. 
@verbatim[
   Example:  (polygon :name =x :size-of 3 :perimeter-of =y)]
Note: this is only what patterns look like in the source production system file.
The '=' sign is actually a read macro which, when seen by the production
reader, expands into (var x).  Thus, a trace function which returns a
PATTERN would actually return (polygon :size-of 3 :perimeter-of (var x))

@b(CONDE.)  Condition Element, LHS pattern.  They may additionally contain
negation tests, which would be of the form (ABS <PATTERN>)

@b(WM.)  Working Memory, a set of WMEs.

@b(Production Instantiation.)  These are productions which have matched.
Thus all of their variables (except ones which only appear in <ABS> tests)
are instantiated with values.
The access functions for instantiations are:
@begin(itemize)

@b(var-names-of <instant>)@*
@b(returns: list of variable names)
  <instant> is an instantiation.  This returns the names of the variables in
the instantiation.

@b(var-in-instant <varname> <instant>)@*
  Returns the value of <varname> in instantiation <instant>.

@b(instant-wmes-of <instant>)@*
  Returns a list of WMEs which matched the given instantiation, in the order
of the corresponding condition elements of the production.

@b(prod-of <instant>)@*
  Production from which the instantiation came.
The production is also a Parmenides frame.  Access functions for the
various fields of the production are described next.

@begin(comment)
@b(instant-makes <instant>)@*
  A list of WMEs which were made by the instantiation <instant>.

@b(instant-removes <instant>)@*
  A list of WMEs which were removed by the instantiation <instant>.
@end(comment)

@b(give-additions-on-cycle <N>)@*
  Returns a list of the WMEs which were made on cycle N.

@b(give-deletions-on-cycle <N>)@*
  Returns a list of the WMEs which were removed on cycle N.

@end(itemize)

@b(Productions.)  The access functions for productions are formed by
concatenating the symbol rk-rule- to the slot name.  For example, the
function rk-rule-lhs returns the lhs of a production.  The useful slots are:

@begin(itemize)

@b(LHS)@*
  The Left Hand Side, an ordered list of condition elements.

@b(RHS)@*
  The Right Hand Side, an ordered list of actions.

@b(PNAME)@*
  The name of the production.

@end(itemize)

  Other slots are described in the command summary under the RULE section.

@begin(comment)  Same as instants.
@b(Production Firing).  Many trace functions return or accept as
arguments production-firing specifications, which are of the form:
(<cycle-number> <firing-number>), where <firing-number> is the number
of the instantiated production for that cycle.  Firings are the same as
matches except matches may not have fired yet because of conflict resolution.
@end(comment)

@subsection[Tracing Productions]

@begin(comment) THIS DOESN'T WORK RIGHT NOW BECAUSE OF THE NEW REPs.
@paragraph(give-productions <CONDE>)
@b{returns: <pnames>}
Gives the productions for which CONDE matches at least one condition.  The
productions may not have completely matched yet. A CONDE matches a condition
if it is positive and the condition is positive and matches, or if it is
negative, the condition is negative, but the positive pattern part of CONDE
matches the positive part of the condition.

Examples:
The CONDE (polygon :size-of 3) matches the condition element
(polygon :size-of =x) but doesn't match (ABS (polygon :size-of =x))
The CONDE (ABS (polygon :size-of 3)) would match the condition element
(ABS (polygon :size-of =x)) but not the positive CONDE (polygon :size-of =x).

@end(comment)

@paragraph(give-matches <?WME>)
@b{returns: <production-matches>}
Give the production instantiations which were ever placed in the conflict set
(they could have since been removed from the c.s.) because the
?WME directly helped it's conditions to match.  This is different from
the first function because with the first function, the production
doesn't have to be completely matched and so only the production, and not
the instantiation, can be returned.

@paragraph(give-firings <?WME>)
@b{returns: <production-firings>}
  Returns all productions which @i(fired) as a result of ?WME being added to
WM. [Note that a negative WME here would mean a WME being deleted from WM.]
A subset of @b(give-matches) since not all of the matches will have
necessarily fired.

@paragraph(give-next-matches <?WME>)
@b{returns: <production-matches>}
  Give the production instantiations which were placed in the conflict
set on the next cycle after the given WME was.
Note that ?WME may not have been responsible for the instantiation, since
more than one instaniation may be created on each cycle.
@comment{ not true since the ?wme may not have caused the instantiation
  This will be a subset of
@b(give-matches) since a production may not have fired immediately after
it has matched.
}

@paragraph(give-next-firings <?WME>)
@b{returns: <production-firings>}
  Returns all production which @i(fired) on the next cycle after ?WME was
added to/deleted from WM.
A subset of both @b(give-firings) and @b(give-next-matches).

@paragraph(give-matches-on-cycle N)
@b{returns: <production-matches>}
  Return all the matches on cycle N.

@paragraph(give-firings-on-cycle N)
@b{returns: <production-firings>}
  Return all the firings on cycle N.  This will be a singleton list for
conflict resolution strategies which don't allow parallelism.

@paragraph(give-named-matches <pname> N)
@b{returns: <production-matches>}
  Returns a list of the instantiations of production <pname> which fired
on cycle N.

@paragraph(give-named-firings <pname> N)
@b{returns: <production-firings>}
  Returns a list of the firings of production <pname> which fired on cycle N.

@paragraph(lhs-of <pname>)
@b{returns: CONDs}
  Return the lhs condition patterns given a production name.

@paragraph(rhs-of <pname>)
@b{returns: PATTERNs}
  Return the rhs patterns given a production name.

@subsection[WME Creation]

@paragraph(prod-responsible-for <?WME>)
@b{returns: <production-firing>}
Give the production firing which last added ?WME to WM if ?WME is
positive, and give the production firing which last deleted ?WME from WM if
?WME is negative.

@paragraph(possible-prods-responsible-for <?WME>)
@b{returns: <production-matches>}
@i(not implemented).
Give the production matches which could possibly add/delete ?WME to/from WM.

@begin(comment)
(Not used by anyone and necessitates $modify to be a macro.  Besides,
this can be done in a less syntactic way for makes too I think)
@paragraph(conde-responsible-for <WME>)
@b{returns: <condition-element>}
Only works for WMEs which have been created by $modify.
Returns the condition element which corresponds to the WME which the
given WME was made by modifying.
@end(comment)

@paragraph(in-wmp <CONDE>)
@b{returns: List of WMEs}
Return a list of WMEs in WM that match the given pattern. If CONDE is an
absence test, then it returns all WMEs that @i(don't) match the pattern.
Ignores variables; thus intra-condition-element testing won't work.

@begin(comment) firings ARE instantiations
@paragraph(give-instant <production-firing>)
@b{returns: <production-instantiation>}
Give the production-instantiation corresponding to a given production
firing.
@end(comment)

@paragraph(give-negs <production-firing>)
@b{returns: <PATTERNS>}
@i(Note: not implemented).
Give the most specific negation tests (i.e., if a negation test has a
  variable in it, give the negation test with the variable replaced by
  it's instantiation for rule firing).


@comment{
  Generalize the actions so they cover all linear traces as well}

@begin(comment)  OLD
@subsection(Linear Tracing)
It might also be useful to have a linear trace of everything Rulekit did
during a run.  This could be notated by forming a list consisting of the
following transition statements:

@begin(itemize)

Enter-bucket(abucket)	;;abucket specifies the bucket entered

Exit-bucket(abucket)

Fire-rule(arule)

Delete-rule(arule, abucket)

Add-rule(arule, abucket)

Delete-bucket(abucket)

Add-bucket(abucket)

@end(itemize)

A Rulekit run is simply an ordered list of transition statements.

When a bucket is re-entered, as in bucket-priority, this is notated by an
exit-bucket and enter-bucket.

@end(comment)

@subsection(Tracing Goals)

  At the moment there is no single representation of goal trees in Rulekit.
However, the following functions are useful for tracing changes of goals'
states, regardless of representation.  Note that the goal trace data
structure is independent of the OPS trace data structures.  The goal
trace is stored in the string, *TRACE-TREE*.

@begin(itemize)

@b(trace-pop-success <goalname>)@*
Records a successful termination of a goal by writing into the
goal trace data structure.

@b(trace-pop-failure <goalname>)@*
Records an un-successful termination of a goal by writing into the
goal trace data structure.

@b(trace-interrupt)@*
Records that an interrupt from a goal tree has occurred.

@b(trace-push-goal <parent-name> <subgoal-name>)@*
Records that a subgoal under parent-name has been activated.

@b(record-action <action>)@*
Inserts given generic action into the goal trace tree.
@end(itemize)

@begin(comment)
@begin(verbatim)
Syntax of goal trace:
<goal-sequence> -->
	( <goalname> {<goal-sequence>* | <action>*} <exit-state> )
<exit-state> --> * | *Success | *Failure | *Interrupt.
* indicates normal completion of the goal, and *interrupt indicates
that the goal was interrupted by a higher-priority goal.
<action> is a generic OPS, Agenda or other action.

Example goal trace: (Gs are goals and As are actions)

;(TOP (G1 (G21 A21a A21b *Success)
;         (G22 (G221 A221a *Success) A22a *Failure)))

Example with interrupt:
;(TOP (G1 (G21 A21a A22b *Interrupt))

;     (G0 (G01 A01a *Success))		;;G1 is interrupted by G0.

;     (G1 (G21 A21a A21b A21c *Success)		;;Resume G1...
;         (G22 (G221 A221a *Success) A22a *Failure)))

@end(verbatim)
@end(comment)
