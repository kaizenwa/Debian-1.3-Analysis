/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** file: solutions.mod
** authors: conway, fjh.
**
** this module defines solutions/2 which takes a closure of type
** pred(T) in which the remaining argument is output.
*/

#include "imp.h"

BEGIN_MODULE(solutions_module)

BEGIN_CODE

/*
** The following is an incomplete start at implementing solutions/2
** for gc != conservative.
**
**
**	do_solutions:
**		mkframe("solutions", 3, LABEL(no_more_solutions));
**	
**		framevar(0) = succip;
**	
**		framevar(1) = hp;
**	
**		framevar(2) = list_empty();
**	
**		r2 = (Word) 1;
**		call(ENTRY(do_call_nondet_closure),
**			LABEL(more_solutions), LABEL(do_solutions));
**	
**	more_solutions:
**		r3 = deep_copy(r1, framevar(1));
**		framevar(2) = list_cons(r3, framevar(2));
**	
**		redo();
**	
**	no_more_solutions:
**		r2 = deep_recopy(framevar(2), framevar(1));
**		maxfr = curprevfr;
**		curfr = maxfr;
**	
**		succip = framevar(0);
**		proceed();
*/

/*
** :- pred builtin_solutions(pred(T), list(T)).
** :- mode builtin_solutions(pred(out) is multi/nondet, out) is det.
**
** Polymorphism will add an extra input parameter, a type_info for T,
** which we don't use at the moment (later it could be used to find
** the address of the deep copy routine).
**
** The type_info structure will be in r1 and the closure will be in r2
** with both caling conventions. The output should go either in r3
** (for the normal parameter convention) or r1 (for the compact parameter
** convention).
*/

#ifdef	COMPACT_ARGS
#define	solutions_output	r1
#else
#define	solutions_output	r3
#endif

mercury__std_util__builtin_solutions_2_0:
#ifdef PROFILE_CALLS
{
	tailcall(ENTRY(mercury__std_util__builtin_solutions_2_1),
			LABEL(mercury__std_util__builtin_solutions_2_0));
}
#endif
mercury__std_util__builtin_solutions_2_1:

/*
** The following algorithm is very straight-forward implementation
** but only works with `--gc conservative'.
** Since with conservative gc, we don't reclaim any memory on failure,
** but instead leave it to the garbage collector, there is no need to
** make deep copies of the solutions.  This is a `copy-zero' implementation ;-)
*/

#ifndef CONSERVATIVE_GC
	fatal_error("solutions/2 only implemented for conservative GC");
#endif

	/* create a nondet stack frame with one slot, to hold the list
	   of solutions, and set the failure continuation */
	mkframe("builtin_solutions", 1,
		LABEL(mercury__std_util__builtin_solutions_2_0_i2));
	framevar(0) = list_empty();

	/* we do not (yet) need the type_info we are passed in r1 */
	/* call the higher-order pred closure that we were passed in r2 */
	r1 = r2;
	r2 = (Word) 0;	/* the higher-order call has 0 extra input arguments */
	r3 = (Word) 1;	/* the higher-order call has 1 extra output argument */
	{ 
		Declare_entry(do_call_nondet_closure);
		call(ENTRY(do_call_nondet_closure),
			LABEL(mercury__std_util__builtin_solutions_2_0_i1),
			LABEL(mercury__std_util__builtin_solutions_2_1));
	}

mercury__std_util__builtin_solutions_2_0_i1:
	/* we found a solution */
	/* insert it into the list, and then look for the next one */
	framevar(0) = list_cons(r1, framevar(0));
	redo();

mercury__std_util__builtin_solutions_2_0_i2:
	/* no more solutions */
	/* return the solutions list and discard the frame we made */
	solutions_output = framevar(0);
	succeed_discard();

END_MODULE
