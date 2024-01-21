/*
** Automatically generated from `require.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__require__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__require__error_1_0);
Define_extern_entry(mercury__require__require_2_0);
Declare_label(mercury__require__require_2_0_i4);
Declare_label(mercury__require__require_2_0_i7);

BEGIN_MODULE(mercury__require_module0)
	init_entry(mercury__require__error_1_0);
BEGIN_CODE

/* code for predicate 'error'/1 in mode 0 */
Define_entry(mercury__require__error_1_0);
	{
		String	Message;
		Message = (String) (Integer) r1;
		
	fflush(stdout);
	fprintf(stderr, "Software error: %s\n", Message);
	exit(1);
#ifndef USE_GCC_NONLOCAL_GOTOS
	return 0;	/* suppress some dumb warnings */
#endif


	}
END_MODULE

BEGIN_MODULE(mercury__require_module1)
	init_entry(mercury__require__require_2_0);
	init_label(mercury__require__require_2_0_i4);
	init_label(mercury__require__require_2_0_i7);
BEGIN_CODE

/* code for predicate 'require'/2 in mode 0 */
Define_entry(mercury__require__require_2_0);
	incr_sp_push_msg(2, "require");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = ((Integer) 0);
	r3 = ((Integer) 0);
	{
	Declare_entry(do_call_semidet_closure);
	call_localret(ENTRY(do_call_semidet_closure),
		mercury__require__require_2_0_i4,
		ENTRY(mercury__require__require_2_0));
	}
Define_label(mercury__require__require_2_0_i4);
	update_prof_current_proc(LABEL(mercury__require__require_2_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__require__require_2_0_i7);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__require__error_1_0),
		ENTRY(mercury__require__require_2_0));
	}
Define_label(mercury__require__require_2_0_i7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__require_bunch_0(void)
{
	mercury__require_module0();
	mercury__require_module1();
}

#endif

void mercury__require__init(void); /* suppress gcc warning */
void mercury__require__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__require_bunch_0();
#endif
}
