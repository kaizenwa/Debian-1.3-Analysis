/*
** Automatically generated from `library.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__library__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__library__version_1_0);

BEGIN_MODULE(mercury__library_module0)
	init_entry(mercury__library__version_1_0);
BEGIN_CODE

/* code for predicate 'library__version'/1 in mode 0 */
Define_entry(mercury__library__version_1_0);
	r1 = string_const("0.6", 3);
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__library_bunch_0(void)
{
	mercury__library_module0();
}

#endif

void mercury__library__init(void); /* suppress gcc warning */
void mercury__library__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__library_bunch_0();
#endif
}
