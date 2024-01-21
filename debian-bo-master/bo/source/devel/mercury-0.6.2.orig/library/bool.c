/*
** Automatically generated from `bool.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__bool__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__bool__or_3_0);
Declare_label(mercury__bool__or_3_0_i3);
Define_extern_entry(mercury__bool__or_list_2_0);
Declare_label(mercury__bool__or_list_2_0_i1003);
Declare_label(mercury__bool__or_list_2_0_i1005);
Define_extern_entry(mercury__bool__and_3_0);
Declare_label(mercury__bool__and_3_0_i3);
Define_extern_entry(mercury__bool__and_list_2_0);
Declare_label(mercury__bool__and_list_2_0_i1003);
Declare_label(mercury__bool__and_list_2_0_i1005);
Define_extern_entry(mercury__bool__not_2_0);
Declare_label(mercury__bool__not_2_0_i3);
Define_extern_entry(mercury____Unify___bool__bool_0_0);
Declare_label(mercury____Unify___bool__bool_0_0_i1);
Define_extern_entry(mercury____Index___bool__bool_0_0);
Define_extern_entry(mercury____Compare___bool__bool_0_0);

extern Word * mercury_data_bool__base_type_layout_bool_0[];
Word * mercury_data_bool__base_type_info_bool_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___bool__bool_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___bool__bool_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___bool__bool_0_0),
	(Word *) (Integer) mercury_data_bool__base_type_layout_bool_0
};

extern Word * mercury_data_bool__common_0[];
Word * mercury_data_bool__base_type_layout_bool_0[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_bool__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_bool__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_bool__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_bool__common_0)
};

Word * mercury_data_bool__common_0[] = {
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 2),
	(Word *) string_const("yes", 3),
	(Word *) string_const("no", 2)
};

BEGIN_MODULE(mercury__bool_module0)
	init_entry(mercury__bool__or_3_0);
	init_label(mercury__bool__or_3_0_i3);
BEGIN_CODE

/* code for predicate 'bool__or'/3 in mode 0 */
Define_entry(mercury__bool__or_3_0);
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury__bool__or_3_0_i3);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__bool__or_3_0_i3);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bool_module1)
	init_entry(mercury__bool__or_list_2_0);
	init_label(mercury__bool__or_list_2_0_i1003);
	init_label(mercury__bool__or_list_2_0_i1005);
BEGIN_CODE

/* code for predicate 'bool__or_list'/2 in mode 0 */
Define_entry(mercury__bool__or_list_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bool__or_list_2_0_i1003);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__bool__or_list_2_0_i1005);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury__bool__or_list_2_0_i1003);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury__bool__or_list_2_0_i1005);
	r1 = (Integer) r2;
	localtailcall(mercury__bool__or_list_2_0,
		ENTRY(mercury__bool__or_list_2_0));
END_MODULE

BEGIN_MODULE(mercury__bool_module2)
	init_entry(mercury__bool__and_3_0);
	init_label(mercury__bool__and_3_0_i3);
BEGIN_CODE

/* code for predicate 'bool__and'/3 in mode 0 */
Define_entry(mercury__bool__and_3_0);
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury__bool__and_3_0_i3);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury__bool__and_3_0_i3);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bool_module3)
	init_entry(mercury__bool__and_list_2_0);
	init_label(mercury__bool__and_list_2_0_i1003);
	init_label(mercury__bool__and_list_2_0_i1005);
BEGIN_CODE

/* code for predicate 'bool__and_list'/2 in mode 0 */
Define_entry(mercury__bool__and_list_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bool__and_list_2_0_i1003);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 != ((Integer) 1)))
		GOTO_LABEL(mercury__bool__and_list_2_0_i1005);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury__bool__and_list_2_0_i1003);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury__bool__and_list_2_0_i1005);
	r1 = (Integer) r2;
	localtailcall(mercury__bool__and_list_2_0,
		ENTRY(mercury__bool__and_list_2_0));
END_MODULE

BEGIN_MODULE(mercury__bool_module4)
	init_entry(mercury__bool__not_2_0);
	init_label(mercury__bool__not_2_0_i3);
BEGIN_CODE

/* code for predicate 'bool__not'/2 in mode 0 */
Define_entry(mercury__bool__not_2_0);
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury__bool__not_2_0_i3);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury__bool__not_2_0_i3);
	r1 = ((Integer) 1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bool_module5)
	init_entry(mercury____Unify___bool__bool_0_0);
	init_label(mercury____Unify___bool__bool_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___bool__bool_0_0);
	if (((Integer) r1 != (Integer) r2))
		GOTO_LABEL(mercury____Unify___bool__bool_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___bool__bool_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bool_module6)
	init_entry(mercury____Index___bool__bool_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___bool__bool_0_0);
	{
	Declare_entry(mercury__builtin_index_int_2_0);
	tailcall(ENTRY(mercury__builtin_index_int_2_0),
		ENTRY(mercury____Index___bool__bool_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bool_module7)
	init_entry(mercury____Compare___bool__bool_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___bool__bool_0_0);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___bool__bool_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__bool_bunch_0(void)
{
	mercury__bool_module0();
	mercury__bool_module1();
	mercury__bool_module2();
	mercury__bool_module3();
	mercury__bool_module4();
	mercury__bool_module5();
	mercury__bool_module6();
	mercury__bool_module7();
}

#endif

void mercury__bool__init(void); /* suppress gcc warning */
void mercury__bool__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__bool_bunch_0();
#endif
}
