/*
** Automatically generated from `random.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__random__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__random__init_2_0);
Define_extern_entry(mercury__random__random_3_0);
Declare_label(mercury__random__random_3_0_i2);
Define_extern_entry(mercury__random__randmax_3_0);
Define_extern_entry(mercury__random__test_4_0);
Declare_label(mercury__random__test_4_0_i2);
Declare_label(mercury__random__test_4_0_i3);
Declare_label(mercury__random__test_4_0_i4);
Declare_static(mercury__random__test_2_4_0);
Declare_label(mercury__random__test_2_4_0_i4);
Declare_label(mercury__random__test_2_4_0_i5);
Declare_label(mercury__random__test_2_4_0_i1002);
Define_extern_entry(mercury____Unify___random__supply_0_0);
Declare_label(mercury____Unify___random__supply_0_0_i1);
Define_extern_entry(mercury____Index___random__supply_0_0);
Define_extern_entry(mercury____Compare___random__supply_0_0);

extern Word * mercury_data_random__base_type_layout_random__supply_0[];
Word * mercury_data_random__base_type_info_random__supply_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___random__supply_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___random__supply_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___random__supply_0_0),
	(Word *) (Integer) mercury_data_random__base_type_layout_random__supply_0
};

extern Word * mercury_data_random__common_1[];
Word * mercury_data_random__base_type_layout_random__supply_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_random__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_random__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_random__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_random__common_1)
};

extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_random__common_0[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_random__common_1[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_random__common_0)
};

BEGIN_MODULE(mercury__random_module0)
	init_entry(mercury__random__init_2_0);
BEGIN_CODE

/* code for predicate 'random__init'/2 in mode 0 */
Define_entry(mercury__random__init_2_0);
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__copy_2_1);
	tailcall(ENTRY(mercury__copy_2_1),
		ENTRY(mercury__random__init_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__random_module1)
	init_entry(mercury__random__random_3_0);
	init_label(mercury__random__random_3_0_i2);
BEGIN_CODE

/* code for predicate 'random__random'/3 in mode 0 */
Define_entry(mercury__random__random_3_0);
	r2 = ((((Integer) r1 * ((Integer) 2416)) + ((Integer) 374441)) % ((Integer) 1771875));
	incr_sp_push_msg(2, "random__random");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__copy_2_1);
	call_localret(ENTRY(mercury__copy_2_1),
		mercury__random__random_3_0_i2,
		ENTRY(mercury__random__random_3_0));
	}
Define_label(mercury__random__random_3_0_i2);
	update_prof_current_proc(LABEL(mercury__random__random_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__random_module2)
	init_entry(mercury__random__randmax_3_0);
BEGIN_CODE

/* code for predicate 'random__randmax'/3 in mode 0 */
Define_entry(mercury__random__randmax_3_0);
	r2 = (Integer) r1;
	r1 = ((Integer) 1771874);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__random_module3)
	init_entry(mercury__random__test_4_0);
	init_label(mercury__random__test_4_0_i2);
	init_label(mercury__random__test_4_0_i3);
	init_label(mercury__random__test_4_0_i4);
BEGIN_CODE

/* code for predicate 'random__test'/4 in mode 0 */
Define_entry(mercury__random__test_4_0);
	incr_sp_push_msg(2, "random__test");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
		call_localret(STATIC(mercury__random__init_2_0),
		mercury__random__test_4_0_i2,
		ENTRY(mercury__random__test_4_0));
	}
Define_label(mercury__random__test_4_0_i2);
	update_prof_current_proc(LABEL(mercury__random__test_4_0));
	{
		call_localret(STATIC(mercury__random__randmax_3_0),
		mercury__random__test_4_0_i3,
		ENTRY(mercury__random__test_4_0));
	}
Define_label(mercury__random__test_4_0_i3);
	update_prof_current_proc(LABEL(mercury__random__test_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__random__test_2_4_0),
		mercury__random__test_4_0_i4,
		ENTRY(mercury__random__test_4_0));
Define_label(mercury__random__test_4_0_i4);
	update_prof_current_proc(LABEL(mercury__random__test_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__random_module4)
	init_entry(mercury__random__test_2_4_0);
	init_label(mercury__random__test_2_4_0_i4);
	init_label(mercury__random__test_2_4_0_i5);
	init_label(mercury__random__test_2_4_0_i1002);
BEGIN_CODE

/* code for predicate 'random__test_2'/4 in mode 0 */
Define_static(mercury__random__test_2_4_0);
	if (((Integer) r1 <= ((Integer) 0)))
		GOTO_LABEL(mercury__random__test_2_4_0_i1002);
	incr_sp_push_msg(2, "random__test_2");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = ((Integer) r1 - ((Integer) 1));
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__random__random_3_0),
		mercury__random__test_2_4_0_i4,
		STATIC(mercury__random__test_2_4_0));
	}
Define_label(mercury__random__test_2_4_0_i4);
	update_prof_current_proc(LABEL(mercury__random__test_2_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r3;
	localcall(mercury__random__test_2_4_0,
		LABEL(mercury__random__test_2_4_0_i5),
		STATIC(mercury__random__test_2_4_0));
Define_label(mercury__random__test_2_4_0_i5);
	update_prof_current_proc(LABEL(mercury__random__test_2_4_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__random__test_2_4_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__random_module5)
	init_entry(mercury____Unify___random__supply_0_0);
	init_label(mercury____Unify___random__supply_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___random__supply_0_0);
	if (((Integer) r1 != (Integer) r2))
		GOTO_LABEL(mercury____Unify___random__supply_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___random__supply_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__random_module6)
	init_entry(mercury____Index___random__supply_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___random__supply_0_0);
	{
	Declare_entry(mercury__builtin_index_int_2_0);
	tailcall(ENTRY(mercury__builtin_index_int_2_0),
		ENTRY(mercury____Index___random__supply_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__random_module7)
	init_entry(mercury____Compare___random__supply_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___random__supply_0_0);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___random__supply_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__random_bunch_0(void)
{
	mercury__random_module0();
	mercury__random_module1();
	mercury__random_module2();
	mercury__random_module3();
	mercury__random_module4();
	mercury__random_module5();
	mercury__random_module6();
	mercury__random_module7();
}

#endif

void mercury__random__init(void); /* suppress gcc warning */
void mercury__random__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__random_bunch_0();
#endif
}
