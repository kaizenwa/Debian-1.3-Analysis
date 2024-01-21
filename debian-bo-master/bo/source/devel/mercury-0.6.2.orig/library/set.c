/*
** Automatically generated from `set.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__set__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__set__list_to_set_2_0);
Define_extern_entry(mercury__set__sorted_list_to_set_2_0);
Define_extern_entry(mercury__set__to_sorted_list_2_0);
Define_extern_entry(mercury__set__init_1_0);
Define_extern_entry(mercury__set__singleton_set_2_0);
Declare_label(mercury__set__singleton_set_2_0_i2);
Declare_label(mercury__set__singleton_set_2_0_i1000);
Define_extern_entry(mercury__set__singleton_set_2_1);
Define_extern_entry(mercury__set__equal_2_0);
Define_extern_entry(mercury__set__empty_1_0);
Define_extern_entry(mercury__set__subset_2_0);
Define_extern_entry(mercury__set__superset_2_0);
Define_extern_entry(mercury__set__member_2_0);
Define_extern_entry(mercury__set__member_2_1);
Declare_label(mercury__set__member_2_1_i1);
Define_extern_entry(mercury__set__is_member_3_0);
Define_extern_entry(mercury__set__insert_3_0);
Define_extern_entry(mercury__set__insert_3_1);
Define_extern_entry(mercury__set__insert_list_3_0);
Define_extern_entry(mercury__set__delete_3_0);
Define_extern_entry(mercury__set__delete_list_3_0);
Define_extern_entry(mercury__set__remove_3_0);
Declare_label(mercury__set__remove_3_0_i2);
Declare_label(mercury__set__remove_3_0_i1000);
Define_extern_entry(mercury__set__remove_list_3_0);
Declare_label(mercury__set__remove_list_3_0_i2);
Declare_label(mercury__set__remove_list_3_0_i1000);
Define_extern_entry(mercury__set__remove_least_3_0);
Declare_label(mercury__set__remove_least_3_0_i2);
Declare_label(mercury__set__remove_least_3_0_i1000);
Define_extern_entry(mercury__set__union_3_0);
Define_extern_entry(mercury__set__power_union_2_0);
Define_extern_entry(mercury__set__intersect_3_0);
Define_extern_entry(mercury__set__power_intersect_2_0);
Define_extern_entry(mercury__set__difference_3_0);
Define_extern_entry(mercury____Unify___set__set_1_0);
Define_extern_entry(mercury____Index___set__set_1_0);
Define_extern_entry(mercury____Compare___set__set_1_0);

extern Word * mercury_data_set__base_type_layout_set_1[];
Word * mercury_data_set__base_type_info_set_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___set__set_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___set__set_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___set__set_1_0),
	(Word *) (Integer) mercury_data_set__base_type_layout_set_1
};

extern Word * mercury_data_set__common_1[];
Word * mercury_data_set__base_type_layout_set_1[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_set__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_set__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_set__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_set__common_1)
};

extern Word * mercury_data_set_ordlist__base_type_info_set_ordlist_1[];
Word * mercury_data_set__common_0[] = {
	(Word *) (Integer) mercury_data_set_ordlist__base_type_info_set_ordlist_1,
	(Word *) ((Integer) 1)
};

Word * mercury_data_set__common_1[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_set__common_0)
};

BEGIN_MODULE(mercury__set_module0)
	init_entry(mercury__set__list_to_set_2_0);
BEGIN_CODE

/* code for predicate 'set__list_to_set'/2 in mode 0 */
Define_entry(mercury__set__list_to_set_2_0);
	{
	Declare_entry(mercury__set_ordlist__list_to_set_2_0);
	tailcall(ENTRY(mercury__set_ordlist__list_to_set_2_0),
		ENTRY(mercury__set__list_to_set_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module1)
	init_entry(mercury__set__sorted_list_to_set_2_0);
BEGIN_CODE

/* code for predicate 'set__sorted_list_to_set'/2 in mode 0 */
Define_entry(mercury__set__sorted_list_to_set_2_0);
	{
	Declare_entry(mercury__set_ordlist__sorted_list_to_set_2_0);
	tailcall(ENTRY(mercury__set_ordlist__sorted_list_to_set_2_0),
		ENTRY(mercury__set__sorted_list_to_set_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module2)
	init_entry(mercury__set__to_sorted_list_2_0);
BEGIN_CODE

/* code for predicate 'set__to_sorted_list'/2 in mode 0 */
Define_entry(mercury__set__to_sorted_list_2_0);
	{
	Declare_entry(mercury__set_ordlist__to_sorted_list_2_0);
	tailcall(ENTRY(mercury__set_ordlist__to_sorted_list_2_0),
		ENTRY(mercury__set__to_sorted_list_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module3)
	init_entry(mercury__set__init_1_0);
BEGIN_CODE

/* code for predicate 'set__init'/1 in mode 0 */
Define_entry(mercury__set__init_1_0);
	{
	Declare_entry(mercury__set_ordlist__init_1_0);
	tailcall(ENTRY(mercury__set_ordlist__init_1_0),
		ENTRY(mercury__set__init_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module4)
	init_entry(mercury__set__singleton_set_2_0);
	init_label(mercury__set__singleton_set_2_0_i2);
	init_label(mercury__set__singleton_set_2_0_i1000);
BEGIN_CODE

/* code for predicate 'set__singleton_set'/2 in mode 0 */
Define_entry(mercury__set__singleton_set_2_0);
	incr_sp_push_msg(1, "set__singleton_set");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__set_ordlist__singleton_set_2_0);
	call_localret(ENTRY(mercury__set_ordlist__singleton_set_2_0),
		mercury__set__singleton_set_2_0_i2,
		ENTRY(mercury__set__singleton_set_2_0));
	}
Define_label(mercury__set__singleton_set_2_0_i2);
	update_prof_current_proc(LABEL(mercury__set__singleton_set_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set__singleton_set_2_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__set__singleton_set_2_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_module5)
	init_entry(mercury__set__singleton_set_2_1);
BEGIN_CODE

/* code for predicate 'set__singleton_set'/2 in mode 1 */
Define_entry(mercury__set__singleton_set_2_1);
	{
	Declare_entry(mercury__set_ordlist__singleton_set_2_1);
	tailcall(ENTRY(mercury__set_ordlist__singleton_set_2_1),
		ENTRY(mercury__set__singleton_set_2_1));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module6)
	init_entry(mercury__set__equal_2_0);
BEGIN_CODE

/* code for predicate 'set__equal'/2 in mode 0 */
Define_entry(mercury__set__equal_2_0);
	{
	Declare_entry(mercury__set_ordlist__equal_2_0);
	tailcall(ENTRY(mercury__set_ordlist__equal_2_0),
		ENTRY(mercury__set__equal_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module7)
	init_entry(mercury__set__empty_1_0);
BEGIN_CODE

/* code for predicate 'set__empty'/1 in mode 0 */
Define_entry(mercury__set__empty_1_0);
	{
	Declare_entry(mercury__set_ordlist__empty_1_0);
	tailcall(ENTRY(mercury__set_ordlist__empty_1_0),
		ENTRY(mercury__set__empty_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module8)
	init_entry(mercury__set__subset_2_0);
BEGIN_CODE

/* code for predicate 'set__subset'/2 in mode 0 */
Define_entry(mercury__set__subset_2_0);
	{
	Declare_entry(mercury__set_ordlist__subset_2_0);
	tailcall(ENTRY(mercury__set_ordlist__subset_2_0),
		ENTRY(mercury__set__subset_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module9)
	init_entry(mercury__set__superset_2_0);
BEGIN_CODE

/* code for predicate 'set__superset'/2 in mode 0 */
Define_entry(mercury__set__superset_2_0);
	{
	Declare_entry(mercury__set_ordlist__superset_2_0);
	tailcall(ENTRY(mercury__set_ordlist__superset_2_0),
		ENTRY(mercury__set__superset_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module10)
	init_entry(mercury__set__member_2_0);
BEGIN_CODE

/* code for predicate 'set__member'/2 in mode 0 */
Define_entry(mercury__set__member_2_0);
	{
	Declare_entry(mercury__set_ordlist__member_2_0);
	tailcall(ENTRY(mercury__set_ordlist__member_2_0),
		ENTRY(mercury__set__member_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module11)
	init_entry(mercury__set__member_2_1);
	init_label(mercury__set__member_2_1_i1);
BEGIN_CODE

/* code for predicate 'set__member'/2 in mode 1 */
Define_entry(mercury__set__member_2_1);
	{
	Declare_entry(do_fail);
	mkframe("set__member/2", 1, ENTRY(do_fail));
	}
	{
	Declare_entry(mercury__set_ordlist__member_2_1);
	call_localret(ENTRY(mercury__set_ordlist__member_2_1),
		mercury__set__member_2_1_i1,
		ENTRY(mercury__set__member_2_1));
	}
Define_label(mercury__set__member_2_1_i1);
	update_prof_current_proc(LABEL(mercury__set__member_2_1));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__set_module12)
	init_entry(mercury__set__is_member_3_0);
BEGIN_CODE

/* code for predicate 'set__is_member'/3 in mode 0 */
Define_entry(mercury__set__is_member_3_0);
	{
	Declare_entry(mercury__set_ordlist__is_member_3_0);
	tailcall(ENTRY(mercury__set_ordlist__is_member_3_0),
		ENTRY(mercury__set__is_member_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module13)
	init_entry(mercury__set__insert_3_0);
BEGIN_CODE

/* code for predicate 'set__insert'/3 in mode 0 */
Define_entry(mercury__set__insert_3_0);
	{
	Declare_entry(mercury__set_ordlist__insert_3_0);
	tailcall(ENTRY(mercury__set_ordlist__insert_3_0),
		ENTRY(mercury__set__insert_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module14)
	init_entry(mercury__set__insert_3_1);
BEGIN_CODE

/* code for predicate 'set__insert'/3 in mode 1 */
Define_entry(mercury__set__insert_3_1);
	{
	Declare_entry(mercury__set_ordlist__insert_3_1);
	tailcall(ENTRY(mercury__set_ordlist__insert_3_1),
		ENTRY(mercury__set__insert_3_1));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module15)
	init_entry(mercury__set__insert_list_3_0);
BEGIN_CODE

/* code for predicate 'set__insert_list'/3 in mode 0 */
Define_entry(mercury__set__insert_list_3_0);
	{
	Declare_entry(mercury__set_ordlist__insert_list_3_0);
	tailcall(ENTRY(mercury__set_ordlist__insert_list_3_0),
		ENTRY(mercury__set__insert_list_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module16)
	init_entry(mercury__set__delete_3_0);
BEGIN_CODE

/* code for predicate 'set__delete'/3 in mode 0 */
Define_entry(mercury__set__delete_3_0);
	{
	Declare_entry(mercury__set_ordlist__delete_3_0);
	tailcall(ENTRY(mercury__set_ordlist__delete_3_0),
		ENTRY(mercury__set__delete_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module17)
	init_entry(mercury__set__delete_list_3_0);
BEGIN_CODE

/* code for predicate 'set__delete_list'/3 in mode 0 */
Define_entry(mercury__set__delete_list_3_0);
	{
	Declare_entry(mercury__set_ordlist__delete_list_3_0);
	tailcall(ENTRY(mercury__set_ordlist__delete_list_3_0),
		ENTRY(mercury__set__delete_list_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module18)
	init_entry(mercury__set__remove_3_0);
	init_label(mercury__set__remove_3_0_i2);
	init_label(mercury__set__remove_3_0_i1000);
BEGIN_CODE

/* code for predicate 'set__remove'/3 in mode 0 */
Define_entry(mercury__set__remove_3_0);
	incr_sp_push_msg(1, "set__remove");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__set_ordlist__remove_3_0);
	call_localret(ENTRY(mercury__set_ordlist__remove_3_0),
		mercury__set__remove_3_0_i2,
		ENTRY(mercury__set__remove_3_0));
	}
Define_label(mercury__set__remove_3_0_i2);
	update_prof_current_proc(LABEL(mercury__set__remove_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set__remove_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__set__remove_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_module19)
	init_entry(mercury__set__remove_list_3_0);
	init_label(mercury__set__remove_list_3_0_i2);
	init_label(mercury__set__remove_list_3_0_i1000);
BEGIN_CODE

/* code for predicate 'set__remove_list'/3 in mode 0 */
Define_entry(mercury__set__remove_list_3_0);
	incr_sp_push_msg(1, "set__remove_list");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__set_ordlist__remove_list_3_0);
	call_localret(ENTRY(mercury__set_ordlist__remove_list_3_0),
		mercury__set__remove_list_3_0_i2,
		ENTRY(mercury__set__remove_list_3_0));
	}
Define_label(mercury__set__remove_list_3_0_i2);
	update_prof_current_proc(LABEL(mercury__set__remove_list_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set__remove_list_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__set__remove_list_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_module20)
	init_entry(mercury__set__remove_least_3_0);
	init_label(mercury__set__remove_least_3_0_i2);
	init_label(mercury__set__remove_least_3_0_i1000);
BEGIN_CODE

/* code for predicate 'set__remove_least'/3 in mode 0 */
Define_entry(mercury__set__remove_least_3_0);
	incr_sp_push_msg(1, "set__remove_least");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__set_ordlist__remove_least_3_0);
	call_localret(ENTRY(mercury__set_ordlist__remove_least_3_0),
		mercury__set__remove_least_3_0_i2,
		ENTRY(mercury__set__remove_least_3_0));
	}
Define_label(mercury__set__remove_least_3_0_i2);
	update_prof_current_proc(LABEL(mercury__set__remove_least_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__set__remove_least_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__set__remove_least_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__set_module21)
	init_entry(mercury__set__union_3_0);
BEGIN_CODE

/* code for predicate 'set__union'/3 in mode 0 */
Define_entry(mercury__set__union_3_0);
	{
	Declare_entry(mercury__set_ordlist__union_3_0);
	tailcall(ENTRY(mercury__set_ordlist__union_3_0),
		ENTRY(mercury__set__union_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module22)
	init_entry(mercury__set__power_union_2_0);
BEGIN_CODE

/* code for predicate 'set__power_union'/2 in mode 0 */
Define_entry(mercury__set__power_union_2_0);
	{
	Declare_entry(mercury__set_ordlist__power_union_2_0);
	tailcall(ENTRY(mercury__set_ordlist__power_union_2_0),
		ENTRY(mercury__set__power_union_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module23)
	init_entry(mercury__set__intersect_3_0);
BEGIN_CODE

/* code for predicate 'set__intersect'/3 in mode 0 */
Define_entry(mercury__set__intersect_3_0);
	{
	Declare_entry(mercury__set_ordlist__intersect_3_0);
	tailcall(ENTRY(mercury__set_ordlist__intersect_3_0),
		ENTRY(mercury__set__intersect_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module24)
	init_entry(mercury__set__power_intersect_2_0);
BEGIN_CODE

/* code for predicate 'set__power_intersect'/2 in mode 0 */
Define_entry(mercury__set__power_intersect_2_0);
	{
	Declare_entry(mercury__set_ordlist__power_intersect_2_0);
	tailcall(ENTRY(mercury__set_ordlist__power_intersect_2_0),
		ENTRY(mercury__set__power_intersect_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module25)
	init_entry(mercury__set__difference_3_0);
BEGIN_CODE

/* code for predicate 'set__difference'/3 in mode 0 */
Define_entry(mercury__set__difference_3_0);
	{
	Declare_entry(mercury__set_ordlist__difference_3_0);
	tailcall(ENTRY(mercury__set_ordlist__difference_3_0),
		ENTRY(mercury__set__difference_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module26)
	init_entry(mercury____Unify___set__set_1_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___set__set_1_0);
	{
	Declare_entry(mercury____Unify___set_ordlist__set_ordlist_1_0);
	tailcall(ENTRY(mercury____Unify___set_ordlist__set_ordlist_1_0),
		ENTRY(mercury____Unify___set__set_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module27)
	init_entry(mercury____Index___set__set_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___set__set_1_0);
	{
	Declare_entry(mercury____Index___set_ordlist__set_ordlist_1_0);
	tailcall(ENTRY(mercury____Index___set_ordlist__set_ordlist_1_0),
		ENTRY(mercury____Index___set__set_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__set_module28)
	init_entry(mercury____Compare___set__set_1_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___set__set_1_0);
	{
	Declare_entry(mercury____Compare___set_ordlist__set_ordlist_1_0);
	tailcall(ENTRY(mercury____Compare___set_ordlist__set_ordlist_1_0),
		ENTRY(mercury____Compare___set__set_1_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__set_bunch_0(void)
{
	mercury__set_module0();
	mercury__set_module1();
	mercury__set_module2();
	mercury__set_module3();
	mercury__set_module4();
	mercury__set_module5();
	mercury__set_module6();
	mercury__set_module7();
	mercury__set_module8();
	mercury__set_module9();
	mercury__set_module10();
	mercury__set_module11();
	mercury__set_module12();
	mercury__set_module13();
	mercury__set_module14();
	mercury__set_module15();
	mercury__set_module16();
	mercury__set_module17();
	mercury__set_module18();
	mercury__set_module19();
	mercury__set_module20();
	mercury__set_module21();
	mercury__set_module22();
	mercury__set_module23();
	mercury__set_module24();
	mercury__set_module25();
	mercury__set_module26();
	mercury__set_module27();
	mercury__set_module28();
}

#endif

void mercury__set__init(void); /* suppress gcc warning */
void mercury__set__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__set_bunch_0();
#endif
}
