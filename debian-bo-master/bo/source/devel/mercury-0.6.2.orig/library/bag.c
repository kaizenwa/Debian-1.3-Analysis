/*
** Automatically generated from `bag.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__bag__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__bag__init_1_0);
Define_extern_entry(mercury__bag__insert_3_0);
Declare_label(mercury__bag__insert_3_0_i4);
Declare_label(mercury__bag__insert_3_0_i3);
Define_extern_entry(mercury__bag__insert_list_3_0);
Declare_label(mercury__bag__insert_list_3_0_i4);
Declare_label(mercury__bag__insert_list_3_0_i1002);
Define_extern_entry(mercury__bag__remove_3_0);
Declare_label(mercury__bag__remove_3_0_i4);
Declare_label(mercury__bag__remove_3_0_i6);
Declare_label(mercury__bag__remove_3_0_i3);
Define_extern_entry(mercury__bag__remove_all_3_0);
Define_extern_entry(mercury__bag__contains_2_0);
Define_extern_entry(mercury__bag__to_list_without_duplicates_2_0);
Define_extern_entry(mercury____Unify___bag__bag_1_0);
Define_extern_entry(mercury____Index___bag__bag_1_0);
Define_extern_entry(mercury____Compare___bag__bag_1_0);

extern Word * mercury_data_bag__base_type_layout_bag_1[];
Word * mercury_data_bag__base_type_info_bag_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___bag__bag_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___bag__bag_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___bag__bag_1_0),
	(Word *) (Integer) mercury_data_bag__base_type_layout_bag_1
};

extern Word * mercury_data_bag__common_1[];
Word * mercury_data_bag__base_type_layout_bag_1[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_bag__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_bag__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_bag__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_bag__common_1)
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_bag__common_0[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) ((Integer) 1),
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_bag__common_1[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_bag__common_0)
};

BEGIN_MODULE(mercury__bag_module0)
	init_entry(mercury__bag__init_1_0);
BEGIN_CODE

/* code for predicate 'bag__init'/1 in mode 0 */
Define_entry(mercury__bag__init_1_0);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__init_1_0);
	tailcall(ENTRY(mercury__map__init_1_0),
		ENTRY(mercury__bag__init_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bag_module1)
	init_entry(mercury__bag__insert_3_0);
	init_label(mercury__bag__insert_3_0_i4);
	init_label(mercury__bag__insert_3_0_i3);
BEGIN_CODE

/* code for predicate 'bag__insert'/3 in mode 0 */
Define_entry(mercury__bag__insert_3_0);
	r4 = (Integer) r3;
	incr_sp_push_msg(4, "bag__insert");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__bag__insert_3_0_i4,
		ENTRY(mercury__bag__insert_3_0));
	}
Define_label(mercury__bag__insert_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bag__insert_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bag__insert_3_0_i3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = ((Integer) r2 + ((Integer) 1));
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__map__set_4_1);
	tailcall(ENTRY(mercury__map__set_4_1),
		ENTRY(mercury__bag__insert_3_0));
	}
Define_label(mercury__bag__insert_3_0_i3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = ((Integer) 1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__map__set_4_1);
	tailcall(ENTRY(mercury__map__set_4_1),
		ENTRY(mercury__bag__insert_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bag_module2)
	init_entry(mercury__bag__insert_list_3_0);
	init_label(mercury__bag__insert_list_3_0_i4);
	init_label(mercury__bag__insert_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'bag__insert_list'/3 in mode 0 */
Define_entry(mercury__bag__insert_list_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__bag__insert_list_3_0_i1002);
	incr_sp_push_msg(3, "bag__insert_list");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
		call_localret(STATIC(mercury__bag__insert_3_0),
		mercury__bag__insert_list_3_0_i4,
		ENTRY(mercury__bag__insert_list_3_0));
	}
Define_label(mercury__bag__insert_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bag__insert_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__bag__insert_list_3_0,
		ENTRY(mercury__bag__insert_list_3_0));
Define_label(mercury__bag__insert_list_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bag_module3)
	init_entry(mercury__bag__remove_3_0);
	init_label(mercury__bag__remove_3_0_i4);
	init_label(mercury__bag__remove_3_0_i6);
	init_label(mercury__bag__remove_3_0_i3);
BEGIN_CODE

/* code for predicate 'bag__remove'/3 in mode 0 */
Define_entry(mercury__bag__remove_3_0);
	r4 = (Integer) r3;
	incr_sp_push_msg(4, "bag__remove");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__bag__remove_3_0_i4,
		ENTRY(mercury__bag__remove_3_0));
	}
Define_label(mercury__bag__remove_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bag__remove_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bag__remove_3_0_i3);
	r5 = ((Integer) r2 - ((Integer) 1));
	if (((Integer) r5 <= ((Integer) 0)))
		GOTO_LABEL(mercury__bag__remove_3_0_i6);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__map__set_4_1);
	tailcall(ENTRY(mercury__map__set_4_1),
		ENTRY(mercury__bag__remove_3_0));
	}
Define_label(mercury__bag__remove_3_0_i6);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__map__delete_3_1);
	tailcall(ENTRY(mercury__map__delete_3_1),
		ENTRY(mercury__bag__remove_3_0));
	}
Define_label(mercury__bag__remove_3_0_i3);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bag_module4)
	init_entry(mercury__bag__remove_all_3_0);
BEGIN_CODE

/* code for predicate 'bag__remove_all'/3 in mode 0 */
Define_entry(mercury__bag__remove_all_3_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__delete_3_1);
	tailcall(ENTRY(mercury__map__delete_3_1),
		ENTRY(mercury__bag__remove_all_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bag_module5)
	init_entry(mercury__bag__contains_2_0);
BEGIN_CODE

/* code for predicate 'bag__contains'/2 in mode 0 */
Define_entry(mercury__bag__contains_2_0);
	r4 = (Integer) r2;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__contains_2_0);
	tailcall(ENTRY(mercury__map__contains_2_0),
		ENTRY(mercury__bag__contains_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bag_module6)
	init_entry(mercury__bag__to_list_without_duplicates_2_0);
BEGIN_CODE

/* code for predicate 'bag__to_list_without_duplicates'/2 in mode 0 */
Define_entry(mercury__bag__to_list_without_duplicates_2_0);
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__keys_2_0);
	tailcall(ENTRY(mercury__map__keys_2_0),
		ENTRY(mercury__bag__to_list_without_duplicates_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bag_module7)
	init_entry(mercury____Unify___bag__bag_1_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___bag__bag_1_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___bag__bag_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bag_module8)
	init_entry(mercury____Index___bag__bag_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___bag__bag_1_0);
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury____Index___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Index___tree234__tree234_2_0),
		ENTRY(mercury____Index___bag__bag_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bag_module9)
	init_entry(mercury____Compare___bag__bag_1_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___bag__bag_1_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___bag__bag_1_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__bag_bunch_0(void)
{
	mercury__bag_module0();
	mercury__bag_module1();
	mercury__bag_module2();
	mercury__bag_module3();
	mercury__bag_module4();
	mercury__bag_module5();
	mercury__bag_module6();
	mercury__bag_module7();
	mercury__bag_module8();
	mercury__bag_module9();
}

#endif

void mercury__bag__init(void); /* suppress gcc warning */
void mercury__bag__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__bag_bunch_0();
#endif
}
