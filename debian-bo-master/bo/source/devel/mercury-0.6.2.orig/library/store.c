/*
** Automatically generated from `store.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__store__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Compare___store_node_id_1__ua10000_3_0);
Declare_static(mercury____Index___store_node_id_1__ua10000_2_0);
Declare_static(mercury____Unify___store_node_id_1__ua0_2_0);
Declare_label(mercury____Unify___store_node_id_1__ua0_2_0_i1);
Declare_static(mercury____Index___store_store_1__ua10000_2_0);
Define_extern_entry(mercury__store__init_1_0);
Declare_label(mercury__store__init_1_0_i2);
Define_extern_entry(mercury__store__new_node_4_0);
Declare_label(mercury__store__new_node_4_0_i2);
Define_extern_entry(mercury__store__set_node_4_0);
Declare_label(mercury__store__set_node_4_0_i2);
Define_extern_entry(mercury__store__lookup_node_3_0);
Define_extern_entry(mercury____Unify___store__store_1_0);
Declare_label(mercury____Unify___store__store_1_0_i1002);
Define_extern_entry(mercury____Index___store__store_1_0);
Define_extern_entry(mercury____Compare___store__store_1_0);
Declare_label(mercury____Compare___store__store_1_0_i4);
Declare_label(mercury____Compare___store__store_1_0_i3);
Define_extern_entry(mercury____Unify___store__node_id_1_0);
Define_extern_entry(mercury____Index___store__node_id_1_0);
Define_extern_entry(mercury____Compare___store__node_id_1_0);

extern Word * mercury_data_store__base_type_layout_node_id_1[];
Word * mercury_data_store__base_type_info_node_id_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___store__node_id_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___store__node_id_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___store__node_id_1_0),
	(Word *) (Integer) mercury_data_store__base_type_layout_node_id_1
};

extern Word * mercury_data_store__base_type_layout_store_1[];
Word * mercury_data_store__base_type_info_store_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___store__store_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___store__store_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___store__store_1_0),
	(Word *) (Integer) mercury_data_store__base_type_layout_store_1
};

extern Word * mercury_data_store__common_2[];
Word * mercury_data_store__base_type_layout_store_1[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_store__common_2),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_store__common_3[];
Word * mercury_data_store__base_type_layout_node_id_1[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_store__common_3),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_store__common_3),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_store__common_3),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_store__common_3)
};

extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_store__common_0[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
Word * mercury_data_store__common_1[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data___base_type_info_int_0,
	(Word *) ((Integer) 1)
};

Word * mercury_data_store__common_2[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_store__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_store__common_1),
	(Word *) string_const("store", 5)
};

Word * mercury_data_store__common_3[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_store__common_0)
};

BEGIN_MODULE(mercury__store_module0)
	init_entry(mercury____Compare___store_node_id_1__ua10000_3_0);
BEGIN_CODE

/* code for predicate '__Compare___store_node_id_1__ua10000'/3 in mode 0 */
Define_static(mercury____Compare___store_node_id_1__ua10000_3_0);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		STATIC(mercury____Compare___store_node_id_1__ua10000_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__store_module1)
	init_entry(mercury____Index___store_node_id_1__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___store_node_id_1__ua10000'/2 in mode 0 */
Define_static(mercury____Index___store_node_id_1__ua10000_2_0);
	{
	Declare_entry(mercury__builtin_index_int_2_0);
	tailcall(ENTRY(mercury__builtin_index_int_2_0),
		STATIC(mercury____Index___store_node_id_1__ua10000_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__store_module2)
	init_entry(mercury____Unify___store_node_id_1__ua0_2_0);
	init_label(mercury____Unify___store_node_id_1__ua0_2_0_i1);
BEGIN_CODE

/* code for predicate '__Unify___store_node_id_1__ua0'/2 in mode 0 */
Define_static(mercury____Unify___store_node_id_1__ua0_2_0);
	if (((Integer) r1 != (Integer) r2))
		GOTO_LABEL(mercury____Unify___store_node_id_1__ua0_2_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___store_node_id_1__ua0_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__store_module3)
	init_entry(mercury____Index___store_store_1__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___store_store_1__ua10000'/2 in mode 0 */
Define_static(mercury____Index___store_store_1__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__store_module4)
	init_entry(mercury__store__init_1_0);
	init_label(mercury__store__init_1_0_i2);
BEGIN_CODE

/* code for predicate 'store__init'/1 in mode 0 */
Define_entry(mercury__store__init_1_0);
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	incr_sp_push_msg(1, "store__init");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__store__init_1_0_i2,
		ENTRY(mercury__store__init_1_0));
	}
Define_label(mercury__store__init_1_0_i2);
	update_prof_current_proc(LABEL(mercury__store__init_1_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__store_module5)
	init_entry(mercury__store__new_node_4_0);
	init_label(mercury__store__new_node_4_0_i2);
BEGIN_CODE

/* code for predicate 'store__new_node'/4 in mode 0 */
Define_entry(mercury__store__new_node_4_0);
	r5 = (Integer) r3;
	r4 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	incr_sp_push_msg(3, "store__new_node");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	detstackvar(2) = ((Integer) r4 + ((Integer) 1));
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__store__new_node_4_0_i2,
		ENTRY(mercury__store__new_node_4_0));
	}
Define_label(mercury__store__new_node_4_0_i2);
	update_prof_current_proc(LABEL(mercury__store__new_node_4_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__store_module6)
	init_entry(mercury__store__set_node_4_0);
	init_label(mercury__store__set_node_4_0_i2);
BEGIN_CODE

/* code for predicate 'store__set_node'/4 in mode 0 */
Define_entry(mercury__store__set_node_4_0);
	r5 = (Integer) r4;
	r4 = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	incr_sp_push_msg(2, "store__set_node");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__store__set_node_4_0_i2,
		ENTRY(mercury__store__set_node_4_0));
	}
Define_label(mercury__store__set_node_4_0_i2);
	update_prof_current_proc(LABEL(mercury__store__set_node_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__store_module7)
	init_entry(mercury__store__lookup_node_3_0);
BEGIN_CODE

/* code for predicate 'store__lookup_node'/3 in mode 0 */
Define_entry(mercury__store__lookup_node_3_0);
	r4 = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__lookup_3_1);
	tailcall(ENTRY(mercury__map__lookup_3_1),
		ENTRY(mercury__store__lookup_node_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__store_module8)
	init_entry(mercury____Unify___store__store_1_0);
	init_label(mercury____Unify___store__store_1_0_i1002);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___store__store_1_0);
	if (((Integer) field(mktag(0), (Integer) r2, ((Integer) 0)) != (Integer) field(mktag(0), (Integer) r3, ((Integer) 0))))
		GOTO_LABEL(mercury____Unify___store__store_1_0_i1002);
	r4 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___store__store_1_0));
	}
Define_label(mercury____Unify___store__store_1_0_i1002);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__store_module9)
	init_entry(mercury____Index___store__store_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___store__store_1_0);
	tailcall(STATIC(mercury____Index___store_store_1__ua10000_2_0),
		ENTRY(mercury____Index___store__store_1_0));
END_MODULE

BEGIN_MODULE(mercury__store_module10)
	init_entry(mercury____Compare___store__store_1_0);
	init_label(mercury____Compare___store__store_1_0_i4);
	init_label(mercury____Compare___store__store_1_0_i3);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___store__store_1_0);
	incr_sp_push_msg(4, "__Compare__");
	detstackvar(4) = (Integer) succip;
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	call_localret(ENTRY(mercury__builtin_compare_int_3_0),
		mercury____Compare___store__store_1_0_i4,
		ENTRY(mercury____Compare___store__store_1_0));
	}
Define_label(mercury____Compare___store__store_1_0_i4);
	update_prof_current_proc(LABEL(mercury____Compare___store__store_1_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___store__store_1_0_i3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___store__store_1_0_i3);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___store__store_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__store_module11)
	init_entry(mercury____Unify___store__node_id_1_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___store__node_id_1_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury____Unify___store_node_id_1__ua0_2_0),
		ENTRY(mercury____Unify___store__node_id_1_0));
END_MODULE

BEGIN_MODULE(mercury__store_module12)
	init_entry(mercury____Index___store__node_id_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___store__node_id_1_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury____Index___store_node_id_1__ua10000_2_0),
		ENTRY(mercury____Index___store__node_id_1_0));
END_MODULE

BEGIN_MODULE(mercury__store_module13)
	init_entry(mercury____Compare___store__node_id_1_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___store__node_id_1_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury____Compare___store_node_id_1__ua10000_3_0),
		ENTRY(mercury____Compare___store__node_id_1_0));
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__store_bunch_0(void)
{
	mercury__store_module0();
	mercury__store_module1();
	mercury__store_module2();
	mercury__store_module3();
	mercury__store_module4();
	mercury__store_module5();
	mercury__store_module6();
	mercury__store_module7();
	mercury__store_module8();
	mercury__store_module9();
	mercury__store_module10();
	mercury__store_module11();
	mercury__store_module12();
	mercury__store_module13();
}

#endif

void mercury__store__init(void); /* suppress gcc warning */
void mercury__store__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__store_bunch_0();
#endif
}
