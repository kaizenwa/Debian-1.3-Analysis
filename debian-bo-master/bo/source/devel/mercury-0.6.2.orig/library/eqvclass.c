/*
** Automatically generated from `eqvclass.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__eqvclass__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Index___eqvclass_eqvclass_1__ua10000_2_0);
Define_extern_entry(mercury__eqvclass__init_1_0);
Declare_label(mercury__eqvclass__init_1_0_i2);
Declare_label(mercury__eqvclass__init_1_0_i3);
Define_extern_entry(mercury__eqvclass__is_member_2_0);
Define_extern_entry(mercury__eqvclass__ensure_element_3_0);
Declare_label(mercury__eqvclass__ensure_element_3_0_i2);
Define_extern_entry(mercury__eqvclass__new_element_3_0);
Declare_label(mercury__eqvclass__new_element_3_0_i4);
Declare_label(mercury__eqvclass__new_element_3_0_i3);
Declare_label(mercury__eqvclass__new_element_3_0_i7);
Define_extern_entry(mercury__eqvclass__ensure_equivalence_4_0);
Declare_label(mercury__eqvclass__ensure_equivalence_4_0_i2);
Declare_label(mercury__eqvclass__ensure_equivalence_4_0_i3);
Declare_label(mercury__eqvclass__ensure_equivalence_4_0_i4);
Define_extern_entry(mercury__eqvclass__new_equivalence_4_0);
Declare_label(mercury__eqvclass__new_equivalence_4_0_i2);
Declare_label(mercury__eqvclass__new_equivalence_4_0_i3);
Declare_label(mercury__eqvclass__new_equivalence_4_0_i4);
Define_extern_entry(mercury__eqvclass__same_eqvclass_3_0);
Declare_label(mercury__eqvclass__same_eqvclass_3_0_i2);
Declare_label(mercury__eqvclass__same_eqvclass_3_0_i4);
Declare_label(mercury__eqvclass__same_eqvclass_3_0_i1);
Define_extern_entry(mercury__eqvclass__partition_set_2_0);
Declare_label(mercury__eqvclass__partition_set_2_0_i2);
Declare_label(mercury__eqvclass__partition_set_2_0_i3);
Define_extern_entry(mercury__eqvclass__partition_list_2_0);
Declare_label(mercury__eqvclass__partition_list_2_0_i2);
Define_extern_entry(mercury__eqvclass__partition_set_to_eqvclass_2_0);
Declare_label(mercury__eqvclass__partition_set_to_eqvclass_2_0_i2);
Define_extern_entry(mercury__eqvclass__partition_list_to_eqvclass_2_0);
Declare_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i4);
Declare_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i5);
Declare_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i6);
Declare_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i9);
Declare_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i10);
Declare_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i1006);
Declare_static(mercury__eqvclass__ensure_element_2_4_0);
Declare_label(mercury__eqvclass__ensure_element_2_4_0_i4);
Declare_label(mercury__eqvclass__ensure_element_2_4_0_i3);
Declare_static(mercury__eqvclass__add_element_4_0);
Declare_label(mercury__eqvclass__add_element_4_0_i2);
Declare_label(mercury__eqvclass__add_element_4_0_i3);
Declare_label(mercury__eqvclass__add_element_4_0_i4);
Declare_static(mercury__eqvclass__add_equivalence_4_0);
Declare_label(mercury__eqvclass__add_equivalence_4_0_i2);
Declare_label(mercury__eqvclass__add_equivalence_4_0_i3);
Declare_label(mercury__eqvclass__add_equivalence_4_0_i4);
Declare_label(mercury__eqvclass__add_equivalence_4_0_i5);
Declare_label(mercury__eqvclass__add_equivalence_4_0_i6);
Declare_label(mercury__eqvclass__add_equivalence_4_0_i7);
Declare_static(mercury__eqvclass__change_partition_4_0);
Declare_label(mercury__eqvclass__change_partition_4_0_i4);
Declare_label(mercury__eqvclass__change_partition_4_0_i1002);
Declare_static(mercury__eqvclass__partitions_3_0);
Declare_label(mercury__eqvclass__partitions_3_0_i6);
Declare_label(mercury__eqvclass__partitions_3_0_i5);
Declare_label(mercury__eqvclass__partitions_3_0_i8);
Declare_label(mercury__eqvclass__partitions_3_0_i9);
Declare_label(mercury__eqvclass__partitions_3_0_i10);
Declare_label(mercury__eqvclass__partitions_3_0_i1004);
Declare_static(mercury__eqvclass__make_partition_4_0);
Declare_label(mercury__eqvclass__make_partition_4_0_i4);
Declare_label(mercury__eqvclass__make_partition_4_0_i1002);
Define_extern_entry(mercury____Unify___eqvclass__eqvclass_1_0);
Declare_label(mercury____Unify___eqvclass__eqvclass_1_0_i2);
Declare_label(mercury____Unify___eqvclass__eqvclass_1_0_i1003);
Declare_label(mercury____Unify___eqvclass__eqvclass_1_0_i1);
Define_extern_entry(mercury____Index___eqvclass__eqvclass_1_0);
Define_extern_entry(mercury____Compare___eqvclass__eqvclass_1_0);
Declare_label(mercury____Compare___eqvclass__eqvclass_1_0_i4);
Declare_label(mercury____Compare___eqvclass__eqvclass_1_0_i5);
Declare_label(mercury____Compare___eqvclass__eqvclass_1_0_i3);
Declare_label(mercury____Compare___eqvclass__eqvclass_1_0_i10);

extern Word * mercury_data_eqvclass__base_type_layout_eqvclass_1[];
Word * mercury_data_eqvclass__base_type_info_eqvclass_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___eqvclass__eqvclass_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___eqvclass__eqvclass_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___eqvclass__eqvclass_1_0),
	(Word *) (Integer) mercury_data_eqvclass__base_type_layout_eqvclass_1
};

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_eqvclass__base_type_layout_partition_id_0[];
Word * mercury_data_eqvclass__base_type_info_partition_id_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_eqvclass__base_type_layout_partition_id_0
};

extern Word * mercury_data_eqvclass__common_1[];
Word * mercury_data_eqvclass__base_type_layout_partition_id_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_eqvclass__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_eqvclass__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_eqvclass__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_eqvclass__common_1)
};

extern Word * mercury_data_eqvclass__common_5[];
Word * mercury_data_eqvclass__base_type_layout_eqvclass_1[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_eqvclass__common_5),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_eqvclass__common_0[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_eqvclass__common_1[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_eqvclass__common_0)
};

extern Word * mercury_data_set__base_type_info_set_1[];
Word * mercury_data_eqvclass__common_2[] = {
	(Word *) (Integer) mercury_data_set__base_type_info_set_1,
	(Word *) ((Integer) 1)
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
Word * mercury_data_eqvclass__common_3[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data___base_type_info_int_0,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_eqvclass__common_2)
};

Word * mercury_data_eqvclass__common_4[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) ((Integer) 1),
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_eqvclass__common_5[] = {
	(Word *) ((Integer) 3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_eqvclass__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_eqvclass__common_3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_eqvclass__common_4),
	(Word *) string_const("eqvclass", 8)
};

BEGIN_MODULE(mercury__eqvclass_module0)
	init_entry(mercury____Index___eqvclass_eqvclass_1__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___eqvclass_eqvclass_1__ua10000'/2 in mode 0 */
Define_static(mercury____Index___eqvclass_eqvclass_1__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module1)
	init_entry(mercury__eqvclass__init_1_0);
	init_label(mercury__eqvclass__init_1_0_i2);
	init_label(mercury__eqvclass__init_1_0_i3);
BEGIN_CODE

/* code for predicate 'eqvclass__init'/1 in mode 0 */
Define_entry(mercury__eqvclass__init_1_0);
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	incr_sp_push_msg(2, "eqvclass__init");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__eqvclass__init_1_0_i2,
		ENTRY(mercury__eqvclass__init_1_0));
	}
Define_label(mercury__eqvclass__init_1_0_i2);
	update_prof_current_proc(LABEL(mercury__eqvclass__init_1_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__eqvclass__init_1_0_i3,
		ENTRY(mercury__eqvclass__init_1_0));
	}
Define_label(mercury__eqvclass__init_1_0_i3);
	update_prof_current_proc(LABEL(mercury__eqvclass__init_1_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module2)
	init_entry(mercury__eqvclass__is_member_2_0);
BEGIN_CODE

/* code for predicate 'eqvclass__is_member'/2 in mode 0 */
Define_entry(mercury__eqvclass__is_member_2_0);
	r4 = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r2 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__search_3_1);
	tailcall(ENTRY(mercury__map__search_3_1),
		ENTRY(mercury__eqvclass__is_member_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module3)
	init_entry(mercury__eqvclass__ensure_element_3_0);
	init_label(mercury__eqvclass__ensure_element_3_0_i2);
BEGIN_CODE

/* code for predicate 'eqvclass__ensure_element'/3 in mode 0 */
Define_entry(mercury__eqvclass__ensure_element_3_0);
	incr_sp_push_msg(1, "eqvclass__ensure_element");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__eqvclass__ensure_element_2_4_0),
		mercury__eqvclass__ensure_element_3_0_i2,
		ENTRY(mercury__eqvclass__ensure_element_3_0));
Define_label(mercury__eqvclass__ensure_element_3_0_i2);
	update_prof_current_proc(LABEL(mercury__eqvclass__ensure_element_3_0));
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module4)
	init_entry(mercury__eqvclass__new_element_3_0);
	init_label(mercury__eqvclass__new_element_3_0_i4);
	init_label(mercury__eqvclass__new_element_3_0_i3);
	init_label(mercury__eqvclass__new_element_3_0_i7);
BEGIN_CODE

/* code for predicate 'eqvclass__new_element'/3 in mode 0 */
Define_entry(mercury__eqvclass__new_element_3_0);
	r4 = (Integer) r3;
	incr_sp_push_msg(4, "eqvclass__new_element");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__eqvclass__new_element_3_0_i4,
		ENTRY(mercury__eqvclass__new_element_3_0));
	}
Define_label(mercury__eqvclass__new_element_3_0_i4);
	update_prof_current_proc(LABEL(mercury__eqvclass__new_element_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__eqvclass__new_element_3_0_i3);
	r1 = string_const("new element is already in equivalence class", 43);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__eqvclass__new_element_3_0));
	}
Define_label(mercury__eqvclass__new_element_3_0_i3);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__eqvclass__add_element_4_0),
		mercury__eqvclass__new_element_3_0_i7,
		ENTRY(mercury__eqvclass__new_element_3_0));
Define_label(mercury__eqvclass__new_element_3_0_i7);
	update_prof_current_proc(LABEL(mercury__eqvclass__new_element_3_0));
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module5)
	init_entry(mercury__eqvclass__ensure_equivalence_4_0);
	init_label(mercury__eqvclass__ensure_equivalence_4_0_i2);
	init_label(mercury__eqvclass__ensure_equivalence_4_0_i3);
	init_label(mercury__eqvclass__ensure_equivalence_4_0_i4);
BEGIN_CODE

/* code for predicate 'eqvclass__ensure_equivalence'/4 in mode 0 */
Define_entry(mercury__eqvclass__ensure_equivalence_4_0);
	incr_sp_push_msg(3, "eqvclass__ensure_equivalence");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r1;
	call_localret(STATIC(mercury__eqvclass__ensure_element_2_4_0),
		mercury__eqvclass__ensure_equivalence_4_0_i2,
		ENTRY(mercury__eqvclass__ensure_equivalence_4_0));
Define_label(mercury__eqvclass__ensure_equivalence_4_0_i2);
	update_prof_current_proc(LABEL(mercury__eqvclass__ensure_equivalence_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__eqvclass__ensure_element_2_4_0),
		mercury__eqvclass__ensure_equivalence_4_0_i3,
		ENTRY(mercury__eqvclass__ensure_equivalence_4_0));
Define_label(mercury__eqvclass__ensure_equivalence_4_0_i3);
	update_prof_current_proc(LABEL(mercury__eqvclass__ensure_equivalence_4_0));
	if (((Integer) detstackvar(1) != (Integer) r1))
		GOTO_LABEL(mercury__eqvclass__ensure_equivalence_4_0_i4);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__eqvclass__ensure_equivalence_4_0_i4);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__eqvclass__add_equivalence_4_0),
		ENTRY(mercury__eqvclass__ensure_equivalence_4_0));
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module6)
	init_entry(mercury__eqvclass__new_equivalence_4_0);
	init_label(mercury__eqvclass__new_equivalence_4_0_i2);
	init_label(mercury__eqvclass__new_equivalence_4_0_i3);
	init_label(mercury__eqvclass__new_equivalence_4_0_i4);
BEGIN_CODE

/* code for predicate 'eqvclass__new_equivalence'/4 in mode 0 */
Define_entry(mercury__eqvclass__new_equivalence_4_0);
	incr_sp_push_msg(3, "eqvclass__new_equivalence");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r1;
	call_localret(STATIC(mercury__eqvclass__ensure_element_2_4_0),
		mercury__eqvclass__new_equivalence_4_0_i2,
		ENTRY(mercury__eqvclass__new_equivalence_4_0));
Define_label(mercury__eqvclass__new_equivalence_4_0_i2);
	update_prof_current_proc(LABEL(mercury__eqvclass__new_equivalence_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__eqvclass__ensure_element_2_4_0),
		mercury__eqvclass__new_equivalence_4_0_i3,
		ENTRY(mercury__eqvclass__new_equivalence_4_0));
Define_label(mercury__eqvclass__new_equivalence_4_0_i3);
	update_prof_current_proc(LABEL(mercury__eqvclass__new_equivalence_4_0));
	if (((Integer) detstackvar(1) != (Integer) r1))
		GOTO_LABEL(mercury__eqvclass__new_equivalence_4_0_i4);
	r1 = string_const("two elements are already equivalent", 35);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__eqvclass__new_equivalence_4_0));
	}
Define_label(mercury__eqvclass__new_equivalence_4_0_i4);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__eqvclass__add_equivalence_4_0),
		ENTRY(mercury__eqvclass__new_equivalence_4_0));
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module7)
	init_entry(mercury__eqvclass__same_eqvclass_3_0);
	init_label(mercury__eqvclass__same_eqvclass_3_0_i2);
	init_label(mercury__eqvclass__same_eqvclass_3_0_i4);
	init_label(mercury__eqvclass__same_eqvclass_3_0_i1);
BEGIN_CODE

/* code for predicate 'eqvclass__same_eqvclass'/3 in mode 0 */
Define_entry(mercury__eqvclass__same_eqvclass_3_0);
	incr_sp_push_msg(4, "eqvclass__same_eqvclass");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	r4 = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(2) = (Integer) r3;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__eqvclass__same_eqvclass_3_0_i2,
		ENTRY(mercury__eqvclass__same_eqvclass_3_0));
	}
Define_label(mercury__eqvclass__same_eqvclass_3_0_i2);
	update_prof_current_proc(LABEL(mercury__eqvclass__same_eqvclass_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__eqvclass__same_eqvclass_3_0_i1);
	r4 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__eqvclass__same_eqvclass_3_0_i4,
		ENTRY(mercury__eqvclass__same_eqvclass_3_0));
	}
Define_label(mercury__eqvclass__same_eqvclass_3_0_i4);
	update_prof_current_proc(LABEL(mercury__eqvclass__same_eqvclass_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__eqvclass__same_eqvclass_3_0_i1);
	if (((Integer) detstackvar(1) != (Integer) r2))
		GOTO_LABEL(mercury__eqvclass__same_eqvclass_3_0_i1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__eqvclass__same_eqvclass_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module8)
	init_entry(mercury__eqvclass__partition_set_2_0);
	init_label(mercury__eqvclass__partition_set_2_0_i2);
	init_label(mercury__eqvclass__partition_set_2_0_i3);
BEGIN_CODE

/* code for predicate 'eqvclass__partition_set'/2 in mode 0 */
Define_entry(mercury__eqvclass__partition_set_2_0);
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	incr_sp_push_msg(3, "eqvclass__partition_set");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	detstackvar(2) = (Integer) r1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__eqvclass__partition_set_2_0_i2,
		ENTRY(mercury__eqvclass__partition_set_2_0));
	}
Define_label(mercury__eqvclass__partition_set_2_0_i2);
	update_prof_current_proc(LABEL(mercury__eqvclass__partition_set_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__eqvclass__partitions_3_0),
		mercury__eqvclass__partition_set_2_0_i3,
		ENTRY(mercury__eqvclass__partition_set_2_0));
Define_label(mercury__eqvclass__partition_set_2_0_i3);
	update_prof_current_proc(LABEL(mercury__eqvclass__partition_set_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__set__list_to_set_2_0);
	tailcall(ENTRY(mercury__set__list_to_set_2_0),
		ENTRY(mercury__eqvclass__partition_set_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module9)
	init_entry(mercury__eqvclass__partition_list_2_0);
	init_label(mercury__eqvclass__partition_list_2_0_i2);
BEGIN_CODE

/* code for predicate 'eqvclass__partition_list'/2 in mode 0 */
Define_entry(mercury__eqvclass__partition_list_2_0);
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	incr_sp_push_msg(3, "eqvclass__partition_list");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	detstackvar(2) = (Integer) r1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__eqvclass__partition_list_2_0_i2,
		ENTRY(mercury__eqvclass__partition_list_2_0));
	}
Define_label(mercury__eqvclass__partition_list_2_0_i2);
	update_prof_current_proc(LABEL(mercury__eqvclass__partition_list_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__eqvclass__partitions_3_0),
		ENTRY(mercury__eqvclass__partition_list_2_0));
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module10)
	init_entry(mercury__eqvclass__partition_set_to_eqvclass_2_0);
	init_label(mercury__eqvclass__partition_set_to_eqvclass_2_0_i2);
BEGIN_CODE

/* code for predicate 'eqvclass__partition_set_to_eqvclass'/2 in mode 0 */
Define_entry(mercury__eqvclass__partition_set_to_eqvclass_2_0);
	incr_sp_push_msg(2, "eqvclass__partition_set_to_eqvclass");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__eqvclass__partition_set_to_eqvclass_2_0_i2,
		ENTRY(mercury__eqvclass__partition_set_to_eqvclass_2_0));
	}
Define_label(mercury__eqvclass__partition_set_to_eqvclass_2_0_i2);
	update_prof_current_proc(LABEL(mercury__eqvclass__partition_set_to_eqvclass_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__eqvclass__partition_list_to_eqvclass_2_0),
		ENTRY(mercury__eqvclass__partition_set_to_eqvclass_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module11)
	init_entry(mercury__eqvclass__partition_list_to_eqvclass_2_0);
	init_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i4);
	init_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i5);
	init_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i6);
	init_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i9);
	init_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i10);
	init_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i1006);
BEGIN_CODE

/* code for predicate 'eqvclass__partition_list_to_eqvclass'/2 in mode 0 */
Define_entry(mercury__eqvclass__partition_list_to_eqvclass_2_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__eqvclass__partition_list_to_eqvclass_2_0_i1006);
	incr_sp_push_msg(6, "eqvclass__partition_list_to_eqvclass");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(5) = (Integer) r1;
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	localcall(mercury__eqvclass__partition_list_to_eqvclass_2_0,
		LABEL(mercury__eqvclass__partition_list_to_eqvclass_2_0_i4),
		ENTRY(mercury__eqvclass__partition_list_to_eqvclass_2_0));
Define_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i4);
	update_prof_current_proc(LABEL(mercury__eqvclass__partition_list_to_eqvclass_2_0));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__eqvclass__partition_list_to_eqvclass_2_0_i5,
		ENTRY(mercury__eqvclass__partition_list_to_eqvclass_2_0));
	}
Define_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i5);
	update_prof_current_proc(LABEL(mercury__eqvclass__partition_list_to_eqvclass_2_0));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__eqvclass__partition_list_to_eqvclass_2_0_i6);
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i6);
	r2 = (Integer) r1;
	r4 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(5);
	detstackvar(4) = ((Integer) r3 + ((Integer) 1));
	call_localret(STATIC(mercury__eqvclass__make_partition_4_0),
		mercury__eqvclass__partition_list_to_eqvclass_2_0_i9,
		ENTRY(mercury__eqvclass__partition_list_to_eqvclass_2_0));
Define_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i9);
	update_prof_current_proc(LABEL(mercury__eqvclass__partition_list_to_eqvclass_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r4 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(3);
	r5 = (Integer) r4;
	r4 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__eqvclass__partition_list_to_eqvclass_2_0_i10,
		ENTRY(mercury__eqvclass__partition_list_to_eqvclass_2_0));
	}
Define_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i10);
	update_prof_current_proc(LABEL(mercury__eqvclass__partition_list_to_eqvclass_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__eqvclass__partition_list_to_eqvclass_2_0_i1006);
	{
		tailcall(STATIC(mercury__eqvclass__init_1_0),
		ENTRY(mercury__eqvclass__partition_list_to_eqvclass_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module12)
	init_entry(mercury__eqvclass__ensure_element_2_4_0);
	init_label(mercury__eqvclass__ensure_element_2_4_0_i4);
	init_label(mercury__eqvclass__ensure_element_2_4_0_i3);
BEGIN_CODE

/* code for predicate 'eqvclass__ensure_element_2'/4 in mode 0 */
Define_static(mercury__eqvclass__ensure_element_2_4_0);
	r4 = (Integer) r3;
	incr_sp_push_msg(4, "eqvclass__ensure_element_2");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) mercury_data___base_type_info_int_0;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__eqvclass__ensure_element_2_4_0_i4,
		STATIC(mercury__eqvclass__ensure_element_2_4_0));
	}
Define_label(mercury__eqvclass__ensure_element_2_4_0_i4);
	update_prof_current_proc(LABEL(mercury__eqvclass__ensure_element_2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__eqvclass__ensure_element_2_4_0_i3);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__eqvclass__ensure_element_2_4_0_i3);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__eqvclass__add_element_4_0),
		STATIC(mercury__eqvclass__ensure_element_2_4_0));
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module13)
	init_entry(mercury__eqvclass__add_element_4_0);
	init_label(mercury__eqvclass__add_element_4_0_i2);
	init_label(mercury__eqvclass__add_element_4_0_i3);
	init_label(mercury__eqvclass__add_element_4_0_i4);
BEGIN_CODE

/* code for predicate 'eqvclass__add_element'/4 in mode 0 */
Define_static(mercury__eqvclass__add_element_4_0);
	r4 = (Integer) r3;
	incr_sp_push_msg(6, "eqvclass__add_element");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	r5 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r2 = (Integer) mercury_data___base_type_info_int_0;
	detstackvar(4) = ((Integer) r5 + ((Integer) 1));
	detstackvar(5) = (Integer) r1;
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__eqvclass__add_element_4_0_i2,
		STATIC(mercury__eqvclass__add_element_4_0));
	}
Define_label(mercury__eqvclass__add_element_4_0_i2);
	update_prof_current_proc(LABEL(mercury__eqvclass__add_element_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__set__singleton_set_2_1);
	call_localret(ENTRY(mercury__set__singleton_set_2_1),
		mercury__eqvclass__add_element_4_0_i3,
		STATIC(mercury__eqvclass__add_element_4_0));
	}
Define_label(mercury__eqvclass__add_element_4_0_i3);
	update_prof_current_proc(LABEL(mercury__eqvclass__add_element_4_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__eqvclass__add_element_4_0_i4,
		STATIC(mercury__eqvclass__add_element_4_0));
	}
Define_label(mercury__eqvclass__add_element_4_0_i4);
	update_prof_current_proc(LABEL(mercury__eqvclass__add_element_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	tag_incr_hp(r2, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module14)
	init_entry(mercury__eqvclass__add_equivalence_4_0);
	init_label(mercury__eqvclass__add_equivalence_4_0_i2);
	init_label(mercury__eqvclass__add_equivalence_4_0_i3);
	init_label(mercury__eqvclass__add_equivalence_4_0_i4);
	init_label(mercury__eqvclass__add_equivalence_4_0_i5);
	init_label(mercury__eqvclass__add_equivalence_4_0_i6);
	init_label(mercury__eqvclass__add_equivalence_4_0_i7);
BEGIN_CODE

/* code for predicate 'eqvclass__add_equivalence'/4 in mode 0 */
Define_static(mercury__eqvclass__add_equivalence_4_0);
	incr_sp_push_msg(7, "eqvclass__add_equivalence");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	detstackvar(6) = (Integer) r1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	{
	Declare_entry(mercury__map__det_remove_4_0);
	call_localret(ENTRY(mercury__map__det_remove_4_0),
		mercury__eqvclass__add_equivalence_4_0_i2,
		STATIC(mercury__eqvclass__add_equivalence_4_0));
	}
Define_label(mercury__eqvclass__add_equivalence_4_0_i2);
	update_prof_current_proc(LABEL(mercury__eqvclass__add_equivalence_4_0));
	detstackvar(4) = (Integer) r1;
	detstackvar(5) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r4 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(6);
	r3 = (Integer) r4;
	r4 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__eqvclass__add_equivalence_4_0_i3,
		STATIC(mercury__eqvclass__add_equivalence_4_0));
	}
Define_label(mercury__eqvclass__add_equivalence_4_0_i3);
	update_prof_current_proc(LABEL(mercury__eqvclass__add_equivalence_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__eqvclass__add_equivalence_4_0_i4,
		STATIC(mercury__eqvclass__add_equivalence_4_0));
	}
Define_label(mercury__eqvclass__add_equivalence_4_0_i4);
	update_prof_current_proc(LABEL(mercury__eqvclass__add_equivalence_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	r5 = (Integer) r3;
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__eqvclass__add_equivalence_4_0_i5,
		STATIC(mercury__eqvclass__add_equivalence_4_0));
	}
Define_label(mercury__eqvclass__add_equivalence_4_0_i5);
	update_prof_current_proc(LABEL(mercury__eqvclass__add_equivalence_4_0));
	r2 = (Integer) detstackvar(4);
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__eqvclass__add_equivalence_4_0_i6,
		STATIC(mercury__eqvclass__add_equivalence_4_0));
	}
Define_label(mercury__eqvclass__add_equivalence_4_0_i6);
	update_prof_current_proc(LABEL(mercury__eqvclass__add_equivalence_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__eqvclass__change_partition_4_0),
		mercury__eqvclass__add_equivalence_4_0_i7,
		STATIC(mercury__eqvclass__add_equivalence_4_0));
Define_label(mercury__eqvclass__add_equivalence_4_0_i7);
	update_prof_current_proc(LABEL(mercury__eqvclass__add_equivalence_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module15)
	init_entry(mercury__eqvclass__change_partition_4_0);
	init_label(mercury__eqvclass__change_partition_4_0_i4);
	init_label(mercury__eqvclass__change_partition_4_0_i1002);
BEGIN_CODE

/* code for predicate 'eqvclass__change_partition'/4 in mode 0 */
Define_static(mercury__eqvclass__change_partition_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__eqvclass__change_partition_4_0_i1002);
	incr_sp_push_msg(4, "eqvclass__change_partition");
	detstackvar(4) = (Integer) succip;
	r5 = (Integer) r3;
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) mercury_data___base_type_info_int_0;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__eqvclass__change_partition_4_0_i4,
		STATIC(mercury__eqvclass__change_partition_4_0));
	}
Define_label(mercury__eqvclass__change_partition_4_0_i4);
	update_prof_current_proc(LABEL(mercury__eqvclass__change_partition_4_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__eqvclass__change_partition_4_0,
		STATIC(mercury__eqvclass__change_partition_4_0));
Define_label(mercury__eqvclass__change_partition_4_0_i1002);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module16)
	init_entry(mercury__eqvclass__partitions_3_0);
	init_label(mercury__eqvclass__partitions_3_0_i6);
	init_label(mercury__eqvclass__partitions_3_0_i5);
	init_label(mercury__eqvclass__partitions_3_0_i8);
	init_label(mercury__eqvclass__partitions_3_0_i9);
	init_label(mercury__eqvclass__partitions_3_0_i10);
	init_label(mercury__eqvclass__partitions_3_0_i1004);
BEGIN_CODE

/* code for predicate 'eqvclass__partitions'/3 in mode 0 */
Define_static(mercury__eqvclass__partitions_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__eqvclass__partitions_3_0_i1004);
	incr_sp_push_msg(5, "eqvclass__partitions");
	detstackvar(5) = (Integer) succip;
	r4 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	detstackvar(4) = (Integer) r1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__eqvclass__partitions_3_0_i6,
		STATIC(mercury__eqvclass__partitions_3_0));
	}
Define_label(mercury__eqvclass__partitions_3_0_i6);
	update_prof_current_proc(LABEL(mercury__eqvclass__partitions_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__eqvclass__partitions_3_0_i5);
	r4 = (Integer) r2;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	GOTO_LABEL(mercury__eqvclass__partitions_3_0_i9);
Define_label(mercury__eqvclass__partitions_3_0_i5);
	r1 = string_const("partition id not known to equivalence class", 43);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__eqvclass__partitions_3_0_i8,
		STATIC(mercury__eqvclass__partitions_3_0));
	}
Define_label(mercury__eqvclass__partitions_3_0_i8);
	update_prof_current_proc(LABEL(mercury__eqvclass__partitions_3_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r1 = (Integer) detstackvar(4);
Define_label(mercury__eqvclass__partitions_3_0_i9);
	detstackvar(3) = (Integer) r4;
	localcall(mercury__eqvclass__partitions_3_0,
		LABEL(mercury__eqvclass__partitions_3_0_i10),
		STATIC(mercury__eqvclass__partitions_3_0));
Define_label(mercury__eqvclass__partitions_3_0_i10);
	update_prof_current_proc(LABEL(mercury__eqvclass__partitions_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__eqvclass__partitions_3_0_i1004);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module17)
	init_entry(mercury__eqvclass__make_partition_4_0);
	init_label(mercury__eqvclass__make_partition_4_0_i4);
	init_label(mercury__eqvclass__make_partition_4_0_i1002);
BEGIN_CODE

/* code for predicate 'eqvclass__make_partition'/4 in mode 0 */
Define_static(mercury__eqvclass__make_partition_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__eqvclass__make_partition_4_0_i1002);
	incr_sp_push_msg(4, "eqvclass__make_partition");
	detstackvar(4) = (Integer) succip;
	r5 = (Integer) r3;
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) mercury_data___base_type_info_int_0;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__eqvclass__make_partition_4_0_i4,
		STATIC(mercury__eqvclass__make_partition_4_0));
	}
Define_label(mercury__eqvclass__make_partition_4_0_i4);
	update_prof_current_proc(LABEL(mercury__eqvclass__make_partition_4_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__eqvclass__make_partition_4_0,
		STATIC(mercury__eqvclass__make_partition_4_0));
Define_label(mercury__eqvclass__make_partition_4_0_i1002);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module18)
	init_entry(mercury____Unify___eqvclass__eqvclass_1_0);
	init_label(mercury____Unify___eqvclass__eqvclass_1_0_i2);
	init_label(mercury____Unify___eqvclass__eqvclass_1_0_i1003);
	init_label(mercury____Unify___eqvclass__eqvclass_1_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___eqvclass__eqvclass_1_0);
	if (((Integer) field(mktag(0), (Integer) r2, ((Integer) 0)) != (Integer) field(mktag(0), (Integer) r3, ((Integer) 0))))
		GOTO_LABEL(mercury____Unify___eqvclass__eqvclass_1_0_i1003);
	incr_sp_push_msg(4, "__Unify__");
	detstackvar(4) = (Integer) succip;
	r4 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 2));
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	detstackvar(3) = (Integer) r1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Unify___tree234__tree234_2_0),
		mercury____Unify___eqvclass__eqvclass_1_0_i2,
		ENTRY(mercury____Unify___eqvclass__eqvclass_1_0));
	}
Define_label(mercury____Unify___eqvclass__eqvclass_1_0_i2);
	update_prof_current_proc(LABEL(mercury____Unify___eqvclass__eqvclass_1_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___eqvclass__eqvclass_1_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___eqvclass__eqvclass_1_0));
	}
Define_label(mercury____Unify___eqvclass__eqvclass_1_0_i1003);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___eqvclass__eqvclass_1_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module19)
	init_entry(mercury____Index___eqvclass__eqvclass_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___eqvclass__eqvclass_1_0);
	tailcall(STATIC(mercury____Index___eqvclass_eqvclass_1__ua10000_2_0),
		ENTRY(mercury____Index___eqvclass__eqvclass_1_0));
END_MODULE

BEGIN_MODULE(mercury__eqvclass_module20)
	init_entry(mercury____Compare___eqvclass__eqvclass_1_0);
	init_label(mercury____Compare___eqvclass__eqvclass_1_0_i4);
	init_label(mercury____Compare___eqvclass__eqvclass_1_0_i5);
	init_label(mercury____Compare___eqvclass__eqvclass_1_0_i3);
	init_label(mercury____Compare___eqvclass__eqvclass_1_0_i10);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___eqvclass__eqvclass_1_0);
	incr_sp_push_msg(6, "__Compare__");
	detstackvar(6) = (Integer) succip;
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r2 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 2));
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	call_localret(ENTRY(mercury__builtin_compare_int_3_0),
		mercury____Compare___eqvclass__eqvclass_1_0_i4,
		ENTRY(mercury____Compare___eqvclass__eqvclass_1_0));
	}
Define_label(mercury____Compare___eqvclass__eqvclass_1_0_i4);
	update_prof_current_proc(LABEL(mercury____Compare___eqvclass__eqvclass_1_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___eqvclass__eqvclass_1_0_i3);
Define_label(mercury____Compare___eqvclass__eqvclass_1_0_i5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury____Compare___eqvclass__eqvclass_1_0_i3);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mercury_data_set__base_type_info_set_1;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Compare___tree234__tree234_2_0),
		mercury____Compare___eqvclass__eqvclass_1_0_i10,
		ENTRY(mercury____Compare___eqvclass__eqvclass_1_0));
	}
Define_label(mercury____Compare___eqvclass__eqvclass_1_0_i10);
	update_prof_current_proc(LABEL(mercury____Compare___eqvclass__eqvclass_1_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___eqvclass__eqvclass_1_0_i5);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___eqvclass__eqvclass_1_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__eqvclass_bunch_0(void)
{
	mercury__eqvclass_module0();
	mercury__eqvclass_module1();
	mercury__eqvclass_module2();
	mercury__eqvclass_module3();
	mercury__eqvclass_module4();
	mercury__eqvclass_module5();
	mercury__eqvclass_module6();
	mercury__eqvclass_module7();
	mercury__eqvclass_module8();
	mercury__eqvclass_module9();
	mercury__eqvclass_module10();
	mercury__eqvclass_module11();
	mercury__eqvclass_module12();
	mercury__eqvclass_module13();
	mercury__eqvclass_module14();
	mercury__eqvclass_module15();
	mercury__eqvclass_module16();
	mercury__eqvclass_module17();
	mercury__eqvclass_module18();
	mercury__eqvclass_module19();
	mercury__eqvclass_module20();
}

#endif

void mercury__eqvclass__init(void); /* suppress gcc warning */
void mercury__eqvclass__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__eqvclass_bunch_0();
#endif
}
