/*
** Automatically generated from `varset.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__varset__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Index___varset_varset_0__ua10000_2_0);
Declare_static(mercury__varset__merge_subst_2__ua10000_8_0);
Declare_label(mercury__varset__merge_subst_2__ua10000_8_0_i4);
Declare_label(mercury__varset__merge_subst_2__ua10000_8_0_i3);
Declare_label(mercury__varset__merge_subst_2__ua10000_8_0_i6);
Declare_label(mercury__varset__merge_subst_2__ua10000_8_0_i7);
Declare_label(mercury__varset__merge_subst_2__ua10000_8_0_i10);
Declare_label(mercury__varset__merge_subst_2__ua10000_8_0_i12);
Declare_label(mercury__varset__merge_subst_2__ua10000_8_0_i9);
Declare_label(mercury__varset__merge_subst_2__ua10000_8_0_i13);
Declare_label(mercury__varset__merge_subst_2__ua10000_8_0_i14);
Define_extern_entry(mercury__varset__init_1_0);
Declare_label(mercury__varset__init_1_0_i2);
Declare_label(mercury__varset__init_1_0_i3);
Declare_label(mercury__varset__init_1_0_i4);
Define_extern_entry(mercury__varset__is_empty_1_0);
Define_extern_entry(mercury__varset__new_var_3_0);
Declare_label(mercury__varset__new_var_3_0_i2);
Define_extern_entry(mercury__varset__new_vars_4_0);
Define_extern_entry(mercury__varset__delete_var_3_0);
Declare_label(mercury__varset__delete_var_3_0_i2);
Declare_label(mercury__varset__delete_var_3_0_i3);
Define_extern_entry(mercury__varset__delete_vars_3_0);
Declare_label(mercury__varset__delete_vars_3_0_i4);
Declare_label(mercury__varset__delete_vars_3_0_i1002);
Define_extern_entry(mercury__varset__vars_2_0);
Declare_label(mercury__varset__vars_2_0_i2);
Declare_label(mercury__varset__vars_2_0_i3);
Define_extern_entry(mercury__varset__name_var_4_0);
Declare_label(mercury__varset__name_var_4_0_i4);
Declare_label(mercury__varset__name_var_4_0_i6);
Declare_label(mercury__varset__name_var_4_0_i3);
Declare_label(mercury__varset__name_var_4_0_i7);
Declare_label(mercury__varset__name_var_4_0_i8);
Define_extern_entry(mercury__varset__lookup_name_3_0);
Declare_label(mercury__varset__lookup_name_3_0_i4);
Declare_label(mercury__varset__lookup_name_3_0_i3);
Declare_label(mercury__varset__lookup_name_3_0_i6);
Declare_label(mercury__varset__lookup_name_3_0_i7);
Define_extern_entry(mercury__varset__lookup_name_4_0);
Declare_label(mercury__varset__lookup_name_4_0_i4);
Declare_label(mercury__varset__lookup_name_4_0_i3);
Declare_label(mercury__varset__lookup_name_4_0_i6);
Declare_label(mercury__varset__lookup_name_4_0_i7);
Define_extern_entry(mercury__varset__search_name_3_0);
Declare_label(mercury__varset__search_name_3_0_i2);
Declare_label(mercury__varset__search_name_3_0_i1000);
Define_extern_entry(mercury__varset__bind_var_4_0);
Declare_label(mercury__varset__bind_var_4_0_i2);
Define_extern_entry(mercury__varset__bind_vars_3_0);
Declare_label(mercury__varset__bind_vars_3_0_i2);
Define_extern_entry(mercury__varset__search_var_3_0);
Declare_label(mercury__varset__search_var_3_0_i2);
Declare_label(mercury__varset__search_var_3_0_i1000);
Define_extern_entry(mercury__varset__lookup_vars_2_0);
Define_extern_entry(mercury__varset__merge_5_0);
Declare_label(mercury__varset__merge_5_0_i2);
Declare_label(mercury__varset__merge_5_0_i3);
Define_extern_entry(mercury__varset__merge_subst_4_0);
Declare_label(mercury__varset__merge_subst_4_0_i2);
Declare_label(mercury__varset__merge_subst_4_0_i3);
Define_extern_entry(mercury__varset__get_bindings_2_0);
Define_extern_entry(mercury__varset__set_bindings_3_0);
Define_extern_entry(mercury__varset__create_name_var_map_2_0);
Declare_label(mercury__varset__create_name_var_map_2_0_i2);
Declare_label(mercury__varset__create_name_var_map_2_0_i3);
Define_extern_entry(mercury__varset__var_name_list_2_0);
Define_extern_entry(mercury__varset__ensure_unique_names_4_0);
Declare_label(mercury__varset__ensure_unique_names_4_0_i2);
Declare_label(mercury__varset__ensure_unique_names_4_0_i3);
Declare_label(mercury__varset__ensure_unique_names_4_0_i4);
Declare_static(mercury__varset__new_vars_2_5_0);
Declare_label(mercury__varset__new_vars_2_5_0_i4);
Declare_label(mercury__varset__new_vars_2_5_0_i1003);
Declare_label(mercury__varset__new_vars_2_5_0_i1004);
Declare_static(mercury__varset__vars_2_4_0);
Declare_label(mercury__varset__vars_2_4_0_i4);
Declare_label(mercury__varset__vars_2_4_0_i3);
Declare_label(mercury__varset__vars_2_4_0_i6);
Declare_static(mercury__varset__bind_vars_2_3_0);
Declare_label(mercury__varset__bind_vars_2_3_0_i4);
Declare_label(mercury__varset__bind_vars_2_3_0_i1002);
Declare_static(mercury__varset__ensure_unique_names_2_6_0);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i6);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i10);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i12);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i13);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i14);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i15);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i9);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i5);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i17);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i18);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i19);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i20);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i21);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i22);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i23);
Declare_label(mercury__varset__ensure_unique_names_2_6_0_i1004);
Declare_static(mercury__varset__ensure_unique_names_3_4_0);
Declare_label(mercury__varset__ensure_unique_names_3_4_0_i4);
Declare_label(mercury__varset__ensure_unique_names_3_4_0_i6);
Declare_label(mercury__varset__ensure_unique_names_3_4_0_i3);
Define_extern_entry(mercury____Unify___varset__varset_0_0);
Declare_label(mercury____Unify___varset__varset_0_0_i2);
Declare_label(mercury____Unify___varset__varset_0_0_i4);
Declare_label(mercury____Unify___varset__varset_0_0_i1);
Define_extern_entry(mercury____Index___varset__varset_0_0);
Define_extern_entry(mercury____Compare___varset__varset_0_0);
Declare_label(mercury____Compare___varset__varset_0_0_i4);
Declare_label(mercury____Compare___varset__varset_0_0_i5);
Declare_label(mercury____Compare___varset__varset_0_0_i3);
Declare_label(mercury____Compare___varset__varset_0_0_i10);

extern Word * mercury_data_varset__base_type_layout_varset_0[];
Word * mercury_data_varset__base_type_info_varset_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___varset__varset_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___varset__varset_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___varset__varset_0_0),
	(Word *) (Integer) mercury_data_varset__base_type_layout_varset_0
};

extern Word * mercury_data_varset__common_3[];
Word * mercury_data_varset__base_type_layout_varset_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_varset__common_3),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_mercury_builtin__base_type_info_var_supply_0[];
Word * mercury_data_varset__common_0[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_var_supply_0
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data_mercury_builtin__base_type_info_var_0[];
extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_varset__common_1[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_var_0,
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

extern Word * mercury_data_mercury_builtin__base_type_info_term_0[];
Word * mercury_data_varset__common_2[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_var_0,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_term_0
};

Word * mercury_data_varset__common_3[] = {
	(Word *) ((Integer) 3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_varset__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_varset__common_1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_varset__common_2),
	(Word *) string_const("varset", 6)
};

BEGIN_MODULE(mercury__varset_module0)
	init_entry(mercury____Index___varset_varset_0__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___varset_varset_0__ua10000'/2 in mode 0 */
Define_static(mercury____Index___varset_varset_0__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module1)
	init_entry(mercury__varset__merge_subst_2__ua10000_8_0);
	init_label(mercury__varset__merge_subst_2__ua10000_8_0_i4);
	init_label(mercury__varset__merge_subst_2__ua10000_8_0_i3);
	init_label(mercury__varset__merge_subst_2__ua10000_8_0_i6);
	init_label(mercury__varset__merge_subst_2__ua10000_8_0_i7);
	init_label(mercury__varset__merge_subst_2__ua10000_8_0_i10);
	init_label(mercury__varset__merge_subst_2__ua10000_8_0_i12);
	init_label(mercury__varset__merge_subst_2__ua10000_8_0_i9);
	init_label(mercury__varset__merge_subst_2__ua10000_8_0_i13);
	init_label(mercury__varset__merge_subst_2__ua10000_8_0_i14);
BEGIN_CODE

/* code for predicate 'varset__merge_subst_2__ua10000'/8 in mode 0 */
Define_static(mercury__varset__merge_subst_2__ua10000_8_0);
	incr_sp_push_msg(9, "varset__merge_subst_2__ua10000");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	{
	Declare_entry(mercury____Unify___mercury_builtin__var_supply_0_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__var_supply_0_0),
		mercury__varset__merge_subst_2__ua10000_8_0_i4,
		STATIC(mercury__varset__merge_subst_2__ua10000_8_0));
	}
Define_label(mercury__varset__merge_subst_2__ua10000_8_0_i4);
	update_prof_current_proc(LABEL(mercury__varset__merge_subst_2__ua10000_8_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__varset__merge_subst_2__ua10000_8_0_i3);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__varset__merge_subst_2__ua10000_8_0_i3);
	r1 = (Integer) detstackvar(4);
	{
		call_localret(STATIC(mercury__varset__new_var_3_0),
		mercury__varset__merge_subst_2__ua10000_8_0_i6,
		STATIC(mercury__varset__merge_subst_2__ua10000_8_0));
	}
Define_label(mercury__varset__merge_subst_2__ua10000_8_0_i6);
	update_prof_current_proc(LABEL(mercury__varset__merge_subst_2__ua10000_8_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__term__create_var_3_0);
	call_localret(ENTRY(mercury__term__create_var_3_0),
		mercury__varset__merge_subst_2__ua10000_8_0_i7,
		STATIC(mercury__varset__merge_subst_2__ua10000_8_0));
	}
Define_label(mercury__varset__merge_subst_2__ua10000_8_0_i7);
	update_prof_current_proc(LABEL(mercury__varset__merge_subst_2__ua10000_8_0));
	r4 = (Integer) r1;
	detstackvar(6) = (Integer) r1;
	detstackvar(7) = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__varset__merge_subst_2__ua10000_8_0_i10,
		STATIC(mercury__varset__merge_subst_2__ua10000_8_0));
	}
Define_label(mercury__varset__merge_subst_2__ua10000_8_0_i10);
	update_prof_current_proc(LABEL(mercury__varset__merge_subst_2__ua10000_8_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__varset__merge_subst_2__ua10000_8_0_i9);
	r1 = (Integer) detstackvar(4);
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__varset__name_var_4_0),
		mercury__varset__merge_subst_2__ua10000_8_0_i12,
		STATIC(mercury__varset__merge_subst_2__ua10000_8_0));
	}
Define_label(mercury__varset__merge_subst_2__ua10000_8_0_i12);
	update_prof_current_proc(LABEL(mercury__varset__merge_subst_2__ua10000_8_0));
	r6 = (Integer) detstackvar(2);
	r7 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(7);
	r9 = (Integer) r1;
	tag_incr_hp(r5, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) detstackvar(1);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	GOTO_LABEL(mercury__varset__merge_subst_2__ua10000_8_0_i13);
Define_label(mercury__varset__merge_subst_2__ua10000_8_0_i9);
	r6 = (Integer) detstackvar(2);
	r7 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(7);
	r9 = (Integer) detstackvar(4);
	tag_incr_hp(r5, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) detstackvar(1);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
Define_label(mercury__varset__merge_subst_2__ua10000_8_0_i13);
	detstackvar(2) = (Integer) r6;
	detstackvar(3) = (Integer) r7;
	detstackvar(7) = (Integer) r8;
	detstackvar(8) = (Integer) r9;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__varset__merge_subst_2__ua10000_8_0_i14,
		STATIC(mercury__varset__merge_subst_2__ua10000_8_0));
	}
Define_label(mercury__varset__merge_subst_2__ua10000_8_0_i14);
	update_prof_current_proc(LABEL(mercury__varset__merge_subst_2__ua10000_8_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(8);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__varset__merge_subst_2__ua10000_8_0,
		STATIC(mercury__varset__merge_subst_2__ua10000_8_0));
END_MODULE

BEGIN_MODULE(mercury__varset_module2)
	init_entry(mercury__varset__init_1_0);
	init_label(mercury__varset__init_1_0_i2);
	init_label(mercury__varset__init_1_0_i3);
	init_label(mercury__varset__init_1_0_i4);
BEGIN_CODE

/* code for predicate 'varset__init'/1 in mode 0 */
Define_entry(mercury__varset__init_1_0);
	incr_sp_push_msg(3, "varset__init");
	detstackvar(3) = (Integer) succip;
	{
	Declare_entry(mercury__term__init_var_supply_1_0);
	call_localret(ENTRY(mercury__term__init_var_supply_1_0),
		mercury__varset__init_1_0_i2,
		ENTRY(mercury__varset__init_1_0));
	}
Define_label(mercury__varset__init_1_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__init_1_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__varset__init_1_0_i3,
		ENTRY(mercury__varset__init_1_0));
	}
Define_label(mercury__varset__init_1_0_i3);
	update_prof_current_proc(LABEL(mercury__varset__init_1_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__varset__init_1_0_i4,
		ENTRY(mercury__varset__init_1_0));
	}
Define_label(mercury__varset__init_1_0_i4);
	update_prof_current_proc(LABEL(mercury__varset__init_1_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module3)
	init_entry(mercury__varset__is_empty_1_0);
BEGIN_CODE

/* code for predicate 'varset__is_empty'/1 in mode 0 */
Define_entry(mercury__varset__is_empty_1_0);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__term__init_var_supply_1_1);
	tailcall(ENTRY(mercury__term__init_var_supply_1_1),
		ENTRY(mercury__varset__is_empty_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__varset_module4)
	init_entry(mercury__varset__new_var_3_0);
	init_label(mercury__varset__new_var_3_0_i2);
BEGIN_CODE

/* code for predicate 'varset__new_var'/3 in mode 0 */
Define_entry(mercury__varset__new_var_3_0);
	incr_sp_push_msg(3, "varset__new_var");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__term__create_var_3_0);
	call_localret(ENTRY(mercury__term__create_var_3_0),
		mercury__varset__new_var_3_0_i2,
		ENTRY(mercury__varset__new_var_3_0));
	}
Define_label(mercury__varset__new_var_3_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__new_var_3_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module5)
	init_entry(mercury__varset__new_vars_4_0);
BEGIN_CODE

/* code for predicate 'varset__new_vars'/4 in mode 0 */
Define_entry(mercury__varset__new_vars_4_0);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tailcall(STATIC(mercury__varset__new_vars_2_5_0),
		ENTRY(mercury__varset__new_vars_4_0));
END_MODULE

BEGIN_MODULE(mercury__varset_module6)
	init_entry(mercury__varset__delete_var_3_0);
	init_label(mercury__varset__delete_var_3_0_i2);
	init_label(mercury__varset__delete_var_3_0_i3);
BEGIN_CODE

/* code for predicate 'varset__delete_var'/3 in mode 0 */
Define_entry(mercury__varset__delete_var_3_0);
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r4 = (Integer) r2;
	incr_sp_push_msg(4, "varset__delete_var");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__map__delete_3_1);
	call_localret(ENTRY(mercury__map__delete_3_1),
		mercury__varset__delete_var_3_0_i2,
		ENTRY(mercury__varset__delete_var_3_0));
	}
Define_label(mercury__varset__delete_var_3_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__delete_var_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r4 = (Integer) r2;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__delete_3_1);
	call_localret(ENTRY(mercury__map__delete_3_1),
		mercury__varset__delete_var_3_0_i3,
		ENTRY(mercury__varset__delete_var_3_0));
	}
Define_label(mercury__varset__delete_var_3_0_i3);
	update_prof_current_proc(LABEL(mercury__varset__delete_var_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module7)
	init_entry(mercury__varset__delete_vars_3_0);
	init_label(mercury__varset__delete_vars_3_0_i4);
	init_label(mercury__varset__delete_vars_3_0_i1002);
BEGIN_CODE

/* code for predicate 'varset__delete_vars'/3 in mode 0 */
Define_entry(mercury__varset__delete_vars_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__varset__delete_vars_3_0_i1002);
	incr_sp_push_msg(2, "varset__delete_vars");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	{
		call_localret(STATIC(mercury__varset__delete_var_3_0),
		mercury__varset__delete_vars_3_0_i4,
		ENTRY(mercury__varset__delete_vars_3_0));
	}
Define_label(mercury__varset__delete_vars_3_0_i4);
	update_prof_current_proc(LABEL(mercury__varset__delete_vars_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__varset__delete_vars_3_0,
		ENTRY(mercury__varset__delete_vars_3_0));
Define_label(mercury__varset__delete_vars_3_0_i1002);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module8)
	init_entry(mercury__varset__vars_2_0);
	init_label(mercury__varset__vars_2_0_i2);
	init_label(mercury__varset__vars_2_0_i3);
BEGIN_CODE

/* code for predicate 'varset__vars'/2 in mode 0 */
Define_entry(mercury__varset__vars_2_0);
	incr_sp_push_msg(2, "varset__vars");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__term__init_var_supply_1_0);
	call_localret(ENTRY(mercury__term__init_var_supply_1_0),
		mercury__varset__vars_2_0_i2,
		ENTRY(mercury__varset__vars_2_0));
	}
Define_label(mercury__varset__vars_2_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__vars_2_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	call_localret(STATIC(mercury__varset__vars_2_4_0),
		mercury__varset__vars_2_0_i3,
		ENTRY(mercury__varset__vars_2_0));
Define_label(mercury__varset__vars_2_0_i3);
	update_prof_current_proc(LABEL(mercury__varset__vars_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__list__reverse_2_0);
	tailcall(ENTRY(mercury__list__reverse_2_0),
		ENTRY(mercury__varset__vars_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__varset_module9)
	init_entry(mercury__varset__name_var_4_0);
	init_label(mercury__varset__name_var_4_0_i4);
	init_label(mercury__varset__name_var_4_0_i6);
	init_label(mercury__varset__name_var_4_0_i3);
	init_label(mercury__varset__name_var_4_0_i7);
	init_label(mercury__varset__name_var_4_0_i8);
BEGIN_CODE

/* code for predicate 'varset__name_var'/4 in mode 0 */
Define_entry(mercury__varset__name_var_4_0);
	incr_sp_push_msg(6, "varset__name_var");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) r3;
	r2 = string_const("'", 1);
	{
	Declare_entry(mercury__string__remove_suffix_3_0);
	call_localret(ENTRY(mercury__string__remove_suffix_3_0),
		mercury__varset__name_var_4_0_i4,
		ENTRY(mercury__varset__name_var_4_0));
	}
Define_label(mercury__varset__name_var_4_0_i4);
	update_prof_current_proc(LABEL(mercury__varset__name_var_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__varset__name_var_4_0_i3);
	r1 = string_const("varset__name_var: name is already primed", 40);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__varset__name_var_4_0_i6,
		ENTRY(mercury__varset__name_var_4_0));
	}
Define_label(mercury__varset__name_var_4_0_i6);
	update_prof_current_proc(LABEL(mercury__varset__name_var_4_0));
	{
	Word tempr1;
	tempr1 = (Integer) r3;
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) tempr1;
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	GOTO_LABEL(mercury__varset__name_var_4_0_i7);
	}
Define_label(mercury__varset__name_var_4_0_i3);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(1);
	r7 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 2));
	r3 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r6 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	}
Define_label(mercury__varset__name_var_4_0_i7);
	detstackvar(4) = (Integer) r6;
	detstackvar(5) = (Integer) r7;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__varset__name_var_4_0_i8,
		ENTRY(mercury__varset__name_var_4_0));
	}
Define_label(mercury__varset__name_var_4_0_i8);
	update_prof_current_proc(LABEL(mercury__varset__name_var_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module10)
	init_entry(mercury__varset__lookup_name_3_0);
	init_label(mercury__varset__lookup_name_3_0_i4);
	init_label(mercury__varset__lookup_name_3_0_i3);
	init_label(mercury__varset__lookup_name_3_0_i6);
	init_label(mercury__varset__lookup_name_3_0_i7);
BEGIN_CODE

/* code for predicate 'varset__lookup_name'/3 in mode 0 */
Define_entry(mercury__varset__lookup_name_3_0);
	incr_sp_push_msg(2, "varset__lookup_name");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
		call_localret(STATIC(mercury__varset__search_name_3_0),
		mercury__varset__lookup_name_3_0_i4,
		ENTRY(mercury__varset__lookup_name_3_0));
	}
Define_label(mercury__varset__lookup_name_3_0_i4);
	update_prof_current_proc(LABEL(mercury__varset__lookup_name_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__varset__lookup_name_3_0_i3);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__varset__lookup_name_3_0_i3);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__term__var_to_int_2_0);
	call_localret(ENTRY(mercury__term__var_to_int_2_0),
		mercury__varset__lookup_name_3_0_i6,
		ENTRY(mercury__varset__lookup_name_3_0));
	}
Define_label(mercury__varset__lookup_name_3_0_i6);
	update_prof_current_proc(LABEL(mercury__varset__lookup_name_3_0));
	{
	Declare_entry(mercury__string__int_to_string_2_0);
	call_localret(ENTRY(mercury__string__int_to_string_2_0),
		mercury__varset__lookup_name_3_0_i7,
		ENTRY(mercury__varset__lookup_name_3_0));
	}
Define_label(mercury__varset__lookup_name_3_0_i7);
	update_prof_current_proc(LABEL(mercury__varset__lookup_name_3_0));
	r2 = (Integer) r1;
	r1 = string_const("V_", 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__string__append_3_2);
	tailcall(ENTRY(mercury__string__append_3_2),
		ENTRY(mercury__varset__lookup_name_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__varset_module11)
	init_entry(mercury__varset__lookup_name_4_0);
	init_label(mercury__varset__lookup_name_4_0_i4);
	init_label(mercury__varset__lookup_name_4_0_i3);
	init_label(mercury__varset__lookup_name_4_0_i6);
	init_label(mercury__varset__lookup_name_4_0_i7);
BEGIN_CODE

/* code for predicate 'varset__lookup_name'/4 in mode 0 */
Define_entry(mercury__varset__lookup_name_4_0);
	incr_sp_push_msg(3, "varset__lookup_name");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	{
		call_localret(STATIC(mercury__varset__search_name_3_0),
		mercury__varset__lookup_name_4_0_i4,
		ENTRY(mercury__varset__lookup_name_4_0));
	}
Define_label(mercury__varset__lookup_name_4_0_i4);
	update_prof_current_proc(LABEL(mercury__varset__lookup_name_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__varset__lookup_name_4_0_i3);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__varset__lookup_name_4_0_i3);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__term__var_to_int_2_0);
	call_localret(ENTRY(mercury__term__var_to_int_2_0),
		mercury__varset__lookup_name_4_0_i6,
		ENTRY(mercury__varset__lookup_name_4_0));
	}
Define_label(mercury__varset__lookup_name_4_0_i6);
	update_prof_current_proc(LABEL(mercury__varset__lookup_name_4_0));
	{
	Declare_entry(mercury__string__int_to_string_2_0);
	call_localret(ENTRY(mercury__string__int_to_string_2_0),
		mercury__varset__lookup_name_4_0_i7,
		ENTRY(mercury__varset__lookup_name_4_0));
	}
Define_label(mercury__varset__lookup_name_4_0_i7);
	update_prof_current_proc(LABEL(mercury__varset__lookup_name_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__string__append_3_2);
	tailcall(ENTRY(mercury__string__append_3_2),
		ENTRY(mercury__varset__lookup_name_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__varset_module12)
	init_entry(mercury__varset__search_name_3_0);
	init_label(mercury__varset__search_name_3_0_i2);
	init_label(mercury__varset__search_name_3_0_i1000);
BEGIN_CODE

/* code for predicate 'varset__search_name'/3 in mode 0 */
Define_entry(mercury__varset__search_name_3_0);
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r4 = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	incr_sp_push_msg(1, "varset__search_name");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__varset__search_name_3_0_i2,
		ENTRY(mercury__varset__search_name_3_0));
	}
Define_label(mercury__varset__search_name_3_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__search_name_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__varset__search_name_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__varset__search_name_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module13)
	init_entry(mercury__varset__bind_var_4_0);
	init_label(mercury__varset__bind_var_4_0_i2);
BEGIN_CODE

/* code for predicate 'varset__bind_var'/4 in mode 0 */
Define_entry(mercury__varset__bind_var_4_0);
	r5 = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	r4 = (Integer) r2;
	incr_sp_push_msg(3, "varset__bind_var");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__varset__bind_var_4_0_i2,
		ENTRY(mercury__varset__bind_var_4_0));
	}
Define_label(mercury__varset__bind_var_4_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__bind_var_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module14)
	init_entry(mercury__varset__bind_vars_3_0);
	init_label(mercury__varset__bind_vars_3_0_i2);
BEGIN_CODE

/* code for predicate 'varset__bind_vars'/3 in mode 0 */
Define_entry(mercury__varset__bind_vars_3_0);
	r3 = (Integer) r2;
	incr_sp_push_msg(2, "varset__bind_vars");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	{
	Declare_entry(mercury__map__to_assoc_list_2_0);
	call_localret(ENTRY(mercury__map__to_assoc_list_2_0),
		mercury__varset__bind_vars_3_0_i2,
		ENTRY(mercury__varset__bind_vars_3_0));
	}
Define_label(mercury__varset__bind_vars_3_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__bind_vars_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__varset__bind_vars_2_3_0),
		ENTRY(mercury__varset__bind_vars_3_0));
END_MODULE

BEGIN_MODULE(mercury__varset_module15)
	init_entry(mercury__varset__search_var_3_0);
	init_label(mercury__varset__search_var_3_0_i2);
	init_label(mercury__varset__search_var_3_0_i1000);
BEGIN_CODE

/* code for predicate 'varset__search_var'/3 in mode 0 */
Define_entry(mercury__varset__search_var_3_0);
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	r4 = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	incr_sp_push_msg(1, "varset__search_var");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__varset__search_var_3_0_i2,
		ENTRY(mercury__varset__search_var_3_0));
	}
Define_label(mercury__varset__search_var_3_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__search_var_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__varset__search_var_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__varset__search_var_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module16)
	init_entry(mercury__varset__lookup_vars_2_0);
BEGIN_CODE

/* code for predicate 'varset__lookup_vars'/2 in mode 0 */
Define_entry(mercury__varset__lookup_vars_2_0);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module17)
	init_entry(mercury__varset__merge_5_0);
	init_label(mercury__varset__merge_5_0_i2);
	init_label(mercury__varset__merge_5_0_i3);
BEGIN_CODE

/* code for predicate 'varset__merge'/5 in mode 0 */
Define_entry(mercury__varset__merge_5_0);
	incr_sp_push_msg(2, "varset__merge");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	{
		call_localret(STATIC(mercury__varset__merge_subst_4_0),
		mercury__varset__merge_5_0_i2,
		ENTRY(mercury__varset__merge_5_0));
	}
Define_label(mercury__varset__merge_5_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__merge_5_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__term__apply_substitution_to_list_3_0);
	call_localret(ENTRY(mercury__term__apply_substitution_to_list_3_0),
		mercury__varset__merge_5_0_i3,
		ENTRY(mercury__varset__merge_5_0));
	}
Define_label(mercury__varset__merge_5_0_i3);
	update_prof_current_proc(LABEL(mercury__varset__merge_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module18)
	init_entry(mercury__varset__merge_subst_4_0);
	init_label(mercury__varset__merge_subst_4_0_i2);
	init_label(mercury__varset__merge_subst_4_0_i3);
BEGIN_CODE

/* code for predicate 'varset__merge_subst'/4 in mode 0 */
Define_entry(mercury__varset__merge_subst_4_0);
	incr_sp_push_msg(5, "varset__merge_subst");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	{
	Declare_entry(mercury__term__init_var_supply_1_0);
	call_localret(ENTRY(mercury__term__init_var_supply_1_0),
		mercury__varset__merge_subst_4_0_i2,
		ENTRY(mercury__varset__merge_subst_4_0));
	}
Define_label(mercury__varset__merge_subst_4_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__merge_subst_4_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__varset__merge_subst_4_0_i3,
		ENTRY(mercury__varset__merge_subst_4_0));
	}
Define_label(mercury__varset__merge_subst_4_0_i3);
	update_prof_current_proc(LABEL(mercury__varset__merge_subst_4_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__varset__merge_subst_2__ua10000_8_0),
		ENTRY(mercury__varset__merge_subst_4_0));
END_MODULE

BEGIN_MODULE(mercury__varset_module19)
	init_entry(mercury__varset__get_bindings_2_0);
BEGIN_CODE

/* code for predicate 'varset__get_bindings'/2 in mode 0 */
Define_entry(mercury__varset__get_bindings_2_0);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module20)
	init_entry(mercury__varset__set_bindings_3_0);
BEGIN_CODE

/* code for predicate 'varset__set_bindings'/3 in mode 0 */
Define_entry(mercury__varset__set_bindings_3_0);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module21)
	init_entry(mercury__varset__create_name_var_map_2_0);
	init_label(mercury__varset__create_name_var_map_2_0_i2);
	init_label(mercury__varset__create_name_var_map_2_0_i3);
BEGIN_CODE

/* code for predicate 'varset__create_name_var_map'/2 in mode 0 */
Define_entry(mercury__varset__create_name_var_map_2_0);
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	incr_sp_push_msg(2, "varset__create_name_var_map");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__varset__create_name_var_map_2_0_i2,
		ENTRY(mercury__varset__create_name_var_map_2_0));
	}
Define_label(mercury__varset__create_name_var_map_2_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__create_name_var_map_2_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__map__values_2_0);
	call_localret(ENTRY(mercury__map__values_2_0),
		mercury__varset__create_name_var_map_2_0_i3,
		ENTRY(mercury__varset__create_name_var_map_2_0));
	}
Define_label(mercury__varset__create_name_var_map_2_0_i3);
	update_prof_current_proc(LABEL(mercury__varset__create_name_var_map_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r4 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__map__from_corresponding_lists_3_0);
	tailcall(ENTRY(mercury__map__from_corresponding_lists_3_0),
		ENTRY(mercury__varset__create_name_var_map_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__varset_module22)
	init_entry(mercury__varset__var_name_list_2_0);
BEGIN_CODE

/* code for predicate 'varset__var_name_list'/2 in mode 0 */
Define_entry(mercury__varset__var_name_list_2_0);
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__map__to_assoc_list_2_0);
	tailcall(ENTRY(mercury__map__to_assoc_list_2_0),
		ENTRY(mercury__varset__var_name_list_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__varset_module23)
	init_entry(mercury__varset__ensure_unique_names_4_0);
	init_label(mercury__varset__ensure_unique_names_4_0_i2);
	init_label(mercury__varset__ensure_unique_names_4_0_i3);
	init_label(mercury__varset__ensure_unique_names_4_0_i4);
BEGIN_CODE

/* code for predicate 'varset__ensure_unique_names'/4 in mode 0 */
Define_entry(mercury__varset__ensure_unique_names_4_0);
	incr_sp_push_msg(7, "varset__ensure_unique_names");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 2));
	r1 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__varset__ensure_unique_names_4_0_i2,
		ENTRY(mercury__varset__ensure_unique_names_4_0));
	}
Define_label(mercury__varset__ensure_unique_names_4_0_i2);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_4_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__varset__ensure_unique_names_4_0_i3,
		ENTRY(mercury__varset__ensure_unique_names_4_0));
	}
Define_label(mercury__varset__ensure_unique_names_4_0_i3);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_4_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__varset__ensure_unique_names_2_6_0),
		mercury__varset__ensure_unique_names_4_0_i4,
		ENTRY(mercury__varset__ensure_unique_names_4_0));
Define_label(mercury__varset__ensure_unique_names_4_0_i4);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module24)
	init_entry(mercury__varset__new_vars_2_5_0);
	init_label(mercury__varset__new_vars_2_5_0_i4);
	init_label(mercury__varset__new_vars_2_5_0_i1003);
	init_label(mercury__varset__new_vars_2_5_0_i1004);
BEGIN_CODE

/* code for predicate 'varset__new_vars_2'/5 in mode 0 */
Define_static(mercury__varset__new_vars_2_5_0);
	incr_sp_push_msg(3, "varset__new_vars_2");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r2 <= ((Integer) 0)))
		GOTO_LABEL(mercury__varset__new_vars_2_5_0_i1003);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = ((Integer) r2 - ((Integer) 1));
	{
		call_localret(STATIC(mercury__varset__new_var_3_0),
		mercury__varset__new_vars_2_5_0_i4,
		STATIC(mercury__varset__new_vars_2_5_0));
	}
Define_label(mercury__varset__new_vars_2_5_0_i4);
	update_prof_current_proc(LABEL(mercury__varset__new_vars_2_5_0));
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__varset__new_vars_2_5_0,
		STATIC(mercury__varset__new_vars_2_5_0));
Define_label(mercury__varset__new_vars_2_5_0_i1003);
	decr_sp_pop_msg(3);
	if (((Integer) r2 != ((Integer) 0)))
		GOTO_LABEL(mercury__varset__new_vars_2_5_0_i1004);
	r2 = (Integer) r1;
	r1 = (Integer) r3;
	proceed();
Define_label(mercury__varset__new_vars_2_5_0_i1004);
	r1 = string_const("varset__new_vars - invalid call", 31);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__varset__new_vars_2_5_0));
	}
END_MODULE

BEGIN_MODULE(mercury__varset_module25)
	init_entry(mercury__varset__vars_2_4_0);
	init_label(mercury__varset__vars_2_4_0_i4);
	init_label(mercury__varset__vars_2_4_0_i3);
	init_label(mercury__varset__vars_2_4_0_i6);
BEGIN_CODE

/* code for predicate 'varset__vars_2'/4 in mode 0 */
Define_static(mercury__varset__vars_2_4_0);
	incr_sp_push_msg(4, "varset__vars_2");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	{
	Declare_entry(mercury____Unify___mercury_builtin__var_supply_0_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__var_supply_0_0),
		mercury__varset__vars_2_4_0_i4,
		STATIC(mercury__varset__vars_2_4_0));
	}
Define_label(mercury__varset__vars_2_4_0_i4);
	update_prof_current_proc(LABEL(mercury__varset__vars_2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__varset__vars_2_4_0_i3);
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__varset__vars_2_4_0_i3);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__term__create_var_3_0);
	call_localret(ENTRY(mercury__term__create_var_3_0),
		mercury__varset__vars_2_4_0_i6,
		STATIC(mercury__varset__vars_2_4_0));
	}
Define_label(mercury__varset__vars_2_4_0_i6);
	update_prof_current_proc(LABEL(mercury__varset__vars_2_4_0));
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__varset__vars_2_4_0,
		STATIC(mercury__varset__vars_2_4_0));
END_MODULE

BEGIN_MODULE(mercury__varset_module26)
	init_entry(mercury__varset__bind_vars_2_3_0);
	init_label(mercury__varset__bind_vars_2_3_0_i4);
	init_label(mercury__varset__bind_vars_2_3_0_i1002);
BEGIN_CODE

/* code for predicate 'varset__bind_vars_2'/3 in mode 0 */
Define_static(mercury__varset__bind_vars_2_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__varset__bind_vars_2_3_0_i1002);
	incr_sp_push_msg(2, "varset__bind_vars_2");
	detstackvar(2) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r2;
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	{
		call_localret(STATIC(mercury__varset__bind_var_4_0),
		mercury__varset__bind_vars_2_3_0_i4,
		STATIC(mercury__varset__bind_vars_2_3_0));
	}
	}
Define_label(mercury__varset__bind_vars_2_3_0_i4);
	update_prof_current_proc(LABEL(mercury__varset__bind_vars_2_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__varset__bind_vars_2_3_0,
		STATIC(mercury__varset__bind_vars_2_3_0));
Define_label(mercury__varset__bind_vars_2_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module27)
	init_entry(mercury__varset__ensure_unique_names_2_6_0);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i6);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i10);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i12);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i13);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i14);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i15);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i9);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i5);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i17);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i18);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i19);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i20);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i21);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i22);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i23);
	init_label(mercury__varset__ensure_unique_names_2_6_0_i1004);
BEGIN_CODE

/* code for predicate 'varset__ensure_unique_names_2'/6 in mode 0 */
Define_static(mercury__varset__ensure_unique_names_2_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__varset__ensure_unique_names_2_6_0_i1004);
	incr_sp_push_msg(9, "varset__ensure_unique_names_2");
	detstackvar(9) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) r4;
	detstackvar(3) = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(5) = (Integer) r4;
	detstackvar(1) = (Integer) r2;
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	detstackvar(4) = (Integer) r5;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__varset__ensure_unique_names_2_6_0_i6,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
	}
Define_label(mercury__varset__ensure_unique_names_2_6_0_i6);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__varset__ensure_unique_names_2_6_0_i5);
	detstackvar(7) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__set__member_2_0);
	call_localret(ENTRY(mercury__set__member_2_0),
		mercury__varset__ensure_unique_names_2_6_0_i10,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
	}
Define_label(mercury__varset__ensure_unique_names_2_6_0_i10);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__varset__ensure_unique_names_2_6_0_i9);
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__term__var_to_int_2_0);
	call_localret(ENTRY(mercury__term__var_to_int_2_0),
		mercury__varset__ensure_unique_names_2_6_0_i12,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
	}
Define_label(mercury__varset__ensure_unique_names_2_6_0_i12);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	{
	Declare_entry(mercury__string__int_to_string_2_0);
	call_localret(ENTRY(mercury__string__int_to_string_2_0),
		mercury__varset__ensure_unique_names_2_6_0_i13,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
	}
Define_label(mercury__varset__ensure_unique_names_2_6_0_i13);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	r2 = (Integer) r1;
	r1 = string_const("_", 1);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__varset__ensure_unique_names_2_6_0_i14,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
	}
Define_label(mercury__varset__ensure_unique_names_2_6_0_i14);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__varset__ensure_unique_names_2_6_0_i15,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
	}
Define_label(mercury__varset__ensure_unique_names_2_6_0_i15);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__varset__ensure_unique_names_2_6_0_i20);
Define_label(mercury__varset__ensure_unique_names_2_6_0_i9);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r1 = (Integer) detstackvar(7);
	GOTO_LABEL(mercury__varset__ensure_unique_names_2_6_0_i20);
Define_label(mercury__varset__ensure_unique_names_2_6_0_i5);
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__term__var_to_int_2_0);
	call_localret(ENTRY(mercury__term__var_to_int_2_0),
		mercury__varset__ensure_unique_names_2_6_0_i17,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
	}
Define_label(mercury__varset__ensure_unique_names_2_6_0_i17);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	{
	Declare_entry(mercury__string__int_to_string_2_0);
	call_localret(ENTRY(mercury__string__int_to_string_2_0),
		mercury__varset__ensure_unique_names_2_6_0_i18,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
	}
Define_label(mercury__varset__ensure_unique_names_2_6_0_i18);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	r2 = (Integer) r1;
	r1 = string_const("Var_", 4);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__varset__ensure_unique_names_2_6_0_i19,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
	}
Define_label(mercury__varset__ensure_unique_names_2_6_0_i19);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
Define_label(mercury__varset__ensure_unique_names_2_6_0_i20);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	call_localret(STATIC(mercury__varset__ensure_unique_names_3_4_0),
		mercury__varset__ensure_unique_names_2_6_0_i21,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
Define_label(mercury__varset__ensure_unique_names_2_6_0_i21);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	r3 = (Integer) r1;
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__varset__ensure_unique_names_2_6_0_i22,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
	}
Define_label(mercury__varset__ensure_unique_names_2_6_0_i22);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	r5 = (Integer) detstackvar(8);
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__varset__ensure_unique_names_2_6_0_i23,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
	}
Define_label(mercury__varset__ensure_unique_names_2_6_0_i23);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_2_6_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	localtailcall(mercury__varset__ensure_unique_names_2_6_0,
		STATIC(mercury__varset__ensure_unique_names_2_6_0));
Define_label(mercury__varset__ensure_unique_names_2_6_0_i1004);
	r1 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module28)
	init_entry(mercury__varset__ensure_unique_names_3_4_0);
	init_label(mercury__varset__ensure_unique_names_3_4_0_i4);
	init_label(mercury__varset__ensure_unique_names_3_4_0_i6);
	init_label(mercury__varset__ensure_unique_names_3_4_0_i3);
BEGIN_CODE

/* code for predicate 'varset__ensure_unique_names_3'/4 in mode 0 */
Define_static(mercury__varset__ensure_unique_names_3_4_0);
	incr_sp_push_msg(4, "varset__ensure_unique_names_3");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) r1;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	detstackvar(3) = (Integer) r3;
	{
	Declare_entry(mercury__set__member_2_0);
	call_localret(ENTRY(mercury__set__member_2_0),
		mercury__varset__ensure_unique_names_3_4_0_i4,
		STATIC(mercury__varset__ensure_unique_names_3_4_0));
	}
Define_label(mercury__varset__ensure_unique_names_3_4_0_i4);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_3_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__varset__ensure_unique_names_3_4_0_i3);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__varset__ensure_unique_names_3_4_0_i6,
		STATIC(mercury__varset__ensure_unique_names_3_4_0));
	}
Define_label(mercury__varset__ensure_unique_names_3_4_0_i6);
	update_prof_current_proc(LABEL(mercury__varset__ensure_unique_names_3_4_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__varset__ensure_unique_names_3_4_0,
		STATIC(mercury__varset__ensure_unique_names_3_4_0));
Define_label(mercury__varset__ensure_unique_names_3_4_0_i3);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module29)
	init_entry(mercury____Unify___varset__varset_0_0);
	init_label(mercury____Unify___varset__varset_0_0_i2);
	init_label(mercury____Unify___varset__varset_0_0_i4);
	init_label(mercury____Unify___varset__varset_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___varset__varset_0_0);
	incr_sp_push_msg(5, "__Unify__");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury____Unify___mercury_builtin__var_supply_0_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__var_supply_0_0),
		mercury____Unify___varset__varset_0_0_i2,
		ENTRY(mercury____Unify___varset__varset_0_0));
	}
Define_label(mercury____Unify___varset__varset_0_0_i2);
	update_prof_current_proc(LABEL(mercury____Unify___varset__varset_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___varset__varset_0_0_i1);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Unify___tree234__tree234_2_0),
		mercury____Unify___varset__varset_0_0_i4,
		ENTRY(mercury____Unify___varset__varset_0_0));
	}
Define_label(mercury____Unify___varset__varset_0_0_i4);
	update_prof_current_proc(LABEL(mercury____Unify___varset__varset_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___varset__varset_0_0_i1);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___varset__varset_0_0));
	}
Define_label(mercury____Unify___varset__varset_0_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__varset_module30)
	init_entry(mercury____Index___varset__varset_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___varset__varset_0_0);
	tailcall(STATIC(mercury____Index___varset_varset_0__ua10000_2_0),
		ENTRY(mercury____Index___varset__varset_0_0));
END_MODULE

BEGIN_MODULE(mercury__varset_module31)
	init_entry(mercury____Compare___varset__varset_0_0);
	init_label(mercury____Compare___varset__varset_0_0_i4);
	init_label(mercury____Compare___varset__varset_0_0_i5);
	init_label(mercury____Compare___varset__varset_0_0_i3);
	init_label(mercury____Compare___varset__varset_0_0_i10);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___varset__varset_0_0);
	incr_sp_push_msg(5, "__Compare__");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury____Compare___mercury_builtin__var_supply_0_0);
	call_localret(ENTRY(mercury____Compare___mercury_builtin__var_supply_0_0),
		mercury____Compare___varset__varset_0_0_i4,
		ENTRY(mercury____Compare___varset__varset_0_0));
	}
Define_label(mercury____Compare___varset__varset_0_0_i4);
	update_prof_current_proc(LABEL(mercury____Compare___varset__varset_0_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___varset__varset_0_0_i3);
Define_label(mercury____Compare___varset__varset_0_0_i5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___varset__varset_0_0_i3);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Compare___tree234__tree234_2_0),
		mercury____Compare___varset__varset_0_0_i10,
		ENTRY(mercury____Compare___varset__varset_0_0));
	}
Define_label(mercury____Compare___varset__varset_0_0_i10);
	update_prof_current_proc(LABEL(mercury____Compare___varset__varset_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___varset__varset_0_0_i5);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___varset__varset_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__varset_bunch_0(void)
{
	mercury__varset_module0();
	mercury__varset_module1();
	mercury__varset_module2();
	mercury__varset_module3();
	mercury__varset_module4();
	mercury__varset_module5();
	mercury__varset_module6();
	mercury__varset_module7();
	mercury__varset_module8();
	mercury__varset_module9();
	mercury__varset_module10();
	mercury__varset_module11();
	mercury__varset_module12();
	mercury__varset_module13();
	mercury__varset_module14();
	mercury__varset_module15();
	mercury__varset_module16();
	mercury__varset_module17();
	mercury__varset_module18();
	mercury__varset_module19();
	mercury__varset_module20();
	mercury__varset_module21();
	mercury__varset_module22();
	mercury__varset_module23();
	mercury__varset_module24();
	mercury__varset_module25();
	mercury__varset_module26();
	mercury__varset_module27();
	mercury__varset_module28();
	mercury__varset_module29();
	mercury__varset_module30();
	mercury__varset_module31();
}

#endif

void mercury__varset__init(void); /* suppress gcc warning */
void mercury__varset__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__varset_bunch_0();
#endif
}
