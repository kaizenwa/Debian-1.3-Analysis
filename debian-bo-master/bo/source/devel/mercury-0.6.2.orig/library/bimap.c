/*
** Automatically generated from `bimap.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__bimap__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Index___bimap_bimap_2__ua10000_2_0);
Define_extern_entry(mercury__bimap__init_1_0);
Declare_label(mercury__bimap__init_1_0_i2);
Declare_label(mercury__bimap__init_1_0_i3);
Define_extern_entry(mercury__bimap__is_empty_1_0);
Define_extern_entry(mercury__bimap__search_3_0);
Declare_label(mercury__bimap__search_3_0_i2);
Declare_label(mercury__bimap__search_3_0_i4);
Declare_label(mercury__bimap__search_3_0_i1);
Define_extern_entry(mercury__bimap__search_3_1);
Declare_label(mercury__bimap__search_3_1_i2);
Declare_label(mercury__bimap__search_3_1_i4);
Declare_label(mercury__bimap__search_3_1_i1);
Define_extern_entry(mercury__bimap__lookup_3_0);
Define_extern_entry(mercury__bimap__reverse_lookup_3_0);
Define_extern_entry(mercury__bimap__insert_4_0);
Declare_label(mercury__bimap__insert_4_0_i2);
Declare_label(mercury__bimap__insert_4_0_i4);
Declare_label(mercury__bimap__insert_4_0_i1);
Define_extern_entry(mercury__bimap__set_4_0);
Declare_label(mercury__bimap__set_4_0_i2);
Declare_label(mercury__bimap__set_4_0_i3);
Define_extern_entry(mercury__bimap__ordinates_2_0);
Define_extern_entry(mercury__bimap__coordinates_2_0);
Define_extern_entry(mercury__bimap__to_assoc_list_2_0);
Define_extern_entry(mercury__bimap__from_assoc_list_2_0);
Declare_label(mercury__bimap__from_assoc_list_2_0_i2);
Declare_label(mercury__bimap__from_assoc_list_2_0_i3);
Declare_label(mercury__bimap__from_assoc_list_2_0_i4);
Define_extern_entry(mercury____Unify___bimap__bimap_2_0);
Declare_label(mercury____Unify___bimap__bimap_2_0_i2);
Declare_label(mercury____Unify___bimap__bimap_2_0_i1);
Define_extern_entry(mercury____Index___bimap__bimap_2_0);
Define_extern_entry(mercury____Compare___bimap__bimap_2_0);
Declare_label(mercury____Compare___bimap__bimap_2_0_i4);
Declare_label(mercury____Compare___bimap__bimap_2_0_i3);

extern Word * mercury_data_bimap__base_type_layout_bimap_2[];
Word * mercury_data_bimap__base_type_info_bimap_2[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) ENTRY(mercury____Unify___bimap__bimap_2_0),
	(Word *) (Integer) ENTRY(mercury____Index___bimap__bimap_2_0),
	(Word *) (Integer) ENTRY(mercury____Compare___bimap__bimap_2_0),
	(Word *) (Integer) mercury_data_bimap__base_type_layout_bimap_2
};

extern Word * mercury_data_bimap__common_2[];
Word * mercury_data_bimap__base_type_layout_bimap_2[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_bimap__common_2),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
Word * mercury_data_bimap__common_0[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 2)
};

Word * mercury_data_bimap__common_1[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) ((Integer) 2),
	(Word *) ((Integer) 1)
};

Word * mercury_data_bimap__common_2[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_bimap__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_bimap__common_1),
	(Word *) string_const("bimap", 5)
};

BEGIN_MODULE(mercury__bimap_module0)
	init_entry(mercury____Index___bimap_bimap_2__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___bimap_bimap_2__ua10000'/2 in mode 0 */
Define_static(mercury____Index___bimap_bimap_2__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bimap_module1)
	init_entry(mercury__bimap__init_1_0);
	init_label(mercury__bimap__init_1_0_i2);
	init_label(mercury__bimap__init_1_0_i3);
BEGIN_CODE

/* code for predicate 'bimap__init'/1 in mode 0 */
Define_entry(mercury__bimap__init_1_0);
	incr_sp_push_msg(3, "bimap__init");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__bimap__init_1_0_i2,
		ENTRY(mercury__bimap__init_1_0));
	}
Define_label(mercury__bimap__init_1_0_i2);
	update_prof_current_proc(LABEL(mercury__bimap__init_1_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__bimap__init_1_0_i3,
		ENTRY(mercury__bimap__init_1_0));
	}
Define_label(mercury__bimap__init_1_0_i3);
	update_prof_current_proc(LABEL(mercury__bimap__init_1_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bimap_module2)
	init_entry(mercury__bimap__is_empty_1_0);
BEGIN_CODE

/* code for predicate 'bimap__is_empty'/1 in mode 0 */
Define_entry(mercury__bimap__is_empty_1_0);
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__map__is_empty_1_0);
	tailcall(ENTRY(mercury__map__is_empty_1_0),
		ENTRY(mercury__bimap__is_empty_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bimap_module3)
	init_entry(mercury__bimap__search_3_0);
	init_label(mercury__bimap__search_3_0_i2);
	init_label(mercury__bimap__search_3_0_i4);
	init_label(mercury__bimap__search_3_0_i1);
BEGIN_CODE

/* code for predicate 'bimap__search'/3 in mode 0 */
Define_entry(mercury__bimap__search_3_0);
	incr_sp_push_msg(5, "bimap__search");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__bimap__search_3_0_i2,
		ENTRY(mercury__bimap__search_3_0));
	}
Define_label(mercury__bimap__search_3_0_i2);
	update_prof_current_proc(LABEL(mercury__bimap__search_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bimap__search_3_0_i1);
	r5 = (Integer) detstackvar(1);
	r4 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__search_3_0);
	call_localret(ENTRY(mercury__map__search_3_0),
		mercury__bimap__search_3_0_i4,
		ENTRY(mercury__bimap__search_3_0));
	}
Define_label(mercury__bimap__search_3_0_i4);
	update_prof_current_proc(LABEL(mercury__bimap__search_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bimap__search_3_0_i1);
	r2 = (Integer) detstackvar(1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__bimap__search_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bimap_module4)
	init_entry(mercury__bimap__search_3_1);
	init_label(mercury__bimap__search_3_1_i2);
	init_label(mercury__bimap__search_3_1_i4);
	init_label(mercury__bimap__search_3_1_i1);
BEGIN_CODE

/* code for predicate 'bimap__search'/3 in mode 1 */
Define_entry(mercury__bimap__search_3_1);
	incr_sp_push_msg(5, "bimap__search");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__bimap__search_3_1_i2,
		ENTRY(mercury__bimap__search_3_1));
	}
Define_label(mercury__bimap__search_3_1_i2);
	update_prof_current_proc(LABEL(mercury__bimap__search_3_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bimap__search_3_1_i1);
	r5 = (Integer) detstackvar(1);
	r4 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__search_3_0);
	call_localret(ENTRY(mercury__map__search_3_0),
		mercury__bimap__search_3_1_i4,
		ENTRY(mercury__bimap__search_3_1));
	}
Define_label(mercury__bimap__search_3_1_i4);
	update_prof_current_proc(LABEL(mercury__bimap__search_3_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bimap__search_3_1_i1);
	r2 = (Integer) detstackvar(1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__bimap__search_3_1_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bimap_module5)
	init_entry(mercury__bimap__lookup_3_0);
BEGIN_CODE

/* code for predicate 'bimap__lookup'/3 in mode 0 */
Define_entry(mercury__bimap__lookup_3_0);
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__map__lookup_3_1);
	tailcall(ENTRY(mercury__map__lookup_3_1),
		ENTRY(mercury__bimap__lookup_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bimap_module6)
	init_entry(mercury__bimap__reverse_lookup_3_0);
BEGIN_CODE

/* code for predicate 'bimap__reverse_lookup'/3 in mode 0 */
Define_entry(mercury__bimap__reverse_lookup_3_0);
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) tempr1;
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__map__lookup_3_1);
	tailcall(ENTRY(mercury__map__lookup_3_1),
		ENTRY(mercury__bimap__reverse_lookup_3_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__bimap_module7)
	init_entry(mercury__bimap__insert_4_0);
	init_label(mercury__bimap__insert_4_0_i2);
	init_label(mercury__bimap__insert_4_0_i4);
	init_label(mercury__bimap__insert_4_0_i1);
BEGIN_CODE

/* code for predicate 'bimap__insert'/4 in mode 0 */
Define_entry(mercury__bimap__insert_4_0);
	incr_sp_push_msg(6, "bimap__insert");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(4) = (Integer) r1;
	detstackvar(5) = (Integer) r2;
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__map__insert_4_0);
	call_localret(ENTRY(mercury__map__insert_4_0),
		mercury__bimap__insert_4_0_i2,
		ENTRY(mercury__bimap__insert_4_0));
	}
Define_label(mercury__bimap__insert_4_0_i2);
	update_prof_current_proc(LABEL(mercury__bimap__insert_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bimap__insert_4_0_i1);
	r5 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__insert_4_0);
	call_localret(ENTRY(mercury__map__insert_4_0),
		mercury__bimap__insert_4_0_i4,
		ENTRY(mercury__bimap__insert_4_0));
	}
Define_label(mercury__bimap__insert_4_0_i4);
	update_prof_current_proc(LABEL(mercury__bimap__insert_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__bimap__insert_4_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__bimap__insert_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bimap_module8)
	init_entry(mercury__bimap__set_4_0);
	init_label(mercury__bimap__set_4_0_i2);
	init_label(mercury__bimap__set_4_0_i3);
BEGIN_CODE

/* code for predicate 'bimap__set'/4 in mode 0 */
Define_entry(mercury__bimap__set_4_0);
	incr_sp_push_msg(6, "bimap__set");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(4) = (Integer) r1;
	detstackvar(5) = (Integer) r2;
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__bimap__set_4_0_i2,
		ENTRY(mercury__bimap__set_4_0));
	}
Define_label(mercury__bimap__set_4_0_i2);
	update_prof_current_proc(LABEL(mercury__bimap__set_4_0));
	r5 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__bimap__set_4_0_i3,
		ENTRY(mercury__bimap__set_4_0));
	}
Define_label(mercury__bimap__set_4_0_i3);
	update_prof_current_proc(LABEL(mercury__bimap__set_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bimap_module9)
	init_entry(mercury__bimap__ordinates_2_0);
BEGIN_CODE

/* code for predicate 'bimap__ordinates'/2 in mode 0 */
Define_entry(mercury__bimap__ordinates_2_0);
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__map__keys_2_0);
	tailcall(ENTRY(mercury__map__keys_2_0),
		ENTRY(mercury__bimap__ordinates_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bimap_module10)
	init_entry(mercury__bimap__coordinates_2_0);
BEGIN_CODE

/* code for predicate 'bimap__coordinates'/2 in mode 0 */
Define_entry(mercury__bimap__coordinates_2_0);
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) tempr1;
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__map__keys_2_0);
	tailcall(ENTRY(mercury__map__keys_2_0),
		ENTRY(mercury__bimap__coordinates_2_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__bimap_module11)
	init_entry(mercury__bimap__to_assoc_list_2_0);
BEGIN_CODE

/* code for predicate 'bimap__to_assoc_list'/2 in mode 0 */
Define_entry(mercury__bimap__to_assoc_list_2_0);
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__map__to_assoc_list_2_0);
	tailcall(ENTRY(mercury__map__to_assoc_list_2_0),
		ENTRY(mercury__bimap__to_assoc_list_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__bimap_module12)
	init_entry(mercury__bimap__from_assoc_list_2_0);
	init_label(mercury__bimap__from_assoc_list_2_0_i2);
	init_label(mercury__bimap__from_assoc_list_2_0_i3);
	init_label(mercury__bimap__from_assoc_list_2_0_i4);
BEGIN_CODE

/* code for predicate 'bimap__from_assoc_list'/2 in mode 0 */
Define_entry(mercury__bimap__from_assoc_list_2_0);
	incr_sp_push_msg(4, "bimap__from_assoc_list");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__map__from_assoc_list_2_0);
	call_localret(ENTRY(mercury__map__from_assoc_list_2_0),
		mercury__bimap__from_assoc_list_2_0_i2,
		ENTRY(mercury__bimap__from_assoc_list_2_0));
	}
Define_label(mercury__bimap__from_assoc_list_2_0_i2);
	update_prof_current_proc(LABEL(mercury__bimap__from_assoc_list_2_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__assoc_list__reverse_members_2_0);
	call_localret(ENTRY(mercury__assoc_list__reverse_members_2_0),
		mercury__bimap__from_assoc_list_2_0_i3,
		ENTRY(mercury__bimap__from_assoc_list_2_0));
	}
Define_label(mercury__bimap__from_assoc_list_2_0_i3);
	update_prof_current_proc(LABEL(mercury__bimap__from_assoc_list_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__from_assoc_list_2_0);
	call_localret(ENTRY(mercury__map__from_assoc_list_2_0),
		mercury__bimap__from_assoc_list_2_0_i4,
		ENTRY(mercury__bimap__from_assoc_list_2_0));
	}
Define_label(mercury__bimap__from_assoc_list_2_0_i4);
	update_prof_current_proc(LABEL(mercury__bimap__from_assoc_list_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bimap_module13)
	init_entry(mercury____Unify___bimap__bimap_2_0);
	init_label(mercury____Unify___bimap__bimap_2_0_i2);
	init_label(mercury____Unify___bimap__bimap_2_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___bimap__bimap_2_0);
	incr_sp_push_msg(5, "__Unify__");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r4 = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Unify___tree234__tree234_2_0),
		mercury____Unify___bimap__bimap_2_0_i2,
		ENTRY(mercury____Unify___bimap__bimap_2_0));
	}
Define_label(mercury____Unify___bimap__bimap_2_0_i2);
	update_prof_current_proc(LABEL(mercury____Unify___bimap__bimap_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___bimap__bimap_2_0_i1);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___bimap__bimap_2_0));
	}
Define_label(mercury____Unify___bimap__bimap_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__bimap_module14)
	init_entry(mercury____Index___bimap__bimap_2_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___bimap__bimap_2_0);
	tailcall(STATIC(mercury____Index___bimap_bimap_2__ua10000_2_0),
		ENTRY(mercury____Index___bimap__bimap_2_0));
END_MODULE

BEGIN_MODULE(mercury__bimap_module15)
	init_entry(mercury____Compare___bimap__bimap_2_0);
	init_label(mercury____Compare___bimap__bimap_2_0_i4);
	init_label(mercury____Compare___bimap__bimap_2_0_i3);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___bimap__bimap_2_0);
	incr_sp_push_msg(5, "__Compare__");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 1));
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r4 = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Compare___tree234__tree234_2_0),
		mercury____Compare___bimap__bimap_2_0_i4,
		ENTRY(mercury____Compare___bimap__bimap_2_0));
	}
Define_label(mercury____Compare___bimap__bimap_2_0_i4);
	update_prof_current_proc(LABEL(mercury____Compare___bimap__bimap_2_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___bimap__bimap_2_0_i3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___bimap__bimap_2_0_i3);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___bimap__bimap_2_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__bimap_bunch_0(void)
{
	mercury__bimap_module0();
	mercury__bimap_module1();
	mercury__bimap_module2();
	mercury__bimap_module3();
	mercury__bimap_module4();
	mercury__bimap_module5();
	mercury__bimap_module6();
	mercury__bimap_module7();
	mercury__bimap_module8();
	mercury__bimap_module9();
	mercury__bimap_module10();
	mercury__bimap_module11();
	mercury__bimap_module12();
	mercury__bimap_module13();
	mercury__bimap_module14();
	mercury__bimap_module15();
}

#endif

void mercury__bimap__init(void); /* suppress gcc warning */
void mercury__bimap__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__bimap_bunch_0();
#endif
}
