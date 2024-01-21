/*
** Automatically generated from `array.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__array__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Index___array_array_1__ua10000_2_0);
Declare_static(mercury__array__bounds__ua10000_3_0);
Define_extern_entry(mercury__array__init_4_0);
Declare_label(mercury__array__init_4_0_i2);
Declare_label(mercury__array__init_4_0_i3);
Define_extern_entry(mercury__array__bounds_3_0);
Define_extern_entry(mercury__array__lookup_3_0);
Declare_label(mercury__array__lookup_3_0_i1004);
Define_extern_entry(mercury__array__semidet_lookup_3_0);
Declare_label(mercury__array__semidet_lookup_3_0_i2);
Declare_label(mercury__array__semidet_lookup_3_0_i1003);
Define_extern_entry(mercury__array__set_4_0);
Declare_label(mercury__array__set_4_0_i4);
Declare_label(mercury__array__set_4_0_i1004);
Define_extern_entry(mercury__array__resize_4_0);
Declare_label(mercury__array__resize_4_0_i2);
Declare_label(mercury__array__resize_4_0_i3);
Declare_label(mercury__array__resize_4_0_i4);
Declare_label(mercury__array__resize_4_0_i5);
Declare_label(mercury__array__resize_4_0_i6);
Declare_label(mercury__array__resize_4_0_i7);
Define_extern_entry(mercury__array__from_list_2_0);
Declare_label(mercury__array__from_list_2_0_i4);
Declare_label(mercury__array__from_list_2_0_i5);
Declare_label(mercury__array__from_list_2_0_i3);
Declare_label(mercury__array__from_list_2_0_i7);
Define_extern_entry(mercury__array__to_list_2_0);
Declare_label(mercury__array__to_list_2_0_i2);
Define_extern_entry(mercury__array__fetch_items_4_0);
Declare_label(mercury__array__fetch_items_4_0_i1000);
Declare_label(mercury__array__fetch_items_4_0_i4);
Declare_label(mercury__array__fetch_items_4_0_i5);
Define_extern_entry(mercury__array__bsearch_4_0);
Declare_label(mercury__array__bsearch_4_0_i2);
Declare_label(mercury__array__bsearch_4_0_i3);
Declare_label(mercury__array__bsearch_4_0_i1);
Declare_label(mercury__array__bsearch_4_0_i1000);
Declare_static(mercury__array__init_2_5_0);
Declare_label(mercury__array__init_2_5_0_i1000);
Declare_label(mercury__array__init_2_5_0_i4);
Declare_static(mercury__array__insert_items_4_0);
Declare_label(mercury__array__insert_items_4_0_i4);
Declare_label(mercury__array__insert_items_4_0_i1002);
Declare_static(mercury__array__bsearch_2_6_0);
Declare_label(mercury__array__bsearch_2_6_0_i5);
Declare_label(mercury__array__bsearch_2_6_0_i6);
Declare_label(mercury__array__bsearch_2_6_0_i2);
Declare_label(mercury__array__bsearch_2_6_0_i7);
Declare_label(mercury__array__bsearch_2_6_0_i8);
Declare_label(mercury__array__bsearch_2_6_0_i11);
Declare_label(mercury__array__bsearch_2_6_0_i10);
Declare_label(mercury__array__bsearch_2_6_0_i13);
Declare_label(mercury__array__bsearch_2_6_0_i16);
Declare_label(mercury__array__bsearch_2_6_0_i1);
Declare_label(mercury__array__bsearch_2_6_0_i1000);
Declare_label(mercury__array__bsearch_2_6_0_i1001);
Define_extern_entry(mercury____Unify___array__array_1_0);
Declare_label(mercury____Unify___array__array_1_0_i1003);
Define_extern_entry(mercury____Index___array__array_1_0);
Define_extern_entry(mercury____Compare___array__array_1_0);
Declare_label(mercury____Compare___array__array_1_0_i4);
Declare_label(mercury____Compare___array__array_1_0_i5);
Declare_label(mercury____Compare___array__array_1_0_i3);
Declare_label(mercury____Compare___array__array_1_0_i10);

extern Word * mercury_data_array__base_type_layout_array_1[];
Word * mercury_data_array__base_type_info_array_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___array__array_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___array__array_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___array__array_1_0),
	(Word *) (Integer) mercury_data_array__base_type_layout_array_1
};

extern Word * mercury_data_array__common_2[];
Word * mercury_data_array__base_type_layout_array_1[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_array__common_2),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_array__common_0[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
Word * mercury_data_array__common_1[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data___base_type_info_int_0,
	(Word *) ((Integer) 1)
};

Word * mercury_data_array__common_2[] = {
	(Word *) ((Integer) 3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_array__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_array__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_array__common_1),
	(Word *) string_const("array", 5)
};

BEGIN_MODULE(mercury__array_module0)
	init_entry(mercury____Index___array_array_1__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___array_array_1__ua10000'/2 in mode 0 */
Define_static(mercury____Index___array_array_1__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__array_module1)
	init_entry(mercury__array__bounds__ua10000_3_0);
BEGIN_CODE

/* code for predicate 'array__bounds__ua10000'/3 in mode 0 */
Define_static(mercury__array__bounds__ua10000_3_0);
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__array_module2)
	init_entry(mercury__array__init_4_0);
	init_label(mercury__array__init_4_0_i2);
	init_label(mercury__array__init_4_0_i3);
BEGIN_CODE

/* code for predicate 'array__init'/4 in mode 0 */
Define_entry(mercury__array__init_4_0);
	incr_sp_push_msg(5, "array__init");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r1;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__array__init_4_0_i2,
		ENTRY(mercury__array__init_4_0));
	}
Define_label(mercury__array__init_4_0_i2);
	update_prof_current_proc(LABEL(mercury__array__init_4_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__array__init_2_5_0),
		mercury__array__init_4_0_i3,
		ENTRY(mercury__array__init_4_0));
Define_label(mercury__array__init_4_0_i3);
	update_prof_current_proc(LABEL(mercury__array__init_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__array_module3)
	init_entry(mercury__array__bounds_3_0);
BEGIN_CODE

/* code for predicate 'array__bounds'/3 in mode 0 */
Define_entry(mercury__array__bounds_3_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__array__bounds__ua10000_3_0),
		ENTRY(mercury__array__bounds_3_0));
END_MODULE

BEGIN_MODULE(mercury__array_module4)
	init_entry(mercury__array__lookup_3_0);
	init_label(mercury__array__lookup_3_0_i1004);
BEGIN_CODE

/* code for predicate 'array__lookup'/3 in mode 0 */
Define_entry(mercury__array__lookup_3_0);
	if (((Integer) field(mktag(0), (Integer) r2, ((Integer) 0)) > (Integer) r3))
		GOTO_LABEL(mercury__array__lookup_3_0_i1004);
	if (((Integer) r3 > (Integer) field(mktag(0), (Integer) r2, ((Integer) 1))))
		GOTO_LABEL(mercury__array__lookup_3_0_i1004);
	r4 = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__lookup_3_1);
	tailcall(ENTRY(mercury__map__lookup_3_1),
		ENTRY(mercury__array__lookup_3_0));
	}
Define_label(mercury__array__lookup_3_0_i1004);
	r1 = string_const("array__lookup: Array subscript out of bounds", 44);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__array__lookup_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__array_module5)
	init_entry(mercury__array__semidet_lookup_3_0);
	init_label(mercury__array__semidet_lookup_3_0_i2);
	init_label(mercury__array__semidet_lookup_3_0_i1003);
BEGIN_CODE

/* code for predicate 'array__semidet_lookup'/3 in mode 0 */
Define_entry(mercury__array__semidet_lookup_3_0);
	if (((Integer) field(mktag(0), (Integer) r2, ((Integer) 0)) > (Integer) r3))
		GOTO_LABEL(mercury__array__semidet_lookup_3_0_i1003);
	if (((Integer) r3 > (Integer) field(mktag(0), (Integer) r2, ((Integer) 1))))
		GOTO_LABEL(mercury__array__semidet_lookup_3_0_i1003);
	r4 = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	incr_sp_push_msg(1, "array__semidet_lookup");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__array__semidet_lookup_3_0_i2,
		ENTRY(mercury__array__semidet_lookup_3_0));
	}
Define_label(mercury__array__semidet_lookup_3_0_i2);
	update_prof_current_proc(LABEL(mercury__array__semidet_lookup_3_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__array__semidet_lookup_3_0_i1003);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__array_module6)
	init_entry(mercury__array__set_4_0);
	init_label(mercury__array__set_4_0_i4);
	init_label(mercury__array__set_4_0_i1004);
BEGIN_CODE

/* code for predicate 'array__set'/4 in mode 0 */
Define_entry(mercury__array__set_4_0);
	{
	Word tempr1, tempr2;
	tempr1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	if (((Integer) tempr1 > (Integer) r3))
		GOTO_LABEL(mercury__array__set_4_0_i1004);
	tempr2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	if (((Integer) r3 > (Integer) tempr2))
		GOTO_LABEL(mercury__array__set_4_0_i1004);
	incr_sp_push_msg(3, "array__set");
	detstackvar(3) = (Integer) succip;
	r5 = (Integer) r4;
	r4 = (Integer) r3;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(1) = (Integer) tempr1;
	detstackvar(2) = (Integer) tempr2;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__array__set_4_0_i4,
		ENTRY(mercury__array__set_4_0));
	}
	}
Define_label(mercury__array__set_4_0_i4);
	update_prof_current_proc(LABEL(mercury__array__set_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__array__set_4_0_i1004);
	r1 = string_const("array__set: Array subscript out of bounds", 41);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__array__set_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__array_module7)
	init_entry(mercury__array__resize_4_0);
	init_label(mercury__array__resize_4_0_i2);
	init_label(mercury__array__resize_4_0_i3);
	init_label(mercury__array__resize_4_0_i4);
	init_label(mercury__array__resize_4_0_i5);
	init_label(mercury__array__resize_4_0_i6);
	init_label(mercury__array__resize_4_0_i7);
BEGIN_CODE

/* code for predicate 'array__resize'/4 in mode 0 */
Define_entry(mercury__array__resize_4_0);
	incr_sp_push_msg(7, "array__resize");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__array__bounds__ua10000_3_0),
		mercury__array__resize_4_0_i2,
		ENTRY(mercury__array__resize_4_0));
Define_label(mercury__array__resize_4_0_i2);
	update_prof_current_proc(LABEL(mercury__array__resize_4_0));
	r3 = (Integer) r1;
	detstackvar(4) = (Integer) r1;
	detstackvar(5) = (Integer) r2;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__array__lookup_3_0),
		mercury__array__resize_4_0_i3,
		ENTRY(mercury__array__resize_4_0));
	}
Define_label(mercury__array__resize_4_0_i3);
	update_prof_current_proc(LABEL(mercury__array__resize_4_0));
	r2 = (Integer) detstackvar(4);
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__int__max_3_0);
	call_localret(ENTRY(mercury__int__max_3_0),
		mercury__array__resize_4_0_i4,
		ENTRY(mercury__array__resize_4_0));
	}
Define_label(mercury__array__resize_4_0_i4);
	update_prof_current_proc(LABEL(mercury__array__resize_4_0));
	r2 = (Integer) detstackvar(5);
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__int__min_3_0);
	call_localret(ENTRY(mercury__int__min_3_0),
		mercury__array__resize_4_0_i5,
		ENTRY(mercury__array__resize_4_0));
	}
Define_label(mercury__array__resize_4_0_i5);
	update_prof_current_proc(LABEL(mercury__array__resize_4_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(5);
	{
		call_localret(STATIC(mercury__array__fetch_items_4_0),
		mercury__array__resize_4_0_i6,
		ENTRY(mercury__array__resize_4_0));
	}
Define_label(mercury__array__resize_4_0_i6);
	update_prof_current_proc(LABEL(mercury__array__resize_4_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	{
		call_localret(STATIC(mercury__array__init_4_0),
		mercury__array__resize_4_0_i7,
		ENTRY(mercury__array__resize_4_0));
	}
Define_label(mercury__array__resize_4_0_i7);
	update_prof_current_proc(LABEL(mercury__array__resize_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	tailcall(STATIC(mercury__array__insert_items_4_0),
		ENTRY(mercury__array__resize_4_0));
END_MODULE

BEGIN_MODULE(mercury__array_module8)
	init_entry(mercury__array__from_list_2_0);
	init_label(mercury__array__from_list_2_0_i4);
	init_label(mercury__array__from_list_2_0_i5);
	init_label(mercury__array__from_list_2_0_i3);
	init_label(mercury__array__from_list_2_0_i7);
BEGIN_CODE

/* code for predicate 'array__from_list'/2 in mode 0 */
Define_entry(mercury__array__from_list_2_0);
	incr_sp_push_msg(4, "array__from_list");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__array__from_list_2_0_i3);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__array__from_list_2_0_i4,
		ENTRY(mercury__array__from_list_2_0));
	}
Define_label(mercury__array__from_list_2_0_i4);
	update_prof_current_proc(LABEL(mercury__array__from_list_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = ((Integer) 1);
	r4 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__array__init_4_0),
		mercury__array__from_list_2_0_i5,
		ENTRY(mercury__array__from_list_2_0));
	}
Define_label(mercury__array__from_list_2_0_i5);
	update_prof_current_proc(LABEL(mercury__array__from_list_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = ((Integer) 2);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__array__insert_items_4_0),
		ENTRY(mercury__array__from_list_2_0));
Define_label(mercury__array__from_list_2_0_i3);
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__array__from_list_2_0_i7,
		ENTRY(mercury__array__from_list_2_0));
	}
Define_label(mercury__array__from_list_2_0_i7);
	update_prof_current_proc(LABEL(mercury__array__from_list_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = ((Integer) 0);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__array_module9)
	init_entry(mercury__array__to_list_2_0);
	init_label(mercury__array__to_list_2_0_i2);
BEGIN_CODE

/* code for predicate 'array__to_list'/2 in mode 0 */
Define_entry(mercury__array__to_list_2_0);
	incr_sp_push_msg(3, "array__to_list");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__array__bounds__ua10000_3_0),
		mercury__array__to_list_2_0_i2,
		ENTRY(mercury__array__to_list_2_0));
Define_label(mercury__array__to_list_2_0_i2);
	update_prof_current_proc(LABEL(mercury__array__to_list_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__array__fetch_items_4_0),
		ENTRY(mercury__array__to_list_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__array_module10)
	init_entry(mercury__array__fetch_items_4_0);
	init_label(mercury__array__fetch_items_4_0_i1000);
	init_label(mercury__array__fetch_items_4_0_i4);
	init_label(mercury__array__fetch_items_4_0_i5);
BEGIN_CODE

/* code for predicate 'array__fetch_items'/4 in mode 0 */
Define_entry(mercury__array__fetch_items_4_0);
	if (((Integer) r3 <= (Integer) r4))
		GOTO_LABEL(mercury__array__fetch_items_4_0_i1000);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__array__fetch_items_4_0_i1000);
	incr_sp_push_msg(4, "array__fetch_items");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	r3 = ((Integer) r3 + ((Integer) 1));
	localcall(mercury__array__fetch_items_4_0,
		LABEL(mercury__array__fetch_items_4_0_i4),
		ENTRY(mercury__array__fetch_items_4_0));
Define_label(mercury__array__fetch_items_4_0_i4);
	update_prof_current_proc(LABEL(mercury__array__fetch_items_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__array__lookup_3_0),
		mercury__array__fetch_items_4_0_i5,
		ENTRY(mercury__array__fetch_items_4_0));
	}
Define_label(mercury__array__fetch_items_4_0_i5);
	update_prof_current_proc(LABEL(mercury__array__fetch_items_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__array_module11)
	init_entry(mercury__array__bsearch_4_0);
	init_label(mercury__array__bsearch_4_0_i2);
	init_label(mercury__array__bsearch_4_0_i3);
	init_label(mercury__array__bsearch_4_0_i1);
	init_label(mercury__array__bsearch_4_0_i1000);
BEGIN_CODE

/* code for predicate 'array__bsearch'/4 in mode 0 */
Define_entry(mercury__array__bsearch_4_0);
	incr_sp_push_msg(5, "array__bsearch");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__array__bounds__ua10000_3_0),
		mercury__array__bsearch_4_0_i2,
		ENTRY(mercury__array__bsearch_4_0));
Define_label(mercury__array__bsearch_4_0_i2);
	update_prof_current_proc(LABEL(mercury__array__bsearch_4_0));
	if (((Integer) r1 > (Integer) r2))
		GOTO_LABEL(mercury__array__bsearch_4_0_i1);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__array__bsearch_2_6_0),
		mercury__array__bsearch_4_0_i3,
		ENTRY(mercury__array__bsearch_4_0));
Define_label(mercury__array__bsearch_4_0_i3);
	update_prof_current_proc(LABEL(mercury__array__bsearch_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__array__bsearch_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__array__bsearch_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__array__bsearch_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__array_module12)
	init_entry(mercury__array__init_2_5_0);
	init_label(mercury__array__init_2_5_0_i1000);
	init_label(mercury__array__init_2_5_0_i4);
BEGIN_CODE

/* code for predicate 'array__init_2'/5 in mode 0 */
Define_static(mercury__array__init_2_5_0);
	if (((Integer) r2 <= (Integer) r3))
		GOTO_LABEL(mercury__array__init_2_5_0_i1000);
	r1 = (Integer) r5;
	proceed();
Define_label(mercury__array__init_2_5_0_i1000);
	incr_sp_push_msg(5, "array__init_2");
	detstackvar(5) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) r5;
	r5 = (Integer) r4;
	detstackvar(3) = (Integer) r4;
	r4 = (Integer) r2;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r1;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__array__init_2_5_0_i4,
		STATIC(mercury__array__init_2_5_0));
	}
Define_label(mercury__array__init_2_5_0_i4);
	update_prof_current_proc(LABEL(mercury__array__init_2_5_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = ((Integer) detstackvar(1) + ((Integer) 1));
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__array__init_2_5_0,
		STATIC(mercury__array__init_2_5_0));
END_MODULE

BEGIN_MODULE(mercury__array_module13)
	init_entry(mercury__array__insert_items_4_0);
	init_label(mercury__array__insert_items_4_0_i4);
	init_label(mercury__array__insert_items_4_0_i1002);
BEGIN_CODE

/* code for predicate 'array__insert_items'/4 in mode 0 */
Define_static(mercury__array__insert_items_4_0);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__array__insert_items_4_0_i1002);
	incr_sp_push_msg(4, "array__insert_items");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	r4 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	{
		call_localret(STATIC(mercury__array__set_4_0),
		mercury__array__insert_items_4_0_i4,
		STATIC(mercury__array__insert_items_4_0));
	}
Define_label(mercury__array__insert_items_4_0_i4);
	update_prof_current_proc(LABEL(mercury__array__insert_items_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = ((Integer) detstackvar(1) + ((Integer) 1));
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__array__insert_items_4_0,
		STATIC(mercury__array__insert_items_4_0));
Define_label(mercury__array__insert_items_4_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__array_module14)
	init_entry(mercury__array__bsearch_2_6_0);
	init_label(mercury__array__bsearch_2_6_0_i5);
	init_label(mercury__array__bsearch_2_6_0_i6);
	init_label(mercury__array__bsearch_2_6_0_i2);
	init_label(mercury__array__bsearch_2_6_0_i7);
	init_label(mercury__array__bsearch_2_6_0_i8);
	init_label(mercury__array__bsearch_2_6_0_i11);
	init_label(mercury__array__bsearch_2_6_0_i10);
	init_label(mercury__array__bsearch_2_6_0_i13);
	init_label(mercury__array__bsearch_2_6_0_i16);
	init_label(mercury__array__bsearch_2_6_0_i1);
	init_label(mercury__array__bsearch_2_6_0_i1000);
	init_label(mercury__array__bsearch_2_6_0_i1001);
BEGIN_CODE

/* code for predicate 'array__bsearch_2'/6 in mode 0 */
Define_static(mercury__array__bsearch_2_6_0);
	{
	Word tempr1;
	tempr1 = ((Integer) r4 - (Integer) r3);
	if (((Integer) tempr1 < ((Integer) 0)))
		GOTO_LABEL(mercury__array__bsearch_2_6_0_i1001);
	incr_sp_push_msg(8, "array__bsearch_2");
	detstackvar(8) = (Integer) succip;
	if (((Integer) tempr1 != ((Integer) 0)))
		GOTO_LABEL(mercury__array__bsearch_2_6_0_i2);
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	{
		call_localret(STATIC(mercury__array__lookup_3_0),
		mercury__array__bsearch_2_6_0_i5,
		STATIC(mercury__array__bsearch_2_6_0));
	}
	}
Define_label(mercury__array__bsearch_2_6_0_i5);
	update_prof_current_proc(LABEL(mercury__array__bsearch_2_6_0));
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = ((Integer) 2);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_det_closure);
	call_localret(ENTRY(do_call_det_closure),
		mercury__array__bsearch_2_6_0_i6,
		STATIC(mercury__array__bsearch_2_6_0));
	}
Define_label(mercury__array__bsearch_2_6_0_i6);
	update_prof_current_proc(LABEL(mercury__array__bsearch_2_6_0));
	if ((((Integer) 0) != (Integer) r1))
		GOTO_LABEL(mercury__array__bsearch_2_6_0_i1);
	r2 = (Integer) detstackvar(2);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__array__bsearch_2_6_0_i2);
	detstackvar(2) = (Integer) r3;
	r3 = (((Integer) r3 + (Integer) r4) >> ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	detstackvar(6) = (Integer) r3;
	detstackvar(7) = (Integer) r1;
	{
		call_localret(STATIC(mercury__array__lookup_3_0),
		mercury__array__bsearch_2_6_0_i7,
		STATIC(mercury__array__bsearch_2_6_0));
	}
Define_label(mercury__array__bsearch_2_6_0_i7);
	update_prof_current_proc(LABEL(mercury__array__bsearch_2_6_0));
	r4 = (Integer) r1;
	r5 = (Integer) detstackvar(4);
	r1 = (Integer) detstackvar(5);
	r2 = ((Integer) 2);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_det_closure);
	call_localret(ENTRY(do_call_det_closure),
		mercury__array__bsearch_2_6_0_i8,
		STATIC(mercury__array__bsearch_2_6_0));
	}
Define_label(mercury__array__bsearch_2_6_0_i8);
	update_prof_current_proc(LABEL(mercury__array__bsearch_2_6_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__array__bsearch_2_6_0_i10);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(6);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	localcall(mercury__array__bsearch_2_6_0,
		LABEL(mercury__array__bsearch_2_6_0_i11),
		STATIC(mercury__array__bsearch_2_6_0));
Define_label(mercury__array__bsearch_2_6_0_i11);
	update_prof_current_proc(LABEL(mercury__array__bsearch_2_6_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	if ((Integer) r1)
		GOTO_LABEL(mercury__array__bsearch_2_6_0_i1000);
	r1 = FALSE;
	proceed();
Define_label(mercury__array__bsearch_2_6_0_i10);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__array__bsearch_2_6_0_i13);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = ((Integer) detstackvar(6) + ((Integer) 1));
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	localcall(mercury__array__bsearch_2_6_0,
		LABEL(mercury__array__bsearch_2_6_0_i11),
		STATIC(mercury__array__bsearch_2_6_0));
Define_label(mercury__array__bsearch_2_6_0_i13);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = ((Integer) detstackvar(6) - ((Integer) 1));
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	localcall(mercury__array__bsearch_2_6_0,
		LABEL(mercury__array__bsearch_2_6_0_i16),
		STATIC(mercury__array__bsearch_2_6_0));
Define_label(mercury__array__bsearch_2_6_0_i16);
	update_prof_current_proc(LABEL(mercury__array__bsearch_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__array__bsearch_2_6_0_i1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__array__bsearch_2_6_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__array__bsearch_2_6_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__array__bsearch_2_6_0_i1001);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__array_module15)
	init_entry(mercury____Unify___array__array_1_0);
	init_label(mercury____Unify___array__array_1_0_i1003);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___array__array_1_0);
	if (((Integer) field(mktag(0), (Integer) r2, ((Integer) 0)) != (Integer) field(mktag(0), (Integer) r3, ((Integer) 0))))
		GOTO_LABEL(mercury____Unify___array__array_1_0_i1003);
	if (((Integer) field(mktag(0), (Integer) r2, ((Integer) 1)) != (Integer) field(mktag(0), (Integer) r3, ((Integer) 1))))
		GOTO_LABEL(mercury____Unify___array__array_1_0_i1003);
	r4 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 2));
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___array__array_1_0));
	}
Define_label(mercury____Unify___array__array_1_0_i1003);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__array_module16)
	init_entry(mercury____Index___array__array_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___array__array_1_0);
	tailcall(STATIC(mercury____Index___array_array_1__ua10000_2_0),
		ENTRY(mercury____Index___array__array_1_0));
END_MODULE

BEGIN_MODULE(mercury__array_module17)
	init_entry(mercury____Compare___array__array_1_0);
	init_label(mercury____Compare___array__array_1_0_i4);
	init_label(mercury____Compare___array__array_1_0_i5);
	init_label(mercury____Compare___array__array_1_0_i3);
	init_label(mercury____Compare___array__array_1_0_i10);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___array__array_1_0);
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
		mercury____Compare___array__array_1_0_i4,
		ENTRY(mercury____Compare___array__array_1_0));
	}
Define_label(mercury____Compare___array__array_1_0_i4);
	update_prof_current_proc(LABEL(mercury____Compare___array__array_1_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___array__array_1_0_i3);
Define_label(mercury____Compare___array__array_1_0_i5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury____Compare___array__array_1_0_i3);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	call_localret(ENTRY(mercury__builtin_compare_int_3_0),
		mercury____Compare___array__array_1_0_i10,
		ENTRY(mercury____Compare___array__array_1_0));
	}
Define_label(mercury____Compare___array__array_1_0_i10);
	update_prof_current_proc(LABEL(mercury____Compare___array__array_1_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___array__array_1_0_i5);
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___array__array_1_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__array_bunch_0(void)
{
	mercury__array_module0();
	mercury__array_module1();
	mercury__array_module2();
	mercury__array_module3();
	mercury__array_module4();
	mercury__array_module5();
	mercury__array_module6();
	mercury__array_module7();
	mercury__array_module8();
	mercury__array_module9();
	mercury__array_module10();
	mercury__array_module11();
	mercury__array_module12();
	mercury__array_module13();
	mercury__array_module14();
	mercury__array_module15();
	mercury__array_module16();
	mercury__array_module17();
}

#endif

void mercury__array__init(void); /* suppress gcc warning */
void mercury__array__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__array_bunch_0();
#endif
}
