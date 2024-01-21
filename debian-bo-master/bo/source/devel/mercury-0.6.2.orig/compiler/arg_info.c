/*
** Automatically generated from `arg_info.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__arg_info__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__arg_info__generate_arg_info_3_0);
Declare_label(mercury__arg_info__generate_arg_info_3_0_i2);
Declare_label(mercury__arg_info__generate_arg_info_3_0_i3);
Define_extern_entry(mercury__arg_info__unify_arg_info_3_0);
Declare_label(mercury__arg_info__unify_arg_info_3_0_i1007);
Declare_label(mercury__arg_info__unify_arg_info_3_0_i4);
Declare_label(mercury__arg_info__unify_arg_info_3_0_i1005);
Define_extern_entry(mercury__arg_info__make_arg_infos_6_0);
Declare_label(mercury__arg_info__make_arg_infos_6_0_i1006);
Declare_label(mercury__arg_info__make_arg_infos_6_0_i1004);
Declare_label(mercury__arg_info__make_arg_infos_6_0_i5);
Declare_label(mercury__arg_info__make_arg_infos_6_0_i1005);
Declare_static(mercury__arg_info__generate_pred_arg_info_4_0);
Declare_label(mercury__arg_info__generate_pred_arg_info_4_0_i4);
Declare_label(mercury__arg_info__generate_pred_arg_info_4_0_i5);
Declare_label(mercury__arg_info__generate_pred_arg_info_4_0_i6);
Declare_label(mercury__arg_info__generate_pred_arg_info_4_0_i7);
Declare_label(mercury__arg_info__generate_pred_arg_info_4_0_i1002);
Declare_static(mercury__arg_info__generate_proc_list_arg_info_5_0);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i4);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i5);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i6);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i7);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i8);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i9);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i10);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i11);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i12);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i13);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i14);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i15);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i16);
Declare_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i1002);
Declare_static(mercury__arg_info__make_arg_infos_list_5_0);
Declare_label(mercury__arg_info__make_arg_infos_list_5_0_i6);
Declare_label(mercury__arg_info__make_arg_infos_list_5_0_i7);
Declare_label(mercury__arg_info__make_arg_infos_list_5_0_i1009);
Declare_label(mercury__arg_info__make_arg_infos_list_5_0_i1007);
Declare_label(mercury__arg_info__make_arg_infos_list_5_0_i1008);
Declare_static(mercury__arg_info__make_arg_infos_compact_list_6_0);
Declare_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i6);
Declare_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i8);
Declare_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i9);
Declare_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i7);
Declare_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i10);
Declare_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i1013);
Declare_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i1011);
Declare_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i1012);

Word mercury_data_arg_info__common_0[] = {
	((Integer) 1),
	((Integer) 0)
};

Word mercury_data_arg_info__common_1[] = {
	((Integer) 2),
	((Integer) 0)
};

Word * mercury_data_arg_info__common_2[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_arg_info__common_1),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_arg_info__common_3[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_arg_info__common_0),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_arg_info__common_2)
};

Word mercury_data_arg_info__common_4[] = {
	((Integer) 3),
	((Integer) 0)
};

Word * mercury_data_arg_info__common_5[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_arg_info__common_4),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_arg_info__common_6[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_arg_info__common_1),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_arg_info__common_5)
};

BEGIN_MODULE(mercury__arg_info_module0)
	init_entry(mercury__arg_info__generate_arg_info_3_0);
	init_label(mercury__arg_info__generate_arg_info_3_0_i2);
	init_label(mercury__arg_info__generate_arg_info_3_0_i3);
BEGIN_CODE

/* code for predicate 'generate_arg_info'/3 in mode 0 */
Define_entry(mercury__arg_info__generate_arg_info_3_0);
	incr_sp_push_msg(3, "generate_arg_info");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__arg_info__generate_arg_info_3_0_i2,
		ENTRY(mercury__arg_info__generate_arg_info_3_0));
	}
Define_label(mercury__arg_info__generate_arg_info_3_0_i2);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_arg_info_3_0));
	r3 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__arg_info__generate_arg_info_3_0_i3,
		ENTRY(mercury__arg_info__generate_arg_info_3_0));
	}
Define_label(mercury__arg_info__generate_arg_info_3_0_i3);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_arg_info_3_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__arg_info__generate_pred_arg_info_4_0),
		ENTRY(mercury__arg_info__generate_arg_info_3_0));
END_MODULE

BEGIN_MODULE(mercury__arg_info_module1)
	init_entry(mercury__arg_info__unify_arg_info_3_0);
	init_label(mercury__arg_info__unify_arg_info_3_0_i1007);
	init_label(mercury__arg_info__unify_arg_info_3_0_i4);
	init_label(mercury__arg_info__unify_arg_info_3_0_i1005);
BEGIN_CODE

/* code for predicate 'arg_info__unify_arg_info'/3 in mode 0 */
Define_entry(mercury__arg_info__unify_arg_info_3_0);
	if (((Integer) r2 != ((Integer) 0)))
		GOTO_LABEL(mercury__arg_info__unify_arg_info_3_0_i1007);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_arg_info__common_3);
	proceed();
Define_label(mercury__arg_info__unify_arg_info_3_0_i1007);
	incr_sp_push_msg(1, "arg_info__unify_arg_info");
	detstackvar(1) = (Integer) succip;
	if (((Integer) r2 != ((Integer) 1)))
		GOTO_LABEL(mercury__arg_info__unify_arg_info_3_0_i4);
	decr_sp_pop_msg(1);
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury__arg_info__unify_arg_info_3_0_i1005);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_arg_info__common_3);
	proceed();
Define_label(mercury__arg_info__unify_arg_info_3_0_i4);
	r1 = string_const("arg_info: nondet unify!", 23);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__arg_info__unify_arg_info_3_0));
	}
Define_label(mercury__arg_info__unify_arg_info_3_0_i1005);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_arg_info__common_6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__arg_info_module2)
	init_entry(mercury__arg_info__make_arg_infos_6_0);
	init_label(mercury__arg_info__make_arg_infos_6_0_i1006);
	init_label(mercury__arg_info__make_arg_infos_6_0_i1004);
	init_label(mercury__arg_info__make_arg_infos_6_0_i5);
	init_label(mercury__arg_info__make_arg_infos_6_0_i1005);
BEGIN_CODE

/* code for predicate 'make_arg_infos'/6 in mode 0 */
Define_entry(mercury__arg_info__make_arg_infos_6_0);
	if (((Integer) r4 != ((Integer) 1)))
		GOTO_LABEL(mercury__arg_info__make_arg_infos_6_0_i1006);
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) r3;
	r3 = (Integer) tempr1;
	r4 = ((Integer) 2);
	incr_sp_push_msg(1, "make_arg_infos");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury__arg_info__make_arg_infos_6_0_i1004);
	}
Define_label(mercury__arg_info__make_arg_infos_6_0_i1006);
	incr_sp_push_msg(1, "make_arg_infos");
	detstackvar(1) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) r3;
	r3 = (Integer) tempr1;
	r4 = ((Integer) 1);
	GOTO_LABEL(mercury__arg_info__make_arg_infos_6_0_i5);
	}
Define_label(mercury__arg_info__make_arg_infos_6_0_i1004);
	decr_sp_pop_msg(1);
	if (((Integer) r3 == ((Integer) 0)))
		GOTO_LABEL(mercury__arg_info__make_arg_infos_6_0_i1005);
	r3 = ((Integer) 1);
	tailcall(STATIC(mercury__arg_info__make_arg_infos_compact_list_6_0),
		ENTRY(mercury__arg_info__make_arg_infos_6_0));
Define_label(mercury__arg_info__make_arg_infos_6_0_i5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (((Integer) r3 == ((Integer) 0)))
		GOTO_LABEL(mercury__arg_info__make_arg_infos_6_0_i1005);
	r3 = ((Integer) 1);
	tailcall(STATIC(mercury__arg_info__make_arg_infos_compact_list_6_0),
		ENTRY(mercury__arg_info__make_arg_infos_6_0));
Define_label(mercury__arg_info__make_arg_infos_6_0_i1005);
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	tailcall(STATIC(mercury__arg_info__make_arg_infos_list_5_0),
		ENTRY(mercury__arg_info__make_arg_infos_6_0));
END_MODULE

BEGIN_MODULE(mercury__arg_info_module3)
	init_entry(mercury__arg_info__generate_pred_arg_info_4_0);
	init_label(mercury__arg_info__generate_pred_arg_info_4_0_i4);
	init_label(mercury__arg_info__generate_pred_arg_info_4_0_i5);
	init_label(mercury__arg_info__generate_pred_arg_info_4_0_i6);
	init_label(mercury__arg_info__generate_pred_arg_info_4_0_i7);
	init_label(mercury__arg_info__generate_pred_arg_info_4_0_i1002);
BEGIN_CODE

/* code for predicate 'generate_pred_arg_info'/4 in mode 0 */
Define_static(mercury__arg_info__generate_pred_arg_info_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__arg_info__generate_pred_arg_info_4_0_i1002);
	incr_sp_push_msg(5, "generate_pred_arg_info");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__arg_info__generate_pred_arg_info_4_0_i4,
		STATIC(mercury__arg_info__generate_pred_arg_info_4_0));
	}
Define_label(mercury__arg_info__generate_pred_arg_info_4_0_i4);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_pred_arg_info_4_0));
	r3 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__arg_info__generate_pred_arg_info_4_0_i5,
		STATIC(mercury__arg_info__generate_pred_arg_info_4_0));
	}
Define_label(mercury__arg_info__generate_pred_arg_info_4_0_i5);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_pred_arg_info_4_0));
	{
	Declare_entry(mercury__hlds_pred__pred_info_procids_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procids_2_0),
		mercury__arg_info__generate_pred_arg_info_4_0_i6,
		STATIC(mercury__arg_info__generate_pred_arg_info_4_0));
	}
Define_label(mercury__arg_info__generate_pred_arg_info_4_0_i6);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_pred_arg_info_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0),
		mercury__arg_info__generate_pred_arg_info_4_0_i7,
		STATIC(mercury__arg_info__generate_pred_arg_info_4_0));
Define_label(mercury__arg_info__generate_pred_arg_info_4_0_i7);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_pred_arg_info_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__arg_info__generate_pred_arg_info_4_0,
		STATIC(mercury__arg_info__generate_pred_arg_info_4_0));
Define_label(mercury__arg_info__generate_pred_arg_info_4_0_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__arg_info_module4)
	init_entry(mercury__arg_info__generate_proc_list_arg_info_5_0);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i4);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i5);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i6);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i7);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i8);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i9);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i10);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i11);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i12);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i13);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i14);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i15);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i16);
	init_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i1002);
BEGIN_CODE

/* code for predicate 'generate_proc_list_arg_info'/5 in mode 0 */
Define_static(mercury__arg_info__generate_proc_list_arg_info_5_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0_i1002);
	incr_sp_push_msg(12, "generate_proc_list_arg_info");
	detstackvar(12) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r1 = (Integer) r4;
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i4,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i4);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	r3 = (Integer) r1;
	detstackvar(6) = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r4 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i5,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i5);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	detstackvar(7) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i6,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i6);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__pred_info_arg_types_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_arg_types_3_0),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i7,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i7);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	detstackvar(9) = (Integer) r2;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	}
	r3 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i8,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i8);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	detstackvar(10) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__proc_info_argmodes_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_argmodes_2_0),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i9,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i9);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	detstackvar(11) = (Integer) r1;
	r1 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__hlds_pred__proc_info_interface_code_model_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_interface_code_model_2_0),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i10,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i10);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(9);
	r3 = (Integer) detstackvar(11);
	r5 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__arg_info__make_arg_infos_6_0),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i11,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i11);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_arg_info_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_arg_info_3_0),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i12,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i12);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	r5 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	}
	r3 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i13,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i13);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__pred_info_set_procedures_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_set_procedures_3_0),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i14,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i14);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	r5 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_int_0[];
	r1 = (Integer) mercury_data___base_type_info_int_0;
	}
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i15,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i15);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__hlds_module__module_info_set_preds_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_set_preds_3_0),
		mercury__arg_info__generate_proc_list_arg_info_5_0_i16,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
	}
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i16);
	update_prof_current_proc(LABEL(mercury__arg_info__generate_proc_list_arg_info_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	localtailcall(mercury__arg_info__generate_proc_list_arg_info_5_0,
		STATIC(mercury__arg_info__generate_proc_list_arg_info_5_0));
Define_label(mercury__arg_info__generate_proc_list_arg_info_5_0_i1002);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__arg_info_module5)
	init_entry(mercury__arg_info__make_arg_infos_list_5_0);
	init_label(mercury__arg_info__make_arg_infos_list_5_0_i6);
	init_label(mercury__arg_info__make_arg_infos_list_5_0_i7);
	init_label(mercury__arg_info__make_arg_infos_list_5_0_i1009);
	init_label(mercury__arg_info__make_arg_infos_list_5_0_i1007);
	init_label(mercury__arg_info__make_arg_infos_list_5_0_i1008);
BEGIN_CODE

/* code for predicate 'make_arg_infos_list'/5 in mode 0 */
Define_static(mercury__arg_info__make_arg_infos_list_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__arg_info__make_arg_infos_list_5_0_i1009);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__arg_info__make_arg_infos_list_5_0_i1007);
	incr_sp_push_msg(5, "make_arg_infos_list");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r4;
	detstackvar(2) = (Integer) r4;
	{
	Declare_entry(mercury__mode_util__mode_to_arg_mode_4_0);
	call_localret(ENTRY(mercury__mode_util__mode_to_arg_mode_4_0),
		mercury__arg_info__make_arg_infos_list_5_0_i6,
		STATIC(mercury__arg_info__make_arg_infos_list_5_0));
	}
Define_label(mercury__arg_info__make_arg_infos_list_5_0_i6);
	update_prof_current_proc(LABEL(mercury__arg_info__make_arg_infos_list_5_0));
	{
	Word tempr1, tempr2;
	tempr1 = (Integer) detstackvar(1);
	tag_incr_hp(tempr2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr2, ((Integer) 0)) = (Integer) tempr1;
	detstackvar(1) = (Integer) tempr2;
	field(mktag(0), (Integer) tempr2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = ((Integer) tempr1 + ((Integer) 1));
	r4 = (Integer) detstackvar(2);
	localcall(mercury__arg_info__make_arg_infos_list_5_0,
		LABEL(mercury__arg_info__make_arg_infos_list_5_0_i7),
		STATIC(mercury__arg_info__make_arg_infos_list_5_0));
	}
Define_label(mercury__arg_info__make_arg_infos_list_5_0_i7);
	update_prof_current_proc(LABEL(mercury__arg_info__make_arg_infos_list_5_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__arg_info__make_arg_infos_list_5_0_i1009);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__arg_info__make_arg_infos_list_5_0_i1008);
	r1 = string_const("make_arg_infos_list: length mis-match", 37);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__arg_info__make_arg_infos_list_5_0));
	}
Define_label(mercury__arg_info__make_arg_infos_list_5_0_i1007);
	r1 = string_const("make_arg_infos_list: length mis-match", 37);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__arg_info__make_arg_infos_list_5_0));
	}
Define_label(mercury__arg_info__make_arg_infos_list_5_0_i1008);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__arg_info_module6)
	init_entry(mercury__arg_info__make_arg_infos_compact_list_6_0);
	init_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i6);
	init_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i8);
	init_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i9);
	init_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i7);
	init_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i10);
	init_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i1013);
	init_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i1011);
	init_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i1012);
BEGIN_CODE

/* code for predicate 'make_arg_infos_compact_list'/6 in mode 0 */
Define_static(mercury__arg_info__make_arg_infos_compact_list_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__arg_info__make_arg_infos_compact_list_6_0_i1013);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__arg_info__make_arg_infos_compact_list_6_0_i1011);
	incr_sp_push_msg(6, "make_arg_infos_compact_list");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r5;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	{
	Declare_entry(mercury__mode_util__mode_to_arg_mode_4_0);
	call_localret(ENTRY(mercury__mode_util__mode_to_arg_mode_4_0),
		mercury__arg_info__make_arg_infos_compact_list_6_0_i6,
		STATIC(mercury__arg_info__make_arg_infos_compact_list_6_0));
	}
Define_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i6);
	update_prof_current_proc(LABEL(mercury__arg_info__make_arg_infos_compact_list_6_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__arg_info__make_arg_infos_compact_list_6_0_i8);
	r2 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	tag_incr_hp(r6, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r6, ((Integer) 1)) = (Integer) r1;
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(5);
	r3 = ((Integer) tempr1 + ((Integer) 1));
	field(mktag(0), (Integer) r6, ((Integer) 0)) = (Integer) tempr1;
	GOTO_LABEL(mercury__arg_info__make_arg_infos_compact_list_6_0_i7);
	}
Define_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i8);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__arg_info__make_arg_infos_compact_list_6_0_i9);
	r5 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	tag_incr_hp(r6, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r6, ((Integer) 1)) = (Integer) r1;
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(5);
	r4 = ((Integer) tempr1 + ((Integer) 1));
	field(mktag(0), (Integer) r6, ((Integer) 0)) = (Integer) tempr1;
	GOTO_LABEL(mercury__arg_info__make_arg_infos_compact_list_6_0_i7);
	}
Define_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i9);
	r5 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	tag_incr_hp(r6, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r6, ((Integer) 1)) = (Integer) r1;
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(5);
	r4 = ((Integer) tempr1 + ((Integer) 1));
	field(mktag(0), (Integer) r6, ((Integer) 0)) = (Integer) tempr1;
	}
Define_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i7);
	detstackvar(1) = (Integer) r6;
	localcall(mercury__arg_info__make_arg_infos_compact_list_6_0,
		LABEL(mercury__arg_info__make_arg_infos_compact_list_6_0_i10),
		STATIC(mercury__arg_info__make_arg_infos_compact_list_6_0));
Define_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i10);
	update_prof_current_proc(LABEL(mercury__arg_info__make_arg_infos_compact_list_6_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i1013);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__arg_info__make_arg_infos_compact_list_6_0_i1012);
	r1 = string_const("make_arg_infos_list: length mis-match", 37);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__arg_info__make_arg_infos_compact_list_6_0));
	}
Define_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i1011);
	r1 = string_const("make_arg_infos_list: length mis-match", 37);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__arg_info__make_arg_infos_compact_list_6_0));
	}
Define_label(mercury__arg_info__make_arg_infos_compact_list_6_0_i1012);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__arg_info_bunch_0(void)
{
	mercury__arg_info_module0();
	mercury__arg_info_module1();
	mercury__arg_info_module2();
	mercury__arg_info_module3();
	mercury__arg_info_module4();
	mercury__arg_info_module5();
	mercury__arg_info_module6();
}

#endif

void mercury__arg_info__init(void); /* suppress gcc warning */
void mercury__arg_info__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__arg_info_bunch_0();
#endif
}
