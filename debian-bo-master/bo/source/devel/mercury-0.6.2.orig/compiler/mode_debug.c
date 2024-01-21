/*
** Automatically generated from `mode_debug.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__mode_debug__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__mode_debug__mode_checkpoint_4_0);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i2);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i3);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i7);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i11);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i8);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i15);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i12);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i19);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i16);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i20);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i23);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i24);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i28);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i29);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i30);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i31);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i34);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i36);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i37);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i38);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i39);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i33);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i40);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i25);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i42);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i43);
Declare_label(mercury__mode_debug__mode_checkpoint_4_0_i4);
Declare_static(mercury__mode_debug__write_var_insts_5_0);
Declare_label(mercury__mode_debug__write_var_insts_5_0_i4);
Declare_label(mercury__mode_debug__write_var_insts_5_0_i5);
Declare_label(mercury__mode_debug__write_var_insts_5_0_i6);
Declare_label(mercury__mode_debug__write_var_insts_5_0_i7);
Declare_label(mercury__mode_debug__write_var_insts_5_0_i11);
Declare_label(mercury__mode_debug__write_var_insts_5_0_i3);
Declare_label(mercury__mode_debug__write_var_insts_5_0_i2);
Define_extern_entry(mercury____Unify___mode_debug__port_0_0);
Declare_label(mercury____Unify___mode_debug__port_0_0_i1);
Define_extern_entry(mercury____Index___mode_debug__port_0_0);
Define_extern_entry(mercury____Compare___mode_debug__port_0_0);

extern Word * mercury_data_mode_debug__base_type_layout_port_0[];
Word * mercury_data_mode_debug__base_type_info_port_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___mode_debug__port_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___mode_debug__port_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___mode_debug__port_0_0),
	(Word *) (Integer) mercury_data_mode_debug__base_type_layout_port_0
};

extern Word * mercury_data_mode_debug__common_0[];
Word * mercury_data_mode_debug__base_type_layout_port_0[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_mode_debug__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_mode_debug__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_mode_debug__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_mode_debug__common_0)
};

Word * mercury_data_mode_debug__common_0[] = {
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 3),
	(Word *) string_const("enter", 5),
	(Word *) string_const("exit", 4),
	(Word *) string_const("wakeup", 6)
};

BEGIN_MODULE(mercury__mode_debug_module0)
	init_entry(mercury__mode_debug__mode_checkpoint_4_0);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i2);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i3);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i7);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i11);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i8);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i15);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i12);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i19);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i16);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i20);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i23);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i24);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i28);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i29);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i30);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i31);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i34);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i36);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i37);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i38);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i39);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i33);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i40);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i25);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i42);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i43);
	init_label(mercury__mode_debug__mode_checkpoint_4_0_i4);
BEGIN_CODE

/* code for predicate 'mode_checkpoint'/4 in mode 0 */
Define_entry(mercury__mode_debug__mode_checkpoint_4_0);
	incr_sp_push_msg(5, "mode_checkpoint");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__mode_info__mode_info_get_io_state_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_io_state_2_0),
		mercury__mode_debug__mode_checkpoint_4_0_i2,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i2);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 16);
	{
	Declare_entry(mercury__globals__io_lookup_bool_option_4_0);
	call_localret(ENTRY(mercury__globals__io_lookup_bool_option_4_0),
		mercury__mode_debug__mode_checkpoint_4_0_i3,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i3);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__mode_debug__mode_checkpoint_4_0_i4);
	detstackvar(4) = (Integer) r2;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__mode_info__mode_info_get_errors_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_errors_2_0),
		mercury__mode_debug__mode_checkpoint_4_0_i7,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i7);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	if (((Integer) detstackvar(1) != ((Integer) 0)))
		GOTO_LABEL(mercury__mode_debug__mode_checkpoint_4_0_i8);
	r1 = string_const("Enter ", 6);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__mode_debug__mode_checkpoint_4_0_i11,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i11);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = ((Integer) 0);
	GOTO_LABEL(mercury__mode_debug__mode_checkpoint_4_0_i23);
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i8);
	if (((Integer) detstackvar(1) != ((Integer) 2)))
		GOTO_LABEL(mercury__mode_debug__mode_checkpoint_4_0_i12);
	r1 = string_const("Wake  ", 6);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__mode_debug__mode_checkpoint_4_0_i15,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i15);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = ((Integer) 1);
	GOTO_LABEL(mercury__mode_debug__mode_checkpoint_4_0_i23);
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i12);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__mode_debug__mode_checkpoint_4_0_i16);
	r1 = string_const("Exit ", 5);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__mode_debug__mode_checkpoint_4_0_i19,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i19);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = ((Integer) 0);
	GOTO_LABEL(mercury__mode_debug__mode_checkpoint_4_0_i23);
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i16);
	r1 = string_const("Delay  ", 7);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__mode_debug__mode_checkpoint_4_0_i20,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i20);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = ((Integer) 1);
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i23);
	detstackvar(3) = (Integer) r3;
	detstackvar(1) = (Integer) r4;
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__mode_debug__mode_checkpoint_4_0_i24,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i24);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	if (((Integer) detstackvar(1) != ((Integer) 0)))
		GOTO_LABEL(mercury__mode_debug__mode_checkpoint_4_0_i25);
	r2 = (Integer) r1;
	r1 = string_const(":\n", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__mode_debug__mode_checkpoint_4_0_i28,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i28);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 14);
	{
	Declare_entry(mercury__globals__io_lookup_bool_option_4_0);
	call_localret(ENTRY(mercury__globals__io_lookup_bool_option_4_0),
		mercury__mode_debug__mode_checkpoint_4_0_i29,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i29);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	{
	Declare_entry(mercury__passes_aux__maybe_report_stats_3_0);
	call_localret(ENTRY(mercury__passes_aux__maybe_report_stats_3_0),
		mercury__mode_debug__mode_checkpoint_4_0_i30,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i30);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__mode_info__mode_info_get_instmap_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_instmap_2_0),
		mercury__mode_debug__mode_checkpoint_4_0_i31,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i31);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__instmap__is_reachable_1_0);
	call_localret(ENTRY(mercury__instmap__is_reachable_1_0),
		mercury__mode_debug__mode_checkpoint_4_0_i34,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i34);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__mode_debug__mode_checkpoint_4_0_i33);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__instmap__to_assoc_list_2_0);
	call_localret(ENTRY(mercury__instmap__to_assoc_list_2_0),
		mercury__mode_debug__mode_checkpoint_4_0_i36,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i36);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__mode_info__mode_info_get_varset_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_varset_2_0),
		mercury__mode_debug__mode_checkpoint_4_0_i37,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i37);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__mode_info__mode_info_get_instvarset_2_0);
	call_localret(ENTRY(mercury__mode_info__mode_info_get_instvarset_2_0),
		mercury__mode_debug__mode_checkpoint_4_0_i38,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i38);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__mode_debug__write_var_insts_5_0),
		mercury__mode_debug__mode_checkpoint_4_0_i39,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i39);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	r3 = (Integer) detstackvar(3);
	r2 = (Integer) r1;
	r1 = string_const("\n", 1);
	GOTO_LABEL(mercury__mode_debug__mode_checkpoint_4_0_i42);
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i33);
	r1 = string_const("\tUnreachable\n", 13);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__mode_debug__mode_checkpoint_4_0_i40,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i40);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	r3 = (Integer) detstackvar(3);
	r2 = (Integer) r1;
	r1 = string_const("\n", 1);
	GOTO_LABEL(mercury__mode_debug__mode_checkpoint_4_0_i42);
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i25);
	r3 = (Integer) detstackvar(3);
	r2 = (Integer) r1;
	r1 = string_const("\n", 1);
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i42);
	detstackvar(3) = (Integer) r3;
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__mode_debug__mode_checkpoint_4_0_i43,
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i43);
	update_prof_current_proc(LABEL(mercury__mode_debug__mode_checkpoint_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__mode_info__mode_info_set_io_state_3_0);
	tailcall(ENTRY(mercury__mode_info__mode_info_set_io_state_3_0),
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
Define_label(mercury__mode_debug__mode_checkpoint_4_0_i4);
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__mode_info__mode_info_set_io_state_3_0);
	tailcall(ENTRY(mercury__mode_info__mode_info_set_io_state_3_0),
		ENTRY(mercury__mode_debug__mode_checkpoint_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__mode_debug_module1)
	init_entry(mercury__mode_debug__write_var_insts_5_0);
	init_label(mercury__mode_debug__write_var_insts_5_0_i4);
	init_label(mercury__mode_debug__write_var_insts_5_0_i5);
	init_label(mercury__mode_debug__write_var_insts_5_0_i6);
	init_label(mercury__mode_debug__write_var_insts_5_0_i7);
	init_label(mercury__mode_debug__write_var_insts_5_0_i11);
	init_label(mercury__mode_debug__write_var_insts_5_0_i3);
	init_label(mercury__mode_debug__write_var_insts_5_0_i2);
BEGIN_CODE

/* code for predicate 'write_var_insts'/5 in mode 0 */
Define_static(mercury__mode_debug__write_var_insts_5_0);
	incr_sp_push_msg(6, "write_var_insts");
	detstackvar(6) = (Integer) succip;
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__mode_debug__write_var_insts_5_0_i3);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = string_const("\t", 1);
	r2 = (Integer) r4;
	detstackvar(2) = (Integer) r3;
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__mode_debug__write_var_insts_5_0_i4,
		STATIC(mercury__mode_debug__write_var_insts_5_0));
	}
	}
Define_label(mercury__mode_debug__write_var_insts_5_0_i4);
	update_prof_current_proc(LABEL(mercury__mode_debug__write_var_insts_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__mercury_to_mercury__mercury_output_var_4_0);
	call_localret(ENTRY(mercury__mercury_to_mercury__mercury_output_var_4_0),
		mercury__mode_debug__write_var_insts_5_0_i5,
		STATIC(mercury__mode_debug__write_var_insts_5_0));
	}
Define_label(mercury__mode_debug__write_var_insts_5_0_i5);
	update_prof_current_proc(LABEL(mercury__mode_debug__write_var_insts_5_0));
	r2 = (Integer) r1;
	r1 = string_const(" :: ", 4);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__mode_debug__write_var_insts_5_0_i6,
		STATIC(mercury__mode_debug__write_var_insts_5_0));
	}
Define_label(mercury__mode_debug__write_var_insts_5_0_i6);
	update_prof_current_proc(LABEL(mercury__mode_debug__write_var_insts_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__mercury_to_mercury__mercury_output_inst_4_0);
	call_localret(ENTRY(mercury__mercury_to_mercury__mercury_output_inst_4_0),
		mercury__mode_debug__write_var_insts_5_0_i7,
		STATIC(mercury__mode_debug__write_var_insts_5_0));
	}
Define_label(mercury__mode_debug__write_var_insts_5_0_i7);
	update_prof_current_proc(LABEL(mercury__mode_debug__write_var_insts_5_0));
	if (((Integer) detstackvar(5) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__mode_debug__write_var_insts_5_0_i2);
	r2 = (Integer) r1;
	r1 = string_const("\n", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__mode_debug__write_var_insts_5_0_i11,
		STATIC(mercury__mode_debug__write_var_insts_5_0));
	}
Define_label(mercury__mode_debug__write_var_insts_5_0_i11);
	update_prof_current_proc(LABEL(mercury__mode_debug__write_var_insts_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__mode_debug__write_var_insts_5_0,
		STATIC(mercury__mode_debug__write_var_insts_5_0));
Define_label(mercury__mode_debug__write_var_insts_5_0_i3);
	r1 = (Integer) r4;
Define_label(mercury__mode_debug__write_var_insts_5_0_i2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__mode_debug_module2)
	init_entry(mercury____Unify___mode_debug__port_0_0);
	init_label(mercury____Unify___mode_debug__port_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___mode_debug__port_0_0);
	if (((Integer) r1 != (Integer) r2))
		GOTO_LABEL(mercury____Unify___mode_debug__port_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___mode_debug__port_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__mode_debug_module3)
	init_entry(mercury____Index___mode_debug__port_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___mode_debug__port_0_0);
	{
	Declare_entry(mercury__builtin_index_int_2_0);
	tailcall(ENTRY(mercury__builtin_index_int_2_0),
		ENTRY(mercury____Index___mode_debug__port_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__mode_debug_module4)
	init_entry(mercury____Compare___mode_debug__port_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___mode_debug__port_0_0);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___mode_debug__port_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__mode_debug_bunch_0(void)
{
	mercury__mode_debug_module0();
	mercury__mode_debug_module1();
	mercury__mode_debug_module2();
	mercury__mode_debug_module3();
	mercury__mode_debug_module4();
}

#endif

void mercury__mode_debug__init(void); /* suppress gcc warning */
void mercury__mode_debug__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__mode_debug_bunch_0();
#endif
}
