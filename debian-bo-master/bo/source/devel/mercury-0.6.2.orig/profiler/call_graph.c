/*
** Automatically generated from `call_graph.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__call_graph__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__call_graph__main_5_0);
Declare_label(mercury__call_graph__main_5_0_i2);
Declare_label(mercury__call_graph__main_5_0_i3);
Declare_label(mercury__call_graph__main_5_0_i4);
Declare_static(mercury__call_graph__build_static_call_graph_6_0);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i4);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i5);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i6);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i7);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i10);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i11);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i12);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i9);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i13);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i14);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i8);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i15);
Declare_label(mercury__call_graph__build_static_call_graph_6_0_i1006);
Declare_static(mercury__call_graph__process_prof_file_2_4_0);
Declare_label(mercury__call_graph__process_prof_file_2_4_0_i2);
Declare_label(mercury__call_graph__process_prof_file_2_4_0_i5);
Declare_label(mercury__call_graph__process_prof_file_2_4_0_i6);
Declare_label(mercury__call_graph__process_prof_file_2_4_0_i7);
Declare_label(mercury__call_graph__process_prof_file_2_4_0_i8);
Declare_label(mercury__call_graph__process_prof_file_2_4_0_i4);

Word * mercury_data_call_graph__common_0[] = {
	(Word *) string_const("\n", 1),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

BEGIN_MODULE(mercury__call_graph_module0)
	init_entry(mercury__call_graph__main_5_0);
	init_label(mercury__call_graph__main_5_0_i2);
	init_label(mercury__call_graph__main_5_0_i3);
	init_label(mercury__call_graph__main_5_0_i4);
BEGIN_CODE

/* code for predicate 'call_graph__main'/5 in mode 0 */
Define_entry(mercury__call_graph__main_5_0);
	incr_sp_push_msg(4, "call_graph__main");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = ((Integer) 2);
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__globals__io_lookup_bool_option_4_0);
	call_localret(ENTRY(mercury__globals__io_lookup_bool_option_4_0),
		mercury__call_graph__main_5_0_i2,
		ENTRY(mercury__call_graph__main_5_0));
	}
Define_label(mercury__call_graph__main_5_0_i2);
	update_prof_current_proc(LABEL(mercury__call_graph__main_5_0));
	detstackvar(3) = (Integer) r1;
	r1 = ((Integer) 1);
	{
	Declare_entry(mercury__globals__io_lookup_bool_option_4_0);
	call_localret(ENTRY(mercury__globals__io_lookup_bool_option_4_0),
		mercury__call_graph__main_5_0_i3,
		ENTRY(mercury__call_graph__main_5_0));
	}
Define_label(mercury__call_graph__main_5_0_i3);
	update_prof_current_proc(LABEL(mercury__call_graph__main_5_0));
	if (((Integer) detstackvar(3) != ((Integer) 0)))
		GOTO_LABEL(mercury__call_graph__main_5_0_i4);
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__call_graph__main_5_0_i4);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r4 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__call_graph__build_static_call_graph_6_0),
		ENTRY(mercury__call_graph__main_5_0));
END_MODULE

BEGIN_MODULE(mercury__call_graph_module1)
	init_entry(mercury__call_graph__build_static_call_graph_6_0);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i4);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i5);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i6);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i7);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i10);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i11);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i12);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i9);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i13);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i14);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i8);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i15);
	init_label(mercury__call_graph__build_static_call_graph_6_0_i1006);
BEGIN_CODE

/* code for predicate 'build_static_call_graph'/6 in mode 0 */
Define_static(mercury__call_graph__build_static_call_graph_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__call_graph__build_static_call_graph_6_0_i1006);
	incr_sp_push_msg(6, "build_static_call_graph");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r3;
	r2 = string_const("\n\tProcessing ", 13);
	r3 = (Integer) r4;
	{
	Declare_entry(mercury__options__maybe_write_string_4_0);
	call_localret(ENTRY(mercury__options__maybe_write_string_4_0),
		mercury__call_graph__build_static_call_graph_6_0_i4,
		STATIC(mercury__call_graph__build_static_call_graph_6_0));
	}
Define_label(mercury__call_graph__build_static_call_graph_6_0_i4);
	update_prof_current_proc(LABEL(mercury__call_graph__build_static_call_graph_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__options__maybe_write_string_4_0);
	call_localret(ENTRY(mercury__options__maybe_write_string_4_0),
		mercury__call_graph__build_static_call_graph_6_0_i5,
		STATIC(mercury__call_graph__build_static_call_graph_6_0));
	}
Define_label(mercury__call_graph__build_static_call_graph_6_0_i5);
	update_prof_current_proc(LABEL(mercury__call_graph__build_static_call_graph_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = string_const("...", 3);
	{
	Declare_entry(mercury__options__maybe_write_string_4_0);
	call_localret(ENTRY(mercury__options__maybe_write_string_4_0),
		mercury__call_graph__build_static_call_graph_6_0_i6,
		STATIC(mercury__call_graph__build_static_call_graph_6_0));
	}
Define_label(mercury__call_graph__build_static_call_graph_6_0_i6);
	update_prof_current_proc(LABEL(mercury__call_graph__build_static_call_graph_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__see_4_0);
	call_localret(ENTRY(mercury__io__see_4_0),
		mercury__call_graph__build_static_call_graph_6_0_i7,
		STATIC(mercury__call_graph__build_static_call_graph_6_0));
	}
Define_label(mercury__call_graph__build_static_call_graph_6_0_i7);
	update_prof_current_proc(LABEL(mercury__call_graph__build_static_call_graph_6_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__call_graph__build_static_call_graph_6_0_i9);
	detstackvar(5) = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__io__error_message_2_0);
	call_localret(ENTRY(mercury__io__error_message_2_0),
		mercury__call_graph__build_static_call_graph_6_0_i10,
		STATIC(mercury__call_graph__build_static_call_graph_6_0));
	}
Define_label(mercury__call_graph__build_static_call_graph_6_0_i10);
	update_prof_current_proc(LABEL(mercury__call_graph__build_static_call_graph_6_0));
	r2 = (Integer) detstackvar(5);
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__stderr_stream_3_0);
	call_localret(ENTRY(mercury__io__stderr_stream_3_0),
		mercury__call_graph__build_static_call_graph_6_0_i11,
		STATIC(mercury__call_graph__build_static_call_graph_6_0));
	}
Define_label(mercury__call_graph__build_static_call_graph_6_0_i11);
	update_prof_current_proc(LABEL(mercury__call_graph__build_static_call_graph_6_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = string_const("mprof: error opening file `", 27);
	tag_incr_hp(r4, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 0)) = (Integer) detstackvar(3);
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = string_const("': ", 3);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r4;
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) r5;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_call_graph__common_0);
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__io__write_strings_4_0);
	call_localret(ENTRY(mercury__io__write_strings_4_0),
		mercury__call_graph__build_static_call_graph_6_0_i12,
		STATIC(mercury__call_graph__build_static_call_graph_6_0));
	}
	}
Define_label(mercury__call_graph__build_static_call_graph_6_0_i12);
	update_prof_current_proc(LABEL(mercury__call_graph__build_static_call_graph_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = string_const(" done", 5);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(1);
	GOTO_LABEL(mercury__call_graph__build_static_call_graph_6_0_i8);
Define_label(mercury__call_graph__build_static_call_graph_6_0_i9);
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__call_graph__process_prof_file_2_4_0),
		mercury__call_graph__build_static_call_graph_6_0_i13,
		STATIC(mercury__call_graph__build_static_call_graph_6_0));
Define_label(mercury__call_graph__build_static_call_graph_6_0_i13);
	update_prof_current_proc(LABEL(mercury__call_graph__build_static_call_graph_6_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__seen_2_0);
	call_localret(ENTRY(mercury__io__seen_2_0),
		mercury__call_graph__build_static_call_graph_6_0_i14,
		STATIC(mercury__call_graph__build_static_call_graph_6_0));
	}
Define_label(mercury__call_graph__build_static_call_graph_6_0_i14);
	update_prof_current_proc(LABEL(mercury__call_graph__build_static_call_graph_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = string_const(" done", 5);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(1);
Define_label(mercury__call_graph__build_static_call_graph_6_0_i8);
	detstackvar(2) = (Integer) r1;
	detstackvar(4) = (Integer) r4;
	detstackvar(1) = (Integer) r5;
	{
	Declare_entry(mercury__options__maybe_write_string_4_0);
	call_localret(ENTRY(mercury__options__maybe_write_string_4_0),
		mercury__call_graph__build_static_call_graph_6_0_i15,
		STATIC(mercury__call_graph__build_static_call_graph_6_0));
	}
Define_label(mercury__call_graph__build_static_call_graph_6_0_i15);
	update_prof_current_proc(LABEL(mercury__call_graph__build_static_call_graph_6_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__call_graph__build_static_call_graph_6_0,
		STATIC(mercury__call_graph__build_static_call_graph_6_0));
Define_label(mercury__call_graph__build_static_call_graph_6_0_i1006);
	r1 = (Integer) r2;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__call_graph_module2)
	init_entry(mercury__call_graph__process_prof_file_2_4_0);
	init_label(mercury__call_graph__process_prof_file_2_4_0_i2);
	init_label(mercury__call_graph__process_prof_file_2_4_0_i5);
	init_label(mercury__call_graph__process_prof_file_2_4_0_i6);
	init_label(mercury__call_graph__process_prof_file_2_4_0_i7);
	init_label(mercury__call_graph__process_prof_file_2_4_0_i8);
	init_label(mercury__call_graph__process_prof_file_2_4_0_i4);
BEGIN_CODE

/* code for predicate 'process_prof_file_2'/4 in mode 0 */
Define_static(mercury__call_graph__process_prof_file_2_4_0);
	incr_sp_push_msg(4, "process_prof_file_2");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__read__maybe_read_label_name_3_0);
	call_localret(ENTRY(mercury__read__maybe_read_label_name_3_0),
		mercury__call_graph__process_prof_file_2_4_0_i2,
		STATIC(mercury__call_graph__process_prof_file_2_4_0));
	}
Define_label(mercury__call_graph__process_prof_file_2_4_0_i2);
	update_prof_current_proc(LABEL(mercury__call_graph__process_prof_file_2_4_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__call_graph__process_prof_file_2_4_0_i4);
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__read__read_label_name_3_0);
	call_localret(ENTRY(mercury__read__read_label_name_3_0),
		mercury__call_graph__process_prof_file_2_4_0_i5,
		STATIC(mercury__call_graph__process_prof_file_2_4_0));
	}
Define_label(mercury__call_graph__process_prof_file_2_4_0_i5);
	update_prof_current_proc(LABEL(mercury__call_graph__process_prof_file_2_4_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	extern Word * mercury_data___base_type_info_string_0[];
	r1 = (Integer) mercury_data___base_type_info_string_0;
	}
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__relation__lookup_element_3_0);
	call_localret(ENTRY(mercury__relation__lookup_element_3_0),
		mercury__call_graph__process_prof_file_2_4_0_i6,
		STATIC(mercury__call_graph__process_prof_file_2_4_0));
	}
Define_label(mercury__call_graph__process_prof_file_2_4_0_i6);
	update_prof_current_proc(LABEL(mercury__call_graph__process_prof_file_2_4_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_string_0[];
	r1 = (Integer) mercury_data___base_type_info_string_0;
	}
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__relation__lookup_element_3_0);
	call_localret(ENTRY(mercury__relation__lookup_element_3_0),
		mercury__call_graph__process_prof_file_2_4_0_i7,
		STATIC(mercury__call_graph__process_prof_file_2_4_0));
	}
Define_label(mercury__call_graph__process_prof_file_2_4_0_i7);
	update_prof_current_proc(LABEL(mercury__call_graph__process_prof_file_2_4_0));
	r4 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_string_0[];
	r1 = (Integer) mercury_data___base_type_info_string_0;
	}
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__relation__add_4_0);
	call_localret(ENTRY(mercury__relation__add_4_0),
		mercury__call_graph__process_prof_file_2_4_0_i8,
		STATIC(mercury__call_graph__process_prof_file_2_4_0));
	}
Define_label(mercury__call_graph__process_prof_file_2_4_0_i8);
	update_prof_current_proc(LABEL(mercury__call_graph__process_prof_file_2_4_0));
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__call_graph__process_prof_file_2_4_0,
		STATIC(mercury__call_graph__process_prof_file_2_4_0));
Define_label(mercury__call_graph__process_prof_file_2_4_0_i4);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__call_graph_bunch_0(void)
{
	mercury__call_graph_module0();
	mercury__call_graph_module1();
	mercury__call_graph_module2();
}

#endif

void mercury__call_graph__init(void); /* suppress gcc warning */
void mercury__call_graph__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__call_graph_bunch_0();
#endif
}
