/*
** Automatically generated from `prog_out.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__prog_out__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__prog_out__write_messages_3_0);
Declare_label(mercury__prog_out__write_messages_3_0_i7);
Declare_label(mercury__prog_out__write_messages_3_0_i4);
Declare_label(mercury__prog_out__write_messages_3_0_i8);
Declare_label(mercury__prog_out__write_messages_3_0_i9);
Declare_label(mercury__prog_out__write_messages_3_0_i16);
Declare_label(mercury__prog_out__write_messages_3_0_i10);
Declare_label(mercury__prog_out__write_messages_3_0_i17);
Declare_label(mercury__prog_out__write_messages_3_0_i18);
Declare_label(mercury__prog_out__write_messages_3_0_i19);
Declare_label(mercury__prog_out__write_messages_3_0_i1007);
Define_extern_entry(mercury__prog_out__write_context_3_0);
Declare_label(mercury__prog_out__write_context_3_0_i2);
Declare_label(mercury__prog_out__write_context_3_0_i3);
Declare_label(mercury__prog_out__write_context_3_0_i4);
Declare_label(mercury__prog_out__write_context_3_0_i7);
Define_extern_entry(mercury__prog_out__write_sym_name_3_0);
Declare_label(mercury__prog_out__write_sym_name_3_0_i4);
Declare_label(mercury__prog_out__write_sym_name_3_0_i5);
Declare_label(mercury__prog_out__write_sym_name_3_0_i1003);
Define_extern_entry(mercury__prog_out__write_module_spec_3_0);

BEGIN_MODULE(mercury__prog_out_module0)
	init_entry(mercury__prog_out__write_messages_3_0);
	init_label(mercury__prog_out__write_messages_3_0_i7);
	init_label(mercury__prog_out__write_messages_3_0_i4);
	init_label(mercury__prog_out__write_messages_3_0_i8);
	init_label(mercury__prog_out__write_messages_3_0_i9);
	init_label(mercury__prog_out__write_messages_3_0_i16);
	init_label(mercury__prog_out__write_messages_3_0_i10);
	init_label(mercury__prog_out__write_messages_3_0_i17);
	init_label(mercury__prog_out__write_messages_3_0_i18);
	init_label(mercury__prog_out__write_messages_3_0_i19);
	init_label(mercury__prog_out__write_messages_3_0_i1007);
BEGIN_CODE

/* code for predicate 'prog_out__write_messages'/3 in mode 0 */
Define_entry(mercury__prog_out__write_messages_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__prog_out__write_messages_3_0_i1007);
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	incr_sp_push_msg(4, "prog_out__write_messages");
	detstackvar(4) = (Integer) succip;
	if ((tag((Integer) r3) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__prog_out__write_messages_3_0_i4);
	detstackvar(1) = (Integer) r5;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 2));
	{
		call_localret(STATIC(mercury__prog_out__write_context_3_0),
		mercury__prog_out__write_messages_3_0_i7,
		ENTRY(mercury__prog_out__write_messages_3_0));
	}
	}
Define_label(mercury__prog_out__write_messages_3_0_i7);
	update_prof_current_proc(LABEL(mercury__prog_out__write_messages_3_0));
	r3 = (Integer) detstackvar(1);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	GOTO_LABEL(mercury__prog_out__write_messages_3_0_i8);
Define_label(mercury__prog_out__write_messages_3_0_i4);
	r1 = (Integer) r4;
	r4 = (Integer) r3;
	r3 = (Integer) r5;
Define_label(mercury__prog_out__write_messages_3_0_i8);
	detstackvar(1) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prog_out__write_messages_3_0_i9,
		ENTRY(mercury__prog_out__write_messages_3_0));
	}
Define_label(mercury__prog_out__write_messages_3_0_i9);
	update_prof_current_proc(LABEL(mercury__prog_out__write_messages_3_0));
	if ((tag((Integer) detstackvar(3)) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__prog_out__write_messages_3_0_i10);
	r2 = (Integer) field(mktag(0), (Integer) detstackvar(3), ((Integer) 0));
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__prog_out__write_messages_3_0_i10);
	if ((strcmp((char *)(Integer) field(mktag(0), (Integer) r2, ((Integer) 0)), (char *)string_const("", 0)) !=0))
		GOTO_LABEL(mercury__prog_out__write_messages_3_0_i10);
	r2 = (Integer) field(mktag(0), (Integer) detstackvar(3), ((Integer) 1));
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__prog_out__write_messages_3_0_i10);
	r2 = (Integer) r1;
	r1 = string_const(".\n", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prog_out__write_messages_3_0_i16,
		ENTRY(mercury__prog_out__write_messages_3_0));
	}
Define_label(mercury__prog_out__write_messages_3_0_i16);
	update_prof_current_proc(LABEL(mercury__prog_out__write_messages_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__prog_out__write_messages_3_0,
		ENTRY(mercury__prog_out__write_messages_3_0));
Define_label(mercury__prog_out__write_messages_3_0_i10);
	r2 = (Integer) r1;
	r1 = string_const(": ", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prog_out__write_messages_3_0_i17,
		ENTRY(mercury__prog_out__write_messages_3_0));
	}
Define_label(mercury__prog_out__write_messages_3_0_i17);
	update_prof_current_proc(LABEL(mercury__prog_out__write_messages_3_0));
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__varset__init_1_0);
	call_localret(ENTRY(mercury__varset__init_1_0),
		mercury__prog_out__write_messages_3_0_i18,
		ENTRY(mercury__prog_out__write_messages_3_0));
	}
Define_label(mercury__prog_out__write_messages_3_0_i18);
	update_prof_current_proc(LABEL(mercury__prog_out__write_messages_3_0));
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__term_io__write_term_nl_4_0);
	call_localret(ENTRY(mercury__term_io__write_term_nl_4_0),
		mercury__prog_out__write_messages_3_0_i19,
		ENTRY(mercury__prog_out__write_messages_3_0));
	}
Define_label(mercury__prog_out__write_messages_3_0_i19);
	update_prof_current_proc(LABEL(mercury__prog_out__write_messages_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__prog_out__write_messages_3_0,
		ENTRY(mercury__prog_out__write_messages_3_0));
Define_label(mercury__prog_out__write_messages_3_0_i1007);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__prog_out_module1)
	init_entry(mercury__prog_out__write_context_3_0);
	init_label(mercury__prog_out__write_context_3_0_i2);
	init_label(mercury__prog_out__write_context_3_0_i3);
	init_label(mercury__prog_out__write_context_3_0_i4);
	init_label(mercury__prog_out__write_context_3_0_i7);
BEGIN_CODE

/* code for predicate 'prog_out__write_context'/3 in mode 0 */
Define_entry(mercury__prog_out__write_context_3_0);
	incr_sp_push_msg(3, "prog_out__write_context");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__term__context_file_2_0);
	call_localret(ENTRY(mercury__term__context_file_2_0),
		mercury__prog_out__write_context_3_0_i2,
		ENTRY(mercury__prog_out__write_context_3_0));
	}
Define_label(mercury__prog_out__write_context_3_0_i2);
	update_prof_current_proc(LABEL(mercury__prog_out__write_context_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__term__context_line_2_0);
	call_localret(ENTRY(mercury__term__context_line_2_0),
		mercury__prog_out__write_context_3_0_i3,
		ENTRY(mercury__prog_out__write_context_3_0));
	}
Define_label(mercury__prog_out__write_context_3_0_i3);
	update_prof_current_proc(LABEL(mercury__prog_out__write_context_3_0));
	if ((strcmp((char *)(Integer) detstackvar(1), (char *)string_const("", 0)) !=0))
		GOTO_LABEL(mercury__prog_out__write_context_3_0_i4);
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__prog_out__write_context_3_0_i4);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	r3 = (Integer) r1;
	r1 = string_const("%s:%03d: ", 9);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(2), ((Integer) 1));
	field(mktag(2), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) tempr1;
	tag_incr_hp(r4, mktag(1), ((Integer) 2));
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r4;
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	{
	Declare_entry(mercury__string__format_3_0);
	call_localret(ENTRY(mercury__string__format_3_0),
		mercury__prog_out__write_context_3_0_i7,
		ENTRY(mercury__prog_out__write_context_3_0));
	}
	}
Define_label(mercury__prog_out__write_context_3_0_i7);
	update_prof_current_proc(LABEL(mercury__prog_out__write_context_3_0));
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		ENTRY(mercury__prog_out__write_context_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__prog_out_module2)
	init_entry(mercury__prog_out__write_sym_name_3_0);
	init_label(mercury__prog_out__write_sym_name_3_0_i4);
	init_label(mercury__prog_out__write_sym_name_3_0_i5);
	init_label(mercury__prog_out__write_sym_name_3_0_i1003);
BEGIN_CODE

/* code for predicate 'prog_out__write_sym_name'/3 in mode 0 */
Define_entry(mercury__prog_out__write_sym_name_3_0);
	if ((tag((Integer) r1) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury__prog_out__write_sym_name_3_0_i1003);
	incr_sp_push_msg(2, "prog_out__write_sym_name");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
		call_localret(STATIC(mercury__prog_out__write_module_spec_3_0),
		mercury__prog_out__write_sym_name_3_0_i4,
		ENTRY(mercury__prog_out__write_sym_name_3_0));
	}
Define_label(mercury__prog_out__write_sym_name_3_0_i4);
	update_prof_current_proc(LABEL(mercury__prog_out__write_sym_name_3_0));
	r2 = (Integer) r1;
	r1 = string_const(":", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prog_out__write_sym_name_3_0_i5,
		ENTRY(mercury__prog_out__write_sym_name_3_0));
	}
Define_label(mercury__prog_out__write_sym_name_3_0_i5);
	update_prof_current_proc(LABEL(mercury__prog_out__write_sym_name_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		ENTRY(mercury__prog_out__write_sym_name_3_0));
	}
Define_label(mercury__prog_out__write_sym_name_3_0_i1003);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		ENTRY(mercury__prog_out__write_sym_name_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__prog_out_module3)
	init_entry(mercury__prog_out__write_module_spec_3_0);
BEGIN_CODE

/* code for predicate 'prog_out__write_module_spec'/3 in mode 0 */
Define_entry(mercury__prog_out__write_module_spec_3_0);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		ENTRY(mercury__prog_out__write_module_spec_3_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__prog_out_bunch_0(void)
{
	mercury__prog_out_module0();
	mercury__prog_out_module1();
	mercury__prog_out_module2();
	mercury__prog_out_module3();
}

#endif

void mercury__prog_out__init(void); /* suppress gcc warning */
void mercury__prog_out__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__prog_out_bunch_0();
#endif
}
