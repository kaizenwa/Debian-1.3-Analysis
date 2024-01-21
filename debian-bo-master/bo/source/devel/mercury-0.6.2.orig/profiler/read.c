/*
** Automatically generated from `read.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__read__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__read__maybe_read_label_addr_3_0);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i2);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i5);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i7);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i10);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i9);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i14);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i13);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i16);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i6);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i19);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i20);
Declare_label(mercury__read__maybe_read_label_addr_3_0_i21);
Define_extern_entry(mercury__read__maybe_read_label_name_3_0);
Declare_label(mercury__read__maybe_read_label_name_3_0_i2);
Declare_label(mercury__read__maybe_read_label_name_3_0_i5);
Declare_label(mercury__read__maybe_read_label_name_3_0_i7);
Declare_label(mercury__read__maybe_read_label_name_3_0_i8);
Declare_label(mercury__read__maybe_read_label_name_3_0_i6);
Declare_label(mercury__read__maybe_read_label_name_3_0_i9);
Declare_label(mercury__read__maybe_read_label_name_3_0_i10);
Declare_label(mercury__read__maybe_read_label_name_3_0_i11);
Define_extern_entry(mercury__read__read_label_addr_3_0);
Declare_label(mercury__read__read_label_addr_3_0_i2);
Declare_label(mercury__read__read_label_addr_3_0_i6);
Declare_label(mercury__read__read_label_addr_3_0_i5);
Declare_label(mercury__read__read_label_addr_3_0_i8);
Declare_label(mercury__read__read_label_addr_3_0_i11);
Declare_label(mercury__read__read_label_addr_3_0_i10);
Declare_label(mercury__read__read_label_addr_3_0_i15);
Declare_label(mercury__read__read_label_addr_3_0_i14);
Declare_label(mercury__read__read_label_addr_3_0_i7);
Declare_label(mercury__read__read_label_addr_3_0_i20);
Declare_label(mercury__read__read_label_addr_3_0_i21);
Declare_label(mercury__read__read_label_addr_3_0_i22);
Define_extern_entry(mercury__read__read_label_name_3_0);
Declare_label(mercury__read__read_label_name_3_0_i2);
Declare_label(mercury__read__read_label_name_3_0_i6);
Declare_label(mercury__read__read_label_name_3_0_i5);
Declare_label(mercury__read__read_label_name_3_0_i8);
Declare_label(mercury__read__read_label_name_3_0_i7);
Declare_label(mercury__read__read_label_name_3_0_i10);
Declare_label(mercury__read__read_label_name_3_0_i11);
Declare_label(mercury__read__read_label_name_3_0_i12);
Define_extern_entry(mercury__read__read_int_3_0);
Declare_label(mercury__read__read_int_3_0_i2);
Declare_label(mercury__read__read_int_3_0_i6);
Declare_label(mercury__read__read_int_3_0_i5);
Declare_label(mercury__read__read_int_3_0_i8);
Declare_label(mercury__read__read_int_3_0_i11);
Declare_label(mercury__read__read_int_3_0_i10);
Declare_label(mercury__read__read_int_3_0_i13);
Declare_label(mercury__read__read_int_3_0_i14);
Declare_label(mercury__read__read_int_3_0_i7);
Declare_label(mercury__read__read_int_3_0_i17);
Declare_label(mercury__read__read_int_3_0_i18);
Declare_label(mercury__read__read_int_3_0_i19);
Declare_static(mercury__read__label_demangle_2_0);
Declare_label(mercury__read__label_demangle_2_0_i22);
Declare_label(mercury__read__label_demangle_2_0_i23);
Declare_label(mercury__read__label_demangle_2_0_i24);
Declare_label(mercury__read__label_demangle_2_0_i1019);
Declare_static(mercury__read__replace_4_0);
Declare_label(mercury__read__replace_4_0_i4);
Declare_label(mercury__read__replace_4_0_i1000);

BEGIN_MODULE(mercury__read_module0)
	init_entry(mercury__read__maybe_read_label_addr_3_0);
	init_label(mercury__read__maybe_read_label_addr_3_0_i2);
	init_label(mercury__read__maybe_read_label_addr_3_0_i5);
	init_label(mercury__read__maybe_read_label_addr_3_0_i7);
	init_label(mercury__read__maybe_read_label_addr_3_0_i10);
	init_label(mercury__read__maybe_read_label_addr_3_0_i9);
	init_label(mercury__read__maybe_read_label_addr_3_0_i14);
	init_label(mercury__read__maybe_read_label_addr_3_0_i13);
	init_label(mercury__read__maybe_read_label_addr_3_0_i16);
	init_label(mercury__read__maybe_read_label_addr_3_0_i6);
	init_label(mercury__read__maybe_read_label_addr_3_0_i19);
	init_label(mercury__read__maybe_read_label_addr_3_0_i20);
	init_label(mercury__read__maybe_read_label_addr_3_0_i21);
BEGIN_CODE

/* code for predicate 'maybe_read_label_addr'/3 in mode 0 */
Define_entry(mercury__read__maybe_read_label_addr_3_0);
	incr_sp_push_msg(3, "maybe_read_label_addr");
	detstackvar(3) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_word_3_0);
	call_localret(ENTRY(mercury__io__read_word_3_0),
		mercury__read__maybe_read_label_addr_3_0_i2,
		ENTRY(mercury__read__maybe_read_label_addr_3_0));
	}
Define_label(mercury__read__maybe_read_label_addr_3_0_i2);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_addr_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__read__maybe_read_label_addr_3_0_i5);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__read__maybe_read_label_addr_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__read__maybe_read_label_addr_3_0_i6);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__string__from_char_list_2_0);
	call_localret(ENTRY(mercury__string__from_char_list_2_0),
		mercury__read__maybe_read_label_addr_3_0_i7,
		ENTRY(mercury__read__maybe_read_label_addr_3_0));
	}
Define_label(mercury__read__maybe_read_label_addr_3_0_i7);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_addr_3_0));
	r2 = (Integer) r1;
	detstackvar(2) = (Integer) r1;
	r1 = ((Integer) 10);
	{
	Declare_entry(mercury__string__base_string_to_int_3_0);
	call_localret(ENTRY(mercury__string__base_string_to_int_3_0),
		mercury__read__maybe_read_label_addr_3_0_i10,
		ENTRY(mercury__read__maybe_read_label_addr_3_0));
	}
Define_label(mercury__read__maybe_read_label_addr_3_0_i10);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_addr_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__read__maybe_read_label_addr_3_0_i9);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__read__maybe_read_label_addr_3_0_i9);
	r1 = ((Integer) 16);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__string__base_string_to_int_3_0);
	call_localret(ENTRY(mercury__string__base_string_to_int_3_0),
		mercury__read__maybe_read_label_addr_3_0_i14,
		ENTRY(mercury__read__maybe_read_label_addr_3_0));
	}
Define_label(mercury__read__maybe_read_label_addr_3_0_i14);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_addr_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__read__maybe_read_label_addr_3_0_i13);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__read__maybe_read_label_addr_3_0_i13);
	r1 = string_const("maybe_read_label_addr: Label address not hexadecimal or integer\n", 64);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__read__maybe_read_label_addr_3_0_i16,
		ENTRY(mercury__read__maybe_read_label_addr_3_0));
	}
Define_label(mercury__read__maybe_read_label_addr_3_0_i16);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_addr_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__read__maybe_read_label_addr_3_0_i6);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__io__error_message_2_0);
	call_localret(ENTRY(mercury__io__error_message_2_0),
		mercury__read__maybe_read_label_addr_3_0_i19,
		ENTRY(mercury__read__maybe_read_label_addr_3_0));
	}
Define_label(mercury__read__maybe_read_label_addr_3_0_i19);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_addr_3_0));
	r2 = (Integer) r1;
	r1 = string_const("maybe_read_label_addr: ", 23);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__read__maybe_read_label_addr_3_0_i20,
		ENTRY(mercury__read__maybe_read_label_addr_3_0));
	}
Define_label(mercury__read__maybe_read_label_addr_3_0_i20);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_addr_3_0));
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__read__maybe_read_label_addr_3_0_i21,
		ENTRY(mercury__read__maybe_read_label_addr_3_0));
	}
Define_label(mercury__read__maybe_read_label_addr_3_0_i21);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_addr_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__read_module1)
	init_entry(mercury__read__maybe_read_label_name_3_0);
	init_label(mercury__read__maybe_read_label_name_3_0_i2);
	init_label(mercury__read__maybe_read_label_name_3_0_i5);
	init_label(mercury__read__maybe_read_label_name_3_0_i7);
	init_label(mercury__read__maybe_read_label_name_3_0_i8);
	init_label(mercury__read__maybe_read_label_name_3_0_i6);
	init_label(mercury__read__maybe_read_label_name_3_0_i9);
	init_label(mercury__read__maybe_read_label_name_3_0_i10);
	init_label(mercury__read__maybe_read_label_name_3_0_i11);
BEGIN_CODE

/* code for predicate 'maybe_read_label_name'/3 in mode 0 */
Define_entry(mercury__read__maybe_read_label_name_3_0);
	incr_sp_push_msg(2, "maybe_read_label_name");
	detstackvar(2) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_word_3_0);
	call_localret(ENTRY(mercury__io__read_word_3_0),
		mercury__read__maybe_read_label_name_3_0_i2,
		ENTRY(mercury__read__maybe_read_label_name_3_0));
	}
Define_label(mercury__read__maybe_read_label_name_3_0_i2);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_name_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__read__maybe_read_label_name_3_0_i5);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__read__maybe_read_label_name_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__read__maybe_read_label_name_3_0_i6);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__read__label_demangle_2_0),
		mercury__read__maybe_read_label_name_3_0_i7,
		ENTRY(mercury__read__maybe_read_label_name_3_0));
Define_label(mercury__read__maybe_read_label_name_3_0_i7);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_name_3_0));
	{
	Declare_entry(mercury__string__from_char_list_2_0);
	call_localret(ENTRY(mercury__string__from_char_list_2_0),
		mercury__read__maybe_read_label_name_3_0_i8,
		ENTRY(mercury__read__maybe_read_label_name_3_0));
	}
Define_label(mercury__read__maybe_read_label_name_3_0_i8);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_name_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__read__maybe_read_label_name_3_0_i6);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__io__error_message_2_0);
	call_localret(ENTRY(mercury__io__error_message_2_0),
		mercury__read__maybe_read_label_name_3_0_i9,
		ENTRY(mercury__read__maybe_read_label_name_3_0));
	}
Define_label(mercury__read__maybe_read_label_name_3_0_i9);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_name_3_0));
	r2 = (Integer) r1;
	r1 = string_const("maybe_read_label_name: ", 23);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__read__maybe_read_label_name_3_0_i10,
		ENTRY(mercury__read__maybe_read_label_name_3_0));
	}
Define_label(mercury__read__maybe_read_label_name_3_0_i10);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_name_3_0));
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__read__maybe_read_label_name_3_0_i11,
		ENTRY(mercury__read__maybe_read_label_name_3_0));
	}
Define_label(mercury__read__maybe_read_label_name_3_0_i11);
	update_prof_current_proc(LABEL(mercury__read__maybe_read_label_name_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__read_module2)
	init_entry(mercury__read__read_label_addr_3_0);
	init_label(mercury__read__read_label_addr_3_0_i2);
	init_label(mercury__read__read_label_addr_3_0_i6);
	init_label(mercury__read__read_label_addr_3_0_i5);
	init_label(mercury__read__read_label_addr_3_0_i8);
	init_label(mercury__read__read_label_addr_3_0_i11);
	init_label(mercury__read__read_label_addr_3_0_i10);
	init_label(mercury__read__read_label_addr_3_0_i15);
	init_label(mercury__read__read_label_addr_3_0_i14);
	init_label(mercury__read__read_label_addr_3_0_i7);
	init_label(mercury__read__read_label_addr_3_0_i20);
	init_label(mercury__read__read_label_addr_3_0_i21);
	init_label(mercury__read__read_label_addr_3_0_i22);
BEGIN_CODE

/* code for predicate 'read_label_addr'/3 in mode 0 */
Define_entry(mercury__read__read_label_addr_3_0);
	incr_sp_push_msg(3, "read_label_addr");
	detstackvar(3) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_word_3_0);
	call_localret(ENTRY(mercury__io__read_word_3_0),
		mercury__read__read_label_addr_3_0_i2,
		ENTRY(mercury__read__read_label_addr_3_0));
	}
Define_label(mercury__read__read_label_addr_3_0_i2);
	update_prof_current_proc(LABEL(mercury__read__read_label_addr_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__read__read_label_addr_3_0_i5);
	r1 = string_const("read_label_addr: EOF reached", 28);
	detstackvar(1) = (Integer) r2;
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__read__read_label_addr_3_0_i6,
		ENTRY(mercury__read__read_label_addr_3_0));
	}
Define_label(mercury__read__read_label_addr_3_0_i6);
	update_prof_current_proc(LABEL(mercury__read__read_label_addr_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__read__read_label_addr_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__read__read_label_addr_3_0_i7);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__string__from_char_list_2_0);
	call_localret(ENTRY(mercury__string__from_char_list_2_0),
		mercury__read__read_label_addr_3_0_i8,
		ENTRY(mercury__read__read_label_addr_3_0));
	}
Define_label(mercury__read__read_label_addr_3_0_i8);
	update_prof_current_proc(LABEL(mercury__read__read_label_addr_3_0));
	r2 = (Integer) r1;
	detstackvar(2) = (Integer) r1;
	r1 = ((Integer) 10);
	{
	Declare_entry(mercury__string__base_string_to_int_3_0);
	call_localret(ENTRY(mercury__string__base_string_to_int_3_0),
		mercury__read__read_label_addr_3_0_i11,
		ENTRY(mercury__read__read_label_addr_3_0));
	}
Define_label(mercury__read__read_label_addr_3_0_i11);
	update_prof_current_proc(LABEL(mercury__read__read_label_addr_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__read__read_label_addr_3_0_i10);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__read__read_label_addr_3_0_i10);
	r1 = ((Integer) 16);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__string__base_string_to_int_3_0);
	call_localret(ENTRY(mercury__string__base_string_to_int_3_0),
		mercury__read__read_label_addr_3_0_i15,
		ENTRY(mercury__read__read_label_addr_3_0));
	}
Define_label(mercury__read__read_label_addr_3_0_i15);
	update_prof_current_proc(LABEL(mercury__read__read_label_addr_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__read__read_label_addr_3_0_i14);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__read__read_label_addr_3_0_i14);
	r1 = string_const("maybe_read_label_addr: Label address not hexadecimal or integer\n", 64);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__read__read_label_addr_3_0_i6,
		ENTRY(mercury__read__read_label_addr_3_0));
	}
Define_label(mercury__read__read_label_addr_3_0_i7);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__io__error_message_2_0);
	call_localret(ENTRY(mercury__io__error_message_2_0),
		mercury__read__read_label_addr_3_0_i20,
		ENTRY(mercury__read__read_label_addr_3_0));
	}
Define_label(mercury__read__read_label_addr_3_0_i20);
	update_prof_current_proc(LABEL(mercury__read__read_label_addr_3_0));
	r2 = (Integer) r1;
	r1 = string_const("read_label_addr: ", 17);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__read__read_label_addr_3_0_i21,
		ENTRY(mercury__read__read_label_addr_3_0));
	}
Define_label(mercury__read__read_label_addr_3_0_i21);
	update_prof_current_proc(LABEL(mercury__read__read_label_addr_3_0));
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__read__read_label_addr_3_0_i22,
		ENTRY(mercury__read__read_label_addr_3_0));
	}
Define_label(mercury__read__read_label_addr_3_0_i22);
	update_prof_current_proc(LABEL(mercury__read__read_label_addr_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__read_module3)
	init_entry(mercury__read__read_label_name_3_0);
	init_label(mercury__read__read_label_name_3_0_i2);
	init_label(mercury__read__read_label_name_3_0_i6);
	init_label(mercury__read__read_label_name_3_0_i5);
	init_label(mercury__read__read_label_name_3_0_i8);
	init_label(mercury__read__read_label_name_3_0_i7);
	init_label(mercury__read__read_label_name_3_0_i10);
	init_label(mercury__read__read_label_name_3_0_i11);
	init_label(mercury__read__read_label_name_3_0_i12);
BEGIN_CODE

/* code for predicate 'read_label_name'/3 in mode 0 */
Define_entry(mercury__read__read_label_name_3_0);
	incr_sp_push_msg(2, "read_label_name");
	detstackvar(2) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_word_3_0);
	call_localret(ENTRY(mercury__io__read_word_3_0),
		mercury__read__read_label_name_3_0_i2,
		ENTRY(mercury__read__read_label_name_3_0));
	}
Define_label(mercury__read__read_label_name_3_0_i2);
	update_prof_current_proc(LABEL(mercury__read__read_label_name_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__read__read_label_name_3_0_i5);
	r1 = string_const("read_label_name: EOF reached", 28);
	detstackvar(1) = (Integer) r2;
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__read__read_label_name_3_0_i6,
		ENTRY(mercury__read__read_label_name_3_0));
	}
Define_label(mercury__read__read_label_name_3_0_i6);
	update_prof_current_proc(LABEL(mercury__read__read_label_name_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__read__read_label_name_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__read__read_label_name_3_0_i7);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__read__label_demangle_2_0),
		mercury__read__read_label_name_3_0_i8,
		ENTRY(mercury__read__read_label_name_3_0));
Define_label(mercury__read__read_label_name_3_0_i8);
	update_prof_current_proc(LABEL(mercury__read__read_label_name_3_0));
	{
	Declare_entry(mercury__string__from_char_list_2_0);
	call_localret(ENTRY(mercury__string__from_char_list_2_0),
		mercury__read__read_label_name_3_0_i6,
		ENTRY(mercury__read__read_label_name_3_0));
	}
Define_label(mercury__read__read_label_name_3_0_i7);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__io__error_message_2_0);
	call_localret(ENTRY(mercury__io__error_message_2_0),
		mercury__read__read_label_name_3_0_i10,
		ENTRY(mercury__read__read_label_name_3_0));
	}
Define_label(mercury__read__read_label_name_3_0_i10);
	update_prof_current_proc(LABEL(mercury__read__read_label_name_3_0));
	r2 = (Integer) r1;
	r1 = string_const("read_label_name: ", 17);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__read__read_label_name_3_0_i11,
		ENTRY(mercury__read__read_label_name_3_0));
	}
Define_label(mercury__read__read_label_name_3_0_i11);
	update_prof_current_proc(LABEL(mercury__read__read_label_name_3_0));
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__read__read_label_name_3_0_i12,
		ENTRY(mercury__read__read_label_name_3_0));
	}
Define_label(mercury__read__read_label_name_3_0_i12);
	update_prof_current_proc(LABEL(mercury__read__read_label_name_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__read_module4)
	init_entry(mercury__read__read_int_3_0);
	init_label(mercury__read__read_int_3_0_i2);
	init_label(mercury__read__read_int_3_0_i6);
	init_label(mercury__read__read_int_3_0_i5);
	init_label(mercury__read__read_int_3_0_i8);
	init_label(mercury__read__read_int_3_0_i11);
	init_label(mercury__read__read_int_3_0_i10);
	init_label(mercury__read__read_int_3_0_i13);
	init_label(mercury__read__read_int_3_0_i14);
	init_label(mercury__read__read_int_3_0_i7);
	init_label(mercury__read__read_int_3_0_i17);
	init_label(mercury__read__read_int_3_0_i18);
	init_label(mercury__read__read_int_3_0_i19);
BEGIN_CODE

/* code for predicate 'read_int'/3 in mode 0 */
Define_entry(mercury__read__read_int_3_0);
	incr_sp_push_msg(3, "read_int");
	detstackvar(3) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_word_3_0);
	call_localret(ENTRY(mercury__io__read_word_3_0),
		mercury__read__read_int_3_0_i2,
		ENTRY(mercury__read__read_int_3_0));
	}
Define_label(mercury__read__read_int_3_0_i2);
	update_prof_current_proc(LABEL(mercury__read__read_int_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__read__read_int_3_0_i5);
	r1 = string_const("read_int: EOF reached", 21);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__read__read_int_3_0_i6,
		ENTRY(mercury__read__read_int_3_0));
	}
Define_label(mercury__read__read_int_3_0_i6);
	update_prof_current_proc(LABEL(mercury__read__read_int_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__read__read_int_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__read__read_int_3_0_i7);
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__string__from_char_list_2_0);
	call_localret(ENTRY(mercury__string__from_char_list_2_0),
		mercury__read__read_int_3_0_i8,
		ENTRY(mercury__read__read_int_3_0));
	}
Define_label(mercury__read__read_int_3_0_i8);
	update_prof_current_proc(LABEL(mercury__read__read_int_3_0));
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__string__to_int_2_0);
	call_localret(ENTRY(mercury__string__to_int_2_0),
		mercury__read__read_int_3_0_i11,
		ENTRY(mercury__read__read_int_3_0));
	}
Define_label(mercury__read__read_int_3_0_i11);
	update_prof_current_proc(LABEL(mercury__read__read_int_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__read__read_int_3_0_i10);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__read__read_int_3_0_i10);
	r1 = string_const("\nInteger = ", 11);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__read__read_int_3_0_i13,
		ENTRY(mercury__read__read_int_3_0));
	}
Define_label(mercury__read__read_int_3_0_i13);
	update_prof_current_proc(LABEL(mercury__read__read_int_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__read__read_int_3_0_i14,
		ENTRY(mercury__read__read_int_3_0));
	}
Define_label(mercury__read__read_int_3_0_i14);
	update_prof_current_proc(LABEL(mercury__read__read_int_3_0));
	detstackvar(1) = (Integer) r1;
	r1 = string_const("\nread_int: Not an integer\n", 26);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__read__read_int_3_0_i6,
		ENTRY(mercury__read__read_int_3_0));
	}
Define_label(mercury__read__read_int_3_0_i7);
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__io__error_message_2_0);
	call_localret(ENTRY(mercury__io__error_message_2_0),
		mercury__read__read_int_3_0_i17,
		ENTRY(mercury__read__read_int_3_0));
	}
Define_label(mercury__read__read_int_3_0_i17);
	update_prof_current_proc(LABEL(mercury__read__read_int_3_0));
	r2 = (Integer) r1;
	r1 = string_const("read_int: ", 10);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__read__read_int_3_0_i18,
		ENTRY(mercury__read__read_int_3_0));
	}
Define_label(mercury__read__read_int_3_0_i18);
	update_prof_current_proc(LABEL(mercury__read__read_int_3_0));
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__read__read_int_3_0_i19,
		ENTRY(mercury__read__read_int_3_0));
	}
Define_label(mercury__read__read_int_3_0_i19);
	update_prof_current_proc(LABEL(mercury__read__read_int_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__read_module5)
	init_entry(mercury__read__label_demangle_2_0);
	init_label(mercury__read__label_demangle_2_0_i22);
	init_label(mercury__read__label_demangle_2_0_i23);
	init_label(mercury__read__label_demangle_2_0_i24);
	init_label(mercury__read__label_demangle_2_0_i1019);
BEGIN_CODE

/* code for predicate 'label_demangle'/2 in mode 0 */
Define_static(mercury__read__label_demangle_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r2 != ((Integer) 109)))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) r2, ((Integer) 0)) != ((Integer) 101)))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) r2, ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 0)) != ((Integer) 114)))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 0)) != ((Integer) 99)))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 0)) != ((Integer) 117)))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 0)) != ((Integer) 114)))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 0)) != ((Integer) 121)))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 0)) != ((Integer) 95)))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	if (((Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 0)) != ((Integer) 95)))
		GOTO_LABEL(mercury__read__label_demangle_2_0_i1019);
	{
	extern Word * mercury_data___base_type_info_character_0[];
	r1 = (Integer) mercury_data___base_type_info_character_0;
	}
	r2 = (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1)), ((Integer) 1));
	incr_sp_push_msg(1, "label_demangle");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__read__label_demangle_2_0_i22,
		STATIC(mercury__read__label_demangle_2_0));
	}
Define_label(mercury__read__label_demangle_2_0_i22);
	update_prof_current_proc(LABEL(mercury__read__label_demangle_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = ((Integer) 41);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = ((Integer) 95);
	r3 = ((Integer) 40);
	call_localret(STATIC(mercury__read__replace_4_0),
		mercury__read__label_demangle_2_0_i23,
		STATIC(mercury__read__label_demangle_2_0));
Define_label(mercury__read__label_demangle_2_0_i23);
	update_prof_current_proc(LABEL(mercury__read__label_demangle_2_0));
	r2 = ((Integer) 95);
	r3 = ((Integer) 47);
	call_localret(STATIC(mercury__read__replace_4_0),
		mercury__read__label_demangle_2_0_i24,
		STATIC(mercury__read__label_demangle_2_0));
Define_label(mercury__read__label_demangle_2_0_i24);
	update_prof_current_proc(LABEL(mercury__read__label_demangle_2_0));
	r2 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_character_0[];
	r1 = (Integer) mercury_data___base_type_info_character_0;
	}
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__list__reverse_2_0);
	tailcall(ENTRY(mercury__list__reverse_2_0),
		STATIC(mercury__read__label_demangle_2_0));
	}
Define_label(mercury__read__label_demangle_2_0_i1019);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__read_module6)
	init_entry(mercury__read__replace_4_0);
	init_label(mercury__read__replace_4_0_i4);
	init_label(mercury__read__replace_4_0_i1000);
BEGIN_CODE

/* code for predicate 'replace'/4 in mode 0 */
Define_static(mercury__read__replace_4_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_character_0[];
	r1 = (Integer) mercury_data___base_type_info_character_0;
	}
	incr_sp_push_msg(1, "replace");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__list__replace_first_4_0);
	call_localret(ENTRY(mercury__list__replace_first_4_0),
		mercury__read__replace_4_0_i4,
		STATIC(mercury__read__replace_4_0));
	}
Define_label(mercury__read__replace_4_0_i4);
	update_prof_current_proc(LABEL(mercury__read__replace_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__read__replace_4_0_i1000);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__read__replace_4_0_i1000);
	r1 = string_const("demangle_label: ill formed label\n", 33);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__read__replace_4_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__read_bunch_0(void)
{
	mercury__read_module0();
	mercury__read_module1();
	mercury__read_module2();
	mercury__read_module3();
	mercury__read_module4();
	mercury__read_module5();
	mercury__read_module6();
}

#endif

void mercury__read__init(void); /* suppress gcc warning */
void mercury__read__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__read_bunch_0();
#endif
}
