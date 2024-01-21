/*
** Automatically generated from `globals.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__globals__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Index___globals_globals_0__ua10000_2_0);
Declare_static(mercury__globals__set_options__ua10000_3_0);
Define_extern_entry(mercury__globals__init_2_0);
Define_extern_entry(mercury__globals__get_options_2_0);
Define_extern_entry(mercury__globals__set_options_3_0);
Define_extern_entry(mercury__globals__lookup_option_3_0);
Declare_label(mercury__globals__lookup_option_3_0_i2);
Define_extern_entry(mercury__globals__lookup_bool_option_3_1);
Declare_label(mercury__globals__lookup_bool_option_3_1_i2);
Declare_label(mercury__globals__lookup_bool_option_3_1_i3);
Declare_label(mercury__globals__lookup_bool_option_3_1_i9);
Define_extern_entry(mercury__globals__lookup_bool_option_3_0);
Declare_label(mercury__globals__lookup_bool_option_3_0_i2);
Declare_label(mercury__globals__lookup_bool_option_3_0_i1000);
Define_extern_entry(mercury__globals__lookup_int_option_3_0);
Declare_label(mercury__globals__lookup_int_option_3_0_i2);
Declare_label(mercury__globals__lookup_int_option_3_0_i1000);
Define_extern_entry(mercury__globals__lookup_string_option_3_0);
Declare_label(mercury__globals__lookup_string_option_3_0_i2);
Declare_label(mercury__globals__lookup_string_option_3_0_i3);
Declare_label(mercury__globals__lookup_string_option_3_0_i1000);
Define_extern_entry(mercury__globals__lookup_accumulating_option_3_0);
Declare_label(mercury__globals__lookup_accumulating_option_3_0_i2);
Declare_label(mercury__globals__lookup_accumulating_option_3_0_i3);
Declare_label(mercury__globals__lookup_accumulating_option_3_0_i1000);
Define_extern_entry(mercury__globals__io_init_3_0);
Declare_label(mercury__globals__io_init_3_0_i2);
Define_extern_entry(mercury__globals__io_get_globals_3_0);
Declare_label(mercury__globals__io_get_globals_3_0_i2);
Declare_label(mercury__globals__io_get_globals_3_0_i5);
Declare_label(mercury__globals__io_get_globals_3_0_i4);
Declare_label(mercury__globals__io_get_globals_3_0_i7);
Define_extern_entry(mercury__globals__io_set_globals_3_0);
Declare_label(mercury__globals__io_set_globals_3_0_i2);
Declare_label(mercury__globals__io_set_globals_3_0_i3);
Define_extern_entry(mercury__globals__io_lookup_option_4_0);
Declare_label(mercury__globals__io_lookup_option_4_0_i2);
Declare_label(mercury__globals__io_lookup_option_4_0_i3);
Declare_label(mercury__globals__io_lookup_option_4_0_i4);
Define_extern_entry(mercury__globals__io_set_option_4_0);
Declare_label(mercury__globals__io_set_option_4_0_i2);
Declare_label(mercury__globals__io_set_option_4_0_i3);
Declare_label(mercury__globals__io_set_option_4_0_i4);
Declare_label(mercury__globals__io_set_option_4_0_i5);
Define_extern_entry(mercury__globals__io_lookup_bool_option_4_1);
Declare_label(mercury__globals__io_lookup_bool_option_4_1_i2);
Declare_label(mercury__globals__io_lookup_bool_option_4_1_i3);
Declare_label(mercury__globals__io_lookup_bool_option_4_1_i1);
Define_extern_entry(mercury__globals__io_lookup_bool_option_4_0);
Declare_label(mercury__globals__io_lookup_bool_option_4_0_i2);
Declare_label(mercury__globals__io_lookup_bool_option_4_0_i3);
Define_extern_entry(mercury__globals__io_lookup_int_option_4_0);
Declare_label(mercury__globals__io_lookup_int_option_4_0_i2);
Declare_label(mercury__globals__io_lookup_int_option_4_0_i3);
Define_extern_entry(mercury__globals__io_lookup_string_option_4_0);
Declare_label(mercury__globals__io_lookup_string_option_4_0_i2);
Declare_label(mercury__globals__io_lookup_string_option_4_0_i3);
Define_extern_entry(mercury__globals__io_lookup_accumulating_option_4_0);
Declare_label(mercury__globals__io_lookup_accumulating_option_4_0_i2);
Declare_label(mercury__globals__io_lookup_accumulating_option_4_0_i3);
Define_extern_entry(mercury____Unify___globals__globals_0_0);
Define_extern_entry(mercury____Index___globals__globals_0_0);
Define_extern_entry(mercury____Compare___globals__globals_0_0);

extern Word * mercury_data_globals__base_type_layout_globals_0[];
Word * mercury_data_globals__base_type_info_globals_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___globals__globals_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___globals__globals_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___globals__globals_0_0),
	(Word *) (Integer) mercury_data_globals__base_type_layout_globals_0
};

extern Word * mercury_data_globals__common_1[];
Word * mercury_data_globals__base_type_layout_globals_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_globals__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_globals__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_globals__common_1),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_globals__common_1)
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data_options__base_type_info_option_0[];
extern Word * mercury_data_getopt__base_type_info_option_data_0[];
Word * mercury_data_globals__common_0[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_options__base_type_info_option_0,
	(Word *) (Integer) mercury_data_getopt__base_type_info_option_data_0
};

Word * mercury_data_globals__common_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_globals__common_0),
	(Word *) string_const("globals", 7)
};

BEGIN_MODULE(mercury__globals_module0)
	init_entry(mercury____Index___globals_globals_0__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___globals_globals_0__ua10000'/2 in mode 0 */
Define_static(mercury____Index___globals_globals_0__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module1)
	init_entry(mercury__globals__set_options__ua10000_3_0);
BEGIN_CODE

/* code for predicate 'globals__set_options__ua10000'/3 in mode 0 */
Define_static(mercury__globals__set_options__ua10000_3_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module2)
	init_entry(mercury__globals__init_2_0);
BEGIN_CODE

/* code for predicate 'globals__init'/2 in mode 0 */
Define_entry(mercury__globals__init_2_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module3)
	init_entry(mercury__globals__get_options_2_0);
BEGIN_CODE

/* code for predicate 'globals__get_options'/2 in mode 0 */
Define_entry(mercury__globals__get_options_2_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module4)
	init_entry(mercury__globals__set_options_3_0);
BEGIN_CODE

/* code for predicate 'globals__set_options'/3 in mode 0 */
Define_entry(mercury__globals__set_options_3_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__globals__set_options__ua10000_3_0),
		ENTRY(mercury__globals__set_options_3_0));
END_MODULE

BEGIN_MODULE(mercury__globals_module5)
	init_entry(mercury__globals__lookup_option_3_0);
	init_label(mercury__globals__lookup_option_3_0_i2);
BEGIN_CODE

/* code for predicate 'globals__lookup_option'/3 in mode 0 */
Define_entry(mercury__globals__lookup_option_3_0);
	incr_sp_push_msg(2, "globals__lookup_option");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
		call_localret(STATIC(mercury__globals__get_options_2_0),
		mercury__globals__lookup_option_3_0_i2,
		ENTRY(mercury__globals__lookup_option_3_0));
	}
Define_label(mercury__globals__lookup_option_3_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__lookup_option_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_options__base_type_info_option_0;
	r2 = (Integer) mercury_data_getopt__base_type_info_option_data_0;
	r4 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__map__lookup_3_1);
	tailcall(ENTRY(mercury__map__lookup_3_1),
		ENTRY(mercury__globals__lookup_option_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__globals_module6)
	init_entry(mercury__globals__lookup_bool_option_3_1);
	init_label(mercury__globals__lookup_bool_option_3_1_i2);
	init_label(mercury__globals__lookup_bool_option_3_1_i3);
	init_label(mercury__globals__lookup_bool_option_3_1_i9);
BEGIN_CODE

/* code for predicate 'globals__lookup_bool_option'/3 in mode 1 */
Define_entry(mercury__globals__lookup_bool_option_3_1);
	incr_sp_push_msg(2, "globals__lookup_bool_option");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	{
		call_localret(STATIC(mercury__globals__lookup_option_3_0),
		mercury__globals__lookup_bool_option_3_1_i2,
		ENTRY(mercury__globals__lookup_bool_option_3_1));
	}
Define_label(mercury__globals__lookup_bool_option_3_1_i2);
	update_prof_current_proc(LABEL(mercury__globals__lookup_bool_option_3_1));
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__globals__lookup_bool_option_3_1_i3);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury____Unify___bool__bool_0_0);
	tailcall(ENTRY(mercury____Unify___bool__bool_0_0),
		ENTRY(mercury__globals__lookup_bool_option_3_1));
	}
Define_label(mercury__globals__lookup_bool_option_3_1_i3);
	r1 = string_const("globals__lookup_bool_option: invalid bool option", 48);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__globals__lookup_bool_option_3_1_i9,
		ENTRY(mercury__globals__lookup_bool_option_3_1));
	}
Define_label(mercury__globals__lookup_bool_option_3_1_i9);
	update_prof_current_proc(LABEL(mercury__globals__lookup_bool_option_3_1));
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module7)
	init_entry(mercury__globals__lookup_bool_option_3_0);
	init_label(mercury__globals__lookup_bool_option_3_0_i2);
	init_label(mercury__globals__lookup_bool_option_3_0_i1000);
BEGIN_CODE

/* code for predicate 'globals__lookup_bool_option'/3 in mode 0 */
Define_entry(mercury__globals__lookup_bool_option_3_0);
	incr_sp_push_msg(1, "globals__lookup_bool_option");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__globals__lookup_option_3_0),
		mercury__globals__lookup_bool_option_3_0_i2,
		ENTRY(mercury__globals__lookup_bool_option_3_0));
	}
Define_label(mercury__globals__lookup_bool_option_3_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__lookup_bool_option_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__globals__lookup_bool_option_3_0_i1000);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	proceed();
Define_label(mercury__globals__lookup_bool_option_3_0_i1000);
	r1 = string_const("globals__lookup_bool_option: invalid bool option", 48);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__globals__lookup_bool_option_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__globals_module8)
	init_entry(mercury__globals__lookup_int_option_3_0);
	init_label(mercury__globals__lookup_int_option_3_0_i2);
	init_label(mercury__globals__lookup_int_option_3_0_i1000);
BEGIN_CODE

/* code for predicate 'globals__lookup_int_option'/3 in mode 0 */
Define_entry(mercury__globals__lookup_int_option_3_0);
	incr_sp_push_msg(1, "globals__lookup_int_option");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__globals__lookup_option_3_0),
		mercury__globals__lookup_int_option_3_0_i2,
		ENTRY(mercury__globals__lookup_int_option_3_0));
	}
Define_label(mercury__globals__lookup_int_option_3_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__lookup_int_option_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if ((tag((Integer) r1) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__globals__lookup_int_option_3_0_i1000);
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	proceed();
Define_label(mercury__globals__lookup_int_option_3_0_i1000);
	r1 = string_const("globals__lookup_int_option: invalid int option", 46);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__globals__lookup_int_option_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__globals_module9)
	init_entry(mercury__globals__lookup_string_option_3_0);
	init_label(mercury__globals__lookup_string_option_3_0_i2);
	init_label(mercury__globals__lookup_string_option_3_0_i3);
	init_label(mercury__globals__lookup_string_option_3_0_i1000);
BEGIN_CODE

/* code for predicate 'globals__lookup_string_option'/3 in mode 0 */
Define_entry(mercury__globals__lookup_string_option_3_0);
	incr_sp_push_msg(1, "globals__lookup_string_option");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__globals__lookup_option_3_0),
		mercury__globals__lookup_string_option_3_0_i2,
		ENTRY(mercury__globals__lookup_string_option_3_0));
	}
Define_label(mercury__globals__lookup_string_option_3_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__lookup_string_option_3_0));
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__globals__lookup_string_option_3_0_i3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury__globals__lookup_string_option_3_0_i1000);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	proceed();
Define_label(mercury__globals__lookup_string_option_3_0_i3);
	r1 = string_const("globals__lookup_string_option: invalid string option", 52);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__globals__lookup_string_option_3_0));
	}
Define_label(mercury__globals__lookup_string_option_3_0_i1000);
	r1 = string_const("globals__lookup_string_option: invalid string option", 52);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__globals__lookup_string_option_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__globals_module10)
	init_entry(mercury__globals__lookup_accumulating_option_3_0);
	init_label(mercury__globals__lookup_accumulating_option_3_0_i2);
	init_label(mercury__globals__lookup_accumulating_option_3_0_i3);
	init_label(mercury__globals__lookup_accumulating_option_3_0_i1000);
BEGIN_CODE

/* code for predicate 'globals__lookup_accumulating_option'/3 in mode 0 */
Define_entry(mercury__globals__lookup_accumulating_option_3_0);
	incr_sp_push_msg(1, "globals__lookup_accumulating_option");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__globals__lookup_option_3_0),
		mercury__globals__lookup_accumulating_option_3_0_i2,
		ENTRY(mercury__globals__lookup_accumulating_option_3_0));
	}
Define_label(mercury__globals__lookup_accumulating_option_3_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__lookup_accumulating_option_3_0));
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__globals__lookup_accumulating_option_3_0_i3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury__globals__lookup_accumulating_option_3_0_i1000);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	proceed();
Define_label(mercury__globals__lookup_accumulating_option_3_0_i3);
	r1 = string_const("globals__lookup_accumulating_option: invalid accumulating option", 64);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__globals__lookup_accumulating_option_3_0));
	}
Define_label(mercury__globals__lookup_accumulating_option_3_0_i1000);
	r1 = string_const("globals__lookup_accumulating_option: invalid accumulating option", 64);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__globals__lookup_accumulating_option_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__globals_module11)
	init_entry(mercury__globals__io_init_3_0);
	init_label(mercury__globals__io_init_3_0_i2);
BEGIN_CODE

/* code for predicate 'globals__io_init'/3 in mode 0 */
Define_entry(mercury__globals__io_init_3_0);
	incr_sp_push_msg(2, "globals__io_init");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
		call_localret(STATIC(mercury__globals__init_2_0),
		mercury__globals__io_init_3_0_i2,
		ENTRY(mercury__globals__io_init_3_0));
	}
Define_label(mercury__globals__io_init_3_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__io_init_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__globals__io_set_globals_3_0),
		ENTRY(mercury__globals__io_init_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__globals_module12)
	init_entry(mercury__globals__io_get_globals_3_0);
	init_label(mercury__globals__io_get_globals_3_0_i2);
	init_label(mercury__globals__io_get_globals_3_0_i5);
	init_label(mercury__globals__io_get_globals_3_0_i4);
	init_label(mercury__globals__io_get_globals_3_0_i7);
BEGIN_CODE

/* code for predicate 'globals__io_get_globals'/3 in mode 0 */
Define_entry(mercury__globals__io_get_globals_3_0);
	incr_sp_push_msg(2, "globals__io_get_globals");
	detstackvar(2) = (Integer) succip;
	{
	Declare_entry(mercury__io__get_globals_3_0);
	call_localret(ENTRY(mercury__io__get_globals_3_0),
		mercury__globals__io_get_globals_3_0_i2,
		ENTRY(mercury__globals__io_get_globals_3_0));
	}
Define_label(mercury__globals__io_get_globals_3_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__io_get_globals_3_0));
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_globals__base_type_info_globals_0;
	{
	Declare_entry(mercury__std_util__univ_to_type_2_0);
	call_localret(ENTRY(mercury__std_util__univ_to_type_2_0),
		mercury__globals__io_get_globals_3_0_i5,
		ENTRY(mercury__globals__io_get_globals_3_0));
	}
Define_label(mercury__globals__io_get_globals_3_0_i5);
	update_prof_current_proc(LABEL(mercury__globals__io_get_globals_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__globals__io_get_globals_3_0_i4);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__globals__io_get_globals_3_0_i4);
	r1 = string_const("globals__io_get_globals: univ_to_type failed", 44);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__globals__io_get_globals_3_0_i7,
		ENTRY(mercury__globals__io_get_globals_3_0));
	}
Define_label(mercury__globals__io_get_globals_3_0_i7);
	update_prof_current_proc(LABEL(mercury__globals__io_get_globals_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module13)
	init_entry(mercury__globals__io_set_globals_3_0);
	init_label(mercury__globals__io_set_globals_3_0_i2);
	init_label(mercury__globals__io_set_globals_3_0_i3);
BEGIN_CODE

/* code for predicate 'globals__io_set_globals'/3 in mode 0 */
Define_entry(mercury__globals__io_set_globals_3_0);
	incr_sp_push_msg(2, "globals__io_set_globals");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_globals__base_type_info_globals_0;
	{
	Declare_entry(mercury__copy_2_1);
	call_localret(ENTRY(mercury__copy_2_1),
		mercury__globals__io_set_globals_3_0_i2,
		ENTRY(mercury__globals__io_set_globals_3_0));
	}
Define_label(mercury__globals__io_set_globals_3_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__io_set_globals_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_globals__base_type_info_globals_0;
	{
	Declare_entry(mercury__std_util__type_to_univ_2_0);
	call_localret(ENTRY(mercury__std_util__type_to_univ_2_0),
		mercury__globals__io_set_globals_3_0_i3,
		ENTRY(mercury__globals__io_set_globals_3_0));
	}
Define_label(mercury__globals__io_set_globals_3_0_i3);
	update_prof_current_proc(LABEL(mercury__globals__io_set_globals_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__set_globals_3_0);
	tailcall(ENTRY(mercury__io__set_globals_3_0),
		ENTRY(mercury__globals__io_set_globals_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__globals_module14)
	init_entry(mercury__globals__io_lookup_option_4_0);
	init_label(mercury__globals__io_lookup_option_4_0_i2);
	init_label(mercury__globals__io_lookup_option_4_0_i3);
	init_label(mercury__globals__io_lookup_option_4_0_i4);
BEGIN_CODE

/* code for predicate 'globals__io_lookup_option'/4 in mode 0 */
Define_entry(mercury__globals__io_lookup_option_4_0);
	incr_sp_push_msg(3, "globals__io_lookup_option");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__globals__io_get_globals_3_0),
		mercury__globals__io_lookup_option_4_0_i2,
		ENTRY(mercury__globals__io_lookup_option_4_0));
	}
Define_label(mercury__globals__io_lookup_option_4_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_option_4_0));
	detstackvar(2) = (Integer) r2;
	{
		call_localret(STATIC(mercury__globals__get_options_2_0),
		mercury__globals__io_lookup_option_4_0_i3,
		ENTRY(mercury__globals__io_lookup_option_4_0));
	}
Define_label(mercury__globals__io_lookup_option_4_0_i3);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_option_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_options__base_type_info_option_0;
	r2 = (Integer) mercury_data_getopt__base_type_info_option_data_0;
	r4 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__globals__io_lookup_option_4_0_i4,
		ENTRY(mercury__globals__io_lookup_option_4_0));
	}
Define_label(mercury__globals__io_lookup_option_4_0_i4);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_option_4_0));
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module15)
	init_entry(mercury__globals__io_set_option_4_0);
	init_label(mercury__globals__io_set_option_4_0_i2);
	init_label(mercury__globals__io_set_option_4_0_i3);
	init_label(mercury__globals__io_set_option_4_0_i4);
	init_label(mercury__globals__io_set_option_4_0_i5);
BEGIN_CODE

/* code for predicate 'globals__io_set_option'/4 in mode 0 */
Define_entry(mercury__globals__io_set_option_4_0);
	incr_sp_push_msg(4, "globals__io_set_option");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	{
		call_localret(STATIC(mercury__globals__io_get_globals_3_0),
		mercury__globals__io_set_option_4_0_i2,
		ENTRY(mercury__globals__io_set_option_4_0));
	}
Define_label(mercury__globals__io_set_option_4_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__io_set_option_4_0));
	detstackvar(3) = (Integer) r2;
	{
		call_localret(STATIC(mercury__globals__get_options_2_0),
		mercury__globals__io_set_option_4_0_i3,
		ENTRY(mercury__globals__io_set_option_4_0));
	}
Define_label(mercury__globals__io_set_option_4_0_i3);
	update_prof_current_proc(LABEL(mercury__globals__io_set_option_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_options__base_type_info_option_0;
	r2 = (Integer) mercury_data_getopt__base_type_info_option_data_0;
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__globals__io_set_option_4_0_i4,
		ENTRY(mercury__globals__io_set_option_4_0));
	}
Define_label(mercury__globals__io_set_option_4_0_i4);
	update_prof_current_proc(LABEL(mercury__globals__io_set_option_4_0));
	call_localret(STATIC(mercury__globals__set_options__ua10000_3_0),
		mercury__globals__io_set_option_4_0_i5,
		ENTRY(mercury__globals__io_set_option_4_0));
Define_label(mercury__globals__io_set_option_4_0_i5);
	update_prof_current_proc(LABEL(mercury__globals__io_set_option_4_0));
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
		tailcall(STATIC(mercury__globals__io_set_globals_3_0),
		ENTRY(mercury__globals__io_set_option_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__globals_module16)
	init_entry(mercury__globals__io_lookup_bool_option_4_1);
	init_label(mercury__globals__io_lookup_bool_option_4_1_i2);
	init_label(mercury__globals__io_lookup_bool_option_4_1_i3);
	init_label(mercury__globals__io_lookup_bool_option_4_1_i1);
BEGIN_CODE

/* code for predicate 'globals__io_lookup_bool_option'/4 in mode 1 */
Define_entry(mercury__globals__io_lookup_bool_option_4_1);
	incr_sp_push_msg(3, "globals__io_lookup_bool_option");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	{
		call_localret(STATIC(mercury__globals__io_get_globals_3_0),
		mercury__globals__io_lookup_bool_option_4_1_i2,
		ENTRY(mercury__globals__io_lookup_bool_option_4_1));
	}
Define_label(mercury__globals__io_lookup_bool_option_4_1_i2);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_bool_option_4_1));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__globals__lookup_bool_option_3_1),
		mercury__globals__io_lookup_bool_option_4_1_i3,
		ENTRY(mercury__globals__io_lookup_bool_option_4_1));
	}
Define_label(mercury__globals__io_lookup_bool_option_4_1_i3);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_bool_option_4_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__globals__io_lookup_bool_option_4_1_i1);
	r2 = (Integer) detstackvar(1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__globals__io_lookup_bool_option_4_1_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module17)
	init_entry(mercury__globals__io_lookup_bool_option_4_0);
	init_label(mercury__globals__io_lookup_bool_option_4_0_i2);
	init_label(mercury__globals__io_lookup_bool_option_4_0_i3);
BEGIN_CODE

/* code for predicate 'globals__io_lookup_bool_option'/4 in mode 0 */
Define_entry(mercury__globals__io_lookup_bool_option_4_0);
	incr_sp_push_msg(2, "globals__io_lookup_bool_option");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__globals__io_get_globals_3_0),
		mercury__globals__io_lookup_bool_option_4_0_i2,
		ENTRY(mercury__globals__io_lookup_bool_option_4_0));
	}
Define_label(mercury__globals__io_lookup_bool_option_4_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_bool_option_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	{
		call_localret(STATIC(mercury__globals__lookup_bool_option_3_0),
		mercury__globals__io_lookup_bool_option_4_0_i3,
		ENTRY(mercury__globals__io_lookup_bool_option_4_0));
	}
Define_label(mercury__globals__io_lookup_bool_option_4_0_i3);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_bool_option_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module18)
	init_entry(mercury__globals__io_lookup_int_option_4_0);
	init_label(mercury__globals__io_lookup_int_option_4_0_i2);
	init_label(mercury__globals__io_lookup_int_option_4_0_i3);
BEGIN_CODE

/* code for predicate 'globals__io_lookup_int_option'/4 in mode 0 */
Define_entry(mercury__globals__io_lookup_int_option_4_0);
	incr_sp_push_msg(2, "globals__io_lookup_int_option");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__globals__io_get_globals_3_0),
		mercury__globals__io_lookup_int_option_4_0_i2,
		ENTRY(mercury__globals__io_lookup_int_option_4_0));
	}
Define_label(mercury__globals__io_lookup_int_option_4_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_int_option_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	{
		call_localret(STATIC(mercury__globals__lookup_int_option_3_0),
		mercury__globals__io_lookup_int_option_4_0_i3,
		ENTRY(mercury__globals__io_lookup_int_option_4_0));
	}
Define_label(mercury__globals__io_lookup_int_option_4_0_i3);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_int_option_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module19)
	init_entry(mercury__globals__io_lookup_string_option_4_0);
	init_label(mercury__globals__io_lookup_string_option_4_0_i2);
	init_label(mercury__globals__io_lookup_string_option_4_0_i3);
BEGIN_CODE

/* code for predicate 'globals__io_lookup_string_option'/4 in mode 0 */
Define_entry(mercury__globals__io_lookup_string_option_4_0);
	incr_sp_push_msg(2, "globals__io_lookup_string_option");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__globals__io_get_globals_3_0),
		mercury__globals__io_lookup_string_option_4_0_i2,
		ENTRY(mercury__globals__io_lookup_string_option_4_0));
	}
Define_label(mercury__globals__io_lookup_string_option_4_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_string_option_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	{
		call_localret(STATIC(mercury__globals__lookup_string_option_3_0),
		mercury__globals__io_lookup_string_option_4_0_i3,
		ENTRY(mercury__globals__io_lookup_string_option_4_0));
	}
Define_label(mercury__globals__io_lookup_string_option_4_0_i3);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_string_option_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module20)
	init_entry(mercury__globals__io_lookup_accumulating_option_4_0);
	init_label(mercury__globals__io_lookup_accumulating_option_4_0_i2);
	init_label(mercury__globals__io_lookup_accumulating_option_4_0_i3);
BEGIN_CODE

/* code for predicate 'globals__io_lookup_accumulating_option'/4 in mode 0 */
Define_entry(mercury__globals__io_lookup_accumulating_option_4_0);
	incr_sp_push_msg(2, "globals__io_lookup_accumulating_option");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__globals__io_get_globals_3_0),
		mercury__globals__io_lookup_accumulating_option_4_0_i2,
		ENTRY(mercury__globals__io_lookup_accumulating_option_4_0));
	}
Define_label(mercury__globals__io_lookup_accumulating_option_4_0_i2);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_accumulating_option_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r3;
	{
		call_localret(STATIC(mercury__globals__lookup_accumulating_option_3_0),
		mercury__globals__io_lookup_accumulating_option_4_0_i3,
		ENTRY(mercury__globals__io_lookup_accumulating_option_4_0));
	}
Define_label(mercury__globals__io_lookup_accumulating_option_4_0_i3);
	update_prof_current_proc(LABEL(mercury__globals__io_lookup_accumulating_option_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__globals_module21)
	init_entry(mercury____Unify___globals__globals_0_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___globals__globals_0_0);
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_options__base_type_info_option_0;
	r4 = (Integer) r2;
	r2 = (Integer) mercury_data_getopt__base_type_info_option_data_0;
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___globals__globals_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__globals_module22)
	init_entry(mercury____Index___globals__globals_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___globals__globals_0_0);
	tailcall(STATIC(mercury____Index___globals_globals_0__ua10000_2_0),
		ENTRY(mercury____Index___globals__globals_0_0));
END_MODULE

BEGIN_MODULE(mercury__globals_module23)
	init_entry(mercury____Compare___globals__globals_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___globals__globals_0_0);
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_options__base_type_info_option_0;
	r4 = (Integer) r2;
	r2 = (Integer) mercury_data_getopt__base_type_info_option_data_0;
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___globals__globals_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__globals_bunch_0(void)
{
	mercury__globals_module0();
	mercury__globals_module1();
	mercury__globals_module2();
	mercury__globals_module3();
	mercury__globals_module4();
	mercury__globals_module5();
	mercury__globals_module6();
	mercury__globals_module7();
	mercury__globals_module8();
	mercury__globals_module9();
	mercury__globals_module10();
	mercury__globals_module11();
	mercury__globals_module12();
	mercury__globals_module13();
	mercury__globals_module14();
	mercury__globals_module15();
	mercury__globals_module16();
	mercury__globals_module17();
	mercury__globals_module18();
	mercury__globals_module19();
	mercury__globals_module20();
	mercury__globals_module21();
	mercury__globals_module22();
	mercury__globals_module23();
}

#endif

void mercury__globals__init(void); /* suppress gcc warning */
void mercury__globals__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__globals_bunch_0();
#endif
}
