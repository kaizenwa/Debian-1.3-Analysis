/*
** Automatically generated from `dir.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__dir__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__dir__directory_separator_1_1);
Declare_label(mercury__dir__directory_separator_1_1_i1);
Define_extern_entry(mercury__dir__directory_separator_1_0);
Define_extern_entry(mercury__dir__this_directory_1_1);
Declare_label(mercury__dir__this_directory_1_1_i1);
Define_extern_entry(mercury__dir__this_directory_1_0);
Define_extern_entry(mercury__dir__split_name_3_0);
Declare_label(mercury__dir__split_name_3_0_i2);
Define_extern_entry(mercury__dir__basename_2_0);
Declare_label(mercury__dir__basename_2_0_i2);
Define_extern_entry(mercury__dir__dirname_2_0);
Declare_static(mercury__dir__split_name_2_4_0);
Declare_label(mercury__dir__split_name_2_4_0_i4);
Declare_label(mercury__dir__split_name_2_4_0_i2);
Declare_label(mercury__dir__split_name_2_4_0_i7);
Declare_label(mercury__dir__split_name_2_4_0_i8);
Declare_label(mercury__dir__split_name_2_4_0_i10);
Declare_label(mercury__dir__split_name_2_4_0_i13);
Declare_label(mercury__dir__split_name_2_4_0_i12);
Declare_label(mercury__dir__split_name_2_4_0_i15);
Declare_label(mercury__dir__split_name_2_4_0_i6);

BEGIN_MODULE(mercury__dir_module0)
	init_entry(mercury__dir__directory_separator_1_1);
	init_label(mercury__dir__directory_separator_1_1_i1);
BEGIN_CODE

/* code for predicate 'dir__directory_separator'/1 in mode 1 */
Define_entry(mercury__dir__directory_separator_1_1);
	if (((Integer) r1 != ((Integer) 47)))
		GOTO_LABEL(mercury__dir__directory_separator_1_1_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__dir__directory_separator_1_1_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dir_module1)
	init_entry(mercury__dir__directory_separator_1_0);
BEGIN_CODE

/* code for predicate 'dir__directory_separator'/1 in mode 0 */
Define_entry(mercury__dir__directory_separator_1_0);
	r1 = ((Integer) 47);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dir_module2)
	init_entry(mercury__dir__this_directory_1_1);
	init_label(mercury__dir__this_directory_1_1_i1);
BEGIN_CODE

/* code for predicate 'dir__this_directory'/1 in mode 1 */
Define_entry(mercury__dir__this_directory_1_1);
	if ((strcmp((char *)(Integer) r1, (char *)string_const(".", 1)) !=0))
		GOTO_LABEL(mercury__dir__this_directory_1_1_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__dir__this_directory_1_1_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dir_module3)
	init_entry(mercury__dir__this_directory_1_0);
BEGIN_CODE

/* code for predicate 'dir__this_directory'/1 in mode 0 */
Define_entry(mercury__dir__this_directory_1_0);
	r1 = string_const(".", 1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dir_module4)
	init_entry(mercury__dir__split_name_3_0);
	init_label(mercury__dir__split_name_3_0_i2);
BEGIN_CODE

/* code for predicate 'dir__split_name'/3 in mode 0 */
Define_entry(mercury__dir__split_name_3_0);
	incr_sp_push_msg(2, "dir__split_name");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__string__length_2_0);
	call_localret(ENTRY(mercury__string__length_2_0),
		mercury__dir__split_name_3_0_i2,
		ENTRY(mercury__dir__split_name_3_0));
	}
Define_label(mercury__dir__split_name_3_0_i2);
	update_prof_current_proc(LABEL(mercury__dir__split_name_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__dir__split_name_2_4_0),
		ENTRY(mercury__dir__split_name_3_0));
END_MODULE

BEGIN_MODULE(mercury__dir_module5)
	init_entry(mercury__dir__basename_2_0);
	init_label(mercury__dir__basename_2_0_i2);
BEGIN_CODE

/* code for predicate 'dir__basename'/2 in mode 0 */
Define_entry(mercury__dir__basename_2_0);
	incr_sp_push_msg(1, "dir__basename");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__dir__split_name_3_0),
		mercury__dir__basename_2_0_i2,
		ENTRY(mercury__dir__basename_2_0));
	}
Define_label(mercury__dir__basename_2_0_i2);
	update_prof_current_proc(LABEL(mercury__dir__basename_2_0));
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__dir_module6)
	init_entry(mercury__dir__dirname_2_0);
BEGIN_CODE

/* code for predicate 'dir__dirname'/2 in mode 0 */
Define_entry(mercury__dir__dirname_2_0);
	{
		tailcall(STATIC(mercury__dir__split_name_3_0),
		ENTRY(mercury__dir__dirname_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__dir_module7)
	init_entry(mercury__dir__split_name_2_4_0);
	init_label(mercury__dir__split_name_2_4_0_i4);
	init_label(mercury__dir__split_name_2_4_0_i2);
	init_label(mercury__dir__split_name_2_4_0_i7);
	init_label(mercury__dir__split_name_2_4_0_i8);
	init_label(mercury__dir__split_name_2_4_0_i10);
	init_label(mercury__dir__split_name_2_4_0_i13);
	init_label(mercury__dir__split_name_2_4_0_i12);
	init_label(mercury__dir__split_name_2_4_0_i15);
	init_label(mercury__dir__split_name_2_4_0_i6);
BEGIN_CODE

/* code for predicate 'dir__split_name_2'/4 in mode 0 */
Define_static(mercury__dir__split_name_2_4_0);
	r3 = ((Integer) r2 - ((Integer) 1));
	incr_sp_push_msg(4, "dir__split_name_2");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r3 >= ((Integer) 0)))
		GOTO_LABEL(mercury__dir__split_name_2_4_0_i2);
	detstackvar(1) = (Integer) r1;
	{
		call_localret(STATIC(mercury__dir__this_directory_1_0),
		mercury__dir__split_name_2_4_0_i4,
		STATIC(mercury__dir__split_name_2_4_0));
	}
Define_label(mercury__dir__split_name_2_4_0_i4);
	update_prof_current_proc(LABEL(mercury__dir__split_name_2_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dir__split_name_2_4_0_i2);
	detstackvar(1) = (Integer) r1;
	detstackvar(3) = (Integer) r3;
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__string__index_det_3_0);
	call_localret(ENTRY(mercury__string__index_det_3_0),
		mercury__dir__split_name_2_4_0_i7,
		STATIC(mercury__dir__split_name_2_4_0));
	}
Define_label(mercury__dir__split_name_2_4_0_i7);
	update_prof_current_proc(LABEL(mercury__dir__split_name_2_4_0));
	{
		call_localret(STATIC(mercury__dir__directory_separator_1_1),
		mercury__dir__split_name_2_4_0_i8,
		STATIC(mercury__dir__split_name_2_4_0));
	}
Define_label(mercury__dir__split_name_2_4_0_i8);
	update_prof_current_proc(LABEL(mercury__dir__split_name_2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dir__split_name_2_4_0_i6);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__string__split_4_0);
	call_localret(ENTRY(mercury__string__split_4_0),
		mercury__dir__split_name_2_4_0_i10,
		STATIC(mercury__dir__split_name_2_4_0));
	}
Define_label(mercury__dir__split_name_2_4_0_i10);
	update_prof_current_proc(LABEL(mercury__dir__split_name_2_4_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__string__first_char_3_3);
	call_localret(ENTRY(mercury__string__first_char_3_3),
		mercury__dir__split_name_2_4_0_i13,
		STATIC(mercury__dir__split_name_2_4_0));
	}
Define_label(mercury__dir__split_name_2_4_0_i13);
	update_prof_current_proc(LABEL(mercury__dir__split_name_2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__dir__split_name_2_4_0_i12);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dir__split_name_2_4_0_i12);
	r1 = string_const("dir__split_name_2", 17);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__dir__split_name_2_4_0_i15,
		STATIC(mercury__dir__split_name_2_4_0));
	}
Define_label(mercury__dir__split_name_2_4_0_i15);
	update_prof_current_proc(LABEL(mercury__dir__split_name_2_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__dir__split_name_2_4_0_i6);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__dir__split_name_2_4_0,
		STATIC(mercury__dir__split_name_2_4_0));
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__dir_bunch_0(void)
{
	mercury__dir_module0();
	mercury__dir_module1();
	mercury__dir_module2();
	mercury__dir_module3();
	mercury__dir_module4();
	mercury__dir_module5();
	mercury__dir_module6();
	mercury__dir_module7();
}

#endif

void mercury__dir__init(void); /* suppress gcc warning */
void mercury__dir__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__dir_bunch_0();
#endif
}
