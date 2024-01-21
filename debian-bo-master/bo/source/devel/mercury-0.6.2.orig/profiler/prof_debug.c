/*
** Automatically generated from `prof_debug.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__prof_debug__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__prof_debug__output_cliques_3_0);
Declare_label(mercury__prof_debug__output_cliques_3_0_i4);
Declare_label(mercury__prof_debug__output_cliques_3_0_i5);
Declare_label(mercury__prof_debug__output_cliques_3_0_i6);
Declare_label(mercury__prof_debug__output_cliques_3_0_i1002);
Define_extern_entry(mercury__prof_debug__output_propagate_info_4_0);
Declare_label(mercury__prof_debug__output_propagate_info_4_0_i2);
Declare_label(mercury__prof_debug__output_propagate_info_4_0_i3);
Declare_label(mercury__prof_debug__output_propagate_info_4_0_i4);
Declare_label(mercury__prof_debug__output_propagate_info_4_0_i5);
Declare_label(mercury__prof_debug__output_propagate_info_4_0_i6);
Declare_static(mercury__prof_debug__print_assoc_list_3_0);
Declare_label(mercury__prof_debug__print_assoc_list_3_0_i4);
Declare_label(mercury__prof_debug__print_assoc_list_3_0_i5);
Declare_label(mercury__prof_debug__print_assoc_list_3_0_i6);
Declare_label(mercury__prof_debug__print_assoc_list_3_0_i7);
Declare_label(mercury__prof_debug__print_assoc_list_3_0_i1002);
Declare_static(mercury__prof_debug__print_list_3_0);
Declare_label(mercury__prof_debug__print_list_3_0_i4);
Declare_label(mercury__prof_debug__print_list_3_0_i5);
Declare_label(mercury__prof_debug__print_list_3_0_i1002);

BEGIN_MODULE(mercury__prof_debug_module0)
	init_entry(mercury__prof_debug__output_cliques_3_0);
	init_label(mercury__prof_debug__output_cliques_3_0_i4);
	init_label(mercury__prof_debug__output_cliques_3_0_i5);
	init_label(mercury__prof_debug__output_cliques_3_0_i6);
	init_label(mercury__prof_debug__output_cliques_3_0_i1002);
BEGIN_CODE

/* code for predicate 'output_cliques'/3 in mode 0 */
Define_entry(mercury__prof_debug__output_cliques_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__prof_debug__output_cliques_3_0_i1002);
	incr_sp_push_msg(3, "output_cliques");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = string_const("================================\n", 33);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prof_debug__output_cliques_3_0_i4,
		ENTRY(mercury__prof_debug__output_cliques_3_0));
	}
Define_label(mercury__prof_debug__output_cliques_3_0_i4);
	update_prof_current_proc(LABEL(mercury__prof_debug__output_cliques_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_string_0[];
	r1 = (Integer) mercury_data___base_type_info_string_0;
	}
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__prof_debug__output_cliques_3_0_i5,
		ENTRY(mercury__prof_debug__output_cliques_3_0));
	}
Define_label(mercury__prof_debug__output_cliques_3_0_i5);
	update_prof_current_proc(LABEL(mercury__prof_debug__output_cliques_3_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__prof_debug__print_list_3_0),
		mercury__prof_debug__output_cliques_3_0_i6,
		ENTRY(mercury__prof_debug__output_cliques_3_0));
Define_label(mercury__prof_debug__output_cliques_3_0_i6);
	update_prof_current_proc(LABEL(mercury__prof_debug__output_cliques_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__prof_debug__output_cliques_3_0,
		ENTRY(mercury__prof_debug__output_cliques_3_0));
Define_label(mercury__prof_debug__output_cliques_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__prof_debug_module1)
	init_entry(mercury__prof_debug__output_propagate_info_4_0);
	init_label(mercury__prof_debug__output_propagate_info_4_0_i2);
	init_label(mercury__prof_debug__output_propagate_info_4_0_i3);
	init_label(mercury__prof_debug__output_propagate_info_4_0_i4);
	init_label(mercury__prof_debug__output_propagate_info_4_0_i5);
	init_label(mercury__prof_debug__output_propagate_info_4_0_i6);
BEGIN_CODE

/* code for predicate 'output_propagate_info'/4 in mode 0 */
Define_entry(mercury__prof_debug__output_propagate_info_4_0);
	incr_sp_push_msg(3, "output_propagate_info");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = string_const("************************\n", 25);
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prof_debug__output_propagate_info_4_0_i2,
		ENTRY(mercury__prof_debug__output_propagate_info_4_0));
	}
Define_label(mercury__prof_debug__output_propagate_info_4_0_i2);
	update_prof_current_proc(LABEL(mercury__prof_debug__output_propagate_info_4_0));
	r2 = (Integer) r1;
	r1 = string_const("Clique\n", 7);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prof_debug__output_propagate_info_4_0_i3,
		ENTRY(mercury__prof_debug__output_propagate_info_4_0));
	}
Define_label(mercury__prof_debug__output_propagate_info_4_0_i3);
	update_prof_current_proc(LABEL(mercury__prof_debug__output_propagate_info_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_string_0[];
	r1 = (Integer) mercury_data___base_type_info_string_0;
	}
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__prof_debug__output_propagate_info_4_0_i4,
		ENTRY(mercury__prof_debug__output_propagate_info_4_0));
	}
Define_label(mercury__prof_debug__output_propagate_info_4_0_i4);
	update_prof_current_proc(LABEL(mercury__prof_debug__output_propagate_info_4_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__prof_debug__print_list_3_0),
		mercury__prof_debug__output_propagate_info_4_0_i5,
		ENTRY(mercury__prof_debug__output_propagate_info_4_0));
Define_label(mercury__prof_debug__output_propagate_info_4_0_i5);
	update_prof_current_proc(LABEL(mercury__prof_debug__output_propagate_info_4_0));
	r2 = (Integer) r1;
	r1 = string_const("\nParents\n", 9);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prof_debug__output_propagate_info_4_0_i6,
		ENTRY(mercury__prof_debug__output_propagate_info_4_0));
	}
Define_label(mercury__prof_debug__output_propagate_info_4_0_i6);
	update_prof_current_proc(LABEL(mercury__prof_debug__output_propagate_info_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__prof_debug__print_assoc_list_3_0),
		ENTRY(mercury__prof_debug__output_propagate_info_4_0));
END_MODULE

BEGIN_MODULE(mercury__prof_debug_module2)
	init_entry(mercury__prof_debug__print_assoc_list_3_0);
	init_label(mercury__prof_debug__print_assoc_list_3_0_i4);
	init_label(mercury__prof_debug__print_assoc_list_3_0_i5);
	init_label(mercury__prof_debug__print_assoc_list_3_0_i6);
	init_label(mercury__prof_debug__print_assoc_list_3_0_i7);
	init_label(mercury__prof_debug__print_assoc_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'print_assoc_list'/3 in mode 0 */
Define_static(mercury__prof_debug__print_assoc_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__prof_debug__print_assoc_list_3_0_i1002);
	incr_sp_push_msg(3, "print_assoc_list");
	detstackvar(3) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prof_debug__print_assoc_list_3_0_i4,
		STATIC(mercury__prof_debug__print_assoc_list_3_0));
	}
Define_label(mercury__prof_debug__print_assoc_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__prof_debug__print_assoc_list_3_0));
	r2 = (Integer) r1;
	r1 = string_const("\t-\t", 3);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prof_debug__print_assoc_list_3_0_i5,
		STATIC(mercury__prof_debug__print_assoc_list_3_0));
	}
Define_label(mercury__prof_debug__print_assoc_list_3_0_i5);
	update_prof_current_proc(LABEL(mercury__prof_debug__print_assoc_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__io__write_int_3_0);
	call_localret(ENTRY(mercury__io__write_int_3_0),
		mercury__prof_debug__print_assoc_list_3_0_i6,
		STATIC(mercury__prof_debug__print_assoc_list_3_0));
	}
Define_label(mercury__prof_debug__print_assoc_list_3_0_i6);
	update_prof_current_proc(LABEL(mercury__prof_debug__print_assoc_list_3_0));
	r2 = (Integer) r1;
	r1 = string_const("\n", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prof_debug__print_assoc_list_3_0_i7,
		STATIC(mercury__prof_debug__print_assoc_list_3_0));
	}
Define_label(mercury__prof_debug__print_assoc_list_3_0_i7);
	update_prof_current_proc(LABEL(mercury__prof_debug__print_assoc_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__prof_debug__print_assoc_list_3_0,
		STATIC(mercury__prof_debug__print_assoc_list_3_0));
Define_label(mercury__prof_debug__print_assoc_list_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__prof_debug_module3)
	init_entry(mercury__prof_debug__print_list_3_0);
	init_label(mercury__prof_debug__print_list_3_0_i4);
	init_label(mercury__prof_debug__print_list_3_0_i5);
	init_label(mercury__prof_debug__print_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'print_list'/3 in mode 0 */
Define_static(mercury__prof_debug__print_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__prof_debug__print_list_3_0_i1002);
	incr_sp_push_msg(2, "print_list");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prof_debug__print_list_3_0_i4,
		STATIC(mercury__prof_debug__print_list_3_0));
	}
Define_label(mercury__prof_debug__print_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__prof_debug__print_list_3_0));
	r2 = (Integer) r1;
	r1 = string_const("\n", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__prof_debug__print_list_3_0_i5,
		STATIC(mercury__prof_debug__print_list_3_0));
	}
Define_label(mercury__prof_debug__print_list_3_0_i5);
	update_prof_current_proc(LABEL(mercury__prof_debug__print_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__prof_debug__print_list_3_0,
		STATIC(mercury__prof_debug__print_list_3_0));
Define_label(mercury__prof_debug__print_list_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__prof_debug_bunch_0(void)
{
	mercury__prof_debug_module0();
	mercury__prof_debug_module1();
	mercury__prof_debug_module2();
	mercury__prof_debug_module3();
}

#endif

void mercury__prof_debug__init(void); /* suppress gcc warning */
void mercury__prof_debug__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__prof_debug_bunch_0();
#endif
}
