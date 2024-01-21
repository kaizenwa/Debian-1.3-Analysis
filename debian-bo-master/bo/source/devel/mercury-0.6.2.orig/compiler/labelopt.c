/*
** Automatically generated from `labelopt.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__labelopt__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__labelopt__main_4_0);
Declare_label(mercury__labelopt__main_4_0_i2);
Declare_label(mercury__labelopt__main_4_0_i3);
Declare_label(mercury__labelopt__main_4_0_i8);
Declare_label(mercury__labelopt__main_4_0_i4);
Define_extern_entry(mercury__labelopt__build_useset_2_0);
Declare_label(mercury__labelopt__build_useset_2_0_i2);
Declare_static(mercury__labelopt__build_useset_2_3_0);
Declare_label(mercury__labelopt__build_useset_2_3_0_i4);
Declare_label(mercury__labelopt__build_useset_2_3_0_i5);
Declare_label(mercury__labelopt__build_useset_2_3_0_i1002);
Declare_static(mercury__labelopt__instr_list_5_0);
Declare_label(mercury__labelopt__instr_list_5_0_i1017);
Declare_label(mercury__labelopt__instr_list_5_0_i10);
Declare_label(mercury__labelopt__instr_list_5_0_i15);
Declare_label(mercury__labelopt__instr_list_5_0_i9);
Declare_label(mercury__labelopt__instr_list_5_0_i8);
Declare_label(mercury__labelopt__instr_list_5_0_i17);
Declare_label(mercury__labelopt__instr_list_5_0_i1016);
Declare_label(mercury__labelopt__instr_list_5_0_i19);
Declare_label(mercury__labelopt__instr_list_5_0_i22);
Declare_label(mercury__labelopt__instr_list_5_0_i23);
Declare_label(mercury__labelopt__instr_list_5_0_i24);
Declare_label(mercury__labelopt__instr_list_5_0_i25);
Declare_label(mercury__labelopt__instr_list_5_0_i29);
Declare_label(mercury__labelopt__instr_list_5_0_i30);
Declare_label(mercury__labelopt__instr_list_5_0_i31);
Declare_label(mercury__labelopt__instr_list_5_0_i32);
Declare_label(mercury__labelopt__instr_list_5_0_i1013);
Declare_static(mercury__labelopt__eliminate_4_0);

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_llds__base_type_info_instr_0[];
extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_labelopt__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_llds__base_type_info_instr_0,
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

BEGIN_MODULE(mercury__labelopt_module0)
	init_entry(mercury__labelopt__main_4_0);
	init_label(mercury__labelopt__main_4_0_i2);
	init_label(mercury__labelopt__main_4_0_i3);
	init_label(mercury__labelopt__main_4_0_i8);
	init_label(mercury__labelopt__main_4_0_i4);
BEGIN_CODE

/* code for predicate 'labelopt__main'/4 in mode 0 */
Define_entry(mercury__labelopt__main_4_0);
	incr_sp_push_msg(3, "labelopt__main");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
		call_localret(STATIC(mercury__labelopt__build_useset_2_0),
		mercury__labelopt__main_4_0_i2,
		ENTRY(mercury__labelopt__main_4_0));
	}
Define_label(mercury__labelopt__main_4_0_i2);
	update_prof_current_proc(LABEL(mercury__labelopt__main_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = ((Integer) 0);
	call_localret(STATIC(mercury__labelopt__instr_list_5_0),
		mercury__labelopt__main_4_0_i3,
		ENTRY(mercury__labelopt__main_4_0));
Define_label(mercury__labelopt__main_4_0_i3);
	update_prof_current_proc(LABEL(mercury__labelopt__main_4_0));
	if (((Integer) detstackvar(2) != ((Integer) 0)))
		GOTO_LABEL(mercury__labelopt__main_4_0_i4);
	if (((Integer) r2 != ((Integer) 0)))
		GOTO_LABEL(mercury__labelopt__main_4_0_i4);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	localcall(mercury__labelopt__main_4_0,
		LABEL(mercury__labelopt__main_4_0_i8),
		ENTRY(mercury__labelopt__main_4_0));
Define_label(mercury__labelopt__main_4_0_i8);
	update_prof_current_proc(LABEL(mercury__labelopt__main_4_0));
	r2 = (Integer) detstackvar(1);
Define_label(mercury__labelopt__main_4_0_i4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__labelopt_module1)
	init_entry(mercury__labelopt__build_useset_2_0);
	init_label(mercury__labelopt__build_useset_2_0_i2);
BEGIN_CODE

/* code for predicate 'labelopt__build_useset'/2 in mode 0 */
Define_entry(mercury__labelopt__build_useset_2_0);
	incr_sp_push_msg(2, "labelopt__build_useset");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__labelopt__build_useset_2_0_i2,
		ENTRY(mercury__labelopt__build_useset_2_0));
	}
Define_label(mercury__labelopt__build_useset_2_0_i2);
	update_prof_current_proc(LABEL(mercury__labelopt__build_useset_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__labelopt__build_useset_2_3_0),
		ENTRY(mercury__labelopt__build_useset_2_0));
END_MODULE

BEGIN_MODULE(mercury__labelopt_module2)
	init_entry(mercury__labelopt__build_useset_2_3_0);
	init_label(mercury__labelopt__build_useset_2_3_0_i4);
	init_label(mercury__labelopt__build_useset_2_3_0_i5);
	init_label(mercury__labelopt__build_useset_2_3_0_i1002);
BEGIN_CODE

/* code for predicate 'labelopt__build_useset_2'/3 in mode 0 */
Define_static(mercury__labelopt__build_useset_2_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__labelopt__build_useset_2_3_0_i1002);
	incr_sp_push_msg(3, "labelopt__build_useset_2");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	{
	Declare_entry(mercury__opt_util__instr_labels_3_0);
	call_localret(ENTRY(mercury__opt_util__instr_labels_3_0),
		mercury__labelopt__build_useset_2_3_0_i4,
		STATIC(mercury__labelopt__build_useset_2_3_0));
	}
Define_label(mercury__labelopt__build_useset_2_3_0_i4);
	update_prof_current_proc(LABEL(mercury__labelopt__build_useset_2_3_0));
	r3 = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__insert_list_3_0);
	call_localret(ENTRY(mercury__set__insert_list_3_0),
		mercury__labelopt__build_useset_2_3_0_i5,
		STATIC(mercury__labelopt__build_useset_2_3_0));
	}
Define_label(mercury__labelopt__build_useset_2_3_0_i5);
	update_prof_current_proc(LABEL(mercury__labelopt__build_useset_2_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__labelopt__build_useset_2_3_0,
		STATIC(mercury__labelopt__build_useset_2_3_0));
Define_label(mercury__labelopt__build_useset_2_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__labelopt_module3)
	init_entry(mercury__labelopt__instr_list_5_0);
	init_label(mercury__labelopt__instr_list_5_0_i1017);
	init_label(mercury__labelopt__instr_list_5_0_i10);
	init_label(mercury__labelopt__instr_list_5_0_i15);
	init_label(mercury__labelopt__instr_list_5_0_i9);
	init_label(mercury__labelopt__instr_list_5_0_i8);
	init_label(mercury__labelopt__instr_list_5_0_i17);
	init_label(mercury__labelopt__instr_list_5_0_i1016);
	init_label(mercury__labelopt__instr_list_5_0_i19);
	init_label(mercury__labelopt__instr_list_5_0_i22);
	init_label(mercury__labelopt__instr_list_5_0_i23);
	init_label(mercury__labelopt__instr_list_5_0_i24);
	init_label(mercury__labelopt__instr_list_5_0_i25);
	init_label(mercury__labelopt__instr_list_5_0_i29);
	init_label(mercury__labelopt__instr_list_5_0_i30);
	init_label(mercury__labelopt__instr_list_5_0_i31);
	init_label(mercury__labelopt__instr_list_5_0_i32);
	init_label(mercury__labelopt__instr_list_5_0_i1013);
BEGIN_CODE

/* code for predicate 'labelopt__instr_list'/5 in mode 0 */
Define_static(mercury__labelopt__instr_list_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__labelopt__instr_list_5_0_i1013);
	r4 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r6 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if ((tag((Integer) r4) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__labelopt__instr_list_5_0_i1016);
	if (((Integer) field(mktag(3), (Integer) r4, ((Integer) 0)) != ((Integer) 5)))
		GOTO_LABEL(mercury__labelopt__instr_list_5_0_i1016);
	r7 = (Integer) field(mktag(3), (Integer) r4, ((Integer) 1));
	if ((tag((Integer) r7) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__labelopt__instr_list_5_0_i1017);
	r1 = (Integer) r5;
	r4 = (Integer) r6;
	incr_sp_push_msg(7, "labelopt__instr_list");
	detstackvar(7) = (Integer) succip;
	GOTO_LABEL(mercury__labelopt__instr_list_5_0_i9);
Define_label(mercury__labelopt__instr_list_5_0_i1017);
	incr_sp_push_msg(7, "labelopt__instr_list");
	detstackvar(7) = (Integer) succip;
	if ((tag((Integer) r7) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__labelopt__instr_list_5_0_i10);
	r1 = (Integer) r5;
	r4 = (Integer) r6;
	GOTO_LABEL(mercury__labelopt__instr_list_5_0_i9);
Define_label(mercury__labelopt__instr_list_5_0_i10);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r6;
	detstackvar(4) = (Integer) r5;
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	r2 = (Integer) r7;
	{
	Declare_entry(mercury__set__member_2_0);
	call_localret(ENTRY(mercury__set__member_2_0),
		mercury__labelopt__instr_list_5_0_i15,
		STATIC(mercury__labelopt__instr_list_5_0));
	}
Define_label(mercury__labelopt__instr_list_5_0_i15);
	update_prof_current_proc(LABEL(mercury__labelopt__instr_list_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__labelopt__instr_list_5_0_i8);
	r1 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
Define_label(mercury__labelopt__instr_list_5_0_i9);
	r2 = ((Integer) 0);
	r5 = ((Integer) 1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r4;
	r4 = (Integer) tempr1;
	GOTO_LABEL(mercury__labelopt__instr_list_5_0_i29);
	}
Define_label(mercury__labelopt__instr_list_5_0_i8);
	r1 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(1);
	tag_incr_hp(r2, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r4;
	call_localret(STATIC(mercury__labelopt__eliminate_4_0),
		mercury__labelopt__instr_list_5_0_i17,
		STATIC(mercury__labelopt__instr_list_5_0));
Define_label(mercury__labelopt__instr_list_5_0_i17);
	update_prof_current_proc(LABEL(mercury__labelopt__instr_list_5_0));
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r5 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	GOTO_LABEL(mercury__labelopt__instr_list_5_0_i29);
Define_label(mercury__labelopt__instr_list_5_0_i1016);
	incr_sp_push_msg(7, "labelopt__instr_list");
	detstackvar(7) = (Integer) succip;
	if (((Integer) r2 != ((Integer) 0)))
		GOTO_LABEL(mercury__labelopt__instr_list_5_0_i19);
	r1 = (Integer) r4;
	r4 = (Integer) r5;
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) r6;
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r6 = ((Integer) 1);
	GOTO_LABEL(mercury__labelopt__instr_list_5_0_i23);
Define_label(mercury__labelopt__instr_list_5_0_i19);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r5;
	detstackvar(3) = (Integer) r4;
	r1 = (Integer) r6;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	call_localret(STATIC(mercury__labelopt__eliminate_4_0),
		mercury__labelopt__instr_list_5_0_i22,
		STATIC(mercury__labelopt__instr_list_5_0));
Define_label(mercury__labelopt__instr_list_5_0_i22);
	update_prof_current_proc(LABEL(mercury__labelopt__instr_list_5_0));
	r5 = (Integer) r1;
	r6 = (Integer) r2;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
Define_label(mercury__labelopt__instr_list_5_0_i23);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(6) = (Integer) r6;
	{
	Declare_entry(mercury__opt_util__can_instr_fall_through_2_0);
	call_localret(ENTRY(mercury__opt_util__can_instr_fall_through_2_0),
		mercury__labelopt__instr_list_5_0_i24,
		STATIC(mercury__labelopt__instr_list_5_0));
	}
Define_label(mercury__labelopt__instr_list_5_0_i24);
	update_prof_current_proc(LABEL(mercury__labelopt__instr_list_5_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__labelopt__instr_list_5_0_i25);
	r3 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__labelopt__instr_list_5_0_i29);
Define_label(mercury__labelopt__instr_list_5_0_i25);
	r3 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r2 = ((Integer) 1);
	r5 = (Integer) detstackvar(6);
Define_label(mercury__labelopt__instr_list_5_0_i29);
	detstackvar(5) = (Integer) r4;
	detstackvar(6) = (Integer) r5;
	localcall(mercury__labelopt__instr_list_5_0,
		LABEL(mercury__labelopt__instr_list_5_0_i30),
		STATIC(mercury__labelopt__instr_list_5_0));
Define_label(mercury__labelopt__instr_list_5_0_i30);
	update_prof_current_proc(LABEL(mercury__labelopt__instr_list_5_0));
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) detstackvar(5);
	detstackvar(5) = (Integer) tempr1;
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_labelopt__common_0);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__labelopt__instr_list_5_0_i31,
		STATIC(mercury__labelopt__instr_list_5_0));
	}
	}
Define_label(mercury__labelopt__instr_list_5_0_i31);
	update_prof_current_proc(LABEL(mercury__labelopt__instr_list_5_0));
	if (((Integer) detstackvar(6) != ((Integer) 1)))
		GOTO_LABEL(mercury__labelopt__instr_list_5_0_i32);
	if (((Integer) detstackvar(5) != ((Integer) 1)))
		GOTO_LABEL(mercury__labelopt__instr_list_5_0_i32);
	r2 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__labelopt__instr_list_5_0_i32);
	r2 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__labelopt__instr_list_5_0_i1013);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = ((Integer) 1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__labelopt_module4)
	init_entry(mercury__labelopt__eliminate_4_0);
BEGIN_CODE

/* code for predicate 'labelopt__eliminate'/4 in mode 0 */
Define_static(mercury__labelopt__eliminate_4_0);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = ((Integer) 0);
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__labelopt_bunch_0(void)
{
	mercury__labelopt_module0();
	mercury__labelopt_module1();
	mercury__labelopt_module2();
	mercury__labelopt_module3();
	mercury__labelopt_module4();
}

#endif

void mercury__labelopt__init(void); /* suppress gcc warning */
void mercury__labelopt__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__labelopt_bunch_0();
#endif
}
