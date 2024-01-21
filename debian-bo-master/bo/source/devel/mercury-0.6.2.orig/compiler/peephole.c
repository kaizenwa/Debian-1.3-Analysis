/*
** Automatically generated from `peephole.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__peephole__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__peephole__optimize_5_0);
Declare_label(mercury__peephole__optimize_5_0_i5);
Declare_label(mercury__peephole__optimize_5_0_i6);
Declare_label(mercury__peephole__optimize_5_0_i2);
Declare_label(mercury__peephole__optimize_5_0_i7);
Declare_static(mercury__peephole__find_setups_3_0);
Declare_label(mercury__peephole__find_setups_3_0_i16);
Declare_label(mercury__peephole__find_setups_3_0_i1018);
Declare_label(mercury__peephole__find_setups_3_0_i1020);
Declare_static(mercury__peephole__opt_instr_list_5_0);
Declare_label(mercury__peephole__opt_instr_list_5_0_i4);
Declare_label(mercury__peephole__opt_instr_list_5_0_i5);
Declare_label(mercury__peephole__opt_instr_list_5_0_i6);
Declare_label(mercury__peephole__opt_instr_list_5_0_i1004);
Declare_label(mercury__peephole__opt_instr_list_5_0_i1005);
Declare_static(mercury__peephole__opt_instr_7_0);
Declare_label(mercury__peephole__opt_instr_7_0_i4);
Declare_label(mercury__peephole__opt_instr_7_0_i5);
Declare_label(mercury__peephole__opt_instr_7_0_i10);
Declare_label(mercury__peephole__opt_instr_7_0_i7);
Declare_label(mercury__peephole__opt_instr_7_0_i3);
Declare_static(mercury__peephole__match_6_0);
Declare_label(mercury__peephole__match_6_0_i1193);
Declare_label(mercury__peephole__match_6_0_i1192);
Declare_label(mercury__peephole__match_6_0_i1191);
Declare_label(mercury__peephole__match_6_0_i1190);
Declare_label(mercury__peephole__match_6_0_i1189);
Declare_label(mercury__peephole__match_6_0_i1188);
Declare_label(mercury__peephole__match_6_0_i1187);
Declare_label(mercury__peephole__match_6_0_i1186);
Declare_label(mercury__peephole__match_6_0_i1185);
Declare_label(mercury__peephole__match_6_0_i9);
Declare_label(mercury__peephole__match_6_0_i12);
Declare_label(mercury__peephole__match_6_0_i21);
Declare_label(mercury__peephole__match_6_0_i13);
Declare_label(mercury__peephole__match_6_0_i22);
Declare_label(mercury__peephole__match_6_0_i26);
Declare_label(mercury__peephole__match_6_0_i28);
Declare_label(mercury__peephole__match_6_0_i29);
Declare_label(mercury__peephole__match_6_0_i32);
Declare_label(mercury__peephole__match_6_0_i35);
Declare_label(mercury__peephole__match_6_0_i37);
Declare_label(mercury__peephole__match_6_0_i1124);
Declare_label(mercury__peephole__match_6_0_i34);
Declare_label(mercury__peephole__match_6_0_i39);
Declare_label(mercury__peephole__match_6_0_i46);
Declare_label(mercury__peephole__match_6_0_i45);
Declare_label(mercury__peephole__match_6_0_i42);
Declare_label(mercury__peephole__match_6_0_i49);
Declare_label(mercury__peephole__match_6_0_i56);
Declare_label(mercury__peephole__match_6_0_i59);
Declare_label(mercury__peephole__match_6_0_i61);
Declare_label(mercury__peephole__match_6_0_i1154);
Declare_label(mercury__peephole__match_6_0_i58);
Declare_label(mercury__peephole__match_6_0_i64);
Declare_label(mercury__peephole__match_6_0_i66);
Declare_label(mercury__peephole__match_6_0_i67);
Declare_label(mercury__peephole__match_6_0_i69);
Declare_label(mercury__peephole__match_6_0_i71);
Declare_label(mercury__peephole__match_6_0_i73);
Declare_label(mercury__peephole__match_6_0_i74);
Declare_label(mercury__peephole__match_6_0_i77);
Declare_label(mercury__peephole__match_6_0_i81);
Declare_label(mercury__peephole__match_6_0_i1174);
Declare_label(mercury__peephole__match_6_0_i80);
Declare_label(mercury__peephole__match_6_0_i85);
Declare_label(mercury__peephole__match_6_0_i90);
Declare_label(mercury__peephole__match_6_0_i1181);
Declare_label(mercury__peephole__match_6_0_i86);
Declare_label(mercury__peephole__match_6_0_i97);
Declare_label(mercury__peephole__match_6_0_i98);
Declare_label(mercury__peephole__match_6_0_i102);
Declare_label(mercury__peephole__match_6_0_i104);
Declare_label(mercury__peephole__match_6_0_i107);
Declare_label(mercury__peephole__match_6_0_i106);
Declare_label(mercury__peephole__match_6_0_i119);
Declare_label(mercury__peephole__match_6_0_i122);
Declare_label(mercury__peephole__match_6_0_i123);
Declare_label(mercury__peephole__match_6_0_i1);
Declare_label(mercury__peephole__match_6_0_i1184);
Declare_static(mercury__peephole__decr_4_0);
Declare_label(mercury__peephole__decr_4_0_i2);
Declare_label(mercury__peephole__decr_4_0_i7);
Declare_label(mercury__peephole__decr_4_0_i8);
Declare_label(mercury__peephole__decr_4_0_i9);
Declare_label(mercury__peephole__decr_4_0_i10);
Declare_label(mercury__peephole__decr_4_0_i12);
Declare_label(mercury__peephole__decr_4_0_i14);
Declare_label(mercury__peephole__decr_4_0_i16);
Declare_label(mercury__peephole__decr_4_0_i1017);
Declare_label(mercury__peephole__decr_4_0_i19);
Declare_label(mercury__peephole__decr_4_0_i24);
Declare_label(mercury__peephole__decr_4_0_i6);
Declare_label(mercury__peephole__decr_4_0_i25);
Declare_label(mercury__peephole__decr_4_0_i1);

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_llds__base_type_info_instr_0[];
extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_peephole__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_llds__base_type_info_instr_0,
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

Word mercury_data_peephole__common_1[] = {
	((Integer) 0)
};

Word * mercury_data_peephole__common_2[] = {
	(Word *) ((Integer) 6),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_peephole__common_1)
};

Word * mercury_data_peephole__common_3[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_peephole__common_2),
	(Word *) string_const("early discard", 13)
};

Word * mercury_data_peephole__common_4[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_peephole__common_3),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

BEGIN_MODULE(mercury__peephole_module0)
	init_entry(mercury__peephole__optimize_5_0);
	init_label(mercury__peephole__optimize_5_0_i5);
	init_label(mercury__peephole__optimize_5_0_i6);
	init_label(mercury__peephole__optimize_5_0_i2);
	init_label(mercury__peephole__optimize_5_0_i7);
BEGIN_CODE

/* code for predicate 'peephole__optimize'/5 in mode 0 */
Define_entry(mercury__peephole__optimize_5_0);
	incr_sp_push_msg(3, "peephole__optimize");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__peephole__optimize_5_0_i2);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__peephole__optimize_5_0_i5,
		ENTRY(mercury__peephole__optimize_5_0));
	}
Define_label(mercury__peephole__optimize_5_0_i5);
	update_prof_current_proc(LABEL(mercury__peephole__optimize_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__peephole__find_setups_3_0),
		mercury__peephole__optimize_5_0_i6,
		ENTRY(mercury__peephole__optimize_5_0));
Define_label(mercury__peephole__optimize_5_0_i6);
	update_prof_current_proc(LABEL(mercury__peephole__optimize_5_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__peephole__opt_instr_list_5_0),
		ENTRY(mercury__peephole__optimize_5_0));
Define_label(mercury__peephole__optimize_5_0_i2);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__peephole__optimize_5_0_i7,
		ENTRY(mercury__peephole__optimize_5_0));
	}
Define_label(mercury__peephole__optimize_5_0_i7);
	update_prof_current_proc(LABEL(mercury__peephole__optimize_5_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__peephole__opt_instr_list_5_0),
		ENTRY(mercury__peephole__optimize_5_0));
END_MODULE

BEGIN_MODULE(mercury__peephole_module1)
	init_entry(mercury__peephole__find_setups_3_0);
	init_label(mercury__peephole__find_setups_3_0_i16);
	init_label(mercury__peephole__find_setups_3_0_i1018);
	init_label(mercury__peephole__find_setups_3_0_i1020);
BEGIN_CODE

/* code for predicate 'peephole__find_setups'/3 in mode 0 */
Define_static(mercury__peephole__find_setups_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1018);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if ((tag((Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if (((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0)), ((Integer) 0)) != ((Integer) 5)))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	r4 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if (((Integer) field(mktag(1), (Integer) r4, ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	r5 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r3, ((Integer) 0)), ((Integer) 0));
	if ((tag((Integer) r5) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if (((Integer) field(mktag(3), (Integer) r5, ((Integer) 0)) != ((Integer) 15)))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if ((tag((Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r4, ((Integer) 0)), ((Integer) 0))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if (((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r4, ((Integer) 0)), ((Integer) 0)), ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if ((tag((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r4, ((Integer) 0)), ((Integer) 0)), ((Integer) 1))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if (((Integer) field(mktag(3), (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r4, ((Integer) 0)), ((Integer) 0)), ((Integer) 1)), ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if (((Integer) field(mktag(3), (Integer) r5, ((Integer) 1)) != (Integer) field(mktag(3), (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r4, ((Integer) 0)), ((Integer) 0)), ((Integer) 1)), ((Integer) 1))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if ((tag((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r4, ((Integer) 0)), ((Integer) 0)), ((Integer) 2))) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if (((Integer) field(mktag(0), (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r4, ((Integer) 0)), ((Integer) 0)), ((Integer) 2)), ((Integer) 0)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if ((tag((Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r4, ((Integer) 1)), ((Integer) 0)), ((Integer) 0))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	if (((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r4, ((Integer) 1)), ((Integer) 0)), ((Integer) 0)), ((Integer) 0)) != ((Integer) 5)))
		GOTO_LABEL(mercury__peephole__find_setups_3_0_i1020);
	incr_sp_push_msg(2, "peephole__find_setups");
	detstackvar(2) = (Integer) succip;
	r5 = (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r4, ((Integer) 1)), ((Integer) 0)), ((Integer) 0)), ((Integer) 1));
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) r2;
	r4 = (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0)), ((Integer) 1));
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__peephole__find_setups_3_0_i16,
		STATIC(mercury__peephole__find_setups_3_0));
	}
Define_label(mercury__peephole__find_setups_3_0_i16);
	update_prof_current_proc(LABEL(mercury__peephole__find_setups_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__peephole__find_setups_3_0,
		STATIC(mercury__peephole__find_setups_3_0));
Define_label(mercury__peephole__find_setups_3_0_i1018);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__peephole__find_setups_3_0_i1020);
	r1 = (Integer) r3;
	localtailcall(mercury__peephole__find_setups_3_0,
		STATIC(mercury__peephole__find_setups_3_0));
END_MODULE

BEGIN_MODULE(mercury__peephole_module2)
	init_entry(mercury__peephole__opt_instr_list_5_0);
	init_label(mercury__peephole__opt_instr_list_5_0_i4);
	init_label(mercury__peephole__opt_instr_list_5_0_i5);
	init_label(mercury__peephole__opt_instr_list_5_0_i6);
	init_label(mercury__peephole__opt_instr_list_5_0_i1004);
	init_label(mercury__peephole__opt_instr_list_5_0_i1005);
BEGIN_CODE

/* code for predicate 'peephole__opt_instr_list'/5 in mode 0 */
Define_static(mercury__peephole__opt_instr_list_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__opt_instr_list_5_0_i1004);
	incr_sp_push_msg(5, "peephole__opt_instr_list");
	detstackvar(5) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	localcall(mercury__peephole__opt_instr_list_5_0,
		LABEL(mercury__peephole__opt_instr_list_5_0_i4),
		STATIC(mercury__peephole__opt_instr_list_5_0));
	}
Define_label(mercury__peephole__opt_instr_list_5_0_i4);
	update_prof_current_proc(LABEL(mercury__peephole__opt_instr_list_5_0));
	r3 = (Integer) detstackvar(1);
	r5 = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__peephole__opt_instr_7_0),
		mercury__peephole__opt_instr_list_5_0_i5,
		STATIC(mercury__peephole__opt_instr_list_5_0));
Define_label(mercury__peephole__opt_instr_list_5_0_i5);
	update_prof_current_proc(LABEL(mercury__peephole__opt_instr_list_5_0));
	if (((Integer) detstackvar(1) != ((Integer) 1)))
		GOTO_LABEL(mercury__peephole__opt_instr_list_5_0_i6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	if (((Integer) r2 != ((Integer) 1)))
		GOTO_LABEL(mercury__peephole__opt_instr_list_5_0_i1005);
	r2 = ((Integer) 1);
	proceed();
Define_label(mercury__peephole__opt_instr_list_5_0_i6);
	r2 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__peephole__opt_instr_list_5_0_i1004);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = ((Integer) 1);
	proceed();
Define_label(mercury__peephole__opt_instr_list_5_0_i1005);
	r2 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__peephole_module3)
	init_entry(mercury__peephole__opt_instr_7_0);
	init_label(mercury__peephole__opt_instr_7_0_i4);
	init_label(mercury__peephole__opt_instr_7_0_i5);
	init_label(mercury__peephole__opt_instr_7_0_i10);
	init_label(mercury__peephole__opt_instr_7_0_i7);
	init_label(mercury__peephole__opt_instr_7_0_i3);
BEGIN_CODE

/* code for predicate 'peephole__opt_instr'/7 in mode 0 */
Define_static(mercury__peephole__opt_instr_7_0);
	incr_sp_push_msg(6, "peephole__opt_instr");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	r1 = (Integer) r5;
	{
	Declare_entry(mercury__opt_util__skip_comments_2_0);
	call_localret(ENTRY(mercury__opt_util__skip_comments_2_0),
		mercury__peephole__opt_instr_7_0_i4,
		STATIC(mercury__peephole__opt_instr_7_0));
	}
Define_label(mercury__peephole__opt_instr_7_0_i4);
	update_prof_current_proc(LABEL(mercury__peephole__opt_instr_7_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__peephole__match_6_0),
		mercury__peephole__opt_instr_7_0_i5,
		STATIC(mercury__peephole__opt_instr_7_0));
Define_label(mercury__peephole__opt_instr_7_0_i5);
	update_prof_current_proc(LABEL(mercury__peephole__opt_instr_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__opt_instr_7_0_i3);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__opt_instr_7_0_i7);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r5 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	localcall(mercury__peephole__opt_instr_7_0,
		LABEL(mercury__peephole__opt_instr_7_0_i10),
		STATIC(mercury__peephole__opt_instr_7_0));
	}
Define_label(mercury__peephole__opt_instr_7_0_i10);
	update_prof_current_proc(LABEL(mercury__peephole__opt_instr_7_0));
	r2 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__peephole__opt_instr_7_0_i7);
	r1 = (Integer) r2;
	r2 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__peephole__opt_instr_7_0_i3);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(5);
	r2 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__peephole_module4)
	init_entry(mercury__peephole__match_6_0);
	init_label(mercury__peephole__match_6_0_i1193);
	init_label(mercury__peephole__match_6_0_i1192);
	init_label(mercury__peephole__match_6_0_i1191);
	init_label(mercury__peephole__match_6_0_i1190);
	init_label(mercury__peephole__match_6_0_i1189);
	init_label(mercury__peephole__match_6_0_i1188);
	init_label(mercury__peephole__match_6_0_i1187);
	init_label(mercury__peephole__match_6_0_i1186);
	init_label(mercury__peephole__match_6_0_i1185);
	init_label(mercury__peephole__match_6_0_i9);
	init_label(mercury__peephole__match_6_0_i12);
	init_label(mercury__peephole__match_6_0_i21);
	init_label(mercury__peephole__match_6_0_i13);
	init_label(mercury__peephole__match_6_0_i22);
	init_label(mercury__peephole__match_6_0_i26);
	init_label(mercury__peephole__match_6_0_i28);
	init_label(mercury__peephole__match_6_0_i29);
	init_label(mercury__peephole__match_6_0_i32);
	init_label(mercury__peephole__match_6_0_i35);
	init_label(mercury__peephole__match_6_0_i37);
	init_label(mercury__peephole__match_6_0_i1124);
	init_label(mercury__peephole__match_6_0_i34);
	init_label(mercury__peephole__match_6_0_i39);
	init_label(mercury__peephole__match_6_0_i46);
	init_label(mercury__peephole__match_6_0_i45);
	init_label(mercury__peephole__match_6_0_i42);
	init_label(mercury__peephole__match_6_0_i49);
	init_label(mercury__peephole__match_6_0_i56);
	init_label(mercury__peephole__match_6_0_i59);
	init_label(mercury__peephole__match_6_0_i61);
	init_label(mercury__peephole__match_6_0_i1154);
	init_label(mercury__peephole__match_6_0_i58);
	init_label(mercury__peephole__match_6_0_i64);
	init_label(mercury__peephole__match_6_0_i66);
	init_label(mercury__peephole__match_6_0_i67);
	init_label(mercury__peephole__match_6_0_i69);
	init_label(mercury__peephole__match_6_0_i71);
	init_label(mercury__peephole__match_6_0_i73);
	init_label(mercury__peephole__match_6_0_i74);
	init_label(mercury__peephole__match_6_0_i77);
	init_label(mercury__peephole__match_6_0_i81);
	init_label(mercury__peephole__match_6_0_i1174);
	init_label(mercury__peephole__match_6_0_i80);
	init_label(mercury__peephole__match_6_0_i85);
	init_label(mercury__peephole__match_6_0_i90);
	init_label(mercury__peephole__match_6_0_i1181);
	init_label(mercury__peephole__match_6_0_i86);
	init_label(mercury__peephole__match_6_0_i97);
	init_label(mercury__peephole__match_6_0_i98);
	init_label(mercury__peephole__match_6_0_i102);
	init_label(mercury__peephole__match_6_0_i104);
	init_label(mercury__peephole__match_6_0_i107);
	init_label(mercury__peephole__match_6_0_i106);
	init_label(mercury__peephole__match_6_0_i119);
	init_label(mercury__peephole__match_6_0_i122);
	init_label(mercury__peephole__match_6_0_i123);
	init_label(mercury__peephole__match_6_0_i1);
	init_label(mercury__peephole__match_6_0_i1184);
BEGIN_CODE

/* code for predicate 'peephole__match'/6 in mode 0 */
Define_static(mercury__peephole__match_6_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1184);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__peephole__match_6_0_i1184) AND
		LABEL(mercury__peephole__match_6_0_i1193) AND
		LABEL(mercury__peephole__match_6_0_i1184) AND
		LABEL(mercury__peephole__match_6_0_i1192) AND
		LABEL(mercury__peephole__match_6_0_i1191) AND
		LABEL(mercury__peephole__match_6_0_i1184) AND
		LABEL(mercury__peephole__match_6_0_i1190) AND
		LABEL(mercury__peephole__match_6_0_i1189) AND
		LABEL(mercury__peephole__match_6_0_i1184) AND
		LABEL(mercury__peephole__match_6_0_i1188) AND
		LABEL(mercury__peephole__match_6_0_i1184) AND
		LABEL(mercury__peephole__match_6_0_i1184) AND
		LABEL(mercury__peephole__match_6_0_i1184) AND
		LABEL(mercury__peephole__match_6_0_i1187) AND
		LABEL(mercury__peephole__match_6_0_i1184) AND
		LABEL(mercury__peephole__match_6_0_i1186) AND
		LABEL(mercury__peephole__match_6_0_i1185) AND
		LABEL(mercury__peephole__match_6_0_i1184));
Define_label(mercury__peephole__match_6_0_i1193);
	incr_sp_push_msg(10, "peephole__match");
	detstackvar(10) = (Integer) succip;
	{
	Word tempr1, tempr2, tempr3;
	tempr2 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if ((tag((Integer) tempr2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	tempr3 = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 0));
	if ((tag((Integer) tempr3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) field(mktag(3), (Integer) tempr3, ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	detstackvar(1) = (Integer) r1;
	detstackvar(5) = (Integer) field(mktag(3), (Integer) tempr3, ((Integer) 1));
	r1 = (Integer) r5;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	{
	Declare_entry(mercury__opt_util__skip_comments_livevals_2_0);
	call_localret(ENTRY(mercury__opt_util__skip_comments_livevals_2_0),
		mercury__peephole__match_6_0_i9,
		STATIC(mercury__peephole__match_6_0));
	}
	}
Define_label(mercury__peephole__match_6_0_i1192);
	incr_sp_push_msg(10, "peephole__match");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__peephole__match_6_0_i32);
Define_label(mercury__peephole__match_6_0_i1191);
	incr_sp_push_msg(10, "peephole__match");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__peephole__match_6_0_i56);
Define_label(mercury__peephole__match_6_0_i1190);
	incr_sp_push_msg(10, "peephole__match");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__peephole__match_6_0_i69);
Define_label(mercury__peephole__match_6_0_i1189);
	incr_sp_push_msg(10, "peephole__match");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__peephole__match_6_0_i73);
Define_label(mercury__peephole__match_6_0_i1188);
	incr_sp_push_msg(10, "peephole__match");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__peephole__match_6_0_i77);
Define_label(mercury__peephole__match_6_0_i1187);
	incr_sp_push_msg(10, "peephole__match");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__peephole__match_6_0_i97);
Define_label(mercury__peephole__match_6_0_i1186);
	incr_sp_push_msg(10, "peephole__match");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__peephole__match_6_0_i104);
Define_label(mercury__peephole__match_6_0_i1185);
	incr_sp_push_msg(10, "peephole__match");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__peephole__match_6_0_i122);
Define_label(mercury__peephole__match_6_0_i9);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)) != ((Integer) 16)))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) detstackvar(5) != (Integer) field(mktag(3), (Integer) r2, ((Integer) 1))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	Declare_entry(mercury__opt_util__skip_comments_livevals_2_0);
	call_localret(ENTRY(mercury__opt_util__skip_comments_livevals_2_0),
		mercury__peephole__match_6_0_i12,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i12);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i13);
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i13);
	if (((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury__peephole__match_6_0_i13);
	if ((tag((Integer) field(mktag(3), (Integer) r2, ((Integer) 1))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i13);
	if (((Integer) field(mktag(3), (Integer) field(mktag(3), (Integer) r2, ((Integer) 1)), ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury__peephole__match_6_0_i13);
	if (((Integer) field(mktag(3), (Integer) field(mktag(3), (Integer) r2, ((Integer) 1)), ((Integer) 1)) != ((Integer) 0)))
		GOTO_LABEL(mercury__peephole__match_6_0_i13);
	if ((tag((Integer) field(mktag(3), (Integer) r2, ((Integer) 2))) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__peephole__match_6_0_i13);
	if (((Integer) field(mktag(0), (Integer) field(mktag(3), (Integer) r2, ((Integer) 2)), ((Integer) 0)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i13);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__peephole__opt_instr_list_5_0),
		mercury__peephole__match_6_0_i21,
		STATIC(mercury__peephole__match_6_0));
Define_label(mercury__peephole__match_6_0_i21);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__peephole__match_6_0_i13);
	{
	Declare_entry(mercury__opt_util__no_stack_straight_line_3_0);
	call_localret(ENTRY(mercury__opt_util__no_stack_straight_line_3_0),
		mercury__peephole__match_6_0_i22,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i22);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	{
	Word tempr1, tempr2, tempr3, tempr4;
	tempr2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	tempr1 = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 0));
	if ((tag((Integer) tempr1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) field(mktag(3), (Integer) tempr1, ((Integer) 0)) != ((Integer) 9)))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	tempr4 = (Integer) field(mktag(3), (Integer) tempr1, ((Integer) 2));
	if ((tag((Integer) tempr4) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	detstackvar(6) = (Integer) r1;
	detstackvar(7) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(8) = (Integer) field(mktag(3), (Integer) tempr1, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) tempr4, ((Integer) 0));
	detstackvar(9) = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 1));
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__peephole__match_6_0_i26,
		STATIC(mercury__peephole__match_6_0));
	}
	}
Define_label(mercury__peephole__match_6_0_i26);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r1 = (Integer) detstackvar(9);
	detstackvar(9) = (Integer) r2;
	r2 = string_const(" (bypassed setup)", 17);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__peephole__match_6_0_i28,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i28);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	r2 = (Integer) detstackvar(1);
	r5 = (Integer) r1;
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_peephole__common_0);
	r2 = (Integer) detstackvar(6);
	detstackvar(1) = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	tag_incr_hp(r6, mktag(0), ((Integer) 2));
	tag_incr_hp(r7, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r7, ((Integer) 0)) = ((Integer) 9);
	field(mktag(3), (Integer) r7, ((Integer) 1)) = (Integer) detstackvar(8);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(9);
	field(mktag(0), (Integer) r6, ((Integer) 1)) = (Integer) r5;
	field(mktag(3), (Integer) r7, ((Integer) 2)) = (Integer) tempr1;
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r6;
	field(mktag(0), (Integer) r6, ((Integer) 0)) = (Integer) r7;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__peephole__match_6_0_i29,
		STATIC(mercury__peephole__match_6_0));
	}
	}
Define_label(mercury__peephole__match_6_0_i29);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__peephole__opt_instr_list_5_0),
		mercury__peephole__match_6_0_i21,
		STATIC(mercury__peephole__match_6_0));
Define_label(mercury__peephole__match_6_0_i32);
	detstackvar(1) = (Integer) r1;
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r5;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	detstackvar(3) = (Integer) r5;
	{
	Declare_entry(mercury__opt_util__next_modframe_5_0);
	call_localret(ENTRY(mercury__opt_util__next_modframe_5_0),
		mercury__peephole__match_6_0_i35,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i35);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i34);
	detstackvar(7) = (Integer) r2;
	detstackvar(8) = (Integer) r3;
	detstackvar(9) = (Integer) r4;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__opt_util__touches_nondet_ctrl_2_0);
	call_localret(ENTRY(mercury__opt_util__touches_nondet_ctrl_2_0),
		mercury__peephole__match_6_0_i37,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i37);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if ((((Integer) 1) != (Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i34);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_peephole__common_0);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__peephole__match_6_0_i1124,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i1124);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 4));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) tempr1;
	r1 = TRUE;
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
	}
Define_label(mercury__peephole__match_6_0_i34);
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__opt_util__skip_comments_livevals_2_0);
	call_localret(ENTRY(mercury__opt_util__skip_comments_livevals_2_0),
		mercury__peephole__match_6_0_i39,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i39);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if ((tag((Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0)), ((Integer) 0)) != ((Integer) 9)))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	r3 = (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0)), ((Integer) 2));
	r4 = (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0)), ((Integer) 1));
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) detstackvar(6) != (Integer) mkword(mktag(0), mkbody(((Integer) 2)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i42);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 1)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i46);
	r7 = (Integer) r4;
	r9 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r6 = (Integer) r5;
	GOTO_LABEL(mercury__peephole__match_6_0_i45);
Define_label(mercury__peephole__match_6_0_i46);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 2)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i42);
	r7 = (Integer) r4;
	r9 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r6 = (Integer) r5;
Define_label(mercury__peephole__match_6_0_i45);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r5, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) mkword(mktag(0), mkbody(((Integer) 1)));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r7;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 9);
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) r9;
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r5;
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	tag_incr_hp(r8, mktag(0), ((Integer) 2));
	tag_incr_hp(tempr1, mktag(3), ((Integer) 4));
	field(mktag(0), (Integer) r8, ((Integer) 1)) = (Integer) r1;
	field(mktag(0), (Integer) r8, ((Integer) 0)) = (Integer) tempr1;
	r1 = TRUE;
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) r4;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) mkword(mktag(0), mkbody(((Integer) 2)));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r5;
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) r6;
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) r8;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
	}
Define_label(mercury__peephole__match_6_0_i42);
	if ((tag((Integer) detstackvar(6)) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 2)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i49);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) mkword(mktag(0), mkbody(((Integer) 1)));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r4;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 9);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) r1;
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) tempr1;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) tempr1;
	r1 = TRUE;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r5;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
	}
Define_label(mercury__peephole__match_6_0_i49);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 1)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	tag_incr_hp(r6, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(0), (Integer) r6, ((Integer) 1)) = (Integer) r1;
	field(mktag(0), (Integer) r6, ((Integer) 0)) = (Integer) tempr1;
	r1 = TRUE;
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(6);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r4;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 9);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r5;
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r6;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
	}
Define_label(mercury__peephole__match_6_0_i56);
	r3 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r5;
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) r5;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__opt_util__next_modframe_5_0);
	call_localret(ENTRY(mercury__opt_util__next_modframe_5_0),
		mercury__peephole__match_6_0_i59,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i59);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i58);
	detstackvar(4) = (Integer) r2;
	detstackvar(5) = (Integer) r3;
	detstackvar(6) = (Integer) r4;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__opt_util__touches_nondet_ctrl_2_0);
	call_localret(ENTRY(mercury__opt_util__touches_nondet_ctrl_2_0),
		mercury__peephole__match_6_0_i61,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i61);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if ((((Integer) 1) != (Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i58);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_peephole__common_0);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__peephole__match_6_0_i1154,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i1154);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) tempr1;
	r1 = TRUE;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 4);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
	}
Define_label(mercury__peephole__match_6_0_i58);
	if (((Integer) detstackvar(1) != (Integer) mkword(mktag(0), mkbody(((Integer) 2)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__opt_util__straight_alternative_3_0);
	call_localret(ENTRY(mercury__opt_util__straight_alternative_3_0),
		mercury__peephole__match_6_0_i64,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i64);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__opt_util__touches_nondet_ctrl_2_0);
	call_localret(ENTRY(mercury__opt_util__touches_nondet_ctrl_2_0),
		mercury__peephole__match_6_0_i66,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i66);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if ((((Integer) 1) != (Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_peephole__common_0);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) mkword(mktag(1), (Integer) mercury_data_peephole__common_4);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__condense_2_0);
	call_localret(ENTRY(mercury__list__condense_2_0),
		mercury__peephole__match_6_0_i67,
		STATIC(mercury__peephole__match_6_0));
	}
	}
Define_label(mercury__peephole__match_6_0_i67);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__peephole__match_6_0_i69);
	r3 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r2 = (Integer) r5;
	detstackvar(3) = (Integer) r5;
	{
	Declare_entry(mercury__opt_util__is_this_label_next_3_0);
	call_localret(ENTRY(mercury__opt_util__is_this_label_next_3_0),
		mercury__peephole__match_6_0_i71,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i71);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r1 = TRUE;
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__peephole__match_6_0_i73);
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(1) = (Integer) r2;
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	detstackvar(3) = (Integer) r5;
	{
	Declare_entry(mercury__list__all_same_1_0);
	call_localret(ENTRY(mercury__list__all_same_1_0),
		mercury__peephole__match_6_0_i74,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i74);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) detstackvar(1) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	tag_incr_hp(r3, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 6);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) detstackvar(1), ((Integer) 0));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
	}
Define_label(mercury__peephole__match_6_0_i77);
	r3 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	detstackvar(5) = (Integer) r3;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(1) = (Integer) r1;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r5;
	{
	Declare_entry(mercury__opt_util__is_const_condition_2_0);
	call_localret(ENTRY(mercury__opt_util__is_const_condition_2_0),
		mercury__peephole__match_6_0_i81,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i81);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i80);
	if (((Integer) r2 == ((Integer) 0)))
		GOTO_LABEL(mercury__peephole__match_6_0_i1174);
	r2 = (Integer) detstackvar(3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__peephole__match_6_0_i1174);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	tag_incr_hp(r3, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 6);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__peephole__match_6_0_i80);
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__opt_util__skip_comments_2_0);
	call_localret(ENTRY(mercury__opt_util__skip_comments_2_0),
		mercury__peephole__match_6_0_i85,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i85);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i86);
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i86);
	if (((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)) != ((Integer) 6)))
		GOTO_LABEL(mercury__peephole__match_6_0_i86);
	r3 = (Integer) r2;
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	r1 = (Integer) detstackvar(4);
	detstackvar(4) = (Integer) r2;
	{
	Declare_entry(mercury__opt_util__is_this_label_next_3_0);
	call_localret(ENTRY(mercury__opt_util__is_this_label_next_3_0),
		mercury__peephole__match_6_0_i90,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i90);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__code_util__neg_rval_2_0);
	call_localret(ENTRY(mercury__code_util__neg_rval_2_0),
		mercury__peephole__match_6_0_i1181,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i1181);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) tempr1;
	r1 = TRUE;
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 9);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
	}
Define_label(mercury__peephole__match_6_0_i86);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__opt_util__is_this_label_next_3_0);
	call_localret(ENTRY(mercury__opt_util__is_this_label_next_3_0),
		mercury__peephole__match_6_0_i71,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i97);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r5;
	{
	Declare_entry(mercury__opt_util__skip_comments_2_0);
	call_localret(ENTRY(mercury__opt_util__skip_comments_2_0),
		mercury__peephole__match_6_0_i98,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i98);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if ((tag((Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0)), ((Integer) 0)) != ((Integer) 14)))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if ((tag((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0)), ((Integer) 1))) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r3, ((Integer) 0)), ((Integer) 0)), ((Integer) 1)), ((Integer) 0));
	{
	Declare_entry(mercury____Unify___llds__lval_0_0);
	call_localret(ENTRY(mercury____Unify___llds__lval_0_0),
		mercury__peephole__match_6_0_i102,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i102);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__peephole__match_6_0_i104);
	r2 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) r5;
	detstackvar(3) = (Integer) r5;
	{
	Declare_entry(mercury__opt_util__no_stackvars_til_decr_sp_4_0);
	call_localret(ENTRY(mercury__opt_util__no_stackvars_til_decr_sp_4_0),
		mercury__peephole__match_6_0_i107,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i107);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i106);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_peephole__common_0);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__peephole__match_6_0_i67,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i106);
	r5 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	if (((Integer) r5 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) field(mktag(1), (Integer) r5, ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r1 = (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r5, ((Integer) 1)), ((Integer) 1));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 1)) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if ((tag((Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r5, ((Integer) 0)), ((Integer) 0))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r5, ((Integer) 0)), ((Integer) 0)), ((Integer) 0)) != ((Integer) 6)))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if ((tag((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r5, ((Integer) 0)), ((Integer) 0)), ((Integer) 1))) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r4 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r5, ((Integer) 1)), ((Integer) 0)), ((Integer) 0));
	if ((tag((Integer) r4) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) field(mktag(3), (Integer) r4, ((Integer) 0)) != ((Integer) 5)))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if ((tag((Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0)), ((Integer) 0)) != ((Integer) 15)))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) r3 != (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0)), ((Integer) 1))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if ((strcmp((char *)(Integer) r2, (char *)(Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0)), ((Integer) 2))) !=0))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if ((tag((Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r1, ((Integer) 1)), ((Integer) 0)), ((Integer) 0))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	if (((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r1, ((Integer) 1)), ((Integer) 0)), ((Integer) 0)), ((Integer) 0)) != ((Integer) 5)))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r5, ((Integer) 1));
	r2 = (Integer) r1;
	r1 = (Integer) field(mktag(1), (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r5, ((Integer) 0)), ((Integer) 0)), ((Integer) 1)), ((Integer) 0));
	r2 = (Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r2, ((Integer) 1)), ((Integer) 0)), ((Integer) 0)), ((Integer) 1));
	{
	Declare_entry(mercury____Unify___llds__label_0_0);
	call_localret(ENTRY(mercury____Unify___llds__label_0_0),
		mercury__peephole__match_6_0_i119,
		STATIC(mercury__peephole__match_6_0));
	}
Define_label(mercury__peephole__match_6_0_i119);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i1);
	r2 = (Integer) detstackvar(1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__peephole__match_6_0_i122);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r3;
	r3 = (Integer) r5;
	call_localret(STATIC(mercury__peephole__decr_4_0),
		mercury__peephole__match_6_0_i123,
		STATIC(mercury__peephole__match_6_0));
Define_label(mercury__peephole__match_6_0_i123);
	update_prof_current_proc(LABEL(mercury__peephole__match_6_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__match_6_0_i1184);
	r1 = TRUE;
	proceed();
Define_label(mercury__peephole__match_6_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__peephole__match_6_0_i1184);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__peephole_module5)
	init_entry(mercury__peephole__decr_4_0);
	init_label(mercury__peephole__decr_4_0_i2);
	init_label(mercury__peephole__decr_4_0_i7);
	init_label(mercury__peephole__decr_4_0_i8);
	init_label(mercury__peephole__decr_4_0_i9);
	init_label(mercury__peephole__decr_4_0_i10);
	init_label(mercury__peephole__decr_4_0_i12);
	init_label(mercury__peephole__decr_4_0_i14);
	init_label(mercury__peephole__decr_4_0_i16);
	init_label(mercury__peephole__decr_4_0_i1017);
	init_label(mercury__peephole__decr_4_0_i19);
	init_label(mercury__peephole__decr_4_0_i24);
	init_label(mercury__peephole__decr_4_0_i6);
	init_label(mercury__peephole__decr_4_0_i25);
	init_label(mercury__peephole__decr_4_0_i1);
BEGIN_CODE

/* code for predicate 'peephole__decr'/4 in mode 0 */
Define_static(mercury__peephole__decr_4_0);
	incr_sp_push_msg(6, "peephole__decr");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__opt_util__skip_comments_livevals_2_0);
	call_localret(ENTRY(mercury__opt_util__skip_comments_livevals_2_0),
		mercury__peephole__decr_4_0_i2,
		STATIC(mercury__peephole__decr_4_0));
	}
Define_label(mercury__peephole__decr_4_0_i2);
	update_prof_current_proc(LABEL(mercury__peephole__decr_4_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__peephole__decr_4_0_i1);
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	r3 = tag((Integer) r2);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__peephole__decr_4_0_i6);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)),
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i7) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i12) AND
		LABEL(mercury__peephole__decr_4_0_i19) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i24) AND
		LABEL(mercury__peephole__decr_4_0_i1) AND
		LABEL(mercury__peephole__decr_4_0_i1));
Define_label(mercury__peephole__decr_4_0_i7);
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	{
	Declare_entry(mercury__opt_util__lval_refers_stackvars_2_0);
	call_localret(ENTRY(mercury__opt_util__lval_refers_stackvars_2_0),
		mercury__peephole__decr_4_0_i8,
		STATIC(mercury__peephole__decr_4_0));
	}
Define_label(mercury__peephole__decr_4_0_i8);
	update_prof_current_proc(LABEL(mercury__peephole__decr_4_0));
	if ((((Integer) 1) != (Integer) r1))
		GOTO_LABEL(mercury__peephole__decr_4_0_i1);
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__opt_util__rval_refers_stackvars_2_0);
	call_localret(ENTRY(mercury__opt_util__rval_refers_stackvars_2_0),
		mercury__peephole__decr_4_0_i9,
		STATIC(mercury__peephole__decr_4_0));
	}
Define_label(mercury__peephole__decr_4_0_i9);
	update_prof_current_proc(LABEL(mercury__peephole__decr_4_0));
	if ((((Integer) 1) != (Integer) r1))
		GOTO_LABEL(mercury__peephole__decr_4_0_i1);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(4);
	localcall(mercury__peephole__decr_4_0,
		LABEL(mercury__peephole__decr_4_0_i10),
		STATIC(mercury__peephole__decr_4_0));
Define_label(mercury__peephole__decr_4_0_i10);
	update_prof_current_proc(LABEL(mercury__peephole__decr_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__decr_4_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__peephole__decr_4_0_i12);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 2));
	if ((tag((Integer) tempr1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__peephole__decr_4_0_i1);
	detstackvar(3) = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r2 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__bimap__search_3_1);
	call_localret(ENTRY(mercury__bimap__search_3_1),
		mercury__peephole__decr_4_0_i14,
		STATIC(mercury__peephole__decr_4_0));
	}
	}
Define_label(mercury__peephole__decr_4_0_i14);
	update_prof_current_proc(LABEL(mercury__peephole__decr_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__decr_4_0_i1);
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(4);
	localcall(mercury__peephole__decr_4_0,
		LABEL(mercury__peephole__decr_4_0_i16),
		STATIC(mercury__peephole__decr_4_0));
Define_label(mercury__peephole__decr_4_0_i16);
	update_prof_current_proc(LABEL(mercury__peephole__decr_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__peephole__decr_4_0_i1);
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) detstackvar(3);
	r2 = string_const(" (original)", 11);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__peephole__decr_4_0_i1017,
		STATIC(mercury__peephole__decr_4_0));
	}
Define_label(mercury__peephole__decr_4_0_i1017);
	update_prof_current_proc(LABEL(mercury__peephole__decr_4_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	tag_incr_hp(r4, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r4, ((Integer) 0)) = ((Integer) 9);
	field(mktag(3), (Integer) r4, ((Integer) 1)) = (Integer) detstackvar(5);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(3), (Integer) r4, ((Integer) 2)) = (Integer) tempr1;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
	}
Define_label(mercury__peephole__decr_4_0_i19);
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 3));
	r1 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	{
	Declare_entry(mercury__opt_util__lval_refers_stackvars_2_0);
	call_localret(ENTRY(mercury__opt_util__lval_refers_stackvars_2_0),
		mercury__peephole__decr_4_0_i8,
		STATIC(mercury__peephole__decr_4_0));
	}
Define_label(mercury__peephole__decr_4_0_i24);
	if (((Integer) detstackvar(1) != (Integer) field(mktag(3), (Integer) r2, ((Integer) 1))))
		GOTO_LABEL(mercury__peephole__decr_4_0_i1);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__peephole__decr_4_0_i6);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__peephole__decr_4_0_i25);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	localcall(mercury__peephole__decr_4_0,
		LABEL(mercury__peephole__decr_4_0_i10),
		STATIC(mercury__peephole__decr_4_0));
Define_label(mercury__peephole__decr_4_0_i25);
	if (((Integer) r3 != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__peephole__decr_4_0_i1);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	localcall(mercury__peephole__decr_4_0,
		LABEL(mercury__peephole__decr_4_0_i10),
		STATIC(mercury__peephole__decr_4_0));
Define_label(mercury__peephole__decr_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__peephole_bunch_0(void)
{
	mercury__peephole_module0();
	mercury__peephole_module1();
	mercury__peephole_module2();
	mercury__peephole_module3();
	mercury__peephole_module4();
	mercury__peephole_module5();
}

#endif

void mercury__peephole__init(void); /* suppress gcc warning */
void mercury__peephole__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__peephole_bunch_0();
#endif
}
