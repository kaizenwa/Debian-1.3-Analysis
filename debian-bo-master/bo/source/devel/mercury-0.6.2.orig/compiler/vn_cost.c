/*
** Automatically generated from `vn_cost.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__vn_cost__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__vn_cost__block_cost_6_0);
Define_extern_entry(mercury__vn_cost__lval_cost_3_0);
Declare_label(mercury__vn_cost__lval_cost_3_0_i1007);
Declare_label(mercury__vn_cost__lval_cost_3_0_i1006);
Declare_label(mercury__vn_cost__lval_cost_3_0_i1003);
Declare_label(mercury__vn_cost__lval_cost_3_0_i9);
Declare_label(mercury__vn_cost__lval_cost_3_0_i10);
Declare_label(mercury__vn_cost__lval_cost_3_0_i11);
Declare_label(mercury__vn_cost__lval_cost_3_0_i12);
Declare_label(mercury__vn_cost__lval_cost_3_0_i13);
Declare_label(mercury__vn_cost__lval_cost_3_0_i14);
Declare_label(mercury__vn_cost__lval_cost_3_0_i21);
Declare_label(mercury__vn_cost__lval_cost_3_0_i22);
Declare_label(mercury__vn_cost__lval_cost_3_0_i23);
Declare_label(mercury__vn_cost__lval_cost_3_0_i24);
Declare_label(mercury__vn_cost__lval_cost_3_0_i1002);
Declare_label(mercury__vn_cost__lval_cost_3_0_i27);
Declare_label(mercury__vn_cost__lval_cost_3_0_i36);
Declare_label(mercury__vn_cost__lval_cost_3_0_i37);
Declare_label(mercury__vn_cost__lval_cost_3_0_i35);
Declare_label(mercury__vn_cost__lval_cost_3_0_i33);
Declare_label(mercury__vn_cost__lval_cost_3_0_i47);
Declare_label(mercury__vn_cost__lval_cost_3_0_i53);
Declare_label(mercury__vn_cost__lval_cost_3_0_i54);
Declare_label(mercury__vn_cost__lval_cost_3_0_i1000);
Declare_label(mercury__vn_cost__lval_cost_3_0_i1001);
Define_extern_entry(mercury__vn_cost__rval_cost_3_0);
Declare_label(mercury__vn_cost__rval_cost_3_0_i6);
Declare_label(mercury__vn_cost__rval_cost_3_0_i7);
Declare_label(mercury__vn_cost__rval_cost_3_0_i5);
Declare_label(mercury__vn_cost__rval_cost_3_0_i8);
Declare_label(mercury__vn_cost__rval_cost_3_0_i9);
Declare_label(mercury__vn_cost__rval_cost_3_0_i12);
Declare_label(mercury__vn_cost__rval_cost_3_0_i13);
Declare_label(mercury__vn_cost__rval_cost_3_0_i14);
Declare_label(mercury__vn_cost__rval_cost_3_0_i1001);
Declare_label(mercury__vn_cost__rval_cost_3_0_i15);
Declare_label(mercury__vn_cost__rval_cost_3_0_i1000);
Declare_static(mercury__vn_cost__block_cost_2_7_0);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i1020);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i1019);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i1018);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i1013);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i1012);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i1010);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i1009);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i1003);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i7);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i8);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i9);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i10);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i11);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i12);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i13);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i17);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i22);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i27);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i29);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i33);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i34);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i35);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i36);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i39);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i38);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i40);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i50);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i1002);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i52);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i53);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i4);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i54);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i57);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i60);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i58);
Declare_label(mercury__vn_cost__block_cost_2_7_0_i1001);

BEGIN_MODULE(mercury__vn_cost_module0)
	init_entry(mercury__vn_cost__block_cost_6_0);
BEGIN_CODE

/* code for predicate 'vn_cost__block_cost'/6 in mode 0 */
Define_entry(mercury__vn_cost__block_cost_6_0);
	r5 = (Integer) r4;
	r4 = ((Integer) 0);
	tailcall(STATIC(mercury__vn_cost__block_cost_2_7_0),
		ENTRY(mercury__vn_cost__block_cost_6_0));
END_MODULE

BEGIN_MODULE(mercury__vn_cost_module1)
	init_entry(mercury__vn_cost__lval_cost_3_0);
	init_label(mercury__vn_cost__lval_cost_3_0_i1007);
	init_label(mercury__vn_cost__lval_cost_3_0_i1006);
	init_label(mercury__vn_cost__lval_cost_3_0_i1003);
	init_label(mercury__vn_cost__lval_cost_3_0_i9);
	init_label(mercury__vn_cost__lval_cost_3_0_i10);
	init_label(mercury__vn_cost__lval_cost_3_0_i11);
	init_label(mercury__vn_cost__lval_cost_3_0_i12);
	init_label(mercury__vn_cost__lval_cost_3_0_i13);
	init_label(mercury__vn_cost__lval_cost_3_0_i14);
	init_label(mercury__vn_cost__lval_cost_3_0_i21);
	init_label(mercury__vn_cost__lval_cost_3_0_i22);
	init_label(mercury__vn_cost__lval_cost_3_0_i23);
	init_label(mercury__vn_cost__lval_cost_3_0_i24);
	init_label(mercury__vn_cost__lval_cost_3_0_i1002);
	init_label(mercury__vn_cost__lval_cost_3_0_i27);
	init_label(mercury__vn_cost__lval_cost_3_0_i36);
	init_label(mercury__vn_cost__lval_cost_3_0_i37);
	init_label(mercury__vn_cost__lval_cost_3_0_i35);
	init_label(mercury__vn_cost__lval_cost_3_0_i33);
	init_label(mercury__vn_cost__lval_cost_3_0_i47);
	init_label(mercury__vn_cost__lval_cost_3_0_i53);
	init_label(mercury__vn_cost__lval_cost_3_0_i54);
	init_label(mercury__vn_cost__lval_cost_3_0_i1000);
	init_label(mercury__vn_cost__lval_cost_3_0_i1001);
BEGIN_CODE

/* code for predicate 'vn_cost__lval_cost'/3 in mode 0 */
Define_entry(mercury__vn_cost__lval_cost_3_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_cost__lval_cost_3_0_i1002);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__vn_cost__lval_cost_3_0_i1000) AND
		LABEL(mercury__vn_cost__lval_cost_3_0_i1000) AND
		LABEL(mercury__vn_cost__lval_cost_3_0_i1007) AND
		LABEL(mercury__vn_cost__lval_cost_3_0_i1006) AND
		LABEL(mercury__vn_cost__lval_cost_3_0_i1006) AND
		LABEL(mercury__vn_cost__lval_cost_3_0_i1006) AND
		LABEL(mercury__vn_cost__lval_cost_3_0_i1003) AND
		LABEL(mercury__vn_cost__lval_cost_3_0_i1001));
Define_label(mercury__vn_cost__lval_cost_3_0_i1007);
	incr_sp_push_msg(3, "vn_cost__lval_cost");
	detstackvar(3) = (Integer) succip;
	GOTO_LABEL(mercury__vn_cost__lval_cost_3_0_i9);
Define_label(mercury__vn_cost__lval_cost_3_0_i1006);
	incr_sp_push_msg(3, "vn_cost__lval_cost");
	detstackvar(3) = (Integer) succip;
	GOTO_LABEL(mercury__vn_cost__lval_cost_3_0_i12);
Define_label(mercury__vn_cost__lval_cost_3_0_i1003);
	incr_sp_push_msg(3, "vn_cost__lval_cost");
	detstackvar(3) = (Integer) succip;
	GOTO_LABEL(mercury__vn_cost__lval_cost_3_0_i21);
Define_label(mercury__vn_cost__lval_cost_3_0_i9);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	{
		call_localret(STATIC(mercury__vn_cost__rval_cost_3_0),
		mercury__vn_cost__lval_cost_3_0_i10,
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i10);
	update_prof_current_proc(LABEL(mercury__vn_cost__lval_cost_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_type__costof_stackref_2_0);
	call_localret(ENTRY(mercury__vn_type__costof_stackref_2_0),
		mercury__vn_cost__lval_cost_3_0_i11,
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i11);
	update_prof_current_proc(LABEL(mercury__vn_cost__lval_cost_3_0));
	r1 = ((Integer) detstackvar(1) + (Integer) r1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_cost__lval_cost_3_0_i12);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_type__costof_stackref_2_0);
	call_localret(ENTRY(mercury__vn_type__costof_stackref_2_0),
		mercury__vn_cost__lval_cost_3_0_i13,
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i13);
	update_prof_current_proc(LABEL(mercury__vn_cost__lval_cost_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__vn_cost__rval_cost_3_0),
		mercury__vn_cost__lval_cost_3_0_i14,
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i14);
	update_prof_current_proc(LABEL(mercury__vn_cost__lval_cost_3_0));
	r1 = ((Integer) r1 + (Integer) detstackvar(1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_cost__lval_cost_3_0_i21);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	{
		call_localret(STATIC(mercury__vn_cost__rval_cost_3_0),
		mercury__vn_cost__lval_cost_3_0_i22,
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i22);
	update_prof_current_proc(LABEL(mercury__vn_cost__lval_cost_3_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__vn_cost__rval_cost_3_0),
		mercury__vn_cost__lval_cost_3_0_i23,
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i23);
	update_prof_current_proc(LABEL(mercury__vn_cost__lval_cost_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_type__costof_heapref_2_0);
	call_localret(ENTRY(mercury__vn_type__costof_heapref_2_0),
		mercury__vn_cost__lval_cost_3_0_i24,
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i24);
	update_prof_current_proc(LABEL(mercury__vn_cost__lval_cost_3_0));
	r1 = (((Integer) detstackvar(2) + (Integer) detstackvar(1)) + (Integer) r1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_cost__lval_cost_3_0_i1002);
	incr_sp_push_msg(3, "vn_cost__lval_cost");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_cost__lval_cost_3_0_i27);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_cost__lval_cost_3_0_i27);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_cost__lval_cost_3_0_i33);
	if ((tag((Integer) field(mktag(1), (Integer) r1, ((Integer) 0))) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_cost__lval_cost_3_0_i35);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_type__real_f_regs_2_0);
	call_localret(ENTRY(mercury__vn_type__real_f_regs_2_0),
		mercury__vn_cost__lval_cost_3_0_i36,
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i36);
	update_prof_current_proc(LABEL(mercury__vn_cost__lval_cost_3_0));
	if (((Integer) detstackvar(2) > (Integer) r1))
		GOTO_LABEL(mercury__vn_cost__lval_cost_3_0_i37);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_cost__lval_cost_3_0_i37);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__vn_type__costof_stackref_2_0);
	tailcall(ENTRY(mercury__vn_type__costof_stackref_2_0),
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i35);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_type__real_r_regs_2_0);
	call_localret(ENTRY(mercury__vn_type__real_r_regs_2_0),
		mercury__vn_cost__lval_cost_3_0_i36,
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i33);
	if ((tag((Integer) field(mktag(2), (Integer) r1, ((Integer) 0))) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_cost__lval_cost_3_0_i47);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) field(mktag(2), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_type__real_f_temps_2_0);
	call_localret(ENTRY(mercury__vn_type__real_f_temps_2_0),
		mercury__vn_cost__lval_cost_3_0_i36,
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i47);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) field(mktag(2), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_type__real_r_temps_2_0);
	call_localret(ENTRY(mercury__vn_type__real_r_temps_2_0),
		mercury__vn_cost__lval_cost_3_0_i53,
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i53);
	update_prof_current_proc(LABEL(mercury__vn_cost__lval_cost_3_0));
	if (((Integer) detstackvar(2) > (Integer) r1))
		GOTO_LABEL(mercury__vn_cost__lval_cost_3_0_i54);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_cost__lval_cost_3_0_i54);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__vn_type__costof_stackref_2_0);
	tailcall(ENTRY(mercury__vn_type__costof_stackref_2_0),
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i1000);
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_type__costof_stackref_2_0);
	tailcall(ENTRY(mercury__vn_type__costof_stackref_2_0),
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
Define_label(mercury__vn_cost__lval_cost_3_0_i1001);
	r1 = string_const("lvar found in lval_cost", 23);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__vn_cost__lval_cost_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__vn_cost_module2)
	init_entry(mercury__vn_cost__rval_cost_3_0);
	init_label(mercury__vn_cost__rval_cost_3_0_i6);
	init_label(mercury__vn_cost__rval_cost_3_0_i7);
	init_label(mercury__vn_cost__rval_cost_3_0_i5);
	init_label(mercury__vn_cost__rval_cost_3_0_i8);
	init_label(mercury__vn_cost__rval_cost_3_0_i9);
	init_label(mercury__vn_cost__rval_cost_3_0_i12);
	init_label(mercury__vn_cost__rval_cost_3_0_i13);
	init_label(mercury__vn_cost__rval_cost_3_0_i14);
	init_label(mercury__vn_cost__rval_cost_3_0_i1001);
	init_label(mercury__vn_cost__rval_cost_3_0_i15);
	init_label(mercury__vn_cost__rval_cost_3_0_i1000);
BEGIN_CODE

/* code for predicate 'vn_cost__rval_cost'/3 in mode 0 */
Define_entry(mercury__vn_cost__rval_cost_3_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_cost__rval_cost_3_0_i1001);
	r3 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 0));
	incr_sp_push_msg(3, "vn_cost__rval_cost");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__vn_cost__rval_cost_3_0_i5);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localcall(mercury__vn_cost__rval_cost_3_0,
		LABEL(mercury__vn_cost__rval_cost_3_0_i6),
		ENTRY(mercury__vn_cost__rval_cost_3_0));
Define_label(mercury__vn_cost__rval_cost_3_0_i6);
	update_prof_current_proc(LABEL(mercury__vn_cost__rval_cost_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_type__costof_intops_2_0);
	call_localret(ENTRY(mercury__vn_type__costof_intops_2_0),
		mercury__vn_cost__rval_cost_3_0_i7,
		ENTRY(mercury__vn_cost__rval_cost_3_0));
	}
Define_label(mercury__vn_cost__rval_cost_3_0_i7);
	update_prof_current_proc(LABEL(mercury__vn_cost__rval_cost_3_0));
	r1 = ((Integer) detstackvar(1) + (Integer) r1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_cost__rval_cost_3_0_i5);
	if (((Integer) r3 != ((Integer) 1)))
		GOTO_LABEL(mercury__vn_cost__rval_cost_3_0_i8);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_cost__rval_cost_3_0_i8);
	if (((Integer) r3 != ((Integer) 2)))
		GOTO_LABEL(mercury__vn_cost__rval_cost_3_0_i9);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localcall(mercury__vn_cost__rval_cost_3_0,
		LABEL(mercury__vn_cost__rval_cost_3_0_i6),
		ENTRY(mercury__vn_cost__rval_cost_3_0));
Define_label(mercury__vn_cost__rval_cost_3_0_i9);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	localcall(mercury__vn_cost__rval_cost_3_0,
		LABEL(mercury__vn_cost__rval_cost_3_0_i12),
		ENTRY(mercury__vn_cost__rval_cost_3_0));
Define_label(mercury__vn_cost__rval_cost_3_0_i12);
	update_prof_current_proc(LABEL(mercury__vn_cost__rval_cost_3_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	localcall(mercury__vn_cost__rval_cost_3_0,
		LABEL(mercury__vn_cost__rval_cost_3_0_i13),
		ENTRY(mercury__vn_cost__rval_cost_3_0));
Define_label(mercury__vn_cost__rval_cost_3_0_i13);
	update_prof_current_proc(LABEL(mercury__vn_cost__rval_cost_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_type__costof_intops_2_0);
	call_localret(ENTRY(mercury__vn_type__costof_intops_2_0),
		mercury__vn_cost__rval_cost_3_0_i14,
		ENTRY(mercury__vn_cost__rval_cost_3_0));
	}
Define_label(mercury__vn_cost__rval_cost_3_0_i14);
	update_prof_current_proc(LABEL(mercury__vn_cost__rval_cost_3_0));
	r1 = (((Integer) detstackvar(2) + (Integer) detstackvar(1)) + (Integer) r1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__vn_cost__rval_cost_3_0_i1001);
	incr_sp_push_msg(3, "vn_cost__rval_cost");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_cost__rval_cost_3_0_i15);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__vn_cost__lval_cost_3_0),
		ENTRY(mercury__vn_cost__rval_cost_3_0));
	}
Define_label(mercury__vn_cost__rval_cost_3_0_i15);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_cost__rval_cost_3_0_i1000);
	r1 = string_const("var found in rval_cost", 22);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__vn_cost__rval_cost_3_0));
	}
Define_label(mercury__vn_cost__rval_cost_3_0_i1000);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__vn_cost_module3)
	init_entry(mercury__vn_cost__block_cost_2_7_0);
	init_label(mercury__vn_cost__block_cost_2_7_0_i1020);
	init_label(mercury__vn_cost__block_cost_2_7_0_i1019);
	init_label(mercury__vn_cost__block_cost_2_7_0_i1018);
	init_label(mercury__vn_cost__block_cost_2_7_0_i1013);
	init_label(mercury__vn_cost__block_cost_2_7_0_i1012);
	init_label(mercury__vn_cost__block_cost_2_7_0_i1010);
	init_label(mercury__vn_cost__block_cost_2_7_0_i1009);
	init_label(mercury__vn_cost__block_cost_2_7_0_i1003);
	init_label(mercury__vn_cost__block_cost_2_7_0_i7);
	init_label(mercury__vn_cost__block_cost_2_7_0_i8);
	init_label(mercury__vn_cost__block_cost_2_7_0_i9);
	init_label(mercury__vn_cost__block_cost_2_7_0_i10);
	init_label(mercury__vn_cost__block_cost_2_7_0_i11);
	init_label(mercury__vn_cost__block_cost_2_7_0_i12);
	init_label(mercury__vn_cost__block_cost_2_7_0_i13);
	init_label(mercury__vn_cost__block_cost_2_7_0_i17);
	init_label(mercury__vn_cost__block_cost_2_7_0_i22);
	init_label(mercury__vn_cost__block_cost_2_7_0_i27);
	init_label(mercury__vn_cost__block_cost_2_7_0_i29);
	init_label(mercury__vn_cost__block_cost_2_7_0_i33);
	init_label(mercury__vn_cost__block_cost_2_7_0_i34);
	init_label(mercury__vn_cost__block_cost_2_7_0_i35);
	init_label(mercury__vn_cost__block_cost_2_7_0_i36);
	init_label(mercury__vn_cost__block_cost_2_7_0_i39);
	init_label(mercury__vn_cost__block_cost_2_7_0_i38);
	init_label(mercury__vn_cost__block_cost_2_7_0_i40);
	init_label(mercury__vn_cost__block_cost_2_7_0_i50);
	init_label(mercury__vn_cost__block_cost_2_7_0_i1002);
	init_label(mercury__vn_cost__block_cost_2_7_0_i52);
	init_label(mercury__vn_cost__block_cost_2_7_0_i53);
	init_label(mercury__vn_cost__block_cost_2_7_0_i4);
	init_label(mercury__vn_cost__block_cost_2_7_0_i54);
	init_label(mercury__vn_cost__block_cost_2_7_0_i57);
	init_label(mercury__vn_cost__block_cost_2_7_0_i60);
	init_label(mercury__vn_cost__block_cost_2_7_0_i58);
	init_label(mercury__vn_cost__block_cost_2_7_0_i1001);
BEGIN_CODE

/* code for predicate 'vn_cost__block_cost_2'/7 in mode 0 */
Define_static(mercury__vn_cost__block_cost_2_7_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i1001);
	r6 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	if ((tag((Integer) r6) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i1002);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r6, ((Integer) 0)),
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1020) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1019) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1018) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1018) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1018) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1018) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1018) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1013) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1012) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1013) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1010) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1009) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1013) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1009) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1013) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1018) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1018) AND
		LABEL(mercury__vn_cost__block_cost_2_7_0_i1003));
Define_label(mercury__vn_cost__block_cost_2_7_0_i1020);
	incr_sp_push_msg(11, "vn_cost__block_cost_2");
	detstackvar(11) = (Integer) succip;
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i7);
Define_label(mercury__vn_cost__block_cost_2_7_0_i1019);
	incr_sp_push_msg(11, "vn_cost__block_cost_2");
	detstackvar(11) = (Integer) succip;
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i9);
Define_label(mercury__vn_cost__block_cost_2_7_0_i1018);
	incr_sp_push_msg(11, "vn_cost__block_cost_2");
	detstackvar(11) = (Integer) succip;
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i22);
Define_label(mercury__vn_cost__block_cost_2_7_0_i1013);
	incr_sp_push_msg(11, "vn_cost__block_cost_2");
	detstackvar(11) = (Integer) succip;
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i27);
Define_label(mercury__vn_cost__block_cost_2_7_0_i1012);
	incr_sp_push_msg(11, "vn_cost__block_cost_2");
	detstackvar(11) = (Integer) succip;
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i29);
Define_label(mercury__vn_cost__block_cost_2_7_0_i1010);
	incr_sp_push_msg(11, "vn_cost__block_cost_2");
	detstackvar(11) = (Integer) succip;
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i33);
Define_label(mercury__vn_cost__block_cost_2_7_0_i1009);
	incr_sp_push_msg(11, "vn_cost__block_cost_2");
	detstackvar(11) = (Integer) succip;
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i40);
Define_label(mercury__vn_cost__block_cost_2_7_0_i1003);
	incr_sp_push_msg(11, "vn_cost__block_cost_2");
	detstackvar(11) = (Integer) succip;
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i50);
Define_label(mercury__vn_cost__block_cost_2_7_0_i7);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(6) = (Integer) r6;
	r1 = string_const("block found in vn_block_cost", 28);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__vn_cost__block_cost_2_7_0_i8,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i8);
	update_prof_current_proc(LABEL(mercury__vn_cost__block_cost_2_7_0));
	r7 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i4);
Define_label(mercury__vn_cost__block_cost_2_7_0_i9);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(6) = (Integer) r6;
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	{
		call_localret(STATIC(mercury__vn_cost__lval_cost_3_0),
		mercury__vn_cost__block_cost_2_7_0_i10,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i10);
	update_prof_current_proc(LABEL(mercury__vn_cost__block_cost_2_7_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__vn_cost__rval_cost_3_0),
		mercury__vn_cost__block_cost_2_7_0_i11,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i11);
	update_prof_current_proc(LABEL(mercury__vn_cost__block_cost_2_7_0));
	detstackvar(9) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__vn_type__costof_assign_2_0);
	call_localret(ENTRY(mercury__vn_type__costof_assign_2_0),
		mercury__vn_cost__block_cost_2_7_0_i12,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i12);
	update_prof_current_proc(LABEL(mercury__vn_cost__block_cost_2_7_0));
	if (((Integer) detstackvar(8) != ((Integer) 0)))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i13);
	if ((tag((Integer) detstackvar(7)) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i13);
	if (((Integer) detstackvar(9) <= ((Integer) 0)))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i13);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r1 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(6);
	r7 = (Integer) detstackvar(9);
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i4);
Define_label(mercury__vn_cost__block_cost_2_7_0_i13);
	if ((tag((Integer) detstackvar(7)) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i17);
	if (((Integer) field(mktag(3), (Integer) detstackvar(7), ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i17);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r1 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(6);
	r7 = ((Integer) detstackvar(9) + (Integer) detstackvar(8));
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i4);
Define_label(mercury__vn_cost__block_cost_2_7_0_i17);
	r7 = (((Integer) detstackvar(9) + (Integer) detstackvar(8)) + (Integer) r1);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i4);
Define_label(mercury__vn_cost__block_cost_2_7_0_i22);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r7 = ((Integer) 0);
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i4);
Define_label(mercury__vn_cost__block_cost_2_7_0_i27);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(6) = (Integer) r6;
	r1 = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	{
		call_localret(STATIC(mercury__vn_cost__rval_cost_3_0),
		mercury__vn_cost__block_cost_2_7_0_i8,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i29);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(6) = (Integer) r6;
	r1 = string_const("c_code found in vn_block_cost", 29);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__vn_cost__block_cost_2_7_0_i8,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i33);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(6) = (Integer) r6;
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 2));
	detstackvar(8) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	detstackvar(9) = (Integer) field(mktag(3), (Integer) r6, ((Integer) 3));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__vn_type__costof_assign_2_0);
	call_localret(ENTRY(mercury__vn_type__costof_assign_2_0),
		mercury__vn_cost__block_cost_2_7_0_i34,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i34);
	update_prof_current_proc(LABEL(mercury__vn_cost__block_cost_2_7_0));
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__vn_cost__lval_cost_3_0),
		mercury__vn_cost__block_cost_2_7_0_i35,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i35);
	update_prof_current_proc(LABEL(mercury__vn_cost__block_cost_2_7_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__vn_cost__rval_cost_3_0),
		mercury__vn_cost__block_cost_2_7_0_i36,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i36);
	update_prof_current_proc(LABEL(mercury__vn_cost__block_cost_2_7_0));
	if (((Integer) detstackvar(7) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i38);
	detstackvar(7) = (((Integer) r1 + (Integer) detstackvar(8)) + (((Integer) 3) * (Integer) detstackvar(10)));
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__vn_type__costof_intops_2_0);
	call_localret(ENTRY(mercury__vn_type__costof_intops_2_0),
		mercury__vn_cost__block_cost_2_7_0_i39,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i39);
	update_prof_current_proc(LABEL(mercury__vn_cost__block_cost_2_7_0));
	r7 = ((Integer) detstackvar(7) + (Integer) r1);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i4);
Define_label(mercury__vn_cost__block_cost_2_7_0_i38);
	r7 = ((((Integer) r1 + (Integer) detstackvar(8)) + (((Integer) 3) * (Integer) detstackvar(10))) + ((Integer) 0));
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i4);
Define_label(mercury__vn_cost__block_cost_2_7_0_i40);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(6) = (Integer) r6;
	r1 = (Integer) field(mktag(3), (Integer) r6, ((Integer) 1));
	{
		call_localret(STATIC(mercury__vn_cost__lval_cost_3_0),
		mercury__vn_cost__block_cost_2_7_0_i8,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i50);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(6) = (Integer) r6;
	r1 = string_const("pragma_c found in vn_block_cost", 31);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__vn_cost__block_cost_2_7_0_i8,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i1002);
	incr_sp_push_msg(11, "vn_cost__block_cost_2");
	detstackvar(11) = (Integer) succip;
	if ((tag((Integer) r6) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i52);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r7 = ((Integer) 0);
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i4);
Define_label(mercury__vn_cost__block_cost_2_7_0_i52);
	if ((tag((Integer) r6) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i53);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r7 = ((Integer) 0);
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i4);
Define_label(mercury__vn_cost__block_cost_2_7_0_i53);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r7 = ((Integer) 0);
Define_label(mercury__vn_cost__block_cost_2_7_0_i4);
	if ((tag((Integer) r6) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i54);
	if (((Integer) field(mktag(3), (Integer) r6, ((Integer) 0)) != ((Integer) 9)))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i54);
	r4 = (((Integer) 2) * ((Integer) r4 + (Integer) r7));
	GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i57);
Define_label(mercury__vn_cost__block_cost_2_7_0_i54);
	r4 = ((Integer) r4 + (Integer) r7);
Define_label(mercury__vn_cost__block_cost_2_7_0_i57);
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__vn_cost__block_cost_2_7_0_i58);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(5) = (Integer) r1;
	detstackvar(3) = (Integer) r4;
	r1 = (Integer) r6;
	r2 = (Integer) r7;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	{
	Declare_entry(mercury__vn_debug__cost_detail_msg_5_0);
	call_localret(ENTRY(mercury__vn_debug__cost_detail_msg_5_0),
		mercury__vn_cost__block_cost_2_7_0_i60,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
	}
Define_label(mercury__vn_cost__block_cost_2_7_0_i60);
	update_prof_current_proc(LABEL(mercury__vn_cost__block_cost_2_7_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
Define_label(mercury__vn_cost__block_cost_2_7_0_i58);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	localtailcall(mercury__vn_cost__block_cost_2_7_0,
		STATIC(mercury__vn_cost__block_cost_2_7_0));
Define_label(mercury__vn_cost__block_cost_2_7_0_i1001);
	r1 = (Integer) r4;
	r2 = (Integer) r5;
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__vn_cost_bunch_0(void)
{
	mercury__vn_cost_module0();
	mercury__vn_cost_module1();
	mercury__vn_cost_module2();
	mercury__vn_cost_module3();
}

#endif

void mercury__vn_cost__init(void); /* suppress gcc warning */
void mercury__vn_cost__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__vn_cost_bunch_0();
#endif
}
