/*
** Automatically generated from `tree.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__tree__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Index___tree_tree_1__ua10000_2_0);
Declare_label(mercury____Index___tree_tree_1__ua10000_2_0_i4);
Declare_label(mercury____Index___tree_tree_1__ua10000_2_0_i5);
Declare_static(mercury__tree__flatten_2__ua10000_3_0);
Declare_label(mercury__tree__flatten_2__ua10000_3_0_i1000);
Declare_label(mercury__tree__flatten_2__ua10000_3_0_i5);
Declare_label(mercury__tree__flatten_2__ua10000_3_0_i6);
Declare_static(mercury__tree__flatten__ua10000_2_0);
Define_extern_entry(mercury__tree__flatten_2_0);
Define_extern_entry(mercury____Unify___tree__tree_1_0);
Declare_label(mercury____Unify___tree__tree_1_0_i1011);
Declare_label(mercury____Unify___tree__tree_1_0_i6);
Declare_label(mercury____Unify___tree__tree_1_0_i11);
Declare_label(mercury____Unify___tree__tree_1_0_i1008);
Declare_label(mercury____Unify___tree__tree_1_0_i1);
Declare_label(mercury____Unify___tree__tree_1_0_i1010);
Define_extern_entry(mercury____Index___tree__tree_1_0);
Define_extern_entry(mercury____Compare___tree__tree_1_0);
Declare_label(mercury____Compare___tree__tree_1_0_i2);
Declare_label(mercury____Compare___tree__tree_1_0_i3);
Declare_label(mercury____Compare___tree__tree_1_0_i4);
Declare_label(mercury____Compare___tree__tree_1_0_i6);
Declare_label(mercury____Compare___tree__tree_1_0_i12);
Declare_label(mercury____Compare___tree__tree_1_0_i14);
Declare_label(mercury____Compare___tree__tree_1_0_i20);
Declare_label(mercury____Compare___tree__tree_1_0_i19);
Declare_label(mercury____Compare___tree__tree_1_0_i9);

extern Word * mercury_data_tree__base_type_layout_tree_1[];
Word * mercury_data_tree__base_type_info_tree_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___tree__tree_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___tree__tree_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___tree__tree_1_0),
	(Word *) (Integer) mercury_data_tree__base_type_layout_tree_1
};

extern Word * mercury_data_tree__common_0[];
extern Word * mercury_data_tree__common_1[];
extern Word * mercury_data_tree__common_3[];
Word * mercury_data_tree__base_type_layout_tree_1[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_tree__common_0),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_tree__common_1),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_tree__common_3),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

Word * mercury_data_tree__common_0[] = {
	(Word *) ((Integer) 0),
	(Word *) ((Integer) 1),
	(Word *) string_const("empty", 5)
};

Word * mercury_data_tree__common_1[] = {
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 1),
	(Word *) string_const("node", 4)
};

Word * mercury_data_tree__common_2[] = {
	(Word *) (Integer) mercury_data_tree__base_type_info_tree_1,
	(Word *) ((Integer) 1)
};

Word * mercury_data_tree__common_3[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_tree__common_2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_tree__common_2),
	(Word *) string_const("tree", 4)
};

BEGIN_MODULE(mercury__tree_module0)
	init_entry(mercury____Index___tree_tree_1__ua10000_2_0);
	init_label(mercury____Index___tree_tree_1__ua10000_2_0_i4);
	init_label(mercury____Index___tree_tree_1__ua10000_2_0_i5);
BEGIN_CODE

/* code for predicate '__Index___tree_tree_1__ua10000'/2 in mode 0 */
Define_static(mercury____Index___tree_tree_1__ua10000_2_0);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Index___tree_tree_1__ua10000_2_0_i4);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury____Index___tree_tree_1__ua10000_2_0_i4);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Index___tree_tree_1__ua10000_2_0_i5);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury____Index___tree_tree_1__ua10000_2_0_i5);
	r1 = ((Integer) 2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__tree_module1)
	init_entry(mercury__tree__flatten_2__ua10000_3_0);
	init_label(mercury__tree__flatten_2__ua10000_3_0_i1000);
	init_label(mercury__tree__flatten_2__ua10000_3_0_i5);
	init_label(mercury__tree__flatten_2__ua10000_3_0_i6);
BEGIN_CODE

/* code for predicate 'tree__flatten_2__ua10000'/3 in mode 0 */
Define_static(mercury__tree__flatten_2__ua10000_3_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__tree__flatten_2__ua10000_3_0_i1000);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__tree__flatten_2__ua10000_3_0_i1000);
	incr_sp_push_msg(2, "tree__flatten_2__ua10000");
	detstackvar(2) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__tree__flatten_2__ua10000_3_0_i5);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__tree__flatten_2__ua10000_3_0_i5);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 1));
	localcall(mercury__tree__flatten_2__ua10000_3_0,
		LABEL(mercury__tree__flatten_2__ua10000_3_0_i6),
		STATIC(mercury__tree__flatten_2__ua10000_3_0));
Define_label(mercury__tree__flatten_2__ua10000_3_0_i6);
	update_prof_current_proc(LABEL(mercury__tree__flatten_2__ua10000_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__tree__flatten_2__ua10000_3_0,
		STATIC(mercury__tree__flatten_2__ua10000_3_0));
END_MODULE

BEGIN_MODULE(mercury__tree_module2)
	init_entry(mercury__tree__flatten__ua10000_2_0);
BEGIN_CODE

/* code for predicate 'tree__flatten__ua10000'/2 in mode 0 */
Define_static(mercury__tree__flatten__ua10000_2_0);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tailcall(STATIC(mercury__tree__flatten_2__ua10000_3_0),
		STATIC(mercury__tree__flatten__ua10000_2_0));
END_MODULE

BEGIN_MODULE(mercury__tree_module3)
	init_entry(mercury__tree__flatten_2_0);
BEGIN_CODE

/* code for predicate 'tree__flatten'/2 in mode 0 */
Define_entry(mercury__tree__flatten_2_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__tree__flatten__ua10000_2_0),
		ENTRY(mercury__tree__flatten_2_0));
END_MODULE

BEGIN_MODULE(mercury__tree_module4)
	init_entry(mercury____Unify___tree__tree_1_0);
	init_label(mercury____Unify___tree__tree_1_0_i1011);
	init_label(mercury____Unify___tree__tree_1_0_i6);
	init_label(mercury____Unify___tree__tree_1_0_i11);
	init_label(mercury____Unify___tree__tree_1_0_i1008);
	init_label(mercury____Unify___tree__tree_1_0_i1);
	init_label(mercury____Unify___tree__tree_1_0_i1010);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___tree__tree_1_0);
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Unify___tree__tree_1_0_i1011);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___tree__tree_1_0_i1008);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___tree__tree_1_0_i1011);
	incr_sp_push_msg(4, "__Unify__");
	detstackvar(4) = (Integer) succip;
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___tree__tree_1_0_i6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___tree__tree_1_0_i1010);
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__unify_2_0);
	tailcall(ENTRY(mercury__unify_2_0),
		ENTRY(mercury____Unify___tree__tree_1_0));
	}
Define_label(mercury____Unify___tree__tree_1_0_i6);
	if ((tag((Integer) r3) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Unify___tree__tree_1_0_i1);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r2, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(2), (Integer) r3, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	r2 = (Integer) field(mktag(2), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(2), (Integer) r3, ((Integer) 0));
	localcall(mercury____Unify___tree__tree_1_0,
		LABEL(mercury____Unify___tree__tree_1_0_i11),
		ENTRY(mercury____Unify___tree__tree_1_0));
Define_label(mercury____Unify___tree__tree_1_0_i11);
	update_prof_current_proc(LABEL(mercury____Unify___tree__tree_1_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___tree__tree_1_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury____Unify___tree__tree_1_0,
		ENTRY(mercury____Unify___tree__tree_1_0));
Define_label(mercury____Unify___tree__tree_1_0_i1008);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___tree__tree_1_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Unify___tree__tree_1_0_i1010);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__tree_module5)
	init_entry(mercury____Index___tree__tree_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___tree__tree_1_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury____Index___tree_tree_1__ua10000_2_0),
		ENTRY(mercury____Index___tree__tree_1_0));
END_MODULE

BEGIN_MODULE(mercury__tree_module6)
	init_entry(mercury____Compare___tree__tree_1_0);
	init_label(mercury____Compare___tree__tree_1_0_i2);
	init_label(mercury____Compare___tree__tree_1_0_i3);
	init_label(mercury____Compare___tree__tree_1_0_i4);
	init_label(mercury____Compare___tree__tree_1_0_i6);
	init_label(mercury____Compare___tree__tree_1_0_i12);
	init_label(mercury____Compare___tree__tree_1_0_i14);
	init_label(mercury____Compare___tree__tree_1_0_i20);
	init_label(mercury____Compare___tree__tree_1_0_i19);
	init_label(mercury____Compare___tree__tree_1_0_i9);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___tree__tree_1_0);
	incr_sp_push_msg(5, "__Compare__");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury____Index___tree_tree_1__ua10000_2_0),
		mercury____Compare___tree__tree_1_0_i2,
		ENTRY(mercury____Compare___tree__tree_1_0));
Define_label(mercury____Compare___tree__tree_1_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___tree__tree_1_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury____Index___tree_tree_1__ua10000_2_0),
		mercury____Compare___tree__tree_1_0_i3,
		ENTRY(mercury____Compare___tree__tree_1_0));
Define_label(mercury____Compare___tree__tree_1_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___tree__tree_1_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___tree__tree_1_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___tree__tree_1_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___tree__tree_1_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___tree__tree_1_0_i6);
	r1 = (Integer) detstackvar(1);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Compare___tree__tree_1_0_i12);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___tree__tree_1_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___tree__tree_1_0_i12);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___tree__tree_1_0_i14);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(2);
	if ((tag((Integer) tempr1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___tree__tree_1_0_i9);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__compare_3_3);
	tailcall(ENTRY(mercury__compare_3_3),
		ENTRY(mercury____Compare___tree__tree_1_0));
	}
	}
Define_label(mercury____Compare___tree__tree_1_0_i14);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(2);
	if ((tag((Integer) tempr1) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Compare___tree__tree_1_0_i9);
	r2 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	r3 = (Integer) field(mktag(2), (Integer) tempr1, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(2), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) detstackvar(4);
	localcall(mercury____Compare___tree__tree_1_0,
		LABEL(mercury____Compare___tree__tree_1_0_i20),
		ENTRY(mercury____Compare___tree__tree_1_0));
	}
Define_label(mercury____Compare___tree__tree_1_0_i20);
	update_prof_current_proc(LABEL(mercury____Compare___tree__tree_1_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___tree__tree_1_0_i19);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___tree__tree_1_0_i19);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury____Compare___tree__tree_1_0,
		ENTRY(mercury____Compare___tree__tree_1_0));
Define_label(mercury____Compare___tree__tree_1_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___tree__tree_1_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__tree_bunch_0(void)
{
	mercury__tree_module0();
	mercury__tree_module1();
	mercury__tree_module2();
	mercury__tree_module3();
	mercury__tree_module4();
	mercury__tree_module5();
	mercury__tree_module6();
}

#endif

void mercury__tree__init(void); /* suppress gcc warning */
void mercury__tree__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__tree_bunch_0();
#endif
}
