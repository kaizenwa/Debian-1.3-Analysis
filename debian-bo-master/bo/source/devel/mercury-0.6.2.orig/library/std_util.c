/*
** Automatically generated from `std_util.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__std_util__init
ENDINIT
*/

#include "imp.h"


#include "type_info.h"

int	mercury_compare_type_info(Word type_info_1, Word type_info_2);



/*
**	`univ' is represented as a two word structure.
**	The first word contains the address of a type_info for the type.
**	The second word contains the data.
*/

#define UNIV_OFFSET_FOR_TYPEINFO 0
#define UNIV_OFFSET_FOR_DATA 1



Declare_static(mercury____Index___std_util_pair_1__ua10000_2_0);
Declare_static(mercury____Index___std_util_pair_2__ua10000_2_0);
Declare_static(mercury____Index___std_util_maybe_1__ua10000_2_0);
Declare_label(mercury____Index___std_util_maybe_1__ua10000_2_0_i3);
Declare_static(mercury__std_util__maybe_pred__ua10000_3_0);
Declare_label(mercury__std_util__maybe_pred__ua10000_3_0_i4);
Declare_label(mercury__std_util__maybe_pred__ua10000_3_0_i3);
Define_extern_entry(mercury__std_util__type_to_univ_2_2);
Declare_label(mercury__std_util__type_to_univ_2_2_i1);
Define_extern_entry(mercury__std_util__type_to_univ_2_0);
Define_extern_entry(mercury__std_util__type_to_univ_2_1);
Define_extern_entry(mercury__std_util__univ_to_type_2_0);
Declare_label(mercury__std_util__univ_to_type_2_0_i2);
Declare_label(mercury__std_util__univ_to_type_2_0_i1000);
Define_extern_entry(mercury__std_util__univ_to_type_2_1);
Define_extern_entry(mercury__std_util__solutions_2_0);
Declare_label(mercury__std_util__solutions_2_0_i2);
Define_extern_entry(mercury__std_util__solutions_2_1);
Declare_label(mercury__std_util__solutions_2_1_i2);
Define_extern_entry(mercury__std_util__solutions_set_2_0);
Declare_label(mercury__std_util__solutions_set_2_0_i2);
Define_extern_entry(mercury__std_util__solutions_set_2_1);
Declare_label(mercury__std_util__solutions_set_2_1_i2);
Define_extern_entry(mercury__std_util__unsorted_solutions_2_0);
Declare_label(mercury__std_util__unsorted_solutions_2_0_i2);
Define_extern_entry(mercury__std_util__unsorted_solutions_2_1);
Declare_label(mercury__std_util__unsorted_solutions_2_1_i2);
Define_extern_entry(mercury__std_util__maybe_pred_3_0);
Define_extern_entry(mercury__std_util__semidet_succeed_0_0);
Declare_label(mercury__std_util__semidet_succeed_0_0_i1);
Define_extern_entry(mercury__std_util__semidet_fail_0_0);
Declare_label(mercury__std_util__semidet_fail_0_0_i1);
Define_extern_entry(mercury__std_util__cc_multi_equal_2_0);
Define_extern_entry(mercury____Unify___std_util__maybe_1_0);
Declare_label(mercury____Unify___std_util__maybe_1_0_i1008);
Declare_label(mercury____Unify___std_util__maybe_1_0_i1005);
Declare_label(mercury____Unify___std_util__maybe_1_0_i1007);
Define_extern_entry(mercury____Index___std_util__maybe_1_0);
Define_extern_entry(mercury____Compare___std_util__maybe_1_0);
Declare_label(mercury____Compare___std_util__maybe_1_0_i2);
Declare_label(mercury____Compare___std_util__maybe_1_0_i3);
Declare_label(mercury____Compare___std_util__maybe_1_0_i4);
Declare_label(mercury____Compare___std_util__maybe_1_0_i6);
Declare_label(mercury____Compare___std_util__maybe_1_0_i11);
Declare_label(mercury____Compare___std_util__maybe_1_0_i9);
Define_extern_entry(mercury____Unify___std_util__unit_0_0);
Declare_label(mercury____Unify___std_util__unit_0_0_i1);
Define_extern_entry(mercury____Index___std_util__unit_0_0);
Define_extern_entry(mercury____Compare___std_util__unit_0_0);
Define_extern_entry(mercury____Unify___std_util__pair_2_0);
Declare_label(mercury____Unify___std_util__pair_2_0_i2);
Declare_label(mercury____Unify___std_util__pair_2_0_i1);
Define_extern_entry(mercury____Index___std_util__pair_2_0);
Define_extern_entry(mercury____Compare___std_util__pair_2_0);
Declare_label(mercury____Compare___std_util__pair_2_0_i4);
Declare_label(mercury____Compare___std_util__pair_2_0_i3);
Define_extern_entry(mercury____Unify___std_util__pair_1_0);
Define_extern_entry(mercury____Index___std_util__pair_1_0);
Define_extern_entry(mercury____Compare___std_util__pair_1_0);


/*
 * Univ has a special value reserved for its layout, since it needs to
 * be handled as a special case. See above for information on 
 * the representation of data of type `univ'.
 */

#ifdef  USE_TYPE_LAYOUT

Word * mercury_data_std_util__base_type_layout_univ_0[] = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_UNIV_VALUE))
};

#endif

Define_extern_entry(mercury____Unify___std_util__univ_0_0);
Define_extern_entry(mercury____Index___std_util__univ_0_0);
Define_extern_entry(mercury____Compare___std_util__univ_0_0);
Declare_label(mercury____Compare___std_util__univ_0_0_i1);
Define_extern_entry(mercury____Term_To_Type___std_util__univ_0_0);
Define_extern_entry(mercury____Type_To_Term___std_util__univ_0_0);

BEGIN_MODULE(unify_univ_module)
	init_entry(mercury____Unify___std_util__univ_0_0);
	init_entry(mercury____Index___std_util__univ_0_0);
	init_entry(mercury____Compare___std_util__univ_0_0);
	init_label(mercury____Compare___std_util__univ_0_0_i1);
	init_entry(mercury____Term_To_Type___std_util__univ_0_0);
	init_entry(mercury____Type_To_Term___std_util__univ_0_0);
BEGIN_CODE
Define_entry(mercury____Unify___std_util__univ_0_0);
{
	/*
	** Unification for univ.
	**
	** The two inputs are in the registers named by unify_input[12].
	** The success/failure indication should go in unify_output.
	*/

	Word univ1, univ2;
	Word typeinfo1, typeinfo2;

	univ1 = unify_input1;
	univ2 = unify_input2;

	/* First check the type_infos compare equal */
	typeinfo1 = field(mktag(0), univ1, UNIV_OFFSET_FOR_TYPEINFO);
	typeinfo2 = field(mktag(0), univ2, UNIV_OFFSET_FOR_TYPEINFO);
	if (mercury_compare_type_info(typeinfo1, typeinfo2) != COMPARE_EQUAL)
	{
		unify_output = FALSE;
		proceed();
	}

	/*
	** Then invoke the generic unification predicate on the
	** unwrapped args
	*/
	mercury__unify__x = field(mktag(0), univ1, UNIV_OFFSET_FOR_DATA);
	mercury__unify__y = field(mktag(0), univ2, UNIV_OFFSET_FOR_DATA);
	mercury__unify__typeinfo = typeinfo1;
	{
		Declare_entry(mercury__unify_2_0);
		tailcall(ENTRY(mercury__unify_2_0),
			LABEL(mercury____Unify___std_util__univ_0_0));
	}
}

Define_entry(mercury____Index___std_util__univ_0_0);
	r2 = -1;
	proceed();

Define_entry(mercury____Compare___std_util__univ_0_0);
{
	/*
	** Comparison for univ:
	**
	** The two inputs are in the registers named by compare_input[12].
	** The result should go in compare_output.
	*/

	Word univ1, univ2;
	Word typeinfo1, typeinfo2;

	univ1 = compare_input1;
	univ2 = compare_input2;

	/* First compare the type_infos */
	typeinfo1 = field(mktag(0), univ1, UNIV_OFFSET_FOR_TYPEINFO);
	typeinfo2 = field(mktag(0), univ2, UNIV_OFFSET_FOR_TYPEINFO);
	compare_output = mercury_compare_type_info(typeinfo1, typeinfo2);
	if (compare_output != COMPARE_EQUAL) {
		proceed();
	}

	/*
	** If the types are the same, then invoke the generic compare/3
	** predicate on the unwrapped args.
	*/
#ifdef	COMPACT_ARGS
	r1 = typeinfo1;
	r3 = field(mktag(0), univ2, UNIV_OFFSET_FOR_DATA);
	r2 = field(mktag(0), univ1, UNIV_OFFSET_FOR_DATA);
	{
		Declare_entry(mercury__compare_3_0);
		tailcall(ENTRY(mercury__compare_3_0),
			LABEL(mercury____Compare___std_util__univ_0_0));
	}
#else
	r1 = typeinfo1;
	r4 = field(mktag(0), univ2, UNIV_OFFSET_FOR_DATA);
	r3 = field(mktag(0), univ1, UNIV_OFFSET_FOR_DATA);
	{
		Declare_entry(mercury__compare_3_0);
		call(ENTRY(mercury__compare_3_0),
			LABEL(mercury____Compare___std_util__univ_0_0_i1),
			LABEL(mercury____Compare___std_util__univ_0_0));
	}
#endif
}
Define_label(mercury____Compare___std_util__univ_0_0_i1);
#ifdef	COMPACT_ARGS
	fatal_error("mercury____Compare___std_util__univ_0_0_i1 reached in COMPACT_ARGS mode");
#else
	/* shuffle the return value into the right register */
	r1 = r2;
	proceed();
#endif

Define_entry(mercury____Term_To_Type___std_util__univ_0_0);
	/* don't know what to put here. */
	fatal_error("cannot convert univ type to term");

Define_entry(mercury____Type_To_Term___std_util__univ_0_0);
	/* don't know what to put here. */
	fatal_error("cannot convert type univ to term");

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_unify_univ_module
*/
void sys_init_unify_univ_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_unify_univ_module(void) {
	extern ModuleFunc unify_univ_module;
	unify_univ_module();
}




/*
** Compare two type_info structures, using an arbitrary ordering
** (based on the addresses of the unification predicates).
*/

int
mercury_compare_type_info(Word type_info_1, Word type_info_2)
{
	int	i, num_arg_types, comp;
	Word	unify_pred_1, unify_pred_2;
#ifdef	ONE_OR_TWO_CELL_TYPE_INFO
	Word	base_type_info_1, base_type_info_2;
#endif

	/* First find the addresses of the unify preds in the type_infos */

#ifdef	ONE_OR_TWO_CELL_TYPE_INFO
	base_type_info_1 = field(mktag(0), type_info_1, 0);
	base_type_info_2 = field(mktag(0), type_info_2, 0);

	if (base_type_info_1 == 0)
		unify_pred_1 = field(mktag(0), type_info_1,
				OFFSET_FOR_UNIFY_PRED);
	else
		unify_pred_1 = field(mktag(0), base_type_info_1,
				OFFSET_FOR_UNIFY_PRED);

	if (base_type_info_2 == 0)
		unify_pred_2 = field(mktag(0), type_info_2,
				OFFSET_FOR_UNIFY_PRED);
	else
		unify_pred_2 = field(mktag(0), base_type_info_2,
				OFFSET_FOR_UNIFY_PRED);
#else
	unify_pred_1 = field(mktag(0), type_info_1, OFFSET_FOR_UNIFY_PRED);
	unify_pred_2 = field(mktag(0), type_info_2, OFFSET_FOR_UNIFY_PRED);
#endif

	/* Then compare the addresses of the unify preds in the type_infos */
	if (unify_pred_1 < unify_pred_2) {
		return COMPARE_LESS;
	}
	if (unify_pred_1 > unify_pred_2) {
		return COMPARE_GREATER;
	}

	/*
	** If the addresses of the unify preds are equal, we don't need to
	** compare the arity of the types - they must be the same.
	** But we need to recursively compare the argument types, if any.
	*/

#ifdef	ONE_OR_TWO_CELL_TYPE_INFO
	if (base_type_info_1 == 0)
		return COMPARE_EQUAL;
	else
	{
		num_arg_types = field(mktag(0), base_type_info_1,
				OFFSET_FOR_COUNT);
		for (i = 1; i <= num_arg_types; i++) {
			Word arg_type_info_1 = field(mktag(0),
						base_type_info_1, i);
			Word arg_type_info_2 = field(mktag(0),
						base_type_info_2, i);
			comp = mercury_compare_type_info(
					arg_type_info_1, arg_type_info_2);
			if (comp != COMPARE_EQUAL)
				return comp;
		}
		return COMPARE_EQUAL;
	}
#else
	num_arg_types = field(mktag(0), type_info_1, OFFSET_FOR_COUNT);
	for (i = 0; i < num_arg_types; i++) {
		Word arg_type_info_1 = field(mktag(0), type_info_1,
					OFFSET_FOR_ARG_TYPE_INFOS + i);
		Word arg_type_info_2 = field(mktag(0), type_info_2,
					OFFSET_FOR_ARG_TYPE_INFOS + i);
		comp = mercury_compare_type_info(
				arg_type_info_1, arg_type_info_2);
		if (comp != COMPARE_EQUAL)
			return comp;
	}
	return COMPARE_EQUAL;
#endif
}



extern Word * mercury_data_std_util__base_type_layout_maybe_1[];
Word * mercury_data_std_util__base_type_info_maybe_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___std_util__maybe_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___std_util__maybe_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___std_util__maybe_1_0),
	(Word *) (Integer) mercury_data_std_util__base_type_layout_maybe_1
};

extern Word * mercury_data_std_util__base_type_layout_pair_1[];
Word * mercury_data_std_util__base_type_info_pair_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___std_util__pair_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___std_util__pair_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___std_util__pair_1_0),
	(Word *) (Integer) mercury_data_std_util__base_type_layout_pair_1
};

extern Word * mercury_data_std_util__base_type_layout_pair_2[];
Word * mercury_data_std_util__base_type_info_pair_2[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) ENTRY(mercury____Unify___std_util__pair_2_0),
	(Word *) (Integer) ENTRY(mercury____Index___std_util__pair_2_0),
	(Word *) (Integer) ENTRY(mercury____Compare___std_util__pair_2_0),
	(Word *) (Integer) mercury_data_std_util__base_type_layout_pair_2
};

extern Word * mercury_data_std_util__base_type_layout_unit_0[];
Word * mercury_data_std_util__base_type_info_unit_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___std_util__unit_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___std_util__unit_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___std_util__unit_0_0),
	(Word *) (Integer) mercury_data_std_util__base_type_layout_unit_0
};

Declare_entry(mercury____Unify___std_util__univ_0_0);
Declare_entry(mercury____Index___std_util__univ_0_0);
Declare_entry(mercury____Compare___std_util__univ_0_0);
extern Word * mercury_data_std_util__base_type_layout_univ_0[];
Word * mercury_data_std_util__base_type_info_univ_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___std_util__univ_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___std_util__univ_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___std_util__univ_0_0),
	(Word *) (Integer) mercury_data_std_util__base_type_layout_univ_0
};

extern Word * mercury_data_std_util__common_0[];
Word * mercury_data_std_util__base_type_layout_unit_0[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_std_util__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_std_util__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_std_util__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_std_util__common_0)
};

extern Word * mercury_data_std_util__common_1[];
Word * mercury_data_std_util__base_type_layout_pair_2[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_std_util__common_1),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_std_util__common_3[];
Word * mercury_data_std_util__base_type_layout_pair_1[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_std_util__common_3),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_std_util__common_3),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_std_util__common_3),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_std_util__common_3)
};

extern Word * mercury_data_std_util__common_4[];
extern Word * mercury_data_std_util__common_5[];
Word * mercury_data_std_util__base_type_layout_maybe_1[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_std_util__common_4),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_std_util__common_5),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

Word * mercury_data_std_util__common_0[] = {
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 1),
	(Word *) string_const("unit", 4)
};

Word * mercury_data_std_util__common_1[] = {
	(Word *) ((Integer) 2),
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 2),
	(Word *) string_const("-", 1)
};

Word * mercury_data_std_util__common_2[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 1)
};

Word * mercury_data_std_util__common_3[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_std_util__common_2)
};

Word * mercury_data_std_util__common_4[] = {
	(Word *) ((Integer) 0),
	(Word *) ((Integer) 1),
	(Word *) string_const("no", 2)
};

Word * mercury_data_std_util__common_5[] = {
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 1),
	(Word *) string_const("yes", 3)
};

BEGIN_MODULE(mercury__std_util_module0)
	init_entry(mercury____Index___std_util_pair_1__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___std_util_pair_1__ua10000'/2 in mode 0 */
Define_static(mercury____Index___std_util_pair_1__ua10000_2_0);
	tailcall(STATIC(mercury____Index___std_util_pair_2__ua10000_2_0),
		STATIC(mercury____Index___std_util_pair_1__ua10000_2_0));
END_MODULE

BEGIN_MODULE(mercury__std_util_module1)
	init_entry(mercury____Index___std_util_pair_2__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___std_util_pair_2__ua10000'/2 in mode 0 */
Define_static(mercury____Index___std_util_pair_2__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module2)
	init_entry(mercury____Index___std_util_maybe_1__ua10000_2_0);
	init_label(mercury____Index___std_util_maybe_1__ua10000_2_0_i3);
BEGIN_CODE

/* code for predicate '__Index___std_util_maybe_1__ua10000'/2 in mode 0 */
Define_static(mercury____Index___std_util_maybe_1__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Index___std_util_maybe_1__ua10000_2_0_i3);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury____Index___std_util_maybe_1__ua10000_2_0_i3);
	r1 = ((Integer) 1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module3)
	init_entry(mercury__std_util__maybe_pred__ua10000_3_0);
	init_label(mercury__std_util__maybe_pred__ua10000_3_0_i4);
	init_label(mercury__std_util__maybe_pred__ua10000_3_0_i3);
BEGIN_CODE

/* code for predicate 'maybe_pred__ua10000'/3 in mode 0 */
Define_static(mercury__std_util__maybe_pred__ua10000_3_0);
	r4 = (Integer) r2;
	r2 = ((Integer) 1);
	r3 = ((Integer) 1);
	incr_sp_push_msg(1, "maybe_pred__ua10000");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(do_call_semidet_closure);
	call_localret(ENTRY(do_call_semidet_closure),
		mercury__std_util__maybe_pred__ua10000_3_0_i4,
		STATIC(mercury__std_util__maybe_pred__ua10000_3_0));
	}
Define_label(mercury__std_util__maybe_pred__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__std_util__maybe_pred__ua10000_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__std_util__maybe_pred__ua10000_3_0_i3);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__std_util__maybe_pred__ua10000_3_0_i3);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module4)
	init_entry(mercury__std_util__type_to_univ_2_2);
	init_label(mercury__std_util__type_to_univ_2_2_i1);
BEGIN_CODE

/* code for predicate 'type_to_univ'/2 in mode 2 */
Define_entry(mercury__std_util__type_to_univ_2_2);
	r3 = (Integer) r1;
	{
		Word	TypeInfo_for_T;
		Word	Type;
		Word	Univ;
		TypeInfo_for_T = (Integer) r1;
		Univ = (Integer) r2;
		{
	Word univ_type_info = field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO);
	if (mercury_compare_type_info(univ_type_info, TypeInfo_for_T)
		== COMPARE_EQUAL)
	{
		Type = field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA);
		SUCCESS_INDICATOR = TRUE;
	} else {
		SUCCESS_INDICATOR = FALSE;
	}
}
		r4 = Type;

	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__std_util__type_to_univ_2_2_i1);
	r2 = (Integer) r4;
	r1 = TRUE;
	proceed();
Define_label(mercury__std_util__type_to_univ_2_2_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module5)
	init_entry(mercury__std_util__type_to_univ_2_0);
BEGIN_CODE

/* code for predicate 'type_to_univ'/2 in mode 0 */
Define_entry(mercury__std_util__type_to_univ_2_0);
	{
		Word	TypeInfo_for_T;
		Word	Type;
		Word	Univ;
		TypeInfo_for_T = (Integer) r1;
		Type = (Integer) r2;
		
	incr_hp(Univ, 2);
	field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO) = TypeInfo_for_T;
	field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA) = Type;

		r3 = Univ;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module6)
	init_entry(mercury__std_util__type_to_univ_2_1);
BEGIN_CODE

/* code for predicate 'type_to_univ'/2 in mode 1 */
Define_entry(mercury__std_util__type_to_univ_2_1);
	{
		Word	TypeInfo_for_T;
		Word	Type;
		Word	Univ;
		TypeInfo_for_T = (Integer) r1;
		Type = (Integer) r2;
		
	incr_hp(Univ, 2);
	field(mktag(0), Univ, UNIV_OFFSET_FOR_TYPEINFO) = TypeInfo_for_T;
	field(mktag(0), Univ, UNIV_OFFSET_FOR_DATA) = Type;

		r3 = Univ;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module7)
	init_entry(mercury__std_util__univ_to_type_2_0);
	init_label(mercury__std_util__univ_to_type_2_0_i2);
	init_label(mercury__std_util__univ_to_type_2_0_i1000);
BEGIN_CODE

/* code for predicate 'univ_to_type'/2 in mode 0 */
Define_entry(mercury__std_util__univ_to_type_2_0);
	incr_sp_push_msg(1, "univ_to_type");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__std_util__type_to_univ_2_2),
		mercury__std_util__univ_to_type_2_0_i2,
		ENTRY(mercury__std_util__univ_to_type_2_0));
	}
Define_label(mercury__std_util__univ_to_type_2_0_i2);
	update_prof_current_proc(LABEL(mercury__std_util__univ_to_type_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__std_util__univ_to_type_2_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__std_util__univ_to_type_2_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module8)
	init_entry(mercury__std_util__univ_to_type_2_1);
BEGIN_CODE

/* code for predicate 'univ_to_type'/2 in mode 1 */
Define_entry(mercury__std_util__univ_to_type_2_1);
	{
		tailcall(STATIC(mercury__std_util__type_to_univ_2_1),
		ENTRY(mercury__std_util__univ_to_type_2_1));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module9)
	init_entry(mercury__std_util__solutions_2_0);
	init_label(mercury__std_util__solutions_2_0_i2);
BEGIN_CODE

/* code for predicate 'solutions'/2 in mode 0 */
Define_entry(mercury__std_util__solutions_2_0);
	incr_sp_push_msg(2, "solutions");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__std_util__builtin_solutions_2_0);
	call_localret(ENTRY(mercury__std_util__builtin_solutions_2_0),
		mercury__std_util__solutions_2_0_i2,
		ENTRY(mercury__std_util__solutions_2_0));
	}
Define_label(mercury__std_util__solutions_2_0_i2);
	update_prof_current_proc(LABEL(mercury__std_util__solutions_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__list__sort_and_remove_dups_2_0);
	tailcall(ENTRY(mercury__list__sort_and_remove_dups_2_0),
		ENTRY(mercury__std_util__solutions_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module10)
	init_entry(mercury__std_util__solutions_2_1);
	init_label(mercury__std_util__solutions_2_1_i2);
BEGIN_CODE

/* code for predicate 'solutions'/2 in mode 1 */
Define_entry(mercury__std_util__solutions_2_1);
	incr_sp_push_msg(2, "solutions");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__std_util__builtin_solutions_2_1);
	call_localret(ENTRY(mercury__std_util__builtin_solutions_2_1),
		mercury__std_util__solutions_2_1_i2,
		ENTRY(mercury__std_util__solutions_2_1));
	}
Define_label(mercury__std_util__solutions_2_1_i2);
	update_prof_current_proc(LABEL(mercury__std_util__solutions_2_1));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__list__sort_and_remove_dups_2_0);
	tailcall(ENTRY(mercury__list__sort_and_remove_dups_2_0),
		ENTRY(mercury__std_util__solutions_2_1));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module11)
	init_entry(mercury__std_util__solutions_set_2_0);
	init_label(mercury__std_util__solutions_set_2_0_i2);
BEGIN_CODE

/* code for predicate 'solutions_set'/2 in mode 0 */
Define_entry(mercury__std_util__solutions_set_2_0);
	incr_sp_push_msg(2, "solutions_set");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__std_util__builtin_solutions_2_0);
	call_localret(ENTRY(mercury__std_util__builtin_solutions_2_0),
		mercury__std_util__solutions_set_2_0_i2,
		ENTRY(mercury__std_util__solutions_set_2_0));
	}
Define_label(mercury__std_util__solutions_set_2_0_i2);
	update_prof_current_proc(LABEL(mercury__std_util__solutions_set_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__set__list_to_set_2_0);
	tailcall(ENTRY(mercury__set__list_to_set_2_0),
		ENTRY(mercury__std_util__solutions_set_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module12)
	init_entry(mercury__std_util__solutions_set_2_1);
	init_label(mercury__std_util__solutions_set_2_1_i2);
BEGIN_CODE

/* code for predicate 'solutions_set'/2 in mode 1 */
Define_entry(mercury__std_util__solutions_set_2_1);
	incr_sp_push_msg(2, "solutions_set");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__std_util__builtin_solutions_2_1);
	call_localret(ENTRY(mercury__std_util__builtin_solutions_2_1),
		mercury__std_util__solutions_set_2_1_i2,
		ENTRY(mercury__std_util__solutions_set_2_1));
	}
Define_label(mercury__std_util__solutions_set_2_1_i2);
	update_prof_current_proc(LABEL(mercury__std_util__solutions_set_2_1));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__set__list_to_set_2_0);
	tailcall(ENTRY(mercury__set__list_to_set_2_0),
		ENTRY(mercury__std_util__solutions_set_2_1));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module13)
	init_entry(mercury__std_util__unsorted_solutions_2_0);
	init_label(mercury__std_util__unsorted_solutions_2_0_i2);
BEGIN_CODE

/* code for predicate 'unsorted_solutions'/2 in mode 0 */
Define_entry(mercury__std_util__unsorted_solutions_2_0);
	incr_sp_push_msg(2, "unsorted_solutions");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__std_util__builtin_solutions_2_0);
	call_localret(ENTRY(mercury__std_util__builtin_solutions_2_0),
		mercury__std_util__unsorted_solutions_2_0_i2,
		ENTRY(mercury__std_util__unsorted_solutions_2_0));
	}
Define_label(mercury__std_util__unsorted_solutions_2_0_i2);
	update_prof_current_proc(LABEL(mercury__std_util__unsorted_solutions_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	}
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__std_util__cc_multi_equal_2_0),
		ENTRY(mercury__std_util__unsorted_solutions_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module14)
	init_entry(mercury__std_util__unsorted_solutions_2_1);
	init_label(mercury__std_util__unsorted_solutions_2_1_i2);
BEGIN_CODE

/* code for predicate 'unsorted_solutions'/2 in mode 1 */
Define_entry(mercury__std_util__unsorted_solutions_2_1);
	incr_sp_push_msg(2, "unsorted_solutions");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__std_util__builtin_solutions_2_1);
	call_localret(ENTRY(mercury__std_util__builtin_solutions_2_1),
		mercury__std_util__unsorted_solutions_2_1_i2,
		ENTRY(mercury__std_util__unsorted_solutions_2_1));
	}
Define_label(mercury__std_util__unsorted_solutions_2_1_i2);
	update_prof_current_proc(LABEL(mercury__std_util__unsorted_solutions_2_1));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mercury_data_mercury_builtin__base_type_info_list_1;
	}
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__std_util__cc_multi_equal_2_0),
		ENTRY(mercury__std_util__unsorted_solutions_2_1));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module15)
	init_entry(mercury__std_util__maybe_pred_3_0);
BEGIN_CODE

/* code for predicate 'maybe_pred'/3 in mode 0 */
Define_entry(mercury__std_util__maybe_pred_3_0);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	tailcall(STATIC(mercury__std_util__maybe_pred__ua10000_3_0),
		ENTRY(mercury__std_util__maybe_pred_3_0));
END_MODULE

BEGIN_MODULE(mercury__std_util_module16)
	init_entry(mercury__std_util__semidet_succeed_0_0);
	init_label(mercury__std_util__semidet_succeed_0_0_i1);
BEGIN_CODE

/* code for predicate 'semidet_succeed'/0 in mode 0 */
Define_entry(mercury__std_util__semidet_succeed_0_0);
	{
		SUCCESS_INDICATOR = TRUE;

	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__std_util__semidet_succeed_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__std_util__semidet_succeed_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module17)
	init_entry(mercury__std_util__semidet_fail_0_0);
	init_label(mercury__std_util__semidet_fail_0_0_i1);
BEGIN_CODE

/* code for predicate 'semidet_fail'/0 in mode 0 */
Define_entry(mercury__std_util__semidet_fail_0_0);
	{
		SUCCESS_INDICATOR = FALSE;

	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__std_util__semidet_fail_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__std_util__semidet_fail_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module18)
	init_entry(mercury__std_util__cc_multi_equal_2_0);
BEGIN_CODE

/* code for predicate 'cc_multi_equal'/2 in mode 0 */
Define_entry(mercury__std_util__cc_multi_equal_2_0);
	{
		Word	TypeInfo_for_T;
		Word	X;
		Word	Y;
		TypeInfo_for_T = (Integer) r1;
		X = (Integer) r2;
		Y = X;
		r3 = Y;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module19)
	init_entry(mercury____Unify___std_util__maybe_1_0);
	init_label(mercury____Unify___std_util__maybe_1_0_i1008);
	init_label(mercury____Unify___std_util__maybe_1_0_i1005);
	init_label(mercury____Unify___std_util__maybe_1_0_i1007);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___std_util__maybe_1_0);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___std_util__maybe_1_0_i1008);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___std_util__maybe_1_0_i1005);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___std_util__maybe_1_0_i1008);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___std_util__maybe_1_0_i1007);
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__unify_2_0);
	tailcall(ENTRY(mercury__unify_2_0),
		ENTRY(mercury____Unify___std_util__maybe_1_0));
	}
Define_label(mercury____Unify___std_util__maybe_1_0_i1005);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___std_util__maybe_1_0_i1007);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module20)
	init_entry(mercury____Index___std_util__maybe_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___std_util__maybe_1_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury____Index___std_util_maybe_1__ua10000_2_0),
		ENTRY(mercury____Index___std_util__maybe_1_0));
END_MODULE

BEGIN_MODULE(mercury__std_util_module21)
	init_entry(mercury____Compare___std_util__maybe_1_0);
	init_label(mercury____Compare___std_util__maybe_1_0_i2);
	init_label(mercury____Compare___std_util__maybe_1_0_i3);
	init_label(mercury____Compare___std_util__maybe_1_0_i4);
	init_label(mercury____Compare___std_util__maybe_1_0_i6);
	init_label(mercury____Compare___std_util__maybe_1_0_i11);
	init_label(mercury____Compare___std_util__maybe_1_0_i9);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___std_util__maybe_1_0);
	incr_sp_push_msg(5, "__Compare__");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury____Index___std_util_maybe_1__ua10000_2_0),
		mercury____Compare___std_util__maybe_1_0_i2,
		ENTRY(mercury____Compare___std_util__maybe_1_0));
Define_label(mercury____Compare___std_util__maybe_1_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___std_util__maybe_1_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury____Index___std_util_maybe_1__ua10000_2_0),
		mercury____Compare___std_util__maybe_1_0_i3,
		ENTRY(mercury____Compare___std_util__maybe_1_0));
Define_label(mercury____Compare___std_util__maybe_1_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___std_util__maybe_1_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___std_util__maybe_1_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___std_util__maybe_1_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___std_util__maybe_1_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___std_util__maybe_1_0_i6);
	if (((Integer) detstackvar(1) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___std_util__maybe_1_0_i11);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___std_util__maybe_1_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___std_util__maybe_1_0_i11);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(2);
	if (((Integer) tempr1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___std_util__maybe_1_0_i9);
	r2 = (Integer) field(mktag(1), (Integer) detstackvar(1), ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__compare_3_3);
	tailcall(ENTRY(mercury__compare_3_3),
		ENTRY(mercury____Compare___std_util__maybe_1_0));
	}
	}
Define_label(mercury____Compare___std_util__maybe_1_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___std_util__maybe_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module22)
	init_entry(mercury____Unify___std_util__unit_0_0);
	init_label(mercury____Unify___std_util__unit_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___std_util__unit_0_0);
	if (((Integer) r1 != (Integer) r2))
		GOTO_LABEL(mercury____Unify___std_util__unit_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___std_util__unit_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module23)
	init_entry(mercury____Index___std_util__unit_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___std_util__unit_0_0);
	{
	Declare_entry(mercury__builtin_index_int_2_0);
	tailcall(ENTRY(mercury__builtin_index_int_2_0),
		ENTRY(mercury____Index___std_util__unit_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module24)
	init_entry(mercury____Compare___std_util__unit_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___std_util__unit_0_0);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___std_util__unit_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module25)
	init_entry(mercury____Unify___std_util__pair_2_0);
	init_label(mercury____Unify___std_util__pair_2_0_i2);
	init_label(mercury____Unify___std_util__pair_2_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___std_util__pair_2_0);
	incr_sp_push_msg(4, "__Unify__");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 1));
	detstackvar(3) = (Integer) r2;
	r2 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury____Unify___std_util__pair_2_0_i2,
		ENTRY(mercury____Unify___std_util__pair_2_0));
	}
Define_label(mercury____Unify___std_util__pair_2_0_i2);
	update_prof_current_proc(LABEL(mercury____Unify___std_util__pair_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___std_util__pair_2_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__unify_2_0);
	tailcall(ENTRY(mercury__unify_2_0),
		ENTRY(mercury____Unify___std_util__pair_2_0));
	}
Define_label(mercury____Unify___std_util__pair_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__std_util_module26)
	init_entry(mercury____Index___std_util__pair_2_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___std_util__pair_2_0);
	tailcall(STATIC(mercury____Index___std_util_pair_2__ua10000_2_0),
		ENTRY(mercury____Index___std_util__pair_2_0));
END_MODULE

BEGIN_MODULE(mercury__std_util_module27)
	init_entry(mercury____Compare___std_util__pair_2_0);
	init_label(mercury____Compare___std_util__pair_2_0_i4);
	init_label(mercury____Compare___std_util__pair_2_0_i3);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___std_util__pair_2_0);
	incr_sp_push_msg(4, "__Compare__");
	detstackvar(4) = (Integer) succip;
	detstackvar(3) = (Integer) r2;
	r2 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	r3 = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 1));
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury____Compare___std_util__pair_2_0_i4,
		ENTRY(mercury____Compare___std_util__pair_2_0));
	}
Define_label(mercury____Compare___std_util__pair_2_0_i4);
	update_prof_current_proc(LABEL(mercury____Compare___std_util__pair_2_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___std_util__pair_2_0_i3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___std_util__pair_2_0_i3);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__compare_3_3);
	tailcall(ENTRY(mercury__compare_3_3),
		ENTRY(mercury____Compare___std_util__pair_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module28)
	init_entry(mercury____Unify___std_util__pair_1_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___std_util__pair_1_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	{
		tailcall(STATIC(mercury____Unify___std_util__pair_2_0),
		ENTRY(mercury____Unify___std_util__pair_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__std_util_module29)
	init_entry(mercury____Index___std_util__pair_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___std_util__pair_1_0);
	tailcall(STATIC(mercury____Index___std_util_pair_1__ua10000_2_0),
		ENTRY(mercury____Index___std_util__pair_1_0));
END_MODULE

BEGIN_MODULE(mercury__std_util_module30)
	init_entry(mercury____Compare___std_util__pair_1_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___std_util__pair_1_0);
	r4 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	{
		tailcall(STATIC(mercury____Compare___std_util__pair_2_0),
		ENTRY(mercury____Compare___std_util__pair_1_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__std_util_bunch_0(void)
{
	mercury__std_util_module0();
	mercury__std_util_module1();
	mercury__std_util_module2();
	mercury__std_util_module3();
	mercury__std_util_module4();
	mercury__std_util_module5();
	mercury__std_util_module6();
	mercury__std_util_module7();
	mercury__std_util_module8();
	mercury__std_util_module9();
	mercury__std_util_module10();
	mercury__std_util_module11();
	mercury__std_util_module12();
	mercury__std_util_module13();
	mercury__std_util_module14();
	mercury__std_util_module15();
	mercury__std_util_module16();
	mercury__std_util_module17();
	mercury__std_util_module18();
	mercury__std_util_module19();
	mercury__std_util_module20();
	mercury__std_util_module21();
	mercury__std_util_module22();
	mercury__std_util_module23();
	mercury__std_util_module24();
	mercury__std_util_module25();
	mercury__std_util_module26();
	mercury__std_util_module27();
	mercury__std_util_module28();
	mercury__std_util_module29();
	mercury__std_util_module30();
}

#endif

void mercury__std_util__init(void); /* suppress gcc warning */
void mercury__std_util__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__std_util_bunch_0();
#endif
}
