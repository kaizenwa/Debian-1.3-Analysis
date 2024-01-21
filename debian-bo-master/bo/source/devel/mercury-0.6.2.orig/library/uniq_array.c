/*
** Automatically generated from `uniq_array.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__uniq_array__init
ENDINIT
*/

#include "imp.h"


	typedef struct {
		Integer size;
		Word *elements;
	} UniqArrayType;



UniqArrayType *mercury_make_uniq_array(Integer size, Word item);


UniqArrayType * mercury_resize_uniq_array(UniqArrayType *old_array,
					Integer array_size, Word item);


UniqArrayType *mercury_copy_uniq_array(UniqArrayType *old_array);


Define_extern_entry(mercury__uniq_array__make_empty_array_1_0);
Define_extern_entry(mercury__uniq_array__init_3_0);
Define_extern_entry(mercury__uniq_array__max_2_0);
Define_extern_entry(mercury__uniq_array__max_2_1);
Define_extern_entry(mercury__uniq_array__size_2_0);
Define_extern_entry(mercury__uniq_array__size_2_1);
Define_extern_entry(mercury__uniq_array__in_bounds_2_0);
Declare_label(mercury__uniq_array__in_bounds_2_0_i2);
Declare_label(mercury__uniq_array__in_bounds_2_0_i3);
Declare_label(mercury__uniq_array__in_bounds_2_0_i1);
Declare_label(mercury__uniq_array__in_bounds_2_0_i1000);
Define_extern_entry(mercury__uniq_array__in_bounds_2_1);
Declare_label(mercury__uniq_array__in_bounds_2_1_i2);
Declare_label(mercury__uniq_array__in_bounds_2_1_i3);
Declare_label(mercury__uniq_array__in_bounds_2_1_i1);
Declare_label(mercury__uniq_array__in_bounds_2_1_i1000);
Define_extern_entry(mercury__uniq_array__lookup_3_0);
Define_extern_entry(mercury__uniq_array__lookup_3_1);
Define_extern_entry(mercury__uniq_array__semidet_lookup_3_0);
Declare_label(mercury__uniq_array__semidet_lookup_3_0_i2);
Declare_label(mercury__uniq_array__semidet_lookup_3_0_i4);
Declare_label(mercury__uniq_array__semidet_lookup_3_0_i1);
Define_extern_entry(mercury__uniq_array__semidet_lookup_3_1);
Declare_label(mercury__uniq_array__semidet_lookup_3_1_i2);
Declare_label(mercury__uniq_array__semidet_lookup_3_1_i4);
Declare_label(mercury__uniq_array__semidet_lookup_3_1_i1);
Define_extern_entry(mercury__uniq_array__set_4_0);
Define_extern_entry(mercury__uniq_array__semidet_set_4_0);
Declare_label(mercury__uniq_array__semidet_set_4_0_i2);
Declare_label(mercury__uniq_array__semidet_set_4_0_i4);
Declare_label(mercury__uniq_array__semidet_set_4_0_i1);
Define_extern_entry(mercury__uniq_array__slow_set_4_0);
Declare_label(mercury__uniq_array__slow_set_4_0_i2);
Define_extern_entry(mercury__uniq_array__slow_set_4_1);
Declare_label(mercury__uniq_array__slow_set_4_1_i2);
Define_extern_entry(mercury__uniq_array__semidet_slow_set_4_0);
Declare_label(mercury__uniq_array__semidet_slow_set_4_0_i2);
Declare_label(mercury__uniq_array__semidet_slow_set_4_0_i4);
Declare_label(mercury__uniq_array__semidet_slow_set_4_0_i1);
Define_extern_entry(mercury__uniq_array__semidet_slow_set_4_1);
Declare_label(mercury__uniq_array__semidet_slow_set_4_1_i2);
Declare_label(mercury__uniq_array__semidet_slow_set_4_1_i4);
Declare_label(mercury__uniq_array__semidet_slow_set_4_1_i1);
Define_extern_entry(mercury__uniq_array__copy_2_0);
Define_extern_entry(mercury__uniq_array__copy_2_1);
Define_extern_entry(mercury__uniq_array__resize_4_0);
Define_extern_entry(mercury__uniq_array__from_list_2_0);
Declare_label(mercury__uniq_array__from_list_2_0_i4);
Declare_label(mercury__uniq_array__from_list_2_0_i5);
Declare_label(mercury__uniq_array__from_list_2_0_i1003);
Define_extern_entry(mercury__uniq_array__to_list_2_0);
Declare_label(mercury__uniq_array__to_list_2_0_i2);
Declare_label(mercury__uniq_array__to_list_2_0_i3);
Define_extern_entry(mercury__uniq_array__fetch_items_4_0);
Declare_label(mercury__uniq_array__fetch_items_4_0_i1000);
Declare_label(mercury__uniq_array__fetch_items_4_0_i4);
Declare_label(mercury__uniq_array__fetch_items_4_0_i5);
Define_extern_entry(mercury__uniq_array__bsearch_4_0);
Declare_label(mercury__uniq_array__bsearch_4_0_i2);
Declare_label(mercury__uniq_array__bsearch_4_0_i3);
Define_extern_entry(mercury__uniq_array__bsearch_4_1);
Declare_label(mercury__uniq_array__bsearch_4_1_i2);
Declare_label(mercury__uniq_array__bsearch_4_1_i3);
Declare_static(mercury__uniq_array__min_2_0);
Declare_static(mercury__uniq_array__min_2_1);
Declare_static(mercury__uniq_array__insert_items_4_0);
Declare_label(mercury__uniq_array__insert_items_4_0_i4);
Declare_label(mercury__uniq_array__insert_items_4_0_i1002);
Declare_static(mercury__uniq_array__bsearch_2_6_0);
Declare_label(mercury__uniq_array__bsearch_2_6_0_i1001);
Declare_label(mercury__uniq_array__bsearch_2_6_0_i7);
Declare_label(mercury__uniq_array__bsearch_2_6_0_i10);
Declare_label(mercury__uniq_array__bsearch_2_6_0_i9);
Declare_label(mercury__uniq_array__bsearch_2_6_0_i4);
Declare_label(mercury__uniq_array__bsearch_2_6_0_i12);
Declare_label(mercury__uniq_array__bsearch_2_6_0_i13);
Declare_label(mercury__uniq_array__bsearch_2_6_0_i15);
Declare_label(mercury__uniq_array__bsearch_2_6_0_i17);

UniqArrayType *
mercury_copy_uniq_array(UniqArrayType *old_array)
{
	Integer i;
	Word *array_elements;
	UniqArrayType* array;
	Integer array_size;
	Word *old_array_elements;

	array_size = old_array->size;
	old_array_elements = old_array->elements;

	array_elements = make_many(Word, array_size);
	for (i = 0; i < array_size; i++) {
		array_elements[i] = old_array_elements[i];
	}

	array = make(UniqArrayType);
	array->elements = array_elements;
	array->size = array_size;
	return array;
}


UniqArrayType *
mercury_resize_uniq_array(UniqArrayType *old_array, Integer array_size,
				Word item)
{
	Integer i;
	Word *array_elements;
	UniqArrayType* array;
	Integer old_array_size;
	Word *old_array_elements;

	old_array_size = old_array->size;
	if (old_array_size == array_size) return old_array;
	old_array_elements = old_array->elements;

	array_elements = make_many(Word, array_size);
	for (i = 0; i < old_array_size; i++) {
		array_elements[i] = old_array_elements[i];
	}
	for (; i < array_size; i++) {
		array_elements[i] = item;
	}

	/*
	** since the mode on the old array is `uniq_array_di', it is safe to
	** deallocate the storage for it
	*/
	oldmem(old_array->elements);
	oldmem(old_array);

	array = make(UniqArrayType);
	array->elements = array_elements;
	array->size = array_size;
	return array;
}


UniqArrayType *
mercury_make_uniq_array(Integer size, Word item)
{
	Integer i;
	Word *array_elements;
	UniqArrayType *array;

	array_elements = make_many(Word, size);
	for (i = 0; i < size; i++) {
		array_elements[i] = item;
	}
	array = make(UniqArrayType);
	array->elements = array_elements;
	array->size = size;
	return array;
}



Define_extern_entry(mercury____Unify___uniq_array__uniq_array_1_0);
Define_extern_entry(mercury____Index___uniq_array__uniq_array_1_0);
Define_extern_entry(mercury____Compare___uniq_array__uniq_array_1_0);
Define_extern_entry(mercury____TermToType___uniq_array__uniq_array_1_0);
Define_extern_entry(mercury____TypeToTerm___uniq_array__uniq_array_1_0);

#ifdef  USE_TYPE_LAYOUT

	/* This isn't really an integer, but we don't yet have a way of
	 * describing C types.
	 */

Word * mercury_data_uniq_array__base_type_layout_uniq_array_1[] = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_INT_VALUE))
};

#endif

BEGIN_MODULE(uniq_array_module)
	init_entry(mercury____Unify___uniq_array__uniq_array_1_0);
	init_entry(mercury____Index___uniq_array__uniq_array_1_0);
	init_entry(mercury____Compare___uniq_array__uniq_array_1_0);
	init_entry(mercury____TermToType___uniq_array__uniq_array_1_0);
	init_entry(mercury____TypeToTerm___uniq_array__uniq_array_1_0);
BEGIN_CODE

Define_entry(mercury____Unify___uniq_array__uniq_array_1_0);
	fatal_error("cannot unify uniq_arrays");

Define_entry(mercury____Index___uniq_array__uniq_array_1_0);
	fatal_error("cannot index uniq_array");

Define_entry(mercury____Compare___uniq_array__uniq_array_1_0);
	fatal_error("cannot compare uniq_arrays");

Define_entry(mercury____TermToType___uniq_array__uniq_array_1_0);
	fatal_error("cannot term_to_type uniq_array");

Define_entry(mercury____TypeToTerm___uniq_array__uniq_array_1_0);
	fatal_error("cannot type_to_term uniq_array");

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_uniq_array_module
*/
void sys_init_uniq_array_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_uniq_array_module(void) {
	extern ModuleFunc uniq_array_module;
	uniq_array_module();
}



Declare_entry(mercury____Unify___uniq_array__uniq_array_1_0);
Declare_entry(mercury____Index___uniq_array__uniq_array_1_0);
Declare_entry(mercury____Compare___uniq_array__uniq_array_1_0);
extern Word * mercury_data_uniq_array__base_type_layout_uniq_array_1[];
Word * mercury_data_uniq_array__base_type_info_uniq_array_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___uniq_array__uniq_array_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___uniq_array__uniq_array_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___uniq_array__uniq_array_1_0),
	(Word *) (Integer) mercury_data_uniq_array__base_type_layout_uniq_array_1
};

BEGIN_MODULE(mercury__uniq_array_module0)
	init_entry(mercury__uniq_array__make_empty_array_1_0);
BEGIN_CODE

/* code for predicate 'uniq_array__make_empty_array'/1 in mode 0 */
Define_entry(mercury__uniq_array__make_empty_array_1_0);
	{
		Word	TypeInfo_for_T;
		Word	UniqArray;
		TypeInfo_for_T = (Integer) r1;
		
	UniqArray = (Word) mercury_make_uniq_array(0, 0);

		r2 = UniqArray;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module1)
	init_entry(mercury__uniq_array__init_3_0);
BEGIN_CODE

/* code for predicate 'uniq_array__init'/3 in mode 0 */
Define_entry(mercury__uniq_array__init_3_0);
	{
		Word	TypeInfo_for_T;
		Integer	Size;
		Word	Item;
		Word	UniqArray;
		TypeInfo_for_T = (Integer) r1;
		Size = (Integer) r2;
		Item = (Integer) r3;
		
	UniqArray = (Word) mercury_make_uniq_array(Size, Item);

		r4 = UniqArray;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module2)
	init_entry(mercury__uniq_array__max_2_0);
BEGIN_CODE

/* code for predicate 'uniq_array__max'/2 in mode 0 */
Define_entry(mercury__uniq_array__max_2_0);
	{
		Word	TypeInfo_for__T;
		Word	UniqArray;
		Integer	Max;
		TypeInfo_for__T = (Integer) r1;
		UniqArray = (Integer) r2;
		
	Max = ((UniqArrayType *)UniqArray)->size - 1;

		r3 = Max;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module3)
	init_entry(mercury__uniq_array__max_2_1);
BEGIN_CODE

/* code for predicate 'uniq_array__max'/2 in mode 1 */
Define_entry(mercury__uniq_array__max_2_1);
	{
		Word	TypeInfo_for__T;
		Word	UniqArray;
		Integer	Max;
		TypeInfo_for__T = (Integer) r1;
		UniqArray = (Integer) r2;
		
	Max = ((UniqArrayType *)UniqArray)->size - 1;

		r3 = Max;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module4)
	init_entry(mercury__uniq_array__size_2_0);
BEGIN_CODE

/* code for predicate 'uniq_array__size'/2 in mode 0 */
Define_entry(mercury__uniq_array__size_2_0);
	{
		Word	TypeInfo_for__T;
		Word	UniqArray;
		Integer	Max;
		TypeInfo_for__T = (Integer) r1;
		UniqArray = (Integer) r2;
		
	Max = ((UniqArrayType *)UniqArray)->size;

		r3 = Max;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module5)
	init_entry(mercury__uniq_array__size_2_1);
BEGIN_CODE

/* code for predicate 'uniq_array__size'/2 in mode 1 */
Define_entry(mercury__uniq_array__size_2_1);
	{
		Word	TypeInfo_for__T;
		Word	UniqArray;
		Integer	Max;
		TypeInfo_for__T = (Integer) r1;
		UniqArray = (Integer) r2;
		
	Max = ((UniqArrayType *)UniqArray)->size;

		r3 = Max;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module6)
	init_entry(mercury__uniq_array__in_bounds_2_0);
	init_label(mercury__uniq_array__in_bounds_2_0_i2);
	init_label(mercury__uniq_array__in_bounds_2_0_i3);
	init_label(mercury__uniq_array__in_bounds_2_0_i1);
	init_label(mercury__uniq_array__in_bounds_2_0_i1000);
BEGIN_CODE

/* code for predicate 'uniq_array__in_bounds'/2 in mode 0 */
Define_entry(mercury__uniq_array__in_bounds_2_0);
	incr_sp_push_msg(4, "uniq_array__in_bounds");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	call_localret(STATIC(mercury__uniq_array__min_2_0),
		mercury__uniq_array__in_bounds_2_0_i2,
		ENTRY(mercury__uniq_array__in_bounds_2_0));
Define_label(mercury__uniq_array__in_bounds_2_0_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__in_bounds_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__uniq_array__max_2_0),
		mercury__uniq_array__in_bounds_2_0_i3,
		ENTRY(mercury__uniq_array__in_bounds_2_0));
	}
Define_label(mercury__uniq_array__in_bounds_2_0_i3);
	update_prof_current_proc(LABEL(mercury__uniq_array__in_bounds_2_0));
	r2 = (Integer) detstackvar(2);
	if (((Integer) detstackvar(1) > (Integer) r2))
		GOTO_LABEL(mercury__uniq_array__in_bounds_2_0_i1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (((Integer) r2 > (Integer) r1))
		GOTO_LABEL(mercury__uniq_array__in_bounds_2_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__uniq_array__in_bounds_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__uniq_array__in_bounds_2_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module7)
	init_entry(mercury__uniq_array__in_bounds_2_1);
	init_label(mercury__uniq_array__in_bounds_2_1_i2);
	init_label(mercury__uniq_array__in_bounds_2_1_i3);
	init_label(mercury__uniq_array__in_bounds_2_1_i1);
	init_label(mercury__uniq_array__in_bounds_2_1_i1000);
BEGIN_CODE

/* code for predicate 'uniq_array__in_bounds'/2 in mode 1 */
Define_entry(mercury__uniq_array__in_bounds_2_1);
	incr_sp_push_msg(4, "uniq_array__in_bounds");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	call_localret(STATIC(mercury__uniq_array__min_2_1),
		mercury__uniq_array__in_bounds_2_1_i2,
		ENTRY(mercury__uniq_array__in_bounds_2_1));
Define_label(mercury__uniq_array__in_bounds_2_1_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__in_bounds_2_1));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__uniq_array__max_2_1),
		mercury__uniq_array__in_bounds_2_1_i3,
		ENTRY(mercury__uniq_array__in_bounds_2_1));
	}
Define_label(mercury__uniq_array__in_bounds_2_1_i3);
	update_prof_current_proc(LABEL(mercury__uniq_array__in_bounds_2_1));
	r2 = (Integer) detstackvar(2);
	if (((Integer) detstackvar(1) > (Integer) r2))
		GOTO_LABEL(mercury__uniq_array__in_bounds_2_1_i1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (((Integer) r2 > (Integer) r1))
		GOTO_LABEL(mercury__uniq_array__in_bounds_2_1_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__uniq_array__in_bounds_2_1_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__uniq_array__in_bounds_2_1_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module8)
	init_entry(mercury__uniq_array__lookup_3_0);
BEGIN_CODE

/* code for predicate 'uniq_array__lookup'/3 in mode 0 */
Define_entry(mercury__uniq_array__lookup_3_0);
	{
		Word	TypeInfo_for_T;
		Word	UniqArray;
		Integer	Index;
		Word	Item;
		TypeInfo_for_T = (Integer) r1;
		UniqArray = (Integer) r2;
		Index = (Integer) r3;
		{
	UniqArrayType *uniq_array = (UniqArrayType *)UniqArray;
	if ((Unsigned) Index >= (Unsigned) uniq_array->size) {
		fatal_error("uniq_array__lookup: array index out of bounds");
	}
	Item = uniq_array->elements[Index];
}
		r4 = Item;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module9)
	init_entry(mercury__uniq_array__lookup_3_1);
BEGIN_CODE

/* code for predicate 'uniq_array__lookup'/3 in mode 1 */
Define_entry(mercury__uniq_array__lookup_3_1);
	{
		Word	TypeInfo_for_T;
		Word	UniqArray;
		Integer	Index;
		Word	Item;
		TypeInfo_for_T = (Integer) r1;
		UniqArray = (Integer) r2;
		Index = (Integer) r3;
		{
	UniqArrayType *uniq_array = (UniqArrayType *)UniqArray;
	if ((Unsigned) Index >= (Unsigned) uniq_array->size) {
		fatal_error("uniq_array__lookup: array index out of bounds");
	}
	Item = uniq_array->elements[Index];
}
		r4 = Item;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module10)
	init_entry(mercury__uniq_array__semidet_lookup_3_0);
	init_label(mercury__uniq_array__semidet_lookup_3_0_i2);
	init_label(mercury__uniq_array__semidet_lookup_3_0_i4);
	init_label(mercury__uniq_array__semidet_lookup_3_0_i1);
BEGIN_CODE

/* code for predicate 'uniq_array__semidet_lookup'/3 in mode 0 */
Define_entry(mercury__uniq_array__semidet_lookup_3_0);
	incr_sp_push_msg(4, "uniq_array__semidet_lookup");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	{
		call_localret(STATIC(mercury__uniq_array__in_bounds_2_0),
		mercury__uniq_array__semidet_lookup_3_0_i2,
		ENTRY(mercury__uniq_array__semidet_lookup_3_0));
	}
Define_label(mercury__uniq_array__semidet_lookup_3_0_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__semidet_lookup_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__uniq_array__semidet_lookup_3_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__uniq_array__lookup_3_0),
		mercury__uniq_array__semidet_lookup_3_0_i4,
		ENTRY(mercury__uniq_array__semidet_lookup_3_0));
	}
Define_label(mercury__uniq_array__semidet_lookup_3_0_i4);
	update_prof_current_proc(LABEL(mercury__uniq_array__semidet_lookup_3_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__uniq_array__semidet_lookup_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module11)
	init_entry(mercury__uniq_array__semidet_lookup_3_1);
	init_label(mercury__uniq_array__semidet_lookup_3_1_i2);
	init_label(mercury__uniq_array__semidet_lookup_3_1_i4);
	init_label(mercury__uniq_array__semidet_lookup_3_1_i1);
BEGIN_CODE

/* code for predicate 'uniq_array__semidet_lookup'/3 in mode 1 */
Define_entry(mercury__uniq_array__semidet_lookup_3_1);
	incr_sp_push_msg(4, "uniq_array__semidet_lookup");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	{
		call_localret(STATIC(mercury__uniq_array__in_bounds_2_1),
		mercury__uniq_array__semidet_lookup_3_1_i2,
		ENTRY(mercury__uniq_array__semidet_lookup_3_1));
	}
Define_label(mercury__uniq_array__semidet_lookup_3_1_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__semidet_lookup_3_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__uniq_array__semidet_lookup_3_1_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__uniq_array__lookup_3_1),
		mercury__uniq_array__semidet_lookup_3_1_i4,
		ENTRY(mercury__uniq_array__semidet_lookup_3_1));
	}
Define_label(mercury__uniq_array__semidet_lookup_3_1_i4);
	update_prof_current_proc(LABEL(mercury__uniq_array__semidet_lookup_3_1));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__uniq_array__semidet_lookup_3_1_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module12)
	init_entry(mercury__uniq_array__set_4_0);
BEGIN_CODE

/* code for predicate 'uniq_array__set'/4 in mode 0 */
Define_entry(mercury__uniq_array__set_4_0);
	{
		Word	TypeInfo_for_T;
		Word	UniqArray0;
		Integer	Index;
		Word	Item;
		Word	UniqArray;
		TypeInfo_for_T = (Integer) r1;
		UniqArray0 = (Integer) r2;
		Index = (Integer) r3;
		Item = (Integer) r4;
		{
	UniqArrayType *uniq_array = (UniqArrayType *)UniqArray0;
	if ((Unsigned) Index >= (Unsigned) uniq_array->size) {
		fatal_error("uniq_array__set: array index out of bounds");
	}
	uniq_array->elements[Index] = Item;	/* destructive update! */
	UniqArray = UniqArray0;
}
		r5 = UniqArray;

	}
	r1 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module13)
	init_entry(mercury__uniq_array__semidet_set_4_0);
	init_label(mercury__uniq_array__semidet_set_4_0_i2);
	init_label(mercury__uniq_array__semidet_set_4_0_i4);
	init_label(mercury__uniq_array__semidet_set_4_0_i1);
BEGIN_CODE

/* code for predicate 'uniq_array__semidet_set'/4 in mode 0 */
Define_entry(mercury__uniq_array__semidet_set_4_0);
	incr_sp_push_msg(5, "uniq_array__semidet_set");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r1;
	{
		call_localret(STATIC(mercury__uniq_array__in_bounds_2_0),
		mercury__uniq_array__semidet_set_4_0_i2,
		ENTRY(mercury__uniq_array__semidet_set_4_0));
	}
Define_label(mercury__uniq_array__semidet_set_4_0_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__semidet_set_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__uniq_array__semidet_set_4_0_i1);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__uniq_array__set_4_0),
		mercury__uniq_array__semidet_set_4_0_i4,
		ENTRY(mercury__uniq_array__semidet_set_4_0));
	}
Define_label(mercury__uniq_array__semidet_set_4_0_i4);
	update_prof_current_proc(LABEL(mercury__uniq_array__semidet_set_4_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__uniq_array__semidet_set_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module14)
	init_entry(mercury__uniq_array__slow_set_4_0);
	init_label(mercury__uniq_array__slow_set_4_0_i2);
BEGIN_CODE

/* code for predicate 'uniq_array__slow_set'/4 in mode 0 */
Define_entry(mercury__uniq_array__slow_set_4_0);
	incr_sp_push_msg(4, "uniq_array__slow_set");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r1;
	{
		call_localret(STATIC(mercury__uniq_array__copy_2_0),
		mercury__uniq_array__slow_set_4_0_i2,
		ENTRY(mercury__uniq_array__slow_set_4_0));
	}
Define_label(mercury__uniq_array__slow_set_4_0_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__slow_set_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
		tailcall(STATIC(mercury__uniq_array__set_4_0),
		ENTRY(mercury__uniq_array__slow_set_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module15)
	init_entry(mercury__uniq_array__slow_set_4_1);
	init_label(mercury__uniq_array__slow_set_4_1_i2);
BEGIN_CODE

/* code for predicate 'uniq_array__slow_set'/4 in mode 1 */
Define_entry(mercury__uniq_array__slow_set_4_1);
	incr_sp_push_msg(4, "uniq_array__slow_set");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r1;
	{
		call_localret(STATIC(mercury__uniq_array__copy_2_1),
		mercury__uniq_array__slow_set_4_1_i2,
		ENTRY(mercury__uniq_array__slow_set_4_1));
	}
Define_label(mercury__uniq_array__slow_set_4_1_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__slow_set_4_1));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
		tailcall(STATIC(mercury__uniq_array__set_4_0),
		ENTRY(mercury__uniq_array__slow_set_4_1));
	}
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module16)
	init_entry(mercury__uniq_array__semidet_slow_set_4_0);
	init_label(mercury__uniq_array__semidet_slow_set_4_0_i2);
	init_label(mercury__uniq_array__semidet_slow_set_4_0_i4);
	init_label(mercury__uniq_array__semidet_slow_set_4_0_i1);
BEGIN_CODE

/* code for predicate 'uniq_array__semidet_slow_set'/4 in mode 0 */
Define_entry(mercury__uniq_array__semidet_slow_set_4_0);
	incr_sp_push_msg(5, "uniq_array__semidet_slow_set");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r1;
	{
		call_localret(STATIC(mercury__uniq_array__in_bounds_2_0),
		mercury__uniq_array__semidet_slow_set_4_0_i2,
		ENTRY(mercury__uniq_array__semidet_slow_set_4_0));
	}
Define_label(mercury__uniq_array__semidet_slow_set_4_0_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__semidet_slow_set_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__uniq_array__semidet_slow_set_4_0_i1);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__uniq_array__slow_set_4_0),
		mercury__uniq_array__semidet_slow_set_4_0_i4,
		ENTRY(mercury__uniq_array__semidet_slow_set_4_0));
	}
Define_label(mercury__uniq_array__semidet_slow_set_4_0_i4);
	update_prof_current_proc(LABEL(mercury__uniq_array__semidet_slow_set_4_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__uniq_array__semidet_slow_set_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module17)
	init_entry(mercury__uniq_array__semidet_slow_set_4_1);
	init_label(mercury__uniq_array__semidet_slow_set_4_1_i2);
	init_label(mercury__uniq_array__semidet_slow_set_4_1_i4);
	init_label(mercury__uniq_array__semidet_slow_set_4_1_i1);
BEGIN_CODE

/* code for predicate 'uniq_array__semidet_slow_set'/4 in mode 1 */
Define_entry(mercury__uniq_array__semidet_slow_set_4_1);
	incr_sp_push_msg(5, "uniq_array__semidet_slow_set");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r1;
	{
		call_localret(STATIC(mercury__uniq_array__in_bounds_2_1),
		mercury__uniq_array__semidet_slow_set_4_1_i2,
		ENTRY(mercury__uniq_array__semidet_slow_set_4_1));
	}
Define_label(mercury__uniq_array__semidet_slow_set_4_1_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__semidet_slow_set_4_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__uniq_array__semidet_slow_set_4_1_i1);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__uniq_array__slow_set_4_1),
		mercury__uniq_array__semidet_slow_set_4_1_i4,
		ENTRY(mercury__uniq_array__semidet_slow_set_4_1));
	}
Define_label(mercury__uniq_array__semidet_slow_set_4_1_i4);
	update_prof_current_proc(LABEL(mercury__uniq_array__semidet_slow_set_4_1));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__uniq_array__semidet_slow_set_4_1_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module18)
	init_entry(mercury__uniq_array__copy_2_0);
BEGIN_CODE

/* code for predicate 'uniq_array__copy'/2 in mode 0 */
Define_entry(mercury__uniq_array__copy_2_0);
	{
		Word	TypeInfo_for_T;
		Word	UniqArray0;
		Word	UniqArray;
		TypeInfo_for_T = (Integer) r1;
		UniqArray0 = (Integer) r2;
		
	UniqArray =
		(Word) mercury_copy_uniq_array((UniqArrayType *) UniqArray0);

		r3 = UniqArray;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module19)
	init_entry(mercury__uniq_array__copy_2_1);
BEGIN_CODE

/* code for predicate 'uniq_array__copy'/2 in mode 1 */
Define_entry(mercury__uniq_array__copy_2_1);
	{
		Word	TypeInfo_for_T;
		Word	UniqArray0;
		Word	UniqArray;
		TypeInfo_for_T = (Integer) r1;
		UniqArray0 = (Integer) r2;
		
	UniqArray =
		(Word) mercury_copy_uniq_array((UniqArrayType *) UniqArray0);

		r3 = UniqArray;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module20)
	init_entry(mercury__uniq_array__resize_4_0);
BEGIN_CODE

/* code for predicate 'uniq_array__resize'/4 in mode 0 */
Define_entry(mercury__uniq_array__resize_4_0);
	{
		Word	TypeInfo_for_T;
		Word	UniqArray0;
		Integer	Size;
		Word	Item;
		Word	UniqArray;
		TypeInfo_for_T = (Integer) r1;
		UniqArray0 = (Integer) r2;
		Size = (Integer) r3;
		Item = (Integer) r4;
		
	UniqArray = (Word) mercury_resize_uniq_array(
				(UniqArrayType *) UniqArray0, Size, Item);

		r5 = UniqArray;

	}
	r1 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module21)
	init_entry(mercury__uniq_array__from_list_2_0);
	init_label(mercury__uniq_array__from_list_2_0_i4);
	init_label(mercury__uniq_array__from_list_2_0_i5);
	init_label(mercury__uniq_array__from_list_2_0_i1003);
BEGIN_CODE

/* code for predicate 'uniq_array__from_list'/2 in mode 0 */
Define_entry(mercury__uniq_array__from_list_2_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__uniq_array__from_list_2_0_i1003);
	incr_sp_push_msg(4, "uniq_array__from_list");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__uniq_array__from_list_2_0_i4,
		ENTRY(mercury__uniq_array__from_list_2_0));
	}
Define_label(mercury__uniq_array__from_list_2_0_i4);
	update_prof_current_proc(LABEL(mercury__uniq_array__from_list_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__uniq_array__init_3_0),
		mercury__uniq_array__from_list_2_0_i5,
		ENTRY(mercury__uniq_array__from_list_2_0));
	}
Define_label(mercury__uniq_array__from_list_2_0_i5);
	update_prof_current_proc(LABEL(mercury__uniq_array__from_list_2_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	r3 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__uniq_array__insert_items_4_0),
		ENTRY(mercury__uniq_array__from_list_2_0));
Define_label(mercury__uniq_array__from_list_2_0_i1003);
	{
		tailcall(STATIC(mercury__uniq_array__make_empty_array_1_0),
		ENTRY(mercury__uniq_array__from_list_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module22)
	init_entry(mercury__uniq_array__to_list_2_0);
	init_label(mercury__uniq_array__to_list_2_0_i2);
	init_label(mercury__uniq_array__to_list_2_0_i3);
BEGIN_CODE

/* code for predicate 'uniq_array__to_list'/2 in mode 0 */
Define_entry(mercury__uniq_array__to_list_2_0);
	incr_sp_push_msg(4, "uniq_array__to_list");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r1;
	call_localret(STATIC(mercury__uniq_array__min_2_0),
		mercury__uniq_array__to_list_2_0_i2,
		ENTRY(mercury__uniq_array__to_list_2_0));
Define_label(mercury__uniq_array__to_list_2_0_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__to_list_2_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__uniq_array__max_2_0),
		mercury__uniq_array__to_list_2_0_i3,
		ENTRY(mercury__uniq_array__to_list_2_0));
	}
Define_label(mercury__uniq_array__to_list_2_0_i3);
	update_prof_current_proc(LABEL(mercury__uniq_array__to_list_2_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
		tailcall(STATIC(mercury__uniq_array__fetch_items_4_0),
		ENTRY(mercury__uniq_array__to_list_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module23)
	init_entry(mercury__uniq_array__fetch_items_4_0);
	init_label(mercury__uniq_array__fetch_items_4_0_i1000);
	init_label(mercury__uniq_array__fetch_items_4_0_i4);
	init_label(mercury__uniq_array__fetch_items_4_0_i5);
BEGIN_CODE

/* code for predicate 'uniq_array__fetch_items'/4 in mode 0 */
Define_entry(mercury__uniq_array__fetch_items_4_0);
	if (((Integer) r3 <= (Integer) r4))
		GOTO_LABEL(mercury__uniq_array__fetch_items_4_0_i1000);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__uniq_array__fetch_items_4_0_i1000);
	incr_sp_push_msg(4, "uniq_array__fetch_items");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	r3 = ((Integer) r3 + ((Integer) 1));
	localcall(mercury__uniq_array__fetch_items_4_0,
		LABEL(mercury__uniq_array__fetch_items_4_0_i4),
		ENTRY(mercury__uniq_array__fetch_items_4_0));
Define_label(mercury__uniq_array__fetch_items_4_0_i4);
	update_prof_current_proc(LABEL(mercury__uniq_array__fetch_items_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__uniq_array__lookup_3_1),
		mercury__uniq_array__fetch_items_4_0_i5,
		ENTRY(mercury__uniq_array__fetch_items_4_0));
	}
Define_label(mercury__uniq_array__fetch_items_4_0_i5);
	update_prof_current_proc(LABEL(mercury__uniq_array__fetch_items_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module24)
	init_entry(mercury__uniq_array__bsearch_4_0);
	init_label(mercury__uniq_array__bsearch_4_0_i2);
	init_label(mercury__uniq_array__bsearch_4_0_i3);
BEGIN_CODE

/* code for predicate 'uniq_array__bsearch'/4 in mode 0 */
Define_entry(mercury__uniq_array__bsearch_4_0);
	incr_sp_push_msg(6, "uniq_array__bsearch");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(5) = (Integer) r1;
	call_localret(STATIC(mercury__uniq_array__min_2_0),
		mercury__uniq_array__bsearch_4_0_i2,
		ENTRY(mercury__uniq_array__bsearch_4_0));
Define_label(mercury__uniq_array__bsearch_4_0_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__bsearch_4_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__uniq_array__max_2_0),
		mercury__uniq_array__bsearch_4_0_i3,
		ENTRY(mercury__uniq_array__bsearch_4_0));
	}
Define_label(mercury__uniq_array__bsearch_4_0_i3);
	update_prof_current_proc(LABEL(mercury__uniq_array__bsearch_4_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__uniq_array__bsearch_2_6_0),
		ENTRY(mercury__uniq_array__bsearch_4_0));
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module25)
	init_entry(mercury__uniq_array__bsearch_4_1);
	init_label(mercury__uniq_array__bsearch_4_1_i2);
	init_label(mercury__uniq_array__bsearch_4_1_i3);
BEGIN_CODE

/* code for predicate 'uniq_array__bsearch'/4 in mode 1 */
Define_entry(mercury__uniq_array__bsearch_4_1);
	incr_sp_push_msg(6, "uniq_array__bsearch");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(5) = (Integer) r1;
	call_localret(STATIC(mercury__uniq_array__min_2_1),
		mercury__uniq_array__bsearch_4_1_i2,
		ENTRY(mercury__uniq_array__bsearch_4_1));
Define_label(mercury__uniq_array__bsearch_4_1_i2);
	update_prof_current_proc(LABEL(mercury__uniq_array__bsearch_4_1));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__uniq_array__max_2_1),
		mercury__uniq_array__bsearch_4_1_i3,
		ENTRY(mercury__uniq_array__bsearch_4_1));
	}
Define_label(mercury__uniq_array__bsearch_4_1_i3);
	update_prof_current_proc(LABEL(mercury__uniq_array__bsearch_4_1));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__uniq_array__bsearch_2_6_0),
		ENTRY(mercury__uniq_array__bsearch_4_1));
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module26)
	init_entry(mercury__uniq_array__min_2_0);
BEGIN_CODE

/* code for predicate 'uniq_array__min'/2 in mode 0 */
Define_static(mercury__uniq_array__min_2_0);
	{
		Word	TypeInfo_for__T;
		Word	UniqArray;
		Integer	Min;
		TypeInfo_for__T = (Integer) r1;
		UniqArray = (Integer) r2;
		
	/* UniqArray not used */
	Min = 0;

		r3 = Min;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module27)
	init_entry(mercury__uniq_array__min_2_1);
BEGIN_CODE

/* code for predicate 'uniq_array__min'/2 in mode 1 */
Define_static(mercury__uniq_array__min_2_1);
	{
		Word	TypeInfo_for__T;
		Word	UniqArray;
		Integer	Min;
		TypeInfo_for__T = (Integer) r1;
		UniqArray = (Integer) r2;
		
	/* UniqArray not used */
	Min = 0;

		r3 = Min;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module28)
	init_entry(mercury__uniq_array__insert_items_4_0);
	init_label(mercury__uniq_array__insert_items_4_0_i4);
	init_label(mercury__uniq_array__insert_items_4_0_i1002);
BEGIN_CODE

/* code for predicate 'uniq_array__insert_items'/4 in mode 0 */
Define_static(mercury__uniq_array__insert_items_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__uniq_array__insert_items_4_0_i1002);
	incr_sp_push_msg(4, "uniq_array__insert_items");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	detstackvar(1) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	{
		call_localret(STATIC(mercury__uniq_array__set_4_0),
		mercury__uniq_array__insert_items_4_0_i4,
		STATIC(mercury__uniq_array__insert_items_4_0));
	}
	}
Define_label(mercury__uniq_array__insert_items_4_0_i4);
	update_prof_current_proc(LABEL(mercury__uniq_array__insert_items_4_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	r3 = ((Integer) detstackvar(1) + ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__uniq_array__insert_items_4_0,
		STATIC(mercury__uniq_array__insert_items_4_0));
Define_label(mercury__uniq_array__insert_items_4_0_i1002);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__uniq_array_module29)
	init_entry(mercury__uniq_array__bsearch_2_6_0);
	init_label(mercury__uniq_array__bsearch_2_6_0_i1001);
	init_label(mercury__uniq_array__bsearch_2_6_0_i7);
	init_label(mercury__uniq_array__bsearch_2_6_0_i10);
	init_label(mercury__uniq_array__bsearch_2_6_0_i9);
	init_label(mercury__uniq_array__bsearch_2_6_0_i4);
	init_label(mercury__uniq_array__bsearch_2_6_0_i12);
	init_label(mercury__uniq_array__bsearch_2_6_0_i13);
	init_label(mercury__uniq_array__bsearch_2_6_0_i15);
	init_label(mercury__uniq_array__bsearch_2_6_0_i17);
BEGIN_CODE

/* code for predicate 'uniq_array__bsearch_2'/6 in mode 0 */
Define_static(mercury__uniq_array__bsearch_2_6_0);
	r7 = ((Integer) r4 - (Integer) r3);
	r8 = (Integer) r7;
	if (((Integer) r8 >= ((Integer) 0)))
		GOTO_LABEL(mercury__uniq_array__bsearch_2_6_0_i1001);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__uniq_array__bsearch_2_6_0_i1001);
	incr_sp_push_msg(8, "uniq_array__bsearch_2");
	detstackvar(8) = (Integer) succip;
	if (((Integer) r7 != ((Integer) 0)))
		GOTO_LABEL(mercury__uniq_array__bsearch_2_6_0_i4);
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	{
		call_localret(STATIC(mercury__uniq_array__lookup_3_1),
		mercury__uniq_array__bsearch_2_6_0_i7,
		STATIC(mercury__uniq_array__bsearch_2_6_0));
	}
Define_label(mercury__uniq_array__bsearch_2_6_0_i7);
	update_prof_current_proc(LABEL(mercury__uniq_array__bsearch_2_6_0));
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = ((Integer) 2);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_det_closure);
	call_localret(ENTRY(do_call_det_closure),
		mercury__uniq_array__bsearch_2_6_0_i10,
		STATIC(mercury__uniq_array__bsearch_2_6_0));
	}
Define_label(mercury__uniq_array__bsearch_2_6_0_i10);
	update_prof_current_proc(LABEL(mercury__uniq_array__bsearch_2_6_0));
	if ((((Integer) 0) != (Integer) r1))
		GOTO_LABEL(mercury__uniq_array__bsearch_2_6_0_i9);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__uniq_array__bsearch_2_6_0_i9);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__uniq_array__bsearch_2_6_0_i4);
	detstackvar(2) = (Integer) r3;
	r3 = (((Integer) r3 + (Integer) r4) >> ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	detstackvar(6) = (Integer) r3;
	detstackvar(7) = (Integer) r1;
	{
		call_localret(STATIC(mercury__uniq_array__lookup_3_1),
		mercury__uniq_array__bsearch_2_6_0_i12,
		STATIC(mercury__uniq_array__bsearch_2_6_0));
	}
Define_label(mercury__uniq_array__bsearch_2_6_0_i12);
	update_prof_current_proc(LABEL(mercury__uniq_array__bsearch_2_6_0));
	r4 = (Integer) r1;
	r5 = (Integer) detstackvar(4);
	r1 = (Integer) detstackvar(5);
	r2 = ((Integer) 2);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_det_closure);
	call_localret(ENTRY(do_call_det_closure),
		mercury__uniq_array__bsearch_2_6_0_i13,
		STATIC(mercury__uniq_array__bsearch_2_6_0));
	}
Define_label(mercury__uniq_array__bsearch_2_6_0_i13);
	update_prof_current_proc(LABEL(mercury__uniq_array__bsearch_2_6_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__uniq_array__bsearch_2_6_0_i15);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(6);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__uniq_array__bsearch_2_6_0,
		STATIC(mercury__uniq_array__bsearch_2_6_0));
Define_label(mercury__uniq_array__bsearch_2_6_0_i15);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__uniq_array__bsearch_2_6_0_i17);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = ((Integer) detstackvar(6) + ((Integer) 1));
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__uniq_array__bsearch_2_6_0,
		STATIC(mercury__uniq_array__bsearch_2_6_0));
Define_label(mercury__uniq_array__bsearch_2_6_0_i17);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = ((Integer) detstackvar(6) - ((Integer) 1));
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__uniq_array__bsearch_2_6_0,
		STATIC(mercury__uniq_array__bsearch_2_6_0));
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__uniq_array_bunch_0(void)
{
	mercury__uniq_array_module0();
	mercury__uniq_array_module1();
	mercury__uniq_array_module2();
	mercury__uniq_array_module3();
	mercury__uniq_array_module4();
	mercury__uniq_array_module5();
	mercury__uniq_array_module6();
	mercury__uniq_array_module7();
	mercury__uniq_array_module8();
	mercury__uniq_array_module9();
	mercury__uniq_array_module10();
	mercury__uniq_array_module11();
	mercury__uniq_array_module12();
	mercury__uniq_array_module13();
	mercury__uniq_array_module14();
	mercury__uniq_array_module15();
	mercury__uniq_array_module16();
	mercury__uniq_array_module17();
	mercury__uniq_array_module18();
	mercury__uniq_array_module19();
	mercury__uniq_array_module20();
	mercury__uniq_array_module21();
	mercury__uniq_array_module22();
	mercury__uniq_array_module23();
	mercury__uniq_array_module24();
	mercury__uniq_array_module25();
	mercury__uniq_array_module26();
	mercury__uniq_array_module27();
	mercury__uniq_array_module28();
	mercury__uniq_array_module29();
}

#endif

void mercury__uniq_array__init(void); /* suppress gcc warning */
void mercury__uniq_array__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__uniq_array_bunch_0();
#endif
}
