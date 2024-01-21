/*
** Automatically generated from `parser.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__parser__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__parser__error__ua10000_4_0);
Declare_static(mercury__parser__unexpected_tok__ua10000_6_0);
Declare_label(mercury__parser__unexpected_tok__ua10000_6_0_i1002);
Declare_label(mercury__parser__unexpected_tok__ua10000_6_0_i4);
Declare_label(mercury__parser__unexpected_tok__ua10000_6_0_i7);
Declare_label(mercury__parser__unexpected_tok__ua10000_6_0_i11);
Declare_label(mercury__parser__unexpected_tok__ua10000_6_0_i10);
Declare_label(mercury__parser__unexpected_tok__ua10000_6_0_i13);
Declare_label(mercury__parser__unexpected_tok__ua10000_6_0_i8);
Declare_label(mercury__parser__unexpected_tok__ua10000_6_0_i3);
Declare_label(mercury__parser__unexpected_tok__ua10000_6_0_i2);
Declare_static(mercury__parser__unexpected__ua10000_4_0);
Declare_label(mercury__parser__unexpected__ua10000_4_0_i4);
Declare_label(mercury__parser__unexpected__ua10000_4_0_i3);
Define_extern_entry(mercury__parser__read_term_3_0);
Declare_label(mercury__parser__read_term_3_0_i2);
Define_extern_entry(mercury__parser__read_term_4_0);
Declare_label(mercury__parser__read_term_4_0_i2);
Declare_label(mercury__parser__read_term_4_0_i3);
Declare_label(mercury__parser__read_term_4_0_i6);
Declare_label(mercury__parser__read_term_4_0_i7);
Declare_label(mercury__parser__read_term_4_0_i8);
Declare_label(mercury__parser__read_term_4_0_i9);
Declare_label(mercury__parser__read_term_4_0_i15);
Declare_label(mercury__parser__read_term_4_0_i17);
Declare_label(mercury__parser__read_term_4_0_i14);
Declare_label(mercury__parser__read_term_4_0_i19);
Declare_label(mercury__parser__read_term_4_0_i10);
Declare_label(mercury__parser__read_term_4_0_i21);
Declare_label(mercury__parser__read_term_4_0_i26);
Declare_label(mercury__parser__read_term_4_0_i25);
Declare_label(mercury__parser__read_term_4_0_i31);
Declare_label(mercury__parser__read_term_4_0_i32);
Declare_label(mercury__parser__read_term_4_0_i28);
Declare_label(mercury__parser__read_term_4_0_i33);
Declare_label(mercury__parser__read_term_4_0_i36);
Declare_label(mercury__parser__read_term_4_0_i37);
Declare_label(mercury__parser__read_term_4_0_i38);
Declare_label(mercury__parser__read_term_4_0_i23);
Declare_label(mercury__parser__read_term_4_0_i43);
Declare_label(mercury__parser__read_term_4_0_i42);
Declare_label(mercury__parser__read_term_4_0_i48);
Declare_label(mercury__parser__read_term_4_0_i1037);
Declare_static(mercury__parser__check_for_bad_token_3_0);
Declare_label(mercury__parser__check_for_bad_token_3_0_i6);
Declare_label(mercury__parser__check_for_bad_token_3_0_i7);
Declare_label(mercury__parser__check_for_bad_token_3_0_i1001);
Declare_label(mercury__parser__check_for_bad_token_3_0_i3);
Declare_label(mercury__parser__check_for_bad_token_3_0_i11);
Declare_label(mercury__parser__check_for_bad_token_3_0_i12);
Declare_label(mercury__parser__check_for_bad_token_3_0_i13);
Declare_label(mercury__parser__check_for_bad_token_3_0_i8);
Declare_label(mercury__parser__check_for_bad_token_3_0_i15);
Declare_label(mercury__parser__check_for_bad_token_3_0_i19);
Declare_label(mercury__parser__check_for_bad_token_3_0_i1);
Declare_label(mercury__parser__check_for_bad_token_3_0_i1000);
Declare_static(mercury__parser__parse_term_3_0);
Declare_static(mercury__parser__parse_term_2_5_0);
Declare_label(mercury__parser__parse_term_2_5_0_i2);
Declare_label(mercury__parser__parse_term_2_5_0_i3);
Declare_static(mercury__parser__parse_left_term_6_0);
Declare_label(mercury__parser__parse_left_term_6_0_i4);
Declare_label(mercury__parser__parse_left_term_6_0_i10);
Declare_label(mercury__parser__parse_left_term_6_0_i13);
Declare_label(mercury__parser__parse_left_term_6_0_i7);
Declare_label(mercury__parser__parse_left_term_6_0_i6);
Declare_label(mercury__parser__parse_left_term_6_0_i18);
Declare_label(mercury__parser__parse_left_term_6_0_i21);
Declare_label(mercury__parser__parse_left_term_6_0_i15);
Declare_label(mercury__parser__parse_left_term_6_0_i14);
Declare_label(mercury__parser__parse_left_term_6_0_i27);
Declare_label(mercury__parser__parse_left_term_6_0_i29);
Declare_label(mercury__parser__parse_left_term_6_0_i26);
Declare_label(mercury__parser__parse_left_term_6_0_i31);
Declare_label(mercury__parser__parse_left_term_6_0_i32);
Declare_label(mercury__parser__parse_left_term_6_0_i34);
Declare_label(mercury__parser__parse_left_term_6_0_i36);
Declare_label(mercury__parser__parse_left_term_6_0_i37);
Declare_label(mercury__parser__parse_left_term_6_0_i38);
Declare_label(mercury__parser__parse_left_term_6_0_i39);
Declare_label(mercury__parser__parse_left_term_6_0_i43);
Declare_label(mercury__parser__parse_left_term_6_0_i47);
Declare_label(mercury__parser__parse_left_term_6_0_i44);
Declare_label(mercury__parser__parse_left_term_6_0_i23);
Declare_label(mercury__parser__parse_left_term_6_0_i22);
Declare_label(mercury__parser__parse_left_term_6_0_i55);
Declare_label(mercury__parser__parse_left_term_6_0_i57);
Declare_label(mercury__parser__parse_left_term_6_0_i54);
Declare_label(mercury__parser__parse_left_term_6_0_i59);
Declare_label(mercury__parser__parse_left_term_6_0_i60);
Declare_label(mercury__parser__parse_left_term_6_0_i62);
Declare_label(mercury__parser__parse_left_term_6_0_i64);
Declare_label(mercury__parser__parse_left_term_6_0_i65);
Declare_label(mercury__parser__parse_left_term_6_0_i66);
Declare_label(mercury__parser__parse_left_term_6_0_i70);
Declare_label(mercury__parser__parse_left_term_6_0_i51);
Declare_label(mercury__parser__parse_left_term_6_0_i50);
Declare_label(mercury__parser__parse_left_term_6_0_i72);
Declare_label(mercury__parser__parse_left_term_6_0_i3);
Declare_static(mercury__parser__parse_rest_7_0);
Declare_label(mercury__parser__parse_rest_7_0_i4);
Declare_label(mercury__parser__parse_rest_7_0_i6);
Declare_label(mercury__parser__parse_rest_7_0_i11);
Declare_label(mercury__parser__parse_rest_7_0_i12);
Declare_label(mercury__parser__parse_rest_7_0_i13);
Declare_label(mercury__parser__parse_rest_7_0_i16);
Declare_label(mercury__parser__parse_rest_7_0_i15);
Declare_label(mercury__parser__parse_rest_7_0_i17);
Declare_label(mercury__parser__parse_rest_7_0_i18);
Declare_label(mercury__parser__parse_rest_7_0_i22);
Declare_label(mercury__parser__parse_rest_7_0_i19);
Declare_label(mercury__parser__parse_rest_7_0_i3);
Declare_label(mercury__parser__parse_rest_7_0_i27);
Declare_label(mercury__parser__parse_rest_7_0_i30);
Declare_label(mercury__parser__parse_rest_7_0_i31);
Declare_label(mercury__parser__parse_rest_7_0_i34);
Declare_label(mercury__parser__parse_rest_7_0_i33);
Declare_label(mercury__parser__parse_rest_7_0_i35);
Declare_label(mercury__parser__parse_rest_7_0_i1047);
Declare_static(mercury__parser__parse_simple_term_6_0);
Declare_label(mercury__parser__parse_simple_term_6_0_i4);
Declare_label(mercury__parser__parse_simple_term_6_0_i3);
Declare_static(mercury__parser__parse_simple_term_2_6_0);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i1111);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i1110);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i1109);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i1108);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i5);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i6);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i12);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i14);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i11);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i16);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i19);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i20);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i22);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i25);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i27);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i29);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i24);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i32);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i33);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i36);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i38);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i35);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i40);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i46);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i48);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i45);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i1107);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i55);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i56);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i57);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i58);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i59);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i60);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i54);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i62);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i65);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i67);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i69);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i71);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i64);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i72);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i75);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i74);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i61);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i80);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i83);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i85);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i87);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i88);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i90);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i1103);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i1);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i1105);
Declare_label(mercury__parser__parse_simple_term_2_6_0_i1106);
Declare_static(mercury__parser__parse_list_3_0);
Declare_label(mercury__parser__parse_list_3_0_i2);
Declare_label(mercury__parser__parse_list_3_0_i8);
Declare_label(mercury__parser__parse_list_3_0_i10);
Declare_label(mercury__parser__parse_list_3_0_i14);
Declare_label(mercury__parser__parse_list_3_0_i11);
Declare_label(mercury__parser__parse_list_3_0_i22);
Declare_label(mercury__parser__parse_list_3_0_i28);
Declare_label(mercury__parser__parse_list_3_0_i30);
Declare_label(mercury__parser__parse_list_3_0_i27);
Declare_label(mercury__parser__parse_list_3_0_i19);
Declare_label(mercury__parser__parse_list_3_0_i35);
Declare_label(mercury__parser__parse_list_3_0_i7);
Declare_label(mercury__parser__parse_list_3_0_i44);
Declare_static(mercury__parser__parse_args_3_0);
Declare_label(mercury__parser__parse_args_3_0_i2);
Declare_label(mercury__parser__parse_args_3_0_i7);
Declare_label(mercury__parser__parse_args_3_0_i12);
Declare_label(mercury__parser__parse_args_3_0_i9);
Declare_label(mercury__parser__parse_args_3_0_i17);
Declare_label(mercury__parser__parse_args_3_0_i6);
Declare_label(mercury__parser__parse_args_3_0_i3);
Declare_static(mercury__parser__could_start_term_2_0);
Declare_label(mercury__parser__could_start_term_2_0_i5);
Declare_label(mercury__parser__could_start_term_2_0_i7);
Declare_label(mercury__parser__could_start_term_2_0_i4);
Declare_label(mercury__parser__could_start_term_2_0_i16);
Declare_label(mercury__parser__could_start_term_2_0_i23);
Declare_static(mercury__parser__get_token_3_0);
Declare_label(mercury__parser__get_token_3_0_i2);
Declare_label(mercury__parser__get_token_3_0_i1000);
Declare_static(mercury__parser__get_token_4_0);
Declare_label(mercury__parser__get_token_4_0_i1);
Declare_static(mercury__parser__peek_token_3_0);
Declare_label(mercury__parser__peek_token_3_0_i1);
Declare_static(mercury__parser__add_var_4_0);
Declare_label(mercury__parser__add_var_4_0_i5);
Declare_label(mercury__parser__add_var_4_0_i2);
Declare_label(mercury__parser__add_var_4_0_i8);
Declare_label(mercury__parser__add_var_4_0_i7);
Declare_label(mercury__parser__add_var_4_0_i10);
Declare_label(mercury__parser__add_var_4_0_i11);
Declare_label(mercury__parser__add_var_4_0_i12);
Declare_static(mercury__parser__get_ops_table_3_0);
Declare_static(mercury__parser__adjust_priority_3_0);
Declare_label(mercury__parser__adjust_priority_3_0_i3);
Declare_static(mercury__parser__get_term_context_4_0);
Declare_label(mercury__parser__get_term_context_4_0_i2);

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_parser__base_type_layout_parse_1[];
Word * mercury_data_parser__base_type_info_parse_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_parser__base_type_layout_parse_1
};

extern Word * mercury_data_parser__base_type_layout_parser__state_0[];
Word * mercury_data_parser__base_type_info_parser__state_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_parser__base_type_layout_parser__state_0
};

extern Word * mercury_data_parser__common_11[];
Word * mercury_data_parser__base_type_layout_parser__state_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_parser__common_11),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_parser__common_12[];
extern Word * mercury_data_parser__common_13[];
Word * mercury_data_parser__base_type_layout_parse_1[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_parser__common_12),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_parser__common_13),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

Word * mercury_data_parser__common_0[] = {
	(Word *) string_const(") in input", 10),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_parser__common_1[] = {
	(Word *) string_const("[]", 2)
};

Word * mercury_data_parser__common_2[] = {
	(Word *) string_const("{}", 2)
};

Word * mercury_data_parser__common_3[] = {
	(Word *) string_const("", 0)
};

Word * mercury_data_parser__common_4[] = {
	(Word *) string_const(".", 1)
};

extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_parser__common_5[] = {
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

extern Word * mercury_data_ops__base_type_info_ops__table_0[];
Word * mercury_data_parser__common_6[] = {
	(Word *) (Integer) mercury_data_ops__base_type_info_ops__table_0
};

extern Word * mercury_data_varset__base_type_info_varset_0[];
Word * mercury_data_parser__common_7[] = {
	(Word *) (Integer) mercury_data_varset__base_type_info_varset_0
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_lexer__base_type_info_token_0[];
extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_parser__common_8[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_lexer__base_type_info_token_0,
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
Word * mercury_data_parser__common_9[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_8)
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data_mercury_builtin__base_type_info_var_0[];
Word * mercury_data_parser__common_10[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data___base_type_info_string_0,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_var_0
};

Word * mercury_data_parser__common_11[] = {
	(Word *) ((Integer) 5),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_5),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_6),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_7),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_9),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_10),
	(Word *) string_const("parser__state", 13)
};

Word * mercury_data_parser__common_12[] = {
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 1),
	(Word *) string_const("ok", 2)
};

Word * mercury_data_parser__common_13[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_5),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_9),
	(Word *) string_const("error", 5)
};

BEGIN_MODULE(mercury__parser_module0)
	init_entry(mercury__parser__error__ua10000_4_0);
BEGIN_CODE

/* code for predicate 'parser__error__ua10000'/4 in mode 0 */
Define_static(mercury__parser__error__ua10000_4_0);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module1)
	init_entry(mercury__parser__unexpected_tok__ua10000_6_0);
	init_label(mercury__parser__unexpected_tok__ua10000_6_0_i1002);
	init_label(mercury__parser__unexpected_tok__ua10000_6_0_i4);
	init_label(mercury__parser__unexpected_tok__ua10000_6_0_i7);
	init_label(mercury__parser__unexpected_tok__ua10000_6_0_i11);
	init_label(mercury__parser__unexpected_tok__ua10000_6_0_i10);
	init_label(mercury__parser__unexpected_tok__ua10000_6_0_i13);
	init_label(mercury__parser__unexpected_tok__ua10000_6_0_i8);
	init_label(mercury__parser__unexpected_tok__ua10000_6_0_i3);
	init_label(mercury__parser__unexpected_tok__ua10000_6_0_i2);
BEGIN_CODE

/* code for predicate 'parser__unexpected_tok__ua10000'/6 in mode 0 */
Define_static(mercury__parser__unexpected_tok__ua10000_6_0);
	tag_incr_hp(r5, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 1));
	field(mktag(0), (Integer) r5, ((Integer) 2)) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 2));
	tag_incr_hp(r6, mktag(1), ((Integer) 2));
	tag_incr_hp(r7, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r7, ((Integer) 0)) = (Integer) r1;
	field(mktag(0), (Integer) r7, ((Integer) 1)) = (Integer) r2;
	field(mktag(1), (Integer) r6, ((Integer) 0)) = (Integer) r7;
	field(mktag(1), (Integer) r6, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 3));
	field(mktag(0), (Integer) r5, ((Integer) 3)) = (Integer) r6;
	field(mktag(0), (Integer) r5, ((Integer) 4)) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 4));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 8)))))
		GOTO_LABEL(mercury__parser__unexpected_tok__ua10000_6_0_i1002);
	r2 = (Integer) r3;
	r3 = string_const(",", 1);
	r1 = (Integer) r5;
	incr_sp_push_msg(6, "parser__unexpected_tok__ua10000");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__parser__unexpected_tok__ua10000_6_0_i4);
Define_label(mercury__parser__unexpected_tok__ua10000_6_0_i1002);
	incr_sp_push_msg(6, "parser__unexpected_tok__ua10000");
	detstackvar(6) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__parser__unexpected_tok__ua10000_6_0_i2);
	r2 = (Integer) r3;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) r5;
Define_label(mercury__parser__unexpected_tok__ua10000_6_0_i4);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	call_localret(STATIC(mercury__parser__get_ops_table_3_0),
		mercury__parser__unexpected_tok__ua10000_6_0_i7,
		STATIC(mercury__parser__unexpected_tok__ua10000_6_0));
Define_label(mercury__parser__unexpected_tok__ua10000_6_0_i7);
	update_prof_current_proc(LABEL(mercury__parser__unexpected_tok__ua10000_6_0));
	detstackvar(3) = (Integer) r1;
	detstackvar(5) = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__ops__lookup_infix_op_5_0);
	call_localret(ENTRY(mercury__ops__lookup_infix_op_5_0),
		mercury__parser__unexpected_tok__ua10000_6_0_i11,
		STATIC(mercury__parser__unexpected_tok__ua10000_6_0));
	}
Define_label(mercury__parser__unexpected_tok__ua10000_6_0_i11);
	update_prof_current_proc(LABEL(mercury__parser__unexpected_tok__ua10000_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__unexpected_tok__ua10000_6_0_i10);
	r2 = (Integer) detstackvar(5);
	GOTO_LABEL(mercury__parser__unexpected_tok__ua10000_6_0_i8);
Define_label(mercury__parser__unexpected_tok__ua10000_6_0_i10);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__ops__lookup_postfix_op_4_0);
	call_localret(ENTRY(mercury__ops__lookup_postfix_op_4_0),
		mercury__parser__unexpected_tok__ua10000_6_0_i13,
		STATIC(mercury__parser__unexpected_tok__ua10000_6_0));
	}
Define_label(mercury__parser__unexpected_tok__ua10000_6_0_i13);
	update_prof_current_proc(LABEL(mercury__parser__unexpected_tok__ua10000_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__unexpected_tok__ua10000_6_0_i3);
	r2 = (Integer) detstackvar(5);
Define_label(mercury__parser__unexpected_tok__ua10000_6_0_i8);
	r1 = string_const("operator precedence error", 25);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__parser__error__ua10000_4_0),
		STATIC(mercury__parser__unexpected_tok__ua10000_6_0));
Define_label(mercury__parser__unexpected_tok__ua10000_6_0_i3);
	r3 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(4);
Define_label(mercury__parser__unexpected_tok__ua10000_6_0_i2);
	r1 = (Integer) r3;
	r2 = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__parser__error__ua10000_4_0),
		STATIC(mercury__parser__unexpected_tok__ua10000_6_0));
END_MODULE

BEGIN_MODULE(mercury__parser_module2)
	init_entry(mercury__parser__unexpected__ua10000_4_0);
	init_label(mercury__parser__unexpected__ua10000_4_0_i4);
	init_label(mercury__parser__unexpected__ua10000_4_0_i3);
BEGIN_CODE

/* code for predicate 'parser__unexpected__ua10000'/4 in mode 0 */
Define_static(mercury__parser__unexpected__ua10000_4_0);
	incr_sp_push_msg(3, "parser__unexpected__ua10000");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__get_token_4_0),
		mercury__parser__unexpected__ua10000_4_0_i4,
		STATIC(mercury__parser__unexpected__ua10000_4_0));
Define_label(mercury__parser__unexpected__ua10000_4_0_i4);
	update_prof_current_proc(LABEL(mercury__parser__unexpected__ua10000_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__unexpected__ua10000_4_0_i3);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__parser__unexpected_tok__ua10000_6_0),
		STATIC(mercury__parser__unexpected__ua10000_4_0));
Define_label(mercury__parser__unexpected__ua10000_4_0_i3);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__parser__error__ua10000_4_0),
		STATIC(mercury__parser__unexpected__ua10000_4_0));
END_MODULE

BEGIN_MODULE(mercury__parser_module3)
	init_entry(mercury__parser__read_term_3_0);
	init_label(mercury__parser__read_term_3_0_i2);
BEGIN_CODE

/* code for predicate 'parser__read_term'/3 in mode 0 */
Define_entry(mercury__parser__read_term_3_0);
	incr_sp_push_msg(1, "parser__read_term");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__io__input_stream_name_3_0);
	call_localret(ENTRY(mercury__io__input_stream_name_3_0),
		mercury__parser__read_term_3_0_i2,
		ENTRY(mercury__parser__read_term_3_0));
	}
Define_label(mercury__parser__read_term_3_0_i2);
	update_prof_current_proc(LABEL(mercury__parser__read_term_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__parser__read_term_4_0),
		ENTRY(mercury__parser__read_term_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__parser_module4)
	init_entry(mercury__parser__read_term_4_0);
	init_label(mercury__parser__read_term_4_0_i2);
	init_label(mercury__parser__read_term_4_0_i3);
	init_label(mercury__parser__read_term_4_0_i6);
	init_label(mercury__parser__read_term_4_0_i7);
	init_label(mercury__parser__read_term_4_0_i8);
	init_label(mercury__parser__read_term_4_0_i9);
	init_label(mercury__parser__read_term_4_0_i15);
	init_label(mercury__parser__read_term_4_0_i17);
	init_label(mercury__parser__read_term_4_0_i14);
	init_label(mercury__parser__read_term_4_0_i19);
	init_label(mercury__parser__read_term_4_0_i10);
	init_label(mercury__parser__read_term_4_0_i21);
	init_label(mercury__parser__read_term_4_0_i26);
	init_label(mercury__parser__read_term_4_0_i25);
	init_label(mercury__parser__read_term_4_0_i31);
	init_label(mercury__parser__read_term_4_0_i32);
	init_label(mercury__parser__read_term_4_0_i28);
	init_label(mercury__parser__read_term_4_0_i33);
	init_label(mercury__parser__read_term_4_0_i36);
	init_label(mercury__parser__read_term_4_0_i37);
	init_label(mercury__parser__read_term_4_0_i38);
	init_label(mercury__parser__read_term_4_0_i23);
	init_label(mercury__parser__read_term_4_0_i43);
	init_label(mercury__parser__read_term_4_0_i42);
	init_label(mercury__parser__read_term_4_0_i48);
	init_label(mercury__parser__read_term_4_0_i1037);
BEGIN_CODE

/* code for predicate 'parser__read_term'/4 in mode 0 */
Define_entry(mercury__parser__read_term_4_0);
	incr_sp_push_msg(7, "parser__read_term");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__lexer__get_token_list_3_0);
	call_localret(ENTRY(mercury__lexer__get_token_list_3_0),
		mercury__parser__read_term_4_0_i2,
		ENTRY(mercury__parser__read_term_4_0));
	}
Define_label(mercury__parser__read_term_4_0_i2);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__parser__read_term_4_0_i3);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__parser__read_term_4_0_i3);
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__ops__init_op_table_1_0);
	call_localret(ENTRY(mercury__ops__init_op_table_1_0),
		mercury__parser__read_term_4_0_i6,
		ENTRY(mercury__parser__read_term_4_0));
	}
Define_label(mercury__parser__read_term_4_0_i6);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	detstackvar(4) = (Integer) r1;
	{
	Declare_entry(mercury__varset__init_1_0);
	call_localret(ENTRY(mercury__varset__init_1_0),
		mercury__parser__read_term_4_0_i7,
		ENTRY(mercury__parser__read_term_4_0));
	}
Define_label(mercury__parser__read_term_4_0_i7);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__parser__read_term_4_0_i8,
		ENTRY(mercury__parser__read_term_4_0));
	}
Define_label(mercury__parser__read_term_4_0_i8);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r3, ((Integer) 4)) = (Integer) r1;
	r1 = ((Integer) 1201);
	r2 = ((Integer) 1);
	field(mktag(0), (Integer) r3, ((Integer) 3)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__parser__parse_term_2_5_0),
		mercury__parser__read_term_4_0_i9,
		ENTRY(mercury__parser__read_term_4_0));
Define_label(mercury__parser__read_term_4_0_i9);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__read_term_4_0_i10);
	detstackvar(1) = (Integer) r1;
	detstackvar(5) = (Integer) r2;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__get_token_4_0),
		mercury__parser__read_term_4_0_i15,
		ENTRY(mercury__parser__read_term_4_0));
Define_label(mercury__parser__read_term_4_0_i15);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__read_term_4_0_i14);
	detstackvar(4) = (Integer) r4;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 9)));
	{
	Declare_entry(mercury____Unify___lexer__token_0_0);
	call_localret(ENTRY(mercury____Unify___lexer__token_0_0),
		mercury__parser__read_term_4_0_i17,
		ENTRY(mercury__parser__read_term_4_0));
	}
Define_label(mercury__parser__read_term_4_0_i17);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__read_term_4_0_i14);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(4);
	r5 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 3));
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 2));
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	GOTO_LABEL(mercury__parser__read_term_4_0_i21);
	}
Define_label(mercury__parser__read_term_4_0_i14);
	r1 = string_const("operator or `.' expected", 24);
	r2 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__parser__unexpected__ua10000_4_0),
		mercury__parser__read_term_4_0_i19,
		ENTRY(mercury__parser__read_term_4_0));
Define_label(mercury__parser__read_term_4_0_i19);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	r3 = (Integer) r1;
	r4 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r5 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	GOTO_LABEL(mercury__parser__read_term_4_0_i21);
Define_label(mercury__parser__read_term_4_0_i10);
	r3 = (Integer) r1;
	r4 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r5 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
Define_label(mercury__parser__read_term_4_0_i21);
	if ((tag((Integer) r3) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__read_term_4_0_i23);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r1;
	call_localret(STATIC(mercury__parser__check_for_bad_token_3_0),
		mercury__parser__read_term_4_0_i26,
		ENTRY(mercury__parser__read_term_4_0));
Define_label(mercury__parser__read_term_4_0_i26);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__read_term_4_0_i25);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__parser__read_term_4_0_i25);
	r2 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(4);
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__parser__read_term_4_0_i28);
	r3 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(1) = (Integer) r5;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__lexer__token_to_string_2_0);
	call_localret(ENTRY(mercury__lexer__token_to_string_2_0),
		mercury__parser__read_term_4_0_i31,
		ENTRY(mercury__parser__read_term_4_0));
	}
Define_label(mercury__parser__read_term_4_0_i31);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("Syntax error at ", 16);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	tag_incr_hp(r4, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 0)) = string_const(": ", 2);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r4;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__string__append_list_2_0);
	call_localret(ENTRY(mercury__string__append_list_2_0),
		mercury__parser__read_term_4_0_i32,
		ENTRY(mercury__parser__read_term_4_0));
	}
Define_label(mercury__parser__read_term_4_0_i32);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__parser__read_term_4_0_i28);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__parser__read_term_4_0_i33);
	r3 = (Integer) r2;
	r4 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	r1 = string_const("Syntax error: ", 14);
	r2 = (Integer) r5;
	GOTO_LABEL(mercury__parser__read_term_4_0_i37);
Define_label(mercury__parser__read_term_4_0_i33);
	detstackvar(2) = (Integer) r2;
	detstackvar(1) = (Integer) r5;
	r1 = string_const("parser__check_for_errors", 24);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__parser__read_term_4_0_i36,
		ENTRY(mercury__parser__read_term_4_0));
	}
Define_label(mercury__parser__read_term_4_0_i36);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	r3 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(1);
Define_label(mercury__parser__read_term_4_0_i37);
	detstackvar(2) = (Integer) r3;
	detstackvar(1) = (Integer) r4;
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__parser__read_term_4_0_i38,
		ENTRY(mercury__parser__read_term_4_0));
	}
Define_label(mercury__parser__read_term_4_0_i38);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__parser__read_term_4_0_i23);
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r2;
	detstackvar(4) = (Integer) r5;
	call_localret(STATIC(mercury__parser__check_for_bad_token_3_0),
		mercury__parser__read_term_4_0_i43,
		ENTRY(mercury__parser__read_term_4_0));
Define_label(mercury__parser__read_term_4_0_i43);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__read_term_4_0_i42);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__parser__read_term_4_0_i42);
	r2 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(4);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__parser__read_term_4_0_i1037);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	{
	Declare_entry(mercury__lexer__token_to_string_2_0);
	call_localret(ENTRY(mercury__lexer__token_to_string_2_0),
		mercury__parser__read_term_4_0_i48,
		ENTRY(mercury__parser__read_term_4_0));
	}
	}
Define_label(mercury__parser__read_term_4_0_i48);
	update_prof_current_proc(LABEL(mercury__parser__read_term_4_0));
	r2 = (Integer) r1;
	r1 = string_const("Syntax error: unexpected ", 25);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__parser__read_term_4_0_i38,
		ENTRY(mercury__parser__read_term_4_0));
	}
Define_label(mercury__parser__read_term_4_0_i1037);
	tag_incr_hp(r1, mktag(2), ((Integer) 2));
	field(mktag(2), (Integer) r1, ((Integer) 0)) = (Integer) r4;
	field(mktag(2), (Integer) r1, ((Integer) 1)) = (Integer) r6;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module5)
	init_entry(mercury__parser__check_for_bad_token_3_0);
	init_label(mercury__parser__check_for_bad_token_3_0_i6);
	init_label(mercury__parser__check_for_bad_token_3_0_i7);
	init_label(mercury__parser__check_for_bad_token_3_0_i1001);
	init_label(mercury__parser__check_for_bad_token_3_0_i3);
	init_label(mercury__parser__check_for_bad_token_3_0_i11);
	init_label(mercury__parser__check_for_bad_token_3_0_i12);
	init_label(mercury__parser__check_for_bad_token_3_0_i13);
	init_label(mercury__parser__check_for_bad_token_3_0_i8);
	init_label(mercury__parser__check_for_bad_token_3_0_i15);
	init_label(mercury__parser__check_for_bad_token_3_0_i19);
	init_label(mercury__parser__check_for_bad_token_3_0_i1);
	init_label(mercury__parser__check_for_bad_token_3_0_i1000);
BEGIN_CODE

/* code for predicate 'parser__check_for_bad_token'/3 in mode 0 */
Define_static(mercury__parser__check_for_bad_token_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__parser__check_for_bad_token_3_0_i1000);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r3 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	r4 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 1));
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__parser__check_for_bad_token_3_0_i1001);
	incr_sp_push_msg(3, "parser__check_for_bad_token");
	detstackvar(3) = (Integer) succip;
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 5)))
		GOTO_LABEL(mercury__parser__check_for_bad_token_3_0_i3);
	detstackvar(1) = (Integer) r4;
	r1 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__io__error_message_2_0);
	call_localret(ENTRY(mercury__io__error_message_2_0),
		mercury__parser__check_for_bad_token_3_0_i6,
		STATIC(mercury__parser__check_for_bad_token_3_0));
	}
Define_label(mercury__parser__check_for_bad_token_3_0_i6);
	update_prof_current_proc(LABEL(mercury__parser__check_for_bad_token_3_0));
	r2 = (Integer) r1;
	r1 = string_const("I/O error: ", 11);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__parser__check_for_bad_token_3_0_i7,
		STATIC(mercury__parser__check_for_bad_token_3_0));
	}
Define_label(mercury__parser__check_for_bad_token_3_0_i7);
	update_prof_current_proc(LABEL(mercury__parser__check_for_bad_token_3_0));
	r2 = (Integer) r1;
	r3 = (Integer) detstackvar(1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__parser__check_for_bad_token_3_0_i1001);
	incr_sp_push_msg(3, "parser__check_for_bad_token");
	detstackvar(3) = (Integer) succip;
Define_label(mercury__parser__check_for_bad_token_3_0_i3);
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__parser__check_for_bad_token_3_0_i8);
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 3)))
		GOTO_LABEL(mercury__parser__check_for_bad_token_3_0_i8);
	detstackvar(1) = (Integer) r4;
	r1 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__char__to_int_2_0);
	call_localret(ENTRY(mercury__char__to_int_2_0),
		mercury__parser__check_for_bad_token_3_0_i11,
		STATIC(mercury__parser__check_for_bad_token_3_0));
	}
Define_label(mercury__parser__check_for_bad_token_3_0_i11);
	update_prof_current_proc(LABEL(mercury__parser__check_for_bad_token_3_0));
	detstackvar(2) = (Integer) r1;
	r2 = ((Integer) 10);
	{
	Declare_entry(mercury__string__int_to_base_string_3_0);
	call_localret(ENTRY(mercury__string__int_to_base_string_3_0),
		mercury__parser__check_for_bad_token_3_0_i12,
		STATIC(mercury__parser__check_for_bad_token_3_0));
	}
Define_label(mercury__parser__check_for_bad_token_3_0_i12);
	update_prof_current_proc(LABEL(mercury__parser__check_for_bad_token_3_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 16);
	{
	Declare_entry(mercury__string__int_to_base_string_3_0);
	call_localret(ENTRY(mercury__string__int_to_base_string_3_0),
		mercury__parser__check_for_bad_token_3_0_i13,
		STATIC(mercury__parser__check_for_bad_token_3_0));
	}
Define_label(mercury__parser__check_for_bad_token_3_0_i13);
	update_prof_current_proc(LABEL(mercury__parser__check_for_bad_token_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("Syntax error: Illegal character 0x", 34);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	tag_incr_hp(r4, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 0)) = string_const(" (", 2);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r4;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_parser__common_0);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__string__append_list_2_0);
	call_localret(ENTRY(mercury__string__append_list_2_0),
		mercury__parser__check_for_bad_token_3_0_i7,
		STATIC(mercury__parser__check_for_bad_token_3_0));
	}
Define_label(mercury__parser__check_for_bad_token_3_0_i8);
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__parser__check_for_bad_token_3_0_i15);
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 4)))
		GOTO_LABEL(mercury__parser__check_for_bad_token_3_0_i15);
	detstackvar(1) = (Integer) r4;
	r1 = string_const("Syntax error: ", 14);
	r2 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__parser__check_for_bad_token_3_0_i7,
		STATIC(mercury__parser__check_for_bad_token_3_0));
	}
Define_label(mercury__parser__check_for_bad_token_3_0_i15);
	detstackvar(1) = (Integer) r4;
	r1 = (Integer) r2;
	localcall(mercury__parser__check_for_bad_token_3_0,
		LABEL(mercury__parser__check_for_bad_token_3_0_i19),
		STATIC(mercury__parser__check_for_bad_token_3_0));
Define_label(mercury__parser__check_for_bad_token_3_0_i19);
	update_prof_current_proc(LABEL(mercury__parser__check_for_bad_token_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__check_for_bad_token_3_0_i1);
	if (((Integer) detstackvar(1) != (Integer) r3))
		GOTO_LABEL(mercury__parser__check_for_bad_token_3_0_i1);
	r3 = (Integer) detstackvar(1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__parser__check_for_bad_token_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__parser__check_for_bad_token_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module6)
	init_entry(mercury__parser__parse_term_3_0);
BEGIN_CODE

/* code for predicate 'parser__parse_term'/3 in mode 0 */
Define_static(mercury__parser__parse_term_3_0);
	r3 = (Integer) r1;
	r1 = ((Integer) 1201);
	r2 = ((Integer) 1);
	tailcall(STATIC(mercury__parser__parse_term_2_5_0),
		STATIC(mercury__parser__parse_term_3_0));
END_MODULE

BEGIN_MODULE(mercury__parser_module7)
	init_entry(mercury__parser__parse_term_2_5_0);
	init_label(mercury__parser__parse_term_2_5_0_i2);
	init_label(mercury__parser__parse_term_2_5_0_i3);
BEGIN_CODE

/* code for predicate 'parser__parse_term_2'/5 in mode 0 */
Define_static(mercury__parser__parse_term_2_5_0);
	incr_sp_push_msg(3, "parser__parse_term_2");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	call_localret(STATIC(mercury__parser__parse_left_term_6_0),
		mercury__parser__parse_term_2_5_0_i2,
		STATIC(mercury__parser__parse_term_2_5_0));
Define_label(mercury__parser__parse_term_2_5_0_i2);
	update_prof_current_proc(LABEL(mercury__parser__parse_term_2_5_0));
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_term_2_5_0_i3);
	r5 = (Integer) r3;
	r3 = (Integer) r1;
	r4 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__parser__parse_rest_7_0),
		STATIC(mercury__parser__parse_term_2_5_0));
Define_label(mercury__parser__parse_term_2_5_0_i3);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module8)
	init_entry(mercury__parser__parse_left_term_6_0);
	init_label(mercury__parser__parse_left_term_6_0_i4);
	init_label(mercury__parser__parse_left_term_6_0_i10);
	init_label(mercury__parser__parse_left_term_6_0_i13);
	init_label(mercury__parser__parse_left_term_6_0_i7);
	init_label(mercury__parser__parse_left_term_6_0_i6);
	init_label(mercury__parser__parse_left_term_6_0_i18);
	init_label(mercury__parser__parse_left_term_6_0_i21);
	init_label(mercury__parser__parse_left_term_6_0_i15);
	init_label(mercury__parser__parse_left_term_6_0_i14);
	init_label(mercury__parser__parse_left_term_6_0_i27);
	init_label(mercury__parser__parse_left_term_6_0_i29);
	init_label(mercury__parser__parse_left_term_6_0_i26);
	init_label(mercury__parser__parse_left_term_6_0_i31);
	init_label(mercury__parser__parse_left_term_6_0_i32);
	init_label(mercury__parser__parse_left_term_6_0_i34);
	init_label(mercury__parser__parse_left_term_6_0_i36);
	init_label(mercury__parser__parse_left_term_6_0_i37);
	init_label(mercury__parser__parse_left_term_6_0_i38);
	init_label(mercury__parser__parse_left_term_6_0_i39);
	init_label(mercury__parser__parse_left_term_6_0_i43);
	init_label(mercury__parser__parse_left_term_6_0_i47);
	init_label(mercury__parser__parse_left_term_6_0_i44);
	init_label(mercury__parser__parse_left_term_6_0_i23);
	init_label(mercury__parser__parse_left_term_6_0_i22);
	init_label(mercury__parser__parse_left_term_6_0_i55);
	init_label(mercury__parser__parse_left_term_6_0_i57);
	init_label(mercury__parser__parse_left_term_6_0_i54);
	init_label(mercury__parser__parse_left_term_6_0_i59);
	init_label(mercury__parser__parse_left_term_6_0_i60);
	init_label(mercury__parser__parse_left_term_6_0_i62);
	init_label(mercury__parser__parse_left_term_6_0_i64);
	init_label(mercury__parser__parse_left_term_6_0_i65);
	init_label(mercury__parser__parse_left_term_6_0_i66);
	init_label(mercury__parser__parse_left_term_6_0_i70);
	init_label(mercury__parser__parse_left_term_6_0_i51);
	init_label(mercury__parser__parse_left_term_6_0_i50);
	init_label(mercury__parser__parse_left_term_6_0_i72);
	init_label(mercury__parser__parse_left_term_6_0_i3);
BEGIN_CODE

/* code for predicate 'parser__parse_left_term'/6 in mode 0 */
Define_static(mercury__parser__parse_left_term_6_0);
	incr_sp_push_msg(12, "parser__parse_left_term");
	detstackvar(12) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(4) = (Integer) r3;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__parser__get_token_4_0),
		mercury__parser__parse_left_term_6_0_i4,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i4);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i3);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i6);
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	if ((strcmp((char *)(Integer) r1, (char *)string_const("-", 1)) !=0))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i6);
	detstackvar(5) = (Integer) r2;
	detstackvar(6) = (Integer) r3;
	detstackvar(10) = (Integer) r4;
	r1 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_token_4_0),
		mercury__parser__parse_left_term_6_0_i10,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i10);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i7);
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i7);
	if (((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i7);
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_left_term_6_0_i13,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i13);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	r3 = (Integer) r1;
	r1 = ((Integer) 0);
	r4 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r5, mktag(0), ((Integer) 3));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(0), (Integer) r5, ((Integer) 2)) = (Integer) r3;
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	r3 = (Integer) r4;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (((Integer) 0) - (Integer) detstackvar(3));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r5;
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	proceed();
	}
Define_label(mercury__parser__parse_left_term_6_0_i7);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(10);
Define_label(mercury__parser__parse_left_term_6_0_i6);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i14);
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	if ((strcmp((char *)(Integer) r1, (char *)string_const("-", 1)) !=0))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i14);
	detstackvar(5) = (Integer) r2;
	detstackvar(6) = (Integer) r3;
	detstackvar(10) = (Integer) r4;
	r1 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_token_4_0),
		mercury__parser__parse_left_term_6_0_i18,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i18);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i15);
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i15);
	if (((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i15);
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_left_term_6_0_i21,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i21);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	r3 = (Integer) r1;
	r1 = ((Integer) 0);
	r4 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r5, mktag(0), ((Integer) 3));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 1));
	field(mktag(0), (Integer) r5, ((Integer) 2)) = (Integer) r3;
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	r3 = (Integer) r4;
	{
	static const Float mercury_float_const_0 = 0;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = float_to_word(((Float) 0.00000000000000) - word_to_float((Integer) detstackvar(3)));
	}
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r5;
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	proceed();
	}
Define_label(mercury__parser__parse_left_term_6_0_i15);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(10);
Define_label(mercury__parser__parse_left_term_6_0_i14);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i22);
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(5) = (Integer) r2;
	detstackvar(6) = (Integer) r3;
	detstackvar(7) = (Integer) r1;
	detstackvar(10) = (Integer) r4;
	r1 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_token_3_0),
		mercury__parser__parse_left_term_6_0_i27,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i27);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i26);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 1)));
	{
	Declare_entry(mercury____Unify___lexer__token_0_0);
	call_localret(ENTRY(mercury____Unify___lexer__token_0_0),
		mercury__parser__parse_left_term_6_0_i29,
		STATIC(mercury__parser__parse_left_term_6_0));
	}
Define_label(mercury__parser__parse_left_term_6_0_i29);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i23);
Define_label(mercury__parser__parse_left_term_6_0_i26);
	r1 = (Integer) detstackvar(10);
	call_localret(STATIC(mercury__parser__get_ops_table_3_0),
		mercury__parser__parse_left_term_6_0_i31,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i31);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	detstackvar(3) = (Integer) r2;
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__ops__lookup_binary_prefix_op_5_0);
	call_localret(ENTRY(mercury__ops__lookup_binary_prefix_op_5_0),
		mercury__parser__parse_left_term_6_0_i32,
		STATIC(mercury__parser__parse_left_term_6_0));
	}
Define_label(mercury__parser__parse_left_term_6_0_i32);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i23);
	if (((Integer) r2 > (Integer) detstackvar(1)))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i23);
	r1 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r2;
	detstackvar(8) = (Integer) r3;
	detstackvar(9) = (Integer) r4;
	call_localret(STATIC(mercury__parser__peek_token_3_0),
		mercury__parser__parse_left_term_6_0_i34,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i34);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i23);
	detstackvar(11) = (Integer) r3;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__could_start_term_2_0),
		mercury__parser__parse_left_term_6_0_i36,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i36);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if ((((Integer) 0) != (Integer) r1))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i23);
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__parser__adjust_priority_3_0),
		mercury__parser__parse_left_term_6_0_i37,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i37);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__parser__adjust_priority_3_0),
		mercury__parser__parse_left_term_6_0_i38,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i38);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	r2 = (Integer) detstackvar(8);
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(11);
	call_localret(STATIC(mercury__parser__parse_term_2_5_0),
		mercury__parser__parse_left_term_6_0_i39,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i39);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i44);
	r3 = (Integer) detstackvar(8);
	detstackvar(8) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__parser__parse_term_2_5_0),
		mercury__parser__parse_left_term_6_0_i43,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i43);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i44);
	detstackvar(9) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_left_term_6_0_i47,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i47);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r4 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r5, mktag(0), ((Integer) 3));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(7);
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	tag_incr_hp(r6, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r6, ((Integer) 0)) = (Integer) detstackvar(8);
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r6, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(0), (Integer) r5, ((Integer) 2)) = (Integer) r3;
	r3 = (Integer) r4;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(9);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r5;
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) r6;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	proceed();
	}
Define_label(mercury__parser__parse_left_term_6_0_i44);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	proceed();
Define_label(mercury__parser__parse_left_term_6_0_i23);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(10);
Define_label(mercury__parser__parse_left_term_6_0_i22);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i50);
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(5) = (Integer) r2;
	detstackvar(6) = (Integer) r3;
	detstackvar(10) = (Integer) r4;
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_token_3_0),
		mercury__parser__parse_left_term_6_0_i55,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i55);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i54);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 1)));
	{
	Declare_entry(mercury____Unify___lexer__token_0_0);
	call_localret(ENTRY(mercury____Unify___lexer__token_0_0),
		mercury__parser__parse_left_term_6_0_i57,
		STATIC(mercury__parser__parse_left_term_6_0));
	}
Define_label(mercury__parser__parse_left_term_6_0_i57);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i51);
Define_label(mercury__parser__parse_left_term_6_0_i54);
	r1 = (Integer) detstackvar(10);
	call_localret(STATIC(mercury__parser__get_ops_table_3_0),
		mercury__parser__parse_left_term_6_0_i59,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i59);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	detstackvar(3) = (Integer) r2;
	r2 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__ops__lookup_prefix_op_4_0);
	call_localret(ENTRY(mercury__ops__lookup_prefix_op_4_0),
		mercury__parser__parse_left_term_6_0_i60,
		STATIC(mercury__parser__parse_left_term_6_0));
	}
Define_label(mercury__parser__parse_left_term_6_0_i60);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i51);
	if (((Integer) r2 > (Integer) detstackvar(1)))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i51);
	r1 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r2;
	detstackvar(9) = (Integer) r3;
	call_localret(STATIC(mercury__parser__peek_token_3_0),
		mercury__parser__parse_left_term_6_0_i62,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i62);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i51);
	detstackvar(7) = (Integer) r3;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__could_start_term_2_0),
		mercury__parser__parse_left_term_6_0_i64,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i64);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if ((((Integer) 0) != (Integer) r1))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i51);
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__parser__adjust_priority_3_0),
		mercury__parser__parse_left_term_6_0_i65,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i65);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(7);
	call_localret(STATIC(mercury__parser__parse_term_2_5_0),
		mercury__parser__parse_left_term_6_0_i66,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i66);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_left_term_6_0_i44);
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_left_term_6_0_i70,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i70);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r4 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r5, mktag(0), ((Integer) 3));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(8);
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(0), (Integer) r5, ((Integer) 2)) = (Integer) r3;
	r3 = (Integer) r4;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	proceed();
	}
Define_label(mercury__parser__parse_left_term_6_0_i51);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(10);
Define_label(mercury__parser__parse_left_term_6_0_i50);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__parser__parse_simple_term_6_0),
		mercury__parser__parse_left_term_6_0_i72,
		STATIC(mercury__parser__parse_left_term_6_0));
Define_label(mercury__parser__parse_left_term_6_0_i72);
	update_prof_current_proc(LABEL(mercury__parser__parse_left_term_6_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	proceed();
Define_label(mercury__parser__parse_left_term_6_0_i3);
	r1 = string_const("unexpected end-of-file at start of sub-term", 43);
	r2 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__parser__error__ua10000_4_0),
		mercury__parser__parse_left_term_6_0_i72,
		STATIC(mercury__parser__parse_left_term_6_0));
END_MODULE

BEGIN_MODULE(mercury__parser_module9)
	init_entry(mercury__parser__parse_rest_7_0);
	init_label(mercury__parser__parse_rest_7_0_i4);
	init_label(mercury__parser__parse_rest_7_0_i6);
	init_label(mercury__parser__parse_rest_7_0_i11);
	init_label(mercury__parser__parse_rest_7_0_i12);
	init_label(mercury__parser__parse_rest_7_0_i13);
	init_label(mercury__parser__parse_rest_7_0_i16);
	init_label(mercury__parser__parse_rest_7_0_i15);
	init_label(mercury__parser__parse_rest_7_0_i17);
	init_label(mercury__parser__parse_rest_7_0_i18);
	init_label(mercury__parser__parse_rest_7_0_i22);
	init_label(mercury__parser__parse_rest_7_0_i19);
	init_label(mercury__parser__parse_rest_7_0_i3);
	init_label(mercury__parser__parse_rest_7_0_i27);
	init_label(mercury__parser__parse_rest_7_0_i30);
	init_label(mercury__parser__parse_rest_7_0_i31);
	init_label(mercury__parser__parse_rest_7_0_i34);
	init_label(mercury__parser__parse_rest_7_0_i33);
	init_label(mercury__parser__parse_rest_7_0_i35);
	init_label(mercury__parser__parse_rest_7_0_i1047);
BEGIN_CODE

/* code for predicate 'parser__parse_rest'/7 in mode 0 */
Define_static(mercury__parser__parse_rest_7_0);
	incr_sp_push_msg(10, "parser__parse_rest");
	detstackvar(10) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	r1 = (Integer) r5;
	call_localret(STATIC(mercury__parser__get_token_4_0),
		mercury__parser__parse_rest_7_0_i4,
		STATIC(mercury__parser__parse_rest_7_0));
Define_label(mercury__parser__parse_rest_7_0_i4);
	update_prof_current_proc(LABEL(mercury__parser__parse_rest_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i3);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 8)))))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i6);
	if (((Integer) detstackvar(2) != ((Integer) 1)))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i6);
	r1 = (Integer) r4;
	r7 = (Integer) r3;
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r8 = string_const(",", 1);
	GOTO_LABEL(mercury__parser__parse_rest_7_0_i11);
Define_label(mercury__parser__parse_rest_7_0_i6);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i3);
	r1 = (Integer) r4;
	r7 = (Integer) r3;
	r8 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
Define_label(mercury__parser__parse_rest_7_0_i11);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	detstackvar(7) = (Integer) r8;
	call_localret(STATIC(mercury__parser__get_ops_table_3_0),
		mercury__parser__parse_rest_7_0_i12,
		STATIC(mercury__parser__parse_rest_7_0));
Define_label(mercury__parser__parse_rest_7_0_i12);
	update_prof_current_proc(LABEL(mercury__parser__parse_rest_7_0));
	detstackvar(9) = (Integer) r2;
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__ops__lookup_infix_op_5_0);
	call_localret(ENTRY(mercury__ops__lookup_infix_op_5_0),
		mercury__parser__parse_rest_7_0_i13,
		STATIC(mercury__parser__parse_rest_7_0));
	}
Define_label(mercury__parser__parse_rest_7_0_i13);
	update_prof_current_proc(LABEL(mercury__parser__parse_rest_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i3);
	if (((Integer) r2 > (Integer) detstackvar(1)))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i3);
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i16);
	if (((Integer) detstackvar(3) >= (Integer) r2))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i3);
	r1 = (Integer) r4;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(4);
	r8 = (Integer) detstackvar(6);
	r9 = (Integer) detstackvar(7);
	r10 = (Integer) detstackvar(9);
	GOTO_LABEL(mercury__parser__parse_rest_7_0_i15);
Define_label(mercury__parser__parse_rest_7_0_i16);
	if (((Integer) detstackvar(3) > (Integer) r2))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i3);
	r1 = (Integer) r4;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(4);
	r8 = (Integer) detstackvar(6);
	r9 = (Integer) detstackvar(7);
	r10 = (Integer) detstackvar(9);
Define_label(mercury__parser__parse_rest_7_0_i15);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(4) = (Integer) r6;
	detstackvar(6) = (Integer) r8;
	detstackvar(7) = (Integer) r9;
	detstackvar(8) = (Integer) r2;
	detstackvar(9) = (Integer) r10;
	call_localret(STATIC(mercury__parser__adjust_priority_3_0),
		mercury__parser__parse_rest_7_0_i17,
		STATIC(mercury__parser__parse_rest_7_0));
Define_label(mercury__parser__parse_rest_7_0_i17);
	update_prof_current_proc(LABEL(mercury__parser__parse_rest_7_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(9);
	call_localret(STATIC(mercury__parser__parse_term_2_5_0),
		mercury__parser__parse_rest_7_0_i18,
		STATIC(mercury__parser__parse_rest_7_0));
Define_label(mercury__parser__parse_rest_7_0_i18);
	update_prof_current_proc(LABEL(mercury__parser__parse_rest_7_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i19);
	r3 = (Integer) detstackvar(6);
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_rest_7_0_i22,
		STATIC(mercury__parser__parse_rest_7_0));
Define_label(mercury__parser__parse_rest_7_0_i22);
	update_prof_current_proc(LABEL(mercury__parser__parse_rest_7_0));
	r3 = (Integer) detstackvar(8);
	tag_incr_hp(r4, mktag(0), ((Integer) 3));
	r5 = (Integer) r1;
	r6 = (Integer) r2;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(7);
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	tag_incr_hp(r7, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r7, ((Integer) 0)) = (Integer) detstackvar(4);
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r7, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(0), (Integer) r4, ((Integer) 2)) = (Integer) r5;
	r5 = (Integer) r6;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) r7;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	localtailcall(mercury__parser__parse_rest_7_0,
		STATIC(mercury__parser__parse_rest_7_0));
	}
Define_label(mercury__parser__parse_rest_7_0_i19);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__parser__parse_rest_7_0_i3);
	r1 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__parser__get_token_4_0),
		mercury__parser__parse_rest_7_0_i27,
		STATIC(mercury__parser__parse_rest_7_0));
Define_label(mercury__parser__parse_rest_7_0_i27);
	update_prof_current_proc(LABEL(mercury__parser__parse_rest_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i1047);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i1047);
	detstackvar(7) = (Integer) r3;
	detstackvar(8) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r1 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_ops_table_3_0),
		mercury__parser__parse_rest_7_0_i30,
		STATIC(mercury__parser__parse_rest_7_0));
Define_label(mercury__parser__parse_rest_7_0_i30);
	update_prof_current_proc(LABEL(mercury__parser__parse_rest_7_0));
	detstackvar(6) = (Integer) r2;
	r2 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__ops__lookup_postfix_op_4_0);
	call_localret(ENTRY(mercury__ops__lookup_postfix_op_4_0),
		mercury__parser__parse_rest_7_0_i31,
		STATIC(mercury__parser__parse_rest_7_0));
	}
Define_label(mercury__parser__parse_rest_7_0_i31);
	update_prof_current_proc(LABEL(mercury__parser__parse_rest_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i1047);
	if (((Integer) r2 > (Integer) detstackvar(1)))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i1047);
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i34);
	if (((Integer) detstackvar(3) >= (Integer) r2))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i1047);
	r8 = (Integer) r2;
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(8);
	GOTO_LABEL(mercury__parser__parse_rest_7_0_i33);
Define_label(mercury__parser__parse_rest_7_0_i34);
	if (((Integer) detstackvar(3) > (Integer) r2))
		GOTO_LABEL(mercury__parser__parse_rest_7_0_i1047);
	r8 = (Integer) r2;
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(8);
Define_label(mercury__parser__parse_rest_7_0_i33);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(8) = (Integer) r7;
	detstackvar(3) = (Integer) r8;
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_rest_7_0_i35,
		STATIC(mercury__parser__parse_rest_7_0));
Define_label(mercury__parser__parse_rest_7_0_i35);
	update_prof_current_proc(LABEL(mercury__parser__parse_rest_7_0));
	r5 = (Integer) r1;
	r6 = (Integer) r2;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	tag_incr_hp(r4, mktag(0), ((Integer) 3));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(8);
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(0), (Integer) r4, ((Integer) 2)) = (Integer) r5;
	r5 = (Integer) r6;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	localtailcall(mercury__parser__parse_rest_7_0,
		STATIC(mercury__parser__parse_rest_7_0));
	}
Define_label(mercury__parser__parse_rest_7_0_i1047);
	tag_incr_hp(r1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module10)
	init_entry(mercury__parser__parse_simple_term_6_0);
	init_label(mercury__parser__parse_simple_term_6_0_i4);
	init_label(mercury__parser__parse_simple_term_6_0_i3);
BEGIN_CODE

/* code for predicate 'parser__parse_simple_term'/6 in mode 0 */
Define_static(mercury__parser__parse_simple_term_6_0);
	incr_sp_push_msg(4, "parser__parse_simple_term");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	call_localret(STATIC(mercury__parser__parse_simple_term_2_6_0),
		mercury__parser__parse_simple_term_6_0_i4,
		STATIC(mercury__parser__parse_simple_term_6_0));
Define_label(mercury__parser__parse_simple_term_6_0_i4);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_6_0_i3);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__parser__parse_simple_term_6_0_i3);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r3 = string_const("unexpected token at start of (sub)term", 38);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__parser__unexpected_tok__ua10000_6_0),
		STATIC(mercury__parser__parse_simple_term_6_0));
END_MODULE

BEGIN_MODULE(mercury__parser_module11)
	init_entry(mercury__parser__parse_simple_term_2_6_0);
	init_label(mercury__parser__parse_simple_term_2_6_0_i1111);
	init_label(mercury__parser__parse_simple_term_2_6_0_i1110);
	init_label(mercury__parser__parse_simple_term_2_6_0_i1109);
	init_label(mercury__parser__parse_simple_term_2_6_0_i1108);
	init_label(mercury__parser__parse_simple_term_2_6_0_i5);
	init_label(mercury__parser__parse_simple_term_2_6_0_i6);
	init_label(mercury__parser__parse_simple_term_2_6_0_i12);
	init_label(mercury__parser__parse_simple_term_2_6_0_i14);
	init_label(mercury__parser__parse_simple_term_2_6_0_i11);
	init_label(mercury__parser__parse_simple_term_2_6_0_i16);
	init_label(mercury__parser__parse_simple_term_2_6_0_i19);
	init_label(mercury__parser__parse_simple_term_2_6_0_i20);
	init_label(mercury__parser__parse_simple_term_2_6_0_i22);
	init_label(mercury__parser__parse_simple_term_2_6_0_i25);
	init_label(mercury__parser__parse_simple_term_2_6_0_i27);
	init_label(mercury__parser__parse_simple_term_2_6_0_i29);
	init_label(mercury__parser__parse_simple_term_2_6_0_i24);
	init_label(mercury__parser__parse_simple_term_2_6_0_i32);
	init_label(mercury__parser__parse_simple_term_2_6_0_i33);
	init_label(mercury__parser__parse_simple_term_2_6_0_i36);
	init_label(mercury__parser__parse_simple_term_2_6_0_i38);
	init_label(mercury__parser__parse_simple_term_2_6_0_i35);
	init_label(mercury__parser__parse_simple_term_2_6_0_i40);
	init_label(mercury__parser__parse_simple_term_2_6_0_i46);
	init_label(mercury__parser__parse_simple_term_2_6_0_i48);
	init_label(mercury__parser__parse_simple_term_2_6_0_i45);
	init_label(mercury__parser__parse_simple_term_2_6_0_i1107);
	init_label(mercury__parser__parse_simple_term_2_6_0_i55);
	init_label(mercury__parser__parse_simple_term_2_6_0_i56);
	init_label(mercury__parser__parse_simple_term_2_6_0_i57);
	init_label(mercury__parser__parse_simple_term_2_6_0_i58);
	init_label(mercury__parser__parse_simple_term_2_6_0_i59);
	init_label(mercury__parser__parse_simple_term_2_6_0_i60);
	init_label(mercury__parser__parse_simple_term_2_6_0_i54);
	init_label(mercury__parser__parse_simple_term_2_6_0_i62);
	init_label(mercury__parser__parse_simple_term_2_6_0_i65);
	init_label(mercury__parser__parse_simple_term_2_6_0_i67);
	init_label(mercury__parser__parse_simple_term_2_6_0_i69);
	init_label(mercury__parser__parse_simple_term_2_6_0_i71);
	init_label(mercury__parser__parse_simple_term_2_6_0_i64);
	init_label(mercury__parser__parse_simple_term_2_6_0_i72);
	init_label(mercury__parser__parse_simple_term_2_6_0_i75);
	init_label(mercury__parser__parse_simple_term_2_6_0_i74);
	init_label(mercury__parser__parse_simple_term_2_6_0_i61);
	init_label(mercury__parser__parse_simple_term_2_6_0_i80);
	init_label(mercury__parser__parse_simple_term_2_6_0_i83);
	init_label(mercury__parser__parse_simple_term_2_6_0_i85);
	init_label(mercury__parser__parse_simple_term_2_6_0_i87);
	init_label(mercury__parser__parse_simple_term_2_6_0_i88);
	init_label(mercury__parser__parse_simple_term_2_6_0_i90);
	init_label(mercury__parser__parse_simple_term_2_6_0_i1103);
	init_label(mercury__parser__parse_simple_term_2_6_0_i1);
	init_label(mercury__parser__parse_simple_term_2_6_0_i1105);
	init_label(mercury__parser__parse_simple_term_2_6_0_i1106);
BEGIN_CODE

/* code for predicate 'parser__parse_simple_term_2'/6 in mode 0 */
Define_static(mercury__parser__parse_simple_term_2_6_0);
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i1107);
	COMPUTED_GOTO(unmkbody((Integer) r1),
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1111) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1110) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1106) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1109) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1106) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1108) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1106) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1106) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1106) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1106) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1106));
Define_label(mercury__parser__parse_simple_term_2_6_0_i1111);
	incr_sp_push_msg(6, "parser__parse_simple_term_2");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i5);
Define_label(mercury__parser__parse_simple_term_2_6_0_i1110);
	incr_sp_push_msg(6, "parser__parse_simple_term_2");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i19);
Define_label(mercury__parser__parse_simple_term_2_6_0_i1109);
	incr_sp_push_msg(6, "parser__parse_simple_term_2");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i22);
Define_label(mercury__parser__parse_simple_term_2_6_0_i1108);
	incr_sp_push_msg(6, "parser__parse_simple_term_2");
	detstackvar(6) = (Integer) succip;
	GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i32);
Define_label(mercury__parser__parse_simple_term_2_6_0_i5);
	r1 = (Integer) r4;
	call_localret(STATIC(mercury__parser__parse_term_3_0),
		mercury__parser__parse_simple_term_2_6_0_i6,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i6);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i16);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__get_token_3_0),
		mercury__parser__parse_simple_term_2_6_0_i12,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i12);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i11);
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 2)));
	{
	Declare_entry(mercury____Unify___lexer__token_0_0);
	call_localret(ENTRY(mercury____Unify___lexer__token_0_0),
		mercury__parser__parse_simple_term_2_6_0_i14,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i14);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i11);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__parser__parse_simple_term_2_6_0_i11);
	r1 = string_const("expecting `)' or operator", 25);
	r2 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__parser__unexpected__ua10000_4_0),
		mercury__parser__parse_simple_term_2_6_0_i16,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i16);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__parser__parse_simple_term_2_6_0_i19);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	localcall(mercury__parser__parse_simple_term_2_6_0,
		LABEL(mercury__parser__parse_simple_term_2_6_0_i20),
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i20);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	if ((Integer) r1)
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i1105);
	r1 = FALSE;
	proceed();
Define_label(mercury__parser__parse_simple_term_2_6_0_i22);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r4;
	r1 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_token_3_0),
		mercury__parser__parse_simple_term_2_6_0_i25,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i25);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i24);
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 4)));
	{
	Declare_entry(mercury____Unify___lexer__token_0_0);
	call_localret(ENTRY(mercury____Unify___lexer__token_0_0),
		mercury__parser__parse_simple_term_2_6_0_i27,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i27);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i24);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_simple_term_2_6_0_i29,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i29);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) r1;
	r1 = TRUE;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i24);
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__parser__parse_list_3_0),
		mercury__parser__parse_simple_term_2_6_0_i16,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i32);
	r1 = (Integer) r2;
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_simple_term_2_6_0_i33,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i33);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__get_token_3_0),
		mercury__parser__parse_simple_term_2_6_0_i36,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i36);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i35);
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 6)));
	{
	Declare_entry(mercury____Unify___lexer__token_0_0);
	call_localret(ENTRY(mercury____Unify___lexer__token_0_0),
		mercury__parser__parse_simple_term_2_6_0_i38,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i38);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i35);
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_2);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	r3 = (Integer) detstackvar(3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__parser__parse_simple_term_2_6_0_i35);
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__parser__parse_term_3_0),
		mercury__parser__parse_simple_term_2_6_0_i40,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i40);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i16);
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__get_token_3_0),
		mercury__parser__parse_simple_term_2_6_0_i46,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i46);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i45);
	detstackvar(4) = (Integer) r3;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 6)));
	{
	Declare_entry(mercury____Unify___lexer__token_0_0);
	call_localret(ENTRY(mercury____Unify___lexer__token_0_0),
		mercury__parser__parse_simple_term_2_6_0_i48,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i48);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i45);
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_2);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	r3 = (Integer) detstackvar(4);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__parser__parse_simple_term_2_6_0_i45);
	r1 = string_const("expecting `}' or operator", 25);
	r2 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__parser__unexpected__ua10000_4_0),
		mercury__parser__parse_simple_term_2_6_0_i16,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i1107);
	incr_sp_push_msg(6, "parser__parse_simple_term_2");
	detstackvar(6) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i54);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__parser__parse_simple_term_2_6_0_i55) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i57) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i59) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1) AND
		LABEL(mercury__parser__parse_simple_term_2_6_0_i1));
Define_label(mercury__parser__parse_simple_term_2_6_0_i55);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r2;
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_simple_term_2_6_0_i56,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i56);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r4, mktag(0), ((Integer) 3));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(0), (Integer) r4, ((Integer) 2)) = (Integer) r1;
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	r1 = TRUE;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i57);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r2;
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_simple_term_2_6_0_i58,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i58);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r4, mktag(0), ((Integer) 3));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 1));
	field(mktag(0), (Integer) r4, ((Integer) 2)) = (Integer) r1;
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	r1 = TRUE;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i59);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r2;
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_simple_term_2_6_0_i60,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i60);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r4, mktag(0), ((Integer) 3));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(2), ((Integer) 1));
	field(mktag(0), (Integer) r4, ((Integer) 2)) = (Integer) r1;
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	r1 = TRUE;
	field(mktag(2), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i54);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i61);
	detstackvar(1) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) r2;
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_simple_term_2_6_0_i62,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i62);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	detstackvar(4) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__get_token_3_0),
		mercury__parser__parse_simple_term_2_6_0_i65,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i65);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i64);
	detstackvar(5) = (Integer) r3;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 1)));
	{
	Declare_entry(mercury____Unify___lexer__token_0_0);
	call_localret(ENTRY(mercury____Unify___lexer__token_0_0),
		mercury__parser__parse_simple_term_2_6_0_i67,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i67);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i64);
	r1 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__parser__parse_args_3_0),
		mercury__parser__parse_simple_term_2_6_0_i69,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i69);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if ((tag((Integer) r1) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i71);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__parser__parse_simple_term_2_6_0_i71);
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r4, mktag(0), ((Integer) 3));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = TRUE;
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r4, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i64);
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__parser__get_ops_table_3_0),
		mercury__parser__parse_simple_term_2_6_0_i72,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i72);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__ops__lookup_op_2_0);
	call_localret(ENTRY(mercury__ops__lookup_op_2_0),
		mercury__parser__parse_simple_term_2_6_0_i75,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i75);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i74);
	if (((Integer) detstackvar(1) < ((Integer) 1201)))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i1);
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r1, mktag(0), ((Integer) 3));
	tag_incr_hp(r3, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	r3 = (Integer) detstackvar(2);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__parser__parse_simple_term_2_6_0_i74);
	r1 = (Integer) detstackvar(2);
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r3, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) tempr1;
	r3 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i61);
	if ((tag((Integer) r1) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i1);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__parser__add_var_4_0),
		mercury__parser__parse_simple_term_2_6_0_i80,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i80);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__get_token_3_0),
		mercury__parser__parse_simple_term_2_6_0_i83,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i83);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i1103);
	detstackvar(4) = (Integer) r3;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 1)));
	{
	Declare_entry(mercury____Unify___lexer__token_0_0);
	call_localret(ENTRY(mercury____Unify___lexer__token_0_0),
		mercury__parser__parse_simple_term_2_6_0_i85,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i85);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i1103);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_simple_term_2_6_0_i87,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i87);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__parse_args_3_0),
		mercury__parser__parse_simple_term_2_6_0_i88,
		STATIC(mercury__parser__parse_simple_term_2_6_0));
Define_label(mercury__parser__parse_simple_term_2_6_0_i88);
	update_prof_current_proc(LABEL(mercury__parser__parse_simple_term_2_6_0));
	if ((tag((Integer) r1) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_simple_term_2_6_0_i90);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__parser__parse_simple_term_2_6_0_i90);
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r4, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_3);
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = TRUE;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r4, ((Integer) 2)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
	}
Define_label(mercury__parser__parse_simple_term_2_6_0_i1103);
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	r3 = (Integer) detstackvar(3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__parser__parse_simple_term_2_6_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__parser__parse_simple_term_2_6_0_i1105);
	r1 = TRUE;
	proceed();
Define_label(mercury__parser__parse_simple_term_2_6_0_i1106);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module12)
	init_entry(mercury__parser__parse_list_3_0);
	init_label(mercury__parser__parse_list_3_0_i2);
	init_label(mercury__parser__parse_list_3_0_i8);
	init_label(mercury__parser__parse_list_3_0_i10);
	init_label(mercury__parser__parse_list_3_0_i14);
	init_label(mercury__parser__parse_list_3_0_i11);
	init_label(mercury__parser__parse_list_3_0_i22);
	init_label(mercury__parser__parse_list_3_0_i28);
	init_label(mercury__parser__parse_list_3_0_i30);
	init_label(mercury__parser__parse_list_3_0_i27);
	init_label(mercury__parser__parse_list_3_0_i19);
	init_label(mercury__parser__parse_list_3_0_i35);
	init_label(mercury__parser__parse_list_3_0_i7);
	init_label(mercury__parser__parse_list_3_0_i44);
BEGIN_CODE

/* code for predicate 'parser__parse_list'/3 in mode 0 */
Define_static(mercury__parser__parse_list_3_0);
	r3 = (Integer) r1;
	r1 = ((Integer) 1201);
	r2 = ((Integer) 0);
	incr_sp_push_msg(7, "parser__parse_list");
	detstackvar(7) = (Integer) succip;
	call_localret(STATIC(mercury__parser__parse_term_2_5_0),
		mercury__parser__parse_list_3_0_i2,
		STATIC(mercury__parser__parse_list_3_0));
Define_label(mercury__parser__parse_list_3_0_i2);
	update_prof_current_proc(LABEL(mercury__parser__parse_list_3_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_list_3_0_i44);
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) r2;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__get_token_4_0),
		mercury__parser__parse_list_3_0_i8,
		STATIC(mercury__parser__parse_list_3_0));
Define_label(mercury__parser__parse_list_3_0_i8);
	update_prof_current_proc(LABEL(mercury__parser__parse_list_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_list_3_0_i7);
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__parser__get_term_context_4_0),
		mercury__parser__parse_list_3_0_i10,
		STATIC(mercury__parser__parse_list_3_0));
Define_label(mercury__parser__parse_list_3_0_i10);
	update_prof_current_proc(LABEL(mercury__parser__parse_list_3_0));
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 8)))))
		GOTO_LABEL(mercury__parser__parse_list_3_0_i11);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__parser__parse_list_3_0,
		LABEL(mercury__parser__parse_list_3_0_i14),
		STATIC(mercury__parser__parse_list_3_0));
Define_label(mercury__parser__parse_list_3_0_i14);
	update_prof_current_proc(LABEL(mercury__parser__parse_list_3_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_list_3_0_i44);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 1));
	tag_incr_hp(r4, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_4);
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) r5;
	field(mktag(0), (Integer) r4, ((Integer) 2)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
	}
Define_label(mercury__parser__parse_list_3_0_i11);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 7)))))
		GOTO_LABEL(mercury__parser__parse_list_3_0_i19);
	r3 = (Integer) r2;
	detstackvar(2) = (Integer) r1;
	r1 = ((Integer) 1201);
	r2 = ((Integer) 0);
	call_localret(STATIC(mercury__parser__parse_term_2_5_0),
		mercury__parser__parse_list_3_0_i22,
		STATIC(mercury__parser__parse_list_3_0));
Define_label(mercury__parser__parse_list_3_0_i22);
	update_prof_current_proc(LABEL(mercury__parser__parse_list_3_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_list_3_0_i44);
	detstackvar(3) = (Integer) r2;
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__get_token_4_0),
		mercury__parser__parse_list_3_0_i28,
		STATIC(mercury__parser__parse_list_3_0));
Define_label(mercury__parser__parse_list_3_0_i28);
	update_prof_current_proc(LABEL(mercury__parser__parse_list_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_list_3_0_i27);
	detstackvar(5) = (Integer) r4;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 4)));
	{
	Declare_entry(mercury____Unify___lexer__token_0_0);
	call_localret(ENTRY(mercury____Unify___lexer__token_0_0),
		mercury__parser__parse_list_3_0_i30,
		STATIC(mercury__parser__parse_list_3_0));
	}
Define_label(mercury__parser__parse_list_3_0_i30);
	update_prof_current_proc(LABEL(mercury__parser__parse_list_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_list_3_0_i27);
	tag_incr_hp(r1, mktag(0), ((Integer) 1));
	tag_incr_hp(r2, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_4);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	r2 = (Integer) detstackvar(5);
	decr_sp_pop_msg(7);
	proceed();
	}
Define_label(mercury__parser__parse_list_3_0_i27);
	r1 = string_const("expecting ']' or operator", 25);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	tailcall(STATIC(mercury__parser__unexpected__ua10000_4_0),
		STATIC(mercury__parser__parse_list_3_0));
Define_label(mercury__parser__parse_list_3_0_i19);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 4)))))
		GOTO_LABEL(mercury__parser__parse_list_3_0_i35);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 1));
	tag_incr_hp(r4, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_4);
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) detstackvar(1);
	tag_incr_hp(r6, mktag(1), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(1), (Integer) r6, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) r6;
	field(mktag(1), (Integer) r6, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r4, ((Integer) 2)) = (Integer) r3;
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) r5;
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mkword(mktag(0), (Integer) mercury_data_parser__common_1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
	}
Define_label(mercury__parser__parse_list_3_0_i35);
	r4 = (Integer) r2;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r3 = string_const("expected comma, `|', `]', or operator", 37);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	tailcall(STATIC(mercury__parser__unexpected_tok__ua10000_6_0),
		STATIC(mercury__parser__parse_list_3_0));
Define_label(mercury__parser__parse_list_3_0_i7);
	r2 = (Integer) detstackvar(4);
	r1 = string_const("unexpected end-of-file in list", 30);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	tailcall(STATIC(mercury__parser__error__ua10000_4_0),
		STATIC(mercury__parser__parse_list_3_0));
Define_label(mercury__parser__parse_list_3_0_i44);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module13)
	init_entry(mercury__parser__parse_args_3_0);
	init_label(mercury__parser__parse_args_3_0_i2);
	init_label(mercury__parser__parse_args_3_0_i7);
	init_label(mercury__parser__parse_args_3_0_i12);
	init_label(mercury__parser__parse_args_3_0_i9);
	init_label(mercury__parser__parse_args_3_0_i17);
	init_label(mercury__parser__parse_args_3_0_i6);
	init_label(mercury__parser__parse_args_3_0_i3);
BEGIN_CODE

/* code for predicate 'parser__parse_args'/3 in mode 0 */
Define_static(mercury__parser__parse_args_3_0);
	r3 = (Integer) r1;
	r1 = ((Integer) 1201);
	r2 = ((Integer) 0);
	incr_sp_push_msg(3, "parser__parse_args");
	detstackvar(3) = (Integer) succip;
	call_localret(STATIC(mercury__parser__parse_term_2_5_0),
		mercury__parser__parse_args_3_0_i2,
		STATIC(mercury__parser__parse_args_3_0));
Define_label(mercury__parser__parse_args_3_0_i2);
	update_prof_current_proc(LABEL(mercury__parser__parse_args_3_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_args_3_0_i3);
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__parser__get_token_4_0),
		mercury__parser__parse_args_3_0_i7,
		STATIC(mercury__parser__parse_args_3_0));
Define_label(mercury__parser__parse_args_3_0_i7);
	update_prof_current_proc(LABEL(mercury__parser__parse_args_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__parse_args_3_0_i6);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 8)))))
		GOTO_LABEL(mercury__parser__parse_args_3_0_i9);
	r1 = (Integer) r4;
	localcall(mercury__parser__parse_args_3_0,
		LABEL(mercury__parser__parse_args_3_0_i12),
		STATIC(mercury__parser__parse_args_3_0));
Define_label(mercury__parser__parse_args_3_0_i12);
	update_prof_current_proc(LABEL(mercury__parser__parse_args_3_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__parse_args_3_0_i3);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 1));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
	}
Define_label(mercury__parser__parse_args_3_0_i9);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 2)))))
		GOTO_LABEL(mercury__parser__parse_args_3_0_i17);
	tag_incr_hp(r1, mktag(0), ((Integer) 1));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__parser__parse_args_3_0_i17);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = string_const("expected `,', `)', or operator", 30);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__parser__unexpected_tok__ua10000_6_0),
		STATIC(mercury__parser__parse_args_3_0));
Define_label(mercury__parser__parse_args_3_0_i6);
	r2 = (Integer) detstackvar(2);
	r1 = string_const("unexpected end-of-file in argument list", 39);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__parser__error__ua10000_4_0),
		STATIC(mercury__parser__parse_args_3_0));
Define_label(mercury__parser__parse_args_3_0_i3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module14)
	init_entry(mercury__parser__could_start_term_2_0);
	init_label(mercury__parser__could_start_term_2_0_i5);
	init_label(mercury__parser__could_start_term_2_0_i7);
	init_label(mercury__parser__could_start_term_2_0_i4);
	init_label(mercury__parser__could_start_term_2_0_i16);
	init_label(mercury__parser__could_start_term_2_0_i23);
BEGIN_CODE

/* code for predicate 'parser__could_start_term'/2 in mode 0 */
Define_static(mercury__parser__could_start_term_2_0);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__parser__could_start_term_2_0_i4);
	COMPUTED_GOTO(unmkbody((Integer) r1),
		LABEL(mercury__parser__could_start_term_2_0_i5) AND
		LABEL(mercury__parser__could_start_term_2_0_i5) AND
		LABEL(mercury__parser__could_start_term_2_0_i7) AND
		LABEL(mercury__parser__could_start_term_2_0_i5) AND
		LABEL(mercury__parser__could_start_term_2_0_i7) AND
		LABEL(mercury__parser__could_start_term_2_0_i5) AND
		LABEL(mercury__parser__could_start_term_2_0_i7) AND
		LABEL(mercury__parser__could_start_term_2_0_i7) AND
		LABEL(mercury__parser__could_start_term_2_0_i7) AND
		LABEL(mercury__parser__could_start_term_2_0_i7) AND
		LABEL(mercury__parser__could_start_term_2_0_i7));
Define_label(mercury__parser__could_start_term_2_0_i5);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury__parser__could_start_term_2_0_i7);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury__parser__could_start_term_2_0_i4);
	if (((Integer) r2 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__parser__could_start_term_2_0_i16);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__parser__could_start_term_2_0_i5) AND
		LABEL(mercury__parser__could_start_term_2_0_i5) AND
		LABEL(mercury__parser__could_start_term_2_0_i5) AND
		LABEL(mercury__parser__could_start_term_2_0_i7) AND
		LABEL(mercury__parser__could_start_term_2_0_i7) AND
		LABEL(mercury__parser__could_start_term_2_0_i7));
Define_label(mercury__parser__could_start_term_2_0_i16);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__parser__could_start_term_2_0_i23);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury__parser__could_start_term_2_0_i23);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module15)
	init_entry(mercury__parser__get_token_3_0);
	init_label(mercury__parser__get_token_3_0_i2);
	init_label(mercury__parser__get_token_3_0_i1000);
BEGIN_CODE

/* code for predicate 'parser__get_token'/3 in mode 0 */
Define_static(mercury__parser__get_token_3_0);
	incr_sp_push_msg(1, "parser__get_token");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__parser__get_token_4_0),
		mercury__parser__get_token_3_0_i2,
		STATIC(mercury__parser__get_token_3_0));
Define_label(mercury__parser__get_token_3_0_i2);
	update_prof_current_proc(LABEL(mercury__parser__get_token_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__get_token_3_0_i1000);
	r1 = TRUE;
	r3 = (Integer) r4;
	proceed();
Define_label(mercury__parser__get_token_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module16)
	init_entry(mercury__parser__get_token_4_0);
	init_label(mercury__parser__get_token_4_0_i1);
BEGIN_CODE

/* code for predicate 'parser__get_token'/4 in mode 0 */
Define_static(mercury__parser__get_token_4_0);
	{
	Word tempr1, tempr2;
	tempr1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	if (((Integer) tempr1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__parser__get_token_4_0_i1);
	tag_incr_hp(r4, mktag(0), ((Integer) 5));
	tempr2 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	field(mktag(0), (Integer) r4, ((Integer) 4)) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 4));
	field(mktag(0), (Integer) r4, ((Integer) 2)) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	field(mktag(0), (Integer) r4, ((Integer) 3)) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 1));
	r3 = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 0));
	r1 = TRUE;
	proceed();
	}
Define_label(mercury__parser__get_token_4_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module17)
	init_entry(mercury__parser__peek_token_3_0);
	init_label(mercury__parser__peek_token_3_0_i1);
BEGIN_CODE

/* code for predicate 'parser__peek_token'/3 in mode 0 */
Define_static(mercury__parser__peek_token_3_0);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	if (((Integer) tempr1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__parser__peek_token_3_0_i1);
	r3 = (Integer) r1;
	r2 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0)), ((Integer) 0));
	r1 = TRUE;
	proceed();
	}
Define_label(mercury__parser__peek_token_3_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module18)
	init_entry(mercury__parser__add_var_4_0);
	init_label(mercury__parser__add_var_4_0_i5);
	init_label(mercury__parser__add_var_4_0_i2);
	init_label(mercury__parser__add_var_4_0_i8);
	init_label(mercury__parser__add_var_4_0_i7);
	init_label(mercury__parser__add_var_4_0_i10);
	init_label(mercury__parser__add_var_4_0_i11);
	init_label(mercury__parser__add_var_4_0_i12);
BEGIN_CODE

/* code for predicate 'parser__add_var'/4 in mode 0 */
Define_static(mercury__parser__add_var_4_0);
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 4));
	r4 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	r5 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r6 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r7 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	incr_sp_push_msg(7, "parser__add_var");
	detstackvar(7) = (Integer) succip;
	if ((strcmp((char *)(Integer) r1, (char *)string_const("_", 1)) !=0))
		GOTO_LABEL(mercury__parser__add_var_4_0_i2);
	detstackvar(3) = (Integer) r7;
	detstackvar(4) = (Integer) r6;
	detstackvar(5) = (Integer) r4;
	detstackvar(6) = (Integer) r3;
	r1 = (Integer) r5;
	{
	Declare_entry(mercury__varset__new_var_3_0);
	call_localret(ENTRY(mercury__varset__new_var_3_0),
		mercury__parser__add_var_4_0_i5,
		STATIC(mercury__parser__add_var_4_0));
	}
Define_label(mercury__parser__add_var_4_0_i5);
	update_prof_current_proc(LABEL(mercury__parser__add_var_4_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) r3;
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__parser__add_var_4_0_i2);
	detstackvar(5) = (Integer) r4;
	r4 = (Integer) r1;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r7;
	detstackvar(4) = (Integer) r6;
	detstackvar(6) = (Integer) r3;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__parser__add_var_4_0_i8,
		STATIC(mercury__parser__add_var_4_0));
	}
Define_label(mercury__parser__add_var_4_0_i8);
	update_prof_current_proc(LABEL(mercury__parser__add_var_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__parser__add_var_4_0_i7);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__parser__add_var_4_0_i7);
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__varset__new_var_3_0);
	call_localret(ENTRY(mercury__varset__new_var_3_0),
		mercury__parser__add_var_4_0_i10,
		STATIC(mercury__parser__add_var_4_0));
	}
Define_label(mercury__parser__add_var_4_0_i10);
	update_prof_current_proc(LABEL(mercury__parser__add_var_4_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__varset__name_var_4_0);
	call_localret(ENTRY(mercury__varset__name_var_4_0),
		mercury__parser__add_var_4_0_i11,
		STATIC(mercury__parser__add_var_4_0));
	}
Define_label(mercury__parser__add_var_4_0_i11);
	update_prof_current_proc(LABEL(mercury__parser__add_var_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r4 = (Integer) r2;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(6);
	r5 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__det_insert_4_0);
	call_localret(ENTRY(mercury__map__det_insert_4_0),
		mercury__parser__add_var_4_0_i12,
		STATIC(mercury__parser__add_var_4_0));
	}
Define_label(mercury__parser__add_var_4_0_i12);
	update_prof_current_proc(LABEL(mercury__parser__add_var_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module19)
	init_entry(mercury__parser__get_ops_table_3_0);
BEGIN_CODE

/* code for predicate 'parser__get_ops_table'/3 in mode 0 */
Define_static(mercury__parser__get_ops_table_3_0);
	r2 = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module20)
	init_entry(mercury__parser__adjust_priority_3_0);
	init_label(mercury__parser__adjust_priority_3_0_i3);
BEGIN_CODE

/* code for predicate 'parser__adjust_priority'/3 in mode 0 */
Define_static(mercury__parser__adjust_priority_3_0);
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury__parser__adjust_priority_3_0_i3);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__parser__adjust_priority_3_0_i3);
	r1 = ((Integer) r2 - ((Integer) 1));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__parser_module21)
	init_entry(mercury__parser__get_term_context_4_0);
	init_label(mercury__parser__get_term_context_4_0_i2);
BEGIN_CODE

/* code for predicate 'parser__get_term_context'/4 in mode 0 */
Define_static(mercury__parser__get_term_context_4_0);
	incr_sp_push_msg(2, "parser__get_term_context");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r3 = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__term__context_init_3_0);
	call_localret(ENTRY(mercury__term__context_init_3_0),
		mercury__parser__get_term_context_4_0_i2,
		STATIC(mercury__parser__get_term_context_4_0));
	}
Define_label(mercury__parser__get_term_context_4_0_i2);
	update_prof_current_proc(LABEL(mercury__parser__get_term_context_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__parser_bunch_0(void)
{
	mercury__parser_module0();
	mercury__parser_module1();
	mercury__parser_module2();
	mercury__parser_module3();
	mercury__parser_module4();
	mercury__parser_module5();
	mercury__parser_module6();
	mercury__parser_module7();
	mercury__parser_module8();
	mercury__parser_module9();
	mercury__parser_module10();
	mercury__parser_module11();
	mercury__parser_module12();
	mercury__parser_module13();
	mercury__parser_module14();
	mercury__parser_module15();
	mercury__parser_module16();
	mercury__parser_module17();
	mercury__parser_module18();
	mercury__parser_module19();
	mercury__parser_module20();
	mercury__parser_module21();
}

#endif

void mercury__parser__init(void); /* suppress gcc warning */
void mercury__parser__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__parser_bunch_0();
#endif
}
