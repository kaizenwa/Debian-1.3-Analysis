/*
** Automatically generated from `garbage_out.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__garbage_out__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury____Index___garbage_out_gc_label_info_0__ua10000_2_0);
Declare_static(mercury____Index___garbage_out_garbage_output_0__ua10000_2_0);
Declare_static(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0);
Declare_label(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0_i1002);
Define_extern_entry(mercury__garbage_out__do_garbage_out_4_0);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i2);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i3);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i7);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i8);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i9);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i10);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i11);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i12);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i13);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i14);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i15);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i4);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i17);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i18);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i19);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i20);
Declare_label(mercury__garbage_out__do_garbage_out_4_0_i21);
Declare_static(mercury__garbage_out__create_cont_list_2_0);
Declare_label(mercury__garbage_out__create_cont_list_2_0_i4);
Declare_label(mercury__garbage_out__create_cont_list_2_0_i8);
Declare_label(mercury__garbage_out__create_cont_list_2_0_i7);
Declare_label(mercury__garbage_out__create_cont_list_2_0_i5);
Declare_label(mercury__garbage_out__create_cont_list_2_0_i3);
Declare_label(mercury__garbage_out__create_cont_list_2_0_i2);
Declare_static(mercury__garbage_out__create_cont_list_2_2_0);
Declare_label(mercury__garbage_out__create_cont_list_2_2_0_i4);
Declare_label(mercury__garbage_out__create_cont_list_2_2_0_i5);
Declare_label(mercury__garbage_out__create_cont_list_2_2_0_i6);
Declare_label(mercury__garbage_out__create_cont_list_2_2_0_i1002);
Declare_static(mercury__garbage_out__proc_instr_list_3_0);
Declare_label(mercury__garbage_out__proc_instr_list_3_0_i7);
Declare_label(mercury__garbage_out__proc_instr_list_3_0_i8);
Declare_label(mercury__garbage_out__proc_instr_list_3_0_i1005);
Declare_label(mercury__garbage_out__proc_instr_list_3_0_i1007);
Declare_static(mercury__garbage_out__get_det_3_0);
Declare_label(mercury__garbage_out__get_det_3_0_i32);
Declare_label(mercury__garbage_out__get_det_3_0_i13);
Declare_label(mercury__garbage_out__get_det_3_0_i10);
Declare_label(mercury__garbage_out__get_det_3_0_i7);
Declare_label(mercury__garbage_out__get_det_3_0_i21);
Declare_label(mercury__garbage_out__get_det_3_0_i18);
Declare_label(mercury__garbage_out__get_det_3_0_i30);
Declare_label(mercury__garbage_out__get_det_3_0_i1);
Declare_label(mercury__garbage_out__get_det_3_0_i5);
Declare_label(mercury__garbage_out__get_det_3_0_i6);
Declare_label(mercury__garbage_out__get_det_3_0_i3);
Declare_static(mercury__garbage_out__write_cont_list_3_0);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i4);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i8);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i9);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i10);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i5);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i11);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i12);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i15);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i14);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i16);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i18);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i13);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i19);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i20);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i21);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i22);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i23);
Declare_label(mercury__garbage_out__write_cont_list_3_0_i1000);
Declare_static(mercury__garbage_out__write_liveinfo_list_3_0);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i7);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i4);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i8);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i9);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i10);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i11);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i12);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i16);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i17);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i18);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i13);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i19);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i20);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i21);
Declare_label(mercury__garbage_out__write_liveinfo_list_3_0_i1004);
Declare_static(mercury__garbage_out__write_lval_list_3_0);
Declare_label(mercury__garbage_out__write_lval_list_3_0_i4);
Declare_label(mercury__garbage_out__write_lval_list_3_0_i10);
Declare_label(mercury__garbage_out__write_lval_list_3_0_i5);
Declare_label(mercury__garbage_out__write_lval_list_3_0_i1003);
Declare_static(mercury__garbage_out__write_liveval_3_0);
Declare_label(mercury__garbage_out__write_liveval_3_0_i1017);
Declare_label(mercury__garbage_out__write_liveval_3_0_i1016);
Declare_label(mercury__garbage_out__write_liveval_3_0_i1015);
Declare_label(mercury__garbage_out__write_liveval_3_0_i1014);
Declare_label(mercury__garbage_out__write_liveval_3_0_i5);
Declare_label(mercury__garbage_out__write_liveval_3_0_i6);
Declare_label(mercury__garbage_out__write_liveval_3_0_i7);
Declare_label(mercury__garbage_out__write_liveval_3_0_i9);
Declare_label(mercury__garbage_out__write_liveval_3_0_i10);
Declare_label(mercury__garbage_out__write_liveval_3_0_i11);
Declare_label(mercury__garbage_out__write_liveval_3_0_i21);
Declare_label(mercury__garbage_out__write_liveval_3_0_i22);
Declare_label(mercury__garbage_out__write_liveval_3_0_i23);
Declare_label(mercury__garbage_out__write_liveval_3_0_i1013);
Declare_label(mercury__garbage_out__write_liveval_3_0_i26);
Declare_label(mercury__garbage_out__write_liveval_3_0_i28);
Declare_label(mercury__garbage_out__write_liveval_3_0_i30);
Declare_label(mercury__garbage_out__write_liveval_3_0_i32);
Declare_label(mercury__garbage_out__write_liveval_3_0_i34);
Declare_label(mercury__garbage_out__write_liveval_3_0_i25);
Declare_label(mercury__garbage_out__write_liveval_3_0_i40);
Declare_label(mercury__garbage_out__write_liveval_3_0_i41);
Declare_label(mercury__garbage_out__write_liveval_3_0_i37);
Declare_label(mercury__garbage_out__write_liveval_3_0_i42);
Declare_label(mercury__garbage_out__write_liveval_3_0_i47);
Declare_label(mercury__garbage_out__write_liveval_3_0_i36);
Declare_label(mercury__garbage_out__write_liveval_3_0_i51);
Declare_label(mercury__garbage_out__write_liveval_3_0_i1002);
Declare_label(mercury__garbage_out__write_liveval_3_0_i1003);
Declare_label(mercury__garbage_out__write_liveval_3_0_i1004);
Declare_label(mercury__garbage_out__write_liveval_3_0_i1005);
Declare_static(mercury__garbage_out__write_shapes_3_0);
Declare_label(mercury__garbage_out__write_shapes_3_0_i4);
Declare_label(mercury__garbage_out__write_shapes_3_0_i5);
Declare_label(mercury__garbage_out__write_shapes_3_0_i6);
Declare_label(mercury__garbage_out__write_shapes_3_0_i11);
Declare_label(mercury__garbage_out__write_shapes_3_0_i12);
Declare_label(mercury__garbage_out__write_shapes_3_0_i13);
Declare_label(mercury__garbage_out__write_shapes_3_0_i10);
Declare_label(mercury__garbage_out__write_shapes_3_0_i14);
Declare_label(mercury__garbage_out__write_shapes_3_0_i15);
Declare_label(mercury__garbage_out__write_shapes_3_0_i16);
Declare_label(mercury__garbage_out__write_shapes_3_0_i9);
Declare_label(mercury__garbage_out__write_shapes_3_0_i19);
Declare_label(mercury__garbage_out__write_shapes_3_0_i20);
Declare_label(mercury__garbage_out__write_shapes_3_0_i21);
Declare_label(mercury__garbage_out__write_shapes_3_0_i22);
Declare_label(mercury__garbage_out__write_shapes_3_0_i23);
Declare_label(mercury__garbage_out__write_shapes_3_0_i24);
Declare_label(mercury__garbage_out__write_shapes_3_0_i25);
Declare_label(mercury__garbage_out__write_shapes_3_0_i26);
Declare_label(mercury__garbage_out__write_shapes_3_0_i18);
Declare_label(mercury__garbage_out__write_shapes_3_0_i29);
Declare_label(mercury__garbage_out__write_shapes_3_0_i30);
Declare_label(mercury__garbage_out__write_shapes_3_0_i31);
Declare_label(mercury__garbage_out__write_shapes_3_0_i32);
Declare_label(mercury__garbage_out__write_shapes_3_0_i33);
Declare_label(mercury__garbage_out__write_shapes_3_0_i28);
Declare_label(mercury__garbage_out__write_shapes_3_0_i35);
Declare_label(mercury__garbage_out__write_shapes_3_0_i36);
Declare_label(mercury__garbage_out__write_shapes_3_0_i37);
Declare_label(mercury__garbage_out__write_shapes_3_0_i7);
Declare_label(mercury__garbage_out__write_shapes_3_0_i38);
Declare_label(mercury__garbage_out__write_shapes_3_0_i1000);
Declare_static(mercury__garbage_out__write_shape_tag_3_0);
Declare_label(mercury__garbage_out__write_shape_tag_3_0_i1002);
Declare_label(mercury__garbage_out__write_shape_tag_3_0_i7);
Declare_label(mercury__garbage_out__write_shape_tag_3_0_i8);
Declare_label(mercury__garbage_out__write_shape_tag_3_0_i6);
Declare_label(mercury__garbage_out__write_shape_tag_3_0_i10);
Declare_label(mercury__garbage_out__write_shape_tag_3_0_i11);
Declare_static(mercury__garbage_out__write_complicated_3_0);
Declare_label(mercury__garbage_out__write_complicated_3_0_i4);
Declare_label(mercury__garbage_out__write_complicated_3_0_i5);
Declare_label(mercury__garbage_out__write_complicated_3_0_i1002);
Declare_static(mercury__garbage_out__write_shape_list_3_0);
Declare_label(mercury__garbage_out__write_shape_list_3_0_i4);
Declare_label(mercury__garbage_out__write_shape_list_3_0_i5);
Declare_label(mercury__garbage_out__write_shape_list_3_0_i1002);
Declare_static(mercury__garbage_out__write_int_list_3_0);
Declare_label(mercury__garbage_out__write_int_list_3_0_i4);
Declare_label(mercury__garbage_out__write_int_list_3_0_i5);
Declare_label(mercury__garbage_out__write_int_list_3_0_i1002);
Declare_static(mercury__garbage_out__write_abs_list_3_0);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i4);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i7);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i8);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i6);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i9);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i10);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i5);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i13);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i14);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i15);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i12);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i16);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i17);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i11);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i18);
Declare_label(mercury__garbage_out__write_abs_list_3_0_i1009);
Declare_static(mercury__garbage_out__write_special_preds_3_0);
Declare_label(mercury__garbage_out__write_special_preds_3_0_i4);
Declare_label(mercury__garbage_out__write_special_preds_3_0_i5);
Declare_label(mercury__garbage_out__write_special_preds_3_0_i6);
Declare_label(mercury__garbage_out__write_special_preds_3_0_i7);
Declare_label(mercury__garbage_out__write_special_preds_3_0_i8);
Declare_label(mercury__garbage_out__write_special_preds_3_0_i9);
Declare_label(mercury__garbage_out__write_special_preds_3_0_i10);
Declare_label(mercury__garbage_out__write_special_preds_3_0_i1002);
Define_extern_entry(mercury____Unify___garbage_out__garbage_output_0_0);
Declare_label(mercury____Unify___garbage_out__garbage_output_0_0_i2);
Declare_label(mercury____Unify___garbage_out__garbage_output_0_0_i4);
Declare_label(mercury____Unify___garbage_out__garbage_output_0_0_i1);
Define_extern_entry(mercury____Index___garbage_out__garbage_output_0_0);
Define_extern_entry(mercury____Compare___garbage_out__garbage_output_0_0);
Declare_label(mercury____Compare___garbage_out__garbage_output_0_0_i4);
Declare_label(mercury____Compare___garbage_out__garbage_output_0_0_i5);
Declare_label(mercury____Compare___garbage_out__garbage_output_0_0_i3);
Declare_label(mercury____Compare___garbage_out__garbage_output_0_0_i10);
Define_extern_entry(mercury____Unify___garbage_out__cont_list_0_0);
Define_extern_entry(mercury____Index___garbage_out__cont_list_0_0);
Define_extern_entry(mercury____Compare___garbage_out__cont_list_0_0);
Define_extern_entry(mercury____Unify___garbage_out__gc_label_info_0_0);
Declare_label(mercury____Unify___garbage_out__gc_label_info_0_0_i2);
Declare_label(mercury____Unify___garbage_out__gc_label_info_0_0_i1);
Define_extern_entry(mercury____Index___garbage_out__gc_label_info_0_0);
Define_extern_entry(mercury____Compare___garbage_out__gc_label_info_0_0);
Declare_label(mercury____Compare___garbage_out__gc_label_info_0_0_i4);
Declare_label(mercury____Compare___garbage_out__gc_label_info_0_0_i5);
Declare_label(mercury____Compare___garbage_out__gc_label_info_0_0_i3);
Declare_label(mercury____Compare___garbage_out__gc_label_info_0_0_i10);
Declare_label(mercury____Compare___garbage_out__gc_label_info_0_0_i16);
Define_extern_entry(mercury____Unify___garbage_out__num_slots_0_0);
Declare_label(mercury____Unify___garbage_out__num_slots_0_0_i1);
Define_extern_entry(mercury____Index___garbage_out__num_slots_0_0);
Define_extern_entry(mercury____Compare___garbage_out__num_slots_0_0);
Define_extern_entry(mercury____Unify___garbage_out__det_0_0);
Declare_label(mercury____Unify___garbage_out__det_0_0_i1);
Define_extern_entry(mercury____Index___garbage_out__det_0_0);
Define_extern_entry(mercury____Compare___garbage_out__det_0_0);

extern Word * mercury_data_garbage_out__base_type_layout_cont_list_0[];
Word * mercury_data_garbage_out__base_type_info_cont_list_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___garbage_out__cont_list_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___garbage_out__cont_list_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___garbage_out__cont_list_0_0),
	(Word *) (Integer) mercury_data_garbage_out__base_type_layout_cont_list_0
};

extern Word * mercury_data_garbage_out__base_type_layout_det_0[];
Word * mercury_data_garbage_out__base_type_info_det_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___garbage_out__det_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___garbage_out__det_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___garbage_out__det_0_0),
	(Word *) (Integer) mercury_data_garbage_out__base_type_layout_det_0
};

extern Word * mercury_data_garbage_out__base_type_layout_garbage_output_0[];
Word * mercury_data_garbage_out__base_type_info_garbage_output_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___garbage_out__garbage_output_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___garbage_out__garbage_output_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___garbage_out__garbage_output_0_0),
	(Word *) (Integer) mercury_data_garbage_out__base_type_layout_garbage_output_0
};

extern Word * mercury_data_garbage_out__base_type_layout_gc_label_info_0[];
Word * mercury_data_garbage_out__base_type_info_gc_label_info_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___garbage_out__gc_label_info_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___garbage_out__gc_label_info_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___garbage_out__gc_label_info_0_0),
	(Word *) (Integer) mercury_data_garbage_out__base_type_layout_gc_label_info_0
};

extern Word * mercury_data_garbage_out__base_type_layout_num_slots_0[];
Word * mercury_data_garbage_out__base_type_info_num_slots_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___garbage_out__num_slots_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___garbage_out__num_slots_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___garbage_out__num_slots_0_0),
	(Word *) (Integer) mercury_data_garbage_out__base_type_layout_num_slots_0
};

extern Word * mercury_data_garbage_out__common_11[];
Word * mercury_data_garbage_out__base_type_layout_num_slots_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_garbage_out__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_garbage_out__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_garbage_out__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_garbage_out__common_11)
};

extern Word * mercury_data_garbage_out__common_15[];
Word * mercury_data_garbage_out__base_type_layout_gc_label_info_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_garbage_out__common_15),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_garbage_out__common_19[];
Word * mercury_data_garbage_out__base_type_layout_garbage_output_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_garbage_out__common_19),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_garbage_out__common_20[];
Word * mercury_data_garbage_out__base_type_layout_det_0[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_20),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_20),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_20),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_20)
};

extern Word * mercury_data_garbage_out__common_21[];
Word * mercury_data_garbage_out__base_type_layout_cont_list_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_garbage_out__common_21),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_garbage_out__common_21),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_garbage_out__common_21),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_garbage_out__common_21)
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_mercury_builtin__base_type_info_term_0[];
extern Word * mercury_data_prog_data__base_type_info_inst_0[];
Word * mercury_data_garbage_out__common_0[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_term_0,
	(Word *) (Integer) mercury_data_prog_data__base_type_info_inst_0
};

extern Word * mercury_data_shapes__base_type_info_shape_num_0[];
extern Word * mercury_data_hlds_module__base_type_info_shape_0[];
Word * mercury_data_garbage_out__common_1[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_shapes__base_type_info_shape_num_0,
	(Word *) (Integer) mercury_data_hlds_module__base_type_info_shape_0
};

extern Word * mercury_data_prog_data__base_type_info_sym_name_0[];
extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_garbage_out__common_2[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_prog_data__base_type_info_sym_name_0,
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word mercury_data_garbage_out__common_3[] = {
	((Integer) 2)
};

Word mercury_data_garbage_out__common_4[] = {
	((Integer) 0)
};

Word mercury_data_garbage_out__common_5[] = {
	((Integer) 1)
};

Word * mercury_data_garbage_out__common_6[] = {
	(Word *) string_const(") - ", 4),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_garbage_out__common_7[] = {
	(Word *) string_const(" - ", 3),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_garbage_out__common_8[] = {
	(Word *) string_const(")", 1),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_garbage_out__common_7)
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
Word * mercury_data_garbage_out__common_9[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_1)
};

Word * mercury_data_garbage_out__common_10[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_garbage_out__common_11[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_10)
};

extern Word * mercury_data_llds__base_type_info_code_addr_0[];
Word * mercury_data_garbage_out__common_12[] = {
	(Word *) (Integer) mercury_data_llds__base_type_info_code_addr_0
};

Word * mercury_data_garbage_out__common_13[] = {
	(Word *) (Integer) mercury_data_garbage_out__base_type_info_det_0
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
extern Word * mercury_data_llds__base_type_info_liveinfo_0[];
Word * mercury_data_garbage_out__common_14[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mercury_data_llds__base_type_info_liveinfo_0
};

Word * mercury_data_garbage_out__common_15[] = {
	(Word *) ((Integer) 4),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_12),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_13),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_10),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_14),
	(Word *) string_const("gc_label_info", 13)
};

Word * mercury_data_garbage_out__common_16[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mercury_data_garbage_out__base_type_info_gc_label_info_0
};

Word * mercury_data_garbage_out__common_17[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_9),
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

extern Word * mercury_data_hlds_module__base_type_info_maybe_shape_num_0[];
Word * mercury_data_garbage_out__common_18[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_2),
	(Word *) (Integer) mercury_data_hlds_module__base_type_info_maybe_shape_num_0
};

Word * mercury_data_garbage_out__common_19[] = {
	(Word *) ((Integer) 3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_16),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_17),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_18),
	(Word *) string_const("garbage_output", 14)
};

Word * mercury_data_garbage_out__common_20[] = {
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 3),
	(Word *) string_const("deterministic", 13),
	(Word *) string_const("nondeterministic", 16),
	(Word *) string_const("commit", 6)
};

Word * mercury_data_garbage_out__common_21[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_16)
};

BEGIN_MODULE(mercury__garbage_out_module0)
	init_entry(mercury____Index___garbage_out_gc_label_info_0__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___garbage_out_gc_label_info_0__ua10000'/2 in mode 0 */
Define_static(mercury____Index___garbage_out_gc_label_info_0__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module1)
	init_entry(mercury____Index___garbage_out_garbage_output_0__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___garbage_out_garbage_output_0__ua10000'/2 in mode 0 */
Define_static(mercury____Index___garbage_out_garbage_output_0__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module2)
	init_entry(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0);
	init_label(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0_i1002);
BEGIN_CODE

/* code for predicate 'garbage_out__maybe_write_comma_space__ua10000'/3 in mode 0 */
Define_static(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0_i1002);
	r1 = string_const(", ", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0));
	}
Define_label(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module3)
	init_entry(mercury__garbage_out__do_garbage_out_4_0);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i2);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i3);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i7);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i8);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i9);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i10);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i11);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i12);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i13);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i14);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i15);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i4);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i17);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i18);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i19);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i20);
	init_label(mercury__garbage_out__do_garbage_out_4_0_i21);
BEGIN_CODE

/* code for predicate 'garbage_out__do_garbage_out'/4 in mode 0 */
Define_entry(mercury__garbage_out__do_garbage_out_4_0);
	incr_sp_push_msg(6, "garbage_out__do_garbage_out");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	r2 = string_const(".garb", 5);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__garbage_out__do_garbage_out_4_0_i2,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i2);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__io__tell_4_0);
	call_localret(ENTRY(mercury__io__tell_4_0),
		mercury__garbage_out__do_garbage_out_4_0_i3,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i3);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__do_garbage_out_4_0_i4);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__garbage_out__create_cont_list_2_0),
		mercury__garbage_out__do_garbage_out_4_0_i7,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
Define_label(mercury__garbage_out__do_garbage_out_4_0_i7);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__garbage_out__write_cont_list_3_0),
		mercury__garbage_out__do_garbage_out_4_0_i8,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
Define_label(mercury__garbage_out__do_garbage_out_4_0_i8);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_0);
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_1);
	r3 = (Integer) field(mktag(0), (Integer) detstackvar(3), ((Integer) 0));
	{
	Declare_entry(mercury__map__values_2_0);
	call_localret(ENTRY(mercury__map__values_2_0),
		mercury__garbage_out__do_garbage_out_4_0_i9,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i9);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_1);
	{
	Declare_entry(mercury__list__sort_and_remove_dups_2_0);
	call_localret(ENTRY(mercury__list__sort_and_remove_dups_2_0),
		mercury__garbage_out__do_garbage_out_4_0_i10,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i10);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__garbage_out__write_shapes_3_0),
		mercury__garbage_out__do_garbage_out_4_0_i11,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
Define_label(mercury__garbage_out__do_garbage_out_4_0_i11);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_2);
	r2 = (Integer) mercury_data_hlds_module__base_type_info_maybe_shape_num_0;
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__map__to_assoc_list_2_0);
	call_localret(ENTRY(mercury__map__to_assoc_list_2_0),
		mercury__garbage_out__do_garbage_out_4_0_i12,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i12);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__garbage_out__write_abs_list_3_0),
		mercury__garbage_out__do_garbage_out_4_0_i13,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
Define_label(mercury__garbage_out__do_garbage_out_4_0_i13);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	detstackvar(1) = (Integer) r1;
	{
	extern Word * mercury_data_llds__base_type_info_label_0[];
	r1 = (Integer) mercury_data_llds__base_type_info_label_0;
	}
	r2 = (Integer) mercury_data_shapes__base_type_info_shape_num_0;
	r3 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__to_assoc_list_2_0);
	call_localret(ENTRY(mercury__map__to_assoc_list_2_0),
		mercury__garbage_out__do_garbage_out_4_0_i14,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i14);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__garbage_out__write_special_preds_3_0),
		mercury__garbage_out__do_garbage_out_4_0_i15,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
Define_label(mercury__garbage_out__do_garbage_out_4_0_i15);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	{
	Declare_entry(mercury__io__told_2_0);
	tailcall(ENTRY(mercury__io__told_2_0),
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i4);
	r1 = string_const("garbage_out.m", 13);
	{
	Declare_entry(mercury__io__progname_base_4_0);
	call_localret(ENTRY(mercury__io__progname_base_4_0),
		mercury__garbage_out__do_garbage_out_4_0_i17,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i17);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	detstackvar(2) = (Integer) r1;
	r1 = string_const("\n", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__do_garbage_out_4_0_i18,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i18);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__do_garbage_out_4_0_i19,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i19);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	r2 = (Integer) r1;
	r1 = string_const(": can't open `", 14);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__do_garbage_out_4_0_i20,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i20);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__do_garbage_out_4_0_i21,
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
Define_label(mercury__garbage_out__do_garbage_out_4_0_i21);
	update_prof_current_proc(LABEL(mercury__garbage_out__do_garbage_out_4_0));
	r2 = (Integer) r1;
	r1 = string_const("' for output\n", 13);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		ENTRY(mercury__garbage_out__do_garbage_out_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module4)
	init_entry(mercury__garbage_out__create_cont_list_2_0);
	init_label(mercury__garbage_out__create_cont_list_2_0_i4);
	init_label(mercury__garbage_out__create_cont_list_2_0_i8);
	init_label(mercury__garbage_out__create_cont_list_2_0_i7);
	init_label(mercury__garbage_out__create_cont_list_2_0_i5);
	init_label(mercury__garbage_out__create_cont_list_2_0_i3);
	init_label(mercury__garbage_out__create_cont_list_2_0_i2);
BEGIN_CODE

/* code for predicate 'garbage_out__create_cont_list'/2 in mode 0 */
Define_static(mercury__garbage_out__create_cont_list_2_0);
	incr_sp_push_msg(2, "garbage_out__create_cont_list");
	detstackvar(2) = (Integer) succip;
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__create_cont_list_2_0_i3);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localcall(mercury__garbage_out__create_cont_list_2_0,
		LABEL(mercury__garbage_out__create_cont_list_2_0_i4),
		STATIC(mercury__garbage_out__create_cont_list_2_0));
Define_label(mercury__garbage_out__create_cont_list_2_0_i4);
	update_prof_current_proc(LABEL(mercury__garbage_out__create_cont_list_2_0));
	r2 = (Integer) detstackvar(1);
	r3 = tag((Integer) r2);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__garbage_out__create_cont_list_2_0_i7);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	call_localret(STATIC(mercury__garbage_out__create_cont_list_2_2_0),
		mercury__garbage_out__create_cont_list_2_0_i8,
		STATIC(mercury__garbage_out__create_cont_list_2_0));
Define_label(mercury__garbage_out__create_cont_list_2_0_i8);
	update_prof_current_proc(LABEL(mercury__garbage_out__create_cont_list_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_garbage_out__base_type_info_gc_label_info_0;
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__list__append_3_1);
	tailcall(ENTRY(mercury__list__append_3_1),
		STATIC(mercury__garbage_out__create_cont_list_2_0));
	}
Define_label(mercury__garbage_out__create_cont_list_2_0_i7);
	if (((Integer) r3 == mktag(((Integer) 1))))
		GOTO_LABEL(mercury__garbage_out__create_cont_list_2_0_i5);
	if (((Integer) r3 != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__garbage_out__create_cont_list_2_0_i2);
Define_label(mercury__garbage_out__create_cont_list_2_0_i5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__garbage_out__create_cont_list_2_0_i3);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
Define_label(mercury__garbage_out__create_cont_list_2_0_i2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module5)
	init_entry(mercury__garbage_out__create_cont_list_2_2_0);
	init_label(mercury__garbage_out__create_cont_list_2_2_0_i4);
	init_label(mercury__garbage_out__create_cont_list_2_2_0_i5);
	init_label(mercury__garbage_out__create_cont_list_2_2_0_i6);
	init_label(mercury__garbage_out__create_cont_list_2_2_0_i1002);
BEGIN_CODE

/* code for predicate 'garbage_out__create_cont_list_2'/2 in mode 0 */
Define_static(mercury__garbage_out__create_cont_list_2_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__create_cont_list_2_2_0_i1002);
	incr_sp_push_msg(2, "garbage_out__create_cont_list_2");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 3));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	call_localret(STATIC(mercury__garbage_out__proc_instr_list_3_0),
		mercury__garbage_out__create_cont_list_2_2_0_i4,
		STATIC(mercury__garbage_out__create_cont_list_2_2_0));
Define_label(mercury__garbage_out__create_cont_list_2_2_0_i4);
	update_prof_current_proc(LABEL(mercury__garbage_out__create_cont_list_2_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_garbage_out__base_type_info_gc_label_info_0;
	{
	Declare_entry(mercury__list__reverse_2_0);
	call_localret(ENTRY(mercury__list__reverse_2_0),
		mercury__garbage_out__create_cont_list_2_2_0_i5,
		STATIC(mercury__garbage_out__create_cont_list_2_2_0));
	}
Define_label(mercury__garbage_out__create_cont_list_2_2_0_i5);
	update_prof_current_proc(LABEL(mercury__garbage_out__create_cont_list_2_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__garbage_out__create_cont_list_2_2_0,
		LABEL(mercury__garbage_out__create_cont_list_2_2_0_i6),
		STATIC(mercury__garbage_out__create_cont_list_2_2_0));
Define_label(mercury__garbage_out__create_cont_list_2_2_0_i6);
	update_prof_current_proc(LABEL(mercury__garbage_out__create_cont_list_2_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_garbage_out__base_type_info_gc_label_info_0;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__list__append_3_1);
	tailcall(ENTRY(mercury__list__append_3_1),
		STATIC(mercury__garbage_out__create_cont_list_2_2_0));
	}
Define_label(mercury__garbage_out__create_cont_list_2_2_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module6)
	init_entry(mercury__garbage_out__proc_instr_list_3_0);
	init_label(mercury__garbage_out__proc_instr_list_3_0_i7);
	init_label(mercury__garbage_out__proc_instr_list_3_0_i8);
	init_label(mercury__garbage_out__proc_instr_list_3_0_i1005);
	init_label(mercury__garbage_out__proc_instr_list_3_0_i1007);
BEGIN_CODE

/* code for predicate 'garbage_out__proc_instr_list'/3 in mode 0 */
Define_static(mercury__garbage_out__proc_instr_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__proc_instr_list_3_0_i1005);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if ((tag((Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__garbage_out__proc_instr_list_3_0_i1007);
	if (((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0)), ((Integer) 0)) != ((Integer) 2)))
		GOTO_LABEL(mercury__garbage_out__proc_instr_list_3_0_i1007);
	incr_sp_push_msg(6, "garbage_out__proc_instr_list");
	detstackvar(6) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(3), (Integer) tempr1, ((Integer) 3));
	detstackvar(4) = (Integer) r1;
	detstackvar(3) = (Integer) field(mktag(3), (Integer) tempr1, ((Integer) 2));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	detstackvar(2) = (Integer) r3;
	call_localret(STATIC(mercury__garbage_out__get_det_3_0),
		mercury__garbage_out__proc_instr_list_3_0_i7,
		STATIC(mercury__garbage_out__proc_instr_list_3_0));
	}
Define_label(mercury__garbage_out__proc_instr_list_3_0_i7);
	update_prof_current_proc(LABEL(mercury__garbage_out__proc_instr_list_3_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) mercury_data_llds__base_type_info_liveinfo_0;
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__garbage_out__proc_instr_list_3_0_i8,
		STATIC(mercury__garbage_out__proc_instr_list_3_0));
	}
Define_label(mercury__garbage_out__proc_instr_list_3_0_i8);
	update_prof_current_proc(LABEL(mercury__garbage_out__proc_instr_list_3_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 4));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__garbage_out__proc_instr_list_3_0,
		STATIC(mercury__garbage_out__proc_instr_list_3_0));
	}
Define_label(mercury__garbage_out__proc_instr_list_3_0_i1005);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__garbage_out__proc_instr_list_3_0_i1007);
	r1 = (Integer) r3;
	localtailcall(mercury__garbage_out__proc_instr_list_3_0,
		STATIC(mercury__garbage_out__proc_instr_list_3_0));
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module7)
	init_entry(mercury__garbage_out__get_det_3_0);
	init_label(mercury__garbage_out__get_det_3_0_i32);
	init_label(mercury__garbage_out__get_det_3_0_i13);
	init_label(mercury__garbage_out__get_det_3_0_i10);
	init_label(mercury__garbage_out__get_det_3_0_i7);
	init_label(mercury__garbage_out__get_det_3_0_i21);
	init_label(mercury__garbage_out__get_det_3_0_i18);
	init_label(mercury__garbage_out__get_det_3_0_i30);
	init_label(mercury__garbage_out__get_det_3_0_i1);
	init_label(mercury__garbage_out__get_det_3_0_i5);
	init_label(mercury__garbage_out__get_det_3_0_i6);
	init_label(mercury__garbage_out__get_det_3_0_i3);
BEGIN_CODE

/* code for predicate 'garbage_out__get_det'/3 in mode 0 */
Define_static(mercury__garbage_out__get_det_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i1);
Define_label(mercury__garbage_out__get_det_3_0_i32);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r5 = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	if ((tag((Integer) r5) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i7);
	if (((Integer) field(mktag(3), (Integer) r5, ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i7);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i10);
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	if (((Integer) r4 != ((Integer) 1)))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i13);
	r1 = (Integer) r3;
	r2 = (Integer) mkword(mktag(1), (Integer) mercury_data_garbage_out__common_3);
	GOTO_LABEL(mercury__garbage_out__get_det_3_0_i30);
Define_label(mercury__garbage_out__get_det_3_0_i13);
	r1 = (Integer) r3;
	GOTO_LABEL(mercury__garbage_out__get_det_3_0_i30);
Define_label(mercury__garbage_out__get_det_3_0_i10);
	r1 = (Integer) r3;
	r2 = (Integer) mkword(mktag(1), (Integer) mercury_data_garbage_out__common_4);
	GOTO_LABEL(mercury__garbage_out__get_det_3_0_i30);
Define_label(mercury__garbage_out__get_det_3_0_i7);
	if ((tag((Integer) field(mktag(0), (Integer) r4, ((Integer) 0))) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i18);
	if (((Integer) field(mktag(3), (Integer) field(mktag(0), (Integer) r4, ((Integer) 0)), ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i18);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i21);
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i13);
	r1 = (Integer) r3;
	r2 = (Integer) mkword(mktag(1), (Integer) mercury_data_garbage_out__common_3);
	GOTO_LABEL(mercury__garbage_out__get_det_3_0_i30);
Define_label(mercury__garbage_out__get_det_3_0_i21);
	r1 = (Integer) r3;
	r2 = (Integer) mkword(mktag(1), (Integer) mercury_data_garbage_out__common_5);
	GOTO_LABEL(mercury__garbage_out__get_det_3_0_i30);
Define_label(mercury__garbage_out__get_det_3_0_i18);
	r1 = (Integer) r3;
Define_label(mercury__garbage_out__get_det_3_0_i30);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i32);
Define_label(mercury__garbage_out__get_det_3_0_i1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i3);
	if (((Integer) field(mktag(1), (Integer) r2, ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i5);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury__garbage_out__get_det_3_0_i5);
	if (((Integer) field(mktag(1), (Integer) r2, ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury__garbage_out__get_det_3_0_i6);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury__garbage_out__get_det_3_0_i6);
	r1 = ((Integer) 2);
	proceed();
Define_label(mercury__garbage_out__get_det_3_0_i3);
	r1 = ((Integer) 1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module8)
	init_entry(mercury__garbage_out__write_cont_list_3_0);
	init_label(mercury__garbage_out__write_cont_list_3_0_i4);
	init_label(mercury__garbage_out__write_cont_list_3_0_i8);
	init_label(mercury__garbage_out__write_cont_list_3_0_i9);
	init_label(mercury__garbage_out__write_cont_list_3_0_i10);
	init_label(mercury__garbage_out__write_cont_list_3_0_i5);
	init_label(mercury__garbage_out__write_cont_list_3_0_i11);
	init_label(mercury__garbage_out__write_cont_list_3_0_i12);
	init_label(mercury__garbage_out__write_cont_list_3_0_i15);
	init_label(mercury__garbage_out__write_cont_list_3_0_i14);
	init_label(mercury__garbage_out__write_cont_list_3_0_i16);
	init_label(mercury__garbage_out__write_cont_list_3_0_i18);
	init_label(mercury__garbage_out__write_cont_list_3_0_i13);
	init_label(mercury__garbage_out__write_cont_list_3_0_i19);
	init_label(mercury__garbage_out__write_cont_list_3_0_i20);
	init_label(mercury__garbage_out__write_cont_list_3_0_i21);
	init_label(mercury__garbage_out__write_cont_list_3_0_i22);
	init_label(mercury__garbage_out__write_cont_list_3_0_i23);
	init_label(mercury__garbage_out__write_cont_list_3_0_i1000);
BEGIN_CODE

/* code for predicate 'garbage_out__write_cont_list'/3 in mode 0 */
Define_static(mercury__garbage_out__write_cont_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_cont_list_3_0_i1000);
	incr_sp_push_msg(6, "garbage_out__write_cont_list");
	detstackvar(6) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r1 = string_const("continuation(", 13);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_cont_list_3_0_i4,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r3 = (Integer) detstackvar(2);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__garbage_out__write_cont_list_3_0_i5);
	r2 = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r1 = string_const("\"", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_cont_list_3_0_i8,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i8);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__llds_out__output_label_3_0);
	call_localret(ENTRY(mercury__llds_out__output_label_3_0),
		mercury__garbage_out__write_cont_list_3_0_i9,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i9);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r2 = (Integer) r1;
	r1 = string_const("\"", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_cont_list_3_0_i10,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i10);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	GOTO_LABEL(mercury__garbage_out__write_cont_list_3_0_i12);
Define_label(mercury__garbage_out__write_cont_list_3_0_i5);
	r1 = string_const("garbage_out : Unexpected code_addr type", 39);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__garbage_out__write_cont_list_3_0_i11,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i11);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
Define_label(mercury__garbage_out__write_cont_list_3_0_i12);
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__garbage_out__write_cont_list_3_0_i14);
	detstackvar(1) = (Integer) r1;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	r1 = string_const(", deterministic", 15);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_cont_list_3_0_i15,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i15);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r2 = (Integer) r1;
	r1 = string_const(", ", 2);
	GOTO_LABEL(mercury__garbage_out__write_cont_list_3_0_i13);
Define_label(mercury__garbage_out__write_cont_list_3_0_i14);
	if (((Integer) r3 != ((Integer) 1)))
		GOTO_LABEL(mercury__garbage_out__write_cont_list_3_0_i16);
	detstackvar(1) = (Integer) r1;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	r1 = string_const(", nondeterministic", 18);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_cont_list_3_0_i15,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i16);
	detstackvar(1) = (Integer) r1;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	r1 = string_const(", commit", 8);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_cont_list_3_0_i18,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i18);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r2 = (Integer) r1;
	r1 = string_const(", ", 2);
Define_label(mercury__garbage_out__write_cont_list_3_0_i13);
	detstackvar(1) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_cont_list_3_0_i19,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i19);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__io__write_int_3_0);
	call_localret(ENTRY(mercury__io__write_int_3_0),
		mercury__garbage_out__write_cont_list_3_0_i20,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i20);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r2 = (Integer) r1;
	r1 = string_const(", [", 3);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_cont_list_3_0_i21,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i21);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__garbage_out__write_liveinfo_list_3_0),
		mercury__garbage_out__write_cont_list_3_0_i22,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
Define_label(mercury__garbage_out__write_cont_list_3_0_i22);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r2 = (Integer) r1;
	r1 = string_const("]).\n", 4);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_cont_list_3_0_i23,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
	}
Define_label(mercury__garbage_out__write_cont_list_3_0_i23);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_cont_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__garbage_out__write_cont_list_3_0,
		STATIC(mercury__garbage_out__write_cont_list_3_0));
Define_label(mercury__garbage_out__write_cont_list_3_0_i1000);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module9)
	init_entry(mercury__garbage_out__write_liveinfo_list_3_0);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i7);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i4);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i8);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i9);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i10);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i11);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i12);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i16);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i17);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i18);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i13);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i19);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i20);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i21);
	init_label(mercury__garbage_out__write_liveinfo_list_3_0_i1004);
BEGIN_CODE

/* code for predicate 'garbage_out__write_liveinfo_list'/3 in mode 0 */
Define_static(mercury__garbage_out__write_liveinfo_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_liveinfo_list_3_0_i1004);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r4 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 2));
	r5 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r6 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	incr_sp_push_msg(5, "garbage_out__write_liveinfo_list");
	detstackvar(5) = (Integer) succip;
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_liveinfo_list_3_0_i4);
	detstackvar(1) = (Integer) r6;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r3;
	r1 = string_const("polyliveinfo(", 13);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_liveinfo_list_3_0_i7,
		STATIC(mercury__garbage_out__write_liveinfo_list_3_0));
	}
	}
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i7);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveinfo_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	GOTO_LABEL(mercury__garbage_out__write_liveinfo_list_3_0_i9);
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i4);
	detstackvar(1) = (Integer) r6;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r3;
	r1 = string_const("liveinfo(", 9);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_liveinfo_list_3_0_i8,
		STATIC(mercury__garbage_out__write_liveinfo_list_3_0));
	}
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i8);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveinfo_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i9);
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	call_localret(STATIC(mercury__garbage_out__write_liveval_3_0),
		mercury__garbage_out__write_liveinfo_list_3_0_i10,
		STATIC(mercury__garbage_out__write_liveinfo_list_3_0));
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i10);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveinfo_list_3_0));
	r2 = (Integer) r1;
	r1 = string_const(", ", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_liveinfo_list_3_0_i11,
		STATIC(mercury__garbage_out__write_liveinfo_list_3_0));
	}
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i11);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveinfo_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__shapes__write_shape_num_3_0);
	call_localret(ENTRY(mercury__shapes__write_shape_num_3_0),
		mercury__garbage_out__write_liveinfo_list_3_0_i12,
		STATIC(mercury__garbage_out__write_liveinfo_list_3_0));
	}
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i12);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveinfo_list_3_0));
	if (((Integer) detstackvar(3) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_liveinfo_list_3_0_i13);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) detstackvar(3), ((Integer) 0));
	r2 = (Integer) r1;
	r1 = string_const(", [", 3);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_liveinfo_list_3_0_i16,
		STATIC(mercury__garbage_out__write_liveinfo_list_3_0));
	}
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i16);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveinfo_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__garbage_out__write_lval_list_3_0),
		mercury__garbage_out__write_liveinfo_list_3_0_i17,
		STATIC(mercury__garbage_out__write_liveinfo_list_3_0));
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i17);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveinfo_list_3_0));
	r2 = (Integer) r1;
	r1 = string_const("]", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_liveinfo_list_3_0_i18,
		STATIC(mercury__garbage_out__write_liveinfo_list_3_0));
	}
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i18);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveinfo_list_3_0));
	r3 = (Integer) detstackvar(4);
	r2 = (Integer) r1;
	r1 = string_const(")", 1);
	GOTO_LABEL(mercury__garbage_out__write_liveinfo_list_3_0_i19);
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i13);
	r3 = (Integer) detstackvar(4);
	r2 = (Integer) r1;
	r1 = string_const(")", 1);
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i19);
	detstackvar(4) = (Integer) r3;
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_liveinfo_list_3_0_i20,
		STATIC(mercury__garbage_out__write_liveinfo_list_3_0));
	}
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i20);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveinfo_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0),
		mercury__garbage_out__write_liveinfo_list_3_0_i21,
		STATIC(mercury__garbage_out__write_liveinfo_list_3_0));
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i21);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveinfo_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__garbage_out__write_liveinfo_list_3_0,
		STATIC(mercury__garbage_out__write_liveinfo_list_3_0));
Define_label(mercury__garbage_out__write_liveinfo_list_3_0_i1004);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module10)
	init_entry(mercury__garbage_out__write_lval_list_3_0);
	init_label(mercury__garbage_out__write_lval_list_3_0_i4);
	init_label(mercury__garbage_out__write_lval_list_3_0_i10);
	init_label(mercury__garbage_out__write_lval_list_3_0_i5);
	init_label(mercury__garbage_out__write_lval_list_3_0_i1003);
BEGIN_CODE

/* code for predicate 'garbage_out__write_lval_list'/3 in mode 0 */
Define_static(mercury__garbage_out__write_lval_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_lval_list_3_0_i1003);
	incr_sp_push_msg(3, "garbage_out__write_lval_list");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__garbage_out__write_liveval_3_0),
		mercury__garbage_out__write_lval_list_3_0_i4,
		STATIC(mercury__garbage_out__write_lval_list_3_0));
Define_label(mercury__garbage_out__write_lval_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_lval_list_3_0));
	if (((Integer) detstackvar(1) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_lval_list_3_0_i5);
	r2 = (Integer) r1;
	r1 = string_const(", ", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_lval_list_3_0_i10,
		STATIC(mercury__garbage_out__write_lval_list_3_0));
	}
Define_label(mercury__garbage_out__write_lval_list_3_0_i10);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_lval_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__garbage_out__write_lval_list_3_0,
		STATIC(mercury__garbage_out__write_lval_list_3_0));
Define_label(mercury__garbage_out__write_lval_list_3_0_i5);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__garbage_out__write_lval_list_3_0,
		STATIC(mercury__garbage_out__write_lval_list_3_0));
Define_label(mercury__garbage_out__write_lval_list_3_0_i1003);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module11)
	init_entry(mercury__garbage_out__write_liveval_3_0);
	init_label(mercury__garbage_out__write_liveval_3_0_i1017);
	init_label(mercury__garbage_out__write_liveval_3_0_i1016);
	init_label(mercury__garbage_out__write_liveval_3_0_i1015);
	init_label(mercury__garbage_out__write_liveval_3_0_i1014);
	init_label(mercury__garbage_out__write_liveval_3_0_i5);
	init_label(mercury__garbage_out__write_liveval_3_0_i6);
	init_label(mercury__garbage_out__write_liveval_3_0_i7);
	init_label(mercury__garbage_out__write_liveval_3_0_i9);
	init_label(mercury__garbage_out__write_liveval_3_0_i10);
	init_label(mercury__garbage_out__write_liveval_3_0_i11);
	init_label(mercury__garbage_out__write_liveval_3_0_i21);
	init_label(mercury__garbage_out__write_liveval_3_0_i22);
	init_label(mercury__garbage_out__write_liveval_3_0_i23);
	init_label(mercury__garbage_out__write_liveval_3_0_i1013);
	init_label(mercury__garbage_out__write_liveval_3_0_i26);
	init_label(mercury__garbage_out__write_liveval_3_0_i28);
	init_label(mercury__garbage_out__write_liveval_3_0_i30);
	init_label(mercury__garbage_out__write_liveval_3_0_i32);
	init_label(mercury__garbage_out__write_liveval_3_0_i34);
	init_label(mercury__garbage_out__write_liveval_3_0_i25);
	init_label(mercury__garbage_out__write_liveval_3_0_i40);
	init_label(mercury__garbage_out__write_liveval_3_0_i41);
	init_label(mercury__garbage_out__write_liveval_3_0_i37);
	init_label(mercury__garbage_out__write_liveval_3_0_i42);
	init_label(mercury__garbage_out__write_liveval_3_0_i47);
	init_label(mercury__garbage_out__write_liveval_3_0_i36);
	init_label(mercury__garbage_out__write_liveval_3_0_i51);
	init_label(mercury__garbage_out__write_liveval_3_0_i1002);
	init_label(mercury__garbage_out__write_liveval_3_0_i1003);
	init_label(mercury__garbage_out__write_liveval_3_0_i1004);
	init_label(mercury__garbage_out__write_liveval_3_0_i1005);
BEGIN_CODE

/* code for predicate 'garbage_out__write_liveval'/3 in mode 0 */
Define_static(mercury__garbage_out__write_liveval_3_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__garbage_out__write_liveval_3_0_i1013);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__garbage_out__write_liveval_3_0_i1017) AND
		LABEL(mercury__garbage_out__write_liveval_3_0_i1016) AND
		LABEL(mercury__garbage_out__write_liveval_3_0_i1002) AND
		LABEL(mercury__garbage_out__write_liveval_3_0_i1003) AND
		LABEL(mercury__garbage_out__write_liveval_3_0_i1004) AND
		LABEL(mercury__garbage_out__write_liveval_3_0_i1005) AND
		LABEL(mercury__garbage_out__write_liveval_3_0_i1015) AND
		LABEL(mercury__garbage_out__write_liveval_3_0_i1014));
Define_label(mercury__garbage_out__write_liveval_3_0_i1017);
	incr_sp_push_msg(2, "garbage_out__write_liveval");
	detstackvar(2) = (Integer) succip;
	GOTO_LABEL(mercury__garbage_out__write_liveval_3_0_i5);
Define_label(mercury__garbage_out__write_liveval_3_0_i1016);
	incr_sp_push_msg(2, "garbage_out__write_liveval");
	detstackvar(2) = (Integer) succip;
	GOTO_LABEL(mercury__garbage_out__write_liveval_3_0_i9);
Define_label(mercury__garbage_out__write_liveval_3_0_i1015);
	incr_sp_push_msg(2, "garbage_out__write_liveval");
	detstackvar(2) = (Integer) succip;
	GOTO_LABEL(mercury__garbage_out__write_liveval_3_0_i21);
Define_label(mercury__garbage_out__write_liveval_3_0_i1014);
	incr_sp_push_msg(2, "garbage_out__write_liveval");
	detstackvar(2) = (Integer) succip;
	GOTO_LABEL(mercury__garbage_out__write_liveval_3_0_i23);
Define_label(mercury__garbage_out__write_liveval_3_0_i5);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = string_const("stackvar(", 9);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_liveval_3_0_i6,
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i6);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveval_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__io__write_int_3_0);
	call_localret(ENTRY(mercury__io__write_int_3_0),
		mercury__garbage_out__write_liveval_3_0_i7,
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i7);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveval_3_0));
	r2 = (Integer) r1;
	r1 = string_const(")", 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i9);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = string_const("framevar(", 9);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_liveval_3_0_i10,
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i10);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveval_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__io__write_int_3_0);
	call_localret(ENTRY(mercury__io__write_int_3_0),
		mercury__garbage_out__write_liveval_3_0_i11,
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i11);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveval_3_0));
	r2 = (Integer) r1;
	r1 = string_const(")", 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i21);
	detstackvar(1) = (Integer) r2;
	r1 = string_const("garbage_out: Unexpected 'field/3' lval", 38);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__garbage_out__write_liveval_3_0_i22,
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i22);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveval_3_0));
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__garbage_out__write_liveval_3_0_i23);
	detstackvar(1) = (Integer) r2;
	r1 = string_const("garbage_out: Unexpected 'lval/1' lval", 37);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__garbage_out__write_liveval_3_0_i22,
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i1013);
	incr_sp_push_msg(2, "garbage_out__write_liveval");
	detstackvar(2) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__garbage_out__write_liveval_3_0_i25);
	COMPUTED_GOTO(unmkbody((Integer) r1),
		LABEL(mercury__garbage_out__write_liveval_3_0_i26) AND
		LABEL(mercury__garbage_out__write_liveval_3_0_i28) AND
		LABEL(mercury__garbage_out__write_liveval_3_0_i30) AND
		LABEL(mercury__garbage_out__write_liveval_3_0_i32) AND
		LABEL(mercury__garbage_out__write_liveval_3_0_i34));
Define_label(mercury__garbage_out__write_liveval_3_0_i26);
	r1 = string_const("succip", 6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i28);
	r1 = string_const("maxfr", 5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i30);
	r1 = string_const("curfr", 5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i32);
	r1 = string_const("hp", 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i34);
	r1 = string_const("sp", 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i25);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__garbage_out__write_liveval_3_0_i36);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if ((tag((Integer) r3) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__garbage_out__write_liveval_3_0_i37);
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r1 = string_const("reg(", 4);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_liveval_3_0_i40,
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i40);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveval_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__io__write_int_3_0);
	call_localret(ENTRY(mercury__io__write_int_3_0),
		mercury__garbage_out__write_liveval_3_0_i41,
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i41);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveval_3_0));
	r2 = (Integer) r1;
	r1 = string_const(")", 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i37);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__garbage_out__write_liveval_3_0_i42);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r1 = string_const("freg(", 5);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_liveval_3_0_i40,
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i42);
	r1 = string_const("garbage_out: Unexpected reg type, not f/1 or r/1", 48);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__garbage_out__write_liveval_3_0_i47,
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i47);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveval_3_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i36);
	detstackvar(1) = (Integer) r2;
	r1 = string_const("garbage_out: Unexpected 'temp/1' lval", 37);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__garbage_out__write_liveval_3_0_i51,
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i51);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_liveval_3_0));
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__garbage_out__write_liveval_3_0_i1002);
	r1 = string_const("succip_slot", 11);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i1003);
	r1 = string_const("redoip", 6);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i1004);
	r1 = string_const("succfr", 6);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
Define_label(mercury__garbage_out__write_liveval_3_0_i1005);
	r1 = string_const("prevfr", 6);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_liveval_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module12)
	init_entry(mercury__garbage_out__write_shapes_3_0);
	init_label(mercury__garbage_out__write_shapes_3_0_i4);
	init_label(mercury__garbage_out__write_shapes_3_0_i5);
	init_label(mercury__garbage_out__write_shapes_3_0_i6);
	init_label(mercury__garbage_out__write_shapes_3_0_i11);
	init_label(mercury__garbage_out__write_shapes_3_0_i12);
	init_label(mercury__garbage_out__write_shapes_3_0_i13);
	init_label(mercury__garbage_out__write_shapes_3_0_i10);
	init_label(mercury__garbage_out__write_shapes_3_0_i14);
	init_label(mercury__garbage_out__write_shapes_3_0_i15);
	init_label(mercury__garbage_out__write_shapes_3_0_i16);
	init_label(mercury__garbage_out__write_shapes_3_0_i9);
	init_label(mercury__garbage_out__write_shapes_3_0_i19);
	init_label(mercury__garbage_out__write_shapes_3_0_i20);
	init_label(mercury__garbage_out__write_shapes_3_0_i21);
	init_label(mercury__garbage_out__write_shapes_3_0_i22);
	init_label(mercury__garbage_out__write_shapes_3_0_i23);
	init_label(mercury__garbage_out__write_shapes_3_0_i24);
	init_label(mercury__garbage_out__write_shapes_3_0_i25);
	init_label(mercury__garbage_out__write_shapes_3_0_i26);
	init_label(mercury__garbage_out__write_shapes_3_0_i18);
	init_label(mercury__garbage_out__write_shapes_3_0_i29);
	init_label(mercury__garbage_out__write_shapes_3_0_i30);
	init_label(mercury__garbage_out__write_shapes_3_0_i31);
	init_label(mercury__garbage_out__write_shapes_3_0_i32);
	init_label(mercury__garbage_out__write_shapes_3_0_i33);
	init_label(mercury__garbage_out__write_shapes_3_0_i28);
	init_label(mercury__garbage_out__write_shapes_3_0_i35);
	init_label(mercury__garbage_out__write_shapes_3_0_i36);
	init_label(mercury__garbage_out__write_shapes_3_0_i37);
	init_label(mercury__garbage_out__write_shapes_3_0_i7);
	init_label(mercury__garbage_out__write_shapes_3_0_i38);
	init_label(mercury__garbage_out__write_shapes_3_0_i1000);
BEGIN_CODE

/* code for predicate 'garbage_out__write_shapes'/3 in mode 0 */
Define_static(mercury__garbage_out__write_shapes_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_shapes_3_0_i1000);
	incr_sp_push_msg(6, "garbage_out__write_shapes");
	detstackvar(6) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r1 = string_const("shapeinfo(", 10);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i4,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i4);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__shapes__write_shape_num_3_0);
	call_localret(ENTRY(mercury__shapes__write_shape_num_3_0),
		mercury__garbage_out__write_shapes_3_0_i5,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i5);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = string_const(", ", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i6,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i6);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) detstackvar(2);
	r3 = tag((Integer) r2);
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__garbage_out__write_shapes_3_0_i9);
	r3 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 0));
	if (((Integer) r3 != ((Integer) 0)))
		GOTO_LABEL(mercury__garbage_out__write_shapes_3_0_i10);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 2));
	r2 = (Integer) r1;
	r1 = string_const("polymorphic(", 12);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i11,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i11);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__io__write_int_3_0);
	call_localret(ENTRY(mercury__io__write_int_3_0),
		mercury__garbage_out__write_shapes_3_0_i12,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i12);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = string_const(")", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i13,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i13);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r3 = (Integer) detstackvar(3);
	r2 = (Integer) r1;
	r1 = string_const(").\n", 3);
	GOTO_LABEL(mercury__garbage_out__write_shapes_3_0_i7);
Define_label(mercury__garbage_out__write_shapes_3_0_i10);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	r2 = (Integer) r1;
	r1 = string_const("closure(", 8);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i14,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i14);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__varset__init_1_0);
	call_localret(ENTRY(mercury__varset__init_1_0),
		mercury__garbage_out__write_shapes_3_0_i15,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i15);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__term_io__write_term_4_0);
	call_localret(ENTRY(mercury__term_io__write_term_4_0),
		mercury__garbage_out__write_shapes_3_0_i16,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i16);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = string_const(")", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i13,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i9);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__garbage_out__write_shapes_3_0_i18);
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	r2 = (Integer) r1;
	r1 = string_const("quad(", 5);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i19,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i19);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__garbage_out__write_shape_tag_3_0),
		mercury__garbage_out__write_shapes_3_0_i20,
		STATIC(mercury__garbage_out__write_shapes_3_0));
Define_label(mercury__garbage_out__write_shapes_3_0_i20);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = string_const(", ", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i21,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i21);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__garbage_out__write_shape_tag_3_0),
		mercury__garbage_out__write_shapes_3_0_i22,
		STATIC(mercury__garbage_out__write_shapes_3_0));
Define_label(mercury__garbage_out__write_shapes_3_0_i22);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = string_const(", ", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i23,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i23);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__garbage_out__write_shape_tag_3_0),
		mercury__garbage_out__write_shapes_3_0_i24,
		STATIC(mercury__garbage_out__write_shapes_3_0));
Define_label(mercury__garbage_out__write_shapes_3_0_i24);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = string_const(", ", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i25,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i25);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__garbage_out__write_shape_tag_3_0),
		mercury__garbage_out__write_shapes_3_0_i26,
		STATIC(mercury__garbage_out__write_shapes_3_0));
Define_label(mercury__garbage_out__write_shapes_3_0_i26);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = string_const(")", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i13,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i18);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__garbage_out__write_shapes_3_0_i28);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) r1;
	r1 = string_const("abstract(", 9);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i29,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i29);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	detstackvar(4) = (Integer) r1;
	{
	Declare_entry(mercury__varset__init_1_0);
	call_localret(ENTRY(mercury__varset__init_1_0),
		mercury__garbage_out__write_shapes_3_0_i30,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i30);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__term_io__write_term_4_0);
	call_localret(ENTRY(mercury__term_io__write_term_4_0),
		mercury__garbage_out__write_shapes_3_0_i31,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i31);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = string_const(", [", 3);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i32,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i32);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__garbage_out__write_int_list_3_0),
		mercury__garbage_out__write_shapes_3_0_i33,
		STATIC(mercury__garbage_out__write_shapes_3_0));
Define_label(mercury__garbage_out__write_shapes_3_0_i33);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = string_const("])", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i13,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i28);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r2, ((Integer) 0));
	r2 = (Integer) r1;
	r1 = string_const("equivalent(", 11);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i35,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i35);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__shapes__write_shape_num_3_0);
	call_localret(ENTRY(mercury__shapes__write_shape_num_3_0),
		mercury__garbage_out__write_shapes_3_0_i36,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i36);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = string_const(")", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i37,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i37);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r3 = (Integer) detstackvar(3);
	r2 = (Integer) r1;
	r1 = string_const(").\n", 3);
Define_label(mercury__garbage_out__write_shapes_3_0_i7);
	detstackvar(3) = (Integer) r3;
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shapes_3_0_i38,
		STATIC(mercury__garbage_out__write_shapes_3_0));
	}
Define_label(mercury__garbage_out__write_shapes_3_0_i38);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shapes_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__garbage_out__write_shapes_3_0,
		STATIC(mercury__garbage_out__write_shapes_3_0));
Define_label(mercury__garbage_out__write_shapes_3_0_i1000);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module13)
	init_entry(mercury__garbage_out__write_shape_tag_3_0);
	init_label(mercury__garbage_out__write_shape_tag_3_0_i1002);
	init_label(mercury__garbage_out__write_shape_tag_3_0_i7);
	init_label(mercury__garbage_out__write_shape_tag_3_0_i8);
	init_label(mercury__garbage_out__write_shape_tag_3_0_i6);
	init_label(mercury__garbage_out__write_shape_tag_3_0_i10);
	init_label(mercury__garbage_out__write_shape_tag_3_0_i11);
BEGIN_CODE

/* code for predicate 'garbage_out__write_shape_tag'/3 in mode 0 */
Define_static(mercury__garbage_out__write_shape_tag_3_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__garbage_out__write_shape_tag_3_0_i1002);
	r1 = string_const("constant", 8);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_shape_tag_3_0));
	}
Define_label(mercury__garbage_out__write_shape_tag_3_0_i1002);
	incr_sp_push_msg(2, "garbage_out__write_shape_tag");
	detstackvar(2) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__garbage_out__write_shape_tag_3_0_i6);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = string_const("simple([", 8);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shape_tag_3_0_i7,
		STATIC(mercury__garbage_out__write_shape_tag_3_0));
	}
Define_label(mercury__garbage_out__write_shape_tag_3_0_i7);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shape_tag_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__garbage_out__write_shape_list_3_0),
		mercury__garbage_out__write_shape_tag_3_0_i8,
		STATIC(mercury__garbage_out__write_shape_tag_3_0));
Define_label(mercury__garbage_out__write_shape_tag_3_0_i8);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shape_tag_3_0));
	r2 = (Integer) r1;
	r1 = string_const("])", 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_shape_tag_3_0));
	}
Define_label(mercury__garbage_out__write_shape_tag_3_0_i6);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	r1 = string_const("complicated([", 13);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_shape_tag_3_0_i10,
		STATIC(mercury__garbage_out__write_shape_tag_3_0));
	}
Define_label(mercury__garbage_out__write_shape_tag_3_0_i10);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shape_tag_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__garbage_out__write_complicated_3_0),
		mercury__garbage_out__write_shape_tag_3_0_i11,
		STATIC(mercury__garbage_out__write_shape_tag_3_0));
Define_label(mercury__garbage_out__write_shape_tag_3_0_i11);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shape_tag_3_0));
	r2 = (Integer) r1;
	r1 = string_const("])", 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	tailcall(ENTRY(mercury__io__write_string_3_0),
		STATIC(mercury__garbage_out__write_shape_tag_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module14)
	init_entry(mercury__garbage_out__write_complicated_3_0);
	init_label(mercury__garbage_out__write_complicated_3_0_i4);
	init_label(mercury__garbage_out__write_complicated_3_0_i5);
	init_label(mercury__garbage_out__write_complicated_3_0_i1002);
BEGIN_CODE

/* code for predicate 'garbage_out__write_complicated'/3 in mode 0 */
Define_static(mercury__garbage_out__write_complicated_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_complicated_3_0_i1002);
	incr_sp_push_msg(2, "garbage_out__write_complicated");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	call_localret(STATIC(mercury__garbage_out__write_shape_tag_3_0),
		mercury__garbage_out__write_complicated_3_0_i4,
		STATIC(mercury__garbage_out__write_complicated_3_0));
Define_label(mercury__garbage_out__write_complicated_3_0_i4);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_complicated_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0),
		mercury__garbage_out__write_complicated_3_0_i5,
		STATIC(mercury__garbage_out__write_complicated_3_0));
Define_label(mercury__garbage_out__write_complicated_3_0_i5);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_complicated_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__garbage_out__write_complicated_3_0,
		STATIC(mercury__garbage_out__write_complicated_3_0));
Define_label(mercury__garbage_out__write_complicated_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module15)
	init_entry(mercury__garbage_out__write_shape_list_3_0);
	init_label(mercury__garbage_out__write_shape_list_3_0_i4);
	init_label(mercury__garbage_out__write_shape_list_3_0_i5);
	init_label(mercury__garbage_out__write_shape_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'garbage_out__write_shape_list'/3 in mode 0 */
Define_static(mercury__garbage_out__write_shape_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_shape_list_3_0_i1002);
	incr_sp_push_msg(2, "garbage_out__write_shape_list");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), ((Integer) 0));
	{
	Declare_entry(mercury__shapes__write_shape_num_3_0);
	call_localret(ENTRY(mercury__shapes__write_shape_num_3_0),
		mercury__garbage_out__write_shape_list_3_0_i4,
		STATIC(mercury__garbage_out__write_shape_list_3_0));
	}
Define_label(mercury__garbage_out__write_shape_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shape_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0),
		mercury__garbage_out__write_shape_list_3_0_i5,
		STATIC(mercury__garbage_out__write_shape_list_3_0));
Define_label(mercury__garbage_out__write_shape_list_3_0_i5);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_shape_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__garbage_out__write_shape_list_3_0,
		STATIC(mercury__garbage_out__write_shape_list_3_0));
Define_label(mercury__garbage_out__write_shape_list_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module16)
	init_entry(mercury__garbage_out__write_int_list_3_0);
	init_label(mercury__garbage_out__write_int_list_3_0_i4);
	init_label(mercury__garbage_out__write_int_list_3_0_i5);
	init_label(mercury__garbage_out__write_int_list_3_0_i1002);
BEGIN_CODE

/* code for predicate 'garbage_out__write_int_list'/3 in mode 0 */
Define_static(mercury__garbage_out__write_int_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_int_list_3_0_i1002);
	incr_sp_push_msg(2, "garbage_out__write_int_list");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__shapes__write_shape_num_3_0);
	call_localret(ENTRY(mercury__shapes__write_shape_num_3_0),
		mercury__garbage_out__write_int_list_3_0_i4,
		STATIC(mercury__garbage_out__write_int_list_3_0));
	}
Define_label(mercury__garbage_out__write_int_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_int_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__garbage_out__maybe_write_comma_space__ua10000_3_0),
		mercury__garbage_out__write_int_list_3_0_i5,
		STATIC(mercury__garbage_out__write_int_list_3_0));
Define_label(mercury__garbage_out__write_int_list_3_0_i5);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_int_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__garbage_out__write_int_list_3_0,
		STATIC(mercury__garbage_out__write_int_list_3_0));
Define_label(mercury__garbage_out__write_int_list_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module17)
	init_entry(mercury__garbage_out__write_abs_list_3_0);
	init_label(mercury__garbage_out__write_abs_list_3_0_i4);
	init_label(mercury__garbage_out__write_abs_list_3_0_i7);
	init_label(mercury__garbage_out__write_abs_list_3_0_i8);
	init_label(mercury__garbage_out__write_abs_list_3_0_i6);
	init_label(mercury__garbage_out__write_abs_list_3_0_i9);
	init_label(mercury__garbage_out__write_abs_list_3_0_i10);
	init_label(mercury__garbage_out__write_abs_list_3_0_i5);
	init_label(mercury__garbage_out__write_abs_list_3_0_i13);
	init_label(mercury__garbage_out__write_abs_list_3_0_i14);
	init_label(mercury__garbage_out__write_abs_list_3_0_i15);
	init_label(mercury__garbage_out__write_abs_list_3_0_i12);
	init_label(mercury__garbage_out__write_abs_list_3_0_i16);
	init_label(mercury__garbage_out__write_abs_list_3_0_i17);
	init_label(mercury__garbage_out__write_abs_list_3_0_i11);
	init_label(mercury__garbage_out__write_abs_list_3_0_i18);
	init_label(mercury__garbage_out__write_abs_list_3_0_i1009);
BEGIN_CODE

/* code for predicate 'garbage_out__write_abs_list'/3 in mode 0 */
Define_static(mercury__garbage_out__write_abs_list_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_abs_list_3_0_i1009);
	incr_sp_push_msg(4, "garbage_out__write_abs_list");
	detstackvar(4) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r1 = string_const("abs_info(", 9);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_abs_list_3_0_i4,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
	}
Define_label(mercury__garbage_out__write_abs_list_3_0_i4);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_abs_list_3_0));
	if ((tag((Integer) field(mktag(0), (Integer) detstackvar(1), ((Integer) 0))) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury__garbage_out__write_abs_list_3_0_i6);
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("qualified(", 10);
	tag_incr_hp(r4, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) field(mktag(0), (Integer) r2, ((Integer) 0)), ((Integer) 0));
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = string_const(", ", 2);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) field(mktag(0), (Integer) r2, ((Integer) 0)), ((Integer) 1));
	r2 = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r4;
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) r5;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_garbage_out__common_6);
	{
	Declare_entry(mercury__io__write_strings_3_0);
	call_localret(ENTRY(mercury__io__write_strings_3_0),
		mercury__garbage_out__write_abs_list_3_0_i7,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
	}
	}
Define_label(mercury__garbage_out__write_abs_list_3_0_i7);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_abs_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__io__write_int_3_0);
	call_localret(ENTRY(mercury__io__write_int_3_0),
		mercury__garbage_out__write_abs_list_3_0_i8,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
	}
Define_label(mercury__garbage_out__write_abs_list_3_0_i8);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_abs_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	GOTO_LABEL(mercury__garbage_out__write_abs_list_3_0_i5);
Define_label(mercury__garbage_out__write_abs_list_3_0_i6);
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("unqualified(", 12);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) field(mktag(0), (Integer) r2, ((Integer) 0)), ((Integer) 0));
	r2 = (Integer) r3;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_garbage_out__common_8);
	{
	Declare_entry(mercury__io__write_strings_3_0);
	call_localret(ENTRY(mercury__io__write_strings_3_0),
		mercury__garbage_out__write_abs_list_3_0_i9,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
	}
	}
Define_label(mercury__garbage_out__write_abs_list_3_0_i9);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_abs_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__io__write_int_3_0);
	call_localret(ENTRY(mercury__io__write_int_3_0),
		mercury__garbage_out__write_abs_list_3_0_i10,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
	}
Define_label(mercury__garbage_out__write_abs_list_3_0_i10);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_abs_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
Define_label(mercury__garbage_out__write_abs_list_3_0_i5);
	if ((tag((Integer) r1) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury__garbage_out__write_abs_list_3_0_i12);
	detstackvar(3) = (Integer) r3;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = string_const(", no(", 5);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_abs_list_3_0_i13,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
	}
Define_label(mercury__garbage_out__write_abs_list_3_0_i13);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_abs_list_3_0));
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__varset__init_1_0);
	call_localret(ENTRY(mercury__varset__init_1_0),
		mercury__garbage_out__write_abs_list_3_0_i14,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
	}
Define_label(mercury__garbage_out__write_abs_list_3_0_i14);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_abs_list_3_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__term_io__write_term_4_0);
	call_localret(ENTRY(mercury__term_io__write_term_4_0),
		mercury__garbage_out__write_abs_list_3_0_i15,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
	}
Define_label(mercury__garbage_out__write_abs_list_3_0_i15);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_abs_list_3_0));
	r3 = (Integer) detstackvar(3);
	r2 = (Integer) r1;
	r1 = string_const(")).\n", 4);
	GOTO_LABEL(mercury__garbage_out__write_abs_list_3_0_i11);
Define_label(mercury__garbage_out__write_abs_list_3_0_i12);
	detstackvar(3) = (Integer) r3;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = string_const(", yes(", 6);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_abs_list_3_0_i16,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
	}
Define_label(mercury__garbage_out__write_abs_list_3_0_i16);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_abs_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__shapes__write_shape_num_3_0);
	call_localret(ENTRY(mercury__shapes__write_shape_num_3_0),
		mercury__garbage_out__write_abs_list_3_0_i17,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
	}
Define_label(mercury__garbage_out__write_abs_list_3_0_i17);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_abs_list_3_0));
	r3 = (Integer) detstackvar(3);
	r2 = (Integer) r1;
	r1 = string_const(")).\n", 4);
Define_label(mercury__garbage_out__write_abs_list_3_0_i11);
	detstackvar(3) = (Integer) r3;
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_abs_list_3_0_i18,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
	}
Define_label(mercury__garbage_out__write_abs_list_3_0_i18);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_abs_list_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__garbage_out__write_abs_list_3_0,
		STATIC(mercury__garbage_out__write_abs_list_3_0));
Define_label(mercury__garbage_out__write_abs_list_3_0_i1009);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module18)
	init_entry(mercury__garbage_out__write_special_preds_3_0);
	init_label(mercury__garbage_out__write_special_preds_3_0_i4);
	init_label(mercury__garbage_out__write_special_preds_3_0_i5);
	init_label(mercury__garbage_out__write_special_preds_3_0_i6);
	init_label(mercury__garbage_out__write_special_preds_3_0_i7);
	init_label(mercury__garbage_out__write_special_preds_3_0_i8);
	init_label(mercury__garbage_out__write_special_preds_3_0_i9);
	init_label(mercury__garbage_out__write_special_preds_3_0_i10);
	init_label(mercury__garbage_out__write_special_preds_3_0_i1002);
BEGIN_CODE

/* code for predicate 'garbage_out__write_special_preds'/3 in mode 0 */
Define_static(mercury__garbage_out__write_special_preds_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__garbage_out__write_special_preds_3_0_i1002);
	incr_sp_push_msg(4, "garbage_out__write_special_preds");
	detstackvar(4) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r1 = string_const("special_pred(", 13);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_special_preds_3_0_i4,
		STATIC(mercury__garbage_out__write_special_preds_3_0));
	}
Define_label(mercury__garbage_out__write_special_preds_3_0_i4);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_special_preds_3_0));
	r2 = (Integer) r1;
	r1 = string_const("\"", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_special_preds_3_0_i5,
		STATIC(mercury__garbage_out__write_special_preds_3_0));
	}
Define_label(mercury__garbage_out__write_special_preds_3_0_i5);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_special_preds_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__llds_out__output_label_3_0);
	call_localret(ENTRY(mercury__llds_out__output_label_3_0),
		mercury__garbage_out__write_special_preds_3_0_i6,
		STATIC(mercury__garbage_out__write_special_preds_3_0));
	}
Define_label(mercury__garbage_out__write_special_preds_3_0_i6);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_special_preds_3_0));
	r2 = (Integer) r1;
	r1 = string_const("\"", 1);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_special_preds_3_0_i7,
		STATIC(mercury__garbage_out__write_special_preds_3_0));
	}
Define_label(mercury__garbage_out__write_special_preds_3_0_i7);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_special_preds_3_0));
	r2 = (Integer) r1;
	r1 = string_const(", ", 2);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_special_preds_3_0_i8,
		STATIC(mercury__garbage_out__write_special_preds_3_0));
	}
Define_label(mercury__garbage_out__write_special_preds_3_0_i8);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_special_preds_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__shapes__write_shape_num_3_0);
	call_localret(ENTRY(mercury__shapes__write_shape_num_3_0),
		mercury__garbage_out__write_special_preds_3_0_i9,
		STATIC(mercury__garbage_out__write_special_preds_3_0));
	}
Define_label(mercury__garbage_out__write_special_preds_3_0_i9);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_special_preds_3_0));
	r2 = (Integer) r1;
	r1 = string_const("). \n", 4);
	{
	Declare_entry(mercury__io__write_string_3_0);
	call_localret(ENTRY(mercury__io__write_string_3_0),
		mercury__garbage_out__write_special_preds_3_0_i10,
		STATIC(mercury__garbage_out__write_special_preds_3_0));
	}
Define_label(mercury__garbage_out__write_special_preds_3_0_i10);
	update_prof_current_proc(LABEL(mercury__garbage_out__write_special_preds_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__garbage_out__write_special_preds_3_0,
		STATIC(mercury__garbage_out__write_special_preds_3_0));
Define_label(mercury__garbage_out__write_special_preds_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module19)
	init_entry(mercury____Unify___garbage_out__garbage_output_0_0);
	init_label(mercury____Unify___garbage_out__garbage_output_0_0_i2);
	init_label(mercury____Unify___garbage_out__garbage_output_0_0_i4);
	init_label(mercury____Unify___garbage_out__garbage_output_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___garbage_out__garbage_output_0_0);
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	incr_sp_push_msg(5, "__Unify__");
	detstackvar(5) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	r1 = (Integer) mercury_data_garbage_out__base_type_info_gc_label_info_0;
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		mercury____Unify___garbage_out__garbage_output_0_0_i2,
		ENTRY(mercury____Unify___garbage_out__garbage_output_0_0));
	}
Define_label(mercury____Unify___garbage_out__garbage_output_0_0_i2);
	update_prof_current_proc(LABEL(mercury____Unify___garbage_out__garbage_output_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___garbage_out__garbage_output_0_0_i1);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_9);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury____Unify___std_util__pair_2_0);
	call_localret(ENTRY(mercury____Unify___std_util__pair_2_0),
		mercury____Unify___garbage_out__garbage_output_0_0_i4,
		ENTRY(mercury____Unify___garbage_out__garbage_output_0_0));
	}
Define_label(mercury____Unify___garbage_out__garbage_output_0_0_i4);
	update_prof_current_proc(LABEL(mercury____Unify___garbage_out__garbage_output_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___garbage_out__garbage_output_0_0_i1);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_2);
	r2 = (Integer) mercury_data_hlds_module__base_type_info_maybe_shape_num_0;
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Unify___tree234__tree234_2_0),
		ENTRY(mercury____Unify___garbage_out__garbage_output_0_0));
	}
Define_label(mercury____Unify___garbage_out__garbage_output_0_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module20)
	init_entry(mercury____Index___garbage_out__garbage_output_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___garbage_out__garbage_output_0_0);
	tailcall(STATIC(mercury____Index___garbage_out_garbage_output_0__ua10000_2_0),
		ENTRY(mercury____Index___garbage_out__garbage_output_0_0));
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module21)
	init_entry(mercury____Compare___garbage_out__garbage_output_0_0);
	init_label(mercury____Compare___garbage_out__garbage_output_0_0_i4);
	init_label(mercury____Compare___garbage_out__garbage_output_0_0_i5);
	init_label(mercury____Compare___garbage_out__garbage_output_0_0_i3);
	init_label(mercury____Compare___garbage_out__garbage_output_0_0_i10);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___garbage_out__garbage_output_0_0);
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	incr_sp_push_msg(5, "__Compare__");
	detstackvar(5) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	r1 = (Integer) mercury_data_garbage_out__base_type_info_gc_label_info_0;
	{
	Declare_entry(mercury____Compare___mercury_builtin__list_1_0);
	call_localret(ENTRY(mercury____Compare___mercury_builtin__list_1_0),
		mercury____Compare___garbage_out__garbage_output_0_0_i4,
		ENTRY(mercury____Compare___garbage_out__garbage_output_0_0));
	}
Define_label(mercury____Compare___garbage_out__garbage_output_0_0_i4);
	update_prof_current_proc(LABEL(mercury____Compare___garbage_out__garbage_output_0_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___garbage_out__garbage_output_0_0_i3);
Define_label(mercury____Compare___garbage_out__garbage_output_0_0_i5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___garbage_out__garbage_output_0_0_i3);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_9);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury____Compare___std_util__pair_2_0);
	call_localret(ENTRY(mercury____Compare___std_util__pair_2_0),
		mercury____Compare___garbage_out__garbage_output_0_0_i10,
		ENTRY(mercury____Compare___garbage_out__garbage_output_0_0));
	}
Define_label(mercury____Compare___garbage_out__garbage_output_0_0_i10);
	update_prof_current_proc(LABEL(mercury____Compare___garbage_out__garbage_output_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___garbage_out__garbage_output_0_0_i5);
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_garbage_out__common_2);
	r2 = (Integer) mercury_data_hlds_module__base_type_info_maybe_shape_num_0;
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	tailcall(ENTRY(mercury____Compare___tree234__tree234_2_0),
		ENTRY(mercury____Compare___garbage_out__garbage_output_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module22)
	init_entry(mercury____Unify___garbage_out__cont_list_0_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___garbage_out__cont_list_0_0);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_garbage_out__base_type_info_gc_label_info_0;
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury____Unify___garbage_out__cont_list_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module23)
	init_entry(mercury____Index___garbage_out__cont_list_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___garbage_out__cont_list_0_0);
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_garbage_out__base_type_info_gc_label_info_0;
	{
	Declare_entry(mercury____Index___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Index___mercury_builtin__list_1_0),
		ENTRY(mercury____Index___garbage_out__cont_list_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module24)
	init_entry(mercury____Compare___garbage_out__cont_list_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___garbage_out__cont_list_0_0);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_garbage_out__base_type_info_gc_label_info_0;
	{
	Declare_entry(mercury____Compare___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Compare___mercury_builtin__list_1_0),
		ENTRY(mercury____Compare___garbage_out__cont_list_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module25)
	init_entry(mercury____Unify___garbage_out__gc_label_info_0_0);
	init_label(mercury____Unify___garbage_out__gc_label_info_0_0_i2);
	init_label(mercury____Unify___garbage_out__gc_label_info_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___garbage_out__gc_label_info_0_0);
	incr_sp_push_msg(7, "__Unify__");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury____Unify___llds__code_addr_0_0);
	call_localret(ENTRY(mercury____Unify___llds__code_addr_0_0),
		mercury____Unify___garbage_out__gc_label_info_0_0_i2,
		ENTRY(mercury____Unify___garbage_out__gc_label_info_0_0));
	}
Define_label(mercury____Unify___garbage_out__gc_label_info_0_0_i2);
	update_prof_current_proc(LABEL(mercury____Unify___garbage_out__gc_label_info_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___garbage_out__gc_label_info_0_0_i1);
	if (((Integer) detstackvar(1) != (Integer) detstackvar(4)))
		GOTO_LABEL(mercury____Unify___garbage_out__gc_label_info_0_0_i1);
	if (((Integer) detstackvar(2) != (Integer) detstackvar(5)))
		GOTO_LABEL(mercury____Unify___garbage_out__gc_label_info_0_0_i1);
	r1 = (Integer) mercury_data_llds__base_type_info_liveinfo_0;
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury____Unify___garbage_out__gc_label_info_0_0));
	}
Define_label(mercury____Unify___garbage_out__gc_label_info_0_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module26)
	init_entry(mercury____Index___garbage_out__gc_label_info_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___garbage_out__gc_label_info_0_0);
	tailcall(STATIC(mercury____Index___garbage_out_gc_label_info_0__ua10000_2_0),
		ENTRY(mercury____Index___garbage_out__gc_label_info_0_0));
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module27)
	init_entry(mercury____Compare___garbage_out__gc_label_info_0_0);
	init_label(mercury____Compare___garbage_out__gc_label_info_0_0_i4);
	init_label(mercury____Compare___garbage_out__gc_label_info_0_0_i5);
	init_label(mercury____Compare___garbage_out__gc_label_info_0_0_i3);
	init_label(mercury____Compare___garbage_out__gc_label_info_0_0_i10);
	init_label(mercury____Compare___garbage_out__gc_label_info_0_0_i16);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___garbage_out__gc_label_info_0_0);
	incr_sp_push_msg(7, "__Compare__");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury____Compare___llds__code_addr_0_0);
	call_localret(ENTRY(mercury____Compare___llds__code_addr_0_0),
		mercury____Compare___garbage_out__gc_label_info_0_0_i4,
		ENTRY(mercury____Compare___garbage_out__gc_label_info_0_0));
	}
Define_label(mercury____Compare___garbage_out__gc_label_info_0_0_i4);
	update_prof_current_proc(LABEL(mercury____Compare___garbage_out__gc_label_info_0_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___garbage_out__gc_label_info_0_0_i3);
Define_label(mercury____Compare___garbage_out__gc_label_info_0_0_i5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury____Compare___garbage_out__gc_label_info_0_0_i3);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	call_localret(ENTRY(mercury__builtin_compare_int_3_0),
		mercury____Compare___garbage_out__gc_label_info_0_0_i10,
		ENTRY(mercury____Compare___garbage_out__gc_label_info_0_0));
	}
Define_label(mercury____Compare___garbage_out__gc_label_info_0_0_i10);
	update_prof_current_proc(LABEL(mercury____Compare___garbage_out__gc_label_info_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___garbage_out__gc_label_info_0_0_i5);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	call_localret(ENTRY(mercury__builtin_compare_int_3_0),
		mercury____Compare___garbage_out__gc_label_info_0_0_i16,
		ENTRY(mercury____Compare___garbage_out__gc_label_info_0_0));
	}
Define_label(mercury____Compare___garbage_out__gc_label_info_0_0_i16);
	update_prof_current_proc(LABEL(mercury____Compare___garbage_out__gc_label_info_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___garbage_out__gc_label_info_0_0_i5);
	r1 = (Integer) mercury_data_llds__base_type_info_liveinfo_0;
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	{
	Declare_entry(mercury____Compare___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Compare___mercury_builtin__list_1_0),
		ENTRY(mercury____Compare___garbage_out__gc_label_info_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module28)
	init_entry(mercury____Unify___garbage_out__num_slots_0_0);
	init_label(mercury____Unify___garbage_out__num_slots_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___garbage_out__num_slots_0_0);
	if (((Integer) r1 != (Integer) r2))
		GOTO_LABEL(mercury____Unify___garbage_out__num_slots_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___garbage_out__num_slots_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module29)
	init_entry(mercury____Index___garbage_out__num_slots_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___garbage_out__num_slots_0_0);
	{
	Declare_entry(mercury__builtin_index_int_2_0);
	tailcall(ENTRY(mercury__builtin_index_int_2_0),
		ENTRY(mercury____Index___garbage_out__num_slots_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module30)
	init_entry(mercury____Compare___garbage_out__num_slots_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___garbage_out__num_slots_0_0);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___garbage_out__num_slots_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module31)
	init_entry(mercury____Unify___garbage_out__det_0_0);
	init_label(mercury____Unify___garbage_out__det_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___garbage_out__det_0_0);
	if (((Integer) r1 != (Integer) r2))
		GOTO_LABEL(mercury____Unify___garbage_out__det_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___garbage_out__det_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module32)
	init_entry(mercury____Index___garbage_out__det_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___garbage_out__det_0_0);
	{
	Declare_entry(mercury__builtin_index_int_2_0);
	tailcall(ENTRY(mercury__builtin_index_int_2_0),
		ENTRY(mercury____Index___garbage_out__det_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__garbage_out_module33)
	init_entry(mercury____Compare___garbage_out__det_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___garbage_out__det_0_0);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___garbage_out__det_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__garbage_out_bunch_0(void)
{
	mercury__garbage_out_module0();
	mercury__garbage_out_module1();
	mercury__garbage_out_module2();
	mercury__garbage_out_module3();
	mercury__garbage_out_module4();
	mercury__garbage_out_module5();
	mercury__garbage_out_module6();
	mercury__garbage_out_module7();
	mercury__garbage_out_module8();
	mercury__garbage_out_module9();
	mercury__garbage_out_module10();
	mercury__garbage_out_module11();
	mercury__garbage_out_module12();
	mercury__garbage_out_module13();
	mercury__garbage_out_module14();
	mercury__garbage_out_module15();
	mercury__garbage_out_module16();
	mercury__garbage_out_module17();
	mercury__garbage_out_module18();
	mercury__garbage_out_module19();
	mercury__garbage_out_module20();
	mercury__garbage_out_module21();
	mercury__garbage_out_module22();
	mercury__garbage_out_module23();
	mercury__garbage_out_module24();
	mercury__garbage_out_module25();
	mercury__garbage_out_module26();
	mercury__garbage_out_module27();
	mercury__garbage_out_module28();
	mercury__garbage_out_module29();
	mercury__garbage_out_module30();
	mercury__garbage_out_module31();
	mercury__garbage_out_module32();
	mercury__garbage_out_module33();
}

#endif

void mercury__garbage_out__init(void); /* suppress gcc warning */
void mercury__garbage_out__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__garbage_out_bunch_0();
#endif
}
