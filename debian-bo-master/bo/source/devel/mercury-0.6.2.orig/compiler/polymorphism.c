/*
** Automatically generated from `polymorphism.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__polymorphism__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__polymorphism__process_call__ua10000_8_0);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i2);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i3);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i4);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i5);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i6);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i9);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i10);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i13);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i12);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i15);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i16);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i17);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i18);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i19);
Declare_label(mercury__polymorphism__process_call__ua10000_8_0_i20);
Define_extern_entry(mercury__polymorphism__process_module_2_0);
Declare_label(mercury__polymorphism__process_module_2_0_i2);
Declare_label(mercury__polymorphism__process_module_2_0_i3);
Declare_label(mercury__polymorphism__process_module_2_0_i4);
Declare_label(mercury__polymorphism__process_module_2_0_i5);
Declare_label(mercury__polymorphism__process_module_2_0_i6);
Declare_static(mercury__polymorphism__process_preds_3_0);
Declare_label(mercury__polymorphism__process_preds_3_0_i4);
Declare_label(mercury__polymorphism__process_preds_3_0_i5);
Declare_label(mercury__polymorphism__process_preds_3_0_i6);
Declare_label(mercury__polymorphism__process_preds_3_0_i1002);
Declare_static(mercury__polymorphism__process_procs_4_0);
Declare_label(mercury__polymorphism__process_procs_4_0_i4);
Declare_label(mercury__polymorphism__process_procs_4_0_i5);
Declare_label(mercury__polymorphism__process_procs_4_0_i6);
Declare_label(mercury__polymorphism__process_procs_4_0_i7);
Declare_label(mercury__polymorphism__process_procs_4_0_i8);
Declare_label(mercury__polymorphism__process_procs_4_0_i9);
Declare_label(mercury__polymorphism__process_procs_4_0_i10);
Declare_label(mercury__polymorphism__process_procs_4_0_i11);
Declare_label(mercury__polymorphism__process_procs_4_0_i12);
Declare_label(mercury__polymorphism__process_procs_4_0_i13);
Declare_label(mercury__polymorphism__process_procs_4_0_i14);
Declare_label(mercury__polymorphism__process_procs_4_0_i15);
Declare_label(mercury__polymorphism__process_procs_4_0_i16);
Declare_label(mercury__polymorphism__process_procs_4_0_i17);
Declare_label(mercury__polymorphism__process_procs_4_0_i18);
Declare_label(mercury__polymorphism__process_procs_4_0_i19);
Declare_label(mercury__polymorphism__process_procs_4_0_i20);
Declare_label(mercury__polymorphism__process_procs_4_0_i21);
Declare_label(mercury__polymorphism__process_procs_4_0_i22);
Declare_label(mercury__polymorphism__process_procs_4_0_i23);
Declare_label(mercury__polymorphism__process_procs_4_0_i24);
Declare_label(mercury__polymorphism__process_procs_4_0_i25);
Declare_label(mercury__polymorphism__process_procs_4_0_i26);
Declare_label(mercury__polymorphism__process_procs_4_0_i27);
Declare_label(mercury__polymorphism__process_procs_4_0_i28);
Declare_label(mercury__polymorphism__process_procs_4_0_i29);
Declare_label(mercury__polymorphism__process_procs_4_0_i30);
Declare_label(mercury__polymorphism__process_procs_4_0_i31);
Declare_label(mercury__polymorphism__process_procs_4_0_i32);
Declare_label(mercury__polymorphism__process_procs_4_0_i33);
Declare_label(mercury__polymorphism__process_procs_4_0_i34);
Declare_label(mercury__polymorphism__process_procs_4_0_i35);
Declare_label(mercury__polymorphism__process_procs_4_0_i36);
Declare_label(mercury__polymorphism__process_procs_4_0_i37);
Declare_label(mercury__polymorphism__process_procs_4_0_i1002);
Declare_static(mercury__polymorphism__fixup_preds_3_0);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i4);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i5);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i6);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i7);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i11);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i12);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i13);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i14);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i15);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i16);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i19);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i21);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i22);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i18);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i23);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i24);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i25);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i26);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i27);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i8);
Declare_label(mercury__polymorphism__fixup_preds_3_0_i1004);
Declare_static(mercury__polymorphism__process_goal_4_0);
Declare_static(mercury__polymorphism__process_goal_expr_5_0);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i1007);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i1006);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i1005);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i1004);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i1003);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i1002);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i1001);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i5);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i6);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i7);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i12);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i16);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i19);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i22);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i18);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i24);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i25);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i26);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i27);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i13);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i30);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i32);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i35);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i38);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i34);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i40);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i41);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i42);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i29);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i46);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i48);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i49);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i50);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i51);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i52);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i45);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i54);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i8);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i61);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i62);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i63);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i64);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i58);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i67);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i68);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i69);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i70);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i71);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i72);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i73);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i74);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i75);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i76);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i77);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i78);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i79);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i80);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i81);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i82);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i83);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i84);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i85);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i86);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i87);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i1000);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i90);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i89);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i95);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i96);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i98);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i100);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i104);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i105);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i107);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i108);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i93);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i92);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i109);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i110);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i111);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i112);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i113);
Declare_label(mercury__polymorphism__process_goal_expr_5_0_i91);
Declare_static(mercury__polymorphism__c_code_add_typeinfos_5_0);
Declare_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i6);
Declare_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i9);
Declare_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i11);
Declare_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i8);
Declare_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i1011);
Declare_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i1009);
Declare_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i1010);
Declare_static(mercury__polymorphism__process_goal_list_4_0);
Declare_label(mercury__polymorphism__process_goal_list_4_0_i4);
Declare_label(mercury__polymorphism__process_goal_list_4_0_i5);
Declare_label(mercury__polymorphism__process_goal_list_4_0_i1002);
Declare_static(mercury__polymorphism__process_case_list_4_0);
Declare_label(mercury__polymorphism__process_case_list_4_0_i4);
Declare_label(mercury__polymorphism__process_case_list_4_0_i5);
Declare_label(mercury__polymorphism__process_case_list_4_0_i1003);
Declare_static(mercury__polymorphism__fixup_quantification_4_0);
Declare_label(mercury__polymorphism__fixup_quantification_4_0_i2);
Declare_label(mercury__polymorphism__fixup_quantification_4_0_i3);
Declare_label(mercury__polymorphism__fixup_quantification_4_0_i6);
Declare_label(mercury__polymorphism__fixup_quantification_4_0_i7);
Declare_label(mercury__polymorphism__fixup_quantification_4_0_i8);
Declare_label(mercury__polymorphism__fixup_quantification_4_0_i9);
Declare_static(mercury__polymorphism__process_lambda_11_0);
Declare_label(mercury__polymorphism__process_lambda_11_0_i2);
Declare_static(mercury__polymorphism__make_vars_9_0);
Declare_label(mercury__polymorphism__make_vars_9_0_i4);
Declare_label(mercury__polymorphism__make_vars_9_0_i5);
Declare_label(mercury__polymorphism__make_vars_9_0_i6);
Declare_label(mercury__polymorphism__make_vars_9_0_i1002);
Declare_static(mercury__polymorphism__make_var_9_0);
Declare_label(mercury__polymorphism__make_var_9_0_i4);
Declare_label(mercury__polymorphism__make_var_9_0_i3);
Declare_label(mercury__polymorphism__make_var_9_0_i9);
Declare_label(mercury__polymorphism__make_var_9_0_i8);
Declare_label(mercury__polymorphism__make_var_9_0_i15);
Declare_label(mercury__polymorphism__make_var_9_0_i13);
Declare_label(mercury__polymorphism__make_var_9_0_i12);
Declare_label(mercury__polymorphism__make_var_9_0_i17);
Declare_label(mercury__polymorphism__make_var_9_0_i18);
Declare_static(mercury__polymorphism__construct_type_info_11_0);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i2);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i3);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i4);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i7);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i8);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i9);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i10);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i11);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i12);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i13);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i6);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i15);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i16);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i17);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i18);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i19);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i14);
Declare_label(mercury__polymorphism__construct_type_info_11_0_i20);
Declare_static(mercury__polymorphism__maybe_init_second_cell_11_0);
Declare_label(mercury__polymorphism__maybe_init_second_cell_11_0_i1000);
Declare_label(mercury__polymorphism__maybe_init_second_cell_11_0_i5);
Declare_label(mercury__polymorphism__maybe_init_second_cell_11_0_i6);
Declare_label(mercury__polymorphism__maybe_init_second_cell_11_0_i7);
Declare_static(mercury__polymorphism__make_count_var_7_0);
Declare_label(mercury__polymorphism__make_count_var_7_0_i2);
Declare_label(mercury__polymorphism__make_count_var_7_0_i3);
Declare_label(mercury__polymorphism__make_count_var_7_0_i4);
Declare_label(mercury__polymorphism__make_count_var_7_0_i5);
Declare_label(mercury__polymorphism__make_count_var_7_0_i6);
Declare_label(mercury__polymorphism__make_count_var_7_0_i7);
Declare_static(mercury__polymorphism__init_with_int_constant_3_0);
Declare_label(mercury__polymorphism__init_with_int_constant_3_0_i2);
Declare_label(mercury__polymorphism__init_with_int_constant_3_0_i3);
Declare_label(mercury__polymorphism__init_with_int_constant_3_0_i4);
Declare_label(mercury__polymorphism__init_with_int_constant_3_0_i5);
Declare_label(mercury__polymorphism__init_with_int_constant_3_0_i6);
Declare_label(mercury__polymorphism__init_with_int_constant_3_0_i7);
Declare_static(mercury__polymorphism__get_special_proc_list_8_0);
Declare_label(mercury__polymorphism__get_special_proc_list_8_0_i2);
Declare_static(mercury__polymorphism__get_special_proc_list_2_9_0);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i4);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i5);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i6);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i7);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i8);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i9);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i10);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i1022);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i12);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i13);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i14);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i15);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i16);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i17);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i18);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i19);
Declare_label(mercury__polymorphism__get_special_proc_list_2_9_0_i1020);
Declare_static(mercury__polymorphism__get_special_proc_6_0);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i5);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i8);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i10);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i7);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i11);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i12);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i13);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i14);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i15);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i2);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i19);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i21);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i22);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i23);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i25);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i26);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i18);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i27);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i16);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i28);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i29);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i30);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i33);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i36);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i32);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i38);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i40);
Declare_label(mercury__polymorphism__get_special_proc_6_0_i41);
Declare_static(mercury__polymorphism__init_type_info_var_9_0);
Declare_label(mercury__polymorphism__init_type_info_var_9_0_i2);
Declare_label(mercury__polymorphism__init_type_info_var_9_0_i3);
Declare_label(mercury__polymorphism__init_type_info_var_9_0_i4);
Declare_label(mercury__polymorphism__init_type_info_var_9_0_i5);
Declare_label(mercury__polymorphism__init_type_info_var_9_0_i6);
Declare_label(mercury__polymorphism__init_type_info_var_9_0_i7);
Declare_label(mercury__polymorphism__init_type_info_var_9_0_i8);
Declare_label(mercury__polymorphism__init_type_info_var_9_0_i9);
Declare_label(mercury__polymorphism__init_type_info_var_9_0_i10);
Declare_label(mercury__polymorphism__init_type_info_var_9_0_i11);
Declare_static(mercury__polymorphism__init_const_base_type_info_var_10_0);
Declare_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i2);
Declare_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i3);
Declare_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i4);
Declare_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i5);
Declare_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i6);
Declare_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i7);
Declare_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i8);
Declare_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i9);
Declare_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i10);
Declare_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i11);
Declare_static(mercury__polymorphism__make_head_vars_7_0);
Declare_label(mercury__polymorphism__make_head_vars_7_0_i4);
Declare_label(mercury__polymorphism__make_head_vars_7_0_i7);
Declare_label(mercury__polymorphism__make_head_vars_7_0_i9);
Declare_label(mercury__polymorphism__make_head_vars_7_0_i10);
Declare_label(mercury__polymorphism__make_head_vars_7_0_i6);
Declare_label(mercury__polymorphism__make_head_vars_7_0_i11);
Declare_label(mercury__polymorphism__make_head_vars_7_0_i12);
Declare_label(mercury__polymorphism__make_head_vars_7_0_i1004);
Declare_static(mercury__polymorphism__new_type_info_var_7_0);
Declare_label(mercury__polymorphism__new_type_info_var_7_0_i2);
Declare_label(mercury__polymorphism__new_type_info_var_7_0_i3);
Declare_label(mercury__polymorphism__new_type_info_var_7_0_i4);
Declare_label(mercury__polymorphism__new_type_info_var_7_0_i5);
Declare_label(mercury__polymorphism__new_type_info_var_7_0_i6);
Declare_label(mercury__polymorphism__new_type_info_var_7_0_i7);
Declare_label(mercury__polymorphism__new_type_info_var_7_0_i8);

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_polymorphism__base_type_layout_poly_info_0[];
Word * mercury_data_polymorphism__base_type_info_poly_info_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_polymorphism__base_type_layout_poly_info_0
};

extern Word * mercury_data_polymorphism__common_20[];
Word * mercury_data_polymorphism__base_type_layout_poly_info_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_polymorphism__common_20),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

Word * mercury_data_polymorphism__common_0[] = {
	(Word *) string_const("mercury_builtin", 15),
	(Word *) string_const("in", 2)
};

Word * mercury_data_polymorphism__common_1[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_polymorphism__common_0),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_polymorphism__common_2[] = {
	(Word *) string_const("unify", 5)
};

Word * mercury_data_polymorphism__common_3[] = {
	(Word *) string_const("builtin_unify_pred", 18)
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data_prog_data__base_type_info_sym_name_0[];
extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_polymorphism__common_4[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_prog_data__base_type_info_sym_name_0,
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

extern Word * mercury_data_special_pred__base_type_info_special_pred_id_0[];
Word * mercury_data_polymorphism__common_5[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_special_pred__base_type_info_special_pred_id_0,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_4)
};

Word * mercury_data_polymorphism__common_6[] = {
	(Word *) string_const("__Unify__", 9)
};

extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0[];
extern Word * mercury_data_hlds_goal__base_type_info_hlds__goal_info_0[];
Word * mercury_data_polymorphism__common_7[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_expr_0,
	(Word *) (Integer) mercury_data_hlds_goal__base_type_info_hlds__goal_info_0
};

Word * mercury_data_polymorphism__common_8[] = {
	(Word *) string_const("pred", 4)
};

Word * mercury_data_polymorphism__common_9[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_8),
	(Word *) ((Integer) 0)
};

Word * mercury_data_polymorphism__common_10[] = {
	(Word *) string_const("int", 3)
};

Word mercury_data_polymorphism__common_11[] = {
	(Integer) mkword(mktag(0), mkbody(((Integer) 0))),
	(Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word mercury_data_polymorphism__common_12[] = {
	((Integer) 1),
	((Integer) 0),
	(Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_polymorphism__common_13[] = {
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0))),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_polymorphism__common_12)
};

Word * mercury_data_polymorphism__common_14[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_polymorphism__common_12),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_polymorphism__common_12)
};

Word * mercury_data_polymorphism__common_15[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_13),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_14)
};

extern Word * mercury_data_varset__base_type_info_varset_0[];
Word * mercury_data_polymorphism__common_16[] = {
	(Word *) (Integer) mercury_data_varset__base_type_info_varset_0
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data_mercury_builtin__base_type_info_var_0[];
extern Word * mercury_data_mercury_builtin__base_type_info_term_0[];
Word * mercury_data_polymorphism__common_17[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_var_0,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_term_0
};

Word * mercury_data_polymorphism__common_18[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_var_0,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_var_0
};

extern Word * mercury_data_hlds_module__base_type_info_module_info_0[];
Word * mercury_data_polymorphism__common_19[] = {
	(Word *) (Integer) mercury_data_hlds_module__base_type_info_module_info_0
};

Word * mercury_data_polymorphism__common_20[] = {
	(Word *) ((Integer) 5),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_16),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_17),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_16),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_18),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_19),
	(Word *) string_const("poly_info", 9)
};

BEGIN_MODULE(mercury__polymorphism_module0)
	init_entry(mercury__polymorphism__process_call__ua10000_8_0);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i2);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i3);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i4);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i5);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i6);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i9);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i10);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i13);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i12);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i15);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i16);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i17);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i18);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i19);
	init_label(mercury__polymorphism__process_call__ua10000_8_0_i20);
BEGIN_CODE

/* code for predicate 'polymorphism__process_call__ua10000'/8 in mode 0 */
Define_static(mercury__polymorphism__process_call__ua10000_8_0);
	incr_sp_push_msg(12, "polymorphism__process_call__ua10000");
	detstackvar(12) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 4));
	detstackvar(7) = (Integer) r1;
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 3));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	detstackvar(2) = (Integer) r3;
	{
	Declare_entry(mercury__hlds_module__module_info_pred_info_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_pred_info_3_0),
		mercury__polymorphism__process_call__ua10000_8_0_i2,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
	}
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i2);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	{
	Declare_entry(mercury__hlds_pred__pred_info_arg_types_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_arg_types_3_0),
		mercury__polymorphism__process_call__ua10000_8_0_i3,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
	}
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i3);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__varset__merge_5_0);
	call_localret(ENTRY(mercury__varset__merge_5_0),
		mercury__polymorphism__process_call__ua10000_8_0_i4,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
	}
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	detstackvar(8) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__term__vars_list_2_0);
	call_localret(ENTRY(mercury__term__vars_list_2_0),
		mercury__polymorphism__process_call__ua10000_8_0_i5,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
	}
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__process_call__ua10000_8_0_i6);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	proceed();
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i6);
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__list__remove_dups_2_0);
	call_localret(ENTRY(mercury__list__remove_dups_2_0),
		mercury__polymorphism__process_call__ua10000_8_0_i9,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
	}
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i9);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	detstackvar(9) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__map__apply_to_list_3_0);
	call_localret(ENTRY(mercury__map__apply_to_list_3_0),
		mercury__polymorphism__process_call__ua10000_8_0_i10,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
	}
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i10);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__type_util__type_list_subsumes_3_0);
	call_localret(ENTRY(mercury__type_util__type_list_subsumes_3_0),
		mercury__polymorphism__process_call__ua10000_8_0_i13,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
	}
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i13);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__process_call__ua10000_8_0_i12);
	r8 = (Integer) r2;
	r1 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(6);
	r6 = (Integer) detstackvar(7);
	r7 = (Integer) detstackvar(8);
	GOTO_LABEL(mercury__polymorphism__process_call__ua10000_8_0_i16);
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i12);
	r1 = string_const("polymorphism__process_goal_expr: type unification failed", 56);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__polymorphism__process_call__ua10000_8_0_i15,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
	}
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i15);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(6);
	r6 = (Integer) detstackvar(7);
	r7 = (Integer) detstackvar(8);
	r1 = (Integer) detstackvar(9);
	r8 = (Integer) detstackvar(2);
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i16);
	detstackvar(1) = (Integer) r2;
	detstackvar(4) = (Integer) r3;
	detstackvar(5) = (Integer) r4;
	detstackvar(6) = (Integer) r5;
	detstackvar(7) = (Integer) r6;
	detstackvar(8) = (Integer) r7;
	detstackvar(2) = (Integer) r8;
	{
	Declare_entry(mercury__term__var_list_to_term_list_2_0);
	call_localret(ENTRY(mercury__term__var_list_to_term_list_2_0),
		mercury__polymorphism__process_call__ua10000_8_0_i17,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
	}
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i17);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__term__apply_rec_substitution_to_list_3_0);
	call_localret(ENTRY(mercury__term__apply_rec_substitution_to_list_3_0),
		mercury__polymorphism__process_call__ua10000_8_0_i18,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
	}
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i18);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__polymorphism__make_vars_9_0),
		mercury__polymorphism__process_call__ua10000_8_0_i19,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i19);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	detstackvar(3) = (Integer) r2;
	r2 = (Integer) r1;
	detstackvar(2) = (Integer) r1;
	detstackvar(10) = (Integer) r3;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(1);
	detstackvar(11) = (Integer) r4;
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__process_call__ua10000_8_0_i20,
		STATIC(mercury__polymorphism__process_call__ua10000_8_0));
	}
Define_label(mercury__polymorphism__process_call__ua10000_8_0_i20);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_call__ua10000_8_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	tag_incr_hp(r4, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) detstackvar(10);
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) detstackvar(11);
	field(mktag(0), (Integer) r4, ((Integer) 2)) = (Integer) detstackvar(8);
	field(mktag(0), (Integer) r4, ((Integer) 3)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) r4, ((Integer) 4)) = (Integer) detstackvar(7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(12);
	decr_sp_pop_msg(12);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module1)
	init_entry(mercury__polymorphism__process_module_2_0);
	init_label(mercury__polymorphism__process_module_2_0_i2);
	init_label(mercury__polymorphism__process_module_2_0_i3);
	init_label(mercury__polymorphism__process_module_2_0_i4);
	init_label(mercury__polymorphism__process_module_2_0_i5);
	init_label(mercury__polymorphism__process_module_2_0_i6);
BEGIN_CODE

/* code for predicate 'polymorphism__process_module'/2 in mode 0 */
Define_entry(mercury__polymorphism__process_module_2_0);
	incr_sp_push_msg(2, "polymorphism__process_module");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__polymorphism__process_module_2_0_i2,
		ENTRY(mercury__polymorphism__process_module_2_0));
	}
Define_label(mercury__polymorphism__process_module_2_0_i2);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_module_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__polymorphism__process_module_2_0_i3,
		ENTRY(mercury__polymorphism__process_module_2_0));
	}
Define_label(mercury__polymorphism__process_module_2_0_i3);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_module_2_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__polymorphism__process_preds_3_0),
		mercury__polymorphism__process_module_2_0_i4,
		ENTRY(mercury__polymorphism__process_module_2_0));
Define_label(mercury__polymorphism__process_module_2_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_module_2_0));
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__polymorphism__process_module_2_0_i5,
		ENTRY(mercury__polymorphism__process_module_2_0));
	}
Define_label(mercury__polymorphism__process_module_2_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_module_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	{
	Declare_entry(mercury__map__keys_2_0);
	call_localret(ENTRY(mercury__map__keys_2_0),
		mercury__polymorphism__process_module_2_0_i6,
		ENTRY(mercury__polymorphism__process_module_2_0));
	}
Define_label(mercury__polymorphism__process_module_2_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_module_2_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__polymorphism__fixup_preds_3_0),
		ENTRY(mercury__polymorphism__process_module_2_0));
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module2)
	init_entry(mercury__polymorphism__process_preds_3_0);
	init_label(mercury__polymorphism__process_preds_3_0_i4);
	init_label(mercury__polymorphism__process_preds_3_0_i5);
	init_label(mercury__polymorphism__process_preds_3_0_i6);
	init_label(mercury__polymorphism__process_preds_3_0_i1002);
BEGIN_CODE

/* code for predicate 'polymorphism__process_preds'/3 in mode 0 */
Define_static(mercury__polymorphism__process_preds_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__process_preds_3_0_i1002);
	incr_sp_push_msg(4, "polymorphism__process_preds");
	detstackvar(4) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	{
	Declare_entry(mercury__hlds_module__module_info_pred_info_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_pred_info_3_0),
		mercury__polymorphism__process_preds_3_0_i4,
		STATIC(mercury__polymorphism__process_preds_3_0));
	}
Define_label(mercury__polymorphism__process_preds_3_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_preds_3_0));
	{
	Declare_entry(mercury__hlds_pred__pred_info_procids_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procids_2_0),
		mercury__polymorphism__process_preds_3_0_i5,
		STATIC(mercury__polymorphism__process_preds_3_0));
	}
Define_label(mercury__polymorphism__process_preds_3_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_preds_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__polymorphism__process_procs_4_0),
		mercury__polymorphism__process_preds_3_0_i6,
		STATIC(mercury__polymorphism__process_preds_3_0));
Define_label(mercury__polymorphism__process_preds_3_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_preds_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__polymorphism__process_preds_3_0,
		STATIC(mercury__polymorphism__process_preds_3_0));
Define_label(mercury__polymorphism__process_preds_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module3)
	init_entry(mercury__polymorphism__process_procs_4_0);
	init_label(mercury__polymorphism__process_procs_4_0_i4);
	init_label(mercury__polymorphism__process_procs_4_0_i5);
	init_label(mercury__polymorphism__process_procs_4_0_i6);
	init_label(mercury__polymorphism__process_procs_4_0_i7);
	init_label(mercury__polymorphism__process_procs_4_0_i8);
	init_label(mercury__polymorphism__process_procs_4_0_i9);
	init_label(mercury__polymorphism__process_procs_4_0_i10);
	init_label(mercury__polymorphism__process_procs_4_0_i11);
	init_label(mercury__polymorphism__process_procs_4_0_i12);
	init_label(mercury__polymorphism__process_procs_4_0_i13);
	init_label(mercury__polymorphism__process_procs_4_0_i14);
	init_label(mercury__polymorphism__process_procs_4_0_i15);
	init_label(mercury__polymorphism__process_procs_4_0_i16);
	init_label(mercury__polymorphism__process_procs_4_0_i17);
	init_label(mercury__polymorphism__process_procs_4_0_i18);
	init_label(mercury__polymorphism__process_procs_4_0_i19);
	init_label(mercury__polymorphism__process_procs_4_0_i20);
	init_label(mercury__polymorphism__process_procs_4_0_i21);
	init_label(mercury__polymorphism__process_procs_4_0_i22);
	init_label(mercury__polymorphism__process_procs_4_0_i23);
	init_label(mercury__polymorphism__process_procs_4_0_i24);
	init_label(mercury__polymorphism__process_procs_4_0_i25);
	init_label(mercury__polymorphism__process_procs_4_0_i26);
	init_label(mercury__polymorphism__process_procs_4_0_i27);
	init_label(mercury__polymorphism__process_procs_4_0_i28);
	init_label(mercury__polymorphism__process_procs_4_0_i29);
	init_label(mercury__polymorphism__process_procs_4_0_i30);
	init_label(mercury__polymorphism__process_procs_4_0_i31);
	init_label(mercury__polymorphism__process_procs_4_0_i32);
	init_label(mercury__polymorphism__process_procs_4_0_i33);
	init_label(mercury__polymorphism__process_procs_4_0_i34);
	init_label(mercury__polymorphism__process_procs_4_0_i35);
	init_label(mercury__polymorphism__process_procs_4_0_i36);
	init_label(mercury__polymorphism__process_procs_4_0_i37);
	init_label(mercury__polymorphism__process_procs_4_0_i1002);
BEGIN_CODE

/* code for predicate 'polymorphism__process_procs'/4 in mode 0 */
Define_static(mercury__polymorphism__process_procs_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__process_procs_4_0_i1002);
	incr_sp_push_msg(15, "polymorphism__process_procs");
	detstackvar(15) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__polymorphism__process_procs_4_0_i4,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r4 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__polymorphism__process_procs_4_0_i5,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(5) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__polymorphism__process_procs_4_0_i6,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	}
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__polymorphism__process_procs_4_0_i7,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_pred__pred_info_arg_types_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_arg_types_3_0),
		mercury__polymorphism__process_procs_4_0_i8,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i8);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(7) = (Integer) r1;
	detstackvar(8) = (Integer) r2;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_pred__pred_info_typevarset_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_typevarset_2_0),
		mercury__polymorphism__process_procs_4_0_i9,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i9);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(9) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__hlds_pred__proc_info_headvars_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_headvars_2_0),
		mercury__polymorphism__process_procs_4_0_i10,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i10);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__hlds_pred__proc_info_variables_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_variables_2_0),
		mercury__polymorphism__process_procs_4_0_i11,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i11);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(11) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__hlds_pred__proc_info_vartypes_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_vartypes_2_0),
		mercury__polymorphism__process_procs_4_0_i12,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i12);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(12) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__hlds_pred__proc_info_goal_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_goal_2_0),
		mercury__polymorphism__process_procs_4_0_i13,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i13);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(13) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__hlds_pred__proc_info_argmodes_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_argmodes_2_0),
		mercury__polymorphism__process_procs_4_0_i14,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i14);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) detstackvar(8);
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__term__vars_list_2_0);
	call_localret(ENTRY(mercury__term__vars_list_2_0),
		mercury__polymorphism__process_procs_4_0_i15,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i15);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__list__remove_dups_2_0);
	call_localret(ENTRY(mercury__list__remove_dups_2_0),
		mercury__polymorphism__process_procs_4_0_i16,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i16);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) detstackvar(7);
	detstackvar(7) = (Integer) r1;
	r3 = (Integer) detstackvar(11);
	r4 = (Integer) detstackvar(12);
	call_localret(STATIC(mercury__polymorphism__make_head_vars_7_0),
		mercury__polymorphism__process_procs_4_0_i17,
		STATIC(mercury__polymorphism__process_procs_4_0));
Define_label(mercury__polymorphism__process_procs_4_0_i17);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(11) = (Integer) r2;
	detstackvar(12) = (Integer) r3;
	r3 = (Integer) detstackvar(10);
	r2 = (Integer) r1;
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__process_procs_4_0_i18,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i18);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(14) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__polymorphism__process_procs_4_0_i19,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i19);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) r1;
	{
	extern Word * mercury_data_prog_data__base_type_info_mode_0[];
	r1 = (Integer) mercury_data_prog_data__base_type_info_mode_0;
	}
	r3 = (Integer) mkword(mktag(1), (Integer) mercury_data_polymorphism__common_1);
	{
	Declare_entry(mercury__list__duplicate_3_0);
	call_localret(ENTRY(mercury__list__duplicate_3_0),
		mercury__polymorphism__process_procs_4_0_i20,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i20);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) r1;
	{
	extern Word * mercury_data_prog_data__base_type_info_mode_0[];
	r1 = (Integer) mercury_data_prog_data__base_type_info_mode_0;
	}
	r3 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__process_procs_4_0_i21,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i21);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r3 = (Integer) detstackvar(7);
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r4 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__map__from_corresponding_lists_3_0);
	call_localret(ENTRY(mercury__map__from_corresponding_lists_3_0),
		mercury__polymorphism__process_procs_4_0_i22,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i22);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) r1;
	r1 = (Integer) detstackvar(13);
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(9);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(12);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(11);
	call_localret(STATIC(mercury__polymorphism__process_goal_4_0),
		mercury__polymorphism__process_procs_4_0_i23,
		STATIC(mercury__polymorphism__process_procs_4_0));
Define_label(mercury__polymorphism__process_procs_4_0_i23);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	call_localret(STATIC(mercury__polymorphism__fixup_quantification_4_0),
		mercury__polymorphism__process_procs_4_0_i24,
		STATIC(mercury__polymorphism__process_procs_4_0));
Define_label(mercury__polymorphism__process_procs_4_0_i24);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 4));
	detstackvar(8) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(9) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(10) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(11) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	r2 = (Integer) detstackvar(14);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_headvars_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_headvars_3_0),
		mercury__polymorphism__process_procs_4_0_i25,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i25);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_goal_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_goal_3_0),
		mercury__polymorphism__process_procs_4_0_i26,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i26);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_varset_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_varset_3_0),
		mercury__polymorphism__process_procs_4_0_i27,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i27);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_vartypes_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_vartypes_3_0),
		mercury__polymorphism__process_procs_4_0_i28,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i28);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_argmodes_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_argmodes_3_0),
		mercury__polymorphism__process_procs_4_0_i29,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i29);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) detstackvar(11);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_typeinfo_varmap_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_typeinfo_varmap_3_0),
		mercury__polymorphism__process_procs_4_0_i30,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i30);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__hlds_pred__pred_info_set_typevarset_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_set_typevarset_3_0),
		mercury__polymorphism__process_procs_4_0_i31,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i31);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(5) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__polymorphism__process_procs_4_0_i32,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i32);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	}
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__polymorphism__process_procs_4_0_i33,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i33);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_pred__pred_info_set_procedures_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_set_procedures_3_0),
		mercury__polymorphism__process_procs_4_0_i34,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i34);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__polymorphism__process_procs_4_0_i35,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i35);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__polymorphism__process_procs_4_0_i36,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i36);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__hlds_module__module_info_set_preds_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_set_preds_3_0),
		mercury__polymorphism__process_procs_4_0_i37,
		STATIC(mercury__polymorphism__process_procs_4_0));
	}
Define_label(mercury__polymorphism__process_procs_4_0_i37);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_procs_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(15);
	decr_sp_pop_msg(15);
	localtailcall(mercury__polymorphism__process_procs_4_0,
		STATIC(mercury__polymorphism__process_procs_4_0));
Define_label(mercury__polymorphism__process_procs_4_0_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module4)
	init_entry(mercury__polymorphism__fixup_preds_3_0);
	init_label(mercury__polymorphism__fixup_preds_3_0_i4);
	init_label(mercury__polymorphism__fixup_preds_3_0_i5);
	init_label(mercury__polymorphism__fixup_preds_3_0_i6);
	init_label(mercury__polymorphism__fixup_preds_3_0_i7);
	init_label(mercury__polymorphism__fixup_preds_3_0_i11);
	init_label(mercury__polymorphism__fixup_preds_3_0_i12);
	init_label(mercury__polymorphism__fixup_preds_3_0_i13);
	init_label(mercury__polymorphism__fixup_preds_3_0_i14);
	init_label(mercury__polymorphism__fixup_preds_3_0_i15);
	init_label(mercury__polymorphism__fixup_preds_3_0_i16);
	init_label(mercury__polymorphism__fixup_preds_3_0_i19);
	init_label(mercury__polymorphism__fixup_preds_3_0_i21);
	init_label(mercury__polymorphism__fixup_preds_3_0_i22);
	init_label(mercury__polymorphism__fixup_preds_3_0_i18);
	init_label(mercury__polymorphism__fixup_preds_3_0_i23);
	init_label(mercury__polymorphism__fixup_preds_3_0_i24);
	init_label(mercury__polymorphism__fixup_preds_3_0_i25);
	init_label(mercury__polymorphism__fixup_preds_3_0_i26);
	init_label(mercury__polymorphism__fixup_preds_3_0_i27);
	init_label(mercury__polymorphism__fixup_preds_3_0_i8);
	init_label(mercury__polymorphism__fixup_preds_3_0_i1004);
BEGIN_CODE

/* code for predicate 'polymorphism__fixup_preds'/3 in mode 0 */
Define_static(mercury__polymorphism__fixup_preds_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__fixup_preds_3_0_i1004);
	incr_sp_push_msg(11, "polymorphism__fixup_preds");
	detstackvar(11) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_module__module_info_preds_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_preds_2_0),
		mercury__polymorphism__fixup_preds_3_0_i4,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	r3 = (Integer) r1;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r4 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__polymorphism__fixup_preds_3_0_i5,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	detstackvar(5) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__pred_info_procedures_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procedures_2_0),
		mercury__polymorphism__fixup_preds_3_0_i6,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_pred__pred_info_procids_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_procids_2_0),
		mercury__polymorphism__fixup_preds_3_0_i7,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__fixup_preds_3_0_i8);
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0;
	}
	r3 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__polymorphism__fixup_preds_3_0_i11,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i11);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	detstackvar(6) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__proc_info_vartypes_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_vartypes_2_0),
		mercury__polymorphism__fixup_preds_3_0_i12,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i12);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	r2 = (Integer) detstackvar(6);
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_pred__proc_info_headvars_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_headvars_2_0),
		mercury__polymorphism__fixup_preds_3_0_i13,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i13);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_pred__pred_info_arg_types_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_arg_types_3_0),
		mercury__polymorphism__fixup_preds_3_0_i14,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i14);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	detstackvar(8) = (Integer) r1;
	detstackvar(9) = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__polymorphism__fixup_preds_3_0_i15,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i15);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__polymorphism__fixup_preds_3_0_i16,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i16);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	r2 = ((Integer) r1 - (Integer) detstackvar(10));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__list__split_list_4_0);
	call_localret(ENTRY(mercury__list__split_list_4_0),
		mercury__polymorphism__fixup_preds_3_0_i19,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i19);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__fixup_preds_3_0_i18);
	r3 = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r4 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__map__apply_to_list_3_0);
	call_localret(ENTRY(mercury__map__apply_to_list_3_0),
		mercury__polymorphism__fixup_preds_3_0_i21,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i21);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__fixup_preds_3_0_i22,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i22);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	r7 = (Integer) detstackvar(4);
	GOTO_LABEL(mercury__polymorphism__fixup_preds_3_0_i24);
Define_label(mercury__polymorphism__fixup_preds_3_0_i18);
	r1 = string_const("polymorphism.m: list__split_list failed", 39);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__polymorphism__fixup_preds_3_0_i23,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i23);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	r7 = (Integer) detstackvar(4);
Define_label(mercury__polymorphism__fixup_preds_3_0_i24);
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r6;
	detstackvar(4) = (Integer) r7;
	{
	Declare_entry(mercury__hlds_pred__pred_info_set_arg_types_4_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_set_arg_types_4_0),
		mercury__polymorphism__fixup_preds_3_0_i25,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i25);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	{
	extern Word * mercury_data_hlds_pred__base_type_info_pred_info_0[];
	r2 = (Integer) mercury_data_hlds_pred__base_type_info_pred_info_0;
	}
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__polymorphism__fixup_preds_3_0_i26,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i26);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_module__module_info_set_preds_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_set_preds_3_0),
		mercury__polymorphism__fixup_preds_3_0_i27,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
	}
Define_label(mercury__polymorphism__fixup_preds_3_0_i27);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_preds_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	localtailcall(mercury__polymorphism__fixup_preds_3_0,
		STATIC(mercury__polymorphism__fixup_preds_3_0));
Define_label(mercury__polymorphism__fixup_preds_3_0_i8);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
Define_label(mercury__polymorphism__fixup_preds_3_0_i1004);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module5)
	init_entry(mercury__polymorphism__process_goal_4_0);
BEGIN_CODE

/* code for predicate 'polymorphism__process_goal'/4 in mode 0 */
Define_static(mercury__polymorphism__process_goal_4_0);
	r3 = (Integer) r2;
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	tailcall(STATIC(mercury__polymorphism__process_goal_expr_5_0),
		STATIC(mercury__polymorphism__process_goal_4_0));
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module6)
	init_entry(mercury__polymorphism__process_goal_expr_5_0);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i1007);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i1006);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i1005);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i1004);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i1003);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i1002);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i1001);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i5);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i6);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i7);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i12);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i16);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i19);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i22);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i18);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i24);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i25);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i26);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i27);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i13);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i30);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i32);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i35);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i38);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i34);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i40);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i41);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i42);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i29);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i46);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i48);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i49);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i50);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i51);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i52);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i45);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i54);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i8);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i61);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i62);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i63);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i64);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i58);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i67);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i68);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i69);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i70);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i71);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i72);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i73);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i74);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i75);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i76);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i77);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i78);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i79);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i80);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i81);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i82);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i83);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i84);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i85);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i86);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i87);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i1000);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i90);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i89);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i95);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i96);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i98);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i100);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i104);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i105);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i107);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i108);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i93);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i92);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i109);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i110);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i111);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i112);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i113);
	init_label(mercury__polymorphism__process_goal_expr_5_0_i91);
BEGIN_CODE

/* code for predicate 'polymorphism__process_goal_expr'/5 in mode 0 */
Define_static(mercury__polymorphism__process_goal_expr_5_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i1000);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__polymorphism__process_goal_expr_5_0_i1007) AND
		LABEL(mercury__polymorphism__process_goal_expr_5_0_i1006) AND
		LABEL(mercury__polymorphism__process_goal_expr_5_0_i1005) AND
		LABEL(mercury__polymorphism__process_goal_expr_5_0_i1004) AND
		LABEL(mercury__polymorphism__process_goal_expr_5_0_i1003) AND
		LABEL(mercury__polymorphism__process_goal_expr_5_0_i1002) AND
		LABEL(mercury__polymorphism__process_goal_expr_5_0_i1001));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i1007);
	incr_sp_push_msg(16, "polymorphism__process_goal_expr");
	detstackvar(16) = (Integer) succip;
	GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i5);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i1006);
	incr_sp_push_msg(16, "polymorphism__process_goal_expr");
	detstackvar(16) = (Integer) succip;
	GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i7);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i1005);
	incr_sp_push_msg(16, "polymorphism__process_goal_expr");
	detstackvar(16) = (Integer) succip;
	GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i67);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i1004);
	incr_sp_push_msg(16, "polymorphism__process_goal_expr");
	detstackvar(16) = (Integer) succip;
	GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i69);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i1003);
	incr_sp_push_msg(16, "polymorphism__process_goal_expr");
	detstackvar(16) = (Integer) succip;
	GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i71);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i1002);
	incr_sp_push_msg(16, "polymorphism__process_goal_expr");
	detstackvar(16) = (Integer) succip;
	GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i73);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i1001);
	incr_sp_push_msg(16, "polymorphism__process_goal_expr");
	detstackvar(16) = (Integer) succip;
	GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i77);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i5);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__polymorphism__process_case_list_4_0),
		mercury__polymorphism__process_goal_expr_5_0_i6,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 5));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 4)) = (Integer) detstackvar(4);
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) r3;
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(3);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	decr_sp_pop_msg(16);
	proceed();
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i7);
	r4 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 5));
	r5 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r6 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r7 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r8 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	if ((tag((Integer) r5) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i8);
	if (((Integer) field(mktag(3), (Integer) r5, ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i8);
	if ((tag((Integer) r7) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i8);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r8;
	detstackvar(4) = (Integer) r7;
	detstackvar(6) = (Integer) r4;
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r5, ((Integer) 1));
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r5, ((Integer) 2));
	detstackvar(8) = (Integer) field(mktag(0), (Integer) r7, ((Integer) 0));
	detstackvar(9) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 3));
	detstackvar(10) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 4));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	r4 = (Integer) r8;
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__polymorphism__process_goal_expr_5_0_i12,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i12);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i13);
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__hlds_module__module_info_get_predicate_table_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_get_predicate_table_2_0),
		mercury__polymorphism__process_goal_expr_5_0_i16,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i16);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = string_const("mercury_builtin", 15);
	r3 = string_const("unify", 5);
	r4 = ((Integer) 2);
	{
	Declare_entry(mercury__hlds_module__predicate_table_search_pred_m_n_a_5_0);
	call_localret(ENTRY(mercury__hlds_module__predicate_table_search_pred_m_n_a_5_0),
		mercury__polymorphism__process_goal_expr_5_0_i19,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i19);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i18);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i18);
	r3 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(7) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		mercury__polymorphism__process_goal_expr_5_0_i22,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i22);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i18);
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r7 = (Integer) detstackvar(3);
	r8 = (Integer) detstackvar(4);
	r9 = (Integer) detstackvar(6);
	r10 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(9);
	r11 = (Integer) detstackvar(10);
	r4 = (Integer) detstackvar(5);
	r12 = (Integer) detstackvar(7);
	r13 = ((Integer) 0);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i25);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i18);
	r1 = string_const("polymorphism.m: can't find `mercury_builtin:unify/2'", 52);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__polymorphism__process_goal_expr_5_0_i24,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i24);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r7 = (Integer) detstackvar(3);
	r8 = (Integer) detstackvar(4);
	r9 = (Integer) detstackvar(6);
	r10 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(9);
	r11 = (Integer) detstackvar(10);
	r4 = (Integer) detstackvar(5);
	r12 = (Integer) detstackvar(7);
	r13 = (Integer) detstackvar(11);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i25);
	detstackvar(1) = (Integer) r5;
	detstackvar(2) = (Integer) r6;
	detstackvar(3) = (Integer) r7;
	detstackvar(4) = (Integer) r8;
	detstackvar(6) = (Integer) r9;
	detstackvar(8) = (Integer) r10;
	detstackvar(10) = (Integer) r11;
	detstackvar(7) = (Integer) r12;
	detstackvar(11) = (Integer) r13;
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__polymorphism__process_goal_expr_5_0_i26,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i26);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	detstackvar(12) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_2);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	detstackvar(13) = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(8);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) detstackvar(13), ((Integer) 1)) = (Integer) r2;
	r1 = (Integer) detstackvar(10);
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(11);
	{
	Declare_entry(mercury__code_util__is_builtin_4_0);
	call_localret(ENTRY(mercury__code_util__is_builtin_4_0),
		mercury__polymorphism__process_goal_expr_5_0_i27,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i27);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	tag_incr_hp(r3, mktag(1), ((Integer) 6));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(11);
	field(mktag(1), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(13);
	field(mktag(1), (Integer) r3, ((Integer) 3)) = (Integer) r2;
	tag_incr_hp(r4, mktag(1), ((Integer) 1));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(1), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	r2 = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r3, ((Integer) 5)) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_2);
	field(mktag(1), (Integer) r3, ((Integer) 4)) = (Integer) r4;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	decr_sp_pop_msg(16);
	proceed();
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i13);
	detstackvar(9) = (Integer) r1;
	{
	Declare_entry(mercury__type_util__type_is_higher_order_3_0);
	call_localret(ENTRY(mercury__type_util__type_is_higher_order_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i30,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i30);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i29);
	detstackvar(13) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_3);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	detstackvar(14) = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(8);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) detstackvar(14), ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__hlds_module__module_info_get_predicate_table_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_get_predicate_table_2_0),
		mercury__polymorphism__process_goal_expr_5_0_i32,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i32);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = string_const("mercury_builtin", 15);
	r3 = string_const("builtin_unify_pred", 18);
	r4 = ((Integer) 2);
	{
	Declare_entry(mercury__hlds_module__predicate_table_search_pred_m_n_a_5_0);
	call_localret(ENTRY(mercury__hlds_module__predicate_table_search_pred_m_n_a_5_0),
		mercury__polymorphism__process_goal_expr_5_0_i35,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i35);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i34);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i34);
	r3 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(11) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		mercury__polymorphism__process_goal_expr_5_0_i38,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i38);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i34);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(6);
	r1 = ((Integer) 1);
	r2 = ((Integer) 1);
	r8 = (Integer) detstackvar(11);
	r9 = ((Integer) 0);
	r10 = (Integer) detstackvar(13);
	r11 = (Integer) detstackvar(14);
	GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i41);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i34);
	r1 = string_const("can't locate mercury_builtin:builtin_unify_pred/2", 49);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__polymorphism__process_goal_expr_5_0_i40,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i40);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(6);
	r8 = (Integer) detstackvar(11);
	r9 = (Integer) detstackvar(12);
	r10 = (Integer) detstackvar(13);
	r11 = (Integer) detstackvar(14);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i41);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	detstackvar(4) = (Integer) r6;
	detstackvar(6) = (Integer) r7;
	detstackvar(11) = (Integer) r8;
	detstackvar(12) = (Integer) r9;
	detstackvar(13) = (Integer) r10;
	detstackvar(14) = (Integer) r11;
	{
	Declare_entry(mercury__hlds_goal__hlds__is_builtin_make_builtin_3_0);
	call_localret(ENTRY(mercury__hlds_goal__hlds__is_builtin_make_builtin_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i42,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i42);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 6));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(11);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(12);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(14);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	tag_incr_hp(r3, mktag(1), ((Integer) 1));
	r2 = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 5)) = (Integer) detstackvar(13);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	field(mktag(1), (Integer) r1, ((Integer) 4)) = (Integer) r3;
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) tempr1;
	r3 = (Integer) detstackvar(2);
	decr_sp_pop_msg(16);
	localtailcall(mercury__polymorphism__process_goal_expr_5_0,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i29);
	r1 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__type_util__type_to_type_id_3_0);
	call_localret(ENTRY(mercury__type_util__type_to_type_id_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i46,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i46);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i45);
	detstackvar(9) = (Integer) r2;
	r1 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__hlds_module__module_info_get_special_pred_map_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_get_special_pred_map_2_0),
		mercury__polymorphism__process_goal_expr_5_0_i48,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i48);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_5);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	tag_incr_hp(r4, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r4, ((Integer) 0)) = ((Integer) 0);
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__polymorphism__process_goal_expr_5_0_i49,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i49);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) detstackvar(7);
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 1);
	{
	Declare_entry(mercury__hlds_data__determinism_components_3_1);
	call_localret(ENTRY(mercury__hlds_data__determinism_components_3_1),
		mercury__polymorphism__process_goal_expr_5_0_i50,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i50);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(10);
	r2 = (Integer) detstackvar(9);
	r3 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__unify_proc__lookup_mode_num_5_0);
	call_localret(ENTRY(mercury__unify_proc__lookup_mode_num_5_0),
		mercury__polymorphism__process_goal_expr_5_0_i51,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i51);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	detstackvar(5) = (Integer) r1;
	r2 = (Integer) detstackvar(8);
	detstackvar(8) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_6);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	detstackvar(9) = (Integer) r1;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) detstackvar(9), ((Integer) 1)) = (Integer) r3;
	r1 = ((Integer) 1);
	r2 = ((Integer) 1);
	{
	Declare_entry(mercury__hlds_goal__hlds__is_builtin_make_builtin_3_0);
	call_localret(ENTRY(mercury__hlds_goal__hlds__is_builtin_make_builtin_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i52,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i52);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 6));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(9);
	field(mktag(1), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	tag_incr_hp(r3, mktag(1), ((Integer) 1));
	r2 = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 5)) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_6);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	field(mktag(1), (Integer) r1, ((Integer) 4)) = (Integer) r3;
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) tempr1;
	r3 = (Integer) detstackvar(2);
	decr_sp_pop_msg(16);
	localtailcall(mercury__polymorphism__process_goal_expr_5_0,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i45);
	r1 = string_const("polymorphism: type_to_type_id failed", 36);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__polymorphism__process_goal_expr_5_0_i54,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i54);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	decr_sp_pop_msg(16);
	proceed();
Define_label(mercury__polymorphism__process_goal_expr_5_0_i8);
	if ((tag((Integer) r7) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i58);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(2), (Integer) r7, ((Integer) 4));
	detstackvar(11) = (Integer) field(mktag(2), (Integer) r7, ((Integer) 3));
	detstackvar(9) = (Integer) field(mktag(2), (Integer) r7, ((Integer) 2));
	detstackvar(8) = (Integer) field(mktag(2), (Integer) r7, ((Integer) 1));
	detstackvar(7) = (Integer) field(mktag(2), (Integer) r7, ((Integer) 0));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r8;
	detstackvar(4) = (Integer) r6;
	detstackvar(5) = (Integer) r5;
	detstackvar(6) = (Integer) r4;
	detstackvar(10) = (Integer) tempr1;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__polymorphism__process_goal_expr_5_0_i61,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i61);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(10);
	call_localret(STATIC(mercury__polymorphism__process_goal_4_0),
		mercury__polymorphism__process_goal_expr_5_0_i62,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i62);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	call_localret(STATIC(mercury__polymorphism__fixup_quantification_4_0),
		mercury__polymorphism__process_goal_expr_5_0_i63,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i63);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r6 = (Integer) r1;
	r8 = (Integer) r2;
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(9);
	r4 = (Integer) detstackvar(11);
	r5 = (Integer) detstackvar(2);
	r7 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__polymorphism__process_lambda_11_0),
		mercury__polymorphism__process_goal_expr_5_0_i64,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i64);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) tempr1, ((Integer) 4)) = (Integer) r2;
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	r2 = (Integer) r3;
	field(mktag(3), (Integer) tempr1, ((Integer) 5)) = (Integer) detstackvar(6);
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(4);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) r4;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	decr_sp_pop_msg(16);
	proceed();
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i58);
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	decr_sp_pop_msg(16);
	proceed();
Define_label(mercury__polymorphism__process_goal_expr_5_0_i67);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__polymorphism__process_goal_list_4_0),
		mercury__polymorphism__process_goal_expr_5_0_i68,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i68);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	decr_sp_pop_msg(16);
	proceed();
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i69);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__polymorphism__process_goal_4_0),
		mercury__polymorphism__process_goal_expr_5_0_i70,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i70);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 3);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	decr_sp_pop_msg(16);
	proceed();
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i71);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__polymorphism__process_goal_4_0),
		mercury__polymorphism__process_goal_expr_5_0_i72,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i72);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) r3;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 4);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	decr_sp_pop_msg(16);
	proceed();
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i73);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 5));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__polymorphism__process_goal_4_0),
		mercury__polymorphism__process_goal_expr_5_0_i74,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i74);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r3 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__polymorphism__process_goal_4_0),
		mercury__polymorphism__process_goal_expr_5_0_i75,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i75);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r3 = (Integer) detstackvar(4);
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__polymorphism__process_goal_4_0),
		mercury__polymorphism__process_goal_expr_5_0_i76,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i76);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 6));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 4)) = (Integer) r3;
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(4);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(3);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) tempr1, ((Integer) 5)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	decr_sp_pop_msg(16);
	proceed();
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i77);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 5));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 6));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(5) = (Integer) r1;
	call_localret(STATIC(mercury__polymorphism__process_call__ua10000_8_0),
		mercury__polymorphism__process_goal_expr_5_0_i78,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i78);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	detstackvar(3) = (Integer) r4;
	detstackvar(8) = (Integer) r1;
	detstackvar(9) = (Integer) r2;
	detstackvar(10) = (Integer) r3;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__polymorphism__process_goal_expr_5_0_i79,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i79);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__set__insert_list_3_0);
	call_localret(ENTRY(mercury__set__insert_list_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i80,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i80);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_nonlocals_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_nonlocals_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i81,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i81);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	detstackvar(11) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) detstackvar(3), ((Integer) 4));
	r2 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_module__module_info_pred_info_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_pred_info_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i82,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i82);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	{
	Declare_entry(mercury__hlds_pred__pred_info_arg_types_3_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_arg_types_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i83,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i83);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	detstackvar(12) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__term__vars_list_2_0);
	call_localret(ENTRY(mercury__term__vars_list_2_0),
		mercury__polymorphism__process_goal_expr_5_0_i84,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i84);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__list__remove_dups_2_0);
	call_localret(ENTRY(mercury__list__remove_dups_2_0),
		mercury__polymorphism__process_goal_expr_5_0_i85,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i85);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	r3 = (Integer) detstackvar(12);
	r4 = (Integer) detstackvar(7);
	call_localret(STATIC(mercury__polymorphism__c_code_add_typeinfos_5_0),
		mercury__polymorphism__process_goal_expr_5_0_i86,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i86);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_7);
	r2 = (Integer) detstackvar(10);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	tag_incr_hp(r5, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 7));
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 5)) = (Integer) detstackvar(8);
	field(mktag(3), (Integer) tempr1, ((Integer) 4)) = (Integer) detstackvar(6);
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(5);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 6);
	field(mktag(3), (Integer) tempr1, ((Integer) 6)) = (Integer) r4;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r5;
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) detstackvar(11);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__process_goal_expr_5_0_i87,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i87);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__conj_list_to_goal_3_0);
	call_localret(ENTRY(mercury__hlds_goal__conj_list_to_goal_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i54,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i1000);
	incr_sp_push_msg(16, "polymorphism__process_goal_expr");
	detstackvar(16) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i89);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__polymorphism__process_goal_list_4_0),
		mercury__polymorphism__process_goal_expr_5_0_i90,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i90);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	decr_sp_pop_msg(16);
	proceed();
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i89);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i91);
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 5));
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 4));
	r6 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 3));
	r7 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 2));
	r8 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r9 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if ((tag((Integer) r4) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i92);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r9;
	detstackvar(5) = (Integer) r8;
	detstackvar(6) = (Integer) r7;
	detstackvar(7) = (Integer) r6;
	detstackvar(8) = (Integer) r5;
	detstackvar(9) = (Integer) r4;
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) r7;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__polymorphism__process_goal_expr_5_0_i95,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i95);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__special_pred__special_pred_name_arity_4_1);
	call_localret(ENTRY(mercury__special_pred__special_pred_name_arity_4_1),
		mercury__polymorphism__process_goal_expr_5_0_i96,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i96);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i93);
	detstackvar(3) = (Integer) r2;
	r2 = (Integer) r3;
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(2);
	detstackvar(11) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 4));
	detstackvar(10) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__special_pred__special_pred_get_type_3_0);
	call_localret(ENTRY(mercury__special_pred__special_pred_get_type_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i98,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i98);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i93);
	r4 = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(10);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__polymorphism__process_goal_expr_5_0_i100,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i100);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	if ((tag((Integer) r1) == mktag(((Integer) 1))))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i93);
	detstackvar(10) = (Integer) r1;
	{
	Declare_entry(mercury__special_pred__special_pred_list_1_0);
	call_localret(ENTRY(mercury__special_pred__special_pred_list_1_0),
		mercury__polymorphism__process_goal_expr_5_0_i104,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i104);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_special_pred__base_type_info_special_pred_id_0;
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__polymorphism__process_goal_expr_5_0_i105,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i105);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i93);
	r1 = (Integer) detstackvar(10);
	r2 = (Integer) detstackvar(11);
	{
	Declare_entry(mercury__type_util__classify_type_3_0);
	call_localret(ENTRY(mercury__type_util__classify_type_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i107,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i107);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(11);
	call_localret(STATIC(mercury__polymorphism__get_special_proc_6_0),
		mercury__polymorphism__process_goal_expr_5_0_i108,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i108);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r7 = (Integer) r1;
	r8 = (Integer) r3;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(1);
	r5 = (Integer) detstackvar(7);
	r6 = (Integer) detstackvar(8);
	GOTO_LABEL(mercury__polymorphism__process_goal_expr_5_0_i109);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i93);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r9 = (Integer) detstackvar(4);
	r8 = (Integer) detstackvar(5);
	r7 = (Integer) detstackvar(6);
	r6 = (Integer) detstackvar(7);
	r5 = (Integer) detstackvar(8);
	r4 = (Integer) detstackvar(9);
Define_label(mercury__polymorphism__process_goal_expr_5_0_i92);
	{
	Word tempr1, tempr2;
	tempr1 = (Integer) r4;
	r4 = (Integer) r2;
	r2 = (Integer) r7;
	r7 = (Integer) tempr1;
	tempr2 = (Integer) r5;
	r5 = (Integer) r6;
	r6 = (Integer) tempr2;
	r1 = (Integer) r9;
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i109);
	detstackvar(1) = (Integer) r4;
	detstackvar(7) = (Integer) r5;
	detstackvar(8) = (Integer) r6;
	detstackvar(10) = (Integer) r7;
	detstackvar(11) = (Integer) r1;
	detstackvar(12) = (Integer) r8;
	call_localret(STATIC(mercury__polymorphism__process_call__ua10000_8_0),
		mercury__polymorphism__process_goal_expr_5_0_i110,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
Define_label(mercury__polymorphism__process_goal_expr_5_0_i110);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	detstackvar(3) = (Integer) r4;
	detstackvar(13) = (Integer) r1;
	detstackvar(14) = (Integer) r2;
	detstackvar(15) = (Integer) r3;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__polymorphism__process_goal_expr_5_0_i111,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i111);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(14);
	{
	Declare_entry(mercury__set__insert_list_3_0);
	call_localret(ENTRY(mercury__set__insert_list_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i112,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i112);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_nonlocals_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_nonlocals_3_0),
		mercury__polymorphism__process_goal_expr_5_0_i113,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i113);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_expr_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_7);
	r2 = (Integer) detstackvar(15);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	tag_incr_hp(r5, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 6));
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r5;
	field(mktag(1), (Integer) tempr1, ((Integer) 5)) = (Integer) detstackvar(10);
	field(mktag(1), (Integer) tempr1, ((Integer) 4)) = (Integer) detstackvar(8);
	field(mktag(1), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(13);
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(12);
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(11);
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) r4;
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__process_goal_expr_5_0_i87,
		STATIC(mercury__polymorphism__process_goal_expr_5_0));
	}
	}
Define_label(mercury__polymorphism__process_goal_expr_5_0_i91);
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(16);
	decr_sp_pop_msg(16);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module7)
	init_entry(mercury__polymorphism__c_code_add_typeinfos_5_0);
	init_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i6);
	init_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i9);
	init_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i11);
	init_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i8);
	init_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i1011);
	init_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i1009);
	init_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i1010);
BEGIN_CODE

/* code for predicate 'polymorphism__c_code_add_typeinfos'/5 in mode 0 */
Define_static(mercury__polymorphism__c_code_add_typeinfos_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__c_code_add_typeinfos_5_0_i1011);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__c_code_add_typeinfos_5_0_i1009);
	incr_sp_push_msg(3, "polymorphism__c_code_add_typeinfos");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	localcall(mercury__polymorphism__c_code_add_typeinfos_5_0,
		LABEL(mercury__polymorphism__c_code_add_typeinfos_5_0_i6),
		STATIC(mercury__polymorphism__c_code_add_typeinfos_5_0));
Define_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__c_code_add_typeinfos_5_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__varset__search_name_3_0);
	call_localret(ENTRY(mercury__varset__search_name_3_0),
		mercury__polymorphism__c_code_add_typeinfos_5_0_i9,
		STATIC(mercury__polymorphism__c_code_add_typeinfos_5_0));
	}
Define_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i9);
	update_prof_current_proc(LABEL(mercury__polymorphism__c_code_add_typeinfos_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__c_code_add_typeinfos_5_0_i8);
	r1 = string_const("TypeInfo_for_", 13);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__polymorphism__c_code_add_typeinfos_5_0_i11,
		STATIC(mercury__polymorphism__c_code_add_typeinfos_5_0));
	}
Define_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i11);
	update_prof_current_proc(LABEL(mercury__polymorphism__c_code_add_typeinfos_5_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i8);
	r2 = (Integer) detstackvar(1);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i1011);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__c_code_add_typeinfos_5_0_i1010);
	r1 = string_const("polymorphism__c_code_add_typeinfos: length mismatch", 51);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__polymorphism__c_code_add_typeinfos_5_0));
	}
Define_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i1009);
	r1 = string_const("polymorphism__c_code_add_typeinfos: length mismatch", 51);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__polymorphism__c_code_add_typeinfos_5_0));
	}
Define_label(mercury__polymorphism__c_code_add_typeinfos_5_0_i1010);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module8)
	init_entry(mercury__polymorphism__process_goal_list_4_0);
	init_label(mercury__polymorphism__process_goal_list_4_0_i4);
	init_label(mercury__polymorphism__process_goal_list_4_0_i5);
	init_label(mercury__polymorphism__process_goal_list_4_0_i1002);
BEGIN_CODE

/* code for predicate 'polymorphism__process_goal_list'/4 in mode 0 */
Define_static(mercury__polymorphism__process_goal_list_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__process_goal_list_4_0_i1002);
	incr_sp_push_msg(2, "polymorphism__process_goal_list");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__polymorphism__process_goal_4_0),
		mercury__polymorphism__process_goal_list_4_0_i4,
		STATIC(mercury__polymorphism__process_goal_list_4_0));
Define_label(mercury__polymorphism__process_goal_list_4_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_list_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r3;
	localcall(mercury__polymorphism__process_goal_list_4_0,
		LABEL(mercury__polymorphism__process_goal_list_4_0_i5),
		STATIC(mercury__polymorphism__process_goal_list_4_0));
Define_label(mercury__polymorphism__process_goal_list_4_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_goal_list_4_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__polymorphism__process_goal_list_4_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module9)
	init_entry(mercury__polymorphism__process_case_list_4_0);
	init_label(mercury__polymorphism__process_case_list_4_0_i4);
	init_label(mercury__polymorphism__process_case_list_4_0_i5);
	init_label(mercury__polymorphism__process_case_list_4_0_i1003);
BEGIN_CODE

/* code for predicate 'polymorphism__process_case_list'/4 in mode 0 */
Define_static(mercury__polymorphism__process_case_list_4_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__process_case_list_4_0_i1003);
	incr_sp_push_msg(3, "polymorphism__process_case_list");
	detstackvar(3) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	call_localret(STATIC(mercury__polymorphism__process_goal_4_0),
		mercury__polymorphism__process_case_list_4_0_i4,
		STATIC(mercury__polymorphism__process_case_list_4_0));
Define_label(mercury__polymorphism__process_case_list_4_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_case_list_4_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	localcall(mercury__polymorphism__process_case_list_4_0,
		LABEL(mercury__polymorphism__process_case_list_4_0_i5),
		STATIC(mercury__polymorphism__process_case_list_4_0));
Define_label(mercury__polymorphism__process_case_list_4_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_case_list_4_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__polymorphism__process_case_list_4_0_i1003);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module10)
	init_entry(mercury__polymorphism__fixup_quantification_4_0);
	init_label(mercury__polymorphism__fixup_quantification_4_0_i2);
	init_label(mercury__polymorphism__fixup_quantification_4_0_i3);
	init_label(mercury__polymorphism__fixup_quantification_4_0_i6);
	init_label(mercury__polymorphism__fixup_quantification_4_0_i7);
	init_label(mercury__polymorphism__fixup_quantification_4_0_i8);
	init_label(mercury__polymorphism__fixup_quantification_4_0_i9);
BEGIN_CODE

/* code for predicate 'polymorphism__fixup_quantification'/4 in mode 0 */
Define_static(mercury__polymorphism__fixup_quantification_4_0);
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	incr_sp_push_msg(8, "polymorphism__fixup_quantification");
	detstackvar(8) = (Integer) succip;
	detstackvar(5) = (Integer) r3;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 4));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__map__values_2_0);
	call_localret(ENTRY(mercury__map__values_2_0),
		mercury__polymorphism__fixup_quantification_4_0_i2,
		STATIC(mercury__polymorphism__fixup_quantification_4_0));
	}
Define_label(mercury__polymorphism__fixup_quantification_4_0_i2);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_quantification_4_0));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__fixup_quantification_4_0_i3);
	r1 = (Integer) detstackvar(1);
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__polymorphism__fixup_quantification_4_0_i3);
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) detstackvar(1), ((Integer) 1));
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__polymorphism__fixup_quantification_4_0_i6,
		STATIC(mercury__polymorphism__fixup_quantification_4_0));
	}
Define_label(mercury__polymorphism__fixup_quantification_4_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_quantification_4_0));
	r2 = (Integer) detstackvar(7);
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__list_to_set_2_0);
	call_localret(ENTRY(mercury__set__list_to_set_2_0),
		mercury__polymorphism__fixup_quantification_4_0_i7,
		STATIC(mercury__polymorphism__fixup_quantification_4_0));
	}
Define_label(mercury__polymorphism__fixup_quantification_4_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_quantification_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__polymorphism__fixup_quantification_4_0_i8,
		STATIC(mercury__polymorphism__fixup_quantification_4_0));
	}
Define_label(mercury__polymorphism__fixup_quantification_4_0_i8);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_quantification_4_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__quantification__implicitly_quantify_goal_8_0);
	call_localret(ENTRY(mercury__quantification__implicitly_quantify_goal_8_0),
		mercury__polymorphism__fixup_quantification_4_0_i9,
		STATIC(mercury__polymorphism__fixup_quantification_4_0));
	}
Define_label(mercury__polymorphism__fixup_quantification_4_0_i9);
	update_prof_current_proc(LABEL(mercury__polymorphism__fixup_quantification_4_0));
	r5 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r5;
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module11)
	init_entry(mercury__polymorphism__process_lambda_11_0);
	init_label(mercury__polymorphism__process_lambda_11_0_i2);
BEGIN_CODE

/* code for predicate 'polymorphism__process_lambda'/11 in mode 0 */
Define_static(mercury__polymorphism__process_lambda_11_0);
	r11 = (Integer) field(mktag(0), (Integer) r8, ((Integer) 3));
	r10 = (Integer) field(mktag(0), (Integer) r8, ((Integer) 2));
	r9 = (Integer) field(mktag(0), (Integer) r8, ((Integer) 1));
	incr_sp_push_msg(5, "polymorphism__process_lambda");
	detstackvar(5) = (Integer) succip;
	detstackvar(4) = (Integer) r11;
	detstackvar(3) = (Integer) r10;
	detstackvar(2) = (Integer) r9;
	r12 = (Integer) field(mktag(0), (Integer) r8, ((Integer) 4));
	r8 = (Integer) field(mktag(0), (Integer) r8, ((Integer) 0));
	detstackvar(1) = (Integer) r8;
	{
	Declare_entry(mercury__lambda__transform_lambda_15_0);
	call_localret(ENTRY(mercury__lambda__transform_lambda_15_0),
		mercury__polymorphism__process_lambda_11_0_i2,
		STATIC(mercury__polymorphism__process_lambda_11_0));
	}
Define_label(mercury__polymorphism__process_lambda_11_0_i2);
	update_prof_current_proc(LABEL(mercury__polymorphism__process_lambda_11_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) tempr1, ((Integer) 4)) = (Integer) r3;
	r3 = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
	}
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module12)
	init_entry(mercury__polymorphism__make_vars_9_0);
	init_label(mercury__polymorphism__make_vars_9_0_i4);
	init_label(mercury__polymorphism__make_vars_9_0_i5);
	init_label(mercury__polymorphism__make_vars_9_0_i6);
	init_label(mercury__polymorphism__make_vars_9_0_i1002);
BEGIN_CODE

/* code for predicate 'polymorphism__make_vars'/9 in mode 0 */
Define_static(mercury__polymorphism__make_vars_9_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__make_vars_9_0_i1002);
	incr_sp_push_msg(4, "polymorphism__make_vars");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__polymorphism__make_var_9_0),
		mercury__polymorphism__make_vars_9_0_i4,
		STATIC(mercury__polymorphism__make_vars_9_0));
Define_label(mercury__polymorphism__make_vars_9_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_vars_9_0));
	r5 = (Integer) r4;
	r4 = (Integer) r3;
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	localcall(mercury__polymorphism__make_vars_9_0,
		LABEL(mercury__polymorphism__make_vars_9_0_i5),
		STATIC(mercury__polymorphism__make_vars_9_0));
Define_label(mercury__polymorphism__make_vars_9_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_vars_9_0));
	{
	Word tempr1, tempr2;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	tempr2 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) tempr2;
	detstackvar(1) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_7);
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__make_vars_9_0_i6,
		STATIC(mercury__polymorphism__make_vars_9_0));
	}
	}
Define_label(mercury__polymorphism__make_vars_9_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_vars_9_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__polymorphism__make_vars_9_0_i1002);
	r3 = (Integer) r4;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module13)
	init_entry(mercury__polymorphism__make_var_9_0);
	init_label(mercury__polymorphism__make_var_9_0_i4);
	init_label(mercury__polymorphism__make_var_9_0_i3);
	init_label(mercury__polymorphism__make_var_9_0_i9);
	init_label(mercury__polymorphism__make_var_9_0_i8);
	init_label(mercury__polymorphism__make_var_9_0_i15);
	init_label(mercury__polymorphism__make_var_9_0_i13);
	init_label(mercury__polymorphism__make_var_9_0_i12);
	init_label(mercury__polymorphism__make_var_9_0_i17);
	init_label(mercury__polymorphism__make_var_9_0_i18);
BEGIN_CODE

/* code for predicate 'polymorphism__make_var'/9 in mode 0 */
Define_static(mercury__polymorphism__make_var_9_0);
	incr_sp_push_msg(6, "polymorphism__make_var");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	{
	Declare_entry(mercury__type_util__type_is_higher_order_3_0);
	call_localret(ENTRY(mercury__type_util__type_is_higher_order_3_0),
		mercury__polymorphism__make_var_9_0_i4,
		STATIC(mercury__polymorphism__make_var_9_0));
	}
Define_label(mercury__polymorphism__make_var_9_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_var_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__make_var_9_0_i3);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_9);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__polymorphism__construct_type_info_11_0),
		STATIC(mercury__polymorphism__make_var_9_0));
Define_label(mercury__polymorphism__make_var_9_0_i3);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__type_util__type_to_type_id_3_0);
	call_localret(ENTRY(mercury__type_util__type_to_type_id_3_0),
		mercury__polymorphism__make_var_9_0_i9,
		STATIC(mercury__polymorphism__make_var_9_0));
	}
Define_label(mercury__polymorphism__make_var_9_0_i9);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_var_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__make_var_9_0_i8);
	r1 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__polymorphism__construct_type_info_11_0),
		STATIC(mercury__polymorphism__make_var_9_0));
Define_label(mercury__polymorphism__make_var_9_0_i8);
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__polymorphism__make_var_9_0_i12);
	detstackvar(4) = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	detstackvar(5) = (Integer) r5;
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__polymorphism__make_var_9_0_i15,
		STATIC(mercury__polymorphism__make_var_9_0));
	}
Define_label(mercury__polymorphism__make_var_9_0_i15);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_var_9_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__make_var_9_0_i13);
	r1 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__polymorphism__make_var_9_0_i13);
	r1 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
Define_label(mercury__polymorphism__make_var_9_0_i12);
	r2 = string_const("type_info", 9);
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	call_localret(STATIC(mercury__polymorphism__new_type_info_var_7_0),
		mercury__polymorphism__make_var_9_0_i17,
		STATIC(mercury__polymorphism__make_var_9_0));
Define_label(mercury__polymorphism__make_var_9_0_i17);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_var_9_0));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	r2 = ((Integer) 0);
	call_localret(STATIC(mercury__polymorphism__init_with_int_constant_3_0),
		mercury__polymorphism__make_var_9_0_i18,
		STATIC(mercury__polymorphism__make_var_9_0));
Define_label(mercury__polymorphism__make_var_9_0_i18);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_var_9_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module14)
	init_entry(mercury__polymorphism__construct_type_info_11_0);
	init_label(mercury__polymorphism__construct_type_info_11_0_i2);
	init_label(mercury__polymorphism__construct_type_info_11_0_i3);
	init_label(mercury__polymorphism__construct_type_info_11_0_i4);
	init_label(mercury__polymorphism__construct_type_info_11_0_i7);
	init_label(mercury__polymorphism__construct_type_info_11_0_i8);
	init_label(mercury__polymorphism__construct_type_info_11_0_i9);
	init_label(mercury__polymorphism__construct_type_info_11_0_i10);
	init_label(mercury__polymorphism__construct_type_info_11_0_i11);
	init_label(mercury__polymorphism__construct_type_info_11_0_i12);
	init_label(mercury__polymorphism__construct_type_info_11_0_i13);
	init_label(mercury__polymorphism__construct_type_info_11_0_i6);
	init_label(mercury__polymorphism__construct_type_info_11_0_i15);
	init_label(mercury__polymorphism__construct_type_info_11_0_i16);
	init_label(mercury__polymorphism__construct_type_info_11_0_i17);
	init_label(mercury__polymorphism__construct_type_info_11_0_i18);
	init_label(mercury__polymorphism__construct_type_info_11_0_i19);
	init_label(mercury__polymorphism__construct_type_info_11_0_i14);
	init_label(mercury__polymorphism__construct_type_info_11_0_i20);
BEGIN_CODE

/* code for predicate 'polymorphism__construct_type_info'/11 in mode 0 */
Define_static(mercury__polymorphism__construct_type_info_11_0);
	incr_sp_push_msg(9, "polymorphism__construct_type_info");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	r3 = (Integer) r5;
	r4 = (Integer) r6;
	r5 = (Integer) r7;
	call_localret(STATIC(mercury__polymorphism__make_vars_9_0),
		mercury__polymorphism__construct_type_info_11_0_i2,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
Define_label(mercury__polymorphism__construct_type_info_11_0_i2);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	detstackvar(5) = (Integer) r1;
	detstackvar(6) = (Integer) r2;
	detstackvar(7) = (Integer) r3;
	detstackvar(8) = (Integer) r4;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__hlds_module__module_info_globals_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_globals_2_0),
		mercury__polymorphism__construct_type_info_11_0_i3,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
	}
Define_label(mercury__polymorphism__construct_type_info_11_0_i3);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	{
	Declare_entry(mercury__globals__get_type_info_method_2_0);
	call_localret(ENTRY(mercury__globals__get_type_info_method_2_0),
		mercury__polymorphism__construct_type_info_11_0_i4,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
	}
Define_label(mercury__polymorphism__construct_type_info_11_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__polymorphism__construct_type_info_11_0_i6);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__polymorphism__make_count_var_7_0),
		mercury__polymorphism__construct_type_info_11_0_i7,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
Define_label(mercury__polymorphism__construct_type_info_11_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	detstackvar(7) = (Integer) r3;
	detstackvar(8) = (Integer) r4;
	{
	Declare_entry(mercury__special_pred__special_pred_list_1_0);
	call_localret(ENTRY(mercury__special_pred__special_pred_list_1_0),
		mercury__polymorphism__construct_type_info_11_0_i8,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
	}
Define_label(mercury__polymorphism__construct_type_info_11_0_i8);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(7);
	r5 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__polymorphism__get_special_proc_list_2_9_0),
		mercury__polymorphism__construct_type_info_11_0_i9,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
Define_label(mercury__polymorphism__construct_type_info_11_0_i9);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	detstackvar(4) = (Integer) r3;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) tempr1;
	r3 = (Integer) detstackvar(5);
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	detstackvar(5) = (Integer) r4;
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__construct_type_info_11_0_i10,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
	}
	}
Define_label(mercury__polymorphism__construct_type_info_11_0_i10);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = string_const("type_info", 9);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__polymorphism__init_type_info_var_9_0),
		mercury__polymorphism__construct_type_info_11_0_i11,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
Define_label(mercury__polymorphism__construct_type_info_11_0_i11);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_7);
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r6;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r5;
	r3 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__construct_type_info_11_0_i12,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
	}
Define_label(mercury__polymorphism__construct_type_info_11_0_i12);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_7);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__construct_type_info_11_0_i13,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
	}
Define_label(mercury__polymorphism__construct_type_info_11_0_i13);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__polymorphism__construct_type_info_11_0_i6);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__polymorphism__construct_type_info_11_0_i14);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__polymorphism__make_count_var_7_0),
		mercury__polymorphism__construct_type_info_11_0_i15,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
Define_label(mercury__polymorphism__construct_type_info_11_0_i15);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__polymorphism__get_special_proc_list_8_0),
		mercury__polymorphism__construct_type_info_11_0_i16,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
Define_label(mercury__polymorphism__construct_type_info_11_0_i16);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(2) = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r5;
	r5 = (Integer) r4;
	r4 = (Integer) r3;
	r3 = string_const("base_type_info", 14);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r6;
	call_localret(STATIC(mercury__polymorphism__init_type_info_var_9_0),
		mercury__polymorphism__construct_type_info_11_0_i17,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
Define_label(mercury__polymorphism__construct_type_info_11_0_i17);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	r5 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r6 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_7);
	r8 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r5;
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r6;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r8;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__construct_type_info_11_0_i18,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
	}
Define_label(mercury__polymorphism__construct_type_info_11_0_i18);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	r7 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__polymorphism__maybe_init_second_cell_11_0),
		mercury__polymorphism__construct_type_info_11_0_i19,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
Define_label(mercury__polymorphism__construct_type_info_11_0_i19);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	{
	Word tempr1;
	tempr1 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) r4;
	r4 = (Integer) tempr1;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
	}
Define_label(mercury__polymorphism__construct_type_info_11_0_i14);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(7);
	r6 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__polymorphism__init_const_base_type_info_var_10_0),
		mercury__polymorphism__construct_type_info_11_0_i20,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
Define_label(mercury__polymorphism__construct_type_info_11_0_i20);
	update_prof_current_proc(LABEL(mercury__polymorphism__construct_type_info_11_0));
	r5 = (Integer) r3;
	r6 = (Integer) r4;
	r8 = (Integer) r2;
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(1);
	tag_incr_hp(r7, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r7, ((Integer) 0)) = (Integer) r8;
	field(mktag(1), (Integer) r7, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	call_localret(STATIC(mercury__polymorphism__maybe_init_second_cell_11_0),
		mercury__polymorphism__construct_type_info_11_0_i19,
		STATIC(mercury__polymorphism__construct_type_info_11_0));
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module15)
	init_entry(mercury__polymorphism__maybe_init_second_cell_11_0);
	init_label(mercury__polymorphism__maybe_init_second_cell_11_0_i1000);
	init_label(mercury__polymorphism__maybe_init_second_cell_11_0_i5);
	init_label(mercury__polymorphism__maybe_init_second_cell_11_0_i6);
	init_label(mercury__polymorphism__maybe_init_second_cell_11_0_i7);
BEGIN_CODE

/* code for predicate 'polymorphism__maybe_init_second_cell'/11 in mode 0 */
Define_static(mercury__polymorphism__maybe_init_second_cell_11_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__maybe_init_second_cell_11_0_i1000);
	r1 = (Integer) r4;
	r2 = (Integer) r5;
	r3 = (Integer) r6;
	r4 = (Integer) r7;
	proceed();
Define_label(mercury__polymorphism__maybe_init_second_cell_11_0_i1000);
	incr_sp_push_msg(5, "polymorphism__maybe_init_second_cell");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r4;
	r1 = (Integer) r3;
	r4 = (Integer) r5;
	r3 = string_const("type_info", 9);
	r5 = (Integer) r6;
	detstackvar(2) = (Integer) r7;
	call_localret(STATIC(mercury__polymorphism__init_type_info_var_9_0),
		mercury__polymorphism__maybe_init_second_cell_11_0_i5,
		STATIC(mercury__polymorphism__maybe_init_second_cell_11_0));
Define_label(mercury__polymorphism__maybe_init_second_cell_11_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__maybe_init_second_cell_11_0));
	r7 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_7);
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r7;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__maybe_init_second_cell_11_0_i6,
		STATIC(mercury__polymorphism__maybe_init_second_cell_11_0));
	}
Define_label(mercury__polymorphism__maybe_init_second_cell_11_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__maybe_init_second_cell_11_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_7);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__polymorphism__maybe_init_second_cell_11_0_i7,
		STATIC(mercury__polymorphism__maybe_init_second_cell_11_0));
	}
Define_label(mercury__polymorphism__maybe_init_second_cell_11_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__maybe_init_second_cell_11_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module16)
	init_entry(mercury__polymorphism__make_count_var_7_0);
	init_label(mercury__polymorphism__make_count_var_7_0_i2);
	init_label(mercury__polymorphism__make_count_var_7_0_i3);
	init_label(mercury__polymorphism__make_count_var_7_0_i4);
	init_label(mercury__polymorphism__make_count_var_7_0_i5);
	init_label(mercury__polymorphism__make_count_var_7_0_i6);
	init_label(mercury__polymorphism__make_count_var_7_0_i7);
BEGIN_CODE

/* code for predicate 'polymorphism__make_count_var'/7 in mode 0 */
Define_static(mercury__polymorphism__make_count_var_7_0);
	incr_sp_push_msg(5, "polymorphism__make_count_var");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__varset__new_var_3_0);
	call_localret(ENTRY(mercury__varset__new_var_3_0),
		mercury__polymorphism__make_count_var_7_0_i2,
		STATIC(mercury__polymorphism__make_count_var_7_0));
	}
Define_label(mercury__polymorphism__make_count_var_7_0_i2);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_count_var_7_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(3);
	r3 = string_const("TypeArity", 9);
	{
	Declare_entry(mercury__varset__name_var_4_0);
	call_localret(ENTRY(mercury__varset__name_var_4_0),
		mercury__polymorphism__make_count_var_7_0_i3,
		STATIC(mercury__polymorphism__make_count_var_7_0));
	}
Define_label(mercury__polymorphism__make_count_var_7_0_i3);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_count_var_7_0));
	detstackvar(4) = (Integer) r1;
	{
	Declare_entry(mercury__term__context_init_1_0);
	call_localret(ENTRY(mercury__term__context_init_1_0),
		mercury__polymorphism__make_count_var_7_0_i4,
		STATIC(mercury__polymorphism__make_count_var_7_0));
	}
Define_label(mercury__polymorphism__make_count_var_7_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_count_var_7_0));
	tag_incr_hp(r5, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r5, ((Integer) 2)) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_10);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__polymorphism__make_count_var_7_0_i5,
		STATIC(mercury__polymorphism__make_count_var_7_0));
	}
Define_label(mercury__polymorphism__make_count_var_7_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_count_var_7_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__polymorphism__make_count_var_7_0_i6,
		STATIC(mercury__polymorphism__make_count_var_7_0));
	}
Define_label(mercury__polymorphism__make_count_var_7_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_count_var_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__polymorphism__init_with_int_constant_3_0),
		mercury__polymorphism__make_count_var_7_0_i7,
		STATIC(mercury__polymorphism__make_count_var_7_0));
Define_label(mercury__polymorphism__make_count_var_7_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_count_var_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module17)
	init_entry(mercury__polymorphism__init_with_int_constant_3_0);
	init_label(mercury__polymorphism__init_with_int_constant_3_0_i2);
	init_label(mercury__polymorphism__init_with_int_constant_3_0_i3);
	init_label(mercury__polymorphism__init_with_int_constant_3_0_i4);
	init_label(mercury__polymorphism__init_with_int_constant_3_0_i5);
	init_label(mercury__polymorphism__init_with_int_constant_3_0_i6);
	init_label(mercury__polymorphism__init_with_int_constant_3_0_i7);
BEGIN_CODE

/* code for predicate 'polymorphism__init_with_int_constant'/3 in mode 0 */
Define_static(mercury__polymorphism__init_with_int_constant_3_0);
	tag_incr_hp(r3, mktag(3), ((Integer) 3));
	incr_sp_push_msg(5, "polymorphism__init_with_int_constant");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r3;
	field(mktag(3), (Integer) r3, ((Integer) 1)) = ((Integer) 1);
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 0);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	tag_incr_hp(r4, mktag(0), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(3), (Integer) detstackvar(2), ((Integer) 2)) = (Integer) r3;
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	r3 = (Integer) detstackvar(2);
	tag_incr_hp(tempr1, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	detstackvar(3) = (Integer) tempr1;
	tag_incr_hp(r6, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r6, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tag_incr_hp(tempr1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	field(mktag(3), (Integer) detstackvar(3), ((Integer) 2)) = (Integer) r6;
	field(mktag(1), (Integer) r6, ((Integer) 0)) = (Integer) tempr1;
	tag_incr_hp(r6, mktag(0), ((Integer) 2));
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r6, ((Integer) 0)) = (Integer) tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(3), (Integer) detstackvar(3), ((Integer) 3)) = (Integer) r6;
	field(mktag(0), (Integer) r6, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	tag_incr_hp(r6, mktag(0), ((Integer) 4));
	field(mktag(0), (Integer) r6, ((Integer) 0)) = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(0), (Integer) r6, ((Integer) 1)) = (Integer) r1;
	field(mktag(3), (Integer) detstackvar(3), ((Integer) 5)) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_11);
	field(mktag(3), (Integer) detstackvar(3), ((Integer) 4)) = (Integer) r6;
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(0), (Integer) r6, ((Integer) 3)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r6, ((Integer) 2)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__hlds_goal__goal_info_init_1_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_init_1_0),
		mercury__polymorphism__init_with_int_constant_3_0_i2,
		STATIC(mercury__polymorphism__init_with_int_constant_3_0));
	}
	}
Define_label(mercury__polymorphism__init_with_int_constant_3_0_i2);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_with_int_constant_3_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__singleton_set_2_1);
	call_localret(ENTRY(mercury__set__singleton_set_2_1),
		mercury__polymorphism__init_with_int_constant_3_0_i3,
		STATIC(mercury__polymorphism__init_with_int_constant_3_0));
	}
Define_label(mercury__polymorphism__init_with_int_constant_3_0_i3);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_with_int_constant_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_nonlocals_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_nonlocals_3_0),
		mercury__polymorphism__init_with_int_constant_3_0_i4,
		STATIC(mercury__polymorphism__init_with_int_constant_3_0));
	}
Define_label(mercury__polymorphism__init_with_int_constant_3_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_with_int_constant_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	{
	Declare_entry(mercury__instmap__instmap_delta_from_assoc_list_2_0);
	call_localret(ENTRY(mercury__instmap__instmap_delta_from_assoc_list_2_0),
		mercury__polymorphism__init_with_int_constant_3_0_i5,
		STATIC(mercury__polymorphism__init_with_int_constant_3_0));
	}
Define_label(mercury__polymorphism__init_with_int_constant_3_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_with_int_constant_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_instmap_delta_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_instmap_delta_3_0),
		mercury__polymorphism__init_with_int_constant_3_0_i6,
		STATIC(mercury__polymorphism__init_with_int_constant_3_0));
	}
Define_label(mercury__polymorphism__init_with_int_constant_3_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_with_int_constant_3_0));
	r2 = ((Integer) 0);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_determinism_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_determinism_3_0),
		mercury__polymorphism__init_with_int_constant_3_0_i7,
		STATIC(mercury__polymorphism__init_with_int_constant_3_0));
	}
Define_label(mercury__polymorphism__init_with_int_constant_3_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_with_int_constant_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module18)
	init_entry(mercury__polymorphism__get_special_proc_list_8_0);
	init_label(mercury__polymorphism__get_special_proc_list_8_0_i2);
BEGIN_CODE

/* code for predicate 'polymorphism__get_special_proc_list'/8 in mode 0 */
Define_static(mercury__polymorphism__get_special_proc_list_8_0);
	incr_sp_push_msg(5, "polymorphism__get_special_proc_list");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	{
	Declare_entry(mercury__special_pred__special_pred_list_1_0);
	call_localret(ENTRY(mercury__special_pred__special_pred_list_1_0),
		mercury__polymorphism__get_special_proc_list_8_0_i2,
		STATIC(mercury__polymorphism__get_special_proc_list_8_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_8_0_i2);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_8_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__polymorphism__get_special_proc_list_2_9_0),
		STATIC(mercury__polymorphism__get_special_proc_list_8_0));
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module19)
	init_entry(mercury__polymorphism__get_special_proc_list_2_9_0);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i4);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i5);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i6);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i7);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i8);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i9);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i10);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i1022);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i12);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i13);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i14);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i15);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i16);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i17);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i18);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i19);
	init_label(mercury__polymorphism__get_special_proc_list_2_9_0_i1020);
BEGIN_CODE

/* code for predicate 'polymorphism__get_special_proc_list_2'/9 in mode 0 */
Define_static(mercury__polymorphism__get_special_proc_list_2_9_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__get_special_proc_list_2_9_0_i1020);
	incr_sp_push_msg(10, "polymorphism__get_special_proc_list_2");
	detstackvar(10) = (Integer) succip;
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(5) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	{
	Declare_entry(mercury__special_pred__special_pred_info_6_0);
	call_localret(ENTRY(mercury__special_pred__special_pred_info_6_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i4,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r3;
	detstackvar(7) = (Integer) r2;
	{
	Declare_entry(mercury__varset__new_var_3_0);
	call_localret(ENTRY(mercury__varset__new_var_3_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i5,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	detstackvar(8) = (Integer) r2;
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	r1 = string_const("Var__", 5);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__polymorphism__get_special_proc_list_2_9_0_i6,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(8);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__varset__name_var_4_0);
	call_localret(ENTRY(mercury__varset__name_var_4_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i7,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	detstackvar(8) = (Integer) r1;
	{
	Declare_entry(mercury__term__context_init_1_0);
	call_localret(ENTRY(mercury__term__context_init_1_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i8,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i8);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	tag_incr_hp(r5, mktag(0), ((Integer) 3));
	field(mktag(0), (Integer) r5, ((Integer) 2)) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r5, ((Integer) 1)) = (Integer) detstackvar(7);
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_8);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__polymorphism__get_special_proc_list_2_9_0_i9,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i9);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__type_util__classify_type_3_0);
	call_localret(ENTRY(mercury__type_util__classify_type_3_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i10,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i10);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__polymorphism__get_special_proc_6_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i1022,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i1022);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	{
	Word tempr1, tempr2;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 4));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(3);
	detstackvar(5) = (Integer) tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) r3;
	tempr2 = (Integer) detstackvar(5);
	field(mktag(0), (Integer) tempr2, ((Integer) 2)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) tempr2, ((Integer) 3)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) tempr2, ((Integer) 1)) = (Integer) tempr1;
	tag_incr_hp(detstackvar(7), mktag(1), ((Integer) 2));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(1), (Integer) detstackvar(7), ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) detstackvar(7), ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r2, ((Integer) 1)) = ((Integer) 0);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	{
	Declare_entry(mercury__prog_util__unqualify_name_2_0);
	call_localret(ENTRY(mercury__prog_util__unqualify_name_2_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i12,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i12);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	r2 = (Integer) detstackvar(5);
	tag_incr_hp(r3, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r3, ((Integer) 1)) = ((Integer) 1);
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 0);
	detstackvar(5) = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	tag_incr_hp(r4, mktag(0), ((Integer) 2));
	tag_incr_hp(r5, mktag(0), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r5, ((Integer) 1)) = ((Integer) 0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r1;
	field(mktag(3), (Integer) detstackvar(5), ((Integer) 2)) = (Integer) r3;
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r4;
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) r5;
	field(mktag(0), (Integer) r5, ((Integer) 0)) = (Integer) tempr1;
	r4 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(7);
	tag_incr_hp(r1, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) r3;
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	detstackvar(7) = (Integer) r1;
	tag_incr_hp(r7, mktag(0), ((Integer) 2));
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r4;
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r7, ((Integer) 0)) = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r7, ((Integer) 1)) = (Integer) r1;
	r3 = (Integer) detstackvar(7);
	field(mktag(3), (Integer) r3, ((Integer) 5)) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_11);
	field(mktag(3), (Integer) r3, ((Integer) 4)) = (Integer) r2;
	field(mktag(3), (Integer) r3, ((Integer) 3)) = (Integer) r7;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r4;
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r4;
	{
	Declare_entry(mercury__hlds_goal__goal_info_init_1_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_init_1_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i13,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i13);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	detstackvar(9) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__singleton_set_2_1);
	call_localret(ENTRY(mercury__set__singleton_set_2_1),
		mercury__polymorphism__get_special_proc_list_2_9_0_i14,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i14);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_nonlocals_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_nonlocals_3_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i15,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i15);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	r2 = (Integer) detstackvar(5);
	detstackvar(5) = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) r2;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__instmap__instmap_delta_from_assoc_list_2_0);
	call_localret(ENTRY(mercury__instmap__instmap_delta_from_assoc_list_2_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i16,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i16);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_instmap_delta_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_instmap_delta_3_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i17,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i17);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	r2 = ((Integer) 0);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_determinism_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_determinism_3_0),
		mercury__polymorphism__get_special_proc_list_2_9_0_i18,
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i18);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	r2 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	detstackvar(1) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(8);
	r5 = (Integer) detstackvar(4);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(7);
	localcall(mercury__polymorphism__get_special_proc_list_2_9_0,
		LABEL(mercury__polymorphism__get_special_proc_list_2_9_0_i19),
		STATIC(mercury__polymorphism__get_special_proc_list_2_9_0));
	}
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i19);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_list_2_9_0));
	r5 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r5;
	r6 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r6;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__polymorphism__get_special_proc_list_2_9_0_i1020);
	r3 = (Integer) r4;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) r5;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module20)
	init_entry(mercury__polymorphism__get_special_proc_6_0);
	init_label(mercury__polymorphism__get_special_proc_6_0_i5);
	init_label(mercury__polymorphism__get_special_proc_6_0_i8);
	init_label(mercury__polymorphism__get_special_proc_6_0_i10);
	init_label(mercury__polymorphism__get_special_proc_6_0_i7);
	init_label(mercury__polymorphism__get_special_proc_6_0_i11);
	init_label(mercury__polymorphism__get_special_proc_6_0_i12);
	init_label(mercury__polymorphism__get_special_proc_6_0_i13);
	init_label(mercury__polymorphism__get_special_proc_6_0_i14);
	init_label(mercury__polymorphism__get_special_proc_6_0_i15);
	init_label(mercury__polymorphism__get_special_proc_6_0_i2);
	init_label(mercury__polymorphism__get_special_proc_6_0_i19);
	init_label(mercury__polymorphism__get_special_proc_6_0_i21);
	init_label(mercury__polymorphism__get_special_proc_6_0_i22);
	init_label(mercury__polymorphism__get_special_proc_6_0_i23);
	init_label(mercury__polymorphism__get_special_proc_6_0_i25);
	init_label(mercury__polymorphism__get_special_proc_6_0_i26);
	init_label(mercury__polymorphism__get_special_proc_6_0_i18);
	init_label(mercury__polymorphism__get_special_proc_6_0_i27);
	init_label(mercury__polymorphism__get_special_proc_6_0_i16);
	init_label(mercury__polymorphism__get_special_proc_6_0_i28);
	init_label(mercury__polymorphism__get_special_proc_6_0_i29);
	init_label(mercury__polymorphism__get_special_proc_6_0_i30);
	init_label(mercury__polymorphism__get_special_proc_6_0_i33);
	init_label(mercury__polymorphism__get_special_proc_6_0_i36);
	init_label(mercury__polymorphism__get_special_proc_6_0_i32);
	init_label(mercury__polymorphism__get_special_proc_6_0_i38);
	init_label(mercury__polymorphism__get_special_proc_6_0_i40);
	init_label(mercury__polymorphism__get_special_proc_6_0_i41);
BEGIN_CODE

/* code for predicate 'polymorphism__get_special_proc'/6 in mode 0 */
Define_static(mercury__polymorphism__get_special_proc_6_0);
	incr_sp_push_msg(5, "polymorphism__get_special_proc");
	detstackvar(5) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i2);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__hlds_module__module_info_get_special_pred_map_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_get_special_pred_map_2_0),
		mercury__polymorphism__get_special_proc_6_0_i5,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__type_util__type_to_type_id_3_0);
	call_localret(ENTRY(mercury__type_util__type_to_type_id_3_0),
		mercury__polymorphism__get_special_proc_6_0_i8,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i8);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i7);
	tag_incr_hp(r4, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r4, ((Integer) 1)) = (Integer) r2;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_5);
	r2 = (Integer) mercury_data___base_type_info_int_0;
	r3 = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r4, ((Integer) 0)) = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__polymorphism__get_special_proc_6_0_i10,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i10);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r3 = (Integer) detstackvar(1);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i12);
Define_label(mercury__polymorphism__get_special_proc_6_0_i7);
	r1 = string_const("polymorphism__get_special_proc: type_to_type_id failed", 54);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__polymorphism__get_special_proc_6_0_i11,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i11);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r3 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(4);
Define_label(mercury__polymorphism__get_special_proc_6_0_i12);
	detstackvar(1) = (Integer) r3;
	detstackvar(4) = (Integer) r2;
	{
	Declare_entry(mercury__hlds_module__module_info_pred_info_3_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_pred_info_3_0),
		mercury__polymorphism__get_special_proc_6_0_i13,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i13);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_pred__pred_info_module_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_module_2_0),
		mercury__polymorphism__get_special_proc_6_0_i14,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i14);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_pred__pred_info_name_2_0);
	call_localret(ENTRY(mercury__hlds_pred__pred_info_name_2_0),
		mercury__polymorphism__get_special_proc_6_0_i15,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i15);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	r3 = (Integer) detstackvar(4);
	GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i40);
Define_label(mercury__polymorphism__get_special_proc_6_0_i2);
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i18);
	COMPUTED_GOTO(unmkbody((Integer) r1),
		LABEL(mercury__polymorphism__get_special_proc_6_0_i19) AND
		LABEL(mercury__polymorphism__get_special_proc_6_0_i19) AND
		LABEL(mercury__polymorphism__get_special_proc_6_0_i21) AND
		LABEL(mercury__polymorphism__get_special_proc_6_0_i22) AND
		LABEL(mercury__polymorphism__get_special_proc_6_0_i23) AND
		LABEL(mercury__polymorphism__get_special_proc_6_0_i19) AND
		LABEL(mercury__polymorphism__get_special_proc_6_0_i25));
Define_label(mercury__polymorphism__get_special_proc_6_0_i19);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = string_const("int", 3);
	GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i16);
Define_label(mercury__polymorphism__get_special_proc_6_0_i21);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = string_const("string", 6);
	GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i16);
Define_label(mercury__polymorphism__get_special_proc_6_0_i22);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = string_const("float", 5);
	GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i16);
Define_label(mercury__polymorphism__get_special_proc_6_0_i23);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = string_const("pred", 4);
	GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i16);
Define_label(mercury__polymorphism__get_special_proc_6_0_i25);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	r1 = string_const("polymorphism__get_category_name: polymorphic type", 49);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__polymorphism__get_special_proc_6_0_i26,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i26);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i16);
Define_label(mercury__polymorphism__get_special_proc_6_0_i18);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	r1 = string_const("polymorphism__get_category_name: user_type", 42);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__polymorphism__get_special_proc_6_0_i27,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i27);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
Define_label(mercury__polymorphism__get_special_proc_6_0_i16);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	{
	Declare_entry(mercury__special_pred__special_pred_name_arity_4_0);
	call_localret(ENTRY(mercury__special_pred__special_pred_name_arity_4_0),
		mercury__polymorphism__get_special_proc_6_0_i28,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i28);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r4 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r3;
	r5 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("builtin_", 8);
	tag_incr_hp(r6, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r6, ((Integer) 0)) = (Integer) r5;
	tag_incr_hp(r7, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r7, ((Integer) 0)) = string_const("_", 1);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r7, ((Integer) 1)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r6;
	field(mktag(1), (Integer) r6, ((Integer) 1)) = (Integer) r7;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r4;
	{
	Declare_entry(mercury__string__append_list_2_0);
	call_localret(ENTRY(mercury__string__append_list_2_0),
		mercury__polymorphism__get_special_proc_6_0_i29,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i29);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_module__module_info_get_predicate_table_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_get_predicate_table_2_0),
		mercury__polymorphism__get_special_proc_6_0_i30,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i30);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r2 = string_const("mercury_builtin", 15);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__hlds_module__predicate_table_search_pred_m_n_a_5_0);
	call_localret(ENTRY(mercury__hlds_module__predicate_table_search_pred_m_n_a_5_0),
		mercury__polymorphism__get_special_proc_6_0_i33,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i33);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i32);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i32);
	r3 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		mercury__polymorphism__get_special_proc_6_0_i36,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i36);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i32);
	r1 = (Integer) detstackvar(1);
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	GOTO_LABEL(mercury__polymorphism__get_special_proc_6_0_i40);
Define_label(mercury__polymorphism__get_special_proc_6_0_i32);
	r1 = string_const("polymorphism__get_pred_id: pred_id lookup failed", 48);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__polymorphism__get_special_proc_6_0_i38,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i38);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
Define_label(mercury__polymorphism__get_special_proc_6_0_i40);
	detstackvar(3) = (Integer) r2;
	detstackvar(4) = (Integer) r3;
	{
	Declare_entry(mercury__special_pred__special_pred_mode_num_2_0);
	call_localret(ENTRY(mercury__special_pred__special_pred_mode_num_2_0),
		mercury__polymorphism__get_special_proc_6_0_i41,
		STATIC(mercury__polymorphism__get_special_proc_6_0));
	}
Define_label(mercury__polymorphism__get_special_proc_6_0_i41);
	update_prof_current_proc(LABEL(mercury__polymorphism__get_special_proc_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module21)
	init_entry(mercury__polymorphism__init_type_info_var_9_0);
	init_label(mercury__polymorphism__init_type_info_var_9_0_i2);
	init_label(mercury__polymorphism__init_type_info_var_9_0_i3);
	init_label(mercury__polymorphism__init_type_info_var_9_0_i4);
	init_label(mercury__polymorphism__init_type_info_var_9_0_i5);
	init_label(mercury__polymorphism__init_type_info_var_9_0_i6);
	init_label(mercury__polymorphism__init_type_info_var_9_0_i7);
	init_label(mercury__polymorphism__init_type_info_var_9_0_i8);
	init_label(mercury__polymorphism__init_type_info_var_9_0_i9);
	init_label(mercury__polymorphism__init_type_info_var_9_0_i10);
	init_label(mercury__polymorphism__init_type_info_var_9_0_i11);
BEGIN_CODE

/* code for predicate 'polymorphism__init_type_info_var'/9 in mode 0 */
Define_static(mercury__polymorphism__init_type_info_var_9_0);
	incr_sp_push_msg(9, "polymorphism__init_type_info_var");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	tag_incr_hp(r6, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r6, ((Integer) 0)) = (Integer) r3;
	tag_incr_hp(detstackvar(5), mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) detstackvar(5), ((Integer) 0)) = (Integer) r6;
	field(mktag(0), (Integer) detstackvar(5), ((Integer) 1)) = ((Integer) 1);
	r7 = (Integer) detstackvar(5);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	detstackvar(6) = (Integer) tempr1;
	r4 = (Integer) r5;
	detstackvar(8) = (Integer) r6;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r7;
	call_localret(STATIC(mercury__polymorphism__new_type_info_var_7_0),
		mercury__polymorphism__init_type_info_var_9_0_i2,
		STATIC(mercury__polymorphism__init_type_info_var_9_0));
	}
Define_label(mercury__polymorphism__init_type_info_var_9_0_i2);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_type_info_var_9_0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	detstackvar(4) = (Integer) r3;
	detstackvar(7) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_15);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__polymorphism__init_type_info_var_9_0_i3,
		STATIC(mercury__polymorphism__init_type_info_var_9_0));
	}
Define_label(mercury__polymorphism__init_type_info_var_9_0_i3);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_type_info_var_9_0));
	r2 = (Integer) r1;
	detstackvar(7) = (Integer) r1;
	{
	extern Word * mercury_data_hlds_goal__base_type_info_uni_mode_0[];
	r1 = (Integer) mercury_data_hlds_goal__base_type_info_uni_mode_0;
	}
	r3 = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_15);
	{
	Declare_entry(mercury__list__duplicate_3_0);
	call_localret(ENTRY(mercury__list__duplicate_3_0),
		mercury__polymorphism__init_type_info_var_9_0_i4,
		STATIC(mercury__polymorphism__init_type_info_var_9_0));
	}
Define_label(mercury__polymorphism__init_type_info_var_9_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_type_info_var_9_0));
	r2 = (Integer) detstackvar(5);
	tag_incr_hp(r3, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) r3, ((Integer) 3)) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_15);
	field(mktag(3), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(6);
	field(mktag(3), (Integer) r3, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	detstackvar(5) = (Integer) r3;
	tag_incr_hp(r3, mktag(0), ((Integer) 4));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) r2;
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r3, ((Integer) 3)) = (Integer) r1;
	field(mktag(3), (Integer) detstackvar(5), ((Integer) 4)) = (Integer) r3;
	field(mktag(3), (Integer) detstackvar(5), ((Integer) 5)) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_11);
	{
	Declare_entry(mercury__hlds_goal__goal_info_init_1_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_init_1_0),
		mercury__polymorphism__init_type_info_var_9_0_i5,
		STATIC(mercury__polymorphism__init_type_info_var_9_0));
	}
Define_label(mercury__polymorphism__init_type_info_var_9_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_type_info_var_9_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r4 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r4;
	{
	Declare_entry(mercury__set__list_to_set_2_0);
	call_localret(ENTRY(mercury__set__list_to_set_2_0),
		mercury__polymorphism__init_type_info_var_9_0_i6,
		STATIC(mercury__polymorphism__init_type_info_var_9_0));
	}
Define_label(mercury__polymorphism__init_type_info_var_9_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_type_info_var_9_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_nonlocals_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_nonlocals_3_0),
		mercury__polymorphism__init_type_info_var_9_0_i7,
		STATIC(mercury__polymorphism__init_type_info_var_9_0));
	}
Define_label(mercury__polymorphism__init_type_info_var_9_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_type_info_var_9_0));
	detstackvar(1) = (Integer) r1;
	{
	extern Word * mercury_data_prog_data__base_type_info_inst_0[];
	r1 = (Integer) mercury_data_prog_data__base_type_info_inst_0;
	}
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) mkword(mktag(3), (Integer) mercury_data_polymorphism__common_12);
	{
	Declare_entry(mercury__list__duplicate_3_0);
	call_localret(ENTRY(mercury__list__duplicate_3_0),
		mercury__polymorphism__init_type_info_var_9_0_i8,
		STATIC(mercury__polymorphism__init_type_info_var_9_0));
	}
Define_label(mercury__polymorphism__init_type_info_var_9_0_i8);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_type_info_var_9_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	tag_incr_hp(r4, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r4, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r4, ((Integer) 1)) = ((Integer) 1);
	tag_incr_hp(r5, mktag(1), ((Integer) 2));
	tag_incr_hp(r6, mktag(0), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r6, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) r4, ((Integer) 2)) = (Integer) r5;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r5, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r5, ((Integer) 0)) = (Integer) r6;
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) r4;
	field(mktag(0), (Integer) r6, ((Integer) 1)) = (Integer) r2;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(7);
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__instmap__instmap_delta_from_assoc_list_2_0);
	call_localret(ENTRY(mercury__instmap__instmap_delta_from_assoc_list_2_0),
		mercury__polymorphism__init_type_info_var_9_0_i9,
		STATIC(mercury__polymorphism__init_type_info_var_9_0));
	}
	}
Define_label(mercury__polymorphism__init_type_info_var_9_0_i9);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_type_info_var_9_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_instmap_delta_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_instmap_delta_3_0),
		mercury__polymorphism__init_type_info_var_9_0_i10,
		STATIC(mercury__polymorphism__init_type_info_var_9_0));
	}
Define_label(mercury__polymorphism__init_type_info_var_9_0_i10);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_type_info_var_9_0));
	r2 = ((Integer) 0);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_determinism_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_determinism_3_0),
		mercury__polymorphism__init_type_info_var_9_0_i11,
		STATIC(mercury__polymorphism__init_type_info_var_9_0));
	}
Define_label(mercury__polymorphism__init_type_info_var_9_0_i11);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_type_info_var_9_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module22)
	init_entry(mercury__polymorphism__init_const_base_type_info_var_10_0);
	init_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i2);
	init_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i3);
	init_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i4);
	init_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i5);
	init_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i6);
	init_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i7);
	init_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i8);
	init_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i9);
	init_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i10);
	init_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i11);
BEGIN_CODE

/* code for predicate 'polymorphism__init_const_base_type_info_var'/10 in mode 0 */
Define_static(mercury__polymorphism__init_const_base_type_info_var_10_0);
	incr_sp_push_msg(7, "polymorphism__init_const_base_type_info_var");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(6) = (Integer) r6;
	r1 = (Integer) r4;
	{
	Declare_entry(mercury__type_util__type_id_module_3_0);
	call_localret(ENTRY(mercury__type_util__type_id_module_3_0),
		mercury__polymorphism__init_const_base_type_info_var_10_0_i2,
		STATIC(mercury__polymorphism__init_const_base_type_info_var_10_0));
	}
Define_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i2);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_const_base_type_info_var_10_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__type_util__type_id_name_3_0);
	call_localret(ENTRY(mercury__type_util__type_id_name_3_0),
		mercury__polymorphism__init_const_base_type_info_var_10_0_i3,
		STATIC(mercury__polymorphism__init_const_base_type_info_var_10_0));
	}
Define_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i3);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_const_base_type_info_var_10_0));
	r2 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	{
	Declare_entry(mercury__list__length_2_0);
	call_localret(ENTRY(mercury__list__length_2_0),
		mercury__polymorphism__init_const_base_type_info_var_10_0_i4,
		STATIC(mercury__polymorphism__init_const_base_type_info_var_10_0));
	}
Define_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_const_base_type_info_var_10_0));
	r2 = (Integer) detstackvar(1);
	tag_incr_hp(r4, mktag(3), ((Integer) 4));
	field(mktag(3), (Integer) r4, ((Integer) 2)) = (Integer) detstackvar(3);
	field(mktag(3), (Integer) r4, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r4, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r4, ((Integer) 3)) = (Integer) r1;
	detstackvar(1) = (Integer) r4;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	r1 = (Integer) r2;
	detstackvar(2) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r4;
	r2 = string_const("base_type_info", 14);
	r3 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(6);
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	call_localret(STATIC(mercury__polymorphism__new_type_info_var_7_0),
		mercury__polymorphism__init_const_base_type_info_var_10_0_i5,
		STATIC(mercury__polymorphism__init_const_base_type_info_var_10_0));
	}
Define_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_const_base_type_info_var_10_0));
	r4 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r5 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	tag_incr_hp(r2, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) r2, ((Integer) 3)) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_15);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) r5;
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	detstackvar(4) = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 4));
	field(mktag(3), (Integer) detstackvar(4), ((Integer) 4)) = (Integer) r2;
	field(mktag(3), (Integer) detstackvar(4), ((Integer) 5)) = (Integer) mkword(mktag(0), (Integer) mercury_data_polymorphism__common_11);
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r4;
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	{
	Declare_entry(mercury__hlds_goal__goal_info_init_1_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_init_1_0),
		mercury__polymorphism__init_const_base_type_info_var_10_0_i6,
		STATIC(mercury__polymorphism__init_const_base_type_info_var_10_0));
	}
Define_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_const_base_type_info_var_10_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__set__list_to_set_2_0);
	call_localret(ENTRY(mercury__set__list_to_set_2_0),
		mercury__polymorphism__init_const_base_type_info_var_10_0_i7,
		STATIC(mercury__polymorphism__init_const_base_type_info_var_10_0));
	}
Define_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_const_base_type_info_var_10_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_nonlocals_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_nonlocals_3_0),
		mercury__polymorphism__init_const_base_type_info_var_10_0_i8,
		STATIC(mercury__polymorphism__init_const_base_type_info_var_10_0));
	}
Define_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i8);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_const_base_type_info_var_10_0));
	detstackvar(5) = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(3), (Integer) mercury_data_polymorphism__common_12);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__instmap__instmap_delta_from_assoc_list_2_0);
	call_localret(ENTRY(mercury__instmap__instmap_delta_from_assoc_list_2_0),
		mercury__polymorphism__init_const_base_type_info_var_10_0_i9,
		STATIC(mercury__polymorphism__init_const_base_type_info_var_10_0));
	}
Define_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i9);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_const_base_type_info_var_10_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_instmap_delta_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_instmap_delta_3_0),
		mercury__polymorphism__init_const_base_type_info_var_10_0_i10,
		STATIC(mercury__polymorphism__init_const_base_type_info_var_10_0));
	}
Define_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i10);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_const_base_type_info_var_10_0));
	r2 = ((Integer) 0);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_determinism_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_determinism_3_0),
		mercury__polymorphism__init_const_base_type_info_var_10_0_i11,
		STATIC(mercury__polymorphism__init_const_base_type_info_var_10_0));
	}
Define_label(mercury__polymorphism__init_const_base_type_info_var_10_0_i11);
	update_prof_current_proc(LABEL(mercury__polymorphism__init_const_base_type_info_var_10_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module23)
	init_entry(mercury__polymorphism__make_head_vars_7_0);
	init_label(mercury__polymorphism__make_head_vars_7_0_i4);
	init_label(mercury__polymorphism__make_head_vars_7_0_i7);
	init_label(mercury__polymorphism__make_head_vars_7_0_i9);
	init_label(mercury__polymorphism__make_head_vars_7_0_i10);
	init_label(mercury__polymorphism__make_head_vars_7_0_i6);
	init_label(mercury__polymorphism__make_head_vars_7_0_i11);
	init_label(mercury__polymorphism__make_head_vars_7_0_i12);
	init_label(mercury__polymorphism__make_head_vars_7_0_i1004);
BEGIN_CODE

/* code for predicate 'polymorphism__make_head_vars'/7 in mode 0 */
Define_static(mercury__polymorphism__make_head_vars_7_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__polymorphism__make_head_vars_7_0_i1004);
	incr_sp_push_msg(6, "polymorphism__make_head_vars");
	detstackvar(6) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) tempr1;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	detstackvar(1) = (Integer) r2;
	r2 = string_const("type_info", 9);
	call_localret(STATIC(mercury__polymorphism__new_type_info_var_7_0),
		mercury__polymorphism__make_head_vars_7_0_i4,
		STATIC(mercury__polymorphism__make_head_vars_7_0));
	}
Define_label(mercury__polymorphism__make_head_vars_7_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_head_vars_7_0));
	detstackvar(4) = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(5) = (Integer) r3;
	{
	Declare_entry(mercury__varset__search_name_3_0);
	call_localret(ENTRY(mercury__varset__search_name_3_0),
		mercury__polymorphism__make_head_vars_7_0_i7,
		STATIC(mercury__polymorphism__make_head_vars_7_0));
	}
Define_label(mercury__polymorphism__make_head_vars_7_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_head_vars_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__polymorphism__make_head_vars_7_0_i6);
	r1 = string_const("TypeInfo_for_", 13);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__polymorphism__make_head_vars_7_0_i9,
		STATIC(mercury__polymorphism__make_head_vars_7_0));
	}
Define_label(mercury__polymorphism__make_head_vars_7_0_i9);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_head_vars_7_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__varset__name_var_4_0);
	call_localret(ENTRY(mercury__varset__name_var_4_0),
		mercury__polymorphism__make_head_vars_7_0_i10,
		STATIC(mercury__polymorphism__make_head_vars_7_0));
	}
Define_label(mercury__polymorphism__make_head_vars_7_0_i10);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_head_vars_7_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(5);
	GOTO_LABEL(mercury__polymorphism__make_head_vars_7_0_i11);
Define_label(mercury__polymorphism__make_head_vars_7_0_i6);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(2);
Define_label(mercury__polymorphism__make_head_vars_7_0_i11);
	detstackvar(2) = (Integer) r5;
	localcall(mercury__polymorphism__make_head_vars_7_0,
		LABEL(mercury__polymorphism__make_head_vars_7_0_i12),
		STATIC(mercury__polymorphism__make_head_vars_7_0));
Define_label(mercury__polymorphism__make_head_vars_7_0_i12);
	update_prof_current_proc(LABEL(mercury__polymorphism__make_head_vars_7_0));
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__polymorphism__make_head_vars_7_0_i1004);
	r2 = (Integer) r3;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__polymorphism_module24)
	init_entry(mercury__polymorphism__new_type_info_var_7_0);
	init_label(mercury__polymorphism__new_type_info_var_7_0_i2);
	init_label(mercury__polymorphism__new_type_info_var_7_0_i3);
	init_label(mercury__polymorphism__new_type_info_var_7_0_i4);
	init_label(mercury__polymorphism__new_type_info_var_7_0_i5);
	init_label(mercury__polymorphism__new_type_info_var_7_0_i6);
	init_label(mercury__polymorphism__new_type_info_var_7_0_i7);
	init_label(mercury__polymorphism__new_type_info_var_7_0_i8);
BEGIN_CODE

/* code for predicate 'polymorphism__new_type_info_var'/7 in mode 0 */
Define_static(mercury__polymorphism__new_type_info_var_7_0);
	incr_sp_push_msg(6, "polymorphism__new_type_info_var");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__varset__new_var_3_0);
	call_localret(ENTRY(mercury__varset__new_var_3_0),
		mercury__polymorphism__new_type_info_var_7_0_i2,
		STATIC(mercury__polymorphism__new_type_info_var_7_0));
	}
Define_label(mercury__polymorphism__new_type_info_var_7_0_i2);
	update_prof_current_proc(LABEL(mercury__polymorphism__new_type_info_var_7_0));
	detstackvar(4) = (Integer) r1;
	detstackvar(5) = (Integer) r2;
	{
	Declare_entry(mercury__term__var_to_int_2_0);
	call_localret(ENTRY(mercury__term__var_to_int_2_0),
		mercury__polymorphism__new_type_info_var_7_0_i3,
		STATIC(mercury__polymorphism__new_type_info_var_7_0));
	}
Define_label(mercury__polymorphism__new_type_info_var_7_0_i3);
	update_prof_current_proc(LABEL(mercury__polymorphism__new_type_info_var_7_0));
	{
	Declare_entry(mercury__string__int_to_string_2_0);
	call_localret(ENTRY(mercury__string__int_to_string_2_0),
		mercury__polymorphism__new_type_info_var_7_0_i4,
		STATIC(mercury__polymorphism__new_type_info_var_7_0));
	}
Define_label(mercury__polymorphism__new_type_info_var_7_0_i4);
	update_prof_current_proc(LABEL(mercury__polymorphism__new_type_info_var_7_0));
	r2 = (Integer) r1;
	r1 = string_const("TypeInfo_", 9);
	{
	Declare_entry(mercury__string__append_3_2);
	call_localret(ENTRY(mercury__string__append_3_2),
		mercury__polymorphism__new_type_info_var_7_0_i5,
		STATIC(mercury__polymorphism__new_type_info_var_7_0));
	}
Define_label(mercury__polymorphism__new_type_info_var_7_0_i5);
	update_prof_current_proc(LABEL(mercury__polymorphism__new_type_info_var_7_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__varset__name_var_4_0);
	call_localret(ENTRY(mercury__varset__name_var_4_0),
		mercury__polymorphism__new_type_info_var_7_0_i6,
		STATIC(mercury__polymorphism__new_type_info_var_7_0));
	}
Define_label(mercury__polymorphism__new_type_info_var_7_0_i6);
	update_prof_current_proc(LABEL(mercury__polymorphism__new_type_info_var_7_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	r4 = (Integer) r2;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r3, ((Integer) 0)) = string_const("mercury_builtin", 15);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = ((Integer) 1);
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r4;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__type_util__construct_type_3_0);
	call_localret(ENTRY(mercury__type_util__construct_type_3_0),
		mercury__polymorphism__new_type_info_var_7_0_i7,
		STATIC(mercury__polymorphism__new_type_info_var_7_0));
	}
Define_label(mercury__polymorphism__new_type_info_var_7_0_i7);
	update_prof_current_proc(LABEL(mercury__polymorphism__new_type_info_var_7_0));
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__map__set_4_1);
	call_localret(ENTRY(mercury__map__set_4_1),
		mercury__polymorphism__new_type_info_var_7_0_i8,
		STATIC(mercury__polymorphism__new_type_info_var_7_0));
	}
Define_label(mercury__polymorphism__new_type_info_var_7_0_i8);
	update_prof_current_proc(LABEL(mercury__polymorphism__new_type_info_var_7_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__polymorphism_bunch_0(void)
{
	mercury__polymorphism_module0();
	mercury__polymorphism_module1();
	mercury__polymorphism_module2();
	mercury__polymorphism_module3();
	mercury__polymorphism_module4();
	mercury__polymorphism_module5();
	mercury__polymorphism_module6();
	mercury__polymorphism_module7();
	mercury__polymorphism_module8();
	mercury__polymorphism_module9();
	mercury__polymorphism_module10();
	mercury__polymorphism_module11();
	mercury__polymorphism_module12();
	mercury__polymorphism_module13();
	mercury__polymorphism_module14();
	mercury__polymorphism_module15();
	mercury__polymorphism_module16();
	mercury__polymorphism_module17();
	mercury__polymorphism_module18();
	mercury__polymorphism_module19();
	mercury__polymorphism_module20();
	mercury__polymorphism_module21();
	mercury__polymorphism_module22();
	mercury__polymorphism_module23();
	mercury__polymorphism_module24();
}

#endif

void mercury__polymorphism__init(void); /* suppress gcc warning */
void mercury__polymorphism__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__polymorphism_bunch_0();
#endif
}
