/*
** Automatically generated from `liveness.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__liveness__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__liveness__LambdaGoal__3_2_0);
Declare_static(mercury__liveness__LambdaGoal__2_2_0);
Declare_static(mercury__liveness__LambdaGoal__1_2_0);
Define_extern_entry(mercury__liveness__detect_liveness_proc_7_0);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i2);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i3);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i4);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i5);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i6);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i7);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i8);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i9);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i10);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i11);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i12);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i15);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i14);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i17);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i18);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i19);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i20);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i24);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i25);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i21);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i26);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i27);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i28);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i29);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i30);
Declare_label(mercury__liveness__detect_liveness_proc_7_0_i31);
Define_extern_entry(mercury__liveness__initial_liveness_3_0);
Declare_label(mercury__liveness__initial_liveness_3_0_i2);
Declare_label(mercury__liveness__initial_liveness_3_0_i3);
Declare_label(mercury__liveness__initial_liveness_3_0_i4);
Declare_label(mercury__liveness__initial_liveness_3_0_i5);
Declare_label(mercury__liveness__initial_liveness_3_0_i6);
Declare_label(mercury__liveness__initial_liveness_3_0_i9);
Declare_label(mercury__liveness__initial_liveness_3_0_i1000);
Declare_static(mercury__liveness__detect_liveness_in_goal_5_0);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i2);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i3);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i4);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i5);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i6);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i7);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i8);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i11);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i13);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i10);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i14);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i15);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i16);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i17);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i18);
Declare_label(mercury__liveness__detect_liveness_in_goal_5_0_i19);
Declare_static(mercury__liveness__detect_liveness_in_goal_2_6_0);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1007);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1006);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1005);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1004);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1003);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i5);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i6);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i7);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i8);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i10);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i11);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i12);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i14);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i15);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i16);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i17);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i18);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i19);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i20);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i21);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i22);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i23);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i24);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i25);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i26);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i27);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1002);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i30);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i29);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1000);
Declare_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1001);
Declare_static(mercury__liveness__detect_liveness_in_conj_5_0);
Declare_label(mercury__liveness__detect_liveness_in_conj_5_0_i4);
Declare_label(mercury__liveness__detect_liveness_in_conj_5_0_i7);
Declare_label(mercury__liveness__detect_liveness_in_conj_5_0_i8);
Declare_label(mercury__liveness__detect_liveness_in_conj_5_0_i6);
Declare_label(mercury__liveness__detect_liveness_in_conj_5_0_i10);
Declare_label(mercury__liveness__detect_liveness_in_conj_5_0_i1004);
Declare_static(mercury__liveness__detect_liveness_in_disj_7_0);
Declare_label(mercury__liveness__detect_liveness_in_disj_7_0_i4);
Declare_label(mercury__liveness__detect_liveness_in_disj_7_0_i5);
Declare_label(mercury__liveness__detect_liveness_in_disj_7_0_i6);
Declare_label(mercury__liveness__detect_liveness_in_disj_7_0_i7);
Declare_label(mercury__liveness__detect_liveness_in_disj_7_0_i8);
Declare_label(mercury__liveness__detect_liveness_in_disj_7_0_i9);
Declare_label(mercury__liveness__detect_liveness_in_disj_7_0_i1002);
Declare_static(mercury__liveness__detect_liveness_in_cases_7_0);
Declare_label(mercury__liveness__detect_liveness_in_cases_7_0_i4);
Declare_label(mercury__liveness__detect_liveness_in_cases_7_0_i5);
Declare_label(mercury__liveness__detect_liveness_in_cases_7_0_i6);
Declare_label(mercury__liveness__detect_liveness_in_cases_7_0_i7);
Declare_label(mercury__liveness__detect_liveness_in_cases_7_0_i8);
Declare_label(mercury__liveness__detect_liveness_in_cases_7_0_i9);
Declare_label(mercury__liveness__detect_liveness_in_cases_7_0_i1003);
Declare_static(mercury__liveness__detect_deadness_in_goal_5_0);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i2);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i3);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i4);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i7);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i12);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i13);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i9);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i14);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i15);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i16);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i17);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i6);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i18);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i22);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i23);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i19);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i24);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i25);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i26);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i27);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i28);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i29);
Declare_label(mercury__liveness__detect_deadness_in_goal_5_0_i30);
Declare_static(mercury__liveness__detect_deadness_in_goal_2_6_0);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1011);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1010);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1009);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1008);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1007);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i5);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i6);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i7);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i8);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i9);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i11);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i12);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i13);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1003);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i16);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i17);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i18);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i19);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i20);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i21);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i22);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i23);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i24);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i25);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i26);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i27);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i28);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i29);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i30);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1006);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i33);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i32);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1004);
Declare_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1005);
Declare_static(mercury__liveness__detect_deadness_in_conj_5_0);
Declare_label(mercury__liveness__detect_deadness_in_conj_5_0_i6);
Declare_label(mercury__liveness__detect_deadness_in_conj_5_0_i7);
Declare_label(mercury__liveness__detect_deadness_in_conj_5_0_i9);
Declare_label(mercury__liveness__detect_deadness_in_conj_5_0_i5);
Declare_label(mercury__liveness__detect_deadness_in_conj_5_0_i10);
Declare_label(mercury__liveness__detect_deadness_in_conj_5_0_i11);
Declare_label(mercury__liveness__detect_deadness_in_conj_5_0_i1004);
Declare_static(mercury__liveness__detect_deadness_in_disj_7_0);
Declare_label(mercury__liveness__detect_deadness_in_disj_7_0_i4);
Declare_label(mercury__liveness__detect_deadness_in_disj_7_0_i5);
Declare_label(mercury__liveness__detect_deadness_in_disj_7_0_i6);
Declare_label(mercury__liveness__detect_deadness_in_disj_7_0_i7);
Declare_label(mercury__liveness__detect_deadness_in_disj_7_0_i8);
Declare_label(mercury__liveness__detect_deadness_in_disj_7_0_i9);
Declare_label(mercury__liveness__detect_deadness_in_disj_7_0_i1002);
Declare_static(mercury__liveness__detect_deadness_in_cases_8_0);
Declare_label(mercury__liveness__detect_deadness_in_cases_8_0_i4);
Declare_label(mercury__liveness__detect_deadness_in_cases_8_0_i5);
Declare_label(mercury__liveness__detect_deadness_in_cases_8_0_i6);
Declare_label(mercury__liveness__detect_deadness_in_cases_8_0_i7);
Declare_label(mercury__liveness__detect_deadness_in_cases_8_0_i8);
Declare_label(mercury__liveness__detect_deadness_in_cases_8_0_i9);
Declare_label(mercury__liveness__detect_deadness_in_cases_8_0_i10);
Declare_label(mercury__liveness__detect_deadness_in_cases_8_0_i1003);
Declare_static(mercury__liveness__detect_resume_points_in_goal_6_0);
Declare_label(mercury__liveness__detect_resume_points_in_goal_6_0_i2);
Declare_label(mercury__liveness__detect_resume_points_in_goal_6_0_i3);
Declare_label(mercury__liveness__detect_resume_points_in_goal_6_0_i4);
Declare_label(mercury__liveness__detect_resume_points_in_goal_6_0_i5);
Declare_label(mercury__liveness__detect_resume_points_in_goal_6_0_i6);
Declare_label(mercury__liveness__detect_resume_points_in_goal_6_0_i7);
Declare_label(mercury__liveness__detect_resume_points_in_goal_6_0_i8);
Declare_label(mercury__liveness__detect_resume_points_in_goal_6_0_i9);
Declare_label(mercury__liveness__detect_resume_points_in_goal_6_0_i10);
Declare_static(mercury__liveness__detect_resume_points_in_goal_2_7_0);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1041);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1040);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1039);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1038);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1037);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i5);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i6);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i8);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i9);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i13);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i10);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i16);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i17);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i18);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i19);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i22);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i21);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i26);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i25);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i29);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i30);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i31);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i32);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i34);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i35);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i36);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i37);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i38);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i39);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i42);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i44);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i41);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i50);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i49);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i53);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1033);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i57);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i56);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i59);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i60);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i61);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i62);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i63);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i64);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i65);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i66);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i67);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i68);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i69);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1036);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i73);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i72);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1034);
Declare_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1035);
Declare_static(mercury__liveness__detect_resume_points_in_conj_6_0);
Declare_label(mercury__liveness__detect_resume_points_in_conj_6_0_i4);
Declare_label(mercury__liveness__detect_resume_points_in_conj_6_0_i5);
Declare_label(mercury__liveness__detect_resume_points_in_conj_6_0_i1002);
Declare_static(mercury__liveness__detect_resume_points_in_non_disj_7_0);
Declare_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i7);
Declare_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i8);
Declare_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i4);
Declare_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i1000);
Declare_static(mercury__liveness__detect_resume_points_in_pruned_disj_7_0);
Declare_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i4);
Declare_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i5);
Declare_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i10);
Declare_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i11);
Declare_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i6);
Declare_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i12);
Declare_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i3);
Declare_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i14);
Declare_static(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i2);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i3);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i7);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i5);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i4);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i11);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i10);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i14);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i15);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i16);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i17);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i18);
Declare_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i19);
Declare_static(mercury__liveness__detect_resume_points_in_last_disjunct_7_0);
Declare_label(mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i2);
Declare_label(mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i3);
Declare_label(mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i4);
Declare_static(mercury__liveness__detect_resume_points_in_cases_6_0);
Declare_label(mercury__liveness__detect_resume_points_in_cases_6_0_i4);
Declare_label(mercury__liveness__detect_resume_points_in_cases_6_0_i8);
Declare_label(mercury__liveness__detect_resume_points_in_cases_6_0_i1010);
Declare_label(mercury__liveness__detect_resume_points_in_cases_6_0_i1011);
Declare_label(mercury__liveness__detect_resume_points_in_cases_6_0_i1006);
Declare_static(mercury__liveness__initial_liveness_2_6_0);
Declare_label(mercury__liveness__initial_liveness_2_6_0_i1012);
Declare_label(mercury__liveness__initial_liveness_2_6_0_i10);
Declare_label(mercury__liveness__initial_liveness_2_6_0_i11);
Declare_label(mercury__liveness__initial_liveness_2_6_0_i9);
Declare_label(mercury__liveness__initial_liveness_2_6_0_i13);
Declare_label(mercury__liveness__initial_liveness_2_6_0_i1009);
Declare_label(mercury__liveness__initial_liveness_2_6_0_i1);
Declare_label(mercury__liveness__initial_liveness_2_6_0_i1011);
Declare_static(mercury__liveness__initial_deadness_2_6_0);
Declare_label(mercury__liveness__initial_deadness_2_6_0_i1012);
Declare_label(mercury__liveness__initial_deadness_2_6_0_i10);
Declare_label(mercury__liveness__initial_deadness_2_6_0_i11);
Declare_label(mercury__liveness__initial_deadness_2_6_0_i9);
Declare_label(mercury__liveness__initial_deadness_2_6_0_i13);
Declare_label(mercury__liveness__initial_deadness_2_6_0_i1009);
Declare_label(mercury__liveness__initial_deadness_2_6_0_i1);
Declare_label(mercury__liveness__initial_deadness_2_6_0_i1011);
Declare_static(mercury__liveness__stuff_liveness_residue_after_goal_3_0);
Declare_label(mercury__liveness__stuff_liveness_residue_after_goal_3_0_i2);
Declare_label(mercury__liveness__stuff_liveness_residue_after_goal_3_0_i3);
Declare_label(mercury__liveness__stuff_liveness_residue_after_goal_3_0_i4);
Declare_static(mercury__liveness__stuff_deadness_residue_before_goal_3_0);
Declare_label(mercury__liveness__stuff_deadness_residue_before_goal_3_0_i2);
Declare_label(mercury__liveness__stuff_deadness_residue_before_goal_3_0_i3);
Declare_label(mercury__liveness__stuff_deadness_residue_before_goal_3_0_i4);
Declare_static(mercury__liveness__find_binding_occurrences_5_0);
Declare_label(mercury__liveness__find_binding_occurrences_5_0_i4);
Declare_label(mercury__liveness__find_binding_occurrences_5_0_i5);
Declare_label(mercury__liveness__find_binding_occurrences_5_0_i8);
Declare_label(mercury__liveness__find_binding_occurrences_5_0_i9);
Declare_label(mercury__liveness__find_binding_occurrences_5_0_i7);
Declare_label(mercury__liveness__find_binding_occurrences_5_0_i1003);
Declare_static(mercury__liveness__live_info_get_varset_2_0);

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_liveness__base_type_layout_live_info_0[];
Word * mercury_data_liveness__base_type_info_live_info_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_liveness__base_type_layout_live_info_0
};

extern Word * mercury_data_liveness__common_5[];
Word * mercury_data_liveness__base_type_layout_live_info_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_liveness__common_5),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

Word * mercury_data_liveness__common_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) STATIC(mercury__liveness__LambdaGoal__1_2_0)
};

extern Word * mercury_data_hlds_module__base_type_info_module_info_0[];
Word * mercury_data_liveness__common_1[] = {
	(Word *) (Integer) mercury_data_hlds_module__base_type_info_module_info_0
};

extern Word * mercury_data_hlds_pred__base_type_info_proc_info_0[];
Word * mercury_data_liveness__common_2[] = {
	(Word *) (Integer) mercury_data_hlds_pred__base_type_info_proc_info_0
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data_mercury_builtin__base_type_info_var_0[];
extern Word * mercury_data_mercury_builtin__base_type_info_term_0[];
Word * mercury_data_liveness__common_3[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_var_0,
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_term_0
};

extern Word * mercury_data_varset__base_type_info_varset_0[];
Word * mercury_data_liveness__common_4[] = {
	(Word *) (Integer) mercury_data_varset__base_type_info_varset_0
};

Word * mercury_data_liveness__common_5[] = {
	(Word *) ((Integer) 4),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_liveness__common_1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_liveness__common_2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_liveness__common_3),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_liveness__common_4),
	(Word *) string_const("live_info", 9)
};

BEGIN_MODULE(mercury__liveness_module0)
	init_entry(mercury__liveness__LambdaGoal__3_2_0);
BEGIN_CODE

/* code for predicate 'liveness__LambdaGoal__3'/2 in mode 0 */
Define_static(mercury__liveness__LambdaGoal__3_2_0);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__equal_2_0);
	tailcall(ENTRY(mercury__set__equal_2_0),
		STATIC(mercury__liveness__LambdaGoal__3_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__liveness_module1)
	init_entry(mercury__liveness__LambdaGoal__2_2_0);
BEGIN_CODE

/* code for predicate 'liveness__LambdaGoal__2'/2 in mode 0 */
Define_static(mercury__liveness__LambdaGoal__2_2_0);
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__equal_2_0);
	tailcall(ENTRY(mercury__set__equal_2_0),
		STATIC(mercury__liveness__LambdaGoal__2_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__liveness_module2)
	init_entry(mercury__liveness__LambdaGoal__1_2_0);
BEGIN_CODE

/* code for predicate 'liveness__LambdaGoal__1'/2 in mode 0 */
Define_static(mercury__liveness__LambdaGoal__1_2_0);
	r2 = string_const(" ", 1);
	{
	Declare_entry(mercury__string__append_3_2);
	tailcall(ENTRY(mercury__string__append_3_2),
		STATIC(mercury__liveness__LambdaGoal__1_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__liveness_module3)
	init_entry(mercury__liveness__detect_liveness_proc_7_0);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i2);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i3);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i4);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i5);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i6);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i7);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i8);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i9);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i10);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i11);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i12);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i15);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i14);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i17);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i18);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i19);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i20);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i24);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i25);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i21);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i26);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i27);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i28);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i29);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i30);
	init_label(mercury__liveness__detect_liveness_proc_7_0_i31);
BEGIN_CODE

/* code for predicate 'detect_liveness_proc'/7 in mode 0 */
Define_entry(mercury__liveness__detect_liveness_proc_7_0);
	incr_sp_push_msg(10, "detect_liveness_proc");
	detstackvar(10) = (Integer) succip;
	detstackvar(2) = (Integer) r4;
	r4 = (Integer) r3;
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = string_const("% Computing liveness in ", 24);
	{
	Declare_entry(mercury__passes_aux__write_proc_progress_message_6_0);
	call_localret(ENTRY(mercury__passes_aux__write_proc_progress_message_6_0),
		mercury__liveness__detect_liveness_proc_7_0_i2,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i2);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_pred__proc_info_goal_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_goal_2_0),
		mercury__liveness__detect_liveness_proc_7_0_i3,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i3);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_pred__proc_info_variables_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_variables_2_0),
		mercury__liveness__detect_liveness_proc_7_0_i4,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_pred__proc_info_vartypes_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_vartypes_2_0),
		mercury__liveness__detect_liveness_proc_7_0_i5,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i5);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 4));
	field(mktag(0), (Integer) r3, ((Integer) 3)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) r1;
	detstackvar(5) = (Integer) r3;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) r1;
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	{
		call_localret(STATIC(mercury__liveness__initial_liveness_3_0),
		mercury__liveness__detect_liveness_proc_7_0_i6,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	detstackvar(4) = (Integer) r2;
	r3 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__liveness__detect_liveness_in_goal_5_0),
		mercury__liveness__detect_liveness_proc_7_0_i7,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
Define_label(mercury__liveness__detect_liveness_proc_7_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r1 = (Integer) detstackvar(2);
	detstackvar(6) = (Integer) r2;
	{
	Declare_entry(mercury__hlds_pred__proc_info_headvars_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_headvars_2_0),
		mercury__liveness__detect_liveness_proc_7_0_i8,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_pred__proc_info_argmodes_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_argmodes_2_0),
		mercury__liveness__detect_liveness_proc_7_0_i9,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i9);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_pred__proc_info_vartypes_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_vartypes_2_0),
		mercury__liveness__detect_liveness_proc_7_0_i10,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i10);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r4 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__map__apply_to_list_3_0);
	call_localret(ENTRY(mercury__map__apply_to_list_3_0),
		mercury__liveness__detect_liveness_proc_7_0_i11,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i11);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	detstackvar(9) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_liveness_proc_7_0_i12,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i12);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(9);
	r4 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__liveness__initial_deadness_2_6_0),
		mercury__liveness__detect_liveness_proc_7_0_i15,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
Define_label(mercury__liveness__detect_liveness_proc_7_0_i15);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_liveness_proc_7_0_i14);
	r7 = (Integer) r2;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__liveness__detect_liveness_proc_7_0_i18);
Define_label(mercury__liveness__detect_liveness_proc_7_0_i14);
	r1 = string_const("initial_deadness: list length mis-match", 39);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__liveness__detect_liveness_proc_7_0_i17,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i17);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(6);
	r7 = (Integer) detstackvar(7);
Define_label(mercury__liveness__detect_liveness_proc_7_0_i18);
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(5) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(6) = (Integer) r6;
	detstackvar(7) = (Integer) r7;
	{
	Declare_entry(mercury__hlds_module__module_info_globals_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_globals_2_0),
		mercury__liveness__detect_liveness_proc_7_0_i19,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i19);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	{
	Declare_entry(mercury__globals__get_gc_method_2_0);
	call_localret(ENTRY(mercury__globals__get_gc_method_2_0),
		mercury__liveness__detect_liveness_proc_7_0_i20,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i20);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	if (((Integer) r1 != ((Integer) 2)))
		GOTO_LABEL(mercury__liveness__detect_liveness_proc_7_0_i21);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__proc_info_get_used_typeinfos_setwise_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_get_used_typeinfos_setwise_3_0),
		mercury__liveness__detect_liveness_proc_7_0_i24,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i24);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_liveness_proc_7_0_i25,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i25);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(4);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__liveness__detect_liveness_proc_7_0_i26);
Define_label(mercury__liveness__detect_liveness_proc_7_0_i21);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(4);
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(7);
Define_label(mercury__liveness__detect_liveness_proc_7_0_i26);
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	detstackvar(5) = (Integer) r3;
	detstackvar(4) = (Integer) r6;
	call_localret(STATIC(mercury__liveness__detect_deadness_in_goal_5_0),
		mercury__liveness__detect_liveness_proc_7_0_i27,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
Define_label(mercury__liveness__detect_liveness_proc_7_0_i27);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	detstackvar(7) = (Integer) r2;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_liveness_proc_7_0_i28,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i28);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_6_0),
		mercury__liveness__detect_liveness_proc_7_0_i29,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
Define_label(mercury__liveness__detect_liveness_proc_7_0_i29);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_goal_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_goal_3_0),
		mercury__liveness__detect_liveness_proc_7_0_i30,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i30);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__hlds_pred__proc_info_set_liveness_info_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_set_liveness_info_3_0),
		mercury__liveness__detect_liveness_proc_7_0_i31,
		ENTRY(mercury__liveness__detect_liveness_proc_7_0));
	}
Define_label(mercury__liveness__detect_liveness_proc_7_0_i31);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_proc_7_0));
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module4)
	init_entry(mercury__liveness__initial_liveness_3_0);
	init_label(mercury__liveness__initial_liveness_3_0_i2);
	init_label(mercury__liveness__initial_liveness_3_0_i3);
	init_label(mercury__liveness__initial_liveness_3_0_i4);
	init_label(mercury__liveness__initial_liveness_3_0_i5);
	init_label(mercury__liveness__initial_liveness_3_0_i6);
	init_label(mercury__liveness__initial_liveness_3_0_i9);
	init_label(mercury__liveness__initial_liveness_3_0_i1000);
BEGIN_CODE

/* code for predicate 'initial_liveness'/3 in mode 0 */
Define_entry(mercury__liveness__initial_liveness_3_0);
	incr_sp_push_msg(5, "initial_liveness");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__hlds_pred__proc_info_headvars_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_headvars_2_0),
		mercury__liveness__initial_liveness_3_0_i2,
		ENTRY(mercury__liveness__initial_liveness_3_0));
	}
Define_label(mercury__liveness__initial_liveness_3_0_i2);
	update_prof_current_proc(LABEL(mercury__liveness__initial_liveness_3_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_pred__proc_info_argmodes_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_argmodes_2_0),
		mercury__liveness__initial_liveness_3_0_i3,
		ENTRY(mercury__liveness__initial_liveness_3_0));
	}
Define_label(mercury__liveness__initial_liveness_3_0_i3);
	update_prof_current_proc(LABEL(mercury__liveness__initial_liveness_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_pred__proc_info_vartypes_2_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_vartypes_2_0),
		mercury__liveness__initial_liveness_3_0_i4,
		ENTRY(mercury__liveness__initial_liveness_3_0));
	}
Define_label(mercury__liveness__initial_liveness_3_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__initial_liveness_3_0));
	r4 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__apply_to_list_3_0);
	call_localret(ENTRY(mercury__map__apply_to_list_3_0),
		mercury__liveness__initial_liveness_3_0_i5,
		ENTRY(mercury__liveness__initial_liveness_3_0));
	}
Define_label(mercury__liveness__initial_liveness_3_0_i5);
	update_prof_current_proc(LABEL(mercury__liveness__initial_liveness_3_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__initial_liveness_3_0_i6,
		ENTRY(mercury__liveness__initial_liveness_3_0));
	}
Define_label(mercury__liveness__initial_liveness_3_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__initial_liveness_3_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__liveness__initial_liveness_2_6_0),
		mercury__liveness__initial_liveness_3_0_i9,
		ENTRY(mercury__liveness__initial_liveness_3_0));
Define_label(mercury__liveness__initial_liveness_3_0_i9);
	update_prof_current_proc(LABEL(mercury__liveness__initial_liveness_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__initial_liveness_3_0_i1000);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__liveness__initial_liveness_3_0_i1000);
	r1 = string_const("initial_liveness: list length mismatch", 38);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__liveness__initial_liveness_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__liveness_module5)
	init_entry(mercury__liveness__detect_liveness_in_goal_5_0);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i2);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i3);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i4);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i5);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i6);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i7);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i8);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i11);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i13);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i10);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i14);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i15);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i16);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i17);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i18);
	init_label(mercury__liveness__detect_liveness_in_goal_5_0_i19);
BEGIN_CODE

/* code for predicate 'detect_liveness_in_goal'/5 in mode 0 */
Define_static(mercury__liveness__detect_liveness_in_goal_5_0);
	incr_sp_push_msg(10, "detect_liveness_in_goal");
	detstackvar(10) = (Integer) succip;
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i2,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i2);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	r2 = (Integer) r1;
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i3,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i3);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i4,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_instmap_delta_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_instmap_delta_2_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i5,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i5);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i6,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__liveness__find_binding_occurrences_5_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i7,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	r3 = (Integer) r1;
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i8,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__hlds_goal__goal_is_atomic_1_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_is_atomic_1_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i11,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i11);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_liveness_in_goal_5_0_i10);
	detstackvar(9) = (Integer) detstackvar(8);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i13,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i13);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	GOTO_LABEL(mercury__liveness__detect_liveness_in_goal_5_0_i17);
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i10);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i14,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i14);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	detstackvar(9) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(7);
	r4 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i15,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i15);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	detstackvar(6) = (Integer) r2;
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i16,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i16);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(9);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(6);
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i17);
	detstackvar(3) = (Integer) r3;
	detstackvar(6) = (Integer) r4;
	detstackvar(9) = (Integer) r5;
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_pre_births_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_pre_births_3_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i18,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i18);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	r2 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_post_births_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_post_births_3_0),
		mercury__liveness__detect_liveness_in_goal_5_0_i19,
		STATIC(mercury__liveness__detect_liveness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_5_0_i19);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_5_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module6)
	init_entry(mercury__liveness__detect_liveness_in_goal_2_6_0);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1007);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1006);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1005);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1004);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1003);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i5);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i6);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i7);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i8);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i10);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i11);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i12);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i14);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i15);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i16);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i17);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i18);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i19);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i20);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i21);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i22);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i23);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i24);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i25);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i26);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i27);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1002);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i30);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i29);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1000);
	init_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1001);
BEGIN_CODE

/* code for predicate 'detect_liveness_in_goal_2'/6 in mode 0 */
Define_static(mercury__liveness__detect_liveness_in_goal_2_6_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i1002);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i1007) AND
		LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i1000) AND
		LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i1006) AND
		LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i1005) AND
		LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i1004) AND
		LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i1003) AND
		LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i1000));
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1007);
	incr_sp_push_msg(10, "detect_liveness_in_goal_2");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i5);
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1006);
	incr_sp_push_msg(10, "detect_liveness_in_goal_2");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i10);
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1005);
	incr_sp_push_msg(10, "detect_liveness_in_goal_2");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i14);
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1004);
	incr_sp_push_msg(10, "detect_liveness_in_goal_2");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i16);
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1003);
	incr_sp_push_msg(10, "detect_liveness_in_goal_2");
	detstackvar(10) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i18);
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i5);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i6,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__detect_liveness_in_cases_7_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i7,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 5));
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) r1;
	detstackvar(1) = (Integer) tempr1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	field(mktag(3), (Integer) tempr1, ((Integer) 4)) = (Integer) detstackvar(7);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 0);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i8,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
	}
	}
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i10);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i11,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i11);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__detect_liveness_in_disj_7_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i12,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i12);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r3 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r1;
	detstackvar(1) = (Integer) tempr1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i8,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
	}
	}
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i14);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__liveness__detect_liveness_in_goal_5_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i15,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i15);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i16);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__liveness__detect_liveness_in_goal_5_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i17,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i17);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 4);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i18);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(6) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 5));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__liveness__detect_liveness_in_goal_5_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i19,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i19);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	detstackvar(5) = (Integer) r2;
	r2 = (Integer) tempr1;
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__detect_liveness_in_goal_5_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i20,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i20);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r3 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	detstackvar(6) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__liveness__detect_liveness_in_goal_5_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i21,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i21);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r3 = (Integer) r1;
	detstackvar(8) = (Integer) r1;
	detstackvar(9) = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i22,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i22);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r2 = (Integer) r1;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__set__intersect_3_0);
	call_localret(ENTRY(mercury__set__intersect_3_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i23,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i23);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r2 = (Integer) r1;
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i24,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i24);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i25,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i25);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__liveness__stuff_liveness_residue_after_goal_3_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i26,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i26);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(9);
	call_localret(STATIC(mercury__liveness__stuff_liveness_residue_after_goal_3_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i27,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i27);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	tag_incr_hp(r2, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(3), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r2, ((Integer) 4)) = (Integer) r3;
	field(mktag(3), (Integer) r2, ((Integer) 5)) = (Integer) detstackvar(7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1002);
	incr_sp_push_msg(10, "detect_liveness_in_goal_2");
	detstackvar(10) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i29);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__liveness__detect_liveness_in_conj_5_0),
		mercury__liveness__detect_liveness_in_goal_2_6_0_i30,
		STATIC(mercury__liveness__detect_liveness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i30);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	proceed();
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i29);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(10);
	decr_sp_pop_msg(10);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__liveness__detect_liveness_in_goal_2_6_0_i1001);
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1000);
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
Define_label(mercury__liveness__detect_liveness_in_goal_2_6_0_i1001);
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module7)
	init_entry(mercury__liveness__detect_liveness_in_conj_5_0);
	init_label(mercury__liveness__detect_liveness_in_conj_5_0_i4);
	init_label(mercury__liveness__detect_liveness_in_conj_5_0_i7);
	init_label(mercury__liveness__detect_liveness_in_conj_5_0_i8);
	init_label(mercury__liveness__detect_liveness_in_conj_5_0_i6);
	init_label(mercury__liveness__detect_liveness_in_conj_5_0_i10);
	init_label(mercury__liveness__detect_liveness_in_conj_5_0_i1004);
BEGIN_CODE

/* code for predicate 'detect_liveness_in_conj'/5 in mode 0 */
Define_static(mercury__liveness__detect_liveness_in_conj_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_liveness_in_conj_5_0_i1004);
	incr_sp_push_msg(5, "detect_liveness_in_conj");
	detstackvar(5) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(1) = (Integer) r3;
	call_localret(STATIC(mercury__liveness__detect_liveness_in_goal_5_0),
		mercury__liveness__detect_liveness_in_conj_5_0_i4,
		STATIC(mercury__liveness__detect_liveness_in_conj_5_0));
Define_label(mercury__liveness__detect_liveness_in_conj_5_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_conj_5_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_instmap_delta_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_instmap_delta_2_0),
		mercury__liveness__detect_liveness_in_conj_5_0_i7,
		STATIC(mercury__liveness__detect_liveness_in_conj_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_conj_5_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_conj_5_0));
	{
	Declare_entry(mercury__instmap__instmap_delta_is_unreachable_1_0);
	call_localret(ENTRY(mercury__instmap__instmap_delta_is_unreachable_1_0),
		mercury__liveness__detect_liveness_in_conj_5_0_i8,
		STATIC(mercury__liveness__detect_liveness_in_conj_5_0));
	}
Define_label(mercury__liveness__detect_liveness_in_conj_5_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_conj_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_liveness_in_conj_5_0_i6);
	r1 = (Integer) detstackvar(4);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__liveness__detect_liveness_in_conj_5_0_i6);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__liveness__detect_liveness_in_conj_5_0,
		LABEL(mercury__liveness__detect_liveness_in_conj_5_0_i10),
		STATIC(mercury__liveness__detect_liveness_in_conj_5_0));
Define_label(mercury__liveness__detect_liveness_in_conj_5_0_i10);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_conj_5_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__liveness__detect_liveness_in_conj_5_0_i1004);
	r1 = (Integer) r2;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module8)
	init_entry(mercury__liveness__detect_liveness_in_disj_7_0);
	init_label(mercury__liveness__detect_liveness_in_disj_7_0_i4);
	init_label(mercury__liveness__detect_liveness_in_disj_7_0_i5);
	init_label(mercury__liveness__detect_liveness_in_disj_7_0_i6);
	init_label(mercury__liveness__detect_liveness_in_disj_7_0_i7);
	init_label(mercury__liveness__detect_liveness_in_disj_7_0_i8);
	init_label(mercury__liveness__detect_liveness_in_disj_7_0_i9);
	init_label(mercury__liveness__detect_liveness_in_disj_7_0_i1002);
BEGIN_CODE

/* code for predicate 'detect_liveness_in_disj'/7 in mode 0 */
Define_static(mercury__liveness__detect_liveness_in_disj_7_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_liveness_in_disj_7_0_i1002);
	incr_sp_push_msg(7, "detect_liveness_in_disj");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__liveness__detect_liveness_in_goal_5_0),
		mercury__liveness__detect_liveness_in_disj_7_0_i4,
		STATIC(mercury__liveness__detect_liveness_in_disj_7_0));
Define_label(mercury__liveness__detect_liveness_in_disj_7_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_disj_7_0));
	detstackvar(6) = (Integer) r2;
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) r1;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_liveness_in_disj_7_0_i5,
		STATIC(mercury__liveness__detect_liveness_in_disj_7_0));
	}
Define_label(mercury__liveness__detect_liveness_in_disj_7_0_i5);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_disj_7_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	localcall(mercury__liveness__detect_liveness_in_disj_7_0,
		LABEL(mercury__liveness__detect_liveness_in_disj_7_0_i6),
		STATIC(mercury__liveness__detect_liveness_in_disj_7_0));
Define_label(mercury__liveness__detect_liveness_in_disj_7_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_disj_7_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) r1;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__intersect_3_0);
	call_localret(ENTRY(mercury__set__intersect_3_0),
		mercury__liveness__detect_liveness_in_disj_7_0_i7,
		STATIC(mercury__liveness__detect_liveness_in_disj_7_0));
	}
Define_label(mercury__liveness__detect_liveness_in_disj_7_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_disj_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_liveness_in_disj_7_0_i8,
		STATIC(mercury__liveness__detect_liveness_in_disj_7_0));
	}
Define_label(mercury__liveness__detect_liveness_in_disj_7_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_disj_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__liveness__stuff_liveness_residue_after_goal_3_0),
		mercury__liveness__detect_liveness_in_disj_7_0_i9,
		STATIC(mercury__liveness__detect_liveness_in_disj_7_0));
Define_label(mercury__liveness__detect_liveness_in_disj_7_0_i9);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_disj_7_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__liveness__detect_liveness_in_disj_7_0_i1002);
	r1 = (Integer) r5;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module9)
	init_entry(mercury__liveness__detect_liveness_in_cases_7_0);
	init_label(mercury__liveness__detect_liveness_in_cases_7_0_i4);
	init_label(mercury__liveness__detect_liveness_in_cases_7_0_i5);
	init_label(mercury__liveness__detect_liveness_in_cases_7_0_i6);
	init_label(mercury__liveness__detect_liveness_in_cases_7_0_i7);
	init_label(mercury__liveness__detect_liveness_in_cases_7_0_i8);
	init_label(mercury__liveness__detect_liveness_in_cases_7_0_i9);
	init_label(mercury__liveness__detect_liveness_in_cases_7_0_i1003);
BEGIN_CODE

/* code for predicate 'detect_liveness_in_cases'/7 in mode 0 */
Define_static(mercury__liveness__detect_liveness_in_cases_7_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_liveness_in_cases_7_0_i1003);
	incr_sp_push_msg(8, "detect_liveness_in_cases");
	detstackvar(8) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r3 = (Integer) r4;
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	call_localret(STATIC(mercury__liveness__detect_liveness_in_goal_5_0),
		mercury__liveness__detect_liveness_in_cases_7_0_i4,
		STATIC(mercury__liveness__detect_liveness_in_cases_7_0));
	}
Define_label(mercury__liveness__detect_liveness_in_cases_7_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_cases_7_0));
	detstackvar(7) = (Integer) r2;
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) r1;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_liveness_in_cases_7_0_i5,
		STATIC(mercury__liveness__detect_liveness_in_cases_7_0));
	}
Define_label(mercury__liveness__detect_liveness_in_cases_7_0_i5);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_cases_7_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	localcall(mercury__liveness__detect_liveness_in_cases_7_0,
		LABEL(mercury__liveness__detect_liveness_in_cases_7_0_i6),
		STATIC(mercury__liveness__detect_liveness_in_cases_7_0));
Define_label(mercury__liveness__detect_liveness_in_cases_7_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_cases_7_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) r1;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__intersect_3_0);
	call_localret(ENTRY(mercury__set__intersect_3_0),
		mercury__liveness__detect_liveness_in_cases_7_0_i7,
		STATIC(mercury__liveness__detect_liveness_in_cases_7_0));
	}
Define_label(mercury__liveness__detect_liveness_in_cases_7_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_cases_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_liveness_in_cases_7_0_i8,
		STATIC(mercury__liveness__detect_liveness_in_cases_7_0));
	}
Define_label(mercury__liveness__detect_liveness_in_cases_7_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_cases_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(7);
	call_localret(STATIC(mercury__liveness__stuff_liveness_residue_after_goal_3_0),
		mercury__liveness__detect_liveness_in_cases_7_0_i9,
		STATIC(mercury__liveness__detect_liveness_in_cases_7_0));
Define_label(mercury__liveness__detect_liveness_in_cases_7_0_i9);
	update_prof_current_proc(LABEL(mercury__liveness__detect_liveness_in_cases_7_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
	}
Define_label(mercury__liveness__detect_liveness_in_cases_7_0_i1003);
	r1 = (Integer) r5;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module10)
	init_entry(mercury__liveness__detect_deadness_in_goal_5_0);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i2);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i3);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i4);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i7);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i12);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i13);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i9);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i14);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i15);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i16);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i17);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i6);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i18);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i22);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i23);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i19);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i24);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i25);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i26);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i27);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i28);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i29);
	init_label(mercury__liveness__detect_deadness_in_goal_5_0_i30);
BEGIN_CODE

/* code for predicate 'detect_deadness_in_goal'/5 in mode 0 */
Define_static(mercury__liveness__detect_deadness_in_goal_5_0);
	incr_sp_push_msg(11, "detect_deadness_in_goal");
	detstackvar(11) = (Integer) succip;
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i2,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i2);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) detstackvar(2), ((Integer) 0));
	{
	Declare_entry(mercury__hlds_module__module_info_globals_2_0);
	call_localret(ENTRY(mercury__hlds_module__module_info_globals_2_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i3,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i3);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	{
	Declare_entry(mercury__globals__get_gc_method_2_0);
	call_localret(ENTRY(mercury__globals__get_gc_method_2_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i4,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__hlds_goal__goal_is_atomic_1_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_is_atomic_1_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i7,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_5_0_i6);
	if (((Integer) detstackvar(8) != ((Integer) 2)))
		GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_5_0_i9);
	r1 = (Integer) field(mktag(0), (Integer) detstackvar(2), ((Integer) 1));
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__proc_info_get_used_typeinfos_setwise_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_get_used_typeinfos_setwise_3_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i12,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i12);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i13,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i13);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_5_0_i14);
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i9);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(4);
	r4 = (Integer) detstackvar(5);
	r5 = (Integer) detstackvar(7);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i14);
	detstackvar(1) = (Integer) r2;
	detstackvar(4) = (Integer) r3;
	detstackvar(5) = (Integer) r4;
	detstackvar(3) = (Integer) r5;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i15,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i15);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	detstackvar(9) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i16,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i16);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	r3 = (Integer) r1;
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i17,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i17);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(9);
	r2 = (Integer) detstackvar(10);
	GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_5_0_i28);
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i6);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i18,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i18);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	if (((Integer) detstackvar(8) != ((Integer) 2)))
		GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_5_0_i19);
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) detstackvar(2), ((Integer) 1));
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__hlds_pred__proc_info_get_used_typeinfos_setwise_3_0);
	call_localret(ENTRY(mercury__hlds_pred__proc_info_get_used_typeinfos_setwise_3_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i22,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i22);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i23,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i23);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) r1;
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_5_0_i24);
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i19);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) r1;
	r5 = (Integer) detstackvar(4);
	r6 = (Integer) detstackvar(5);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i24);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i25,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i25);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i26,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i26);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	detstackvar(6) = (Integer) r2;
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i27,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i27);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(10);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(6);
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i28);
	detstackvar(3) = (Integer) r3;
	detstackvar(6) = (Integer) r4;
	detstackvar(9) = (Integer) r5;
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_post_deaths_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_post_deaths_3_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i29,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i29);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	r2 = (Integer) detstackvar(9);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_pre_deaths_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_pre_deaths_3_0),
		mercury__liveness__detect_deadness_in_goal_5_0_i30,
		STATIC(mercury__liveness__detect_deadness_in_goal_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_5_0_i30);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_5_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(11);
	decr_sp_pop_msg(11);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module11)
	init_entry(mercury__liveness__detect_deadness_in_goal_2_6_0);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1011);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1010);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1009);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1008);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1007);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i5);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i6);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i7);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i8);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i9);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i11);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i12);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i13);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1003);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i16);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i17);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i18);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i19);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i20);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i21);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i22);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i23);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i24);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i25);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i26);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i27);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i28);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i29);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i30);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1006);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i33);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i32);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1004);
	init_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1005);
BEGIN_CODE

/* code for predicate 'detect_deadness_in_goal_2'/6 in mode 0 */
Define_static(mercury__liveness__detect_deadness_in_goal_2_6_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i1006);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i1011) AND
		LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i1004) AND
		LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i1010) AND
		LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i1009) AND
		LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i1008) AND
		LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i1007) AND
		LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i1004));
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1011);
	incr_sp_push_msg(9, "detect_deadness_in_goal_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i5);
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1010);
	incr_sp_push_msg(9, "detect_deadness_in_goal_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i11);
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1009);
	incr_sp_push_msg(9, "detect_deadness_in_goal_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i16);
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1008);
	incr_sp_push_msg(9, "detect_deadness_in_goal_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i18);
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1007);
	incr_sp_push_msg(9, "detect_deadness_in_goal_2");
	detstackvar(9) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i20);
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i5);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i6,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i7,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	r6 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__liveness__detect_deadness_in_cases_8_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i8,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 5));
	r3 = (Integer) r1;
	detstackvar(1) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 3)) = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 4)) = (Integer) detstackvar(7);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(5);
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 0);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i9,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i9);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i11);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i12,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i12);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i13,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i13);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__liveness__detect_deadness_in_disj_7_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i1003,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1003);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 3));
	r3 = (Integer) r1;
	detstackvar(1) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(2);
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i9,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i16);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__liveness__detect_deadness_in_goal_5_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i17,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i17);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i18);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__liveness__detect_deadness_in_goal_5_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i19,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i19);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 4);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i20);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(6) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 5));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__liveness__detect_deadness_in_goal_5_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i21,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i21);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	detstackvar(6) = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) tempr1;
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__detect_deadness_in_goal_5_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i22,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i22);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r3 = (Integer) detstackvar(3);
	detstackvar(3) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__liveness__detect_deadness_in_goal_5_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i23,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i23);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	detstackvar(5) = (Integer) r1;
	detstackvar(8) = (Integer) r2;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_nonlocals_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_nonlocals_2_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i24,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i24);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i25,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i25);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r3 = (Integer) detstackvar(1);
	r2 = (Integer) r1;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__intersect_3_0);
	call_localret(ENTRY(mercury__set__intersect_3_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i26,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i26);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r3 = (Integer) detstackvar(5);
	r2 = (Integer) r1;
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i27,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i27);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i28,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
	}
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i28);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__liveness__stuff_deadness_residue_before_goal_3_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i29,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i29);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__liveness__stuff_deadness_residue_before_goal_3_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i30,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i30);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	tag_incr_hp(r2, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(3);
	field(mktag(3), (Integer) r2, ((Integer) 4)) = (Integer) r3;
	field(mktag(3), (Integer) r2, ((Integer) 5)) = (Integer) detstackvar(7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1006);
	incr_sp_push_msg(9, "detect_deadness_in_goal_2");
	detstackvar(9) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i32);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__liveness__detect_deadness_in_conj_5_0),
		mercury__liveness__detect_deadness_in_goal_2_6_0_i33,
		STATIC(mercury__liveness__detect_deadness_in_goal_2_6_0));
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i33);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i32);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__liveness__detect_deadness_in_goal_2_6_0_i1005);
	r2 = (Integer) r1;
	r1 = (Integer) r3;
	proceed();
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1004);
	r2 = (Integer) r1;
	r1 = (Integer) r3;
	proceed();
Define_label(mercury__liveness__detect_deadness_in_goal_2_6_0_i1005);
	r2 = (Integer) r1;
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module12)
	init_entry(mercury__liveness__detect_deadness_in_conj_5_0);
	init_label(mercury__liveness__detect_deadness_in_conj_5_0_i6);
	init_label(mercury__liveness__detect_deadness_in_conj_5_0_i7);
	init_label(mercury__liveness__detect_deadness_in_conj_5_0_i9);
	init_label(mercury__liveness__detect_deadness_in_conj_5_0_i5);
	init_label(mercury__liveness__detect_deadness_in_conj_5_0_i10);
	init_label(mercury__liveness__detect_deadness_in_conj_5_0_i11);
	init_label(mercury__liveness__detect_deadness_in_conj_5_0_i1004);
BEGIN_CODE

/* code for predicate 'detect_deadness_in_conj'/5 in mode 0 */
Define_static(mercury__liveness__detect_deadness_in_conj_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_deadness_in_conj_5_0_i1004);
	incr_sp_push_msg(5, "detect_deadness_in_conj");
	detstackvar(5) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) tempr1;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_instmap_delta_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_instmap_delta_2_0),
		mercury__liveness__detect_deadness_in_conj_5_0_i6,
		STATIC(mercury__liveness__detect_deadness_in_conj_5_0));
	}
	}
Define_label(mercury__liveness__detect_deadness_in_conj_5_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_conj_5_0));
	{
	Declare_entry(mercury__instmap__instmap_delta_is_unreachable_1_0);
	call_localret(ENTRY(mercury__instmap__instmap_delta_is_unreachable_1_0),
		mercury__liveness__detect_deadness_in_conj_5_0_i7,
		STATIC(mercury__liveness__detect_deadness_in_conj_5_0));
	}
Define_label(mercury__liveness__detect_deadness_in_conj_5_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_conj_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_deadness_in_conj_5_0_i5);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__liveness__detect_deadness_in_goal_5_0),
		mercury__liveness__detect_deadness_in_conj_5_0_i9,
		STATIC(mercury__liveness__detect_deadness_in_conj_5_0));
Define_label(mercury__liveness__detect_deadness_in_conj_5_0_i9);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_conj_5_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(4);
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__liveness__detect_deadness_in_conj_5_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	localcall(mercury__liveness__detect_deadness_in_conj_5_0,
		LABEL(mercury__liveness__detect_deadness_in_conj_5_0_i10),
		STATIC(mercury__liveness__detect_deadness_in_conj_5_0));
Define_label(mercury__liveness__detect_deadness_in_conj_5_0_i10);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_conj_5_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__liveness__detect_deadness_in_goal_5_0),
		mercury__liveness__detect_deadness_in_conj_5_0_i11,
		STATIC(mercury__liveness__detect_deadness_in_conj_5_0));
Define_label(mercury__liveness__detect_deadness_in_conj_5_0_i11);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_conj_5_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__liveness__detect_deadness_in_conj_5_0_i1004);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module13)
	init_entry(mercury__liveness__detect_deadness_in_disj_7_0);
	init_label(mercury__liveness__detect_deadness_in_disj_7_0_i4);
	init_label(mercury__liveness__detect_deadness_in_disj_7_0_i5);
	init_label(mercury__liveness__detect_deadness_in_disj_7_0_i6);
	init_label(mercury__liveness__detect_deadness_in_disj_7_0_i7);
	init_label(mercury__liveness__detect_deadness_in_disj_7_0_i8);
	init_label(mercury__liveness__detect_deadness_in_disj_7_0_i9);
	init_label(mercury__liveness__detect_deadness_in_disj_7_0_i1002);
BEGIN_CODE

/* code for predicate 'detect_deadness_in_disj'/7 in mode 0 */
Define_static(mercury__liveness__detect_deadness_in_disj_7_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_deadness_in_disj_7_0_i1002);
	incr_sp_push_msg(7, "detect_deadness_in_disj");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) r4;
	call_localret(STATIC(mercury__liveness__detect_deadness_in_goal_5_0),
		mercury__liveness__detect_deadness_in_disj_7_0_i4,
		STATIC(mercury__liveness__detect_deadness_in_disj_7_0));
Define_label(mercury__liveness__detect_deadness_in_disj_7_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_disj_7_0));
	detstackvar(6) = (Integer) r2;
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) r1;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_deadness_in_disj_7_0_i5,
		STATIC(mercury__liveness__detect_deadness_in_disj_7_0));
	}
Define_label(mercury__liveness__detect_deadness_in_disj_7_0_i5);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_disj_7_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	localcall(mercury__liveness__detect_deadness_in_disj_7_0,
		LABEL(mercury__liveness__detect_deadness_in_disj_7_0_i6),
		STATIC(mercury__liveness__detect_deadness_in_disj_7_0));
Define_label(mercury__liveness__detect_deadness_in_disj_7_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_disj_7_0));
	r3 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) r1;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__intersect_3_0);
	call_localret(ENTRY(mercury__set__intersect_3_0),
		mercury__liveness__detect_deadness_in_disj_7_0_i7,
		STATIC(mercury__liveness__detect_deadness_in_disj_7_0));
	}
Define_label(mercury__liveness__detect_deadness_in_disj_7_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_disj_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_deadness_in_disj_7_0_i8,
		STATIC(mercury__liveness__detect_deadness_in_disj_7_0));
	}
Define_label(mercury__liveness__detect_deadness_in_disj_7_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_disj_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	call_localret(STATIC(mercury__liveness__stuff_deadness_residue_before_goal_3_0),
		mercury__liveness__detect_deadness_in_disj_7_0_i9,
		STATIC(mercury__liveness__detect_deadness_in_disj_7_0));
Define_label(mercury__liveness__detect_deadness_in_disj_7_0_i9);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_disj_7_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__liveness__detect_deadness_in_disj_7_0_i1002);
	r1 = (Integer) r5;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module14)
	init_entry(mercury__liveness__detect_deadness_in_cases_8_0);
	init_label(mercury__liveness__detect_deadness_in_cases_8_0_i4);
	init_label(mercury__liveness__detect_deadness_in_cases_8_0_i5);
	init_label(mercury__liveness__detect_deadness_in_cases_8_0_i6);
	init_label(mercury__liveness__detect_deadness_in_cases_8_0_i7);
	init_label(mercury__liveness__detect_deadness_in_cases_8_0_i8);
	init_label(mercury__liveness__detect_deadness_in_cases_8_0_i9);
	init_label(mercury__liveness__detect_deadness_in_cases_8_0_i10);
	init_label(mercury__liveness__detect_deadness_in_cases_8_0_i1003);
BEGIN_CODE

/* code for predicate 'detect_deadness_in_cases'/8 in mode 0 */
Define_static(mercury__liveness__detect_deadness_in_cases_8_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_deadness_in_cases_8_0_i1003);
	incr_sp_push_msg(9, "detect_deadness_in_cases");
	detstackvar(9) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(7) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) r3;
	detstackvar(2) = (Integer) r3;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r3 = (Integer) r5;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(5) = (Integer) r6;
	call_localret(STATIC(mercury__liveness__detect_deadness_in_goal_5_0),
		mercury__liveness__detect_deadness_in_cases_8_0_i4,
		STATIC(mercury__liveness__detect_deadness_in_cases_8_0));
	}
Define_label(mercury__liveness__detect_deadness_in_cases_8_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_cases_8_0));
	detstackvar(8) = (Integer) r2;
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) r1;
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_deadness_in_cases_8_0_i5,
		STATIC(mercury__liveness__detect_deadness_in_cases_8_0));
	}
Define_label(mercury__liveness__detect_deadness_in_cases_8_0_i5);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_cases_8_0));
	r6 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(7);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	localcall(mercury__liveness__detect_deadness_in_cases_8_0,
		LABEL(mercury__liveness__detect_deadness_in_cases_8_0_i6),
		STATIC(mercury__liveness__detect_deadness_in_cases_8_0));
Define_label(mercury__liveness__detect_deadness_in_cases_8_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_cases_8_0));
	detstackvar(2) = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__liveness__detect_deadness_in_cases_8_0_i7,
		STATIC(mercury__liveness__detect_deadness_in_cases_8_0));
	}
Define_label(mercury__liveness__detect_deadness_in_cases_8_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_cases_8_0));
	r2 = (Integer) r1;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__intersect_3_0);
	call_localret(ENTRY(mercury__set__intersect_3_0),
		mercury__liveness__detect_deadness_in_cases_8_0_i8,
		STATIC(mercury__liveness__detect_deadness_in_cases_8_0));
	}
Define_label(mercury__liveness__detect_deadness_in_cases_8_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_cases_8_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_deadness_in_cases_8_0_i9,
		STATIC(mercury__liveness__detect_deadness_in_cases_8_0));
	}
Define_label(mercury__liveness__detect_deadness_in_cases_8_0_i9);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_cases_8_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(8);
	call_localret(STATIC(mercury__liveness__stuff_deadness_residue_before_goal_3_0),
		mercury__liveness__detect_deadness_in_cases_8_0_i10,
		STATIC(mercury__liveness__detect_deadness_in_cases_8_0));
Define_label(mercury__liveness__detect_deadness_in_cases_8_0_i10);
	update_prof_current_proc(LABEL(mercury__liveness__detect_deadness_in_cases_8_0));
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
	}
Define_label(mercury__liveness__detect_deadness_in_cases_8_0_i1003);
	r1 = (Integer) r6;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module15)
	init_entry(mercury__liveness__detect_resume_points_in_goal_6_0);
	init_label(mercury__liveness__detect_resume_points_in_goal_6_0_i2);
	init_label(mercury__liveness__detect_resume_points_in_goal_6_0_i3);
	init_label(mercury__liveness__detect_resume_points_in_goal_6_0_i4);
	init_label(mercury__liveness__detect_resume_points_in_goal_6_0_i5);
	init_label(mercury__liveness__detect_resume_points_in_goal_6_0_i6);
	init_label(mercury__liveness__detect_resume_points_in_goal_6_0_i7);
	init_label(mercury__liveness__detect_resume_points_in_goal_6_0_i8);
	init_label(mercury__liveness__detect_resume_points_in_goal_6_0_i9);
	init_label(mercury__liveness__detect_resume_points_in_goal_6_0_i10);
BEGIN_CODE

/* code for predicate 'detect_resume_points_in_goal'/6 in mode 0 */
Define_static(mercury__liveness__detect_resume_points_in_goal_6_0);
	incr_sp_push_msg(9, "detect_resume_points_in_goal");
	detstackvar(9) = (Integer) succip;
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_pre_births_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_pre_births_2_0),
		mercury__liveness__detect_resume_points_in_goal_6_0_i2,
		STATIC(mercury__liveness__detect_resume_points_in_goal_6_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_6_0_i2);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_6_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_post_births_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_post_births_2_0),
		mercury__liveness__detect_resume_points_in_goal_6_0_i3,
		STATIC(mercury__liveness__detect_resume_points_in_goal_6_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_6_0_i3);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_6_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_pre_deaths_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_pre_deaths_2_0),
		mercury__liveness__detect_resume_points_in_goal_6_0_i4,
		STATIC(mercury__liveness__detect_resume_points_in_goal_6_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_6_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_6_0));
	detstackvar(8) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_post_deaths_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_post_deaths_2_0),
		mercury__liveness__detect_resume_points_in_goal_6_0_i5,
		STATIC(mercury__liveness__detect_resume_points_in_goal_6_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_6_0_i5);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_6_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_resume_points_in_goal_6_0_i6,
		STATIC(mercury__liveness__detect_resume_points_in_goal_6_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_6_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_resume_points_in_goal_6_0_i7,
		STATIC(mercury__liveness__detect_resume_points_in_goal_6_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_6_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_6_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0),
		mercury__liveness__detect_resume_points_in_goal_6_0_i8,
		STATIC(mercury__liveness__detect_resume_points_in_goal_6_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_6_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_6_0));
	r3 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	detstackvar(1) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(5);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_resume_points_in_goal_6_0_i9,
		STATIC(mercury__liveness__detect_resume_points_in_goal_6_0));
	}
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_6_0_i9);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_resume_points_in_goal_6_0_i10,
		STATIC(mercury__liveness__detect_resume_points_in_goal_6_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_6_0_i10);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module16)
	init_entry(mercury__liveness__detect_resume_points_in_goal_2_7_0);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1041);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1040);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1039);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1038);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1037);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i5);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i6);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i8);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i9);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i13);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i10);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i16);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i17);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i18);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i19);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i22);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i21);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i26);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i25);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i29);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i30);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i31);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i32);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i34);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i35);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i36);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i37);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i38);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i39);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i42);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i44);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i41);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i50);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i49);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i53);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1033);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i57);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i56);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i59);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i60);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i61);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i62);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i63);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i64);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i65);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i66);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i67);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i68);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i69);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1036);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i73);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i72);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1034);
	init_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1035);
BEGIN_CODE

/* code for predicate 'detect_resume_points_in_goal_2'/7 in mode 0 */
Define_static(mercury__liveness__detect_resume_points_in_goal_2_7_0);
	if ((tag((Integer) r1) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1036);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1041) AND
		LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1034) AND
		LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1040) AND
		LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1039) AND
		LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1038) AND
		LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1037) AND
		LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1034));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1041);
	incr_sp_push_msg(13, "detect_resume_points_in_goal_2");
	detstackvar(13) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i5);
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1040);
	incr_sp_push_msg(13, "detect_resume_points_in_goal_2");
	detstackvar(13) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i8);
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1039);
	incr_sp_push_msg(13, "detect_resume_points_in_goal_2");
	detstackvar(13) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i16);
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1038);
	incr_sp_push_msg(13, "detect_resume_points_in_goal_2");
	detstackvar(13) = (Integer) succip;
	GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i31);
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1037);
	incr_sp_push_msg(13, "detect_resume_points_in_goal_2");
	detstackvar(13) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(7) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	detstackvar(8) = (Integer) tempr1;
	detstackvar(9) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 5));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_pre_deaths_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_pre_deaths_2_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i34,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i5);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 4));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 3));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_cases_6_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i6,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i6);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 5));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r1, ((Integer) 3)) = (Integer) r3;
	field(mktag(3), (Integer) r1, ((Integer) 4)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i8);
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_code_model_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_code_model_2_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i9,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i9);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	if (((Integer) r1 != ((Integer) 2)))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i10);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_non_disj_7_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i13,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i13);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r4;
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i10);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_pruned_disj_7_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i13,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i16);
	r2 = (Integer) r3;
	detstackvar(2) = (Integer) r3;
	r3 = (Integer) r4;
	detstackvar(3) = (Integer) r4;
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	detstackvar(1) = (Integer) r1;
	r4 = (Integer) r5;
	detstackvar(4) = (Integer) r5;
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_6_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i17,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i17);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r3 = (Integer) detstackvar(4);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	detstackvar(4) = (Integer) r2;
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i18,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i18);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r4;
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_6_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i19,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i19);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__code_util__cannot_stack_flush_1_0);
	call_localret(ENTRY(mercury__code_util__cannot_stack_flush_1_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i22,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i22);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i21);
	r3 = (Integer) detstackvar(4);
	r1 = (Integer) detstackvar(2);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = ((Integer) 0);
	GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i29);
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i21);
	r1 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__code_util__cannot_fail_before_stack_flush_1_0);
	call_localret(ENTRY(mercury__code_util__cannot_fail_before_stack_flush_1_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i26,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i26);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i25);
	r3 = (Integer) detstackvar(4);
	r1 = (Integer) detstackvar(2);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = ((Integer) 1);
	GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i29);
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i25);
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(4);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = ((Integer) 3);
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i29);
	detstackvar(4) = (Integer) r3;
	{
	Declare_entry(mercury__hlds_goal__goal_set_resume_point_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_set_resume_point_3_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i30,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i30);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i31);
	detstackvar(1) = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 2));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_6_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i32,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i32);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 3));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 4);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(3), (Integer) r1, ((Integer) 2)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i34);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i35,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i35);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i36,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i36);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r4 = (Integer) r1;
	detstackvar(10) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_6_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i37,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i37);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r3 = (Integer) detstackvar(7);
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) r3;
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_6_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i38,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i38);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r4 = (Integer) detstackvar(4);
	detstackvar(4) = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_6_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i39,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i39);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	detstackvar(6) = (Integer) r1;
	detstackvar(8) = (Integer) r2;
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__code_util__cannot_stack_flush_1_0);
	call_localret(ENTRY(mercury__code_util__cannot_stack_flush_1_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i42,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i42);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i41);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_code_model_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_code_model_2_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i44,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i44);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	if (((Integer) r1 == ((Integer) 2)))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i41);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(9);
	r7 = (Integer) detstackvar(2);
	r8 = (Integer) detstackvar(6);
	r1 = (Integer) detstackvar(7);
	r9 = (Integer) detstackvar(8);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(10);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = ((Integer) 0);
	GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i53);
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i41);
	r1 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury__code_util__cannot_fail_before_stack_flush_1_0);
	call_localret(ENTRY(mercury__code_util__cannot_fail_before_stack_flush_1_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i50,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i50);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i49);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(9);
	r7 = (Integer) detstackvar(2);
	r8 = (Integer) detstackvar(6);
	r1 = (Integer) detstackvar(7);
	r9 = (Integer) detstackvar(8);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(10);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = ((Integer) 1);
	GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i53);
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i49);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(5);
	r6 = (Integer) detstackvar(9);
	r7 = (Integer) detstackvar(2);
	r8 = (Integer) detstackvar(6);
	r1 = (Integer) detstackvar(7);
	r9 = (Integer) detstackvar(8);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(10);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = ((Integer) 3);
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i53);
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(9) = (Integer) r6;
	detstackvar(2) = (Integer) r7;
	detstackvar(6) = (Integer) r8;
	detstackvar(8) = (Integer) r9;
	{
	Declare_entry(mercury__hlds_goal__goal_set_resume_point_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_set_resume_point_3_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i1033,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1033);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	tag_incr_hp(r2, mktag(3), ((Integer) 6));
	field(mktag(3), (Integer) r2, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(5);
	field(mktag(3), (Integer) r2, ((Integer) 2)) = (Integer) r1;
	field(mktag(3), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(2);
	field(mktag(3), (Integer) r2, ((Integer) 4)) = (Integer) detstackvar(6);
	field(mktag(3), (Integer) r2, ((Integer) 5)) = (Integer) detstackvar(9);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__set__equal_2_0);
	call_localret(ENTRY(mercury__set__equal_2_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i57,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i57);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i56);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i56);
	r1 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__live_info_get_varset_2_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i59,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i59);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	detstackvar(11) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i60,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i60);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	detstackvar(12) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(8);
	{
	Declare_entry(mercury__set__to_sorted_list_2_0);
	call_localret(ENTRY(mercury__set__to_sorted_list_2_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i61,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i61);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r2 = (Integer) detstackvar(11);
	detstackvar(11) = (Integer) r1;
	r3 = (Integer) detstackvar(12);
	r4 = (Integer) r3;
	tag_incr_hp(r3, mktag(0), ((Integer) 3));
	detstackvar(12) = (Integer) r3;
	field(mktag(0), (Integer) r3, ((Integer) 2)) = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	extern Word * mercury_data___base_type_info_string_0[];
	r2 = (Integer) mercury_data___base_type_info_string_0;
	}
	field(mktag(0), (Integer) r3, ((Integer) 0)) = ((Integer) 1);
	{
	Declare_entry(mercury__varset__lookup_name_3_0);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) ENTRY(mercury__varset__lookup_name_3_0);
	}
	{
	Declare_entry(mercury__list__map_3_0);
	call_localret(ENTRY(mercury__list__map_3_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i62,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i62);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r2 = (Integer) detstackvar(11);
	detstackvar(11) = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r4 = (Integer) r2;
	{
	extern Word * mercury_data___base_type_info_string_0[];
	r2 = (Integer) mercury_data___base_type_info_string_0;
	}
	r3 = (Integer) detstackvar(12);
	{
	Declare_entry(mercury__list__map_3_0);
	call_localret(ENTRY(mercury__list__map_3_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i63,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i63);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r4 = (Integer) detstackvar(11);
	detstackvar(11) = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_string_0[];
	r1 = (Integer) mercury_data___base_type_info_string_0;
	}
	{
	extern Word * mercury_data___base_type_info_string_0[];
	r2 = (Integer) mercury_data___base_type_info_string_0;
	}
	r3 = (Integer) mkword(mktag(0), (Integer) mercury_data_liveness__common_0);
	detstackvar(12) = (Integer) mkword(mktag(0), (Integer) mercury_data_liveness__common_0);
	{
	Declare_entry(mercury__list__map_3_0);
	call_localret(ENTRY(mercury__list__map_3_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i64,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i64);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r2 = (Integer) detstackvar(11);
	detstackvar(11) = (Integer) r1;
	{
	extern Word * mercury_data___base_type_info_string_0[];
	r1 = (Integer) mercury_data___base_type_info_string_0;
	}
	r4 = (Integer) r2;
	{
	extern Word * mercury_data___base_type_info_string_0[];
	r2 = (Integer) mercury_data___base_type_info_string_0;
	}
	r3 = (Integer) detstackvar(12);
	{
	Declare_entry(mercury__list__map_3_0);
	call_localret(ENTRY(mercury__list__map_3_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i65,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i65);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r2 = (Integer) detstackvar(11);
	detstackvar(11) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__string__append_list_2_0);
	call_localret(ENTRY(mercury__string__append_list_2_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i66,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i66);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r2 = (Integer) detstackvar(11);
	detstackvar(11) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__string__append_list_2_0);
	call_localret(ENTRY(mercury__string__append_list_2_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i67,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i67);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("branches of if-then-else disagree on liveness\nThen: ", 52);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(11);
	tag_incr_hp(r4, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 0)) = string_const("\nElse: ", 7);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) tempr1;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r4;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	{
	Declare_entry(mercury__string__append_list_2_0);
	call_localret(ENTRY(mercury__string__append_list_2_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i68,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i68);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i69,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i69);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1036);
	incr_sp_push_msg(13, "detect_resume_points_in_goal_2");
	detstackvar(13) = (Integer) succip;
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i72);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r4 = (Integer) r5;
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_conj_6_0),
		mercury__liveness__detect_resume_points_in_goal_2_7_0_i73,
		STATIC(mercury__liveness__detect_resume_points_in_goal_2_7_0));
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i73);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i72);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(13);
	decr_sp_pop_msg(13);
	if ((tag((Integer) r1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1035);
	r2 = (Integer) r3;
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1034);
	r2 = (Integer) r3;
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_goal_2_7_0_i1035);
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module17)
	init_entry(mercury__liveness__detect_resume_points_in_conj_6_0);
	init_label(mercury__liveness__detect_resume_points_in_conj_6_0_i4);
	init_label(mercury__liveness__detect_resume_points_in_conj_6_0_i5);
	init_label(mercury__liveness__detect_resume_points_in_conj_6_0_i1002);
BEGIN_CODE

/* code for predicate 'detect_resume_points_in_conj'/6 in mode 0 */
Define_static(mercury__liveness__detect_resume_points_in_conj_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_conj_6_0_i1002);
	incr_sp_push_msg(4, "detect_resume_points_in_conj");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_6_0),
		mercury__liveness__detect_resume_points_in_conj_6_0_i4,
		STATIC(mercury__liveness__detect_resume_points_in_conj_6_0));
Define_label(mercury__liveness__detect_resume_points_in_conj_6_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_conj_6_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__liveness__detect_resume_points_in_conj_6_0,
		LABEL(mercury__liveness__detect_resume_points_in_conj_6_0_i5),
		STATIC(mercury__liveness__detect_resume_points_in_conj_6_0));
Define_label(mercury__liveness__detect_resume_points_in_conj_6_0_i5);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_conj_6_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_conj_6_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module18)
	init_entry(mercury__liveness__detect_resume_points_in_non_disj_7_0);
	init_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i7);
	init_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i8);
	init_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i4);
	init_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i1000);
BEGIN_CODE

/* code for predicate 'detect_resume_points_in_non_disj'/7 in mode 0 */
Define_static(mercury__liveness__detect_resume_points_in_non_disj_7_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_non_disj_7_0_i1000);
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r6 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	incr_sp_push_msg(5, "detect_resume_points_in_non_disj");
	detstackvar(5) = (Integer) succip;
	if (((Integer) r5 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_non_disj_7_0_i4);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r6;
	r1 = (Integer) r5;
	localcall(mercury__liveness__detect_resume_points_in_non_disj_7_0,
		LABEL(mercury__liveness__detect_resume_points_in_non_disj_7_0_i7),
		STATIC(mercury__liveness__detect_resume_points_in_non_disj_7_0));
Define_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_non_disj_7_0));
	r7 = (Integer) r3;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) r2;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = ((Integer) 1);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0),
		mercury__liveness__detect_resume_points_in_non_disj_7_0_i8,
		STATIC(mercury__liveness__detect_resume_points_in_non_disj_7_0));
Define_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_non_disj_7_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
	}
Define_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i4);
	detstackvar(1) = (Integer) r5;
	r1 = (Integer) r6;
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_last_disjunct_7_0),
		mercury__liveness__detect_resume_points_in_non_disj_7_0_i8,
		STATIC(mercury__liveness__detect_resume_points_in_non_disj_7_0));
Define_label(mercury__liveness__detect_resume_points_in_non_disj_7_0_i1000);
	r1 = string_const("empty nondet disjunction", 24);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__liveness__detect_resume_points_in_non_disj_7_0));
	}
END_MODULE

BEGIN_MODULE(mercury__liveness_module19)
	init_entry(mercury__liveness__detect_resume_points_in_pruned_disj_7_0);
	init_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i4);
	init_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i5);
	init_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i10);
	init_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i11);
	init_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i6);
	init_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i12);
	init_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i3);
	init_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i14);
BEGIN_CODE

/* code for predicate 'detect_resume_points_in_pruned_disj'/7 in mode 0 */
Define_static(mercury__liveness__detect_resume_points_in_pruned_disj_7_0);
	incr_sp_push_msg(6, "detect_resume_points_in_pruned_disj");
	detstackvar(6) = (Integer) succip;
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i3);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) tempr1;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_determinism_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_determinism_2_0),
		mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i4,
		STATIC(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
	}
	}
Define_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
	{
	Declare_entry(mercury__hlds_data__determinism_components_3_0);
	call_localret(ENTRY(mercury__hlds_data__determinism_components_3_0),
		mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i5,
		STATIC(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i5);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i6);
	if (((Integer) detstackvar(5) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i6);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	localcall(mercury__liveness__detect_resume_points_in_pruned_disj_7_0,
		LABEL(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i10),
		STATIC(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
Define_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i10);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
	r7 = (Integer) r3;
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) r2;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = ((Integer) 0);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0),
		mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i11,
		STATIC(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
Define_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i11);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
	}
Define_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i6);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_last_disjunct_7_0),
		mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i12,
		STATIC(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
Define_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i12);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r4;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i3);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	{
	Declare_entry(mercury__set__init_1_0);
	call_localret(ENTRY(mercury__set__init_1_0),
		mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i14,
		STATIC(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_pruned_disj_7_0_i14);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_pruned_disj_7_0));
	r3 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module20)
	init_entry(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i2);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i3);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i7);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i5);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i4);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i11);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i10);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i14);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i15);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i16);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i17);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i18);
	init_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i19);
BEGIN_CODE

/* code for predicate 'detect_resume_points_in_non_last_disjunct'/10 in mode 0 */
Define_static(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0);
	incr_sp_push_msg(9, "detect_resume_points_in_non_last_disjunct");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r5;
	detstackvar(6) = (Integer) r7;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) r7;
	r3 = (Integer) r6;
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i2,
		STATIC(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i2);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	r4 = (Integer) r1;
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(5);
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_6_0),
		mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i3,
		STATIC(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i3);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	if (((Integer) detstackvar(2) != ((Integer) 0)))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i4);
	detstackvar(2) = (Integer) r2;
	detstackvar(8) = (Integer) r1;
	{
	Declare_entry(mercury__code_util__cannot_stack_flush_1_0);
	call_localret(ENTRY(mercury__code_util__cannot_stack_flush_1_0),
		mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i7,
		STATIC(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i7);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i5);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(6);
	r1 = (Integer) detstackvar(8);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = ((Integer) 0);
	GOTO_LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i14);
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i5);
	r2 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(8);
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i4);
	detstackvar(2) = (Integer) r2;
	detstackvar(8) = (Integer) r1;
	{
	Declare_entry(mercury__code_util__cannot_fail_before_stack_flush_1_0);
	call_localret(ENTRY(mercury__code_util__cannot_fail_before_stack_flush_1_0),
		mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i11,
		STATIC(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i11);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i10);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(6);
	r1 = (Integer) detstackvar(8);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = ((Integer) 1);
	GOTO_LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i14);
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i10);
	r1 = (Integer) detstackvar(8);
	r3 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(2);
	r6 = (Integer) detstackvar(6);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = ((Integer) 3);
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i14);
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	detstackvar(6) = (Integer) r6;
	{
	Declare_entry(mercury__hlds_goal__goal_set_resume_point_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_set_resume_point_3_0),
		mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i15,
		STATIC(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i15);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_pre_deaths_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_pre_deaths_2_0),
		mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i16,
		STATIC(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i16);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i17,
		STATIC(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i17);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i18,
		STATIC(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i18);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	detstackvar(5) = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 4));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) STATIC(mercury__liveness__LambdaGoal__2_2_0);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r1, ((Integer) 3)) = (Integer) detstackvar(2);
	r2 = string_const("branches of disjunction disagree on liveness", 44);
	{
	Declare_entry(mercury__require__require_2_0);
	call_localret(ENTRY(mercury__require__require_2_0),
		mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i19,
		STATIC(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0_i19);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_non_last_disjunct_10_0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module21)
	init_entry(mercury__liveness__detect_resume_points_in_last_disjunct_7_0);
	init_label(mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i2);
	init_label(mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i3);
	init_label(mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i4);
BEGIN_CODE

/* code for predicate 'detect_resume_points_in_last_disjunct'/7 in mode 0 */
Define_static(mercury__liveness__detect_resume_points_in_last_disjunct_7_0);
	incr_sp_push_msg(4, "detect_resume_points_in_last_disjunct");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_6_0),
		mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i2,
		STATIC(mercury__liveness__detect_resume_points_in_last_disjunct_7_0));
Define_label(mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i2);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_last_disjunct_7_0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_pre_deaths_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_pre_deaths_2_0),
		mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i3,
		STATIC(mercury__liveness__detect_resume_points_in_last_disjunct_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i3);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_last_disjunct_7_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__difference_3_0);
	call_localret(ENTRY(mercury__set__difference_3_0),
		mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i4,
		STATIC(mercury__liveness__detect_resume_points_in_last_disjunct_7_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_last_disjunct_7_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_last_disjunct_7_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module22)
	init_entry(mercury__liveness__detect_resume_points_in_cases_6_0);
	init_label(mercury__liveness__detect_resume_points_in_cases_6_0_i4);
	init_label(mercury__liveness__detect_resume_points_in_cases_6_0_i8);
	init_label(mercury__liveness__detect_resume_points_in_cases_6_0_i1010);
	init_label(mercury__liveness__detect_resume_points_in_cases_6_0_i1011);
	init_label(mercury__liveness__detect_resume_points_in_cases_6_0_i1006);
BEGIN_CODE

/* code for predicate 'detect_resume_points_in_cases'/6 in mode 0 */
Define_static(mercury__liveness__detect_resume_points_in_cases_6_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_cases_6_0_i1006);
	incr_sp_push_msg(6, "detect_resume_points_in_cases");
	detstackvar(6) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	call_localret(STATIC(mercury__liveness__detect_resume_points_in_goal_6_0),
		mercury__liveness__detect_resume_points_in_cases_6_0_i4,
		STATIC(mercury__liveness__detect_resume_points_in_cases_6_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_cases_6_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_cases_6_0));
	tag_incr_hp(r3, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r3, ((Integer) 1)) = (Integer) r1;
	if (((Integer) detstackvar(5) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__detect_resume_points_in_cases_6_0_i1011);
	r4 = (Integer) detstackvar(3);
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) tempr1;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(2);
	localcall(mercury__liveness__detect_resume_points_in_cases_6_0,
		LABEL(mercury__liveness__detect_resume_points_in_cases_6_0_i8),
		STATIC(mercury__liveness__detect_resume_points_in_cases_6_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_cases_6_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_cases_6_0));
	detstackvar(2) = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 4));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) STATIC(mercury__liveness__LambdaGoal__3_2_0);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	r2 = string_const("branches of switch disagree on liveness", 39);
	{
	Declare_entry(mercury__require__require_2_0);
	call_localret(ENTRY(mercury__require__require_2_0),
		mercury__liveness__detect_resume_points_in_cases_6_0_i1010,
		STATIC(mercury__liveness__detect_resume_points_in_cases_6_0));
	}
Define_label(mercury__liveness__detect_resume_points_in_cases_6_0_i1010);
	update_prof_current_proc(LABEL(mercury__liveness__detect_resume_points_in_cases_6_0));
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_cases_6_0_i1011);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__liveness__detect_resume_points_in_cases_6_0_i1006);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module23)
	init_entry(mercury__liveness__initial_liveness_2_6_0);
	init_label(mercury__liveness__initial_liveness_2_6_0_i1012);
	init_label(mercury__liveness__initial_liveness_2_6_0_i10);
	init_label(mercury__liveness__initial_liveness_2_6_0_i11);
	init_label(mercury__liveness__initial_liveness_2_6_0_i9);
	init_label(mercury__liveness__initial_liveness_2_6_0_i13);
	init_label(mercury__liveness__initial_liveness_2_6_0_i1009);
	init_label(mercury__liveness__initial_liveness_2_6_0_i1);
	init_label(mercury__liveness__initial_liveness_2_6_0_i1011);
BEGIN_CODE

/* code for predicate 'initial_liveness_2'/6 in mode 0 */
Define_static(mercury__liveness__initial_liveness_2_6_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__initial_liveness_2_6_0_i1012);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__initial_liveness_2_6_0_i1009);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__initial_liveness_2_6_0_i1009);
	r2 = (Integer) r5;
	r1 = TRUE;
	proceed();
Define_label(mercury__liveness__initial_liveness_2_6_0_i1012);
	incr_sp_push_msg(7, "initial_liveness_2");
	detstackvar(7) = (Integer) succip;
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__initial_liveness_2_6_0_i1);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__initial_liveness_2_6_0_i1);
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r1 = (Integer) r4;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	{
	Declare_entry(mercury__mode_util__mode_to_arg_mode_4_0);
	call_localret(ENTRY(mercury__mode_util__mode_to_arg_mode_4_0),
		mercury__liveness__initial_liveness_2_6_0_i10,
		STATIC(mercury__liveness__initial_liveness_2_6_0));
	}
Define_label(mercury__liveness__initial_liveness_2_6_0_i10);
	update_prof_current_proc(LABEL(mercury__liveness__initial_liveness_2_6_0));
	if ((((Integer) 0) != (Integer) r1))
		GOTO_LABEL(mercury__liveness__initial_liveness_2_6_0_i9);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__liveness__initial_liveness_2_6_0_i11,
		STATIC(mercury__liveness__initial_liveness_2_6_0));
	}
Define_label(mercury__liveness__initial_liveness_2_6_0_i11);
	update_prof_current_proc(LABEL(mercury__liveness__initial_liveness_2_6_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(1);
	localcall(mercury__liveness__initial_liveness_2_6_0,
		LABEL(mercury__liveness__initial_liveness_2_6_0_i13),
		STATIC(mercury__liveness__initial_liveness_2_6_0));
Define_label(mercury__liveness__initial_liveness_2_6_0_i9);
	r4 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(6);
	r5 = (Integer) detstackvar(2);
	localcall(mercury__liveness__initial_liveness_2_6_0,
		LABEL(mercury__liveness__initial_liveness_2_6_0_i13),
		STATIC(mercury__liveness__initial_liveness_2_6_0));
Define_label(mercury__liveness__initial_liveness_2_6_0_i13);
	update_prof_current_proc(LABEL(mercury__liveness__initial_liveness_2_6_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__initial_liveness_2_6_0_i1011);
	r1 = TRUE;
	proceed();
Define_label(mercury__liveness__initial_liveness_2_6_0_i1009);
	r1 = FALSE;
	proceed();
Define_label(mercury__liveness__initial_liveness_2_6_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__liveness__initial_liveness_2_6_0_i1011);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module24)
	init_entry(mercury__liveness__initial_deadness_2_6_0);
	init_label(mercury__liveness__initial_deadness_2_6_0_i1012);
	init_label(mercury__liveness__initial_deadness_2_6_0_i10);
	init_label(mercury__liveness__initial_deadness_2_6_0_i11);
	init_label(mercury__liveness__initial_deadness_2_6_0_i9);
	init_label(mercury__liveness__initial_deadness_2_6_0_i13);
	init_label(mercury__liveness__initial_deadness_2_6_0_i1009);
	init_label(mercury__liveness__initial_deadness_2_6_0_i1);
	init_label(mercury__liveness__initial_deadness_2_6_0_i1011);
BEGIN_CODE

/* code for predicate 'initial_deadness_2'/6 in mode 0 */
Define_static(mercury__liveness__initial_deadness_2_6_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__initial_deadness_2_6_0_i1012);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__initial_deadness_2_6_0_i1009);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__initial_deadness_2_6_0_i1009);
	r2 = (Integer) r5;
	r1 = TRUE;
	proceed();
Define_label(mercury__liveness__initial_deadness_2_6_0_i1012);
	incr_sp_push_msg(7, "initial_deadness_2");
	detstackvar(7) = (Integer) succip;
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__initial_deadness_2_6_0_i1);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__initial_deadness_2_6_0_i1);
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r1 = (Integer) r4;
	detstackvar(1) = (Integer) r4;
	detstackvar(2) = (Integer) r5;
	{
	Declare_entry(mercury__mode_util__mode_to_arg_mode_4_0);
	call_localret(ENTRY(mercury__mode_util__mode_to_arg_mode_4_0),
		mercury__liveness__initial_deadness_2_6_0_i10,
		STATIC(mercury__liveness__initial_deadness_2_6_0));
	}
Define_label(mercury__liveness__initial_deadness_2_6_0_i10);
	update_prof_current_proc(LABEL(mercury__liveness__initial_deadness_2_6_0));
	if ((((Integer) 1) != (Integer) r1))
		GOTO_LABEL(mercury__liveness__initial_deadness_2_6_0_i9);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__liveness__initial_deadness_2_6_0_i11,
		STATIC(mercury__liveness__initial_deadness_2_6_0));
	}
Define_label(mercury__liveness__initial_deadness_2_6_0_i11);
	update_prof_current_proc(LABEL(mercury__liveness__initial_deadness_2_6_0));
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(6);
	r4 = (Integer) detstackvar(1);
	localcall(mercury__liveness__initial_deadness_2_6_0,
		LABEL(mercury__liveness__initial_deadness_2_6_0_i13),
		STATIC(mercury__liveness__initial_deadness_2_6_0));
Define_label(mercury__liveness__initial_deadness_2_6_0_i9);
	r4 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(5);
	r3 = (Integer) detstackvar(6);
	r5 = (Integer) detstackvar(2);
	localcall(mercury__liveness__initial_deadness_2_6_0,
		LABEL(mercury__liveness__initial_deadness_2_6_0_i13),
		STATIC(mercury__liveness__initial_deadness_2_6_0));
Define_label(mercury__liveness__initial_deadness_2_6_0_i13);
	update_prof_current_proc(LABEL(mercury__liveness__initial_deadness_2_6_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__liveness__initial_deadness_2_6_0_i1011);
	r1 = TRUE;
	proceed();
Define_label(mercury__liveness__initial_deadness_2_6_0_i1009);
	r1 = FALSE;
	proceed();
Define_label(mercury__liveness__initial_deadness_2_6_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__liveness__initial_deadness_2_6_0_i1011);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module25)
	init_entry(mercury__liveness__stuff_liveness_residue_after_goal_3_0);
	init_label(mercury__liveness__stuff_liveness_residue_after_goal_3_0_i2);
	init_label(mercury__liveness__stuff_liveness_residue_after_goal_3_0_i3);
	init_label(mercury__liveness__stuff_liveness_residue_after_goal_3_0_i4);
BEGIN_CODE

/* code for predicate 'stuff_liveness_residue_after_goal'/3 in mode 0 */
Define_static(mercury__liveness__stuff_liveness_residue_after_goal_3_0);
	incr_sp_push_msg(4, "stuff_liveness_residue_after_goal");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_post_births_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_post_births_2_0),
		mercury__liveness__stuff_liveness_residue_after_goal_3_0_i2,
		STATIC(mercury__liveness__stuff_liveness_residue_after_goal_3_0));
	}
Define_label(mercury__liveness__stuff_liveness_residue_after_goal_3_0_i2);
	update_prof_current_proc(LABEL(mercury__liveness__stuff_liveness_residue_after_goal_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__stuff_liveness_residue_after_goal_3_0_i3,
		STATIC(mercury__liveness__stuff_liveness_residue_after_goal_3_0));
	}
Define_label(mercury__liveness__stuff_liveness_residue_after_goal_3_0_i3);
	update_prof_current_proc(LABEL(mercury__liveness__stuff_liveness_residue_after_goal_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_post_births_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_post_births_3_0),
		mercury__liveness__stuff_liveness_residue_after_goal_3_0_i4,
		STATIC(mercury__liveness__stuff_liveness_residue_after_goal_3_0));
	}
Define_label(mercury__liveness__stuff_liveness_residue_after_goal_3_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__stuff_liveness_residue_after_goal_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module26)
	init_entry(mercury__liveness__stuff_deadness_residue_before_goal_3_0);
	init_label(mercury__liveness__stuff_deadness_residue_before_goal_3_0_i2);
	init_label(mercury__liveness__stuff_deadness_residue_before_goal_3_0_i3);
	init_label(mercury__liveness__stuff_deadness_residue_before_goal_3_0_i4);
BEGIN_CODE

/* code for predicate 'stuff_deadness_residue_before_goal'/3 in mode 0 */
Define_static(mercury__liveness__stuff_deadness_residue_before_goal_3_0);
	incr_sp_push_msg(4, "stuff_deadness_residue_before_goal");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	detstackvar(1) = (Integer) r2;
	{
	Declare_entry(mercury__hlds_goal__goal_info_get_pre_deaths_2_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_get_pre_deaths_2_0),
		mercury__liveness__stuff_deadness_residue_before_goal_3_0_i2,
		STATIC(mercury__liveness__stuff_deadness_residue_before_goal_3_0));
	}
Define_label(mercury__liveness__stuff_deadness_residue_before_goal_3_0_i2);
	update_prof_current_proc(LABEL(mercury__liveness__stuff_deadness_residue_before_goal_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r3 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__set__union_3_0);
	call_localret(ENTRY(mercury__set__union_3_0),
		mercury__liveness__stuff_deadness_residue_before_goal_3_0_i3,
		STATIC(mercury__liveness__stuff_deadness_residue_before_goal_3_0));
	}
Define_label(mercury__liveness__stuff_deadness_residue_before_goal_3_0_i3);
	update_prof_current_proc(LABEL(mercury__liveness__stuff_deadness_residue_before_goal_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__hlds_goal__goal_info_set_pre_deaths_3_0);
	call_localret(ENTRY(mercury__hlds_goal__goal_info_set_pre_deaths_3_0),
		mercury__liveness__stuff_deadness_residue_before_goal_3_0_i4,
		STATIC(mercury__liveness__stuff_deadness_residue_before_goal_3_0));
	}
Define_label(mercury__liveness__stuff_deadness_residue_before_goal_3_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__stuff_deadness_residue_before_goal_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module27)
	init_entry(mercury__liveness__find_binding_occurrences_5_0);
	init_label(mercury__liveness__find_binding_occurrences_5_0_i4);
	init_label(mercury__liveness__find_binding_occurrences_5_0_i5);
	init_label(mercury__liveness__find_binding_occurrences_5_0_i8);
	init_label(mercury__liveness__find_binding_occurrences_5_0_i9);
	init_label(mercury__liveness__find_binding_occurrences_5_0_i7);
	init_label(mercury__liveness__find_binding_occurrences_5_0_i1003);
BEGIN_CODE

/* code for predicate 'find_binding_occurrences'/5 in mode 0 */
Define_static(mercury__liveness__find_binding_occurrences_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__liveness__find_binding_occurrences_5_0_i1003);
	incr_sp_push_msg(8, "find_binding_occurrences");
	detstackvar(8) = (Integer) succip;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(4) = (Integer) r4;
	r3 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(1) = (Integer) r2;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) mercury_data_mercury_builtin__base_type_info_term_0;
	{
	Declare_entry(mercury__map__lookup_3_1);
	call_localret(ENTRY(mercury__map__lookup_3_1),
		mercury__liveness__find_binding_occurrences_5_0_i4,
		STATIC(mercury__liveness__find_binding_occurrences_5_0));
	}
Define_label(mercury__liveness__find_binding_occurrences_5_0_i4);
	update_prof_current_proc(LABEL(mercury__liveness__find_binding_occurrences_5_0));
	detstackvar(7) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__instmap__instmap_delta_lookup_var_3_0);
	call_localret(ENTRY(mercury__instmap__instmap_delta_lookup_var_3_0),
		mercury__liveness__find_binding_occurrences_5_0_i5,
		STATIC(mercury__liveness__find_binding_occurrences_5_0));
	}
Define_label(mercury__liveness__find_binding_occurrences_5_0_i5);
	update_prof_current_proc(LABEL(mercury__liveness__find_binding_occurrences_5_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 2));
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(7);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__mode_util__mode_to_arg_mode_4_0);
	call_localret(ENTRY(mercury__mode_util__mode_to_arg_mode_4_0),
		mercury__liveness__find_binding_occurrences_5_0_i8,
		STATIC(mercury__liveness__find_binding_occurrences_5_0));
	}
Define_label(mercury__liveness__find_binding_occurrences_5_0_i8);
	update_prof_current_proc(LABEL(mercury__liveness__find_binding_occurrences_5_0));
	if ((((Integer) 1) != (Integer) r1))
		GOTO_LABEL(mercury__liveness__find_binding_occurrences_5_0_i7);
	r1 = (Integer) mercury_data_mercury_builtin__base_type_info_var_0;
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__set__insert_3_1);
	call_localret(ENTRY(mercury__set__insert_3_1),
		mercury__liveness__find_binding_occurrences_5_0_i9,
		STATIC(mercury__liveness__find_binding_occurrences_5_0));
	}
Define_label(mercury__liveness__find_binding_occurrences_5_0_i9);
	update_prof_current_proc(LABEL(mercury__liveness__find_binding_occurrences_5_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__liveness__find_binding_occurrences_5_0,
		STATIC(mercury__liveness__find_binding_occurrences_5_0));
Define_label(mercury__liveness__find_binding_occurrences_5_0_i7);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r1 = (Integer) detstackvar(5);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__liveness__find_binding_occurrences_5_0,
		STATIC(mercury__liveness__find_binding_occurrences_5_0));
Define_label(mercury__liveness__find_binding_occurrences_5_0_i1003);
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__liveness_module28)
	init_entry(mercury__liveness__live_info_get_varset_2_0);
BEGIN_CODE

/* code for predicate 'live_info_get_varset'/2 in mode 0 */
Define_static(mercury__liveness__live_info_get_varset_2_0);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	proceed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__liveness_bunch_0(void)
{
	mercury__liveness_module0();
	mercury__liveness_module1();
	mercury__liveness_module2();
	mercury__liveness_module3();
	mercury__liveness_module4();
	mercury__liveness_module5();
	mercury__liveness_module6();
	mercury__liveness_module7();
	mercury__liveness_module8();
	mercury__liveness_module9();
	mercury__liveness_module10();
	mercury__liveness_module11();
	mercury__liveness_module12();
	mercury__liveness_module13();
	mercury__liveness_module14();
	mercury__liveness_module15();
	mercury__liveness_module16();
	mercury__liveness_module17();
	mercury__liveness_module18();
	mercury__liveness_module19();
	mercury__liveness_module20();
	mercury__liveness_module21();
	mercury__liveness_module22();
	mercury__liveness_module23();
	mercury__liveness_module24();
	mercury__liveness_module25();
	mercury__liveness_module26();
	mercury__liveness_module27();
	mercury__liveness_module28();
}

#endif

void mercury__liveness__init(void); /* suppress gcc warning */
void mercury__liveness__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__liveness_bunch_0();
#endif
}
