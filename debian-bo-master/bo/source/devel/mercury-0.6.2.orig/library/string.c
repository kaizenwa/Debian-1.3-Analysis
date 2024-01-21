/*
** Automatically generated from `string.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__string__init
ENDINIT
*/

#include "imp.h"

#include <string.h>
#include <stdio.h>


Declare_static(mercury__string__format_add_sign__ua10000_6_0);
Declare_label(mercury__string__format_add_sign__ua10000_6_0_i4);
Declare_label(mercury__string__format_add_sign__ua10000_6_0_i3);
Declare_label(mercury__string__format_add_sign__ua10000_6_0_i6);
Declare_label(mercury__string__format_add_sign__ua10000_6_0_i9);
Declare_label(mercury__string__format_add_sign__ua10000_6_0_i11);
Declare_label(mercury__string__format_add_sign__ua10000_6_0_i12);
Declare_label(mercury__string__format_add_sign__ua10000_6_0_i8);
Declare_label(mercury__string__format_add_sign__ua10000_6_0_i15);
Declare_label(mercury__string__format_add_sign__ua10000_6_0_i14);
Define_extern_entry(mercury__string__length_2_0);
Define_extern_entry(mercury__string__append_3_0);
Declare_label(mercury__string__append_3_0_i1);
Define_extern_entry(mercury__string__append_3_1);
Declare_label(mercury__string__append_3_1_i1);
Define_extern_entry(mercury__string__append_3_2);
Define_extern_entry(mercury__string__append_3_3);
Define_extern_entry(mercury__string__remove_suffix_3_0);
Declare_label(mercury__string__remove_suffix_3_0_i2);
Declare_label(mercury__string__remove_suffix_3_0_i3);
Declare_label(mercury__string__remove_suffix_3_0_i4);
Declare_label(mercury__string__remove_suffix_3_0_i6);
Declare_label(mercury__string__remove_suffix_3_0_i1);
Define_extern_entry(mercury__string__prefix_2_0);
Define_extern_entry(mercury__string__prefix_2_1);
Declare_label(mercury__string__prefix_2_1_i1);
Define_extern_entry(mercury__string__char_to_string_2_1);
Declare_label(mercury__string__char_to_string_2_1_i2);
Declare_label(mercury__string__char_to_string_2_1_i4);
Declare_label(mercury__string__char_to_string_2_1_i6);
Declare_label(mercury__string__char_to_string_2_1_i1);
Declare_label(mercury__string__char_to_string_2_1_i1000);
Define_extern_entry(mercury__string__char_to_string_2_0);
Declare_label(mercury__string__char_to_string_2_0_i2);
Define_extern_entry(mercury__string__int_to_string_2_0);
Define_extern_entry(mercury__string__int_to_base_string_3_0);
Declare_label(mercury__string__int_to_base_string_3_0_i1002);
Declare_label(mercury__string__int_to_base_string_3_0_i2);
Declare_label(mercury__string__int_to_base_string_3_0_i4);
Declare_label(mercury__string__int_to_base_string_3_0_i1003);
Declare_label(mercury__string__int_to_base_string_3_0_i5);
Declare_label(mercury__string__int_to_base_string_3_0_i8);
Declare_label(mercury__string__int_to_base_string_3_0_i6);
Define_extern_entry(mercury__string__float_to_string_2_0);
Define_extern_entry(mercury__string__first_char_3_0);
Declare_label(mercury__string__first_char_3_0_i1);
Define_extern_entry(mercury__string__first_char_3_1);
Declare_label(mercury__string__first_char_3_1_i1);
Define_extern_entry(mercury__string__first_char_3_2);
Declare_label(mercury__string__first_char_3_2_i1);
Define_extern_entry(mercury__string__first_char_3_3);
Declare_label(mercury__string__first_char_3_3_i1);
Define_extern_entry(mercury__string__first_char_3_4);
Define_extern_entry(mercury__string__replace_4_0);
Declare_label(mercury__string__replace_4_0_i2);
Declare_label(mercury__string__replace_4_0_i3);
Declare_label(mercury__string__replace_4_0_i4);
Declare_label(mercury__string__replace_4_0_i6);
Declare_label(mercury__string__replace_4_0_i7);
Declare_label(mercury__string__replace_4_0_i8);
Declare_label(mercury__string__replace_4_0_i9);
Declare_label(mercury__string__replace_4_0_i1);
Define_extern_entry(mercury__string__replace_all_4_0);
Declare_label(mercury__string__replace_all_4_0_i2);
Declare_label(mercury__string__replace_all_4_0_i3);
Declare_label(mercury__string__replace_all_4_0_i4);
Declare_label(mercury__string__replace_all_4_0_i5);
Define_extern_entry(mercury__string__to_lower_2_1);
Declare_label(mercury__string__to_lower_2_1_i2);
Declare_label(mercury__string__to_lower_2_1_i3);
Declare_label(mercury__string__to_lower_2_1_i4);
Declare_label(mercury__string__to_lower_2_1_i1);
Define_extern_entry(mercury__string__to_lower_2_0);
Declare_label(mercury__string__to_lower_2_0_i2);
Declare_label(mercury__string__to_lower_2_0_i3);
Define_extern_entry(mercury__string__to_upper_2_1);
Declare_label(mercury__string__to_upper_2_1_i2);
Declare_label(mercury__string__to_upper_2_1_i3);
Declare_label(mercury__string__to_upper_2_1_i4);
Declare_label(mercury__string__to_upper_2_1_i1);
Define_extern_entry(mercury__string__to_upper_2_0);
Declare_label(mercury__string__to_upper_2_0_i2);
Declare_label(mercury__string__to_upper_2_0_i3);
Define_extern_entry(mercury__string__capitalize_first_2_0);
Declare_label(mercury__string__capitalize_first_2_0_i4);
Declare_label(mercury__string__capitalize_first_2_0_i6);
Declare_label(mercury__string__capitalize_first_2_0_i3);
Define_extern_entry(mercury__string__uncapitalize_first_2_0);
Declare_label(mercury__string__uncapitalize_first_2_0_i4);
Declare_label(mercury__string__uncapitalize_first_2_0_i6);
Declare_label(mercury__string__uncapitalize_first_2_0_i3);
Define_extern_entry(mercury__string__to_char_list_2_0);
Declare_label(mercury__string__to_char_list_2_0_i2);
Define_extern_entry(mercury__string__from_char_list_2_1);
Declare_label(mercury__string__from_char_list_2_1_i2);
Declare_label(mercury__string__from_char_list_2_1_i3);
Declare_label(mercury__string__from_char_list_2_1_i1000);
Define_extern_entry(mercury__string__from_char_list_2_0);
Declare_label(mercury__string__from_char_list_2_0_i2);
Define_extern_entry(mercury__string__to_int_2_0);
Declare_label(mercury__string__to_int_2_0_i2);
Declare_label(mercury__string__to_int_2_0_i1000);
Define_extern_entry(mercury__string__base_string_to_int_3_0);
Declare_label(mercury__string__base_string_to_int_3_0_i4);
Declare_label(mercury__string__base_string_to_int_3_0_i9);
Declare_label(mercury__string__base_string_to_int_3_0_i6);
Declare_label(mercury__string__base_string_to_int_3_0_i14);
Declare_label(mercury__string__base_string_to_int_3_0_i11);
Declare_label(mercury__string__base_string_to_int_3_0_i3);
Declare_label(mercury__string__base_string_to_int_3_0_i1000);
Declare_label(mercury__string__base_string_to_int_3_0_i1001);
Define_extern_entry(mercury__string__to_float_2_0);
Declare_label(mercury__string__to_float_2_0_i1);
Define_extern_entry(mercury__string__is_alpha_1_0);
Declare_label(mercury__string__is_alpha_1_0_i4);
Declare_label(mercury__string__is_alpha_1_0_i6);
Declare_label(mercury__string__is_alpha_1_0_i3);
Declare_label(mercury__string__is_alpha_1_0_i1);
Define_extern_entry(mercury__string__is_alpha_or_underscore_1_0);
Declare_label(mercury__string__is_alpha_or_underscore_1_0_i4);
Declare_label(mercury__string__is_alpha_or_underscore_1_0_i6);
Declare_label(mercury__string__is_alpha_or_underscore_1_0_i3);
Declare_label(mercury__string__is_alpha_or_underscore_1_0_i1);
Define_extern_entry(mercury__string__is_alnum_or_underscore_1_0);
Declare_label(mercury__string__is_alnum_or_underscore_1_0_i4);
Declare_label(mercury__string__is_alnum_or_underscore_1_0_i6);
Declare_label(mercury__string__is_alnum_or_underscore_1_0_i3);
Declare_label(mercury__string__is_alnum_or_underscore_1_0_i1);
Define_extern_entry(mercury__string__pad_left_4_0);
Declare_label(mercury__string__pad_left_4_0_i2);
Declare_label(mercury__string__pad_left_4_0_i5);
Declare_label(mercury__string__pad_left_4_0_i3);
Define_extern_entry(mercury__string__pad_right_4_0);
Declare_label(mercury__string__pad_right_4_0_i2);
Declare_label(mercury__string__pad_right_4_0_i5);
Declare_label(mercury__string__pad_right_4_0_i3);
Define_extern_entry(mercury__string__duplicate_char_3_0);
Declare_label(mercury__string__duplicate_char_3_0_i1000);
Declare_label(mercury__string__duplicate_char_3_0_i4);
Define_extern_entry(mercury__string__index_3_0);
Declare_label(mercury__string__index_3_0_i1);
Define_extern_entry(mercury__string__index_det_3_0);
Declare_label(mercury__string__index_det_3_0_i4);
Declare_label(mercury__string__index_det_3_0_i1000);
Define_extern_entry(mercury__string__split_4_0);
Define_extern_entry(mercury__string__left_3_0);
Define_extern_entry(mercury__string__right_3_0);
Declare_label(mercury__string__right_3_0_i2);
Declare_label(mercury__string__right_3_0_i3);
Define_extern_entry(mercury__string__substring_4_0);
Declare_label(mercury__string__substring_4_0_i2);
Define_extern_entry(mercury__string__append_list_2_0);
Declare_label(mercury__string__append_list_2_0_i4);
Declare_label(mercury__string__append_list_2_0_i1002);
Define_extern_entry(mercury__string__append_list_2_1);
Declare_label(mercury__string__append_list_2_1_i3);
Declare_label(mercury__string__append_list_2_1_i1007);
Declare_label(mercury__string__append_list_2_1_i5);
Declare_label(mercury__string__append_list_2_1_i6);
Define_extern_entry(mercury__string__hash_2_0);
Declare_label(mercury__string__hash_2_0_i2);
Declare_label(mercury__string__hash_2_0_i3);
Declare_label(mercury__string__hash_2_0_i4);
Define_extern_entry(mercury__string__sub_string_search_3_0);
Declare_label(mercury__string__sub_string_search_3_0_i2);
Declare_label(mercury__string__sub_string_search_3_0_i3);
Declare_label(mercury__string__sub_string_search_3_0_i4);
Declare_label(mercury__string__sub_string_search_3_0_i1000);
Define_extern_entry(mercury__string__format_3_0);
Declare_label(mercury__string__format_3_0_i2);
Declare_static(mercury__string__to_int_list_2_0);
Declare_static(mercury__string__to_int_list_2_1);
Declare_static(mercury__string__find_all_sub_charlist_4_0);
Declare_label(mercury__string__find_all_sub_charlist_4_0_i4);
Declare_label(mercury__string__find_all_sub_charlist_4_0_i6);
Declare_label(mercury__string__find_all_sub_charlist_4_0_i10);
Declare_label(mercury__string__find_all_sub_charlist_4_0_i11);
Declare_label(mercury__string__find_all_sub_charlist_4_0_i3);
Declare_static(mercury__string__find_sub_charlist_4_0);
Declare_label(mercury__string__find_sub_charlist_4_0_i1001);
Declare_label(mercury__string__find_sub_charlist_4_0_i9);
Declare_label(mercury__string__find_sub_charlist_4_0_i8);
Declare_label(mercury__string__find_sub_charlist_4_0_i11);
Declare_label(mercury__string__find_sub_charlist_4_0_i5);
Declare_label(mercury__string__find_sub_charlist_4_0_i14);
Declare_label(mercury__string__find_sub_charlist_4_0_i1);
Declare_static(mercury__string__base_string_to_int_2_4_0);
Declare_label(mercury__string__base_string_to_int_2_4_0_i4);
Declare_label(mercury__string__base_string_to_int_2_4_0_i6);
Declare_label(mercury__string__base_string_to_int_2_4_0_i8);
Declare_label(mercury__string__base_string_to_int_2_4_0_i3);
Declare_label(mercury__string__base_string_to_int_2_4_0_i1);
Declare_label(mercury__string__base_string_to_int_2_4_0_i1000);
Declare_static(mercury__string__int_to_base_string_2_3_0);
Declare_label(mercury__string__int_to_base_string_2_3_0_i4);
Declare_label(mercury__string__int_to_base_string_2_3_0_i1001);
Declare_label(mercury__string__int_to_base_string_2_3_0_i6);
Declare_label(mercury__string__int_to_base_string_2_3_0_i7);
Declare_label(mercury__string__int_to_base_string_2_3_0_i8);
Declare_static(mercury__string__digit_to_char_det_2_0);
Declare_label(mercury__string__digit_to_char_det_2_0_i1003);
Declare_static(mercury__string__int_list_to_char_list_2_0);
Declare_label(mercury__string__int_list_to_char_list_2_0_i6);
Declare_label(mercury__string__int_list_to_char_list_2_0_i5);
Declare_label(mercury__string__int_list_to_char_list_2_0_i8);
Declare_label(mercury__string__int_list_to_char_list_2_0_i9);
Declare_label(mercury__string__int_list_to_char_list_2_0_i10);
Declare_label(mercury__string__int_list_to_char_list_2_0_i1003);
Declare_static(mercury__string__char_list_to_int_list_2_1);
Declare_label(mercury__string__char_list_to_int_list_2_1_i1001);
Declare_label(mercury__string__char_list_to_int_list_2_1_i4);
Declare_label(mercury__string__char_list_to_int_list_2_1_i6);
Declare_label(mercury__string__char_list_to_int_list_2_1_i1);
Declare_static(mercury__string__char_list_to_int_list_2_0);
Declare_label(mercury__string__char_list_to_int_list_2_0_i4);
Declare_label(mercury__string__char_list_to_int_list_2_0_i5);
Declare_label(mercury__string__char_list_to_int_list_2_0_i1002);
Declare_static(mercury__string__char_list_to_upper_2_0);
Declare_label(mercury__string__char_list_to_upper_2_0_i4);
Declare_label(mercury__string__char_list_to_upper_2_0_i5);
Declare_label(mercury__string__char_list_to_upper_2_0_i1002);
Declare_static(mercury__string__char_list_to_lower_2_0);
Declare_label(mercury__string__char_list_to_lower_2_0_i4);
Declare_label(mercury__string__char_list_to_lower_2_0_i5);
Declare_label(mercury__string__char_list_to_lower_2_0_i1002);
Declare_static(mercury__string__hash_2_3_0);
Declare_label(mercury__string__hash_2_3_0_i3);
Declare_label(mercury__string__hash_2_3_0_i1);
Declare_static(mercury__string__sub_string_search2_5_0);
Declare_label(mercury__string__sub_string_search2_5_0_i4);
Declare_label(mercury__string__sub_string_search2_5_0_i3);
Declare_label(mercury__string__sub_string_search2_5_0_i6);
Declare_label(mercury__string__sub_string_search2_5_0_i8);
Declare_label(mercury__string__sub_string_search2_5_0_i1005);
Declare_label(mercury__string__sub_string_search2_5_0_i1);
Declare_label(mercury__string__sub_string_search2_5_0_i1007);
Declare_static(mercury__string__format_2_3_0);
Declare_label(mercury__string__format_2_3_0_i11);
Declare_label(mercury__string__format_2_3_0_i1011);
Declare_label(mercury__string__format_2_3_0_i15);
Declare_label(mercury__string__format_2_3_0_i17);
Declare_label(mercury__string__format_2_3_0_i14);
Declare_label(mercury__string__format_2_3_0_i1010);
Declare_label(mercury__string__format_2_3_0_i22);
Declare_label(mercury__string__format_2_3_0_i1006);
Declare_static(mercury__string__format_top_convert_variable_5_0);
Declare_label(mercury__string__format_top_convert_variable_5_0_i1001);
Declare_label(mercury__string__format_top_convert_variable_5_0_i6);
Declare_label(mercury__string__format_top_convert_variable_5_0_i8);
Declare_label(mercury__string__format_top_convert_variable_5_0_i9);
Declare_label(mercury__string__format_top_convert_variable_5_0_i13);
Declare_label(mercury__string__format_top_convert_variable_5_0_i16);
Declare_label(mercury__string__format_top_convert_variable_5_0_i17);
Declare_label(mercury__string__format_top_convert_variable_5_0_i21);
Declare_label(mercury__string__format_top_convert_variable_5_0_i24);
Declare_label(mercury__string__format_top_convert_variable_5_0_i25);
Declare_label(mercury__string__format_top_convert_variable_5_0_i28);
Declare_label(mercury__string__format_top_convert_variable_5_0_i29);
Declare_label(mercury__string__format_top_convert_variable_5_0_i38);
Declare_label(mercury__string__format_top_convert_variable_5_0_i39);
Declare_label(mercury__string__format_top_convert_variable_5_0_i40);
Declare_label(mercury__string__format_top_convert_variable_5_0_i41);
Declare_label(mercury__string__format_top_convert_variable_5_0_i35);
Declare_label(mercury__string__format_top_convert_variable_5_0_i44);
Declare_label(mercury__string__format_top_convert_variable_5_0_i32);
Declare_label(mercury__string__format_top_convert_variable_5_0_i52);
Declare_label(mercury__string__format_top_convert_variable_5_0_i53);
Declare_label(mercury__string__format_top_convert_variable_5_0_i54);
Declare_label(mercury__string__format_top_convert_variable_5_0_i55);
Declare_label(mercury__string__format_top_convert_variable_5_0_i49);
Declare_label(mercury__string__format_top_convert_variable_5_0_i46);
Declare_label(mercury__string__format_top_convert_variable_5_0_i62);
Declare_label(mercury__string__format_top_convert_variable_5_0_i63);
Declare_label(mercury__string__format_top_convert_variable_5_0_i66);
Declare_label(mercury__string__format_top_convert_variable_5_0_i69);
Declare_label(mercury__string__format_top_convert_variable_5_0_i74);
Declare_label(mercury__string__format_top_convert_variable_5_0_i75);
Declare_label(mercury__string__format_top_convert_variable_5_0_i76);
Declare_label(mercury__string__format_top_convert_variable_5_0_i77);
Declare_label(mercury__string__format_top_convert_variable_5_0_i1);
Declare_label(mercury__string__format_top_convert_variable_5_0_i1000);
Declare_static(mercury__string__format_do_conversion_6_0);
Declare_label(mercury__string__format_do_conversion_6_0_i4);
Declare_label(mercury__string__format_do_conversion_6_0_i3);
Declare_static(mercury__string__do_conversion_0_6_0);
Declare_label(mercury__string__do_conversion_0_6_0_i5);
Declare_label(mercury__string__do_conversion_0_6_0_i6);
Declare_label(mercury__string__do_conversion_0_6_0_i7);
Declare_label(mercury__string__do_conversion_0_6_0_i1006);
Declare_label(mercury__string__do_conversion_0_6_0_i15);
Declare_label(mercury__string__do_conversion_0_6_0_i12);
Declare_label(mercury__string__do_conversion_0_6_0_i17);
Declare_label(mercury__string__do_conversion_0_6_0_i18);
Declare_label(mercury__string__do_conversion_0_6_0_i19);
Declare_label(mercury__string__do_conversion_0_6_0_i23);
Declare_label(mercury__string__do_conversion_0_6_0_i25);
Declare_label(mercury__string__do_conversion_0_6_0_i22);
Declare_label(mercury__string__do_conversion_0_6_0_i27);
Declare_label(mercury__string__do_conversion_0_6_0_i10);
Declare_label(mercury__string__do_conversion_0_6_0_i31);
Declare_label(mercury__string__do_conversion_0_6_0_i36);
Declare_label(mercury__string__do_conversion_0_6_0_i37);
Declare_label(mercury__string__do_conversion_0_6_0_i39);
Declare_label(mercury__string__do_conversion_0_6_0_i34);
Declare_label(mercury__string__do_conversion_0_6_0_i42);
Declare_label(mercury__string__do_conversion_0_6_0_i50);
Declare_label(mercury__string__do_conversion_0_6_0_i51);
Declare_label(mercury__string__do_conversion_0_6_0_i48);
Declare_label(mercury__string__do_conversion_0_6_0_i60);
Declare_label(mercury__string__do_conversion_0_6_0_i57);
Declare_label(mercury__string__do_conversion_0_6_0_i62);
Declare_label(mercury__string__do_conversion_0_6_0_i63);
Declare_label(mercury__string__do_conversion_0_6_0_i67);
Declare_label(mercury__string__do_conversion_0_6_0_i69);
Declare_label(mercury__string__do_conversion_0_6_0_i66);
Declare_label(mercury__string__do_conversion_0_6_0_i71);
Declare_label(mercury__string__do_conversion_0_6_0_i55);
Declare_label(mercury__string__do_conversion_0_6_0_i77);
Declare_label(mercury__string__do_conversion_0_6_0_i75);
Declare_label(mercury__string__do_conversion_0_6_0_i86);
Declare_label(mercury__string__do_conversion_0_6_0_i81);
Declare_label(mercury__string__do_conversion_0_6_0_i90);
Declare_label(mercury__string__do_conversion_0_6_0_i91);
Declare_label(mercury__string__do_conversion_0_6_0_i92);
Declare_label(mercury__string__do_conversion_0_6_0_i88);
Declare_label(mercury__string__do_conversion_0_6_0_i99);
Declare_label(mercury__string__do_conversion_0_6_0_i96);
Declare_label(mercury__string__do_conversion_0_6_0_i101);
Declare_label(mercury__string__do_conversion_0_6_0_i102);
Declare_label(mercury__string__do_conversion_0_6_0_i103);
Declare_label(mercury__string__do_conversion_0_6_0_i107);
Declare_label(mercury__string__do_conversion_0_6_0_i109);
Declare_label(mercury__string__do_conversion_0_6_0_i106);
Declare_label(mercury__string__do_conversion_0_6_0_i111);
Declare_label(mercury__string__do_conversion_0_6_0_i1);
Declare_label(mercury__string__do_conversion_0_6_0_i1002);
Declare_label(mercury__string__do_conversion_0_6_0_i1004);
Declare_label(mercury__string__do_conversion_0_6_0_i1005);
Declare_static(mercury__string__do_conversion_fail_1_0);
Declare_label(mercury__string__do_conversion_fail_1_0_i2);
Declare_label(mercury__string__do_conversion_fail_1_0_i3);
Declare_static(mercury__string__format_int_precision_4_0);
Declare_label(mercury__string__format_int_precision_4_0_i1002);
Declare_label(mercury__string__format_int_precision_4_0_i5);
Declare_label(mercury__string__format_int_precision_4_0_i6);
Declare_label(mercury__string__format_int_precision_4_0_i9);
Declare_label(mercury__string__format_int_precision_4_0_i8);
Declare_label(mercury__string__format_int_precision_4_0_i11);
Declare_label(mercury__string__format_int_precision_4_0_i14);
Declare_label(mercury__string__format_int_precision_4_0_i15);
Declare_label(mercury__string__format_int_precision_4_0_i25);
Declare_label(mercury__string__format_int_precision_4_0_i17);
Declare_label(mercury__string__format_int_precision_4_0_i26);
Declare_label(mercury__string__format_int_precision_4_0_i12);
Declare_label(mercury__string__format_int_precision_4_0_i1);
Declare_static(mercury__string__format_calc_exp_4_0);
Declare_label(mercury__string__format_calc_exp_4_0_i4);
Declare_label(mercury__string__format_calc_exp_4_0_i1001);
Declare_label(mercury__string__format_calc_exp_4_0_i6);
Declare_label(mercury__string__format_calc_exp_4_0_i9);
Declare_label(mercury__string__format_calc_exp_4_0_i12);
Declare_label(mercury__string__format_calc_exp_4_0_i13);
Declare_label(mercury__string__format_calc_exp_4_0_i14);
Declare_label(mercury__string__format_calc_exp_4_0_i17);
Declare_label(mercury__string__format_calc_exp_4_0_i15);
Declare_label(mercury__string__format_calc_exp_4_0_i19);
Declare_static(mercury__string__format_calc_prec_3_0);
Declare_label(mercury__string__format_calc_prec_3_0_i1000);
Declare_label(mercury__string__format_calc_prec_3_0_i5);
Declare_label(mercury__string__format_calc_prec_3_0_i8);
Declare_label(mercury__string__format_calc_prec_3_0_i9);
Declare_label(mercury__string__format_calc_prec_3_0_i7);
Declare_label(mercury__string__format_calc_prec_3_0_i11);
Declare_label(mercury__string__format_calc_prec_3_0_i12);
Declare_label(mercury__string__format_calc_prec_3_0_i13);
Declare_label(mercury__string__format_calc_prec_3_0_i14);
Declare_label(mercury__string__format_calc_prec_3_0_i20);
Declare_label(mercury__string__format_calc_prec_3_0_i21);
Declare_label(mercury__string__format_calc_prec_3_0_i22);
Declare_label(mercury__string__format_calc_prec_3_0_i19);
Declare_static(mercury__string__find_index_2_3_0);
Declare_label(mercury__string__find_index_2_3_0_i1001);
Declare_label(mercury__string__find_index_2_3_0_i4);
Declare_label(mercury__string__find_index_2_3_0_i6);
Declare_label(mercury__string__find_index_2_3_0_i1000);
Declare_static(mercury__string__format_pad_width_5_0);
Declare_label(mercury__string__format_pad_width_5_0_i2);
Declare_label(mercury__string__format_pad_width_5_0_i8);
Declare_label(mercury__string__format_pad_width_5_0_i7);
Declare_label(mercury__string__format_pad_width_5_0_i10);
Declare_label(mercury__string__format_pad_width_5_0_i11);
Declare_label(mercury__string__format_pad_width_5_0_i14);
Declare_label(mercury__string__format_pad_width_5_0_i13);
Declare_label(mercury__string__format_pad_width_5_0_i19);
Declare_label(mercury__string__format_pad_width_5_0_i21);
Declare_label(mercury__string__format_pad_width_5_0_i22);
Declare_label(mercury__string__format_pad_width_5_0_i18);
Declare_label(mercury__string__format_pad_width_5_0_i3);
Declare_static(mercury__string__format_get_optional_args_5_0);
Declare_label(mercury__string__format_get_optional_args_5_0_i7);
Declare_label(mercury__string__format_get_optional_args_5_0_i8);
Declare_label(mercury__string__format_get_optional_args_5_0_i1002);
Declare_label(mercury__string__format_get_optional_args_5_0_i4);
Declare_label(mercury__string__format_get_optional_args_5_0_i12);
Declare_label(mercury__string__format_get_optional_args_5_0_i13);
Declare_label(mercury__string__format_get_optional_args_5_0_i14);
Declare_label(mercury__string__format_get_optional_args_5_0_i15);
Declare_label(mercury__string__format_get_optional_args_5_0_i16);
Declare_label(mercury__string__format_get_optional_args_5_0_i9);
Declare_label(mercury__string__format_get_optional_args_5_0_i23);
Declare_label(mercury__string__format_get_optional_args_5_0_i24);
Declare_label(mercury__string__format_get_optional_args_5_0_i22);
Declare_label(mercury__string__format_get_optional_args_5_0_i26);
Declare_label(mercury__string__format_get_optional_args_5_0_i20);
Declare_label(mercury__string__format_get_optional_args_5_0_i30);
Declare_label(mercury__string__format_get_optional_args_5_0_i31);
Declare_label(mercury__string__format_get_optional_args_5_0_i27);
Declare_label(mercury__string__format_get_optional_args_5_0_i1001);
Declare_static(mercury__string__format_takewhile1_3_0);
Declare_label(mercury__string__format_takewhile1_3_0_i1023);
Declare_label(mercury__string__format_takewhile1_3_0_i8);
Declare_label(mercury__string__format_takewhile1_3_0_i9);
Declare_label(mercury__string__format_takewhile1_3_0_i10);
Declare_label(mercury__string__format_takewhile1_3_0_i11);
Declare_label(mercury__string__format_takewhile1_3_0_i12);
Declare_label(mercury__string__format_takewhile1_3_0_i13);
Declare_label(mercury__string__format_takewhile1_3_0_i14);
Declare_label(mercury__string__format_takewhile1_3_0_i15);
Declare_label(mercury__string__format_takewhile1_3_0_i16);
Declare_label(mercury__string__format_takewhile1_3_0_i17);
Declare_label(mercury__string__format_takewhile1_3_0_i18);
Declare_label(mercury__string__format_takewhile1_3_0_i19);
Declare_label(mercury__string__format_takewhile1_3_0_i20);
Declare_label(mercury__string__format_takewhile1_3_0_i1019);
Declare_label(mercury__string__format_takewhile1_3_0_i6);
Declare_label(mercury__string__format_takewhile1_3_0_i4);
Declare_label(mercury__string__format_takewhile1_3_0_i22);
Declare_label(mercury__string__format_takewhile1_3_0_i1017);
Declare_static(mercury__string__format_string_to_ints_5_0);
Declare_label(mercury__string__format_string_to_ints_5_0_i6);
Declare_label(mercury__string__format_string_to_ints_5_0_i11);
Declare_label(mercury__string__format_string_to_ints_5_0_i8);
Declare_label(mercury__string__format_string_to_ints_5_0_i12);
Declare_label(mercury__string__format_string_to_ints_5_0_i5);
Declare_label(mercury__string__format_string_to_ints_5_0_i1006);
Declare_label(mercury__string__format_string_to_ints_5_0_i1007);
Declare_static(mercury__string__format_int_from_char_list_2_0);
Declare_label(mercury__string__format_int_from_char_list_2_0_i6);
Declare_label(mercury__string__format_int_from_char_list_2_0_i7);
Declare_label(mercury__string__format_int_from_char_list_2_0_i1003);
Declare_label(mercury__string__format_int_from_char_list_2_0_i1004);
Declare_static(mercury__string__float_to_f_string_2_0);
Define_extern_entry(mercury____Unify___string__poly_type_0_0);
Declare_label(mercury____Unify___string__poly_type_0_0_i4);
Declare_label(mercury____Unify___string__poly_type_0_0_i6);
Declare_label(mercury____Unify___string__poly_type_0_0_i8);
Declare_label(mercury____Unify___string__poly_type_0_0_i1);
Define_extern_entry(mercury____Index___string__poly_type_0_0);
Declare_label(mercury____Index___string__poly_type_0_0_i4);
Declare_label(mercury____Index___string__poly_type_0_0_i5);
Declare_label(mercury____Index___string__poly_type_0_0_i6);
Define_extern_entry(mercury____Compare___string__poly_type_0_0);
Declare_label(mercury____Compare___string__poly_type_0_0_i2);
Declare_label(mercury____Compare___string__poly_type_0_0_i3);
Declare_label(mercury____Compare___string__poly_type_0_0_i4);
Declare_label(mercury____Compare___string__poly_type_0_0_i6);
Declare_label(mercury____Compare___string__poly_type_0_0_i12);
Declare_label(mercury____Compare___string__poly_type_0_0_i15);
Declare_label(mercury____Compare___string__poly_type_0_0_i18);
Declare_label(mercury____Compare___string__poly_type_0_0_i1000);


#ifdef	COMPACT_ARGS
#define	string__append_ooi_input_reg	r1
#else
#define	string__append_ooi_input_reg	r3
#endif

Define_extern_entry(mercury__string__append_3_3_xx);
Declare_label(mercury__string__append_3_3_xx_i1);

BEGIN_MODULE(string_append_module)
	init_entry(mercury__string__append_3_3_xx);
	init_label(mercury__string__append_3_3_xx_i1);
BEGIN_CODE
Define_entry(mercury__string__append_3_3_xx);
	mkframe("string__append/3", 4,
		LABEL(mercury__string__append_3_3_xx_i1));
	mark_hp(framevar(0));
	framevar(1) = string__append_ooi_input_reg;
	framevar(2) = strlen((char *) string__append_ooi_input_reg);
	framevar(3) = 0;
Define_label(mercury__string__append_3_3_xx_i1);
{
	String	s3;
	size_t	s3_len;
	size_t	count;

	restore_hp(framevar(0));
	s3 = (String) framevar(1);
	s3_len = framevar(2);
	count = framevar(3);
	if (count > s3_len) {
		/* modframe(ENTRY(do_fail)); */
		fail();
	}
	incr_hp_atomic(r1, (count + sizeof(Word)) / sizeof(Word));
	memcpy((char *) r1, s3, count);
	((char *) r1)[count] = '\0';
	incr_hp_atomic(r2, (s3_len - count + sizeof(Word)) / sizeof(Word));
	strcpy((char *) r2, s3 + count);
	framevar(3) = count + 1;
	succeed();
}
END_MODULE

#undef	string__append_ooi_input_reg

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_string_append_module
*/
/* suppress gcc -Wmissing-decl warning */
void sys_init_string_append_module(void);

void sys_init_string_append_module(void) {
	extern ModuleFunc string_append_module;
	string_append_module();
}



extern Word * mercury_data_string__base_type_layout_string__poly_type_0[];
Word * mercury_data_string__base_type_info_string__poly_type_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___string__poly_type_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___string__poly_type_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___string__poly_type_0_0),
	(Word *) (Integer) mercury_data_string__base_type_layout_string__poly_type_0
};

extern Word * mercury_data_string__common_3[];
extern Word * mercury_data_string__common_5[];
extern Word * mercury_data_string__common_7[];
extern Word * mercury_data_string__common_9[];
Word * mercury_data_string__base_type_layout_string__poly_type_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_string__common_3),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_string__common_5),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_string__common_7),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_string__common_9)
};

Word mercury_data_string__common_0[] = {
	((Integer) 0)
};

Word * mercury_data_string__common_1[] = {
	(Word *) string_const("string__format: statement has used type", 39)
};

extern Word * mercury_data___base_type_info_float_0[];
Word * mercury_data_string__common_2[] = {
	(Word *) (Integer) mercury_data___base_type_info_float_0
};

Word * mercury_data_string__common_3[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_string__common_2),
	(Word *) string_const("f", 1)
};

extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_string__common_4[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_string__common_5[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_string__common_4),
	(Word *) string_const("i", 1)
};

extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_string__common_6[] = {
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

Word * mercury_data_string__common_7[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_string__common_6),
	(Word *) string_const("s", 1)
};

extern Word * mercury_data___base_type_info_character_0[];
Word * mercury_data_string__common_8[] = {
	(Word *) (Integer) mercury_data___base_type_info_character_0
};

Word * mercury_data_string__common_9[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_string__common_8),
	(Word *) string_const("c", 1)
};

BEGIN_MODULE(mercury__string_module0)
	init_entry(mercury__string__format_add_sign__ua10000_6_0);
	init_label(mercury__string__format_add_sign__ua10000_6_0_i4);
	init_label(mercury__string__format_add_sign__ua10000_6_0_i3);
	init_label(mercury__string__format_add_sign__ua10000_6_0_i6);
	init_label(mercury__string__format_add_sign__ua10000_6_0_i9);
	init_label(mercury__string__format_add_sign__ua10000_6_0_i11);
	init_label(mercury__string__format_add_sign__ua10000_6_0_i12);
	init_label(mercury__string__format_add_sign__ua10000_6_0_i8);
	init_label(mercury__string__format_add_sign__ua10000_6_0_i15);
	init_label(mercury__string__format_add_sign__ua10000_6_0_i14);
BEGIN_CODE

/* code for predicate 'string__format_add_sign__ua10000'/6 in mode 0 */
Define_static(mercury__string__format_add_sign__ua10000_6_0);
	incr_sp_push_msg(6, "string__format_add_sign__ua10000");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	r2 = ((Integer) r3 - ((Integer) 1));
	{
		call_localret(STATIC(mercury__string__index_3_0),
		mercury__string__format_add_sign__ua10000_6_0_i4,
		STATIC(mercury__string__format_add_sign__ua10000_6_0));
	}
Define_label(mercury__string__format_add_sign__ua10000_6_0_i4);
	update_prof_current_proc(LABEL(mercury__string__format_add_sign__ua10000_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_add_sign__ua10000_6_0_i3);
	if ((((Integer) 45) != (Integer) r2))
		GOTO_LABEL(mercury__string__format_add_sign__ua10000_6_0_i3);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__string__format_add_sign__ua10000_6_0_i3);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__string__split_4_0),
		mercury__string__format_add_sign__ua10000_6_0_i6,
		STATIC(mercury__string__format_add_sign__ua10000_6_0));
	}
Define_label(mercury__string__format_add_sign__ua10000_6_0_i6);
	update_prof_current_proc(LABEL(mercury__string__format_add_sign__ua10000_6_0));
	detstackvar(4) = (Integer) r1;
	detstackvar(5) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r2 = ((Integer) 43);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__string__format_add_sign__ua10000_6_0_i9,
		STATIC(mercury__string__format_add_sign__ua10000_6_0));
	}
Define_label(mercury__string__format_add_sign__ua10000_6_0_i9);
	update_prof_current_proc(LABEL(mercury__string__format_add_sign__ua10000_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_add_sign__ua10000_6_0_i8);
	r1 = string_const("+", 1);
	r2 = (Integer) detstackvar(5);
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__format_add_sign__ua10000_6_0_i11,
		STATIC(mercury__string__format_add_sign__ua10000_6_0));
	}
Define_label(mercury__string__format_add_sign__ua10000_6_0_i11);
	update_prof_current_proc(LABEL(mercury__string__format_add_sign__ua10000_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__format_add_sign__ua10000_6_0_i12,
		STATIC(mercury__string__format_add_sign__ua10000_6_0));
	}
Define_label(mercury__string__format_add_sign__ua10000_6_0_i12);
	update_prof_current_proc(LABEL(mercury__string__format_add_sign__ua10000_6_0));
	r2 = (Integer) r1;
	r1 = ((Integer) detstackvar(3) + ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__string__format_add_sign__ua10000_6_0_i8);
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r2 = ((Integer) 32);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__string__format_add_sign__ua10000_6_0_i15,
		STATIC(mercury__string__format_add_sign__ua10000_6_0));
	}
Define_label(mercury__string__format_add_sign__ua10000_6_0_i15);
	update_prof_current_proc(LABEL(mercury__string__format_add_sign__ua10000_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_add_sign__ua10000_6_0_i14);
	r1 = string_const(" ", 1);
	r2 = (Integer) detstackvar(5);
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__format_add_sign__ua10000_6_0_i11,
		STATIC(mercury__string__format_add_sign__ua10000_6_0));
	}
Define_label(mercury__string__format_add_sign__ua10000_6_0_i14);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module1)
	init_entry(mercury__string__length_2_0);
BEGIN_CODE

/* code for predicate 'string__length'/2 in mode 0 */
Define_entry(mercury__string__length_2_0);
	{
		String	Str;
		Integer	Length;
		Str = (String) (Integer) r1;
		
	Length = strlen(Str);

		r2 = Length;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module2)
	init_entry(mercury__string__append_3_0);
	init_label(mercury__string__append_3_0_i1);
BEGIN_CODE

/* code for predicate 'string__append'/3 in mode 0 */
Define_entry(mercury__string__append_3_0);
	r4 = (Integer) r1;
	{
		String	S1;
		String	S2;
		String	S3;
		S1 = (String) (Integer) r1;
		S2 = (String) (Integer) r2;
		S3 = (String) (Integer) r3;
		{
	size_t len_1 = strlen(S1);
	SUCCESS_INDICATOR = (
		strncmp(S1, S3, len_1) == 0 &&
		strcmp(S2, S3 + len_1) == 0
	);
}

	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__append_3_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__string__append_3_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module3)
	init_entry(mercury__string__append_3_1);
	init_label(mercury__string__append_3_1_i1);
BEGIN_CODE

/* code for predicate 'string__append'/3 in mode 1 */
Define_entry(mercury__string__append_3_1);
	r3 = (Integer) r1;
	{
		String	S1;
		String	S2;
		String	S3;
		S1 = (String) (Integer) r1;
		S3 = (String) (Integer) r2;
		{
	Word tmp;
	size_t len_1, len_2, len_3;

	len_1 = strlen(S1);
	if (strncmp(S1, S3, len_1) != 0) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		len_3 = strlen(S3);
		len_2 = len_3 - len_1;
		incr_hp_atomic(tmp, (len_2 + sizeof(Word)) / sizeof(Word));
		S2 = (char *) tmp;
		strcpy(S2, S3 + len_1);
		SUCCESS_INDICATOR = TRUE;
	}
}
		r4 = (Word) S2;

	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__append_3_1_i1);
	r2 = (Integer) r4;
	r1 = TRUE;
	proceed();
Define_label(mercury__string__append_3_1_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module4)
	init_entry(mercury__string__append_3_2);
BEGIN_CODE

/* code for predicate 'string__append'/3 in mode 2 */
Define_entry(mercury__string__append_3_2);
	{
		String	S1;
		String	S2;
		String	S3;
		S1 = (String) (Integer) r1;
		S2 = (String) (Integer) r2;
		{
	size_t len_1, len_2;
	Word tmp;
	len_1 = strlen(S1);
	len_2 = strlen(S2);
	incr_hp_atomic(tmp, (len_1 + len_2 + sizeof(Word)) / sizeof(Word));
	S3 = (char *) tmp;
	strcpy(S3, S1);
	strcpy(S3 + len_1, S2);
}
		r3 = (Word) S3;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module5)
	init_entry(mercury__string__append_3_3);
BEGIN_CODE

/* code for predicate 'string__append'/3 in mode 3 */
Define_entry(mercury__string__append_3_3);
	{
	Declare_entry(do_fail);
	mkframe("string__append/3", 1, ENTRY(do_fail));
	}
	{
		String	S1;
		String	S2;
		String	S3;
		S3 = (String) (Integer) r1;
		
	/*
	** The pragma_c_code will generate a mkframe();
	** we need to pop off that frame before jumping to the hand-coded
	** fragment above.
	**
	** We mention S1, S2 and S3 here to shut up a warning.
	*/

	maxfr = curprevfr;
	curfr = cursuccfr;
	{
		Declare_entry(mercury__string__append_3_3_xx);
		GOTO(ENTRY(mercury__string__append_3_3_xx));
	}

		r2 = (Word) S1;
		r3 = (Word) S2;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	succeed();
END_MODULE

BEGIN_MODULE(mercury__string_module6)
	init_entry(mercury__string__remove_suffix_3_0);
	init_label(mercury__string__remove_suffix_3_0_i2);
	init_label(mercury__string__remove_suffix_3_0_i3);
	init_label(mercury__string__remove_suffix_3_0_i4);
	init_label(mercury__string__remove_suffix_3_0_i6);
	init_label(mercury__string__remove_suffix_3_0_i1);
BEGIN_CODE

/* code for predicate 'string__remove_suffix'/3 in mode 0 */
Define_entry(mercury__string__remove_suffix_3_0);
	incr_sp_push_msg(2, "string__remove_suffix");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	call_localret(STATIC(mercury__string__to_int_list_2_1),
		mercury__string__remove_suffix_3_0_i2,
		ENTRY(mercury__string__remove_suffix_3_0));
Define_label(mercury__string__remove_suffix_3_0_i2);
	update_prof_current_proc(LABEL(mercury__string__remove_suffix_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__string__to_int_list_2_1),
		mercury__string__remove_suffix_3_0_i3,
		ENTRY(mercury__string__remove_suffix_3_0));
Define_label(mercury__string__remove_suffix_3_0_i3);
	update_prof_current_proc(LABEL(mercury__string__remove_suffix_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__list__remove_suffix_3_0);
	call_localret(ENTRY(mercury__list__remove_suffix_3_0),
		mercury__string__remove_suffix_3_0_i4,
		ENTRY(mercury__string__remove_suffix_3_0));
	}
Define_label(mercury__string__remove_suffix_3_0_i4);
	update_prof_current_proc(LABEL(mercury__string__remove_suffix_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__remove_suffix_3_0_i1);
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__string__to_int_list_2_0),
		mercury__string__remove_suffix_3_0_i6,
		ENTRY(mercury__string__remove_suffix_3_0));
Define_label(mercury__string__remove_suffix_3_0_i6);
	update_prof_current_proc(LABEL(mercury__string__remove_suffix_3_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__remove_suffix_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module7)
	init_entry(mercury__string__prefix_2_0);
BEGIN_CODE

/* code for predicate 'string__prefix'/2 in mode 0 */
Define_entry(mercury__string__prefix_2_0);
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	{
		tailcall(STATIC(mercury__string__append_3_1),
		ENTRY(mercury__string__prefix_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module8)
	init_entry(mercury__string__prefix_2_1);
	init_label(mercury__string__prefix_2_1_i1);
BEGIN_CODE

/* code for predicate 'string__prefix'/2 in mode 1 */
Define_entry(mercury__string__prefix_2_1);
	{
	Declare_entry(do_fail);
	mkframe("string__prefix/2", 1, ENTRY(do_fail));
	}
	{
		call_localret(STATIC(mercury__string__append_3_3),
		mercury__string__prefix_2_1_i1,
		ENTRY(mercury__string__prefix_2_1));
	}
Define_label(mercury__string__prefix_2_1_i1);
	update_prof_current_proc(LABEL(mercury__string__prefix_2_1));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__string_module9)
	init_entry(mercury__string__char_to_string_2_1);
	init_label(mercury__string__char_to_string_2_1_i2);
	init_label(mercury__string__char_to_string_2_1_i4);
	init_label(mercury__string__char_to_string_2_1_i6);
	init_label(mercury__string__char_to_string_2_1_i1);
	init_label(mercury__string__char_to_string_2_1_i1000);
BEGIN_CODE

/* code for predicate 'string__char_to_string'/2 in mode 1 */
Define_entry(mercury__string__char_to_string_2_1);
	incr_sp_push_msg(2, "string__char_to_string");
	detstackvar(2) = (Integer) succip;
	call_localret(STATIC(mercury__string__to_int_list_2_1),
		mercury__string__char_to_string_2_1_i2,
		ENTRY(mercury__string__char_to_string_2_1));
Define_label(mercury__string__char_to_string_2_1_i2);
	update_prof_current_proc(LABEL(mercury__string__char_to_string_2_1));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__char_to_string_2_1_i1);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) mercury_data___base_type_info_int_0;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		mercury__string__char_to_string_2_1_i4,
		ENTRY(mercury__string__char_to_string_2_1));
	}
Define_label(mercury__string__char_to_string_2_1_i4);
	update_prof_current_proc(LABEL(mercury__string__char_to_string_2_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__char_to_string_2_1_i1);
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__char__to_int_2_2);
	call_localret(ENTRY(mercury__char__to_int_2_2),
		mercury__string__char_to_string_2_1_i6,
		ENTRY(mercury__string__char_to_string_2_1));
	}
Define_label(mercury__string__char_to_string_2_1_i6);
	update_prof_current_proc(LABEL(mercury__string__char_to_string_2_1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__char_to_string_2_1_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__string__char_to_string_2_1_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__char_to_string_2_1_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module10)
	init_entry(mercury__string__char_to_string_2_0);
	init_label(mercury__string__char_to_string_2_0_i2);
BEGIN_CODE

/* code for predicate 'string__char_to_string'/2 in mode 0 */
Define_entry(mercury__string__char_to_string_2_0);
	incr_sp_push_msg(1, "string__char_to_string");
	detstackvar(1) = (Integer) succip;
	{
	Declare_entry(mercury__char__to_int_2_0);
	call_localret(ENTRY(mercury__char__to_int_2_0),
		mercury__string__char_to_string_2_0_i2,
		ENTRY(mercury__string__char_to_string_2_0));
	}
Define_label(mercury__string__char_to_string_2_0_i2);
	update_prof_current_proc(LABEL(mercury__string__char_to_string_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	tailcall(STATIC(mercury__string__to_int_list_2_0),
		ENTRY(mercury__string__char_to_string_2_0));
END_MODULE

BEGIN_MODULE(mercury__string_module11)
	init_entry(mercury__string__int_to_string_2_0);
BEGIN_CODE

/* code for predicate 'string__int_to_string'/2 in mode 0 */
Define_entry(mercury__string__int_to_string_2_0);
	r2 = ((Integer) 10);
	{
		tailcall(STATIC(mercury__string__int_to_base_string_3_0),
		ENTRY(mercury__string__int_to_string_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module12)
	init_entry(mercury__string__int_to_base_string_3_0);
	init_label(mercury__string__int_to_base_string_3_0_i1002);
	init_label(mercury__string__int_to_base_string_3_0_i2);
	init_label(mercury__string__int_to_base_string_3_0_i4);
	init_label(mercury__string__int_to_base_string_3_0_i1003);
	init_label(mercury__string__int_to_base_string_3_0_i5);
	init_label(mercury__string__int_to_base_string_3_0_i8);
	init_label(mercury__string__int_to_base_string_3_0_i6);
BEGIN_CODE

/* code for predicate 'string__int_to_base_string'/3 in mode 0 */
Define_entry(mercury__string__int_to_base_string_3_0);
	if (((Integer) r2 < ((Integer) 2)))
		GOTO_LABEL(mercury__string__int_to_base_string_3_0_i1002);
	if (((Integer) r2 <= ((Integer) 36)))
		GOTO_LABEL(mercury__string__int_to_base_string_3_0_i1003);
	incr_sp_push_msg(3, "string__int_to_base_string");
	detstackvar(3) = (Integer) succip;
	GOTO_LABEL(mercury__string__int_to_base_string_3_0_i2);
Define_label(mercury__string__int_to_base_string_3_0_i1002);
	incr_sp_push_msg(3, "string__int_to_base_string");
	detstackvar(3) = (Integer) succip;
Define_label(mercury__string__int_to_base_string_3_0_i2);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = string_const("string__int_to_base_string: invalid base", 40);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__string__int_to_base_string_3_0_i4,
		ENTRY(mercury__string__int_to_base_string_3_0));
	}
Define_label(mercury__string__int_to_base_string_3_0_i4);
	update_prof_current_proc(LABEL(mercury__string__int_to_base_string_3_0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	GOTO_LABEL(mercury__string__int_to_base_string_3_0_i5);
Define_label(mercury__string__int_to_base_string_3_0_i1003);
	incr_sp_push_msg(3, "string__int_to_base_string");
	detstackvar(3) = (Integer) succip;
Define_label(mercury__string__int_to_base_string_3_0_i5);
	if (((Integer) r1 >= ((Integer) 0)))
		GOTO_LABEL(mercury__string__int_to_base_string_3_0_i6);
	r1 = (((Integer) 0) - (Integer) r1);
	call_localret(STATIC(mercury__string__int_to_base_string_2_3_0),
		mercury__string__int_to_base_string_3_0_i8,
		ENTRY(mercury__string__int_to_base_string_3_0));
Define_label(mercury__string__int_to_base_string_3_0_i8);
	update_prof_current_proc(LABEL(mercury__string__int_to_base_string_3_0));
	r2 = (Integer) r1;
	r1 = string_const("-", 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__string__append_3_2),
		ENTRY(mercury__string__int_to_base_string_3_0));
	}
Define_label(mercury__string__int_to_base_string_3_0_i6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__string__int_to_base_string_2_3_0),
		ENTRY(mercury__string__int_to_base_string_3_0));
END_MODULE

BEGIN_MODULE(mercury__string_module13)
	init_entry(mercury__string__float_to_string_2_0);
BEGIN_CODE

/* code for predicate 'string__float_to_string'/2 in mode 0 */
Define_entry(mercury__string__float_to_string_2_0);
	{
		Float	FloatVal;
		String	FloatString;
		FloatVal = word_to_float((Integer) r1);
		{
	char buf[500];
	Word tmp;
	sprintf(buf, "%.15g", FloatVal);
	incr_hp_atomic(tmp, (strlen(buf) + sizeof(Word)) / sizeof(Word));
	FloatString = (char *)tmp;
	strcpy(FloatString, buf);
}
		r2 = (Word) FloatString;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module14)
	init_entry(mercury__string__first_char_3_0);
	init_label(mercury__string__first_char_3_0_i1);
BEGIN_CODE

/* code for predicate 'string__first_char'/3 in mode 0 */
Define_entry(mercury__string__first_char_3_0);
	r4 = (Integer) r1;
	{
		String	Str;
		Char	First;
		String	Rest;
		Str = (String) (Integer) r1;
		First = (Integer) r2;
		Rest = (String) (Integer) r3;
		
	SUCCESS_INDICATOR = (
		Str[0] == First &&
		First != '\0' &&
		strcmp(Str + 1, Rest) == 0
	);


	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__first_char_3_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__string__first_char_3_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module15)
	init_entry(mercury__string__first_char_3_1);
	init_label(mercury__string__first_char_3_1_i1);
BEGIN_CODE

/* code for predicate 'string__first_char'/3 in mode 1 */
Define_entry(mercury__string__first_char_3_1);
	r3 = (Integer) r1;
	{
		String	Str;
		Char	First;
		String	Rest;
		Str = (String) (Integer) r1;
		Rest = (String) (Integer) r2;
		
	First = Str[0];
	SUCCESS_INDICATOR = (First != '\0' && strcmp(Str + 1, Rest) == 0);

		r4 = First;

	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__first_char_3_1_i1);
	r2 = (Integer) r4;
	r1 = TRUE;
	proceed();
Define_label(mercury__string__first_char_3_1_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module16)
	init_entry(mercury__string__first_char_3_2);
	init_label(mercury__string__first_char_3_2_i1);
BEGIN_CODE

/* code for predicate 'string__first_char'/3 in mode 2 */
Define_entry(mercury__string__first_char_3_2);
	r3 = (Integer) r1;
	{
		String	Str;
		Char	First;
		String	Rest;
		Str = (String) (Integer) r1;
		First = (Integer) r2;
		{
	Word tmp;
	if (Str[0] != First || First == '\0') {
		SUCCESS_INDICATOR = FALSE;
	} else {
		Str++;
		incr_hp_atomic(tmp,
			(strlen(Str) + sizeof(Word)) / sizeof(Word));
		Rest = (char *) tmp;
		strcpy(Rest, Str);
		SUCCESS_INDICATOR = TRUE;
	}
}
		r4 = (Word) Rest;

	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__first_char_3_2_i1);
	r2 = (Integer) r4;
	r1 = TRUE;
	proceed();
Define_label(mercury__string__first_char_3_2_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module17)
	init_entry(mercury__string__first_char_3_3);
	init_label(mercury__string__first_char_3_3_i1);
BEGIN_CODE

/* code for predicate 'string__first_char'/3 in mode 3 */
Define_entry(mercury__string__first_char_3_3);
	r2 = (Integer) r1;
	{
		String	Str;
		Char	First;
		String	Rest;
		Str = (String) (Integer) r1;
		{
	Word tmp;
	First = Str[0];
	if (First == '\0') {
		SUCCESS_INDICATOR = FALSE;
	} else {
		Str++;
		incr_hp_atomic(tmp,
			(strlen(Str) + sizeof(Word)) / sizeof(Word));
		Rest = (char *) tmp;
		strcpy(Rest, Str);
		SUCCESS_INDICATOR = TRUE;
	}
}
		r3 = First;
		r4 = (Word) Rest;

	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__first_char_3_3_i1);
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	r1 = TRUE;
	proceed();
Define_label(mercury__string__first_char_3_3_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module18)
	init_entry(mercury__string__first_char_3_4);
BEGIN_CODE

/* code for predicate 'string__first_char'/3 in mode 4 */
Define_entry(mercury__string__first_char_3_4);
	{
		String	Str;
		Char	First;
		String	Rest;
		First = (Integer) r1;
		Rest = (String) (Integer) r2;
		{
	size_t len = strlen(Rest) + 1;
	Word tmp;
	incr_hp_atomic(tmp, (len + sizeof(Word)) / sizeof(Word));
	Str = (char *) tmp;
	Str[0] = First;
	strcpy(Str + 1, Rest);
}
		r3 = (Word) Str;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module19)
	init_entry(mercury__string__replace_4_0);
	init_label(mercury__string__replace_4_0_i2);
	init_label(mercury__string__replace_4_0_i3);
	init_label(mercury__string__replace_4_0_i4);
	init_label(mercury__string__replace_4_0_i6);
	init_label(mercury__string__replace_4_0_i7);
	init_label(mercury__string__replace_4_0_i8);
	init_label(mercury__string__replace_4_0_i9);
	init_label(mercury__string__replace_4_0_i1);
BEGIN_CODE

/* code for predicate 'string__replace'/4 in mode 0 */
Define_entry(mercury__string__replace_4_0);
	incr_sp_push_msg(3, "string__replace");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__replace_4_0_i2,
		ENTRY(mercury__string__replace_4_0));
	}
Define_label(mercury__string__replace_4_0_i2);
	update_prof_current_proc(LABEL(mercury__string__replace_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__replace_4_0_i3,
		ENTRY(mercury__string__replace_4_0));
	}
Define_label(mercury__string__replace_4_0_i3);
	update_prof_current_proc(LABEL(mercury__string__replace_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__string__find_sub_charlist_4_0),
		mercury__string__replace_4_0_i4,
		ENTRY(mercury__string__replace_4_0));
Define_label(mercury__string__replace_4_0_i4);
	update_prof_current_proc(LABEL(mercury__string__replace_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__replace_4_0_i1);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r3;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__replace_4_0_i6,
		ENTRY(mercury__string__replace_4_0));
	}
Define_label(mercury__string__replace_4_0_i6);
	update_prof_current_proc(LABEL(mercury__string__replace_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r2 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__string__replace_4_0_i7,
		ENTRY(mercury__string__replace_4_0));
	}
Define_label(mercury__string__replace_4_0_i7);
	update_prof_current_proc(LABEL(mercury__string__replace_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__string__replace_4_0_i8,
		ENTRY(mercury__string__replace_4_0));
	}
Define_label(mercury__string__replace_4_0_i8);
	update_prof_current_proc(LABEL(mercury__string__replace_4_0));
	{
		call_localret(STATIC(mercury__string__from_char_list_2_0),
		mercury__string__replace_4_0_i9,
		ENTRY(mercury__string__replace_4_0));
	}
Define_label(mercury__string__replace_4_0_i9);
	update_prof_current_proc(LABEL(mercury__string__replace_4_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__string__replace_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module20)
	init_entry(mercury__string__replace_all_4_0);
	init_label(mercury__string__replace_all_4_0_i2);
	init_label(mercury__string__replace_all_4_0_i3);
	init_label(mercury__string__replace_all_4_0_i4);
	init_label(mercury__string__replace_all_4_0_i5);
BEGIN_CODE

/* code for predicate 'string__replace_all'/4 in mode 0 */
Define_entry(mercury__string__replace_all_4_0);
	incr_sp_push_msg(3, "string__replace_all");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__replace_all_4_0_i2,
		ENTRY(mercury__string__replace_all_4_0));
	}
Define_label(mercury__string__replace_all_4_0_i2);
	update_prof_current_proc(LABEL(mercury__string__replace_all_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__replace_all_4_0_i3,
		ENTRY(mercury__string__replace_all_4_0));
	}
Define_label(mercury__string__replace_all_4_0_i3);
	update_prof_current_proc(LABEL(mercury__string__replace_all_4_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__replace_all_4_0_i4,
		ENTRY(mercury__string__replace_all_4_0));
	}
Define_label(mercury__string__replace_all_4_0_i4);
	update_prof_current_proc(LABEL(mercury__string__replace_all_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__string__find_all_sub_charlist_4_0),
		mercury__string__replace_all_4_0_i5,
		ENTRY(mercury__string__replace_all_4_0));
Define_label(mercury__string__replace_all_4_0_i5);
	update_prof_current_proc(LABEL(mercury__string__replace_all_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__string__from_char_list_2_0),
		ENTRY(mercury__string__replace_all_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module21)
	init_entry(mercury__string__to_lower_2_1);
	init_label(mercury__string__to_lower_2_1_i2);
	init_label(mercury__string__to_lower_2_1_i3);
	init_label(mercury__string__to_lower_2_1_i4);
	init_label(mercury__string__to_lower_2_1_i1);
BEGIN_CODE

/* code for predicate 'string__to_lower'/2 in mode 1 */
Define_entry(mercury__string__to_lower_2_1);
	incr_sp_push_msg(2, "string__to_lower");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__to_lower_2_1_i2,
		ENTRY(mercury__string__to_lower_2_1));
	}
Define_label(mercury__string__to_lower_2_1_i2);
	update_prof_current_proc(LABEL(mercury__string__to_lower_2_1));
	call_localret(STATIC(mercury__string__char_list_to_lower_2_0),
		mercury__string__to_lower_2_1_i3,
		ENTRY(mercury__string__to_lower_2_1));
Define_label(mercury__string__to_lower_2_1_i3);
	update_prof_current_proc(LABEL(mercury__string__to_lower_2_1));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__from_char_list_2_1),
		mercury__string__to_lower_2_1_i4,
		ENTRY(mercury__string__to_lower_2_1));
	}
Define_label(mercury__string__to_lower_2_1_i4);
	update_prof_current_proc(LABEL(mercury__string__to_lower_2_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__to_lower_2_1_i1);
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury__string__to_lower_2_1));
	}
Define_label(mercury__string__to_lower_2_1_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module22)
	init_entry(mercury__string__to_lower_2_0);
	init_label(mercury__string__to_lower_2_0_i2);
	init_label(mercury__string__to_lower_2_0_i3);
BEGIN_CODE

/* code for predicate 'string__to_lower'/2 in mode 0 */
Define_entry(mercury__string__to_lower_2_0);
	incr_sp_push_msg(1, "string__to_lower");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__to_lower_2_0_i2,
		ENTRY(mercury__string__to_lower_2_0));
	}
Define_label(mercury__string__to_lower_2_0_i2);
	update_prof_current_proc(LABEL(mercury__string__to_lower_2_0));
	call_localret(STATIC(mercury__string__char_list_to_lower_2_0),
		mercury__string__to_lower_2_0_i3,
		ENTRY(mercury__string__to_lower_2_0));
Define_label(mercury__string__to_lower_2_0_i3);
	update_prof_current_proc(LABEL(mercury__string__to_lower_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__string__from_char_list_2_0),
		ENTRY(mercury__string__to_lower_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module23)
	init_entry(mercury__string__to_upper_2_1);
	init_label(mercury__string__to_upper_2_1_i2);
	init_label(mercury__string__to_upper_2_1_i3);
	init_label(mercury__string__to_upper_2_1_i4);
	init_label(mercury__string__to_upper_2_1_i1);
BEGIN_CODE

/* code for predicate 'string__to_upper'/2 in mode 1 */
Define_entry(mercury__string__to_upper_2_1);
	incr_sp_push_msg(2, "string__to_upper");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__to_upper_2_1_i2,
		ENTRY(mercury__string__to_upper_2_1));
	}
Define_label(mercury__string__to_upper_2_1_i2);
	update_prof_current_proc(LABEL(mercury__string__to_upper_2_1));
	call_localret(STATIC(mercury__string__char_list_to_upper_2_0),
		mercury__string__to_upper_2_1_i3,
		ENTRY(mercury__string__to_upper_2_1));
Define_label(mercury__string__to_upper_2_1_i3);
	update_prof_current_proc(LABEL(mercury__string__to_upper_2_1));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__from_char_list_2_1),
		mercury__string__to_upper_2_1_i4,
		ENTRY(mercury__string__to_upper_2_1));
	}
Define_label(mercury__string__to_upper_2_1_i4);
	update_prof_current_proc(LABEL(mercury__string__to_upper_2_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__to_upper_2_1_i1);
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury__string__to_upper_2_1));
	}
Define_label(mercury__string__to_upper_2_1_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module24)
	init_entry(mercury__string__to_upper_2_0);
	init_label(mercury__string__to_upper_2_0_i2);
	init_label(mercury__string__to_upper_2_0_i3);
BEGIN_CODE

/* code for predicate 'string__to_upper'/2 in mode 0 */
Define_entry(mercury__string__to_upper_2_0);
	incr_sp_push_msg(1, "string__to_upper");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__to_upper_2_0_i2,
		ENTRY(mercury__string__to_upper_2_0));
	}
Define_label(mercury__string__to_upper_2_0_i2);
	update_prof_current_proc(LABEL(mercury__string__to_upper_2_0));
	call_localret(STATIC(mercury__string__char_list_to_upper_2_0),
		mercury__string__to_upper_2_0_i3,
		ENTRY(mercury__string__to_upper_2_0));
Define_label(mercury__string__to_upper_2_0_i3);
	update_prof_current_proc(LABEL(mercury__string__to_upper_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__string__from_char_list_2_0),
		ENTRY(mercury__string__to_upper_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module25)
	init_entry(mercury__string__capitalize_first_2_0);
	init_label(mercury__string__capitalize_first_2_0_i4);
	init_label(mercury__string__capitalize_first_2_0_i6);
	init_label(mercury__string__capitalize_first_2_0_i3);
BEGIN_CODE

/* code for predicate 'string__capitalize_first'/2 in mode 0 */
Define_entry(mercury__string__capitalize_first_2_0);
	incr_sp_push_msg(3, "string__capitalize_first");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
		call_localret(STATIC(mercury__string__first_char_3_3),
		mercury__string__capitalize_first_2_0_i4,
		ENTRY(mercury__string__capitalize_first_2_0));
	}
Define_label(mercury__string__capitalize_first_2_0_i4);
	update_prof_current_proc(LABEL(mercury__string__capitalize_first_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__capitalize_first_2_0_i3);
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__char__to_upper_2_0);
	call_localret(ENTRY(mercury__char__to_upper_2_0),
		mercury__string__capitalize_first_2_0_i6,
		ENTRY(mercury__string__capitalize_first_2_0));
	}
Define_label(mercury__string__capitalize_first_2_0_i6);
	update_prof_current_proc(LABEL(mercury__string__capitalize_first_2_0));
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__string__first_char_3_4),
		ENTRY(mercury__string__capitalize_first_2_0));
	}
Define_label(mercury__string__capitalize_first_2_0_i3);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module26)
	init_entry(mercury__string__uncapitalize_first_2_0);
	init_label(mercury__string__uncapitalize_first_2_0_i4);
	init_label(mercury__string__uncapitalize_first_2_0_i6);
	init_label(mercury__string__uncapitalize_first_2_0_i3);
BEGIN_CODE

/* code for predicate 'string__uncapitalize_first'/2 in mode 0 */
Define_entry(mercury__string__uncapitalize_first_2_0);
	incr_sp_push_msg(3, "string__uncapitalize_first");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
		call_localret(STATIC(mercury__string__first_char_3_3),
		mercury__string__uncapitalize_first_2_0_i4,
		ENTRY(mercury__string__uncapitalize_first_2_0));
	}
Define_label(mercury__string__uncapitalize_first_2_0_i4);
	update_prof_current_proc(LABEL(mercury__string__uncapitalize_first_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__uncapitalize_first_2_0_i3);
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__char__to_lower_2_0);
	call_localret(ENTRY(mercury__char__to_lower_2_0),
		mercury__string__uncapitalize_first_2_0_i6,
		ENTRY(mercury__string__uncapitalize_first_2_0));
	}
Define_label(mercury__string__uncapitalize_first_2_0_i6);
	update_prof_current_proc(LABEL(mercury__string__uncapitalize_first_2_0));
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__string__first_char_3_4),
		ENTRY(mercury__string__uncapitalize_first_2_0));
	}
Define_label(mercury__string__uncapitalize_first_2_0_i3);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module27)
	init_entry(mercury__string__to_char_list_2_0);
	init_label(mercury__string__to_char_list_2_0_i2);
BEGIN_CODE

/* code for predicate 'string__to_char_list'/2 in mode 0 */
Define_entry(mercury__string__to_char_list_2_0);
	incr_sp_push_msg(1, "string__to_char_list");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__string__to_int_list_2_1),
		mercury__string__to_char_list_2_0_i2,
		ENTRY(mercury__string__to_char_list_2_0));
Define_label(mercury__string__to_char_list_2_0_i2);
	update_prof_current_proc(LABEL(mercury__string__to_char_list_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	tailcall(STATIC(mercury__string__int_list_to_char_list_2_0),
		ENTRY(mercury__string__to_char_list_2_0));
END_MODULE

BEGIN_MODULE(mercury__string_module28)
	init_entry(mercury__string__from_char_list_2_1);
	init_label(mercury__string__from_char_list_2_1_i2);
	init_label(mercury__string__from_char_list_2_1_i3);
	init_label(mercury__string__from_char_list_2_1_i1000);
BEGIN_CODE

/* code for predicate 'string__from_char_list'/2 in mode 1 */
Define_entry(mercury__string__from_char_list_2_1);
	incr_sp_push_msg(1, "string__from_char_list");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__string__to_int_list_2_1),
		mercury__string__from_char_list_2_1_i2,
		ENTRY(mercury__string__from_char_list_2_1));
Define_label(mercury__string__from_char_list_2_1_i2);
	update_prof_current_proc(LABEL(mercury__string__from_char_list_2_1));
	call_localret(STATIC(mercury__string__char_list_to_int_list_2_1),
		mercury__string__from_char_list_2_1_i3,
		ENTRY(mercury__string__from_char_list_2_1));
Define_label(mercury__string__from_char_list_2_1_i3);
	update_prof_current_proc(LABEL(mercury__string__from_char_list_2_1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__from_char_list_2_1_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__string__from_char_list_2_1_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module29)
	init_entry(mercury__string__from_char_list_2_0);
	init_label(mercury__string__from_char_list_2_0_i2);
BEGIN_CODE

/* code for predicate 'string__from_char_list'/2 in mode 0 */
Define_entry(mercury__string__from_char_list_2_0);
	incr_sp_push_msg(1, "string__from_char_list");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__string__char_list_to_int_list_2_0),
		mercury__string__from_char_list_2_0_i2,
		ENTRY(mercury__string__from_char_list_2_0));
Define_label(mercury__string__from_char_list_2_0_i2);
	update_prof_current_proc(LABEL(mercury__string__from_char_list_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	tailcall(STATIC(mercury__string__to_int_list_2_0),
		ENTRY(mercury__string__from_char_list_2_0));
END_MODULE

BEGIN_MODULE(mercury__string_module30)
	init_entry(mercury__string__to_int_2_0);
	init_label(mercury__string__to_int_2_0_i2);
	init_label(mercury__string__to_int_2_0_i1000);
BEGIN_CODE

/* code for predicate 'string__to_int'/2 in mode 0 */
Define_entry(mercury__string__to_int_2_0);
	r2 = (Integer) r1;
	r1 = ((Integer) 10);
	incr_sp_push_msg(1, "string__to_int");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__string__base_string_to_int_3_0),
		mercury__string__to_int_2_0_i2,
		ENTRY(mercury__string__to_int_2_0));
	}
Define_label(mercury__string__to_int_2_0_i2);
	update_prof_current_proc(LABEL(mercury__string__to_int_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__to_int_2_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__string__to_int_2_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module31)
	init_entry(mercury__string__base_string_to_int_3_0);
	init_label(mercury__string__base_string_to_int_3_0_i4);
	init_label(mercury__string__base_string_to_int_3_0_i9);
	init_label(mercury__string__base_string_to_int_3_0_i6);
	init_label(mercury__string__base_string_to_int_3_0_i14);
	init_label(mercury__string__base_string_to_int_3_0_i11);
	init_label(mercury__string__base_string_to_int_3_0_i3);
	init_label(mercury__string__base_string_to_int_3_0_i1000);
	init_label(mercury__string__base_string_to_int_3_0_i1001);
BEGIN_CODE

/* code for predicate 'string__base_string_to_int'/3 in mode 0 */
Define_entry(mercury__string__base_string_to_int_3_0);
	incr_sp_push_msg(3, "string__base_string_to_int");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__first_char_3_3),
		mercury__string__base_string_to_int_3_0_i4,
		ENTRY(mercury__string__base_string_to_int_3_0));
	}
Define_label(mercury__string__base_string_to_int_3_0_i4);
	update_prof_current_proc(LABEL(mercury__string__base_string_to_int_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__base_string_to_int_3_0_i3);
	if (((Integer) r2 != ((Integer) 45)))
		GOTO_LABEL(mercury__string__base_string_to_int_3_0_i6);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) r3;
	r3 = ((Integer) 0);
	call_localret(STATIC(mercury__string__base_string_to_int_2_4_0),
		mercury__string__base_string_to_int_3_0_i9,
		ENTRY(mercury__string__base_string_to_int_3_0));
Define_label(mercury__string__base_string_to_int_3_0_i9);
	update_prof_current_proc(LABEL(mercury__string__base_string_to_int_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__base_string_to_int_3_0_i1001);
	r2 = (((Integer) 0) - (Integer) r2);
	r1 = TRUE;
	proceed();
Define_label(mercury__string__base_string_to_int_3_0_i6);
	if (((Integer) r2 != ((Integer) 43)))
		GOTO_LABEL(mercury__string__base_string_to_int_3_0_i11);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) r3;
	r3 = ((Integer) 0);
	call_localret(STATIC(mercury__string__base_string_to_int_2_4_0),
		mercury__string__base_string_to_int_3_0_i14,
		ENTRY(mercury__string__base_string_to_int_3_0));
Define_label(mercury__string__base_string_to_int_3_0_i14);
	update_prof_current_proc(LABEL(mercury__string__base_string_to_int_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if ((Integer) r1)
		GOTO_LABEL(mercury__string__base_string_to_int_3_0_i1000);
	r1 = FALSE;
	proceed();
Define_label(mercury__string__base_string_to_int_3_0_i11);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = ((Integer) 0);
	call_localret(STATIC(mercury__string__base_string_to_int_2_4_0),
		mercury__string__base_string_to_int_3_0_i14,
		ENTRY(mercury__string__base_string_to_int_3_0));
Define_label(mercury__string__base_string_to_int_3_0_i3);
	r2 = ((Integer) 0);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__string__base_string_to_int_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__string__base_string_to_int_3_0_i1001);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module32)
	init_entry(mercury__string__to_float_2_0);
	init_label(mercury__string__to_float_2_0_i1);
BEGIN_CODE

/* code for predicate 'string__to_float'/2 in mode 0 */
Define_entry(mercury__string__to_float_2_0);
	r2 = (Integer) r1;
	{
		String	FloatString;
		Float	FloatVal;
		FloatString = (String) (Integer) r1;
		{
	/* use a temporary, since we can't don't know whether FloatVal
	   is a double or float */
	double tmp;
	SUCCESS_INDICATOR = (sscanf(FloatString, "%lf", &tmp) == 1);
		/* TRUE if sscanf succeeds, FALSE otherwise */
	FloatVal = tmp;
}
		r3 = float_to_word(FloatVal);

	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__to_float_2_0_i1);
	r2 = (Integer) r3;
	r1 = TRUE;
	proceed();
Define_label(mercury__string__to_float_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module33)
	init_entry(mercury__string__is_alpha_1_0);
	init_label(mercury__string__is_alpha_1_0_i4);
	init_label(mercury__string__is_alpha_1_0_i6);
	init_label(mercury__string__is_alpha_1_0_i3);
	init_label(mercury__string__is_alpha_1_0_i1);
BEGIN_CODE

/* code for predicate 'string__is_alpha'/1 in mode 0 */
Define_entry(mercury__string__is_alpha_1_0);
	incr_sp_push_msg(2, "string__is_alpha");
	detstackvar(2) = (Integer) succip;
	{
		call_localret(STATIC(mercury__string__first_char_3_3),
		mercury__string__is_alpha_1_0_i4,
		ENTRY(mercury__string__is_alpha_1_0));
	}
Define_label(mercury__string__is_alpha_1_0_i4);
	update_prof_current_proc(LABEL(mercury__string__is_alpha_1_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__is_alpha_1_0_i3);
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__char__is_alpha_1_0);
	call_localret(ENTRY(mercury__char__is_alpha_1_0),
		mercury__string__is_alpha_1_0_i6,
		ENTRY(mercury__string__is_alpha_1_0));
	}
Define_label(mercury__string__is_alpha_1_0_i6);
	update_prof_current_proc(LABEL(mercury__string__is_alpha_1_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__is_alpha_1_0_i1);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__string__is_alpha_1_0,
		ENTRY(mercury__string__is_alpha_1_0));
Define_label(mercury__string__is_alpha_1_0_i3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__is_alpha_1_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module34)
	init_entry(mercury__string__is_alpha_or_underscore_1_0);
	init_label(mercury__string__is_alpha_or_underscore_1_0_i4);
	init_label(mercury__string__is_alpha_or_underscore_1_0_i6);
	init_label(mercury__string__is_alpha_or_underscore_1_0_i3);
	init_label(mercury__string__is_alpha_or_underscore_1_0_i1);
BEGIN_CODE

/* code for predicate 'string__is_alpha_or_underscore'/1 in mode 0 */
Define_entry(mercury__string__is_alpha_or_underscore_1_0);
	incr_sp_push_msg(2, "string__is_alpha_or_underscore");
	detstackvar(2) = (Integer) succip;
	{
		call_localret(STATIC(mercury__string__first_char_3_3),
		mercury__string__is_alpha_or_underscore_1_0_i4,
		ENTRY(mercury__string__is_alpha_or_underscore_1_0));
	}
Define_label(mercury__string__is_alpha_or_underscore_1_0_i4);
	update_prof_current_proc(LABEL(mercury__string__is_alpha_or_underscore_1_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__is_alpha_or_underscore_1_0_i3);
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__char__is_alpha_or_underscore_1_0);
	call_localret(ENTRY(mercury__char__is_alpha_or_underscore_1_0),
		mercury__string__is_alpha_or_underscore_1_0_i6,
		ENTRY(mercury__string__is_alpha_or_underscore_1_0));
	}
Define_label(mercury__string__is_alpha_or_underscore_1_0_i6);
	update_prof_current_proc(LABEL(mercury__string__is_alpha_or_underscore_1_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__is_alpha_or_underscore_1_0_i1);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__string__is_alpha_or_underscore_1_0,
		ENTRY(mercury__string__is_alpha_or_underscore_1_0));
Define_label(mercury__string__is_alpha_or_underscore_1_0_i3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__is_alpha_or_underscore_1_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module35)
	init_entry(mercury__string__is_alnum_or_underscore_1_0);
	init_label(mercury__string__is_alnum_or_underscore_1_0_i4);
	init_label(mercury__string__is_alnum_or_underscore_1_0_i6);
	init_label(mercury__string__is_alnum_or_underscore_1_0_i3);
	init_label(mercury__string__is_alnum_or_underscore_1_0_i1);
BEGIN_CODE

/* code for predicate 'string__is_alnum_or_underscore'/1 in mode 0 */
Define_entry(mercury__string__is_alnum_or_underscore_1_0);
	incr_sp_push_msg(2, "string__is_alnum_or_underscore");
	detstackvar(2) = (Integer) succip;
	{
		call_localret(STATIC(mercury__string__first_char_3_3),
		mercury__string__is_alnum_or_underscore_1_0_i4,
		ENTRY(mercury__string__is_alnum_or_underscore_1_0));
	}
Define_label(mercury__string__is_alnum_or_underscore_1_0_i4);
	update_prof_current_proc(LABEL(mercury__string__is_alnum_or_underscore_1_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__is_alnum_or_underscore_1_0_i3);
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__char__is_alnum_or_underscore_1_0);
	call_localret(ENTRY(mercury__char__is_alnum_or_underscore_1_0),
		mercury__string__is_alnum_or_underscore_1_0_i6,
		ENTRY(mercury__string__is_alnum_or_underscore_1_0));
	}
Define_label(mercury__string__is_alnum_or_underscore_1_0_i6);
	update_prof_current_proc(LABEL(mercury__string__is_alnum_or_underscore_1_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__is_alnum_or_underscore_1_0_i1);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__string__is_alnum_or_underscore_1_0,
		ENTRY(mercury__string__is_alnum_or_underscore_1_0));
Define_label(mercury__string__is_alnum_or_underscore_1_0_i3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__is_alnum_or_underscore_1_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module36)
	init_entry(mercury__string__pad_left_4_0);
	init_label(mercury__string__pad_left_4_0_i2);
	init_label(mercury__string__pad_left_4_0_i5);
	init_label(mercury__string__pad_left_4_0_i3);
BEGIN_CODE

/* code for predicate 'string__pad_left'/4 in mode 0 */
Define_entry(mercury__string__pad_left_4_0);
	incr_sp_push_msg(4, "string__pad_left");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	{
		call_localret(STATIC(mercury__string__length_2_0),
		mercury__string__pad_left_4_0_i2,
		ENTRY(mercury__string__pad_left_4_0));
	}
Define_label(mercury__string__pad_left_4_0_i2);
	update_prof_current_proc(LABEL(mercury__string__pad_left_4_0));
	if (((Integer) r1 >= (Integer) detstackvar(3)))
		GOTO_LABEL(mercury__string__pad_left_4_0_i3);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = ((Integer) detstackvar(3) - (Integer) r2);
	{
		call_localret(STATIC(mercury__string__duplicate_char_3_0),
		mercury__string__pad_left_4_0_i5,
		ENTRY(mercury__string__pad_left_4_0));
	}
Define_label(mercury__string__pad_left_4_0_i5);
	update_prof_current_proc(LABEL(mercury__string__pad_left_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
		tailcall(STATIC(mercury__string__append_3_2),
		ENTRY(mercury__string__pad_left_4_0));
	}
Define_label(mercury__string__pad_left_4_0_i3);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module37)
	init_entry(mercury__string__pad_right_4_0);
	init_label(mercury__string__pad_right_4_0_i2);
	init_label(mercury__string__pad_right_4_0_i5);
	init_label(mercury__string__pad_right_4_0_i3);
BEGIN_CODE

/* code for predicate 'string__pad_right'/4 in mode 0 */
Define_entry(mercury__string__pad_right_4_0);
	incr_sp_push_msg(4, "string__pad_right");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	{
		call_localret(STATIC(mercury__string__length_2_0),
		mercury__string__pad_right_4_0_i2,
		ENTRY(mercury__string__pad_right_4_0));
	}
Define_label(mercury__string__pad_right_4_0_i2);
	update_prof_current_proc(LABEL(mercury__string__pad_right_4_0));
	if (((Integer) r1 >= (Integer) detstackvar(3)))
		GOTO_LABEL(mercury__string__pad_right_4_0_i3);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = ((Integer) detstackvar(3) - (Integer) r2);
	{
		call_localret(STATIC(mercury__string__duplicate_char_3_0),
		mercury__string__pad_right_4_0_i5,
		ENTRY(mercury__string__pad_right_4_0));
	}
Define_label(mercury__string__pad_right_4_0_i5);
	update_prof_current_proc(LABEL(mercury__string__pad_right_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
		tailcall(STATIC(mercury__string__append_3_2),
		ENTRY(mercury__string__pad_right_4_0));
	}
Define_label(mercury__string__pad_right_4_0_i3);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module38)
	init_entry(mercury__string__duplicate_char_3_0);
	init_label(mercury__string__duplicate_char_3_0_i1000);
	init_label(mercury__string__duplicate_char_3_0_i4);
BEGIN_CODE

/* code for predicate 'string__duplicate_char'/3 in mode 0 */
Define_entry(mercury__string__duplicate_char_3_0);
	if (((Integer) r2 > ((Integer) 0)))
		GOTO_LABEL(mercury__string__duplicate_char_3_0_i1000);
	r1 = string_const("", 0);
	proceed();
Define_label(mercury__string__duplicate_char_3_0_i1000);
	incr_sp_push_msg(2, "string__duplicate_char");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r2 = ((Integer) r2 - ((Integer) 1));
	localcall(mercury__string__duplicate_char_3_0,
		LABEL(mercury__string__duplicate_char_3_0_i4),
		ENTRY(mercury__string__duplicate_char_3_0));
Define_label(mercury__string__duplicate_char_3_0_i4);
	update_prof_current_proc(LABEL(mercury__string__duplicate_char_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__string__first_char_3_4),
		ENTRY(mercury__string__duplicate_char_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module39)
	init_entry(mercury__string__index_3_0);
	init_label(mercury__string__index_3_0_i1);
BEGIN_CODE

/* code for predicate 'string__index'/3 in mode 0 */
Define_entry(mercury__string__index_3_0);
	r3 = (Integer) r1;
	{
		String	Str;
		Integer	Index;
		Char	Ch;
		Str = (String) (Integer) r1;
		Index = (Integer) r2;
		
	if ((Word) Index >= strlen(Str)) {
		SUCCESS_INDICATOR = FALSE;
	} else {
		SUCCESS_INDICATOR = TRUE;
		Ch = Str[Index];
	}

		r4 = Ch;

	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__index_3_0_i1);
	r2 = (Integer) r4;
	r1 = TRUE;
	proceed();
Define_label(mercury__string__index_3_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module40)
	init_entry(mercury__string__index_det_3_0);
	init_label(mercury__string__index_det_3_0_i4);
	init_label(mercury__string__index_det_3_0_i1000);
BEGIN_CODE

/* code for predicate 'string__index_det'/3 in mode 0 */
Define_entry(mercury__string__index_det_3_0);
	incr_sp_push_msg(1, "string__index_det");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__string__index_3_0),
		mercury__string__index_det_3_0_i4,
		ENTRY(mercury__string__index_det_3_0));
	}
Define_label(mercury__string__index_det_3_0_i4);
	update_prof_current_proc(LABEL(mercury__string__index_det_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__index_det_3_0_i1000);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__string__index_det_3_0_i1000);
	r1 = string_const("string__index_det: index out of range", 37);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__string__index_det_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module41)
	init_entry(mercury__string__split_4_0);
BEGIN_CODE

/* code for predicate 'string__split'/4 in mode 0 */
Define_entry(mercury__string__split_4_0);
	{
		String	Str;
		Integer	Count;
		String	Left;
		String	Right;
		Str = (String) (Integer) r1;
		Count = (Integer) r2;
		{
	Integer len;
	Word tmp;
	if (Count <= 0) {
		/* XXX need to guarantee alignment of strings */
		Left = (String) (Word) "";
		Right = Str;
	} else {
		len = strlen(Str);
		if (Count > len) Count = len;
		incr_hp_atomic(tmp, (Count + sizeof(Word)) / sizeof(Word));
		Left = (char *) tmp;
		memcpy(Left, Str, Count);
		Left[Count] = '\0';
		incr_hp_atomic(tmp,
			(len - Count + sizeof(Word)) / sizeof(Word));
		Right = (char *) tmp;
		strcpy(Right, Str + Count);
	}
}
		r3 = (Word) Left;
		r4 = (Word) Right;

	}
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module42)
	init_entry(mercury__string__left_3_0);
BEGIN_CODE

/* code for predicate 'string__left'/3 in mode 0 */
Define_entry(mercury__string__left_3_0);
	{
		tailcall(STATIC(mercury__string__split_4_0),
		ENTRY(mercury__string__left_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module43)
	init_entry(mercury__string__right_3_0);
	init_label(mercury__string__right_3_0_i2);
	init_label(mercury__string__right_3_0_i3);
BEGIN_CODE

/* code for predicate 'string__right'/3 in mode 0 */
Define_entry(mercury__string__right_3_0);
	incr_sp_push_msg(3, "string__right");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__length_2_0),
		mercury__string__right_3_0_i2,
		ENTRY(mercury__string__right_3_0));
	}
Define_label(mercury__string__right_3_0_i2);
	update_prof_current_proc(LABEL(mercury__string__right_3_0));
	r2 = ((Integer) r1 - (Integer) detstackvar(2));
	r1 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__string__split_4_0),
		mercury__string__right_3_0_i3,
		ENTRY(mercury__string__right_3_0));
	}
Define_label(mercury__string__right_3_0_i3);
	update_prof_current_proc(LABEL(mercury__string__right_3_0));
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module44)
	init_entry(mercury__string__substring_4_0);
	init_label(mercury__string__substring_4_0_i2);
BEGIN_CODE

/* code for predicate 'string__substring'/4 in mode 0 */
Define_entry(mercury__string__substring_4_0);
	incr_sp_push_msg(2, "string__substring");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	{
		call_localret(STATIC(mercury__string__right_3_0),
		mercury__string__substring_4_0_i2,
		ENTRY(mercury__string__substring_4_0));
	}
Define_label(mercury__string__substring_4_0_i2);
	update_prof_current_proc(LABEL(mercury__string__substring_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__string__left_3_0),
		ENTRY(mercury__string__substring_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module45)
	init_entry(mercury__string__append_list_2_0);
	init_label(mercury__string__append_list_2_0_i4);
	init_label(mercury__string__append_list_2_0_i1002);
BEGIN_CODE

/* code for predicate 'string__append_list'/2 in mode 0 */
Define_entry(mercury__string__append_list_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__append_list_2_0_i1002);
	incr_sp_push_msg(2, "string__append_list");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localcall(mercury__string__append_list_2_0,
		LABEL(mercury__string__append_list_2_0_i4),
		ENTRY(mercury__string__append_list_2_0));
Define_label(mercury__string__append_list_2_0_i4);
	update_prof_current_proc(LABEL(mercury__string__append_list_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__string__append_3_2),
		ENTRY(mercury__string__append_list_2_0));
	}
Define_label(mercury__string__append_list_2_0_i1002);
	r1 = string_const("", 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module46)
	init_entry(mercury__string__append_list_2_1);
	init_label(mercury__string__append_list_2_1_i3);
	init_label(mercury__string__append_list_2_1_i1007);
	init_label(mercury__string__append_list_2_1_i5);
	init_label(mercury__string__append_list_2_1_i6);
BEGIN_CODE

/* code for predicate 'string__append_list'/2 in mode 1 */
Define_entry(mercury__string__append_list_2_1);
	mkframe("string__append_list/2", 1, LABEL(mercury__string__append_list_2_1_i3));
	if ((strcmp((char *)(Integer) r1, (char *)string_const("", 0)) !=0))
		GOTO_LABEL(mercury__string__append_list_2_1_i1007);
	framevar(0) = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	succeed();
Define_label(mercury__string__append_list_2_1_i3);
	update_prof_current_proc(LABEL(mercury__string__append_list_2_1));
	r1 = (Integer) framevar(0);
Define_label(mercury__string__append_list_2_1_i1007);
	{
	Declare_entry(do_fail);
	LVALUE_CAST(Word,bt_redoip((Integer) maxfr)) = (Integer) ENTRY(do_fail);
	}
	{
		call_localret(STATIC(mercury__string__append_3_3),
		mercury__string__append_list_2_1_i5,
		ENTRY(mercury__string__append_list_2_1));
	}
Define_label(mercury__string__append_list_2_1_i5);
	update_prof_current_proc(LABEL(mercury__string__append_list_2_1));
	framevar(0) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__string__append_list_2_1,
		LABEL(mercury__string__append_list_2_1_i6),
		ENTRY(mercury__string__append_list_2_1));
Define_label(mercury__string__append_list_2_1_i6);
	update_prof_current_proc(LABEL(mercury__string__append_list_2_1));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) framevar(0);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	succeed();
END_MODULE

BEGIN_MODULE(mercury__string_module47)
	init_entry(mercury__string__hash_2_0);
	init_label(mercury__string__hash_2_0_i2);
	init_label(mercury__string__hash_2_0_i3);
	init_label(mercury__string__hash_2_0_i4);
BEGIN_CODE

/* code for predicate 'string__hash'/2 in mode 0 */
Define_entry(mercury__string__hash_2_0);
	incr_sp_push_msg(2, "string__hash");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
		call_localret(STATIC(mercury__string__length_2_0),
		mercury__string__hash_2_0_i2,
		ENTRY(mercury__string__hash_2_0));
	}
Define_label(mercury__string__hash_2_0_i2);
	update_prof_current_proc(LABEL(mercury__string__hash_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__string__to_int_list_2_1),
		mercury__string__hash_2_0_i3,
		ENTRY(mercury__string__hash_2_0));
Define_label(mercury__string__hash_2_0_i3);
	update_prof_current_proc(LABEL(mercury__string__hash_2_0));
	r2 = ((Integer) 0);
	call_localret(STATIC(mercury__string__hash_2_3_0),
		mercury__string__hash_2_0_i4,
		ENTRY(mercury__string__hash_2_0));
Define_label(mercury__string__hash_2_0_i4);
	update_prof_current_proc(LABEL(mercury__string__hash_2_0));
	r1 = ((Integer) r1 ^ (Integer) detstackvar(1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module48)
	init_entry(mercury__string__sub_string_search_3_0);
	init_label(mercury__string__sub_string_search_3_0_i2);
	init_label(mercury__string__sub_string_search_3_0_i3);
	init_label(mercury__string__sub_string_search_3_0_i4);
	init_label(mercury__string__sub_string_search_3_0_i1000);
BEGIN_CODE

/* code for predicate 'string__sub_string_search'/3 in mode 0 */
Define_entry(mercury__string__sub_string_search_3_0);
	incr_sp_push_msg(4, "string__sub_string_search");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__length_2_0),
		mercury__string__sub_string_search_3_0_i2,
		ENTRY(mercury__string__sub_string_search_3_0));
	}
Define_label(mercury__string__sub_string_search_3_0_i2);
	update_prof_current_proc(LABEL(mercury__string__sub_string_search_3_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__string__length_2_0),
		mercury__string__sub_string_search_3_0_i3,
		ENTRY(mercury__string__sub_string_search_3_0));
	}
Define_label(mercury__string__sub_string_search_3_0_i3);
	update_prof_current_proc(LABEL(mercury__string__sub_string_search_3_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__string__sub_string_search2_5_0),
		mercury__string__sub_string_search_3_0_i4,
		ENTRY(mercury__string__sub_string_search_3_0));
Define_label(mercury__string__sub_string_search_3_0_i4);
	update_prof_current_proc(LABEL(mercury__string__sub_string_search_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__sub_string_search_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__string__sub_string_search_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module49)
	init_entry(mercury__string__format_3_0);
	init_label(mercury__string__format_3_0_i2);
BEGIN_CODE

/* code for predicate 'string__format'/3 in mode 0 */
Define_entry(mercury__string__format_3_0);
	incr_sp_push_msg(2, "string__format");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__format_3_0_i2,
		ENTRY(mercury__string__format_3_0));
	}
Define_label(mercury__string__format_3_0_i2);
	update_prof_current_proc(LABEL(mercury__string__format_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__string__format_2_3_0),
		ENTRY(mercury__string__format_3_0));
END_MODULE

BEGIN_MODULE(mercury__string_module50)
	init_entry(mercury__string__to_int_list_2_0);
BEGIN_CODE

/* code for predicate 'string__to_int_list'/2 in mode 0 */
Define_static(mercury__string__to_int_list_2_0);
	{
		String	Str;
		Word	IntList;
		IntList = (Integer) r1;
		{
		/* mode (out, in) is det */
	Word int_list_ptr;
	Word size;
	Word str_ptr;
/*
** loop to calculate list length + sizeof(Word) in `size' using list in
** `int_list_ptr'
*/
	size = sizeof(Word);
	int_list_ptr = IntList;
	while (!list_is_empty(int_list_ptr)) {
		size++;
		int_list_ptr = list_tail(int_list_ptr);
	}
/*
** allocate (length + 1) bytes of heap space for string
** i.e. (length + 1 + sizeof(Word) - 1) / sizeof(Word) words
*/
	incr_hp_atomic(str_ptr, size / sizeof(Word));
	Str = (char *) str_ptr;
/*
** loop to copy the characters from the int_list to the string
*/
	size = 0;
	int_list_ptr = IntList;
	while (!list_is_empty(int_list_ptr)) {
		Str[size++] = (char) list_head(int_list_ptr);
		int_list_ptr = list_tail(int_list_ptr);
	}
/*
** null terminate the string
*/
	Str[size] = '\0';
}
		r2 = (Word) Str;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module51)
	init_entry(mercury__string__to_int_list_2_1);
BEGIN_CODE

/* code for predicate 'string__to_int_list'/2 in mode 1 */
Define_static(mercury__string__to_int_list_2_1);
	{
		String	Str;
		Word	IntList;
		Str = (String) (Integer) r1;
		{
	char *p = Str + strlen(Str);
	IntList = list_empty();
	while (--p >= Str) {
		IntList = list_cons(*p, IntList);
	}
}
		r2 = IntList;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module52)
	init_entry(mercury__string__find_all_sub_charlist_4_0);
	init_label(mercury__string__find_all_sub_charlist_4_0_i4);
	init_label(mercury__string__find_all_sub_charlist_4_0_i6);
	init_label(mercury__string__find_all_sub_charlist_4_0_i10);
	init_label(mercury__string__find_all_sub_charlist_4_0_i11);
	init_label(mercury__string__find_all_sub_charlist_4_0_i3);
BEGIN_CODE

/* code for predicate 'find_all_sub_charlist'/4 in mode 0 */
Define_static(mercury__string__find_all_sub_charlist_4_0);
	incr_sp_push_msg(4, "find_all_sub_charlist");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	call_localret(STATIC(mercury__string__find_sub_charlist_4_0),
		mercury__string__find_all_sub_charlist_4_0_i4,
		STATIC(mercury__string__find_all_sub_charlist_4_0));
Define_label(mercury__string__find_all_sub_charlist_4_0_i4);
	update_prof_current_proc(LABEL(mercury__string__find_all_sub_charlist_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__find_all_sub_charlist_4_0_i3);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__find_all_sub_charlist_4_0_i6);
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__list__append_3_1);
	tailcall(ENTRY(mercury__list__append_3_1),
		STATIC(mercury__string__find_all_sub_charlist_4_0));
	}
Define_label(mercury__string__find_all_sub_charlist_4_0_i6);
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) tempr1;
	r1 = (Integer) r3;
	r3 = (Integer) detstackvar(3);
	localcall(mercury__string__find_all_sub_charlist_4_0,
		LABEL(mercury__string__find_all_sub_charlist_4_0_i10),
		STATIC(mercury__string__find_all_sub_charlist_4_0));
	}
Define_label(mercury__string__find_all_sub_charlist_4_0_i10);
	update_prof_current_proc(LABEL(mercury__string__find_all_sub_charlist_4_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__list__append_3_1);
	call_localret(ENTRY(mercury__list__append_3_1),
		mercury__string__find_all_sub_charlist_4_0_i11,
		STATIC(mercury__string__find_all_sub_charlist_4_0));
	}
Define_label(mercury__string__find_all_sub_charlist_4_0_i11);
	update_prof_current_proc(LABEL(mercury__string__find_all_sub_charlist_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__list__append_3_1);
	tailcall(ENTRY(mercury__list__append_3_1),
		STATIC(mercury__string__find_all_sub_charlist_4_0));
	}
Define_label(mercury__string__find_all_sub_charlist_4_0_i3);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module53)
	init_entry(mercury__string__find_sub_charlist_4_0);
	init_label(mercury__string__find_sub_charlist_4_0_i1001);
	init_label(mercury__string__find_sub_charlist_4_0_i9);
	init_label(mercury__string__find_sub_charlist_4_0_i8);
	init_label(mercury__string__find_sub_charlist_4_0_i11);
	init_label(mercury__string__find_sub_charlist_4_0_i5);
	init_label(mercury__string__find_sub_charlist_4_0_i14);
	init_label(mercury__string__find_sub_charlist_4_0_i1);
BEGIN_CODE

/* code for predicate 'find_sub_charlist'/4 in mode 0 */
Define_static(mercury__string__find_sub_charlist_4_0);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__find_sub_charlist_4_0_i1001);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) r1;
	r1 = TRUE;
	proceed();
Define_label(mercury__string__find_sub_charlist_4_0_i1001);
	incr_sp_push_msg(4, "find_sub_charlist");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__find_sub_charlist_4_0_i1);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r5 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	if (((Integer) r4 != (Integer) r5))
		GOTO_LABEL(mercury__string__find_sub_charlist_4_0_i5);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	{
	Declare_entry(mercury__list__append_3_3);
	call_localret(ENTRY(mercury__list__append_3_3),
		mercury__string__find_sub_charlist_4_0_i9,
		STATIC(mercury__string__find_sub_charlist_4_0));
	}
Define_label(mercury__string__find_sub_charlist_4_0_i9);
	update_prof_current_proc(LABEL(mercury__string__find_sub_charlist_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__find_sub_charlist_4_0_i8);
	r3 = (Integer) r2;
	r1 = TRUE;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__find_sub_charlist_4_0_i8);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	localcall(mercury__string__find_sub_charlist_4_0,
		LABEL(mercury__string__find_sub_charlist_4_0_i11),
		STATIC(mercury__string__find_sub_charlist_4_0));
Define_label(mercury__string__find_sub_charlist_4_0_i11);
	update_prof_current_proc(LABEL(mercury__string__find_sub_charlist_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__find_sub_charlist_4_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__find_sub_charlist_4_0_i5);
	detstackvar(2) = (Integer) r4;
	r1 = (Integer) r3;
	localcall(mercury__string__find_sub_charlist_4_0,
		LABEL(mercury__string__find_sub_charlist_4_0_i14),
		STATIC(mercury__string__find_sub_charlist_4_0));
Define_label(mercury__string__find_sub_charlist_4_0_i14);
	update_prof_current_proc(LABEL(mercury__string__find_sub_charlist_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__find_sub_charlist_4_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__find_sub_charlist_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module54)
	init_entry(mercury__string__base_string_to_int_2_4_0);
	init_label(mercury__string__base_string_to_int_2_4_0_i4);
	init_label(mercury__string__base_string_to_int_2_4_0_i6);
	init_label(mercury__string__base_string_to_int_2_4_0_i8);
	init_label(mercury__string__base_string_to_int_2_4_0_i3);
	init_label(mercury__string__base_string_to_int_2_4_0_i1);
	init_label(mercury__string__base_string_to_int_2_4_0_i1000);
BEGIN_CODE

/* code for predicate 'string__base_string_to_int_2'/4 in mode 0 */
Define_static(mercury__string__base_string_to_int_2_4_0);
	incr_sp_push_msg(4, "string__base_string_to_int_2");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r3;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__first_char_3_3),
		mercury__string__base_string_to_int_2_4_0_i4,
		STATIC(mercury__string__base_string_to_int_2_4_0));
	}
Define_label(mercury__string__base_string_to_int_2_4_0_i4);
	update_prof_current_proc(LABEL(mercury__string__base_string_to_int_2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__base_string_to_int_2_4_0_i3);
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__char__to_upper_2_0);
	call_localret(ENTRY(mercury__char__to_upper_2_0),
		mercury__string__base_string_to_int_2_4_0_i6,
		STATIC(mercury__string__base_string_to_int_2_4_0));
	}
Define_label(mercury__string__base_string_to_int_2_4_0_i6);
	update_prof_current_proc(LABEL(mercury__string__base_string_to_int_2_4_0));
	{
	Word tempr1, tempr2, tempr3, tempr4;
	tempr2 = (Unsigned)((Integer) r1);
	{
	static const Word mercury_const_1[] = {
		((Integer) 0),
		((Integer) 67043328),
		((Integer) 134217726)
	};
	if (!(((((Integer) 1) << ((Integer) tempr2 % ((Integer) 32))) & (Integer) field(mktag(0), mkword(mktag(0), mercury_const_1), ((Integer) tempr2 / ((Integer) 32))))))
		GOTO_LABEL(mercury__string__base_string_to_int_2_4_0_i1);
	}
	{
	static const Word mercury_const_2[] = {
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 1),
		((Integer) 2),
		((Integer) 3),
		((Integer) 4),
		((Integer) 5),
		((Integer) 6),
		((Integer) 7),
		((Integer) 8),
		((Integer) 9),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 0),
		((Integer) 10),
		((Integer) 11),
		((Integer) 12),
		((Integer) 13),
		((Integer) 14),
		((Integer) 15),
		((Integer) 16),
		((Integer) 17),
		((Integer) 18),
		((Integer) 19),
		((Integer) 20),
		((Integer) 21),
		((Integer) 22),
		((Integer) 23),
		((Integer) 24),
		((Integer) 25),
		((Integer) 26),
		((Integer) 27),
		((Integer) 28),
		((Integer) 29),
		((Integer) 30),
		((Integer) 31),
		((Integer) 32),
		((Integer) 33),
		((Integer) 34),
		((Integer) 35)
	};
	tempr3 = (Integer) field(mktag(0), mkword(mktag(0), mercury_const_2), (Integer) r1);
	}
	tempr4 = (Integer) detstackvar(1);
	if (((Integer) tempr3 >= (Integer) tempr4))
		GOTO_LABEL(mercury__string__base_string_to_int_2_4_0_i1);
	r1 = (Integer) tempr4;
	r2 = (Integer) detstackvar(3);
	r3 = (((Integer) tempr4 * (Integer) detstackvar(2)) + (Integer) tempr3);
	localcall(mercury__string__base_string_to_int_2_4_0,
		LABEL(mercury__string__base_string_to_int_2_4_0_i8),
		STATIC(mercury__string__base_string_to_int_2_4_0));
	}
Define_label(mercury__string__base_string_to_int_2_4_0_i8);
	update_prof_current_proc(LABEL(mercury__string__base_string_to_int_2_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if ((Integer) r1)
		GOTO_LABEL(mercury__string__base_string_to_int_2_4_0_i1000);
	r1 = FALSE;
	proceed();
Define_label(mercury__string__base_string_to_int_2_4_0_i3);
	r2 = (Integer) detstackvar(2);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__base_string_to_int_2_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__base_string_to_int_2_4_0_i1000);
	r1 = TRUE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module55)
	init_entry(mercury__string__int_to_base_string_2_3_0);
	init_label(mercury__string__int_to_base_string_2_3_0_i4);
	init_label(mercury__string__int_to_base_string_2_3_0_i1001);
	init_label(mercury__string__int_to_base_string_2_3_0_i6);
	init_label(mercury__string__int_to_base_string_2_3_0_i7);
	init_label(mercury__string__int_to_base_string_2_3_0_i8);
BEGIN_CODE

/* code for predicate 'string__int_to_base_string_2'/3 in mode 0 */
Define_static(mercury__string__int_to_base_string_2_3_0);
	if (((Integer) r1 >= (Integer) r2))
		GOTO_LABEL(mercury__string__int_to_base_string_2_3_0_i1001);
	incr_sp_push_msg(3, "string__int_to_base_string_2");
	detstackvar(3) = (Integer) succip;
	call_localret(STATIC(mercury__string__digit_to_char_det_2_0),
		mercury__string__int_to_base_string_2_3_0_i4,
		STATIC(mercury__string__int_to_base_string_2_3_0));
Define_label(mercury__string__int_to_base_string_2_3_0_i4);
	update_prof_current_proc(LABEL(mercury__string__int_to_base_string_2_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__string__char_to_string_2_0),
		STATIC(mercury__string__int_to_base_string_2_3_0));
	}
Define_label(mercury__string__int_to_base_string_2_3_0_i1001);
	incr_sp_push_msg(3, "string__int_to_base_string_2");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = ((Integer) r1 / (Integer) r2);
	r1 = ((Integer) r1 % (Integer) r2);
	call_localret(STATIC(mercury__string__digit_to_char_det_2_0),
		mercury__string__int_to_base_string_2_3_0_i6,
		STATIC(mercury__string__int_to_base_string_2_3_0));
Define_label(mercury__string__int_to_base_string_2_3_0_i6);
	update_prof_current_proc(LABEL(mercury__string__int_to_base_string_2_3_0));
	{
		call_localret(STATIC(mercury__string__char_to_string_2_0),
		mercury__string__int_to_base_string_2_3_0_i7,
		STATIC(mercury__string__int_to_base_string_2_3_0));
	}
Define_label(mercury__string__int_to_base_string_2_3_0_i7);
	update_prof_current_proc(LABEL(mercury__string__int_to_base_string_2_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	localcall(mercury__string__int_to_base_string_2_3_0,
		LABEL(mercury__string__int_to_base_string_2_3_0_i8),
		STATIC(mercury__string__int_to_base_string_2_3_0));
Define_label(mercury__string__int_to_base_string_2_3_0_i8);
	update_prof_current_proc(LABEL(mercury__string__int_to_base_string_2_3_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__string__append_3_2),
		STATIC(mercury__string__int_to_base_string_2_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module56)
	init_entry(mercury__string__digit_to_char_det_2_0);
	init_label(mercury__string__digit_to_char_det_2_0_i1003);
BEGIN_CODE

/* code for predicate 'string__digit_to_char_det'/2 in mode 0 */
Define_static(mercury__string__digit_to_char_det_2_0);
	if (((Unsigned)((Integer) r1) > ((Integer) 35)))
		GOTO_LABEL(mercury__string__digit_to_char_det_2_0_i1003);
	{
	static const Word mercury_const_1[] = {
		((Integer) 48),
		((Integer) 49),
		((Integer) 50),
		((Integer) 51),
		((Integer) 52),
		((Integer) 53),
		((Integer) 54),
		((Integer) 55),
		((Integer) 56),
		((Integer) 57),
		((Integer) 65),
		((Integer) 66),
		((Integer) 67),
		((Integer) 68),
		((Integer) 69),
		((Integer) 70),
		((Integer) 71),
		((Integer) 72),
		((Integer) 73),
		((Integer) 74),
		((Integer) 75),
		((Integer) 76),
		((Integer) 77),
		((Integer) 78),
		((Integer) 79),
		((Integer) 80),
		((Integer) 81),
		((Integer) 82),
		((Integer) 83),
		((Integer) 84),
		((Integer) 85),
		((Integer) 86),
		((Integer) 87),
		((Integer) 88),
		((Integer) 89),
		((Integer) 90)
	};
	r1 = (Integer) field(mktag(0), mkword(mktag(0), mercury_const_1), (Integer) r1);
	}
	proceed();
Define_label(mercury__string__digit_to_char_det_2_0_i1003);
	r1 = string_const("string__digit_to_char failed", 28);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__string__digit_to_char_det_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module57)
	init_entry(mercury__string__int_list_to_char_list_2_0);
	init_label(mercury__string__int_list_to_char_list_2_0_i6);
	init_label(mercury__string__int_list_to_char_list_2_0_i5);
	init_label(mercury__string__int_list_to_char_list_2_0_i8);
	init_label(mercury__string__int_list_to_char_list_2_0_i9);
	init_label(mercury__string__int_list_to_char_list_2_0_i10);
	init_label(mercury__string__int_list_to_char_list_2_0_i1003);
BEGIN_CODE

/* code for predicate 'string__int_list_to_char_list'/2 in mode 0 */
Define_static(mercury__string__int_list_to_char_list_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__int_list_to_char_list_2_0_i1003);
	incr_sp_push_msg(3, "string__int_list_to_char_list");
	detstackvar(3) = (Integer) succip;
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__char__to_int_2_2);
	call_localret(ENTRY(mercury__char__to_int_2_2),
		mercury__string__int_list_to_char_list_2_0_i6,
		STATIC(mercury__string__int_list_to_char_list_2_0));
	}
Define_label(mercury__string__int_list_to_char_list_2_0_i6);
	update_prof_current_proc(LABEL(mercury__string__int_list_to_char_list_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__int_list_to_char_list_2_0_i5);
	r1 = (Integer) detstackvar(1);
	GOTO_LABEL(mercury__string__int_list_to_char_list_2_0_i9);
Define_label(mercury__string__int_list_to_char_list_2_0_i5);
	r1 = string_const("string__int_list_to_char_list: char__to_int failed", 50);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__string__int_list_to_char_list_2_0_i8,
		STATIC(mercury__string__int_list_to_char_list_2_0));
	}
Define_label(mercury__string__int_list_to_char_list_2_0_i8);
	update_prof_current_proc(LABEL(mercury__string__int_list_to_char_list_2_0));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
Define_label(mercury__string__int_list_to_char_list_2_0_i9);
	detstackvar(2) = (Integer) r2;
	localcall(mercury__string__int_list_to_char_list_2_0,
		LABEL(mercury__string__int_list_to_char_list_2_0_i10),
		STATIC(mercury__string__int_list_to_char_list_2_0));
Define_label(mercury__string__int_list_to_char_list_2_0_i10);
	update_prof_current_proc(LABEL(mercury__string__int_list_to_char_list_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__string__int_list_to_char_list_2_0_i1003);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module58)
	init_entry(mercury__string__char_list_to_int_list_2_1);
	init_label(mercury__string__char_list_to_int_list_2_1_i1001);
	init_label(mercury__string__char_list_to_int_list_2_1_i4);
	init_label(mercury__string__char_list_to_int_list_2_1_i6);
	init_label(mercury__string__char_list_to_int_list_2_1_i1);
BEGIN_CODE

/* code for predicate 'string__char_list_to_int_list'/2 in mode 1 */
Define_static(mercury__string__char_list_to_int_list_2_1);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__char_list_to_int_list_2_1_i1001);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = TRUE;
	proceed();
Define_label(mercury__string__char_list_to_int_list_2_1_i1001);
	incr_sp_push_msg(2, "string__char_list_to_int_list");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__char__to_int_2_2);
	call_localret(ENTRY(mercury__char__to_int_2_2),
		mercury__string__char_list_to_int_list_2_1_i4,
		STATIC(mercury__string__char_list_to_int_list_2_1));
	}
Define_label(mercury__string__char_list_to_int_list_2_1_i4);
	update_prof_current_proc(LABEL(mercury__string__char_list_to_int_list_2_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__char_list_to_int_list_2_1_i1);
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	localcall(mercury__string__char_list_to_int_list_2_1,
		LABEL(mercury__string__char_list_to_int_list_2_1_i6),
		STATIC(mercury__string__char_list_to_int_list_2_1));
Define_label(mercury__string__char_list_to_int_list_2_1_i6);
	update_prof_current_proc(LABEL(mercury__string__char_list_to_int_list_2_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__char_list_to_int_list_2_1_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__char_list_to_int_list_2_1_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module59)
	init_entry(mercury__string__char_list_to_int_list_2_0);
	init_label(mercury__string__char_list_to_int_list_2_0_i4);
	init_label(mercury__string__char_list_to_int_list_2_0_i5);
	init_label(mercury__string__char_list_to_int_list_2_0_i1002);
BEGIN_CODE

/* code for predicate 'string__char_list_to_int_list'/2 in mode 0 */
Define_static(mercury__string__char_list_to_int_list_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__char_list_to_int_list_2_0_i1002);
	incr_sp_push_msg(2, "string__char_list_to_int_list");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__char__to_int_2_0);
	call_localret(ENTRY(mercury__char__to_int_2_0),
		mercury__string__char_list_to_int_list_2_0_i4,
		STATIC(mercury__string__char_list_to_int_list_2_0));
	}
Define_label(mercury__string__char_list_to_int_list_2_0_i4);
	update_prof_current_proc(LABEL(mercury__string__char_list_to_int_list_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__string__char_list_to_int_list_2_0,
		LABEL(mercury__string__char_list_to_int_list_2_0_i5),
		STATIC(mercury__string__char_list_to_int_list_2_0));
Define_label(mercury__string__char_list_to_int_list_2_0_i5);
	update_prof_current_proc(LABEL(mercury__string__char_list_to_int_list_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__char_list_to_int_list_2_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module60)
	init_entry(mercury__string__char_list_to_upper_2_0);
	init_label(mercury__string__char_list_to_upper_2_0_i4);
	init_label(mercury__string__char_list_to_upper_2_0_i5);
	init_label(mercury__string__char_list_to_upper_2_0_i1002);
BEGIN_CODE

/* code for predicate 'string__char_list_to_upper'/2 in mode 0 */
Define_static(mercury__string__char_list_to_upper_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__char_list_to_upper_2_0_i1002);
	incr_sp_push_msg(2, "string__char_list_to_upper");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__char__to_upper_2_0);
	call_localret(ENTRY(mercury__char__to_upper_2_0),
		mercury__string__char_list_to_upper_2_0_i4,
		STATIC(mercury__string__char_list_to_upper_2_0));
	}
Define_label(mercury__string__char_list_to_upper_2_0_i4);
	update_prof_current_proc(LABEL(mercury__string__char_list_to_upper_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__string__char_list_to_upper_2_0,
		LABEL(mercury__string__char_list_to_upper_2_0_i5),
		STATIC(mercury__string__char_list_to_upper_2_0));
Define_label(mercury__string__char_list_to_upper_2_0_i5);
	update_prof_current_proc(LABEL(mercury__string__char_list_to_upper_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__char_list_to_upper_2_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module61)
	init_entry(mercury__string__char_list_to_lower_2_0);
	init_label(mercury__string__char_list_to_lower_2_0_i4);
	init_label(mercury__string__char_list_to_lower_2_0_i5);
	init_label(mercury__string__char_list_to_lower_2_0_i1002);
BEGIN_CODE

/* code for predicate 'string__char_list_to_lower'/2 in mode 0 */
Define_static(mercury__string__char_list_to_lower_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__char_list_to_lower_2_0_i1002);
	incr_sp_push_msg(2, "string__char_list_to_lower");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__char__to_lower_2_0);
	call_localret(ENTRY(mercury__char__to_lower_2_0),
		mercury__string__char_list_to_lower_2_0_i4,
		STATIC(mercury__string__char_list_to_lower_2_0));
	}
Define_label(mercury__string__char_list_to_lower_2_0_i4);
	update_prof_current_proc(LABEL(mercury__string__char_list_to_lower_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__string__char_list_to_lower_2_0,
		LABEL(mercury__string__char_list_to_lower_2_0_i5),
		STATIC(mercury__string__char_list_to_lower_2_0));
Define_label(mercury__string__char_list_to_lower_2_0_i5);
	update_prof_current_proc(LABEL(mercury__string__char_list_to_lower_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__char_list_to_lower_2_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module62)
	init_entry(mercury__string__hash_2_3_0);
	init_label(mercury__string__hash_2_3_0_i3);
	init_label(mercury__string__hash_2_3_0_i1);
BEGIN_CODE

/* code for predicate 'string__hash_2'/3 in mode 0 */
Define_static(mercury__string__hash_2_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__hash_2_3_0_i1);
Define_label(mercury__string__hash_2_3_0_i3);
	while (1) {
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = ((((Integer) r2 << ((Integer) 5)) ^ (Integer) r2) ^ (Integer) r3);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	break; } /* end while */
Define_label(mercury__string__hash_2_3_0_i1);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module63)
	init_entry(mercury__string__sub_string_search2_5_0);
	init_label(mercury__string__sub_string_search2_5_0_i4);
	init_label(mercury__string__sub_string_search2_5_0_i3);
	init_label(mercury__string__sub_string_search2_5_0_i6);
	init_label(mercury__string__sub_string_search2_5_0_i8);
	init_label(mercury__string__sub_string_search2_5_0_i1005);
	init_label(mercury__string__sub_string_search2_5_0_i1);
	init_label(mercury__string__sub_string_search2_5_0_i1007);
BEGIN_CODE

/* code for predicate 'string__sub_string_search2'/5 in mode 0 */
Define_static(mercury__string__sub_string_search2_5_0);
	if (((Integer) r3 < (Integer) r4))
		GOTO_LABEL(mercury__string__sub_string_search2_5_0_i1005);
	incr_sp_push_msg(5, "string__sub_string_search2");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	{
		call_localret(STATIC(mercury__string__prefix_2_0),
		mercury__string__sub_string_search2_5_0_i4,
		STATIC(mercury__string__sub_string_search2_5_0));
	}
Define_label(mercury__string__sub_string_search2_5_0_i4);
	update_prof_current_proc(LABEL(mercury__string__sub_string_search2_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__sub_string_search2_5_0_i3);
	r2 = ((Integer) 0);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__string__sub_string_search2_5_0_i3);
	r1 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__string__first_char_3_3),
		mercury__string__sub_string_search2_5_0_i6,
		STATIC(mercury__string__sub_string_search2_5_0));
	}
Define_label(mercury__string__sub_string_search2_5_0_i6);
	update_prof_current_proc(LABEL(mercury__string__sub_string_search2_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__sub_string_search2_5_0_i1);
	r1 = (Integer) r3;
	r2 = (Integer) detstackvar(2);
	r3 = ((Integer) detstackvar(3) - ((Integer) 1));
	r4 = (Integer) detstackvar(4);
	localcall(mercury__string__sub_string_search2_5_0,
		LABEL(mercury__string__sub_string_search2_5_0_i8),
		STATIC(mercury__string__sub_string_search2_5_0));
Define_label(mercury__string__sub_string_search2_5_0_i8);
	update_prof_current_proc(LABEL(mercury__string__sub_string_search2_5_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__sub_string_search2_5_0_i1007);
	r2 = ((Integer) r2 + ((Integer) 1));
	r1 = TRUE;
	proceed();
Define_label(mercury__string__sub_string_search2_5_0_i1005);
	r1 = FALSE;
	proceed();
Define_label(mercury__string__sub_string_search2_5_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__string__sub_string_search2_5_0_i1007);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module64)
	init_entry(mercury__string__format_2_3_0);
	init_label(mercury__string__format_2_3_0_i11);
	init_label(mercury__string__format_2_3_0_i1011);
	init_label(mercury__string__format_2_3_0_i15);
	init_label(mercury__string__format_2_3_0_i17);
	init_label(mercury__string__format_2_3_0_i14);
	init_label(mercury__string__format_2_3_0_i1010);
	init_label(mercury__string__format_2_3_0_i22);
	init_label(mercury__string__format_2_3_0_i1006);
BEGIN_CODE

/* code for predicate 'string__format_2'/3 in mode 0 */
Define_static(mercury__string__format_2_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_2_3_0_i1006);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r4 != ((Integer) 37)))
		GOTO_LABEL(mercury__string__format_2_3_0_i1010);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_2_3_0_i1011);
	if (((Integer) field(mktag(1), (Integer) r3, ((Integer) 0)) != ((Integer) 37)))
		GOTO_LABEL(mercury__string__format_2_3_0_i1011);
	r1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	incr_sp_push_msg(2, "string__format_2");
	detstackvar(2) = (Integer) succip;
	localcall(mercury__string__format_2_3_0,
		LABEL(mercury__string__format_2_3_0_i11),
		STATIC(mercury__string__format_2_3_0));
Define_label(mercury__string__format_2_3_0_i11);
	update_prof_current_proc(LABEL(mercury__string__format_2_3_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 37);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__string__first_char_3_4),
		STATIC(mercury__string__format_2_3_0));
	}
Define_label(mercury__string__format_2_3_0_i1011);
	incr_sp_push_msg(2, "string__format_2");
	detstackvar(2) = (Integer) succip;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__string__format_top_convert_variable_5_0),
		mercury__string__format_2_3_0_i15,
		STATIC(mercury__string__format_2_3_0));
Define_label(mercury__string__format_2_3_0_i15);
	update_prof_current_proc(LABEL(mercury__string__format_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_2_3_0_i14);
	detstackvar(1) = (Integer) r4;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	localcall(mercury__string__format_2_3_0,
		LABEL(mercury__string__format_2_3_0_i17),
		STATIC(mercury__string__format_2_3_0));
Define_label(mercury__string__format_2_3_0_i17);
	update_prof_current_proc(LABEL(mercury__string__format_2_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__string__append_3_2),
		STATIC(mercury__string__format_2_3_0));
	}
Define_label(mercury__string__format_2_3_0_i14);
	r1 = string_const("string__format: Too few variables.", 34);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__string__format_2_3_0));
	}
Define_label(mercury__string__format_2_3_0_i1010);
	incr_sp_push_msg(2, "string__format_2");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r4;
	r1 = (Integer) r3;
	localcall(mercury__string__format_2_3_0,
		LABEL(mercury__string__format_2_3_0_i22),
		STATIC(mercury__string__format_2_3_0));
Define_label(mercury__string__format_2_3_0_i22);
	update_prof_current_proc(LABEL(mercury__string__format_2_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__string__first_char_3_4),
		STATIC(mercury__string__format_2_3_0));
	}
Define_label(mercury__string__format_2_3_0_i1006);
	r1 = string_const("", 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module65)
	init_entry(mercury__string__format_top_convert_variable_5_0);
	init_label(mercury__string__format_top_convert_variable_5_0_i1001);
	init_label(mercury__string__format_top_convert_variable_5_0_i6);
	init_label(mercury__string__format_top_convert_variable_5_0_i8);
	init_label(mercury__string__format_top_convert_variable_5_0_i9);
	init_label(mercury__string__format_top_convert_variable_5_0_i13);
	init_label(mercury__string__format_top_convert_variable_5_0_i16);
	init_label(mercury__string__format_top_convert_variable_5_0_i17);
	init_label(mercury__string__format_top_convert_variable_5_0_i21);
	init_label(mercury__string__format_top_convert_variable_5_0_i24);
	init_label(mercury__string__format_top_convert_variable_5_0_i25);
	init_label(mercury__string__format_top_convert_variable_5_0_i28);
	init_label(mercury__string__format_top_convert_variable_5_0_i29);
	init_label(mercury__string__format_top_convert_variable_5_0_i38);
	init_label(mercury__string__format_top_convert_variable_5_0_i39);
	init_label(mercury__string__format_top_convert_variable_5_0_i40);
	init_label(mercury__string__format_top_convert_variable_5_0_i41);
	init_label(mercury__string__format_top_convert_variable_5_0_i35);
	init_label(mercury__string__format_top_convert_variable_5_0_i44);
	init_label(mercury__string__format_top_convert_variable_5_0_i32);
	init_label(mercury__string__format_top_convert_variable_5_0_i52);
	init_label(mercury__string__format_top_convert_variable_5_0_i53);
	init_label(mercury__string__format_top_convert_variable_5_0_i54);
	init_label(mercury__string__format_top_convert_variable_5_0_i55);
	init_label(mercury__string__format_top_convert_variable_5_0_i49);
	init_label(mercury__string__format_top_convert_variable_5_0_i46);
	init_label(mercury__string__format_top_convert_variable_5_0_i62);
	init_label(mercury__string__format_top_convert_variable_5_0_i63);
	init_label(mercury__string__format_top_convert_variable_5_0_i66);
	init_label(mercury__string__format_top_convert_variable_5_0_i69);
	init_label(mercury__string__format_top_convert_variable_5_0_i74);
	init_label(mercury__string__format_top_convert_variable_5_0_i75);
	init_label(mercury__string__format_top_convert_variable_5_0_i76);
	init_label(mercury__string__format_top_convert_variable_5_0_i77);
	init_label(mercury__string__format_top_convert_variable_5_0_i1);
	init_label(mercury__string__format_top_convert_variable_5_0_i1000);
BEGIN_CODE

/* code for predicate 'string__format_top_convert_variable'/5 in mode 0 */
Define_static(mercury__string__format_top_convert_variable_5_0);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i1001);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i1000);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 0)) != ((Integer) 37)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i1000);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = string_const("%", 1);
	r1 = TRUE;
	proceed();
Define_label(mercury__string__format_top_convert_variable_5_0_i1001);
	incr_sp_push_msg(9, "string__format_top_convert_variable");
	detstackvar(9) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	call_localret(STATIC(mercury__string__format_takewhile1_3_0),
		mercury__string__format_top_convert_variable_5_0_i6,
		STATIC(mercury__string__format_top_convert_variable_5_0));
Define_label(mercury__string__format_top_convert_variable_5_0_i6);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i1);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__string__format_get_optional_args_5_0),
		mercury__string__format_top_convert_variable_5_0_i8,
		STATIC(mercury__string__format_top_convert_variable_5_0));
Define_label(mercury__string__format_top_convert_variable_5_0_i8);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	if (((Integer) r2 != ((Integer) -1)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i9);
	{
	Word tempr1, tempr2;
	tempr1 = (Integer) detstackvar(2);
	if (((Integer) tempr1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i1);
	tempr2 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	if ((tag((Integer) tempr2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i1);
	r6 = (Integer) r4;
	r4 = (Integer) r1;
	r5 = (Integer) r3;
	tag_incr_hp(r7, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r7, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) tempr2, ((Integer) 0));
	r8 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i16);
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i9);
	if (((Integer) r2 != ((Integer) -15)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i13);
	r5 = (Integer) r3;
	r6 = (Integer) r4;
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	r7 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r8 = (Integer) detstackvar(2);
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i16);
Define_label(mercury__string__format_top_convert_variable_5_0_i13);
	r5 = (Integer) r3;
	r6 = (Integer) r4;
	r10 = (Integer) r2;
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	tag_incr_hp(r7, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r7, ((Integer) 0)) = (Integer) r10;
	r8 = (Integer) detstackvar(2);
Define_label(mercury__string__format_top_convert_variable_5_0_i16);
	if (((Integer) r5 != ((Integer) -1)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i17);
	if (((Integer) r8 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i1);
	if ((tag((Integer) field(mktag(1), (Integer) r8, ((Integer) 0))) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i1);
	r5 = (Integer) field(mktag(1), (Integer) r8, ((Integer) 1));
	r11 = (Integer) r8;
	r8 = (Integer) r7;
	r7 = (Integer) r6;
	r6 = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) field(mktag(1), (Integer) r11, ((Integer) 0)), ((Integer) 0));
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i24);
Define_label(mercury__string__format_top_convert_variable_5_0_i17);
	if (((Integer) r5 != ((Integer) -15)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i21);
	r5 = (Integer) r8;
	r8 = (Integer) r7;
	r7 = (Integer) r6;
	r6 = (Integer) r3;
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i24);
Define_label(mercury__string__format_top_convert_variable_5_0_i21);
	r9 = (Integer) r5;
	r5 = (Integer) r8;
	r8 = (Integer) r7;
	r7 = (Integer) r6;
	r6 = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r9;
Define_label(mercury__string__format_top_convert_variable_5_0_i24);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i25);
	r9 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i28);
Define_label(mercury__string__format_top_convert_variable_5_0_i25);
	r9 = ((Integer) 0);
Define_label(mercury__string__format_top_convert_variable_5_0_i28);
	if (((Integer) r6 != ((Integer) 105)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i29);
	r6 = (Integer) r5;
	r5 = (Integer) r1;
	r1 = ((Integer) 100);
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i62);
Define_label(mercury__string__format_top_convert_variable_5_0_i29);
	if (((Integer) r6 != ((Integer) 103)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i32);
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i35);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r2;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r7;
	detstackvar(6) = (Integer) r8;
	detstackvar(7) = (Integer) r3;
	detstackvar(8) = (Integer) r9;
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury__float__abs_2_0);
	call_localret(ENTRY(mercury__float__abs_2_0),
		mercury__string__format_top_convert_variable_5_0_i38,
		STATIC(mercury__string__format_top_convert_variable_5_0));
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i38);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	r2 = (Integer) detstackvar(8);
	detstackvar(8) = (Integer) r1;
	r1 = ((Integer) 10);
	{
	Declare_entry(mercury__int__pow_3_0);
	call_localret(ENTRY(mercury__int__pow_3_0),
		mercury__string__format_top_convert_variable_5_0_i39,
		STATIC(mercury__string__format_top_convert_variable_5_0));
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i39);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	{
	Declare_entry(mercury__int__to_float_2_0);
	call_localret(ENTRY(mercury__int__to_float_2_0),
		mercury__string__format_top_convert_variable_5_0_i40,
		STATIC(mercury__string__format_top_convert_variable_5_0));
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i40);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(8);
	{
	static const Float mercury_float_const_0pt0001 = 0.0001;
	if ((word_to_float((Integer) tempr1) <= ((Float) 0.000100000000000000)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i41);
	}
	if ((word_to_float((Integer) r1) <= word_to_float((Integer) tempr1)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i41);
	r1 = ((Integer) 102);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) mkword(mktag(1), (Integer) mercury_data_string__common_0);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r7 = (Integer) detstackvar(5);
	r8 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i62);
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i41);
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	r8 = (Integer) detstackvar(6);
	r1 = ((Integer) 101);
	r3 = (Integer) detstackvar(7);
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i62);
Define_label(mercury__string__format_top_convert_variable_5_0_i35);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r2;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r7;
	detstackvar(6) = (Integer) r8;
	r1 = string_const("string__format:  %g without a f(Float).", 39);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__string__format_top_convert_variable_5_0_i44,
		STATIC(mercury__string__format_top_convert_variable_5_0));
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i44);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	r8 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i62);
Define_label(mercury__string__format_top_convert_variable_5_0_i32);
	if (((Integer) r6 != ((Integer) 71)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i46);
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i49);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r2;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r7;
	detstackvar(6) = (Integer) r8;
	detstackvar(7) = (Integer) r3;
	detstackvar(8) = (Integer) r9;
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury__float__abs_2_0);
	call_localret(ENTRY(mercury__float__abs_2_0),
		mercury__string__format_top_convert_variable_5_0_i52,
		STATIC(mercury__string__format_top_convert_variable_5_0));
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i52);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	r2 = (Integer) detstackvar(8);
	detstackvar(8) = (Integer) r1;
	r1 = ((Integer) 10);
	{
	Declare_entry(mercury__int__pow_3_0);
	call_localret(ENTRY(mercury__int__pow_3_0),
		mercury__string__format_top_convert_variable_5_0_i53,
		STATIC(mercury__string__format_top_convert_variable_5_0));
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i53);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	{
	Declare_entry(mercury__int__to_float_2_0);
	call_localret(ENTRY(mercury__int__to_float_2_0),
		mercury__string__format_top_convert_variable_5_0_i54,
		STATIC(mercury__string__format_top_convert_variable_5_0));
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i54);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(8);
	{
	static const Float mercury_float_const_0pt0001 = 0.0001;
	if ((word_to_float((Integer) tempr1) <= ((Float) 0.000100000000000000)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i55);
	}
	if ((word_to_float((Integer) r1) <= word_to_float((Integer) tempr1)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i55);
	r1 = ((Integer) 102);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) mkword(mktag(1), (Integer) mercury_data_string__common_0);
	r4 = (Integer) detstackvar(4);
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r7 = (Integer) detstackvar(5);
	r8 = (Integer) detstackvar(6);
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i62);
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i55);
	r5 = (Integer) detstackvar(1);
	r6 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(4);
	r7 = (Integer) detstackvar(5);
	r8 = (Integer) detstackvar(6);
	r1 = ((Integer) 69);
	r3 = (Integer) detstackvar(7);
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i62);
Define_label(mercury__string__format_top_convert_variable_5_0_i49);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r2;
	detstackvar(4) = (Integer) r4;
	detstackvar(5) = (Integer) r7;
	detstackvar(6) = (Integer) r8;
	r1 = string_const("string__format:  %G without a f(Float).", 39);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__string__format_top_convert_variable_5_0_i44,
		STATIC(mercury__string__format_top_convert_variable_5_0));
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i46);
	{
	Word tempr1;
	tempr1 = (Integer) r5;
	r5 = (Integer) r1;
	r1 = (Integer) r6;
	r6 = (Integer) tempr1;
	}
Define_label(mercury__string__format_top_convert_variable_5_0_i62);
	if (((Integer) r7 != ((Integer) 104)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i63);
	r7 = (Integer) r8;
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i74);
Define_label(mercury__string__format_top_convert_variable_5_0_i63);
	if (((Integer) r7 != ((Integer) 108)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i66);
	r7 = (Integer) r8;
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i74);
Define_label(mercury__string__format_top_convert_variable_5_0_i66);
	if (((Integer) r7 != ((Integer) 76)))
		GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i69);
	r7 = (Integer) r8;
	GOTO_LABEL(mercury__string__format_top_convert_variable_5_0_i74);
Define_label(mercury__string__format_top_convert_variable_5_0_i69);
	r7 = (Integer) r8;
Define_label(mercury__string__format_top_convert_variable_5_0_i74);
	detstackvar(1) = (Integer) r5;
	detstackvar(2) = (Integer) r6;
	detstackvar(4) = (Integer) r4;
	detstackvar(6) = (Integer) r7;
	call_localret(STATIC(mercury__string__format_do_conversion_6_0),
		mercury__string__format_top_convert_variable_5_0_i75,
		STATIC(mercury__string__format_top_convert_variable_5_0));
Define_label(mercury__string__format_top_convert_variable_5_0_i75);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	r3 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__string__format_add_sign__ua10000_6_0),
		mercury__string__format_top_convert_variable_5_0_i76,
		STATIC(mercury__string__format_top_convert_variable_5_0));
Define_label(mercury__string__format_top_convert_variable_5_0_i76);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	r4 = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(4);
	call_localret(STATIC(mercury__string__format_pad_width_5_0),
		mercury__string__format_top_convert_variable_5_0_i77,
		STATIC(mercury__string__format_top_convert_variable_5_0));
Define_label(mercury__string__format_top_convert_variable_5_0_i77);
	update_prof_current_proc(LABEL(mercury__string__format_top_convert_variable_5_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__string__format_top_convert_variable_5_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury__string__format_top_convert_variable_5_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module66)
	init_entry(mercury__string__format_do_conversion_6_0);
	init_label(mercury__string__format_do_conversion_6_0_i4);
	init_label(mercury__string__format_do_conversion_6_0_i3);
BEGIN_CODE

/* code for predicate 'string__format_do_conversion'/6 in mode 0 */
Define_static(mercury__string__format_do_conversion_6_0);
	incr_sp_push_msg(2, "string__format_do_conversion");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	call_localret(STATIC(mercury__string__do_conversion_0_6_0),
		mercury__string__format_do_conversion_6_0_i4,
		STATIC(mercury__string__format_do_conversion_6_0));
Define_label(mercury__string__format_do_conversion_6_0_i4);
	update_prof_current_proc(LABEL(mercury__string__format_do_conversion_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_do_conversion_6_0_i3);
	r1 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__format_do_conversion_6_0_i3);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__string__do_conversion_fail_1_0),
		STATIC(mercury__string__format_do_conversion_6_0));
END_MODULE

BEGIN_MODULE(mercury__string_module67)
	init_entry(mercury__string__do_conversion_0_6_0);
	init_label(mercury__string__do_conversion_0_6_0_i5);
	init_label(mercury__string__do_conversion_0_6_0_i6);
	init_label(mercury__string__do_conversion_0_6_0_i7);
	init_label(mercury__string__do_conversion_0_6_0_i1006);
	init_label(mercury__string__do_conversion_0_6_0_i15);
	init_label(mercury__string__do_conversion_0_6_0_i12);
	init_label(mercury__string__do_conversion_0_6_0_i17);
	init_label(mercury__string__do_conversion_0_6_0_i18);
	init_label(mercury__string__do_conversion_0_6_0_i19);
	init_label(mercury__string__do_conversion_0_6_0_i23);
	init_label(mercury__string__do_conversion_0_6_0_i25);
	init_label(mercury__string__do_conversion_0_6_0_i22);
	init_label(mercury__string__do_conversion_0_6_0_i27);
	init_label(mercury__string__do_conversion_0_6_0_i10);
	init_label(mercury__string__do_conversion_0_6_0_i31);
	init_label(mercury__string__do_conversion_0_6_0_i36);
	init_label(mercury__string__do_conversion_0_6_0_i37);
	init_label(mercury__string__do_conversion_0_6_0_i39);
	init_label(mercury__string__do_conversion_0_6_0_i34);
	init_label(mercury__string__do_conversion_0_6_0_i42);
	init_label(mercury__string__do_conversion_0_6_0_i50);
	init_label(mercury__string__do_conversion_0_6_0_i51);
	init_label(mercury__string__do_conversion_0_6_0_i48);
	init_label(mercury__string__do_conversion_0_6_0_i60);
	init_label(mercury__string__do_conversion_0_6_0_i57);
	init_label(mercury__string__do_conversion_0_6_0_i62);
	init_label(mercury__string__do_conversion_0_6_0_i63);
	init_label(mercury__string__do_conversion_0_6_0_i67);
	init_label(mercury__string__do_conversion_0_6_0_i69);
	init_label(mercury__string__do_conversion_0_6_0_i66);
	init_label(mercury__string__do_conversion_0_6_0_i71);
	init_label(mercury__string__do_conversion_0_6_0_i55);
	init_label(mercury__string__do_conversion_0_6_0_i77);
	init_label(mercury__string__do_conversion_0_6_0_i75);
	init_label(mercury__string__do_conversion_0_6_0_i86);
	init_label(mercury__string__do_conversion_0_6_0_i81);
	init_label(mercury__string__do_conversion_0_6_0_i90);
	init_label(mercury__string__do_conversion_0_6_0_i91);
	init_label(mercury__string__do_conversion_0_6_0_i92);
	init_label(mercury__string__do_conversion_0_6_0_i88);
	init_label(mercury__string__do_conversion_0_6_0_i99);
	init_label(mercury__string__do_conversion_0_6_0_i96);
	init_label(mercury__string__do_conversion_0_6_0_i101);
	init_label(mercury__string__do_conversion_0_6_0_i102);
	init_label(mercury__string__do_conversion_0_6_0_i103);
	init_label(mercury__string__do_conversion_0_6_0_i107);
	init_label(mercury__string__do_conversion_0_6_0_i109);
	init_label(mercury__string__do_conversion_0_6_0_i106);
	init_label(mercury__string__do_conversion_0_6_0_i111);
	init_label(mercury__string__do_conversion_0_6_0_i1);
	init_label(mercury__string__do_conversion_0_6_0_i1002);
	init_label(mercury__string__do_conversion_0_6_0_i1004);
	init_label(mercury__string__do_conversion_0_6_0_i1005);
BEGIN_CODE

/* code for predicate 'string__do_conversion_0'/6 in mode 0 */
Define_static(mercury__string__do_conversion_0_6_0);
	if (((Integer) r1 != ((Integer) 69)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1006);
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1005);
	incr_sp_push_msg(4, "string__do_conversion_0");
	detstackvar(4) = (Integer) succip;
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(1) = (Integer) r1;
	r2 = (Integer) r3;
	r3 = ((Integer) 0);
	call_localret(STATIC(mercury__string__format_calc_exp_4_0),
		mercury__string__do_conversion_0_6_0_i5,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i5);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	{
		call_localret(STATIC(mercury__string__to_upper_2_0),
		mercury__string__do_conversion_0_6_0_i6,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i6);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	{
	static const Float mercury_float_const_0 = 0;
	if ((word_to_float((Integer) detstackvar(1)) >= ((Float) 0.00000000000000)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i7);
	}
	r2 = (Integer) r1;
	r3 = ((Integer) 1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i7);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	r2 = (Integer) r1;
	r3 = ((Integer) 0);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i1006);
	incr_sp_push_msg(4, "string__do_conversion_0");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r1 != ((Integer) 88)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i10);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i12);
	detstackvar(3) = (Integer) r1;
	r1 = string_const("0", 1);
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__string__format_int_precision_4_0),
		mercury__string__do_conversion_0_6_0_i15,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i15);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = (Integer) detstackvar(3);
	r3 = ((Integer) 0);
	GOTO_LABEL(mercury__string__do_conversion_0_6_0_i27);
Define_label(mercury__string__do_conversion_0_6_0_i12);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r1;
	r2 = ((Integer) 16);
	{
		call_localret(STATIC(mercury__string__int_to_base_string_3_0),
		mercury__string__do_conversion_0_6_0_i17,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i17);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	{
		call_localret(STATIC(mercury__string__to_upper_2_0),
		mercury__string__do_conversion_0_6_0_i18,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i18);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__string__format_int_precision_4_0),
		mercury__string__do_conversion_0_6_0_i19,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i19);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r2 = ((Integer) 35);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__string__do_conversion_0_6_0_i23,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i23);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i22);
	r1 = string_const("0X", 2);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__do_conversion_0_6_0_i25,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i25);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = ((Integer) 2);
	GOTO_LABEL(mercury__string__do_conversion_0_6_0_i27);
Define_label(mercury__string__do_conversion_0_6_0_i22);
	r2 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(3);
	r3 = ((Integer) 0);
Define_label(mercury__string__do_conversion_0_6_0_i27);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (((Integer) r1 >= ((Integer) 0)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1004);
	r3 = ((Integer) r3 + ((Integer) 1));
	r1 = TRUE;
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i10);
	if (((Integer) r1 != ((Integer) 99)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i31);
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 0));
	{
		call_localret(STATIC(mercury__string__char_to_string_2_0),
		mercury__string__do_conversion_0_6_0_i7,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i31);
	if (((Integer) r1 != ((Integer) 100)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i34);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(1) = (Integer) r3;
	{
		call_localret(STATIC(mercury__string__int_to_string_2_0),
		mercury__string__do_conversion_0_6_0_i36,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i36);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__string__format_int_precision_4_0),
		mercury__string__do_conversion_0_6_0_i37,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i37);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	if (((Integer) detstackvar(2) >= ((Integer) 0)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i39);
	r1 = TRUE;
	r3 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i39);
	r3 = ((Integer) 0);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i34);
	if (((Integer) r1 != ((Integer) 101)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i42);
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(1) = (Integer) r1;
	r2 = (Integer) r3;
	r3 = ((Integer) 0);
	call_localret(STATIC(mercury__string__format_calc_exp_4_0),
		mercury__string__do_conversion_0_6_0_i6,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i42);
	if (((Integer) r1 != ((Integer) 102)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i48);
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(1) = (Integer) r3;
	call_localret(STATIC(mercury__string__float_to_f_string_2_0),
		mercury__string__do_conversion_0_6_0_i50,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i50);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__string__format_calc_prec_3_0),
		mercury__string__do_conversion_0_6_0_i51,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i51);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	{
	static const Float mercury_float_const_0 = 0;
	if ((word_to_float((Integer) detstackvar(2)) >= ((Float) 0.00000000000000)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i7);
	}
	r2 = (Integer) r1;
	r3 = ((Integer) 1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i48);
	if (((Integer) r1 != ((Integer) 111)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i55);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i57);
	detstackvar(3) = (Integer) r1;
	r1 = string_const("0", 1);
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__string__format_int_precision_4_0),
		mercury__string__do_conversion_0_6_0_i60,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i60);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = ((Integer) 0);
	r3 = (Integer) detstackvar(3);
	GOTO_LABEL(mercury__string__do_conversion_0_6_0_i71);
Define_label(mercury__string__do_conversion_0_6_0_i57);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r1;
	r2 = ((Integer) 8);
	{
		call_localret(STATIC(mercury__string__int_to_base_string_3_0),
		mercury__string__do_conversion_0_6_0_i62,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i62);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__string__format_int_precision_4_0),
		mercury__string__do_conversion_0_6_0_i63,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i63);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r2 = ((Integer) 35);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__string__do_conversion_0_6_0_i67,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i67);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i66);
	r1 = ((Integer) 48);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__string__first_char_3_4),
		mercury__string__do_conversion_0_6_0_i69,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i69);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 1);
	r3 = (Integer) detstackvar(3);
	GOTO_LABEL(mercury__string__do_conversion_0_6_0_i71);
Define_label(mercury__string__do_conversion_0_6_0_i66);
	r2 = (Integer) detstackvar(1);
	r1 = ((Integer) 0);
	r3 = (Integer) detstackvar(3);
Define_label(mercury__string__do_conversion_0_6_0_i71);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (((Integer) r3 >= ((Integer) 0)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1002);
	r3 = ((Integer) r1 + ((Integer) 1));
	r1 = TRUE;
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i55);
	if (((Integer) r1 != ((Integer) 112)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i75);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(1) = (Integer) r1;
	{
		call_localret(STATIC(mercury__string__int_to_string_2_0),
		mercury__string__do_conversion_0_6_0_i77,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i77);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	if (((Integer) detstackvar(1) >= ((Integer) 0)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i7);
	r2 = (Integer) r1;
	r3 = ((Integer) 1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i75);
	if (((Integer) r1 != ((Integer) 115)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i81);
	if ((tag((Integer) r2) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = (Integer) field(mktag(2), (Integer) r2, ((Integer) 0));
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i7);
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
		call_localret(STATIC(mercury__string__split_4_0),
		mercury__string__do_conversion_0_6_0_i86,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i86);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	r2 = (Integer) r1;
	r1 = TRUE;
	r3 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i81);
	if (((Integer) r1 != ((Integer) 117)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i88);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	{
	Declare_entry(mercury__int__abs_2_0);
	call_localret(ENTRY(mercury__int__abs_2_0),
		mercury__string__do_conversion_0_6_0_i90,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i90);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	{
		call_localret(STATIC(mercury__string__int_to_string_2_0),
		mercury__string__do_conversion_0_6_0_i91,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i91);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__string__format_int_precision_4_0),
		mercury__string__do_conversion_0_6_0_i92,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i92);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if ((Integer) r1)
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1004);
	r1 = FALSE;
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i88);
	if (((Integer) r1 != ((Integer) 120)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i96);
	detstackvar(3) = (Integer) r1;
	r1 = string_const("0", 1);
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__string__format_int_precision_4_0),
		mercury__string__do_conversion_0_6_0_i99,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i99);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	r1 = (Integer) detstackvar(3);
	r3 = ((Integer) 0);
	GOTO_LABEL(mercury__string__do_conversion_0_6_0_i111);
Define_label(mercury__string__do_conversion_0_6_0_i96);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(3) = (Integer) r1;
	r2 = ((Integer) 16);
	{
		call_localret(STATIC(mercury__string__int_to_base_string_3_0),
		mercury__string__do_conversion_0_6_0_i101,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i101);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	{
		call_localret(STATIC(mercury__string__to_lower_2_0),
		mercury__string__do_conversion_0_6_0_i102,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i102);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__string__format_int_precision_4_0),
		mercury__string__do_conversion_0_6_0_i103,
		STATIC(mercury__string__do_conversion_0_6_0));
Define_label(mercury__string__do_conversion_0_6_0_i103);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r2 = ((Integer) 35);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__string__do_conversion_0_6_0_i107,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i107);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i106);
	r1 = string_const("0x", 2);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__do_conversion_0_6_0_i109,
		STATIC(mercury__string__do_conversion_0_6_0));
	}
Define_label(mercury__string__do_conversion_0_6_0_i109);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_0_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	r3 = ((Integer) 2);
	GOTO_LABEL(mercury__string__do_conversion_0_6_0_i111);
Define_label(mercury__string__do_conversion_0_6_0_i106);
	r2 = (Integer) detstackvar(1);
	r1 = (Integer) detstackvar(3);
	r3 = ((Integer) 0);
Define_label(mercury__string__do_conversion_0_6_0_i111);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (((Integer) r1 >= ((Integer) 0)))
		GOTO_LABEL(mercury__string__do_conversion_0_6_0_i1004);
	r3 = ((Integer) r3 + ((Integer) 1));
	r1 = TRUE;
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i1002);
	r3 = (Integer) r1;
	r1 = TRUE;
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i1004);
	r1 = TRUE;
	proceed();
Define_label(mercury__string__do_conversion_0_6_0_i1005);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module68)
	init_entry(mercury__string__do_conversion_fail_1_0);
	init_label(mercury__string__do_conversion_fail_1_0_i2);
	init_label(mercury__string__do_conversion_fail_1_0_i3);
BEGIN_CODE

/* code for predicate 'string__do_conversion_fail'/1 in mode 0 */
Define_static(mercury__string__do_conversion_fail_1_0);
	r3 = (Integer) r1;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	r1 = string_const("%s `%%%c', without a correct poly-variable.", 43);
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) mkword(mktag(2), (Integer) mercury_data_string__common_1);
	tag_incr_hp(r4, mktag(1), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(3), ((Integer) 1));
	field(mktag(1), (Integer) r4, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(3), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r4;
	field(mktag(1), (Integer) r4, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	incr_sp_push_msg(1, "string__do_conversion_fail");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__string__format_3_0),
		mercury__string__do_conversion_fail_1_0_i2,
		STATIC(mercury__string__do_conversion_fail_1_0));
	}
	}
Define_label(mercury__string__do_conversion_fail_1_0_i2);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_fail_1_0));
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__string__do_conversion_fail_1_0_i3,
		STATIC(mercury__string__do_conversion_fail_1_0));
	}
Define_label(mercury__string__do_conversion_fail_1_0_i3);
	update_prof_current_proc(LABEL(mercury__string__do_conversion_fail_1_0));
END_MODULE

BEGIN_MODULE(mercury__string_module69)
	init_entry(mercury__string__format_int_precision_4_0);
	init_label(mercury__string__format_int_precision_4_0_i1002);
	init_label(mercury__string__format_int_precision_4_0_i5);
	init_label(mercury__string__format_int_precision_4_0_i6);
	init_label(mercury__string__format_int_precision_4_0_i9);
	init_label(mercury__string__format_int_precision_4_0_i8);
	init_label(mercury__string__format_int_precision_4_0_i11);
	init_label(mercury__string__format_int_precision_4_0_i14);
	init_label(mercury__string__format_int_precision_4_0_i15);
	init_label(mercury__string__format_int_precision_4_0_i25);
	init_label(mercury__string__format_int_precision_4_0_i17);
	init_label(mercury__string__format_int_precision_4_0_i26);
	init_label(mercury__string__format_int_precision_4_0_i12);
	init_label(mercury__string__format_int_precision_4_0_i1);
BEGIN_CODE

/* code for predicate 'string__format_int_precision'/4 in mode 0 */
Define_static(mercury__string__format_int_precision_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_int_precision_4_0_i1002);
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	incr_sp_push_msg(8, "string__format_int_precision");
	detstackvar(8) = (Integer) succip;
	GOTO_LABEL(mercury__string__format_int_precision_4_0_i5);
Define_label(mercury__string__format_int_precision_4_0_i1002);
	incr_sp_push_msg(8, "string__format_int_precision");
	detstackvar(8) = (Integer) succip;
	r2 = ((Integer) 0);
Define_label(mercury__string__format_int_precision_4_0_i5);
	detstackvar(1) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__length_2_0),
		mercury__string__format_int_precision_4_0_i6,
		STATIC(mercury__string__format_int_precision_4_0));
	}
Define_label(mercury__string__format_int_precision_4_0_i6);
	update_prof_current_proc(LABEL(mercury__string__format_int_precision_4_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = ((Integer) 45);
	{
		call_localret(STATIC(mercury__string__first_char_3_2),
		mercury__string__format_int_precision_4_0_i9,
		STATIC(mercury__string__format_int_precision_4_0));
	}
Define_label(mercury__string__format_int_precision_4_0_i9);
	update_prof_current_proc(LABEL(mercury__string__format_int_precision_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_int_precision_4_0_i8);
	r1 = (Integer) detstackvar(1);
	r2 = (((Integer) detstackvar(3) - (Integer) detstackvar(4)) + ((Integer) 1));
	GOTO_LABEL(mercury__string__format_int_precision_4_0_i11);
Define_label(mercury__string__format_int_precision_4_0_i8);
	r1 = (Integer) detstackvar(1);
	r2 = ((Integer) detstackvar(3) - (Integer) detstackvar(4));
Define_label(mercury__string__format_int_precision_4_0_i11);
	if (((Integer) r2 <= ((Integer) 0)))
		GOTO_LABEL(mercury__string__format_int_precision_4_0_i12);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = ((Integer) 48);
	{
		call_localret(STATIC(mercury__string__duplicate_char_3_0),
		mercury__string__format_int_precision_4_0_i14,
		STATIC(mercury__string__format_int_precision_4_0));
	}
Define_label(mercury__string__format_int_precision_4_0_i14);
	update_prof_current_proc(LABEL(mercury__string__format_int_precision_4_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__string__first_char_3_3),
		mercury__string__format_int_precision_4_0_i15,
		STATIC(mercury__string__format_int_precision_4_0));
	}
Define_label(mercury__string__format_int_precision_4_0_i15);
	update_prof_current_proc(LABEL(mercury__string__format_int_precision_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_int_precision_4_0_i1);
	if (((Integer) r2 == ((Integer) 45)))
		GOTO_LABEL(mercury__string__format_int_precision_4_0_i17);
	if (((Integer) r2 == ((Integer) 43)))
		GOTO_LABEL(mercury__string__format_int_precision_4_0_i17);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__format_int_precision_4_0_i25,
		STATIC(mercury__string__format_int_precision_4_0));
	}
Define_label(mercury__string__format_int_precision_4_0_i25);
	update_prof_current_proc(LABEL(mercury__string__format_int_precision_4_0));
	r2 = (Integer) r1;
	r3 = (Integer) detstackvar(2);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__string__format_int_precision_4_0_i17);
	detstackvar(6) = (Integer) r2;
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) r3;
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__format_int_precision_4_0_i26,
		STATIC(mercury__string__format_int_precision_4_0));
	}
Define_label(mercury__string__format_int_precision_4_0_i26);
	update_prof_current_proc(LABEL(mercury__string__format_int_precision_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	{
		call_localret(STATIC(mercury__string__first_char_3_4),
		mercury__string__format_int_precision_4_0_i25,
		STATIC(mercury__string__format_int_precision_4_0));
	}
Define_label(mercury__string__format_int_precision_4_0_i12);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__string__format_int_precision_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module70)
	init_entry(mercury__string__format_calc_exp_4_0);
	init_label(mercury__string__format_calc_exp_4_0_i4);
	init_label(mercury__string__format_calc_exp_4_0_i1001);
	init_label(mercury__string__format_calc_exp_4_0_i6);
	init_label(mercury__string__format_calc_exp_4_0_i9);
	init_label(mercury__string__format_calc_exp_4_0_i12);
	init_label(mercury__string__format_calc_exp_4_0_i13);
	init_label(mercury__string__format_calc_exp_4_0_i14);
	init_label(mercury__string__format_calc_exp_4_0_i17);
	init_label(mercury__string__format_calc_exp_4_0_i15);
	init_label(mercury__string__format_calc_exp_4_0_i19);
BEGIN_CODE

/* code for predicate 'string__format_calc_exp'/4 in mode 0 */
Define_static(mercury__string__format_calc_exp_4_0);
	{
	static const Float mercury_float_const_0 = 0;
	if ((word_to_float((Integer) r1) >= ((Float) 0.00000000000000)))
		GOTO_LABEL(mercury__string__format_calc_exp_4_0_i1001);
	}
	{
	static const Float mercury_float_const_0 = 0;
	r1 = float_to_word(((Float) 0.00000000000000) - word_to_float((Integer) r1));
	}
	incr_sp_push_msg(3, "string__format_calc_exp");
	detstackvar(3) = (Integer) succip;
	localcall(mercury__string__format_calc_exp_4_0,
		LABEL(mercury__string__format_calc_exp_4_0_i4),
		STATIC(mercury__string__format_calc_exp_4_0));
Define_label(mercury__string__format_calc_exp_4_0_i4);
	update_prof_current_proc(LABEL(mercury__string__format_calc_exp_4_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 45);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__string__first_char_3_4),
		STATIC(mercury__string__format_calc_exp_4_0));
	}
Define_label(mercury__string__format_calc_exp_4_0_i1001);
	incr_sp_push_msg(3, "string__format_calc_exp");
	detstackvar(3) = (Integer) succip;
	{
	static const Float mercury_float_const_0 = 0;
	if ((word_to_float((Integer) r1) <= ((Float) 0.00000000000000)))
		GOTO_LABEL(mercury__string__format_calc_exp_4_0_i6);
	}
	{
	static const Float mercury_float_const_1 = 1;
	if ((word_to_float((Integer) r1) >= ((Float) 1.00000000000000)))
		GOTO_LABEL(mercury__string__format_calc_exp_4_0_i6);
	}
	{
	static const Float mercury_float_const_10 = 10;
	r1 = float_to_word(((Float) 10.0000000000000) * word_to_float((Integer) r1));
	}
	r3 = ((Integer) r3 - ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__string__format_calc_exp_4_0,
		STATIC(mercury__string__format_calc_exp_4_0));
Define_label(mercury__string__format_calc_exp_4_0_i6);
	{
	static const Float mercury_float_const_10 = 10;
	if ((word_to_float((Integer) r1) < ((Float) 10.0000000000000)))
		GOTO_LABEL(mercury__string__format_calc_exp_4_0_i9);
	}
	{
	static const Float mercury_float_const_10 = 10;
	r1 = float_to_word(word_to_float((Integer) r1) / ((Float) 10.0000000000000));
	}
	r3 = ((Integer) r3 + ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__string__format_calc_exp_4_0,
		STATIC(mercury__string__format_calc_exp_4_0));
Define_label(mercury__string__format_calc_exp_4_0_i9);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	call_localret(STATIC(mercury__string__float_to_f_string_2_0),
		mercury__string__format_calc_exp_4_0_i12,
		STATIC(mercury__string__format_calc_exp_4_0));
Define_label(mercury__string__format_calc_exp_4_0_i12);
	update_prof_current_proc(LABEL(mercury__string__format_calc_exp_4_0));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__string__format_calc_prec_3_0),
		mercury__string__format_calc_exp_4_0_i13,
		STATIC(mercury__string__format_calc_exp_4_0));
Define_label(mercury__string__format_calc_exp_4_0_i13);
	update_prof_current_proc(LABEL(mercury__string__format_calc_exp_4_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__string__int_to_string_2_0),
		mercury__string__format_calc_exp_4_0_i14,
		STATIC(mercury__string__format_calc_exp_4_0));
	}
Define_label(mercury__string__format_calc_exp_4_0_i14);
	update_prof_current_proc(LABEL(mercury__string__format_calc_exp_4_0));
	if (((Integer) detstackvar(2) >= ((Integer) 0)))
		GOTO_LABEL(mercury__string__format_calc_exp_4_0_i15);
	r2 = (Integer) r1;
	r1 = string_const("e", 1);
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__format_calc_exp_4_0_i17,
		STATIC(mercury__string__format_calc_exp_4_0));
	}
Define_label(mercury__string__format_calc_exp_4_0_i17);
	update_prof_current_proc(LABEL(mercury__string__format_calc_exp_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__string__append_3_2),
		STATIC(mercury__string__format_calc_exp_4_0));
	}
Define_label(mercury__string__format_calc_exp_4_0_i15);
	r2 = (Integer) r1;
	r1 = string_const("e+", 2);
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__format_calc_exp_4_0_i19,
		STATIC(mercury__string__format_calc_exp_4_0));
	}
Define_label(mercury__string__format_calc_exp_4_0_i19);
	update_prof_current_proc(LABEL(mercury__string__format_calc_exp_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__string__append_3_2),
		STATIC(mercury__string__format_calc_exp_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module71)
	init_entry(mercury__string__format_calc_prec_3_0);
	init_label(mercury__string__format_calc_prec_3_0_i1000);
	init_label(mercury__string__format_calc_prec_3_0_i5);
	init_label(mercury__string__format_calc_prec_3_0_i8);
	init_label(mercury__string__format_calc_prec_3_0_i9);
	init_label(mercury__string__format_calc_prec_3_0_i7);
	init_label(mercury__string__format_calc_prec_3_0_i11);
	init_label(mercury__string__format_calc_prec_3_0_i12);
	init_label(mercury__string__format_calc_prec_3_0_i13);
	init_label(mercury__string__format_calc_prec_3_0_i14);
	init_label(mercury__string__format_calc_prec_3_0_i20);
	init_label(mercury__string__format_calc_prec_3_0_i21);
	init_label(mercury__string__format_calc_prec_3_0_i22);
	init_label(mercury__string__format_calc_prec_3_0_i19);
BEGIN_CODE

/* code for predicate 'string__format_calc_prec'/3 in mode 0 */
Define_static(mercury__string__format_calc_prec_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_calc_prec_3_0_i1000);
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	incr_sp_push_msg(5, "string__format_calc_prec");
	detstackvar(5) = (Integer) succip;
	GOTO_LABEL(mercury__string__format_calc_prec_3_0_i5);
Define_label(mercury__string__format_calc_prec_3_0_i1000);
	incr_sp_push_msg(5, "string__format_calc_prec");
	detstackvar(5) = (Integer) succip;
	r2 = ((Integer) 15);
Define_label(mercury__string__format_calc_prec_3_0_i5);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
		call_localret(STATIC(mercury__string__to_char_list_2_0),
		mercury__string__format_calc_prec_3_0_i8,
		STATIC(mercury__string__format_calc_prec_3_0));
	}
Define_label(mercury__string__format_calc_prec_3_0_i8);
	update_prof_current_proc(LABEL(mercury__string__format_calc_prec_3_0));
	r2 = ((Integer) 46);
	call_localret(STATIC(mercury__string__find_index_2_3_0),
		mercury__string__format_calc_prec_3_0_i9,
		STATIC(mercury__string__format_calc_prec_3_0));
Define_label(mercury__string__format_calc_prec_3_0_i9);
	update_prof_current_proc(LABEL(mercury__string__format_calc_prec_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_calc_prec_3_0_i7);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	r3 = ((Integer) r2 + (Integer) r1);
	r1 = (Integer) detstackvar(1);
	GOTO_LABEL(mercury__string__format_calc_prec_3_0_i13);
Define_label(mercury__string__format_calc_prec_3_0_i7);
	r1 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__string__length_2_0),
		mercury__string__format_calc_prec_3_0_i11,
		STATIC(mercury__string__format_calc_prec_3_0));
	}
Define_label(mercury__string__format_calc_prec_3_0_i11);
	update_prof_current_proc(LABEL(mercury__string__format_calc_prec_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (((Integer) r1 + ((Integer) 1)) + (Integer) detstackvar(2));
	r1 = (Integer) r2;
	r2 = string_const(".", 1);
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__format_calc_prec_3_0_i12,
		STATIC(mercury__string__format_calc_prec_3_0));
	}
Define_label(mercury__string__format_calc_prec_3_0_i12);
	update_prof_current_proc(LABEL(mercury__string__format_calc_prec_3_0));
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
Define_label(mercury__string__format_calc_prec_3_0_i13);
	if (((Integer) r2 != ((Integer) 0)))
		GOTO_LABEL(mercury__string__format_calc_prec_3_0_i14);
	r3 = ((Integer) r3 - ((Integer) 1));
Define_label(mercury__string__format_calc_prec_3_0_i14);
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r1;
	detstackvar(4) = (Integer) r3;
	{
		call_localret(STATIC(mercury__string__length_2_0),
		mercury__string__format_calc_prec_3_0_i20,
		STATIC(mercury__string__format_calc_prec_3_0));
	}
Define_label(mercury__string__format_calc_prec_3_0_i20);
	update_prof_current_proc(LABEL(mercury__string__format_calc_prec_3_0));
	if (((Integer) r1 >= (Integer) detstackvar(4)))
		GOTO_LABEL(mercury__string__format_calc_prec_3_0_i19);
	r1 = ((Integer) 48);
	r2 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__string__duplicate_char_3_0),
		mercury__string__format_calc_prec_3_0_i21,
		STATIC(mercury__string__format_calc_prec_3_0));
	}
Define_label(mercury__string__format_calc_prec_3_0_i21);
	update_prof_current_proc(LABEL(mercury__string__format_calc_prec_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__format_calc_prec_3_0_i22,
		STATIC(mercury__string__format_calc_prec_3_0));
	}
Define_label(mercury__string__format_calc_prec_3_0_i22);
	update_prof_current_proc(LABEL(mercury__string__format_calc_prec_3_0));
	r2 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
		tailcall(STATIC(mercury__string__split_4_0),
		STATIC(mercury__string__format_calc_prec_3_0));
	}
Define_label(mercury__string__format_calc_prec_3_0_i19);
	r2 = (Integer) detstackvar(4);
	r1 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
		tailcall(STATIC(mercury__string__split_4_0),
		STATIC(mercury__string__format_calc_prec_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__string_module72)
	init_entry(mercury__string__find_index_2_3_0);
	init_label(mercury__string__find_index_2_3_0_i1001);
	init_label(mercury__string__find_index_2_3_0_i4);
	init_label(mercury__string__find_index_2_3_0_i6);
	init_label(mercury__string__find_index_2_3_0_i1000);
BEGIN_CODE

/* code for predicate 'string__find_index_2'/3 in mode 0 */
Define_static(mercury__string__find_index_2_3_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__find_index_2_3_0_i1001);
	r1 = FALSE;
	proceed();
Define_label(mercury__string__find_index_2_3_0_i1001);
	incr_sp_push_msg(1, "string__find_index_2");
	detstackvar(1) = (Integer) succip;
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 0)) != (Integer) r2))
		GOTO_LABEL(mercury__string__find_index_2_3_0_i4);
	r2 = ((Integer) 1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__string__find_index_2_3_0_i4);
	r1 = (Integer) r3;
	localcall(mercury__string__find_index_2_3_0,
		LABEL(mercury__string__find_index_2_3_0_i6),
		STATIC(mercury__string__find_index_2_3_0));
Define_label(mercury__string__find_index_2_3_0_i6);
	update_prof_current_proc(LABEL(mercury__string__find_index_2_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__find_index_2_3_0_i1000);
	r2 = ((Integer) r2 + ((Integer) 1));
	r1 = TRUE;
	proceed();
Define_label(mercury__string__find_index_2_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module73)
	init_entry(mercury__string__format_pad_width_5_0);
	init_label(mercury__string__format_pad_width_5_0_i2);
	init_label(mercury__string__format_pad_width_5_0_i8);
	init_label(mercury__string__format_pad_width_5_0_i7);
	init_label(mercury__string__format_pad_width_5_0_i10);
	init_label(mercury__string__format_pad_width_5_0_i11);
	init_label(mercury__string__format_pad_width_5_0_i14);
	init_label(mercury__string__format_pad_width_5_0_i13);
	init_label(mercury__string__format_pad_width_5_0_i19);
	init_label(mercury__string__format_pad_width_5_0_i21);
	init_label(mercury__string__format_pad_width_5_0_i22);
	init_label(mercury__string__format_pad_width_5_0_i18);
	init_label(mercury__string__format_pad_width_5_0_i3);
BEGIN_CODE

/* code for predicate 'string__format_pad_width'/5 in mode 0 */
Define_static(mercury__string__format_pad_width_5_0);
	incr_sp_push_msg(7, "string__format_pad_width");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) r4;
	{
		call_localret(STATIC(mercury__string__length_2_0),
		mercury__string__format_pad_width_5_0_i2,
		STATIC(mercury__string__format_pad_width_5_0));
	}
Define_label(mercury__string__format_pad_width_5_0_i2);
	update_prof_current_proc(LABEL(mercury__string__format_pad_width_5_0));
	{
	Word tempr1, tempr2, tempr3;
	tempr1 = (Integer) detstackvar(2);
	if (((Integer) tempr1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_pad_width_5_0_i3);
	tempr2 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	if (((Integer) r1 >= (Integer) tempr2))
		GOTO_LABEL(mercury__string__format_pad_width_5_0_i3);
	tempr3 = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r2 = ((Integer) 48);
	r3 = (Integer) detstackvar(3);
	detstackvar(2) = ((Integer) tempr2 - (Integer) tempr3);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__string__format_pad_width_5_0_i8,
		STATIC(mercury__string__format_pad_width_5_0));
	}
	}
Define_label(mercury__string__format_pad_width_5_0_i8);
	update_prof_current_proc(LABEL(mercury__string__format_pad_width_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_pad_width_5_0_i7);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	r1 = ((Integer) 48);
	GOTO_LABEL(mercury__string__format_pad_width_5_0_i10);
Define_label(mercury__string__format_pad_width_5_0_i7);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	r5 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	r1 = ((Integer) 32);
Define_label(mercury__string__format_pad_width_5_0_i10);
	detstackvar(1) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	detstackvar(4) = (Integer) r5;
	{
		call_localret(STATIC(mercury__string__duplicate_char_3_0),
		mercury__string__format_pad_width_5_0_i11,
		STATIC(mercury__string__format_pad_width_5_0));
	}
Define_label(mercury__string__format_pad_width_5_0_i11);
	update_prof_current_proc(LABEL(mercury__string__format_pad_width_5_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r2 = ((Integer) 45);
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__string__format_pad_width_5_0_i14,
		STATIC(mercury__string__format_pad_width_5_0));
	}
Define_label(mercury__string__format_pad_width_5_0_i14);
	update_prof_current_proc(LABEL(mercury__string__format_pad_width_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_pad_width_5_0_i13);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	{
		tailcall(STATIC(mercury__string__append_3_2),
		STATIC(mercury__string__format_pad_width_5_0));
	}
Define_label(mercury__string__format_pad_width_5_0_i13);
	r1 = (Integer) mercury_data___base_type_info_character_0;
	r2 = ((Integer) 48);
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__list__member_2_0);
	call_localret(ENTRY(mercury__list__member_2_0),
		mercury__string__format_pad_width_5_0_i19,
		STATIC(mercury__string__format_pad_width_5_0));
	}
Define_label(mercury__string__format_pad_width_5_0_i19);
	update_prof_current_proc(LABEL(mercury__string__format_pad_width_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_pad_width_5_0_i18);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(4);
	{
		call_localret(STATIC(mercury__string__split_4_0),
		mercury__string__format_pad_width_5_0_i21,
		STATIC(mercury__string__format_pad_width_5_0));
	}
Define_label(mercury__string__format_pad_width_5_0_i21);
	update_prof_current_proc(LABEL(mercury__string__format_pad_width_5_0));
	detstackvar(6) = (Integer) r1;
	r1 = (Integer) detstackvar(5);
	{
		call_localret(STATIC(mercury__string__append_3_2),
		mercury__string__format_pad_width_5_0_i22,
		STATIC(mercury__string__format_pad_width_5_0));
	}
Define_label(mercury__string__format_pad_width_5_0_i22);
	update_prof_current_proc(LABEL(mercury__string__format_pad_width_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	{
		tailcall(STATIC(mercury__string__append_3_2),
		STATIC(mercury__string__format_pad_width_5_0));
	}
Define_label(mercury__string__format_pad_width_5_0_i18);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	{
		tailcall(STATIC(mercury__string__append_3_2),
		STATIC(mercury__string__format_pad_width_5_0));
	}
Define_label(mercury__string__format_pad_width_5_0_i3);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module74)
	init_entry(mercury__string__format_get_optional_args_5_0);
	init_label(mercury__string__format_get_optional_args_5_0_i7);
	init_label(mercury__string__format_get_optional_args_5_0_i8);
	init_label(mercury__string__format_get_optional_args_5_0_i1002);
	init_label(mercury__string__format_get_optional_args_5_0_i4);
	init_label(mercury__string__format_get_optional_args_5_0_i12);
	init_label(mercury__string__format_get_optional_args_5_0_i13);
	init_label(mercury__string__format_get_optional_args_5_0_i14);
	init_label(mercury__string__format_get_optional_args_5_0_i15);
	init_label(mercury__string__format_get_optional_args_5_0_i16);
	init_label(mercury__string__format_get_optional_args_5_0_i9);
	init_label(mercury__string__format_get_optional_args_5_0_i23);
	init_label(mercury__string__format_get_optional_args_5_0_i24);
	init_label(mercury__string__format_get_optional_args_5_0_i22);
	init_label(mercury__string__format_get_optional_args_5_0_i26);
	init_label(mercury__string__format_get_optional_args_5_0_i20);
	init_label(mercury__string__format_get_optional_args_5_0_i30);
	init_label(mercury__string__format_get_optional_args_5_0_i31);
	init_label(mercury__string__format_get_optional_args_5_0_i27);
	init_label(mercury__string__format_get_optional_args_5_0_i1001);
BEGIN_CODE

/* code for predicate 'string__format_get_optional_args'/5 in mode 0 */
Define_static(mercury__string__format_get_optional_args_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i1001);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Unsigned)(((Integer) r3 - ((Integer) 32))) > ((Integer) 16)))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i1002);
	incr_sp_push_msg(4, "string__format_get_optional_args");
	detstackvar(4) = (Integer) succip;
	{
	static const Word mercury_const_1[] = {
		((Integer) 75785)
	};
	if (!(((((Integer) 1) << ((Unsigned)(((Integer) r3 - ((Integer) 32))) % ((Integer) 32))) & (Integer) field(mktag(0), mkword(mktag(0), mercury_const_1), ((Unsigned)(((Integer) r3 - ((Integer) 32))) / ((Integer) 32))))))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i4);
	}
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	detstackvar(1) = (Integer) r3;
	localcall(mercury__string__format_get_optional_args_5_0,
		LABEL(mercury__string__format_get_optional_args_5_0_i7),
		STATIC(mercury__string__format_get_optional_args_5_0));
Define_label(mercury__string__format_get_optional_args_5_0_i7);
	update_prof_current_proc(LABEL(mercury__string__format_get_optional_args_5_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_character_0;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r4;
	{
	Declare_entry(mercury__list__sort_and_remove_dups_2_0);
	call_localret(ENTRY(mercury__list__sort_and_remove_dups_2_0),
		mercury__string__format_get_optional_args_5_0_i8,
		STATIC(mercury__string__format_get_optional_args_5_0));
	}
	}
Define_label(mercury__string__format_get_optional_args_5_0_i8);
	update_prof_current_proc(LABEL(mercury__string__format_get_optional_args_5_0));
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__format_get_optional_args_5_0_i1002);
	incr_sp_push_msg(4, "string__format_get_optional_args");
	detstackvar(4) = (Integer) succip;
Define_label(mercury__string__format_get_optional_args_5_0_i4);
	if (((Unsigned)(((Integer) r3 - ((Integer) 46))) > ((Integer) 11)))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i9);
	{
	static const Word mercury_const_1[] = {
		((Integer) 4089)
	};
	if (!(((((Integer) 1) << ((Unsigned)(((Integer) r3 - ((Integer) 46))) % ((Integer) 32))) & (Integer) field(mktag(0), mkword(mktag(0), mercury_const_1), ((Unsigned)(((Integer) r3 - ((Integer) 46))) / ((Integer) 32))))))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i9);
	}
	r2 = ((Integer) 0);
	call_localret(STATIC(mercury__string__format_string_to_ints_5_0),
		mercury__string__format_get_optional_args_5_0_i12,
		STATIC(mercury__string__format_get_optional_args_5_0));
Define_label(mercury__string__format_get_optional_args_5_0_i12);
	update_prof_current_proc(LABEL(mercury__string__format_get_optional_args_5_0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__string__format_int_from_char_list_2_0),
		mercury__string__format_get_optional_args_5_0_i13,
		STATIC(mercury__string__format_get_optional_args_5_0));
Define_label(mercury__string__format_get_optional_args_5_0_i13);
	update_prof_current_proc(LABEL(mercury__string__format_get_optional_args_5_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	call_localret(STATIC(mercury__string__format_int_from_char_list_2_0),
		mercury__string__format_get_optional_args_5_0_i14,
		STATIC(mercury__string__format_get_optional_args_5_0));
Define_label(mercury__string__format_get_optional_args_5_0_i14);
	update_prof_current_proc(LABEL(mercury__string__format_get_optional_args_5_0));
	r2 = (Integer) detstackvar(2);
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__string__format_get_optional_args_5_0,
		LABEL(mercury__string__format_get_optional_args_5_0_i15),
		STATIC(mercury__string__format_get_optional_args_5_0));
Define_label(mercury__string__format_get_optional_args_5_0_i15);
	update_prof_current_proc(LABEL(mercury__string__format_get_optional_args_5_0));
	if (((Integer) detstackvar(3) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i16);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__format_get_optional_args_5_0_i16);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__format_get_optional_args_5_0_i9);
	if (((Integer) r3 != ((Integer) 76)))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i23);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i22);
Define_label(mercury__string__format_get_optional_args_5_0_i23);
	if (((Integer) r3 != ((Integer) 104)))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i24);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i22);
Define_label(mercury__string__format_get_optional_args_5_0_i24);
	if (((Integer) r3 != ((Integer) 108)))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i20);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
Define_label(mercury__string__format_get_optional_args_5_0_i22);
	detstackvar(3) = (Integer) r2;
	localcall(mercury__string__format_get_optional_args_5_0,
		LABEL(mercury__string__format_get_optional_args_5_0_i26),
		STATIC(mercury__string__format_get_optional_args_5_0));
Define_label(mercury__string__format_get_optional_args_5_0_i26);
	update_prof_current_proc(LABEL(mercury__string__format_get_optional_args_5_0));
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__format_get_optional_args_5_0_i20);
	if (((Integer) r3 != ((Integer) 42)))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i27);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) r2;
	localcall(mercury__string__format_get_optional_args_5_0,
		LABEL(mercury__string__format_get_optional_args_5_0_i30),
		STATIC(mercury__string__format_get_optional_args_5_0));
Define_label(mercury__string__format_get_optional_args_5_0_i30);
	update_prof_current_proc(LABEL(mercury__string__format_get_optional_args_5_0));
	if (((Integer) detstackvar(1) == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i31);
	if (((Integer) field(mktag(1), (Integer) detstackvar(1), ((Integer) 0)) != ((Integer) 46)))
		GOTO_LABEL(mercury__string__format_get_optional_args_5_0_i31);
	r2 = ((Integer) -1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__format_get_optional_args_5_0_i31);
	r3 = ((Integer) -1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__string__format_get_optional_args_5_0_i27);
	r1 = string_const("string__format:  Unrecognised formatting information\n", 53);
	{
	Declare_entry(mercury__require__error_1_0);
	call_localret(ENTRY(mercury__require__error_1_0),
		mercury__string__format_get_optional_args_5_0_i8,
		STATIC(mercury__string__format_get_optional_args_5_0));
	}
Define_label(mercury__string__format_get_optional_args_5_0_i1001);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = ((Integer) 0);
	r3 = ((Integer) -15);
	r4 = ((Integer) 32);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module75)
	init_entry(mercury__string__format_takewhile1_3_0);
	init_label(mercury__string__format_takewhile1_3_0_i1023);
	init_label(mercury__string__format_takewhile1_3_0_i8);
	init_label(mercury__string__format_takewhile1_3_0_i9);
	init_label(mercury__string__format_takewhile1_3_0_i10);
	init_label(mercury__string__format_takewhile1_3_0_i11);
	init_label(mercury__string__format_takewhile1_3_0_i12);
	init_label(mercury__string__format_takewhile1_3_0_i13);
	init_label(mercury__string__format_takewhile1_3_0_i14);
	init_label(mercury__string__format_takewhile1_3_0_i15);
	init_label(mercury__string__format_takewhile1_3_0_i16);
	init_label(mercury__string__format_takewhile1_3_0_i17);
	init_label(mercury__string__format_takewhile1_3_0_i18);
	init_label(mercury__string__format_takewhile1_3_0_i19);
	init_label(mercury__string__format_takewhile1_3_0_i20);
	init_label(mercury__string__format_takewhile1_3_0_i1019);
	init_label(mercury__string__format_takewhile1_3_0_i6);
	init_label(mercury__string__format_takewhile1_3_0_i4);
	init_label(mercury__string__format_takewhile1_3_0_i22);
	init_label(mercury__string__format_takewhile1_3_0_i1017);
BEGIN_CODE

/* code for predicate 'string__format_takewhile1'/3 in mode 0 */
Define_static(mercury__string__format_takewhile1_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i1017);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 == ((Integer) 37)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i1019);
Define_label(mercury__string__format_takewhile1_3_0_i1023);
	incr_sp_push_msg(2, "string__format_takewhile1");
	detstackvar(2) = (Integer) succip;
	if (((Integer) r3 == ((Integer) 69)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i8);
	if (((Integer) r3 == ((Integer) 71)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i9);
	if (((Integer) r3 == ((Integer) 88)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i10);
	if (((Integer) r3 == ((Integer) 99)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i11);
	if (((Integer) r3 == ((Integer) 100)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i12);
	if (((Integer) r3 == ((Integer) 101)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i13);
	if (((Integer) r3 == ((Integer) 102)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i14);
	if (((Integer) r3 == ((Integer) 103)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i15);
	if (((Integer) r3 == ((Integer) 105)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i16);
	if (((Integer) r3 == ((Integer) 111)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i17);
	if (((Integer) r3 == ((Integer) 112)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i18);
	if (((Integer) r3 == ((Integer) 115)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i19);
	if (((Integer) r3 == ((Integer) 117)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i20);
	if (((Integer) r3 != ((Integer) 120)))
		GOTO_LABEL(mercury__string__format_takewhile1_3_0_i4);
	GOTO_LABEL(mercury__string__format_takewhile1_3_0_i6);
Define_label(mercury__string__format_takewhile1_3_0_i1019);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__string__format_takewhile1_3_0_i6);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__format_takewhile1_3_0_i4);
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) r2;
	localcall(mercury__string__format_takewhile1_3_0,
		LABEL(mercury__string__format_takewhile1_3_0_i22),
		STATIC(mercury__string__format_takewhile1_3_0));
Define_label(mercury__string__format_takewhile1_3_0_i22);
	update_prof_current_proc(LABEL(mercury__string__format_takewhile1_3_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__string__format_takewhile1_3_0_i1017);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module76)
	init_entry(mercury__string__format_string_to_ints_5_0);
	init_label(mercury__string__format_string_to_ints_5_0_i6);
	init_label(mercury__string__format_string_to_ints_5_0_i11);
	init_label(mercury__string__format_string_to_ints_5_0_i8);
	init_label(mercury__string__format_string_to_ints_5_0_i12);
	init_label(mercury__string__format_string_to_ints_5_0_i5);
	init_label(mercury__string__format_string_to_ints_5_0_i1006);
	init_label(mercury__string__format_string_to_ints_5_0_i1007);
BEGIN_CODE

/* code for predicate 'string__format_string_to_ints'/5 in mode 0 */
Define_static(mercury__string__format_string_to_ints_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_string_to_ints_5_0_i1006);
	incr_sp_push_msg(5, "string__format_string_to_ints");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_digit_1_0);
	call_localret(ENTRY(mercury__char__is_digit_1_0),
		mercury__string__format_string_to_ints_5_0_i6,
		STATIC(mercury__string__format_string_to_ints_5_0));
	}
Define_label(mercury__string__format_string_to_ints_5_0_i6);
	update_prof_current_proc(LABEL(mercury__string__format_string_to_ints_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_string_to_ints_5_0_i5);
	if (((Integer) detstackvar(2) != ((Integer) 0)))
		GOTO_LABEL(mercury__string__format_string_to_ints_5_0_i8);
	r1 = (Integer) detstackvar(4);
	r2 = ((Integer) 0);
	localcall(mercury__string__format_string_to_ints_5_0,
		LABEL(mercury__string__format_string_to_ints_5_0_i11),
		STATIC(mercury__string__format_string_to_ints_5_0));
Define_label(mercury__string__format_string_to_ints_5_0_i11);
	update_prof_current_proc(LABEL(mercury__string__format_string_to_ints_5_0));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
	}
Define_label(mercury__string__format_string_to_ints_5_0_i8);
	r1 = (Integer) detstackvar(4);
	r2 = ((Integer) 1);
	localcall(mercury__string__format_string_to_ints_5_0,
		LABEL(mercury__string__format_string_to_ints_5_0_i12),
		STATIC(mercury__string__format_string_to_ints_5_0));
Define_label(mercury__string__format_string_to_ints_5_0_i12);
	update_prof_current_proc(LABEL(mercury__string__format_string_to_ints_5_0));
	r4 = (Integer) r3;
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__string__format_string_to_ints_5_0_i5);
	r1 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	if (((Integer) r4 != ((Integer) 46)))
		GOTO_LABEL(mercury__string__format_string_to_ints_5_0_i1007);
	r1 = (Integer) r3;
	r2 = ((Integer) 1);
	localtailcall(mercury__string__format_string_to_ints_5_0,
		STATIC(mercury__string__format_string_to_ints_5_0));
Define_label(mercury__string__format_string_to_ints_5_0_i1006);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__string__format_string_to_ints_5_0_i1007);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module77)
	init_entry(mercury__string__format_int_from_char_list_2_0);
	init_label(mercury__string__format_int_from_char_list_2_0_i6);
	init_label(mercury__string__format_int_from_char_list_2_0_i7);
	init_label(mercury__string__format_int_from_char_list_2_0_i1003);
	init_label(mercury__string__format_int_from_char_list_2_0_i1004);
BEGIN_CODE

/* code for predicate 'string__format_int_from_char_list'/2 in mode 0 */
Define_static(mercury__string__format_int_from_char_list_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__string__format_int_from_char_list_2_0_i1003);
	incr_sp_push_msg(1, "string__format_int_from_char_list");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__string__from_char_list_2_0),
		mercury__string__format_int_from_char_list_2_0_i6,
		STATIC(mercury__string__format_int_from_char_list_2_0));
	}
Define_label(mercury__string__format_int_from_char_list_2_0_i6);
	update_prof_current_proc(LABEL(mercury__string__format_int_from_char_list_2_0));
	{
		call_localret(STATIC(mercury__string__to_int_2_0),
		mercury__string__format_int_from_char_list_2_0_i7,
		STATIC(mercury__string__format_int_from_char_list_2_0));
	}
Define_label(mercury__string__format_int_from_char_list_2_0_i7);
	update_prof_current_proc(LABEL(mercury__string__format_int_from_char_list_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__string__format_int_from_char_list_2_0_i1004);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__string__format_int_from_char_list_2_0_i1003);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury__string__format_int_from_char_list_2_0_i1004);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module78)
	init_entry(mercury__string__float_to_f_string_2_0);
BEGIN_CODE

/* code for predicate 'string__float_to_f_string'/2 in mode 0 */
Define_static(mercury__string__float_to_f_string_2_0);
	{
		Float	FloatVal;
		String	FloatString;
		FloatVal = word_to_float((Integer) r1);
		{
	char buf[500];
	Word tmp;
	sprintf(buf, "%.15f", FloatVal);
	incr_hp_atomic(tmp, (strlen(buf) + sizeof(Word)) / sizeof(Word));
	FloatString = (char *)tmp;
	strcpy(FloatString, buf);
}
		r2 = (Word) FloatString;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module79)
	init_entry(mercury____Unify___string__poly_type_0_0);
	init_label(mercury____Unify___string__poly_type_0_0_i4);
	init_label(mercury____Unify___string__poly_type_0_0_i6);
	init_label(mercury____Unify___string__poly_type_0_0_i8);
	init_label(mercury____Unify___string__poly_type_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___string__poly_type_0_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Unify___string__poly_type_0_0_i4);
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Unify___string__poly_type_0_0_i1);
	if ((word_to_float((Integer) field(mktag(0), (Integer) r1, ((Integer) 0))) != word_to_float((Integer) field(mktag(0), (Integer) r2, ((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___string__poly_type_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___string__poly_type_0_0_i4);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___string__poly_type_0_0_i6);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___string__poly_type_0_0_i1);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 0)) != (Integer) field(mktag(1), (Integer) r2, ((Integer) 0))))
		GOTO_LABEL(mercury____Unify___string__poly_type_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___string__poly_type_0_0_i6);
	if (((Integer) r3 != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Unify___string__poly_type_0_0_i8);
	if ((tag((Integer) r2) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Unify___string__poly_type_0_0_i1);
	if ((strcmp((char *)(Integer) field(mktag(2), (Integer) r1, ((Integer) 0)), (char *)(Integer) field(mktag(2), (Integer) r2, ((Integer) 0))) !=0))
		GOTO_LABEL(mercury____Unify___string__poly_type_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___string__poly_type_0_0_i8);
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Unify___string__poly_type_0_0_i1);
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)) != (Integer) field(mktag(3), (Integer) r2, ((Integer) 0))))
		GOTO_LABEL(mercury____Unify___string__poly_type_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___string__poly_type_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module80)
	init_entry(mercury____Index___string__poly_type_0_0);
	init_label(mercury____Index___string__poly_type_0_0_i4);
	init_label(mercury____Index___string__poly_type_0_0_i5);
	init_label(mercury____Index___string__poly_type_0_0_i6);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___string__poly_type_0_0);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Index___string__poly_type_0_0_i4);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury____Index___string__poly_type_0_0_i4);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Index___string__poly_type_0_0_i5);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury____Index___string__poly_type_0_0_i5);
	if (((Integer) r2 != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Index___string__poly_type_0_0_i6);
	r1 = ((Integer) 2);
	proceed();
Define_label(mercury____Index___string__poly_type_0_0_i6);
	r1 = ((Integer) 3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__string_module81)
	init_entry(mercury____Compare___string__poly_type_0_0);
	init_label(mercury____Compare___string__poly_type_0_0_i2);
	init_label(mercury____Compare___string__poly_type_0_0_i3);
	init_label(mercury____Compare___string__poly_type_0_0_i4);
	init_label(mercury____Compare___string__poly_type_0_0_i6);
	init_label(mercury____Compare___string__poly_type_0_0_i12);
	init_label(mercury____Compare___string__poly_type_0_0_i15);
	init_label(mercury____Compare___string__poly_type_0_0_i18);
	init_label(mercury____Compare___string__poly_type_0_0_i1000);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___string__poly_type_0_0);
	incr_sp_push_msg(4, "__Compare__");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
		call_localret(STATIC(mercury____Index___string__poly_type_0_0),
		mercury____Compare___string__poly_type_0_0_i2,
		ENTRY(mercury____Compare___string__poly_type_0_0));
	}
Define_label(mercury____Compare___string__poly_type_0_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___string__poly_type_0_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury____Index___string__poly_type_0_0),
		mercury____Compare___string__poly_type_0_0_i3,
		ENTRY(mercury____Compare___string__poly_type_0_0));
	}
Define_label(mercury____Compare___string__poly_type_0_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___string__poly_type_0_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___string__poly_type_0_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___string__poly_type_0_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___string__poly_type_0_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___string__poly_type_0_0_i6);
	r1 = (Integer) detstackvar(1);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Compare___string__poly_type_0_0_i12);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if ((tag((Integer) r3) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Compare___string__poly_type_0_0_i1000);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__builtin_compare_float_3_0);
	tailcall(ENTRY(mercury__builtin_compare_float_3_0),
		ENTRY(mercury____Compare___string__poly_type_0_0));
	}
Define_label(mercury____Compare___string__poly_type_0_0_i12);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___string__poly_type_0_0_i15);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___string__poly_type_0_0_i1000);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___string__poly_type_0_0));
	}
Define_label(mercury____Compare___string__poly_type_0_0_i15);
	if (((Integer) r2 != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Compare___string__poly_type_0_0_i18);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if ((tag((Integer) r3) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Compare___string__poly_type_0_0_i1000);
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(2), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	tailcall(ENTRY(mercury__builtin_compare_string_3_0),
		ENTRY(mercury____Compare___string__poly_type_0_0));
	}
Define_label(mercury____Compare___string__poly_type_0_0_i18);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Compare___string__poly_type_0_0_i1000);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___string__poly_type_0_0));
	}
Define_label(mercury____Compare___string__poly_type_0_0_i1000);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___string__poly_type_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__string_bunch_0(void)
{
	mercury__string_module0();
	mercury__string_module1();
	mercury__string_module2();
	mercury__string_module3();
	mercury__string_module4();
	mercury__string_module5();
	mercury__string_module6();
	mercury__string_module7();
	mercury__string_module8();
	mercury__string_module9();
	mercury__string_module10();
	mercury__string_module11();
	mercury__string_module12();
	mercury__string_module13();
	mercury__string_module14();
	mercury__string_module15();
	mercury__string_module16();
	mercury__string_module17();
	mercury__string_module18();
	mercury__string_module19();
	mercury__string_module20();
	mercury__string_module21();
	mercury__string_module22();
	mercury__string_module23();
	mercury__string_module24();
	mercury__string_module25();
	mercury__string_module26();
	mercury__string_module27();
	mercury__string_module28();
	mercury__string_module29();
	mercury__string_module30();
	mercury__string_module31();
	mercury__string_module32();
	mercury__string_module33();
	mercury__string_module34();
	mercury__string_module35();
	mercury__string_module36();
	mercury__string_module37();
	mercury__string_module38();
	mercury__string_module39();
	mercury__string_module40();
}

static void mercury__string_bunch_1(void)
{
	mercury__string_module41();
	mercury__string_module42();
	mercury__string_module43();
	mercury__string_module44();
	mercury__string_module45();
	mercury__string_module46();
	mercury__string_module47();
	mercury__string_module48();
	mercury__string_module49();
	mercury__string_module50();
	mercury__string_module51();
	mercury__string_module52();
	mercury__string_module53();
	mercury__string_module54();
	mercury__string_module55();
	mercury__string_module56();
	mercury__string_module57();
	mercury__string_module58();
	mercury__string_module59();
	mercury__string_module60();
	mercury__string_module61();
	mercury__string_module62();
	mercury__string_module63();
	mercury__string_module64();
	mercury__string_module65();
	mercury__string_module66();
	mercury__string_module67();
	mercury__string_module68();
	mercury__string_module69();
	mercury__string_module70();
	mercury__string_module71();
	mercury__string_module72();
	mercury__string_module73();
	mercury__string_module74();
	mercury__string_module75();
	mercury__string_module76();
	mercury__string_module77();
	mercury__string_module78();
	mercury__string_module79();
	mercury__string_module80();
	mercury__string_module81();
}

#endif

void mercury__string__init(void); /* suppress gcc warning */
void mercury__string__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__string_bunch_0();
	mercury__string_bunch_1();
#endif
}
