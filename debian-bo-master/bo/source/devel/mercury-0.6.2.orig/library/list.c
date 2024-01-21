/*
** Automatically generated from `list.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__list__init
ENDINIT
*/

#include "imp.h"

Declare_static(mercury__list__apply__ua60002_2_0);
Declare_label(mercury__list__apply__ua60002_2_0_i2);
Declare_label(mercury__list__apply__ua60002_2_0_i3);
Declare_label(mercury__list__apply__ua60002_2_0_i4);
Declare_static(mercury__list__apply__ua40003_2_0);
Declare_label(mercury__list__apply__ua40003_2_0_i2);
Declare_label(mercury__list__apply__ua40003_2_0_i3);
Declare_label(mercury__list__apply__ua40003_2_0_i4);
Declare_static(mercury__list__apply__ua10000_2_0);
Declare_label(mercury__list__apply__ua10000_2_0_i4);
Declare_label(mercury__list__apply__ua10000_2_0_i5);
Declare_label(mercury__list__apply__ua10000_2_0_i1002);
Declare_static(mercury__list__apply__ua1_2_0);
Declare_label(mercury__list__apply__ua1_2_0_i1001);
Declare_label(mercury__list__apply__ua1_2_0_i4);
Declare_label(mercury__list__apply__ua1_2_0_i6);
Declare_label(mercury__list__apply__ua1_2_0_i1);
Declare_static(mercury__list__hosort__ua0_5_0);
Declare_label(mercury__list__hosort__ua0_5_0_i1032);
Declare_label(mercury__list__hosort__ua0_5_0_i11);
Declare_label(mercury__list__hosort__ua0_5_0_i13);
Declare_label(mercury__list__hosort__ua0_5_0_i1029);
Declare_label(mercury__list__hosort__ua0_5_0_i6);
Declare_label(mercury__list__hosort__ua0_5_0_i15);
Declare_label(mercury__list__hosort__ua0_5_0_i17);
Declare_label(mercury__list__hosort__ua0_5_0_i19);
Declare_label(mercury__list__hosort__ua0_5_0_i1017);
Declare_label(mercury__list__hosort__ua0_5_0_i1);
Declare_static(mercury__list__chunk_2__ua10000_5_0);
Declare_label(mercury__list__chunk_2__ua10000_5_0_i4);
Declare_label(mercury__list__chunk_2__ua10000_5_0_i7);
Declare_label(mercury__list__chunk_2__ua10000_5_0_i8);
Declare_label(mercury__list__chunk_2__ua10000_5_0_i1008);
Declare_label(mercury__list__chunk_2__ua10000_5_0_i10);
Declare_label(mercury__list__chunk_2__ua10000_5_0_i13);
Declare_static(mercury__list__zip2__ua10000_3_0);
Declare_label(mercury__list__zip2__ua10000_3_0_i4);
Declare_label(mercury__list__zip2__ua10000_3_0_i1002);
Declare_static(mercury__list__reverse_2__ua10000_3_0);
Declare_label(mercury__list__reverse_2__ua10000_3_0_i3);
Declare_label(mercury__list__reverse_2__ua10000_3_0_i1);
Declare_static(mercury__list__length_2__ua10000_3_0);
Declare_label(mercury__list__length_2__ua10000_3_0_i3);
Declare_label(mercury__list__length_2__ua10000_3_0_i1);
Declare_static(mercury__list__merge__ua10000_4_0);
Declare_label(mercury__list__merge__ua10000_4_0_i6);
Declare_label(mercury__list__merge__ua10000_4_0_i9);
Declare_label(mercury__list__merge__ua10000_4_0_i8);
Declare_label(mercury__list__merge__ua10000_4_0_i11);
Declare_label(mercury__list__merge__ua10000_4_0_i10);
Declare_label(mercury__list__merge__ua10000_4_0_i12);
Declare_label(mercury__list__merge__ua10000_4_0_i1010);
Declare_label(mercury__list__merge__ua10000_4_0_i1013);
Declare_label(mercury__list__merge__ua10000_4_0_i1012);
Declare_static(mercury__list__filter_map__ua10000_3_0);
Declare_label(mercury__list__filter_map__ua10000_3_0_i4);
Declare_label(mercury__list__filter_map__ua10000_3_0_i7);
Declare_label(mercury__list__filter_map__ua10000_3_0_i6);
Declare_label(mercury__list__filter_map__ua10000_3_0_i1003);
Declare_static(mercury__list__filter__ua10000_4_0);
Declare_label(mercury__list__filter__ua10000_4_0_i4);
Declare_label(mercury__list__filter__ua10000_4_0_i7);
Declare_label(mercury__list__filter__ua10000_4_0_i6);
Declare_label(mercury__list__filter__ua10000_4_0_i1004);
Declare_static(mercury__list__filter__ua10000_3_0);
Declare_static(mercury__list__foldr__ua10000_4_0);
Declare_label(mercury__list__foldr__ua10000_4_0_i4);
Declare_label(mercury__list__foldr__ua10000_4_0_i1002);
Declare_static(mercury__list__foldr__ua1_4_0);
Declare_label(mercury__list__foldr__ua1_4_0_i1001);
Declare_label(mercury__list__foldr__ua1_4_0_i4);
Declare_label(mercury__list__foldr__ua1_4_0_i6);
Declare_label(mercury__list__foldr__ua1_4_0_i1);
Declare_label(mercury__list__foldr__ua1_4_0_i1000);
Declare_static(mercury__list__foldl__ua10001_4_0);
Declare_label(mercury__list__foldl__ua10001_4_0_i4);
Declare_label(mercury__list__foldl__ua10001_4_0_i1002);
Declare_static(mercury__list__foldl__ua10000_4_0);
Declare_label(mercury__list__foldl__ua10000_4_0_i4);
Declare_label(mercury__list__foldl__ua10000_4_0_i1002);
Declare_static(mercury__list__foldl__ua2_4_0);
Declare_label(mercury__list__foldl__ua2_4_0_i1001);
Declare_label(mercury__list__foldl__ua2_4_0_i4);
Declare_label(mercury__list__foldl__ua2_4_0_i6);
Declare_label(mercury__list__foldl__ua2_4_0_i1);
Declare_label(mercury__list__foldl__ua2_4_0_i1000);
Declare_static(mercury__list__map__ua60002_3_0);
Declare_label(mercury__list__map__ua60002_3_0_i2);
Declare_label(mercury__list__map__ua60002_3_0_i3);
Declare_label(mercury__list__map__ua60002_3_0_i4);
Declare_static(mercury__list__map__ua40003_3_0);
Declare_label(mercury__list__map__ua40003_3_0_i2);
Declare_label(mercury__list__map__ua40003_3_0_i3);
Declare_label(mercury__list__map__ua40003_3_0_i4);
Declare_static(mercury__list__map__ua10000_3_0);
Declare_label(mercury__list__map__ua10000_3_0_i4);
Declare_label(mercury__list__map__ua10000_3_0_i5);
Declare_label(mercury__list__map__ua10000_3_0_i1002);
Declare_static(mercury__list__map__ua1_3_0);
Declare_label(mercury__list__map__ua1_3_0_i1001);
Declare_label(mercury__list__map__ua1_3_0_i4);
Declare_label(mercury__list__map__ua1_3_0_i6);
Declare_label(mercury__list__map__ua1_3_0_i1);
Declare_static(mercury__list__last__ua0_2_0);
Declare_label(mercury__list__last__ua0_2_0_i1007);
Declare_label(mercury__list__last__ua0_2_0_i5);
Declare_label(mercury__list__last__ua0_2_0_i1004);
Declare_label(mercury__list__last__ua0_2_0_i1006);
Declare_static(mercury__list__chunk__ua10000_3_0);
Declare_static(mercury__list__condense__ua10000_2_0);
Declare_label(mercury__list__condense__ua10000_2_0_i4);
Declare_label(mercury__list__condense__ua10000_2_0_i1002);
Declare_static(mercury__list__duplicate__ua10000_3_0);
Declare_label(mercury__list__duplicate__ua10000_3_0_i4);
Declare_label(mercury__list__duplicate__ua10000_3_0_i1002);
Declare_static(mercury__list__zip__ua10000_3_0);
Declare_label(mercury__list__zip__ua10000_3_0_i4);
Declare_label(mercury__list__zip__ua10000_3_0_i1002);
Declare_static(mercury__list__index1_det__ua10000_3_0);
Declare_static(mercury__list__index0_det__ua10000_3_0);
Declare_label(mercury__list__index0_det__ua10000_3_0_i4);
Declare_label(mercury__list__index0_det__ua10000_3_0_i1000);
Declare_static(mercury__list__index1__ua0_3_0);
Declare_label(mercury__list__index1__ua0_3_0_i2);
Declare_label(mercury__list__index1__ua0_3_0_i1000);
Declare_static(mercury__list__index0__ua0_3_0);
Declare_label(mercury__list__index0__ua0_3_0_i1007);
Declare_label(mercury__list__index0__ua0_3_0_i6);
Declare_label(mercury__list__index0__ua0_3_0_i1004);
Declare_label(mercury__list__index0__ua0_3_0_i1006);
Declare_static(mercury__list__perm__ua40000_2_0);
Declare_label(mercury__list__perm__ua40000_2_0_i2);
Declare_label(mercury__list__perm__ua40000_2_0_i3);
Declare_label(mercury__list__perm__ua40000_2_0_i1);
Declare_static(mercury__list__reverse__ua10000_2_0);
Declare_static(mercury__list__delete__ua60003_3_0);
Declare_label(mercury__list__delete__ua60003_3_0_i3);
Declare_label(mercury__list__delete__ua60003_3_0_i5);
Declare_static(mercury__list__delete__ua40002_3_0);
Declare_label(mercury__list__delete__ua40002_3_0_i4);
Declare_label(mercury__list__delete__ua40002_3_0_i5);
Declare_static(mercury__list__insert__ua60003_3_0);
Declare_label(mercury__list__insert__ua60003_3_0_i1);
Declare_static(mercury__list__insert__ua40002_3_0);
Declare_label(mercury__list__insert__ua40002_3_0_i1);
Declare_static(mercury__list__drop__ua0_3_0);
Declare_label(mercury__list__drop__ua0_3_0_i5);
Declare_label(mercury__list__drop__ua0_3_0_i1003);
Declare_label(mercury__list__drop__ua0_3_0_i1004);
Declare_label(mercury__list__drop__ua0_3_0_i1007);
Declare_static(mercury__list__take__ua0_3_0);
Declare_label(mercury__list__take__ua0_3_0_i5);
Declare_label(mercury__list__take__ua0_3_0_i1004);
Declare_label(mercury__list__take__ua0_3_0_i1005);
Declare_label(mercury__list__take__ua0_3_0_i1);
Declare_static(mercury__list__split_list__ua0_4_0);
Declare_label(mercury__list__split_list__ua0_4_0_i1001);
Declare_label(mercury__list__split_list__ua0_4_0_i6);
Declare_label(mercury__list__split_list__ua0_4_0_i1);
Declare_static(mercury__list__same_length__ua10001_2_0);
Declare_label(mercury__list__same_length__ua10001_2_0_i3);
Declare_label(mercury__list__same_length__ua10001_2_0_i4);
Declare_label(mercury__list__same_length__ua10001_2_0_i1);
Declare_static(mercury__list__same_length__ua10000_2_0);
Declare_label(mercury__list__same_length__ua10000_2_0_i3);
Declare_label(mercury__list__same_length__ua10000_2_0_i4);
Declare_label(mercury__list__same_length__ua10000_2_0_i1);
Declare_static(mercury__list__same_length__ua2_2_0);
Declare_label(mercury__list__same_length__ua2_2_0_i1008);
Declare_label(mercury__list__same_length__ua2_2_0_i1005);
Declare_label(mercury__list__same_length__ua2_2_0_i1007);
Declare_static(mercury__list__length__ua10000_2_0);
Declare_static(mercury__list__member__ua40000_3_0);
Declare_label(mercury__list__member__ua40000_3_0_i1);
Declare_static(mercury__list__member__ua40001_2_0);
Declare_label(mercury__list__member__ua40001_2_0_i4);
Declare_label(mercury__list__member__ua40001_2_0_i2);
Declare_static(mercury__list__append__ua60004_3_0);
Declare_label(mercury__list__append__ua60004_3_0_i3);
Declare_label(mercury__list__append__ua60004_3_0_i5);
Declare_static(mercury__list__append__ua10001_3_0);
Declare_label(mercury__list__append__ua10001_3_0_i3);
Declare_label(mercury__list__append__ua10001_3_0_i4);
Declare_label(mercury__list__append__ua10001_3_0_i1);
Declare_static(mercury__list__append__ua10000_3_0);
Declare_label(mercury__list__append__ua10000_3_0_i3);
Declare_label(mercury__list__append__ua10000_3_0_i4);
Declare_label(mercury__list__append__ua10000_3_0_i1);
Define_extern_entry(mercury__list__append_3_2);
Declare_label(mercury__list__append_3_2_i1001);
Declare_label(mercury__list__append_3_2_i7);
Declare_label(mercury__list__append_3_2_i1);
Define_extern_entry(mercury__list__append_3_3);
Declare_label(mercury__list__append_3_3_i1001);
Declare_label(mercury__list__append_3_3_i5);
Declare_label(mercury__list__append_3_3_i7);
Declare_label(mercury__list__append_3_3_i1);
Declare_label(mercury__list__append_3_3_i1000);
Define_extern_entry(mercury__list__append_3_0);
Define_extern_entry(mercury__list__append_3_1);
Define_extern_entry(mercury__list__append_3_4);
Declare_label(mercury__list__append_3_4_i1);
Define_extern_entry(mercury__list__remove_suffix_3_0);
Declare_label(mercury__list__remove_suffix_3_0_i2);
Declare_label(mercury__list__remove_suffix_3_0_i3);
Declare_label(mercury__list__remove_suffix_3_0_i4);
Declare_label(mercury__list__remove_suffix_3_0_i6);
Declare_label(mercury__list__remove_suffix_3_0_i1);
Define_extern_entry(mercury__list__merge_3_0);
Declare_label(mercury__list__merge_3_0_i10);
Declare_label(mercury__list__merge_3_0_i11);
Declare_label(mercury__list__merge_3_0_i9);
Declare_label(mercury__list__merge_3_0_i12);
Declare_label(mercury__list__merge_3_0_i1006);
Declare_label(mercury__list__merge_3_0_i1005);
Define_extern_entry(mercury__list__merge_and_remove_dups_3_0);
Declare_label(mercury__list__merge_and_remove_dups_3_0_i8);
Declare_label(mercury__list__merge_and_remove_dups_3_0_i12);
Declare_label(mercury__list__merge_and_remove_dups_3_0_i9);
Declare_label(mercury__list__merge_and_remove_dups_3_0_i16);
Declare_label(mercury__list__merge_and_remove_dups_3_0_i13);
Declare_label(mercury__list__merge_and_remove_dups_3_0_i1007);
Declare_label(mercury__list__merge_and_remove_dups_3_0_i1006);
Define_extern_entry(mercury__list__remove_adjacent_dups_2_0);
Declare_label(mercury__list__remove_adjacent_dups_2_0_i1002);
Define_extern_entry(mercury__list__remove_dups_2_0);
Declare_label(mercury__list__remove_dups_2_0_i2);
Define_extern_entry(mercury__list__member_2_0);
Declare_label(mercury__list__member_2_0_i6);
Declare_label(mercury__list__member_2_0_i3);
Declare_label(mercury__list__member_2_0_i1003);
Define_extern_entry(mercury__list__member_2_1);
Declare_label(mercury__list__member_2_1_i1);
Define_extern_entry(mercury__list__member_3_0);
Declare_label(mercury__list__member_3_0_i1);
Define_extern_entry(mercury__list__length_2_0);
Define_extern_entry(mercury__list__same_length_2_2);
Define_extern_entry(mercury__list__same_length_2_0);
Define_extern_entry(mercury__list__same_length_2_1);
Define_extern_entry(mercury__list__split_list_4_0);
Declare_label(mercury__list__split_list_4_0_i2);
Declare_label(mercury__list__split_list_4_0_i1000);
Define_extern_entry(mercury__list__take_3_0);
Declare_label(mercury__list__take_3_0_i2);
Declare_label(mercury__list__take_3_0_i1000);
Define_extern_entry(mercury__list__drop_3_0);
Declare_label(mercury__list__drop_3_0_i2);
Declare_label(mercury__list__drop_3_0_i1000);
Define_extern_entry(mercury__list__insert_3_0);
Define_extern_entry(mercury__list__insert_3_1);
Declare_label(mercury__list__insert_3_1_i1);
Define_extern_entry(mercury__list__insert_3_2);
Declare_label(mercury__list__insert_3_2_i1);
Define_extern_entry(mercury__list__insert_3_3);
Declare_label(mercury__list__insert_3_3_i1);
Define_extern_entry(mercury__list__delete_3_0);
Declare_label(mercury__list__delete_3_0_i6);
Declare_label(mercury__list__delete_3_0_i8);
Declare_label(mercury__list__delete_3_0_i5);
Declare_label(mercury__list__delete_3_0_i11);
Declare_label(mercury__list__delete_3_0_i3);
Declare_label(mercury__list__delete_3_0_i1005);
Declare_label(mercury__list__delete_3_0_i1);
Define_extern_entry(mercury__list__delete_3_1);
Declare_label(mercury__list__delete_3_1_i5);
Declare_label(mercury__list__delete_3_1_i4);
Declare_label(mercury__list__delete_3_1_i1009);
Declare_label(mercury__list__delete_3_1_i7);
Define_extern_entry(mercury__list__delete_3_2);
Declare_label(mercury__list__delete_3_2_i1);
Define_extern_entry(mercury__list__delete_3_3);
Declare_label(mercury__list__delete_3_3_i1);
Define_extern_entry(mercury__list__delete_first_3_0);
Declare_label(mercury__list__delete_first_3_0_i5);
Declare_label(mercury__list__delete_first_3_0_i4);
Declare_label(mercury__list__delete_first_3_0_i7);
Declare_label(mercury__list__delete_first_3_0_i1004);
Declare_label(mercury__list__delete_first_3_0_i1);
Define_extern_entry(mercury__list__delete_all_3_0);
Declare_label(mercury__list__delete_all_3_0_i6);
Declare_label(mercury__list__delete_all_3_0_i5);
Declare_label(mercury__list__delete_all_3_0_i9);
Declare_label(mercury__list__delete_all_3_0_i1003);
Define_extern_entry(mercury__list__delete_all_3_1);
Declare_label(mercury__list__delete_all_3_1_i6);
Declare_label(mercury__list__delete_all_3_1_i5);
Declare_label(mercury__list__delete_all_3_1_i9);
Declare_label(mercury__list__delete_all_3_1_i1003);
Define_extern_entry(mercury__list__delete_elems_3_0);
Declare_label(mercury__list__delete_elems_3_0_i4);
Declare_label(mercury__list__delete_elems_3_0_i1002);
Define_extern_entry(mercury__list__replace_4_0);
Declare_label(mercury__list__replace_4_0_i7);
Declare_label(mercury__list__replace_4_0_i9);
Declare_label(mercury__list__replace_4_0_i11);
Declare_label(mercury__list__replace_4_0_i6);
Declare_label(mercury__list__replace_4_0_i13);
Declare_label(mercury__list__replace_4_0_i4);
Declare_label(mercury__list__replace_4_0_i1006);
Declare_label(mercury__list__replace_4_0_i1);
Define_extern_entry(mercury__list__replace_4_1);
Declare_label(mercury__list__replace_4_1_i5);
Declare_label(mercury__list__replace_4_1_i4);
Declare_label(mercury__list__replace_4_1_i1011);
Declare_label(mercury__list__replace_4_1_i7);
Define_extern_entry(mercury__list__replace_first_4_0);
Declare_label(mercury__list__replace_first_4_0_i5);
Declare_label(mercury__list__replace_first_4_0_i4);
Declare_label(mercury__list__replace_first_4_0_i7);
Declare_label(mercury__list__replace_first_4_0_i1005);
Declare_label(mercury__list__replace_first_4_0_i1);
Define_extern_entry(mercury__list__replace_all_4_0);
Declare_label(mercury__list__replace_all_4_0_i6);
Declare_label(mercury__list__replace_all_4_0_i8);
Declare_label(mercury__list__replace_all_4_0_i5);
Declare_label(mercury__list__replace_all_4_0_i9);
Declare_label(mercury__list__replace_all_4_0_i1004);
Define_extern_entry(mercury__list__sort_and_remove_dups_2_0);
Declare_label(mercury__list__sort_and_remove_dups_2_0_i2);
Define_extern_entry(mercury__list__sort_2_0);
Define_extern_entry(mercury__list__reverse_2_0);
Define_extern_entry(mercury__list__perm_2_0);
Declare_label(mercury__list__perm_2_0_i1);
Define_extern_entry(mercury__list__nth_member_search_3_0);
Declare_label(mercury__list__nth_member_search_3_0_i5);
Declare_label(mercury__list__nth_member_search_3_0_i4);
Declare_label(mercury__list__nth_member_search_3_0_i7);
Declare_label(mercury__list__nth_member_search_3_0_i1004);
Declare_label(mercury__list__nth_member_search_3_0_i1006);
Define_extern_entry(mercury__list__index0_3_0);
Declare_label(mercury__list__index0_3_0_i2);
Declare_label(mercury__list__index0_3_0_i1000);
Define_extern_entry(mercury__list__index1_3_0);
Declare_label(mercury__list__index1_3_0_i2);
Declare_label(mercury__list__index1_3_0_i1000);
Define_extern_entry(mercury__list__index0_det_3_0);
Define_extern_entry(mercury__list__index1_det_3_0);
Define_extern_entry(mercury__list__zip_3_0);
Define_extern_entry(mercury__list__duplicate_3_0);
Define_extern_entry(mercury__list__condense_2_0);
Define_extern_entry(mercury__list__chunk_3_0);
Define_extern_entry(mercury__list__sublist_2_0);
Declare_label(mercury__list__sublist_2_0_i7);
Declare_label(mercury__list__sublist_2_0_i6);
Declare_label(mercury__list__sublist_2_0_i1004);
Declare_label(mercury__list__sublist_2_0_i1005);
Define_extern_entry(mercury__list__all_same_1_0);
Declare_label(mercury__list__all_same_1_0_i1002);
Define_extern_entry(mercury__list__last_2_0);
Declare_label(mercury__list__last_2_0_i2);
Declare_label(mercury__list__last_2_0_i1000);
Define_extern_entry(mercury__list__map_3_1);
Declare_label(mercury__list__map_3_1_i2);
Declare_label(mercury__list__map_3_1_i1000);
Define_extern_entry(mercury__list__map_3_0);
Define_extern_entry(mercury__list__map_3_3);
Declare_label(mercury__list__map_3_3_i1);
Define_extern_entry(mercury__list__map_3_2);
Declare_label(mercury__list__map_3_2_i1);
Define_extern_entry(mercury__list__foldl_4_2);
Declare_label(mercury__list__foldl_4_2_i2);
Declare_label(mercury__list__foldl_4_2_i1000);
Define_extern_entry(mercury__list__foldl_4_0);
Define_extern_entry(mercury__list__foldl_4_1);
Define_extern_entry(mercury__list__foldr_4_1);
Declare_label(mercury__list__foldr_4_1_i2);
Declare_label(mercury__list__foldr_4_1_i1000);
Define_extern_entry(mercury__list__foldr_4_0);
Define_extern_entry(mercury__list__filter_3_0);
Define_extern_entry(mercury__list__filter_4_0);
Define_extern_entry(mercury__list__filter_map_3_0);
Define_extern_entry(mercury__list__sort_3_0);
Declare_label(mercury__list__sort_3_0_i2);
Declare_label(mercury__list__sort_3_0_i3);
Declare_label(mercury__list__sort_3_0_i8);
Declare_label(mercury__list__sort_3_0_i10);
Declare_label(mercury__list__sort_3_0_i7);
Define_extern_entry(mercury__list__merge_4_0);
Declare_static(mercury__list__merge_sort_2_0);
Declare_label(mercury__list__merge_sort_2_0_i6);
Declare_label(mercury__list__merge_sort_2_0_i9);
Declare_label(mercury__list__merge_sort_2_0_i11);
Declare_label(mercury__list__merge_sort_2_0_i12);
Declare_label(mercury__list__merge_sort_2_0_i8);
Declare_label(mercury__list__merge_sort_2_0_i1005);
Declare_label(mercury__list__merge_sort_2_0_i1004);
Declare_static(mercury__list__remove_dups_2_3_0);
Declare_label(mercury__list__remove_dups_2_3_0_i6);
Declare_label(mercury__list__remove_dups_2_3_0_i5);
Declare_label(mercury__list__remove_dups_2_3_0_i9);
Declare_label(mercury__list__remove_dups_2_3_0_i10);
Declare_label(mercury__list__remove_dups_2_3_0_i1003);
Declare_static(mercury__list__remove_adjacent_dups_2_3_0);
Declare_label(mercury__list__remove_adjacent_dups_2_3_0_i6);
Declare_label(mercury__list__remove_adjacent_dups_2_3_0_i5);
Declare_label(mercury__list__remove_adjacent_dups_2_3_0_i9);
Declare_label(mercury__list__remove_adjacent_dups_2_3_0_i1005);
Declare_static(mercury__list__all_same_2_2_0);
Declare_label(mercury__list__all_same_2_2_0_i4);
Declare_label(mercury__list__all_same_2_2_0_i1003);
Declare_label(mercury__list__all_same_2_2_0_i1);
Define_extern_entry(mercury__list__apply_2_1);
Declare_label(mercury__list__apply_2_1_i2);
Declare_label(mercury__list__apply_2_1_i1000);
Define_extern_entry(mercury__list__apply_2_0);
Define_extern_entry(mercury__list__apply_2_3);
Declare_label(mercury__list__apply_2_3_i1);
Define_extern_entry(mercury__list__apply_2_2);
Declare_label(mercury__list__apply_2_2_i1);

BEGIN_MODULE(mercury__list_module0)
	init_entry(mercury__list__apply__ua60002_2_0);
	init_label(mercury__list__apply__ua60002_2_0_i2);
	init_label(mercury__list__apply__ua60002_2_0_i3);
	init_label(mercury__list__apply__ua60002_2_0_i4);
BEGIN_CODE

/* code for predicate 'list__apply__ua60002'/2 in mode 0 */
Define_static(mercury__list__apply__ua60002_2_0);
	{
	Declare_entry(do_fail);
	mkframe("list__apply__ua60002/2", 2, ENTRY(do_fail));
	}
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__apply__ua60002_2_0_i2);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	succeed();
Define_label(mercury__list__apply__ua60002_2_0_i2);
	framevar(0) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = ((Integer) 0);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_nondet_closure);
	call_localret(ENTRY(do_call_nondet_closure),
		mercury__list__apply__ua60002_2_0_i3,
		STATIC(mercury__list__apply__ua60002_2_0));
	}
Define_label(mercury__list__apply__ua60002_2_0_i3);
	update_prof_current_proc(LABEL(mercury__list__apply__ua60002_2_0));
	framevar(1) = (Integer) r1;
	r1 = (Integer) framevar(0);
	localcall(mercury__list__apply__ua60002_2_0,
		LABEL(mercury__list__apply__ua60002_2_0_i4),
		STATIC(mercury__list__apply__ua60002_2_0));
Define_label(mercury__list__apply__ua60002_2_0_i4);
	update_prof_current_proc(LABEL(mercury__list__apply__ua60002_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) framevar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module1)
	init_entry(mercury__list__apply__ua40003_2_0);
	init_label(mercury__list__apply__ua40003_2_0_i2);
	init_label(mercury__list__apply__ua40003_2_0_i3);
	init_label(mercury__list__apply__ua40003_2_0_i4);
BEGIN_CODE

/* code for predicate 'list__apply__ua40003'/2 in mode 0 */
Define_static(mercury__list__apply__ua40003_2_0);
	{
	Declare_entry(do_fail);
	mkframe("list__apply__ua40003/2", 2, ENTRY(do_fail));
	}
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__apply__ua40003_2_0_i2);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	succeed();
Define_label(mercury__list__apply__ua40003_2_0_i2);
	framevar(0) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = ((Integer) 0);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_nondet_closure);
	call_localret(ENTRY(do_call_nondet_closure),
		mercury__list__apply__ua40003_2_0_i3,
		STATIC(mercury__list__apply__ua40003_2_0));
	}
Define_label(mercury__list__apply__ua40003_2_0_i3);
	update_prof_current_proc(LABEL(mercury__list__apply__ua40003_2_0));
	framevar(1) = (Integer) r1;
	r1 = (Integer) framevar(0);
	localcall(mercury__list__apply__ua40003_2_0,
		LABEL(mercury__list__apply__ua40003_2_0_i4),
		STATIC(mercury__list__apply__ua40003_2_0));
Define_label(mercury__list__apply__ua40003_2_0_i4);
	update_prof_current_proc(LABEL(mercury__list__apply__ua40003_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) framevar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module2)
	init_entry(mercury__list__apply__ua10000_2_0);
	init_label(mercury__list__apply__ua10000_2_0_i4);
	init_label(mercury__list__apply__ua10000_2_0_i5);
	init_label(mercury__list__apply__ua10000_2_0_i1002);
BEGIN_CODE

/* code for predicate 'list__apply__ua10000'/2 in mode 0 */
Define_static(mercury__list__apply__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__apply__ua10000_2_0_i1002);
	incr_sp_push_msg(2, "list__apply__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = ((Integer) 0);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_det_closure);
	call_localret(ENTRY(do_call_det_closure),
		mercury__list__apply__ua10000_2_0_i4,
		STATIC(mercury__list__apply__ua10000_2_0));
	}
Define_label(mercury__list__apply__ua10000_2_0_i4);
	update_prof_current_proc(LABEL(mercury__list__apply__ua10000_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	localcall(mercury__list__apply__ua10000_2_0,
		LABEL(mercury__list__apply__ua10000_2_0_i5),
		STATIC(mercury__list__apply__ua10000_2_0));
Define_label(mercury__list__apply__ua10000_2_0_i5);
	update_prof_current_proc(LABEL(mercury__list__apply__ua10000_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__list__apply__ua10000_2_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module3)
	init_entry(mercury__list__apply__ua1_2_0);
	init_label(mercury__list__apply__ua1_2_0_i1001);
	init_label(mercury__list__apply__ua1_2_0_i4);
	init_label(mercury__list__apply__ua1_2_0_i6);
	init_label(mercury__list__apply__ua1_2_0_i1);
BEGIN_CODE

/* code for predicate 'list__apply__ua1'/2 in mode 0 */
Define_static(mercury__list__apply__ua1_2_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__apply__ua1_2_0_i1001);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = TRUE;
	proceed();
Define_label(mercury__list__apply__ua1_2_0_i1001);
	incr_sp_push_msg(2, "list__apply__ua1");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = ((Integer) 0);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_semidet_closure);
	call_localret(ENTRY(do_call_semidet_closure),
		mercury__list__apply__ua1_2_0_i4,
		STATIC(mercury__list__apply__ua1_2_0));
	}
Define_label(mercury__list__apply__ua1_2_0_i4);
	update_prof_current_proc(LABEL(mercury__list__apply__ua1_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__apply__ua1_2_0_i1);
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	localcall(mercury__list__apply__ua1_2_0,
		LABEL(mercury__list__apply__ua1_2_0_i6),
		STATIC(mercury__list__apply__ua1_2_0));
Define_label(mercury__list__apply__ua1_2_0_i6);
	update_prof_current_proc(LABEL(mercury__list__apply__ua1_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__apply__ua1_2_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__list__apply__ua1_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module4)
	init_entry(mercury__list__hosort__ua0_5_0);
	init_label(mercury__list__hosort__ua0_5_0_i1032);
	init_label(mercury__list__hosort__ua0_5_0_i11);
	init_label(mercury__list__hosort__ua0_5_0_i13);
	init_label(mercury__list__hosort__ua0_5_0_i1029);
	init_label(mercury__list__hosort__ua0_5_0_i6);
	init_label(mercury__list__hosort__ua0_5_0_i15);
	init_label(mercury__list__hosort__ua0_5_0_i17);
	init_label(mercury__list__hosort__ua0_5_0_i19);
	init_label(mercury__list__hosort__ua0_5_0_i1017);
	init_label(mercury__list__hosort__ua0_5_0_i1);
BEGIN_CODE

/* code for predicate 'list__hosort__ua0'/5 in mode 0 */
Define_static(mercury__list__hosort__ua0_5_0);
	if (((Integer) r2 != ((Integer) 1)))
		GOTO_LABEL(mercury__list__hosort__ua0_5_0_i1032);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__hosort__ua0_5_0_i1017);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	r1 = TRUE;
	proceed();
Define_label(mercury__list__hosort__ua0_5_0_i1032);
	incr_sp_push_msg(4, "list__hosort__ua0");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r2 != ((Integer) 2)))
		GOTO_LABEL(mercury__list__hosort__ua0_5_0_i6);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__hosort__ua0_5_0_i1);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	if (((Integer) tempr1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__hosort__ua0_5_0_i1);
	r4 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r5 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	detstackvar(2) = (Integer) r5;
	detstackvar(3) = (Integer) r4;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 1));
	r2 = ((Integer) 2);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_det_closure);
	call_localret(ENTRY(do_call_det_closure),
		mercury__list__hosort__ua0_5_0_i11,
		STATIC(mercury__list__hosort__ua0_5_0));
	}
	}
Define_label(mercury__list__hosort__ua0_5_0_i11);
	update_prof_current_proc(LABEL(mercury__list__hosort__ua0_5_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__list__hosort__ua0_5_0_i13);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r3 = (Integer) detstackvar(1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__list__hosort__ua0_5_0_i13);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__list__hosort__ua0_5_0_i1029);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r3 = (Integer) detstackvar(1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__list__hosort__ua0_5_0_i1029);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r3 = (Integer) detstackvar(1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__list__hosort__ua0_5_0_i6);
	detstackvar(2) = (Integer) r2;
	r2 = ((Integer) r2 / ((Integer) 2));
	detstackvar(1) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	localcall(mercury__list__hosort__ua0_5_0,
		LABEL(mercury__list__hosort__ua0_5_0_i15),
		STATIC(mercury__list__hosort__ua0_5_0));
Define_label(mercury__list__hosort__ua0_5_0_i15);
	update_prof_current_proc(LABEL(mercury__list__hosort__ua0_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__hosort__ua0_5_0_i1);
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = ((Integer) detstackvar(2) - (Integer) detstackvar(3));
	detstackvar(2) = (Integer) tempr1;
	r1 = (Integer) detstackvar(1);
	localcall(mercury__list__hosort__ua0_5_0,
		LABEL(mercury__list__hosort__ua0_5_0_i17),
		STATIC(mercury__list__hosort__ua0_5_0));
	}
Define_label(mercury__list__hosort__ua0_5_0_i17);
	update_prof_current_proc(LABEL(mercury__list__hosort__ua0_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__hosort__ua0_5_0_i1);
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__list__merge__ua10000_4_0),
		mercury__list__hosort__ua0_5_0_i19,
		STATIC(mercury__list__hosort__ua0_5_0));
Define_label(mercury__list__hosort__ua0_5_0_i19);
	update_prof_current_proc(LABEL(mercury__list__hosort__ua0_5_0));
	r2 = (Integer) r1;
	r3 = (Integer) detstackvar(1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__list__hosort__ua0_5_0_i1017);
	r1 = FALSE;
	proceed();
Define_label(mercury__list__hosort__ua0_5_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module5)
	init_entry(mercury__list__chunk_2__ua10000_5_0);
	init_label(mercury__list__chunk_2__ua10000_5_0_i4);
	init_label(mercury__list__chunk_2__ua10000_5_0_i7);
	init_label(mercury__list__chunk_2__ua10000_5_0_i8);
	init_label(mercury__list__chunk_2__ua10000_5_0_i1008);
	init_label(mercury__list__chunk_2__ua10000_5_0_i10);
	init_label(mercury__list__chunk_2__ua10000_5_0_i13);
BEGIN_CODE

/* code for predicate 'list__chunk_2__ua10000'/5 in mode 0 */
Define_static(mercury__list__chunk_2__ua10000_5_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__chunk_2__ua10000_5_0_i1008);
	r5 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r6 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	incr_sp_push_msg(3, "list__chunk_2__ua10000");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r4 <= ((Integer) 1)))
		GOTO_LABEL(mercury__list__chunk_2__ua10000_5_0_i4);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	r3 = (Integer) tempr1;
	r1 = (Integer) r5;
	r4 = ((Integer) r4 - ((Integer) 1));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r6;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__list__chunk_2__ua10000_5_0,
		STATIC(mercury__list__chunk_2__ua10000_5_0));
	}
Define_label(mercury__list__chunk_2__ua10000_5_0_i4);
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r5;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r6;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	call_localret(STATIC(mercury__list__reverse__ua10000_2_0),
		mercury__list__chunk_2__ua10000_5_0_i7,
		STATIC(mercury__list__chunk_2__ua10000_5_0));
Define_label(mercury__list__chunk_2__ua10000_5_0_i7);
	update_prof_current_proc(LABEL(mercury__list__chunk_2__ua10000_5_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) r2;
	localcall(mercury__list__chunk_2__ua10000_5_0,
		LABEL(mercury__list__chunk_2__ua10000_5_0_i8),
		STATIC(mercury__list__chunk_2__ua10000_5_0));
Define_label(mercury__list__chunk_2__ua10000_5_0_i8);
	update_prof_current_proc(LABEL(mercury__list__chunk_2__ua10000_5_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__list__chunk_2__ua10000_5_0_i1008);
	incr_sp_push_msg(3, "list__chunk_2__ua10000");
	detstackvar(3) = (Integer) succip;
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__chunk_2__ua10000_5_0_i10);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__list__chunk_2__ua10000_5_0_i10);
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__list__reverse__ua10000_2_0),
		mercury__list__chunk_2__ua10000_5_0_i13,
		STATIC(mercury__list__chunk_2__ua10000_5_0));
Define_label(mercury__list__chunk_2__ua10000_5_0_i13);
	update_prof_current_proc(LABEL(mercury__list__chunk_2__ua10000_5_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module6)
	init_entry(mercury__list__zip2__ua10000_3_0);
	init_label(mercury__list__zip2__ua10000_3_0_i4);
	init_label(mercury__list__zip2__ua10000_3_0_i1002);
BEGIN_CODE

/* code for predicate 'list__zip2__ua10000'/3 in mode 0 */
Define_static(mercury__list__zip2__ua10000_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__zip2__ua10000_3_0_i1002);
	incr_sp_push_msg(2, "list__zip2__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	call_localret(STATIC(mercury__list__zip__ua10000_3_0),
		mercury__list__zip2__ua10000_3_0_i4,
		STATIC(mercury__list__zip2__ua10000_3_0));
Define_label(mercury__list__zip2__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__zip2__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__list__zip2__ua10000_3_0_i1002);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module7)
	init_entry(mercury__list__reverse_2__ua10000_3_0);
	init_label(mercury__list__reverse_2__ua10000_3_0_i3);
	init_label(mercury__list__reverse_2__ua10000_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__reverse_2__ua10000'/3 in mode 0 */
Define_static(mercury__list__reverse_2__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__reverse_2__ua10000_3_0_i1);
Define_label(mercury__list__reverse_2__ua10000_3_0_i3);
	while (1) {
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) r3;
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	break; } /* end while */
Define_label(mercury__list__reverse_2__ua10000_3_0_i1);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module8)
	init_entry(mercury__list__length_2__ua10000_3_0);
	init_label(mercury__list__length_2__ua10000_3_0_i3);
	init_label(mercury__list__length_2__ua10000_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__length_2__ua10000'/3 in mode 0 */
Define_static(mercury__list__length_2__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__length_2__ua10000_3_0_i1);
Define_label(mercury__list__length_2__ua10000_3_0_i3);
	while (1) {
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = ((Integer) r2 + ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	break; } /* end while */
Define_label(mercury__list__length_2__ua10000_3_0_i1);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module9)
	init_entry(mercury__list__merge__ua10000_4_0);
	init_label(mercury__list__merge__ua10000_4_0_i6);
	init_label(mercury__list__merge__ua10000_4_0_i9);
	init_label(mercury__list__merge__ua10000_4_0_i8);
	init_label(mercury__list__merge__ua10000_4_0_i11);
	init_label(mercury__list__merge__ua10000_4_0_i10);
	init_label(mercury__list__merge__ua10000_4_0_i12);
	init_label(mercury__list__merge__ua10000_4_0_i1010);
	init_label(mercury__list__merge__ua10000_4_0_i1013);
	init_label(mercury__list__merge__ua10000_4_0_i1012);
BEGIN_CODE

/* code for predicate 'list__merge__ua10000'/4 in mode 0 */
Define_static(mercury__list__merge__ua10000_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__merge__ua10000_4_0_i1013);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__merge__ua10000_4_0_i1010);
	incr_sp_push_msg(8, "list__merge__ua10000");
	detstackvar(8) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(7) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r4 = (Integer) detstackvar(7);
	r5 = (Integer) detstackvar(4);
	r2 = ((Integer) 2);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_det_closure);
	call_localret(ENTRY(do_call_det_closure),
		mercury__list__merge__ua10000_4_0_i6,
		STATIC(mercury__list__merge__ua10000_4_0));
	}
Define_label(mercury__list__merge__ua10000_4_0_i6);
	update_prof_current_proc(LABEL(mercury__list__merge__ua10000_4_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__list__merge__ua10000_4_0_i8);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(5);
	localcall(mercury__list__merge__ua10000_4_0,
		LABEL(mercury__list__merge__ua10000_4_0_i9),
		STATIC(mercury__list__merge__ua10000_4_0));
Define_label(mercury__list__merge__ua10000_4_0_i9);
	update_prof_current_proc(LABEL(mercury__list__merge__ua10000_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(7);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__list__merge__ua10000_4_0_i8);
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__list__merge__ua10000_4_0_i10);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(6);
	r3 = (Integer) detstackvar(3);
	localcall(mercury__list__merge__ua10000_4_0,
		LABEL(mercury__list__merge__ua10000_4_0_i11),
		STATIC(mercury__list__merge__ua10000_4_0));
Define_label(mercury__list__merge__ua10000_4_0_i11);
	update_prof_current_proc(LABEL(mercury__list__merge__ua10000_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(7);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__list__merge__ua10000_4_0_i10);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(5);
	localcall(mercury__list__merge__ua10000_4_0,
		LABEL(mercury__list__merge__ua10000_4_0_i12),
		STATIC(mercury__list__merge__ua10000_4_0));
Define_label(mercury__list__merge__ua10000_4_0_i12);
	update_prof_current_proc(LABEL(mercury__list__merge__ua10000_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__list__merge__ua10000_4_0_i1010);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__list__merge__ua10000_4_0_i1013);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__merge__ua10000_4_0_i1012);
	r1 = (Integer) r3;
	proceed();
Define_label(mercury__list__merge__ua10000_4_0_i1012);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module10)
	init_entry(mercury__list__filter_map__ua10000_3_0);
	init_label(mercury__list__filter_map__ua10000_3_0_i4);
	init_label(mercury__list__filter_map__ua10000_3_0_i7);
	init_label(mercury__list__filter_map__ua10000_3_0_i6);
	init_label(mercury__list__filter_map__ua10000_3_0_i1003);
BEGIN_CODE

/* code for predicate 'list__filter_map__ua10000'/3 in mode 0 */
Define_static(mercury__list__filter_map__ua10000_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__filter_map__ua10000_3_0_i1003);
	incr_sp_push_msg(3, "list__filter_map__ua10000");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	localcall(mercury__list__filter_map__ua10000_3_0,
		LABEL(mercury__list__filter_map__ua10000_3_0_i4),
		STATIC(mercury__list__filter_map__ua10000_3_0));
Define_label(mercury__list__filter_map__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__filter_map__ua10000_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r4 = (Integer) detstackvar(2);
	r1 = (Integer) r2;
	r2 = ((Integer) 1);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_semidet_closure);
	call_localret(ENTRY(do_call_semidet_closure),
		mercury__list__filter_map__ua10000_3_0_i7,
		STATIC(mercury__list__filter_map__ua10000_3_0));
	}
Define_label(mercury__list__filter_map__ua10000_3_0_i7);
	update_prof_current_proc(LABEL(mercury__list__filter_map__ua10000_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__filter_map__ua10000_3_0_i6);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__list__filter_map__ua10000_3_0_i6);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__list__filter_map__ua10000_3_0_i1003);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module11)
	init_entry(mercury__list__filter__ua10000_4_0);
	init_label(mercury__list__filter__ua10000_4_0_i4);
	init_label(mercury__list__filter__ua10000_4_0_i7);
	init_label(mercury__list__filter__ua10000_4_0_i6);
	init_label(mercury__list__filter__ua10000_4_0_i1004);
BEGIN_CODE

/* code for predicate 'list__filter__ua10000'/4 in mode 0 */
Define_static(mercury__list__filter__ua10000_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__filter__ua10000_4_0_i1004);
	incr_sp_push_msg(4, "list__filter__ua10000");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	localcall(mercury__list__filter__ua10000_4_0,
		LABEL(mercury__list__filter__ua10000_4_0_i4),
		STATIC(mercury__list__filter__ua10000_4_0));
Define_label(mercury__list__filter__ua10000_4_0_i4);
	update_prof_current_proc(LABEL(mercury__list__filter__ua10000_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	r4 = (Integer) detstackvar(2);
	r1 = (Integer) r3;
	r2 = ((Integer) 1);
	r3 = ((Integer) 0);
	{
	Declare_entry(do_call_semidet_closure);
	call_localret(ENTRY(do_call_semidet_closure),
		mercury__list__filter__ua10000_4_0_i7,
		STATIC(mercury__list__filter__ua10000_4_0));
	}
Define_label(mercury__list__filter__ua10000_4_0_i7);
	update_prof_current_proc(LABEL(mercury__list__filter__ua10000_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__filter__ua10000_4_0_i6);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__list__filter__ua10000_4_0_i6);
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__list__filter__ua10000_4_0_i1004);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module12)
	init_entry(mercury__list__filter__ua10000_3_0);
BEGIN_CODE

/* code for predicate 'list__filter__ua10000'/3 in mode 0 */
Define_static(mercury__list__filter__ua10000_3_0);
	tailcall(STATIC(mercury__list__filter__ua10000_4_0),
		STATIC(mercury__list__filter__ua10000_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module13)
	init_entry(mercury__list__foldr__ua10000_4_0);
	init_label(mercury__list__foldr__ua10000_4_0_i4);
	init_label(mercury__list__foldr__ua10000_4_0_i1002);
BEGIN_CODE

/* code for predicate 'list__foldr__ua10000'/4 in mode 0 */
Define_static(mercury__list__foldr__ua10000_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__foldr__ua10000_4_0_i1002);
	incr_sp_push_msg(3, "list__foldr__ua10000");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	localcall(mercury__list__foldr__ua10000_4_0,
		LABEL(mercury__list__foldr__ua10000_4_0_i4),
		STATIC(mercury__list__foldr__ua10000_4_0));
Define_label(mercury__list__foldr__ua10000_4_0_i4);
	update_prof_current_proc(LABEL(mercury__list__foldr__ua10000_4_0));
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = ((Integer) 2);
	r3 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(do_call_det_closure);
	tailcall(ENTRY(do_call_det_closure),
		STATIC(mercury__list__foldr__ua10000_4_0));
	}
Define_label(mercury__list__foldr__ua10000_4_0_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module14)
	init_entry(mercury__list__foldr__ua1_4_0);
	init_label(mercury__list__foldr__ua1_4_0_i1001);
	init_label(mercury__list__foldr__ua1_4_0_i4);
	init_label(mercury__list__foldr__ua1_4_0_i6);
	init_label(mercury__list__foldr__ua1_4_0_i1);
	init_label(mercury__list__foldr__ua1_4_0_i1000);
BEGIN_CODE

/* code for predicate 'list__foldr__ua1'/4 in mode 0 */
Define_static(mercury__list__foldr__ua1_4_0);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__foldr__ua1_4_0_i1001);
	r2 = (Integer) r3;
	r1 = TRUE;
	proceed();
Define_label(mercury__list__foldr__ua1_4_0_i1001);
	incr_sp_push_msg(3, "list__foldr__ua1");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	localcall(mercury__list__foldr__ua1_4_0,
		LABEL(mercury__list__foldr__ua1_4_0_i4),
		STATIC(mercury__list__foldr__ua1_4_0));
Define_label(mercury__list__foldr__ua1_4_0_i4);
	update_prof_current_proc(LABEL(mercury__list__foldr__ua1_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__foldr__ua1_4_0_i1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) r2;
	r1 = (Integer) detstackvar(1);
	r2 = ((Integer) 2);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_semidet_closure);
	call_localret(ENTRY(do_call_semidet_closure),
		mercury__list__foldr__ua1_4_0_i6,
		STATIC(mercury__list__foldr__ua1_4_0));
	}
Define_label(mercury__list__foldr__ua1_4_0_i6);
	update_prof_current_proc(LABEL(mercury__list__foldr__ua1_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__foldr__ua1_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__foldr__ua1_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__list__foldr__ua1_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module15)
	init_entry(mercury__list__foldl__ua10001_4_0);
	init_label(mercury__list__foldl__ua10001_4_0_i4);
	init_label(mercury__list__foldl__ua10001_4_0_i1002);
BEGIN_CODE

/* code for predicate 'list__foldl__ua10001'/4 in mode 0 */
Define_static(mercury__list__foldl__ua10001_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__foldl__ua10001_4_0_i1002);
	incr_sp_push_msg(3, "list__foldl__ua10001");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r5 = (Integer) r3;
	r2 = ((Integer) 2);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_det_closure);
	call_localret(ENTRY(do_call_det_closure),
		mercury__list__foldl__ua10001_4_0_i4,
		STATIC(mercury__list__foldl__ua10001_4_0));
	}
Define_label(mercury__list__foldl__ua10001_4_0_i4);
	update_prof_current_proc(LABEL(mercury__list__foldl__ua10001_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__list__foldl__ua10001_4_0,
		STATIC(mercury__list__foldl__ua10001_4_0));
Define_label(mercury__list__foldl__ua10001_4_0_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module16)
	init_entry(mercury__list__foldl__ua10000_4_0);
	init_label(mercury__list__foldl__ua10000_4_0_i4);
	init_label(mercury__list__foldl__ua10000_4_0_i1002);
BEGIN_CODE

/* code for predicate 'list__foldl__ua10000'/4 in mode 0 */
Define_static(mercury__list__foldl__ua10000_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__foldl__ua10000_4_0_i1002);
	incr_sp_push_msg(3, "list__foldl__ua10000");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r5 = (Integer) r3;
	r2 = ((Integer) 2);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_det_closure);
	call_localret(ENTRY(do_call_det_closure),
		mercury__list__foldl__ua10000_4_0_i4,
		STATIC(mercury__list__foldl__ua10000_4_0));
	}
Define_label(mercury__list__foldl__ua10000_4_0_i4);
	update_prof_current_proc(LABEL(mercury__list__foldl__ua10000_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__list__foldl__ua10000_4_0,
		STATIC(mercury__list__foldl__ua10000_4_0));
Define_label(mercury__list__foldl__ua10000_4_0_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module17)
	init_entry(mercury__list__foldl__ua2_4_0);
	init_label(mercury__list__foldl__ua2_4_0_i1001);
	init_label(mercury__list__foldl__ua2_4_0_i4);
	init_label(mercury__list__foldl__ua2_4_0_i6);
	init_label(mercury__list__foldl__ua2_4_0_i1);
	init_label(mercury__list__foldl__ua2_4_0_i1000);
BEGIN_CODE

/* code for predicate 'list__foldl__ua2'/4 in mode 0 */
Define_static(mercury__list__foldl__ua2_4_0);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__foldl__ua2_4_0_i1001);
	r2 = (Integer) r3;
	r1 = TRUE;
	proceed();
Define_label(mercury__list__foldl__ua2_4_0_i1001);
	incr_sp_push_msg(3, "list__foldl__ua2");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r5 = (Integer) r3;
	r2 = ((Integer) 2);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_semidet_closure);
	call_localret(ENTRY(do_call_semidet_closure),
		mercury__list__foldl__ua2_4_0_i4,
		STATIC(mercury__list__foldl__ua2_4_0));
	}
Define_label(mercury__list__foldl__ua2_4_0_i4);
	update_prof_current_proc(LABEL(mercury__list__foldl__ua2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__foldl__ua2_4_0_i1);
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	localcall(mercury__list__foldl__ua2_4_0,
		LABEL(mercury__list__foldl__ua2_4_0_i6),
		STATIC(mercury__list__foldl__ua2_4_0));
Define_label(mercury__list__foldl__ua2_4_0_i6);
	update_prof_current_proc(LABEL(mercury__list__foldl__ua2_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__foldl__ua2_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__foldl__ua2_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__list__foldl__ua2_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module18)
	init_entry(mercury__list__map__ua60002_3_0);
	init_label(mercury__list__map__ua60002_3_0_i2);
	init_label(mercury__list__map__ua60002_3_0_i3);
	init_label(mercury__list__map__ua60002_3_0_i4);
BEGIN_CODE

/* code for predicate 'list__map__ua60002'/3 in mode 0 */
Define_static(mercury__list__map__ua60002_3_0);
	{
	Declare_entry(do_fail);
	mkframe("list__map__ua60002/3", 3, ENTRY(do_fail));
	}
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__map__ua60002_3_0_i2);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	succeed();
Define_label(mercury__list__map__ua60002_3_0_i2);
	framevar(0) = (Integer) r1;
	framevar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = ((Integer) 1);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_nondet_closure);
	call_localret(ENTRY(do_call_nondet_closure),
		mercury__list__map__ua60002_3_0_i3,
		STATIC(mercury__list__map__ua60002_3_0));
	}
Define_label(mercury__list__map__ua60002_3_0_i3);
	update_prof_current_proc(LABEL(mercury__list__map__ua60002_3_0));
	framevar(2) = (Integer) r1;
	r1 = (Integer) framevar(0);
	r2 = (Integer) framevar(1);
	localcall(mercury__list__map__ua60002_3_0,
		LABEL(mercury__list__map__ua60002_3_0_i4),
		STATIC(mercury__list__map__ua60002_3_0));
Define_label(mercury__list__map__ua60002_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__map__ua60002_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) framevar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module19)
	init_entry(mercury__list__map__ua40003_3_0);
	init_label(mercury__list__map__ua40003_3_0_i2);
	init_label(mercury__list__map__ua40003_3_0_i3);
	init_label(mercury__list__map__ua40003_3_0_i4);
BEGIN_CODE

/* code for predicate 'list__map__ua40003'/3 in mode 0 */
Define_static(mercury__list__map__ua40003_3_0);
	{
	Declare_entry(do_fail);
	mkframe("list__map__ua40003/3", 3, ENTRY(do_fail));
	}
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__map__ua40003_3_0_i2);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	succeed();
Define_label(mercury__list__map__ua40003_3_0_i2);
	framevar(0) = (Integer) r1;
	framevar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = ((Integer) 1);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_nondet_closure);
	call_localret(ENTRY(do_call_nondet_closure),
		mercury__list__map__ua40003_3_0_i3,
		STATIC(mercury__list__map__ua40003_3_0));
	}
Define_label(mercury__list__map__ua40003_3_0_i3);
	update_prof_current_proc(LABEL(mercury__list__map__ua40003_3_0));
	framevar(2) = (Integer) r1;
	r1 = (Integer) framevar(0);
	r2 = (Integer) framevar(1);
	localcall(mercury__list__map__ua40003_3_0,
		LABEL(mercury__list__map__ua40003_3_0_i4),
		STATIC(mercury__list__map__ua40003_3_0));
Define_label(mercury__list__map__ua40003_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__map__ua40003_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) framevar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module20)
	init_entry(mercury__list__map__ua10000_3_0);
	init_label(mercury__list__map__ua10000_3_0_i4);
	init_label(mercury__list__map__ua10000_3_0_i5);
	init_label(mercury__list__map__ua10000_3_0_i1002);
BEGIN_CODE

/* code for predicate 'list__map__ua10000'/3 in mode 0 */
Define_static(mercury__list__map__ua10000_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__map__ua10000_3_0_i1002);
	incr_sp_push_msg(3, "list__map__ua10000");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = ((Integer) 1);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_det_closure);
	call_localret(ENTRY(do_call_det_closure),
		mercury__list__map__ua10000_3_0_i4,
		STATIC(mercury__list__map__ua10000_3_0));
	}
Define_label(mercury__list__map__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__map__ua10000_3_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	localcall(mercury__list__map__ua10000_3_0,
		LABEL(mercury__list__map__ua10000_3_0_i5),
		STATIC(mercury__list__map__ua10000_3_0));
Define_label(mercury__list__map__ua10000_3_0_i5);
	update_prof_current_proc(LABEL(mercury__list__map__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__list__map__ua10000_3_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module21)
	init_entry(mercury__list__map__ua1_3_0);
	init_label(mercury__list__map__ua1_3_0_i1001);
	init_label(mercury__list__map__ua1_3_0_i4);
	init_label(mercury__list__map__ua1_3_0_i6);
	init_label(mercury__list__map__ua1_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__map__ua1'/3 in mode 0 */
Define_static(mercury__list__map__ua1_3_0);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__map__ua1_3_0_i1001);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r1 = TRUE;
	proceed();
Define_label(mercury__list__map__ua1_3_0_i1001);
	incr_sp_push_msg(3, "list__map__ua1");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = ((Integer) 1);
	r3 = ((Integer) 1);
	{
	Declare_entry(do_call_semidet_closure);
	call_localret(ENTRY(do_call_semidet_closure),
		mercury__list__map__ua1_3_0_i4,
		STATIC(mercury__list__map__ua1_3_0));
	}
Define_label(mercury__list__map__ua1_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__map__ua1_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__map__ua1_3_0_i1);
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	localcall(mercury__list__map__ua1_3_0,
		LABEL(mercury__list__map__ua1_3_0_i6),
		STATIC(mercury__list__map__ua1_3_0));
Define_label(mercury__list__map__ua1_3_0_i6);
	update_prof_current_proc(LABEL(mercury__list__map__ua1_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__map__ua1_3_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__list__map__ua1_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module22)
	init_entry(mercury__list__last__ua0_2_0);
	init_label(mercury__list__last__ua0_2_0_i1007);
	init_label(mercury__list__last__ua0_2_0_i5);
	init_label(mercury__list__last__ua0_2_0_i1004);
	init_label(mercury__list__last__ua0_2_0_i1006);
BEGIN_CODE

/* code for predicate 'list__last__ua0'/2 in mode 0 */
Define_static(mercury__list__last__ua0_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__last__ua0_2_0_i1004);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 1)) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__last__ua0_2_0_i1007);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = TRUE;
	proceed();
Define_label(mercury__list__last__ua0_2_0_i1007);
	incr_sp_push_msg(1, "list__last__ua0");
	detstackvar(1) = (Integer) succip;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localcall(mercury__list__last__ua0_2_0,
		LABEL(mercury__list__last__ua0_2_0_i5),
		STATIC(mercury__list__last__ua0_2_0));
Define_label(mercury__list__last__ua0_2_0_i5);
	update_prof_current_proc(LABEL(mercury__list__last__ua0_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__last__ua0_2_0_i1006);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__last__ua0_2_0_i1004);
	r1 = FALSE;
	proceed();
Define_label(mercury__list__last__ua0_2_0_i1006);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module23)
	init_entry(mercury__list__chunk__ua10000_3_0);
BEGIN_CODE

/* code for predicate 'list__chunk__ua10000'/3 in mode 0 */
Define_static(mercury__list__chunk__ua10000_3_0);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) r2;
	tailcall(STATIC(mercury__list__chunk_2__ua10000_5_0),
		STATIC(mercury__list__chunk__ua10000_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module24)
	init_entry(mercury__list__condense__ua10000_2_0);
	init_label(mercury__list__condense__ua10000_2_0_i4);
	init_label(mercury__list__condense__ua10000_2_0_i1002);
BEGIN_CODE

/* code for predicate 'list__condense__ua10000'/2 in mode 0 */
Define_static(mercury__list__condense__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__condense__ua10000_2_0_i1002);
	incr_sp_push_msg(2, "list__condense__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localcall(mercury__list__condense__ua10000_2_0,
		LABEL(mercury__list__condense__ua10000_2_0_i4),
		STATIC(mercury__list__condense__ua10000_2_0));
Define_label(mercury__list__condense__ua10000_2_0_i4);
	update_prof_current_proc(LABEL(mercury__list__condense__ua10000_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__list__append__ua10001_3_0),
		STATIC(mercury__list__condense__ua10000_2_0));
Define_label(mercury__list__condense__ua10000_2_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module25)
	init_entry(mercury__list__duplicate__ua10000_3_0);
	init_label(mercury__list__duplicate__ua10000_3_0_i4);
	init_label(mercury__list__duplicate__ua10000_3_0_i1002);
BEGIN_CODE

/* code for predicate 'list__duplicate__ua10000'/3 in mode 0 */
Define_static(mercury__list__duplicate__ua10000_3_0);
	if (((Integer) r1 <= ((Integer) 0)))
		GOTO_LABEL(mercury__list__duplicate__ua10000_3_0_i1002);
	incr_sp_push_msg(2, "list__duplicate__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r1 = ((Integer) r1 - ((Integer) 1));
	localcall(mercury__list__duplicate__ua10000_3_0,
		LABEL(mercury__list__duplicate__ua10000_3_0_i4),
		STATIC(mercury__list__duplicate__ua10000_3_0));
Define_label(mercury__list__duplicate__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__duplicate__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__list__duplicate__ua10000_3_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module26)
	init_entry(mercury__list__zip__ua10000_3_0);
	init_label(mercury__list__zip__ua10000_3_0_i4);
	init_label(mercury__list__zip__ua10000_3_0_i1002);
BEGIN_CODE

/* code for predicate 'list__zip__ua10000'/3 in mode 0 */
Define_static(mercury__list__zip__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__zip__ua10000_3_0_i1002);
	incr_sp_push_msg(2, "list__zip__ua10000");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	call_localret(STATIC(mercury__list__zip2__ua10000_3_0),
		mercury__list__zip__ua10000_3_0_i4,
		STATIC(mercury__list__zip__ua10000_3_0));
Define_label(mercury__list__zip__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__zip__ua10000_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__list__zip__ua10000_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module27)
	init_entry(mercury__list__index1_det__ua10000_3_0);
BEGIN_CODE

/* code for predicate 'list__index1_det__ua10000'/3 in mode 0 */
Define_static(mercury__list__index1_det__ua10000_3_0);
	r2 = ((Integer) r2 - ((Integer) 1));
	tailcall(STATIC(mercury__list__index0_det__ua10000_3_0),
		STATIC(mercury__list__index1_det__ua10000_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module28)
	init_entry(mercury__list__index0_det__ua10000_3_0);
	init_label(mercury__list__index0_det__ua10000_3_0_i4);
	init_label(mercury__list__index0_det__ua10000_3_0_i1000);
BEGIN_CODE

/* code for predicate 'list__index0_det__ua10000'/3 in mode 0 */
Define_static(mercury__list__index0_det__ua10000_3_0);
	incr_sp_push_msg(1, "list__index0_det__ua10000");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__list__index0__ua0_3_0),
		mercury__list__index0_det__ua10000_3_0_i4,
		STATIC(mercury__list__index0_det__ua10000_3_0));
Define_label(mercury__list__index0_det__ua10000_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__index0_det__ua10000_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__index0_det__ua10000_3_0_i1000);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__list__index0_det__ua10000_3_0_i1000);
	r1 = string_const("list__index: index out of range", 31);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__list__index0_det__ua10000_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__list_module29)
	init_entry(mercury__list__index1__ua0_3_0);
	init_label(mercury__list__index1__ua0_3_0_i2);
	init_label(mercury__list__index1__ua0_3_0_i1000);
BEGIN_CODE

/* code for predicate 'list__index1__ua0'/3 in mode 0 */
Define_static(mercury__list__index1__ua0_3_0);
	r2 = ((Integer) r2 - ((Integer) 1));
	incr_sp_push_msg(1, "list__index1__ua0");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__list__index0__ua0_3_0),
		mercury__list__index1__ua0_3_0_i2,
		STATIC(mercury__list__index1__ua0_3_0));
Define_label(mercury__list__index1__ua0_3_0_i2);
	update_prof_current_proc(LABEL(mercury__list__index1__ua0_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__index1__ua0_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__index1__ua0_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module30)
	init_entry(mercury__list__index0__ua0_3_0);
	init_label(mercury__list__index0__ua0_3_0_i1007);
	init_label(mercury__list__index0__ua0_3_0_i6);
	init_label(mercury__list__index0__ua0_3_0_i1004);
	init_label(mercury__list__index0__ua0_3_0_i1006);
BEGIN_CODE

/* code for predicate 'list__index0__ua0'/3 in mode 0 */
Define_static(mercury__list__index0__ua0_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__index0__ua0_3_0_i1004);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r2 != ((Integer) 0)))
		GOTO_LABEL(mercury__list__index0__ua0_3_0_i1007);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = TRUE;
	proceed();
Define_label(mercury__list__index0__ua0_3_0_i1007);
	incr_sp_push_msg(1, "list__index0__ua0");
	detstackvar(1) = (Integer) succip;
	r1 = (Integer) r3;
	r2 = ((Integer) r2 - ((Integer) 1));
	localcall(mercury__list__index0__ua0_3_0,
		LABEL(mercury__list__index0__ua0_3_0_i6),
		STATIC(mercury__list__index0__ua0_3_0));
Define_label(mercury__list__index0__ua0_3_0_i6);
	update_prof_current_proc(LABEL(mercury__list__index0__ua0_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__index0__ua0_3_0_i1006);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__index0__ua0_3_0_i1004);
	r1 = FALSE;
	proceed();
Define_label(mercury__list__index0__ua0_3_0_i1006);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module31)
	init_entry(mercury__list__perm__ua40000_2_0);
	init_label(mercury__list__perm__ua40000_2_0_i2);
	init_label(mercury__list__perm__ua40000_2_0_i3);
	init_label(mercury__list__perm__ua40000_2_0_i1);
BEGIN_CODE

/* code for predicate 'list__perm__ua40000'/2 in mode 0 */
Define_static(mercury__list__perm__ua40000_2_0);
	{
	Declare_entry(do_fail);
	mkframe("list__perm__ua40000/2", 1, ENTRY(do_fail));
	}
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__perm__ua40000_2_0_i2);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	succeed();
Define_label(mercury__list__perm__ua40000_2_0_i2);
	framevar(0) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	localcall(mercury__list__perm__ua40000_2_0,
		LABEL(mercury__list__perm__ua40000_2_0_i3),
		STATIC(mercury__list__perm__ua40000_2_0));
Define_label(mercury__list__perm__ua40000_2_0_i3);
	update_prof_current_proc(LABEL(mercury__list__perm__ua40000_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) framevar(0);
	call_localret(STATIC(mercury__list__insert__ua60003_3_0),
		mercury__list__perm__ua40000_2_0_i1,
		STATIC(mercury__list__perm__ua40000_2_0));
Define_label(mercury__list__perm__ua40000_2_0_i1);
	update_prof_current_proc(LABEL(mercury__list__perm__ua40000_2_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module32)
	init_entry(mercury__list__reverse__ua10000_2_0);
BEGIN_CODE

/* code for predicate 'list__reverse__ua10000'/2 in mode 0 */
Define_static(mercury__list__reverse__ua10000_2_0);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	tailcall(STATIC(mercury__list__reverse_2__ua10000_3_0),
		STATIC(mercury__list__reverse__ua10000_2_0));
END_MODULE

BEGIN_MODULE(mercury__list_module33)
	init_entry(mercury__list__delete__ua60003_3_0);
	init_label(mercury__list__delete__ua60003_3_0_i3);
	init_label(mercury__list__delete__ua60003_3_0_i5);
BEGIN_CODE

/* code for predicate 'list__delete__ua60003'/3 in mode 0 */
Define_static(mercury__list__delete__ua60003_3_0);
	mkframe("list__delete__ua60003/3", 2, LABEL(mercury__list__delete__ua60003_3_0_i3));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	framevar(0) = (Integer) r3;
	framevar(1) = (Integer) r2;
	succeed();
Define_label(mercury__list__delete__ua60003_3_0_i3);
	update_prof_current_proc(LABEL(mercury__list__delete__ua60003_3_0));
	{
	Declare_entry(do_fail);
	LVALUE_CAST(Word,bt_redoip((Integer) maxfr)) = (Integer) ENTRY(do_fail);
	}
	r3 = (Integer) framevar(1);
	{
	Declare_entry(do_fail);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO(ENTRY(do_fail));
	}
	r1 = (Integer) framevar(0);
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	framevar(0) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	localcall(mercury__list__delete__ua60003_3_0,
		LABEL(mercury__list__delete__ua60003_3_0_i5),
		STATIC(mercury__list__delete__ua60003_3_0));
Define_label(mercury__list__delete__ua60003_3_0_i5);
	update_prof_current_proc(LABEL(mercury__list__delete__ua60003_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) framevar(0);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module34)
	init_entry(mercury__list__delete__ua40002_3_0);
	init_label(mercury__list__delete__ua40002_3_0_i4);
	init_label(mercury__list__delete__ua40002_3_0_i5);
BEGIN_CODE

/* code for predicate 'list__delete__ua40002'/3 in mode 0 */
Define_static(mercury__list__delete__ua40002_3_0);
	{
	Declare_entry(do_redo);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO(ENTRY(do_redo));
	}
	mkframe("list__delete__ua40002/3", 2, LABEL(mercury__list__delete__ua40002_3_0_i4));
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	framevar(0) = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	framevar(1) = (Integer) r1;
	succeed();
Define_label(mercury__list__delete__ua40002_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__delete__ua40002_3_0));
	{
	Declare_entry(do_fail);
	LVALUE_CAST(Word,bt_redoip((Integer) maxfr)) = (Integer) ENTRY(do_fail);
	}
	r1 = (Integer) framevar(0);
	localcall(mercury__list__delete__ua40002_3_0,
		LABEL(mercury__list__delete__ua40002_3_0_i5),
		STATIC(mercury__list__delete__ua40002_3_0));
Define_label(mercury__list__delete__ua40002_3_0_i5);
	update_prof_current_proc(LABEL(mercury__list__delete__ua40002_3_0));
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) framevar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r3;
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module35)
	init_entry(mercury__list__insert__ua60003_3_0);
	init_label(mercury__list__insert__ua60003_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__insert__ua60003'/3 in mode 0 */
Define_static(mercury__list__insert__ua60003_3_0);
	{
	Declare_entry(do_fail);
	mkframe("list__insert__ua60003/3", 1, ENTRY(do_fail));
	}
	call_localret(STATIC(mercury__list__delete__ua60003_3_0),
		mercury__list__insert__ua60003_3_0_i1,
		STATIC(mercury__list__insert__ua60003_3_0));
Define_label(mercury__list__insert__ua60003_3_0_i1);
	update_prof_current_proc(LABEL(mercury__list__insert__ua60003_3_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module36)
	init_entry(mercury__list__insert__ua40002_3_0);
	init_label(mercury__list__insert__ua40002_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__insert__ua40002'/3 in mode 0 */
Define_static(mercury__list__insert__ua40002_3_0);
	{
	Declare_entry(do_fail);
	mkframe("list__insert__ua40002/3", 1, ENTRY(do_fail));
	}
	call_localret(STATIC(mercury__list__delete__ua40002_3_0),
		mercury__list__insert__ua40002_3_0_i1,
		STATIC(mercury__list__insert__ua40002_3_0));
Define_label(mercury__list__insert__ua40002_3_0_i1);
	update_prof_current_proc(LABEL(mercury__list__insert__ua40002_3_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module37)
	init_entry(mercury__list__drop__ua0_3_0);
	init_label(mercury__list__drop__ua0_3_0_i5);
	init_label(mercury__list__drop__ua0_3_0_i1003);
	init_label(mercury__list__drop__ua0_3_0_i1004);
	init_label(mercury__list__drop__ua0_3_0_i1007);
BEGIN_CODE

/* code for predicate 'list__drop__ua0'/3 in mode 0 */
Define_static(mercury__list__drop__ua0_3_0);
	if (((Integer) r1 <= ((Integer) 0)))
		GOTO_LABEL(mercury__list__drop__ua0_3_0_i1003);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__drop__ua0_3_0_i1004);
	r1 = ((Integer) r1 - ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	incr_sp_push_msg(1, "list__drop__ua0");
	detstackvar(1) = (Integer) succip;
	localcall(mercury__list__drop__ua0_3_0,
		LABEL(mercury__list__drop__ua0_3_0_i5),
		STATIC(mercury__list__drop__ua0_3_0));
Define_label(mercury__list__drop__ua0_3_0_i5);
	update_prof_current_proc(LABEL(mercury__list__drop__ua0_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__drop__ua0_3_0_i1007);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__drop__ua0_3_0_i1003);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__drop__ua0_3_0_i1004);
	r1 = FALSE;
	proceed();
Define_label(mercury__list__drop__ua0_3_0_i1007);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module38)
	init_entry(mercury__list__take__ua0_3_0);
	init_label(mercury__list__take__ua0_3_0_i5);
	init_label(mercury__list__take__ua0_3_0_i1004);
	init_label(mercury__list__take__ua0_3_0_i1005);
	init_label(mercury__list__take__ua0_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__take__ua0'/3 in mode 0 */
Define_static(mercury__list__take__ua0_3_0);
	if (((Integer) r1 <= ((Integer) 0)))
		GOTO_LABEL(mercury__list__take__ua0_3_0_i1004);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__take__ua0_3_0_i1005);
	incr_sp_push_msg(2, "list__take__ua0");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r1 = ((Integer) r1 - ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	localcall(mercury__list__take__ua0_3_0,
		LABEL(mercury__list__take__ua0_3_0_i5),
		STATIC(mercury__list__take__ua0_3_0));
Define_label(mercury__list__take__ua0_3_0_i5);
	update_prof_current_proc(LABEL(mercury__list__take__ua0_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__take__ua0_3_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__list__take__ua0_3_0_i1004);
	r1 = TRUE;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__list__take__ua0_3_0_i1005);
	r1 = FALSE;
	proceed();
Define_label(mercury__list__take__ua0_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module39)
	init_entry(mercury__list__split_list__ua0_4_0);
	init_label(mercury__list__split_list__ua0_4_0_i1001);
	init_label(mercury__list__split_list__ua0_4_0_i6);
	init_label(mercury__list__split_list__ua0_4_0_i1);
BEGIN_CODE

/* code for predicate 'list__split_list__ua0'/4 in mode 0 */
Define_static(mercury__list__split_list__ua0_4_0);
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__list__split_list__ua0_4_0_i1001);
	r3 = (Integer) r2;
	r1 = TRUE;
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__list__split_list__ua0_4_0_i1001);
	incr_sp_push_msg(2, "list__split_list__ua0");
	detstackvar(2) = (Integer) succip;
	if (((Integer) r1 <= ((Integer) 0)))
		GOTO_LABEL(mercury__list__split_list__ua0_4_0_i1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__split_list__ua0_4_0_i1);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r1 = ((Integer) r1 - ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	localcall(mercury__list__split_list__ua0_4_0,
		LABEL(mercury__list__split_list__ua0_4_0_i6),
		STATIC(mercury__list__split_list__ua0_4_0));
Define_label(mercury__list__split_list__ua0_4_0_i6);
	update_prof_current_proc(LABEL(mercury__list__split_list__ua0_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__split_list__ua0_4_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__list__split_list__ua0_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module40)
	init_entry(mercury__list__same_length__ua10001_2_0);
	init_label(mercury__list__same_length__ua10001_2_0_i3);
	init_label(mercury__list__same_length__ua10001_2_0_i4);
	init_label(mercury__list__same_length__ua10001_2_0_i1);
BEGIN_CODE

/* code for predicate 'list__same_length__ua10001'/2 in mode 0 */
Define_static(mercury__list__same_length__ua10001_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__same_length__ua10001_2_0_i1);
	r3 = ((Integer) 0);
Define_label(mercury__list__same_length__ua10001_2_0_i3);
	while (1) {
	r3 = ((Integer) r3 + ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	break; } /* end while */
Define_label(mercury__list__same_length__ua10001_2_0_i4);
	while (1) {
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r3 = ((Integer) r3 - ((Integer) 1));
	if (((Integer) r3 > ((Integer) 0)))
		continue;
	proceed();
	break; } /* end while */
Define_label(mercury__list__same_length__ua10001_2_0_i1);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module41)
	init_entry(mercury__list__same_length__ua10000_2_0);
	init_label(mercury__list__same_length__ua10000_2_0_i3);
	init_label(mercury__list__same_length__ua10000_2_0_i4);
	init_label(mercury__list__same_length__ua10000_2_0_i1);
BEGIN_CODE

/* code for predicate 'list__same_length__ua10000'/2 in mode 0 */
Define_static(mercury__list__same_length__ua10000_2_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__same_length__ua10000_2_0_i1);
	r3 = ((Integer) 0);
Define_label(mercury__list__same_length__ua10000_2_0_i3);
	while (1) {
	r3 = ((Integer) r3 + ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	break; } /* end while */
Define_label(mercury__list__same_length__ua10000_2_0_i4);
	while (1) {
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r3 = ((Integer) r3 - ((Integer) 1));
	if (((Integer) r3 > ((Integer) 0)))
		continue;
	proceed();
	break; } /* end while */
Define_label(mercury__list__same_length__ua10000_2_0_i1);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module42)
	init_entry(mercury__list__same_length__ua2_2_0);
	init_label(mercury__list__same_length__ua2_2_0_i1008);
	init_label(mercury__list__same_length__ua2_2_0_i1005);
	init_label(mercury__list__same_length__ua2_2_0_i1007);
BEGIN_CODE

/* code for predicate 'list__same_length__ua2'/2 in mode 0 */
Define_static(mercury__list__same_length__ua2_2_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__same_length__ua2_2_0_i1008);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__same_length__ua2_2_0_i1005);
	r1 = FALSE;
	proceed();
Define_label(mercury__list__same_length__ua2_2_0_i1008);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__same_length__ua2_2_0_i1007);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	localtailcall(mercury__list__same_length__ua2_2_0,
		STATIC(mercury__list__same_length__ua2_2_0));
Define_label(mercury__list__same_length__ua2_2_0_i1005);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__same_length__ua2_2_0_i1007);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module43)
	init_entry(mercury__list__length__ua10000_2_0);
BEGIN_CODE

/* code for predicate 'list__length__ua10000'/2 in mode 0 */
Define_static(mercury__list__length__ua10000_2_0);
	r2 = ((Integer) 0);
	tailcall(STATIC(mercury__list__length_2__ua10000_3_0),
		STATIC(mercury__list__length__ua10000_2_0));
END_MODULE

BEGIN_MODULE(mercury__list_module44)
	init_entry(mercury__list__member__ua40000_3_0);
	init_label(mercury__list__member__ua40000_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__member__ua40000'/3 in mode 0 */
Define_static(mercury__list__member__ua40000_3_0);
	{
	Declare_entry(do_fail);
	mkframe("list__member__ua40000/3", 1, ENTRY(do_fail));
	}
	call_localret(STATIC(mercury__list__append__ua60004_3_0),
		mercury__list__member__ua40000_3_0_i1,
		STATIC(mercury__list__member__ua40000_3_0));
Define_label(mercury__list__member__ua40000_3_0_i1);
	update_prof_current_proc(LABEL(mercury__list__member__ua40000_3_0));
	{
	Declare_entry(do_redo);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO(ENTRY(do_redo));
	}
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module45)
	init_entry(mercury__list__member__ua40001_2_0);
	init_label(mercury__list__member__ua40001_2_0_i4);
	init_label(mercury__list__member__ua40001_2_0_i2);
BEGIN_CODE

/* code for predicate 'list__member__ua40001'/2 in mode 0 */
Define_static(mercury__list__member__ua40001_2_0);
	{
	Declare_entry(do_redo);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO(ENTRY(do_redo));
	}
	mkframe("list__member__ua40001/2", 1, LABEL(mercury__list__member__ua40001_2_0_i4));
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	framevar(0) = (Integer) r2;
	succeed();
Define_label(mercury__list__member__ua40001_2_0_i4);
	update_prof_current_proc(LABEL(mercury__list__member__ua40001_2_0));
	{
	Declare_entry(do_fail);
	LVALUE_CAST(Word,bt_redoip((Integer) maxfr)) = (Integer) ENTRY(do_fail);
	}
	r1 = (Integer) framevar(0);
	localcall(mercury__list__member__ua40001_2_0,
		LABEL(mercury__list__member__ua40001_2_0_i2),
		STATIC(mercury__list__member__ua40001_2_0));
Define_label(mercury__list__member__ua40001_2_0_i2);
	update_prof_current_proc(LABEL(mercury__list__member__ua40001_2_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module46)
	init_entry(mercury__list__append__ua60004_3_0);
	init_label(mercury__list__append__ua60004_3_0_i3);
	init_label(mercury__list__append__ua60004_3_0_i5);
BEGIN_CODE

/* code for predicate 'list__append__ua60004'/3 in mode 0 */
Define_static(mercury__list__append__ua60004_3_0);
	mkframe("list__append__ua60004/3", 1, LABEL(mercury__list__append__ua60004_3_0_i3));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	framevar(0) = (Integer) r2;
	succeed();
Define_label(mercury__list__append__ua60004_3_0_i3);
	update_prof_current_proc(LABEL(mercury__list__append__ua60004_3_0));
	{
	Declare_entry(do_fail);
	LVALUE_CAST(Word,bt_redoip((Integer) maxfr)) = (Integer) ENTRY(do_fail);
	}
	r2 = (Integer) framevar(0);
	{
	Declare_entry(do_fail);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO(ENTRY(do_fail));
	}
	r1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	framevar(0) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	localcall(mercury__list__append__ua60004_3_0,
		LABEL(mercury__list__append__ua60004_3_0_i5),
		STATIC(mercury__list__append__ua60004_3_0));
Define_label(mercury__list__append__ua60004_3_0_i5);
	update_prof_current_proc(LABEL(mercury__list__append__ua60004_3_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) framevar(0);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module47)
	init_entry(mercury__list__append__ua10001_3_0);
	init_label(mercury__list__append__ua10001_3_0_i3);
	init_label(mercury__list__append__ua10001_3_0_i4);
	init_label(mercury__list__append__ua10001_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__append__ua10001'/3 in mode 0 */
Define_static(mercury__list__append__ua10001_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__append__ua10001_3_0_i1);
	r3 = (Integer) sp;
Define_label(mercury__list__append__ua10001_3_0_i3);
	while (1) {
	incr_sp_push_msg(1, "list__append__ua10001");
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = (Integer) r2;
	break; } /* end while */
Define_label(mercury__list__append__ua10001_3_0_i4);
	while (1) {
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	decr_sp_pop_msg(1);
	if (((Integer) sp > (Integer) r3))
		continue;
	proceed();
	break; } /* end while */
Define_label(mercury__list__append__ua10001_3_0_i1);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module48)
	init_entry(mercury__list__append__ua10000_3_0);
	init_label(mercury__list__append__ua10000_3_0_i3);
	init_label(mercury__list__append__ua10000_3_0_i4);
	init_label(mercury__list__append__ua10000_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__append__ua10000'/3 in mode 0 */
Define_static(mercury__list__append__ua10000_3_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__append__ua10000_3_0_i1);
	r3 = (Integer) sp;
Define_label(mercury__list__append__ua10000_3_0_i3);
	while (1) {
	incr_sp_push_msg(1, "list__append__ua10000");
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 1));
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		continue;
	r1 = (Integer) r2;
	break; } /* end while */
Define_label(mercury__list__append__ua10000_3_0_i4);
	while (1) {
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	decr_sp_pop_msg(1);
	if (((Integer) sp > (Integer) r3))
		continue;
	proceed();
	break; } /* end while */
Define_label(mercury__list__append__ua10000_3_0_i1);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module49)
	init_entry(mercury__list__append_3_2);
	init_label(mercury__list__append_3_2_i1001);
	init_label(mercury__list__append_3_2_i7);
	init_label(mercury__list__append_3_2_i1);
BEGIN_CODE

/* code for predicate 'list__append'/3 in mode 2 */
Define_entry(mercury__list__append_3_2);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__append_3_2_i1001);
	r2 = (Integer) r4;
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury__list__append_3_2));
	}
Define_label(mercury__list__append_3_2_i1001);
	incr_sp_push_msg(5, "list__append");
	detstackvar(5) = (Integer) succip;
	if (((Integer) r4 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__append_3_2_i1);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r4, ((Integer) 1));
	detstackvar(4) = (Integer) r1;
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__append_3_2_i7,
		ENTRY(mercury__list__append_3_2));
	}
Define_label(mercury__list__append_3_2_i7);
	update_prof_current_proc(LABEL(mercury__list__append_3_2));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__append_3_2_i1);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__list__append_3_2,
		ENTRY(mercury__list__append_3_2));
Define_label(mercury__list__append_3_2_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module50)
	init_entry(mercury__list__append_3_3);
	init_label(mercury__list__append_3_3_i1001);
	init_label(mercury__list__append_3_3_i5);
	init_label(mercury__list__append_3_3_i7);
	init_label(mercury__list__append_3_3_i1);
	init_label(mercury__list__append_3_3_i1000);
BEGIN_CODE

/* code for predicate 'list__append'/3 in mode 3 */
Define_entry(mercury__list__append_3_3);
	if (((Integer) r2 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__append_3_3_i1001);
	r2 = (Integer) r3;
	r1 = TRUE;
	proceed();
Define_label(mercury__list__append_3_3_i1001);
	incr_sp_push_msg(4, "list__append");
	detstackvar(4) = (Integer) succip;
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__append_3_3_i1);
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__append_3_3_i5,
		ENTRY(mercury__list__append_3_3));
	}
Define_label(mercury__list__append_3_3_i5);
	update_prof_current_proc(LABEL(mercury__list__append_3_3));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__append_3_3_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	localcall(mercury__list__append_3_3,
		LABEL(mercury__list__append_3_3_i7),
		ENTRY(mercury__list__append_3_3));
Define_label(mercury__list__append_3_3_i7);
	update_prof_current_proc(LABEL(mercury__list__append_3_3));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__append_3_3_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__append_3_3_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__list__append_3_3_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module51)
	init_entry(mercury__list__append_3_0);
BEGIN_CODE

/* code for predicate 'list__append'/3 in mode 0 */
Define_entry(mercury__list__append_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__list__append__ua10000_3_0),
		ENTRY(mercury__list__append_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module52)
	init_entry(mercury__list__append_3_1);
BEGIN_CODE

/* code for predicate 'list__append'/3 in mode 1 */
Define_entry(mercury__list__append_3_1);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__list__append__ua10001_3_0),
		ENTRY(mercury__list__append_3_1));
END_MODULE

BEGIN_MODULE(mercury__list_module53)
	init_entry(mercury__list__append_3_4);
	init_label(mercury__list__append_3_4_i1);
BEGIN_CODE

/* code for predicate 'list__append'/3 in mode 4 */
Define_entry(mercury__list__append_3_4);
	{
	Declare_entry(do_fail);
	mkframe("list__append/3", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__append__ua60004_3_0),
		mercury__list__append_3_4_i1,
		ENTRY(mercury__list__append_3_4));
Define_label(mercury__list__append_3_4_i1);
	update_prof_current_proc(LABEL(mercury__list__append_3_4));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module54)
	init_entry(mercury__list__remove_suffix_3_0);
	init_label(mercury__list__remove_suffix_3_0_i2);
	init_label(mercury__list__remove_suffix_3_0_i3);
	init_label(mercury__list__remove_suffix_3_0_i4);
	init_label(mercury__list__remove_suffix_3_0_i6);
	init_label(mercury__list__remove_suffix_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__remove_suffix'/3 in mode 0 */
Define_entry(mercury__list__remove_suffix_3_0);
	incr_sp_push_msg(5, "list__remove_suffix");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__length__ua10000_2_0),
		mercury__list__remove_suffix_3_0_i2,
		ENTRY(mercury__list__remove_suffix_3_0));
Define_label(mercury__list__remove_suffix_3_0_i2);
	update_prof_current_proc(LABEL(mercury__list__remove_suffix_3_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__list__length__ua10000_2_0),
		mercury__list__remove_suffix_3_0_i3,
		ENTRY(mercury__list__remove_suffix_3_0));
Define_label(mercury__list__remove_suffix_3_0_i3);
	update_prof_current_proc(LABEL(mercury__list__remove_suffix_3_0));
	r1 = ((Integer) detstackvar(3) - (Integer) r1);
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__list__split_list__ua0_4_0),
		mercury__list__remove_suffix_3_0_i4,
		ENTRY(mercury__list__remove_suffix_3_0));
Define_label(mercury__list__remove_suffix_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__remove_suffix_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__remove_suffix_3_0_i1);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		mercury__list__remove_suffix_3_0_i6,
		ENTRY(mercury__list__remove_suffix_3_0));
	}
Define_label(mercury__list__remove_suffix_3_0_i6);
	update_prof_current_proc(LABEL(mercury__list__remove_suffix_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__remove_suffix_3_0_i1);
	r2 = (Integer) detstackvar(1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__list__remove_suffix_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module55)
	init_entry(mercury__list__merge_3_0);
	init_label(mercury__list__merge_3_0_i10);
	init_label(mercury__list__merge_3_0_i11);
	init_label(mercury__list__merge_3_0_i9);
	init_label(mercury__list__merge_3_0_i12);
	init_label(mercury__list__merge_3_0_i1006);
	init_label(mercury__list__merge_3_0_i1005);
BEGIN_CODE

/* code for predicate 'list__merge'/3 in mode 0 */
Define_entry(mercury__list__merge_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__merge_3_0_i1005);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__merge_3_0_i1006);
	incr_sp_push_msg(8, "list__merge");
	detstackvar(8) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(3) = (Integer) r2;
	detstackvar(5) = (Integer) r3;
	detstackvar(7) = (Integer) r1;
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__list__merge_3_0_i10,
		ENTRY(mercury__list__merge_3_0));
	}
Define_label(mercury__list__merge_3_0_i10);
	update_prof_current_proc(LABEL(mercury__list__merge_3_0));
	if ((((Integer) 1) != (Integer) r1))
		GOTO_LABEL(mercury__list__merge_3_0_i9);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(2);
	localcall(mercury__list__merge_3_0,
		LABEL(mercury__list__merge_3_0_i11),
		ENTRY(mercury__list__merge_3_0));
Define_label(mercury__list__merge_3_0_i11);
	update_prof_current_proc(LABEL(mercury__list__merge_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__list__merge_3_0_i9);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(6);
	localcall(mercury__list__merge_3_0,
		LABEL(mercury__list__merge_3_0_i12),
		ENTRY(mercury__list__merge_3_0));
Define_label(mercury__list__merge_3_0_i12);
	update_prof_current_proc(LABEL(mercury__list__merge_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__list__merge_3_0_i1006);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__list__merge_3_0_i1005);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module56)
	init_entry(mercury__list__merge_and_remove_dups_3_0);
	init_label(mercury__list__merge_and_remove_dups_3_0_i8);
	init_label(mercury__list__merge_and_remove_dups_3_0_i12);
	init_label(mercury__list__merge_and_remove_dups_3_0_i9);
	init_label(mercury__list__merge_and_remove_dups_3_0_i16);
	init_label(mercury__list__merge_and_remove_dups_3_0_i13);
	init_label(mercury__list__merge_and_remove_dups_3_0_i1007);
	init_label(mercury__list__merge_and_remove_dups_3_0_i1006);
BEGIN_CODE

/* code for predicate 'list__merge_and_remove_dups'/3 in mode 0 */
Define_entry(mercury__list__merge_and_remove_dups_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__merge_and_remove_dups_3_0_i1006);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__merge_and_remove_dups_3_0_i1007);
	incr_sp_push_msg(8, "list__merge_and_remove_dups");
	detstackvar(8) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(5) = (Integer) r3;
	detstackvar(3) = (Integer) r2;
	detstackvar(7) = (Integer) r1;
	{
	Declare_entry(mercury__compare_3_3);
	call_localret(ENTRY(mercury__compare_3_3),
		mercury__list__merge_and_remove_dups_3_0_i8,
		ENTRY(mercury__list__merge_and_remove_dups_3_0));
	}
Define_label(mercury__list__merge_and_remove_dups_3_0_i8);
	update_prof_current_proc(LABEL(mercury__list__merge_and_remove_dups_3_0));
	if (((Integer) r1 != ((Integer) 1)))
		GOTO_LABEL(mercury__list__merge_and_remove_dups_3_0_i9);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(2);
	localcall(mercury__list__merge_and_remove_dups_3_0,
		LABEL(mercury__list__merge_and_remove_dups_3_0_i12),
		ENTRY(mercury__list__merge_and_remove_dups_3_0));
Define_label(mercury__list__merge_and_remove_dups_3_0_i12);
	update_prof_current_proc(LABEL(mercury__list__merge_and_remove_dups_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__list__merge_and_remove_dups_3_0_i9);
	if (((Integer) r1 != ((Integer) 2)))
		GOTO_LABEL(mercury__list__merge_and_remove_dups_3_0_i13);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(6);
	localcall(mercury__list__merge_and_remove_dups_3_0,
		LABEL(mercury__list__merge_and_remove_dups_3_0_i16),
		ENTRY(mercury__list__merge_and_remove_dups_3_0));
Define_label(mercury__list__merge_and_remove_dups_3_0_i16);
	update_prof_current_proc(LABEL(mercury__list__merge_and_remove_dups_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(5);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__list__merge_and_remove_dups_3_0_i13);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__list__merge_and_remove_dups_3_0,
		ENTRY(mercury__list__merge_and_remove_dups_3_0));
Define_label(mercury__list__merge_and_remove_dups_3_0_i1007);
	r1 = (Integer) r2;
	proceed();
Define_label(mercury__list__merge_and_remove_dups_3_0_i1006);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module57)
	init_entry(mercury__list__remove_adjacent_dups_2_0);
	init_label(mercury__list__remove_adjacent_dups_2_0_i1002);
BEGIN_CODE

/* code for predicate 'list__remove_adjacent_dups'/2 in mode 0 */
Define_entry(mercury__list__remove_adjacent_dups_2_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__remove_adjacent_dups_2_0_i1002);
	r3 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	tailcall(STATIC(mercury__list__remove_adjacent_dups_2_3_0),
		ENTRY(mercury__list__remove_adjacent_dups_2_0));
Define_label(mercury__list__remove_adjacent_dups_2_0_i1002);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module58)
	init_entry(mercury__list__remove_dups_2_0);
	init_label(mercury__list__remove_dups_2_0_i2);
BEGIN_CODE

/* code for predicate 'list__remove_dups'/2 in mode 0 */
Define_entry(mercury__list__remove_dups_2_0);
	incr_sp_push_msg(3, "list__remove_dups");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r1;
	{
	Declare_entry(mercury__bintree_set__init_1_0);
	call_localret(ENTRY(mercury__bintree_set__init_1_0),
		mercury__list__remove_dups_2_0_i2,
		ENTRY(mercury__list__remove_dups_2_0));
	}
Define_label(mercury__list__remove_dups_2_0_i2);
	update_prof_current_proc(LABEL(mercury__list__remove_dups_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__list__remove_dups_2_3_0),
		ENTRY(mercury__list__remove_dups_2_0));
END_MODULE

BEGIN_MODULE(mercury__list_module59)
	init_entry(mercury__list__member_2_0);
	init_label(mercury__list__member_2_0_i6);
	init_label(mercury__list__member_2_0_i3);
	init_label(mercury__list__member_2_0_i1003);
BEGIN_CODE

/* code for predicate 'list__member'/2 in mode 0 */
Define_entry(mercury__list__member_2_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__member_2_0_i1003);
	incr_sp_push_msg(4, "list__member");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	detstackvar(1) = (Integer) r2;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__member_2_0_i6,
		ENTRY(mercury__list__member_2_0));
	}
Define_label(mercury__list__member_2_0_i6);
	update_prof_current_proc(LABEL(mercury__list__member_2_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__list__member_2_0_i3);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__list__member_2_0,
		ENTRY(mercury__list__member_2_0));
Define_label(mercury__list__member_2_0_i3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__list__member_2_0_i1003);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module60)
	init_entry(mercury__list__member_2_1);
	init_label(mercury__list__member_2_1_i1);
BEGIN_CODE

/* code for predicate 'list__member'/2 in mode 1 */
Define_entry(mercury__list__member_2_1);
	{
	Declare_entry(do_fail);
	mkframe("list__member/2", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__member__ua40001_2_0),
		mercury__list__member_2_1_i1,
		ENTRY(mercury__list__member_2_1));
Define_label(mercury__list__member_2_1_i1);
	update_prof_current_proc(LABEL(mercury__list__member_2_1));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module61)
	init_entry(mercury__list__member_3_0);
	init_label(mercury__list__member_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__member'/3 in mode 0 */
Define_entry(mercury__list__member_3_0);
	{
	Declare_entry(do_fail);
	mkframe("list__member/3", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__member__ua40000_3_0),
		mercury__list__member_3_0_i1,
		ENTRY(mercury__list__member_3_0));
Define_label(mercury__list__member_3_0_i1);
	update_prof_current_proc(LABEL(mercury__list__member_3_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module62)
	init_entry(mercury__list__length_2_0);
BEGIN_CODE

/* code for predicate 'list__length'/2 in mode 0 */
Define_entry(mercury__list__length_2_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__list__length__ua10000_2_0),
		ENTRY(mercury__list__length_2_0));
END_MODULE

BEGIN_MODULE(mercury__list_module63)
	init_entry(mercury__list__same_length_2_2);
BEGIN_CODE

/* code for predicate 'list__same_length'/2 in mode 2 */
Define_entry(mercury__list__same_length_2_2);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	tailcall(STATIC(mercury__list__same_length__ua2_2_0),
		ENTRY(mercury__list__same_length_2_2));
END_MODULE

BEGIN_MODULE(mercury__list_module64)
	init_entry(mercury__list__same_length_2_0);
BEGIN_CODE

/* code for predicate 'list__same_length'/2 in mode 0 */
Define_entry(mercury__list__same_length_2_0);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__list__same_length__ua10000_2_0),
		ENTRY(mercury__list__same_length_2_0));
END_MODULE

BEGIN_MODULE(mercury__list_module65)
	init_entry(mercury__list__same_length_2_1);
BEGIN_CODE

/* code for predicate 'list__same_length'/2 in mode 1 */
Define_entry(mercury__list__same_length_2_1);
	r1 = (Integer) r3;
	tailcall(STATIC(mercury__list__same_length__ua10001_2_0),
		ENTRY(mercury__list__same_length_2_1));
END_MODULE

BEGIN_MODULE(mercury__list_module66)
	init_entry(mercury__list__split_list_4_0);
	init_label(mercury__list__split_list_4_0_i2);
	init_label(mercury__list__split_list_4_0_i1000);
BEGIN_CODE

/* code for predicate 'list__split_list'/4 in mode 0 */
Define_entry(mercury__list__split_list_4_0);
	incr_sp_push_msg(2, "list__split_list");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__list__split_list__ua0_4_0),
		mercury__list__split_list_4_0_i2,
		ENTRY(mercury__list__split_list_4_0));
Define_label(mercury__list__split_list_4_0_i2);
	update_prof_current_proc(LABEL(mercury__list__split_list_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__split_list_4_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__split_list_4_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module67)
	init_entry(mercury__list__take_3_0);
	init_label(mercury__list__take_3_0_i2);
	init_label(mercury__list__take_3_0_i1000);
BEGIN_CODE

/* code for predicate 'list__take'/3 in mode 0 */
Define_entry(mercury__list__take_3_0);
	incr_sp_push_msg(2, "list__take");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__list__take__ua0_3_0),
		mercury__list__take_3_0_i2,
		ENTRY(mercury__list__take_3_0));
Define_label(mercury__list__take_3_0_i2);
	update_prof_current_proc(LABEL(mercury__list__take_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__take_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__take_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module68)
	init_entry(mercury__list__drop_3_0);
	init_label(mercury__list__drop_3_0_i2);
	init_label(mercury__list__drop_3_0_i1000);
BEGIN_CODE

/* code for predicate 'list__drop'/3 in mode 0 */
Define_entry(mercury__list__drop_3_0);
	incr_sp_push_msg(2, "list__drop");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__list__drop__ua0_3_0),
		mercury__list__drop_3_0_i2,
		ENTRY(mercury__list__drop_3_0));
Define_label(mercury__list__drop_3_0_i2);
	update_prof_current_proc(LABEL(mercury__list__drop_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__drop_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__drop_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module69)
	init_entry(mercury__list__insert_3_0);
BEGIN_CODE

/* code for predicate 'list__insert'/3 in mode 0 */
Define_entry(mercury__list__insert_3_0);
	{
	Word tempr1;
	tempr1 = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) r4;
	r4 = (Integer) tempr1;
	{
		tailcall(STATIC(mercury__list__delete_3_0),
		ENTRY(mercury__list__insert_3_0));
	}
	}
END_MODULE

BEGIN_MODULE(mercury__list_module70)
	init_entry(mercury__list__insert_3_1);
	init_label(mercury__list__insert_3_1_i1);
BEGIN_CODE

/* code for predicate 'list__insert'/3 in mode 1 */
Define_entry(mercury__list__insert_3_1);
	{
	Declare_entry(do_fail);
	mkframe("list__insert/3", 1, ENTRY(do_fail));
	}
	{
	Word tempr1;
	tempr1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	{
		call_localret(STATIC(mercury__list__delete_3_1),
		mercury__list__insert_3_1_i1,
		ENTRY(mercury__list__insert_3_1));
	}
	}
Define_label(mercury__list__insert_3_1_i1);
	update_prof_current_proc(LABEL(mercury__list__insert_3_1));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module71)
	init_entry(mercury__list__insert_3_2);
	init_label(mercury__list__insert_3_2_i1);
BEGIN_CODE

/* code for predicate 'list__insert'/3 in mode 2 */
Define_entry(mercury__list__insert_3_2);
	{
	Declare_entry(do_fail);
	mkframe("list__insert/3", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__insert__ua40002_3_0),
		mercury__list__insert_3_2_i1,
		ENTRY(mercury__list__insert_3_2));
Define_label(mercury__list__insert_3_2_i1);
	update_prof_current_proc(LABEL(mercury__list__insert_3_2));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module72)
	init_entry(mercury__list__insert_3_3);
	init_label(mercury__list__insert_3_3_i1);
BEGIN_CODE

/* code for predicate 'list__insert'/3 in mode 3 */
Define_entry(mercury__list__insert_3_3);
	{
	Declare_entry(do_fail);
	mkframe("list__insert/3", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__list__insert__ua60003_3_0),
		mercury__list__insert_3_3_i1,
		ENTRY(mercury__list__insert_3_3));
Define_label(mercury__list__insert_3_3_i1);
	update_prof_current_proc(LABEL(mercury__list__insert_3_3));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module73)
	init_entry(mercury__list__delete_3_0);
	init_label(mercury__list__delete_3_0_i6);
	init_label(mercury__list__delete_3_0_i8);
	init_label(mercury__list__delete_3_0_i5);
	init_label(mercury__list__delete_3_0_i11);
	init_label(mercury__list__delete_3_0_i3);
	init_label(mercury__list__delete_3_0_i1005);
	init_label(mercury__list__delete_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__delete'/3 in mode 0 */
Define_entry(mercury__list__delete_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__delete_3_0_i1005);
	incr_sp_push_msg(6, "list__delete");
	detstackvar(6) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) tempr1;
	detstackvar(1) = (Integer) r3;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	detstackvar(2) = (Integer) r4;
	detstackvar(5) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__delete_3_0_i6,
		ENTRY(mercury__list__delete_3_0));
	}
	}
Define_label(mercury__list__delete_3_0_i6);
	update_prof_current_proc(LABEL(mercury__list__delete_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__delete_3_0_i5);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		mercury__list__delete_3_0_i8,
		ENTRY(mercury__list__delete_3_0));
	}
Define_label(mercury__list__delete_3_0_i8);
	update_prof_current_proc(LABEL(mercury__list__delete_3_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__list__delete_3_0_i3);
Define_label(mercury__list__delete_3_0_i5);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(2);
	if (((Integer) tempr1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__delete_3_0_i1);
	r3 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 1));
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__delete_3_0_i11,
		ENTRY(mercury__list__delete_3_0));
	}
	}
Define_label(mercury__list__delete_3_0_i11);
	update_prof_current_proc(LABEL(mercury__list__delete_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__delete_3_0_i1);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__list__delete_3_0,
		ENTRY(mercury__list__delete_3_0));
Define_label(mercury__list__delete_3_0_i3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__list__delete_3_0_i1005);
	r1 = FALSE;
	proceed();
Define_label(mercury__list__delete_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module74)
	init_entry(mercury__list__delete_3_1);
	init_label(mercury__list__delete_3_1_i5);
	init_label(mercury__list__delete_3_1_i4);
	init_label(mercury__list__delete_3_1_i1009);
	init_label(mercury__list__delete_3_1_i7);
BEGIN_CODE

/* code for predicate 'list__delete'/3 in mode 1 */
Define_entry(mercury__list__delete_3_1);
	{
	Declare_entry(do_redo);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO(ENTRY(do_redo));
	}
	mkframe("list__delete/3", 4, LABEL(mercury__list__delete_3_1_i4));
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	framevar(1) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	framevar(2) = (Integer) tempr1;
	framevar(0) = (Integer) r3;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	framevar(3) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__delete_3_1_i5,
		ENTRY(mercury__list__delete_3_1));
	}
	}
Define_label(mercury__list__delete_3_1_i5);
	update_prof_current_proc(LABEL(mercury__list__delete_3_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__delete_3_1_i1009);
	r1 = (Integer) framevar(1);
	succeed();
Define_label(mercury__list__delete_3_1_i4);
	update_prof_current_proc(LABEL(mercury__list__delete_3_1));
Define_label(mercury__list__delete_3_1_i1009);
	{
	Declare_entry(do_fail);
	LVALUE_CAST(Word,bt_redoip((Integer) maxfr)) = (Integer) ENTRY(do_fail);
	}
	r1 = (Integer) framevar(3);
	r2 = (Integer) framevar(1);
	r3 = (Integer) framevar(0);
	localcall(mercury__list__delete_3_1,
		LABEL(mercury__list__delete_3_1_i7),
		ENTRY(mercury__list__delete_3_1));
Define_label(mercury__list__delete_3_1_i7);
	update_prof_current_proc(LABEL(mercury__list__delete_3_1));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) framevar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module75)
	init_entry(mercury__list__delete_3_2);
	init_label(mercury__list__delete_3_2_i1);
BEGIN_CODE

/* code for predicate 'list__delete'/3 in mode 2 */
Define_entry(mercury__list__delete_3_2);
	{
	Declare_entry(do_fail);
	mkframe("list__delete/3", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__delete__ua40002_3_0),
		mercury__list__delete_3_2_i1,
		ENTRY(mercury__list__delete_3_2));
Define_label(mercury__list__delete_3_2_i1);
	update_prof_current_proc(LABEL(mercury__list__delete_3_2));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module76)
	init_entry(mercury__list__delete_3_3);
	init_label(mercury__list__delete_3_3_i1);
BEGIN_CODE

/* code for predicate 'list__delete'/3 in mode 3 */
Define_entry(mercury__list__delete_3_3);
	{
	Declare_entry(do_fail);
	mkframe("list__delete/3", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__list__delete__ua60003_3_0),
		mercury__list__delete_3_3_i1,
		ENTRY(mercury__list__delete_3_3));
Define_label(mercury__list__delete_3_3_i1);
	update_prof_current_proc(LABEL(mercury__list__delete_3_3));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module77)
	init_entry(mercury__list__delete_first_3_0);
	init_label(mercury__list__delete_first_3_0_i5);
	init_label(mercury__list__delete_first_3_0_i4);
	init_label(mercury__list__delete_first_3_0_i7);
	init_label(mercury__list__delete_first_3_0_i1004);
	init_label(mercury__list__delete_first_3_0_i1);
BEGIN_CODE

/* code for predicate 'list__delete_first'/3 in mode 0 */
Define_entry(mercury__list__delete_first_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__delete_first_3_0_i1004);
	incr_sp_push_msg(5, "list__delete_first");
	detstackvar(5) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__delete_first_3_0_i5,
		ENTRY(mercury__list__delete_first_3_0));
	}
Define_label(mercury__list__delete_first_3_0_i5);
	update_prof_current_proc(LABEL(mercury__list__delete_first_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__delete_first_3_0_i4);
	r2 = (Integer) detstackvar(3);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__list__delete_first_3_0_i4);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__list__delete_first_3_0,
		LABEL(mercury__list__delete_first_3_0_i7),
		ENTRY(mercury__list__delete_first_3_0));
Define_label(mercury__list__delete_first_3_0_i7);
	update_prof_current_proc(LABEL(mercury__list__delete_first_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__delete_first_3_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__list__delete_first_3_0_i1004);
	r1 = FALSE;
	proceed();
Define_label(mercury__list__delete_first_3_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module78)
	init_entry(mercury__list__delete_all_3_0);
	init_label(mercury__list__delete_all_3_0_i6);
	init_label(mercury__list__delete_all_3_0_i5);
	init_label(mercury__list__delete_all_3_0_i9);
	init_label(mercury__list__delete_all_3_0_i1003);
BEGIN_CODE

/* code for predicate 'list__delete_all'/3 in mode 0 */
Define_entry(mercury__list__delete_all_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__delete_all_3_0_i1003);
	incr_sp_push_msg(5, "list__delete_all");
	detstackvar(5) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__delete_all_3_0_i6,
		ENTRY(mercury__list__delete_all_3_0));
	}
Define_label(mercury__list__delete_all_3_0_i6);
	update_prof_current_proc(LABEL(mercury__list__delete_all_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__delete_all_3_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__list__delete_all_3_0,
		ENTRY(mercury__list__delete_all_3_0));
Define_label(mercury__list__delete_all_3_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__list__delete_all_3_0,
		LABEL(mercury__list__delete_all_3_0_i9),
		ENTRY(mercury__list__delete_all_3_0));
Define_label(mercury__list__delete_all_3_0_i9);
	update_prof_current_proc(LABEL(mercury__list__delete_all_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__list__delete_all_3_0_i1003);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module79)
	init_entry(mercury__list__delete_all_3_1);
	init_label(mercury__list__delete_all_3_1_i6);
	init_label(mercury__list__delete_all_3_1_i5);
	init_label(mercury__list__delete_all_3_1_i9);
	init_label(mercury__list__delete_all_3_1_i1003);
BEGIN_CODE

/* code for predicate 'list__delete_all'/3 in mode 1 */
Define_entry(mercury__list__delete_all_3_1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__delete_all_3_1_i1003);
	incr_sp_push_msg(5, "list__delete_all");
	detstackvar(5) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__delete_all_3_1_i6,
		ENTRY(mercury__list__delete_all_3_1));
	}
Define_label(mercury__list__delete_all_3_1_i6);
	update_prof_current_proc(LABEL(mercury__list__delete_all_3_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__delete_all_3_1_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__list__delete_all_3_1,
		ENTRY(mercury__list__delete_all_3_1));
Define_label(mercury__list__delete_all_3_1_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__list__delete_all_3_1,
		LABEL(mercury__list__delete_all_3_1_i9),
		ENTRY(mercury__list__delete_all_3_1));
Define_label(mercury__list__delete_all_3_1_i9);
	update_prof_current_proc(LABEL(mercury__list__delete_all_3_1));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__list__delete_all_3_1_i1003);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module80)
	init_entry(mercury__list__delete_elems_3_0);
	init_label(mercury__list__delete_elems_3_0_i4);
	init_label(mercury__list__delete_elems_3_0_i1002);
BEGIN_CODE

/* code for predicate 'list__delete_elems'/3 in mode 0 */
Define_entry(mercury__list__delete_elems_3_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__delete_elems_3_0_i1002);
	incr_sp_push_msg(3, "list__delete_elems");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) r1;
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
		call_localret(STATIC(mercury__list__delete_all_3_1),
		mercury__list__delete_elems_3_0_i4,
		ENTRY(mercury__list__delete_elems_3_0));
	}
Define_label(mercury__list__delete_elems_3_0_i4);
	update_prof_current_proc(LABEL(mercury__list__delete_elems_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__list__delete_elems_3_0,
		ENTRY(mercury__list__delete_elems_3_0));
Define_label(mercury__list__delete_elems_3_0_i1002);
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module81)
	init_entry(mercury__list__replace_4_0);
	init_label(mercury__list__replace_4_0_i7);
	init_label(mercury__list__replace_4_0_i9);
	init_label(mercury__list__replace_4_0_i11);
	init_label(mercury__list__replace_4_0_i6);
	init_label(mercury__list__replace_4_0_i13);
	init_label(mercury__list__replace_4_0_i4);
	init_label(mercury__list__replace_4_0_i1006);
	init_label(mercury__list__replace_4_0_i1);
BEGIN_CODE

/* code for predicate 'list__replace'/4 in mode 0 */
Define_entry(mercury__list__replace_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__replace_4_0_i1006);
	if (((Integer) r5 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__replace_4_0_i1006);
	incr_sp_push_msg(8, "list__replace");
	detstackvar(8) = (Integer) succip;
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) tempr1;
	detstackvar(1) = (Integer) r3;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	detstackvar(5) = (Integer) field(mktag(1), (Integer) r5, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(1), (Integer) r5, ((Integer) 0));
	detstackvar(2) = (Integer) r4;
	detstackvar(7) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__replace_4_0_i7,
		ENTRY(mercury__list__replace_4_0));
	}
	}
Define_label(mercury__list__replace_4_0_i7);
	update_prof_current_proc(LABEL(mercury__list__replace_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__replace_4_0_i6);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__replace_4_0_i9,
		ENTRY(mercury__list__replace_4_0));
	}
Define_label(mercury__list__replace_4_0_i9);
	update_prof_current_proc(LABEL(mercury__list__replace_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__replace_4_0_i6);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		mercury__list__replace_4_0_i11,
		ENTRY(mercury__list__replace_4_0));
	}
Define_label(mercury__list__replace_4_0_i11);
	update_prof_current_proc(LABEL(mercury__list__replace_4_0));
	if ((Integer) r1)
		GOTO_LABEL(mercury__list__replace_4_0_i4);
Define_label(mercury__list__replace_4_0_i6);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__replace_4_0_i13,
		ENTRY(mercury__list__replace_4_0));
	}
Define_label(mercury__list__replace_4_0_i13);
	update_prof_current_proc(LABEL(mercury__list__replace_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__replace_4_0_i1);
	r1 = (Integer) detstackvar(7);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	r5 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	localtailcall(mercury__list__replace_4_0,
		ENTRY(mercury__list__replace_4_0));
Define_label(mercury__list__replace_4_0_i4);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
Define_label(mercury__list__replace_4_0_i1006);
	r1 = FALSE;
	proceed();
Define_label(mercury__list__replace_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(8);
	decr_sp_pop_msg(8);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module82)
	init_entry(mercury__list__replace_4_1);
	init_label(mercury__list__replace_4_1_i5);
	init_label(mercury__list__replace_4_1_i4);
	init_label(mercury__list__replace_4_1_i1011);
	init_label(mercury__list__replace_4_1_i7);
BEGIN_CODE

/* code for predicate 'list__replace'/4 in mode 1 */
Define_entry(mercury__list__replace_4_1);
	{
	Declare_entry(do_redo);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO(ENTRY(do_redo));
	}
	mkframe("list__replace/4", 5, LABEL(mercury__list__replace_4_1_i4));
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	framevar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	framevar(3) = (Integer) tempr1;
	framevar(0) = (Integer) r3;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	framevar(1) = (Integer) r4;
	framevar(4) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__replace_4_1_i5,
		ENTRY(mercury__list__replace_4_1));
	}
	}
Define_label(mercury__list__replace_4_1_i5);
	update_prof_current_proc(LABEL(mercury__list__replace_4_1));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__replace_4_1_i1011);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) framevar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) framevar(2);
	succeed();
Define_label(mercury__list__replace_4_1_i4);
	update_prof_current_proc(LABEL(mercury__list__replace_4_1));
Define_label(mercury__list__replace_4_1_i1011);
	{
	Declare_entry(do_fail);
	LVALUE_CAST(Word,bt_redoip((Integer) maxfr)) = (Integer) ENTRY(do_fail);
	}
	r1 = (Integer) framevar(4);
	r2 = (Integer) framevar(2);
	r3 = (Integer) framevar(0);
	r4 = (Integer) framevar(1);
	localcall(mercury__list__replace_4_1,
		LABEL(mercury__list__replace_4_1_i7),
		ENTRY(mercury__list__replace_4_1));
Define_label(mercury__list__replace_4_1_i7);
	update_prof_current_proc(LABEL(mercury__list__replace_4_1));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) framevar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module83)
	init_entry(mercury__list__replace_first_4_0);
	init_label(mercury__list__replace_first_4_0_i5);
	init_label(mercury__list__replace_first_4_0_i4);
	init_label(mercury__list__replace_first_4_0_i7);
	init_label(mercury__list__replace_first_4_0_i1005);
	init_label(mercury__list__replace_first_4_0_i1);
BEGIN_CODE

/* code for predicate 'list__replace_first'/4 in mode 0 */
Define_entry(mercury__list__replace_first_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__replace_first_4_0_i1005);
	incr_sp_push_msg(6, "list__replace_first");
	detstackvar(6) = (Integer) succip;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(5) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__replace_first_4_0_i5,
		ENTRY(mercury__list__replace_first_4_0));
	}
Define_label(mercury__list__replace_first_4_0_i5);
	update_prof_current_proc(LABEL(mercury__list__replace_first_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__replace_first_4_0_i4);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(4);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__list__replace_first_4_0_i4);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__list__replace_first_4_0,
		LABEL(mercury__list__replace_first_4_0_i7),
		ENTRY(mercury__list__replace_first_4_0));
Define_label(mercury__list__replace_first_4_0_i7);
	update_prof_current_proc(LABEL(mercury__list__replace_first_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__replace_first_4_0_i1);
	r1 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r1;
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__list__replace_first_4_0_i1005);
	r1 = FALSE;
	proceed();
Define_label(mercury__list__replace_first_4_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module84)
	init_entry(mercury__list__replace_all_4_0);
	init_label(mercury__list__replace_all_4_0_i6);
	init_label(mercury__list__replace_all_4_0_i8);
	init_label(mercury__list__replace_all_4_0_i5);
	init_label(mercury__list__replace_all_4_0_i9);
	init_label(mercury__list__replace_all_4_0_i1004);
BEGIN_CODE

/* code for predicate 'list__replace_all'/4 in mode 0 */
Define_entry(mercury__list__replace_all_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__replace_all_4_0_i1004);
	incr_sp_push_msg(6, "list__replace_all");
	detstackvar(6) = (Integer) succip;
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r4;
	detstackvar(5) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__replace_all_4_0_i6,
		ENTRY(mercury__list__replace_all_4_0));
	}
Define_label(mercury__list__replace_all_4_0_i6);
	update_prof_current_proc(LABEL(mercury__list__replace_all_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__replace_all_4_0_i5);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__list__replace_all_4_0,
		LABEL(mercury__list__replace_all_4_0_i8),
		ENTRY(mercury__list__replace_all_4_0));
Define_label(mercury__list__replace_all_4_0_i8);
	update_prof_current_proc(LABEL(mercury__list__replace_all_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__list__replace_all_4_0_i5);
	r1 = (Integer) detstackvar(5);
	r2 = (Integer) detstackvar(4);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(2);
	localcall(mercury__list__replace_all_4_0,
		LABEL(mercury__list__replace_all_4_0_i9),
		ENTRY(mercury__list__replace_all_4_0));
Define_label(mercury__list__replace_all_4_0_i9);
	update_prof_current_proc(LABEL(mercury__list__replace_all_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__list__replace_all_4_0_i1004);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module85)
	init_entry(mercury__list__sort_and_remove_dups_2_0);
	init_label(mercury__list__sort_and_remove_dups_2_0_i2);
BEGIN_CODE

/* code for predicate 'list__sort_and_remove_dups'/2 in mode 0 */
Define_entry(mercury__list__sort_and_remove_dups_2_0);
	incr_sp_push_msg(2, "list__sort_and_remove_dups");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	call_localret(STATIC(mercury__list__merge_sort_2_0),
		mercury__list__sort_and_remove_dups_2_0_i2,
		ENTRY(mercury__list__sort_and_remove_dups_2_0));
Define_label(mercury__list__sort_and_remove_dups_2_0_i2);
	update_prof_current_proc(LABEL(mercury__list__sort_and_remove_dups_2_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__list__remove_adjacent_dups_2_0),
		ENTRY(mercury__list__sort_and_remove_dups_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__list_module86)
	init_entry(mercury__list__sort_2_0);
BEGIN_CODE

/* code for predicate 'list__sort'/2 in mode 0 */
Define_entry(mercury__list__sort_2_0);
	tailcall(STATIC(mercury__list__merge_sort_2_0),
		ENTRY(mercury__list__sort_2_0));
END_MODULE

BEGIN_MODULE(mercury__list_module87)
	init_entry(mercury__list__reverse_2_0);
BEGIN_CODE

/* code for predicate 'list__reverse'/2 in mode 0 */
Define_entry(mercury__list__reverse_2_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__list__reverse__ua10000_2_0),
		ENTRY(mercury__list__reverse_2_0));
END_MODULE

BEGIN_MODULE(mercury__list_module88)
	init_entry(mercury__list__perm_2_0);
	init_label(mercury__list__perm_2_0_i1);
BEGIN_CODE

/* code for predicate 'list__perm'/2 in mode 0 */
Define_entry(mercury__list__perm_2_0);
	{
	Declare_entry(do_fail);
	mkframe("list__perm/2", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__perm__ua40000_2_0),
		mercury__list__perm_2_0_i1,
		ENTRY(mercury__list__perm_2_0));
Define_label(mercury__list__perm_2_0_i1);
	update_prof_current_proc(LABEL(mercury__list__perm_2_0));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module89)
	init_entry(mercury__list__nth_member_search_3_0);
	init_label(mercury__list__nth_member_search_3_0_i5);
	init_label(mercury__list__nth_member_search_3_0_i4);
	init_label(mercury__list__nth_member_search_3_0_i7);
	init_label(mercury__list__nth_member_search_3_0_i1004);
	init_label(mercury__list__nth_member_search_3_0_i1006);
BEGIN_CODE

/* code for predicate 'list__nth_member_search'/3 in mode 0 */
Define_entry(mercury__list__nth_member_search_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__nth_member_search_3_0_i1004);
	incr_sp_push_msg(4, "list__nth_member_search");
	detstackvar(4) = (Integer) succip;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(1) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__nth_member_search_3_0_i5,
		ENTRY(mercury__list__nth_member_search_3_0));
	}
Define_label(mercury__list__nth_member_search_3_0_i5);
	update_prof_current_proc(LABEL(mercury__list__nth_member_search_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__nth_member_search_3_0_i4);
	r2 = ((Integer) 1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__list__nth_member_search_3_0_i4);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(1);
	localcall(mercury__list__nth_member_search_3_0,
		LABEL(mercury__list__nth_member_search_3_0_i7),
		ENTRY(mercury__list__nth_member_search_3_0));
Define_label(mercury__list__nth_member_search_3_0_i7);
	update_prof_current_proc(LABEL(mercury__list__nth_member_search_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__nth_member_search_3_0_i1006);
	r2 = ((Integer) r2 + ((Integer) 1));
	r1 = TRUE;
	proceed();
Define_label(mercury__list__nth_member_search_3_0_i1004);
	r1 = FALSE;
	proceed();
Define_label(mercury__list__nth_member_search_3_0_i1006);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module90)
	init_entry(mercury__list__index0_3_0);
	init_label(mercury__list__index0_3_0_i2);
	init_label(mercury__list__index0_3_0_i1000);
BEGIN_CODE

/* code for predicate 'list__index0'/3 in mode 0 */
Define_entry(mercury__list__index0_3_0);
	incr_sp_push_msg(2, "list__index0");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__list__index0__ua0_3_0),
		mercury__list__index0_3_0_i2,
		ENTRY(mercury__list__index0_3_0));
Define_label(mercury__list__index0_3_0_i2);
	update_prof_current_proc(LABEL(mercury__list__index0_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__index0_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__index0_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module91)
	init_entry(mercury__list__index1_3_0);
	init_label(mercury__list__index1_3_0_i2);
	init_label(mercury__list__index1_3_0_i1000);
BEGIN_CODE

/* code for predicate 'list__index1'/3 in mode 0 */
Define_entry(mercury__list__index1_3_0);
	incr_sp_push_msg(2, "list__index1");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	call_localret(STATIC(mercury__list__index1__ua0_3_0),
		mercury__list__index1_3_0_i2,
		ENTRY(mercury__list__index1_3_0));
Define_label(mercury__list__index1_3_0_i2);
	update_prof_current_proc(LABEL(mercury__list__index1_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__index1_3_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__index1_3_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module92)
	init_entry(mercury__list__index0_det_3_0);
BEGIN_CODE

/* code for predicate 'list__index0_det'/3 in mode 0 */
Define_entry(mercury__list__index0_det_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__list__index0_det__ua10000_3_0),
		ENTRY(mercury__list__index0_det_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module93)
	init_entry(mercury__list__index1_det_3_0);
BEGIN_CODE

/* code for predicate 'list__index1_det'/3 in mode 0 */
Define_entry(mercury__list__index1_det_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__list__index1_det__ua10000_3_0),
		ENTRY(mercury__list__index1_det_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module94)
	init_entry(mercury__list__zip_3_0);
BEGIN_CODE

/* code for predicate 'list__zip'/3 in mode 0 */
Define_entry(mercury__list__zip_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__list__zip__ua10000_3_0),
		ENTRY(mercury__list__zip_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module95)
	init_entry(mercury__list__duplicate_3_0);
BEGIN_CODE

/* code for predicate 'list__duplicate'/3 in mode 0 */
Define_entry(mercury__list__duplicate_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__list__duplicate__ua10000_3_0),
		ENTRY(mercury__list__duplicate_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module96)
	init_entry(mercury__list__condense_2_0);
BEGIN_CODE

/* code for predicate 'list__condense'/2 in mode 0 */
Define_entry(mercury__list__condense_2_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__list__condense__ua10000_2_0),
		ENTRY(mercury__list__condense_2_0));
END_MODULE

BEGIN_MODULE(mercury__list_module97)
	init_entry(mercury__list__chunk_3_0);
BEGIN_CODE

/* code for predicate 'list__chunk'/3 in mode 0 */
Define_entry(mercury__list__chunk_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__list__chunk__ua10000_3_0),
		ENTRY(mercury__list__chunk_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module98)
	init_entry(mercury__list__sublist_2_0);
	init_label(mercury__list__sublist_2_0_i7);
	init_label(mercury__list__sublist_2_0_i6);
	init_label(mercury__list__sublist_2_0_i1004);
	init_label(mercury__list__sublist_2_0_i1005);
BEGIN_CODE

/* code for predicate 'list__sublist'/2 in mode 0 */
Define_entry(mercury__list__sublist_2_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__sublist_2_0_i1004);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__sublist_2_0_i1005);
	incr_sp_push_msg(5, "list__sublist");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(4) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__sublist_2_0_i7,
		ENTRY(mercury__list__sublist_2_0));
	}
Define_label(mercury__list__sublist_2_0_i7);
	update_prof_current_proc(LABEL(mercury__list__sublist_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__sublist_2_0_i6);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__list__sublist_2_0,
		ENTRY(mercury__list__sublist_2_0));
Define_label(mercury__list__sublist_2_0_i6);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__list__sublist_2_0,
		ENTRY(mercury__list__sublist_2_0));
Define_label(mercury__list__sublist_2_0_i1004);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__sublist_2_0_i1005);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module99)
	init_entry(mercury__list__all_same_1_0);
	init_label(mercury__list__all_same_1_0_i1002);
BEGIN_CODE

/* code for predicate 'list__all_same'/1 in mode 0 */
Define_entry(mercury__list__all_same_1_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__all_same_1_0_i1002);
	r3 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	tailcall(STATIC(mercury__list__all_same_2_2_0),
		ENTRY(mercury__list__all_same_1_0));
Define_label(mercury__list__all_same_1_0_i1002);
	r1 = TRUE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module100)
	init_entry(mercury__list__last_2_0);
	init_label(mercury__list__last_2_0_i2);
	init_label(mercury__list__last_2_0_i1000);
BEGIN_CODE

/* code for predicate 'list__last'/2 in mode 0 */
Define_entry(mercury__list__last_2_0);
	incr_sp_push_msg(2, "list__last");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__last__ua0_2_0),
		mercury__list__last_2_0_i2,
		ENTRY(mercury__list__last_2_0));
Define_label(mercury__list__last_2_0_i2);
	update_prof_current_proc(LABEL(mercury__list__last_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__last_2_0_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__last_2_0_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module101)
	init_entry(mercury__list__map_3_1);
	init_label(mercury__list__map_3_1_i2);
	init_label(mercury__list__map_3_1_i1000);
BEGIN_CODE

/* code for predicate 'list__map'/3 in mode 1 */
Define_entry(mercury__list__map_3_1);
	incr_sp_push_msg(3, "list__map");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__list__map__ua1_3_0),
		mercury__list__map_3_1_i2,
		ENTRY(mercury__list__map_3_1));
Define_label(mercury__list__map_3_1_i2);
	update_prof_current_proc(LABEL(mercury__list__map_3_1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__map_3_1_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__map_3_1_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module102)
	init_entry(mercury__list__map_3_0);
BEGIN_CODE

/* code for predicate 'list__map'/3 in mode 0 */
Define_entry(mercury__list__map_3_0);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	tailcall(STATIC(mercury__list__map__ua10000_3_0),
		ENTRY(mercury__list__map_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module103)
	init_entry(mercury__list__map_3_3);
	init_label(mercury__list__map_3_3_i1);
BEGIN_CODE

/* code for predicate 'list__map'/3 in mode 3 */
Define_entry(mercury__list__map_3_3);
	{
	Declare_entry(do_fail);
	mkframe("list__map/3", 2, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	framevar(1) = (Integer) r2;
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__list__map__ua40003_3_0),
		mercury__list__map_3_3_i1,
		ENTRY(mercury__list__map_3_3));
Define_label(mercury__list__map_3_3_i1);
	update_prof_current_proc(LABEL(mercury__list__map_3_3));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module104)
	init_entry(mercury__list__map_3_2);
	init_label(mercury__list__map_3_2_i1);
BEGIN_CODE

/* code for predicate 'list__map'/3 in mode 2 */
Define_entry(mercury__list__map_3_2);
	{
	Declare_entry(do_fail);
	mkframe("list__map/3", 2, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	framevar(1) = (Integer) r2;
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	call_localret(STATIC(mercury__list__map__ua60002_3_0),
		mercury__list__map_3_2_i1,
		ENTRY(mercury__list__map_3_2));
Define_label(mercury__list__map_3_2_i1);
	update_prof_current_proc(LABEL(mercury__list__map_3_2));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module105)
	init_entry(mercury__list__foldl_4_2);
	init_label(mercury__list__foldl_4_2_i2);
	init_label(mercury__list__foldl_4_2_i1000);
BEGIN_CODE

/* code for predicate 'list__foldl'/4 in mode 2 */
Define_entry(mercury__list__foldl_4_2);
	incr_sp_push_msg(3, "list__foldl");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	r3 = (Integer) r5;
	call_localret(STATIC(mercury__list__foldl__ua2_4_0),
		mercury__list__foldl_4_2_i2,
		ENTRY(mercury__list__foldl_4_2));
Define_label(mercury__list__foldl_4_2_i2);
	update_prof_current_proc(LABEL(mercury__list__foldl_4_2));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__foldl_4_2_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__foldl_4_2_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module106)
	init_entry(mercury__list__foldl_4_0);
BEGIN_CODE

/* code for predicate 'list__foldl'/4 in mode 0 */
Define_entry(mercury__list__foldl_4_0);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	r3 = (Integer) r5;
	tailcall(STATIC(mercury__list__foldl__ua10000_4_0),
		ENTRY(mercury__list__foldl_4_0));
END_MODULE

BEGIN_MODULE(mercury__list_module107)
	init_entry(mercury__list__foldl_4_1);
BEGIN_CODE

/* code for predicate 'list__foldl'/4 in mode 1 */
Define_entry(mercury__list__foldl_4_1);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	r3 = (Integer) r5;
	tailcall(STATIC(mercury__list__foldl__ua10001_4_0),
		ENTRY(mercury__list__foldl_4_1));
END_MODULE

BEGIN_MODULE(mercury__list_module108)
	init_entry(mercury__list__foldr_4_1);
	init_label(mercury__list__foldr_4_1_i2);
	init_label(mercury__list__foldr_4_1_i1000);
BEGIN_CODE

/* code for predicate 'list__foldr'/4 in mode 1 */
Define_entry(mercury__list__foldr_4_1);
	incr_sp_push_msg(3, "list__foldr");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	r3 = (Integer) r5;
	call_localret(STATIC(mercury__list__foldr__ua1_4_0),
		mercury__list__foldr_4_1_i2,
		ENTRY(mercury__list__foldr_4_1));
Define_label(mercury__list__foldr_4_1_i2);
	update_prof_current_proc(LABEL(mercury__list__foldr_4_1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__foldr_4_1_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__foldr_4_1_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module109)
	init_entry(mercury__list__foldr_4_0);
BEGIN_CODE

/* code for predicate 'list__foldr'/4 in mode 0 */
Define_entry(mercury__list__foldr_4_0);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	r3 = (Integer) r5;
	tailcall(STATIC(mercury__list__foldr__ua10000_4_0),
		ENTRY(mercury__list__foldr_4_0));
END_MODULE

BEGIN_MODULE(mercury__list_module110)
	init_entry(mercury__list__filter_3_0);
BEGIN_CODE

/* code for predicate 'list__filter'/3 in mode 0 */
Define_entry(mercury__list__filter_3_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__list__filter__ua10000_3_0),
		ENTRY(mercury__list__filter_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module111)
	init_entry(mercury__list__filter_4_0);
BEGIN_CODE

/* code for predicate 'list__filter'/4 in mode 0 */
Define_entry(mercury__list__filter_4_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	tailcall(STATIC(mercury__list__filter__ua10000_4_0),
		ENTRY(mercury__list__filter_4_0));
END_MODULE

BEGIN_MODULE(mercury__list_module112)
	init_entry(mercury__list__filter_map_3_0);
BEGIN_CODE

/* code for predicate 'list__filter_map'/3 in mode 0 */
Define_entry(mercury__list__filter_map_3_0);
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	tailcall(STATIC(mercury__list__filter_map__ua10000_3_0),
		ENTRY(mercury__list__filter_map_3_0));
END_MODULE

BEGIN_MODULE(mercury__list_module113)
	init_entry(mercury__list__sort_3_0);
	init_label(mercury__list__sort_3_0_i2);
	init_label(mercury__list__sort_3_0_i3);
	init_label(mercury__list__sort_3_0_i8);
	init_label(mercury__list__sort_3_0_i10);
	init_label(mercury__list__sort_3_0_i7);
BEGIN_CODE

/* code for predicate 'list__sort'/3 in mode 0 */
Define_entry(mercury__list__sort_3_0);
	incr_sp_push_msg(4, "list__sort");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__list__length__ua10000_2_0),
		mercury__list__sort_3_0_i2,
		ENTRY(mercury__list__sort_3_0));
Define_label(mercury__list__sort_3_0_i2);
	update_prof_current_proc(LABEL(mercury__list__sort_3_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury__list__sort_3_0_i3);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__list__sort_3_0_i3);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__list__hosort__ua0_5_0),
		mercury__list__sort_3_0_i8,
		ENTRY(mercury__list__sort_3_0));
Define_label(mercury__list__sort_3_0_i8);
	update_prof_current_proc(LABEL(mercury__list__sort_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__sort_3_0_i7);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	call_localret(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		mercury__list__sort_3_0_i10,
		ENTRY(mercury__list__sort_3_0));
	}
Define_label(mercury__list__sort_3_0_i10);
	update_prof_current_proc(LABEL(mercury__list__sort_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__sort_3_0_i7);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__list__sort_3_0_i7);
	r1 = string_const("hosort failed", 13);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__list__sort_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__list_module114)
	init_entry(mercury__list__merge_4_0);
BEGIN_CODE

/* code for predicate 'list__merge'/4 in mode 0 */
Define_entry(mercury__list__merge_4_0);
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	r3 = (Integer) r4;
	tailcall(STATIC(mercury__list__merge__ua10000_4_0),
		ENTRY(mercury__list__merge_4_0));
END_MODULE

BEGIN_MODULE(mercury__list_module115)
	init_entry(mercury__list__merge_sort_2_0);
	init_label(mercury__list__merge_sort_2_0_i6);
	init_label(mercury__list__merge_sort_2_0_i9);
	init_label(mercury__list__merge_sort_2_0_i11);
	init_label(mercury__list__merge_sort_2_0_i12);
	init_label(mercury__list__merge_sort_2_0_i8);
	init_label(mercury__list__merge_sort_2_0_i1005);
	init_label(mercury__list__merge_sort_2_0_i1004);
BEGIN_CODE

/* code for predicate 'list__merge_sort'/2 in mode 0 */
Define_static(mercury__list__merge_sort_2_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__merge_sort_2_0_i1004);
	r3 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__merge_sort_2_0_i1005);
	incr_sp_push_msg(3, "list__merge_sort");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__length__ua10000_2_0),
		mercury__list__merge_sort_2_0_i6,
		STATIC(mercury__list__merge_sort_2_0));
Define_label(mercury__list__merge_sort_2_0_i6);
	update_prof_current_proc(LABEL(mercury__list__merge_sort_2_0));
	r1 = ((Integer) r1 / ((Integer) 2));
	r2 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__list__split_list__ua0_4_0),
		mercury__list__merge_sort_2_0_i9,
		STATIC(mercury__list__merge_sort_2_0));
Define_label(mercury__list__merge_sort_2_0_i9);
	update_prof_current_proc(LABEL(mercury__list__merge_sort_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__merge_sort_2_0_i8);
	detstackvar(1) = (Integer) r3;
	r1 = (Integer) detstackvar(2);
	localcall(mercury__list__merge_sort_2_0,
		LABEL(mercury__list__merge_sort_2_0_i11),
		STATIC(mercury__list__merge_sort_2_0));
Define_label(mercury__list__merge_sort_2_0_i11);
	update_prof_current_proc(LABEL(mercury__list__merge_sort_2_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	localcall(mercury__list__merge_sort_2_0,
		LABEL(mercury__list__merge_sort_2_0_i12),
		STATIC(mercury__list__merge_sort_2_0));
Define_label(mercury__list__merge_sort_2_0_i12);
	update_prof_current_proc(LABEL(mercury__list__merge_sort_2_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
		tailcall(STATIC(mercury__list__merge_3_0),
		STATIC(mercury__list__merge_sort_2_0));
	}
Define_label(mercury__list__merge_sort_2_0_i8);
	r1 = string_const("list__merge_sort", 16);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__list__merge_sort_2_0));
	}
Define_label(mercury__list__merge_sort_2_0_i1005);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
Define_label(mercury__list__merge_sort_2_0_i1004);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module116)
	init_entry(mercury__list__remove_dups_2_3_0);
	init_label(mercury__list__remove_dups_2_3_0_i6);
	init_label(mercury__list__remove_dups_2_3_0_i5);
	init_label(mercury__list__remove_dups_2_3_0_i9);
	init_label(mercury__list__remove_dups_2_3_0_i10);
	init_label(mercury__list__remove_dups_2_3_0_i1003);
BEGIN_CODE

/* code for predicate 'list__remove_dups_2'/3 in mode 0 */
Define_static(mercury__list__remove_dups_2_3_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__remove_dups_2_3_0_i1003);
	incr_sp_push_msg(5, "list__remove_dups_2");
	detstackvar(5) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) r2;
	detstackvar(1) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	{
	Declare_entry(mercury__bintree_set__member_2_0);
	call_localret(ENTRY(mercury__bintree_set__member_2_0),
		mercury__list__remove_dups_2_3_0_i6,
		STATIC(mercury__list__remove_dups_2_3_0));
	}
Define_label(mercury__list__remove_dups_2_3_0_i6);
	update_prof_current_proc(LABEL(mercury__list__remove_dups_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__remove_dups_2_3_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__list__remove_dups_2_3_0,
		STATIC(mercury__list__remove_dups_2_3_0));
Define_label(mercury__list__remove_dups_2_3_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__bintree_set__insert_3_1);
	call_localret(ENTRY(mercury__bintree_set__insert_3_1),
		mercury__list__remove_dups_2_3_0_i9,
		STATIC(mercury__list__remove_dups_2_3_0));
	}
Define_label(mercury__list__remove_dups_2_3_0_i9);
	update_prof_current_proc(LABEL(mercury__list__remove_dups_2_3_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	localcall(mercury__list__remove_dups_2_3_0,
		LABEL(mercury__list__remove_dups_2_3_0_i10),
		STATIC(mercury__list__remove_dups_2_3_0));
Define_label(mercury__list__remove_dups_2_3_0_i10);
	update_prof_current_proc(LABEL(mercury__list__remove_dups_2_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__list__remove_dups_2_3_0_i1003);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module117)
	init_entry(mercury__list__remove_adjacent_dups_2_3_0);
	init_label(mercury__list__remove_adjacent_dups_2_3_0_i6);
	init_label(mercury__list__remove_adjacent_dups_2_3_0_i5);
	init_label(mercury__list__remove_adjacent_dups_2_3_0_i9);
	init_label(mercury__list__remove_adjacent_dups_2_3_0_i1005);
BEGIN_CODE

/* code for predicate 'list__remove_adjacent_dups_2'/3 in mode 0 */
Define_static(mercury__list__remove_adjacent_dups_2_3_0);
	incr_sp_push_msg(5, "list__remove_adjacent_dups_2");
	detstackvar(5) = (Integer) succip;
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__remove_adjacent_dups_2_3_0_i1005);
	{
	Word tempr1;
	tempr1 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	detstackvar(2) = (Integer) tempr1;
	detstackvar(1) = (Integer) r3;
	r2 = (Integer) r3;
	r3 = (Integer) tempr1;
	detstackvar(4) = (Integer) r1;
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__remove_adjacent_dups_2_3_0_i6,
		STATIC(mercury__list__remove_adjacent_dups_2_3_0));
	}
	}
Define_label(mercury__list__remove_adjacent_dups_2_3_0_i6);
	update_prof_current_proc(LABEL(mercury__list__remove_adjacent_dups_2_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__remove_adjacent_dups_2_3_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	localtailcall(mercury__list__remove_adjacent_dups_2_3_0,
		STATIC(mercury__list__remove_adjacent_dups_2_3_0));
Define_label(mercury__list__remove_adjacent_dups_2_3_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	localcall(mercury__list__remove_adjacent_dups_2_3_0,
		LABEL(mercury__list__remove_adjacent_dups_2_3_0_i9),
		STATIC(mercury__list__remove_adjacent_dups_2_3_0));
Define_label(mercury__list__remove_adjacent_dups_2_3_0_i9);
	update_prof_current_proc(LABEL(mercury__list__remove_adjacent_dups_2_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__list__remove_adjacent_dups_2_3_0_i1005);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module118)
	init_entry(mercury__list__all_same_2_2_0);
	init_label(mercury__list__all_same_2_2_0_i4);
	init_label(mercury__list__all_same_2_2_0_i1003);
	init_label(mercury__list__all_same_2_2_0_i1);
BEGIN_CODE

/* code for predicate 'list__all_same_2'/2 in mode 0 */
Define_static(mercury__list__all_same_2_2_0);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__list__all_same_2_2_0_i1003);
	incr_sp_push_msg(4, "list__all_same_2");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	detstackvar(3) = (Integer) r1;
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__unify_2_0);
	call_localret(ENTRY(mercury__unify_2_0),
		mercury__list__all_same_2_2_0_i4,
		STATIC(mercury__list__all_same_2_2_0));
	}
Define_label(mercury__list__all_same_2_2_0_i4);
	update_prof_current_proc(LABEL(mercury__list__all_same_2_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__all_same_2_2_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(1);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__list__all_same_2_2_0,
		STATIC(mercury__list__all_same_2_2_0));
Define_label(mercury__list__all_same_2_2_0_i1003);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__all_same_2_2_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module119)
	init_entry(mercury__list__apply_2_1);
	init_label(mercury__list__apply_2_1_i2);
	init_label(mercury__list__apply_2_1_i1000);
BEGIN_CODE

/* code for predicate 'list__apply'/2 in mode 1 */
Define_entry(mercury__list__apply_2_1);
	incr_sp_push_msg(2, "list__apply");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__apply__ua1_2_0),
		mercury__list__apply_2_1_i2,
		ENTRY(mercury__list__apply_2_1));
Define_label(mercury__list__apply_2_1_i2);
	update_prof_current_proc(LABEL(mercury__list__apply_2_1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (!((Integer) r1))
		GOTO_LABEL(mercury__list__apply_2_1_i1000);
	r1 = TRUE;
	proceed();
Define_label(mercury__list__apply_2_1_i1000);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__list_module120)
	init_entry(mercury__list__apply_2_0);
BEGIN_CODE

/* code for predicate 'list__apply'/2 in mode 0 */
Define_entry(mercury__list__apply_2_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__list__apply__ua10000_2_0),
		ENTRY(mercury__list__apply_2_0));
END_MODULE

BEGIN_MODULE(mercury__list_module121)
	init_entry(mercury__list__apply_2_3);
	init_label(mercury__list__apply_2_3_i1);
BEGIN_CODE

/* code for predicate 'list__apply'/2 in mode 3 */
Define_entry(mercury__list__apply_2_3);
	{
	Declare_entry(do_fail);
	mkframe("list__apply/2", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__apply__ua40003_2_0),
		mercury__list__apply_2_3_i1,
		ENTRY(mercury__list__apply_2_3));
Define_label(mercury__list__apply_2_3_i1);
	update_prof_current_proc(LABEL(mercury__list__apply_2_3));
	succeed();
END_MODULE

BEGIN_MODULE(mercury__list_module122)
	init_entry(mercury__list__apply_2_2);
	init_label(mercury__list__apply_2_2_i1);
BEGIN_CODE

/* code for predicate 'list__apply'/2 in mode 2 */
Define_entry(mercury__list__apply_2_2);
	{
	Declare_entry(do_fail);
	mkframe("list__apply/2", 1, ENTRY(do_fail));
	}
	framevar(0) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__list__apply__ua60002_2_0),
		mercury__list__apply_2_2_i1,
		ENTRY(mercury__list__apply_2_2));
Define_label(mercury__list__apply_2_2_i1);
	update_prof_current_proc(LABEL(mercury__list__apply_2_2));
	succeed();
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__list_bunch_0(void)
{
	mercury__list_module0();
	mercury__list_module1();
	mercury__list_module2();
	mercury__list_module3();
	mercury__list_module4();
	mercury__list_module5();
	mercury__list_module6();
	mercury__list_module7();
	mercury__list_module8();
	mercury__list_module9();
	mercury__list_module10();
	mercury__list_module11();
	mercury__list_module12();
	mercury__list_module13();
	mercury__list_module14();
	mercury__list_module15();
	mercury__list_module16();
	mercury__list_module17();
	mercury__list_module18();
	mercury__list_module19();
	mercury__list_module20();
	mercury__list_module21();
	mercury__list_module22();
	mercury__list_module23();
	mercury__list_module24();
	mercury__list_module25();
	mercury__list_module26();
	mercury__list_module27();
	mercury__list_module28();
	mercury__list_module29();
	mercury__list_module30();
	mercury__list_module31();
	mercury__list_module32();
	mercury__list_module33();
	mercury__list_module34();
	mercury__list_module35();
	mercury__list_module36();
	mercury__list_module37();
	mercury__list_module38();
	mercury__list_module39();
	mercury__list_module40();
}

static void mercury__list_bunch_1(void)
{
	mercury__list_module41();
	mercury__list_module42();
	mercury__list_module43();
	mercury__list_module44();
	mercury__list_module45();
	mercury__list_module46();
	mercury__list_module47();
	mercury__list_module48();
	mercury__list_module49();
	mercury__list_module50();
	mercury__list_module51();
	mercury__list_module52();
	mercury__list_module53();
	mercury__list_module54();
	mercury__list_module55();
	mercury__list_module56();
	mercury__list_module57();
	mercury__list_module58();
	mercury__list_module59();
	mercury__list_module60();
	mercury__list_module61();
	mercury__list_module62();
	mercury__list_module63();
	mercury__list_module64();
	mercury__list_module65();
	mercury__list_module66();
	mercury__list_module67();
	mercury__list_module68();
	mercury__list_module69();
	mercury__list_module70();
	mercury__list_module71();
	mercury__list_module72();
	mercury__list_module73();
	mercury__list_module74();
	mercury__list_module75();
	mercury__list_module76();
	mercury__list_module77();
	mercury__list_module78();
	mercury__list_module79();
	mercury__list_module80();
	mercury__list_module81();
}

static void mercury__list_bunch_2(void)
{
	mercury__list_module82();
	mercury__list_module83();
	mercury__list_module84();
	mercury__list_module85();
	mercury__list_module86();
	mercury__list_module87();
	mercury__list_module88();
	mercury__list_module89();
	mercury__list_module90();
	mercury__list_module91();
	mercury__list_module92();
	mercury__list_module93();
	mercury__list_module94();
	mercury__list_module95();
	mercury__list_module96();
	mercury__list_module97();
	mercury__list_module98();
	mercury__list_module99();
	mercury__list_module100();
	mercury__list_module101();
	mercury__list_module102();
	mercury__list_module103();
	mercury__list_module104();
	mercury__list_module105();
	mercury__list_module106();
	mercury__list_module107();
	mercury__list_module108();
	mercury__list_module109();
	mercury__list_module110();
	mercury__list_module111();
	mercury__list_module112();
	mercury__list_module113();
	mercury__list_module114();
	mercury__list_module115();
	mercury__list_module116();
	mercury__list_module117();
	mercury__list_module118();
	mercury__list_module119();
	mercury__list_module120();
	mercury__list_module121();
	mercury__list_module122();
}

#endif

void mercury__list__init(void); /* suppress gcc warning */
void mercury__list__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__list_bunch_0();
	mercury__list_bunch_1();
	mercury__list_bunch_2();
#endif
}
