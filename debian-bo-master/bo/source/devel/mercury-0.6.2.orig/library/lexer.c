/*
** Automatically generated from `lexer.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__lexer__init
ENDINIT
*/

#include "imp.h"

Define_extern_entry(mercury__lexer__get_token_list_3_0);
Declare_label(mercury__lexer__get_token_list_3_0_i2);
Declare_label(mercury__lexer__get_token_list_3_0_i3);
Declare_label(mercury__lexer__get_token_list_3_0_i4);
Declare_label(mercury__lexer__get_token_list_3_0_i12);
Declare_label(mercury__lexer__get_token_list_3_0_i11);
Declare_label(mercury__lexer__get_token_list_3_0_i9);
Declare_label(mercury__lexer__get_token_list_3_0_i7);
Declare_label(mercury__lexer__get_token_list_3_0_i16);
Define_extern_entry(mercury__lexer__token_to_string_2_0);
Declare_label(mercury__lexer__token_to_string_2_0_i1018);
Declare_label(mercury__lexer__token_to_string_2_0_i1019);
Declare_label(mercury__lexer__token_to_string_2_0_i1020);
Declare_label(mercury__lexer__token_to_string_2_0_i1021);
Declare_label(mercury__lexer__token_to_string_2_0_i1022);
Declare_label(mercury__lexer__token_to_string_2_0_i1023);
Declare_label(mercury__lexer__token_to_string_2_0_i1024);
Declare_label(mercury__lexer__token_to_string_2_0_i1025);
Declare_label(mercury__lexer__token_to_string_2_0_i1026);
Declare_label(mercury__lexer__token_to_string_2_0_i1027);
Declare_label(mercury__lexer__token_to_string_2_0_i1028);
Declare_label(mercury__lexer__token_to_string_2_0_i1041);
Declare_label(mercury__lexer__token_to_string_2_0_i17);
Declare_label(mercury__lexer__token_to_string_2_0_i18);
Declare_label(mercury__lexer__token_to_string_2_0_i20);
Declare_label(mercury__lexer__token_to_string_2_0_i21);
Declare_label(mercury__lexer__token_to_string_2_0_i23);
Declare_label(mercury__lexer__token_to_string_2_0_i25);
Declare_label(mercury__lexer__token_to_string_2_0_i26);
Declare_label(mercury__lexer__token_to_string_2_0_i27);
Declare_label(mercury__lexer__token_to_string_2_0_i29);
Declare_label(mercury__lexer__token_to_string_2_0_i31);
Declare_label(mercury__lexer__token_to_string_2_0_i32);
Declare_label(mercury__lexer__token_to_string_2_0_i16);
Declare_label(mercury__lexer__token_to_string_2_0_i34);
Declare_static(mercury__lexer__get_token_1_3_0);
Declare_label(mercury__lexer__get_token_1_3_0_i2);
Declare_label(mercury__lexer__get_token_1_3_0_i6);
Declare_label(mercury__lexer__get_token_1_3_0_i5);
Declare_label(mercury__lexer__get_token_1_3_0_i9);
Declare_label(mercury__lexer__get_token_1_3_0_i17);
Declare_label(mercury__lexer__get_token_1_3_0_i45);
Declare_label(mercury__lexer__get_token_1_3_0_i46);
Declare_label(mercury__lexer__get_token_1_3_0_i48);
Declare_label(mercury__lexer__get_token_1_3_0_i50);
Declare_label(mercury__lexer__get_token_1_3_0_i52);
Declare_label(mercury__lexer__get_token_1_3_0_i54);
Declare_label(mercury__lexer__get_token_1_3_0_i58);
Declare_label(mercury__lexer__get_token_1_3_0_i59);
Declare_label(mercury__lexer__get_token_1_3_0_i60);
Declare_label(mercury__lexer__get_token_1_3_0_i62);
Declare_label(mercury__lexer__get_token_1_3_0_i64);
Declare_label(mercury__lexer__get_token_1_3_0_i65);
Declare_label(mercury__lexer__get_token_1_3_0_i67);
Declare_label(mercury__lexer__get_token_1_3_0_i69);
Declare_label(mercury__lexer__get_token_1_3_0_i71);
Declare_label(mercury__lexer__get_token_1_3_0_i73);
Declare_label(mercury__lexer__get_token_1_3_0_i75);
Declare_label(mercury__lexer__get_token_1_3_0_i77);
Declare_label(mercury__lexer__get_token_1_3_0_i79);
Declare_label(mercury__lexer__get_token_1_3_0_i81);
Declare_label(mercury__lexer__get_token_1_3_0_i83);
Declare_label(mercury__lexer__get_token_1_3_0_i85);
Declare_label(mercury__lexer__get_token_1_3_0_i87);
Declare_label(mercury__lexer__get_token_1_3_0_i89);
Declare_label(mercury__lexer__get_token_1_3_0_i91);
Declare_label(mercury__lexer__get_token_1_3_0_i93);
Declare_label(mercury__lexer__get_token_1_3_0_i94);
Declare_label(mercury__lexer__get_token_1_3_0_i96);
Declare_label(mercury__lexer__get_token_1_3_0_i98);
Declare_label(mercury__lexer__get_token_1_3_0_i100);
Declare_label(mercury__lexer__get_token_1_3_0_i102);
Declare_label(mercury__lexer__get_token_1_3_0_i104);
Declare_label(mercury__lexer__get_token_1_3_0_i106);
Declare_label(mercury__lexer__get_token_1_3_0_i108);
Declare_label(mercury__lexer__get_token_1_3_0_i110);
Declare_label(mercury__lexer__get_token_1_3_0_i112);
Declare_label(mercury__lexer__get_token_1_3_0_i114);
Declare_label(mercury__lexer__get_token_1_3_0_i116);
Declare_label(mercury__lexer__get_token_1_3_0_i118);
Declare_label(mercury__lexer__get_token_1_3_0_i120);
Declare_label(mercury__lexer__get_token_1_3_0_i122);
Declare_label(mercury__lexer__get_token_1_3_0_i124);
Declare_label(mercury__lexer__get_token_1_3_0_i126);
Declare_label(mercury__lexer__get_token_1_3_0_i128);
Declare_label(mercury__lexer__get_token_1_3_0_i130);
Declare_label(mercury__lexer__get_token_1_3_0_i132);
Declare_label(mercury__lexer__get_token_1_3_0_i134);
Declare_label(mercury__lexer__get_token_1_3_0_i136);
Declare_label(mercury__lexer__get_token_1_3_0_i138);
Declare_label(mercury__lexer__get_token_1_3_0_i140);
Declare_label(mercury__lexer__get_token_1_3_0_i142);
Declare_label(mercury__lexer__get_token_1_3_0_i144);
Declare_label(mercury__lexer__get_token_1_3_0_i146);
Declare_label(mercury__lexer__get_token_1_3_0_i148);
Declare_label(mercury__lexer__get_token_1_3_0_i150);
Declare_label(mercury__lexer__get_token_1_3_0_i152);
Declare_label(mercury__lexer__get_token_1_3_0_i154);
Declare_label(mercury__lexer__get_token_1_3_0_i156);
Declare_label(mercury__lexer__get_token_1_3_0_i157);
Declare_label(mercury__lexer__get_token_1_3_0_i159);
Declare_label(mercury__lexer__get_token_1_3_0_i160);
Declare_label(mercury__lexer__get_token_1_3_0_i162);
Declare_label(mercury__lexer__get_token_1_3_0_i165);
Declare_label(mercury__lexer__get_token_1_3_0_i167);
Declare_label(mercury__lexer__get_token_1_3_0_i169);
Declare_label(mercury__lexer__get_token_1_3_0_i171);
Declare_label(mercury__lexer__get_token_1_3_0_i173);
Declare_label(mercury__lexer__get_token_1_3_0_i175);
Declare_label(mercury__lexer__get_token_1_3_0_i177);
Declare_label(mercury__lexer__get_token_1_3_0_i179);
Declare_label(mercury__lexer__get_token_1_3_0_i181);
Declare_label(mercury__lexer__get_token_1_3_0_i183);
Declare_label(mercury__lexer__get_token_1_3_0_i185);
Declare_label(mercury__lexer__get_token_1_3_0_i187);
Declare_label(mercury__lexer__get_token_1_3_0_i189);
Declare_label(mercury__lexer__get_token_1_3_0_i191);
Declare_label(mercury__lexer__get_token_1_3_0_i193);
Declare_label(mercury__lexer__get_token_1_3_0_i195);
Declare_label(mercury__lexer__get_token_1_3_0_i197);
Declare_label(mercury__lexer__get_token_1_3_0_i199);
Declare_label(mercury__lexer__get_token_1_3_0_i201);
Declare_label(mercury__lexer__get_token_1_3_0_i203);
Declare_label(mercury__lexer__get_token_1_3_0_i205);
Declare_label(mercury__lexer__get_token_1_3_0_i207);
Declare_label(mercury__lexer__get_token_1_3_0_i209);
Declare_label(mercury__lexer__get_token_1_3_0_i211);
Declare_label(mercury__lexer__get_token_1_3_0_i213);
Declare_label(mercury__lexer__get_token_1_3_0_i215);
Declare_label(mercury__lexer__get_token_1_3_0_i217);
Declare_label(mercury__lexer__get_token_1_3_0_i218);
Declare_label(mercury__lexer__get_token_1_3_0_i219);
Declare_label(mercury__lexer__get_token_1_3_0_i220);
Declare_label(mercury__lexer__get_token_1_3_0_i7);
Declare_label(mercury__lexer__get_token_1_3_0_i223);
Declare_static(mercury__lexer__get_token_2_3_0);
Declare_label(mercury__lexer__get_token_2_3_0_i2);
Declare_label(mercury__lexer__get_token_2_3_0_i6);
Declare_label(mercury__lexer__get_token_2_3_0_i5);
Declare_label(mercury__lexer__get_token_2_3_0_i9);
Declare_label(mercury__lexer__get_token_2_3_0_i17);
Declare_label(mercury__lexer__get_token_2_3_0_i45);
Declare_label(mercury__lexer__get_token_2_3_0_i46);
Declare_label(mercury__lexer__get_token_2_3_0_i48);
Declare_label(mercury__lexer__get_token_2_3_0_i50);
Declare_label(mercury__lexer__get_token_2_3_0_i52);
Declare_label(mercury__lexer__get_token_2_3_0_i54);
Declare_label(mercury__lexer__get_token_2_3_0_i58);
Declare_label(mercury__lexer__get_token_2_3_0_i59);
Declare_label(mercury__lexer__get_token_2_3_0_i60);
Declare_label(mercury__lexer__get_token_2_3_0_i62);
Declare_label(mercury__lexer__get_token_2_3_0_i64);
Declare_label(mercury__lexer__get_token_2_3_0_i65);
Declare_label(mercury__lexer__get_token_2_3_0_i67);
Declare_label(mercury__lexer__get_token_2_3_0_i69);
Declare_label(mercury__lexer__get_token_2_3_0_i71);
Declare_label(mercury__lexer__get_token_2_3_0_i73);
Declare_label(mercury__lexer__get_token_2_3_0_i75);
Declare_label(mercury__lexer__get_token_2_3_0_i77);
Declare_label(mercury__lexer__get_token_2_3_0_i79);
Declare_label(mercury__lexer__get_token_2_3_0_i81);
Declare_label(mercury__lexer__get_token_2_3_0_i83);
Declare_label(mercury__lexer__get_token_2_3_0_i85);
Declare_label(mercury__lexer__get_token_2_3_0_i87);
Declare_label(mercury__lexer__get_token_2_3_0_i89);
Declare_label(mercury__lexer__get_token_2_3_0_i91);
Declare_label(mercury__lexer__get_token_2_3_0_i93);
Declare_label(mercury__lexer__get_token_2_3_0_i94);
Declare_label(mercury__lexer__get_token_2_3_0_i96);
Declare_label(mercury__lexer__get_token_2_3_0_i98);
Declare_label(mercury__lexer__get_token_2_3_0_i100);
Declare_label(mercury__lexer__get_token_2_3_0_i102);
Declare_label(mercury__lexer__get_token_2_3_0_i104);
Declare_label(mercury__lexer__get_token_2_3_0_i106);
Declare_label(mercury__lexer__get_token_2_3_0_i108);
Declare_label(mercury__lexer__get_token_2_3_0_i110);
Declare_label(mercury__lexer__get_token_2_3_0_i112);
Declare_label(mercury__lexer__get_token_2_3_0_i114);
Declare_label(mercury__lexer__get_token_2_3_0_i116);
Declare_label(mercury__lexer__get_token_2_3_0_i118);
Declare_label(mercury__lexer__get_token_2_3_0_i120);
Declare_label(mercury__lexer__get_token_2_3_0_i122);
Declare_label(mercury__lexer__get_token_2_3_0_i124);
Declare_label(mercury__lexer__get_token_2_3_0_i126);
Declare_label(mercury__lexer__get_token_2_3_0_i128);
Declare_label(mercury__lexer__get_token_2_3_0_i130);
Declare_label(mercury__lexer__get_token_2_3_0_i132);
Declare_label(mercury__lexer__get_token_2_3_0_i134);
Declare_label(mercury__lexer__get_token_2_3_0_i136);
Declare_label(mercury__lexer__get_token_2_3_0_i138);
Declare_label(mercury__lexer__get_token_2_3_0_i140);
Declare_label(mercury__lexer__get_token_2_3_0_i142);
Declare_label(mercury__lexer__get_token_2_3_0_i144);
Declare_label(mercury__lexer__get_token_2_3_0_i146);
Declare_label(mercury__lexer__get_token_2_3_0_i148);
Declare_label(mercury__lexer__get_token_2_3_0_i150);
Declare_label(mercury__lexer__get_token_2_3_0_i152);
Declare_label(mercury__lexer__get_token_2_3_0_i154);
Declare_label(mercury__lexer__get_token_2_3_0_i156);
Declare_label(mercury__lexer__get_token_2_3_0_i157);
Declare_label(mercury__lexer__get_token_2_3_0_i159);
Declare_label(mercury__lexer__get_token_2_3_0_i160);
Declare_label(mercury__lexer__get_token_2_3_0_i162);
Declare_label(mercury__lexer__get_token_2_3_0_i165);
Declare_label(mercury__lexer__get_token_2_3_0_i167);
Declare_label(mercury__lexer__get_token_2_3_0_i169);
Declare_label(mercury__lexer__get_token_2_3_0_i171);
Declare_label(mercury__lexer__get_token_2_3_0_i173);
Declare_label(mercury__lexer__get_token_2_3_0_i175);
Declare_label(mercury__lexer__get_token_2_3_0_i177);
Declare_label(mercury__lexer__get_token_2_3_0_i179);
Declare_label(mercury__lexer__get_token_2_3_0_i181);
Declare_label(mercury__lexer__get_token_2_3_0_i183);
Declare_label(mercury__lexer__get_token_2_3_0_i185);
Declare_label(mercury__lexer__get_token_2_3_0_i187);
Declare_label(mercury__lexer__get_token_2_3_0_i189);
Declare_label(mercury__lexer__get_token_2_3_0_i191);
Declare_label(mercury__lexer__get_token_2_3_0_i193);
Declare_label(mercury__lexer__get_token_2_3_0_i195);
Declare_label(mercury__lexer__get_token_2_3_0_i197);
Declare_label(mercury__lexer__get_token_2_3_0_i199);
Declare_label(mercury__lexer__get_token_2_3_0_i201);
Declare_label(mercury__lexer__get_token_2_3_0_i203);
Declare_label(mercury__lexer__get_token_2_3_0_i205);
Declare_label(mercury__lexer__get_token_2_3_0_i207);
Declare_label(mercury__lexer__get_token_2_3_0_i209);
Declare_label(mercury__lexer__get_token_2_3_0_i211);
Declare_label(mercury__lexer__get_token_2_3_0_i213);
Declare_label(mercury__lexer__get_token_2_3_0_i215);
Declare_label(mercury__lexer__get_token_2_3_0_i217);
Declare_label(mercury__lexer__get_token_2_3_0_i218);
Declare_label(mercury__lexer__get_token_2_3_0_i219);
Declare_label(mercury__lexer__get_token_2_3_0_i220);
Declare_label(mercury__lexer__get_token_2_3_0_i7);
Declare_label(mercury__lexer__get_token_2_3_0_i223);
Declare_static(mercury__lexer__graphic_token_char_1_0);
Declare_label(mercury__lexer__graphic_token_char_1_0_i2);
Declare_label(mercury__lexer__graphic_token_char_1_0_i1);
Declare_static(mercury__lexer__get_dot_3_0);
Declare_label(mercury__lexer__get_dot_3_0_i2);
Declare_label(mercury__lexer__get_dot_3_0_i6);
Declare_label(mercury__lexer__get_dot_3_0_i5);
Declare_label(mercury__lexer__get_dot_3_0_i11);
Declare_label(mercury__lexer__get_dot_3_0_i12);
Declare_label(mercury__lexer__get_dot_3_0_i13);
Declare_label(mercury__lexer__get_dot_3_0_i8);
Declare_label(mercury__lexer__get_dot_3_0_i18);
Declare_label(mercury__lexer__get_dot_3_0_i17);
Declare_label(mercury__lexer__get_dot_3_0_i21);
Declare_label(mercury__lexer__get_dot_3_0_i7);
Declare_label(mercury__lexer__get_dot_3_0_i24);
Declare_static(mercury__lexer__skip_to_eol_3_0);
Declare_label(mercury__lexer__skip_to_eol_3_0_i2);
Declare_label(mercury__lexer__skip_to_eol_3_0_i6);
Declare_label(mercury__lexer__skip_to_eol_3_0_i5);
Declare_label(mercury__lexer__skip_to_eol_3_0_i7);
Declare_label(mercury__lexer__skip_to_eol_3_0_i14);
Declare_label(mercury__lexer__skip_to_eol_3_0_i1001);
Declare_static(mercury__lexer__get_slash_3_0);
Declare_label(mercury__lexer__get_slash_3_0_i2);
Declare_label(mercury__lexer__get_slash_3_0_i6);
Declare_label(mercury__lexer__get_slash_3_0_i5);
Declare_label(mercury__lexer__get_slash_3_0_i8);
Declare_label(mercury__lexer__get_slash_3_0_i14);
Declare_label(mercury__lexer__get_slash_3_0_i13);
Declare_label(mercury__lexer__get_slash_3_0_i7);
Declare_label(mercury__lexer__get_slash_3_0_i20);
Declare_static(mercury__lexer__get_comment_3_0);
Declare_label(mercury__lexer__get_comment_3_0_i2);
Declare_label(mercury__lexer__get_comment_3_0_i6);
Declare_label(mercury__lexer__get_comment_3_0_i5);
Declare_label(mercury__lexer__get_comment_3_0_i7);
Declare_label(mercury__lexer__get_comment_3_0_i14);
Declare_label(mercury__lexer__get_comment_3_0_i1001);
Declare_static(mercury__lexer__get_comment_2_3_0);
Declare_label(mercury__lexer__get_comment_2_3_0_i2);
Declare_label(mercury__lexer__get_comment_2_3_0_i6);
Declare_label(mercury__lexer__get_comment_2_3_0_i5);
Declare_label(mercury__lexer__get_comment_2_3_0_i8);
Declare_label(mercury__lexer__get_comment_2_3_0_i7);
Declare_label(mercury__lexer__get_comment_2_3_0_i19);
Declare_label(mercury__lexer__get_comment_2_3_0_i1001);
Declare_static(mercury__lexer__get_quoted_name_5_0);
Declare_label(mercury__lexer__get_quoted_name_5_0_i2);
Declare_label(mercury__lexer__get_quoted_name_5_0_i6);
Declare_label(mercury__lexer__get_quoted_name_5_0_i5);
Declare_label(mercury__lexer__get_quoted_name_5_0_i8);
Declare_label(mercury__lexer__get_quoted_name_5_0_i11);
Declare_label(mercury__lexer__get_quoted_name_5_0_i7);
Declare_label(mercury__lexer__get_quoted_name_5_0_i18);
Declare_static(mercury__lexer__get_quoted_name_quote_5_0);
Declare_label(mercury__lexer__get_quoted_name_quote_5_0_i2);
Declare_label(mercury__lexer__get_quoted_name_quote_5_0_i6);
Declare_label(mercury__lexer__get_quoted_name_quote_5_0_i7);
Declare_label(mercury__lexer__get_quoted_name_quote_5_0_i5);
Declare_label(mercury__lexer__get_quoted_name_quote_5_0_i9);
Declare_label(mercury__lexer__get_quoted_name_quote_5_0_i8);
Declare_label(mercury__lexer__get_quoted_name_quote_5_0_i15);
Declare_static(mercury__lexer__finish_quoted_name_3_0);
Declare_label(mercury__lexer__finish_quoted_name_3_0_i2);
Declare_label(mercury__lexer__finish_quoted_name_3_0_i3);
Declare_label(mercury__lexer__finish_quoted_name_3_0_i6);
Declare_static(mercury__lexer__get_quoted_name_escape_5_0);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i2);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i6);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i5);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i8);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i9);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i16);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i17);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i18);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i19);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i20);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i21);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i22);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i23);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i24);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i25);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i13);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i28);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i34);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i33);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i7);
Declare_label(mercury__lexer__get_quoted_name_escape_5_0_i41);
Declare_static(mercury__lexer__get_hex_escape_6_0);
Declare_label(mercury__lexer__get_hex_escape_6_0_i2);
Declare_label(mercury__lexer__get_hex_escape_6_0_i6);
Declare_label(mercury__lexer__get_hex_escape_6_0_i5);
Declare_label(mercury__lexer__get_hex_escape_6_0_i8);
Declare_label(mercury__lexer__get_hex_escape_6_0_i11);
Declare_label(mercury__lexer__get_hex_escape_6_0_i10);
Declare_label(mercury__lexer__get_hex_escape_6_0_i14);
Declare_label(mercury__lexer__get_hex_escape_6_0_i7);
Declare_label(mercury__lexer__get_hex_escape_6_0_i20);
Declare_static(mercury__lexer__finish_hex_escape_6_0);
Declare_label(mercury__lexer__finish_hex_escape_6_0_i1000);
Declare_label(mercury__lexer__finish_hex_escape_6_0_i5);
Declare_label(mercury__lexer__finish_hex_escape_6_0_i8);
Declare_label(mercury__lexer__finish_hex_escape_6_0_i10);
Declare_label(mercury__lexer__finish_hex_escape_6_0_i7);
Declare_static(mercury__lexer__get_octal_escape_6_0);
Declare_label(mercury__lexer__get_octal_escape_6_0_i2);
Declare_label(mercury__lexer__get_octal_escape_6_0_i6);
Declare_label(mercury__lexer__get_octal_escape_6_0_i5);
Declare_label(mercury__lexer__get_octal_escape_6_0_i8);
Declare_label(mercury__lexer__get_octal_escape_6_0_i11);
Declare_label(mercury__lexer__get_octal_escape_6_0_i10);
Declare_label(mercury__lexer__get_octal_escape_6_0_i14);
Declare_label(mercury__lexer__get_octal_escape_6_0_i18);
Declare_label(mercury__lexer__get_octal_escape_6_0_i7);
Declare_label(mercury__lexer__get_octal_escape_6_0_i22);
Declare_static(mercury__lexer__finish_octal_escape_6_0);
Declare_label(mercury__lexer__finish_octal_escape_6_0_i1000);
Declare_label(mercury__lexer__finish_octal_escape_6_0_i5);
Declare_label(mercury__lexer__finish_octal_escape_6_0_i8);
Declare_label(mercury__lexer__finish_octal_escape_6_0_i10);
Declare_label(mercury__lexer__finish_octal_escape_6_0_i7);
Declare_static(mercury__lexer__get_name_4_0);
Declare_label(mercury__lexer__get_name_4_0_i2);
Declare_label(mercury__lexer__get_name_4_0_i6);
Declare_label(mercury__lexer__get_name_4_0_i7);
Declare_label(mercury__lexer__get_name_4_0_i5);
Declare_label(mercury__lexer__get_name_4_0_i11);
Declare_label(mercury__lexer__get_name_4_0_i10);
Declare_label(mercury__lexer__get_name_4_0_i8);
Declare_label(mercury__lexer__get_name_4_0_i17);
Declare_static(mercury__lexer__get_source_line_number_4_0);
Declare_label(mercury__lexer__get_source_line_number_4_0_i2);
Declare_label(mercury__lexer__get_source_line_number_4_0_i6);
Declare_label(mercury__lexer__get_source_line_number_4_0_i5);
Declare_label(mercury__lexer__get_source_line_number_4_0_i10);
Declare_label(mercury__lexer__get_source_line_number_4_0_i9);
Declare_label(mercury__lexer__get_source_line_number_4_0_i16);
Declare_label(mercury__lexer__get_source_line_number_4_0_i19);
Declare_label(mercury__lexer__get_source_line_number_4_0_i21);
Declare_label(mercury__lexer__get_source_line_number_4_0_i1013);
Declare_label(mercury__lexer__get_source_line_number_4_0_i23);
Declare_label(mercury__lexer__get_source_line_number_4_0_i13);
Declare_label(mercury__lexer__get_source_line_number_4_0_i25);
Declare_label(mercury__lexer__get_source_line_number_4_0_i7);
Declare_label(mercury__lexer__get_source_line_number_4_0_i29);
Declare_static(mercury__lexer__get_graphic_4_0);
Declare_label(mercury__lexer__get_graphic_4_0_i2);
Declare_label(mercury__lexer__get_graphic_4_0_i6);
Declare_label(mercury__lexer__get_graphic_4_0_i7);
Declare_label(mercury__lexer__get_graphic_4_0_i5);
Declare_label(mercury__lexer__get_graphic_4_0_i11);
Declare_label(mercury__lexer__get_graphic_4_0_i10);
Declare_label(mercury__lexer__get_graphic_4_0_i8);
Declare_label(mercury__lexer__get_graphic_4_0_i17);
Declare_static(mercury__lexer__get_variable_4_0);
Declare_label(mercury__lexer__get_variable_4_0_i2);
Declare_label(mercury__lexer__get_variable_4_0_i6);
Declare_label(mercury__lexer__get_variable_4_0_i7);
Declare_label(mercury__lexer__get_variable_4_0_i5);
Declare_label(mercury__lexer__get_variable_4_0_i11);
Declare_label(mercury__lexer__get_variable_4_0_i10);
Declare_label(mercury__lexer__get_variable_4_0_i8);
Declare_label(mercury__lexer__get_variable_4_0_i17);
Declare_static(mercury__lexer__get_zero_3_0);
Declare_label(mercury__lexer__get_zero_3_0_i2);
Declare_label(mercury__lexer__get_zero_3_0_i6);
Declare_label(mercury__lexer__get_zero_3_0_i5);
Declare_label(mercury__lexer__get_zero_3_0_i10);
Declare_label(mercury__lexer__get_zero_3_0_i9);
Declare_label(mercury__lexer__get_zero_3_0_i16);
Declare_label(mercury__lexer__get_zero_3_0_i20);
Declare_label(mercury__lexer__get_zero_3_0_i19);
Declare_label(mercury__lexer__get_zero_3_0_i22);
Declare_label(mercury__lexer__get_zero_3_0_i21);
Declare_label(mercury__lexer__get_zero_3_0_i23);
Declare_label(mercury__lexer__get_zero_3_0_i13);
Declare_label(mercury__lexer__get_zero_3_0_i27);
Declare_label(mercury__lexer__get_zero_3_0_i31);
Declare_label(mercury__lexer__get_zero_3_0_i30);
Declare_label(mercury__lexer__get_zero_3_0_i35);
Declare_label(mercury__lexer__get_zero_3_0_i34);
Declare_label(mercury__lexer__get_zero_3_0_i32);
Declare_label(mercury__lexer__get_zero_3_0_i24);
Declare_label(mercury__lexer__get_zero_3_0_i44);
Declare_label(mercury__lexer__get_zero_3_0_i48);
Declare_label(mercury__lexer__get_zero_3_0_i47);
Declare_label(mercury__lexer__get_zero_3_0_i52);
Declare_label(mercury__lexer__get_zero_3_0_i51);
Declare_label(mercury__lexer__get_zero_3_0_i41);
Declare_label(mercury__lexer__get_zero_3_0_i61);
Declare_label(mercury__lexer__get_zero_3_0_i65);
Declare_label(mercury__lexer__get_zero_3_0_i64);
Declare_label(mercury__lexer__get_zero_3_0_i69);
Declare_label(mercury__lexer__get_zero_3_0_i68);
Declare_label(mercury__lexer__get_zero_3_0_i58);
Declare_label(mercury__lexer__get_zero_3_0_i75);
Declare_label(mercury__lexer__get_zero_3_0_i82);
Declare_label(mercury__lexer__get_zero_3_0_i81);
Declare_label(mercury__lexer__get_zero_3_0_i79);
Declare_label(mercury__lexer__get_zero_3_0_i7);
Declare_label(mercury__lexer__get_zero_3_0_i93);
Declare_static(mercury__lexer__get_binary_2_4_0);
Declare_label(mercury__lexer__get_binary_2_4_0_i2);
Declare_label(mercury__lexer__get_binary_2_4_0_i6);
Declare_label(mercury__lexer__get_binary_2_4_0_i7);
Declare_label(mercury__lexer__get_binary_2_4_0_i5);
Declare_label(mercury__lexer__get_binary_2_4_0_i11);
Declare_label(mercury__lexer__get_binary_2_4_0_i10);
Declare_label(mercury__lexer__get_binary_2_4_0_i14);
Declare_label(mercury__lexer__get_binary_2_4_0_i8);
Declare_label(mercury__lexer__get_binary_2_4_0_i17);
Declare_static(mercury__lexer__get_octal_2_4_0);
Declare_label(mercury__lexer__get_octal_2_4_0_i2);
Declare_label(mercury__lexer__get_octal_2_4_0_i6);
Declare_label(mercury__lexer__get_octal_2_4_0_i7);
Declare_label(mercury__lexer__get_octal_2_4_0_i5);
Declare_label(mercury__lexer__get_octal_2_4_0_i11);
Declare_label(mercury__lexer__get_octal_2_4_0_i10);
Declare_label(mercury__lexer__get_octal_2_4_0_i14);
Declare_label(mercury__lexer__get_octal_2_4_0_i8);
Declare_label(mercury__lexer__get_octal_2_4_0_i17);
Declare_static(mercury__lexer__get_hex_2_4_0);
Declare_label(mercury__lexer__get_hex_2_4_0_i2);
Declare_label(mercury__lexer__get_hex_2_4_0_i6);
Declare_label(mercury__lexer__get_hex_2_4_0_i7);
Declare_label(mercury__lexer__get_hex_2_4_0_i5);
Declare_label(mercury__lexer__get_hex_2_4_0_i11);
Declare_label(mercury__lexer__get_hex_2_4_0_i10);
Declare_label(mercury__lexer__get_hex_2_4_0_i14);
Declare_label(mercury__lexer__get_hex_2_4_0_i8);
Declare_label(mercury__lexer__get_hex_2_4_0_i17);
Declare_static(mercury__lexer__get_number_4_0);
Declare_label(mercury__lexer__get_number_4_0_i2);
Declare_label(mercury__lexer__get_number_4_0_i6);
Declare_label(mercury__lexer__get_number_4_0_i7);
Declare_label(mercury__lexer__get_number_4_0_i5);
Declare_label(mercury__lexer__get_number_4_0_i11);
Declare_label(mercury__lexer__get_number_4_0_i10);
Declare_label(mercury__lexer__get_number_4_0_i14);
Declare_label(mercury__lexer__get_number_4_0_i21);
Declare_label(mercury__lexer__get_number_4_0_i20);
Declare_label(mercury__lexer__get_number_4_0_i18);
Declare_label(mercury__lexer__get_number_4_0_i24);
Declare_label(mercury__lexer__get_number_4_0_i8);
Declare_label(mercury__lexer__get_number_4_0_i29);
Declare_static(mercury__lexer__get_int_dot_4_0);
Declare_label(mercury__lexer__get_int_dot_4_0_i2);
Declare_label(mercury__lexer__get_int_dot_4_0_i6);
Declare_label(mercury__lexer__get_int_dot_4_0_i7);
Declare_label(mercury__lexer__get_int_dot_4_0_i8);
Declare_label(mercury__lexer__get_int_dot_4_0_i5);
Declare_label(mercury__lexer__get_int_dot_4_0_i12);
Declare_label(mercury__lexer__get_int_dot_4_0_i11);
Declare_label(mercury__lexer__get_int_dot_4_0_i15);
Declare_label(mercury__lexer__get_int_dot_4_0_i16);
Declare_label(mercury__lexer__get_int_dot_4_0_i9);
Declare_label(mercury__lexer__get_int_dot_4_0_i19);
Declare_static(mercury__lexer__get_float_decimals_4_0);
Declare_label(mercury__lexer__get_float_decimals_4_0_i2);
Declare_label(mercury__lexer__get_float_decimals_4_0_i6);
Declare_label(mercury__lexer__get_float_decimals_4_0_i7);
Declare_label(mercury__lexer__get_float_decimals_4_0_i5);
Declare_label(mercury__lexer__get_float_decimals_4_0_i11);
Declare_label(mercury__lexer__get_float_decimals_4_0_i10);
Declare_label(mercury__lexer__get_float_decimals_4_0_i17);
Declare_label(mercury__lexer__get_float_decimals_4_0_i16);
Declare_label(mercury__lexer__get_float_decimals_4_0_i14);
Declare_label(mercury__lexer__get_float_decimals_4_0_i8);
Declare_label(mercury__lexer__get_float_decimals_4_0_i24);
Declare_static(mercury__lexer__get_float_exponent_4_0);
Declare_label(mercury__lexer__get_float_exponent_4_0_i2);
Declare_label(mercury__lexer__get_float_exponent_4_0_i6);
Declare_label(mercury__lexer__get_float_exponent_4_0_i7);
Declare_label(mercury__lexer__get_float_exponent_4_0_i5);
Declare_label(mercury__lexer__get_float_exponent_4_0_i12);
Declare_label(mercury__lexer__get_float_exponent_4_0_i14);
Declare_label(mercury__lexer__get_float_exponent_4_0_i18);
Declare_label(mercury__lexer__get_float_exponent_4_0_i17);
Declare_label(mercury__lexer__get_float_exponent_4_0_i22);
Declare_label(mercury__lexer__get_float_exponent_4_0_i21);
Declare_label(mercury__lexer__get_float_exponent_4_0_i19);
Declare_label(mercury__lexer__get_float_exponent_4_0_i27);
Declare_label(mercury__lexer__get_float_exponent_4_0_i9);
Declare_label(mercury__lexer__get_float_exponent_4_0_i30);
Declare_label(mercury__lexer__get_float_exponent_4_0_i29);
Declare_label(mercury__lexer__get_float_exponent_4_0_i33);
Declare_label(mercury__lexer__get_float_exponent_4_0_i8);
Declare_label(mercury__lexer__get_float_exponent_4_0_i36);
Declare_static(mercury__lexer__get_float_exponent_3_4_0);
Declare_label(mercury__lexer__get_float_exponent_3_4_0_i2);
Declare_label(mercury__lexer__get_float_exponent_3_4_0_i6);
Declare_label(mercury__lexer__get_float_exponent_3_4_0_i7);
Declare_label(mercury__lexer__get_float_exponent_3_4_0_i5);
Declare_label(mercury__lexer__get_float_exponent_3_4_0_i11);
Declare_label(mercury__lexer__get_float_exponent_3_4_0_i10);
Declare_label(mercury__lexer__get_float_exponent_3_4_0_i8);
Declare_label(mercury__lexer__get_float_exponent_3_4_0_i17);
Declare_static(mercury__lexer__rev_char_list_to_int_3_0);
Declare_label(mercury__lexer__rev_char_list_to_int_3_0_i2);
Declare_label(mercury__lexer__rev_char_list_to_int_3_0_i5);
Declare_label(mercury__lexer__rev_char_list_to_int_3_0_i4);
Declare_static(mercury__lexer__rev_char_list_to_float_2_0);
Declare_label(mercury__lexer__rev_char_list_to_float_2_0_i2);
Declare_label(mercury__lexer__rev_char_list_to_float_2_0_i5);
Declare_label(mercury__lexer__rev_char_list_to_float_2_0_i4);
Declare_static(mercury__lexer__rev_char_list_to_string_2_0);
Define_extern_entry(mercury____Unify___lexer__token_0_0);
Declare_label(mercury____Unify___lexer__token_0_0_i1060);
Declare_label(mercury____Unify___lexer__token_0_0_i1059);
Declare_label(mercury____Unify___lexer__token_0_0_i1058);
Declare_label(mercury____Unify___lexer__token_0_0_i1057);
Declare_label(mercury____Unify___lexer__token_0_0_i1056);
Declare_label(mercury____Unify___lexer__token_0_0_i1055);
Declare_label(mercury____Unify___lexer__token_0_0_i1054);
Declare_label(mercury____Unify___lexer__token_0_0_i1053);
Declare_label(mercury____Unify___lexer__token_0_0_i1052);
Declare_label(mercury____Unify___lexer__token_0_0_i1051);
Declare_label(mercury____Unify___lexer__token_0_0_i1050);
Declare_label(mercury____Unify___lexer__token_0_0_i1036);
Declare_label(mercury____Unify___lexer__token_0_0_i1037);
Declare_label(mercury____Unify___lexer__token_0_0_i1038);
Declare_label(mercury____Unify___lexer__token_0_0_i1039);
Declare_label(mercury____Unify___lexer__token_0_0_i1040);
Declare_label(mercury____Unify___lexer__token_0_0_i1041);
Declare_label(mercury____Unify___lexer__token_0_0_i1042);
Declare_label(mercury____Unify___lexer__token_0_0_i1043);
Declare_label(mercury____Unify___lexer__token_0_0_i1044);
Declare_label(mercury____Unify___lexer__token_0_0_i1045);
Declare_label(mercury____Unify___lexer__token_0_0_i1046);
Declare_label(mercury____Unify___lexer__token_0_0_i1049);
Declare_label(mercury____Unify___lexer__token_0_0_i28);
Declare_label(mercury____Unify___lexer__token_0_0_i30);
Declare_label(mercury____Unify___lexer__token_0_0_i32);
Declare_label(mercury____Unify___lexer__token_0_0_i34);
Declare_label(mercury____Unify___lexer__token_0_0_i36);
Declare_label(mercury____Unify___lexer__token_0_0_i38);
Declare_label(mercury____Unify___lexer__token_0_0_i27);
Declare_label(mercury____Unify___lexer__token_0_0_i42);
Declare_label(mercury____Unify___lexer__token_0_0_i1);
Declare_label(mercury____Unify___lexer__token_0_0_i1047);
Declare_label(mercury____Unify___lexer__token_0_0_i1048);
Define_extern_entry(mercury____Index___lexer__token_0_0);
Declare_label(mercury____Index___lexer__token_0_0_i5);
Declare_label(mercury____Index___lexer__token_0_0_i6);
Declare_label(mercury____Index___lexer__token_0_0_i7);
Declare_label(mercury____Index___lexer__token_0_0_i8);
Declare_label(mercury____Index___lexer__token_0_0_i9);
Declare_label(mercury____Index___lexer__token_0_0_i10);
Declare_label(mercury____Index___lexer__token_0_0_i11);
Declare_label(mercury____Index___lexer__token_0_0_i12);
Declare_label(mercury____Index___lexer__token_0_0_i13);
Declare_label(mercury____Index___lexer__token_0_0_i14);
Declare_label(mercury____Index___lexer__token_0_0_i15);
Declare_label(mercury____Index___lexer__token_0_0_i4);
Declare_label(mercury____Index___lexer__token_0_0_i17);
Declare_label(mercury____Index___lexer__token_0_0_i18);
Declare_label(mercury____Index___lexer__token_0_0_i19);
Declare_label(mercury____Index___lexer__token_0_0_i20);
Declare_label(mercury____Index___lexer__token_0_0_i21);
Declare_label(mercury____Index___lexer__token_0_0_i22);
Declare_label(mercury____Index___lexer__token_0_0_i16);
Declare_label(mercury____Index___lexer__token_0_0_i23);
Define_extern_entry(mercury____Compare___lexer__token_0_0);
Declare_label(mercury____Compare___lexer__token_0_0_i2);
Declare_label(mercury____Compare___lexer__token_0_0_i3);
Declare_label(mercury____Compare___lexer__token_0_0_i4);
Declare_label(mercury____Compare___lexer__token_0_0_i6);
Declare_label(mercury____Compare___lexer__token_0_0_i13);
Declare_label(mercury____Compare___lexer__token_0_0_i15);
Declare_label(mercury____Compare___lexer__token_0_0_i17);
Declare_label(mercury____Compare___lexer__token_0_0_i19);
Declare_label(mercury____Compare___lexer__token_0_0_i21);
Declare_label(mercury____Compare___lexer__token_0_0_i23);
Declare_label(mercury____Compare___lexer__token_0_0_i25);
Declare_label(mercury____Compare___lexer__token_0_0_i27);
Declare_label(mercury____Compare___lexer__token_0_0_i29);
Declare_label(mercury____Compare___lexer__token_0_0_i31);
Declare_label(mercury____Compare___lexer__token_0_0_i33);
Declare_label(mercury____Compare___lexer__token_0_0_i12);
Declare_label(mercury____Compare___lexer__token_0_0_i36);
Declare_label(mercury____Compare___lexer__token_0_0_i39);
Declare_label(mercury____Compare___lexer__token_0_0_i42);
Declare_label(mercury____Compare___lexer__token_0_0_i45);
Declare_label(mercury____Compare___lexer__token_0_0_i48);
Declare_label(mercury____Compare___lexer__token_0_0_i51);
Declare_label(mercury____Compare___lexer__token_0_0_i35);
Declare_label(mercury____Compare___lexer__token_0_0_i54);
Declare_label(mercury____Compare___lexer__token_0_0_i9);
Declare_label(mercury____Compare___lexer__token_0_0_i1000);
Define_extern_entry(mercury____Unify___lexer__token_context_0_0);
Declare_label(mercury____Unify___lexer__token_context_0_0_i1);
Define_extern_entry(mercury____Index___lexer__token_context_0_0);
Define_extern_entry(mercury____Compare___lexer__token_context_0_0);
Define_extern_entry(mercury____Unify___lexer__token_list_0_0);
Define_extern_entry(mercury____Index___lexer__token_list_0_0);
Define_extern_entry(mercury____Compare___lexer__token_list_0_0);

extern Word * mercury_data_lexer__base_type_layout_token_0[];
Word * mercury_data_lexer__base_type_info_token_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___lexer__token_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___lexer__token_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___lexer__token_0_0),
	(Word *) (Integer) mercury_data_lexer__base_type_layout_token_0
};

extern Word * mercury_data_lexer__base_type_layout_token_context_0[];
Word * mercury_data_lexer__base_type_info_token_context_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___lexer__token_context_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___lexer__token_context_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___lexer__token_context_0_0),
	(Word *) (Integer) mercury_data_lexer__base_type_layout_token_context_0
};

extern Word * mercury_data_lexer__base_type_layout_token_list_0[];
Word * mercury_data_lexer__base_type_info_token_list_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___lexer__token_list_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___lexer__token_list_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___lexer__token_list_0_0),
	(Word *) (Integer) mercury_data_lexer__base_type_layout_token_list_0
};

extern Word * mercury_data_lexer__common_32[];
Word * mercury_data_lexer__base_type_layout_token_list_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_32),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_32),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_32),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_32)
};

extern Word * mercury_data_lexer__common_34[];
Word * mercury_data_lexer__base_type_layout_token_context_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_34),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_34),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_34),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_34)
};

extern Word * mercury_data_lexer__common_35[];
extern Word * mercury_data_lexer__common_37[];
extern Word * mercury_data_lexer__common_38[];
extern Word * mercury_data_lexer__common_48[];
Word * mercury_data_lexer__base_type_layout_token_0[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_35),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_37),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_38),
	(Word *) (Integer) mkword(mktag(2), (Integer) mercury_data_lexer__common_48)
};

Word * mercury_data_lexer__common_0[] = {
	(Word *) string_const("'", 1),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_lexer__common_1[] = {
	(Word *) string_const("\"", 1),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_lexer__common_2[] = {
	(Word *) string_const(">>", 2),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_lexer__common_3[] = {
	(Word *) string_const(")", 1),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_lexer__common_4[] = {
	(Word *) string_const("!", 1)
};

Word * mercury_data_lexer__common_5[] = {
	(Word *) string_const(";", 1)
};

Word mercury_data_lexer__common_6[] = {
	((Integer) 46),
	(Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_lexer__common_7[] = {
	(Word *) string_const(".", 1)
};

Word * mercury_data_lexer__common_8[] = {
	(Word *) string_const("/", 1)
};

Word mercury_data_lexer__common_9[] = {
	((Integer) 47),
	(Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_lexer__common_10[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("unterminated '/*' comment", 25)
};

Word * mercury_data_lexer__common_11[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("unterminated quote", 18)
};

Word * mercury_data_lexer__common_12[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("unterminated quoted name", 24)
};

Word * mercury_data_lexer__common_13[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("invalid escape character", 24)
};

Word * mercury_data_lexer__common_14[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("unterminated hex escape", 23)
};

Word * mercury_data_lexer__common_15[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("empty hex escape", 16)
};

Word * mercury_data_lexer__common_16[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("invalid hex escape", 18)
};

Word * mercury_data_lexer__common_17[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("empty octal escape", 18)
};

Word * mercury_data_lexer__common_18[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("invalid octal escape", 20)
};

Word * mercury_data_lexer__common_19[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("unexpected end-of-file in `#' line number directive", 51)
};

Word * mercury_data_lexer__common_20[] = {
	(Word *) string_const("' in `#' line number directive", 30),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word mercury_data_lexer__common_21[] = {
	((Integer) 0),
	((Integer) 0)
};

Word * mercury_data_lexer__common_22[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("unterminated char code constant", 31)
};

Word * mercury_data_lexer__common_23[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("unterminated binary constant", 28)
};

Word * mercury_data_lexer__common_24[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("unterminated octal constant", 27)
};

Word * mercury_data_lexer__common_25[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("unterminated hex constant", 25)
};

Word mercury_data_lexer__common_26[] = {
	((Integer) 48),
	(Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

Word * mercury_data_lexer__common_27[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("unterminated exponent in float token", 36)
};

Word * mercury_data_lexer__common_28[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("invalid integer token", 21)
};

Word * mercury_data_lexer__common_29[] = {
	(Word *) ((Integer) 4),
	(Word *) string_const("invalid float token", 19)
};

extern Word * mercury_data_std_util__base_type_info_pair_2[];
extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_lexer__common_30[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_pair_2,
	(Word *) (Integer) mercury_data_lexer__base_type_info_token_0,
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
Word * mercury_data_lexer__common_31[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_30)
};

Word * mercury_data_lexer__common_32[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_31)
};

Word * mercury_data_lexer__common_33[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_lexer__common_34[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_33)
};

Word * mercury_data_lexer__common_35[] = {
	(Word *) ((Integer) 0),
	(Word *) ((Integer) 11),
	(Word *) string_const("open", 4),
	(Word *) string_const("open_ct", 7),
	(Word *) string_const("close", 5),
	(Word *) string_const("open_list", 9),
	(Word *) string_const("close_list", 10),
	(Word *) string_const("open_curly", 10),
	(Word *) string_const("close_curly", 11),
	(Word *) string_const("ht_sep", 6),
	(Word *) string_const("comma", 5),
	(Word *) string_const("end", 3),
	(Word *) string_const("eof", 3)
};

extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_lexer__common_36[] = {
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

Word * mercury_data_lexer__common_37[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_36),
	(Word *) string_const("name", 4)
};

Word * mercury_data_lexer__common_38[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_36),
	(Word *) string_const("variable", 8)
};

Word * mercury_data_lexer__common_39[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_33),
	(Word *) string_const("integer", 7)
};

extern Word * mercury_data___base_type_info_float_0[];
Word * mercury_data_lexer__common_40[] = {
	(Word *) (Integer) mercury_data___base_type_info_float_0
};

Word * mercury_data_lexer__common_41[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_40),
	(Word *) string_const("float", 5)
};

Word * mercury_data_lexer__common_42[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_36),
	(Word *) string_const("string", 6)
};

extern Word * mercury_data___base_type_info_character_0[];
Word * mercury_data_lexer__common_43[] = {
	(Word *) (Integer) mercury_data___base_type_info_character_0
};

Word * mercury_data_lexer__common_44[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_43),
	(Word *) string_const("junk", 4)
};

Word * mercury_data_lexer__common_45[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_36),
	(Word *) string_const("error", 5)
};

extern Word * mercury_data_io__base_type_info_io__error_0[];
Word * mercury_data_lexer__common_46[] = {
	(Word *) (Integer) mercury_data_io__base_type_info_io__error_0
};

Word * mercury_data_lexer__common_47[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_46),
	(Word *) string_const("io_error", 8)
};

Word * mercury_data_lexer__common_48[] = {
	(Word *) ((Integer) 6),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_39),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_41),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_42),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_44),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_45),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_47)
};

BEGIN_MODULE(mercury__lexer_module0)
	init_entry(mercury__lexer__get_token_list_3_0);
	init_label(mercury__lexer__get_token_list_3_0_i2);
	init_label(mercury__lexer__get_token_list_3_0_i3);
	init_label(mercury__lexer__get_token_list_3_0_i4);
	init_label(mercury__lexer__get_token_list_3_0_i12);
	init_label(mercury__lexer__get_token_list_3_0_i11);
	init_label(mercury__lexer__get_token_list_3_0_i9);
	init_label(mercury__lexer__get_token_list_3_0_i7);
	init_label(mercury__lexer__get_token_list_3_0_i16);
BEGIN_CODE

/* code for predicate 'lexer__get_token_list'/3 in mode 0 */
Define_entry(mercury__lexer__get_token_list_3_0);
	incr_sp_push_msg(2, "lexer__get_token_list");
	detstackvar(2) = (Integer) succip;
	call_localret(STATIC(mercury__lexer__get_token_1_3_0),
		mercury__lexer__get_token_list_3_0_i2,
		ENTRY(mercury__lexer__get_token_list_3_0));
Define_label(mercury__lexer__get_token_list_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_token_list_3_0));
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__get_line_number_3_0);
	call_localret(ENTRY(mercury__io__get_line_number_3_0),
		mercury__lexer__get_token_list_3_0_i3,
		ENTRY(mercury__lexer__get_token_list_3_0));
	}
Define_label(mercury__lexer__get_token_list_3_0_i3);
	update_prof_current_proc(LABEL(mercury__lexer__get_token_list_3_0));
	if (((Integer) detstackvar(1) != (Integer) mkword(mktag(0), mkbody(((Integer) 10)))))
		GOTO_LABEL(mercury__lexer__get_token_list_3_0_i4);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_list_3_0_i4);
	r3 = (Integer) detstackvar(1);
	if ((tag((Integer) r3) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_token_list_3_0_i11);
	COMPUTED_GOTO(unmkbody((Integer) r3),
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i12) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7));
Define_label(mercury__lexer__get_token_list_3_0_i12);
	{
	Word tempr1;
	tempr1 = (Integer) r1;
	r1 = (Integer) r3;
	r3 = (Integer) tempr1;
	GOTO_LABEL(mercury__lexer__get_token_list_3_0_i9);
	}
Define_label(mercury__lexer__get_token_list_3_0_i11);
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__lexer__get_token_list_3_0_i7);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)),
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i7) AND
		LABEL(mercury__lexer__get_token_list_3_0_i12) AND
		LABEL(mercury__lexer__get_token_list_3_0_i12));
Define_label(mercury__lexer__get_token_list_3_0_i9);
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r3;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
	}
Define_label(mercury__lexer__get_token_list_3_0_i7);
	r3 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 2));
	detstackvar(1) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) r1;
	r1 = (Integer) r2;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	localcall(mercury__lexer__get_token_list_3_0,
		LABEL(mercury__lexer__get_token_list_3_0_i16),
		ENTRY(mercury__lexer__get_token_list_3_0));
	}
Define_label(mercury__lexer__get_token_list_3_0_i16);
	update_prof_current_proc(LABEL(mercury__lexer__get_token_list_3_0));
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module1)
	init_entry(mercury__lexer__token_to_string_2_0);
	init_label(mercury__lexer__token_to_string_2_0_i1018);
	init_label(mercury__lexer__token_to_string_2_0_i1019);
	init_label(mercury__lexer__token_to_string_2_0_i1020);
	init_label(mercury__lexer__token_to_string_2_0_i1021);
	init_label(mercury__lexer__token_to_string_2_0_i1022);
	init_label(mercury__lexer__token_to_string_2_0_i1023);
	init_label(mercury__lexer__token_to_string_2_0_i1024);
	init_label(mercury__lexer__token_to_string_2_0_i1025);
	init_label(mercury__lexer__token_to_string_2_0_i1026);
	init_label(mercury__lexer__token_to_string_2_0_i1027);
	init_label(mercury__lexer__token_to_string_2_0_i1028);
	init_label(mercury__lexer__token_to_string_2_0_i1041);
	init_label(mercury__lexer__token_to_string_2_0_i17);
	init_label(mercury__lexer__token_to_string_2_0_i18);
	init_label(mercury__lexer__token_to_string_2_0_i20);
	init_label(mercury__lexer__token_to_string_2_0_i21);
	init_label(mercury__lexer__token_to_string_2_0_i23);
	init_label(mercury__lexer__token_to_string_2_0_i25);
	init_label(mercury__lexer__token_to_string_2_0_i26);
	init_label(mercury__lexer__token_to_string_2_0_i27);
	init_label(mercury__lexer__token_to_string_2_0_i29);
	init_label(mercury__lexer__token_to_string_2_0_i31);
	init_label(mercury__lexer__token_to_string_2_0_i32);
	init_label(mercury__lexer__token_to_string_2_0_i16);
	init_label(mercury__lexer__token_to_string_2_0_i34);
BEGIN_CODE

/* code for predicate 'lexer__token_to_string'/2 in mode 0 */
Define_entry(mercury__lexer__token_to_string_2_0);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__token_to_string_2_0_i1041);
	COMPUTED_GOTO(unmkbody((Integer) r1),
		LABEL(mercury__lexer__token_to_string_2_0_i1018) AND
		LABEL(mercury__lexer__token_to_string_2_0_i1019) AND
		LABEL(mercury__lexer__token_to_string_2_0_i1020) AND
		LABEL(mercury__lexer__token_to_string_2_0_i1021) AND
		LABEL(mercury__lexer__token_to_string_2_0_i1022) AND
		LABEL(mercury__lexer__token_to_string_2_0_i1023) AND
		LABEL(mercury__lexer__token_to_string_2_0_i1024) AND
		LABEL(mercury__lexer__token_to_string_2_0_i1025) AND
		LABEL(mercury__lexer__token_to_string_2_0_i1026) AND
		LABEL(mercury__lexer__token_to_string_2_0_i1027) AND
		LABEL(mercury__lexer__token_to_string_2_0_i1028));
Define_label(mercury__lexer__token_to_string_2_0_i1018);
	r1 = string_const("token ` ('", 10);
	proceed();
Define_label(mercury__lexer__token_to_string_2_0_i1019);
	r1 = string_const("token `('", 9);
	proceed();
Define_label(mercury__lexer__token_to_string_2_0_i1020);
	r1 = string_const("token `)'", 9);
	proceed();
Define_label(mercury__lexer__token_to_string_2_0_i1021);
	r1 = string_const("token `['", 9);
	proceed();
Define_label(mercury__lexer__token_to_string_2_0_i1022);
	r1 = string_const("token `]'", 9);
	proceed();
Define_label(mercury__lexer__token_to_string_2_0_i1023);
	r1 = string_const("token `{'", 9);
	proceed();
Define_label(mercury__lexer__token_to_string_2_0_i1024);
	r1 = string_const("token `}'", 9);
	proceed();
Define_label(mercury__lexer__token_to_string_2_0_i1025);
	r1 = string_const("token `|'", 9);
	proceed();
Define_label(mercury__lexer__token_to_string_2_0_i1026);
	r1 = string_const("token `,'", 9);
	proceed();
Define_label(mercury__lexer__token_to_string_2_0_i1027);
	r1 = string_const("token `. '", 10);
	proceed();
Define_label(mercury__lexer__token_to_string_2_0_i1028);
	r1 = string_const("end-of-file", 11);
	proceed();
Define_label(mercury__lexer__token_to_string_2_0_i1041);
	incr_sp_push_msg(1, "lexer__token_to_string");
	detstackvar(1) = (Integer) succip;
	if (((Integer) r2 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury__lexer__token_to_string_2_0_i16);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury__lexer__token_to_string_2_0_i17) AND
		LABEL(mercury__lexer__token_to_string_2_0_i20) AND
		LABEL(mercury__lexer__token_to_string_2_0_i23) AND
		LABEL(mercury__lexer__token_to_string_2_0_i25) AND
		LABEL(mercury__lexer__token_to_string_2_0_i29) AND
		LABEL(mercury__lexer__token_to_string_2_0_i31));
Define_label(mercury__lexer__token_to_string_2_0_i17);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	{
	Declare_entry(mercury__string__int_to_string_2_0);
	call_localret(ENTRY(mercury__string__int_to_string_2_0),
		mercury__lexer__token_to_string_2_0_i18,
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i18);
	update_prof_current_proc(LABEL(mercury__lexer__token_to_string_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("integer `", 9);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_0);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__string__append_list_2_0);
	tailcall(ENTRY(mercury__string__append_list_2_0),
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i20);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	{
	Declare_entry(mercury__string__float_to_string_2_0);
	call_localret(ENTRY(mercury__string__float_to_string_2_0),
		mercury__lexer__token_to_string_2_0_i21,
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i21);
	update_prof_current_proc(LABEL(mercury__lexer__token_to_string_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("float `", 7);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_0);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__string__append_list_2_0);
	tailcall(ENTRY(mercury__string__append_list_2_0),
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i23);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("string \"", 8);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__string__append_list_2_0);
	tailcall(ENTRY(mercury__string__append_list_2_0),
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i25);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	{
	Declare_entry(mercury__char__to_int_2_0);
	call_localret(ENTRY(mercury__char__to_int_2_0),
		mercury__lexer__token_to_string_2_0_i26,
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i26);
	update_prof_current_proc(LABEL(mercury__lexer__token_to_string_2_0));
	r2 = ((Integer) 16);
	{
	Declare_entry(mercury__string__int_to_base_string_3_0);
	call_localret(ENTRY(mercury__string__int_to_base_string_3_0),
		mercury__lexer__token_to_string_2_0_i27,
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i27);
	update_prof_current_proc(LABEL(mercury__lexer__token_to_string_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("illegal character <<0x", 22);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__string__append_list_2_0);
	tailcall(ENTRY(mercury__string__append_list_2_0),
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i29);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("illegal token (", 15);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_3);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__string__append_list_2_0);
	tailcall(ENTRY(mercury__string__append_list_2_0),
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i31);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	{
	Declare_entry(mercury__io__error_message_2_0);
	call_localret(ENTRY(mercury__io__error_message_2_0),
		mercury__lexer__token_to_string_2_0_i32,
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i32);
	update_prof_current_proc(LABEL(mercury__lexer__token_to_string_2_0));
	r2 = (Integer) r1;
	r1 = string_const("I/O error: ", 11);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__string__append_3_2);
	tailcall(ENTRY(mercury__string__append_3_2),
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i16);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__token_to_string_2_0_i34);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("token '", 7);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_0);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__string__append_list_2_0);
	tailcall(ENTRY(mercury__string__append_list_2_0),
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
Define_label(mercury__lexer__token_to_string_2_0_i34);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("variable `", 10);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) field(mktag(2), (Integer) r2, ((Integer) 0));
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_0);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
	Declare_entry(mercury__string__append_list_2_0);
	tailcall(ENTRY(mercury__string__append_list_2_0),
		ENTRY(mercury__lexer__token_to_string_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__lexer_module2)
	init_entry(mercury__lexer__get_token_1_3_0);
	init_label(mercury__lexer__get_token_1_3_0_i2);
	init_label(mercury__lexer__get_token_1_3_0_i6);
	init_label(mercury__lexer__get_token_1_3_0_i5);
	init_label(mercury__lexer__get_token_1_3_0_i9);
	init_label(mercury__lexer__get_token_1_3_0_i17);
	init_label(mercury__lexer__get_token_1_3_0_i45);
	init_label(mercury__lexer__get_token_1_3_0_i46);
	init_label(mercury__lexer__get_token_1_3_0_i48);
	init_label(mercury__lexer__get_token_1_3_0_i50);
	init_label(mercury__lexer__get_token_1_3_0_i52);
	init_label(mercury__lexer__get_token_1_3_0_i54);
	init_label(mercury__lexer__get_token_1_3_0_i58);
	init_label(mercury__lexer__get_token_1_3_0_i59);
	init_label(mercury__lexer__get_token_1_3_0_i60);
	init_label(mercury__lexer__get_token_1_3_0_i62);
	init_label(mercury__lexer__get_token_1_3_0_i64);
	init_label(mercury__lexer__get_token_1_3_0_i65);
	init_label(mercury__lexer__get_token_1_3_0_i67);
	init_label(mercury__lexer__get_token_1_3_0_i69);
	init_label(mercury__lexer__get_token_1_3_0_i71);
	init_label(mercury__lexer__get_token_1_3_0_i73);
	init_label(mercury__lexer__get_token_1_3_0_i75);
	init_label(mercury__lexer__get_token_1_3_0_i77);
	init_label(mercury__lexer__get_token_1_3_0_i79);
	init_label(mercury__lexer__get_token_1_3_0_i81);
	init_label(mercury__lexer__get_token_1_3_0_i83);
	init_label(mercury__lexer__get_token_1_3_0_i85);
	init_label(mercury__lexer__get_token_1_3_0_i87);
	init_label(mercury__lexer__get_token_1_3_0_i89);
	init_label(mercury__lexer__get_token_1_3_0_i91);
	init_label(mercury__lexer__get_token_1_3_0_i93);
	init_label(mercury__lexer__get_token_1_3_0_i94);
	init_label(mercury__lexer__get_token_1_3_0_i96);
	init_label(mercury__lexer__get_token_1_3_0_i98);
	init_label(mercury__lexer__get_token_1_3_0_i100);
	init_label(mercury__lexer__get_token_1_3_0_i102);
	init_label(mercury__lexer__get_token_1_3_0_i104);
	init_label(mercury__lexer__get_token_1_3_0_i106);
	init_label(mercury__lexer__get_token_1_3_0_i108);
	init_label(mercury__lexer__get_token_1_3_0_i110);
	init_label(mercury__lexer__get_token_1_3_0_i112);
	init_label(mercury__lexer__get_token_1_3_0_i114);
	init_label(mercury__lexer__get_token_1_3_0_i116);
	init_label(mercury__lexer__get_token_1_3_0_i118);
	init_label(mercury__lexer__get_token_1_3_0_i120);
	init_label(mercury__lexer__get_token_1_3_0_i122);
	init_label(mercury__lexer__get_token_1_3_0_i124);
	init_label(mercury__lexer__get_token_1_3_0_i126);
	init_label(mercury__lexer__get_token_1_3_0_i128);
	init_label(mercury__lexer__get_token_1_3_0_i130);
	init_label(mercury__lexer__get_token_1_3_0_i132);
	init_label(mercury__lexer__get_token_1_3_0_i134);
	init_label(mercury__lexer__get_token_1_3_0_i136);
	init_label(mercury__lexer__get_token_1_3_0_i138);
	init_label(mercury__lexer__get_token_1_3_0_i140);
	init_label(mercury__lexer__get_token_1_3_0_i142);
	init_label(mercury__lexer__get_token_1_3_0_i144);
	init_label(mercury__lexer__get_token_1_3_0_i146);
	init_label(mercury__lexer__get_token_1_3_0_i148);
	init_label(mercury__lexer__get_token_1_3_0_i150);
	init_label(mercury__lexer__get_token_1_3_0_i152);
	init_label(mercury__lexer__get_token_1_3_0_i154);
	init_label(mercury__lexer__get_token_1_3_0_i156);
	init_label(mercury__lexer__get_token_1_3_0_i157);
	init_label(mercury__lexer__get_token_1_3_0_i159);
	init_label(mercury__lexer__get_token_1_3_0_i160);
	init_label(mercury__lexer__get_token_1_3_0_i162);
	init_label(mercury__lexer__get_token_1_3_0_i165);
	init_label(mercury__lexer__get_token_1_3_0_i167);
	init_label(mercury__lexer__get_token_1_3_0_i169);
	init_label(mercury__lexer__get_token_1_3_0_i171);
	init_label(mercury__lexer__get_token_1_3_0_i173);
	init_label(mercury__lexer__get_token_1_3_0_i175);
	init_label(mercury__lexer__get_token_1_3_0_i177);
	init_label(mercury__lexer__get_token_1_3_0_i179);
	init_label(mercury__lexer__get_token_1_3_0_i181);
	init_label(mercury__lexer__get_token_1_3_0_i183);
	init_label(mercury__lexer__get_token_1_3_0_i185);
	init_label(mercury__lexer__get_token_1_3_0_i187);
	init_label(mercury__lexer__get_token_1_3_0_i189);
	init_label(mercury__lexer__get_token_1_3_0_i191);
	init_label(mercury__lexer__get_token_1_3_0_i193);
	init_label(mercury__lexer__get_token_1_3_0_i195);
	init_label(mercury__lexer__get_token_1_3_0_i197);
	init_label(mercury__lexer__get_token_1_3_0_i199);
	init_label(mercury__lexer__get_token_1_3_0_i201);
	init_label(mercury__lexer__get_token_1_3_0_i203);
	init_label(mercury__lexer__get_token_1_3_0_i205);
	init_label(mercury__lexer__get_token_1_3_0_i207);
	init_label(mercury__lexer__get_token_1_3_0_i209);
	init_label(mercury__lexer__get_token_1_3_0_i211);
	init_label(mercury__lexer__get_token_1_3_0_i213);
	init_label(mercury__lexer__get_token_1_3_0_i215);
	init_label(mercury__lexer__get_token_1_3_0_i217);
	init_label(mercury__lexer__get_token_1_3_0_i218);
	init_label(mercury__lexer__get_token_1_3_0_i219);
	init_label(mercury__lexer__get_token_1_3_0_i220);
	init_label(mercury__lexer__get_token_1_3_0_i7);
	init_label(mercury__lexer__get_token_1_3_0_i223);
BEGIN_CODE

/* code for predicate 'lexer__get_token_1'/3 in mode 0 */
Define_static(mercury__lexer__get_token_1_3_0);
	incr_sp_push_msg(2, "lexer__get_token_1");
	detstackvar(2) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_token_1_3_0_i2,
		STATIC(mercury__lexer__get_token_1_3_0));
	}
Define_label(mercury__lexer__get_token_1_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_token_1_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_token_1_3_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_token_1_3_0_i6,
		STATIC(mercury__lexer__get_token_1_3_0));
	}
Define_label(mercury__lexer__get_token_1_3_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_token_1_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 10)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_token_1_3_0_i7);
	COMPUTED_GOTO(((Integer) field(mktag(1), (Integer) r1, ((Integer) 0)) - ((Integer) 1)),
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i17) AND
		LABEL(mercury__lexer__get_token_1_3_0_i17) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i17) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i17) AND
		LABEL(mercury__lexer__get_token_1_3_0_i45) AND
		LABEL(mercury__lexer__get_token_1_3_0_i46) AND
		LABEL(mercury__lexer__get_token_1_3_0_i48) AND
		LABEL(mercury__lexer__get_token_1_3_0_i50) AND
		LABEL(mercury__lexer__get_token_1_3_0_i52) AND
		LABEL(mercury__lexer__get_token_1_3_0_i54) AND
		LABEL(mercury__lexer__get_token_1_3_0_i46) AND
		LABEL(mercury__lexer__get_token_1_3_0_i58) AND
		LABEL(mercury__lexer__get_token_1_3_0_i59) AND
		LABEL(mercury__lexer__get_token_1_3_0_i60) AND
		LABEL(mercury__lexer__get_token_1_3_0_i62) AND
		LABEL(mercury__lexer__get_token_1_3_0_i64) AND
		LABEL(mercury__lexer__get_token_1_3_0_i65) AND
		LABEL(mercury__lexer__get_token_1_3_0_i67) AND
		LABEL(mercury__lexer__get_token_1_3_0_i69) AND
		LABEL(mercury__lexer__get_token_1_3_0_i71) AND
		LABEL(mercury__lexer__get_token_1_3_0_i73) AND
		LABEL(mercury__lexer__get_token_1_3_0_i75) AND
		LABEL(mercury__lexer__get_token_1_3_0_i77) AND
		LABEL(mercury__lexer__get_token_1_3_0_i79) AND
		LABEL(mercury__lexer__get_token_1_3_0_i81) AND
		LABEL(mercury__lexer__get_token_1_3_0_i83) AND
		LABEL(mercury__lexer__get_token_1_3_0_i85) AND
		LABEL(mercury__lexer__get_token_1_3_0_i87) AND
		LABEL(mercury__lexer__get_token_1_3_0_i89) AND
		LABEL(mercury__lexer__get_token_1_3_0_i91) AND
		LABEL(mercury__lexer__get_token_1_3_0_i93) AND
		LABEL(mercury__lexer__get_token_1_3_0_i94) AND
		LABEL(mercury__lexer__get_token_1_3_0_i96) AND
		LABEL(mercury__lexer__get_token_1_3_0_i98) AND
		LABEL(mercury__lexer__get_token_1_3_0_i100) AND
		LABEL(mercury__lexer__get_token_1_3_0_i102) AND
		LABEL(mercury__lexer__get_token_1_3_0_i104) AND
		LABEL(mercury__lexer__get_token_1_3_0_i106) AND
		LABEL(mercury__lexer__get_token_1_3_0_i108) AND
		LABEL(mercury__lexer__get_token_1_3_0_i110) AND
		LABEL(mercury__lexer__get_token_1_3_0_i112) AND
		LABEL(mercury__lexer__get_token_1_3_0_i114) AND
		LABEL(mercury__lexer__get_token_1_3_0_i116) AND
		LABEL(mercury__lexer__get_token_1_3_0_i118) AND
		LABEL(mercury__lexer__get_token_1_3_0_i120) AND
		LABEL(mercury__lexer__get_token_1_3_0_i122) AND
		LABEL(mercury__lexer__get_token_1_3_0_i124) AND
		LABEL(mercury__lexer__get_token_1_3_0_i126) AND
		LABEL(mercury__lexer__get_token_1_3_0_i128) AND
		LABEL(mercury__lexer__get_token_1_3_0_i130) AND
		LABEL(mercury__lexer__get_token_1_3_0_i132) AND
		LABEL(mercury__lexer__get_token_1_3_0_i134) AND
		LABEL(mercury__lexer__get_token_1_3_0_i136) AND
		LABEL(mercury__lexer__get_token_1_3_0_i138) AND
		LABEL(mercury__lexer__get_token_1_3_0_i140) AND
		LABEL(mercury__lexer__get_token_1_3_0_i142) AND
		LABEL(mercury__lexer__get_token_1_3_0_i144) AND
		LABEL(mercury__lexer__get_token_1_3_0_i146) AND
		LABEL(mercury__lexer__get_token_1_3_0_i148) AND
		LABEL(mercury__lexer__get_token_1_3_0_i150) AND
		LABEL(mercury__lexer__get_token_1_3_0_i152) AND
		LABEL(mercury__lexer__get_token_1_3_0_i154) AND
		LABEL(mercury__lexer__get_token_1_3_0_i156) AND
		LABEL(mercury__lexer__get_token_1_3_0_i157) AND
		LABEL(mercury__lexer__get_token_1_3_0_i159) AND
		LABEL(mercury__lexer__get_token_1_3_0_i160) AND
		LABEL(mercury__lexer__get_token_1_3_0_i162) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9) AND
		LABEL(mercury__lexer__get_token_1_3_0_i165) AND
		LABEL(mercury__lexer__get_token_1_3_0_i167) AND
		LABEL(mercury__lexer__get_token_1_3_0_i169) AND
		LABEL(mercury__lexer__get_token_1_3_0_i171) AND
		LABEL(mercury__lexer__get_token_1_3_0_i173) AND
		LABEL(mercury__lexer__get_token_1_3_0_i175) AND
		LABEL(mercury__lexer__get_token_1_3_0_i177) AND
		LABEL(mercury__lexer__get_token_1_3_0_i179) AND
		LABEL(mercury__lexer__get_token_1_3_0_i181) AND
		LABEL(mercury__lexer__get_token_1_3_0_i183) AND
		LABEL(mercury__lexer__get_token_1_3_0_i185) AND
		LABEL(mercury__lexer__get_token_1_3_0_i187) AND
		LABEL(mercury__lexer__get_token_1_3_0_i189) AND
		LABEL(mercury__lexer__get_token_1_3_0_i191) AND
		LABEL(mercury__lexer__get_token_1_3_0_i193) AND
		LABEL(mercury__lexer__get_token_1_3_0_i195) AND
		LABEL(mercury__lexer__get_token_1_3_0_i197) AND
		LABEL(mercury__lexer__get_token_1_3_0_i199) AND
		LABEL(mercury__lexer__get_token_1_3_0_i201) AND
		LABEL(mercury__lexer__get_token_1_3_0_i203) AND
		LABEL(mercury__lexer__get_token_1_3_0_i205) AND
		LABEL(mercury__lexer__get_token_1_3_0_i207) AND
		LABEL(mercury__lexer__get_token_1_3_0_i209) AND
		LABEL(mercury__lexer__get_token_1_3_0_i211) AND
		LABEL(mercury__lexer__get_token_1_3_0_i213) AND
		LABEL(mercury__lexer__get_token_1_3_0_i215) AND
		LABEL(mercury__lexer__get_token_1_3_0_i217) AND
		LABEL(mercury__lexer__get_token_1_3_0_i218) AND
		LABEL(mercury__lexer__get_token_1_3_0_i219) AND
		LABEL(mercury__lexer__get_token_1_3_0_i220) AND
		LABEL(mercury__lexer__get_token_1_3_0_i9));
Define_label(mercury__lexer__get_token_1_3_0_i9);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i17);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_token_2_3_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i45);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i46);
	r3 = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i48);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_source_line_number_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i50);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i52);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__skip_to_eol_3_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i54);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i58);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 1)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i59);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 2)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i60);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i62);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i64);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 8)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i65);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i67);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_dot_3_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i69);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_slash_3_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i71);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_zero_3_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i73);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i75);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i77);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i79);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i81);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i83);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i85);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i87);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i89);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i91);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i93);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i94);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i96);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i98);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i100);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i102);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i104);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i106);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i108);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i110);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i112);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i114);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i116);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i118);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i120);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i122);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i124);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i126);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i128);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i130);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i132);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i134);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i136);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i138);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i140);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i142);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i144);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i146);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i148);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i150);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i152);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i154);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i156);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 3)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i157);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i159);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 4)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i160);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i162);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i165);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i167);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i169);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i171);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i173);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i175);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i177);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i179);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i181);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i183);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i185);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i187);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i189);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i191);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i193);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i195);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i197);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i199);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i201);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i203);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i205);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i207);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i209);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i211);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i213);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i215);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i217);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 5)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i218);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 7)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i219);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 6)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_1_3_0_i220);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_1_3_0));
Define_label(mercury__lexer__get_token_1_3_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_token_1_3_0_i223,
		STATIC(mercury__lexer__get_token_1_3_0));
	}
Define_label(mercury__lexer__get_token_1_3_0_i223);
	update_prof_current_proc(LABEL(mercury__lexer__get_token_1_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module3)
	init_entry(mercury__lexer__get_token_2_3_0);
	init_label(mercury__lexer__get_token_2_3_0_i2);
	init_label(mercury__lexer__get_token_2_3_0_i6);
	init_label(mercury__lexer__get_token_2_3_0_i5);
	init_label(mercury__lexer__get_token_2_3_0_i9);
	init_label(mercury__lexer__get_token_2_3_0_i17);
	init_label(mercury__lexer__get_token_2_3_0_i45);
	init_label(mercury__lexer__get_token_2_3_0_i46);
	init_label(mercury__lexer__get_token_2_3_0_i48);
	init_label(mercury__lexer__get_token_2_3_0_i50);
	init_label(mercury__lexer__get_token_2_3_0_i52);
	init_label(mercury__lexer__get_token_2_3_0_i54);
	init_label(mercury__lexer__get_token_2_3_0_i58);
	init_label(mercury__lexer__get_token_2_3_0_i59);
	init_label(mercury__lexer__get_token_2_3_0_i60);
	init_label(mercury__lexer__get_token_2_3_0_i62);
	init_label(mercury__lexer__get_token_2_3_0_i64);
	init_label(mercury__lexer__get_token_2_3_0_i65);
	init_label(mercury__lexer__get_token_2_3_0_i67);
	init_label(mercury__lexer__get_token_2_3_0_i69);
	init_label(mercury__lexer__get_token_2_3_0_i71);
	init_label(mercury__lexer__get_token_2_3_0_i73);
	init_label(mercury__lexer__get_token_2_3_0_i75);
	init_label(mercury__lexer__get_token_2_3_0_i77);
	init_label(mercury__lexer__get_token_2_3_0_i79);
	init_label(mercury__lexer__get_token_2_3_0_i81);
	init_label(mercury__lexer__get_token_2_3_0_i83);
	init_label(mercury__lexer__get_token_2_3_0_i85);
	init_label(mercury__lexer__get_token_2_3_0_i87);
	init_label(mercury__lexer__get_token_2_3_0_i89);
	init_label(mercury__lexer__get_token_2_3_0_i91);
	init_label(mercury__lexer__get_token_2_3_0_i93);
	init_label(mercury__lexer__get_token_2_3_0_i94);
	init_label(mercury__lexer__get_token_2_3_0_i96);
	init_label(mercury__lexer__get_token_2_3_0_i98);
	init_label(mercury__lexer__get_token_2_3_0_i100);
	init_label(mercury__lexer__get_token_2_3_0_i102);
	init_label(mercury__lexer__get_token_2_3_0_i104);
	init_label(mercury__lexer__get_token_2_3_0_i106);
	init_label(mercury__lexer__get_token_2_3_0_i108);
	init_label(mercury__lexer__get_token_2_3_0_i110);
	init_label(mercury__lexer__get_token_2_3_0_i112);
	init_label(mercury__lexer__get_token_2_3_0_i114);
	init_label(mercury__lexer__get_token_2_3_0_i116);
	init_label(mercury__lexer__get_token_2_3_0_i118);
	init_label(mercury__lexer__get_token_2_3_0_i120);
	init_label(mercury__lexer__get_token_2_3_0_i122);
	init_label(mercury__lexer__get_token_2_3_0_i124);
	init_label(mercury__lexer__get_token_2_3_0_i126);
	init_label(mercury__lexer__get_token_2_3_0_i128);
	init_label(mercury__lexer__get_token_2_3_0_i130);
	init_label(mercury__lexer__get_token_2_3_0_i132);
	init_label(mercury__lexer__get_token_2_3_0_i134);
	init_label(mercury__lexer__get_token_2_3_0_i136);
	init_label(mercury__lexer__get_token_2_3_0_i138);
	init_label(mercury__lexer__get_token_2_3_0_i140);
	init_label(mercury__lexer__get_token_2_3_0_i142);
	init_label(mercury__lexer__get_token_2_3_0_i144);
	init_label(mercury__lexer__get_token_2_3_0_i146);
	init_label(mercury__lexer__get_token_2_3_0_i148);
	init_label(mercury__lexer__get_token_2_3_0_i150);
	init_label(mercury__lexer__get_token_2_3_0_i152);
	init_label(mercury__lexer__get_token_2_3_0_i154);
	init_label(mercury__lexer__get_token_2_3_0_i156);
	init_label(mercury__lexer__get_token_2_3_0_i157);
	init_label(mercury__lexer__get_token_2_3_0_i159);
	init_label(mercury__lexer__get_token_2_3_0_i160);
	init_label(mercury__lexer__get_token_2_3_0_i162);
	init_label(mercury__lexer__get_token_2_3_0_i165);
	init_label(mercury__lexer__get_token_2_3_0_i167);
	init_label(mercury__lexer__get_token_2_3_0_i169);
	init_label(mercury__lexer__get_token_2_3_0_i171);
	init_label(mercury__lexer__get_token_2_3_0_i173);
	init_label(mercury__lexer__get_token_2_3_0_i175);
	init_label(mercury__lexer__get_token_2_3_0_i177);
	init_label(mercury__lexer__get_token_2_3_0_i179);
	init_label(mercury__lexer__get_token_2_3_0_i181);
	init_label(mercury__lexer__get_token_2_3_0_i183);
	init_label(mercury__lexer__get_token_2_3_0_i185);
	init_label(mercury__lexer__get_token_2_3_0_i187);
	init_label(mercury__lexer__get_token_2_3_0_i189);
	init_label(mercury__lexer__get_token_2_3_0_i191);
	init_label(mercury__lexer__get_token_2_3_0_i193);
	init_label(mercury__lexer__get_token_2_3_0_i195);
	init_label(mercury__lexer__get_token_2_3_0_i197);
	init_label(mercury__lexer__get_token_2_3_0_i199);
	init_label(mercury__lexer__get_token_2_3_0_i201);
	init_label(mercury__lexer__get_token_2_3_0_i203);
	init_label(mercury__lexer__get_token_2_3_0_i205);
	init_label(mercury__lexer__get_token_2_3_0_i207);
	init_label(mercury__lexer__get_token_2_3_0_i209);
	init_label(mercury__lexer__get_token_2_3_0_i211);
	init_label(mercury__lexer__get_token_2_3_0_i213);
	init_label(mercury__lexer__get_token_2_3_0_i215);
	init_label(mercury__lexer__get_token_2_3_0_i217);
	init_label(mercury__lexer__get_token_2_3_0_i218);
	init_label(mercury__lexer__get_token_2_3_0_i219);
	init_label(mercury__lexer__get_token_2_3_0_i220);
	init_label(mercury__lexer__get_token_2_3_0_i7);
	init_label(mercury__lexer__get_token_2_3_0_i223);
BEGIN_CODE

/* code for predicate 'lexer__get_token_2'/3 in mode 0 */
Define_static(mercury__lexer__get_token_2_3_0);
	incr_sp_push_msg(2, "lexer__get_token_2");
	detstackvar(2) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_token_2_3_0_i2,
		STATIC(mercury__lexer__get_token_2_3_0));
	}
Define_label(mercury__lexer__get_token_2_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_token_2_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_token_2_3_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_token_2_3_0_i6,
		STATIC(mercury__lexer__get_token_2_3_0));
	}
Define_label(mercury__lexer__get_token_2_3_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_token_2_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 10)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_token_2_3_0_i7);
	COMPUTED_GOTO(((Integer) field(mktag(1), (Integer) r1, ((Integer) 0)) - ((Integer) 1)),
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i17) AND
		LABEL(mercury__lexer__get_token_2_3_0_i17) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i17) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i17) AND
		LABEL(mercury__lexer__get_token_2_3_0_i45) AND
		LABEL(mercury__lexer__get_token_2_3_0_i46) AND
		LABEL(mercury__lexer__get_token_2_3_0_i48) AND
		LABEL(mercury__lexer__get_token_2_3_0_i50) AND
		LABEL(mercury__lexer__get_token_2_3_0_i52) AND
		LABEL(mercury__lexer__get_token_2_3_0_i54) AND
		LABEL(mercury__lexer__get_token_2_3_0_i46) AND
		LABEL(mercury__lexer__get_token_2_3_0_i58) AND
		LABEL(mercury__lexer__get_token_2_3_0_i59) AND
		LABEL(mercury__lexer__get_token_2_3_0_i60) AND
		LABEL(mercury__lexer__get_token_2_3_0_i62) AND
		LABEL(mercury__lexer__get_token_2_3_0_i64) AND
		LABEL(mercury__lexer__get_token_2_3_0_i65) AND
		LABEL(mercury__lexer__get_token_2_3_0_i67) AND
		LABEL(mercury__lexer__get_token_2_3_0_i69) AND
		LABEL(mercury__lexer__get_token_2_3_0_i71) AND
		LABEL(mercury__lexer__get_token_2_3_0_i73) AND
		LABEL(mercury__lexer__get_token_2_3_0_i75) AND
		LABEL(mercury__lexer__get_token_2_3_0_i77) AND
		LABEL(mercury__lexer__get_token_2_3_0_i79) AND
		LABEL(mercury__lexer__get_token_2_3_0_i81) AND
		LABEL(mercury__lexer__get_token_2_3_0_i83) AND
		LABEL(mercury__lexer__get_token_2_3_0_i85) AND
		LABEL(mercury__lexer__get_token_2_3_0_i87) AND
		LABEL(mercury__lexer__get_token_2_3_0_i89) AND
		LABEL(mercury__lexer__get_token_2_3_0_i91) AND
		LABEL(mercury__lexer__get_token_2_3_0_i93) AND
		LABEL(mercury__lexer__get_token_2_3_0_i94) AND
		LABEL(mercury__lexer__get_token_2_3_0_i96) AND
		LABEL(mercury__lexer__get_token_2_3_0_i98) AND
		LABEL(mercury__lexer__get_token_2_3_0_i100) AND
		LABEL(mercury__lexer__get_token_2_3_0_i102) AND
		LABEL(mercury__lexer__get_token_2_3_0_i104) AND
		LABEL(mercury__lexer__get_token_2_3_0_i106) AND
		LABEL(mercury__lexer__get_token_2_3_0_i108) AND
		LABEL(mercury__lexer__get_token_2_3_0_i110) AND
		LABEL(mercury__lexer__get_token_2_3_0_i112) AND
		LABEL(mercury__lexer__get_token_2_3_0_i114) AND
		LABEL(mercury__lexer__get_token_2_3_0_i116) AND
		LABEL(mercury__lexer__get_token_2_3_0_i118) AND
		LABEL(mercury__lexer__get_token_2_3_0_i120) AND
		LABEL(mercury__lexer__get_token_2_3_0_i122) AND
		LABEL(mercury__lexer__get_token_2_3_0_i124) AND
		LABEL(mercury__lexer__get_token_2_3_0_i126) AND
		LABEL(mercury__lexer__get_token_2_3_0_i128) AND
		LABEL(mercury__lexer__get_token_2_3_0_i130) AND
		LABEL(mercury__lexer__get_token_2_3_0_i132) AND
		LABEL(mercury__lexer__get_token_2_3_0_i134) AND
		LABEL(mercury__lexer__get_token_2_3_0_i136) AND
		LABEL(mercury__lexer__get_token_2_3_0_i138) AND
		LABEL(mercury__lexer__get_token_2_3_0_i140) AND
		LABEL(mercury__lexer__get_token_2_3_0_i142) AND
		LABEL(mercury__lexer__get_token_2_3_0_i144) AND
		LABEL(mercury__lexer__get_token_2_3_0_i146) AND
		LABEL(mercury__lexer__get_token_2_3_0_i148) AND
		LABEL(mercury__lexer__get_token_2_3_0_i150) AND
		LABEL(mercury__lexer__get_token_2_3_0_i152) AND
		LABEL(mercury__lexer__get_token_2_3_0_i154) AND
		LABEL(mercury__lexer__get_token_2_3_0_i156) AND
		LABEL(mercury__lexer__get_token_2_3_0_i157) AND
		LABEL(mercury__lexer__get_token_2_3_0_i159) AND
		LABEL(mercury__lexer__get_token_2_3_0_i160) AND
		LABEL(mercury__lexer__get_token_2_3_0_i162) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9) AND
		LABEL(mercury__lexer__get_token_2_3_0_i165) AND
		LABEL(mercury__lexer__get_token_2_3_0_i167) AND
		LABEL(mercury__lexer__get_token_2_3_0_i169) AND
		LABEL(mercury__lexer__get_token_2_3_0_i171) AND
		LABEL(mercury__lexer__get_token_2_3_0_i173) AND
		LABEL(mercury__lexer__get_token_2_3_0_i175) AND
		LABEL(mercury__lexer__get_token_2_3_0_i177) AND
		LABEL(mercury__lexer__get_token_2_3_0_i179) AND
		LABEL(mercury__lexer__get_token_2_3_0_i181) AND
		LABEL(mercury__lexer__get_token_2_3_0_i183) AND
		LABEL(mercury__lexer__get_token_2_3_0_i185) AND
		LABEL(mercury__lexer__get_token_2_3_0_i187) AND
		LABEL(mercury__lexer__get_token_2_3_0_i189) AND
		LABEL(mercury__lexer__get_token_2_3_0_i191) AND
		LABEL(mercury__lexer__get_token_2_3_0_i193) AND
		LABEL(mercury__lexer__get_token_2_3_0_i195) AND
		LABEL(mercury__lexer__get_token_2_3_0_i197) AND
		LABEL(mercury__lexer__get_token_2_3_0_i199) AND
		LABEL(mercury__lexer__get_token_2_3_0_i201) AND
		LABEL(mercury__lexer__get_token_2_3_0_i203) AND
		LABEL(mercury__lexer__get_token_2_3_0_i205) AND
		LABEL(mercury__lexer__get_token_2_3_0_i207) AND
		LABEL(mercury__lexer__get_token_2_3_0_i209) AND
		LABEL(mercury__lexer__get_token_2_3_0_i211) AND
		LABEL(mercury__lexer__get_token_2_3_0_i213) AND
		LABEL(mercury__lexer__get_token_2_3_0_i215) AND
		LABEL(mercury__lexer__get_token_2_3_0_i217) AND
		LABEL(mercury__lexer__get_token_2_3_0_i218) AND
		LABEL(mercury__lexer__get_token_2_3_0_i219) AND
		LABEL(mercury__lexer__get_token_2_3_0_i220) AND
		LABEL(mercury__lexer__get_token_2_3_0_i9));
Define_label(mercury__lexer__get_token_2_3_0_i9);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 3);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i17);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	localtailcall(mercury__lexer__get_token_2_3_0,
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i45);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i46);
	r3 = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i48);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_source_line_number_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i50);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i52);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__skip_to_eol_3_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i54);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i58);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i59);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 2)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i60);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i62);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i64);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 8)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i65);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i67);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_dot_3_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i69);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_slash_3_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i71);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_zero_3_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i73);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i75);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i77);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i79);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i81);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i83);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i85);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i87);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i89);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i91);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i93);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i94);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i96);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i98);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i100);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i102);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i104);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i106);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i108);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i110);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i112);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i114);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i116);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i118);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i120);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i122);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i124);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i126);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i128);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i130);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i132);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i134);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i136);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i138);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i140);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i142);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i144);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i146);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i148);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i150);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i152);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i154);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i156);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 3)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i157);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i159);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 4)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i160);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i162);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_variable_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i165);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i167);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i169);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i171);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i173);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i175);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i177);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i179);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i181);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i183);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i185);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i187);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i189);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i191);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i193);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i195);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i197);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i199);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i201);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i203);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i205);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i207);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i209);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i211);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i213);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i215);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_name_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i217);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 5)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i218);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 7)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i219);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 6)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_token_2_3_0_i220);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_token_2_3_0));
Define_label(mercury__lexer__get_token_2_3_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_token_2_3_0_i223,
		STATIC(mercury__lexer__get_token_2_3_0));
	}
Define_label(mercury__lexer__get_token_2_3_0_i223);
	update_prof_current_proc(LABEL(mercury__lexer__get_token_2_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module4)
	init_entry(mercury__lexer__graphic_token_char_1_0);
	init_label(mercury__lexer__graphic_token_char_1_0_i2);
	init_label(mercury__lexer__graphic_token_char_1_0_i1);
BEGIN_CODE

/* code for predicate 'lexer__graphic_token_char'/1 in mode 0 */
Define_static(mercury__lexer__graphic_token_char_1_0);
	if (((Integer) r1 == ((Integer) 35)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 36)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 38)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 42)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 43)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 45)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 46)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 47)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 58)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 60)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 61)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 62)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 63)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 64)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 92)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 == ((Integer) 94)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i2);
	if (((Integer) r1 != ((Integer) 126)))
		GOTO_LABEL(mercury__lexer__graphic_token_char_1_0_i1);
Define_label(mercury__lexer__graphic_token_char_1_0_i2);
	r1 = TRUE;
	proceed();
Define_label(mercury__lexer__graphic_token_char_1_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module5)
	init_entry(mercury__lexer__get_dot_3_0);
	init_label(mercury__lexer__get_dot_3_0_i2);
	init_label(mercury__lexer__get_dot_3_0_i6);
	init_label(mercury__lexer__get_dot_3_0_i5);
	init_label(mercury__lexer__get_dot_3_0_i11);
	init_label(mercury__lexer__get_dot_3_0_i12);
	init_label(mercury__lexer__get_dot_3_0_i13);
	init_label(mercury__lexer__get_dot_3_0_i8);
	init_label(mercury__lexer__get_dot_3_0_i18);
	init_label(mercury__lexer__get_dot_3_0_i17);
	init_label(mercury__lexer__get_dot_3_0_i21);
	init_label(mercury__lexer__get_dot_3_0_i7);
	init_label(mercury__lexer__get_dot_3_0_i24);
BEGIN_CODE

/* code for predicate 'lexer__get_dot'/3 in mode 0 */
Define_static(mercury__lexer__get_dot_3_0);
	incr_sp_push_msg(3, "lexer__get_dot");
	detstackvar(3) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_dot_3_0_i2,
		STATIC(mercury__lexer__get_dot_3_0));
	}
Define_label(mercury__lexer__get_dot_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_dot_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_dot_3_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_dot_3_0_i6,
		STATIC(mercury__lexer__get_dot_3_0));
	}
Define_label(mercury__lexer__get_dot_3_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_dot_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 9)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_dot_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_dot_3_0_i7);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 != ((Integer) 9)))
		GOTO_LABEL(mercury__lexer__get_dot_3_0_i11);
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_dot_3_0_i6,
		STATIC(mercury__lexer__get_dot_3_0));
	}
Define_label(mercury__lexer__get_dot_3_0_i11);
	if (((Integer) r3 != ((Integer) 10)))
		GOTO_LABEL(mercury__lexer__get_dot_3_0_i12);
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_dot_3_0_i6,
		STATIC(mercury__lexer__get_dot_3_0));
	}
Define_label(mercury__lexer__get_dot_3_0_i12);
	if (((Integer) r3 != ((Integer) 32)))
		GOTO_LABEL(mercury__lexer__get_dot_3_0_i13);
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_dot_3_0_i6,
		STATIC(mercury__lexer__get_dot_3_0));
	}
Define_label(mercury__lexer__get_dot_3_0_i13);
	if (((Integer) r3 != ((Integer) 37)))
		GOTO_LABEL(mercury__lexer__get_dot_3_0_i8);
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_dot_3_0_i6,
		STATIC(mercury__lexer__get_dot_3_0));
	}
Define_label(mercury__lexer__get_dot_3_0_i8);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__lexer__graphic_token_char_1_0),
		mercury__lexer__get_dot_3_0_i18,
		STATIC(mercury__lexer__get_dot_3_0));
Define_label(mercury__lexer__get_dot_3_0_i18);
	update_prof_current_proc(LABEL(mercury__lexer__get_dot_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_dot_3_0_i17);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_6);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_dot_3_0));
Define_label(mercury__lexer__get_dot_3_0_i17);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_dot_3_0_i21,
		STATIC(mercury__lexer__get_dot_3_0));
	}
Define_label(mercury__lexer__get_dot_3_0_i21);
	update_prof_current_proc(LABEL(mercury__lexer__get_dot_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_dot_3_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_dot_3_0_i24,
		STATIC(mercury__lexer__get_dot_3_0));
	}
Define_label(mercury__lexer__get_dot_3_0_i24);
	update_prof_current_proc(LABEL(mercury__lexer__get_dot_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module6)
	init_entry(mercury__lexer__skip_to_eol_3_0);
	init_label(mercury__lexer__skip_to_eol_3_0_i2);
	init_label(mercury__lexer__skip_to_eol_3_0_i6);
	init_label(mercury__lexer__skip_to_eol_3_0_i5);
	init_label(mercury__lexer__skip_to_eol_3_0_i7);
	init_label(mercury__lexer__skip_to_eol_3_0_i14);
	init_label(mercury__lexer__skip_to_eol_3_0_i1001);
BEGIN_CODE

/* code for predicate 'lexer__skip_to_eol'/3 in mode 0 */
Define_static(mercury__lexer__skip_to_eol_3_0);
	incr_sp_push_msg(2, "lexer__skip_to_eol");
	detstackvar(2) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__skip_to_eol_3_0_i2,
		STATIC(mercury__lexer__skip_to_eol_3_0));
	}
Define_label(mercury__lexer__skip_to_eol_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__skip_to_eol_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__skip_to_eol_3_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__skip_to_eol_3_0_i6,
		STATIC(mercury__lexer__skip_to_eol_3_0));
	}
Define_label(mercury__lexer__skip_to_eol_3_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__skip_to_eol_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 10)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__skip_to_eol_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__skip_to_eol_3_0_i7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 0)) != ((Integer) 10)))
		GOTO_LABEL(mercury__lexer__skip_to_eol_3_0_i1001);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__lexer__get_token_2_3_0),
		STATIC(mercury__lexer__skip_to_eol_3_0));
Define_label(mercury__lexer__skip_to_eol_3_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__skip_to_eol_3_0_i14,
		STATIC(mercury__lexer__skip_to_eol_3_0));
	}
Define_label(mercury__lexer__skip_to_eol_3_0_i14);
	update_prof_current_proc(LABEL(mercury__lexer__skip_to_eol_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__skip_to_eol_3_0_i1001);
	r1 = (Integer) r2;
	localtailcall(mercury__lexer__skip_to_eol_3_0,
		STATIC(mercury__lexer__skip_to_eol_3_0));
END_MODULE

BEGIN_MODULE(mercury__lexer_module7)
	init_entry(mercury__lexer__get_slash_3_0);
	init_label(mercury__lexer__get_slash_3_0_i2);
	init_label(mercury__lexer__get_slash_3_0_i6);
	init_label(mercury__lexer__get_slash_3_0_i5);
	init_label(mercury__lexer__get_slash_3_0_i8);
	init_label(mercury__lexer__get_slash_3_0_i14);
	init_label(mercury__lexer__get_slash_3_0_i13);
	init_label(mercury__lexer__get_slash_3_0_i7);
	init_label(mercury__lexer__get_slash_3_0_i20);
BEGIN_CODE

/* code for predicate 'lexer__get_slash'/3 in mode 0 */
Define_static(mercury__lexer__get_slash_3_0);
	incr_sp_push_msg(3, "lexer__get_slash");
	detstackvar(3) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_slash_3_0_i2,
		STATIC(mercury__lexer__get_slash_3_0));
	}
Define_label(mercury__lexer__get_slash_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_slash_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_slash_3_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_slash_3_0_i6,
		STATIC(mercury__lexer__get_slash_3_0));
	}
Define_label(mercury__lexer__get_slash_3_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_slash_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_8);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_slash_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_slash_3_0_i7);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 != ((Integer) 42)))
		GOTO_LABEL(mercury__lexer__get_slash_3_0_i8);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_comment_3_0),
		STATIC(mercury__lexer__get_slash_3_0));
Define_label(mercury__lexer__get_slash_3_0_i8);
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__lexer__graphic_token_char_1_0),
		mercury__lexer__get_slash_3_0_i14,
		STATIC(mercury__lexer__get_slash_3_0));
Define_label(mercury__lexer__get_slash_3_0_i14);
	update_prof_current_proc(LABEL(mercury__lexer__get_slash_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_slash_3_0_i13);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_9);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_graphic_4_0),
		STATIC(mercury__lexer__get_slash_3_0));
Define_label(mercury__lexer__get_slash_3_0_i13);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_slash_3_0_i6,
		STATIC(mercury__lexer__get_slash_3_0));
	}
Define_label(mercury__lexer__get_slash_3_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_slash_3_0_i20,
		STATIC(mercury__lexer__get_slash_3_0));
	}
Define_label(mercury__lexer__get_slash_3_0_i20);
	update_prof_current_proc(LABEL(mercury__lexer__get_slash_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module8)
	init_entry(mercury__lexer__get_comment_3_0);
	init_label(mercury__lexer__get_comment_3_0_i2);
	init_label(mercury__lexer__get_comment_3_0_i6);
	init_label(mercury__lexer__get_comment_3_0_i5);
	init_label(mercury__lexer__get_comment_3_0_i7);
	init_label(mercury__lexer__get_comment_3_0_i14);
	init_label(mercury__lexer__get_comment_3_0_i1001);
BEGIN_CODE

/* code for predicate 'lexer__get_comment'/3 in mode 0 */
Define_static(mercury__lexer__get_comment_3_0);
	incr_sp_push_msg(2, "lexer__get_comment");
	detstackvar(2) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_comment_3_0_i2,
		STATIC(mercury__lexer__get_comment_3_0));
	}
Define_label(mercury__lexer__get_comment_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_comment_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_comment_3_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_comment_3_0_i6,
		STATIC(mercury__lexer__get_comment_3_0));
	}
Define_label(mercury__lexer__get_comment_3_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_comment_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_10);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_comment_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_comment_3_0_i7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (((Integer) field(mktag(1), (Integer) r1, ((Integer) 0)) != ((Integer) 42)))
		GOTO_LABEL(mercury__lexer__get_comment_3_0_i1001);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__lexer__get_comment_2_3_0),
		STATIC(mercury__lexer__get_comment_3_0));
Define_label(mercury__lexer__get_comment_3_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_comment_3_0_i14,
		STATIC(mercury__lexer__get_comment_3_0));
	}
Define_label(mercury__lexer__get_comment_3_0_i14);
	update_prof_current_proc(LABEL(mercury__lexer__get_comment_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_comment_3_0_i1001);
	r1 = (Integer) r2;
	localtailcall(mercury__lexer__get_comment_3_0,
		STATIC(mercury__lexer__get_comment_3_0));
END_MODULE

BEGIN_MODULE(mercury__lexer_module9)
	init_entry(mercury__lexer__get_comment_2_3_0);
	init_label(mercury__lexer__get_comment_2_3_0_i2);
	init_label(mercury__lexer__get_comment_2_3_0_i6);
	init_label(mercury__lexer__get_comment_2_3_0_i5);
	init_label(mercury__lexer__get_comment_2_3_0_i8);
	init_label(mercury__lexer__get_comment_2_3_0_i7);
	init_label(mercury__lexer__get_comment_2_3_0_i19);
	init_label(mercury__lexer__get_comment_2_3_0_i1001);
BEGIN_CODE

/* code for predicate 'lexer__get_comment_2'/3 in mode 0 */
Define_static(mercury__lexer__get_comment_2_3_0);
	incr_sp_push_msg(2, "lexer__get_comment_2");
	detstackvar(2) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_comment_2_3_0_i2,
		STATIC(mercury__lexer__get_comment_2_3_0));
	}
Define_label(mercury__lexer__get_comment_2_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_comment_2_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_comment_2_3_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_comment_2_3_0_i6,
		STATIC(mercury__lexer__get_comment_2_3_0));
	}
Define_label(mercury__lexer__get_comment_2_3_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_comment_2_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_10);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_comment_2_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_comment_2_3_0_i7);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 != ((Integer) 47)))
		GOTO_LABEL(mercury__lexer__get_comment_2_3_0_i8);
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__lexer__get_token_2_3_0),
		STATIC(mercury__lexer__get_comment_2_3_0));
Define_label(mercury__lexer__get_comment_2_3_0_i8);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	if (((Integer) r3 != ((Integer) 42)))
		GOTO_LABEL(mercury__lexer__get_comment_2_3_0_i1001);
	r1 = (Integer) r2;
	localtailcall(mercury__lexer__get_comment_2_3_0,
		STATIC(mercury__lexer__get_comment_2_3_0));
Define_label(mercury__lexer__get_comment_2_3_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_comment_2_3_0_i19,
		STATIC(mercury__lexer__get_comment_2_3_0));
	}
Define_label(mercury__lexer__get_comment_2_3_0_i19);
	update_prof_current_proc(LABEL(mercury__lexer__get_comment_2_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__get_comment_2_3_0_i1001);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury__lexer__get_comment_3_0),
		STATIC(mercury__lexer__get_comment_2_3_0));
END_MODULE

BEGIN_MODULE(mercury__lexer_module10)
	init_entry(mercury__lexer__get_quoted_name_5_0);
	init_label(mercury__lexer__get_quoted_name_5_0_i2);
	init_label(mercury__lexer__get_quoted_name_5_0_i6);
	init_label(mercury__lexer__get_quoted_name_5_0_i5);
	init_label(mercury__lexer__get_quoted_name_5_0_i8);
	init_label(mercury__lexer__get_quoted_name_5_0_i11);
	init_label(mercury__lexer__get_quoted_name_5_0_i7);
	init_label(mercury__lexer__get_quoted_name_5_0_i18);
BEGIN_CODE

/* code for predicate 'lexer__get_quoted_name'/5 in mode 0 */
Define_static(mercury__lexer__get_quoted_name_5_0);
	incr_sp_push_msg(3, "lexer__get_quoted_name");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_quoted_name_5_0_i2,
		STATIC(mercury__lexer__get_quoted_name_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_5_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_5_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_quoted_name_5_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_quoted_name_5_0_i6,
		STATIC(mercury__lexer__get_quoted_name_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_5_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_11);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_quoted_name_5_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_quoted_name_5_0_i7);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 != (Integer) detstackvar(1)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_5_0_i8);
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_quoted_name_quote_5_0),
		STATIC(mercury__lexer__get_quoted_name_5_0));
Define_label(mercury__lexer__get_quoted_name_5_0_i8);
	if (((Integer) r3 != ((Integer) 92)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_5_0_i11);
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_quoted_name_escape_5_0),
		STATIC(mercury__lexer__get_quoted_name_5_0));
Define_label(mercury__lexer__get_quoted_name_5_0_i11);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) tempr1;
	r1 = (Integer) detstackvar(1);
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__lexer__get_quoted_name_5_0,
		STATIC(mercury__lexer__get_quoted_name_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_5_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_quoted_name_5_0_i18,
		STATIC(mercury__lexer__get_quoted_name_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_5_0_i18);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_5_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module11)
	init_entry(mercury__lexer__get_quoted_name_quote_5_0);
	init_label(mercury__lexer__get_quoted_name_quote_5_0_i2);
	init_label(mercury__lexer__get_quoted_name_quote_5_0_i6);
	init_label(mercury__lexer__get_quoted_name_quote_5_0_i7);
	init_label(mercury__lexer__get_quoted_name_quote_5_0_i5);
	init_label(mercury__lexer__get_quoted_name_quote_5_0_i9);
	init_label(mercury__lexer__get_quoted_name_quote_5_0_i8);
	init_label(mercury__lexer__get_quoted_name_quote_5_0_i15);
BEGIN_CODE

/* code for predicate 'lexer__get_quoted_name_quote'/5 in mode 0 */
Define_static(mercury__lexer__get_quoted_name_quote_5_0);
	incr_sp_push_msg(3, "lexer__get_quoted_name_quote");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_quoted_name_quote_5_0_i2,
		STATIC(mercury__lexer__get_quoted_name_quote_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_quote_5_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_quote_5_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_quoted_name_quote_5_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_quoted_name_quote_5_0_i6,
		STATIC(mercury__lexer__get_quoted_name_quote_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_quote_5_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_quote_5_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury__lexer__finish_quoted_name_3_0),
		mercury__lexer__get_quoted_name_quote_5_0_i7,
		STATIC(mercury__lexer__get_quoted_name_quote_5_0));
Define_label(mercury__lexer__get_quoted_name_quote_5_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_quote_5_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_quoted_name_quote_5_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_quoted_name_quote_5_0_i8);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 != (Integer) detstackvar(1)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_quote_5_0_i9);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	r3 = (Integer) r2;
	r2 = (Integer) tempr1;
	r1 = (Integer) detstackvar(1);
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_quote_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_quote_5_0_i9);
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_quoted_name_quote_5_0_i6,
		STATIC(mercury__lexer__get_quoted_name_quote_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_quote_5_0_i8);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_quoted_name_quote_5_0_i15,
		STATIC(mercury__lexer__get_quoted_name_quote_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_quote_5_0_i15);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_quote_5_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module12)
	init_entry(mercury__lexer__finish_quoted_name_3_0);
	init_label(mercury__lexer__finish_quoted_name_3_0_i2);
	init_label(mercury__lexer__finish_quoted_name_3_0_i3);
	init_label(mercury__lexer__finish_quoted_name_3_0_i6);
BEGIN_CODE

/* code for predicate 'lexer__finish_quoted_name'/3 in mode 0 */
Define_static(mercury__lexer__finish_quoted_name_3_0);
	incr_sp_push_msg(2, "lexer__finish_quoted_name");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__lexer__rev_char_list_to_string_2_0),
		mercury__lexer__finish_quoted_name_3_0_i2,
		STATIC(mercury__lexer__finish_quoted_name_3_0));
Define_label(mercury__lexer__finish_quoted_name_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__finish_quoted_name_3_0));
	if (((Integer) detstackvar(1) != ((Integer) 39)))
		GOTO_LABEL(mercury__lexer__finish_quoted_name_3_0_i3);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__finish_quoted_name_3_0_i3);
	if (((Integer) detstackvar(1) != ((Integer) 34)))
		GOTO_LABEL(mercury__lexer__finish_quoted_name_3_0_i6);
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 2);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__finish_quoted_name_3_0_i6);
	r1 = string_const("lexer.m: unknown quote character", 32);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		STATIC(mercury__lexer__finish_quoted_name_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__lexer_module13)
	init_entry(mercury__lexer__get_quoted_name_escape_5_0);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i2);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i6);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i5);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i8);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i9);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i16);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i17);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i18);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i19);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i20);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i21);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i22);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i23);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i24);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i25);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i13);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i28);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i34);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i33);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i7);
	init_label(mercury__lexer__get_quoted_name_escape_5_0_i41);
BEGIN_CODE

/* code for predicate 'lexer__get_quoted_name_escape'/5 in mode 0 */
Define_static(mercury__lexer__get_quoted_name_escape_5_0);
	incr_sp_push_msg(5, "lexer__get_quoted_name_escape");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_quoted_name_escape_5_0_i2,
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_escape_5_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_quoted_name_escape_5_0_i6,
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_escape_5_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_12);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i7);
	detstackvar(3) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_quoted_name_escape_5_0_i8,
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i8);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_escape_5_0));
	if (((Integer) detstackvar(3) != ((Integer) 10)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i9);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i9);
	if (((Integer) detstackvar(3) != ((Integer) 34)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i16);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r5 = ((Integer) 34);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i16);
	if (((Integer) detstackvar(3) != ((Integer) 39)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i17);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r5 = ((Integer) 39);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i17);
	if (((Integer) detstackvar(3) != ((Integer) 92)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i18);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r5 = ((Integer) 92);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i18);
	if (((Integer) detstackvar(3) != ((Integer) 96)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i19);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r5 = ((Integer) 96);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i19);
	if (((Integer) detstackvar(3) != ((Integer) 97)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i20);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r5 = ((Integer) 7);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i20);
	if (((Integer) detstackvar(3) != ((Integer) 98)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i21);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r5 = ((Integer) 8);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i21);
	if (((Integer) detstackvar(3) != ((Integer) 102)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i22);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r5 = ((Integer) 12);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i22);
	if (((Integer) detstackvar(3) != ((Integer) 110)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i23);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r5 = ((Integer) 10);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i23);
	if (((Integer) detstackvar(3) != ((Integer) 114)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i24);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r5 = ((Integer) 13);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i24);
	if (((Integer) detstackvar(3) != ((Integer) 116)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i25);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r5 = ((Integer) 9);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i25);
	if (((Integer) detstackvar(3) != ((Integer) 118)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i13);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r5 = ((Integer) 11);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r5;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i13);
	if (((Integer) detstackvar(3) != ((Integer) 120)))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i28);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_hex_escape_6_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i28);
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__char__is_octal_digit_1_0);
	call_localret(ENTRY(mercury__char__is_octal_digit_1_0),
		mercury__lexer__get_quoted_name_escape_5_0_i34,
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i34);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_escape_5_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_quoted_name_escape_5_0_i33);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(3);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r4 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__lexer__get_octal_escape_6_0),
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i33);
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_13);
	r2 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_quoted_name_escape_5_0_i41,
		STATIC(mercury__lexer__get_quoted_name_escape_5_0));
	}
Define_label(mercury__lexer__get_quoted_name_escape_5_0_i41);
	update_prof_current_proc(LABEL(mercury__lexer__get_quoted_name_escape_5_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module14)
	init_entry(mercury__lexer__get_hex_escape_6_0);
	init_label(mercury__lexer__get_hex_escape_6_0_i2);
	init_label(mercury__lexer__get_hex_escape_6_0_i6);
	init_label(mercury__lexer__get_hex_escape_6_0_i5);
	init_label(mercury__lexer__get_hex_escape_6_0_i8);
	init_label(mercury__lexer__get_hex_escape_6_0_i11);
	init_label(mercury__lexer__get_hex_escape_6_0_i10);
	init_label(mercury__lexer__get_hex_escape_6_0_i14);
	init_label(mercury__lexer__get_hex_escape_6_0_i7);
	init_label(mercury__lexer__get_hex_escape_6_0_i20);
BEGIN_CODE

/* code for predicate 'lexer__get_hex_escape'/6 in mode 0 */
Define_static(mercury__lexer__get_hex_escape_6_0);
	incr_sp_push_msg(6, "lexer__get_hex_escape");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) r4;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_hex_escape_6_0_i2,
		STATIC(mercury__lexer__get_hex_escape_6_0));
	}
Define_label(mercury__lexer__get_hex_escape_6_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_hex_escape_6_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_hex_escape_6_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_hex_escape_6_0_i6,
		STATIC(mercury__lexer__get_hex_escape_6_0));
	}
Define_label(mercury__lexer__get_hex_escape_6_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_hex_escape_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_11);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__lexer__get_hex_escape_6_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_hex_escape_6_0_i7);
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_hex_escape_6_0_i8,
		STATIC(mercury__lexer__get_hex_escape_6_0));
	}
Define_label(mercury__lexer__get_hex_escape_6_0_i8);
	update_prof_current_proc(LABEL(mercury__lexer__get_hex_escape_6_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__char__is_hex_digit_1_0);
	call_localret(ENTRY(mercury__char__is_hex_digit_1_0),
		mercury__lexer__get_hex_escape_6_0_i11,
		STATIC(mercury__lexer__get_hex_escape_6_0));
	}
Define_label(mercury__lexer__get_hex_escape_6_0_i11);
	update_prof_current_proc(LABEL(mercury__lexer__get_hex_escape_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_hex_escape_6_0_i10);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__lexer__get_hex_escape_6_0,
		STATIC(mercury__lexer__get_hex_escape_6_0));
Define_label(mercury__lexer__get_hex_escape_6_0_i10);
	r1 = (Integer) detstackvar(5);
	if (((Integer) detstackvar(4) != ((Integer) 92)))
		GOTO_LABEL(mercury__lexer__get_hex_escape_6_0_i14);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__lexer__finish_hex_escape_6_0),
		STATIC(mercury__lexer__get_hex_escape_6_0));
Define_label(mercury__lexer__get_hex_escape_6_0_i14);
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_14);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__lexer__get_hex_escape_6_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_hex_escape_6_0_i20,
		STATIC(mercury__lexer__get_hex_escape_6_0));
	}
Define_label(mercury__lexer__get_hex_escape_6_0_i20);
	update_prof_current_proc(LABEL(mercury__lexer__get_hex_escape_6_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module15)
	init_entry(mercury__lexer__finish_hex_escape_6_0);
	init_label(mercury__lexer__finish_hex_escape_6_0_i1000);
	init_label(mercury__lexer__finish_hex_escape_6_0_i5);
	init_label(mercury__lexer__finish_hex_escape_6_0_i8);
	init_label(mercury__lexer__finish_hex_escape_6_0_i10);
	init_label(mercury__lexer__finish_hex_escape_6_0_i7);
BEGIN_CODE

/* code for predicate 'lexer__finish_hex_escape'/6 in mode 0 */
Define_static(mercury__lexer__finish_hex_escape_6_0);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__lexer__finish_hex_escape_6_0_i1000);
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_15);
	r2 = (Integer) r4;
	proceed();
Define_label(mercury__lexer__finish_hex_escape_6_0_i1000);
	incr_sp_push_msg(4, "lexer__finish_hex_escape");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__lexer__rev_char_list_to_string_2_0),
		mercury__lexer__finish_hex_escape_6_0_i5,
		STATIC(mercury__lexer__finish_hex_escape_6_0));
Define_label(mercury__lexer__finish_hex_escape_6_0_i5);
	update_prof_current_proc(LABEL(mercury__lexer__finish_hex_escape_6_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 16);
	{
	Declare_entry(mercury__string__base_string_to_int_3_0);
	call_localret(ENTRY(mercury__string__base_string_to_int_3_0),
		mercury__lexer__finish_hex_escape_6_0_i8,
		STATIC(mercury__lexer__finish_hex_escape_6_0));
	}
Define_label(mercury__lexer__finish_hex_escape_6_0_i8);
	update_prof_current_proc(LABEL(mercury__lexer__finish_hex_escape_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__finish_hex_escape_6_0_i7);
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__char__to_int_2_2);
	call_localret(ENTRY(mercury__char__to_int_2_2),
		mercury__lexer__finish_hex_escape_6_0_i10,
		STATIC(mercury__lexer__finish_hex_escape_6_0));
	}
Define_label(mercury__lexer__finish_hex_escape_6_0_i10);
	update_prof_current_proc(LABEL(mercury__lexer__finish_hex_escape_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__finish_hex_escape_6_0_i7);
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__finish_hex_escape_6_0));
Define_label(mercury__lexer__finish_hex_escape_6_0_i7);
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_16);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module16)
	init_entry(mercury__lexer__get_octal_escape_6_0);
	init_label(mercury__lexer__get_octal_escape_6_0_i2);
	init_label(mercury__lexer__get_octal_escape_6_0_i6);
	init_label(mercury__lexer__get_octal_escape_6_0_i5);
	init_label(mercury__lexer__get_octal_escape_6_0_i8);
	init_label(mercury__lexer__get_octal_escape_6_0_i11);
	init_label(mercury__lexer__get_octal_escape_6_0_i10);
	init_label(mercury__lexer__get_octal_escape_6_0_i14);
	init_label(mercury__lexer__get_octal_escape_6_0_i18);
	init_label(mercury__lexer__get_octal_escape_6_0_i7);
	init_label(mercury__lexer__get_octal_escape_6_0_i22);
BEGIN_CODE

/* code for predicate 'lexer__get_octal_escape'/6 in mode 0 */
Define_static(mercury__lexer__get_octal_escape_6_0);
	incr_sp_push_msg(6, "lexer__get_octal_escape");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r3;
	r1 = (Integer) r4;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_octal_escape_6_0_i2,
		STATIC(mercury__lexer__get_octal_escape_6_0));
	}
Define_label(mercury__lexer__get_octal_escape_6_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_escape_6_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_octal_escape_6_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_octal_escape_6_0_i6,
		STATIC(mercury__lexer__get_octal_escape_6_0));
	}
Define_label(mercury__lexer__get_octal_escape_6_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_escape_6_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_11);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
Define_label(mercury__lexer__get_octal_escape_6_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_octal_escape_6_0_i7);
	detstackvar(4) = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_octal_escape_6_0_i8,
		STATIC(mercury__lexer__get_octal_escape_6_0));
	}
Define_label(mercury__lexer__get_octal_escape_6_0_i8);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_escape_6_0));
	detstackvar(5) = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__char__is_octal_digit_1_0);
	call_localret(ENTRY(mercury__char__is_octal_digit_1_0),
		mercury__lexer__get_octal_escape_6_0_i11,
		STATIC(mercury__lexer__get_octal_escape_6_0));
	}
Define_label(mercury__lexer__get_octal_escape_6_0_i11);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_escape_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_octal_escape_6_0_i10);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(4);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) detstackvar(3);
	r4 = (Integer) detstackvar(5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	localtailcall(mercury__lexer__get_octal_escape_6_0,
		STATIC(mercury__lexer__get_octal_escape_6_0));
Define_label(mercury__lexer__get_octal_escape_6_0_i10);
	r1 = (Integer) detstackvar(5);
	if (((Integer) detstackvar(4) != ((Integer) 92)))
		GOTO_LABEL(mercury__lexer__get_octal_escape_6_0_i14);
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__lexer__finish_octal_escape_6_0),
		STATIC(mercury__lexer__get_octal_escape_6_0));
Define_label(mercury__lexer__get_octal_escape_6_0_i14);
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(4);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_octal_escape_6_0_i18,
		STATIC(mercury__lexer__get_octal_escape_6_0));
	}
Define_label(mercury__lexer__get_octal_escape_6_0_i18);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_escape_6_0));
	r4 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	tailcall(STATIC(mercury__lexer__finish_octal_escape_6_0),
		STATIC(mercury__lexer__get_octal_escape_6_0));
Define_label(mercury__lexer__get_octal_escape_6_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_octal_escape_6_0_i22,
		STATIC(mercury__lexer__get_octal_escape_6_0));
	}
Define_label(mercury__lexer__get_octal_escape_6_0_i22);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_escape_6_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module17)
	init_entry(mercury__lexer__finish_octal_escape_6_0);
	init_label(mercury__lexer__finish_octal_escape_6_0_i1000);
	init_label(mercury__lexer__finish_octal_escape_6_0_i5);
	init_label(mercury__lexer__finish_octal_escape_6_0_i8);
	init_label(mercury__lexer__finish_octal_escape_6_0_i10);
	init_label(mercury__lexer__finish_octal_escape_6_0_i7);
BEGIN_CODE

/* code for predicate 'lexer__finish_octal_escape'/6 in mode 0 */
Define_static(mercury__lexer__finish_octal_escape_6_0);
	if (((Integer) r3 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__lexer__finish_octal_escape_6_0_i1000);
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_17);
	r2 = (Integer) r4;
	proceed();
Define_label(mercury__lexer__finish_octal_escape_6_0_i1000);
	incr_sp_push_msg(4, "lexer__finish_octal_escape");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	detstackvar(3) = (Integer) r4;
	r1 = (Integer) r3;
	call_localret(STATIC(mercury__lexer__rev_char_list_to_string_2_0),
		mercury__lexer__finish_octal_escape_6_0_i5,
		STATIC(mercury__lexer__finish_octal_escape_6_0));
Define_label(mercury__lexer__finish_octal_escape_6_0_i5);
	update_prof_current_proc(LABEL(mercury__lexer__finish_octal_escape_6_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 8);
	{
	Declare_entry(mercury__string__base_string_to_int_3_0);
	call_localret(ENTRY(mercury__string__base_string_to_int_3_0),
		mercury__lexer__finish_octal_escape_6_0_i8,
		STATIC(mercury__lexer__finish_octal_escape_6_0));
	}
Define_label(mercury__lexer__finish_octal_escape_6_0_i8);
	update_prof_current_proc(LABEL(mercury__lexer__finish_octal_escape_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__finish_octal_escape_6_0_i7);
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__char__to_int_2_2);
	call_localret(ENTRY(mercury__char__to_int_2_2),
		mercury__lexer__finish_octal_escape_6_0_i10,
		STATIC(mercury__lexer__finish_octal_escape_6_0));
	}
Define_label(mercury__lexer__finish_octal_escape_6_0_i10);
	update_prof_current_proc(LABEL(mercury__lexer__finish_octal_escape_6_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__finish_octal_escape_6_0_i7);
	r1 = (Integer) detstackvar(1);
	r3 = (Integer) r2;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__lexer__get_quoted_name_5_0),
		STATIC(mercury__lexer__finish_octal_escape_6_0));
Define_label(mercury__lexer__finish_octal_escape_6_0_i7);
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_18);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module18)
	init_entry(mercury__lexer__get_name_4_0);
	init_label(mercury__lexer__get_name_4_0_i2);
	init_label(mercury__lexer__get_name_4_0_i6);
	init_label(mercury__lexer__get_name_4_0_i7);
	init_label(mercury__lexer__get_name_4_0_i5);
	init_label(mercury__lexer__get_name_4_0_i11);
	init_label(mercury__lexer__get_name_4_0_i10);
	init_label(mercury__lexer__get_name_4_0_i8);
	init_label(mercury__lexer__get_name_4_0_i17);
BEGIN_CODE

/* code for predicate 'lexer__get_name'/4 in mode 0 */
Define_static(mercury__lexer__get_name_4_0);
	incr_sp_push_msg(4, "lexer__get_name");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_name_4_0_i2,
		STATIC(mercury__lexer__get_name_4_0));
	}
Define_label(mercury__lexer__get_name_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_name_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_name_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_name_4_0_i6,
		STATIC(mercury__lexer__get_name_4_0));
	}
Define_label(mercury__lexer__get_name_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_name_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__lexer__rev_char_list_to_string_2_0),
		mercury__lexer__get_name_4_0_i7,
		STATIC(mercury__lexer__get_name_4_0));
Define_label(mercury__lexer__get_name_4_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_name_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_name_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_name_4_0_i8);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_alnum_or_underscore_1_0);
	call_localret(ENTRY(mercury__char__is_alnum_or_underscore_1_0),
		mercury__lexer__get_name_4_0_i11,
		STATIC(mercury__lexer__get_name_4_0));
	}
Define_label(mercury__lexer__get_name_4_0_i11);
	update_prof_current_proc(LABEL(mercury__lexer__get_name_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_name_4_0_i10);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__lexer__get_name_4_0,
		STATIC(mercury__lexer__get_name_4_0));
Define_label(mercury__lexer__get_name_4_0_i10);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_name_4_0_i6,
		STATIC(mercury__lexer__get_name_4_0));
	}
Define_label(mercury__lexer__get_name_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_name_4_0_i17,
		STATIC(mercury__lexer__get_name_4_0));
	}
Define_label(mercury__lexer__get_name_4_0_i17);
	update_prof_current_proc(LABEL(mercury__lexer__get_name_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module19)
	init_entry(mercury__lexer__get_source_line_number_4_0);
	init_label(mercury__lexer__get_source_line_number_4_0_i2);
	init_label(mercury__lexer__get_source_line_number_4_0_i6);
	init_label(mercury__lexer__get_source_line_number_4_0_i5);
	init_label(mercury__lexer__get_source_line_number_4_0_i10);
	init_label(mercury__lexer__get_source_line_number_4_0_i9);
	init_label(mercury__lexer__get_source_line_number_4_0_i16);
	init_label(mercury__lexer__get_source_line_number_4_0_i19);
	init_label(mercury__lexer__get_source_line_number_4_0_i21);
	init_label(mercury__lexer__get_source_line_number_4_0_i1013);
	init_label(mercury__lexer__get_source_line_number_4_0_i23);
	init_label(mercury__lexer__get_source_line_number_4_0_i13);
	init_label(mercury__lexer__get_source_line_number_4_0_i25);
	init_label(mercury__lexer__get_source_line_number_4_0_i7);
	init_label(mercury__lexer__get_source_line_number_4_0_i29);
BEGIN_CODE

/* code for predicate 'lexer__get_source_line_number'/4 in mode 0 */
Define_static(mercury__lexer__get_source_line_number_4_0);
	incr_sp_push_msg(4, "lexer__get_source_line_number");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_source_line_number_4_0_i2,
		STATIC(mercury__lexer__get_source_line_number_4_0));
	}
Define_label(mercury__lexer__get_source_line_number_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_source_line_number_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_source_line_number_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_source_line_number_4_0_i6,
		STATIC(mercury__lexer__get_source_line_number_4_0));
	}
Define_label(mercury__lexer__get_source_line_number_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_source_line_number_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_19);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_source_line_number_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_source_line_number_4_0_i7);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_digit_1_0);
	call_localret(ENTRY(mercury__char__is_digit_1_0),
		mercury__lexer__get_source_line_number_4_0_i10,
		STATIC(mercury__lexer__get_source_line_number_4_0));
	}
Define_label(mercury__lexer__get_source_line_number_4_0_i10);
	update_prof_current_proc(LABEL(mercury__lexer__get_source_line_number_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_source_line_number_4_0_i9);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__lexer__get_source_line_number_4_0,
		STATIC(mercury__lexer__get_source_line_number_4_0));
Define_label(mercury__lexer__get_source_line_number_4_0_i9);
	r2 = (Integer) detstackvar(3);
	r3 = (Integer) detstackvar(2);
	if (((Integer) r3 != ((Integer) 10)))
		GOTO_LABEL(mercury__lexer__get_source_line_number_4_0_i13);
	r1 = (Integer) detstackvar(1);
	call_localret(STATIC(mercury__lexer__rev_char_list_to_string_2_0),
		mercury__lexer__get_source_line_number_4_0_i16,
		STATIC(mercury__lexer__get_source_line_number_4_0));
Define_label(mercury__lexer__get_source_line_number_4_0_i16);
	update_prof_current_proc(LABEL(mercury__lexer__get_source_line_number_4_0));
	r2 = (Integer) r1;
	detstackvar(1) = (Integer) r1;
	r1 = ((Integer) 10);
	{
	Declare_entry(mercury__string__base_string_to_int_3_0);
	call_localret(ENTRY(mercury__string__base_string_to_int_3_0),
		mercury__lexer__get_source_line_number_4_0_i19,
		STATIC(mercury__lexer__get_source_line_number_4_0));
	}
Define_label(mercury__lexer__get_source_line_number_4_0_i19);
	update_prof_current_proc(LABEL(mercury__lexer__get_source_line_number_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_source_line_number_4_0_i1013);
	if (((Integer) r2 <= ((Integer) 0)))
		GOTO_LABEL(mercury__lexer__get_source_line_number_4_0_i1013);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__set_line_number_3_0);
	call_localret(ENTRY(mercury__io__set_line_number_3_0),
		mercury__lexer__get_source_line_number_4_0_i21,
		STATIC(mercury__lexer__get_source_line_number_4_0));
	}
Define_label(mercury__lexer__get_source_line_number_4_0_i21);
	update_prof_current_proc(LABEL(mercury__lexer__get_source_line_number_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__lexer__get_token_1_3_0),
		STATIC(mercury__lexer__get_source_line_number_4_0));
Define_label(mercury__lexer__get_source_line_number_4_0_i1013);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("invalid line number `", 21);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_20);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	{
	Declare_entry(mercury__string__append_list_2_0);
	call_localret(ENTRY(mercury__string__append_list_2_0),
		mercury__lexer__get_source_line_number_4_0_i23,
		STATIC(mercury__lexer__get_source_line_number_4_0));
	}
Define_label(mercury__lexer__get_source_line_number_4_0_i23);
	update_prof_current_proc(LABEL(mercury__lexer__get_source_line_number_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 4);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_source_line_number_4_0_i13);
	detstackvar(3) = (Integer) r2;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__string__from_char_list_2_0);
	call_localret(ENTRY(mercury__string__from_char_list_2_0),
		mercury__lexer__get_source_line_number_4_0_i25,
		STATIC(mercury__lexer__get_source_line_number_4_0));
	}
Define_label(mercury__lexer__get_source_line_number_4_0_i25);
	update_prof_current_proc(LABEL(mercury__lexer__get_source_line_number_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = string_const("invalid character `", 19);
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) r2;
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_20);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r3;
	{
	Declare_entry(mercury__string__append_list_2_0);
	call_localret(ENTRY(mercury__string__append_list_2_0),
		mercury__lexer__get_source_line_number_4_0_i23,
		STATIC(mercury__lexer__get_source_line_number_4_0));
	}
Define_label(mercury__lexer__get_source_line_number_4_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_source_line_number_4_0_i29,
		STATIC(mercury__lexer__get_source_line_number_4_0));
	}
Define_label(mercury__lexer__get_source_line_number_4_0_i29);
	update_prof_current_proc(LABEL(mercury__lexer__get_source_line_number_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module20)
	init_entry(mercury__lexer__get_graphic_4_0);
	init_label(mercury__lexer__get_graphic_4_0_i2);
	init_label(mercury__lexer__get_graphic_4_0_i6);
	init_label(mercury__lexer__get_graphic_4_0_i7);
	init_label(mercury__lexer__get_graphic_4_0_i5);
	init_label(mercury__lexer__get_graphic_4_0_i11);
	init_label(mercury__lexer__get_graphic_4_0_i10);
	init_label(mercury__lexer__get_graphic_4_0_i8);
	init_label(mercury__lexer__get_graphic_4_0_i17);
BEGIN_CODE

/* code for predicate 'lexer__get_graphic'/4 in mode 0 */
Define_static(mercury__lexer__get_graphic_4_0);
	incr_sp_push_msg(4, "lexer__get_graphic");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_graphic_4_0_i2,
		STATIC(mercury__lexer__get_graphic_4_0));
	}
Define_label(mercury__lexer__get_graphic_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_graphic_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_graphic_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_graphic_4_0_i6,
		STATIC(mercury__lexer__get_graphic_4_0));
	}
Define_label(mercury__lexer__get_graphic_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_graphic_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__lexer__rev_char_list_to_string_2_0),
		mercury__lexer__get_graphic_4_0_i7,
		STATIC(mercury__lexer__get_graphic_4_0));
Define_label(mercury__lexer__get_graphic_4_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_graphic_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_graphic_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_graphic_4_0_i8);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	call_localret(STATIC(mercury__lexer__graphic_token_char_1_0),
		mercury__lexer__get_graphic_4_0_i11,
		STATIC(mercury__lexer__get_graphic_4_0));
Define_label(mercury__lexer__get_graphic_4_0_i11);
	update_prof_current_proc(LABEL(mercury__lexer__get_graphic_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_graphic_4_0_i10);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__lexer__get_graphic_4_0,
		STATIC(mercury__lexer__get_graphic_4_0));
Define_label(mercury__lexer__get_graphic_4_0_i10);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_graphic_4_0_i6,
		STATIC(mercury__lexer__get_graphic_4_0));
	}
Define_label(mercury__lexer__get_graphic_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_graphic_4_0_i17,
		STATIC(mercury__lexer__get_graphic_4_0));
	}
Define_label(mercury__lexer__get_graphic_4_0_i17);
	update_prof_current_proc(LABEL(mercury__lexer__get_graphic_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module21)
	init_entry(mercury__lexer__get_variable_4_0);
	init_label(mercury__lexer__get_variable_4_0_i2);
	init_label(mercury__lexer__get_variable_4_0_i6);
	init_label(mercury__lexer__get_variable_4_0_i7);
	init_label(mercury__lexer__get_variable_4_0_i5);
	init_label(mercury__lexer__get_variable_4_0_i11);
	init_label(mercury__lexer__get_variable_4_0_i10);
	init_label(mercury__lexer__get_variable_4_0_i8);
	init_label(mercury__lexer__get_variable_4_0_i17);
BEGIN_CODE

/* code for predicate 'lexer__get_variable'/4 in mode 0 */
Define_static(mercury__lexer__get_variable_4_0);
	incr_sp_push_msg(4, "lexer__get_variable");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_variable_4_0_i2,
		STATIC(mercury__lexer__get_variable_4_0));
	}
Define_label(mercury__lexer__get_variable_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_variable_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_variable_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_variable_4_0_i6,
		STATIC(mercury__lexer__get_variable_4_0));
	}
Define_label(mercury__lexer__get_variable_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_variable_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__lexer__rev_char_list_to_string_2_0),
		mercury__lexer__get_variable_4_0_i7,
		STATIC(mercury__lexer__get_variable_4_0));
Define_label(mercury__lexer__get_variable_4_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_variable_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(2), ((Integer) 1));
	field(mktag(2), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_variable_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_variable_4_0_i8);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_alnum_or_underscore_1_0);
	call_localret(ENTRY(mercury__char__is_alnum_or_underscore_1_0),
		mercury__lexer__get_variable_4_0_i11,
		STATIC(mercury__lexer__get_variable_4_0));
	}
Define_label(mercury__lexer__get_variable_4_0_i11);
	update_prof_current_proc(LABEL(mercury__lexer__get_variable_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_variable_4_0_i10);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__lexer__get_variable_4_0,
		STATIC(mercury__lexer__get_variable_4_0));
Define_label(mercury__lexer__get_variable_4_0_i10);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_variable_4_0_i6,
		STATIC(mercury__lexer__get_variable_4_0));
	}
Define_label(mercury__lexer__get_variable_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_variable_4_0_i17,
		STATIC(mercury__lexer__get_variable_4_0));
	}
Define_label(mercury__lexer__get_variable_4_0_i17);
	update_prof_current_proc(LABEL(mercury__lexer__get_variable_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module22)
	init_entry(mercury__lexer__get_zero_3_0);
	init_label(mercury__lexer__get_zero_3_0_i2);
	init_label(mercury__lexer__get_zero_3_0_i6);
	init_label(mercury__lexer__get_zero_3_0_i5);
	init_label(mercury__lexer__get_zero_3_0_i10);
	init_label(mercury__lexer__get_zero_3_0_i9);
	init_label(mercury__lexer__get_zero_3_0_i16);
	init_label(mercury__lexer__get_zero_3_0_i20);
	init_label(mercury__lexer__get_zero_3_0_i19);
	init_label(mercury__lexer__get_zero_3_0_i22);
	init_label(mercury__lexer__get_zero_3_0_i21);
	init_label(mercury__lexer__get_zero_3_0_i23);
	init_label(mercury__lexer__get_zero_3_0_i13);
	init_label(mercury__lexer__get_zero_3_0_i27);
	init_label(mercury__lexer__get_zero_3_0_i31);
	init_label(mercury__lexer__get_zero_3_0_i30);
	init_label(mercury__lexer__get_zero_3_0_i35);
	init_label(mercury__lexer__get_zero_3_0_i34);
	init_label(mercury__lexer__get_zero_3_0_i32);
	init_label(mercury__lexer__get_zero_3_0_i24);
	init_label(mercury__lexer__get_zero_3_0_i44);
	init_label(mercury__lexer__get_zero_3_0_i48);
	init_label(mercury__lexer__get_zero_3_0_i47);
	init_label(mercury__lexer__get_zero_3_0_i52);
	init_label(mercury__lexer__get_zero_3_0_i51);
	init_label(mercury__lexer__get_zero_3_0_i41);
	init_label(mercury__lexer__get_zero_3_0_i61);
	init_label(mercury__lexer__get_zero_3_0_i65);
	init_label(mercury__lexer__get_zero_3_0_i64);
	init_label(mercury__lexer__get_zero_3_0_i69);
	init_label(mercury__lexer__get_zero_3_0_i68);
	init_label(mercury__lexer__get_zero_3_0_i58);
	init_label(mercury__lexer__get_zero_3_0_i75);
	init_label(mercury__lexer__get_zero_3_0_i82);
	init_label(mercury__lexer__get_zero_3_0_i81);
	init_label(mercury__lexer__get_zero_3_0_i79);
	init_label(mercury__lexer__get_zero_3_0_i7);
	init_label(mercury__lexer__get_zero_3_0_i93);
BEGIN_CODE

/* code for predicate 'lexer__get_zero'/3 in mode 0 */
Define_static(mercury__lexer__get_zero_3_0);
	incr_sp_push_msg(3, "lexer__get_zero");
	detstackvar(3) = (Integer) succip;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_zero_3_0_i2,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_zero_3_0_i6,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_21);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_zero_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i7);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_digit_1_0);
	call_localret(ENTRY(mercury__char__is_digit_1_0),
		mercury__lexer__get_zero_3_0_i10,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i10);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i9);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_number_4_0),
		STATIC(mercury__lexer__get_zero_3_0));
Define_label(mercury__lexer__get_zero_3_0_i9);
	r3 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	if (((Integer) r3 != ((Integer) 39)))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i13);
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_zero_3_0_i16,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i16);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i19);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_zero_3_0_i20,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i20);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_22);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_zero_3_0_i19);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i21);
	detstackvar(1) = (Integer) r2;
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	{
	Declare_entry(mercury__char__to_int_2_0);
	call_localret(ENTRY(mercury__char__to_int_2_0),
		mercury__lexer__get_zero_3_0_i22,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i22);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_zero_3_0_i21);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_zero_3_0_i23,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i23);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_zero_3_0_i13);
	if (((Integer) r3 != ((Integer) 98)))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i24);
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_zero_3_0_i27,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i27);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i30);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_zero_3_0_i31,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i31);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_23);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_zero_3_0_i30);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i32);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_binary_digit_1_0);
	call_localret(ENTRY(mercury__char__is_binary_digit_1_0),
		mercury__lexer__get_zero_3_0_i35,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i35);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i34);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_binary_2_4_0),
		STATIC(mercury__lexer__get_zero_3_0));
Define_label(mercury__lexer__get_zero_3_0_i34);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_zero_3_0_i31,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i32);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_zero_3_0_i23,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i24);
	if (((Integer) r3 != ((Integer) 111)))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i41);
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_zero_3_0_i44,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i44);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i47);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_zero_3_0_i48,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i48);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_24);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_zero_3_0_i47);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i32);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_octal_digit_1_0);
	call_localret(ENTRY(mercury__char__is_octal_digit_1_0),
		mercury__lexer__get_zero_3_0_i52,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i52);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i51);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_octal_2_4_0),
		STATIC(mercury__lexer__get_zero_3_0));
Define_label(mercury__lexer__get_zero_3_0_i51);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_zero_3_0_i48,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i41);
	if (((Integer) r3 != ((Integer) 120)))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i58);
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_zero_3_0_i61,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i61);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i64);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_zero_3_0_i65,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i65);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_25);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__lexer__get_zero_3_0_i64);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i32);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_hex_digit_1_0);
	call_localret(ENTRY(mercury__char__is_hex_digit_1_0),
		mercury__lexer__get_zero_3_0_i69,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i69);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i68);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_hex_2_4_0),
		STATIC(mercury__lexer__get_zero_3_0));
Define_label(mercury__lexer__get_zero_3_0_i68);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_zero_3_0_i65,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i58);
	if (((Integer) r3 != ((Integer) 46)))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i75);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_26);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_int_dot_4_0),
		STATIC(mercury__lexer__get_zero_3_0));
Define_label(mercury__lexer__get_zero_3_0_i75);
	if (((Integer) r3 != ((Integer) 69)))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i82);
	r1 = (Integer) r3;
	GOTO_LABEL(mercury__lexer__get_zero_3_0_i81);
Define_label(mercury__lexer__get_zero_3_0_i82);
	if (((Integer) r3 != ((Integer) 101)))
		GOTO_LABEL(mercury__lexer__get_zero_3_0_i79);
	r1 = (Integer) r3;
Define_label(mercury__lexer__get_zero_3_0_i81);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) mkword(mktag(1), (Integer) mercury_data_lexer__common_26);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	tailcall(STATIC(mercury__lexer__get_float_exponent_4_0),
		STATIC(mercury__lexer__get_zero_3_0));
Define_label(mercury__lexer__get_zero_3_0_i79);
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_zero_3_0_i6,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i7);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_zero_3_0_i93,
		STATIC(mercury__lexer__get_zero_3_0));
	}
Define_label(mercury__lexer__get_zero_3_0_i93);
	update_prof_current_proc(LABEL(mercury__lexer__get_zero_3_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module23)
	init_entry(mercury__lexer__get_binary_2_4_0);
	init_label(mercury__lexer__get_binary_2_4_0_i2);
	init_label(mercury__lexer__get_binary_2_4_0_i6);
	init_label(mercury__lexer__get_binary_2_4_0_i7);
	init_label(mercury__lexer__get_binary_2_4_0_i5);
	init_label(mercury__lexer__get_binary_2_4_0_i11);
	init_label(mercury__lexer__get_binary_2_4_0_i10);
	init_label(mercury__lexer__get_binary_2_4_0_i14);
	init_label(mercury__lexer__get_binary_2_4_0_i8);
	init_label(mercury__lexer__get_binary_2_4_0_i17);
BEGIN_CODE

/* code for predicate 'lexer__get_binary_2'/4 in mode 0 */
Define_static(mercury__lexer__get_binary_2_4_0);
	incr_sp_push_msg(4, "lexer__get_binary_2");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_binary_2_4_0_i2,
		STATIC(mercury__lexer__get_binary_2_4_0));
	}
Define_label(mercury__lexer__get_binary_2_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_binary_2_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_binary_2_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_binary_2_4_0_i6,
		STATIC(mercury__lexer__get_binary_2_4_0));
	}
Define_label(mercury__lexer__get_binary_2_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_binary_2_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 2);
	call_localret(STATIC(mercury__lexer__rev_char_list_to_int_3_0),
		mercury__lexer__get_binary_2_4_0_i7,
		STATIC(mercury__lexer__get_binary_2_4_0));
Define_label(mercury__lexer__get_binary_2_4_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_binary_2_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_binary_2_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_binary_2_4_0_i8);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_binary_digit_1_0);
	call_localret(ENTRY(mercury__char__is_binary_digit_1_0),
		mercury__lexer__get_binary_2_4_0_i11,
		STATIC(mercury__lexer__get_binary_2_4_0));
	}
Define_label(mercury__lexer__get_binary_2_4_0_i11);
	update_prof_current_proc(LABEL(mercury__lexer__get_binary_2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_binary_2_4_0_i10);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__lexer__get_binary_2_4_0,
		STATIC(mercury__lexer__get_binary_2_4_0));
Define_label(mercury__lexer__get_binary_2_4_0_i10);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_binary_2_4_0_i14,
		STATIC(mercury__lexer__get_binary_2_4_0));
	}
Define_label(mercury__lexer__get_binary_2_4_0_i14);
	update_prof_current_proc(LABEL(mercury__lexer__get_binary_2_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 2);
	call_localret(STATIC(mercury__lexer__rev_char_list_to_int_3_0),
		mercury__lexer__get_binary_2_4_0_i7,
		STATIC(mercury__lexer__get_binary_2_4_0));
Define_label(mercury__lexer__get_binary_2_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_binary_2_4_0_i17,
		STATIC(mercury__lexer__get_binary_2_4_0));
	}
Define_label(mercury__lexer__get_binary_2_4_0_i17);
	update_prof_current_proc(LABEL(mercury__lexer__get_binary_2_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module24)
	init_entry(mercury__lexer__get_octal_2_4_0);
	init_label(mercury__lexer__get_octal_2_4_0_i2);
	init_label(mercury__lexer__get_octal_2_4_0_i6);
	init_label(mercury__lexer__get_octal_2_4_0_i7);
	init_label(mercury__lexer__get_octal_2_4_0_i5);
	init_label(mercury__lexer__get_octal_2_4_0_i11);
	init_label(mercury__lexer__get_octal_2_4_0_i10);
	init_label(mercury__lexer__get_octal_2_4_0_i14);
	init_label(mercury__lexer__get_octal_2_4_0_i8);
	init_label(mercury__lexer__get_octal_2_4_0_i17);
BEGIN_CODE

/* code for predicate 'lexer__get_octal_2'/4 in mode 0 */
Define_static(mercury__lexer__get_octal_2_4_0);
	incr_sp_push_msg(4, "lexer__get_octal_2");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_octal_2_4_0_i2,
		STATIC(mercury__lexer__get_octal_2_4_0));
	}
Define_label(mercury__lexer__get_octal_2_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_2_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_octal_2_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_octal_2_4_0_i6,
		STATIC(mercury__lexer__get_octal_2_4_0));
	}
Define_label(mercury__lexer__get_octal_2_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_2_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 8);
	call_localret(STATIC(mercury__lexer__rev_char_list_to_int_3_0),
		mercury__lexer__get_octal_2_4_0_i7,
		STATIC(mercury__lexer__get_octal_2_4_0));
Define_label(mercury__lexer__get_octal_2_4_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_2_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_octal_2_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_octal_2_4_0_i8);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_octal_digit_1_0);
	call_localret(ENTRY(mercury__char__is_octal_digit_1_0),
		mercury__lexer__get_octal_2_4_0_i11,
		STATIC(mercury__lexer__get_octal_2_4_0));
	}
Define_label(mercury__lexer__get_octal_2_4_0_i11);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_octal_2_4_0_i10);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__lexer__get_octal_2_4_0,
		STATIC(mercury__lexer__get_octal_2_4_0));
Define_label(mercury__lexer__get_octal_2_4_0_i10);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_octal_2_4_0_i14,
		STATIC(mercury__lexer__get_octal_2_4_0));
	}
Define_label(mercury__lexer__get_octal_2_4_0_i14);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_2_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 8);
	call_localret(STATIC(mercury__lexer__rev_char_list_to_int_3_0),
		mercury__lexer__get_octal_2_4_0_i7,
		STATIC(mercury__lexer__get_octal_2_4_0));
Define_label(mercury__lexer__get_octal_2_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_octal_2_4_0_i17,
		STATIC(mercury__lexer__get_octal_2_4_0));
	}
Define_label(mercury__lexer__get_octal_2_4_0_i17);
	update_prof_current_proc(LABEL(mercury__lexer__get_octal_2_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module25)
	init_entry(mercury__lexer__get_hex_2_4_0);
	init_label(mercury__lexer__get_hex_2_4_0_i2);
	init_label(mercury__lexer__get_hex_2_4_0_i6);
	init_label(mercury__lexer__get_hex_2_4_0_i7);
	init_label(mercury__lexer__get_hex_2_4_0_i5);
	init_label(mercury__lexer__get_hex_2_4_0_i11);
	init_label(mercury__lexer__get_hex_2_4_0_i10);
	init_label(mercury__lexer__get_hex_2_4_0_i14);
	init_label(mercury__lexer__get_hex_2_4_0_i8);
	init_label(mercury__lexer__get_hex_2_4_0_i17);
BEGIN_CODE

/* code for predicate 'lexer__get_hex_2'/4 in mode 0 */
Define_static(mercury__lexer__get_hex_2_4_0);
	incr_sp_push_msg(4, "lexer__get_hex_2");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_hex_2_4_0_i2,
		STATIC(mercury__lexer__get_hex_2_4_0));
	}
Define_label(mercury__lexer__get_hex_2_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_hex_2_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_hex_2_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_hex_2_4_0_i6,
		STATIC(mercury__lexer__get_hex_2_4_0));
	}
Define_label(mercury__lexer__get_hex_2_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_hex_2_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 16);
	call_localret(STATIC(mercury__lexer__rev_char_list_to_int_3_0),
		mercury__lexer__get_hex_2_4_0_i7,
		STATIC(mercury__lexer__get_hex_2_4_0));
Define_label(mercury__lexer__get_hex_2_4_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_hex_2_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_hex_2_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_hex_2_4_0_i8);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_hex_digit_1_0);
	call_localret(ENTRY(mercury__char__is_hex_digit_1_0),
		mercury__lexer__get_hex_2_4_0_i11,
		STATIC(mercury__lexer__get_hex_2_4_0));
	}
Define_label(mercury__lexer__get_hex_2_4_0_i11);
	update_prof_current_proc(LABEL(mercury__lexer__get_hex_2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_hex_2_4_0_i10);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__lexer__get_hex_2_4_0,
		STATIC(mercury__lexer__get_hex_2_4_0));
Define_label(mercury__lexer__get_hex_2_4_0_i10);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_hex_2_4_0_i14,
		STATIC(mercury__lexer__get_hex_2_4_0));
	}
Define_label(mercury__lexer__get_hex_2_4_0_i14);
	update_prof_current_proc(LABEL(mercury__lexer__get_hex_2_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 16);
	call_localret(STATIC(mercury__lexer__rev_char_list_to_int_3_0),
		mercury__lexer__get_hex_2_4_0_i7,
		STATIC(mercury__lexer__get_hex_2_4_0));
Define_label(mercury__lexer__get_hex_2_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_hex_2_4_0_i17,
		STATIC(mercury__lexer__get_hex_2_4_0));
	}
Define_label(mercury__lexer__get_hex_2_4_0_i17);
	update_prof_current_proc(LABEL(mercury__lexer__get_hex_2_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module26)
	init_entry(mercury__lexer__get_number_4_0);
	init_label(mercury__lexer__get_number_4_0_i2);
	init_label(mercury__lexer__get_number_4_0_i6);
	init_label(mercury__lexer__get_number_4_0_i7);
	init_label(mercury__lexer__get_number_4_0_i5);
	init_label(mercury__lexer__get_number_4_0_i11);
	init_label(mercury__lexer__get_number_4_0_i10);
	init_label(mercury__lexer__get_number_4_0_i14);
	init_label(mercury__lexer__get_number_4_0_i21);
	init_label(mercury__lexer__get_number_4_0_i20);
	init_label(mercury__lexer__get_number_4_0_i18);
	init_label(mercury__lexer__get_number_4_0_i24);
	init_label(mercury__lexer__get_number_4_0_i8);
	init_label(mercury__lexer__get_number_4_0_i29);
BEGIN_CODE

/* code for predicate 'lexer__get_number'/4 in mode 0 */
Define_static(mercury__lexer__get_number_4_0);
	incr_sp_push_msg(4, "lexer__get_number");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_number_4_0_i2,
		STATIC(mercury__lexer__get_number_4_0));
	}
Define_label(mercury__lexer__get_number_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_number_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_number_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_number_4_0_i6,
		STATIC(mercury__lexer__get_number_4_0));
	}
Define_label(mercury__lexer__get_number_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_number_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 10);
	call_localret(STATIC(mercury__lexer__rev_char_list_to_int_3_0),
		mercury__lexer__get_number_4_0_i7,
		STATIC(mercury__lexer__get_number_4_0));
Define_label(mercury__lexer__get_number_4_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_number_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_number_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_number_4_0_i8);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_digit_1_0);
	call_localret(ENTRY(mercury__char__is_digit_1_0),
		mercury__lexer__get_number_4_0_i11,
		STATIC(mercury__lexer__get_number_4_0));
	}
Define_label(mercury__lexer__get_number_4_0_i11);
	update_prof_current_proc(LABEL(mercury__lexer__get_number_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_number_4_0_i10);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__lexer__get_number_4_0,
		STATIC(mercury__lexer__get_number_4_0));
Define_label(mercury__lexer__get_number_4_0_i10);
	r3 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	if (((Integer) r3 != ((Integer) 46)))
		GOTO_LABEL(mercury__lexer__get_number_4_0_i14);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__lexer__get_int_dot_4_0),
		STATIC(mercury__lexer__get_number_4_0));
Define_label(mercury__lexer__get_number_4_0_i14);
	if (((Integer) r3 != ((Integer) 69)))
		GOTO_LABEL(mercury__lexer__get_number_4_0_i21);
	r1 = (Integer) detstackvar(1);
	GOTO_LABEL(mercury__lexer__get_number_4_0_i20);
Define_label(mercury__lexer__get_number_4_0_i21);
	if (((Integer) r3 != ((Integer) 101)))
		GOTO_LABEL(mercury__lexer__get_number_4_0_i18);
	r1 = (Integer) detstackvar(1);
Define_label(mercury__lexer__get_number_4_0_i20);
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__lexer__get_float_exponent_4_0),
		STATIC(mercury__lexer__get_number_4_0));
Define_label(mercury__lexer__get_number_4_0_i18);
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_number_4_0_i24,
		STATIC(mercury__lexer__get_number_4_0));
	}
Define_label(mercury__lexer__get_number_4_0_i24);
	update_prof_current_proc(LABEL(mercury__lexer__get_number_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 10);
	call_localret(STATIC(mercury__lexer__rev_char_list_to_int_3_0),
		mercury__lexer__get_number_4_0_i7,
		STATIC(mercury__lexer__get_number_4_0));
Define_label(mercury__lexer__get_number_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_number_4_0_i29,
		STATIC(mercury__lexer__get_number_4_0));
	}
Define_label(mercury__lexer__get_number_4_0_i29);
	update_prof_current_proc(LABEL(mercury__lexer__get_number_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module27)
	init_entry(mercury__lexer__get_int_dot_4_0);
	init_label(mercury__lexer__get_int_dot_4_0_i2);
	init_label(mercury__lexer__get_int_dot_4_0_i6);
	init_label(mercury__lexer__get_int_dot_4_0_i7);
	init_label(mercury__lexer__get_int_dot_4_0_i8);
	init_label(mercury__lexer__get_int_dot_4_0_i5);
	init_label(mercury__lexer__get_int_dot_4_0_i12);
	init_label(mercury__lexer__get_int_dot_4_0_i11);
	init_label(mercury__lexer__get_int_dot_4_0_i15);
	init_label(mercury__lexer__get_int_dot_4_0_i16);
	init_label(mercury__lexer__get_int_dot_4_0_i9);
	init_label(mercury__lexer__get_int_dot_4_0_i19);
BEGIN_CODE

/* code for predicate 'lexer__get_int_dot'/4 in mode 0 */
Define_static(mercury__lexer__get_int_dot_4_0);
	incr_sp_push_msg(4, "lexer__get_int_dot");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_int_dot_4_0_i2,
		STATIC(mercury__lexer__get_int_dot_4_0));
	}
Define_label(mercury__lexer__get_int_dot_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_int_dot_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_int_dot_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_int_dot_4_0_i6,
		STATIC(mercury__lexer__get_int_dot_4_0));
	}
Define_label(mercury__lexer__get_int_dot_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_int_dot_4_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 46);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_int_dot_4_0_i7,
		STATIC(mercury__lexer__get_int_dot_4_0));
	}
Define_label(mercury__lexer__get_int_dot_4_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_int_dot_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 10);
	call_localret(STATIC(mercury__lexer__rev_char_list_to_int_3_0),
		mercury__lexer__get_int_dot_4_0_i8,
		STATIC(mercury__lexer__get_int_dot_4_0));
Define_label(mercury__lexer__get_int_dot_4_0_i8);
	update_prof_current_proc(LABEL(mercury__lexer__get_int_dot_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_int_dot_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_int_dot_4_0_i9);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_digit_1_0);
	call_localret(ENTRY(mercury__char__is_digit_1_0),
		mercury__lexer__get_int_dot_4_0_i12,
		STATIC(mercury__lexer__get_int_dot_4_0));
	}
Define_label(mercury__lexer__get_int_dot_4_0_i12);
	update_prof_current_proc(LABEL(mercury__lexer__get_int_dot_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_int_dot_4_0_i11);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = ((Integer) 46);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__lexer__get_float_decimals_4_0),
		STATIC(mercury__lexer__get_int_dot_4_0));
Define_label(mercury__lexer__get_int_dot_4_0_i11);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_int_dot_4_0_i15,
		STATIC(mercury__lexer__get_int_dot_4_0));
	}
Define_label(mercury__lexer__get_int_dot_4_0_i15);
	update_prof_current_proc(LABEL(mercury__lexer__get_int_dot_4_0));
	r2 = (Integer) r1;
	r1 = ((Integer) 46);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_int_dot_4_0_i16,
		STATIC(mercury__lexer__get_int_dot_4_0));
	}
Define_label(mercury__lexer__get_int_dot_4_0_i16);
	update_prof_current_proc(LABEL(mercury__lexer__get_int_dot_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = ((Integer) 10);
	call_localret(STATIC(mercury__lexer__rev_char_list_to_int_3_0),
		mercury__lexer__get_int_dot_4_0_i8,
		STATIC(mercury__lexer__get_int_dot_4_0));
Define_label(mercury__lexer__get_int_dot_4_0_i9);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_int_dot_4_0_i19,
		STATIC(mercury__lexer__get_int_dot_4_0));
	}
Define_label(mercury__lexer__get_int_dot_4_0_i19);
	update_prof_current_proc(LABEL(mercury__lexer__get_int_dot_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module28)
	init_entry(mercury__lexer__get_float_decimals_4_0);
	init_label(mercury__lexer__get_float_decimals_4_0_i2);
	init_label(mercury__lexer__get_float_decimals_4_0_i6);
	init_label(mercury__lexer__get_float_decimals_4_0_i7);
	init_label(mercury__lexer__get_float_decimals_4_0_i5);
	init_label(mercury__lexer__get_float_decimals_4_0_i11);
	init_label(mercury__lexer__get_float_decimals_4_0_i10);
	init_label(mercury__lexer__get_float_decimals_4_0_i17);
	init_label(mercury__lexer__get_float_decimals_4_0_i16);
	init_label(mercury__lexer__get_float_decimals_4_0_i14);
	init_label(mercury__lexer__get_float_decimals_4_0_i8);
	init_label(mercury__lexer__get_float_decimals_4_0_i24);
BEGIN_CODE

/* code for predicate 'lexer__get_float_decimals'/4 in mode 0 */
Define_static(mercury__lexer__get_float_decimals_4_0);
	incr_sp_push_msg(4, "lexer__get_float_decimals");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_float_decimals_4_0_i2,
		STATIC(mercury__lexer__get_float_decimals_4_0));
	}
Define_label(mercury__lexer__get_float_decimals_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_decimals_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_float_decimals_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_float_decimals_4_0_i6,
		STATIC(mercury__lexer__get_float_decimals_4_0));
	}
Define_label(mercury__lexer__get_float_decimals_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_decimals_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__lexer__rev_char_list_to_float_2_0),
		mercury__lexer__get_float_decimals_4_0_i7,
		STATIC(mercury__lexer__get_float_decimals_4_0));
Define_label(mercury__lexer__get_float_decimals_4_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_decimals_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_float_decimals_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_float_decimals_4_0_i8);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_digit_1_0);
	call_localret(ENTRY(mercury__char__is_digit_1_0),
		mercury__lexer__get_float_decimals_4_0_i11,
		STATIC(mercury__lexer__get_float_decimals_4_0));
	}
Define_label(mercury__lexer__get_float_decimals_4_0_i11);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_decimals_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_float_decimals_4_0_i10);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__lexer__get_float_decimals_4_0,
		STATIC(mercury__lexer__get_float_decimals_4_0));
Define_label(mercury__lexer__get_float_decimals_4_0_i10);
	r3 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	if (((Integer) r3 != ((Integer) 69)))
		GOTO_LABEL(mercury__lexer__get_float_decimals_4_0_i17);
	r1 = (Integer) detstackvar(1);
	GOTO_LABEL(mercury__lexer__get_float_decimals_4_0_i16);
Define_label(mercury__lexer__get_float_decimals_4_0_i17);
	if (((Integer) r3 != ((Integer) 101)))
		GOTO_LABEL(mercury__lexer__get_float_decimals_4_0_i14);
	r1 = (Integer) detstackvar(1);
Define_label(mercury__lexer__get_float_decimals_4_0_i16);
	r4 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) r4;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__lexer__get_float_exponent_4_0),
		STATIC(mercury__lexer__get_float_decimals_4_0));
Define_label(mercury__lexer__get_float_decimals_4_0_i14);
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_float_decimals_4_0_i6,
		STATIC(mercury__lexer__get_float_decimals_4_0));
	}
Define_label(mercury__lexer__get_float_decimals_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_float_decimals_4_0_i24,
		STATIC(mercury__lexer__get_float_decimals_4_0));
	}
Define_label(mercury__lexer__get_float_decimals_4_0_i24);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_decimals_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module29)
	init_entry(mercury__lexer__get_float_exponent_4_0);
	init_label(mercury__lexer__get_float_exponent_4_0_i2);
	init_label(mercury__lexer__get_float_exponent_4_0_i6);
	init_label(mercury__lexer__get_float_exponent_4_0_i7);
	init_label(mercury__lexer__get_float_exponent_4_0_i5);
	init_label(mercury__lexer__get_float_exponent_4_0_i12);
	init_label(mercury__lexer__get_float_exponent_4_0_i14);
	init_label(mercury__lexer__get_float_exponent_4_0_i18);
	init_label(mercury__lexer__get_float_exponent_4_0_i17);
	init_label(mercury__lexer__get_float_exponent_4_0_i22);
	init_label(mercury__lexer__get_float_exponent_4_0_i21);
	init_label(mercury__lexer__get_float_exponent_4_0_i19);
	init_label(mercury__lexer__get_float_exponent_4_0_i27);
	init_label(mercury__lexer__get_float_exponent_4_0_i9);
	init_label(mercury__lexer__get_float_exponent_4_0_i30);
	init_label(mercury__lexer__get_float_exponent_4_0_i29);
	init_label(mercury__lexer__get_float_exponent_4_0_i33);
	init_label(mercury__lexer__get_float_exponent_4_0_i8);
	init_label(mercury__lexer__get_float_exponent_4_0_i36);
BEGIN_CODE

/* code for predicate 'lexer__get_float_exponent'/4 in mode 0 */
Define_static(mercury__lexer__get_float_exponent_4_0);
	incr_sp_push_msg(4, "lexer__get_float_exponent");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_float_exponent_4_0_i2,
		STATIC(mercury__lexer__get_float_exponent_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_float_exponent_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_float_exponent_4_0_i6,
		STATIC(mercury__lexer__get_float_exponent_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__lexer__rev_char_list_to_float_2_0),
		mercury__lexer__get_float_exponent_4_0_i7,
		STATIC(mercury__lexer__get_float_exponent_4_0));
Define_label(mercury__lexer__get_float_exponent_4_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_float_exponent_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_float_exponent_4_0_i8);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 != ((Integer) 43)))
		GOTO_LABEL(mercury__lexer__get_float_exponent_4_0_i12);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	detstackvar(1) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_float_exponent_4_0_i14,
		STATIC(mercury__lexer__get_float_exponent_4_0));
	}
	}
Define_label(mercury__lexer__get_float_exponent_4_0_i12);
	if (((Integer) r3 != ((Integer) 45)))
		GOTO_LABEL(mercury__lexer__get_float_exponent_4_0_i9);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	detstackvar(1) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) r2;
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_float_exponent_4_0_i14,
		STATIC(mercury__lexer__get_float_exponent_4_0));
	}
	}
Define_label(mercury__lexer__get_float_exponent_4_0_i14);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_float_exponent_4_0_i17);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_float_exponent_4_0_i18,
		STATIC(mercury__lexer__get_float_exponent_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_4_0_i18);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_27);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_float_exponent_4_0_i17);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_float_exponent_4_0_i19);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_digit_1_0);
	call_localret(ENTRY(mercury__char__is_digit_1_0),
		mercury__lexer__get_float_exponent_4_0_i22,
		STATIC(mercury__lexer__get_float_exponent_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_4_0_i22);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_float_exponent_4_0_i21);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__lexer__get_float_exponent_3_4_0),
		STATIC(mercury__lexer__get_float_exponent_4_0));
Define_label(mercury__lexer__get_float_exponent_4_0_i21);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_float_exponent_4_0_i18,
		STATIC(mercury__lexer__get_float_exponent_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_4_0_i19);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_float_exponent_4_0_i27,
		STATIC(mercury__lexer__get_float_exponent_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_4_0_i27);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_float_exponent_4_0_i9);
	detstackvar(2) = (Integer) r3;
	detstackvar(3) = (Integer) r2;
	r1 = (Integer) r3;
	{
	Declare_entry(mercury__char__is_digit_1_0);
	call_localret(ENTRY(mercury__char__is_digit_1_0),
		mercury__lexer__get_float_exponent_4_0_i30,
		STATIC(mercury__lexer__get_float_exponent_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_4_0_i30);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_float_exponent_4_0_i29);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	tailcall(STATIC(mercury__lexer__get_float_exponent_3_4_0),
		STATIC(mercury__lexer__get_float_exponent_4_0));
Define_label(mercury__lexer__get_float_exponent_4_0_i29);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_float_exponent_4_0_i33,
		STATIC(mercury__lexer__get_float_exponent_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_4_0_i33);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_27);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_float_exponent_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_float_exponent_4_0_i36,
		STATIC(mercury__lexer__get_float_exponent_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_4_0_i36);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module30)
	init_entry(mercury__lexer__get_float_exponent_3_4_0);
	init_label(mercury__lexer__get_float_exponent_3_4_0_i2);
	init_label(mercury__lexer__get_float_exponent_3_4_0_i6);
	init_label(mercury__lexer__get_float_exponent_3_4_0_i7);
	init_label(mercury__lexer__get_float_exponent_3_4_0_i5);
	init_label(mercury__lexer__get_float_exponent_3_4_0_i11);
	init_label(mercury__lexer__get_float_exponent_3_4_0_i10);
	init_label(mercury__lexer__get_float_exponent_3_4_0_i8);
	init_label(mercury__lexer__get_float_exponent_3_4_0_i17);
BEGIN_CODE

/* code for predicate 'lexer__get_float_exponent_3'/4 in mode 0 */
Define_static(mercury__lexer__get_float_exponent_3_4_0);
	incr_sp_push_msg(4, "lexer__get_float_exponent_3");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__io__read_char_3_0);
	call_localret(ENTRY(mercury__io__read_char_3_0),
		mercury__lexer__get_float_exponent_3_4_0_i2,
		STATIC(mercury__lexer__get_float_exponent_3_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_3_4_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_3_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__lexer__get_float_exponent_3_4_0_i5);
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_float_exponent_3_4_0_i6,
		STATIC(mercury__lexer__get_float_exponent_3_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_3_4_0_i6);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_3_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury__lexer__rev_char_list_to_float_2_0),
		mercury__lexer__get_float_exponent_3_4_0_i7,
		STATIC(mercury__lexer__get_float_exponent_3_4_0));
Define_label(mercury__lexer__get_float_exponent_3_4_0_i7);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_3_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__lexer__get_float_exponent_3_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__lexer__get_float_exponent_3_4_0_i8);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_digit_1_0);
	call_localret(ENTRY(mercury__char__is_digit_1_0),
		mercury__lexer__get_float_exponent_3_4_0_i11,
		STATIC(mercury__lexer__get_float_exponent_3_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_3_4_0_i11);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_3_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__get_float_exponent_3_4_0_i10);
	tag_incr_hp(r1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__lexer__get_float_exponent_3_4_0,
		STATIC(mercury__lexer__get_float_exponent_3_4_0));
Define_label(mercury__lexer__get_float_exponent_3_4_0_i10);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__io__putback_char_3_0);
	call_localret(ENTRY(mercury__io__putback_char_3_0),
		mercury__lexer__get_float_exponent_3_4_0_i6,
		STATIC(mercury__lexer__get_float_exponent_3_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_3_4_0_i8);
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	{
	extern Word * mercury_data_io__base_type_info_io__state_0[];
	r1 = (Integer) mercury_data_io__base_type_info_io__state_0;
	}
	{
	Declare_entry(mercury__f_cut_2_0);
	call_localret(ENTRY(mercury__f_cut_2_0),
		mercury__lexer__get_float_exponent_3_4_0_i17,
		STATIC(mercury__lexer__get_float_exponent_3_4_0));
	}
Define_label(mercury__lexer__get_float_exponent_3_4_0_i17);
	update_prof_current_proc(LABEL(mercury__lexer__get_float_exponent_3_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 5);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module31)
	init_entry(mercury__lexer__rev_char_list_to_int_3_0);
	init_label(mercury__lexer__rev_char_list_to_int_3_0_i2);
	init_label(mercury__lexer__rev_char_list_to_int_3_0_i5);
	init_label(mercury__lexer__rev_char_list_to_int_3_0_i4);
BEGIN_CODE

/* code for predicate 'lexer__rev_char_list_to_int'/3 in mode 0 */
Define_static(mercury__lexer__rev_char_list_to_int_3_0);
	incr_sp_push_msg(2, "lexer__rev_char_list_to_int");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	call_localret(STATIC(mercury__lexer__rev_char_list_to_string_2_0),
		mercury__lexer__rev_char_list_to_int_3_0_i2,
		STATIC(mercury__lexer__rev_char_list_to_int_3_0));
Define_label(mercury__lexer__rev_char_list_to_int_3_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__rev_char_list_to_int_3_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__string__base_string_to_int_3_0);
	call_localret(ENTRY(mercury__string__base_string_to_int_3_0),
		mercury__lexer__rev_char_list_to_int_3_0_i5,
		STATIC(mercury__lexer__rev_char_list_to_int_3_0));
	}
Define_label(mercury__lexer__rev_char_list_to_int_3_0_i5);
	update_prof_current_proc(LABEL(mercury__lexer__rev_char_list_to_int_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__rev_char_list_to_int_3_0_i4);
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 0);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__lexer__rev_char_list_to_int_3_0_i4);
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_28);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module32)
	init_entry(mercury__lexer__rev_char_list_to_float_2_0);
	init_label(mercury__lexer__rev_char_list_to_float_2_0_i2);
	init_label(mercury__lexer__rev_char_list_to_float_2_0_i5);
	init_label(mercury__lexer__rev_char_list_to_float_2_0_i4);
BEGIN_CODE

/* code for predicate 'lexer__rev_char_list_to_float'/2 in mode 0 */
Define_static(mercury__lexer__rev_char_list_to_float_2_0);
	incr_sp_push_msg(1, "lexer__rev_char_list_to_float");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__lexer__rev_char_list_to_string_2_0),
		mercury__lexer__rev_char_list_to_float_2_0_i2,
		STATIC(mercury__lexer__rev_char_list_to_float_2_0));
Define_label(mercury__lexer__rev_char_list_to_float_2_0_i2);
	update_prof_current_proc(LABEL(mercury__lexer__rev_char_list_to_float_2_0));
	{
	Declare_entry(mercury__string__to_float_2_0);
	call_localret(ENTRY(mercury__string__to_float_2_0),
		mercury__lexer__rev_char_list_to_float_2_0_i5,
		STATIC(mercury__lexer__rev_char_list_to_float_2_0));
	}
Define_label(mercury__lexer__rev_char_list_to_float_2_0_i5);
	update_prof_current_proc(LABEL(mercury__lexer__rev_char_list_to_float_2_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__lexer__rev_char_list_to_float_2_0_i4);
	tag_incr_hp(r1, mktag(3), ((Integer) 2));
	field(mktag(3), (Integer) r1, ((Integer) 0)) = ((Integer) 1);
	field(mktag(3), (Integer) r1, ((Integer) 1)) = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__lexer__rev_char_list_to_float_2_0_i4);
	r1 = (Integer) mkword(mktag(3), (Integer) mercury_data_lexer__common_29);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module33)
	init_entry(mercury__lexer__rev_char_list_to_string_2_0);
BEGIN_CODE

/* code for predicate 'lexer__rev_char_list_to_string'/2 in mode 0 */
Define_static(mercury__lexer__rev_char_list_to_string_2_0);
	{
		Word	Chars;
		String	Str;
		Chars = (Integer) r1;
		
{
	Word list_ptr;
	Word size, len;
	Word str_ptr;
/*
** loop to calculate list length + sizeof(Word) in `size' using list in
** `list_ptr' and separately count the length of the string
*/
	size = sizeof(Word);
	len = 1;
	list_ptr = Chars;
	while (!list_is_empty(list_ptr)) {
		size++;
		len++;
		list_ptr = list_tail(list_ptr);
	}
/*
** allocate (length + 1) bytes of heap space for string
** i.e. (length + 1 + sizeof(Word) - 1) / sizeof(Word) words
*/
	incr_hp_atomic(str_ptr, size / sizeof(Word));
	Str = (char *) str_ptr;
/*
** set size to be the offset of the end of the string
** (ie the \0) and null terminate the string.
*/
	Str[--len] = '\0';
/*
** loop to copy the characters from the list_ptr to the string
** in reverse order.
*/
	list_ptr = Chars;
	while (!list_is_empty(list_ptr)) {
		Str[--len] = (char) list_head(list_ptr);
		list_ptr = list_tail(list_ptr);
	}
}
		r2 = (Word) Str;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module34)
	init_entry(mercury____Unify___lexer__token_0_0);
	init_label(mercury____Unify___lexer__token_0_0_i1060);
	init_label(mercury____Unify___lexer__token_0_0_i1059);
	init_label(mercury____Unify___lexer__token_0_0_i1058);
	init_label(mercury____Unify___lexer__token_0_0_i1057);
	init_label(mercury____Unify___lexer__token_0_0_i1056);
	init_label(mercury____Unify___lexer__token_0_0_i1055);
	init_label(mercury____Unify___lexer__token_0_0_i1054);
	init_label(mercury____Unify___lexer__token_0_0_i1053);
	init_label(mercury____Unify___lexer__token_0_0_i1052);
	init_label(mercury____Unify___lexer__token_0_0_i1051);
	init_label(mercury____Unify___lexer__token_0_0_i1050);
	init_label(mercury____Unify___lexer__token_0_0_i1036);
	init_label(mercury____Unify___lexer__token_0_0_i1037);
	init_label(mercury____Unify___lexer__token_0_0_i1038);
	init_label(mercury____Unify___lexer__token_0_0_i1039);
	init_label(mercury____Unify___lexer__token_0_0_i1040);
	init_label(mercury____Unify___lexer__token_0_0_i1041);
	init_label(mercury____Unify___lexer__token_0_0_i1042);
	init_label(mercury____Unify___lexer__token_0_0_i1043);
	init_label(mercury____Unify___lexer__token_0_0_i1044);
	init_label(mercury____Unify___lexer__token_0_0_i1045);
	init_label(mercury____Unify___lexer__token_0_0_i1046);
	init_label(mercury____Unify___lexer__token_0_0_i1049);
	init_label(mercury____Unify___lexer__token_0_0_i28);
	init_label(mercury____Unify___lexer__token_0_0_i30);
	init_label(mercury____Unify___lexer__token_0_0_i32);
	init_label(mercury____Unify___lexer__token_0_0_i34);
	init_label(mercury____Unify___lexer__token_0_0_i36);
	init_label(mercury____Unify___lexer__token_0_0_i38);
	init_label(mercury____Unify___lexer__token_0_0_i27);
	init_label(mercury____Unify___lexer__token_0_0_i42);
	init_label(mercury____Unify___lexer__token_0_0_i1);
	init_label(mercury____Unify___lexer__token_0_0_i1047);
	init_label(mercury____Unify___lexer__token_0_0_i1048);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___lexer__token_0_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1049);
	COMPUTED_GOTO(unmkbody((Integer) r1),
		LABEL(mercury____Unify___lexer__token_0_0_i1060) AND
		LABEL(mercury____Unify___lexer__token_0_0_i1059) AND
		LABEL(mercury____Unify___lexer__token_0_0_i1058) AND
		LABEL(mercury____Unify___lexer__token_0_0_i1057) AND
		LABEL(mercury____Unify___lexer__token_0_0_i1056) AND
		LABEL(mercury____Unify___lexer__token_0_0_i1055) AND
		LABEL(mercury____Unify___lexer__token_0_0_i1054) AND
		LABEL(mercury____Unify___lexer__token_0_0_i1053) AND
		LABEL(mercury____Unify___lexer__token_0_0_i1052) AND
		LABEL(mercury____Unify___lexer__token_0_0_i1051) AND
		LABEL(mercury____Unify___lexer__token_0_0_i1050));
Define_label(mercury____Unify___lexer__token_0_0_i1060);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1036);
Define_label(mercury____Unify___lexer__token_0_0_i1059);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1037);
Define_label(mercury____Unify___lexer__token_0_0_i1058);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1038);
Define_label(mercury____Unify___lexer__token_0_0_i1057);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1039);
Define_label(mercury____Unify___lexer__token_0_0_i1056);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1040);
Define_label(mercury____Unify___lexer__token_0_0_i1055);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1041);
Define_label(mercury____Unify___lexer__token_0_0_i1054);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1042);
Define_label(mercury____Unify___lexer__token_0_0_i1053);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1043);
Define_label(mercury____Unify___lexer__token_0_0_i1052);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1044);
Define_label(mercury____Unify___lexer__token_0_0_i1051);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1045);
Define_label(mercury____Unify___lexer__token_0_0_i1050);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1046);
Define_label(mercury____Unify___lexer__token_0_0_i1036);
	decr_sp_pop_msg(1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1047);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1037);
	decr_sp_pop_msg(1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 1)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1047);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1038);
	decr_sp_pop_msg(1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 2)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1047);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1039);
	decr_sp_pop_msg(1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 3)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1047);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1040);
	decr_sp_pop_msg(1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 4)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1047);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1041);
	decr_sp_pop_msg(1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 5)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1047);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1042);
	decr_sp_pop_msg(1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 6)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1047);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1043);
	decr_sp_pop_msg(1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 7)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1047);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1044);
	decr_sp_pop_msg(1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 8)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1047);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1045);
	decr_sp_pop_msg(1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 9)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1047);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1046);
	decr_sp_pop_msg(1);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 10)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1047);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1049);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	if (((Integer) r3 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i27);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury____Unify___lexer__token_0_0_i28) AND
		LABEL(mercury____Unify___lexer__token_0_0_i30) AND
		LABEL(mercury____Unify___lexer__token_0_0_i32) AND
		LABEL(mercury____Unify___lexer__token_0_0_i34) AND
		LABEL(mercury____Unify___lexer__token_0_0_i36) AND
		LABEL(mercury____Unify___lexer__token_0_0_i38));
Define_label(mercury____Unify___lexer__token_0_0_i28);
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	if (((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 1)) != (Integer) field(mktag(3), (Integer) r2, ((Integer) 1))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1048);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i30);
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	if (((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if ((word_to_float((Integer) field(mktag(3), (Integer) r1, ((Integer) 1))) != word_to_float((Integer) field(mktag(3), (Integer) r2, ((Integer) 1)))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1048);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i32);
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	if (((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)) != ((Integer) 2)))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if ((strcmp((char *)(Integer) field(mktag(3), (Integer) r1, ((Integer) 1)), (char *)(Integer) field(mktag(3), (Integer) r2, ((Integer) 1))) !=0))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1048);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i34);
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	if (((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)) != ((Integer) 3)))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (((Integer) field(mktag(3), (Integer) r1, ((Integer) 1)) != (Integer) field(mktag(3), (Integer) r2, ((Integer) 1))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1048);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i36);
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	if (((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)) != ((Integer) 4)))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if ((strcmp((char *)(Integer) field(mktag(3), (Integer) r1, ((Integer) 1)), (char *)(Integer) field(mktag(3), (Integer) r2, ((Integer) 1))) !=0))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1048);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i38);
	if ((tag((Integer) r2) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (((Integer) field(mktag(3), (Integer) r2, ((Integer) 0)) != ((Integer) 5)))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1048);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(3), (Integer) r2, ((Integer) 1));
	{
	Declare_entry(mercury____Unify___io__error_0_0);
	tailcall(ENTRY(mercury____Unify___io__error_0_0),
		ENTRY(mercury____Unify___lexer__token_0_0));
	}
Define_label(mercury____Unify___lexer__token_0_0_i27);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i42);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if ((strcmp((char *)(Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), (char *)(Integer) field(mktag(1), (Integer) r2, ((Integer) 0))) !=0))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1048);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i42);
	if ((tag((Integer) r2) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	if ((strcmp((char *)(Integer) field(mktag(2), (Integer) r1, ((Integer) 0)), (char *)(Integer) field(mktag(2), (Integer) r2, ((Integer) 0))) !=0))
		GOTO_LABEL(mercury____Unify___lexer__token_0_0_i1);
	r1 = TRUE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1047);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___lexer__token_0_0_i1048);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module35)
	init_entry(mercury____Index___lexer__token_0_0);
	init_label(mercury____Index___lexer__token_0_0_i5);
	init_label(mercury____Index___lexer__token_0_0_i6);
	init_label(mercury____Index___lexer__token_0_0_i7);
	init_label(mercury____Index___lexer__token_0_0_i8);
	init_label(mercury____Index___lexer__token_0_0_i9);
	init_label(mercury____Index___lexer__token_0_0_i10);
	init_label(mercury____Index___lexer__token_0_0_i11);
	init_label(mercury____Index___lexer__token_0_0_i12);
	init_label(mercury____Index___lexer__token_0_0_i13);
	init_label(mercury____Index___lexer__token_0_0_i14);
	init_label(mercury____Index___lexer__token_0_0_i15);
	init_label(mercury____Index___lexer__token_0_0_i4);
	init_label(mercury____Index___lexer__token_0_0_i17);
	init_label(mercury____Index___lexer__token_0_0_i18);
	init_label(mercury____Index___lexer__token_0_0_i19);
	init_label(mercury____Index___lexer__token_0_0_i20);
	init_label(mercury____Index___lexer__token_0_0_i21);
	init_label(mercury____Index___lexer__token_0_0_i22);
	init_label(mercury____Index___lexer__token_0_0_i16);
	init_label(mercury____Index___lexer__token_0_0_i23);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___lexer__token_0_0);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Index___lexer__token_0_0_i4);
	COMPUTED_GOTO(unmkbody((Integer) r1),
		LABEL(mercury____Index___lexer__token_0_0_i5) AND
		LABEL(mercury____Index___lexer__token_0_0_i6) AND
		LABEL(mercury____Index___lexer__token_0_0_i7) AND
		LABEL(mercury____Index___lexer__token_0_0_i8) AND
		LABEL(mercury____Index___lexer__token_0_0_i9) AND
		LABEL(mercury____Index___lexer__token_0_0_i10) AND
		LABEL(mercury____Index___lexer__token_0_0_i11) AND
		LABEL(mercury____Index___lexer__token_0_0_i12) AND
		LABEL(mercury____Index___lexer__token_0_0_i13) AND
		LABEL(mercury____Index___lexer__token_0_0_i14) AND
		LABEL(mercury____Index___lexer__token_0_0_i15));
Define_label(mercury____Index___lexer__token_0_0_i5);
	r1 = ((Integer) 5);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i6);
	r1 = ((Integer) 6);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i7);
	r1 = ((Integer) 7);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i8);
	r1 = ((Integer) 8);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i9);
	r1 = ((Integer) 9);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i10);
	r1 = ((Integer) 10);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i11);
	r1 = ((Integer) 11);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i12);
	r1 = ((Integer) 12);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i13);
	r1 = ((Integer) 13);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i14);
	r1 = ((Integer) 14);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i15);
	r1 = ((Integer) 18);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i4);
	if (((Integer) r2 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Index___lexer__token_0_0_i16);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury____Index___lexer__token_0_0_i17) AND
		LABEL(mercury____Index___lexer__token_0_0_i18) AND
		LABEL(mercury____Index___lexer__token_0_0_i19) AND
		LABEL(mercury____Index___lexer__token_0_0_i20) AND
		LABEL(mercury____Index___lexer__token_0_0_i21) AND
		LABEL(mercury____Index___lexer__token_0_0_i22));
Define_label(mercury____Index___lexer__token_0_0_i17);
	r1 = ((Integer) 2);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i18);
	r1 = ((Integer) 3);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i19);
	r1 = ((Integer) 4);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i20);
	r1 = ((Integer) 15);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i21);
	r1 = ((Integer) 16);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i22);
	r1 = ((Integer) 17);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i16);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Index___lexer__token_0_0_i23);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury____Index___lexer__token_0_0_i23);
	r1 = ((Integer) 1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module36)
	init_entry(mercury____Compare___lexer__token_0_0);
	init_label(mercury____Compare___lexer__token_0_0_i2);
	init_label(mercury____Compare___lexer__token_0_0_i3);
	init_label(mercury____Compare___lexer__token_0_0_i4);
	init_label(mercury____Compare___lexer__token_0_0_i6);
	init_label(mercury____Compare___lexer__token_0_0_i13);
	init_label(mercury____Compare___lexer__token_0_0_i15);
	init_label(mercury____Compare___lexer__token_0_0_i17);
	init_label(mercury____Compare___lexer__token_0_0_i19);
	init_label(mercury____Compare___lexer__token_0_0_i21);
	init_label(mercury____Compare___lexer__token_0_0_i23);
	init_label(mercury____Compare___lexer__token_0_0_i25);
	init_label(mercury____Compare___lexer__token_0_0_i27);
	init_label(mercury____Compare___lexer__token_0_0_i29);
	init_label(mercury____Compare___lexer__token_0_0_i31);
	init_label(mercury____Compare___lexer__token_0_0_i33);
	init_label(mercury____Compare___lexer__token_0_0_i12);
	init_label(mercury____Compare___lexer__token_0_0_i36);
	init_label(mercury____Compare___lexer__token_0_0_i39);
	init_label(mercury____Compare___lexer__token_0_0_i42);
	init_label(mercury____Compare___lexer__token_0_0_i45);
	init_label(mercury____Compare___lexer__token_0_0_i48);
	init_label(mercury____Compare___lexer__token_0_0_i51);
	init_label(mercury____Compare___lexer__token_0_0_i35);
	init_label(mercury____Compare___lexer__token_0_0_i54);
	init_label(mercury____Compare___lexer__token_0_0_i9);
	init_label(mercury____Compare___lexer__token_0_0_i1000);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___lexer__token_0_0);
	incr_sp_push_msg(4, "__Compare__");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
		call_localret(STATIC(mercury____Index___lexer__token_0_0),
		mercury____Compare___lexer__token_0_0_i2,
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
Define_label(mercury____Compare___lexer__token_0_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___lexer__token_0_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury____Index___lexer__token_0_0),
		mercury____Compare___lexer__token_0_0_i3,
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
Define_label(mercury____Compare___lexer__token_0_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___lexer__token_0_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i6);
	r1 = (Integer) detstackvar(1);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i12);
	COMPUTED_GOTO(unmkbody((Integer) r1),
		LABEL(mercury____Compare___lexer__token_0_0_i13) AND
		LABEL(mercury____Compare___lexer__token_0_0_i15) AND
		LABEL(mercury____Compare___lexer__token_0_0_i17) AND
		LABEL(mercury____Compare___lexer__token_0_0_i19) AND
		LABEL(mercury____Compare___lexer__token_0_0_i21) AND
		LABEL(mercury____Compare___lexer__token_0_0_i23) AND
		LABEL(mercury____Compare___lexer__token_0_0_i25) AND
		LABEL(mercury____Compare___lexer__token_0_0_i27) AND
		LABEL(mercury____Compare___lexer__token_0_0_i29) AND
		LABEL(mercury____Compare___lexer__token_0_0_i31) AND
		LABEL(mercury____Compare___lexer__token_0_0_i33));
Define_label(mercury____Compare___lexer__token_0_0_i13);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i15);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 1)))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i17);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 2)))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i19);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 3)))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i21);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 4)))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i23);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 5)))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i25);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 6)))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i27);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 7)))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i29);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 8)))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i31);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 9)))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i33);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 10)))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___lexer__token_0_0_i12);
	if (((Integer) r2 != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i35);
	COMPUTED_GOTO((Integer) field(mktag(3), (Integer) r1, ((Integer) 0)),
		LABEL(mercury____Compare___lexer__token_0_0_i36) AND
		LABEL(mercury____Compare___lexer__token_0_0_i39) AND
		LABEL(mercury____Compare___lexer__token_0_0_i42) AND
		LABEL(mercury____Compare___lexer__token_0_0_i45) AND
		LABEL(mercury____Compare___lexer__token_0_0_i48) AND
		LABEL(mercury____Compare___lexer__token_0_0_i51));
Define_label(mercury____Compare___lexer__token_0_0_i36);
	r3 = (Integer) detstackvar(2);
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i1000);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
Define_label(mercury____Compare___lexer__token_0_0_i39);
	r3 = (Integer) detstackvar(2);
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 1)))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i1000);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__builtin_compare_float_3_0);
	tailcall(ENTRY(mercury__builtin_compare_float_3_0),
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
Define_label(mercury____Compare___lexer__token_0_0_i42);
	r3 = (Integer) detstackvar(2);
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 2)))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i1000);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	tailcall(ENTRY(mercury__builtin_compare_string_3_0),
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
Define_label(mercury____Compare___lexer__token_0_0_i45);
	r3 = (Integer) detstackvar(2);
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 3)))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i1000);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
Define_label(mercury____Compare___lexer__token_0_0_i48);
	r3 = (Integer) detstackvar(2);
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 4)))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i1000);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	tailcall(ENTRY(mercury__builtin_compare_string_3_0),
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
Define_label(mercury____Compare___lexer__token_0_0_i51);
	r3 = (Integer) detstackvar(2);
	if ((tag((Integer) r3) != mktag(((Integer) 3))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if (((Integer) field(mktag(3), (Integer) r3, ((Integer) 0)) != ((Integer) 5)))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i1000);
	r1 = (Integer) field(mktag(3), (Integer) r1, ((Integer) 1));
	r2 = (Integer) field(mktag(3), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury____Compare___io__error_0_0);
	tailcall(ENTRY(mercury____Compare___io__error_0_0),
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
Define_label(mercury____Compare___lexer__token_0_0_i35);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i54);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i1000);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	tailcall(ENTRY(mercury__builtin_compare_string_3_0),
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
Define_label(mercury____Compare___lexer__token_0_0_i54);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if ((tag((Integer) r3) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Compare___lexer__token_0_0_i1000);
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(2), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	tailcall(ENTRY(mercury__builtin_compare_string_3_0),
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
Define_label(mercury____Compare___lexer__token_0_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
Define_label(mercury____Compare___lexer__token_0_0_i1000);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___lexer__token_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__lexer_module37)
	init_entry(mercury____Unify___lexer__token_context_0_0);
	init_label(mercury____Unify___lexer__token_context_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___lexer__token_context_0_0);
	if (((Integer) r1 != (Integer) r2))
		GOTO_LABEL(mercury____Unify___lexer__token_context_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___lexer__token_context_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__lexer_module38)
	init_entry(mercury____Index___lexer__token_context_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___lexer__token_context_0_0);
	{
	Declare_entry(mercury__builtin_index_int_2_0);
	tailcall(ENTRY(mercury__builtin_index_int_2_0),
		ENTRY(mercury____Index___lexer__token_context_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__lexer_module39)
	init_entry(mercury____Compare___lexer__token_context_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___lexer__token_context_0_0);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___lexer__token_context_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__lexer_module40)
	init_entry(mercury____Unify___lexer__token_list_0_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___lexer__token_list_0_0);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_30);
	{
	Declare_entry(mercury____Unify___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Unify___mercury_builtin__list_1_0),
		ENTRY(mercury____Unify___lexer__token_list_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__lexer_module41)
	init_entry(mercury____Index___lexer__token_list_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___lexer__token_list_0_0);
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_30);
	{
	Declare_entry(mercury____Index___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Index___mercury_builtin__list_1_0),
		ENTRY(mercury____Index___lexer__token_list_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__lexer_module42)
	init_entry(mercury____Compare___lexer__token_list_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___lexer__token_list_0_0);
	r3 = (Integer) r2;
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_lexer__common_30);
	{
	Declare_entry(mercury____Compare___mercury_builtin__list_1_0);
	tailcall(ENTRY(mercury____Compare___mercury_builtin__list_1_0),
		ENTRY(mercury____Compare___lexer__token_list_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__lexer_bunch_0(void)
{
	mercury__lexer_module0();
	mercury__lexer_module1();
	mercury__lexer_module2();
	mercury__lexer_module3();
	mercury__lexer_module4();
	mercury__lexer_module5();
	mercury__lexer_module6();
	mercury__lexer_module7();
	mercury__lexer_module8();
	mercury__lexer_module9();
	mercury__lexer_module10();
	mercury__lexer_module11();
	mercury__lexer_module12();
	mercury__lexer_module13();
	mercury__lexer_module14();
	mercury__lexer_module15();
	mercury__lexer_module16();
	mercury__lexer_module17();
	mercury__lexer_module18();
	mercury__lexer_module19();
	mercury__lexer_module20();
	mercury__lexer_module21();
	mercury__lexer_module22();
	mercury__lexer_module23();
	mercury__lexer_module24();
	mercury__lexer_module25();
	mercury__lexer_module26();
	mercury__lexer_module27();
	mercury__lexer_module28();
	mercury__lexer_module29();
	mercury__lexer_module30();
	mercury__lexer_module31();
	mercury__lexer_module32();
	mercury__lexer_module33();
	mercury__lexer_module34();
	mercury__lexer_module35();
	mercury__lexer_module36();
	mercury__lexer_module37();
	mercury__lexer_module38();
	mercury__lexer_module39();
	mercury__lexer_module40();
}

static void mercury__lexer_bunch_1(void)
{
	mercury__lexer_module41();
	mercury__lexer_module42();
}

#endif

void mercury__lexer__init(void); /* suppress gcc warning */
void mercury__lexer__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__lexer_bunch_0();
	mercury__lexer_bunch_1();
#endif
}
