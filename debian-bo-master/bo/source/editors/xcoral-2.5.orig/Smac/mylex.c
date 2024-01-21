/* ########################################################################

				mylex.c

   File: mylex.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/mylex.c
   Description: 
   Created: Tue Feb 21 12:58:37 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:58:37 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


/* A fast lexer, c5.l may be compiled by flex or lex but the result is slower */

#include "rename.h"

#include <stdio.h>
#include "y.tab.h"

char yytext[8196];

FILE *yyin;
extern int numlig;

static int previous_char;
char previous_tchar[256];
static int pt_file = 0;

void smac_read_new_file()
{
  int c = ie_getc();
  
  previous_tchar[pt_file++] = previous_char;
  previous_char = (c == EOF) ? 0 : c;
}

#ifdef XCORAL
#include "smacXcoral.h"
extern char err_msg[];
extern int yyinflag;

extern FCT(int, yyerror, (char *)	);

int ie_getc()
{
  if (yyinflag) {
    int c = fgetc(yyin);
    
    return (c == EOF) ? 0 : c;
  }
  
  if (i_minibuffer)
    return (*i_minibuffer) ? *i_minibuffer++ : 0;
  
  {
    extern FCT(int, ie_the_char,(char *, int)	);
    
    return (i_curseur == i_curseur_sup)
      ? 0
      : ie_the_char(i_buffer_courant, i_curseur++);
  }
}

#else

int ie_getc()
{
  int c = fgetc(yyin);
  
  if (c == EOF) {
    if (yyin == stdin)
      exit(0);
    return 0;
  }
  return c;
}

#endif

int key_err(k)
     char * k;
{
  char msg[64];
  
  sprintf(msg, "sorry %s refused", k);
  yyerror(msg);
  
  return(SYNTAX_ERROR);
}

#define error() previous_char = previous_tchar[--pt_file]

int eof_error()
{
  error();
  yyerror("end of file");
  return(SYNTAX_ERROR);
}

static lex_error(c)
    int c;
{
  char msg[2];
  
  msg[0] = c;
  msg[1] = 0;
  error();
  yyerror(msg);
  return(SYNTAX_ERROR);
}

static unsigned char global_type[256];
static unsigned char separator[256];
static unsigned char is_ident[256];

struct Keyword {
  char * s;
  int isa_keyword;
  struct Keyword * k;
};

static struct Keyword * global_ident[256];

int yylex()
{
  for (;;) {
    int c;
    
    if ((yytext[0] = previous_char) == 0) {
      /* EOF */
      previous_char = previous_tchar[--pt_file];
      
      return 0;
    }
    
    switch (global_type[previous_char]) {
      
    case 0 :
      /* '/' */
      switch (previous_char = ie_getc()) {
      case '*' :
	c = ie_getc();
	for (;;) {
	  while (c != '*') {
	    if (! c) {
	      previous_char = 0;
	      break;
	    }
	    if (c == '\n')
	      numlig += 1;
	    c = ie_getc();
	  }
	  if ((c = ie_getc()) == '/') {
	    previous_char = ie_getc();
	    break;
	  }
	}
	break;
      case '=' :
	previous_char = ie_getc();
	yytext[1] = '=';
	yytext[2] = 0;
	return(DIVassign);
      default:
	yytext[1] = 0;
	return '/';
      }
      break;
      
    case 1 :
      /* single char operator */
      c = previous_char;
      previous_char = ie_getc();
      yytext[1] = 0;
      return c;
      
    case 2 :
      /* . */
      if ((ie_getc() == '.') && (ie_getc() == '.')) {
	/*return(ELLIPSIS);*/
	error();
	yyerror("sorry ... not yet implemented");
	return(SYNTAX_ERROR);
      }
      return lex_error('.');
      
    case 3 :
      /* & | + - */
      c = previous_char;
      if ((previous_char = ie_getc()) == c) {
	yytext[1] = c;
	yytext[2] = 0;
	previous_char = ie_getc();
	switch (c) {
	case '&' : return ANDAND;
	case '|' : return OROR;
	case '+' : return ICR;
	default : return DECR;
	}
      }
      if (previous_char == '=') {
	yytext[1] = '=';
	yytext[2] = 0;
	previous_char = ie_getc();
	switch (c) {
	case '&' : return ANDassign;
	case '|' : return ORassign;
	case '+' : return PLUSassign;
	default : return  MINUSassign;
	}
      }
      if ((c == '-') && (previous_char == '>')) {
	error();
	return(key_err("->"));
      }
      yytext[1] = 0;
      return c;
    
    case 4 :
      /* * ! % ^ = */
      c = previous_char;
      if ((previous_char = ie_getc()) == '=') {
	yytext[1] = '=';
	yytext[2] = 0;
	previous_char = ie_getc();
	switch (c) {
	case '=' : return(EQ);
	case '!' : return(NE);
	case '%' : return(MODassign);
	case '*' : return(MULTassign);
	default : return(ERassign);
	}
      }
      yytext[1] = 0;
      return c;
    
    case 5 : 
      /* < > */
      c = previous_char;
      if ((previous_char = ie_getc()) == '=') {
	yytext[1] = '=';
	yytext[2] = 0;
	previous_char = ie_getc();
	return (c == '<') ? LE : GE;
      }
      if (previous_char != c) {
	yytext[1] = 0;
	return c;
      }
      if ((previous_char = ie_getc()) == '=') {
	yytext[1] = c;
	yytext[2] = '=';
	yytext[3] = 0;
	previous_char = ie_getc();
	return (c == '<') ? LSassign : RSassign;
      }
      yytext[1] = 0;
      return (c == '<') ? LS : RS;

    case 6 :
      /* 0 1 2 3 4 5 6 7 8 9 */
      {
	char * p = yytext;
	
	yytext[0] = previous_char;
	
	while (((c = ie_getc()) >= '0') && (c <= '9'))
	  *++p = c;
	if (! separator[c])
	  return lex_error(c);
	previous_char = c;
	*++p = 0;
	return INTEGERconstant;
      }

    case 7 :
       /* ' */
       {
	 char * p;
	 
	 yytext[0] = '\'';
	
	 if ((c = ie_getc()) == '\\') {
	   p = &yytext[3];
	   yytext[1] = '\\';
	   yytext[2] = ie_getc();
	 }
	 else
	   p = &yytext[1];
	  
	 do {
	   if (! c)
	     return eof_error();
	   *p++ = c;
	 } while ((c = ie_getc()) != '\'');
	 *p = c;
	 *(p + 1) = 0;
       }
       previous_char = ie_getc();
       return CHARACTERconstant;
       
    case 8 :
      /* " */
      {
	char * p = &yytext[1];
	
	yytext[0] = '"';
	
	while ((c = ie_getc()) != '"') {
	  if (! c)
	    return eof_error();
	  else if ((*p++ = c) == '\\')
	    *p++ = ie_getc();
	}
	
	*p = c;
	*(p + 1) = 0;
      }
      previous_char = ie_getc();
      return STRINGliteral;

    case 9 :
      /* \n */
      numlig += 1;
      
    case 10 :
      /* <space>\t\v\f */
      previous_char = ie_getc();
      break;
      
    case 11 :
      /* a-zA-Z_ */
      {
	char * p = &yytext[1];
	struct Keyword * key = global_ident[previous_char];
	
	yytext[0] = previous_char;
	
	while (is_ident[c = ie_getc()]) {
	  *p++ = c;
	  if (key) {
	    char * ps = key->s;
	    struct Keyword * pk = key->k;
	    
	    for (;;) {
	      if (*ps == c) {
		key = pk;
		break;
	      }
	      if  (! *ps) {
		key = 0;
		break;
	      }
	      ps += 1;
	      pk += 1;
	    }
	  }
	}
	if (! separator[c])
	  return lex_error(c);
	previous_char = c;
	*p = 0;
	
	if (! key)
	  return IDENTIFIER;
	switch (key->isa_keyword) {
	case 0 :
	  return IDENTIFIER;
	case -1 :
	  error();
	  return key_err(yytext);
	default :
	  return key->isa_keyword;
	}
      }
      
    default :
      return lex_error(previous_char);
    }
  }
}

static struct Keyword auto__ident = { "", -1, 0 };
static struct Keyword aut_o__ident = { "o", 0, &auto__ident };
static struct Keyword au_to__ident = { "t", 0, &aut_o__ident };
static struct Keyword a_uto__ident = { "u", 0, &au_to__ident };

static struct Keyword break__ident = { "", BREAK, 0 };
static struct Keyword brea_k__ident = { "k", 0, &break__ident };
static struct Keyword bre_ak__ident = { "a", 0, &brea_k__ident };
static struct Keyword br_eak__ident = { "e", 0, &bre_ak__ident };
static struct Keyword b_reak__ident = { "r", 0, &br_eak__ident };


static struct Keyword case__ident = { "", CASE, 0 };
static struct Keyword cas_e__ident = { "e", 0, &case__ident };
#define ca_se__ident { "s", 0, &cas_e__ident }

static struct Keyword char__ident = { "", CHAR, 0 };
static struct Keyword cha_r__ident = { "r", 0, &char__ident };
#define ch_ar__ident { "a", 0, &cha_r__ident }

static struct Keyword const__ident = { "", -1, 0 };
#define cons_t__ident { "t", 0, &const__ident }

static struct Keyword continue__ident = { "", CONTINUE, 0 };
static struct Keyword continu_e__ident = { "e", 0, &continue__ident };
static struct Keyword contin_ue__ident = { "u", 0, &continu_e__ident };
static struct Keyword conti_nue__ident = { "n", 0, &contin_ue__ident };
#define cont_inue__ident { "i", 0, &conti_nue__ident }

static struct Keyword _cont_inue__ident__cons_t__ident[] = {cont_inue__ident, cons_t__ident};
static struct Keyword con_tinue__con_st__ident = { "ts", 0, _cont_inue__ident__cons_t__ident };
#define co_ntinue__co_nst__ident { "n", 0, &con_tinue__con_st__ident }

static struct Keyword _ca_se__ident__ch_ar__ident__co_ntinue__co_nst__ident[] = {ca_se__ident, ch_ar__ident, co_ntinue__co_nst__ident};
static struct Keyword c_ase__c_har__c_ontinue__c_onst__ident = { "aho", 0, _ca_se__ident__ch_ar__ident__co_ntinue__co_nst__ident };

static struct Keyword double__ident = { "", -1, 0 };
static struct Keyword doubl_e__ident = { "e", 0, &double__ident };
static struct Keyword doub_le__ident = { "l", 0, &doubl_e__ident };
static struct Keyword dou_ble__ident = { "b", 0, &doub_le__ident };

#define do___do_uble__ident { "u", DO, &dou_ble__ident }

static struct Keyword default__ident = { "", DEFAULT, 0 };
static struct Keyword defaul_t__ident = { "t", 0, &default__ident };
static struct Keyword defau_lt__ident = { "l", 0, &defaul_t__ident };
static struct Keyword defa_ult__ident = { "u", 0, &defau_lt__ident };
static struct Keyword def_ault__ident = { "a", 0, &defa_ult__ident };
#define de_fault__ident { "f", 0, &def_ault__ident }

static struct Keyword _de_fault__ident__do___do_uble__ident[] = { de_fault__ident, do___do_uble__ident};
static struct Keyword d_efault__d_o__d_ouble__ident = { "eo", 0, _de_fault__ident__do___do_uble__ident };

static struct Keyword else__ident = { "", ELSE, 0 };
static struct Keyword els_e__ident = { "e", 0, &else__ident };
#define el_se__ident { "s", 0, &els_e__ident }

static struct Keyword enum__ident = { "", -1, 0 };
static struct Keyword enu_m__ident = { "m", 0, &enum__ident };
#define en_um__ident { "u", 0, &enu_m__ident }

static struct Keyword extern__ident = { "", -1, 0 };
static struct Keyword exter_n__ident = { "n", 0, &extern__ident };
static struct Keyword exte_rn__ident = { "r", 0, &exter_n__ident };
static struct Keyword ext_ern__ident = { "e", 0, &exte_rn__ident };
#define ex_tern__ident { "t", 0, &ext_ern__ident }

static struct Keyword _el_se__ident__en_um__ident__ex_tern__ident[] = {el_se__ident, en_um__ident, ex_tern__ident};
static struct Keyword e_lse__e_num__e_xtern__ident = { "lnx", 0, _el_se__ident__en_um__ident__ex_tern__ident };

static struct Keyword for__ident = { "", FOR, 0 };
#define fo_r__ident { "r", 0, &for__ident }

static struct Keyword float__ident = { "", -1, 0 };
static struct Keyword floa_t__ident = { "t", 0, &float__ident };
static struct Keyword flo_at__ident = { "a", 0, &floa_t__ident };
#define fl_oat__ident { "o", 0, &flo_at__ident }

static struct Keyword _fl_oat__ident__fo_r__ident[] = { fl_oat__ident, fo_r__ident };
static struct Keyword f_loat__f_or__ident = { "lo", 0, _fl_oat__ident__fo_r__ident };

static struct Keyword goto__ident = { "", -1, 0 };
static struct Keyword got_o__ident = { "o", 0, &goto__ident };
static struct Keyword go_to__ident = { "t", 0, &got_o__ident };
static struct Keyword g_oto__ident = { "o", 0, &go_to__ident };

static struct Keyword int__ident = { "", INT, 0 };
#define in_t__ident { "t", 0, &int__ident }

#define if__ident { "", IF, 0 }

static struct Keyword _if__ident__in_t__ident[] = { if__ident, in_t__ident };
static struct Keyword i_f__i_nt__ident = { "fn", 0, _if__ident__in_t__ident };

static struct Keyword long__ident = { "", -1, 0 };
static struct Keyword lon_g__ident = { "g", 0, &long__ident };
static struct Keyword lo_ng__ident = { "n", 0, &lon_g__ident };
static struct Keyword l_ong__ident = { "o", 0, &lo_ng__ident };

static struct Keyword return__ident = { "", RETURN, 0 };
static struct Keyword retur_n__ident = { "n", 0, &return__ident };
static struct Keyword retu_rn__ident = { "r", 0, &retur_n__ident };
#define ret_urn__ident { "u", 0, &retu_rn__ident }

static struct Keyword register__ident = { "", -1, 0 };
static struct Keyword registe_r__ident = { "r", 0, &register__ident };
static struct Keyword regist_er__ident = { "e", 0, &registe_r__ident };
static struct Keyword regis_ter__ident = { "t", 0, &regist_er__ident };
static struct Keyword regi_ster__ident = { "s", 0, &regis_ter__ident };
#define reg_ister__ident { "i", 0, &regi_ster__ident }

static struct Keyword _reg_ister__ident__ret_urn__ident[] = { reg_ister__ident, ret_urn__ident };
static struct Keyword re_gister__re_turn__ident = { "gt", 0, _reg_ister__ident__ret_urn__ident };
static struct Keyword r_egister__r_eturn__ident = { "e", 0, &re_gister__re_turn__ident };

static struct Keyword switch__ident = { "", SWITCH, 0 };
static struct Keyword switc_h__ident = { "h", 0, &switch__ident };
static struct Keyword swit_ch__ident = { "c", 0, &switc_h__ident };
static struct Keyword swi_tch__ident = { "t", 0, &swit_ch__ident };
#define sw_itch__ident { "i", 0, &swi_tch__ident }

static struct Keyword struct__ident = { "", -1, 0 };
static struct Keyword struc_t__ident = { "t", 0, &struct__ident };
static struct Keyword stru_ct__ident = { "c", 0, &struc_t__ident };
#define str_uct__ident { "u", 0, &stru_ct__ident }

static struct Keyword static__ident = { "", -1, 0 };
static struct Keyword stati_c__ident = { "c", 0, &static__ident };
static struct Keyword stat_ic__ident = { "i", 0, &stati_c__ident };
#define sta_tic__ident { "t", 0, &stat_ic__ident }

static struct Keyword _sta_tic__ident__str_uct__ident[] = { sta_tic__ident, str_uct__ident };
#define st_atic__st_ruct__ident { "ar", 0, _sta_tic__ident__str_uct__ident }

static struct Keyword sizeof__ident = { "", SIZEOF, 0 };
static struct Keyword sizeo_f__ident = { "f", 0, &sizeof__ident };
static struct Keyword size_of__ident = { "o", 0, &sizeo_f__ident };
#define siz_eof__ident { "e", 0, &size_of__ident }

static struct Keyword signed__ident = { "", -1, 0 };
static struct Keyword signe_d__ident = { "d", 0, &signed__ident };
static struct Keyword sign_ed__ident = { "e", 0, &signe_d__ident };
#define sig_ned__ident { "n", 0, &sign_ed__ident }

static struct Keyword _sig_ned__ident__siz_eof__ident[] = { sig_ned__ident, siz_eof__ident };
#define si_gned__si_zeof__ident { "gz", 0, _sig_ned__ident__siz_eof__ident }

static struct Keyword short__ident = { "", -1, 0 };
static struct Keyword shor_t__ident = { "t", 0, &short__ident };
static struct Keyword sho_rt__ident = { "r", 0, &shor_t__ident };
#define sh_ort__ident { "o", 0, &sho_rt__ident }

static struct Keyword _sh_ort__ident__si_gned__si_zeof__ident__st_atic__st_ruct__ident__sw_itch__ident[] = { sh_ort__ident, si_gned__si_zeof__ident, st_atic__st_ruct__ident, sw_itch__ident };
static struct Keyword s_hort__s_igned__s_izeof__s_tatic__s_truct__s_witch__ident = { "hitw", 0, _sh_ort__ident__si_gned__si_zeof__ident__st_atic__st_ruct__ident__sw_itch__ident };

static struct Keyword typedef__ident = { "", -1, 0 };
static struct Keyword typede_f__ident = { "f", 0, &typedef__ident };
static struct Keyword typed_ef__ident = { "e", 0, &typede_f__ident };
static struct Keyword type_def__ident = { "d", 0, &typed_ef__ident };
static struct Keyword typ_edef__ident = { "e", 0, &type_def__ident };
static struct Keyword ty_pedef__ident = { "p", 0, &typ_edef__ident };
static struct Keyword t_ypedef__ident = { "y", 0, &ty_pedef__ident };

static struct Keyword unsigned__ident = { "", -1, 0 };
static struct Keyword unsigne_d__ident = { "d", 0, &unsigned__ident };
static struct Keyword unsign_ed__ident = { "e", 0, &unsigne_d__ident };
static struct Keyword unsig_ned__ident = { "n", 0, &unsign_ed__ident };
static struct Keyword unsi_gned__ident = { "g", 0, &unsig_ned__ident };
#define uns_igned__ident { "i", 0, &unsi_gned__ident }

static struct Keyword union__ident = { "", -1, 0 };
static struct Keyword unio_n__ident = { "n", 0, &union__ident };
#define uni_on__ident { "o", 0, &unio_n__ident }

static struct Keyword _uni_on__ident__uns_igned__ident[] = { uni_on__ident, uns_igned__ident };
static struct Keyword un_ion__un_signed__ident = { "is", 0, _uni_on__ident__uns_igned__ident };
static struct Keyword u_nion__u_nsigned__ident = { "n", 0, &un_ion__un_signed__ident };

static struct Keyword volatile__ident = { "", -1, 0 };
static struct Keyword volatil_e__ident = { "e", 0, &volatile__ident };
static struct Keyword volati_le__ident = { "l", 0, &volatil_e__ident };
static struct Keyword volat_ile__ident = { "i", 0, &volati_le__ident };
static struct Keyword vola_tile__ident = { "t", 0, &volat_ile__ident };
#define vol_atile__ident { "a", 0, &vola_tile__ident }

static struct Keyword void__ident = { "", VOID, 0 };
#define voi_d__ident { "d", 0, &void__ident }

static struct Keyword _voi_d__ident__vol_atile__ident[] = { voi_d__ident, vol_atile__ident };
static struct Keyword vo_id__vo_latile__ident = { "il", 0, _voi_d__ident__vol_atile__ident };
static struct Keyword v_oid__v_olatile__ident = { "o", 0, &vo_id__vo_latile__ident };

static struct Keyword while__ident = { "", WHILE, 0 };
static struct Keyword whil_e__ident = { "e", 0, &while__ident };
static struct Keyword whi_le__ident = { "l", 0, &whil_e__ident };
static struct Keyword wh_ile__ident = { "i", 0, &whi_le__ident };
static struct Keyword w_hile__ident = { "h", 0, &wh_ile__ident };

void mylex_init()
{
  int i;
  
  for (i = 0; i != 256; i += 1) {
    global_type[i] = 255;
    global_ident[i] = 0;
    is_ident[i] = 0;
    separator[i] = 0;
  }
  
  global_type['/'] = 0;
  global_type['('] = 1;
  global_type[')'] = 1;
  global_type[','] = 1;
  global_type['{'] = 1;
  global_type['}'] = 1;
  global_type['['] = 1;
  global_type[']'] = 1;
  global_type['?'] = 1;
  global_type[':'] = 1;
  global_type[';'] = 1;
  global_type['~'] = 1;
  global_type['.'] = 2;
  global_type['&'] = 3;
  global_type['|'] = 3;
  global_type['+'] = 3;
  global_type['-'] = 3;
  global_type['*'] = 4;
  global_type['!'] = 4;
  global_type['%'] = 4;
  global_type['^'] = 4;
  global_type['='] = 4;
  global_type['>'] = 5;
  global_type['<'] = 5;
  for (i = '0'; i <= '9'; i += 1) {
    global_type[i] = 6;
    is_ident[i] = 1;
  }
  global_type['\''] = 7;
  global_type['"'] = 8;
  global_type['\n'] = 9;
  global_type[' '] = 10;
  global_type['\t'] = 10;
  global_type['\v'] = 10;
  global_type['\f'] = 10;
  global_type['_'] = is_ident['_'] = 11;
  for (i = 'a'; i <= 'z'; i += 1)
    is_ident[i] = is_ident[i + 'A' - 'a'] =
      global_type[i] = global_type[i + 'A' - 'a'] = 11;
  
  separator[0] = 1;
  separator['/'] = 1;
  separator['('] = 1;
  separator[')'] = 1;
  separator[','] = 1;
  separator['{'] = 1;
  separator['}'] = 1;
  separator['['] = 1;
  separator[']'] = 1;
  separator['?'] = 1;
  separator[':'] = 1;
  separator[';'] = 1;
  separator['~'] = 1;
  separator['.'] = 1;
  separator['&'] = 1;
  separator['|'] = 1;
  separator['+'] = 1;
  separator['-'] = 1;
  separator['*'] = 1;
  separator['!'] = 1;
  separator['%'] = 1;
  separator['^'] = 1;
  separator['='] = 1;
  separator['>'] = 1;
  separator['<'] = 1;
  separator['\''] = 1;
  separator['"'] = 1;
  separator['\n'] = 1;
  separator[' '] = 11;
  separator['\t'] = 11;
  separator['\v'] = 11;
  separator['\f'] = 11;
  
  global_ident['a'] = &a_uto__ident;
  global_ident['b'] = &b_reak__ident;
  global_ident['c'] = &c_ase__c_har__c_ontinue__c_onst__ident;
  global_ident['d'] = &d_efault__d_o__d_ouble__ident;
  global_ident['e'] = &e_lse__e_num__e_xtern__ident;
  global_ident['f'] = &f_loat__f_or__ident;
  global_ident['g'] = &g_oto__ident;
  global_ident['i'] = &i_f__i_nt__ident;
  global_ident['l'] = &l_ong__ident;
  global_ident['r'] = &r_egister__r_eturn__ident;
  global_ident['s'] = &s_hort__s_igned__s_izeof__s_tatic__s_truct__s_witch__ident;
  global_ident['t'] = &t_ypedef__ident;
  global_ident['u'] = &u_nion__u_nsigned__ident;
  global_ident['v'] = &v_oid__v_olatile__ident;
  global_ident['w'] = &w_hile__ident;
  
  previous_char = ' ';
}

