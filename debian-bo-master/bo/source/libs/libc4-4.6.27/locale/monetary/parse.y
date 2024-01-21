%{

#include <string.h>
#include <stdlib.h>
#include <locale.h>
#include <localeinfo.h>

#include "mkmonetary.h"

extern unsigned char *codename;

%}

%union {
	int number;
	unsigned char *str;
};

%token			CODESET

%token	<str>		CODENAME
%token	<str>		STRING

%token	<number>	NUMBER

%token		CURRENCY_SYMBOL
%token		FRAC_DIGITS
%token		INT_CURR_SYMBOL
%token		INT_FRAC_DIGITS
%token		MON_DECIMAL_POINT
%token		MON_GROUPING
%token		MON_THOUSANDS_SEP
%token		NEGATIVE_SIGN
%token		N_CS_PRECEDES
%token		N_SEP_BY_SPACE
%token		N_SIGN_POSN
%token		POSITIVE_SIGN
%token		P_CS_PRECEDES
%token		P_SEP_BY_SPACE
%token		P_SIGN_POSN

%%

statements	: codeset descriptions
		;

descriptions	: desc
		| descriptions desc
		;

codeset		: CODESET CODENAME {
				codename = $2;
			}
		;

desc		: INT_CURR_SYMBOL STRING {
				if(strlen($2) != 4) {
					yyerror("length of int_curr_symbol != 4");
					YYERROR;
				}
				minfo.int_curr_symbol = $2;
			}
		| CURRENCY_SYMBOL STRING {
				minfo.currency_symbol = $2;
			}
		| MON_DECIMAL_POINT STRING {
				minfo.mon_decimal_point = $2;
			}
		| MON_THOUSANDS_SEP STRING {
				minfo.mon_thousands_sep = $2;
			}
		| MON_GROUPING STRING {
				minfo.mon_grouping = $2;
			}
		| POSITIVE_SIGN STRING {
				minfo.positive_sign = $2;
			}
		| NEGATIVE_SIGN STRING {
				minfo.negative_sign = $2;
			}
		| INT_FRAC_DIGITS NUMBER {
				if($2 < 0 || $2 > CHAR_MAX) {
					yyerror("int_frac_digits out of bounds");
					YYERROR;
				}
				minfo.int_frac_digits = $2;
			}
		| FRAC_DIGITS NUMBER {
				if($2 < 0 || $2 > CHAR_MAX) {
					yyerror("frac_digits out of bounds");
					YYERROR;
				}
				minfo.frac_digits = $2;
			}
		| P_CS_PRECEDES NUMBER {
				if($2 < 0 || $2 > 1) {
					yyerror("invalid p_cs_precedes (0 or 1 only)");
					YYERROR;
				}
				minfo.p_cs_precedes = $2;
			}
		| P_SEP_BY_SPACE NUMBER {
				if($2 < 0 || $2 > 1) {
					yyerror("invalid p_sep_by_space (0 or 1 only)");
					YYERROR;
				}
				minfo.p_sep_by_space = $2;
			}
		| N_CS_PRECEDES NUMBER {
				if($2 < 0 || $2 > 1) {
					yyerror("invalid n_cs_precedes (0 or 1 only)");
					YYERROR;
				}
				minfo.n_cs_precedes = $2;
			}
		| N_SEP_BY_SPACE NUMBER {
				if($2 < 0 || $2 > 1) {
					yyerror("invalid n_sep_by_space (0 or 1 only)");
					YYERROR;
				}
				minfo.n_sep_by_space = $2;
			}
		| P_SIGN_POSN NUMBER {
				if($2 < 0 || $2 > 4) {
					yyerror("invalid p_sign_posn (0..4 only)");
					YYERROR;
				}
				minfo.p_sign_posn = $2;
			}
		| N_SIGN_POSN NUMBER {
				if($2 < 0 || $2 > 4) {
					yyerror("invalid n_sign_posn (0..4 only)");
					YYERROR;
				}
				minfo.n_sign_posn = $2;
			}
		;
