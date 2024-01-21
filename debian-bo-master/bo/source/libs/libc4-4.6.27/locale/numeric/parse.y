%{

#include <string.h>
#include <stdlib.h>
#include <locale.h>
#include <localeinfo.h>

#include "mknumeric.h"

extern unsigned char *codename;

%}

%union {
	int number;
	unsigned char *str;
};

%token			CODESET

%token	<str>		CODENAME
%token	<str>		STRING

%token		DECIMAL_POINT
%token		GROUPING
%token		THOUSANDS_SEP

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

desc		: DECIMAL_POINT STRING {
				if(strlen($2) == 0) {
					yyerror("zero length decimal_point");
					YYERROR;
				}
				ninfo.decimal_point = $2;
			}
		| THOUSANDS_SEP STRING {
				ninfo.thousands_sep = $2;
			}
		| GROUPING STRING {
				ninfo.grouping = $2;
			}
		;
