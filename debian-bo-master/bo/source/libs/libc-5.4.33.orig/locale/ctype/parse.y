%{

#include <string.h>
#include <stdlib.h>

#include "mkctype.h"

extern unsigned char *codename;

void
add_chain(mk_pair_t **ptr, mk_pair_t *new_ent) {
	mk_pair_t *node = new_ent;

	if(*ptr == NULL) {
		*ptr = new_ent;
		return;
	}

	while(node->next != NULL)
		node = node->next;

	node->next = *ptr;
	*ptr = new_ent;
}

%}

%union {
	unsigned char ucode;
	unsigned char *str;
	mk_pair_t *cpair;
};

%token		CODESET
%token	<str>	CODENAME

%token		ISBLANK
%token		ISCNTRL
%token		ISDIGIT
%token		ISLOWER
%token		ISNOGRAPH
%token		ISPUNCT
%token		ISSPACE
%token		ISUPPER
%token		ISXDIGIT

%token	<ucode>		CODE

%token		UL

%type	<cpair>	code codes pair pairs

%%

statements	: codeset descriptions
		;

descriptions	: classes
		| descriptions classes
		| conv
		| descriptions conv
		;

codeset		: CODESET CODENAME {
				codename = $2;
			}
		;

classes		: ISUPPER codes {
				add_chain(&uppers, $2);
			}
		| ISLOWER codes {
				add_chain(&lowers, $2);
			}
		| ISDIGIT codes {
				add_chain(&digits, $2);
			}
		| ISSPACE codes {
				add_chain(&spaces, $2);
			}
		| ISPUNCT codes {
				add_chain(&puncts, $2);
			}
		| ISCNTRL codes {
				add_chain(&cntrls, $2);
			}
		| ISBLANK codes {
				add_chain(&blanks, $2);
			}
		| ISXDIGIT codes {
				add_chain(&hexs, $2);
			}
		| ISNOGRAPH codes {
				add_chain(&nographs, $2);
			}
		;

conv		: UL pairs {
				add_chain(&ul_pairs, $2);
			}
		;

codes		: code {
				$$ = $1;
			}
		| codes code {
				$2->next = $1;
				$$ = $2;
			}
		;

code		: CODE {
				$$ = (mk_pair_t *) malloc(sizeof(mk_pair_t));
				($$)->next = NULL;
				($$)->lower = $1;
				($$)->upper = $1;
			}
		| CODE '-' CODE {
				if($1 > $3) {
					char buffer[128];

					(void) sprintf(buffer, "invalid code range (0x%02x-0x%02x)", $1, $3);
					yyerror(buffer);
					YYERROR;
				}
				$$ = (mk_pair_t *) malloc(sizeof(mk_pair_t));
				($$)->next = NULL;
				($$)->lower = $1;
				($$)->upper = $3;
			}
		;

pairs		: pair {
				$$ = $1;
			}
		| pairs pair {
				$2->next = $1;
				$$ = $2;
			}
		;

pair		: '<' CODE CODE '>' {
				if($2 == $3) {
					char buffer[128];

					(void) sprintf(buffer, "same code (0x%02x) for upper and lower letter", $2);
					yyerror(buffer);
					YYERROR;
				}
				$$ = (mk_pair_t *) malloc(sizeof(mk_pair_t));
				$$->next = NULL;
				$$->lower = $3;
				$$->upper = $2;
			}
		;
