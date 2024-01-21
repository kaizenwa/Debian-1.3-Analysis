%{

#include <string.h>
#include <stdlib.h>

#include "mkcollate.h"

extern unsigned char *codename;

%}

%union {
	unsigned short usval;
	unsigned char *str;
	mksub_t *sub;
	mkorder_t *ord;
};

%token		CODESET
%token		ORDER
%token		RANGE
%token	<str>	STRING
%token		SUBSTITUTE
%token	<usval>	SYMBOL
%token		WITH

%type	<str>	lit

%type	<sub>	subst

%type	<ord>	item items

%%

statements	: codeset order substs
		;

codeset		: CODESET lit {
				codename = $2;
			}
		;

order		: ORDER items {
				order = $2;
			}
		;

substs		: /* */
		| substs subst {
				nsubs++;
				if(last_sub == NULL) {
					last_sub = $2;
					substitutions = $2;
				} else {
					last_sub->next = $2;
					last_sub = $2;
				}
			}
		;

items		: item {
				$$ = $1;
			}
		| items ';' item {
				{
					mkorder_t *wk = $1;

					while(wk->next != NULL)
						wk = wk->next;
					wk->next = $3;
					$$ = $1;
				}
			}
		;

subst		: SUBSTITUTE STRING WITH STRING {
				$$ = (mksub_t *) malloc(sizeof(mksub_t));
				$$->next = NULL;
				$$->repl = $2;
				$$->with = $4;
			}
		;

item		: lit {
				$$ = (mkorder_t *) malloc(sizeof(mkorder_t));
				$$->next = NULL;
				$$->node_type = range;
				$$->low = $1;
				$$->top = $1;
			}
		| lit RANGE lit {
				$$ = (mkorder_t *) malloc(sizeof(mkorder_t));
				$$->next = NULL;
				$$->node_type = range;
				$$->low = $1;
				$$->top = $3;
			}
		| '(' items ')' {
				$$ = (mkorder_t *) malloc(sizeof(mkorder_t));
				$$->next = NULL;
				$$->node_type = secondary;
				$$->extra = $2;
			}
		| '{' items '}' {
				$$ = (mkorder_t *) malloc(sizeof(mkorder_t));
				$$->next = NULL;
				$$->node_type = primary_only;
				$$->extra = $2;
			}
		;

lit		: SYMBOL {
				$$ = (unsigned char *)malloc(128);
				*($$) = $1;
				*($$ + 1) = '\0';
			}
		| lit SYMBOL {
				{
					unsigned char *sbp;

					sbp = $1 + strlen($1);
					*sbp++ = $2;
					*sbp = '\0';
					$$ = $1;
				}
			}
		;
