%{

#include <string.h>

#include "mkctype.h"
#include "y.tab.h"

int lineno = 1;

%}

white	[ \t]

xcode	0[Xx][0-9a-fA-F][0-9a-fA-F]

ocode	0[0-3][0-7][0-7]

%%

^#[^\n]*\n			lineno++;

\n				lineno++;

\\\n				lineno++;

{ocode}				{
					int result;

					(void) sscanf(yytext, "%o", &result);
					yylval.ucode = result;
					return CODE;
				}

{xcode}				{
					int result;

					(void)sscanf(yytext + 2, "%x", &result);
					yylval.ucode = result;
					return CODE;
				}

^{white}*codeset{white}+	{
					return CODESET;
				}

\<				return '<';

\>				return '>';

\-				return '-';

^{white}*isblank{white}+	return ISBLANK;

^{white}*iscntrl{white}+	return ISCNTRL;

^{white}*isdigit{white}+	return ISDIGIT;

^{white}*islower{white}+	return ISLOWER;

^{white}*isnograph{white}+	return ISNOGRAPH;

^{white}*ispunct{white}+	return ISPUNCT;

^{white}*isspace{white}+	return ISSPACE;

^{white}*isupper{white}+	return ISUPPER;

^{white}*isxdigit{white}+	return ISXDIGIT;

^{white}*ul{white}+		return UL;

[a-zA-Z][a-zA-Z0-9_-]*[a-zA-Z0-9]	{
					yylval.str = strdup(yytext);
					return CODENAME;
				}

.				/* Ignore anything else */

%%

void
yyerror(char *txt)
{
  (void) fprintf(stderr, "Line %d: %s\n", lineno, txt);
}
