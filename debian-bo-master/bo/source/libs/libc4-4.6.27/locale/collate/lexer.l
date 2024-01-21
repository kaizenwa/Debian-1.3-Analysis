%{

#include <string.h>

#include "mkcollate.h"
#include "y.tab.h"

void
yyerror(unsigned char *);

int lineno = 1;

unsigned char string_buf[512];
unsigned char *string_buf_ptr;

%}

white	[ \t]

%x	StrinG

%%

^#[^\n]*\n			lineno++;

\n				lineno++;

\\\n				lineno++;

\\[0-3][0-7][0-7]		{
					int result;

					(void) sscanf(yytext + 1, "%o", &result);
					yylval.usval = result;
					return SYMBOL;
				}

0[0-3][0-7][0-7]		{
					int result;

					(void) sscanf(yytext, "%o", &result);
					yylval.usval = result;
					return SYMBOL;
				}

\\[Xx][0-9a-fA-F][0-9a-fA-F]	{
					int result;

					(void) sscanf(yytext + 2, "%x", &result);
					yylval.usval = result;
					return SYMBOL;
				}

0[Xx][0-9a-fA-F][0-9a-fA-F]	{
					int result;

					(void) sscanf(yytext + 2, "%x", &result);
					yylval.usval = result;
					return SYMBOL;
				}

^{white}*codeset{white}+	return CODESET;

^{white}*order{white}+is{white}+		return ORDER;

^{white}*substitute{white}+	return SUBSTITUTE;

\;{white}*\.\.\.{white}*\;	return RANGE;

{white}+with{white}+		return WITH;

\;				return ';';

\(				return '(';

\)				return ')';

\{				return '{';

\}				return '}';

\"				{
					BEGIN(StrinG);
					string_buf_ptr = string_buf;
				}

<StrinG>\"			{
					/* saw closing quote - all done */

					BEGIN(INITIAL);
					*string_buf_ptr = '\0';
					yylval.str = strdup(string_buf);
					return STRING;
				}

<StrinG>\n			{
					(void) fprintf(stderr, "Unterminated string at line %d\n", lineno);
					exit(1);
				}

<StrinG>\\[0-7]{1,3}		{
					int result;

					(void)sscanf(yytext + 1, "%o", &result);

					if (result > 0xff) {
						yyerror("Invalid octal code");
						exit(1);
					}
					*string_buf_ptr++ = result;
				}

<StrinG>\\[0-9]+		{
					yyerror("Invalid backslash escape");
					exit(1);
				}

<StrinG>\\[Xx][0-9a-fA-F][0-9a-fA-F]		{
					int result;

					(void)sscanf(yytext + 2, "%x", &result);
					*string_buf_ptr++ = result;
				}

<StrinG>\\[Xx]..		{
					yyerror("Invalid backslash escape");
					exit(1);
				}

<StrinG>\\n			*string_buf_ptr++ = '\n';
<StrinG>\\t			*string_buf_ptr++ = '\t';
<StrinG>\\r			*string_buf_ptr++ = '\r';
<StrinG>\\b			*string_buf_ptr++ = '\b';
<StrinG>\\f			*string_buf_ptr++ = '\f';

<StrinG>\\(.|\n)		*string_buf_ptr++ = yytext[1];

<StrinG>[^\\\n\"]+		{
					unsigned char *text_ptr = yytext;

					while(*text_ptr)
						*string_buf_ptr++ = *text_ptr++;
				}

{white}+

.				{
					yylval.usval = (unsigned char) yytext[0];
					return SYMBOL;
				}

%%

void
yyerror(unsigned char *txt)
{
  (void) fprintf(stderr, "Line %d: %s\n", lineno, txt);
}
