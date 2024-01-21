%{

#include <string.h>

#include "mknumeric.h"
#include "y.tab.h"

void
yyerror(char *);

int lineno = 1;

%}

white	[ \t]

%x	String

%%

	unsigned char string_buf[512];
	unsigned char *string_buf_ptr;

^#[^\n]*\n			lineno++;

\n				lineno++;

\\\n				lineno++;

^{white}*codeset{white}+	return CODESET;

^{white}*decimal_point{white}+	return DECIMAL_POINT;

^{white}*thousands_sep{white}+	return THOUSANDS_SEP;

^{white}*grouping{white}+	return GROUPING;

[a-zA-Z][a-zA-Z0-9_-]*[a-zA-Z0-9]	{
					yylval.str = strdup(yytext);
					return CODENAME;
				}

\"				{
					BEGIN(String);
					string_buf_ptr = string_buf;
				}

<String>\"			{
					/* saw closing quote - all done */

					BEGIN(INITIAL);
					*string_buf_ptr = '\0';
					yylval.str = strdup(string_buf);
					return STRING;
				}

<String>\n			{
					yyerror("Unterminated string");
					exit(1);
				}

<String>\\[0-7]{1,3}		{
					int result;

					(void)sscanf(yytext + 1, "%o", &result);

					if (result > 0xff) {
						yyerror("Invalid octal code");
						exit(1);
					}

					*string_buf_ptr++ = result;
				}

<String>\\[0-9]+		{
					yyerror("Invalid backslash escape");
					exit(1);
				}

<String>\\[Xx][0-9a-fA-F][0-9a-fA-F]		{
					int result;

					(void)sscanf(yytext + 2, "%x", &result);
					*string_buf_ptr++ = result;
				}

<String>\\[Xx]..		{
					yyerror("Invalid backslash escape");
					exit(1);
				}

<String>\\n			*string_buf_ptr++ = '\n';
<String>\\t			*string_buf_ptr++ = '\t';
<String>\\r			*string_buf_ptr++ = '\r';
<String>\\b			*string_buf_ptr++ = '\b';
<String>\\f			*string_buf_ptr++ = '\f';

<String>\\(.|\n)		*string_buf_ptr++ = yytext[1];

<String>[^\\\n\"]+		{
					unsigned char *text_ptr = yytext;

					while(*text_ptr)
						*string_buf_ptr++ = *text_ptr++;
				}

.

%%

void
yyerror(char *txt)
{
  (void) fprintf(stderr, "Line %d: %s\n", lineno, txt);
}
