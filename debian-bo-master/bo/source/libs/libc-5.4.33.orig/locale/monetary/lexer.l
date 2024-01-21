%{

#include <string.h>

#include "mkmonetary.h"
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

^{white}*codeset{white}+		return CODESET;

^{white}*currency_symbol{white}+	return CURRENCY_SYMBOL;

^{white}*frac_digits{white}+		return FRAC_DIGITS;

^{white}*int_curr_symbol{white}+	return INT_CURR_SYMBOL;

^{white}*int_frac_digits{white}+	return INT_FRAC_DIGITS;

^{white}*mon_decimal_point{white}+	return MON_DECIMAL_POINT;

^{white}*mon_grouping{white}+		return MON_GROUPING;

^{white}*mon_thousands_sep{white}+	return MON_THOUSANDS_SEP;

^{white}*n_cs_precedes{white}+		return N_CS_PRECEDES;

^{white}*n_sep_by_space{white}+		return N_SEP_BY_SPACE;

^{white}*n_sign_posn{white}+		return N_SIGN_POSN;

^{white}*negative_sign{white}+		return NEGATIVE_SIGN;

^{white}*p_cs_precedes{white}+		return P_CS_PRECEDES;

^{white}*p_sep_by_space{white}+		return P_SEP_BY_SPACE;

^{white}*p_sign_posn{white}+		return P_SIGN_POSN;

^{white}*positive_sign{white}+		return POSITIVE_SIGN;

[0-9]+				{
					yylval.number = atoi(yytext);
					return NUMBER;
				}

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

<String>\\[Xx]..			{
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
