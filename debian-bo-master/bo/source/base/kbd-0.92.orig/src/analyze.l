%{
#include <stdlib.h>
#include <linux/kd.h>

int ksymtocode(char *s);

int	line_nr=1;
int	yylval;
int	rvalct;
struct kbsentry kbs_buf;
char *p, *pmax;

#undef yywrap
extern int yywrap(void);
extern int yyerror(char *s);
extern void stringovfl(void);

%}

%s RVALUE
%x STR
Eol			\n
Blank			[ \t]
Decimal			[1-9][0-9]*
Octal			0[0-7]*
Hex			0[xX][0-9a-fA-F]+
Unicode			U\+([0-9a-fA-F]){4}
Literal			[a-zA-Z][a-zA-Z_0-9]*
Octa			([0-7]){1,3}
Charset			[cC][hH][aA][rR][sS][eE][tT]
Keymaps			[kK][eE][yY][mM][aA][pP][sS]
Keycode			[kK][eE][yY][cC][oO][dD][eE]
String			[sS][tT][rR][iI][nN][gG]
Compose                 [cC][oO][mM][pP][oO][sS][eE]
To                      [tT][oO]
Equals			=
Plain			[pP][lL][aA][iI][nN]
Shift			[sS][hH][iI][fF][tT]
Control			[cC][oO][nN][tT][rR][oO][lL]
Alt			[aA][lL][tT]
AltGr			[aA][lL][tT][gG][rR]
ShiftL			[sS][hH][iI][fF][tT][lL]
ShiftR			[sS][hH][iI][fF][tT][rR]
CtrlL			[cC][tT][rR][lL][lL]
CtrlR			[cC][tT][rR][lL][rR]
Comment			#|!
Continuation		\\\n
%%
{Continuation}		{line_nr++;}
{Eol}			{line_nr++;BEGIN(0);return(EOL);}
{Blank}+		; /* do nothing */
{Comment}.*/{Eol}	; /* do nothing */
{Equals}		{BEGIN(RVALUE);rvalct=0;return(EQUALS);}
\-			{return(DASH);}
\,			{return(COMMA);}
\+			{return(PLUS);}
{Unicode}		{yylval=strtol(yytext+1,NULL,16);return(UNUMBER);}
{Decimal}|{Octal}|{Hex}	{yylval=strtol(yytext,NULL,0);return(NUMBER);}
<RVALUE>{Literal}	{return((yylval=ksymtocode(yytext))==-1?ERROR:LITERAL);}
{Charset}		{return(CHARSET);}
{Keymaps}		{return(KEYMAPS);}
{Keycode}		{return(KEYCODE);}
{Compose}               {return(COMPOSE);}
{To}                    {return(TO);}
{String}		{BEGIN(RVALUE);return(STRING);}
{Plain}			{return(PLAIN);}
{Shift}			{return(SHIFT);}
{Control}		{return(CONTROL);}
{Alt}			{return(ALT);}
{AltGr}			{return(ALTGR);}
{ShiftL}		{return(SHIFTL);}
{ShiftR}		{return(SHIFTR);}
{CtrlL}			{return(CTRLL);}
{CtrlR}			{return(CTRLR);}
'\\{Octa}'              {yylval = strtol(yytext+2,NULL,8); return(CCHAR);}
'\\.'                   {yylval = yytext[2]; return(CCHAR);}
'.'                     {yylval = yytext[1]; return(CCHAR);}
\"			{p=kbs_buf.kb_string;
				pmax=p+sizeof(kbs_buf.kb_string)-1;
				BEGIN(STR);}
<STR>\\{Octa}		{if(p>=pmax)stringovfl();*p++=strtol(yytext+1,NULL,8);}
<STR>\\\"               {if(p>=pmax)stringovfl();*p++='"';}
<STR>\\\\               {if(p>=pmax)stringovfl();*p++='\\';}
<STR>\\n		{if(p>=pmax)stringovfl();*p++='\n';}
<STR>[^"\\]*		{char *ptmp=p;p+=strlen(yytext);
				if(p>pmax)stringovfl();strcpy(ptmp,yytext);}
<STR>\"			{*p='\0';BEGIN(0);return(STRLITERAL);}
.			{return(ERROR); /* report any unknown characters */}
%%
#include "ksyms.h"
#include <linux/keyboard.h>

void
stringovfl(void)
{
	fprintf(stderr, "loadkeys: string too long\n");
	exit(1);
}

int
ksymtocode(char *s)
{
	int i;
	int j;
	static warn = 0;

	for (i = 0; i < syms_size; i++)
		for (j = 0; j < syms[i].size; j++)
			if (!strcmp(s,syms[i].table[j]))
				return K(i, j);

	for (i = 0; i < syn_size; i++)
		if (!strcmp(s, synonyms[i].synonym))
			return ksymtocode(synonyms[i].official_name);

	for (i = 0; i < 256 - 160; i++)
		if (!strcmp(s, latin2_syms[i])) {
			if(!warn++)
				fprintf(stderr, "assuming iso-8859-2\n");
			return K(KT_LATIN, 160 + i);
		}

	for (i = 0; i < 256 - 160; i++)
		if (!strcmp(s, latin3_syms[i])) {
			if(!warn++)
				fprintf(stderr, "assuming iso-8859-3\n");
			return K(KT_LATIN, 160 + i);
		}

	for (i = 0; i < 256 - 160; i++)
		if (!strcmp(s, latin4_syms[i])) {
			if(!warn++)
				fprintf(stderr, "assuming iso-8859-4\n");
			return K(KT_LATIN, 160 + i);
		}

	fprintf(stderr, "unknown keysym '%s'\n", s);

	return -1;
}
