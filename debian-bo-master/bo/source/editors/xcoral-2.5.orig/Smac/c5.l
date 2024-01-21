
/*
** Copyright 1993 by Bruno Pages
**
** Permission to use, copy, and distribute for non-commercial purposes,
** is hereby granted without fee, providing that the above copyright
** notice appear in all copies and that both the copyright notice and this
** permission notice appear in supporting documentation.
** The software may be modified for your own purposes, but modified versions
** may not be distributed.
** This software is provided "as is" without any expressed or implied warranty.
**
**
*/

%{

#include <stdio.h>
#include "y.tab.h"

extern int numlig;

#ifdef flex
#undef yywrap
static int yywrap();
#else
/* lex */
#define yywrap() 1
#endif

#ifdef flex
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
      { \
	int c = ie_getc(); \
	result = (c == EOF) ? YY_NULL : (buf[0] = c, 1); \
      }

extern FILE *yyin;
#else
/* lex */
#undef input
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):ie_getc())==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#endif

#ifdef XCORAL
#include "smacXcoral.h"
extern char err_msg[];
extern int yyinflag;

extern FCT(int, yyerror, (char *)	);

int ie_getc()
{
  if (yyinflag)
    return fgetc(yyin);
  else if (i_minibuffer)
    return (*i_minibuffer) ? *i_minibuffer++ : EOF;
  else if (i_curseur != i_curseur_sup) {
    extern FCT(int, ie_the_char,(char *, int)	);
    
    return ie_the_char(i_buffer_courant, i_curseur++);
  }
  else
    return EOF;
}

#else

int ie_getc()
{
  int c = fgetc(yyin);
  
  if ((c == EOF) && (yyin == stdin))
    exit(0);
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

%}

%%
"/*"			{ for(;;) {
  			   char c;
			   
			   while (((c = input()) != '*') && c)
			      if (c == '\n') numlig += 1;
			   if (((c = input()) == '/') || (! c))
			     break;
			   unput(c);
			 }
			}

"auto"			{ return key_err("auto"); }
"break"			{ return(BREAK); }
"case"			{ return(CASE); }
"char"			{ return(CHAR); }
"continue"		{ return(CONTINUE); }
"const"			{ return key_err("const"); }
"default"		{ return(DEFAULT); }
"do"			{ return(DO); }
"double"		{ return key_err("double"); }
"else"			{ return(ELSE); }
"enum"			{ return key_err("enum"); }
"extern"		{ return key_err("extern"); }
"float"			{ return key_err("float"); }
"for"			{ return(FOR); }
"goto"			{ return key_err("goto"); }
"if"			{ return(IF); }
"int"			{ return(INT); }
"long"			{ return key_err("long"); }
"register"		{ return key_err("register"); }
"return"		{ return(RETURN); }
"short"			{ return key_err("short"); }
"signed"		{ return key_err("signed"); }
"sizeof"		{ return (SIZEOF); }
"static"		{ return key_err("static"); }
"struct"		{ return key_err("struct"); }
"switch"		{ return(SWITCH); }
"typedef"		{ return key_err("typedef"); }
"union"			{ return key_err("union"); }
"unsigned"		{ return key_err("unsigned"); }
"void"			{ return(VOID); }
"volatile"		{ return key_err("volatile"); }
"while"			{ return(WHILE); }

[a-zA-Z_][0-9a-zA-Z_]*	{ return(IDENTIFIER); }

[1-9][0-9]*		{ return(INTEGERconstant); }
"0"			{ return(INTEGERconstant); }

'(\\.|[^\\'])'		{ return(CHARACTERconstant); }

\"(\\.|[^\\"])*\"	{ return(STRINGliteral); }


"("                  { return(*yytext);}
")"                  { return(*yytext);}
","                  { return(*yytext);}
"{"                  { return(*yytext);}
"}"                  { return(*yytext);}
"["                  { return(*yytext);}
"]"                  { return(*yytext);}
"."                  { return(key_err("."));}
"&"                  { return(*yytext);}
"*"                  { return(*yytext);}
"+"                  { return(*yytext);}
"-"                  { return(*yytext);}
"~"                  { return(*yytext);}
"!"                  { return(*yytext);}
"/"                  { return(*yytext);}
"%"                  { return(*yytext);}
"<"                  { return(*yytext);}
">"                  { return(*yytext);}
"^"                  { return(*yytext);}
"|"                  { return(*yytext);}
"?"                  { return(*yytext);}
":"                  { return(*yytext);}
";"                  { return(*yytext);}
"="                  { return(*yytext);}

"->"                 { return(key_err("->")); }
"++"                 { return(ICR); }
"--"                 { return(DECR); }
"<<"                 { return(LS); }
">>"                 { return(RS); }
"<="                 { return(LE); }
">="                 { return(GE); }
"=="                 { return(EQ); }
"!="                 { return(NE); }
"&&"                 { return(ANDAND); }
"||"                 { return(OROR); }
"*="                 { return(MULTassign); }
"/="                 { return(DIVassign); }
"%="                 { return(MODassign); }
"+="                 { return(PLUSassign); }
"-="                 { return(MINUSassign); }
"<<="                { return(LSassign); }
">>="                { return(RSassign); }
"&="                 { return(ANDassign); }
"^="                 { return(ERassign); }
"|="                 { return(ORassign); }
"..."                { /*return(ELLIPSIS);*/
		       yyerror("sorry ... not yet implemented");
  		       return(SYNTAX_ERROR);
 		     }

\n			{ numlig += 1; }

[ \t\v\f]		{}
.			{ char msg[2];

			  msg[0] = *yytext;
			  msg[1] = 0;
			  yyerror(msg);
			  return(SYNTAX_ERROR); }
%%

#ifdef flex
static int yywrap()
{
  return 1;
}
#endif

/* pour flex, la reinitialisation n'est pas faite dans yywrap pour que
   celle-ci soit faite apres un ^c pendant une execution precedante */

void smac_read_new_file()
{
#ifdef flex
  YY_NEW_FILE;
#else
  ;
#endif
}
