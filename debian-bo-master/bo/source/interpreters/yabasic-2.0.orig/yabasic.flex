%{
/*
     YABASIC --- a tiny integrated Basic Compiler/Interpreter

     FLEX - part
     
     this Program is subject to the GNU General Public License;
     see the file yabasic.c for details.
*/

#include <string.h>
#include "bison.h"       /* get tokens from BISON */
#include "yabasic.h"     /* definitions of yabasic */

extern int yylineno;
extern int errorlevel;

void switch_to_my_file(FILE *inputfile); /* switches input to given file */
int read(int,char *,int); /* prototype for read */
%}

NAME [A-Za-z][A-za-z0-9]*
%option noyywrap

%%
[ \t]* ;        /* ignore Whitespace */

"\n\n" {yylval.sep=2; return SEP;}
[:\n] {yylval.sep=(yytext[0]==':')?0:1;return SEP;}


FOR return FOR;
TO return TO;
STEP return STEP;
NEXT return NEXT;
GOTO return GOTO;
GOSUB return GOSUB;
ON return ON;
LABEL return LABEL;
IF return IF;
THEN return THEN;
ELSE return ELSE;
ENDIF return ENDIF;
FI return ENDIF;
DO return DO;
OPEN return OPEN;
CLOSE return CLOSE;
PRINT return PRINT;
\? return PRINT;
INPUT return INPUT;
RETURN return RETURN;
DIM return DIM;
END return END;
READ return READ;
DATA return DATA;
RESTORE return RESTORE;
AND return AND;
OR return OR;
NOT return NOT;
WINDOW return WINDOW;
PRINTER return PRINTER;
DOT return DOT;
LINE return LINE;
CIRCLE return CIRCLE;
CLEAR return CLEAR;
TEXT return TEXT;
WAIT return WAIT;
PAUSE return WAIT;
BELL return BELL;
"<>" return NE;
"<=" return LE;
">=" return GE;
"=" return EQ;
"<" return LT;
">" return GT;

[-+*/:(),.;] {return yytext[0];}

(([0-9]+|([0-9]*\.[0-9]*))([eE][-+]?[0-9]+)?) {
  {float f;
  sscanf(yytext,"%f",&f);
  yylval.number=f;
  return NUMBER;}
}

REM.* {yylval.sep=0; return SEP;}  /* comments span 'til the end of the line */

({NAME}+) {
  yylval.symbol=(char *)my_strdup(yytext);
  return SYMBOL;
}

	/* Symbols with a trailing $-sign are treated special */
({NAME}+"$") {
  yylval.strsym=(char *)my_strdup(yytext);
  return STRSYM;
}

\"[^\"\n]*\" {
  yylval.string=(char *)my_strdup(yytext+1);*(yylval.string+yyleng-2)='\0';
  return STRING;
}

\"[^\"\n]*\n {
  error(WARNING,"String not terminated");
  yylval.string=(char *)my_strdup(yytext);
  yylineno++;
  return STRING;
}

. return yytext[0];

%%

void yyerror(char *msg)
{
  int n;
  
  sprintf(string,"%s at %n",msg,&n);
  if (*yytext=='\n') 
    sprintf(string+n,"end of line");
  else 
    sprintf(string+n,"'%s'",yytext);
  error(ERROR,string);
 
  return;
}

void switch_to_my_file(FILE *inputfile) /* switches lex input to given file */
{
  YY_BUFFER_STATE my_buffer;
  char c=' ';

  my_buffer=yy_create_buffer(inputfile,YY_BUF_SIZE);
  yy_switch_to_buffer(my_buffer);

  /* stop gcc from complaining about yyunput */
  if (c!=' ') unput(c);
  
  return;
}

