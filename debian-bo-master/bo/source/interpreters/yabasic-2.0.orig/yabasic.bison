%{
/*
     YABASIC --- a tiny integrated Basic Compiler/Interpreter

     BISON - part
     
     this Program is subject to the GNU General Public License;
     see the file yabasic.c for details.
*/

#include "yabasic.h"     /* definitions of yabasic */
#include <malloc.h>

extern int dimcount;
extern int errorlevel;
extern int interactive; /* true, if commands come from stdin */	

void __yy_bcopy(char *,char *,int); /* prototype missing */

int yylineno=1;
int yylex(void);


%}

%union {
  double number;        /* double number */
  int token;            /* token of command */
  char *string;         /* quoted string */
  char *symbol;         /* general symbol */
  char *strsym;         /* string symbol */
  int sep;              /* number of newlines as seperator */
}

%type <number> step_part
%type <number> const
%type <number> hashed_number

%token <number> NUMBER
%token <symbol> SYMBOL
%token <strsym> STRSYM
%token <string> STRING
%token <sep> SEP

%token FOR TO STEP NEXT GOTO GOSUB LABEL ON 
%token IF THEN ELSE ENDIF DO
%token PRINT INPUT RETURN DIM END
%token AND OR NOT
%token NE LE GE LT GT EQ
%token READ DATA RESTORE
%token OPEN CLOSE
%token WINDOW DOT LINE CIRCLE TEXT CLEAR PRINTER
%token WAIT BELL

%left OR
%left AND
%left NOT

%left '-' '+'
%left '*' '/'
%left '^'
%nonassoc UMINUS

%%

statement_list: statement
  | statement_list SEP {yylineno+=$2;
	     		if ($2==2 && interactive) {YYACCEPT;}
                        if (errorlevel<=ERROR) {YYABORT;}} statement ;
	

statement:  /* empty */
  | string_assignment 
  | assignment
  | DO string_expression {pop();}
  | DO expression {pop();}
  | for_loop 
  | if_clause
  | GOTO SYMBOL {create_goto($2);}
  | GOSUB SYMBOL {create_gosub($2);}
  | ON SYMBOL {create_pushdblsym($2);} GOTO {create_skipper();}
    goto_list {create_nop();}
  | ON SYMBOL {create_pushdblsym($2);} GOSUB {create_skipper();} 
    gosub_list {create_nop();}
  | LABEL SYMBOL {create_label($2);}
  | OPEN hashed_number ',' string_expression ',' STRING
    {create_myopen($2,$6);}
  | OPEN hashed_number ',' string_expression
    {create_myopen($2,"a");}
  | CLOSE hashed_number {create_myclose($2);}
  | PRINT stream printlist semicolon {}
  | INPUT stream prompt inputlist 
  | READ readlist
  | DATA datalist
  | RESTORE {create_restore("");}
  | RESTORE SYMBOL {create_restore($2);}
  | RETURN {create_return();}
  | DIM dimlist
  | OPEN WINDOW {create_openwin(0);}
  | OPEN WINDOW expression ',' expression {create_openwin(2);}
  | OPEN WINDOW expression ',' expression ',' expression {create_openwin(3);}
  | DOT expression ',' expression {create_dot();}
  | LINE expression ',' expression TO expression ',' expression 
    {create_line();}
  | CIRCLE expression ',' expression ',' expression
    {create_circle()}
  | TEXT string_expression ',' expression ',' expression {create_text(1==1);}
  | TEXT expression ',' expression ',' string_expression {create_text(1!=1);}
  | TEXT expression ',' expression ',' expression {
	error(ERROR,"Only string allowed as TEXT");}
  | CLOSE WINDOW {create_closewin();}
  | CLEAR WINDOW {create_clearwin();}
  | OPEN PRINTER {create_openprinter(0);}
  | OPEN PRINTER string_expression {create_openprinter(1);}
  | CLOSE PRINTER {create_closeprinter();}
  | WAIT expression {create_wait();}
  | BELL {create_bell();}
  | SYMBOL {sprintf(string,"'%s' is not a yabasic-command",$1);
            error(ERROR,string);YYABORT;}
  | END {create_myend();};

string_assignment: STRSYM EQ string_expression {create_popstrsym($1);}
  | STRSYM '(' {pushletter("l");} functionlist ')'
    EQ string_expression {create_call($1);}
  ;

string_expression: STRSYM {create_pushstrsym($1);}
  | STRING {create_pushstr($1);}
  | string_expression '+' string_expression {create_concat();}
  | STRSYM '(' {pushletter("r");} functionlist ')' {create_call($1);}
  | '(' string_expression ')'
  ;

assignment: SYMBOL EQ expression {create_popdblsym($1);} 
  | SYMBOL '(' {pushletter("l");} functionlist ')' 
    EQ expression {create_call($1);}
  ;

expression:  NUMBER {create_pushdbl($1);}
  | SYMBOL {create_pushdblsym($1);}
  | SYMBOL '(' {pushletter("r");} functionlist ')' {create_call($1);};
  | '(' expression ')' 
  | expression '+' expression {create_dblbin('+');}
  | expression '-' expression {create_dblbin('-');}
  | expression '*' expression {create_dblbin('*');}
  | expression '/' expression {create_dblbin('/');}
  | expression '^' expression {create_dblbin('^');}
  | '-' expression %prec UMINUS {create_negate();}
  ;

const: NUMBER {$$=$1}
  | '+' NUMBER {$$=$2}
  | '-' NUMBER {$$=-$2;};

dimlist: SYMBOL '(' {dimcount=0;} boundlist ')' {create_dim($1,'d');}
  | dimlist ',' SYMBOL '(' {dimcount=0;} boundlist ')' {create_dim($3,'d');}
  | STRSYM '(' {dimcount=0;} boundlist ')' {create_dim($1,'s');}
  | dimlist ',' STRSYM '(' {dimcount=0;} boundlist ')' {create_dim($3,'s');}
  ;

boundlist: expression {dimcount++;}
  | boundlist ',' expression {dimcount++;}
  ;
 
for_loop: FOR SYMBOL EQ expression 
            {create_popdblsym($2);pushgoto();create_pushdblsym($2);}
	  TO expression 
	  step_part {
	     create_dblrelop(($8>0)?'{':'}');
             create_decide();
             pushlabel();}
          statement_list {
             create_pushdbl($8);
	     create_pushdblsym($2);	
             create_dblbin('+');
	     create_popdblsym($2);
             swap();popgoto();poplabel();}
          NEXT next_symbol ;

step_part: SEP {$$=1.0;} /* can be omitted */
  | STEP const {$$=$2;}
  ;

next_symbol:  /* can be omitted */
  | SYMBOL {;}
  ;

if_clause: IF comparison {create_decide();pushlabel();}
           THEN statement_list {pushlabel();swap();poplabel();}
           else_part {poplabel();}
           ENDIF;

comparison: '(' comparison ')'
  | comparison OR comparison {create_boole('|');}
  | comparison AND comparison {create_boole('&');}
  | NOT comparison {create_boole('!');}
  | string_expression EQ string_expression {create_strrelop('=');}
  | string_expression NE string_expression {create_strrelop('!');}
  | expression EQ expression {create_dblrelop('=');}
  | expression NE expression {create_dblrelop('!');}
  | expression LT expression {create_dblrelop('<');}
  | expression LE expression {create_dblrelop('{');}
  | expression GT expression {create_dblrelop('>');}
  | expression GE expression {create_dblrelop('}');}

else_part: /* can be omitted */
  | ELSE statement_list
  ;

inputlist: input
  | inputlist ',' input
  ;

input: SYMBOL {create_myread('d');create_popdblsym($1);}
  | SYMBOL '(' {pushletter("l");} functionlist ')' 
    {create_myread('d');create_call($1);}
  | STRSYM {create_myread('s');create_popstrsym($1);}
  | STRSYM '(' {pushletter("l");} functionlist ')' 
    {create_myread('s');create_call($1);}
  ;

readlist: readitem
  | readlist ',' readitem
  ;

readitem: SYMBOL {create_readdata('d');create_popdblsym($1);}
  | SYMBOL '(' {pushletter("l");} functionlist ')' 
    {create_readdata('d');create_call($1);}
  | STRSYM {create_readdata('s');create_popstrsym($1);}
  | STRSYM '(' {pushletter("l");} functionlist ')' 
    {create_readdata('s');create_call($1);}
  ;

datalist: STRING {create_strdata($1);}
  | const {create_dbldata($1);}
  | datalist ','  STRING {create_strdata($3);}
  | datalist ',' const {create_dbldata($3);}
  ;

prompt: /* possible empty */ {create_prompt("?");}
  | STRING {create_prompt($1);}
  ;

printlist:  /* possible empty */
  | expression {create_print('d');}
  | printlist ',' expression {create_print('d');} 
  | string_expression {create_print('s');} 
  | printlist ',' string_expression {create_print('s');}
  ;

stream: /* possible empty */ {create_myswitch(0.0);}
  | '#' NUMBER {create_myswitch($2);}
  ;

hashed_number: '#' NUMBER {$$=$2;}
  | NUMBER {$$=$1;} /* need not contain hash */
  ;

semicolon: /* can be left out */ {create_print('n');}
  | ';'
  ;

functionlist:  /* might be empty */
  | expression {pushletter("d");concat();}
  | functionlist ',' expression {pushletter("d");concat();}
  | string_expression {pushletter("s");concat();}
  | functionlist ',' string_expression {pushletter("s");concat();}
  ;

goto_list: SYMBOL {create_goto($1);}
  | goto_list ',' SYMBOL {create_goto($3);}
  ;

gosub_list: SYMBOL {create_gosub($1);}
  | gosub_list ',' SYMBOL {create_gosub($3);}
  ;

