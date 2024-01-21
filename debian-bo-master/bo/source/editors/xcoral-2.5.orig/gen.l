%{

#include "global_parse.h"
#include "gen.tab.h"

extern YYSTYPE yylval;

#include "browser_util.h"
#include <string.h>
#include <ctype.h>


/*------------------------------------------------------------------------------
*/
int line_count;

static int bracket_level;

static int parent_level;

static int tmp_line_count;

static int virtual_flag;

static int const_flag;


/*------------------------------------------------------------------------------
*/

/* #define ARG_BUFF_SIZE  4096 */
#define ARG_BUFF_SIZE  32768

static char arg_buff[ARG_BUFF_SIZE];

static int  arg_index = 0;


/*------------------------------------------------------------------------------
*/

static int  state_stack[20];

static int  next_free;

static int  current_state;


/*------------------------------------------------------------------------------
*/

#define LINE_COUNT()         {                                 \
                               unsigned char* current;                  \
                                                               \
                               current = yytext + yyleng;      \
                               while ((--current) >= yytext) { \
                                 if ((*current) == '\n')       \
                                   line_count++;               \
                               }                               \
                             }


/*------------------------------------------------------------------------------
*/

#define BEGIN_STATE(state)   BEGIN(current_state = state)

#define STACK_STATE(state)   { state_stack[next_free++] = current_state; \
                               BEGIN_STATE(state); }

#define UNSTACK_STATE()      { BEGIN_STATE(state_stack[--next_free]); }


#define YY_USER_INIT         BEGIN_STATE(MAIN_STATE)

%}


WS           [ \v\t\n\r\f]

IDENT        [_a-zA-Z][_a-zA-Z0-9]*


OP1    ("+"|"-"|"*"|"/"|"%"|"^"|"&"|"|"|"~"|"!"|"="|"<"|">")

OP2    ("+="|"-="|"*="|"/="|"%="|"^="|"&="|"|=")

OP3    ("<<"|">>"|">>="|"<<="|"=="|"!="|"<="|">="|"&&"|"||"|"++"|"--")

OP4    ("new"|"delete"|","|"->*"|"->"|"()"|"[]")

OPERS  ({OP1}|{OP2}|{OP3}|{OP4}|{IDENT})


QUALIF       {WS}*:: 


STR_IGNORE   "\\\""|("\""([^\\\"\n]|("\\"(.)))*"\"")


%s MAIN_STATE

%s OPERATOR_STATE
%s PROC_STATE1
%s PROC_STATE2

%s CLASS_STATE1
%s CLASS_STATE2

%s TEMPLATE_STATE

%s PARENT_STATE
%s BODY_STATE

%s ARG_STATE
%s INIT_STATE
%s TAIL_STATE

%s CODE_STATE


%%

 /************************************************************************************/
 /*                    la supression des commentaires et macros                      */
 /************************************************************************************/

"/*"                                    { 
                                          int current;

                                          current = '\0';
                                          while (current != '/') {
                                            while (current != '*') {
         				     current = input();
					     if (current == '\n')
						line_count++;
					     else if (current == EOF)
						yyterminate ();
                                            }
					     current = input();
					     if (current == '\n')
						line_count++;
					     else if (current == EOF)
						yyterminate ();
                                          }
                                        }


"//"(.)*                                {}


^#[ \v\t\r\f]*{IDENT}             {
				    int inComment = 0;
				    int current = '\0';
				    int previous;

				    while ( 1 )
				    {
				      previous = current;
				      current  = input();
				      if (current == EOF )
					yyterminate ();
				      if (current == '\n' )
				      {
					line_count++;
					if (previous != '\\' && !inComment)
					  break;
				      }
				      else if (previous == '/' && current =='*')
				      {
				    	inComment = 1;
				      }
				      else if (previous == '*' &&
					       current =='/' && inComment)
					break;
				    }
					/* The above is incorrect */
				  }


"\n"                              { line_count++; }


[ \v\t]+                          {}


 /************************************************************************************/
 /*     le parcours de la liste des arguments d'une procedure ou d'une methode       */
 /************************************************************************************/

<OPERATOR_STATE>{OPERS}                 {
                                          strcat(yylval.text, (char *) yytext);
                                          strcat(yylval.text, " ");
                                          BEGIN_STATE(PROC_STATE1);
                                        }


 /************************************************************************************/
 /*     le parcours de la liste des arguments d'une procedure ou d'une methode       */
 /************************************************************************************/

<PROC_STATE1>"("                        {
                                         int current;
                                         int  parent_level;

                                         arg_index             = 0;
                                         parent_level          = 1;
                                         tmp_line_count        = 0;
                                         arg_buff[arg_index++] = '(';
                                         while(parent_level != 0) {
	 		   current = input();
                                           switch(current) {
               			case EOF: yyterminate ();
               			 break;
                                             case '(' : parent_level++;
                                                        break;
                                             case ')' : parent_level--;
                                                        break;
                                           }
                                           if (current == '\n')
                                             tmp_line_count++;
                                           if (arg_index < ARG_BUFF_SIZE)
                                             arg_buff[arg_index++] = current;
                                           else {
                                             yylval.position = line_count;
                                             return(LEX_ERROR_TOK);
                                           }
                                         }
                                         const_flag = False;
                                         BEGIN_STATE(PROC_STATE2);
                                       }

<PROC_STATE1>.                         {
                                         unput(yytext[0]);
                                         UNSTACK_STATE();
                                         if (current_state != BODY_STATE)
                                           *yy_class_name = '\0';
                                       }


 /************************************************************************************/
 /*              la recherche du token qui suit la liste des arguments               */
 /************************************************************************************/

<PROC_STATE2>":"                       {
                                         while (--arg_index >= 0)
                                           unput(arg_buff[arg_index]);
                                         parent_level = 0;
                                         yylval.is_virtual = virtual_flag;
                                         virtual_flag      = False;
                                         BEGIN_STATE(ARG_STATE);
                                         return(CPLUS_TOK);
                                       }


<PROC_STATE2>"{"                       {
                                         unput('{');
                                         while (--arg_index >= 0)
                                           unput(arg_buff[arg_index]);
                                         parent_level = 0;
                                         yylval.is_virtual = virtual_flag;
                                         virtual_flag      = False;
                                         BEGIN_STATE(ARG_STATE);
                                         return(CPLUS_TOK);
                                       }


<PROC_STATE2>"const"                   { const_flag = True; }

                                  
<PROC_STATE2>{IDENT}                   {
                                         line_count  += tmp_line_count;
                                         virtual_flag = False;
                                         UNSTACK_STATE();
                                         return(C_TOK);
                                       }

                                  
<PROC_STATE2>";"                       {
                                         unput(';');
                                         UNSTACK_STATE();
                                         if (current_state == BODY_STATE) {
                                           while (--arg_index >= 0)
                                             unput(arg_buff[arg_index]);
                                           parent_level = 0;
                                           yylval.is_virtual = virtual_flag;
                                           virtual_flag      = False;
                                           STACK_STATE(ARG_STATE);
                                           return(DECL_TOK);
                                         }
                                         else
                                           line_count += tmp_line_count;
                                         virtual_flag = False;
                                       }

                                  
<PROC_STATE2>"="                       {
                                         unput('=');
                                         UNSTACK_STATE();
                                         if (current_state == BODY_STATE) {
                                           while (--arg_index >= 0)
                                             unput(arg_buff[arg_index]);
                                           parent_level = 0;
                                           yylval.is_virtual = virtual_flag;
                                           virtual_flag      = False;
                                           STACK_STATE(ARG_STATE);
                                           return(DECL_TOK);
                                         }
                                         else
                                           line_count += tmp_line_count;
                                         virtual_flag = False;
                                       }

                                  
<PROC_STATE2>.                         { 
                                         unput(yytext[0]);
                                         line_count += tmp_line_count;
                                         virtual_flag = False;
                                         UNSTACK_STATE();
                                       }

                                  
 /************************************************************************************/
 /*                         La lecture du nom de la classe                           */
 /************************************************************************************/

<CLASS_STATE1>{IDENT}{QUALIF}{WS}*     {}

<CLASS_STATE1>{IDENT}                  {
                                         strcpy(yylval.text, (char *) yytext);
                                         yylval.position = line_count;
                                         BEGIN_STATE(CLASS_STATE2);
                                       }


<CLASS_STATE1>.                        { unput(yytext[0]);
					 UNSTACK_STATE();
				       }


<CLASS_STATE2>":"                      {
                                         BEGIN_STATE(PARENT_STATE);
                                         return(IDENT_TOK);
                                       }

<CLASS_STATE2>"{"                      {
                                         unput('{');
                                         BEGIN_STATE(PARENT_STATE);
                                         return(IDENT_TOK);
                                       }

<CLASS_STATE2>.                        { unput(yytext[0]);
					 UNSTACK_STATE();
				       }

<CLASS_STATE2>"<"({WS}*{IDENT}{WS}*',')*{WS}*{IDENT}">"
				       {}
 /************************************************************************************/
 /*                               Quelques mots clefs                                */
 /************************************************************************************/

<PARENT_STATE,BODY_STATE>"public"      { return(PUBLIC_TOK);    }

<PARENT_STATE,BODY_STATE>"protected"   { return(PROTECTED_TOK); }

<PARENT_STATE,BODY_STATE>"private"     { return(PRIVATE_TOK);   }


 /************************************************************************************/
 /*                       La lecture de la liste des parents                         */
 /************************************************************************************/

<PARENT_STATE>"virtual"                {}
<PARENT_STATE>"<"                      { return ('<'); }
<PARENT_STATE>">"                      { return ('>');}
<PARENT_STATE>","                      { return (',');}

<PARENT_STATE>{IDENT}{QUALIF}{WS}*     {}
<PARENT_STATE>{IDENT}                  {
                                         strcpy(yylval.text, (char *) yytext);
                                         yylval.position = line_count;
                                         return(IDENT_TOK);
                                       }

<PARENT_STATE>"{"                      { BEGIN_STATE(BODY_STATE); return '{'; }


 /************************************************************************************/
 /*                  La lecture des "members" declares dans une classe               */
 /************************************************************************************/

<BODY_STATE>"virtual"                  { virtual_flag = True; }

<BODY_STATE>"friend"[^;{]*             { LINE_COUNT(); }

<BODY_STATE>"}"                        { 
					 UNSTACK_STATE();
                                         *yy_class_name = '\0';
                                         return('}');
                                       }


 /************************************************************************************/
 /*      La lecture de la liste des arguments d'une methode ou d'une procedure       */
 /************************************************************************************/

<ARG_STATE>"unsigned"                  { return(UNSIGNED_TOK); }

<ARG_STATE>"signed"                    { return(SIGNED_TOK);   }

<ARG_STATE>"char"                      { return(CHAR_TOK);     }

<ARG_STATE>"short"                     { return(SHORT_TOK);    }

<ARG_STATE>"int"                       { return(INT_TOK);      }

<ARG_STATE>"long"                      { return(LONG_TOK);     }

<ARG_STATE>"struct"                    { return(STRUCT_TOK);   }

<ARG_STATE>"union"                     { return(UNION_TOK);    }

<ARG_STATE>"class"                     { return(CLASS_TOK);    }

<ARG_STATE>"const"                     { return(CONST_TOK);    }

<ARG_STATE>"enum"                      { return(ENUM_TOK);    }

<ARG_STATE>"*"                         { return('*');          }

<ARG_STATE>"&"                         { return('&');          }

<ARG_STATE>"["[^\]]*"]"                { LINE_COUNT(); return(ARRAY_TOK);    }

<ARG_STATE>"register"                  {}

<ARG_STATE>"auto"                      {}

<ARG_STATE>"volatile"                  {}

<ARG_STATE>"<"                         { return('<'); }
<ARG_STATE>">"                         { return('>'); }
<ARG_STATE>","                         { return(','); }


<ARG_STATE>"..."                       {
                                         strcpy(yylval.text, (char *) yytext);
                                         return(ELLIPSIS_TOK);
                                       }


<ARG_STATE>({IDENT}{QUALIF}{WS}*)*{IDENT} {
                                         strcpy(yylval.text, (char *) yytext);
                                         return(IDENT_TOK);
                                       }

<ARG_STATE>{IDENT}"::"{IDENT}          {
                                         strcpy(yylval.text, (char *) yytext);
                                         return(IDENT_TOK);
                                       }


<ARG_STATE>"="                         { BEGIN_STATE(INIT_STATE); }


<ARG_STATE>"("                         {
                                         parent_level++;
                                         return('(');
                                       }


<ARG_STATE>")"                         {
                                         if ((--parent_level) == 0)
                                           BEGIN_STATE(TAIL_STATE);
                                         return(')');
                                       }


<INIT_STATE>{STR_IGNORE}               {}


<INIT_STATE>"','"                      {}


<INIT_STATE>"("                        {
                                         int  parent_level;
                                         char current;

                                         parent_level = 1;
                                         while (parent_level != 0) {
                                           current = input();
                                          switch(current) {
               			case EOF: yyterminate ();
               			 break;
                                             case '(' : parent_level++;
                                                        break;
                                             case ')' : parent_level--;
                                                        break;
                                           }
                                      }
                                    }


<INIT_STATE>","                        { unput(','); BEGIN_STATE(ARG_STATE); }


<INIT_STATE>")"                        { unput(')'); BEGIN_STATE(ARG_STATE); }


<INIT_STATE>.                          {}


 /************************************************************************************/
 /*                                  Les Divers                                      */
 /************************************************************************************/

<TAIL_STATE>[^;{]*/(;|"{")             { 
                                         LINE_COUNT(); 
                                         UNSTACK_STATE();
                                         if (const_flag == True) {
                                           const_flag = False;
                                           return(CONST_TOK);
                                         }
                                       }


 /************************************************************************************/
 /*                                  Les Divers                                      */
 /************************************************************************************/

<CODE_STATE>"{"                        { bracket_level++; }

<CODE_STATE>"}"                        {
                                         if ((--bracket_level) == 0) {
                                           UNSTACK_STATE();
                                           return(SYNC_TOK);
                                         }
                                       }

<CODE_STATE>"'{'"                      {}

<CODE_STATE>"'}'"                      {}

<CODE_STATE>"'"                        {}

<CODE_STATE>"/"                        {}

<CODE_STATE>{STR_IGNORE}               {}

<CODE_STATE>[^{}'\"/\n]+               {}

"{"                                    {
                                         bracket_level = 1;
                                         STACK_STATE(CODE_STATE);
	                                   }


";"                                    { return(SYNC_TOK); }


 /************************************************************************************/

<MAIN_STATE>"class"                    {
                                         STACK_STATE(CLASS_STATE1);
                                         /*return(CLASS_TOK);*/
                                       }
<BODY_STATE>"class"                    {
                                         STACK_STATE(CLASS_STATE1);
                                         /*return(CLASS_TOK);*/
                                       }

<MAIN_STATE>"struct"                   {
                                         STACK_STATE(CLASS_STATE1);
                                         /*return(STRUCT_TOK);*/
                                       }

<INIT_STATE,MAIN_STATE>"static"	       { return(STATIC_TOK); }

<INIT_STATE,MAIN_STATE>"inline"	       { return(INLINE_TOK); }

<MAIN_STATE>{IDENT}{QUALIF}            {
                                         unsigned char* current;

                                         current = yytext + yyleng - 2;
                                         while ((--current) >= yytext) {
                                           if (isspace(*current)) {
                                             if ((*current) == '\n')
                                               line_count++;
                                           }
                                           else
                                             break;
                                         }
                                         *(current + 1) = '\0';
                                         strcpy(yy_class_name, (char *) yytext);
                                       }


<MAIN_STATE,BODY_STATE>"operator"      {
                                         strcpy(yylval.text, "operator ");
                                         yylval.position = line_count;
                                         STACK_STATE(OPERATOR_STATE);
                                       }


<MAIN_STATE,BODY_STATE>(~{WS}*)?{IDENT}       {
		         if ( yytext[0] != '~' )
	                                         strcpy(yylval.text, (char *) yytext);
		         else {
			register unsigned char *p = yytext + 1;
			yylval.text [0]= '~';
			while (  isspace( *p ) ) 
				p++;
			strcpy(&(yylval.text[1]), (char *) p );
			LINE_COUNT();
		         }
                                         yylval.position = line_count;
                                         STACK_STATE(PROC_STATE1);
                                       }


.                                      {}


 /************************************************************************************/

%%

void flex_init(file)
   FILE* file;
{
  static int first_call = 1;
  line_count   = 1;
  next_free    = 0;
  virtual_flag = False;
  yyin = file;

  yy_init = 1;
  yy_start = 0;

  if (first_call == 1)
    first_call = 0;
  else
        yyrestart (file);
}
