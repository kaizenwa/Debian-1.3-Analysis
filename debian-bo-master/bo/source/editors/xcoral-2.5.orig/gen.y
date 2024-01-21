%{

#include "global_parse.h"
#include "result_types.h"
#include "file_dict.h"
#include "proc_dict.h"
#include "class_dict.h"
#include "browser_pars.h"
#include "browser_util.h"
#include <stdio.h>


/*------------------------------------------------------------------------------
*/
char yy_class_name[256];

static char yy_class_stack[50][256];
static int yy_class_stack_top = 0;
static int yy_in_class = 0;

static ScopeType yy_scope;

#define S_PRECEDENCE       10
#define P_PRECEDENCE       10
#define A_PRECEDENCE       20
#define MAX_PRECEDENCE     30

#define parentheses(ref_prec, inter, result) \
          if (inter.precedence < ref_prec)   \
            strcat(result.text, "(");        \
          strcat(result.text, inter.text);   \
          if (inter.precedence < ref_prec)   \
            strcat(result.text, ")");        \
          result.precedence = ref_prec;

%}

%start     file

%right     '*' '&'
%left      ARRAY_TOK
%token     UNSIGNED_TOK SIGNED_TOK 
%token     CHAR_TOK SHORT_TOK INT_TOK LONG_TOK 
%token     STRUCT_TOK UNION_TOK CLASS_TOK ENUM_TOK
%token     CONST_TOK ELLIPSIS_TOK
%token     IDENT_TOK CPLUS_TOK C_TOK DECL_TOK SYNC_TOK
%token     PUBLIC_TOK PROTECTED_TOK PRIVATE_TOK
%token	   STATIC_TOK INLINE_TOK
%token     LEX_ERROR_TOK

%%

/************************************************************************/

file
  :
  | file class_decl
  | file procedure_impl
  | file STATIC_TOK
  | file INLINE_TOK
  | file SYNC_TOK
  ;


/************************************************************************/

class_decl
  : class_start class_name '{' member_list '}' class_end
  | class_start class_name '{' '}' class_end
  | class_start class_name parent_list '{' member_list '}' class_end
  | class_start class_name parent_list '{' '}' class_end
  ;


class_start
  : { if (yy_in_class)
      {
	strcpy(yy_class_stack[yy_class_stack_top], yy_class_name);
	yy_class_stack_top++;
      }
      yy_in_class++;
    };

class_end
  : { yy_in_class--;

      if (yy_in_class)
      {
	yy_class_stack_top--;
	strcpy(yy_class_name, yy_class_stack[yy_class_stack_top]);
      }
    }

/************************************************************************/

class_name
  : IDENT_TOK opt_template_args {
      strcpy(yy_class_name, $1.text);
      if (add_class_decl($1.text, $1.position) == BERROR)
        YYABORT;
      yy_scope = PUBLIC_SCOPE;
    }
  ;


/************************************************************************/

parent_list
  : parent
  | parent_list ',' parent
  ;

/************************************************************************/

parent
  : PUBLIC_TOK IDENT_TOK  opt_template_args  {
			char	buf[256];
			sprintf (buf,"%s%s", $2.text, $3.text);
      if (add_parent(yy_class_name, $2.text, PUBLIC_SCOPE) == BERROR)
        YYABORT;
    }
  | PROTECTED_TOK IDENT_TOK opt_template_args   {
			char	buf[256];
			sprintf (buf,"%s%s", $2.text, $3.text);
      if (add_parent(yy_class_name, buf, PROTECTED_SCOPE) == BERROR)
        YYABORT;
    }
  | PRIVATE_TOK   IDENT_TOK   opt_template_args {
			char	buf[256];
			sprintf (buf,"%s%s", $2.text, $3.text);
      if (add_parent(yy_class_name, buf, PRIVATE_SCOPE) == BERROR)
        YYABORT;
    }
  | IDENT_TOK opt_template_args {
			char	buf[256];
			sprintf (buf,"%s%s", $1.text, $2.text);
      if (add_parent(yy_class_name, buf, PRIVATE_SCOPE) == BERROR)
        YYABORT;
    }
  ;


/************************************************************************/

member_list
  : member_decl
  | member_list member_decl
  ;


/************************************************************************/

member_decl
  : class_decl
  | PUBLIC_TOK          {
      yy_scope = PUBLIC_SCOPE;
    }
  | PROTECTED_TOK       {
      yy_scope = PROTECTED_SCOPE;
    }
  | PRIVATE_TOK         {
      yy_scope = PRIVATE_SCOPE;
    }
  | DECL_TOK '(' arguments_list ')' trail_decl {
      strcpy($$.text, $1.text);
      strcat($$.text, "(");
      strcat($$.text, $3.text);
      strcat($$.text, ")");
      strcat($$.text, $5.text);
      if (add_method_decl(yy_class_name, $$.text,
                          yy_scope, $1.is_virtual, $1.position) == BERROR)
        YYABORT;
    }
  | CPLUS_TOK '(' arguments_list ')' trail_decl {
      strcpy($$.text, $1.text);
      strcat($$.text, "(");
      strcat($$.text, $3.text);
      strcat($$.text, ")");
      strcat($$.text, $5.text);
      if (add_method_decl(yy_class_name, $$.text,
                          yy_scope, $1.is_virtual, $1.position) == BERROR)
        YYABORT;
      (void) add_method_impl(yy_class_name, $$.text, $1.position);
    }
  | C_TOK 
  | SYNC_TOK 
  | LEX_ERROR_TOK {
      fprintf(stderr, ">>>>>>  Arguments buffer overflow at line %d\n", $1.position);
      YYABORT;
    }
  ;


/************************************************************************/

procedure_impl
  : STATIC_TOK CPLUS_TOK '(' arguments_list ')' trail_decl {
      }
  | INLINE_TOK CPLUS_TOK '(' arguments_list ')' trail_decl {
      }
  | STATIC_TOK C_TOK {
      }
  | INLINE_TOK C_TOK {
      }
  | CPLUS_TOK '(' arguments_list ')' trail_decl {
      strcpy($$.text, $1.text);
      strcat($$.text, "(");
      strcat($$.text, $3.text);
      strcat($$.text, ")");
      strcat($$.text, $5.text);
      if (yy_class_name[0] == '\0') {
        (void) add_proc_impl($$.text, CPLUS_PROC, $1.position);
      }
      else {
        if (add_method_impl(yy_class_name, $$.text, $1.position) == BERROR)
          YYABORT;
      }
      *yy_class_name = '\0';
    }
  | C_TOK  {
      strcpy($$.text, $1.text);
      strcat($$.text, "()");
      (void) add_proc_impl($$.text, C_PROC, $1.position);
      *yy_class_name = '\0';
    }
  | LEX_ERROR_TOK {
      fprintf(stderr, ">>>>>>  Arguments buffer overflow at line %d\n", $1.position);
      YYABORT;
    } 
  ;


/************************************************************************/

trail_decl
  :           { $$.text[0] = '\0';         }
  | CONST_TOK { strcpy($$.text, " const"); }
  ;


/************************************************************************/

arguments_list
  :                              { $$.text[0] = '\0';        }
  | argument                     { strcpy($$.text, $1.text); }
  | arguments_list ELLIPSIS_TOK  {
                                   strcpy($$.text, $1.text);
                                   strcat($$.text, " ...");
                                 }
  | arguments_list ',' argument  {
                                   strcpy($$.text, $1.text);
                                   strcat($$.text, ", ");
                                   strcat($$.text, $3.text);
                                 }
  | arguments_list ','           {
                                   strcpy($$.text, $1.text);
                                   strcat($$.text, ", ");
                                 }
  ;


/************************************************************************/

argument
  : type declarator             { 
      strcpy($$.text, $1.text);
      strcat($$.text, $2.text);
    }
  | CONST_TOK type declarator   { 
      strcpy($$.text, "const ");
      strcat($$.text, $2.text);
      strcat($$.text, $3.text);
    }
  | type CONST_TOK declarator   { 
      strcpy($$.text, "const ");
      strcat($$.text, $1.text);
      strcat($$.text, $3.text);
    }
  ;


/************************************************************************/

type
  : CHAR_TOK                     { strcpy($$.text, "char"); }
  | UNSIGNED_TOK CHAR_TOK        { strcpy($$.text, "unsigned char"); }
  | SIGNED_TOK   CHAR_TOK        { strcpy($$.text, "char"); }
  | SHORT_TOK                    { strcpy($$.text, "short"); }
  | UNSIGNED_TOK SHORT_TOK       { strcpy($$.text, "unsigned short"); }
  | SIGNED_TOK   SHORT_TOK       { strcpy($$.text, "short"); }
  | INT_TOK                      { strcpy($$.text, "int"); }
  | UNSIGNED_TOK INT_TOK         { strcpy($$.text, "unsigned int"); }
  | SIGNED_TOK   INT_TOK         { strcpy($$.text, "int"); }
  | UNSIGNED_TOK                 { strcpy($$.text, "unsigned int"); }
  | SIGNED_TOK                   { strcpy($$.text, "int"); }
  | LONG_TOK                     { strcpy($$.text, "long"); }
  | UNSIGNED_TOK LONG_TOK        { strcpy($$.text, "unsigned long"); }
  | SIGNED_TOK   LONG_TOK        { strcpy($$.text, "long"); }
  | STRUCT_TOK   IDENT_TOK       { strcpy($$.text, $2.text); }
  | UNION_TOK    IDENT_TOK       { strcpy($$.text, $2.text); }
  | CLASS_TOK    IDENT_TOK       { strcpy($$.text, $2.text); }
  | ENUM_TOK	 IDENT_TOK	 { strcpy($$.text, $2.text); }
  | IDENT_TOK opt_template_args  {
				   strcpy($$.text, $1.text);
				   strcat($$.text, $2.text);
				 }
  ;


opt_template_args
	: { $$.text[0] = '\0'; }
	| '<' template_args argument '>' {
					   sprintf($$.text, "<%s%s>",
					   $1.text, $2.text);
				         }

template_args
	: { $$.text[0] = '\0'; }
	| template_args argument ','     {
					   sprintf($$.text, "%s%s, ",
					   $1.text, $2.text);
					 }
	;

/************************************************************************/

declarator
  :                                      { 
      $$.text[0]    = '\0';
      $$.precedence = MAX_PRECEDENCE;
    }
  | IDENT_TOK                            { 
      $$.text[0]    = '\0';
      $$.precedence = MAX_PRECEDENCE;
    }
  | '*' declarator                       {
      strcpy($$.text, "*");
      parentheses(S_PRECEDENCE, $2, $$)
    }
  | '*' CONST_TOK declarator  %prec '*'  {
      strcpy($$.text, "*const ");
      parentheses(S_PRECEDENCE, $2, $$)
    }
  | '&' declarator                       {
      strcpy($$.text, "&");
      parentheses(P_PRECEDENCE, $2, $$)
    }                     
  | '&' CONST_TOK declarator  %prec '&'  {
      strcpy($$.text, "&const ");
      parentheses(P_PRECEDENCE, $2, $$)
    }                     
  | declarator ARRAY_TOK                 {
      $$.text[0] = '\0';
      parentheses(A_PRECEDENCE, $1, $$)
      strcat($$.text, "[]");
    }
  | '(' declarator ')' '(' arguments_list ')' {
      strcpy($$.text, "(");
      strcat($$.text, $2.text);
      strcat($$.text, ")");
      strcat($$.text, " (");
      strcat($$.text, $5.text);
      strcat($$.text, ")");
      $$.precedence = MAX_PRECEDENCE;
    }
  | '(' declarator ')'                   {
      strcpy($$.text, $2.text);
      $$.precedence = $2.precedence;
    }
  ;


/************************************************************************/


%%

int yyerror(error_msg)
    char* error_msg;
{
	if ( OpVerbose () == True ) {
	  fprintf(stderr, "warning >>>>>>  Internal Parser Error at line %d\n", line_count);
	  fprintf(stderr, "warning    >>  %s\n", error_msg);
	}
}


int browser_yyparse(file_name)
    char* file_name;
{
  FILE* file;
  int   result;

  result = 1;
  *yy_class_name = '\0';
  file = fopen(file_name, "r");
  if (file != Null) {
    flex_init(file);
    result = yyparse();
    fclose(file);
  }
  else
    fprintf(stderr, "\n>>>>>>  Unable to open file %s\n", file_name);
  return(result);
}
