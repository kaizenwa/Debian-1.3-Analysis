
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

    /* This file is a transformation of James A. Roskind's C++ grammar */


    /* Copyright (C) 1989,1990 James A. Roskind, All rights reserved.
    This grammar was developed  and  written  by  James  A.  Roskind.
    Copying  of  this  grammar  description, as a whole, is permitted
    providing this notice is intact and applicable  in  all  complete
    copies.   Translations as a whole to other parser generator input
    languages  (or  grammar  description  languages)   is   permitted
    provided  that  this  notice is intact and applicable in all such
    copies,  along  with  a  disclaimer  that  the  contents  are   a
    translation.   The reproduction of derived text, such as modified
    versions of this grammar, or the output of parser generators,  is
    permitted,  provided  the  resulting  work includes the copyright
    notice "Portions Copyright (c)  1989,  1990  James  A.  Roskind".
    Derived products, such as compilers, translators, browsers, etc.,
    that  use  this  grammar,  must also provide the notice "Portions
    Copyright  (c)  1989,  1990  James  A.  Roskind"  in   a   manner
    appropriate  to  the  utility,  and in keeping with copyright law
    (e.g.: EITHER displayed when first invoked/executed; OR displayed
    continuously on display terminal; OR via placement in the  object
    code  in  form  readable in a printout, with or near the title of
    the work, or at the end of the file).  No royalties, licenses  or
    commissions  of  any  kind are required to copy this grammar, its
    translations, or derivative products, when the copies are made in
    compliance with this notice. Persons or corporations that do make
    copies in compliance with this notice may charge  whatever  price
    is  agreeable  to  a  buyer, for such copies or derivative works.
    THIS GRAMMAR IS PROVIDED ``AS IS'' AND  WITHOUT  ANY  EXPRESS  OR
    IMPLIED  WARRANTIES,  INCLUDING,  WITHOUT LIMITATION, THE IMPLIED
    WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR
    PURPOSE.

    James A. Roskind
    Independent Consultant
    516 Latania Palm Drive
    Indialantic FL, 32903
    (407)729-4348
    jar@ileaf.com


    ---end of copyright notice---


This file is a companion file to a C++ grammar description file.

*/


/* FILENAME: C.Y */

/*  This  is a grammar file for the dpANSI C language.  This file was
last modified by J. Roskind on 3/7/90. Version 1.00 */




/* ACKNOWLEDGMENT:

Without the effort expended by the ANSI C standardizing committee,  I
would  have been lost.  Although the ANSI C standard does not include
a fully disambiguated syntax description, the committee has at  least
provided most of the disambiguating rules in narratives.

Several  reviewers  have also recently critiqued this grammar, and/or
assisted in discussions during it's preparation.  These reviewers are
certainly not responsible for the errors I have committed  here,  but
they  are responsible for allowing me to provide fewer errors.  These
colleagues include: Bruce Blodgett, and Mark Langley. */

#include "rename.h"
  
%}

/* This refined grammar resolves several typedef ambiguities  in  the
draft  proposed  ANSI  C  standard  syntax  down  to  1  shift/reduce
conflict, as reported by a YACC process.  Note  that  the  one  shift
reduce  conflicts  is the traditional if-if-else conflict that is not
resolved by the grammar.  This ambiguity can  be  removed  using  the
method  described in the Dragon Book (2nd edition), but this does not
appear worth the effort.

There was quite a bit of effort made to reduce the conflicts to  this
level,  and  an  additional effort was made to make the grammar quite
similar to the C++ grammar being developed in  parallel.   Note  that
this grammar resolves the following ANSI C ambiguity as follows:

ANSI  C  section  3.5.6,  "If  the [typedef name] is redeclared at an
inner scope, the type specifiers shall not be omitted  in  the  inner
declaration".   Supplying type specifiers prevents consideration of T
as a typedef name in this grammar.  Failure to supply type specifiers
forced the use of the TYPEDEFname as a type specifier.

ANSI C section 3.5.4.3, "In a parameter declaration, a single typedef
name in parentheses is  taken  to  be  an  abstract  declarator  that
specifies  a  function  with  a  single  parameter,  not as redundant
parentheses around the identifier".  This is extended  to  cover  the
following cases:

typedef float T;
int noo(const (T[5]));
int moo(const (T(int)));
...

Where  again the '(' immediately to the left of 'T' is interpreted as
being the start of a parameter type list,  and  not  as  a  redundant
paren around a redeclaration of T.  Hence an equivalent code fragment
is:

typedef float T;
int noo(const int identifier1 (T identifier2 [5]));
int moo(const int identifier1 (T identifier2 (int identifier3)));
...

*/

%{

#ifdef flex
extern char * yytext;
#else
/* lex mylex */
extern char yytext[];
#endif

static char * stringliterallist;
%}

/* Define terminal tokens */


/* keywords */
%token AUTO            DOUBLE          INT             STRUCT
%token BREAK           ELSE            LONG            SWITCH
%token CASE            ENUM            REGISTER        TYPEDEF
%token CHAR            EXTERN          RETURN          UNION
%token CONST           FLOAT           SHORT           UNSIGNED
%token CONTINUE        FOR             SIGNED          VOID
%token DEFAULT         GOTO            SIZEOF          VOLATILE
%token DO              IF              STATIC          WHILE

/* ANSI Grammar suggestions */
%token IDENTIFIER              STRINGliteral
%token FLOATINGconstant        INTEGERconstant        CHARACTERconstant
%token OCTALconstant           HEXconstant

/* New Lexical element, whereas ANSI suggested non-terminal */

%token TYPEDEFname /* Lexer will tell the difference between this and
    an  identifier!   An  identifier  that is CURRENTLY in scope as a
    typedef name is provided to the parser as a TYPEDEFname.*/

/* Multi-Character operators */
%token  ARROW            /*    ->                              */
%token  ICR DECR         /*    ++      --                      */
%token  LS RS            /*    <<      >>                      */
%token  LE GE EQ NE      /*    <=      >=      ==      !=      */
%token  ANDAND OROR      /*    &&      ||                      */
%token  ELLIPSIS         /*    ...                             */

/* modifying assignment operators */
%token MULTassign  DIVassign    MODassign   /*   *=      /=      %=      */
%token PLUSassign  MINUSassign              /*   +=      -=              */
%token LSassign    RSassign                 /*   <<=     >>=             */
%token ANDassign   ERassign     ORassign    /*   &=      ^=      |=      */


%token SYNTAX_ERROR


/*************************************************************************/

%start external_definition

/*************************************************************************/

%%

_IDENTIFIER:
        IDENTIFIER { memo_identifier(yytext); }
        ;


/************************* CONSTANTS *************************/
constant:
        INTEGERconstant
  		{ memo_integer(yytext); }
        | CHARACTERconstant
  		{ memo_char(yytext); }
        ;

string_literal_list:
        STRINGliteral
		{ stringliterallist = Strdup(yytext); }
        | string_literal_list STRINGliteral
		{ 
		  char * s = Malloc(strlen(stringliterallist) +
				    strlen(yytext) - 1);
		  
		  strcpy(s, stringliterallist);
		  strcpy(s + strlen(s) - 1, yytext + 1);
		  
		  free(stringliterallist);
		  stringliterallist = s;
		}
                ;


/************************* EXPRESSIONS ********************************/
primary_expression:
        _IDENTIFIER  /* We cannot use a typedef name as a variable */
        | constant
        | string_literal_list
		{ memo_string(stringliterallist); free(stringliterallist); }
        | '(' comma_expression ')'
        ;

postfix_expression:
        primary_expression
        | postfix_expression '[' comma_expression ']'
  		{ memo_array_access(); }
        | postfix_expression '(' ')'
  		{ memo_funcall_0_arg(); }
        | postfix_expression _argument_expression_list
  		{ memo_funcall_nargs(); }
        | postfix_expression ICR
		{ memo_post_incr(); }
        | postfix_expression DECR
		{ memo_post_decr(); }
        ;


    /* pour faciliter le travail en rassemblant les arguments */
_argument_expression_list:
  	'(' assignment_expression ')' { mark_one_arg(); }
	|
  	'(' assignment_expression ',' { mark_first_arg(); }
	    argument_expression_list ')' { mark_some_args(); }
  	;

argument_expression_list:
        assignment_expression
        | argument_expression_list ',' assignment_expression
        ;

unary_expression:
        postfix_expression
        | ICR unary_expression			{ memo_pre_incr(); }
        | DECR unary_expression			{ memo_pre_decr(); }
        | unary_operator cast_expression	{ memo_unary_oper(); }
        | SIZEOF '(' type_name ')'		{ memo_sizeof_type();}
        ;

unary_operator:
        '&' 					{ memo_oper_ampersand(); }
        | '*' 					{ memo_oper_asterisk(); }
        | '+'					{ memo_oper_mono_plus(); }
        | '-'					{ memo_oper_mono_minus();}
        | '~'					{ memo_oper_complement();}
        | '!'					{ memo_oper_not(); }
        ;

cast_expression:
        unary_expression
        | '(' type_name ')' cast_expression	{ memo_cast(); }
        ;

multiplicative_expression:
        cast_expression
        | multiplicative_expression '*' cast_expression
		{ memo_binary_oper("*"); }
        | multiplicative_expression '/' cast_expression
		{ memo_binary_oper("/"); }
        | multiplicative_expression '%' cast_expression
		{ memo_binary_oper("%"); }
        ;

additive_expression:
        multiplicative_expression
        | additive_expression '+' multiplicative_expression
		{ memo_binary_oper("+"); }
        | additive_expression '-' multiplicative_expression
		{ memo_binary_oper("-"); }
        ;

shift_expression:
        additive_expression
        | shift_expression LS additive_expression
		{ memo_binary_oper("<<"); }
        | shift_expression RS additive_expression
		{ memo_binary_oper(">>"); }
        ;

relational_expression:
        shift_expression
        | relational_expression '<' shift_expression
		{ memo_binary_oper("<"); }
        | relational_expression '>' shift_expression
		{ memo_binary_oper(">"); }
        | relational_expression LE shift_expression
		{ memo_binary_oper("<="); }
        | relational_expression GE shift_expression
		{ memo_binary_oper(">="); }
        ;

equality_expression:
        relational_expression
        | equality_expression EQ relational_expression
		{ memo_binary_oper("=="); }
        | equality_expression NE relational_expression
		{ memo_binary_oper("!="); }
        ;

AND_expression:
        equality_expression
        | AND_expression '&' equality_expression
		{ memo_binary_oper("&"); }
        ;

exclusive_OR_expression:
        AND_expression
        | exclusive_OR_expression '^' AND_expression
		{ memo_binary_oper("^"); }
        ;

inclusive_OR_expression:
        exclusive_OR_expression
        | inclusive_OR_expression '|' exclusive_OR_expression
		{ memo_binary_oper("|"); }
        ;

logical_AND_expression:
        inclusive_OR_expression
        | logical_AND_expression ANDAND inclusive_OR_expression
		{ memo_binary_oper("&&"); }
        ;

logical_OR_expression:
        logical_AND_expression
        | logical_OR_expression OROR logical_AND_expression
		{ memo_binary_oper("||"); }
        ;

conditional_expression:
        logical_OR_expression
        | logical_OR_expression '?' comma_expression ':' conditional_expression
		{ memo_arith_if(); }
        ;

assignment_expression:
        conditional_expression
        | unary_expression _assignment_operator assignment_expression
  		{ memo_assignment(); }
        ;

_assignment_operator:
  	assignment_operator { memo_identifier(yytext); }
	;

assignment_operator:
        '='
        | MULTassign
        | DIVassign
        | MODassign
        | PLUSassign
        | MINUSassign
        | LSassign
        | RSassign
        | ANDassign
        | ERassign
        | ORassign
        ;

comma_expression:
        assignment_expression
        | comma_expression ',' assignment_expression
		{ memo_comma(); }
        ;

constant_expression:
        constant
  	| '+' constant
  	| '-' INTEGERconstant	{ memo_negate_integer(yytext); }
        ;

    /* The following was used for clarity */
comma_expression_opt:
        /* Nothing */
		{ memo_no_comma_expression(); }
        | comma_expression
        ;



/******************************* DECLARATIONS *********************************/

    /* The following is different from the ANSI C specified  grammar.
    The  changes  were  made  to  disambiguate  typedef's presence in
    declaration_specifiers (vs.  in the declarator for redefinition);
    to allow struct/union/enum tag declarations without  declarators,
    and  to  better  reflect the parsing of declarations (declarators
    must be combined with declaration_specifiers ASAP  so  that  they
    are visible in scope).

    Example  of  typedef  use  as either a declaration_specifier or a
    declarator:

      typedef int T;
      struct S { T T;}; /* redefinition of T as member name * /

    Example of legal and illegal statements detected by this grammar:

      int; /* syntax error: vacuous declaration * /
      struct S;  /* no error: tag is defined or elaborated * /

    Example of result of proper declaration binding:

        int a=sizeof(a); /* note that "a" is declared with a type  in
            the name space BEFORE parsing the initializer * /

        int b, c[sizeof(b)]; /* Note that the first declarator "b" is
             declared  with  a  type  BEFORE the second declarator is
             parsed * /

    */

decl_comma:
  	',' 	{ partial_declaration(); }
	;

declaration:
        declaring_list ';'
  		{ declaration(); }
        ;

    /* Note that if a typedef were  redeclared,  then  a  declaration
    specifier must be supplied */

declaring_list:
        type_specifier declarator initializer_opt
        | declaring_list decl_comma declarator initializer_opt
        ;

type_specifier:
        basic_type_specifier                 /* Arithmetic or void */
        ;


basic_type_specifier:
        _basic_type_name            /* Arithmetic or void */
        ;


_basic_type_name:
  	INT	{ memo_typedef(Type_Int); }
	| CHAR	{ memo_typedef(Type_Char); }
  	| VOID	{ memo_typedef(Type_Void); }
        ;

parameter_type_list:
        parameter_list
        | parameter_list ',' ELLIPSIS { memo_ellipsis(); }
        ;

parameter_list:
        parameter_declaration
        | parameter_list ',' parameter_declaration
        ;

parameter_declaration:
        type_specifier				{ ignored_param(); }
        | type_specifier abstract_declarator	{ ignored_param(); }
        | type_specifier identifier_declarator	{ typed_param(); }
        ;

    /*  ANSI  C  section  3.7.1  states  "An identifier declared as a
    typedef name shall not be redeclared as a parameter".  Hence  the
    following is based only on IDENTIFIERs */

identifier_list:
        _IDENTIFIER
        | identifier_list ',' _IDENTIFIER
        ;


type_name:
        type_specifier
        | type_specifier abstract_declarator
        ;

initializer_opt:
        /* nothing */		{ mark_no_initializer(); }
        | '=' initializer
        ;

begin_initializer_list:
  	'{'			{ mark_begin_initializer_list(); }
	;

end_initializer_list:
  	'}'			{ memo_initializer_list(); }
	;
initializer:
  	begin_initializer_list initializer_list end_initializer_list
        | begin_initializer_list initializer_list ',' end_initializer_list
        | assignment_expression
        ;

initializer_item:
  	begin_initializer_list initializer_list end_initializer_list
        | begin_initializer_list initializer_list ',' end_initializer_list
  	| constant_expression
  	| STRINGliteral { memo_string(yytext); }
  	;

initializer_list:
        initializer_item
        | initializer_list ',' initializer_item
        ;


/*************************** STATEMENTS *******************************/
statement:
        labeled_statement
        | compound_statement
        | expression_statement
        | selection_statement
        | iteration_statement
        | jump_statement
        ;

labeled_statement:
        CASE constant_expression ':' { memo_case(); } statement
        | DEFAULT 		 ':' { memo_default(); } statement
        ;

begin_compound_statement:
  	'{'	{ mark_begin_block(); }
	;

compound_statement:
        begin_compound_statement '}'
  		{ memo_empty_block(); }
        | begin_compound_statement declaration_list '}'
  		{ memo_block(); }
        | begin_compound_statement statement_list '}'
  		{ memo_block(); }
        | begin_compound_statement declaration_list statement_list '}'
  		{ memo_block(); }
        ;

declaration_list:
        declaration
        | declaration_list declaration
        ;

statement_list:
        statement
        | statement_list statement
        ;

expression_statement:
        comma_expression_opt ';'
        ;

selection_statement:
          IF '(' comma_expression ')' statement
		{ memo_if();}
        | IF '(' comma_expression ')' statement ELSE statement
		{ memo_if_else();}
        | SWITCH '(' comma_expression ')' statement
		{ memo_switch();}
        ;

iteration_statement:
        WHILE '(' comma_expression ')' statement
		{ memo_while(); }
        | DO statement WHILE '(' comma_expression ')' ';'
		{ memo_do_while(); }
        | FOR '(' comma_expression_opt ';' comma_expression_opt ';'
                comma_expression_opt ')' statement
		{ memo_for(); }
        ;

jump_statement:
        CONTINUE ';'   				{ memo_continue(); }
        | BREAK ';'   				{ memo_break(); }
        | RETURN comma_expression_opt ';'   	{ memo_return(); }
        ;


/***************************** EXTERNAL DEFINITIONS *****************************/

external_definition:
        function_definition	{ YYACCEPT; }
        | declaration		{ YYACCEPT; }

		/* L'evaluateur sera appele apres la construction du block */
	| compound_statement	{ YYACCEPT; }

	| 	/* fin */	{ if (*yytext)
				    yyerror(yytext);
				  YYABORT; }
        ;

function_definition:
                                identifier_declarator compound_statement
  		{ memo_make_function_def(); }
        | type_specifier        identifier_declarator compound_statement
  		{ memo_make_typedef_function_def(); }

        |                       old_function_declarator compound_statement
  		{ memo_make_oldfunction_def_without_decllist(); }
        | type_specifier        old_function_declarator compound_statement
  		{ memo_make_typedef_oldfunction_def_without_decllist(); }

        |                       old_function_declarator old_declaration_list
		compound_statement
  		{ memo_make_oldfunction_def(); }
        | type_specifier        old_function_declarator old_declaration_list
                compound_statement
  		{ memo_make_typedef_oldfunction_def(); }
        ;

old_declaration_list:	/* comme declaration_list mais sans initializer_opt */
  	old_declaration
  	| old_declaration_list old_declaration
  	;

old_declaration:
	old_declaring_list ';'
		{ mark_no_initializer(); declaration(); }
	;

old_declaring_list:	/* comme declaring_list mais sans initializer_opt */
        type_specifier declarator
        | old_declaring_list { mark_no_initializer(); } decl_comma declarator
        ;

  
declarator:
        identifier_declarator
        ;

    /*  The  following have at least one '*'. There is no (redundant)
    '(' between the '*' and the TYPEDEFname. */

asterisk:
  	'*' { mark_asterisk(); }
	;
  
identifier_declarator:
        unary_identifier_declarator
        | paren_identifier_declarator
	;

unary_identifier_declarator:
        postfix_identifier_declarator
        | asterisk identifier_declarator
        ;

begin_of_list:
  	'('		{ mark_begin_of_list(); }
  	;

end_of_list:
  	')'		{ end_of_list(); }

forget_list:
  	')'		{ forget_list(); }

forget_list_if_possible:
  	')'		{ forget_list_if_possible(); }

postfix_identifier_declarator:
        paren_identifier_declarator postfixing_abstract_declarator
  		/* array or function decl */
        | begin_of_list unary_identifier_declarator forget_list
        | begin_of_list unary_identifier_declarator forget_list_if_possible
  		postfixing_abstract_declarator
        ;

paren_identifier_declarator:
        _IDENTIFIER
        | begin_of_list paren_identifier_declarator forget_list
  		/* on peut oublier les () */
        ;

old_function_declarator:
        postfix_old_function_declarator
        | asterisk old_function_declarator
        ;

postfix_old_function_declarator:
        paren_identifier_declarator
  	  '(' { mark_begin_identifier_list(); }
	      identifier_list
	  ')' { end_identifier_list(); }
        | begin_of_list old_function_declarator forget_list
        | begin_of_list old_function_declarator forget_list_if_possible
  		postfixing_abstract_declarator
        ;

abstract_declarator:
        unary_abstract_declarator
        | postfix_abstract_declarator
        | postfixing_abstract_declarator
        ;

postfixing_abstract_declarator:
        array_abstract_declarator
  				{ array_abstract_declarator(); }
        | begin_of_list forget_list
				{ memo_empty_function_parameter_type_list(); }
        | begin_of_list { mark_begin_function_parameter_type_list(); }
	     parameter_type_list forget_list
				{ end_function_parameter_type_list(); }
        ;

array_abstract_declarator:
        '[' ']'
  		{ memo_array_decl_without_dim(); }
        | '[' constant_expression ']'
  		{ memo_array_decl_dim(); }
        | array_abstract_declarator '[' constant_expression ']'
  		{ memo_array_decl_dim(); }
        ;

unary_abstract_declarator:
        asterisk
        | asterisk abstract_declarator
        ;

postfix_abstract_declarator:
        begin_of_list unary_abstract_declarator forget_list
	  	/* ex) (int(*)) = (int *) = type pointeur d'int   */
        | begin_of_list postfix_abstract_declarator forget_list
        | begin_of_list postfixing_abstract_declarator forget_list
  		/* ex) (int ([2])) ou (int(())) */
        | begin_of_list unary_abstract_declarator end_of_list
  		postfixing_abstract_declarator
  		/* ex) (int(*)[1]) = type pointeur sur un vect d'int
		       (int(*)()) = type pointeur sur func retournant int */
        ;

%%
/* ----end of grammar----*/

#include <stdio.h>

#include "decl.h"

int numlig;
char * yyinfilename = 0;
extern char err_msg[];

yyerror(string)
     char *string;
{
  /* Attention, la forme du message est connue dans smacXcoral,c */

  if (! *err_msg)
    /* Pas une erreur (f)lex */
    if (yyinfilename)
      sprintf(err_msg, "parser error in %s line %d : %s",
	      yyinfilename, numlig, string);
    else
      sprintf(err_msg, "parser error line %d : %s", numlig, string);
}

