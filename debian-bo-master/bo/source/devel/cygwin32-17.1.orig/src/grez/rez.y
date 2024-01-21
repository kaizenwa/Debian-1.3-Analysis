/* Parse Rez-syntax expressions.
   Copyright 1994, 1995 Free Software Foundation.

This file is part of GNU Rez.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This was inspired by a Perl-generating grammar by Brad Pickering,
   with a lexical analyzer derived from c-lex.c of GCC.  */

%{
#include "grez.h"

#ifdef CPPLIB
#include "cpplib.h"
#endif

static int readescape PARAMS ((int *ignore_ptr));

#define ishexdigit(c) (isdigit(c) || ((c) >= 'a' && (c) <= 'f') || ((c) >= 'A' && (c) <= 'F'))

#ifdef YYDEBUG
#define yydebug_fprintf(STR,X) if (yydebug) fprintf (stderr, STR, X)
#else
#define yydebug_fprintf(STR,X) 
#endif

int yylex ();
void yyerror ();

int expression_value;

static jmp_buf parse_return_error;

int attribute_bits = 0;

int lineno;

int end_of_file;

#define BITS_PER_UNIT 8
#define BITS_PER_WORD 16
#define HOST_BITS_PER_INT 32
#define HOST_BITS_PER_LONG 32

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE INT_TYPE_SIZE
#endif

/* Yield nonzero if adding two numbers with A's and B's signs can yield a
   number with SUM's sign, where A, B, and SUM are all C integers.  */
#define possible_sum_sign(a, b, sum) ((((a) ^ (b)) | ~ ((a) ^ (sum))) < 0)

#define ERROR (-1)

static void integer_overflow ();
static long left_shift ();
static long right_shift ();

extern int show_progress;

extern FILE *current_input_file;

struct expr *create_number PARAMS ((int num));
struct expr *create_string PARAMS ((char *str));
struct expr *create_symbol PARAMS ((char *str));

struct expr *create_unary_op PARAMS ((int op, struct expr *arg1));
struct expr *create_binary_op PARAMS ((int op, struct expr *arg1, struct expr *arg2));

char *eval_str_expression PARAMS ((struct expr *));

struct rspec *create_rspec_from_type PARAMS ((struct expr *type));

void create_new_type PARAMS ((struct rspec *, struct rtype_node *));
void clone_type PARAMS ((struct rspec *, struct rspec *));

struct rtype_node *create_node PARAMS ((enum rtype_code));
struct rtype_node *create_symbolic PARAMS ((struct expr *sym, struct expr *val));
struct rtype_node *node_reverse PARAMS ((struct rtype_node *));

char *copy_string PARAMS ((char *str));

void drs PARAMS ((struct rspec *));
void drs1 PARAMS ((struct rspec *));
void dex PARAMS ((struct expr *));
void dt PARAMS ((struct rtype_node *));
void dt1 PARAMS ((struct rtype_node *));

%}

%start rez_file

%union {
  long num;
  char *str;
  struct rspec *rspec;
  struct expr *expr;
  struct rtype_node *typenode;
}

/* Types of statements.  */

%token CHANGE DATA DELETE INCLUDE READ RESOURCE TYPE

%token AS NOT TO

/* Names of resource attributes.  */

%token SYSHEAP APPHEAP
%token PURGEABLE NONPURGEABLE
%token LOCKED UNLOCKED
%token PRELOAD NONPRELOAD
%token PROTECTED UNPROTECTED

/* Names of types.  */

%token BOOLEAN
%token CHAR
%token STRING PSTRING CSTRING WSTRING
%token POINT
%token RECT
%token UNSIGNED
%token BINARY OCTAL DECIMAL HEX LITERAL
%token BIT NIBBLE BYTE WORD LONG INT INTEGER LONGINT BITSTRING

%token FILL
%token ALIGN
%token WIDE ARRAY
%token SWITCH CASE KEY

%token IDENTIFIER DD_VARIABLE DD_FUNCTION

%token INTEGER_CONSTANT LITERAL_CONSTANT
%token STRING_CONSTANT HEX_STRING_CONSTANT

%token ENDFILE

%left <num> OROR
%left <num> ANDAND
%left <num> '|'
%left <num> '^'
%left <num> '&'
%left <num> EQUAL NOTEQUAL
%left <num> '<' '>' LEQ GEQ
%left <num> LSH RSH
%left <num> '+' '-'
%left <num> '*' '/' '%'
%right <num> '~' '!' /* '-' */

%type <num> INTEGER_CONSTANT LITERAL_CONSTANT

%type <num> optional_unsigned optional_radix
%type <num> fill_size align_size

%type <str> STRING_CONSTANT HEX_STRING_CONSTANT
%type <str> IDENTIFIER

%type <rspec> existing_resource_specifier new_resource_specifier
%type <rspec> resource_type

%type <expr> resource_id resource_attributes resource_attribute
%type <expr> declaration_constant
%type <expr> symbolic_name
%type <expr> point_constant rect_constant string
%type <expr> optional_string_length
%type <expr> fill_length
%type <expr> expression unary_expression primary_expression
%type <expr> function_arg_list

%type <typenode> label_or_decl_list data_declaration
%type <typenode> boolean_type boolean_type_name boolean_symbolic_list boolean_symbolic
%type <typenode> numeric_type numeric_type_name numeric_symbolic_list numeric_symbolic
%type <typenode> char_type char_type_name char_symbolic_list char_symbolic
%type <typenode> string_type string_type_name string_symbolic_list string_symbolic
%type <typenode> point_type point_type_name point_symbolic_list point_symbolic
%type <typenode> rect_type rect_type_name rect_symbolic_list rect_symbolic
%type <typenode> array_type array_type_name
%type <typenode> switch_type case_list data_declaration_list key_type
%type <typenode> fill_type
%type <typenode> align_type

%%

rez_file:
	  statement_list ENDFILE
	;

statement_list:
	  /* empty */
	| statement_list statement
	;

statement:
	  change_statement
	| data_statement
	| delete_statement
	| include_statement
	| read_statement
	| resource_statement
	| type_statement
	;

/* Various ways to refer to one or more resources. */

existing_resource_specifier:
	  resource_type '(' string ')'
		{ $$ = $1;
		  strncpy ($$->name, $3->fields[0].str, 256); }
	| resource_type '(' resource_id ')'
		{ $$ = $1;
		  $$->id = $3->fields[0].num;
		  $$->id_given = 1; }
	| resource_type '(' resource_id ':' resource_id ')'
		{ $$ = $1;
		  $$->id = $3->fields[0].num;
		  $$->id_given = 1;
		  $$->id2 = $5->fields[0].num; }
 	;

new_resource_specifier:
	  resource_type '(' resource_id resource_options ')'
		{ $$ = $1;
		  $$->id = $3->fields[0].num;
		  $$->id_given = 1; }
	;

resource_type:
	  expression
		{ $$ = create_rspec_from_type ($1); }
	;

resource_id:
	  expression
		{ $$ = $1; }
	;

resource_options:
	  /* empty */
	| ',' string
	| ',' resource_attributes
	| ',' string ',' resource_attributes
	;

resource_attributes:
	  expression
	| resource_attribute
	| resource_attributes '|' resource_attribute

resource_attribute:
	  APPHEAP
		{ attribute_bits = attribute_bits & ~64; }
	| SYSHEAP
		{ attribute_bits = attribute_bits |  64; }
	| NONPURGEABLE
		{ attribute_bits = attribute_bits & ~32; }
	| PURGEABLE
		{ attribute_bits = attribute_bits |  32; }
	| UNLOCKED
		{ attribute_bits = attribute_bits & ~16; }
	| LOCKED
		{ attribute_bits = attribute_bits |  16; }
	| UNPROTECTED
		{ attribute_bits = attribute_bits & ~8; }
	| PROTECTED
		{ attribute_bits = attribute_bits |  8; }
	| NONPRELOAD
		{ attribute_bits = attribute_bits & ~4; }
	| PRELOAD
		{ attribute_bits = attribute_bits |  4; }
	;

/* Definitions of Rez statements. */

change_statement:
	  CHANGE resource_type TO new_resource_specifier ';'
		{ if (show_progress) printf("### Change\n"); }
	| CHANGE existing_resource_specifier TO new_resource_specifier ';'
		{ if (show_progress) printf("### Change\n"); }
	;

data_statement:
	 DATA new_resource_specifier
		{ create_new_resource ("Data", $2, 0); }
	   '{' data_string_list '}' ';'
		{ close_current_resource ("Data"); }
	;

data_string_list:
	  /* empty */
	| data_string_list HEX_STRING_CONSTANT
		{ append_hex_string_to_resource (current_resource, $2, 0, NULL); }
	| data_string_list STRING_CONSTANT
		{ append_string_to_resource (current_resource, $2, 0, NULL); }
	;

delete_statement:
	  DELETE resource_type ';'
		{ if (show_progress) printf("### Delete\n"); }
	| DELETE existing_resource_specifier ';'
		{ if (show_progress) printf("### Delete\n"); }
	;

include_statement:
	  INCLUDE STRING_CONSTANT ';'
		{ if (show_progress) printf("### Include\n"); }
	| INCLUDE STRING_CONSTANT resource_type ';'
		{ if (show_progress) printf("### Include\n"); }
	| INCLUDE STRING_CONSTANT existing_resource_specifier ';'
		{ if (show_progress) printf("### Include\n"); }
	| INCLUDE STRING_CONSTANT NOT resource_type /* or existing_resource_specifier? */ ';'
		{ if (show_progress) printf("### Include\n"); }
	| INCLUDE STRING_CONSTANT resource_type AS resource_type ';'
		{ if (show_progress) printf("### Include\n"); }
	| INCLUDE STRING_CONSTANT existing_resource_specifier AS new_resource_specifier ';'
		{ if (show_progress) printf("### Include\n"); }
	;

read_statement:
	  READ new_resource_specifier STRING_CONSTANT ';'
		{ read_resource_from_file ($2, $3); }
	;

resource_statement:
	  RESOURCE new_resource_specifier
		{ create_new_resource ("Resource", $2, 1); }
	    '{'
		{ field_stack_top = 0;
		  field_stack[0] = current_type->sub;
		  if (field_stack[0]->code == rt_nothing)
		    field_stack[0] = field_stack[0]->next; }
	    data_definition_list optional_comma '}' ';'
		{ close_current_resource (); }
	;

data_definition_list:
	  data_definition
	| data_definition_list ',' data_definition
	;

data_definition:
	  expression
		{ append_number_to_resource (current_resource,
					     eval_expression ($1),
					     field_stack[field_stack_top]->size);
		  field_stack[field_stack_top] = field_stack[field_stack_top]->next; }
	| resource_string_list
		{  }
	| '{' '}'
		{  }
	| '{' data_definition_list optional_comma '}'
		{  }
	| IDENTIFIER '{' '}'
		{  }
	| IDENTIFIER '{' data_definition_list optional_comma '}'
		{  }
	;

/* This rule looks ugly, but allowing an empty resource string list causes a
   reduce/reduce conflict with optional comma handling.  */

resource_string_list:
	  HEX_STRING_CONSTANT
		{ append_hex_string_to_resource (current_resource,
					     $1,
					     field_stack[field_stack_top]->subcode,
					     field_stack[field_stack_top]->length_expr);
		  field_stack[field_stack_top] = field_stack[field_stack_top]->next; }
	| STRING_CONSTANT
		{ append_string_to_resource (current_resource,
					     $1,
					     field_stack[field_stack_top]->subcode,
					     field_stack[field_stack_top]->length_expr);
		  field_stack[field_stack_top] = field_stack[field_stack_top]->next; }
	| resource_string_list HEX_STRING_CONSTANT
		{ append_hex_string_to_resource (current_resource,
					     $2,
					     field_stack[field_stack_top]->subcode,
					     field_stack[field_stack_top]->length_expr);
		  field_stack[field_stack_top] = field_stack[field_stack_top]->next; }
	| resource_string_list STRING_CONSTANT
		{ append_string_to_resource (current_resource,
					     $2,
					     field_stack[field_stack_top]->subcode,
					     field_stack[field_stack_top]->length_expr);
		  field_stack[field_stack_top] = field_stack[field_stack_top]->next; }
	;

optional_comma:
	  /* empty */
	| ','
	;

type_statement:
	  TYPE resource_type '{' label_or_decl_list '}' ';'
		{ create_new_type ($2, node_reverse ($4)); }
	| TYPE existing_resource_specifier '{' label_or_decl_list '}' ';'
		{ create_new_type ($2, node_reverse ($4)); }
	| TYPE new_resource_specifier AS existing_resource_specifier ';'
		{ clone_type ($2, $4); }
	;

label_or_decl_list:
	  /* empty */
		{ $$ = create_node (rt_nothing); }
	| label_or_decl_list optional_unsigned optional_radix data_declaration
		{ $$ = $4;
		  $$->next = $1; }
	| label_or_decl_list IDENTIFIER ':'
		{ $$ = create_node (rts_label);
		  $$->symbol = create_symbol ($2);
		  $$->next = $1; }
	;

optional_unsigned:
	  /* empty */
		{ $$ = 1; }
	| UNSIGNED
		{ $$ = 0; }
	;

optional_radix:
	  /* empty */
		{ $$ = 10; }
	| BINARY
		{ $$ = 2; }
	| OCTAL
		{ $$ = 8; }
	| DECIMAL
		{ $$ = 10; }
	| HEX
		{ $$ = 16; }
	| LITERAL
		{ $$ = 256; }
	;
	
data_declaration:
	  boolean_type
	| numeric_type
	| char_type
	| string_type
	| point_type
	| rect_type
	| array_type
	| switch_type
	| fill_type
	| align_type
	;

boolean_type:
	  boolean_type_name ';'
		{ $$ = $1; }
	| boolean_type_name boolean_symbolic_list ';'
		{ $$ = $1;
		  $$->symbolics = node_reverse ($2); }
	| boolean_type_name '=' expression ';'
		{ $$ = $1;
		  $$->const_expr = $3; }
	;

boolean_type_name:
	  BOOLEAN
		{ $$ = create_node (rtf_boolean); }
	;

boolean_symbolic_list:
	  boolean_symbolic
		{ $$ = $1; }
	| boolean_symbolic_list ',' boolean_symbolic
		{ $$ = $3;
		  $$->next = $1; }
	;

boolean_symbolic:
	  symbolic_name
		{ $$ = create_symbolic ($1, NULL); }
	| symbolic_name '=' expression
		{ $$ = create_symbolic ($1, $3); }
	;

symbolic_name:
	  IDENTIFIER
		{ $$ = create_symbol ($1); }
	;

numeric_type:
	  numeric_type_name ';'
		{ $$ = $1; }
	| numeric_type_name numeric_symbolic_list ';'
		{ $$ = $1;
		  $$->symbolics = node_reverse ($2); }
	| numeric_type_name '=' expression ';'
		{ $$ = $1;
		  $$->const_expr = $3; }
	;

numeric_type_name:
	  BYTE
		{ $$ = create_node (rtf_numeric);
		  $$->size = 8; }
	| INTEGER
		{ $$ = create_node (rtf_numeric);
		  $$->size = 16; }
	| INT
		{ $$ = create_node (rtf_numeric);
		  $$->size = 16; }
	| LONGINT
		{ $$ = create_node (rtf_numeric);
		  $$->size = 32; }
	| BITSTRING '[' expression ']'
		{ $$ = create_node (rtf_numeric);
		  $$->size = 1;
		  $$->length_expr = $3; }
	;

numeric_symbolic_list:
	  numeric_symbolic
		{ $$ = $1; }
	| numeric_symbolic_list ',' numeric_symbolic
		{ $$ = $3;
		  $$->next = $1; }
	;

numeric_symbolic:
	  symbolic_name
		{ $$ = create_symbolic ($1, NULL); }
	| symbolic_name '=' expression
		{ $$ = create_symbolic ($1, $3); }
	;

char_type:
	  char_type_name ';'
		{ $$ = $1; }
	| char_type_name char_symbolic_list ';'
		{ $$ = $1;
		  $$->symbolics = node_reverse ($2); }
	| char_type_name '=' string ';'
		{ $$ = $1;
		  $$->const_expr = $3; }
	;

char_type_name:
	  CHAR
		{ $$ = create_node (rtf_char); }
	;

char_symbolic_list:
	  char_symbolic
		{ $$ = $1; }
	| char_symbolic_list ',' char_symbolic
		{ $$ = $3;
		  $$->next = $1; }
	;

char_symbolic:
	  symbolic_name
		{ $$ = create_symbolic ($1, NULL); }
	| symbolic_name '=' string
		{ $$ = create_symbolic ($1, $3); }
	;

string_type:
	  string_type_name optional_string_length ';'
		{ $$ = $1;
		  $$->length_expr = $2; }
	| string_type_name optional_string_length string_symbolic_list ';'
		{ $$ = $1;
		  $$->length_expr = $2;
		  $$->symbolics = node_reverse ($3); }
	| string_type_name optional_string_length '=' string ';'
		{ $$ = $1;
		  $$->length_expr = $2;
		  $$->const_expr = $4; }
	;

string_type_name:
	  STRING
		{ $$ = create_node (rtf_string);
		  $$->subcode = 0; }
	| PSTRING
		{ $$ = create_node (rtf_string);
		  $$->subcode = 1; }
	| WSTRING
		{ $$ = create_node (rtf_string);
		  $$->subcode = 2; }
	| CSTRING
		{ $$ = create_node (rtf_string);
		  $$->subcode = 3; }
	;

optional_string_length:
	  /* empty */
		{ $$ = create_number (-1); }
	| '[' expression ']'
		{ $$ = $2; }
	;

string_symbolic_list:
	  string_symbolic
		{ $$ = $1; }
	| string_symbolic_list ',' string_symbolic
		{ $$ = $3;
		  $$->next = $1; }
	;

string_symbolic:
	  symbolic_name
		{ $$ = create_symbolic ($1, NULL); }
	| symbolic_name '=' string
		{ $$ = create_symbolic ($1, $3); }
	;

point_type:
	  point_type_name ';'
		{ $$ = $1; }
	| point_type_name point_symbolic_list ';'
		{ $$ = $1;
		  $$->symbolics = node_reverse ($2); }
	| point_type_name '=' point_constant ';'
		{ $$ = $1;
		  $$->const_expr = $3; }
	;

point_type_name:
	  POINT
		{ $$ = create_node (rtf_point); }
	;

point_symbolic_list:
	  point_symbolic
		{ $$ = $1; }
	| point_symbolic_list ',' point_symbolic
		{ $$ = $3;
		  $$->next = $1; }
	;

point_symbolic:
	  symbolic_name
		{ $$ = create_symbolic ($1, NULL); }
	| symbolic_name '=' point_constant
		{ $$ = create_symbolic ($1, $3); }
	;

rect_type:
	  rect_type_name ';'
		{ $$ = $1; }
	| rect_type_name rect_symbolic_list ';'
		{ $$ = $1;
		  $$->symbolics = node_reverse ($2); }
	| rect_type_name '=' rect_constant ';'
		{ $$ = $1;
		  $$->const_expr = $3; }
	;

rect_type_name:
	  RECT
		{ $$ = create_node (rtf_rect); }
	;

rect_symbolic_list:
	  rect_symbolic
		{ $$ = $1; }
	| rect_symbolic_list ',' rect_symbolic
		{ $$ = $3;
		  $$->next = $1; }
	;

rect_symbolic:
	  symbolic_name
		{ $$ = create_symbolic ($1, NULL); }
	| symbolic_name '=' rect_constant
		{ $$ = create_symbolic ($1, $3); }
	;

array_type:
	  array_type_name '{' label_or_decl_list '}' ';'
		{ $$ = $1;
		  $$->sub = node_reverse ($3); }
	| array_type_name IDENTIFIER '{' label_or_decl_list '}' ';'
		{ $$ = $1;
		  $$->symbol = create_symbol ($2);
		  $$->sub = node_reverse ($4); }
	| array_type_name '[' expression ']' '{' label_or_decl_list '}' ';'
		{ $$ = $1;
		  $$->length_expr = $3;
		  $$->sub = node_reverse ($6); }
	;

array_type_name:
	  ARRAY
		{ $$ = create_node (rtf_array); }
	| WIDE ARRAY
		{ $$ = create_node (rtf_array); }
	;

switch_type:
	  SWITCH '{' case_list '}' ';'
		{ $$ = create_node (rtf_switch);
		  $$->sub = node_reverse ($3); }
	;

case_list:
	  /* empty */
		{ $$ = create_node (rt_nothing); }
	| case_list
	  CASE IDENTIFIER ':'
	  data_declaration_list
	  KEY key_type '=' declaration_constant ';'
	  data_declaration_list
		{ $$ = $5;
		  $$->next = $1; }
	;

data_declaration_list:
	  /* empty */
		{ $$ = create_node (rt_nothing); }
	| data_declaration_list optional_unsigned optional_radix data_declaration
		{ $$ = $4;
		  $$->next = $1; }
	;

key_type:
	  boolean_type_name
	| numeric_type_name
	| char_type_name
	| point_type_name
	| rect_type_name
	| string_type_name optional_string_length
	;

declaration_constant:
	  expression
	| point_constant
	| rect_constant
	| string
	;

fill_type:
	  FILL fill_size fill_length ';'
		{ $$ = create_node (rtf_fill);
		  $$->size = $2;
		  $$->length_expr = $3; }
	;

fill_size:
	  BIT
		{ $$ = 1; }
	| NIBBLE
		{ $$ = 4; }
	| BYTE
		{ $$ = 8; }
	| WORD
		{ $$ = 16; }
	| LONG
		{ $$ = 32; }
	;

fill_length:
	  /* empty */
		{ $$ = create_number (1); }
	| '[' expression ']'
		{ $$ = $2; }
	;

align_type:
	  ALIGN align_size ';'
		{ $$ = create_node (rtf_align);
		  $$->size = $2; }
	;

align_size:
	  NIBBLE
		{ $$ = 4; }
	| BYTE
		{ $$ = 8; }
	| WORD
		{ $$ = 16; }
	| LONG
		{ $$ = 32; }
	;

expression:
	  unary_expression
	| expression '*' expression
		{ $$ = create_binary_op (e_mul, $1, $3); }
	| expression '/' expression
		{ $$ = create_binary_op (e_div, $1, $3); }
	| expression '%' expression
		{ $$ = create_binary_op (e_mod, $1, $3); }
	| expression '+' expression
		{ $$ = create_binary_op (e_add, $1, $3); }
	| expression '-' expression
		{ $$ = create_binary_op (e_sub, $1, $3); }
	| expression LSH expression
		{ $$ = create_binary_op (e_lsh, $1, $3); }
	| expression RSH expression
		{ $$ = create_binary_op (e_rsh, $1, $3); }
	| expression '<' expression
		{ $$ = create_binary_op (e_lt,  $1, $3); }
	| expression '>' expression
		{ $$ = create_binary_op (e_gt,  $1, $3); }
	| expression LEQ expression
		{ $$ = create_binary_op (e_leq, $1, $3); }
	| expression GEQ expression
		{ $$ = create_binary_op (e_geq, $1, $3); }
	| expression EQUAL expression
		{ $$ = create_binary_op (e_eq,  $1, $3); }
	| expression NOTEQUAL expression
		{ $$ = create_binary_op (e_ne,  $1, $3); }
	| expression '&' expression
		{ $$ = create_binary_op (e_and, $1, $3); }
	| expression '^' expression
		{ $$ = create_binary_op (e_xor, $1, $3); }
	| expression '|' expression
		{ $$ = create_binary_op (e_or,  $1, $3); }
	| expression ANDAND expression
		{ $$ = create_binary_op (e_aan, $1, $3); }
	| expression OROR expression
		{ $$ = create_binary_op (e_oor, $1, $3); }
	;

unary_expression:
	  primary_expression
		{ $$ = $1; }
	| '-' expression
		{ $$ = create_unary_op (e_neg, $2); }
	| '!' expression
		{ $$ = create_unary_op (e_not, $2); }
	| '~' expression
		{ $$ = create_unary_op (e_cmp, $2); }
	;

primary_expression:
	  INTEGER_CONSTANT
		{ $$ = create_number ($1); }
	| LITERAL_CONSTANT
		{ $$ = create_number ($1); }
	| IDENTIFIER '[' expression ']'
		{ $$ = create_symbol ($1); }
	| IDENTIFIER
		{ $$ = create_symbol ($1); }
	| DD_VARIABLE
		{ $$ = NULL; }
	| DD_FUNCTION '(' function_arg_list ')'
		{ $$ = NULL; }
	| '(' expression ')'
		{ $$ = $2; }
	| '(' error ')'
		{ $$ = NULL; }
	;
	  
function_arg_list:
	  expression
	| function_arg_list ',' expression
	;

string:
	  STRING_CONSTANT
		{ $$ = create_string ($1); }
	| HEX_STRING_CONSTANT
		{ $$ = create_string ($1); }
	| string STRING_CONSTANT
		{ $$ = $1; }
	| string HEX_STRING_CONSTANT
		{ $$ = $1; }
	;
	
point_constant:
	  '{' expression ',' expression '}'
		{ $$ = NULL; }
	;

rect_constant:
	  '{' expression ',' expression ',' expression ',' expression '}'
		{ $$ = NULL; }
	;

%%

struct token {
  char *name;
  int token;
};

static struct token reserved_words[] = {
  {"align", ALIGN},
  {"appheap", APPHEAP},
  {"array", ARRAY},
  {"as", AS},
  {"binary", BINARY},
  {"bit", BIT},
  {"bitstring", BITSTRING},
  {"boolean", BOOLEAN},
  {"byte", BYTE},
  {"case", CASE},
  {"change", CHANGE},
  {"char", CHAR},
  {"cstring", CSTRING},
  {"data", DATA},
  {"decimal", DECIMAL},
  {"delete", DELETE},
  {"fill", FILL},
  {"hex", HEX},
  {"include", INCLUDE},
  {"int", INT},
  {"integer", INTEGER},
  {"key", KEY},
  {"literal", LITERAL},
  {"locked", LOCKED},
  {"long", LONG},
  {"longint", LONGINT},
  {"nibble", NIBBLE},
  {"nonpreload", NONPRELOAD},
  {"nonpurgeable", NONPURGEABLE},
  {"not", NOT},
  {"octal", OCTAL},
  {"point", POINT},
  {"preload", PRELOAD},
  {"protected", PROTECTED},
  {"pstring", PSTRING},
  {"purgeable", PURGEABLE},
  {"read", READ},
  {"rect", RECT},
  {"resource", RESOURCE},
  {"string", STRING},
  {"switch", SWITCH},
  {"sysheap", SYSHEAP},
  {"to", TO},
  {"type", TYPE},
  {"unlocked", UNLOCKED},
  {"unprotected", UNPROTECTED},
  {"unsigned", UNSIGNED},
  {"wide", WIDE},
  {"word", WORD},
  {"wstring", WSTRING},
  {NULL, ERROR}
};

struct builtin {
  char *name;
  int (*fn)();
};

static struct builtin builtin_vars[] = {
  {"attributes", NULL},
  {"date", NULL},
  {"day", NULL},
  {"hour", NULL},
  {"id", NULL},
  {"minute", NULL},
  {"name", NULL},
  {"resourcesize", NULL},
  {"second", NULL},
  {"time", NULL},
  {"type", NULL},
  {"version", NULL},
  {"weekday", NULL},
  {"year", NULL},
  {NULL, NULL}
};

static struct builtin builtin_funs[] = {
  {"arrayindex", NULL},
  {"bitfield", NULL},
  {"byte", NULL},
  {"countof", NULL},
  {"format", NULL},
  {"long", NULL},
  {"packedsize", NULL},
  {"resource", NULL},
  {"shell", NULL},
  {"word", NULL},
  {NULL, NULL}
};

/* Buffered-back input character; faster than using ungetc.  */

static int nextchar;

#ifdef CPPLIB
struct parse_file parse_in;
struct cpp_options options;
#endif

void
init_lex ()
{
#ifdef CPPLIB
  char *p;
  struct cpp_options *opts = &options;

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && p[-1] != '/') --p;
  progname = p;

  init_parse_file (&parse_in);
  parse_in.data = opts;

  init_parse_options (opts);
  handle_parse_options (&parse_in, argc - 1 , argv + 1);

  push_parse_file (&parse_in, opts->in_fname);
#else
  lineno = 0;
  ungetc (check_newline (), current_input_file);
  nextchar = -1;
#endif /* CPPLIB */
}

int
get_one_char ()
{
  int c = fgetc (current_input_file);

#ifdef MPW
  SpinCursor (1);
#endif
/*  fprintf(stderr, "-- '%c' (0x%x)\n", c, c); */
  return c;
}

/* If C is not whitespace, return C.
   Otherwise skip whitespace and return first nonwhite char read.  */

/* (should reindent) */
static int
skip_white_space (c)
     register int c;
{
  int c2;
  
  for (;;)
    {
      switch (c)
	{

	case '/':
	  c2 = get_one_char ();
	  if (c2 == '*')
	    {
	      c = get_one_char ();
	      if (c == '\n')
		lineno++;
	      while (c != EOF)
	        {
		  if (c == '*')
		    {
	              c = get_one_char ();
	     	      if (c == '\n')
			lineno++;
		      if (c == EOF || c == '/')
		        break;
		    }
		  else
		    c = get_one_char ();
		  if (c == '\n')
		    lineno++;
		}
	      c = get_one_char ();
	      if (c == '\n')
		lineno++;
	      break;
	    }
	  else
	    {
	      ungetc (c2, current_input_file);
	      return c;
	    }

	case '\n':
	  c = check_newline ();
	  break;

	case ' ':
	case '\t':
	case '\f':
	case '\v':
	case '\b':
	case '\r':
	  /* For some reason, these are laying around in existing Rez files */
	case 0x1:
	  c = get_one_char ();
	  break;

	case '\\':
	  c = get_one_char ();
	  if (c == '\n')
	    lineno++;
	  else
	    error ("stray '\\' in program");
	  c = get_one_char ();
	  break;

	default:
	  return c;
	}
    }
}

/* At the beginning of a line, increment the line number and process
   any #-directive on this line.  If the line is a #-directive, read
   the entire line and return a newline.  Otherwise, return the line's
   first non-whitespace character.  */

int
check_newline ()
{
  register int c;

  lineno++;

  /* Read first nonwhite char on the line.  */

  c = get_one_char ();
  while (c == ' ' || c == '\t')
    c = get_one_char ();

  /* If not #, return it so caller will use it.  */

  if (c != '#')
    return c;

  return handle_cpp_directive ();
}

int
handle_cpp_directive ()
{
  register int c;
  char *p, linebuf[5000];

  /* Read first nonwhite char after the `#'.  */

  c = get_one_char ();
  while (c == ' ' || c == '\t')
    c = get_one_char ();

  if (c == '\n')
    return c;

  /* Read in the rest of this line.  */

  p = linebuf;
  *p++ = c;

  while ((c = get_one_char ()) != EOF
         && (c != '\n' || *(p - 1) == '\\')
	 && (p - linebuf < 5000))
    if (c == '\n')
      {
      	p--;
	lineno++;
      }
    else
      *p++ = c;

  *p = '\0';

  fprintf (stderr, "Saw \"%s\" directive\n", linebuf);

  return c;
}

/* Read one token.  */

#ifdef CPPLIB

int
yylex ()
{
  enum cpp_token kind;
  int c, i;
  int wide_flag;
  int value = -1;
  char *p, token_buffer[256];

 tryagain:
  parse_in.cur = parse_in.token_buffer;
  kind = cpp_get_token (&parse_in);

  switch (kind)
    {
    case EOF_TOKEN:
      end_of_file = 1;
      value = ENDFILE;
      break;

    case '\'':
    char_constant:
      {
	register int result = 0;
	register int num_chars = 0;
	unsigned width = 8;
	int max_chars;

	max_chars = 4;

	while (1)
	  {
	  try_again:

	    c = get_one_char ();

	    if (c == '\'' || c == EOF)
	      break;

	    if (c == '\\')
	      {
		int ignore = 0;

		c = readescape (&ignore);

		if (ignore)
		  goto try_again;

		if (width < HOST_BITS_PER_INT
		    && (unsigned) c >= (1 << width))
		  warning ("escape sequence out of range for character");
	      }
	    else if (c == '\n')
	      {
		warning ("Rez forbids newline in character constant");
		lineno++;
	      }

	    num_chars++;

	    token_buffer[num_chars] = c;

	    /* Merge character into result; ignore excess chars.  */
	    if (num_chars < max_chars + 1)
	      {
		if (width < HOST_BITS_PER_INT)
		  result = (result << width) | (c & ((1 << width) - 1));
		else
		  result = c;
	      }
	  }

	token_buffer[num_chars + 1] = '\'';
	token_buffer[num_chars + 2] = 0;

	if (c != '\'')
	  error ("malformatted character constant");
	else if (num_chars == 0)
	  error ("empty character constant");
	else if (num_chars > max_chars)
	  {
	    num_chars = max_chars;
	    error ("character constant too long");
	  }

	yylval.num = result;
	value = LITERAL_CONSTANT;
      }
    break;

  case OTHER_TOKEN:
    c = '?';
    switch (c)
      {
  case '/':
  case '+':
  case '-':
  case '*':
  case '%':
  case '^':
  case '~':
  case '@':
  case '[':
  case ']':
  case ':':
  case '{':
  case '}':
  case ',':
  case '(':
  case ')':
  case ';':
    value = c;
    break;

  case '<':
    nextchar = get_one_char ();
    if (nextchar == '<')
      {
        value = LSH;
	nextchar = -1;
      }
    else if (nextchar == '=')
      {
        value = LEQ;
	nextchar = -1;
      }
    else
      value = c;
    break;

  case '>':
    nextchar = get_one_char ();
    if (nextchar == '>')
      {
        value = RSH;
	nextchar = -1;
      }
    else if (nextchar == '=')
      {
        value = GEQ;
	nextchar = -1;
      }
    else
      value = c;
    break;

  case '=':
    nextchar = get_one_char ();
    if (nextchar == '=')
      {
        value = EQUAL;
	nextchar = -1;
      }
    else
      value = c;
    break;

  case '!':
    nextchar = get_one_char ();
    if (nextchar == '=')
      {
        value = NOTEQUAL;
	nextchar = -1;
      }
    else
      value = c;
    break;

  case '|':
    nextchar = get_one_char ();
    if (nextchar == '|')
      {
        value = OROR;
	nextchar = -1;
      }
    else
      value = c;
    break;

  case '&':
    nextchar = get_one_char ();
    if (nextchar == '&')
      {
        value = ANDAND;
	nextchar = -1;
      }
    else
      value = c;
    break;
    }
   break;

case '$':
    nextchar = get_one_char ();
    if (nextchar == '"')
      {
	value = HEX_STRING_CONSTANT;
	nextchar = -1;
        goto string_constant;
      }
    else if (nextchar == '$')
      {
	    p = token_buffer;
      	c = get_one_char ();
	    while (isalnum (c))
	      {
		  *p++ = c;
		  c = get_one_char ();
	      }
	    *p = '\0';
	    nextchar = c;

	    value = '$';

	    for (i = 0; builtin_vars[i].name != NULL; ++i)
	      if (strcasecmp(token_buffer, builtin_vars[i].name) == 0)
	        {
		  value = DD_VARIABLE;
		  break;
		}
	    for (i = 0; builtin_funs[i].name != NULL; ++i)
	      if (strcasecmp(token_buffer, builtin_funs[i].name) == 0)
	        {
		  value = DD_FUNCTION;
		  break;
		}

      }
    else if (ishexdigit(nextchar))
      {
      	int rslt = 0;

      	    c = nextchar;
	    p = token_buffer;
	    while (ishexdigit(c))
	      {
		  *p++ = c;
		  c = get_one_char ();
	      }
	    *p = '\0';
	    nextchar = c;
	
	yydebug_fprintf ("Integer is 0x%x\n", rslt);

	yylval.num = rslt;
        value = INTEGER_CONSTANT;
      }
    else
      {
        error ("invalid use of $");
	value = '$';
      }
    break;

  case '"':
    value = STRING_CONSTANT;
 string_constant:
    p = token_buffer;
    while (1)
      {
	  c = get_one_char ();

	  if (c == '"' || c == EOF)
	    break;

	  *p++ = c;
      }
    *p = '\0';

    yylval.str = copy_string (token_buffer);

    yydebug_fprintf ("String is \"%s\"\n", yylval.str);
    
    break;

  case '0':  case '1':  case '2':  case '3':  case '4':
  case '5':  case '6':  case '7':  case '8':  case '9':
  case '.':
      {
	int base = 10;
	int count = 0;
	int largest_digit = 0;
	int numdigits = 0;
	/* for multi-precision arithmetic,
	   we actually store only HOST_BITS_PER_CHAR bits in each part.
	   The number of parts is chosen so as to be sufficient to hold
	   the enough bits to fit into the two HOST_WIDE_INTs that contain
	   the integer value (this is always at least as many bits as are
	   in a target `long long' value, but may be wider).  */
#define TOTAL_PARTS ((32 / 8) * 2 + 2)
	int parts[TOTAL_PARTS];
	int overflow = 0;

	for (count = 0; count < TOTAL_PARTS; count++)
	  parts[count] = 0;

	p = token_buffer;
	*p++ = c;

	if (c == '0')
	  {
	    *p++ = (c = get_one_char ());
	    if ((c == 'x') || (c == 'X'))
	      {
		base = 16;
		*p++ = (c = get_one_char ());
	      }
	    else if ((c == 'b') || (c == 'B'))
	      {
		base = 2;
		*p++ = (c = get_one_char ());
	      }
	    /* Leading 0 forces octal unless the 0 is the only digit.  */
	    else if (c >= '0' && c <= '9')
	      {
		base = 8;
		numdigits++;
	      }
	    else
	      numdigits++;
	  }

	/* Read all the digits-and-decimal-points.  */

	while (c == '.'
	       || (isalnum (c) && (c != 'l') && (c != 'L')
		   && (c != 'u') && (c != 'U')
		   && (1)))
	  {
	    if (c == '.')
	      {
		if (base == 16)
		  error ("floating constant may not be in radix 16");

		base = 10;
		*p++ = c = get_one_char ();
	      }
	    else
	      {
		/* It is not a decimal point.
		   It should be a digit (perhaps a hex digit).  */

		if (isdigit (c))
		  {
		    c = c - '0';
		  }
		else if (base <= 10)
		  {
		    if ((c & ~040) == 'E')
		      {
			base = 10;
			break;   /* start of exponent */
		      }
		    error ("nondigits in number and not hexadecimal");
		    c = 0;
		  }
		else if (c >= 'a')
		  {
		    c = c - 'a' + 10;
		  }
		else
		  {
		    c = c - 'A' + 10;
		  }
		if (c >= largest_digit)
		  largest_digit = c;
		numdigits++;

		for (count = 0; count < TOTAL_PARTS; count++)
		  {
		    parts[count] *= base;
		    if (count)
		      {
			parts[count]
			  += (parts[count-1] >> 8);
			parts[count-1]
			  &= (1 << 8) - 1;
		      }
		    else
		      parts[0] += c;
		  }

		/* If the extra highest-order part ever gets anything in it,
		   the number is certainly too big.  */
		if (parts[TOTAL_PARTS - 1] != 0)
		  overflow = 1;

		*p++ = (c = get_one_char ());
	      }
	  }

	if (numdigits == 0)
	  error ("numeric constant with no digits");

	if (largest_digit >= base)
	  error ("numeric constant contains digits beyond the radix");

	/* Remove terminating char from the token buffer and delimit the string */
	*--p = 0;

	  {
	    long high, low;
	    int spec_unsigned = 0;
	    int spec_long = 0;
	    int spec_long_long = 0;
	    int bytes, warn, i;

	    while (1)
	      {
		if (c == 'u' || c == 'U')
		  {
		    if (spec_unsigned)
		      error ("two `u's in integer constant");
		    spec_unsigned = 1;
		  }
		else if (c == 'l' || c == 'L')
		  {
		    if (spec_long)
		      {
			if (spec_long_long)
			  error ("three `l's in integer constant");
			spec_long_long = 1;
		      }
		    spec_long = 1;
		  }
		else
		  {
		    if (isalnum (c))
		      {
			error ("garbage at end of number");
			while (isalnum (c))
			  {
			    *p++ = c;
			    c = get_one_char ();
			  }
		      }
		    break;
		  }
		*p++ = c;
		c = get_one_char ();
	      }

	    ungetc (c, current_input_file);

	    bytes = 4;

	    warn = overflow;
	    for (i = bytes; i < TOTAL_PARTS; i++)
	      if (parts[i])
		warn = 1;
	    if (warn)
	      warning ("integer constant out of range");


	    *p = 0;
	  }

    yylval.num = parts[0];

    yydebug_fprintf ("Integer is \"%s\"", p);
    yydebug_fprintf (" %d\n", parts[0]);

    value = INTEGER_CONSTANT;
    }
    break;

  case 'A':  case 'B':  case 'C':  case 'D':  case 'E':
  case 'F':  case 'G':  case 'H':  case 'I':  case 'J':
  case 'K':  case 'L':  case 'M':  case 'N':  case 'O':
  case 'P':  case 'Q':  case 'R':  case 'S':  case 'T':
  case 'U':  case 'V':  case 'W':  case 'X':  case 'Y':
  case 'Z':
  case 'a':  case 'b':  case 'c':  case 'd':  case 'e':
  case 'f':  case 'g':  case 'h':  case 'i':  case 'j':
  case 'k':  case 'l':  case 'm':  case 'n':  case 'o':
  case 'p':  case 'q':  case 'r':  case 's':  case 't':
  case 'u':  case 'v':  case 'w':  case 'x':  case 'y':
  case 'z':
  case '_':
 letter:
    p = token_buffer;
    while (isalnum (c) || c == '_')
      {
	  *p++ = c;
	  c = get_one_char ();
      }
    *p = '\0';
    nextchar = c;

    value = IDENTIFIER;

    for (i = 0; reserved_words[i].name != NULL; ++i)
      if (strcasecmp(token_buffer, reserved_words[i].name) == 0)
        {
          value = reserved_words[i].token;
	  break;
	}
    if (value == IDENTIFIER)
      yydebug_fprintf ("Identifier is `%s'\n", token_buffer);
    if (value == IDENTIFIER)
      yylval.str = copy_string (token_buffer);
    break;

  default:
    warning ("unknown character");
    fprintf (stderr, "Unknown char is '%c' (0x%x)\n", c, c);
    value = c;
    break;
  }

  return value;
}

#else

int
yylex ()
{
  int c, i;
  int wide_flag;
  int value = -1;
  char *p, token_buffer[256];

  if (nextchar >= 0)
    c = nextchar, nextchar = -1;
  else
    c = get_one_char ();

  /* Effectively do c = skip_white_space (c)
     but do it faster in the usual cases.  */
  while (1)
    switch (c)
      {
      case ' ':
      case '\t':
      case '\f':
      case '\v':
      case '\b':
      case 0x1: /* For some reason, these are laying around in Rez files */
	c = get_one_char ();
	break;

      case '\r':
	/* Call skip_white_space so we can warn if appropriate.  */

      case '\n':
      case '/':
      case '\\':
	c = skip_white_space (c);
      default:
	goto found_nonwhite;
      }
 found_nonwhite:

  token_buffer[0] = c;
  token_buffer[1] = 0;

  switch (c)
    {
    case EOF:
      end_of_file = 1;
      token_buffer[0] = 0;
      value = ENDFILE;
      break;

    case '\'':
    char_constant:
      {
	register int result = 0;
	register int num_chars = 0;
	unsigned width = 8;
	int max_chars;

	max_chars = 4;

	while (1)
	  {
	  tryagain:

	    c = get_one_char ();

	    if (c == '\'' || c == EOF)
	      break;

	    if (c == '\\')
	      {
		int ignore = 0;

		c = readescape (&ignore);

		if (ignore)
		  goto tryagain;

		if (width < HOST_BITS_PER_INT
		    && (unsigned) c >= (1 << width))
		  warning ("escape sequence out of range for character");
	      }
	    else if (c == '\n')
	      {
		warning ("Rez forbids newline in character constant");
		lineno++;
	      }

	    num_chars++;

	    token_buffer[num_chars] = c;

	    /* Merge character into result; ignore excess chars.  */
	    if (num_chars < max_chars + 1)
	      {
		if (width < HOST_BITS_PER_INT)
		  result = (result << width) | (c & ((1 << width) - 1));
		else
		  result = c;
	      }
	  }

	token_buffer[num_chars + 1] = '\'';
	token_buffer[num_chars + 2] = 0;

	if (c != '\'')
	  error ("malformatted character constant");
	else if (num_chars == 0)
	  error ("empty character constant");
	else if (num_chars > max_chars)
	  {
	    num_chars = max_chars;
	    error ("character constant too long");
	  }

	yylval.num = result;
	value = LITERAL_CONSTANT;
      }
    break;

  case '/':
  case '+':
  case '-':
  case '*':
  case '%':
  case '^':
  case '~':
  case '@':
  case '[':
  case ']':
  case ':':
  case '{':
  case '}':
  case ',':
  case '(':
  case ')':
  case ';':
    value = c;
    break;

  case '<':
    nextchar = get_one_char ();
    if (nextchar == '<')
      {
        value = LSH;
	nextchar = -1;
      }
    else if (nextchar == '=')
      {
        value = LEQ;
	nextchar = -1;
      }
    else
      value = c;
    break;

  case '>':
    nextchar = get_one_char ();
    if (nextchar == '>')
      {
        value = RSH;
	nextchar = -1;
      }
    else if (nextchar == '=')
      {
        value = GEQ;
	nextchar = -1;
      }
    else
      value = c;
    break;

  case '=':
    nextchar = get_one_char ();
    if (nextchar == '=')
      {
        value = EQUAL;
	nextchar = -1;
      }
    else
      value = c;
    break;

  case '!':
    nextchar = get_one_char ();
    if (nextchar == '=')
      {
        value = NOTEQUAL;
	nextchar = -1;
      }
    else
      value = c;
    break;

  case '|':
    nextchar = get_one_char ();
    if (nextchar == '|')
      {
        value = OROR;
	nextchar = -1;
      }
    else
      value = c;
    break;

  case '&':
    nextchar = get_one_char ();
    if (nextchar == '&')
      {
        value = ANDAND;
	nextchar = -1;
      }
    else
      value = c;
    break;

  case '$':
    nextchar = get_one_char ();
    if (nextchar == '"')
      {
	value = HEX_STRING_CONSTANT;
	nextchar = -1;
        goto string_constant;
      }
    else if (nextchar == '$')
      {
	    p = token_buffer;
      	c = get_one_char ();
	    while (isalnum (c))
	      {
		  *p++ = c;
		  c = get_one_char ();
	      }
	    *p = '\0';
	    nextchar = c;

	    value = '$';

	    for (i = 0; builtin_vars[i].name != NULL; ++i)
	      if (strcasecmp(token_buffer, builtin_vars[i].name) == 0)
	        {
		  value = DD_VARIABLE;
		  break;
		}
	    for (i = 0; builtin_funs[i].name != NULL; ++i)
	      if (strcasecmp(token_buffer, builtin_funs[i].name) == 0)
	        {
		  value = DD_FUNCTION;
		  break;
		}

      }
    else if (ishexdigit(nextchar))
      {
      	int rslt = 0;

      	    c = nextchar;
	    p = token_buffer;
	    while (ishexdigit(c))
	      {
		  *p++ = c;
		  c = get_one_char ();
	      }
	    *p = '\0';
	    nextchar = c;
	
	yydebug_fprintf ("Integer is 0x%x\n", rslt);

	yylval.num = rslt;
        value = INTEGER_CONSTANT;
      }
    else
      {
        error ("invalid use of $");
	value = '$';
      }
    break;

  case '"':
    value = STRING_CONSTANT;
 string_constant:
    p = token_buffer;
    while (1)
      {
	  c = get_one_char ();

	  if (c == '"' || c == EOF)
	    break;

	  *p++ = c;
      }
    *p = '\0';

    yylval.str = copy_string (token_buffer);

    yydebug_fprintf ("String is \"%s\"\n", yylval.str);
    
    break;

  case '0':  case '1':  case '2':  case '3':  case '4':
  case '5':  case '6':  case '7':  case '8':  case '9':
  case '.':
      {
	int base = 10;
	int count = 0;
	int largest_digit = 0;
	int numdigits = 0;
	/* for multi-precision arithmetic,
	   we actually store only HOST_BITS_PER_CHAR bits in each part.
	   The number of parts is chosen so as to be sufficient to hold
	   the enough bits to fit into the two HOST_WIDE_INTs that contain
	   the integer value (this is always at least as many bits as are
	   in a target `long long' value, but may be wider).  */
#define TOTAL_PARTS ((32 / 8) * 2 + 2)
	int parts[TOTAL_PARTS];
	int overflow = 0;

	for (count = 0; count < TOTAL_PARTS; count++)
	  parts[count] = 0;

	p = token_buffer;
	*p++ = c;

	if (c == '0')
	  {
	    *p++ = (c = get_one_char ());
	    if ((c == 'x') || (c == 'X'))
	      {
		base = 16;
		*p++ = (c = get_one_char ());
	      }
	    else if ((c == 'b') || (c == 'B'))
	      {
		base = 2;
		*p++ = (c = get_one_char ());
	      }
	    /* Leading 0 forces octal unless the 0 is the only digit.  */
	    else if (c >= '0' && c <= '9')
	      {
		base = 8;
		numdigits++;
	      }
	    else
	      numdigits++;
	  }

	/* Read all the digits-and-decimal-points.  */

	while (c == '.'
	       || (isalnum (c) && (c != 'l') && (c != 'L')
		   && (c != 'u') && (c != 'U')
		   && (1)))
	  {
	    if (c == '.')
	      {
		if (base == 16)
		  error ("floating constant may not be in radix 16");

		base = 10;
		*p++ = c = get_one_char ();
	      }
	    else
	      {
		/* It is not a decimal point.
		   It should be a digit (perhaps a hex digit).  */

		if (isdigit (c))
		  {
		    c = c - '0';
		  }
		else if (base <= 10)
		  {
		    if ((c & ~040) == 'E')
		      {
			base = 10;
			break;   /* start of exponent */
		      }
		    error ("nondigits in number and not hexadecimal");
		    c = 0;
		  }
		else if (c >= 'a')
		  {
		    c = c - 'a' + 10;
		  }
		else
		  {
		    c = c - 'A' + 10;
		  }
		if (c >= largest_digit)
		  largest_digit = c;
		numdigits++;

		for (count = 0; count < TOTAL_PARTS; count++)
		  {
		    parts[count] *= base;
		    if (count)
		      {
			parts[count]
			  += (parts[count-1] >> 8);
			parts[count-1]
			  &= (1 << 8) - 1;
		      }
		    else
		      parts[0] += c;
		  }

		/* If the extra highest-order part ever gets anything in it,
		   the number is certainly too big.  */
		if (parts[TOTAL_PARTS - 1] != 0)
		  overflow = 1;

		*p++ = (c = get_one_char ());
	      }
	  }

	if (numdigits == 0)
	  error ("numeric constant with no digits");

	if (largest_digit >= base)
	  error ("numeric constant contains digits beyond the radix");

	/* Remove terminating char from the token buffer and delimit the string */
	*--p = 0;

	  {
	    long high, low;
	    int spec_unsigned = 0;
	    int spec_long = 0;
	    int spec_long_long = 0;
	    int bytes, warn, i;

	    while (1)
	      {
		if (c == 'u' || c == 'U')
		  {
		    if (spec_unsigned)
		      error ("two `u's in integer constant");
		    spec_unsigned = 1;
		  }
		else if (c == 'l' || c == 'L')
		  {
		    if (spec_long)
		      {
			if (spec_long_long)
			  error ("three `l's in integer constant");
			spec_long_long = 1;
		      }
		    spec_long = 1;
		  }
		else
		  {
		    if (isalnum (c))
		      {
			error ("garbage at end of number");
			while (isalnum (c))
			  {
			    *p++ = c;
			    c = get_one_char ();
			  }
		      }
		    break;
		  }
		*p++ = c;
		c = get_one_char ();
	      }

	    ungetc (c, current_input_file);

	    bytes = 4;

	    warn = overflow;
	    for (i = bytes; i < TOTAL_PARTS; i++)
	      if (parts[i])
		warn = 1;
	    if (warn)
	      warning ("integer constant out of range");


	    *p = 0;
	  }

    yylval.num = parts[0];

    yydebug_fprintf ("Integer is \"%s\"", p);
    yydebug_fprintf (" %d\n", parts[0]);

    value = INTEGER_CONSTANT;
    }
    break;

  case 'A':  case 'B':  case 'C':  case 'D':  case 'E':
  case 'F':  case 'G':  case 'H':  case 'I':  case 'J':
  case 'K':  case 'L':	case 'M':  case 'N':  case 'O':
  case 'P':  case 'Q':  case 'R':  case 'S':  case 'T':
  case 'U':  case 'V':  case 'W':  case 'X':  case 'Y':
  case 'Z':
  case 'a':  case 'b':  case 'c':  case 'd':  case 'e':
  case 'f':  case 'g':  case 'h':  case 'i':  case 'j':
  case 'k':  case 'l':  case 'm':  case 'n':  case 'o':
  case 'p':  case 'q':  case 'r':  case 's':  case 't':
  case 'u':  case 'v':  case 'w':  case 'x':  case 'y':
  case 'z':
  case '_':
 letter:
    p = token_buffer;
    while (isalnum (c) || c == '_')
      {
	  *p++ = c;
	  c = get_one_char ();
      }
    *p = '\0';
    nextchar = c;

    value = IDENTIFIER;

    for (i = 0; reserved_words[i].name != NULL; ++i)
      if (strcasecmp(token_buffer, reserved_words[i].name) == 0)
        {
          value = reserved_words[i].token;
	  break;
	}

    if (value == IDENTIFIER)
      yydebug_fprintf ("Identifier is `%s'\n", token_buffer);

    if (value == IDENTIFIER)
      yylval.str = copy_string (token_buffer);

    break;

  default:
    warning ("unknown character");
    fprintf (stderr, "Unknown char is '%c' (0x%x)\n", c, c);
    value = c;
    break;
  }

  return value;
}
#endif /* CPPLIB */

/* Read an escape sequence, returning its equivalent as a character,
   or store 1 in *ignore_ptr if it is backslash-newline.  */

static int
readescape (ignore_ptr)
     int *ignore_ptr;
{
  register int c = get_one_char ();
  register int code;
  register unsigned count;
  unsigned firstdig;
  int nonnull;

  switch (c)
    {
    case 'x':
      code = 0;
      count = 0;
      nonnull = 0;
      while (1)
	{
	  c = get_one_char ();
	  if (!(c >= 'a' && c <= 'f')
	      && !(c >= 'A' && c <= 'F')
	      && !(c >= '0' && c <= '9'))
	    {
	      ungetc (c, current_input_file);
	      break;
	    }
	  code *= 16;
	  if (c >= 'a' && c <= 'f')
	    code += c - 'a' + 10;
	  if (c >= 'A' && c <= 'F')
	    code += c - 'A' + 10;
	  if (c >= '0' && c <= '9')
	    code += c - '0';
	  if (code != 0 || count != 0)
	    {
	      if (count == 0)
		firstdig = code;
	      count++;
	    }
	  nonnull = 1;
	}
      if (! nonnull)
	error ("\\x used with no following hex digits");
      else if (count == 0)
	/* Digits are all 0's.  Ok.  */
	;
      else if ((count - 1) * 4 >= 32
	       || (count > 1
		   && ((1 << (32 - (count - 1) * 4))
		       <= firstdig)))
	warning ("hex escape out of range");
      return code;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':
      code = 0;
      count = 0;
      while ((c <= '7') && (c >= '0') && (count++ < 3))
	{
	  code = (code * 8) + (c - '0');
	  c = get_one_char ();
	}
      ungetc (c, current_input_file);
      return code;

    case '\\': case '\'': case '"':
      return c;

    case '\n':
      lineno++;
      *ignore_ptr = 1;
      return 0;

    case 'n':
      return TARGET_NEWLINE;

    case 't':
      return TARGET_TAB;

    case 'r':
      return TARGET_CR;

    case 'f':
      return TARGET_FF;

    case 'b':
      return TARGET_BS;

    case 'a':
      return TARGET_BELL;

    case 'v':
      return TARGET_VT;

    case '?':
      return 0x7f;

      /* `\(', etc, are used at beginning of line to avoid confusing Emacs.  */
    case '(':
    case '{':
    case '[':
      return c;
    }
#if 0
  if (c >= 040 && c < 0177)
    warning ("unknown escape sequence `\\%c'", c);
  else
    warning ("unknown escape sequence: `\\' followed by char code 0x%x", c);
#endif

  return c;
}

void
yyerror (s)
     char *s;
{
  char buf[200];

  strcpy (buf, s);

  if (end_of_file)
    strcat (buf, " at end of input");

  error (buf);
}

/* Copy a string to newly-allocated space.  The new space is never freed. */

char *
copy_string (str)
     char *str;
{
  int len = strlen (str);
  char *rslt;

  rslt = xmalloc (len + 1);
  strcpy (rslt, str);
  return rslt;
}

struct expr *
create_number (num)
     int num;
{
  struct expr *node;

  node = (struct expr *) xmalloc (sizeof (struct expr));

  node->code = 0;
  node->numfields = 1;
  node->fields[0].num = num;
  return node;
}

struct expr *
create_string (str)
     char *str;
{
  struct expr *node;

  node = (struct expr *) xmalloc (sizeof (struct expr));

  node->code = 1;
  node->numfields = 1;
  node->fields[0].str = copy_string (str);
  return node;
}

struct expr *
create_symbol (str)
     char *str;
{
  struct expr *node;

  node = (struct expr *) xmalloc (sizeof (struct expr));

  node->code = 2;
  node->numfields = 1;
  node->fields[0].str = copy_string (str);
  return node;
}

struct expr *
create_unary_op (op, arg1)
     int op;
     struct expr *arg1;
{
  struct expr *node;

  node = (struct expr *) xmalloc (sizeof (struct expr));

  node->code = op;
  node->numfields = 1;
  node->fields[0].exp = arg1;
  return node;
}

struct expr *
create_binary_op (op, arg1, arg2)
     int op;
     struct expr *arg1, *arg2;
{
  struct expr *node;

  node = (struct expr *) xmalloc (sizeof (struct expr));

  node->code = op + 256;
  node->numfields = 2;
  node->fields[0].exp = arg1;
  node->fields[1].exp = arg2;
  return node;
}

int
eval_expression (expr)
     struct expr *expr;
{
  int rslt = 0, op1, op2;

  if (expr->code >= e_neg && expr->code <= e_cmp)
    {
      op1 = eval_expression (expr->fields[0].exp);
    }
  if (expr->code >= e_add && expr->code <= e_oor)
    {
      op1 = eval_expression (expr->fields[0].exp);
      op2 = eval_expression (expr->fields[1].exp);
    }

  switch (expr->code)
    {
    case e_num:
      rslt = expr->fields[0].num;
      break;

    case e_str:
      /* (error?) */
      break;

    case e_sym:
      /* (should look up in table(s)) */
      break;

    case e_neg:
	        rslt = - op1;  break;
    case e_not:
	        rslt = ! op1;  break;
    case e_cmp:
	        rslt = ~ op1;  break;

    case e_add:
      rslt = op1 + op2;  break;
	      case e_sub:
	        rslt = op1 - op2;  break;
	      case e_mul:
	        rslt = op1 * op2;  break;
	      case e_div:
	        rslt = op1 / op2;  break;
	      case e_mod:
	        rslt = op1 % op2;  break;
	      case e_lsh:
	        rslt = op1 << op2;  break;
	      case e_rsh:
	        rslt = op1 >> op2;  break;
	      case e_lt:
	        rslt = op1 < op2;  break;
	      case e_gt:
	        rslt = op1 > op2;  break;
	      case e_leq:
	        rslt = op1 <= op2;  break;
	      case e_geq:
	        rslt = op1 >= op2;  break;
	      case e_eq:
	        rslt = op1 == op2;  break;
	      case e_ne:
	        rslt = op1 != op2;  break;
	      case e_and:
	        rslt = op1 & op2;  break;
	      case e_or:
	        rslt = op1 | op2;  break;
	      case e_xor:
	        rslt = op1 ^ op2;  break;
	      case e_aan:
	        rslt = op1 && op2;  break;
	      case e_oor:
	        rslt = op1 || op2;  break;
      default:
        warning ("unknown expr code in int eval");
        break;
    }

  return rslt;
}

char *
eval_str_expression (expr)
     struct expr *expr;
{
  char *rslt = "", op1, op2;

  switch (expr->code)
    {
    case e_str:
      rslt = expr->fields[0].str;
      break;

    case e_sym:
      /* (should look up in table(s)) */
      break;

      default:
        warning ("unknown expr code in str eval");
        break;
    }

  return rslt;
}

void
dex (expr)
     struct expr *expr;
{
  int i;

  switch (expr->code)
    {
     case e_num:
      fprintf (stderr, "%d", expr->fields[0].num);
      break;

     case e_str:
      fprintf (stderr, " \"%s\"", expr->fields[0].str);
      break;

     case e_sym:
      fprintf (stderr, " `%s'", expr->fields[0].str);
      break;

     default:
	fprintf (stderr, "(");
	fprintf (stderr, "%d", expr->code);
        for (i = 0; i < expr->numfields; ++i)
	  fprintf (stderr, " %d", expr->fields[i].num);
	fprintf (stderr, ")");
    }
}

struct rspec *
create_resource_spec ()
{
  struct rspec *rspec;

  rspec = (struct rspec *) xmalloc (sizeof (struct rspec));

  return rspec;
}

struct rspec *
create_rspec_from_type (type)
     struct expr *type;
{
  struct rspec *rspec;
  union {
    char chars[4];
    long num;
  } converter;

  rspec = create_resource_spec ();

  if (type->code == 0)
    {
      converter.num = type->fields[0].num;
      rspec->Type = type->fields[0].num;
      strncpy (rspec->type, converter.chars, 4);
    }
  else
    {
      rspec->type_expr = type;
    }
  return rspec;
}

void
drs (rspec)
     struct rspec *rspec;
{
  drs1 (rspec);
  fprintf (stderr, "\n");
}

void
drs1 (rspec)
     struct rspec *rspec;
{
  fprintf (stderr, "[Rspec");
  if (rspec->type[0] != '\0')
    fprintf (stderr, " '%c%c%c%c'",
	     rspec->type[0], rspec->type[1], rspec->type[2], rspec->type[3]);
  else if (rspec->type_expr != NULL)
    {
      fprintf (stderr, "type<-");
      dex (rspec->type_expr);
    }
  else
    fprintf (stderr, " (no type)");
  if (rspec->id_given)
    fprintf (stderr, " id=%d", rspec->id);
  else
    fprintf (stderr, " (no id)");
  if (rspec->attrs_given)
    fprintf (stderr, " attrs=%d", rspec->attrs);
  else
    fprintf (stderr, " (no attrs)");
  fprintf (stderr, "]");
}

struct rtype_node *defined_types;

void
create_new_type (rspec, typedesc)
     struct rspec *rspec;
     struct rtype_node *typedesc;
{
  struct rtype_node *type;

  type = (struct rtype_node *) xmalloc (sizeof (struct rtype_node));

  type->code = rt_type;
  type->rspec = rspec;
  type->sub = typedesc;

  type->next = defined_types;
  defined_types = type;

  if (debug)
    dt (type);
}

void
clone_type (rspec, rspec2)
     struct rspec *rspec, *rspec2;
{
  if (debug)
    drs (rspec);
  if (debug)
    drs (rspec2);
  warning ("type cloning not implemented yet");
}

struct rtype_node *
create_node (code)
     enum rtype_code code;
{
  struct rtype_node *node;

  node = (struct rtype_node *) xmalloc (sizeof (struct rtype_node));

  node->code = code;

  return node;
}

struct rtype_node *
create_symbolic (sym, val)
     struct expr *sym, *val;
{
  struct rtype_node *node;

  node = create_node (rts_symbolic);

  node->symbol = sym;
  node->const_expr = val;

  return node;
}

/* Reverse the order of elements in the chain T,
   and return the new head of the chain (old last element).  */

struct rtype_node *
node_reverse (node)
     struct rtype_node *node;
{
  struct rtype_node *prev = NULL, *decl, *next;

  for (decl = node; decl; decl = next)
    {
      next = decl->next;
      decl->next = prev;
      prev = decl;
    }
  return prev;
}

struct rtype_node *
find_type_definition (rspec)
     struct rspec *rspec;
{
  struct rtype_node *type;

  for (type = defined_types; type != NULL; type = type->next)
    {
      if (rspec_matches_type (rspec, type))
        return type;
    }

  return NULL;
}

int
rspec_matches_type (rspec, type)
     struct rspec *rspec;
     struct rtype_node *type;
{
  if (type->rspec == NULL)
    return 0;

  if (rspec->Type == type->rspec->Type)
    {
      if (type->rspec->id_given)
       {
	if (type->rspec->id == type->rspec->id2)
	  return (rspec->id == type->rspec->id);
	else
	  return (rspec->id >= type->rspec->id
	  	  && rspec->id <= type->rspec->id2);
       }
      else
	return 1;
    }
  
  return 0;
}

char *codenames[] = {
  "nil",
  "type",
  "bool",
  "num",
  "char",
  "str",
  "pnt",
  "rect",
  "array",
  "switch",
  "fill",
  "align",
  "label",
  "sym",
  NULL,
  NULL
};

void
dt (type)
     struct rtype_node *type;
{
  dt1 (type);
  fprintf (stderr, "\n");
}

void
dt1 (type)
     struct rtype_node *type;
{
  if (type->code < rt_last && codenames[type->code] != NULL)
    fprintf (stderr, "{%s", codenames[type->code]);
  else
    fprintf (stderr, "{Node %d", type->code);
  if (type->rspec)
    {
      fprintf (stderr, " ");
      drs1 (type->rspec);
      fprintf (stderr, "\n  ");
    }
  if (type->size)
    {
      fprintf (stderr, " (sz %d)", type->size);
    }
  if (type->length_expr)
    {
      fprintf (stderr, " (len ");
      dex (type->length_expr);
      fprintf (stderr, ")");
    }
  if (type->const_expr)
    {
      fprintf (stderr, " (const ");
      dex (type->const_expr);
      fprintf (stderr, ")");
    }
  if (type->symbol)
    {
      fprintf (stderr, " (sym ");
      dex (type->symbol);
      fprintf (stderr, ")");
    }
  if (type->sub)
    {
      fprintf (stderr, " (sub ");
      dt1 (type->sub);
      fprintf (stderr, ")");
    }
  if (type->symbolics)
    {
      fprintf (stderr, " (syms ");
      dt1 (type->symbolics);
      fprintf (stderr, ")");
    }
  if (type->next && type->code != rt_type)
    {
      fprintf (stderr, " ");
      dt1 (type->next);
    }
  fprintf (stderr, "}");
}
