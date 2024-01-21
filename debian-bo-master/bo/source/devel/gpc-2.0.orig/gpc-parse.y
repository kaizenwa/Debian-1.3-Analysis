/* A Bison parser for the programming language Pascal.  
   Copyright (C) 1989, 1995 Free Software Foundation, Inc.

This file is part of GNU GCC.

GNU GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*
 * Author: Jukka Virtanen <jtv@hut.fi>
 *         Helsinki University of Technology
 *         Computing Centre
 *         Finland
 *
 * Bison parser for ISO 7185 Pascal originally written
 * 3 Feb 1987 by Jukka Virtanen <jtv@hut.fi>
 *
 * The parser also parses (most of) the ISO/IEC 10206 Extended Pascal language.
 * 7 Dec 1993 by Jukka Virtanen <jtv@hut.fi>
 * 
 * The parser also parses (most of) the Borland Pascal extensions.
 * 7 Oct 1995 by Peter Gerwinski <peter.gerwinski@uni-essen.de>
 *
 * The Dangling else will not cause a shift/reduce conflict - it's
 * solved by precedence rules.
 *
 * Acknowledgements:
 *
 *  Bill Cox <bill@cygnus.com> added the error recovery rules.
 *
 * Expect some shift/reduce conflicts, due to error reporting.
 * The error detection/recovery strategy ( the placement of 'error'
 * and 'yyerrok' tokens ) used here is that suggested in 
 * "An Introduction to Compiler Construction with Unix", by Axel
 * Schreiner and H. George Friedman, chapter four.  Published by 
 * Prentice-Hall in 1985.
 *
 * jtv: I disabled the error ("Improperly handled....") calls because
 *      they are too verbose and I think an error report is already given
 *      for all cases. If not -> bug.
 */

/* One shift/reduce conflict is coming from initialization with '=' of
 * typed constants or variables.  It crashes with upper-bound espressions
 * of subranges.  For an example, write
 *
 *     Const Foo: Boolean = false;
 * as
 *     Const Foo: false .. 1 = 1 = 2 = 7;
 *
 * Another shift/reduce conflict is coming from structured initializers
 * enclosed in '(' ... ')' which crash with expressions in parentheses.
 * 
 * The remaining 11 shift/reduce conflicts come from error recovery rules.
 * -- PG
 */

%expect 13

/* I took PROTECTED out of new_identifier and introduced
 * enable/disable_keyword instead.  `Protected' can be 
 * redefined now everywhere (including in parameter lists)
 * without shift/reduce conflicts.
 *
 * All the following works:
 *
 *   Procedure Foo ( protected a, b, c: Integer );
 *   Procedure Foo ( protected, a, b, c: Integer );
 *   Procedure Foo ( a, b, c, protected: Integer );
 *   Procedure Foo ( a, b, protected, c: Integer );
 *   Procedure Foo ( protected: Integer );
 *   Procedure Foo ( Var protected: Integer );
 *   Procedure Foo ( protected protected: Integer );
 *
 * -- PG
 */

%{
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#include "config.h"
#include "tree.h"
#include "input.h"
#include "c-tree.h"
#include "flags.h"
#include "gpc-defs.h"

#ifndef errno
extern int errno;
#endif

/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1

extern char *token_buffer;	/* Pointer to token buffer.
				   Actual allocated length is maxtoken + 2.  */

extern struct function * maybe_find_function_data PROTO ((tree));

extern void warning ();

void yyerror();

/* Like YYERROR but do call yyerror */
#define YYERROR1 { yyerror ("syntax error"); YYERROR; }

void position_after_white_space ();

/* Since parsers are distinct for each language, put the language string
 * definition here.
 *
 * rs6000.c calls abort() if this is "GNU Pascal" :-)
 *
 * I have modified rs6000.c.  Since "Pascal" is a person's name
 * like "Ada" it has the right to be written in the same manner. :-)
 * -- PG
 *
 * Sigh, I don't want to change rs6000.c; I will inform Kenner to change
 * it in the next release.
 */
#ifdef _IBMR2
char *language_string = "GNU PASCAL";
#else
char *language_string = "GNU Pascal";
#endif /* Sigh */

tree main_program_name = NULL_TREE;

tree unresolved_pointer_types = NULL_TREE;
%}

%start pascal_program

/* the union representing a semantic stack entry - this will become
 * the yyunion.inc file.
 */
%union {
    char *filename;
    long  itype;
    int   lineno; 
    enum  tree_code code;
    tree  ttype;
}



%token IDENTIFIER

/* reserved words */
/* FILE_ has the underscore appended to avoid problems */
%token AND ARRAY BEGIN CASE CONST DIV DO DOWNTO ELSE END
%token FILE_ FOR FUNCTION GOTO IF IN LABEL MOD NIL NOT OF OR
%token PACKED PROCEDURE PROGRAM RECORD REPEAT SET THEN TO TYPE
%token UNTIL VAR WHILE WITH 

/* additional reserved words in Extended Pascal */
%token AND_THEN BINDABLE EXPORT IMPORT MODULE ONLY OR_ELSE
%token OTHERWISE POW RESTRICTED PROTECTED QUALIFIED VALUE

/* Additional reserved words in Object Pascal extensions to Extended Pascal */
%token OP_ABSTRACT OP_CLASS OP_CONSTRUCTOR OP_DESTRUCTOR OP_INHERITED
%token OP_IS OP_PROPERTY OP_VIEW

/* Additional reserved words in Borland extensions */
%token BP_ASM BP_EXPORTS BP_INLINE BP_LIBRARY BP_OBJECT
%token BP_SHL BP_SHR BP_UNIT BP_USES BP_XOR

%token ALL            /* Extended Pascal "export foo = all" extension */

%token RENAME 	      /* Extended Pascal token '=>' for export renaming */
%token SYMMETRIC_DIFF /* Extended Pascal: Set symmetric difference (XOR) */

%token STRING_SCHEMA  /* Extended Pascal string schema type generator */

%token IMPLEMENTATION /* Extended Pascal module implementation directive */
                      /* or Borland Pascal unit implementation keyword */
%token INTERFACE      /* Extended Pascal module interface directive */
                      /* or Borland Pascal unit interface keyword */

%token D_FORWARD	/* Required directive Forward */
%token D_ASMNAME	/* assembler-name specification (extension) */
%token D_ATTRIBUTE	/* function attributes (e.g. stack conventions) */
%token D_C	        /* directive C */
%token D_C_LANGUAGE	/* directive C_LANGUAGE (same as D_C) */
%token D_EXTERN	        /* directive Extern */
%token D_EXTERNAL	/* directive External */
%token D_OVERRIDE       /* Object pascal directive */
%token D_STATIC         /* directive Static (extension) */
%token BP_ABSOLUTE	/* Borland Pascal absolute directive */
%token BP_VIRTUAL	/* Borland Pascal virtual directive */

/* The following `r_*' and 'z_*' are not tokens used in the parser.
 * However they are used in the same context as some tokens, so assign
 * unique numbers to them.
 */
%token r_WRITE r_READ r_INITFDR r_LAZYGET r_COLLECT r_POW r_EXPON
%token z_ARCTAN z_COS z_EXP z_LN z_SIN z_SQRT z_POW z_EXPON

/* redefinable standard identifiers */
%token p_INPUT p_OUTPUT p_REWRITE p_RESET p_PUT p_GET p_WRITE p_READ
%token p_WRITELN p_READLN p_PAGE p_NEW p_DISPOSE 
%token p_ABS p_SQR p_SIN p_COS p_EXP p_LN p_SQRT p_ARCTAN 
%token p_TRUNC p_ROUND p_PACK p_UNPACK p_ORD p_CHR p_SUCC p_PRED
%token p_ODD p_EOF p_EOLN
%token p_MAXINT p_TRUE p_FALSE

/* Additional redefinable identifiers for Extended Pascal */
%token p_EXTEND p_UPDATE p_SEEKWRITE p_SEEKREAD p_SEEKUPDATE
%token p_READSTR p_WRITESTR p_BIND p_UNBIND p_HALT p_GETTIMESTAMP
%token p_ARG p_RE p_IM p_CARD p_CMPLX p_POLAR
%token p_EMPTY p_POSITION p_LASTPOSITION
%token p_LENGTH p_INDEX p_SUBSTR p_TRIM
%token p_EQ p_LT p_GT p_NE p_LE p_GE
%token p_BINDING
%token p_DATE p_TIME 
%token p_MAXCHAR p_MAXREAL p_MINREAL p_EPSREAL

/* Additional redefinable identifiers for Borland Pascal */
%token bp_GETMEM bp_FREEMEM bp_INC bp_DEC

/* Additional non-redefinable identifiers for Pascal-SC (PXSC) */
%token PXSC_OPERATOR

/* Additional redefinable identifiers for GNU Pascal */
%token gpc_MIN gpc_MAX

/* Extended Pascal module interfaces */
%token STANDARD_OUTPUT STANDARD_INPUT

/* Redefinable Object Pascal functions,constants,types and implicit ref */
%token op_COPY op_NULL op_ROOT op_SELF op_TEXTWRITABLE

/* redefinable GPC keyword extensions */
%token ASM BREAK CONTINUE p_MARK p_RELEASE RETURN
%token DEFAULT OTHERS p_CLOSE
%token SIZEOF ALIGNOF ANDAND CONJUGATE
%token p_DEFINESIZE

/* Gnu extensions:
 * These cannot be redefined. However, the ISO-10206 Pascal forbids
 * identifiers that start with, end with or have two adjacent underscores,
 * so using such ID's is not standard anyway.
 *
 *    __inline__ 	: compile functions inline
 *    __external__      : external variables (C sense)
 *    __volatile__      : volatile variables (C sense)
 *    __static__        : static variables   (C sense)
 *    __const__         : const variables    (C sense)
 */
%token INLINE EXTERNAL ASMNAME VOLATILE STATIC G_CONST

/* GNU Extensions: type qualifiers (for size and unsignedness spec) */
%token TQ_BYTE TQ_SHORT TQ_LONG TQ_LONGLONG TQ_UNSIGNED

/* GPC internal tokens */
%token UNSIGNED_INTEGER STRING_LITERAL CHAR_LITERAL UNSIGNED_REAL
%token GTE LTE NEQ EXPON
%token CEILPLUS CEILMINUS CEILMULT CEILRDIV
%token FLOORPLUS FLOORMINUS FLOORMULT FLOORRDIV
%token ASSIGN LBRACKET RBRACKET

/* ELLIPSIS, "...", used for functions with variable arglists.  */
%token TWODOTS ELLIPSIS

/* For unimplemented stuff, never returned by the lex analyzer */
%token UNIMPLEMENTED

/* Precedence rules */
/* IF and ELSE are just to take care of dangling else */
%nonassoc IF
%nonassoc ELSE

%right  <code> ASSIGN
%binary	<code> '<'	'='	'>'	IN	NEQ	GTE	LTE
%left	<code> '-'	'+'     OR	CEILPLUS	CEILMINUS	FLOORPLUS	FLOORMINUS
%left	<code> '/'	'*'	DIV	MOD	AND	BP_SHL	BP_SHR	BP_XOR	CEILMULT	CEILRDIV	FLOORMULT	FLOORRDIV
%left	<ttype> POW	EXPON   OP_IS
%left	<code> NOT

/* types for the nonterminals. */
%type <ttype>		OP_ABSTRACT
%type <ttype>		BP_ASM 
%type <ttype>		D_ASMNAME
%type <ttype>		D_ATTRIBUTE
%type <ttype>		ALIGNOF
%type <ttype>		AND_THEN
%type <ttype>		ASM
%type <ttype>		BREAK
%type <ttype>		BINDABLE
%type <ttype>		CHAR_LITERAL
%type <ttype>		OP_CLASS
%type <ttype>		CONJUGATE
%type <ttype>		CONTINUE
%type <ttype>		OP_CONSTRUCTOR
%type <ttype>		DEFAULT
%type <ttype>		OP_DESTRUCTOR
%type <ttype>		EXPORT
%type <ttype>		BP_EXPORTS 
%type <ttype>		IDENTIFIER
%type <ttype>		D_C
%type <ttype>		D_C_LANGUAGE
%type <ttype>		D_EXTERN
%type <ttype>		D_EXTERNAL
%type <ttype>		D_FORWARD
%type <ttype>		IMPLEMENTATION
%type <ttype>		BP_INLINE
%type <ttype>		INTERFACE
%type <ttype>		D_OVERRIDE
%type <ttype>		D_STATIC
%type <ttype>		IMPORT
%type <ttype>		OP_INHERITED
%type <ttype>		INLINE
%type <ttype>		BP_LIBRARY
%type <ttype>		MODULE
%type <ttype>		NIL
%type <ttype>		BP_OBJECT
%type <ttype>		ONLY
%type <ttype>		OR_ELSE
%type <ttype>		OTHERS
%type <ttype>		OTHERWISE
%type <ttype>		RESTRICTED
%type <ttype>		PXSC_OPERATOR
%type <ttype>		OP_PROPERTY
%type <ttype>		PROTECTED
%type <ttype>		QUALIFIED
%type <ttype>		RETURN
%type <ttype>		SIZEOF
%type <ttype>		STANDARD_INPUT
%type <ttype>		STANDARD_OUTPUT
%type <ttype>		STRING_LITERAL
%type <ttype>		STRING_SCHEMA
%type <ttype>		BP_UNIT
%type <ttype>		UNSIGNED_INTEGER
%type <ttype>		UNSIGNED_REAL
%type <ttype>		BP_USES 
%type <ttype>		VALUE
%type <ttype>		OP_VIEW
%type <ttype>		absolute_or_value_specification
%type <ttype>		absolute_or_value_specification_1
%type <ttype>		actual_parameter
%type <ttype>		actual_parameter_list
%type <code>		adding_operator
%type <ttype>		address_operator
%type <ttype>		and_then
%type <ttype>		any_term
%type <ttype>		any_word
%type <ttype>		array_index_list
%type <ttype>		array_type
%type <ttype>		asm_clobbers
%type <ttype>		asm_operand
%type <ttype>		asm_operands
%type <code>		assign_operator
%type <ttype>		attrib
%type <ttype>		attribute_list
%type <ttype>		boolean_expression
%type <ttype>		capacity_expression
%type <ttype>		case_constant_list
%type <ttype>		case_default
%type <ttype>		conformant_array_schema
%type <ttype>		constant
%type <ttype>		constant_literal
%type <ttype>		direct_access_index_type
%type <ttype>		directive
%type <ttype>		directive_list
%type <ttype>		directive_or_identifier
%type <ttype>		enum_list
%type <ttype>		enumerated_type
%type <ttype>		enumerator
%type <ttype>		export_list_or_all
%type <ttype>		export_list
%type <ttype>		export_list_item
%type <ttype>		expression
%type <ttype>		factor
%type <ttype>		file_type
%type <ttype>		fixed_part
%type <code>		for_direction
%type <ttype>		function_heading
%type <ttype>		function_identifier
%type <ttype>		function_pointer_domain_type
%type <ttype>		functiontype
%type <ttype>		id_list
%type <ttype>		identifier
%type <ttype>		import_clause
%type <ttype>		import_clause_list
%type <ttype>		index_expression_list
%type <ttype>		index_type_specification
%type <ttype>		index_type_specification_list
%type <ttype>		initializer_expression
%type <ttype>		iso_initializer_expression
%type <ttype>		initializer_list
%type <ttype>		simple_initializer
%type <ttype>		structured_initializer
/* ISO initializers */
%type <ttype>		array_value
%type <ttype>		array_value_element
%type <ttype>		array_value_element_list
%type <ttype>		array_value_initializer
%type <ttype>		optional_array_value_completer
%type <ttype>		component_value
%type <ttype>		field_identifier
%type <ttype>		field_identifier_list
%type <ttype>		field_list_value
%type <ttype>		field_value
%type <ttype>		field_value_list
%type <ttype>		fixed_part_value
%type <ttype>		optional_tag_field_identifier
%type <ttype>		optional_variant_part_value
%type <ttype>		record_value
%type <ttype>		structured_value_constructor
%type <ttype>		structured_value
%type <ttype>		variant_part_value
/* end of ISO initializers */
%type <ttype>		label
%type <ttype>		list_of_parameter_types
%type <ttype>		member_designator
%type <ttype>		method_identifier
%type <code>		multiplying_operator
%type <ttype>		new_identifier
%type <ttype>		new_ordinal_type
%type <ttype>		new_pointer_type
%type <ttype>		new_string_type
%type <ttype>		new_structured_type
%type <ttype>		nonnull_asm_operands
%type <ttype>		number
%type <ttype>		object_field_list
%type <ttype>		object_fixed_part
%type <ttype>		object_method_heading
%type <ttype>		object_section
%type <ttype>		object_type
%type <ttype>		one_case_constant
%type <ttype>		open_array
%type <ttype>		operator_identifier
%type <ttype>		optional_access_qualifier
%type <itype>		optional_type_qualifiers
%type <itype>		optional_gpc_type_qualifiers
%type <ttype>		optional_directive_list
%type <ttype>		optional_filename
%type <ttype>		optional_module_parameters
%type <ttype>		optional_import_qualifier
%type <ttype>		optional_inline
%type <ttype>		optional_inline_1
%type <ttype>		optional_initialization_order
%type <ttype>		optional_par_actual_parameter
%type <ttype>		optional_par_actual_parameter_list
%type <ttype>		optional_par_formal_parameter_list
%type <ttype>		optional_par_id_list
%type <ttype>		optional_par_type_list
%type <ttype>		optional_par_write_parameter_list
%type <ttype>		optional_program_heading
%type <itype>		optional_protected
%type <ttype>		optional_qualifier_list
%type <ttype>		optional_rename
%type <ttype>		optional_retval_def
%type <ttype>		op_COPY
%type <ttype>		op_NULL
%type <ttype>		op_ROOT
%type <ttype>		op_SELF
%type <ttype>		op_TEXTWRITABLE
%type <ttype>		ordinal_index_type
%type <ttype>		or_else
%type <ttype>		p_ABS
%type <ttype>		p_ARCTAN
%type <ttype>		p_ARG
%type <ttype>		p_BIND
%type <ttype>		p_BINDING
%type <ttype>		p_CARD
%type <ttype>		p_CHR
%type <ttype>		p_CLOSE
%type <ttype>		p_CMPLX
%type <ttype>		p_COS
%type <ttype>		p_DATE
%type <ttype>		bp_DEC
%type <ttype>		p_DEFINESIZE
%type <ttype>		p_DISPOSE
%type <ttype>		p_EMPTY
%type <ttype>		p_EOF
%type <ttype>		p_EOLN
%type <ttype>		p_EPSREAL
%type <ttype>		p_EQ
%type <ttype>		p_EXP
%type <ttype>		p_EXTEND
%type <ttype>		p_FALSE
%type <ttype>		bp_FREEMEM
%type <ttype>		p_GE
%type <ttype>		p_GET
%type <ttype>		bp_GETMEM
%type <ttype>		p_GETTIMESTAMP
%type <ttype>		p_GT
%type <ttype>		p_HALT
%type <ttype>		p_IM
%type <ttype>		bp_INC
%type <ttype>		p_INDEX
%type <ttype>		p_INPUT
%type <ttype>		p_LASTPOSITION
%type <ttype>		p_LE
%type <ttype>		p_LENGTH
%type <ttype>		p_LN
%type <ttype>		p_LT
%type <ttype>		p_MARK
%type <ttype>		gpc_MAX
%type <ttype>		p_MAXCHAR
%type <ttype>		p_MAXINT
%type <ttype>		p_MAXREAL
%type <ttype>		gpc_MIN
%type <ttype>		p_MINREAL
%type <ttype>		p_NE
%type <ttype>		p_NEW
%type <ttype>		p_ODD
%type <ttype>		p_ORD
%type <ttype>		p_OUTPUT
%type <ttype>		p_PACK
%type <ttype>		p_PAGE
%type <ttype>		p_POLAR
%type <ttype>		p_POSITION
%type <ttype>		p_PRED
%type <ttype>		p_PUT
%type <ttype>		p_RE
%type <ttype>		p_READ
%type <ttype>		p_READLN
%type <ttype>		p_READSTR
%type <ttype>		p_RELEASE
%type <ttype>		p_RESET
%type <ttype>		p_REWRITE
%type <ttype>		p_ROUND
%type <ttype>		p_SEEKREAD
%type <ttype>		p_SEEKUPDATE
%type <ttype>		p_SEEKWRITE
%type <ttype>		p_SIN
%type <ttype>		p_SQR
%type <ttype>		p_SQRT
%type <ttype>		p_SUBSTR
%type <ttype>		p_SUCC
%type <ttype>		p_TIME
%type <ttype>		p_TRIM
%type <ttype>		p_TRUE
%type <ttype>		p_TRUNC
%type <ttype>		p_UNBIND
%type <ttype>		p_UNPACK
%type <ttype>		p_UPDATE
%type <ttype>		p_WRITE
%type <ttype>		p_WRITELN
%type <ttype>		p_WRITESTR
%type <ttype>		packed_conformant_array_schema
%type <ttype>		par_id_list
%type <ttype>		parameter_form
%type <ttype>		parmlist1
%type <ttype>		parmlist2
%type <ttype>		pointer_domain_type
%type <ttype>		predefined_literal
%type <ttype>		primary
%type <ttype>		program_heading
%type <filename>	pxsc_adding_operator
%type <filename>	pxsc_multiplying_operator
%type <ttype>		record_field_list
%type <ttype>		record_section
%type <ttype>		record_type
%type <code>		relational_operator
%type <ttype>		rest_of_export_item
%type <ttype>		rest_of_statement
%type <ttype>		rest_of_variant
%type <itype>		rts_file_open
%type <itype>		rts_fun_onepar
%type <itype>		rts_fun_optpar
%type <itype>		rts_fun_parlist
%type <itype>		rts_fun_twopar
%type <itype>		rts_proc_onepar
%type <itype>		rts_proc_parlist
%type <filename>	save_filename
%type <lineno>		save_lineno
%type <ttype>		set_constructor
%type <ttype>		set_constructor_element_list
%type <ttype>		set_type
%type <itype>		setspecs
%type <code>		sign
%type <ttype>		simple_expression
%type <ttype>		standard_functions
%type <ttype>		static_expression
%type <ttype>		storage_qualifier
%type <ttype>		storage_qualifier_list
%type <ttype>		string
%type <ttype>		string_constant
%type <itype>		structured_variable
%type <itype>		structured_variable_list
%type <ttype>		subrange_type
%type <ttype>		term
%type <ttype>		type_denoter
%type <ttype>		type_denoter_1
%type <ttype>		type_inquiry
%type <ttype>		typename
%type <ttype>		unpacked_conformant_array_schema
%type <ttype>		unpacked_structured_type
%type <ttype>		unsigned_number
%type <ttype>		variable_access_or_typename
%type <ttype>		variable_or_function_access
%type <ttype>		variable_or_function_access_maybe_assignment
%type <ttype>		variable_or_function_access_no_id
%type <ttype>		variant
%type <ttype>		variant_list
%type <ttype>		variant_part
%type <ttype>		variant_selector
%type <ttype>		variant_type
%type <ttype>		write_actual_parameter
%type <ttype>		write_actual_parameter_list
%{
/* the declaration found for the last IDENTIFIER token read in. */
extern tree lastiddecl;

extern void reinit_parse_for_function ();

extern char *input_filename;		/* file being read */
extern char *main_input_filename;	/* top-level source file */

extern int yylex ();

/* Number of statements (loosely speaking) seen so far.  */
static int stmt_count;

static char *if_stmt_file;
static int if_stmt_line;

/* To allow build_enumerator() to set up the correct type for
 * enumerated type elements (In C they are all ints).
 * Think about [ enumtype ] set constructor.
 */
extern tree current_enum_type;

/*
 * Nonzero if the 
 */
int top_level_labels = 0;
tree main_program_labels;

tree check_set_bounds ();
tree current_type_list = NULL_TREE;

/* This is TRUE if we are in 'TYPE' declaration part
 * It is written by the parser,
 * checked in build_pascal_pointer_type().
 */
int defining_types = FALSE;

/* Nonzero if the current module is an interface module */
int this_is_an_interface_module;
%}

%%

/*
 * pascal parser starts here
 */

pascal_program :
	  /* empty */
	  { warning ("Empty input file"); }
        | program_component_list
	;

/* Allow multiple modules in input file. 
 * Only one PROGRAM is allowed per file.
 */
program_component_list :
	  program_component
	| program_component_list program_component
	;

program_component :
	  main_program_declaration dot_or_error
	| module_declaration dot_or_error
	;

dot_or_error :
	  '.'
		{
		  enable_keyword ("Module");
		  enable_keyword ("Unit");
		  enable_keyword ("Library");
		  disable_keyword ("Interface");
		  disable_keyword ("Implementation");
		}
	| error
  		{
		  warning ("Missing `.' at the end of program");
		  enable_keyword ("Module");
		  enable_keyword ("Unit");
		  enable_keyword ("Library");
		  disable_keyword ("Interface");
		  disable_keyword ("Implementation");
		}
	;

main_program_declaration :
	  optional_program_heading
  		{ 
		  tree parms;

		  if (main_program_name)
		    {
		      error ("Only one program declaration allowed");
		      YYERROR1;
		    }

		  main_program_name = get_main_program_name (TREE_VALUE ($1));

		  if (pedantic && TREE_PURPOSE ($1) == NULL_TREE)
		    warning ("No program parameters specified");

		  /* The main program is one of the modules */
		  initialize_module (main_program_name);
		  current_module->main_program = TRUE;
		  current_module->parms        = TREE_PURPOSE ($1);
		  this_is_an_interface_module  = 0;

		  /* handle parameter list here */
		  pushlevel (0);
		  clear_parm_order ();
	          declare_parm_level (1);
		  parms = get_parm_info (1);
		  poplevel (0, 0, 0);

		  main_program_context = build_nt (CALL_EXPR, main_program_name,
						   parms, NULL_TREE);

		  if (! start_function (build_tree_list (NULL_TREE, void_type_node),
					main_program_context,
					NULL_TREE, NULL_TREE,
					0))
		    YYERROR1;

		  reinit_parse_for_function ();
		  store_parm_decls ();

		  /* All labels get defined as possible targets to a
		   * non-local goto.
		   */
		  top_level_labels = 0;
		}
	  pushlevel1
		{ 
		  push_label_level ();

		  current_module->parms = TREE_PURPOSE ($1);
		  associate_external_objects (current_module->parms);
		}
	  import_or_any_declaration_part
		{
		  associate_external_objects (current_module->parms);

		  /* Now we know if program parameters are undefined */
		  check_external_objects (TREE_PURPOSE ($1), 1);

		  un_initialize_block (getdecls (), 0);
		}
	  statement_part
		{
		  yyerrok;
		  un_initialize_block (getdecls (), 1);
		  pop_label_level ();
		}
	  poplevel1
		{
		  finish_function (0);
		  /* Generate main() that calls the user main program.
		   * To create external pascal routines use "module foo"
		   * instead of "program foo";
		   */
		  output_real_main_program (main_program_name);
		}
	;

optional_program_heading :
          /* empty */
                {
                  warning ("missing program header");
                  $$ = build_tree_list (NULL_TREE, get_identifier ("noname"));
		  /*
		   * Borland Pascal would not even warn,
		   * but I think we should.
		   */
                } 
        | program_heading semi
        ;

program_heading :
	  PROGRAM
		{
		  disable_keyword ("Module");
		  disable_keyword ("Unit");
		  disable_keyword ("Library");
		}
	  new_identifier optional_par_id_list
		{ $$ = build_tree_list ($4, $3); }
	| PROGRAM
		{
		  disable_keyword ("Module");
		  disable_keyword ("Unit");
		  disable_keyword ("Library");
		}
	  error
		{ error ("missing program name");
		  yyerrok;
		}
	  optional_par_id_list
	        { $$ = build_tree_list ($5, get_identifier ("noname")); }
	;

optional_par_id_list :
	  /* empty */
		{ $$ = NULL_TREE; }
	| par_id_list
	;

par_id_list :
	  '(' id_list r_paren_or_error
		{ $$ = $2; }
	;

id_list :
	  new_identifier 
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| id_list ',' new_identifier 
		{ $$ = chainon ($1, build_tree_list (NULL_TREE, $3)); }
        | id_list error new_identifier
                { error ("comma missing after identifier `%s'", 
			   IDENTIFIER_POINTER (TREE_VALUE($1)));
                  $$ = chainon ($1, build_tree_list (NULL_TREE, $3)); 
                  yyerrok; }
        | id_list ',' error
                { error ("extra comma following id_list");
                  $$ = $1; }
        | id_list error
                { /* error ("improperly terminated id_list"); */
                  $$ = $1; }
	;

typename :
	  IDENTIFIER
		{
		  /* This happens when VALUE initializer is appended to
		     variable declaration: The parser finds that out only
		     after reading the "VALUE", and backtracs, but then
		     lastiddecl is gone already. No big deal, recheck it now.
		   */
		  if (! lastiddecl)
		    lastiddecl = check_if_predefined_type ($1);
		      
		  if (lastiddecl != 0 && TREE_CODE (lastiddecl) == TYPE_DECL)
		    {
		      $$ = TREE_TYPE (lastiddecl);
		    }
		  else
		    {
		      error ("type name expected, identifier `%s' given", 
			     IDENTIFIER_POINTER ($1));
		      $$ = error_mark_node;
		    }
		}
	;

identifier :
	  IDENTIFIER
		{
		    $$ = check_identifier (lastiddecl, $1);
		}
	;

new_identifier :
	  IDENTIFIER
/* maxint    */
	| p_MAXINT
/* boolean values */
	| p_FALSE
	| p_TRUE
/* maxchar */
	| p_MAXCHAR
/* real values */
	| p_MAXREAL
	| p_MINREAL
	| p_EPSREAL
/* standard files */
	| p_INPUT
	| p_OUTPUT
/* Pascal I/O */
	| p_REWRITE
	| p_RESET
	| p_PUT
	| p_GET
	| p_WRITE
	| p_READ
	| p_WRITELN
	| p_READLN
	| p_PAGE
/* Extended Pascal File I/O */
	| p_EXTEND
	| p_UPDATE
	| p_SEEKWRITE
	| p_SEEKREAD
	| p_SEEKUPDATE
	| p_EMPTY
	| p_POSITION
	| p_LASTPOSITION
/* heap handling */
	| p_NEW
	| p_DISPOSE
/* arithmetic functions */	
	| p_ABS
	| p_SQR
	| p_SIN
	| p_COS
	| p_EXP
	| p_LN
	| p_SQRT
	| p_ARCTAN
/* Extended Pascal arithmetic */
	| p_ARG
	| p_RE
	| p_IM
/* GNU extended arithmetic */
	| gpc_MAX
	| gpc_MIN
/* transfer functions */
	| p_TRUNC
	| p_ROUND
	| p_PACK
	| p_UNPACK
/* Extended Pascal transfer funs */
	| p_CARD
	| p_CMPLX
	| p_POLAR
/* ordinal functions */
	| p_ORD
	| p_CHR
	| p_SUCC
	| p_PRED
/* boolean functions */
	| p_ODD
	| p_EOF
	| p_EOLN
/* String type generator */
	| STRING_SCHEMA
/* String routines */
	| p_READSTR
	| p_WRITESTR
	| p_LENGTH
	| p_INDEX
	| p_SUBSTR
	| p_TRIM
	| p_EQ
	| p_LT
	| p_GT
	| p_NE
	| p_LE
	| p_GE
/* binding */
	| p_BIND
	| p_UNBIND
	| p_BINDING
/* procedure halt */
	| p_HALT
/* Time routines */
	| p_GETTIMESTAMP
	| p_DATE
	| p_TIME 
/* directives */
	| D_ASMNAME
	| D_ATTRIBUTE
	| D_C
	| D_C_LANGUAGE
	| D_FORWARD
	| D_EXTERN
	| D_EXTERNAL
/* extensions to Extended Pascal: */
	| D_STATIC
	| ASM
	| ALIGNOF
	| BREAK
	| CONTINUE
	| RETURN
	| SIZEOF
  	| CONJUGATE
	| p_MARK
	| p_RELEASE
	| DEFAULT
	| OTHERS
	| p_CLOSE
	| p_DEFINESIZE
/* Standard Module interfaces */
	| STANDARD_OUTPUT
	| STANDARD_INPUT
/*
 * These are reserved words in extended Pascal.
 * GPC allows redefinition of any of them for compatibility
 * to ISO Pascal 7185.
 */
	| AND_THEN
	| BINDABLE
	| EXPORT
	| IMPORT
	| MODULE
	| ONLY
	| OR_ELSE
	| OTHERWISE
	| POW
	| RESTRICTED
	| QUALIFIED
	| VALUE
/*
 * Word symbols in OBJECT PASCAL Extensions to 10206
 */
	| OP_ABSTRACT
	| OP_CLASS
	| OP_IS
	| OP_PROPERTY
	| OP_VIEW
	| D_OVERRIDE	  /* directive */
	| op_COPY	  /* function */
	| op_NULL	  /* constant */
	| op_ROOT	  /* root class type */
	| op_TEXTWRITABLE /* predefined property class type */
	| op_SELF	  /* implicit protected parameter */
/*
 * Borland Pascal reserved words.
 * GPC allows redefinition of any of them for compatibility
 * to ISO Pascal.
 */
	| BP_SHL
		{ $$ = get_identifier ("Shl") }
	| BP_SHR
		{ $$ = get_identifier ("Shr") }
	| BP_XOR
		{ $$ = get_identifier ("Xor") }
/*
 * Other Borland Pascal built-in identifiers
 */
	| bp_GETMEM
	| bp_FREEMEM
        | bp_INC
        | bp_DEC
	| BP_USES
	;

/*
 * Other reserved words in Object Pascal and Borland Pascal are redefinable
 * IDENTIFIERs unless they are enabled via enable_keyword.
 */


import_or_any_declaration_part :
	  IMPORT import_specification_list semi
		{ }
	  any_declaration_part
        | BP_USES uses_list semi
                {
		  if (pedantic)
		    warning ("ISO Pascal does not allow import with `uses'");
		}
          any_declaration_part
	| any_declaration_part
	;

any_declaration_part :
	  /* empty */
	| any_decl_list
	;

any_decl_list :
	  any_decl_1
	| any_decl_list any_decl_1
	;

any_decl_1 :
	  any_decl
	;

any_decl :
	  simple_decl
	| function_declaration
	;

simple_decl :
	  label_declaration_part
	| constant_definition_part
	| type_definition_part
		{ defining_types = FALSE; }
	| variable_declaration_part
	;


/* Label declaration part */

label_declaration_part : 
	  LABEL label_list semi
	| LABEL semi
		{ error ("missing label declaration"); }
	;

label_list :
	  label
		{ declare_label ($1, top_level_labels); }
	| label_list ',' label
		{ declare_label ($3, top_level_labels);
		  yyerrok; }
        | error
                { error ("non-label in label_list"); }
        | label_list error label      
		{ error ("missing comma");
                  declare_label ($3, top_level_labels); 
                  yyerrok; }
        | label_list ',' error
                { error ("extra comma"); }
        | label_list error	  
                { /* error ("improperly terminated label_list"); */ }
	;

/*
 * Labels are returned as identifier nodes for compatibility with gcc
 */
label :
	  UNSIGNED_INTEGER
	      { char *ptr;
		if (pedantic &&
		    ((TREE_INT_CST_HIGH ($1) != 0) ||
		     (TREE_INT_CST_LOW  ($1) > 9999)))
		  warning ("ISO Pascal does not allow label values greater than 9999");
		for (ptr = token_buffer; *ptr == '0'; ptr++);
		if (*ptr == '\0') --ptr;
		$$ = get_identifier (ptr); }
	| new_identifier
	      { if (pedantic)
		  warning ("ISO Pascal does not allow non-numeric labels");
		$$ = $1;
	      }
	;


/* constant definition part */

constant_definition_part : 
	  CONST constant_definition_list
  	| CONST semi
		{ error ("missing constant definition"); }
	| CONST error semi
	;

constant_definition_list : 
	  constant_definition
	| constant_definition_list constant_definition 
        | error
	;

constant_definition :
	  new_identifier '=' static_expression semi
		{ tree d = start_decl ($1, tree_cons (NULL_TREE, TREE_TYPE ($3),
				             build_tree_list (NULL_TREE,
							      const_id)),
				       1, NULL_TREE, NULL_TREE);
		  TREE_CONSTANT (d) = TREE_CONSTANT ($3);
		  finish_decl (d, $3, NULL_TREE);
		  if (pedantic && TREE_CONSTANT (d) == 0)
		     warning ("constant declared as a read-only variable");
		  if (this_is_an_interface_module)
		    handle_autoexport ($1);
		}
/* (causes a shift/reduce conflict) */
	| new_identifier ':' type_denoter '=' initializer_expression semi
		{
		  /*
		   * Borland Pascal allows such a "typed constant"
		   * to be used as an initialized variable and does not
		   * even warn if a value is written to it (because there
		   * are no other initialized variables).
		   *
		   * To be compatible, I don't flag typed constants with
		   * TYPE_READONLY but with a new flag PASCAL_TYPE_TYPEDCONST.
		   * Like this, assignments will work, but we can warn
		   * (not only if pedantic) and keep a clean conscience.
	           *
		   * When doing it like this, typed constants will always get
		   * an address, which is often the purpose of a typed const
		   * declaration.
		   */
		  tree type = copy_node (TREE_VALUE ($3));
		  tree init = $5;
		  if (init)
		    init = build_pascal_initializer ($3, init);
		  else
		    error ("constant must have specified value");
		  PASCAL_TYPE_TYPEDCONST (type) = 1;
		  declare_vars (build_tree_list (NULL_TREE, $1),
		                type, init, NULL_TREE,
				this_is_an_interface_module);
		  if (pedantic)
		    warning ("ISO Pascal does not allow typed constants");
		  if (this_is_an_interface_module)
		    handle_autoexport ($1);
		}
	;

constant :
	  identifier
	| sign identifier
		{ $$ = build_pascal_unary_op ($1, $2, 0); }
	| number
	| constant_literal
	;		      

number :
	  sign unsigned_number
		{ $$ = build_pascal_unary_op ($1, $2, 0); }
    	| unsigned_number
	;

unsigned_number :
	  UNSIGNED_INTEGER
	| UNSIGNED_REAL
	| p_MAXINT
		{ $$ = copy_node (integer_maxint_node); }
	| p_MAXREAL
		{ if (pedantic)
		    warning ("ISO Pascal does not define MAXREAL");
		  $$ = copy_node (real_max_node);
		}
	| p_MINREAL
		{ if (pedantic)
		    warning ("ISO Pascal does not define MINREAL");
		  $$ = copy_node (real_min_node);
		}
	| p_EPSREAL
		{ if (pedantic)
		    warning ("ISO Pascal does not define EPSREAL");
		  $$ = copy_node (real_eps_node);
		}
	| p_MAXCHAR
		{ if (pedantic)
		    warning ("ISO Pascal does not define MAXCHAR");
		  $$ = copy_node (char_max_node);
		}
	;

sign :
	  '+' 
		{ $$ = CONVERT_EXPR; }
	| '-' 
		{ $$ = NEGATE_EXPR; }
	;

constant_literal :
	  string
		{ $$ = combine_strings ($1); }
	| CHAR_LITERAL
	| predefined_literal
	;

predefined_literal :
	  NIL
		{ $$ = copy_node (null_pointer_node); }
	| p_FALSE
		{ $$ = copy_node (boolean_false_node); }
	| p_TRUE
		{ $$ = copy_node (boolean_true_node); }
	;

string :
	  STRING_LITERAL
	| string STRING_LITERAL
		{ $$ = chainon ($1, $2); }
	;

string_constant :
	  STRING_LITERAL
	| CHAR_LITERAL
		{
		  /* convert to a STRING_LITERAL
		   * by simulating the behavior of yylex()
		   */
		  char temp_str = TREE_INT_CST_LOW ($$);
		  $$ = build_string (1, &temp_str);
		  TREE_TYPE ($$) = char_array_type_node;
		}
	;

/* type definition part */

type_definition_part :
	  TYPE 
		{
		  enable_keyword ("Object");
		  defining_types = TRUE;
		  current_type_list = NULL_TREE;
		}
	  type_definition_list semi
		{
		  handle_forward_pointers (current_type_list);
		  disable_keyword ("Object");
		}
	| TYPE semi
		{ error ("missing type definition"); }
	;

type_definition_list :
	  type_definition
	| type_definition_list semi type_definition
	      { yyerrok; }
        | error
        | type_definition_list error type_definition
		{ error ("missing semicolon");
                  yyerrok; }
        | type_definition_list semi error
                { error ("extra semicolon"); }
        | type_definition_list error	  
                { /* error ("improperly terminated type_definition_list"); */ }
	;

/* $1 = IDENTIFIER_NODE
   $4 = ..._TYPE node
 */
type_definition :
	  new_identifier '='
		{
		  current_type_name = $1;
		}
          type_denoter absolute_or_value_specification
		{ tree d, init = $5;
		  tree type = TREE_VALUE ($4);

		  d = start_decl ($1, tree_cons (NULL_TREE, type,
				    build_tree_list(NULL_TREE, type_id)),
				  0, NULL_TREE, NULL_TREE);

		  finish_decl (d, NULL_TREE, NULL_TREE);

		  /* If value_specification is given, attach that to the
		   * TYPE_DECL node just created.
		   */
		  if (init)
		    init = build_pascal_initializer (d, init);
		  DECL_INITIAL (d) = init;

		  /* Pascal allows this:
		   *
		   *    TYPE foo = ^foo;
		   *
		   * which creates a pointer to a new type.
		   *
		   * Avoid leaving this to the tags list as LANG_TYPE.
		   *
		   * @@ This code avoids some problems, but it is not correct
		   * 
		   */
		  if (TREE_CODE (type) == POINTER_TYPE
		      && TREE_CODE (TREE_TYPE (type)) == LANG_TYPE
		      && TYPE_NAME (TREE_TYPE (type)) == $1)
		    {
		      if (!resolve_forward_pointer
			   	(d, build_pointer_type (integer_type_node)))
			abort ();
		    }

		  /* Chain all TYPE_DECL names in this type list
		   * for resolving forward pointers later.
		   */
		  current_type_list
		    = chainon (current_type_list,
			       build_tree_list (NULL_TREE, $1));
		  if (this_is_an_interface_module)
		    handle_autoexport ($1);
		}
	;

/* This Must Return A Tree_List:
    purpose : INITIAL_VALUE
    value   : .._TYPE node
 */
type_denoter :
	   optional_type_qualifiers typename 
		{ tree type   = $2;
		  int variant = $1 & TYPE_VARIANT;
		  int ext     = $1 & TYPE_SELECT;

		  if (variant)
		    type = pascal_type_variant (type, variant, 1);

		  if (lastiddecl)
		    {
		      if (ext)
			type = pascal_type_extension (type, ext);
		      
		      $$ = build_tree_list (DECL_INITIAL (lastiddecl), type);
		    }
		  else /* error() has been called already; avoid crashing */
		    $$ = build_tree_list (NULL_TREE, type);
		}

	| optional_type_qualifiers type_denoter_1
		{ tree type = $2;

		  if ($1 & ~(TYPE_QUALIFIER_BINDABLE))
		    error ("Only `Bindable' type qualifier allowed with a new type");

		  if ($1 & TYPE_QUALIFIER_BINDABLE)
		    type = pascal_type_variant (type, TYPE_QUALIFIER_BINDABLE, 1);

		  $$ = build_tree_list (NULL_TREE, type);
		}
	;

optional_type_qualifiers :
	  optional_gpc_type_qualifiers
	| BINDABLE optional_gpc_type_qualifiers
		{ $$ = TYPE_QUALIFIER_BINDABLE | $2; }
	| BINDABLE RESTRICTED
		{ $$ = TYPE_QUALIFIER_BINDABLE | TYPE_QUALIFIER_RESTRICTED; }
	| RESTRICTED
		{ $$ = TYPE_QUALIFIER_RESTRICTED; }
	;

optional_gpc_type_qualifiers :
	  /* empty */
		{ $$ = 0; }
	| TQ_BYTE
		{ $$ = TYPE_QUALIFIER_BYTE; }
	| TQ_SHORT
		{ $$ = TYPE_QUALIFIER_SHORT; }
	| TQ_LONG
		{ $$ = TYPE_QUALIFIER_LONG; }
	| TQ_LONGLONG
		{ $$ = TYPE_QUALIFIER_LONGLONG; }
	| TQ_UNSIGNED
		{ $$ = TYPE_QUALIFIER_UNSIGNED; }
	;

type_denoter_1 :
	  new_ordinal_type
	| new_pointer_type
	| new_structured_type
	| new_string_type
	| type_inquiry
	;
	


new_ordinal_type :
	  enumerated_type 
	| subrange_type
	;

/* ENUMERATED */

enumerated_type :
	  '(' setspecs
		{ if (current_enum_type) /* @@@ does it need a stack? */
		    abort ();
		  current_enum_type = $$ = start_enum (NULL_TREE);
		}
	  enum_list ')'
		{ $$ = finish_enum ($<ttype>3, nreverse ($4), NULL_TREE);
		  current_enum_type = NULL_TREE;
		  resume_momentary ($2); }
	| '(' error ')'
	        { $$ = error_mark_node; } /* @@@@ ??? */
	;

/* We chain the enumerators in reverse order.
   they are later put in forward order */

enum_list :
	  enumerator
	| enum_list ',' enumerator
		{ $$ = chainon ($3, $1);
		  yyerrok; }
/* (causes a shift/reduce conflict) */
        | error
                { error ("non-enumerator in enumerator list"); }
        | enum_list error enumerator      
		{ $$ = chainon ($3, $1); 
                  error ("missing comma");
                  yyerrok; }
        | enum_list ',' error
                { $$ = $1;
                  error ("extra comma"); }
        | enum_list error	  
                { $$ = $1;
                  /* error ("improperly terminated enum_list"); */ }
	;

/* ord(first_enumerator) == 0 */
/* second arg to build enumerator is the ord number exp of the enumerator */
enumerator:
	  new_identifier
		{ $$ = build_enumerator ($1, NULL_TREE); }
	;

/* SUBRANGE */
/* Here is a problem: In extended Pascal subrange bounds are allowed
   to be arbitrary expressions. If just done the obvious way I get
   49 shift/reduce conflicts. (one for each new_identifier)

   So I currently allow expressions only as the upper bound
   of the range, the lower bound must be a constant until
   I figure out a way to handle this. "(a) .. j;" can be mixed up
   with enumerated_type definition.
 */
subrange_type :
	  constant TWODOTS expression
		{ if (pedantic && (! TREE_CONSTANT ($3) || ! TREE_CONSTANT ($1)))
		      warning ("ISO Pascal does not allow non-constant range bounds");
		  if (TREE_TYPE ($1) != TREE_TYPE ($3)) {
		    error ("subrange bounds are not of the same type");
		    $$ = error_mark_node;
		  } else
		    /* @@@ Changed in 2.4.3.1 */
		    $$ = build_range_type (TREE_TYPE ($1), $1, $3);
		}
	;

/* POINTER
 *
 * If this is a pointer to unknown type, it is pushed to
 * the tags list by build_pascal_pointer_type()
 */
new_pointer_type :
	  pointer_char pointer_domain_type
		{ $$ = build_pascal_pointer_type ($2); }
	;

pointer_char :
	  '^'
    	| '@'
	;

/* new_identifier allows forward referencing. */
/* this is either IDENTIFIER or predefined word */
pointer_domain_type :
	  new_identifier
		{ 
		  if (lastiddecl != 0 && TREE_CODE (lastiddecl) == TYPE_DECL)
		    $$ = TREE_TYPE (lastiddecl);
		  else if (last_id_value == STRING_SCHEMA)
		    $$ = string_schema_proto_type;
		  else /* a forward declared pointer to an unknown type */
		    $$ = $1; /* return an IDENTIFIER_NODE */
		}
	| function_pointer_domain_type
	;

/* GPC extension: function and procedure pointer types */
function_pointer_domain_type :
	  PROCEDURE optional_par_type_list
		{
		  $$ = build_function_type (void_type_node, $2);
		}
	| FUNCTION optional_par_type_list ':' typename
		{
		  $$ = build_function_type ($4, $2);
		}
	;

optional_par_type_list :
	  /* Empty */
		{ $$ = build_tree_list (NULL_TREE, void_type_node); }
	| '(' ELLIPSIS ')'
		{ $$ = NULL_TREE; }
     	| '(' list_of_parameter_types comma_or_semi ELLIPSIS ')'
		{ $$ = $2; }
	| '(' list_of_parameter_types ')'
		{ $$ = chainon ($2,
				build_tree_list (NULL_TREE, void_type_node)); }
	;

list_of_parameter_types :
	  typename
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| list_of_parameter_types comma_or_semi typename
		{ $$ = chainon ($1, build_tree_list (NULL_TREE, $3)); }
	| list_of_parameter_types error
		{ 
		  error ("Invalid argument type list in function pointer");
		  $$ = NULL_TREE;
		}
	;

comma_or_semi :
	  ','
	| semi
	;

/* STRUCTURED TYPES */

new_structured_type :
	  PACKED unpacked_structured_type
		{ $$ = grok_packed ($2); }
	| unpacked_structured_type
	;

unpacked_structured_type :
	  array_type
	| file_type
	| set_type
	| record_type
        |
		{
		  if (! top_level_p (current_module->main_program))
		    error ("OBJECT type definition only allowed at top level");
		  enable_keyword ("Constructor");
		  enable_keyword ("Destructor");
		  defining_methods = 1;
		  check_object_pointer ();
		}
	  object_type
		{
		  defining_methods = 0;
		  $$ = $2;
		}
	;


/* STRING */

new_string_type :
          STRING_SCHEMA capacity_expression
		{ if (pedantic)
		    warning ("ISO Pascal does not have `string' type");
		  if (TREE_CODE (TREE_TYPE ($2)) != INTEGER_TYPE)
		    error ("String schema discriminant must be of integer type");
		  $$ = build_pascal_string_schema ($2);
		}
		    

capacity_expression :
	  '(' expression ')'
		{ $$ = $2; }
	| lbracket expression rbracket
		{ if (pedantic)
		    warning ("ISO Pascal wants string capacity in parenthesis");
		    $$ = $2;
		}
	;

/* ARRAY */

array_type :
	  ARRAY lbracket array_index_list rbracket OF type_denoter
		{ $$ = build_pascal_array_type (TREE_VALUE ($6), $3); }
	;

array_index_list :
	  ordinal_index_type
		{ $$ = $1; }
	| array_index_list ',' ordinal_index_type
		{ $$ = chainon ($1, $3); 
		  yyerrok; }
        | error
                { error ("non-expression in array_index_list"); }
        | array_index_list error ordinal_index_type      
		{ error ("missing comma");
                  $$ = chainon($1, $3);
                  yyerrok; }
        | array_index_list ',' error
                { error ("extra comma");
                  $$ = $1; }
        | array_index_list error	  
                { /* error ("improperly terminated array_index_list"); */
                  $$ = $1; }
	;


/*
 * TREE_LIST whose TREE_PURPOSE is the type and TREE_VALUE
 * is an integer range
 *
 * only for arrays and sets.
 */
ordinal_index_type :
	  new_ordinal_type
		{ $$ = build_tree_list ($1, convert_type_to_range ($1)); }
	| typename
		{ $$ = build_tree_list ($1, convert_type_to_range ($1)); }
	;

/* FILE */
file_type :
	  FILE_ direct_access_index_type  OF type_denoter
		{ $$ = build_file_type (TREE_VALUE ($4), $2); }
	;

direct_access_index_type :
	  /* empty */
		{ $$ = NULL_TREE; }
	| lbracket ordinal_index_type rbracket
		{ $$ = $2; }
	;


/* SET */
set_type :
	  SET OF ordinal_index_type
		{ tree range = TREE_VALUE ($3);
		  /* Avoid huge sets, like 'set of -maxint..maxint' */
		  TREE_VALUE ($3) = check_set_bounds (TREE_PURPOSE ($3),
						      TREE_VALUE ($3));
		  $$ = build_set_type ($3);
		}
	| SET OF error
		{ $$ = build_set_type
		        (build_tree_list
			    (void_type_node,
			     build_index_2_type (size_zero_node,
						 build_int_2 (-1, 0))));
		}
	;

/* RECORD */
record_type :
	  RECORD record_field_list END
		{ $$ = $2; 
		  yyerrok; }
	| RECORD error END	/* Return empty record */
		{ $$ = finish_struct (start_struct (RECORD_TYPE,
						    NULL_TREE),
				      NULL_TREE, NULL_TREE);
		}
	;

/* returns RECORD_TYPE */
record_field_list :
	  /* empty */
		{ $$ = finish_struct (start_struct (RECORD_TYPE,
						    NULL_TREE),
				      NULL_TREE, NULL_TREE);
		}
	| fixed_part optional_semicolon
		{ $$ = finish_struct (start_struct (RECORD_TYPE,
						    NULL_TREE),
				      $1, NULL_TREE); 
		}
	| fixed_part semi variant_part
		{ 
		  /* Chain the tag field or NULL_TREE */
		  tree fields = chainon ($1, TREE_VALUE (TREE_PURPOSE ($3)));
		  /* Chain the variant part */
		  fields = chainon ($1, TREE_VALUE ($3));
		  $$ = finish_struct (start_struct (RECORD_TYPE,
						    NULL_TREE),
				      fields, NULL_TREE);
		  store_variant_tag_info ($$, $3);
		}
	| variant_part
		{ 
		  /* Store the tag field or NULL_TREE */
		  tree fields = TREE_VALUE (TREE_PURPOSE ($1));
		  /* Chain the variant part */
		  fields = chainon (fields, TREE_VALUE ($1));
		  $$ = finish_struct (start_struct (RECORD_TYPE,
						    NULL_TREE),
				      fields, NULL_TREE);
		  store_variant_tag_info ($$, $1);
		}
	;

/* RECORD FIXED PART */
/* chain on FIELD_DECL */
fixed_part :
	  record_section
	| fixed_part semi record_section
		{ $$ = chainon ($1, $3); 
		  yyerrok; }
        | fixed_part error record_section
		{ error ("missing semicolon");
                  $$ = chainon ($1, $3); 
                  yyerrok; }
        | fixed_part semi error
		{ error ("extra semicolon");
                  $$ = $1; }
        | fixed_part error	  
                { /* error ("improperly terminated record fixed part"); */
                  $$ = $1; }
	;

/* chain of FIELD_DECL */
/* @@@ check *_momentary use here? */
record_section :
	  setspecs id_list ':' type_denoter
		{
		  if (TREE_PURPOSE ($4))
		    warning ("initial value ignored in field declaration");
		  $$ = p_grokfields ($2, TREE_VALUE ($4));
		  resume_momentary ($1);
		}
	;

/* RECORD VARIANTS */

/* returns a TREE_LIST node whose
 * PURPOSE : TREE_LIST of variant_selector
 * VALUE   : unnamed FIELD_DECL node of type UNION_TYPE
 */
variant_part :
	  CASE variant_selector OF variant_list rest_of_variant
		{ tree vlist = $4;
		  if ($5)
		    vlist = chainon (vlist, $5);
		  $$ = build_tree_list ($2,
			  build_record_variant_part (input_filename, lineno,
						     $2, vlist)); }
	;

rest_of_variant :
	  optional_semicolon
		{ $$ = NULL_TREE; }
        | semi case_default optional_colon '(' record_field_list ')' optional_semicolon
		{ $$ = build_tree_list (NULL_TREE,
					grokfield (input_filename, lineno,
						   NULL_TREE,   /* field name */
						   build_tree_list (NULL_TREE, $5),
						   NULL_TREE)); /* field width */
		}
	;

/* returns a TREE_LIST node whose
 * PURPOSE is the type of the selector
 * and
 * VALUE is a FIELD_DECL node if the variant selector actually exists,
 *       NULL_TREE otherwise.
 */
variant_selector :
	  new_identifier ':' variant_type
		/* with init value? */
		{ $$ = build_tree_list ($3,
			  grokfield (input_filename,
				     lineno,
				     $1,
				     build_tree_list (NULL_TREE, $3),
				     NULL_TREE));
		  yyerrok;
		}
        | new_identifier error variant_type
		{ error ("missing semicolon");
                  $$ = build_tree_list ($3,
			  grokfield (input_filename,
				     lineno,
				     $1,
				     build_tree_list (NULL_TREE, $3),
				     NULL_TREE));
		  yyerrok;
		}
	| variant_type
		/* with init value? */
		{ $$ = build_tree_list ($1, NULL_TREE); }
	;

variant_type :
	  typename
  	| new_ordinal_type
		{
		  if (pedantic)
		    warning ("ISO Pascal wants type identifier as variant tag type");
		  $$ = $1;
		}
	;

/* returns a chain of TREE_LIST nodes whose
 * PURPOSE field is the case list
 * and
 * VALUE is the FIELD_DECL node of RECORD_TYPE
 */
variant_list :
	  variant
	| variant_list semi variant
		{ $$ = chainon ($1, $3);
		  yyerrok; }
        | variant_list error variant
		{ error ("missing semicolon");
                  $$ = chainon ($1, $3); 
                  yyerrok; }
        | error
                { $$ = NULL_TREE; }
        | variant_list error
                { /* error ("improperly terminated record variant list"); */
                  $$ = $1;
                }
	;

variant :
	  case_constant_list ':' '(' record_field_list ')'
		{ $$ = build_tree_list ($1,
			 grokfield (input_filename, lineno,
				    NULL_TREE,   /* field name */
				    build_tree_list (NULL_TREE, $4),
				    NULL_TREE)); /* field width */
		}
	;

case_constant_list :
	  one_case_constant
	| case_constant_list ',' one_case_constant
		{ $$ = chainon ($1, $3);
		  yyerrok; }
        | case_constant_list ',' error
                { error ("extra comma");
                  $$ = $1; }
        | case_constant_list error one_case_constant
                { error ("missing comma");
		  $$ = chainon ($1, $3); 
                  yyerrok; }
        | case_constant_list error
                { /* error ("improperly terminated case constant list"); */
                  $$ = $1; }
	;

one_case_constant :
	  static_expression
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| static_expression TWODOTS static_expression
		{ $$ = build_tree_list ($3, $1); }
/* (caused 2 shift/reduce conflicts and 6 reduce/reduce conflicts)
	| error
		{ $$ = NULL_TREE; }
*/
/* (causes a shift/reduce conflict) */
        | static_expression error static_expression
		{ error("missing `..'");
                  $$ = build_tree_list ($3, $1); 
                  yyerrok; }
        | static_expression TWODOTS error
		{ error("extra `..'");
                  $$ = build_tree_list (NULL_TREE, $1); }
	;

/* in Extended Pascal */
type_inquiry :
	  TYPE OF expression
		{ $$ = TREE_TYPE ($3);
		  if (pedantic)
		    warning ("ISO Pascal forbids `TYPE OF'");
		}
	;


/* OBJECT */
object_type :
          BP_OBJECT object_field_list END
		{
		  $$ = $2;
		  store_object_name ($$);
		  add_vmt_field ($$);
                  construct_vmt ($$);
		  yyerrok;
		}
        | BP_OBJECT '(' new_identifier ')' object_field_list END
		{
		  $$ = $5;
		  store_object_name ($$);
		  if ($3)
		    {
		      register tree parent_type = lookup_name ($3);
		      if (TREE_CODE (parent_type) == TYPE_DECL)
		        {
		          parent_type = TREE_TYPE (parent_type);
		          if (TREE_CODE (parent_type) == RECORD_TYPE)
		            inherit ($$, parent_type);  /* including vmt field */
		          else
                            {
			      error ("parent type `%s' is no object type",
		                     IDENTIFIER_POINTER ($3));
                              add_vmt_field ($$);
                            }
		        }
		      else
                        {
		          error ("parent object type expected, identifier `%s' given",
		                 IDENTIFIER_POINTER ($3));
                          add_vmt_field ($$);
                        }
		    }
                  else
                    {
                      /* Can this ever happen? */
                      error ("parent object type expected");
                      add_vmt_field ($$);
                    }
                  construct_vmt ($$);
		  yyerrok;
		}
        | BP_OBJECT '(' error ')' object_field_list END
		{
		  $$ = $5;
		  error ("invalid parent object specification");
		  store_object_name ($$);
                  add_vmt_field ($$);
                  construct_vmt ($$);
		  yyerrok;
		}
	| BP_OBJECT error END
		{
		  $$ = finish_struct (start_struct (RECORD_TYPE,
						    NULL_TREE),
				      NULL_TREE, NULL_TREE);
		  store_object_name ($$);
		}
	;

object_field_list :
	  /* empty */
		{ $$ = finish_struct (start_struct (RECORD_TYPE,
						    NULL_TREE),
				      NULL_TREE, NULL_TREE);
		}
	| object_fixed_part optional_semicolon
		{ $$ = finish_struct (start_struct (RECORD_TYPE,
						    NULL_TREE),
				      $1, NULL_TREE); 
		}
        ;

/* Extension to Borland standard:  Data fields and methods may be mixed. */
object_fixed_part :
	  object_section
	| object_fixed_part semi object_section
		{
		  if ($3 == virtual_mark_node)
		    $$ = mark_virtual_method ($1);
		  else
		    $$ = chainon ($1, $3);
		  yyerrok;
		}
        | object_fixed_part error object_section
		{
		  error ("missing semicolon");
		  if ($3 == virtual_mark_node)
		    $$ = mark_virtual_method ($1);
		  else
		    $$ = chainon ($1, $3); 
                  yyerrok;
		}
        | object_fixed_part semi error
		{
		  error ("extra semicolon");
                  $$ = $1;
		}
        | object_fixed_part error
	;

object_section :
	  setspecs id_list ':' type_denoter
		{
		  if (TREE_PURPOSE ($4))
		    warning ("initial value ignored in field declaration");
                  $$ = p_grokfields ($2, TREE_VALUE ($4));
		  resume_momentary ($1);
		  disable_keyword ("Virtual");
                }
        | setspecs object_method_heading
		{
                  tree ftype, arg, argtypes, fwd_heading, method_name;
		  int is_constructor = 
                    PASCAL_CONSTRUCTOR_METHOD (TREE_PURPOSE ($2));

		  /* Generate a FORWARD declaration `Myobj_Mymethod'
		   */
		  fwd_heading = copy_node (TREE_PURPOSE ($2));
		  method_name = get_method_name (current_type_name,
		                                 TREE_OPERAND (fwd_heading, 0));
		  TREE_OPERAND (fwd_heading, 0) = method_name;
		  grok_directive (TREE_VALUE ($2), fwd_heading,
		                  build_tree_list ( NULL_TREE,
		                                    global_bindings_p () ?
 		                                      d_extern : d_static),
		                  0);  /* never nested */

		  argtypes = NULL_TREE;
                  for (arg = TREE_OPERAND (TREE_PURPOSE ($2), 1);
                       arg;
                       arg = TREE_CHAIN (arg))
                    if (TREE_PURPOSE (arg))
                      argtypes = chainon (argtypes, 
                                          TREE_TYPE (TREE_PURPOSE (arg)));

		  ftype = TREE_TYPE (lookup_name (method_name));
                  $$ = p_grokfields (build_tree_list (NULL_TREE,
                                       TREE_OPERAND (TREE_PURPOSE ($2), 0)),
                                     ftype);
		  PASCAL_CONSTRUCTOR_METHOD ($$) = is_constructor;

		  resume_momentary ($1);
		  if (this_is_an_interface_module)
		    handle_autoexport (method_name);
		  enable_keyword ("Virtual"); 
                }
	| BP_VIRTUAL
		{
		  $$ = virtual_mark_node;
		  disable_keyword ("Virtual");
		}
	;

object_method_heading :
          optional_inline
          PROCEDURE
	  new_identifier
	  optional_par_formal_parameter_list
		{
		  $$ = tree_cons (build_nt (CALL_EXPR, $3, $4, NULL_TREE),
                                  tree_cons (NULL_TREE, void_type_node, $1),
                                  NULL_TREE);
	        }
        | optional_inline
          OP_CONSTRUCTOR
	  new_identifier
	  optional_par_formal_parameter_list
		{
		  $$ = tree_cons (build_nt (CALL_EXPR, $3, $4, NULL_TREE),
                                  tree_cons (NULL_TREE, void_type_node, $1),
                                  NULL_TREE);
		  PASCAL_CONSTRUCTOR_METHOD (TREE_PURPOSE ($$)) = 1;
	        }
	| optional_inline
          OP_DESTRUCTOR
	  new_identifier
	  optional_par_formal_parameter_list
		{
		  $$ = tree_cons (build_nt (CALL_EXPR, $3, $4, NULL_TREE),
                                  tree_cons (NULL_TREE, void_type_node, $1),
                                  NULL_TREE);
	        }
	| optional_inline
          FUNCTION 
	  new_identifier
	  optional_par_formal_parameter_list
	  optional_retval_def
	  functiontype
		{ tree retval = $5;
		  tree funtype = $6;

		  if (funtype == NULL_TREE) {
		    /* Only allowed when function has already been declared with
		     * a directive.  But not here.  Here it is just an error.
		     */
                     error ("type of methodfunction `%s' undefined",
                            IDENTIFIER_POINTER ($3));
		  }

		  /* If user did not specify a name, we create one. */
		  if (retval == NULL_TREE)
		    retval =
		      get_identifier
			(concat ("retval_", IDENTIFIER_POINTER ($3), ""));

		  $$ = tree_cons (build_nt (CALL_EXPR, $3, $4, NULL_TREE),
				  tree_cons (NULL_TREE, funtype, $1),
                                  retval);
		}
	;


/* variable declaration part */

variable_declaration_part :
	  VAR variable_declaration_list semi
	| VAR semi
		{ error ("missing variable declaration"); }
	;

variable_declaration_list :
	  variable_declaration
	| variable_declaration_list semi variable_declaration
  		{ yyerrok; }
        | error
	| variable_declaration_list error variable_declaration
                { error("missing semicolon"); 
                  yyerrok; }
	| variable_declaration_list semi error
	;

variable_declaration :
	  id_list ':'
		{ enable_keyword ("Absolute"); }
		/*
		 * Keywords are enabled and disabled in the smallest
		 * possible interval.  If this is still too large to
		 * meet the ISO standard, I suggest to create a compiler
		 * switch (e.g. --strict-iso) which disables the
		 * enable_keyword function (and could be extended to
		 * report Borland extensions as errors, not as warnings
		 * like --pedantic does).
		 */
	  optional_qualifier_list type_denoter absolute_or_value_specification
		{
		  tree init = $6;

		  if (init && PASCAL_ABSOLUTE_CLAUSE (init))
		    {
		      /* no initial value but an absolute clause. */
		      tree type = TREE_VALUE ($5);
		      tree var_name, var;
		      if (pedantic)
		        warning ("ISO Pascal does not allow ABSOLUTE clauses");
		      for (var_name = $1; var_name;
		           var_name = TREE_CHAIN (var_name))
		        {
		          var = build_decl (VAR_DECL, TREE_VALUE ($1), type);
		          type = build_pointer_type (type);
		          DECL_INITIAL (var) = build_c_cast (type, init);
		          PASCAL_ABSOLUTE_CLAUSE (DECL_INITIAL (var)) = 1;
		          pushdecl (var);
		        }
		    }
		  else
		    {
		      /* If variable initialization not given, try type initialization */
		      if (init)
		        init = build_pascal_initializer ($5, init);
		      else
		        init = TREE_PURPOSE ($5);
		      declare_vars ($1, TREE_VALUE ($5), init, $4,
				    this_is_an_interface_module);
		    }
		}
	;

optional_qualifier_list :
	  /* empty */
		{ $$ = NULL_TREE; }
	| storage_qualifier_list
	;

storage_qualifier :
	  EXTERNAL 
		{ $$ = build_tree_list (NULL_TREE, extern_id); }
	| D_EXTERNAL 
		{ /* EXTERNAL is with, D_EXTERNAL without underscores.
		   * @@@ This rule is here just to prevent gpc from crashing
		   *     when somebody forgets the underscores.
		   */
		  $$ = build_tree_list (NULL_TREE, extern_id); }
	| ASMNAME string_constant
		{ $$ = build_tree_list ($2, extern_id); }
	| D_ASMNAME string_constant
		{ /* @@@ Same problem as with EXTERNAL. */
		  $$ = build_tree_list ($2, extern_id); }
	| STATIC
		{ $$ = build_tree_list (NULL_TREE, static_id); }
	| VOLATILE
		{ $$ = build_tree_list (NULL_TREE, volatile_id); }
	;

storage_qualifier_list :
	  storage_qualifier
	| storage_qualifier_list storage_qualifier
		{ $$ = chainon ($1, $2); }
	;

absolute_or_value_specification :
		/* It is too late to call enable_keyword here.
		 * -- PG
		 */
	  absolute_or_value_specification_1	  
		{ disable_keyword ("Absolute");
		  /* This seems to work, but I don't see the difference
		   * to disable_keyword being in variable_declaration.
		   */
		  $$ = $1;
		}
	;

absolute_or_value_specification_1 :
	  /* empty */
		{ $$ = NULL_TREE; }
	| VALUE initializer_expression
		{ if (pedantic)
		    warning ("ISO 7185 Pascal does not allow type or variable initialization with VALUE");
		  $$ = $2;
		}
	| ASSIGN initializer_expression
		{
                  /* VAX Pascal initializer */
                  if (pedantic)
		    warning ("ISO Pascal does not allow initialization with `:='");
		  $$ = $2;
		}
/* (causes a shift/reduce conflict - see also constant_definition) */
	| '=' initializer_expression
		{
		  if (pedantic)
		    warning ("ISO Pascal does not allow initialization with `='");
		  $$ = $2;
		}
	| BP_ABSOLUTE variable_or_function_access
		{
		  $$ = build_pascal_unary_op (ADDR_EXPR, $2, 0);
		  PASCAL_ABSOLUTE_CLAUSE ($$) = 1;
		  if (pedantic)
		    warning ("ISO Pascal does not allow absolute clauses");
                    /* This is no initialization but something like */
                    /* a variant record (a union in C).             */
		}
	| VALUE error
		{ $$ = NULL_TREE; }
	| ASSIGN error
		{ $$ = NULL_TREE; }
/* (causes a shift/reduce conflict - see also constant_definition) */
	| '=' error
		{ $$ = NULL_TREE; }
	| BP_ABSOLUTE error
		{ $$ = NULL_TREE; }
	;

initializer_expression :
	  simple_initializer
/* (causes a shift/reduce conflict) */
	| structured_initializer
	;

iso_initializer_expression :
	  expression
	| structured_value_constructor	
	;

/* @@@@@ UNIMPLEMENTED ISO Initializers */

structured_value_constructor :
	  typename structured_value
		{ }
	;

/* @@@ Must check that the typename in the structured_value_constructor
   is of correct type, i.e: array, record or set type name */
structured_value :
   	  array_value
	| record_value
	| set_constructor
	;

/* tree_list (array_value_element_list, array_value_completer) */
array_value :
	  lbracket array_value_initializer rbracket
		{ $$ = $2; }
	;

array_value_initializer :
	  array_value_element_list
	  optional_array_value_completer
	  optional_semicolon
		{ $$ = build_tree_list ($1, $2); }
	;

array_value_element_list :
	  array_value_element
	| array_value_element_list ';' array_value_element
	;

array_value_element :
	  case_constant_list ':' component_value
		{ }
	;

optional_array_value_completer :
		{ $$ = NULL_TREE; }
	| ';' OTHERWISE component_value
		{ $$ = $3; }
	;

component_value :
	  expression
	| array_value
	| record_value
	;

record_value :
	  lbracket field_list_value optional_semicolon rbracket
		{ $$ = $2; }
	;

field_list_value :
		{ $$ = NULL_TREE; }
	| fixed_part_value optional_variant_part_value
		{ $$ = build_tree_list ($1, $2); }
	| variant_part_value
		{ $$ = build_tree_list (NULL_TREE, $1); }
	;

fixed_part_value :
	  field_value_list
	;

field_value_list :
	  field_value
	| field_value_list semi field_value
	;

field_value :
	  field_identifier_list ':' component_value
	;

optional_variant_part_value :
		{ $$ = NULL_TREE; }
	| variant_part_value
	;

variant_part_value :
	  CASE optional_tag_field_identifier static_expression
	  OF field_list_value
		{ }
	;		

optional_tag_field_identifier :
		{ $$ = NULL_TREE; }
	| field_identifier
	;

field_identifier_list :
	  field_identifier
	| field_identifier_list ',' field_identifier
	;

field_identifier :
	  identifier
	;
/* @@@@ The above is not implemented */ 

simple_initializer :
	  static_expression
		{
		  /* Borland style */
     		  $$ = build_tree_list (NULL_TREE, $1);
		  /* @@@@ EP allows initializer expressions?? */
		  if (pedantic)
		    warning ("ISO Pascal requires index or field name in initializer");
		}
	| identifier ':' static_expression
		{ $$ = build_tree_list ($1, $3); }
	| number ':' static_expression
		{ $$ = build_tree_list ($1, $3); }
	;

/* @@@ Iso pascal does not support '[' initializer_list ']' so I took
       it out because of the s/r && r/r conflict it caused
 */
structured_initializer :
	  '(' initializer_list ')'
		{
		  if (pedantic)
		    warning ("ISO Pascal does not support this initialization");
		  $$ = $2;
		}
	| '(' error ')'
		{
		  if (pedantic)
		    warning ("ISO Pascal does not support this kind of initialization");
		  $$ = NULL_TREE;
		}
	;

initializer_separator :
	  ';'
	| ','
		{ if (pedantic)
		    warning ("ISO Pascal wants initializer components separated with `;'");
		}
	;


initializer_list :
	  initializer_expression
	| initializer_list initializer_separator initializer_expression
		{ $$ = chainon ($1, $3); }
	| initializer_list error initializer_expression
		{
		  error ("missing separator");
		  $$ = chainon ($1, $3);
		}
	;


/* PROCEDURE == void_type_node returning function */
/* FUNCTION */

function_declaration :
	  function_heading semi directive_list semi
		{
		  if (is_known_directive ($3))
		    grok_directive (TREE_VALUE ($1), TREE_PURPOSE ($1), $3,
				    ! global_bindings_p());
		  else
		    error ("A declaration part or a directive expected `%s' given",
			   IDENTIFIER_POINTER (TREE_VALUE ($3)));

		  current_type_name = NULL_TREE;
		  defining_methods = 0;
		  disable_keyword ("Inherited");
	      	}
	| function_heading semi
		{
		  tree qual    = TREE_VALUE ($1);
		  tree heading = TREE_PURPOSE ($1);

		  associate_external_objects (current_module->parms);

		  if (defining_methods 
		      && ! top_level_p (current_module->main_program))
		    error ("method definition only allowed at top level");

		  /* set flag if we are defining a nested function */
		  $<itype>2 = ! global_bindings_p ();

		  if ($<itype>2)
		    push_c_function_context ();

		  if (!current_module->main_program
		      && ! this_is_an_interface_module /* @@@ Can't be. */
		      && TREE_CODE (TREE_OPERAND (heading, 0)) == IDENTIFIER_NODE
		      && ! name_exported_p (TREE_OPERAND (heading, 0)))
		    qual = maybe_make_static (qual);

		  if (! ($<ttype>$ =  start_pascal_function (qual,
							     heading,
							     $<itype>2)))
		    {
		      if ($<itype>2)
			pop_c_function_context ();
		      YYERROR1;
		    }
		  reinit_parse_for_function ();
		  store_parm_decls ();
		}
	  pushlevel1
  		{
		  /* Declare the function RESULT_DECL */
		  tree rtype = $<ttype>3;

		  /* -O optimized result assignments away,
		   *  so I made it volatile
		   */
		  if (rtype != void_type_node)
		    finish_decl (
		       start_decl
			    (TREE_CHAIN ($1),
			     chainon (build_tree_list (NULL_TREE, volatile_id),
				      build_tree_list (NULL_TREE, rtype)),
			     0, NULL_TREE, NULL_TREE),
		       NULL_TREE,
		       NULL_TREE);
		  push_label_level ();
		  if (defining_methods)
		    {
		      tree self = lookup_name (self_name_node);
		      self = build_indirect_ref (self, "VAR parameter ref");
		      shadow_record_fields (self);
		    }
		}
	  any_declaration_part
		{
	          un_initialize_block (getdecls (), 0);
		}
	  statement_part semi
		{
		  tree result = DECL_RESULT (current_function_decl);
		  un_initialize_block (getdecls (), 1);
		  if (defining_methods)
		    poplevel (0, 0, 0);
		  pop_label_level ();
		  if (TREE_TYPE (result) != void_type_node)
		    {
		      tree d;
		      /* I do it like this, since if the code
			 directly assigns to DECL_RESULT it will lose
			 on recursive functions.
			     
			 TREE_CHAIN($1) contains the unique name for
			 the return value parameter of this function.
		       */
		      if (! TREE_CHAIN ($1)
			  || !(d = lookup_name (TREE_CHAIN ($1))))
			abort ();

		      if (   ! PASCAL_VALUE_ASSIGNED (result)
			  && ! PASCAL_VALUE_ASSIGNED (d))
			warning ("return value of function not assigned");

		      expand_expr_stmt
			(build_modify_expr (result, NOP_EXPR, d));
		    }
	        }
	  poplevel1
		{
		  finish_function ($<itype>2);
		  if ($<itype>2)
		    pop_c_function_context ();

		  current_type_name = NULL_TREE;
		  defining_methods = 0;
		  defining_operators = 0;
		  disable_keyword ("Inherited");
		}
	;

/* @@@@@@ Check if parameters of method definition match with previous
 * declaration in object type.  Get parameters from there if omitted here.
 */
function_heading :
	  optional_inline
	  PROCEDURE
	  function_identifier 
	  optional_par_formal_parameter_list
		{
		  $$ = tree_cons (build_nt (CALL_EXPR, $3, $4, NULL_TREE),
				  tree_cons (NULL_TREE, void_type_node, $1),
				  NULL_TREE);
		}
	| optional_inline
	  OP_CONSTRUCTOR
	  method_identifier 
	  optional_par_formal_parameter_list
		{
		  $$ = tree_cons (build_nt (CALL_EXPR, $3, $4, NULL_TREE),
				  tree_cons (NULL_TREE, void_type_node, $1),
				  NULL_TREE);
		}
	| optional_inline
	  OP_DESTRUCTOR
	  method_identifier  
	  optional_par_formal_parameter_list
		{
		  $$ = tree_cons (build_nt (CALL_EXPR, $3, $4, NULL_TREE),
				  tree_cons (NULL_TREE, void_type_node, $1),
				  NULL_TREE);
		}
	| optional_inline
	  FUNCTION 
	  function_identifier
	  optional_par_formal_parameter_list 
	  optional_retval_def
	  functiontype
		{
		  tree retval = $5;
		  tree funtype = $6;

		  if (funtype == NULL_TREE) {
		    /* Only allowed when function has already been declared with
		     * a directive. This will be altered to the real return type.
		     */

		    tree decl = lookup_name ($3);
		    funtype = void_type_node;

		    if (! decl || TREE_CODE (decl) != FUNCTION_DECL)
		      error ("type of function `%s' undefined",
			     IDENTIFIER_POINTER ($3));
		  }

		  /* If user did not specify a name, we create one. */
		  if (retval == NULL_TREE)
		    retval =
		      get_identifier
			(concat ("retval_", IDENTIFIER_POINTER ($3), ""));

		  $$ = tree_cons (build_nt (CALL_EXPR, $3, $4, NULL_TREE),
				  tree_cons (NULL_TREE, funtype, $1),
				  retval);
		}
	| optional_inline
	  PXSC_OPERATOR 
		{ defining_operators = 1; }
	  operator_identifier
	  optional_par_formal_parameter_list
	  optional_retval_def
	  functiontype
		{
		  tree retval = $6;
		  tree funtype = $7;

		  current_type_name = NULL_TREE;

		  if (pedantic)
		    warning ("ISO Pascal does not allow operator definitions");

		  if (funtype == NULL_TREE)
		    { error ("return type of operator undefined"); }

		  if (! $5
		      || ! TREE_CHAIN ($5)
		      || ! TREE_CHAIN (TREE_CHAIN ($5))
		      || ! TREE_CHAIN (TREE_CHAIN (TREE_CHAIN ($5)))
		      || TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (TREE_CHAIN ($5)))))
		    error ("operator must have two arguments");
		  else
		    $4 = get_operator_identifier (IDENTIFIER_POINTER ($4),
						  TREE_CHAIN ($5),
						  TREE_CHAIN (TREE_CHAIN ($5)));

		  if (retval == NULL_TREE)
		    {
		      if (pedantic)
			warning ("PXSC requires operator result variable specification");
		      retval =
			get_identifier
			  (concat ("retval_", IDENTIFIER_POINTER ($4), ""));
		    }

		  $$ = tree_cons (build_nt (CALL_EXPR, $4, $5, NULL_TREE),
				  tree_cons (NULL_TREE, funtype, $1),
				  retval);
		}
	;

optional_inline_1 :
	  INLINE
	| BP_INLINE
	;		

optional_inline :
	  /* empty */
		{ $$ = NULL_TREE; }
	| 	
		{ enable_keyword ("Inline"); }
	  optional_inline_1
		{ $$ = build_tree_list (NULL_TREE, inline_id);
		  disable_keyword ("Inline");
		}
	;

function_identifier : 
	  new_identifier
		{
		  current_type_name = NULL_TREE;
		  $$ = $1;
		}
	| method_identifier
	;

method_identifier :
	  new_identifier '.' new_identifier
		{
		  current_type_name = $1;
		  defining_methods = 1;
		  enable_keyword ("Inherited");
		  $$ = get_method_name ($1, $3); 
		}
	;

operator_identifier :
	  new_identifier
		{ $$ = $1; }
	| '+'
		{ $$ = get_identifier ("plus"); }
	| '-'
		{ $$ = get_identifier ("minus"); }
	| '*'
		{ $$ = get_identifier ("mult"); }
	| '/'
		{ $$ = get_identifier ("rdiv"); }
	| DIV
		{ $$ = get_identifier ("div"); }
	| MOD
		{ $$ = get_identifier ("mod"); }
	| IN
		{ $$ = get_identifier ("in"); }
	| '<'
		{ $$ = get_identifier ("lt"); }
	| '='
		{ $$ = get_identifier ("eq"); }
	| '>'
		{ $$ = get_identifier ("gt"); }
	| NEQ
		{ $$ = get_identifier ("neq"); }
	| GTE
		{ $$ = get_identifier ("gte"); }
	| LTE
		{ $$ = get_identifier ("lte"); }
	| OR
		{ $$ = get_identifier ("or"); }
	| AND
		{ $$ = get_identifier ("and"); }
	| CEILPLUS
		{ $$ = get_identifier ("ceilplus"); }
	| CEILMINUS
		{ $$ = get_identifier ("ceilminus"); }
	| CEILMULT
		{ $$ = get_identifier ("ceilmult"); }
	| CEILRDIV
		{ $$ = get_identifier ("ceilrdiv"); }
	| FLOORPLUS
		{ $$ = get_identifier ("floorplus"); }
	| FLOORMINUS
		{ $$ = get_identifier ("floorminus"); }
	| FLOORMULT
		{ $$ = get_identifier ("floormult"); }
	| FLOORRDIV
		{ $$ = get_identifier ("floorrdiv"); }
	/*
	 * The remaining operators such as "pow" or "shl" belong to
	 * new_identifer.  To include them here would cause reduce/reduce
	 * conflicts but is unnecessary for the same reason.
	 */
	;

directive_or_identifier :  /* For simple error recovery (?) */
	  directive
	| IDENTIFIER
	;

directive_list :
	  directive_or_identifier
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| directive_list semi directive
		{ $$ = chainon ($1, build_tree_list (NULL_TREE, $3)); }

directive :
	  D_FORWARD
	| D_ASMNAME string_constant
		{
		  DECL_ASSEMBLER_NAME ($1) = $2;
		  $$ = $1;
		}
	| D_ATTRIBUTE attribute_list
		{
		  DECL_MACHINE_ATTRIBUTES ($1) = $2;
		  $$ = $1;
		}
	| D_ATTRIBUTE '(' attribute_list ')'
		{
		  DECL_MACHINE_ATTRIBUTES ($1) = $3;
		  $$ = $1;
		}
	| D_C
	| D_C_LANGUAGE
	| D_EXTERNAL
	| D_EXTERN
	| D_STATIC
	;

attribute_list:
      attrib
                { $$ = $1; }
        | attribute_list ',' attrib
                { $$ = chainon ($1, $3); }
        ;

attrib:
    /* empty */
                { $$ = NULL_TREE; }
        | any_word
                { $$ = build_tree_list ($1, NULL_TREE); }
/*
 * This was in c-parse.y, but caused a shift/reduce conflict. - PG
 *
        | any_word '(' IDENTIFIER ')'
                { $$ = build_tree_list ($1, build_tree_list (NULL_TREE, $3)); }
 */
        | any_word '(' IDENTIFIER ',' expression ')'
                { $$ = build_tree_list ($1, tree_cons (NULL_TREE, $3, $5)); }
        | any_word '(' expression ')'
                { $$ = build_tree_list ($1, $3); }
        ;

any_word:
	  new_identifier
		{ $$ = de_capitalize ($1); }
        ;

optional_retval_def :
	  /* empty */
		{ $$ = NULL_TREE; }
	| new_identifier
		{
		  if (pedantic && ! defining_operators)
		    warning ("ISO 10206 Pascal requires `=' before function result variable specification");
		  $$ = $1;
		}
	| '=' new_identifier
		{
		  if (pedantic)
		    if (defining_operators)
		      warning ("Pascal-SC does not allow `=' before operator result variable specification");
		    else
		      warning ("ISO 7185 Pascal does not allow function result variable specification");
		  $$ = $2;
		}
	;

functiontype :
	  /* empty */
		{ $$ = NULL_TREE; }
	| ':' typename
		{ $$ = $2; /* if with a initial value: Should it init the function? */}
/* (causes 3 shift/reduce conflicts) */
	| error
		{ $$ = void_type_node; }
	;


/* parameter specification section */

optional_par_formal_parameter_list :
		{ pushlevel (0);
		  clear_parm_order ();
	          declare_parm_level (1);
		  enable_keyword ("Protected");
		}
	  parmlist1
		{ $$ = $2;
		  parmlist_tags_warning ();
		  poplevel (0, 0, 0);
		  disable_keyword ("Protected");
		}

parmlist1 :
	  /* empty */
		{
		  if (defining_methods)
		    push_self_parameter (current_type_name, defining_types);
		  $$ = get_parm_info (1);
		}
	| '('
		{
		  if (defining_methods)
		    push_self_parameter (current_type_name, defining_types);
		}
	  parmlist2
		{ $$ = $3 }
	;

/* splitted to avoid 131 reduce/reduce conflicts
 * which came with the action after the '('
 */

parmlist2 :
	  formal_parameter_list ')'
		{ $$ = get_parm_info (1); }
	| formal_parameter_list optional_semicolon ELLIPSIS ')'
		{
		  if (pedantic)
		    warning ("ISO Pascal denies variable number of arguments");
		  $$ = get_parm_info (0);
		}
	| ELLIPSIS ')'
		{
		  if (pedantic)
		    warning ("ISO Pascal denies variable number of arguments");
		  $$ = get_parm_info (0);
		}
	| error ')'
		{ $$ = tree_cons (NULL_TREE, NULL_TREE, NULL_TREE); }
	;

formal_parameter_list :
	  formal_parameter
	| formal_parameter_list semi formal_parameter
  		{ yyerrok; }
/* (causes a shift/reduce conflict) */
	| error
/* (causes 4 shift/reduce conflicts) */
	| formal_parameter_list error formal_parameter
               {
		 error ("missing semicolon");
                 yyerrok;
	       }
	| formal_parameter_list semi error
	;

formal_parameter :
	  optional_protected id_list ':' parameter_form
		{ handle_formal_param_list ($2, $4, 0, $1); }
	| CONST id_list
		{ 
		  if (pedantic)
		    warning ("ISO Pascal does not allow untyped CONST parameters");
		  handle_formal_param_list ($2, void_type_node, 1, 1);
		}
	| CONST id_list ':' parameter_form
		{ 
		  if (pedantic)
		    warning ("ISO Pascal does not allow CONST parameters");
		  handle_formal_param_list ($2, $4, 1, 1);
		}
	| optional_protected VAR id_list
		{
		  if (pedantic)
		    warning ("ISO Pascal does not allow untyped VAR parameters");
		  handle_formal_param_list ($3, void_type_node, 1, $1);
		}
	| optional_protected VAR id_list ':' parameter_form
		{ handle_formal_param_list ($3, $5, 1, $1); }
	| function_heading
		{ handle_formal_param_list (TREE_PURPOSE ($1), TREE_VALUE ($1), 0, 0); }
	| optional_protected id_list ':' conformant_array_schema
		{ handle_formal_conf_array_param_list ($2, $4, 0, $1); }
	| CONST id_list ':' conformant_array_schema
		{ 
		  if (pedantic)
		    warning ("ISO Pascal does not allow CONST parameters");
		  handle_formal_conf_array_param_list ($2, $4, 1, 1);
		}
	| optional_protected VAR id_list ':' conformant_array_schema
		{ handle_formal_conf_array_param_list ($3, $5, 1, $1); }
	;

optional_protected :
	  /* Empty */
		{ $$ = 0; }
	| PROTECTED
		{ $$ = 1; }
	;

/* String schema is a special case: It is the only schema type that does not
 * need to be defined by the user, so TYPENAME will not find it
 *
 */
parameter_form :
	  typename
	| type_inquiry
	| open_array
	| STRING_SCHEMA
	        { $$ = string_schema_proto_type; }
	;

conformant_array_schema :
	  packed_conformant_array_schema
	| unpacked_conformant_array_schema
	;

/* Packed conformant array formal parameter */
packed_conformant_array_schema :
	  PACKED ARRAY lbracket index_type_specification rbracket
  	           OF type_denoter
		{ $$ = build_tree_list (error_mark_node,
					build_tree_list ($4, TREE_VALUE ($7)));
		}
	;

/* Unpacked conformant array formal parameter */
unpacked_conformant_array_schema :
	  ARRAY lbracket index_type_specification_list rbracket
  	    OF type_denoter
		{ $$ = build_tree_list (NULL_TREE,
					build_tree_list ($3, TREE_VALUE ($6)));
		}
	;

index_type_specification :
	  new_identifier TWODOTS new_identifier ':' typename
  		{ $$ = build_tree_list (build_tree_list ($1, $3), $5); }
	;

index_type_specification_list :
	  index_type_specification
	| index_type_specification_list semi index_type_specification
		{ $$ = chainon ($1, $3);
		  yyerrok; }
/*	| error
*/
	| index_type_specification_list error index_type_specification
		{ error ("missing semicolon");
		  yyerrok; }
	| index_type_specification_list semi error
	;

/* Open array formal parameter */
open_array :
	  ARRAY OF type_denoter
		{
		  /*
		   * Index range 0..1 is inserted, and the array
		   * is flagged.
		   */
		  tree index = build_range_type (integer_type_node,
		    integer_zero_node, integer_one_node);
		  index = build_tree_list (index,
		                           convert_type_to_range (index));
		  $$ = build_pascal_array_type (TREE_VALUE ($3), index);
		  PASCAL_TYPE_OPEN_ARRAY ($$) = 1;
		}
	;

/* statement part */

statement_part :
	  compound_statement
  		{ stmt_count++; }
	;

compound_statement :
	  BEGIN pushlevel statement_sequence poplevel END
               { yyerrok; }
	;

statement_sequence :
	  lineno_statement
	| statement_sequence semi lineno_statement
              { yyerrok; }
/* (causes a shift/reduce conflict) */
	| statement_sequence error lineno_statement
              { error ("missing semicolon"); }
/* (causes a shift/reduce conflict - see above) */
	| statement_sequence semi error
              { error ("extra semicolon"); /* @@ Perhaps too innovative :-) */
                yyerrok; }
	;

save_filename :
		{ $$ = input_filename; }
	;

save_lineno :
		{ $$ = lineno; }
	;

lineno_statement :
	  save_filename save_lineno statement
  		{ }
	;

statement :
	  label ':'
		{ tree label =
		   define_label (input_filename, lineno, $1);
		  stmt_count++;
		  emit_nop ();
		  if (label)
		    expand_label (label);
		  position_after_white_space (); }
	  save_filename save_lineno unlabelled_statement
	| unlabelled_statement
	| declaring_statement
/* (causes a shift/reduce conflict) */
	| error
	;

/* GPC extension */
declaring_statement :
	  VAR id_list ':'
		{ enable_keyword ("Absolute"); }
	  optional_qualifier_list type_denoter absolute_or_value_specification
		{ tree init = $7;

		  if (init)
                    init = build_pascal_initializer ($6, init);
		  else
		    init = TREE_PURPOSE ($6); /* Try type initialization */

		  if (pedantic)
		    warning ("ISO Pascal does not allow variable declarations in statement part");
		  declare_vars ($2, TREE_VALUE ($6), init, $5, 0);
		  /* disable_keyword ("Absolute") is done by
		   * absolute_or_value_specification.
		   */
		}
	;

unlabelled_statement :
	  structured_statement
	| simple_statement
	;

/* STRUCTURED STATEMENT */
structured_statement :
	  compound_statement
	| with_statement
	| conditional_statement
	| repetitive_statement
	;

/* WITH STATEMENT */
with_statement :
	  WITH structured_variable_list DO lineno_statement
		{ int i;
		  for (i = $2; i ;i--)
		     poplevel (0, 0, 0);
		}
	;

structured_variable_list :
	  structured_variable
	| structured_variable_list ',' structured_variable
		{ $$ += $3;
		  yyerrok; }
        | error
		{ $$ = 0; }
        | structured_variable_list error structured_variable
		{ error ("missing comma");
                  $$ += $3; 
                  yyerrok; }
        | structured_variable_list semi error
		{ error ("extra comma"); }
	;

structured_variable :
	  variable_or_function_access
		{ if (TREE_CODE (TREE_TYPE ($1)) != RECORD_TYPE)
		    {
		      error ("WITH element must be of record or object type");
		      $$ = 0;
		    }
		  else
		    {
	    	      shadow_record_fields ($1);
		      $$ = 1;
		    }
		}
	;

/* CONDITIONAL STATEMENT */
conditional_statement :
	  if_statement 
	| case_statement
	;

/* IF THEN ELSE (shift/reduce conflict avoided with %nonassoc rules) */

simple_if :
	  IF boolean_expression THEN_or_error
		{ emit_line_note ($<filename>-1, $<lineno>0);
		  stmt_count++;
		  expand_start_cond ($2, 0);
		  $<itype>1 = stmt_count;
		  if_stmt_file = $<filename>-1;
		  if_stmt_line = $<lineno>0;
		  position_after_white_space ();
		}
	  lineno_statement
	;

THEN_or_error :
	  THEN
        | error
                { error ("missing THEN"); }
        ;

if_statement :
	  simple_if ELSE
  		{ expand_start_else ();
		  $<itype>1 = stmt_count;
		  position_after_white_space (); }
	  lineno_statement
		{ expand_end_cond ();
		  if (extra_warnings && stmt_count == $<itype>1)
		    warning("empty body in an else statement");
		}
	| simple_if %prec IF
		{ expand_end_cond ();
		  if (extra_warnings && stmt_count == $<itype>1)
		    warning_with_file_and_line (if_stmt_file, if_stmt_line,
						"empty body in an if statement");
		}
	;

/* CASE (see also VARIANT RECORDS) */
case_statement :
	  CASE expression OF_or_error
		{ stmt_count++;
		  emit_line_note ($<filename>-1, $<lineno>0);
		  c_expand_start_case ($2);
		  /* Don't let the tree nodes for $2 be discarded by
		     clear_momentary during the parsing of the next stmt.  */
		position_after_white_space (); }
	  case_element_list optional_semicolon_or_else_branch END
		{ expand_end_case ($2);
		  yyerrok; }
	;

optional_semicolon_or_else_branch :
	  /* empty */
	| semi
	| semi ELSE
		{
		  tree duplicate;
		  register tree label
		    = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
		  int success = pushcase (NULL_TREE, 0, label, &duplicate);
		  stmt_count++;
		  if (pedantic)
		    warning ("ISO Pascal does not allow ELSE in CASE statements");
		  if (success == 1)
		    error ("ELSE not within an IF or CASE statement");
		  else if (success == 2)
		    {
		      error ("multiple default labels in one case statement");
		      error_with_decl (duplicate, "this is the first entry for that value");
		    }
		  position_after_white_space ();
		}
	  pushlevel statement_sequence poplevel
		{ expand_exit_something (); }
	;

OF_or_error :
	  OF
        | error
                { error ("missing OF"); }
        ;

case_element_list :
	  case_element
	| case_element_list semi case_element
              { yyerrok; }
        | error
              { error("case_element expected"); }
        | case_element_list 
          error case_element
              { error("missing semicolon");
                yyerrok; }
        | case_element_list 
          semi error
              { error("extra semicolon"); }
	;

/* now this looks more like a C switch statement */
case_element :
	  case_constant_list ':' 
		{ register tree link;
		  register tree value1, value2;
		  register tree label;
		  tree duplicate;

		  for (link = $1; link; link = TREE_CHAIN (link)) {
		    value1 = TREE_VALUE (link);
		    value2 = TREE_PURPOSE (link);
		    label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

		    /* @@@ use check_case_value ()??? */
		    if (TREE_CODE (value1) != INTEGER_CST
			&& value1 != error_mark_node) {
		      error ("case label does not reduce to an integer constant");
		      continue;
		    }
		    if (value2
			&& TREE_CODE (value2) != INTEGER_CST
			&& value2 != error_mark_node) {
		      error ("upper value of case range does not reduce to an integer constant");
		      continue;
		    }
		    if (value1 != error_mark_node && value2 != error_mark_node) {
		      int success;
		      if (value2) {
			  if (pedantic)
			      warning ("ISO Pascal does not allow case ranges");
			  success = pushcase_range (value1, value2, convert_and_check, label,
						    &duplicate);
		      } else
			  success = pushcase (value1, convert_and_check, label, &duplicate);
		      if (success == 1)
			error ("case label not within case statement");
		      else if (success == 2)
			{
			  error ("duplicate case label value in case statement");
			  error_with_decl (duplicate, "this is the first entry for that value");
			}
		      else if (success == 3)
			warning ("case label out of range");
		      else if (success == 4)
			warning ("empty case label range");
		    }
		  }
		  position_after_white_space ();
		}
	  lineno_statement
		{ expand_exit_something (); }
	| case_default optional_colon
		{
		  tree duplicate;
		  register tree label
		    = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
		  int success = pushcase (NULL_TREE, 0, label, &duplicate);
		  stmt_count++;
		  if (pedantic)
		    warning ("ISO Pascal does not allow default label `%s' in CASE statement",
			     IDENTIFIER_POINTER ($1));
		  if (success == 1)
		    error ("default label not within a case statement");
		  else if (success == 2)
		    {
		      error ("multiple default labels in one case statement");
		      error_with_decl (duplicate, "this is the first entry for that value");
		    }
		  position_after_white_space ();
		}
	  lineno_statement
		{ expand_exit_something (); }
	;

/* all of these are non-standard */
case_default :
	  DEFAULT
	| OTHERS
	| OTHERWISE
	;

/* REPETITIVE STATEMENT */

repetitive_statement :
	  repeat_statement
	| while_statement
	| for_statement 
	;

/* REPEAT */
repeat_statement :
	  REPEAT
		{ emit_nop ();
		  stmt_count++;
		  emit_line_note ($<filename>-1, $<lineno>0);
		  expand_start_loop_continue_elsewhere (1); }
	  statement_sequence UNTIL
		{ expand_loop_continue_here (); }
	  boolean_expression
		{ emit_line_note (input_filename, lineno);
		  expand_exit_loop_if_false (0,
			build_pascal_unary_op (TRUTH_NOT_EXPR, $6, 0));
		  expand_end_loop ();
		  /* clear_momentary (); */ }
	;

/* WHILE */
while_statement :
	  WHILE
		{ emit_nop ();
		  stmt_count++;
		  emit_line_note ($<filename>-1, $<lineno>0);
		  expand_start_loop (1); }
	  boolean_expression 
		{ emit_line_note (input_filename, lineno);
		  expand_exit_loop_if_false (0, $3); }
	  DO_or_error lineno_statement
		{ expand_end_loop (); }
	;

DO_or_error :
	  DO
       | error
            { error ("missing DO"); }
       ;

/* FOR */
/* I did not manage to do this without a temporary variable.
 *
 *
 *
 *
 * Currently this is done so that we allocate a temporary VAR_DECL
 * for the loop upper bound; and store the expression there.
 *
 * ARGV!!! Both the lower and upper bounds must be evaluated
 * before the for loop control variable is assigned. 
 * if not, the following does not work:
 *    for i:= (i+1) to (i+10) do
 *
 * So allocate another temporary variable for the lower bound...
 */
for_statement :
	  FOR identifier ASSIGN expression for_direction expression
		{ tree for_l_bound, for_u_bound;
		  int mark_low = 0;
		  int mark_high = 0;
		  emit_nop ();
		  stmt_count++;
		  emit_line_note ($<filename>-1, $<lineno>0);
		  if (TREE_CONSTANT ($4))
		    for_l_bound = $4;
		  else
		    {
		      mark_low++;
		      for_l_bound = start_decl
			(get_unique_identifier ("for_lower", 0),
			 build_tree_list
			   (NULL_TREE,
#if 0
			    type_for_size (TYPE_PRECISION (TREE_TYPE ($2)),
					   TREE_UNSIGNED (TREE_TYPE ($2)))),
#else
			    TREE_TYPE ($2)),
#endif
			 0, NULL_TREE, NULL_TREE);
		      finish_decl (for_l_bound, NULL_TREE, NULL_TREE);
		      PASCAL_LOOP_CHECK (for_l_bound) = 1;

		      /* Assign the lower bound to temp */
		      /* @@@ c_expand_expr_stmt() ??? */
		      expand_expr_stmt (build_modify_expr (for_l_bound,
							   NOP_EXPR, $4));
		    }

		  if (TREE_CONSTANT ($6))
		    for_u_bound = $6;
		  else
		    {
		      mark_high++;
		      for_u_bound = start_decl
			(get_unique_identifier ("for_upper", 0),
			 build_tree_list
			   (NULL_TREE,
#if 0
			    type_for_size (TYPE_PRECISION (TREE_TYPE ($2)),
					   TREE_UNSIGNED (TREE_TYPE ($2)))),
#else
			    TREE_TYPE ($2)),
#endif
			 0, NULL_TREE, NULL_TREE);
		      finish_decl (for_u_bound, NULL_TREE, NULL_TREE);
		      PASCAL_LOOP_CHECK (for_u_bound) = 1;

		      emit_line_note ($<filename>-1, $<lineno>0);
		      /* and the upper one */
		      expand_expr_stmt (build_modify_expr (for_u_bound,
							   NOP_EXPR, $6));
		    }
		  $<ttype>$ = for_u_bound;

		  /* necessary to prevent infinite loops when
 		   * incrementing/decrementing would cause
		   * wrap-around at maxint/-maxint
		   */
		  expand_start_cond (build_pascal_op ($5, for_l_bound,
						      for_u_bound), 0);
		  if (mark_low)
		    PASCAL_LOOP_CHECK (for_l_bound) = 0;
		  if (mark_high)
		    PASCAL_LOOP_CHECK (for_u_bound) = 0;

		  emit_line_note ($<filename>-1, $<lineno>0);
		  expand_expr_stmt (build_modify_expr ($2, NOP_EXPR, for_l_bound));
		  expand_start_loop_continue_elsewhere (1);
		}
	  DO_or_error
		{ position_after_white_space ();
		}
	  lineno_statement
		{ emit_line_note ($<filename>-1, $<lineno>0);
		  expand_loop_continue_here ();
		  expand_exit_loop_if_false (0,
		     fold (build_pascal_op
			   ($5 == LE_EXPR ? LT_EXPR : GT_EXPR,
			    $2, $<ttype>7)));
		  expand_expr_stmt (build_pascal_unary_op (
		     ($5 == GE_EXPR ? POSTDECREMENT_EXPR : POSTINCREMENT_EXPR),
		     $2, 0));
		  expand_end_loop ();
		  expand_end_cond ();
		}
	| FOR identifier IN simple_expression /* set member iteration */
		{ 
		  error ("Extended Pascal set member iteration not implemented yet");
		}
	  DO_or_error
		{ position_after_white_space (); }
	  lineno_statement
		{ }
	;

for_direction :
	  TO     
		{ $$ = LE_EXPR; }
	| DOWNTO 
		{ $$ = GE_EXPR; }
        | error
                { error("missing TO or DOWNTO"); }
	;

/* SIMPLE STATEMENT */
simple_statement :
	  empty_statement
	| goto_statement
		{ stmt_count++; }
	| assignment_or_call_statement
		{ stmt_count++; }
	| standard_function_statement
		{ stmt_count++; }
	| statement_extensions
		{ stmt_count++; }
	;

empty_statement :
	  /* empty */
	;

goto_statement :
	  GOTO label
	      	{ tree decl;
		  emit_line_note ($<filename>-1, $<lineno>0);
		  decl = lookup_label ($2);
		  if (decl != 0) {
		      TREE_USED (decl) = 1;
		      expand_goto (decl); }
	        }
	| GOTO '*' expression
		{ emit_line_note ($<filename>-1, $<lineno>0);
		  if (pedantic)
		    warning ("ISO Pascal does not allow indirect jumps");
		  expand_computed_goto (convert (ptr_type_node, $3)); }
	;

/* PROCEDURE CALL */

/***** Unused: Handled together with assignment

procedure_statement :
	  identifier optional_par_actual_parameter_list
		{ 
		  expand_expr_stmt (build_function_call ($1, $2));
		}
	;
******/

optional_par_actual_parameter_list :
	  /* empty */
		{ $$ = NULL_TREE; }
	| '('
		{ $<itype>1 = suspend_function_calls (); }
          actual_parameter_list r_paren_or_error
		{
		  $$ = $3;
		  resume_function_calls ($<itype>1);
		  yyerrok;
	        }
	;

r_paren_or_error :
	  ')'
        | error
                { error ("missing ')'"); }
        ;

actual_parameter_list :
	  actual_parameter
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| actual_parameter_list ',' actual_parameter
		{ $$ = chainon ($1, build_tree_list (NULL_TREE, $3));
		  yyerrok; }
        | error
		{ $$ = NULL_TREE; /* @@ verify this */ }
	| actual_parameter_list ',' error
                { error ("extra comma"); }
	;

actual_parameter :
	  expression
	;

/* ASSIGNMENT and procedure calls.
 *
 * These were separated before I implemented procedure and function
 * types for variables and components of structured types.
 */

/* Should also implement threatening of optional_retval_def in assignment */
assignment_or_call_statement :
	  variable_or_function_access_maybe_assignment rest_of_statement
		{ tree target = $1;
		  tree source = $2; /* In assignment != NULL_TREE */

		  if (! source)
		    {
		      if (!target)
			{
			  /* This is a procedure call, so the target
			     is NULL_TREE.
			   */			     
			  int whatever = 0;
			}
		      else
		        {
		          if (MAYBE_CALL_FUNCTION (target))
		            target = maybe_call_function (target, NULL_TREE);
                          else if (CALL_METHOD (target))
                            target = call_method (target, NULL_TREE);

		          if (target != error_mark_node)
			    {
			      if (TREE_CODE (target) != CALL_EXPR)
			        warning ("expression used as a statement -- value is ignored");
			      else if (TREE_TYPE (target) != void_type_node
		                       && ! flag_extended_syntax)
			        warning ("function call as a statement -- value is ignored");

			      expand_expr_stmt (target);
			    }
		        }
		    }
		  else
		    {
		      int allow_restricted_target = FALSE;
		      int allow_restricted_source = FALSE;

		      if (CALL_METHOD (target) && current_type_name)
		        {
		          target = TREE_OPERAND (target, 1);
		          target = lookup_name (get_method_name (current_type_name,
		                                                 DECL_NAME (target)));
		        }
		      if (TREE_CODE (target) == FUNCTION_DECL)
			{
			  if (TREE_TYPE (TREE_TYPE (target)) == void_type_node)
			    {
			      error ("You can't assign to a procedure");
			      target = error_mark_node;
			    }
			  else
			    {
			      if (target == current_function_decl)
				current_function_returns_value = 1;
			      else if (! maybe_find_function_data (target))
				{
				  error ("Function `%s' value assigned outside it's block",
					 IDENTIFIER_POINTER (DECL_NAME (target)));
				  target = error_mark_node;
				}
			      if (target != error_mark_node)
				{
				  target =
				    lookup_name
				      (get_identifier
				       (concat
					("retval_",
					 IDENTIFIER_POINTER (DECL_NAME (target)),
					 "")));
				  if (! target)
				    {
				      error ("Invalid assignment of function value");
				      target = error_mark_node;
				    }

				  /* Allow assignment of a non-restricted type
				   * to a restricted function return value
				   */
				  allow_restricted_target = TRUE;
				}
			    }
			} 

		      if (target != error_mark_node)
			{
			  /* @@@@ Maybe this needs further checking */
			  if (TREE_CODE (source) == CALL_EXPR
			      && PASCAL_TYPE_RESTRICTED (TREE_TYPE (source)))
			    {
			      allow_restricted_source = TRUE;
			      allow_restricted_target = TRUE;
			      
			      if (!PASCAL_TYPE_RESTRICTED (TREE_TYPE (target)))
				error ("A restricted return value may only be assigned to a restricted type object");
			    }

		          /* Warn about Borland Pascal "typed const" misuse */
		          if (PASCAL_TYPE_TYPEDCONST (TREE_TYPE (target)))
		            {
		              static int informed = 0;
		              warning ("typed const misused as initialized variable");
		              if (! informed)
		                {
		                  warning ("(Better use ISO 10206 Extended Pascal initialized");
		                  warning ("types and variables: var foo: integer value 7)");
		                  informed++;
		                }
		              /* It is cruel enough to get this warning once
		               * for each typed const.
		               */
		              PASCAL_TYPE_TYPEDCONST (TREE_TYPE (target)) = 0;
		            }

			  /* To mark that we have assigned to this
			     variable.  Currently only used to flag
			     function value assignments, but it cannot
			     be inside the FUNCTION_DECL conditional
			     above, since GPC allows you to define a
			     name for the return value of the
			     function, and that name is not (at least
			     currently) recorded in the function
			     declaration. GPC assigns a VAR_DECL node
			     for the name. */
			     
			  if (TREE_CODE (target) == VAR_DECL)
			    PASCAL_VALUE_ASSIGNED (target) = 1;

			  emit_line_note (input_filename, lineno);

			  if ((!allow_restricted_target
			       && PASCAL_TYPE_RESTRICTED (TREE_TYPE (target)))
			      || (!allow_restricted_source
				  && PASCAL_TYPE_RESTRICTED (TREE_TYPE (source))))
			    error ("Assigning a restricted type object is not allowed");

			  /* @@ Test new constructor code without this */
			  
			  /* Construct a set directly to the set variable */
			  if (TREE_CODE (TREE_TYPE (target)) == SET_TYPE &&
			      TREE_CODE (source) == CONSTRUCTOR)
			    source = construct_set (source, target, 0);
			  
			  if (source)
			    {
			      /* handle char, vstring and fstring mixing */
			      if (is_string_type (target)
				  || is_string_type (source))
				assign_string (target, source);
			      else
				expand_expr_stmt
				  (build_modify_expr (target,
						      NOP_EXPR, source));
			    }
			}
		    } /* "if (! source) {} else {}" ends here */
		}
	;

variable_or_function_access_maybe_assignment :
	  identifier
		{
		  if (IS_ABSOLUTE_VAR ($1))
		    $$ = build_indirect_ref (DECL_INITIAL ($1),
		                             "ABSOLUTE variable access");
		  else if (TREE_CODE (TREE_TYPE ($1)) == REFERENCE_TYPE)
		    $$ = build_indirect_ref ($1, "VAR parameter ref"); 
		  else
		    $$ = $1;
		}
	| variable_or_function_access_no_id
	;

rest_of_statement :
	  /* Empty */
		{ $$ = NULL_TREE; }
	| assign_operator expression
		{ $$ = $2; }
	;	   

assign_operator :
  	  ASSIGN
	| '='
		{ warning ("Using '=' instead of ':=' in assignment");
		  $<code>$ = $1;
		}
  	;

standard_function_statement :
	  p_HALT optional_par_actual_parameter
		{ if (pedantic && $2 != NULL_TREE)
		      warning ("Extended Pascal does not allow parameters to HALT");
		  build_rts_call (p_HALT, $2); }
	| rts_file_open '(' actual_parameter optional_filename r_paren_or_error
		{ if (pedantic && $4 != NULL_TREE)
		    warning ("ISO Pascal does not allow file name parameter to reset/rewrite");
		  build_rts_call ($1, 
				 tree_cons (NULL_TREE, $3,
				            build_tree_list (NULL_TREE, $4)));
		}
	| rts_proc_onepar '(' actual_parameter r_paren_or_error
		{ build_rts_call ($1, build_tree_list(NULL_TREE, $3)); }
	| rts_proc_parlist '(' actual_parameter_list r_paren_or_error
		{ build_rts_call ($1, $3); }
	| p_WRITE '(' write_actual_parameter_list r_paren_or_error
		{ build_rts_call (p_WRITE, $3); }
	| p_WRITELN optional_par_write_parameter_list
		{ build_rts_call (p_WRITELN, $2); }
	| p_READLN optional_par_actual_parameter_list
		{ build_rts_call (p_READLN, $2); }
        /* Extended pascal write to string / read from string */
	| p_WRITESTR '(' write_actual_parameter_list r_paren_or_error
		{ build_rts_call (p_WRITESTR, $3); }
	| p_READSTR '(' actual_parameter_list r_paren_or_error
		{ build_rts_call (p_READSTR, $3); }
	| p_DISPOSE '(' actual_parameter r_paren_or_error
		{ build_rts_call (p_DISPOSE, build_tree_list (NULL_TREE, $3));
		}
	| p_DISPOSE '(' actual_parameter ','
		{
		  if (TREE_CODE (TREE_TYPE ($3)) == POINTER_TYPE
                      && IS_OBJECT_TYPE (TREE_TYPE (TREE_TYPE ($3))))
		    shadow_record_fields (build_indirect_ref ($3, "`Dispose'"));
		}
	  actual_parameter_list r_paren_or_error
		{
		  if (TREE_CODE (TREE_TYPE ($3)) == POINTER_TYPE
                      && IS_OBJECT_TYPE (TREE_TYPE (TREE_TYPE ($3))))
		    {
		      expand_expr_stmt (TREE_VALUE ($6));
		      poplevel (0, 0, 0);
		      if (TREE_CHAIN ($6))
		        warning ("extra arguments ignored in `Dispose'");
		      build_rts_call (p_DISPOSE, build_tree_list (NULL_TREE, $3));
		    }
		  else
		    build_rts_call (p_DISPOSE,
                      chainon (build_tree_list (NULL_TREE, $3), $6));
		}
	;

rts_file_open :
	  p_REWRITE
		{ $$ = p_REWRITE; }
	| p_RESET
		{ $$ = p_RESET; }
	| p_EXTEND
		{ $$ = p_EXTEND; }
	;

optional_filename :
	  /* empty */
		{ $$ = null_pointer_node; }
	| ',' actual_parameter
		{ $$ = $2; }
	;

optional_par_write_parameter_list :
	  /* empty */
		{ $$ = NULL_TREE; }
	| '(' write_actual_parameter_list r_paren_or_error
		{ $$ = $2;
		  yyerrok; }
	;

write_actual_parameter_list :
	  write_actual_parameter
	| write_actual_parameter_list ',' write_actual_parameter
		{ $$ = chainon ($1, $3);
		  yyerrok; }
        | error
		{ error ("missing write_actual_parameter");
                  $$ = NULL_TREE; }
	| write_actual_parameter_list error write_actual_parameter
		{ error ("missing comma");
                  $$ = chainon ($1, $3); 
                  yyerrok; }
	| write_actual_parameter_list ',' error
                { error ("extra comma"); }
	;


/* how to represent the : expressions? */
/* yes, as a list in TREE_PURPOSE of each actual parameter */
write_actual_parameter :
	  actual_parameter
		{ $$ = build_tree_list (NULL_TREE, $1); }
 	| actual_parameter ':' expression
		{ $$ = build_tree_list (build_tree_list (NULL_TREE, $3), $1); }
	| actual_parameter ':' expression ':' expression
		{ $$ = build_tree_list (build_tree_list ($5, $3), $1); }
	;

/* run time system calls with one parameter */
rts_proc_onepar :
	  p_PUT
		{ $$ = p_PUT; }
	| p_GET
		{ $$ = p_GET; }
	| p_PAGE
		{ $$ = p_PAGE; }
	| p_MARK
		{ $$ = p_MARK; }
	| p_RELEASE
		{ $$ = p_RELEASE; }
	| p_CLOSE
		{ $$ = p_CLOSE; }
	| p_UPDATE
		{ $$ = p_UPDATE; }
	| p_GETTIMESTAMP
		{ $$ = p_GETTIMESTAMP; }
	| p_UNBIND
		{ $$ = p_UNBIND; }
	;

rts_proc_parlist :
	  p_READ	/* varargs */
		{ $$ = p_READ; }
	| p_PACK	/* Three args */
		{ $$ = p_PACK; }
	| p_UNPACK	/* Three args */
		{ $$ = p_UNPACK; }
	| p_BIND	/* Two args */
		{ $$ = p_BIND; }
	| p_SEEKREAD
		{ $$ = p_SEEKREAD; }
	| p_SEEKWRITE
		{ $$ = p_SEEKWRITE; }
	| p_SEEKUPDATE
		{ $$ = p_SEEKUPDATE; }
/* Borland extensions */
	| bp_FREEMEM     /* Two args */
		{ $$ = bp_FREEMEM; }
	| bp_INC         /* One or two args */
		{ $$ = bp_INC; }
	| bp_DEC         /* One or two args */
		{ $$ = bp_DEC; }
/* GPC extension: Define random access file size */
	| p_DEFINESIZE  /* Two args */
		{ $$ = p_DEFINESIZE; }
/* GPC extensions: AND, OR, NOT, and XOR as "procedures" */
        | AND           /* Two args */
		{ $$ = AND; }
        | OR            /* Two args */
		{ $$ = OR; }
        | NOT           /* One arg */
		{ $$ = NOT; }
        | BP_XOR        /* Two args */
		{ $$ = BP_XOR; }
	;

/* statement extensions to ISO Pascal */
statement_extensions :
	  return_statement
	| continue_statement
	| break_statement
	| asm_statement
	;

return_statement :
	  RETURN
		{ emit_line_note ($<filename>-1, $<lineno>0);
		  if (pedantic)
		    warning ("ISO Pascal does not allow return statement");
		  c_expand_return (NULL_TREE); }
	| RETURN expression
		{ emit_line_note ($<filename>-1, $<lineno>0);
		  if (pedantic)
		    warning ("ISO Pascal does not allow return statement");
		  current_function_returns_value = 1;
		  c_expand_return ($2); }
	;

break_statement :
	  BREAK
		{ emit_line_note ($<filename>-1, $<lineno>0);
		  if (pedantic)
		    warning ("ISO Pascal does not allow break statement");
		  if ( ! expand_exit_something ())
		    error ("break statement not within loop or case"); }
	;

continue_statement :
	  CONTINUE
		{ emit_line_note ($<filename>-1, $<lineno>0);
		  if (pedantic)
		    warning ("ISO Pascal does not allow continue statement");
		  if (! expand_continue_loop (0))	/* @@@ NP */
		    error ("continue statement not within a loop"); }
	;

asm_statement :
	  ASM asm_qualifier '(' string ')'
		{ emit_line_note ($<filename>-1, $<lineno>0);
		  if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  expand_asm ($4); }
	/* This is the case with just output operands.  */
	| ASM asm_qualifier '(' string ':' asm_operands ')'
		{ emit_line_note ($<filename>-1, $<lineno>0);
		  if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  c_expand_asm_operands ($4, $6, NULL_TREE, NULL_TREE,
					 0, input_filename, lineno); }
	/* This is the case with input operands as well.  */
	| ASM asm_qualifier '(' string ':' asm_operands ':' asm_operands ')'
		{ emit_line_note ($<filename>-1, $<lineno>0);
		  if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  c_expand_asm_operands ($4, $6, $8, NULL_TREE,
					 0, input_filename, lineno); }
	/* This is the case with clobbered registers as well.  */
	| ASM asm_qualifier '(' string ':' asm_operands ':' asm_operands ':' asm_clobbers ')'
		{ emit_line_note ($<filename>-1, $<lineno>0);
		  if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
 		  c_expand_asm_operands ($4, $6, $8, $10,
					 0, input_filename, lineno); }
	;

/* Now yet decided what to do */
asm_qualifier :
  	  /* empty */
	        { if (pedantic)
		    warning ("ISO Pascal forbids use of `asm' keyword"); } 
	;

/* These are the operands other than the first string and colon
   in  asm ("addextend %2,%1": "=dm" (x): "0" (y), "g" (*x))  */
asm_operands :
	  /* empty */
		{ $$ = NULL_TREE; }
	| nonnull_asm_operands
	;

nonnull_asm_operands :
	  asm_operand
	| nonnull_asm_operands ',' asm_operand
		{ $$ = chainon ($1, $3); }
	;

asm_operand :
	  STRING_LITERAL '(' expression ')'
		{ $$ = build_tree_list ($1, $3); }
	;

asm_clobbers :
	  string
		{ $$ = tree_cons (NULL_TREE, $1, NULL_TREE); }
	| asm_clobbers ',' string
		{ $$ = tree_cons (NULL_TREE, $3, $1); }
	;


/* For sizeof, alignof, New and GetMem */
variable_access_or_typename :
	  variable_or_function_access_no_id
	| IDENTIFIER
		{ if (lastiddecl != 0 && TREE_CODE (lastiddecl) == TYPE_DECL)
		    $$ = lastiddecl;
		  else {
		    tree id = check_identifier (lastiddecl, $1);
		    if (TREE_CODE (TREE_TYPE (id)) == REFERENCE_TYPE)
		      $$ = build_indirect_ref (id, "VAR parameter ref"); 
		    else
		      $$ = id;
		  }
		}
	;

index_expression_list :
	  expression
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| expression TWODOTS expression		/* SUBSTRING ACCESS */
		{ $$ = build_tree_list ($3, $1); }
	| index_expression_list ',' expression
		{ $$ = chainon ($1, build_tree_list (NULL_TREE, $3));
		  yyerrok; }
        | error
		{ error ("missing expression");
                  $$ = NULL_TREE; }
        | index_expression_list error expression
 		{ error ("missing comma");
                  $$ = chainon ($1, build_tree_list (NULL_TREE, $3)); 
                  yyerrok; }
	| index_expression_list ',' error
                { error ("extra comma"); }
	;


/* expressions */

/* We should check that this really is compile time constant */
static_expression :
	  expression
	;

/* the expression result must be type boolean */
/* This should be checked */
boolean_expression :
	  expression
	;

expression :
	  simple_expression relational_operator simple_expression
		{ $$ = fold (build_pascal_op ($2, $1, $3)); }
	| simple_expression IN simple_expression
		{ $$ = fold (build_pascal_op (IN_EXPR, $1, $3)); }
	| simple_expression
		{ $$ = fold ($1); }
	;

simple_expression :
	  any_term
	| simple_expression adding_operator any_term
		{ $$ = build_pascal_op ($2, $1, $3); }
	| simple_expression pxsc_adding_operator any_term
		{ $$ = build_pxsc_operator ($2, $1, $3); }
	| simple_expression OR any_term
		{ enum tree_code code;
		  if (TREE_CODE (TREE_TYPE ($1)) == INTEGER_TYPE)
		    {
                      code = BIT_IOR_EXPR;
		      if (pedantic)
		        warning ("ISO Pascal does not allow bitwise `or'");
		    }
		  else if (flag_short_circuit)
		    code = TRUTH_ORIF_EXPR;
		  else
		    code = TRUTH_OR_EXPR;
		  $$ = build_pascal_op (code, $1, $3); }
	| simple_expression or_else any_term
		{ if (pedantic)
		      warning ("ISO Pascal does not allow `or_else'");
		  $$ = build_pascal_op (TRUTH_ORIF_EXPR, $1, $3); }
	;

or_else :
	  OR ELSE
		{ $$ = NULL_TREE; /* Any tree value is fine */ }
	| OR_ELSE
	;

and_then :
	  AND THEN
		{ $$ = NULL_TREE; /* Any tree value is fine */ }
	| AND_THEN
	;


any_term :
	  sign term
		{ $$ = build_pascal_unary_op ($1, $2, 0); }
	| term
	;


term :
	  primary
	| term multiplying_operator primary
		{ $$ = build_pascal_op ($2, $1, $3); }
	| term pxsc_multiplying_operator primary
		{ $$ = build_pxsc_operator ($2, $1, $3); }
	| term SYMMETRIC_DIFF primary
		{ $$ = build_pascal_op (BIT_XOR_EXPR, $1, $3); }
	| term AND primary
                {
                  enum tree_code code;
		  if (TREE_CODE (TREE_TYPE ($1)) == INTEGER_TYPE)
		    {
                      code = BIT_AND_EXPR;
		      if (pedantic)
		        warning ("ISO Pascal does not allow bitwise `and'");
		    }
                  else if (flag_short_circuit)
                    code = TRUTH_ANDIF_EXPR;
                  else
                    code = TRUTH_AND_EXPR;
	          $$ = build_pascal_op (code, $1, $3); 
                }
	| term and_then primary
		{ if (pedantic)
		    warning ("ISO Pascal does not allow `and_then'");
		  $$ = build_pascal_op (TRUTH_ANDIF_EXPR, $1, $3); }
	;


/* The 10206 does not allow a SIGN before the exponent. Only unsigned factors.
 * GPC used to allow these, but now it's like the standard says.
 */
primary :
	  factor
	| factor identifier factor
		{
		  /* user-defined operator expression */
		  /* is converted to a function call */
		  tree arg = build_tree_list (NULL_TREE, $1);
		  arg = chainon (arg, build_tree_list (NULL_TREE, $3));
		  $$ = build_function_call ($2, arg);
		}
	| factor POW factor
		{ if (TREE_CODE (TREE_TYPE ($3)) != INTEGER_TYPE)
		    {
		      error ("`pow' exponent is not of integer type");
		      $$ = error_mark_node;
		    }
		  else
		    $$ = build_pascal_op (EXPON_EXPR, $1, $3);
		}
	| factor EXPON factor
		{ 
		  tree exp = $3;
		  if (TREE_CODE (TREE_TYPE (exp)) == INTEGER_TYPE)
		    exp = convert (TREE_TYPE (real_zero_node),
				   exp);
		  if (TREE_CODE (TREE_TYPE (exp)) != REAL_TYPE)
		    {
		      error ("`**' exponent is not of real or integer type");
		      $$ = error_mark_node;
		    }
		  else
		    $$ = build_pascal_op (EXPON_EXPR, $1, exp);
		}
	| factor OP_IS factor
		{ error ("Object pascal operator `is' not supported");
		  $$ = error_mark_node;
		}
	;

factor :
	  variable_or_function_access	 
		{ tree temp = $1;
		  if (TREE_CODE ($1) == TYPE_DECL)
		    {
			error ("type name given -- variable access expected");
			$$ = error_mark_node;
		    }
		  else if (MAYBE_CALL_FUNCTION ($1))
		     $$ = maybe_call_function ($1, NULL_TREE);
		  else
		     $$ = $1;
		}
	| constant_literal
	| unsigned_number
	| set_constructor
	| '(' expression ')'
		{ $$ = $2; }
	/* @@@ Check later that NOT is taken from BOOLEAN_TYPE */
	| NOT factor
		{
                  enum tree_code code;
		  if (TREE_CODE (TREE_TYPE ($2)) == INTEGER_TYPE)
		    {
                      code = BIT_NOT_EXPR;
		      if (pedantic)
		        warning ("ISO Pascal does not allow bitwise `not'");
		    }
		  else
		    code = TRUTH_NOT_EXPR;
		  $$ = build_pascal_unary_op (code, $2, 0); 
		}
	| address_operator variable_or_function_access
		{ if (pedantic)
		    warning ("ISO Pascal does not allow to take addresses of objects");
		  /* Let default_conversion() take care of function pointers */
		  if (TREE_CODE (TREE_TYPE ($2)) == FUNCTION_TYPE)
		    $$ = $2;
		  else
		    $$ = build_pascal_unary_op (ADDR_EXPR, $2, 0); }

/* More GNU Pascal extensions */
	/* Refer to the address of a label as a pointer.  */
        /* For computed GOTO * statement */
	| ANDAND label
		{ tree label = lookup_label ($2);
		  TREE_USED (label) = 1;
		  $$ = build1 (ADDR_EXPR, ptr_type_node, label);
		  TREE_CONSTANT ($$) = 1; }
	| CONJUGATE '(' expression ')'
		{ $$ = build_pascal_unary_op (CONJ_EXPR, $3, 1); }
	| SIZEOF '(' variable_access_or_typename ')'
		{ if (TREE_CODE ($3) == COMPONENT_REF
		      && DECL_BIT_FIELD (TREE_OPERAND ($3, 1)))
		    error ("`sizeof' applied to a bit-field");
		  $$ = c_sizeof (TREE_TYPE ($3)); }
	| ALIGNOF '(' variable_access_or_typename ')'
		{ if (TREE_CODE ($3) == COMPONENT_REF
		      && DECL_BIT_FIELD (TREE_OPERAND ($3, 1)))
		    error ("`alignof' applied to a bit-field");
		  if (TREE_CODE ($3) == INDIRECT_REF)
		    {
		      tree t = TREE_OPERAND ($3, 0);
		      tree best = t;
		      int bestalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));
		      while (TREE_CODE (t) == NOP_EXPR
			     && TREE_CODE (TREE_TYPE (TREE_OPERAND (t, 0))) == POINTER_TYPE)
			{
			  int thisalign;
			  t = TREE_OPERAND (t, 0);
			  thisalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));
			  if (thisalign > bestalign)
			    best = t, bestalign = thisalign;
			}
		      $$ = c_alignof (TREE_TYPE (TREE_TYPE (best)));
		    }
		  else
		    $$ = c_alignof (TREE_TYPE ($3)); }
	;

address_operator :
          '&'
		{ }
        | '@'
		{ }
	;

variable_or_function_access :
	  identifier
		{
		  if (IS_ABSOLUTE_VAR ($1))
		    $$ = build_indirect_ref (DECL_INITIAL ($1),
		                             "ABSOLUTE variable access");
		  else if (TREE_CODE (TREE_TYPE ($1)) == REFERENCE_TYPE)
		    $$ = build_indirect_ref ($1, "VAR parameter ref"); 
		  else if (MAYBE_CALL_FUNCTION ($1))
		    $$ = maybe_call_function ($1, NULL_TREE);
		  else if (CALL_METHOD ($1))
		    $$ = call_method ($1, NULL_TREE);
		  else
		    $$ = $1;
		}
	| variable_or_function_access_no_id
	| standard_functions
	;

variable_or_function_access_no_id :
	  p_OUTPUT
		{
		  tree node = get_standard_output ();
		  $$ = node;
		}
	| p_INPUT
		{
		  tree node = get_standard_input ();
		  $$ = node;
		}
	| variable_or_function_access '.' new_identifier optional_par_actual_parameter_list
		{ 
		  if (PASCAL_TYPE_RESTRICTED (TREE_TYPE ($1)))
		    error ("Accessing fields of a restricted type object is not allowed");
		  $$ = build_component_ref ($1, $3);
                  if (CALL_METHOD ($$))
                    $$ = call_method ($$, $4);
		}
	| OP_INHERITED new_identifier optional_par_actual_parameter_list
		{
		  $2 = IDENTIFIER_LOCAL_VALUE ($2);
		  if ($2 && TREE_CODE ($2) == COMPONENT_REF)
		    {
		      tree basetype = TREE_TYPE (TREE_OPERAND ($2, 0));
		      if (TYPE_LANG_SPECIFIC (basetype)->elts [1])
		        basetype = TYPE_LANG_SPECIFIC (basetype)->elts [1];
		      else
		        error ("there is no parent type to inherit from");
		      /* Get a TYPE_DECL insteat of a type */
		      basetype = lookup_name (TYPE_LANG_SPECIFIC (basetype)->elts [0]);
		      $$ = build (COMPONENT_REF, TREE_TYPE ($2),
		                  basetype, TREE_OPERAND ($2, 1));
		      if ($3)
		        $$ = call_method ($$, $3);
		      /*
		       * else 
		       *   it will be called by
		       *   assignment_or_call_statement
		       */
		    }
		  else
		    {
		      error ("method not found");
		      $$ = error_mark_node;
		    }
		}
	| variable_or_function_access pointer_char optional_par_actual_parameter_list
		{
		  if (PASCAL_TYPE_RESTRICTED (TREE_TYPE ($1)))
		    error ("Referencing a restricted type object is not allowed");

		  if (TREE_CODE (TREE_TYPE ($1)) == FILE_TYPE)
		    {
		      $$ = build_buffer_ref ($1);
		      if ($3)
			error ("Argument list given to a file buffer reference");
		    }
		  else if (TREE_CODE (TREE_TYPE ($1)) == POINTER_TYPE
			   && TREE_CODE (TREE_TYPE (TREE_TYPE ($1))) ==
				FUNCTION_TYPE)
		    {
		      /* Force a call by giving non-zero args
		       * If the routine has no arguments, pass empty_arglist
		       */
		      $$ = maybe_call_function ($1, $3 ? $3 : empty_arglist);

		      if (pedantic)
			warning ("GPC specific function call via a function pointer");
		    }
		  else if ($3 != NULL_TREE)
		    {
		      error ("Argument list given to a non-function pointer reference");
		      $$ = error_mark_node;
		    }
		  else
		    {
		      warn_void_dereference++;
		      $$ = build_indirect_ref ($1, "`^'");
		      warn_void_dereference--;
		    }
		}
	| variable_or_function_access lbracket index_expression_list rbracket
		{
		  if (PASCAL_TYPE_RESTRICTED (TREE_TYPE ($1)))
		    error ("Accessing an component of a restricted type object is not allowed");

		  $$ = build_pascal_array_ref ($1, $3);
		}
	| identifier '('
		{ $<itype>2 = suspend_function_calls ();
		  $$ = lastiddecl;
		}
	  actual_parameter_list r_paren_or_error
		{
		  resume_function_calls ($<itype>2);
		  if ($<ttype>3 && TREE_CODE ($<ttype>3) == TYPE_DECL)
		    {
		      if (pedantic)
			 warning ("ISO Pascal does not allow type casts");
		      if (list_length ($4) != 1)
		        {
			  error ("type cast expects one expression argument");
			  $$ = error_mark_node;
		        }
		      else
			{
			  tree type = 
			    groktypename (
				build_tree_list (build_tree_list (NULL_TREE,
						    TREE_TYPE ($<ttype>3)),
						 DECL_NAME ($<ttype>3)));
			  $$ = build_c_cast (type, TREE_VALUE ($4));
			}
		    }
		  else if (CALL_METHOD ($1))
		    $$ = call_method ($1, $4);
		  else
		    $$ = build_function_call ($1, $4);
		}
        | p_NEW '(' variable_access_or_typename ')'
		{
		  $$ = NULL_TREE;
		  if (TREE_CODE (TREE_TYPE ($3)) == POINTER_TYPE
                      && IS_OBJECT_TYPE (TREE_TYPE (TREE_TYPE ($3)))
		      && TREE_CODE ($3) == TYPE_DECL)
		    {
		      /* @@@ finish_decl() is not called?? */
		      tree temp = start_decl (get_unique_identifier ("self", 1),
                        build_tree_list (NULL_TREE, TREE_TYPE ($3)),
					      0, NULL_TREE, NULL_TREE);
		      build_rts_call (p_NEW, build_tree_list (NULL_TREE, temp));
		      $$ = temp;
		    }
		  else
		    build_rts_call (p_NEW, build_tree_list (NULL_TREE, $3));
		}
	| p_NEW '(' variable_access_or_typename ','
		{
		  $$ = NULL_TREE;
		  if (TREE_CODE (TREE_TYPE ($3)) == POINTER_TYPE
                      && IS_OBJECT_TYPE (TREE_TYPE (TREE_TYPE ($3))))
		    {
		      tree self;
		      if (TREE_CODE ($3) == TYPE_DECL)
		        {
			  /* @@@ finish_decl() is not called?? */
		          tree temp = start_decl (get_unique_identifier ("self", 1),
                            build_tree_list (NULL_TREE, TREE_TYPE ($3)),
						  0, NULL_TREE, NULL_TREE);
		          build_rts_call (p_NEW, build_tree_list (NULL_TREE, temp));
		          self = build_indirect_ref (temp, "`New'");
		          $$ = temp;
		        }
		      else
		        {
		          build_rts_call (p_NEW, build_tree_list (NULL_TREE, $3));
		          self = build_indirect_ref ($3, "`New'");
		        }
		      shadow_record_fields (self);
		    }
		}
	  actual_parameter_list ')'
		{
		  if (TREE_CODE (TREE_TYPE ($3)) == POINTER_TYPE
                      && IS_OBJECT_TYPE (TREE_TYPE (TREE_TYPE ($3))))
		    {
		      expand_expr_stmt (TREE_VALUE ($6));
		      poplevel (0, 0, 0);
		    }
		  else
		    build_rts_call (p_NEW,
                      chainon (build_tree_list (NULL_TREE, $3), $6));
		  $$ = $<ttype>5;
		}
        | bp_GETMEM '(' actual_parameter ')'
		{
		  $$ = build_rts_call (bp_GETMEM, 
		                       build_tree_list (NULL_TREE, $3));
		}
        | bp_GETMEM '(' actual_parameter ',' actual_parameter ')'
		{
		  build_rts_call (bp_GETMEM,
		         chainon (build_tree_list (NULL_TREE, $3),
		                  build_tree_list (NULL_TREE, $5)));
		  $$ = NULL_TREE;
		}
	;

/* SET CONSTRUCTOR,
 * save some work with a small kludge.
 */
set_constructor :
	  lbracket set_constructor_element_list rbracket
		{ $$ = build_set_constructor ($2); }
	;

set_constructor_element_list :
	  /* empty */
		{ $$ = NULL_TREE; }
	| member_designator
	| set_constructor_element_list ',' member_designator
		{ if ($3 != NULL_TREE)
		    $$ = chainon ($1, $3);
                  yyerrok; }
/* (causes a shift/reduce conflict) */
        | error
		{ error ("need a member_designator");
                  $$ = NULL_TREE; }
        | set_constructor_element_list error member_designator
		{ error ("missing comma");
                  if ($3 != NULL_TREE)
		    $$ = chainon ($1, $3);
		  yyerrok; }
        | set_constructor_element_list ',' error
                { error ("extra comma"); }
	;

/* Returns a TREE_LIST node with TREE_PURPOSE as the range low bound,
   and TREE_VALUE as the range upper bound */
member_designator :
	  expression
		{ $$ = construct_set_member ($1, NULL_TREE); }
	| expression TWODOTS expression
		{ $$ = construct_set_member ($1, $3);}
	;

standard_functions :
	  rts_fun_onepar '(' actual_parameter ')'
		{ $$ = build_rts_call ($1, build_tree_list (NULL_TREE, $3)); }
	| rts_fun_twopar '(' actual_parameter ',' actual_parameter ')'
		{ $$ = build_rts_call ($1,
			      chainon (build_tree_list (NULL_TREE, $3),
				       build_tree_list (NULL_TREE, $5)));
		}
	| rts_fun_optpar optional_par_actual_parameter
		{ $$ = build_rts_call ($1, $2); }
	| rts_fun_parlist '(' actual_parameter_list ')'
		{ $$ = build_rts_call ($1, $3); }
	;


optional_par_actual_parameter :
	  /* empty */
		{ $$ = NULL_TREE; }
	|  '(' actual_parameter ')'
		{ $$ = build_tree_list (NULL_TREE, $2); }
	;

rts_fun_optpar :
	  p_EOF 
		{ $$ = p_EOF; }
	| p_EOLN
		{ $$ = p_EOLN; }
	;

rts_fun_onepar :
/* arithmetic functions */	
	  p_ABS
		{ $$ = p_ABS; }
	| p_SQR
		{ $$ = p_SQR; }
	| p_SIN
		{ $$ = p_SIN; }
	| p_COS
		{ $$ = p_COS; }
	| p_EXP
		{ $$ = p_EXP; }
	| p_LN
		{ $$ = p_LN; }
	| p_SQRT
		{ $$ = p_SQRT; }
	| p_ARCTAN
		{ $$ = p_ARCTAN; }
	| p_ARG
		{ $$ = p_ARG; }
	| p_RE
		{ $$ = p_RE; }
	| p_IM
		{ $$ = p_IM; }
/* transfer functions */
	| p_TRUNC
		{ $$ = p_TRUNC; }
	| p_ROUND
		{ $$ = p_ROUND; }
	| p_CARD
		{ $$ = p_CARD; }
/* ordinal functions */
	| p_ORD
		{ $$ = p_ORD; }
	| p_CHR
		{ $$ = p_CHR; }
/* boolean functions */
	| p_ODD
		{ $$ = p_ODD; }
	| p_EMPTY
		{ $$ = p_EMPTY; }
/* Direct access position functions */
	| p_POSITION
		{ $$ = p_POSITION; }
	| p_LASTPOSITION
		{ $$ = p_LASTPOSITION; }
/* String functions */
	| p_LENGTH
		{ $$ = p_LENGTH; }
	| p_TRIM
		{ $$ = p_TRIM; }
/* Binding function */
	| p_BINDING
		{ $$ = p_BINDING; }
/* time functions */
	| p_DATE
		{ $$ = p_DATE; }
	| p_TIME
		{ $$ = p_TIME; }
	;

rts_fun_twopar :
	  gpc_MAX
		{ $$ = gpc_MAX; }
	| gpc_MIN
		{ $$ = gpc_MIN; }
/* Complex functions */
	| p_CMPLX
		{ $$ = p_CMPLX; }
	| p_POLAR
		{ $$ = p_POLAR; }
/* String functions */
	| p_INDEX
		{ $$ = p_INDEX; }
	| p_EQ
		{ $$ = p_EQ; }
	| p_LT
		{ $$ = p_LT; }
	| p_GT
		{ $$ = p_GT; }
	| p_NE
		{ $$ = p_NE; }
	| p_LE
		{ $$ = p_LE; }
	| p_GE
		{ $$ = p_GE; }
	;

rts_fun_parlist :
	  p_SUCC	/* One or two args */
		{ $$ = p_SUCC; }
	| p_PRED	/* One or two args */
		{ $$ = p_PRED; }
        | p_SUBSTR	/* Two or three args */
		{ $$ = p_SUBSTR; }
	;

/* operator definitions */

relational_operator :
	  NEQ
		{ $$ = NE_EXPR; }
	| LTE
		{ $$ = LE_EXPR; }
	| GTE
		{ $$ = GE_EXPR; }
	| '='
	| '<'
	| '>'
	;

multiplying_operator :
	  DIV
		{ $$ = TRUNC_DIV_EXPR; }
	| MOD
		{ $$ = FLOOR_MOD_EXPR; }
	| '/'
	| '*'
        | BP_SHL
	        {
		  $$ = LSHIFT_EXPR;
		  if (pedantic)
		    warning ("ISO Pascal does not define `shl'");
		}
        | BP_SHR
	        {
		  $$ = RSHIFT_EXPR;
		  if (pedantic)
		    warning ("ISO Pascal does not define `shr'");
		}
	;

pxsc_multiplying_operator :
	  CEILMULT
		{ $$ = "ceilmult"; }
	| CEILRDIV
		{ $$ = "ceilrdiv"; }
	| FLOORMULT
		{ $$ = "floormult"; }
	| FLOORRDIV
		{ $$ = "floorrdiv"; }
	;

adding_operator :
	  '-'
	| '+'
        | BP_XOR
	        {
		  $$ = BIT_XOR_EXPR;
		  if (pedantic)
		    warning ("ISO Pascal does not define `xor'");
		}
	;

pxsc_adding_operator :
	  CEILPLUS
		{ $$ = "ceilplus"; }
	| CEILMINUS
		{ $$ = "ceilminus"; }
	| FLOORPLUS
		{ $$ = "floorplus"; }
	| FLOORMINUS
		{ $$ = "floorminus"; }
	;

semi :
	  ';'
		{ /* No yyerrok here. */ }
	;

optional_semicolon :
	  /* empty */
              { yyerrok; }
	| ';'
              { yyerrok; }
	;

optional_colon :
	  /* empty */
	| ':'
	;

lbracket :
	  '['
	| LBRACKET
	;

rbracket :
	  ']'
	| RBRACKET
	;

/* These are support states, not pascal syntax */

pushlevel :
	  /* empty */
		{   emit_line_note (input_filename, lineno);
		    pushlevel (0);
		    clear_last_expr ();
		    expand_start_bindings (0); 
		}
	;

/* Do not push_momentary() */
pushlevel1 :
	  /* empty */
		{   emit_line_note (input_filename, lineno);
		    pushlevel (0);
		    clear_last_expr ();
		    expand_start_bindings (0); 
		}
	;

/* @@@ Note that this does not return the value of poplevel().
   GCC uses that value to handle braced-group expressions, which
   are not implemented in GPC (yet :-)) */
poplevel :
	poplevel1
		{ 
		}
	;

poplevel1 :
	  /* empty */
		{ tree decls = getdecls ();
		  emit_line_note (input_filename, lineno);
		  if (decls != NULL_TREE) {
		    expand_end_bindings (decls, 1, 0);
		    poplevel (1, 1, 0);
		  } else {
		    expand_end_bindings (decls, kept_level_p (), 0);
		    poplevel (kept_level_p (), 0, 0);
		  }
		}
	;

setspecs :
	  /* empty */
		{ $$ = suspend_momentary (); }
	;

/*
 * The EXTENDED PASCAL Module support below is currently under construction.
 */

module_declaration :
	  MODULE
		{
		  enable_keyword ("Interface");
		  enable_keyword ("Implementation");
		}
	  new_identifier
		{
		  initialize_module ($3);
		  if (pedantic)
		    warning ("ISO 7185 Pascal does not support modules");
		}
	  rest_of_module
		{
		  current_module = NULL_MODULE;
		}
        | BP_UNIT
		{
		  enable_keyword ("Interface");
		  enable_keyword ("Implementation");
		} 
	  new_identifier semi
		{
                  initialize_module ($3);
                  this_is_an_interface_module = 0;
		  if (pedantic)
		    warning ("ISO Pascal does not support units");
		}
          rest_of_unit
	;

rest_of_unit :
	  INTERFACE
		{
		  this_is_an_interface_module = 1;
		  export_interface (current_module->name, export_all);
		}
          unit_interface
          IMPLEMENTATION
		{
		  create_gpi_files ();
		  this_is_an_interface_module = 0;
		  /* Handle pending decls (see below) */
		  if (current_module->pending_decls)
		    {
		      tree scan;
		      for (scan = current_module->pending_decls;
			   scan;
			   scan = TREE_CHAIN (scan))
			{
			  if (TREE_PURPOSE (scan) == void_type_node)
			    grok_directive (TREE_VALUE (TREE_VALUE (TREE_VALUE (scan))),
					    TREE_PURPOSE (TREE_VALUE (TREE_VALUE (scan))),
					    TREE_PURPOSE (TREE_VALUE (scan)),
					    0);
			  else if (TREE_PURPOSE (scan) == NULL_TREE)
			    declare_vars (TREE_VALUE (TREE_VALUE (scan)), 	        /* names */
					  TREE_PURPOSE (TREE_VALUE (scan)),	        /* type */
					  TREE_PURPOSE (TREE_CHAIN (TREE_VALUE (scan))),/* init */
					  TREE_VALUE (TREE_CHAIN (TREE_VALUE (scan))),  /* qual */
					  0);
			  else
			    abort ();
			}
		      current_module->pending_decls = NULL_TREE;
		    }
		}
          unit_implementation
          optional_unit_constructor
          END
		{
		  current_module = NULL_MODULE;
		}
	;

unit_interface :
          import_part				/* This may be empty */
          any_module_decl_part			/*  as well as this */
        ;

unit_implementation :
          any_declaration_part
          optional_init_and_final_part
	;

rest_of_module :
	  optional_module_parameters module_interface semi 
		{
		  create_gpi_files ();
		  this_is_an_interface_module = 0;
		}
	  module_block
	| INTERFACE optional_module_parameters module_interface
		{
		  if (current_module->interface)
		    error ("Module `%s' already has an interface part",
			   IDENTIFIER_POINTER (current_module->name));
		  else
		     current_module->interface = void_type_node; /* @@@@@ */
		  create_gpi_files ();
		}
	| IMPLEMENTATION semi
		{ 
		  if (current_module->implementation)
		    error ("Module `%s' already has an implementation part",
			   IDENTIFIER_POINTER (current_module->name));
		  else
		     current_module->implementation = void_type_node; /* @@@@@ */

		  if (!current_module->interface)
		    warning ("Module `%s' has no interface module",
			     IDENTIFIER_POINTER (current_module->name));

		  this_is_an_interface_module = 0;

		  /* @@@ if decls other than VAR_DECL are made
		   * pending, this has to be recoded.
		   */
		  if (current_module->pending_decls)
		    {
		      tree scan;
		      for (scan = current_module->pending_decls;
			   scan;
			   scan = TREE_CHAIN (scan))
			{
			  if (TREE_PURPOSE (scan) == void_type_node)
			    grok_directive (TREE_VALUE (TREE_VALUE (TREE_VALUE (scan))),
					    TREE_PURPOSE (TREE_VALUE (TREE_VALUE (scan))),
					    TREE_PURPOSE (TREE_VALUE (scan)),
					    0);
			  else if (TREE_PURPOSE (scan) == NULL_TREE)
			    declare_vars (TREE_VALUE (TREE_VALUE (scan)), 	        /* names */
					  TREE_PURPOSE (TREE_VALUE (scan)),	        /* type */
					  TREE_PURPOSE (TREE_CHAIN (TREE_VALUE (scan))),/* init */
					  TREE_VALUE (TREE_CHAIN (TREE_VALUE (scan))),  /* qual */
					  0);
			  else
			    abort ();
			}
		      current_module->pending_decls = NULL_TREE;
		    }
		}
	  module_block
        | optional_module_parameters 
	  import_or_any_declaration_part
		{ if (pedantic)
		    warning ("GPC specific module declaration");
		}
	  END
	;

optional_module_parameters :
	  optional_par_id_list semi
		{
		  current_module->parms = $1;
		  associate_external_objects (current_module->parms);
		}
	;

module_interface :
	  EXPORT
		{ this_is_an_interface_module = 1; }
	  export_part_list semi		/* EXPORT part is never empty */
	  import_part				/* This may be empty */
	  any_module_decl_part			/*  as well as this */
	  END
		{ }
	;

any_module_decl_part :
	  /* empty */
	| any_module_decl_list
	;

any_module_decl_list :
	  any_module_decl_1
	| any_module_decl_list any_module_decl_1
	;

any_module_decl_1 :
	  any_module_decl
	;

any_module_decl :
	  simple_decl
	| function_interface_decl
	;

/*
 * If there is a user specified directive, use that. If not:
 *
 *  Exported function interface declares a function as it would have
 *  directive FORWARD attached. It does not matter if it is
 *  not declared in the same file. (Then treated as EXTERNAL
 *  directive)
 *
 *  If the function has not been exported, treat it as static
 */
function_interface_decl :
	  function_heading semi optional_directive_list
		{ tree dir;
		  handle_autoexport (TREE_OPERAND (TREE_PURPOSE ($1), 0));
		  dir = $3 ? $3 : 
		    (name_exported_p (TREE_OPERAND (TREE_PURPOSE ($1), 0)) ?
			d_forward : d_static);
		  
		  if (this_is_an_interface_module
		      && dir == d_static
		      && ! we_are_loading_a_gpi_file)
		    {
		      current_module->pending_decls =
			chainon (current_module->pending_decls,
				 build_tree_list (void_type_node,
						  build_tree_list (dir,
								   $1)));
		    }
		  else
		    grok_directive (TREE_VALUE ($1), TREE_PURPOSE ($1), dir, 0);
	      	}
	;

optional_directive_list :
	  /* empty */
		{ $$ = NULL_TREE; }
	| directive_list semi
		{ if (is_known_directive ($1))
		    $$ = $1;
		  else
		    $$ = NULL_TREE;
		}
	;

/* The module block may be empty until END */
module_block :
	  import_or_any_declaration_part
	  optional_init_and_final_part
	  END
	;

module_constructor :
	  TO BEGIN optional_initialization_order DO
		{ tree context;
		  tree name = $<ttype>$ = get_unique_identifier ("constructor", 1);
		  tree parm = no_parameters ();

		  context = build_nt (CALL_EXPR, name, parm, NULL_TREE);

		  if (! start_function (build_tree_list (NULL_TREE, void_type_node),
					context,
					NULL_TREE, NULL_TREE,
					0))
		    YYERROR1;

		  reinit_parse_for_function ();
		  store_parm_decls ();
		  top_level_labels = 1;
		}
	  pushlevel1
		{
		  tree runid = $3;

		  if (! runid)
		     runid = integer_one_node;
		  else if (pedantic)
		     warning ("GPC specific module initialization ordering used");
		     
		  init_constructor (lookup_name ($<ttype>5), runid);
		}
	  statement semi
		{ un_initialize_block (getdecls (), 1);}
	  poplevel1
		{ finish_function (0);
		  assemble_constructor (IDENTIFIER_POINTER ($<ttype>5));
		}
	;

optional_unit_constructor :
	  /* empty */
	| BEGIN
		{ tree context;
		  tree name = $<ttype>$ = get_identifier (concat ("init_", 
                                  IDENTIFIER_POINTER (current_module->name), ""));
		  tree parm = no_parameters ();

		  context = build_nt (CALL_EXPR, name, parm, NULL_TREE);

		  if (! start_function (build_tree_list (NULL_TREE, void_type_node),
					context,
					NULL_TREE, NULL_TREE,
					0))
		    YYERROR1;

		  reinit_parse_for_function ();
		  store_parm_decls ();
		  top_level_labels = 1;
		}
	  pushlevel1
		{
		  tree runid = integer_one_node;
		  init_constructor (lookup_name ($<ttype>2), runid);
		}
	  statement_sequence
		{ un_initialize_block (getdecls (), 1);}
	  poplevel1
		{ finish_function (0);
		  assemble_constructor (IDENTIFIER_POINTER ($<ttype>2));
		}
	;

optional_initialization_order :
	  /* empty */
		{ $$ = NULL_TREE; }
	| unsigned_number
	;

module_destructor :
	  TO END DO
		{ tree context;
		  tree name = $<ttype>$ = get_unique_identifier ("destructor", 1);
		  tree parm = no_parameters ();
		  context = build_nt (CALL_EXPR, name, parm, NULL_TREE);

		  if (! start_function (build_tree_list (NULL_TREE, void_type_node),
					context,
					NULL_TREE, NULL_TREE,
					0))
		    YYERROR1;

		  reinit_parse_for_function ();
		  store_parm_decls ();
		  top_level_labels = 1;
		}
	  pushlevel1
	  statement semi
		{ un_initialize_block (getdecls (), 1);}
	  poplevel1
		{ finish_function (0);
		  assemble_destructor (IDENTIFIER_POINTER ($<ttype>4));
		}
	;

/* Allow both, either or none of these. TO BEGIN DO comes always first. */
optional_init_and_final_part :
	  /* empty */
	| module_constructor module_destructor
		{ }
	| module_constructor
		{ }
	| module_destructor
		{ }
	;

export_part_list :
	  export_part
	| export_part_list semi export_part
              { yyerrok; }
        | error
            { error ("Module specifications need an export part"); }
	| export_part_list error export_part
	    { warning ("missing semicolon");
              yyerrok; }
	| export_part_list semi error
	    { error ("extra semicolon"); }
	;

export_part :
	  new_identifier
		{
		  enable_keyword ("Protected");
		  enable_keyword ("All");
		}
	  optional_equal_sign export_list_or_all
		{
		  export_interface ($1, $4);
		  disable_keyword ("Protected");
		  disable_keyword ("All");
		}
	;

optional_equal_sign :
	  /* Empty */
		{ warning ("Missing '=' after export interface identifier"); }
	| '='
		{ }
	;

export_list_or_all:
	  '(' export_list ')'
		{ $$ = $2; }
	| ALL
		{ $$ = export_all; }
	;

export_list :
	  export_list_item
	| export_list ',' export_list_item
		{ $$ = chainon ($1, $3); }
        | error
		{ $$ = NULL_TREE; }
        | export_list error export_list_item
		{ $$ = NULL_TREE; }
	| export_list ',' error
		{ $$ = NULL_TREE; }
	;

export_list_item :
	  new_identifier rest_of_export_item
		{ if ($2)
		    if (TREE_CODE ($2) == TREE_LIST)
		      $$ = module_export_range ($1, TREE_VALUE ($2));
		    else
		      $$ = module_export_clause ($1, $2, 0);
		  else
		    $$ = module_export_clause ($1, NULL_TREE, 0);
		}		    
	| PROTECTED new_identifier optional_rename
		{ $$ = module_export_clause ($2, $3, 1); }
	;

/* Identifiers must be CONSTANT NAMES */
rest_of_export_item :
	  optional_rename
	| TWODOTS new_identifier
		{ $$ = build_tree_list (NULL_TREE, $2); }
	;

optional_rename :
	  /* empty */
		{ $$ = NULL_TREE; }
	| RENAME new_identifier
		{ $$ = $2; }
	;

import_part :
	  /* empty */
	| IMPORT import_specification_list semi
		{ }
        | BP_USES uses_list semi
                { }
	;

/* Removed: | import_specification_list semi error
 */
import_specification_list :
	  import_specification
	| import_specification_list semi import_specification
        | import_specification_list error import_specification
		{ warning ("Missing semicolon");
		  yyerrok;
		}
	;	

uses_list :
          import_specification
	| uses_list ',' import_specification
        | uses_list error import_specification
		{ warning ("Missing comma");
		  yyerrok;
		}
	;	

import_specification :
	  new_identifier optional_access_qualifier optional_import_qualifier
		{
		  /* Recovering from an error if id is NULL_TREE
		   * If $1 is NULL_TREE lastiddecl is one of the
		   * predefined identifiers, but it does not matter
		   * anymore.
		   */
		  if ($1)
		    import_interface ($1, $3, $2 != NULL_TREE);
		}
	;

optional_access_qualifier :
	  /* Empty */
		{ $$ = NULL_TREE; }
	| QUALIFIED
	;

optional_import_qualifier :
	  /* Empty */
		{ $$ = NULL_TREE; }
	| '(' import_clause_list ')'
		{ $$ = build_tree_list (NULL_TREE, $2); }
	| ONLY '(' import_clause_list ')'
		{ $$ = build_tree_list ($3, $3); }
	;
	  
import_clause_list :
	  import_clause
	| import_clause_list ',' import_clause
		{ $$ = chainon ($1, $3); }
        | error
		{ $$ = NULL_TREE; }
        | import_clause_list error import_clause
		{ warning ("Missing comma");
		  $$ = chainon ($1, $3);
		}
        | import_clause_list ',' error
		{ $$ = NULL_TREE; }
	;

import_clause :
	  new_identifier optional_rename
		{ $$ = build_tree_list ($1, $2); }
	;

%%
/* Lexical analyzer moved to gpc-lex.c */

/* Sets the value of the 'yydebug' varable to VALUE.
   This is a function so we don't have to have YYDEBUG defined
   in order to build the compiler.  */
void
set_yydebug (value)
     int value;
{
#if YYDEBUG != 0
  yydebug = value;
#else
  warning ("YYDEBUG not defined.");
#endif
}

/*
Local variables:
mode:c
tab-width: 8
version-control: t
End:
*/
