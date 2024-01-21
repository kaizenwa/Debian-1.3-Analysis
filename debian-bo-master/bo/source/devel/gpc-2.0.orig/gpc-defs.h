/* Front-end tree definitions for GNU compiler.
   Copyright (C) 1989,1993 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Author: Juki <jtv@hut.fi> */

#ifdef GPC

#include "gpc-lang.h"

#ifdef __STDC__
# define ASSERT(x, str) \
  if (! x) \
   printf("Assert failure at %s line %d : %s\n",__FILE__,__LINE__,str),abort();
#else
/* Thanks Doug! */
# define ASSERT(x, str) \
  if (! x) printf("Assert failure : %s\n", str),abort();
#endif

#include <stdio.h>

/* Macro that inserts it's argument in double quotes */
#ifdef __STDC__
# define GPC_MAKE_STRING(X) #X
#else
# define GPC_MAKE_STRING(X) "X"
#endif

/* If real is not double must take special care to read in floating point constants
 * in gpc-decl.c(init_decl_processing)
 */
#ifdef REAL_IS_NOT_DOUBLE
# define GPC_GET_REAL(real) REAL_VALUE_ATOF (GPC_MAKE_STRING (real), DFmode)
#else
# define GPC_GET_REAL(X) X
#endif /* REAL_IS_NOT_DOUBLE */

/* Definition of GPC_MAIN removed. - PG */

/* Define the type of Pascal we are compiling */
/* Used for reserved word recognition and possibly others... */
#define PASCAL_GNU     99	/* Allow gnu extensions __WORD__ tokens */
#define PASCAL_SC       4       /* Allow Pascal-SC (PXSC) extensions */
#define PASCAL_BORLAND  3	/* Allow Borland-Pascal extensions */
#define PASCAL_OBJECT   2	/* Allow Object-Pascal extensions */
#define PASCAL_EXTEND	1	/* Allow Extended Pascal */
#define PASCAL_ISO	0	/* Allow ISO Pascal */

/*
 * Some Pascal set constructors do not allow us to derive the
 * set size from anywhere. In such cases, the maximum allowed
 * members in the set is defined here. (Otherwise, there is no limit)
 *
 * By default the size of these sets is DEFAULT_SET_SIZE bits
 *
 * Users may change this with -fsetlimit:XX switch at compile time.
 *
 */

/* This is the size in bits */
#define DEFAULT_SET_SIZE	(8 * BITS_PER_WORD)

#define ORDINAL_TYPE(code) \
 ((code) == INTEGER_TYPE || (code) == CHAR_TYPE || \
  (code) == BOOLEAN_TYPE || (code) == ENUMERAL_TYPE)

#define ORDINAL_OR_REAL_TYPE(c) (ORDINAL_TYPE(c) || (c) == REAL_TYPE)

#define ORDINAL_REAL_OR_COMPLEX_TYPE(c) (ORDINAL_OR_REAL_TYPE(c) || (c) == COMPLEX_TYPE)

#define INT_REAL(c) ((c) == INTEGER_TYPE || (c) == REAL_TYPE)

#define INT_REAL_BOOL(c) ((c) == INTEGER_TYPE || (c) == REAL_TYPE || (c) == BOOLEAN_TYPE)

#define IS_ENUMERAL(c) ((c) == BOOLEAN_TYPE || (c) == ENUMERAL_TYPE)

#define IS_BOOLEAN(c) ((c) == BOOLEAN_TYPE)

/* @@ Is this really used anymore? */
#define LOGICAL_RESULT_TYPE boolean_type_node

/* If c is SET_TYPE or a set CONSTRUCTOR */
#define SET_OR_CONST(c) ((c) == SET_TYPE || (c) == CONSTRUCTOR)

/* Pascal type nodes; maybe should be in tree.h */
extern tree boolean_type_node;
extern tree text_type_node;

/* Extended Pascal node for COMPLEX type */
extern tree complex_type_node;

/* this is a TEXT file type node */
#define TYPE_FILE_TEXT		TYPE_MIN_VALUE

/* This object is EXTERNAL in Pascal sense; it has been
   mentioned in the program heading. In Extended Pascal it does
   not have to be a file. 

   Use one bit of the language dependend common bits in the
   tree structure.
*/
#define PASCAL_EXTERNAL_OBJECT(object) TREE_LANG_FLAG_6 (object)

/*
 * This flag is set if the type is `packed'
 */
#define PASCAL_TYPE_PACKED(type) TYPE_LANG_FLAG_6 (type)

/*
 * Flag conformant array schema index types.
 *
 */
#define PASCAL_TYPE_CONFORMANT_INDEX(type) TYPE_LANG_FLAG_5 (type)

/*
 * Flag string schema types.
 */
#define PASCAL_TYPE_STRING(type) TYPE_LANG_FLAG_4 (type)

/* Return the pascal string value of DECL
 */
#define PASCAL_STRING_VALUE(decl) (PASCAL_TYPE_STRING (TREE_TYPE (decl))                 ? \
				   build_component_ref (decl, get_identifier ("string")) : \
				   decl)

/* Each variable length string has a "current length" field,
 * CHAR_TYPE length is always 1,
 * fixed-length-string length is the domain max value.
 */
#define PASCAL_STRING_LENGTH(decl)					\
	 (PASCAL_TYPE_STRING(TREE_TYPE(decl)) 	       	       ?	\
	  build_component_ref (decl,get_identifier ("length")) :	\
	  ((TREE_CODE (TREE_TYPE(decl)) == CHAR_TYPE)   ?	   	\
	   integer_one_node  			  	:   		\
	   ((TREE_CODE (decl) == STRING_CST)		        ?  	\
	    build_int_2 (TREE_STRING_LENGTH (decl) - 1, 0) :  		\
	    TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (decl))))))

/* Each variable length string has a "Capacity" field.
 */
#define PASCAL_STRING_CAPACITY(decl) build_component_ref (decl, get_identifier ("Capacity"))

/*
 * Bindable types get this flag.
 */
#define PASCAL_TYPE_BINDABLE(type) TYPE_LANG_FLAG_3 (type)

/*
 * RESTRICTED type qualified.
 */
#define PASCAL_TYPE_RESTRICTED(type)       TYPE_LANG_FLAG_2 (type)

/*
 * Borland Pascal "typed const" type qualifier ("almost readonly").
 * (I would like to use a TYPE_LANG_FLAG here, but they are all busy.)
 */
#define PASCAL_TYPE_TYPEDCONST(type)	TREE_LANG_FLAG_5 (type)

/*
 * Borland Pascal "open array" parameter.
 * (I would also like to use a TYPE_LANG_FLAG here.)
 */
#define PASCAL_TYPE_OPEN_ARRAY(type)	TREE_LANG_FLAG_4 (type)

/* Nonzero if the PARM_DECL is a standard pascal procedure parameter.
 *
 * If it is a non-standard function pointer parameter, this is not set,
 * in which case the procedure is not automatically called when
 * referenced.
 */
#define PASCAL_PROCEDURE_PARAMETER(decl) DECL_LANG_FLAG_5 (decl)

/* In a method field (FUNCTION_DECL record field): Nonzero indicates that
 * the method is virtual.
 */
#define PASCAL_VIRTUAL_METHOD(decl) TREE_LANG_FLAG_5 (decl)

/* In a method field: Nonzero indicates that the method is a constructor.
 */
#define PASCAL_CONSTRUCTOR_METHOD(decl) TREE_LANG_FLAG_4 (decl)

/*
 * When a VAR_DECL node is directly assigned to we mark it.
 * (Currently only used to find out if a value is assigned to
 *  a function via it's name or a user defined return value name)
 */
#define PASCAL_VALUE_ASSIGNED(decl) DECL_LANG_FLAG_6 (decl)

/*
 * This flag distinguishes a variable initial value from an 
 * ABSOLUTE clause (in Borland sense).  Nonzero -> absolute clause.
 */
#define PASCAL_ABSOLUTE_CLAUSE(init) TREE_LANG_FLAG_6 (init)

/*
 * Set this when compiler is checking if the for loop should be
 * executed at all (just to prevent the warnings in limited
 * range comparisons). Reset the bit after comparison.
 */
#define PASCAL_LOOP_CHECK(decl) PASCAL_VALUE_ASSIGNED(decl)

/*
 * Set this if NODE refers to a known identifier in Pascal.
 * It's used in gpc-lex.c yylex() to return correct tokens to the parser
 * when a Pascal predefined word is seen. Also it allows redefinition of
 * the known identifiers in pushdecl().
 *
 */
#define PASCAL_REDEFINABLE_DECL(decl) DECL_LANG_FLAG_7 (decl)

/* Pascal type variants. 32 different types fit in one integer */
#define TYPE_QUALIFIER_PACKED        1
#define TYPE_QUALIFIER_PROTECTED     2
#define TYPE_QUALIFIER_QUALIFIED     4
#define TYPE_QUALIFIER_BINDABLE      8
#define TYPE_QUALIFIER_RESTRICTED   16
#define TYPE_QUALIFIER_CONFORMANT   32
#define TYPE_QUALIFIER_SHORT	    64
#define TYPE_QUALIFIER_LONG   	   128
#define TYPE_QUALIFIER_LONGLONG    256
#define TYPE_QUALIFIER_UNSIGNED    512
#define TYPE_QUALIFIER_BYTE	  1024

#define TYPE_VARIANT (TYPE_QUALIFIER_BINDABLE		\
		      | TYPE_QUALIFIER_RESTRICTED)

#define TYPE_SELECT (TYPE_QUALIFIER_BYTE		\
		     | TYPE_QUALIFIER_SHORT		\
		     | TYPE_QUALIFIER_LONG		\
		     | TYPE_QUALIFIER_LONGLONG		\
		     | TYPE_QUALIFIER_UNSIGNED)

/* Determine if something may be a function call. */

#define MAYBE_CALL_FUNCTION(target) (TREE_CODE (target) == FUNCTION_DECL     \
                     || (TREE_CODE (target) == PARM_DECL                     \
                         && TREE_CODE (TREE_TYPE (target)) == POINTER_TYPE   \
                         && TREE_CODE (TREE_TYPE (TREE_TYPE (target ))) ==   \
                            FUNCTION_TYPE))

/* Determine if something is a method call. */

#define CALL_METHOD(target) (TREE_CODE (target) == COMPONENT_REF             \
                         && TREE_CODE (TREE_TYPE (target)) == FUNCTION_TYPE)

/* Determine if a given TYPE node represents an OBJECT type */

#define IS_OBJECT_TYPE(type) (TREE_CODE (type) == RECORD_TYPE                \
                              && TYPE_LANG_SPECIFIC (type)                   \
                              && TYPE_LANG_SPECIFIC (type)->len == -2)

/* In a VAR_DECL node: check if it represents an ABSOLUTE
 * declared variable (in Borland sense) 
 */
#define IS_ABSOLUTE_VAR(decl) (TREE_CODE (decl) == VAR_DECL                  \
                           && DECL_INITIAL (decl)                            \
                           && PASCAL_ABSOLUTE_CLAUSE (DECL_INITIAL (decl)))

/* Module support */
typedef struct module  *module_t;

struct module {
  /* TREE_LIST of imported interfaces for this module.  TREE_VALUE is
   * an IDENTIFIER_NODE of an imported interface name.  TREE_PURPOSE
   * is TREE_LIST chain of the names (? decls ?) imported by this
   * interface */
  tree imports;

  /* TREE_LIST of interfaces exported from this module TREE_VALUE is
   * an IDENTIFIER_NODE of an exported interface name.  TREE_PURPOSE
   * is TREE_LIST chain of the names (? decls ?) exported by this
   * module */
  tree exports;

  /* Unexported declarations made in the interface module */
  tree pending_decls;

  /* This module name */
  tree name;

  /* Module parameters */
  tree parms;

  /* What shall be exported automatically? */
  tree autoexport;

  /* ?? */
  tree implementation;

  /* ?? */
  tree interface;

  /* Standard output for this module */
  tree output_file_node;

  /* Standard input for this module */
  tree input_file_node;

  /* If this is the main program */
  int main_program;

  /* Next module */
  module_t	next;

};

#define NULL_MODULE ((module_t) 0)

extern module_t current_module;
extern int this_is_an_interface_module;
extern int we_are_loading_a_gpi_file;
extern tree standard_interface_input;
extern tree standard_interface_output;

/* External definitions */

extern int last_id_value;

extern tree type_id;
extern tree const_id;
extern tree inline_id;
extern tree varparm_id;
extern tree volatile_id;
extern tree auto_id;
extern tree extern_id;
extern tree static_id;

extern tree string_schema_proto_type;

extern int current_function_assigned_value;

extern tree ptr_type_node;
extern tree value_identifier;

extern tree boolean_false_node;
extern tree boolean_true_node;
extern tree complex_zero_node;
extern tree real_zero_node;
extern tree integer_maxint_node;
extern tree null_pointer_node;
extern tree empty_set_node;

/* node for marking virtual methods while parsing */
extern tree virtual_mark_node;

/* identifier node for implicit `Self' formal parameter */
extern tree self_name_node;

/* Extended Pascal nodes */
extern tree real_max_node;
extern tree real_min_node;
extern tree real_eps_node;
extern tree char_max_node;

/* Extended pascal pre-defined type names */
extern tree gpc_type_TIMESTAMP;
extern tree gpc_type_BINDINGTYPE;

extern tree empty_arglist;

/* not used */ extern tree char_array_index_type_node;

extern tree cstring_type_node;

/* directives */
extern tree d_forward;
extern tree d_external;
extern tree d_extern;
extern tree d_c_lang;
extern tree d_c_lang1;
extern tree d_asmname;
extern tree d_o_override;
extern tree d_static;

/* Export "all" mark */
extern tree export_all;

/* A place too look up the name of a currently declared type 
 * (needed to define assembler-names for object methods)
 */
extern tree current_type_name;

/* Must a function get an implicit "Self" parameter or not? */
extern int defining_methods;

/* For giving the correct --pedantic warning only ... */
extern int defining_operators;

/* Nonzero if we warn about void type pointer dereferences. */
extern int warn_void_dereference;

struct lang_type *allocate_type_lang_specific PROTO((void));

void associate_external_objects PROTO((tree));

tree make_signed_range PROTO((tree,tree));

void un_initialize_block PROTO((tree, int));
void handle_formal_param_list PROTO((tree,tree,int,int));
void handle_formal_conf_array_param_list PROTO((tree,tree,int,int));

int top_level_p PROTO ((int));
tree p_grokfields PROTO ((tree,tree));
tree build_rts_call PROTO ((int, tree));
tree build_pascal_string_schema PROTO ((tree));
tree build_pascal_array_type PROTO ((tree,tree));
tree build_pascal_array_ref PROTO ((tree,tree));
tree build_pascal_pointer_type PROTO ((tree));
void declare_rts_types PROTO ((void));
tree build_buffer_ref PROTO ((tree));

tree error_level PROTO((char *));
tree object_size PROTO((tree));

tree construct_set_member PROTO ((tree,tree));
tree construct_set PROTO ((tree,tree,int));
tree build_set_constructor PROTO ((tree));

tree grok_packed PROTO ((tree));
void grok_directive PROTO ((tree,tree,tree,int));

tree build_file_type PROTO ((tree, tree));
tree build_set_type PROTO((tree));
tree build_record_variant_part PROTO ((char *, int, tree, tree));

tree pascal_type_variant PROTO ((tree, int, int));
tree pascal_type_extension PROTO ((tree, int));

char* concat PROTO ((char*, char*, char*));
char* save_string PROTO ((char*));

/* Nonzero means use lazy I/O for text files */
extern int flag_lazy_io;

/* Nonzero means always use short circuit boolean AND and OR */
extern int flag_short_circuit;

/* Nonzero means allow gpc to use C style character escapes */
extern int flag_c_escapes;

/* Nonzero means allow gpc to read C style Octal and Hex numbers */
extern int flag_c_numbers;

/* Level of keywords we recognize */
extern int flag_what_pascal;

/* Nonzero means don't warn about "extended syntax" */
extern int flag_extended_syntax;

/* Nonzero means print gpi debug information to stderr */
extern int flag_debug_gpi;

/* Nonzero means do an Automake */
extern int flag_automake;

/* Options to pass to child gpc processes in AutoMake */
extern char* automake_gpc_options;

/* Name of a file to store information about targets in */
extern char* automake_temp_filename;

/* Nonzero if called with --version (i.e. gpc called with --verbose) */
extern int version_flag;

/* Nonzero means allow nested comments */
/* (* { } *) and { (* *) } are ok */
/* this violates standard */
extern int flag_nested_comments;

extern int gpc_test1;
extern int gpc_test2;
extern int gpc_test3;
extern int gpc_test4;

void init_std_files PROTO((void));

/* convert the TYPE to a range usable as ardinal type
 * if approppriate
 */
tree convert_type_to_range PROTO((tree));

tree probably_call_function PROTO((tree));
tree maybe_call_function PROTO((tree, tree));

/* Handling of objects, methods, and VMTs
 */
tree call_method PROTO((tree, tree));
tree get_method_name PROTO((tree, tree));
void push_self_parameter PROTO((tree, int));
void store_object_name PROTO((tree));
void inherit PROTO((tree, tree));
void add_vmt_field PROTO((tree));
void construct_vmt PROTO((tree));
tree mark_virtual_method PROTO((tree));

/* Enable/disable keywords in certain contexts.
 * With this strategy we can avoid shift/reduce
 * and/or reduce/reduce conflicts while keeping
 * all keywords redefinable.
 */
void enable_keyword PROTO((char*));
void disable_keyword PROTO((char*));

int suspend_function_calls PROTO((void));

void resume_function_calls PROTO((int));

int resolve_forward_pointer PROTO((tree, tree));
tree lookup_forward_pointer PROTO((tree, int));

/* a chain of TREE_LIST nodes that contain the with_elements
 * of the currently active WITH statements
 */
extern tree with_element_chain;

/* Locates from structure/union DATUM the field NAME, and consturucts
 * an expression to refer to that field
 */
tree find_field PROTO ((tree, tree));

/*
 * check the declaration associated with the identifier
 */
tree check_identifier PROTO ((tree, tree));

tree convert_array_to_pointer PROTO ((tree));

/*
 * handles directives, calls start_function and returns the function
 * return type or 0
 */
tree start_pascal_function 		PROTO((tree, tree, int));

/*
 * construct an identifier_node for the assembler-name
 * of a user-defined operator
 */
tree get_operator_identifier		PROTO((char*, tree, tree));

/* convert set operations to their bitwise form;
 * handle user-defined operators;
 * for others just call build_binary_op_nodefault.
 */
tree build_pascal_op			PROTO((enum tree_code, tree, tree));

/* convert PXSC operators to function calls
 */
tree build_pxsc_operator		PROTO((char*, tree, tree));

/* pascal unary op */
tree build_pascal_unary_op		PROTO((enum tree_code, tree, int));

/*
 * labels declared at the current level
 */
extern tree declared_labels;

/*
 * Name of the main program, usually "main"
 */
extern char *gpc_main;

/*
 * Build a FUNCTION_DECL node to define the DECL_CONTEXT of the
 * main program before start_function() is called to make non-local
 * gotos to main program work.
 */
extern tree declare_main_program_context	PROTO((tree,tree));

/*
 * Top level context of the Pascal program
 */
extern tree main_program_context;

/* Specifies the maximum number of members in any Pascal set
 * -fsetlimit:XX sets the limit to XX
 */
extern int requested_set_size;

extern tree integer_set_type_node;

/* Get a unique identifier (acting as a temporary internal variable)
 */
tree get_unique_identifier 	PROTO((char *, int));

/*
 * Returns the base type of a SET_TYPE node TYPE
 */
tree base_type 			PROTO((tree));

tree get_identifier_with_blank	PROTO((char *));

void output_real_main_program	PROTO((tree));
tree get_main_program_name 	PROTO((tree));

void store_variant_tag_info 	PROTO((tree, tree));
tree get_variant_tag_info	PROTO((tree));

tree build_new_string_schema PROTO((void));

/* Build an initializer out of a tree-list */
tree build_pascal_initializer PROTO((tree, tree));

int  is_string_type PROTO((tree));
int  is_of_string_type PROTO((tree));

tree new_string_by_model PROTO((tree, tree, int));

tree make_new_variable PROTO((char *, tree));

tree check_if_predefined_type PROTO((tree));

tree module_export_clause PROTO((tree,tree,int));
tree module_export_range PROTO((tree,tree));
int  name_exported_p PROTO ((tree));
void handle_autoexport PROTO ((tree));
void add_to_automake_temp_file PROTO ((char *));
int  pass_automake_gpc_options PROTO ((char **, int));
int  module_must_be_recompiled PROTO ((char *, char *));
int  compile_module PROTO ((char *));
void store_string PROTO ((FILE *, const char *));
void load_string PROTO ((FILE *, char *));
void store_flags PROTO ((tree, FILE *));
void load_flags PROTO ((tree, FILE *));
void store_type_flags PROTO ((tree, FILE *));
void load_type_flags PROTO ((tree, FILE *));
void store_decl_flags PROTO ((tree, FILE *));
void load_decl_flags PROTO ((tree, FILE *));
void store_tree PROTO ((tree, FILE *, int));
tree load_tree PROTO ((FILE *, int));
FILE *gpi_open PROTO ((char *, int));
tree load_gpi_file PROTO ((char *));
void create_gpi_files PROTO ((void));
tree maybe_make_static PROTO ((tree));

void import_interface PROTO((tree,tree,long));
void export_interface PROTO((tree,tree));

char *which_language PROTO((int));

int  is_known_directive PROTO((tree));

void declare_vars PROTO((tree, tree, tree, tree, int));

tree de_capitalize PROTO((tree));

tree get_standard_input PROTO ((void));
tree get_standard_output PROTO ((void));

/* This is not present in the generic defs of GCC */
tree change_main_variant PROTO ((tree, tree));

tree no_parameters PROTO ((void));

#endif /* GPC */
