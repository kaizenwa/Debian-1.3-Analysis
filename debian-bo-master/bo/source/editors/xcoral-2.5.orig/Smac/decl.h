/* ########################################################################

				 decl.h

   File: decl.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/decl.h
   Description: 
   Created: Tue Feb 21 12:52:20 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:52:21 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */



#include <string.h>

/* Pour aider la construction des instructions */

#include "list.h"
#include "Instruction.h"
  
extern FCT( Instruction *, make_integer,(List *) );
#define memo_integer(str)		\
	AddLast(Memo, List__List(make_integer, (void *) atoi(str)))

#define memo_negate_integer(str)		\
        char * s = (char *) Malloc(strlen(str) + 2);	\
        *s = '-';				\
       	strcpy(s + 1, str);			\
	AddLast(Memo, List__List(make_integer, (void *) -atoi(str)))

extern FCT( Instruction *, make_char,(List *) );
#define memo_char(str)		\
	AddLast(Memo, List__List(make_char, (void *) lire_char(str)))

extern FCT( Instruction *, make_string,(List *) );
extern FCT( char *, lire_string,(char *)	);
#define memo_string(str)		\
	AddLast(Memo, List__List(make_string, lire_string(str)))
     
/**/

extern FCT( void, memo_empty_block,() );
extern FCT( void, memo_block,() );

#define mark_begin_block()		\
  	extern char * begin_block;	\
  	AddLast(Memo, List__List(0, begin_block))
/**/

extern FCT( Instruction *, make_binary_oper,(List *) );
#define memo_binary_oper(str)		\
        void ** m = (void **) Malloc(3 * sizeof(void *));	\
        m[0] = (void*) str;		\
        PopLast(Memo, m[2]);		\
        PopLast(Memo, m[1]);		\
	AddLast(Memo, List__List(make_binary_oper, (char*) m))

		
extern FCT( Object, make_identifier,(List *) );
#define memo_identifier(str)		\
	AddLast(Memo, List__List((InstrFct) make_identifier, Strdup(str)))

extern FCT( Type *, make_typedef,(Type *) );
#define memo_typedef(type)		\
	AddLast(Memo, List__List((InstrFct) make_typedef, (char *) type))

/**/

extern FCT( Instruction *, make_oper_ampersand,(List *)	);
extern FCT( Instruction *, make_oper_asterisk,(List *)	);
extern FCT( Instruction *, make_oper_mono_plus,(List *)	);
extern FCT( Instruction *, make_oper_mono_minus,(List *)	);
extern FCT( Instruction *, make_oper_complement,(List *)	);
extern FCT( Instruction *, make_oper_not,(List *)	);

#define memo_unary_oper()		\
       PopLast(Memo, Memo->previous->info)

#define memo_oper_ampersand()	\
       AddLast(Memo, List__List(make_oper_ampersand, 0))
     
#define memo_oper_asterisk()	\
       AddLast(Memo, List__List(make_oper_asterisk, 0))
     
#define memo_oper_mono_plus()	\
       AddLast(Memo, List__List(make_oper_mono_plus, 0))
     
#define memo_oper_mono_minus()	\
       AddLast(Memo, List__List(make_oper_mono_minus, 0))
     
#define memo_oper_complement()	\
       AddLast(Memo, List__List(make_oper_complement, 0))
     
#define memo_oper_not()	\
       AddLast(Memo, List__List(make_oper_not, 0))

/**/

#define mark_asterisk()		\
  	extern char * asterisk;	\
	AddLast(Memo, List__List((InstrFct) asterisk, 0))

extern FCT( Instruction *, make_cast,(List *) );
#define memo_cast()		\
        void ** m = (void **) Malloc(2 * sizeof(void *));	\
        extern Type * the_abstract_type();		\
        PopLast(Memo, m[1]);		\
        m[0] = the_abstract_type();		\
	AddLast(Memo, List__List(make_cast, m))

/**/

extern FCT( Instruction *, make_arith_if,(List *) );
#define memo_arith_if()		\
       	List ** m = (List **) Malloc(3 * sizeof(List *));	\
        PopLast(Memo, m[2]);	\
        PopLast(Memo, m[1]);	\
        PopLast(Memo, m[0]);	\
	AddLast(Memo, List__List(make_arith_if, m))

/**/
     
extern FCT( Instruction *, make_array_access,(List *) );
#define memo_array_access()		\
       	List ** m = (List **) Malloc(2 * sizeof(void *));	\
        PopLast(Memo, m[1]);	\
        PopLast(Memo, m[0]);	\
	AddLast(Memo, List__List((InstrFct) make_array_access, m))

/**/
     
extern FCT( Instruction *, make_assignment,(List *) );
#define memo_assignment()		\
       	List ** m = (List **) Malloc(3 * sizeof(List *));	\
        PopLast(Memo, m[2]);	\
        PopLast(Memo, m[1]);	\
        PopLast(Memo, m[0]);	\
	AddLast(Memo, List__List(make_assignment, m))

/**/

extern FCT( Instruction *, make_control,(List *) );
#define memo_break()		\
	AddLast(Memo, List__List(make_control, 0))

#define memo_continue()		\
	AddLast(Memo, List__List(make_control, (char *) 1))

extern FCT( Instruction *, make_return,(List *) );
#define memo_return()		\
        List * l = List__List(Memo->fct, Memo->info);	\
        Memo->fct = make_return;	\
        Memo->info = (char *) l

/**/

extern FCT( Instruction *, make_case,(List *) );
#define memo_case()		\
        List * l = List__List(Memo->fct, Memo->info);	\
        Memo->fct = make_case;	\
        Memo->info = l

#define memo_default()		\
	AddLast(Memo, List__List(make_case, 0))

extern FCT( void, memo_switch,() );

extern FCT( Instruction *, make_if,(List *) );
#define memo_if()		\
        void ** m = (void **) Malloc(3 * sizeof(void *));	\
        m[2] = 0;	\
        PopLast(Memo, m[1]);		\
        PopLast(Memo, m[0]);		\
	AddLast(Memo, List__List(make_if, m))

#define memo_if_else()		\
        void ** m = (void **) Malloc(3 * sizeof(void *));	\
        PopLast(Memo, m[2]);		\
        PopLast(Memo, m[1]);		\
        PopLast(Memo, m[0]);		\
	AddLast(Memo, List__List(make_if, m))

extern FCT( Instruction *, make_do_while,(List *) );
#define memo_do_while()		\
        void ** m = (void **) Malloc(2 * sizeof(void *));	\
        PopLast(Memo, m[1]);		\
        PopLast(Memo, m[0]);		\
	AddLast(Memo, List__List(make_do_while, m))

extern FCT( Instruction *, make_while,(List *) );
#define memo_while()		\
        void ** m = (void **) Malloc(2 * sizeof(void *));	\
        PopLast(Memo, m[1]);		\
        PopLast(Memo, m[0]);		\
	AddLast(Memo, List__List(make_while, m))

extern FCT( Instruction *, make_for,(List *) );
#define memo_for()		\
        void ** m = (void **) Malloc(4 * sizeof(void *));	\
        PopLast(Memo, m[3]);		\
        PopLast(Memo, m[2]);		\
        PopLast(Memo, m[1]);		\
        PopLast(Memo, m[0]);		\
	AddLast(Memo, List__List(make_for, m))

/**/

extern FCT( void, memo_comma,() );
     
extern FCT( Instruction *, no_comma_expression,() );
#define memo_no_comma_expression()		\
	AddLast(Memo, List__List(no_comma_expression, 0))


/**/

#define mark_some_args()		\
  	extern char * some_args;	\
	AddLast(Memo, List__List(0, some_args))

#define mark_first_arg()		\
  	extern char * first_arg;	\
	AddLast(Memo, List__List(0, first_arg))

#define mark_one_arg()		\
  	extern char * one_arg;	\
	AddLast(Memo, List__List(0, one_arg))

extern FCT( Instruction *, make_funcall,(List *) );
     
#define memo_funcall_0_arg()		\
        char ** m = (char **) Malloc(2 * sizeof(void *));	\
        m[0] = (char *) 0;		\
        PopLast(Memo, m[1]);	\
	AddLast(Memo, List__List(make_funcall, (char*) m))

extern FCT( void, memo_funcall_nargs,() );

/**/

extern FCT( Instruction *, make_post_incrdecr,(List *) );
#define memo_post_decr()					\
        void ** m = (void **) Malloc(2 * sizeof(void *));	\
        m[0] = List__List(Memo->fct, Memo->info);	\
        m[1] = (char *) -1;				\
	Memo->fct = make_post_incrdecr;	\
        Memo->info = m

#define memo_post_incr()					\
        void ** m = (void **) Malloc(2 * sizeof(void *));	\
        m[0] = List__List(Memo->fct, Memo->info);	\
        m[1] = (char *) 1;				\
	Memo->fct = make_post_incrdecr;	\
        Memo->info = m

#define memo_pre_decr()		\
        void ** m = (void **) Malloc(3 * sizeof(void *));	\
        m[0] = List__List(Memo->fct, Memo->info);	\
        m[1] = List__List((InstrFct) make_identifier, Strdup("-="));	\
        m[2] = List__List(make_integer, (void *) 1);	\
	Memo->fct = make_assignment;	\
        Memo->info = m

#define memo_pre_incr()		\
        void ** m = (void **) Malloc(3 * sizeof(void *));	\
        m[0] = List__List(Memo->fct, Memo->info);	\
        m[1] = List__List((InstrFct) make_identifier, Strdup("+="));	\
        m[2] = List__List(make_integer, (void *) 1);	\
	Memo->fct = make_assignment;	\
        Memo->info = m

/**/

extern FCT( Instruction *, make_sizeof_type,(List *) );
#define memo_sizeof_type()		\
        extern Type * the_abstract_type();	\
        Type * type = the_abstract_type();	\
	AddLast(Memo, List__List(make_sizeof_type, type))

/**/

#define mark_no_initializer()		\
  	extern char * no_initializer;	\
	AddLast(Memo, List__List(0, no_initializer))

#define mark_begin_initializer_list()	\
  	extern char * begin_initializer_list;	\
  	AddLast(Memo, List__List(0, begin_initializer_list))

extern FCT ( void, memo_initializer_list,()	);

/**/
     
#define mark_begin_of_list()		\
  	extern char * begin_of_list;	\
  	AddLast(Memo, List__List(0, begin_of_list))

extern FCT( void, end_of_list, () );
extern FCT( void, forget_list, () );
extern FCT( void, forget_list_if_possible, () );


/**/

#define mark_begin_function_parameter_type_list()		\
        extern char * begin_function_parameter_type_list;	\
  	AddLast(Memo, List__List(0, begin_function_parameter_type_list))

extern FCT( void, end_function_parameter_type_list,() );
extern FCT( void, end_identifier_list,() );

#define mark_begin_identifier_list()		\
       	extern char * begin_identifier_list;	\
  	AddLast(Memo, List__List(0, begin_identifier_list))

FCT( void, function_parameter_type_list,()	);
#define memo_empty_function_parameter_type_list()	\
	AddLast(Memo, List__List((InstrFct) function_parameter_type_list, 0))

extern FCT(void, ignored_param, () );
extern FCT(void, typed_param, () );

#define memo_ellipsis()		\
       	extern char* ellipsis;	\
	AddLast(Memo, List__List((InstrFct) ellipsis, 0))

    
/**/

extern FCT( void, partial_declaration,() );
extern FCT( void, declaration,() );

/**/

extern FCT(void, array_dim,()			);

#define memo_array_decl_dim()		\
  	List * l = List__List(Memo->fct, Memo->info);	\
  	Memo->fct = (InstrFct) array_dim;	\
  	Memo->info = (char *) l

extern FCT( Instruction *, array_decl_without_dim,(char *) );
#define memo_array_decl_without_dim()		\
	AddLast(Memo, List__List((InstrFct) array_dim, 0))

extern FCT( void, array_abstract_declarator,() );

/**/

extern int make_function_def();
#define memo_make_function_def()		\
	AddLast(Memo, List__List((InstrFct) make_function_def, 0))

extern int make_typedef_function_def();
#define memo_make_typedef_function_def()		\
	AddLast(Memo, List__List((InstrFct) make_typedef_function_def, 0))

extern int make_typedef_oldfunction_def_without_decllist();
#define memo_make_typedef_oldfunction_def_without_decllist()		\
  	AddLast(Memo, List__List((InstrFct) make_typedef_oldfunction_def_without_decllist, 0))

extern int make_oldfunction_def_without_decllist();
#define memo_make_oldfunction_def_without_decllist()		\
  	AddLast(Memo, List__List((InstrFct) make_oldfunction_def_without_decllist, 0))

extern int make_typedef_oldfunction_def();
#define memo_make_typedef_oldfunction_def()		\
  	AddLast(Memo, List__List((InstrFct) make_typedef_oldfunction_def, 0))

extern int make_oldfunction_def();
#define memo_make_oldfunction_def()		\
  	AddLast(Memo, List__List((InstrFct) make_oldfunction_def, 0))
