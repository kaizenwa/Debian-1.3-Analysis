/*

COPYRIGHT

Copyright 1992, 1993, 1994 Sun Microsystems, Inc.  Printed in the United
States of America.  All Rights Reserved.

This product is protected by copyright and distributed under the following
license restricting its use.

The Interface Definition Language Compiler Front End (CFE) is made
available for your use provided that you include this license and copyright
notice on all media and documentation and the software program in which
this product is incorporated in whole or part. You may copy and extend
functionality (but may not remove functionality) of the Interface
Definition Language CFE without charge, but you are not authorized to
license or distribute it to anyone else except as part of a product or
program developed by you or with the express written consent of Sun
Microsystems, Inc. ("Sun").

The names of Sun Microsystems, Inc. and any of its subsidiaries or
affiliates may not be used in advertising or publicity pertaining to
distribution of Interface Definition Language CFE as permitted herein.

This license is effective until terminated by Sun for failure to comply
with this license.  Upon termination, you shall destroy or return all code
and documentation for the Interface Definition Language CFE.

INTERFACE DEFINITION LANGUAGE CFE IS PROVIDED AS IS WITH NO WARRANTIES OF
ANY KIND INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS
FOR A PARTICULAR PURPOSE, NONINFRINGEMENT, OR ARISING FROM A COURSE OF
DEALING, USAGE OR TRADE PRACTICE.

INTERFACE DEFINITION LANGUAGE CFE IS PROVIDED WITH NO SUPPORT AND WITHOUT
ANY OBLIGATION ON THE PART OF Sun OR ANY OF ITS SUBSIDIARIES OR AFFILIATES
TO ASSIST IN ITS USE, CORRECTION, MODIFICATION OR ENHANCEMENT.

SUN OR ANY OF ITS SUBSIDIARIES OR AFFILIATES SHALL HAVE NO LIABILITY WITH
RESPECT TO THE INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY
INTERFACE DEFINITION LANGUAGE CFE OR ANY PART THEREOF.

IN NO EVENT WILL SUN OR ANY OF ITS SUBSIDIARIES OR AFFILIATES BE LIABLE FOR
ANY LOST REVENUE OR PROFITS OR OTHER SPECIAL, INDIRECT AND CONSEQUENTIAL
DAMAGES, EVEN IF SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

Use, duplication, or disclosure by the government is subject to
restrictions as set forth in subparagraph (c)(1)(ii) of the Rights in
Technical Data and Computer Software clause at DFARS 252.227-7013 and FAR
52.227-19.

Sun, Sun Microsystems and the Sun logo are trademarks or registered
trademarks of Sun Microsystems, Inc.

SunSoft, Inc.  
2550 Garcia Avenue 
Mountain View, California  94043

NOTE:

SunOS, SunSoft, Sun, Solaris, Sun Microsystems or the Sun logo are
trademarks or registered trademarks of Sun Microsystems, Inc.

 */

/*
 *======================================================================
 *
 * Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  
 *
 * Unlimited use, reproduction, and distribution of this software is
 * permitted.  Any copy of this software must include both the above
 * copyright notice of Xerox Corporation and this paragraph.  Any
 * distribution of this software must comply with all applicable United
 * States export control laws.  This software is made available AS IS,
 * and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
 * INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
 * PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
 * THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
 * CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
 * XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 *======================================================================
 */

#ifndef _BE_CLASSES_BE_CLASSES_HH
#define _BE_CLASSES_BE_CLASSES_HH

#pragma ident "%@(#)be_classes.h	1.20% %92/06/10% Sun Microsystems"
// $Id: be_classes.hh,v 2.3 1996/04/30 06:33:26 janssen Exp $

// be_classes.hh
//
// Defines all classes for dummy BE

/*
 * BE_PredefinedType
 */
class	be_predefined_type : public virtual AST_PredefinedType {
public:
  // Operations
  be_predefined_type();
  be_predefined_type(AST_PredefinedType::PredefinedType t, UTL_ScopedName *n,
		     UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);
  void gen_type_name(ostream &o);
  idl_bool gen_isl_typedef(ostream &o, AST_Decl *td);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_predefined_type, AST_PredefinedType);
  DEF_NARROW_FROM_DECL(be_predefined_type);
};

/*
 * BE_Module
 */

class	be_module : public virtual AST_Module {
public:
  // Operations
  be_module();
  be_module(UTL_ScopedName *n, UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_module, AST_Module);
  DEF_NARROW_FROM_DECL(be_module);
  DEF_NARROW_FROM_SCOPE(be_module);
};

/*
 * BE_Root
 */

class	be_root : public virtual AST_Root {
public:
  // Operations
  be_root();
  be_root(UTL_ScopedName *n, UTL_StrList *p);

  // ISL generation
  void gen_isl(ostream &o);

  // Dumping
  void be_dump(ostream &o);

  // Narrowing
  DEF_NARROW_METHODS1(be_root, AST_Root);
  DEF_NARROW_FROM_DECL(be_root);
  DEF_NARROW_FROM_SCOPE(be_root);
};

/*
 * BE_Interface
 */
class	be_interface : public virtual AST_Interface {
public:
  // Operations
  be_interface();
  be_interface(UTL_ScopedName *n, AST_Interface **ih, long nih,
	       UTL_StrList *p);

  char *repository_id(void);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);
  void gen_type_name(ostream &o);
  void gen_type_id(ostream &o);
  idl_bool gen_isl_typedef(ostream &o, AST_Decl *td);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_interface, AST_Interface);
  DEF_NARROW_FROM_DECL(be_interface);
  DEF_NARROW_FROM_SCOPE(be_interface);

 private:
  char *_repository_id;
};

/*
 * BE_InterfaceFwd
 */
class	be_interface_fwd : public virtual AST_InterfaceFwd {
public:
  // Operations
  be_interface_fwd();
  be_interface_fwd(UTL_ScopedName *n, UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);
  void gen_type_name(ostream &o);
  idl_bool gen_isl_typedef(ostream &o, AST_Decl *td);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_interface_fwd, AST_InterfaceFwd);
  DEF_NARROW_FROM_DECL(be_interface_fwd);
};

/*
 * BE_Exception
 */
class	be_exception : public virtual AST_Exception {
public:
  // Operations
  be_exception();
  be_exception(UTL_ScopedName *n, UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_exception, AST_Exception);
  DEF_NARROW_FROM_DECL(be_exception);
  DEF_NARROW_FROM_SCOPE(be_exception);
};

/*
 * BE_Structure
 */
class	be_structure : public virtual AST_Structure {
public:
  // Operations
  be_structure();
  be_structure(UTL_ScopedName *n, UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);
  void gen_type_name(ostream &o);
  idl_bool gen_isl_typedef(ostream &o, AST_Decl *td);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_structure, AST_Structure);
  DEF_NARROW_FROM_DECL(be_structure);
  DEF_NARROW_FROM_SCOPE(be_structure);
};

/*
 * BE_Enum
 */
class	be_enum : public virtual AST_Enum {
public:
  // Operations
  be_enum();
  be_enum(UTL_ScopedName *n, UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);
  void gen_type_name(ostream &o);
  idl_bool gen_isl_typedef(ostream &o, AST_Decl *td);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_enum, AST_Enum);
  DEF_NARROW_FROM_DECL(be_enum);
  DEF_NARROW_FROM_SCOPE(be_enum);
};

/*
 * BE_Operation
 */
class	be_operation : public virtual AST_Operation {
public:
  // Operations
  be_operation();
  be_operation(AST_Type *rt, AST_Operation::Flags fl, UTL_ScopedName *n,
	       UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_operation, AST_Operation);
  DEF_NARROW_FROM_DECL(be_operation);
  DEF_NARROW_FROM_SCOPE(be_operation);
};

/*
 * BE_Field
 */
class	be_field : public virtual AST_Field {
public:
  // Operations
  be_field();
  be_field(AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_field, AST_Field);
  DEF_NARROW_FROM_DECL(be_field);
};

/*
 * BE_Argument
 */
class	be_argument : public virtual AST_Argument {
public:
  // Operations
  be_argument();
  be_argument(AST_Argument::Direction d, AST_Type *ft, UTL_ScopedName *n,
	      UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_argument, AST_Argument);
  DEF_NARROW_FROM_DECL(be_argument);
};

/*
 * BE_Attribute
 */
class	be_attribute : public virtual AST_Attribute {
public:
  // Operations
  be_attribute();
  be_attribute(idl_bool ro, AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_attribute, AST_Attribute);
  DEF_NARROW_FROM_DECL(be_attribute);
};

/*
 * BE_Union
 */
class	be_union : public virtual AST_Union {
public:
  // Operations
  be_union();
  be_union(AST_ConcreteType *dt, UTL_ScopedName *n, UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);
  void gen_type_name(ostream &o);
  idl_bool gen_isl_typedef(ostream &o, AST_Decl *td);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_union, AST_Union);
  DEF_NARROW_FROM_DECL(be_union);
  DEF_NARROW_FROM_SCOPE(be_union);

private:
  idl_bool needs_others_clause();
  void get_case_info(idl_bool *has_default_arm, unsigned long *num_cases);
};

/*
 * BE_UnionBranch
 */
class	be_union_branch : public virtual AST_UnionBranch {
public:
  // Operations
  be_union_branch();
  be_union_branch(AST_UnionLabelSet *labs, AST_Type *ft, UTL_ScopedName *n,
		  UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_union_branch, AST_UnionBranch);
  DEF_NARROW_FROM_DECL(be_union_branch);
};

/*
 * BE_UnionLabel
 */
class	be_union_label : public virtual AST_UnionLabel {
public:
  // Operations
  be_union_label();
  be_union_label(AST_UnionLabel::UnionLabel ul, AST_Expression *v);

  // Dumping
  void be_dump(ostream &o, int level);
};

/*
 * BE_Constant
 */
class	be_constant : public virtual AST_Constant {
public:
  // Operations
  be_constant();
  be_constant(AST_Expression::ExprType et,
	      AST_Expression *v,
	      UTL_ScopedName *n,
	      UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_constant, AST_Constant);
  DEF_NARROW_FROM_DECL(be_constant);
};

/*
 * BE_Expression
 */
class	be_expr_val : public virtual AST_Expression {
public:
  // Operations
  be_expr_val(UTL_ScopedName *n);
  be_expr_val(AST_Expression *b, AST_Expression::ExprType t);
  be_expr_val(AST_Expression::ExprComb c,
	      AST_Expression *v1,
	      AST_Expression *v2);
  be_expr_val(long l);
  be_expr_val(long l, AST_Expression::ExprType t);
  be_expr_val(unsigned long l);
  be_expr_val(String *s);
  be_expr_val(char c);
  be_expr_val(double d);
};

/*
 * BE_EnumVal
 */
class	be_enum_val : public virtual AST_EnumVal {
public:
  // Operations
  be_enum_val();
  be_enum_val(unsigned long v, UTL_ScopedName *n, UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_enum_val, AST_EnumVal);
  DEF_NARROW_FROM_DECL(be_enum_val);
};

/*
 * BE_Array
 */
class	be_array : public virtual AST_Array {
private:
  char *pd_generated_type_name;

public:
  // Operations
  be_array();
  be_array(UTL_ScopedName *n, unsigned long ndims, UTL_ExprList *dims);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);
  void gen_type_name(ostream &o);
  idl_bool gen_isl_typedef(ostream &o, AST_Decl *td);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_array, AST_Array);
  DEF_NARROW_FROM_DECL(be_array);
};

/*
 * BE_Sequence
 */
class	be_sequence : public virtual AST_Sequence {
private:
  char *pd_generated_type_name;

public:
  // Operations
  be_sequence();
  be_sequence(AST_Expression *v, AST_Type *bt);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);
  void gen_type_name(ostream &o);
  idl_bool gen_isl_typedef(ostream &o, AST_Decl *td);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_sequence, AST_Sequence);
  DEF_NARROW_FROM_DECL(be_sequence);
};

/*
 * BE_String
 */
class	be_string : public virtual AST_String {
private:
  char *pd_generated_type_name;

public:
  // Operations
  be_string();
  be_string(AST_Expression *v);
  be_string(AST_Expression *v, long wide);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);
  void gen_type_name(ostream &o);
  idl_bool gen_isl_typedef(ostream &o, AST_Decl *td);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_string, AST_String);
  DEF_NARROW_FROM_DECL(be_string);
};

/*
 * BE_Typedef
 */
class	be_typedef : public virtual AST_Typedef {
public:
  // Operations
  be_typedef();
  be_typedef(AST_Type *bt, UTL_ScopedName *n, UTL_StrList *p);

  // ISL generation
  void pregen_types(ostream &o);
  void gen_isl(ostream &o);
  void gen_type_name(ostream &o);
  idl_bool gen_isl_typedef(ostream &o, AST_Decl *td);

  // Dumping
  void be_dump(ostream &o, int level);

  // Narrowing
  DEF_NARROW_METHODS1(be_typedef, AST_Typedef);
  DEF_NARROW_FROM_DECL(be_typedef);
};

#endif           // _BE_CLASSES_BE_CLASSES_HH
