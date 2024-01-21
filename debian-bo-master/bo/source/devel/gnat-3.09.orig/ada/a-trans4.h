/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             A - T R A N S 4                              */
/*                                                                          */
/*                              C Header File                               */
/*                                                                          */
/*                            $Revision: 1.31 $                             */
/*                                                                          */
/*    Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.   */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms of the  GNU General Public License as published  by the Free Soft- */
/* ware  Foundation;  either version 2,  or (at your option) any later ver- */
/* sion.  GNAT is distributed in the hope that it will be useful, but WITH- */
/* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/* for  more details.  You should have  received  a copy of the GNU General */
/* Public License  distributed with GNAT;  see file COPYING.  If not, write */
/* to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/* MA 02111-1307, USA.                                                      */
/*                                                                          */
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR or other logical
   operation.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, integer_zero_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be the same as the input type.
   This function is simpler than the corresponding C version since
   the only possible operands will be things of Boolean type.  */
extern tree truthvalue_conversion      PROTO((tree));

/* Return the base type of TYPE.  */
extern tree get_base_type	PROTO((tree));

/* Likewise, but only return types known at Ada source.  */
extern tree get_ada_base_type	PROTO((tree));

/* Make a binary operation of kind OP_CODE.  RESULT_TYPE is the type
   desired for the result.  Usually the operation is to be performed
   in that type.  For MODIFY_EXPR and ARRAY_REF, RESULT_TYPE may be 0
   in which case the type to be used will be derived from the operands.  */
extern tree build_binary_op	PROTO((enum tree_code, tree, tree, tree));

/* Similar, but make unary operation.   */
extern tree build_unary_op	PROTO((enum tree_code, tree, tree));

/* Similar, but for COND_EXPR.  */
extern tree build_cond_expr	PROTO((tree, tree, tree, tree));

/* Build a CALL_EXPR to call FUNDECL with one argument, ARG.  Return
   the CALL_EXPR.  */
extern tree build_call_1_expr	PROTO((tree, tree));

/* Likewise to call FUNDECL with no arguments.  */
extern tree build_call_0_expr	PROTO((tree));

/* Return a CONSTRUCTOR of TYPE whose list is LIST.  */
extern tree build_constructor	PROTO((tree, tree));

/* Return a COMPONENT_REF to access a field that is given by COMPONENT,
   an IDENTIFIER_NODE giving the name of the field, FIELD, a FIELD_DECL,
   for the field, or both.  */
extern tree build_component_ref	PROTO((tree, tree, tree));

/* Build a GCC tree to call an allocation or deallocation function.
   If GNU_OBJ is nonzero, it is an object to deallocate.  Otherwise,
   genrate an allocator.

   GNU_SIZE is the size of the object and ALIGN is the alignment.
   GNAT_PROC, if present is a procedure to call and GNAT_POOL is the
   storage pool to use.  If not preset, malloc and free will be used.  */
extern tree build_call_alloc_dealloc PROTO((tree, tree, int, Entity_Id,
					    Entity_Id));

/* Build a GCC tree to correspond to allocating an object of TYPE whose
   initial value if INIT, if INIT is nonzero.  Convert the expression to
   RESULT_TYPE, which must be some type of pointer.  Return the tree. 
   GNAT_PROC and GNAT_POOL optionally give the procedure to call and
   the storage pool to use.  */
extern tree build_allocator	PROTO((tree, tree, tree, Entity_Id,
				       Entity_Id));

/* Fill in a VMS descriptor for EXPR and return a constructor for it. 
   GNAT_FORMAL is how we find the descriptor record.  */

extern tree fill_vms_descriptor PROTO((tree, Entity_Id));

/* Indicate that we need to make the address of EXPR_NODE and it therefore
   should not be allocated in a register. Return 1 if successful.  */
extern int mark_addressable	PROTO((tree));
