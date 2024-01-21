/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             A - T R A N S 4                              */
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*                            $Revision: 1.139 $                            */
/*                                                                          */
/*          Copyright (C) 1992-1997, Free Software Foundation, Inc.         */
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

#include "config.h"
#include "tree.h"
#include "flags.h"
#include "a-ada.h"
#include "a-types.h"
#include "a-atree.h"
#include "a-nlists.h"
#include "a-elists.h"
#include "a-sinfo.h"
#include "a-einfo.h"
#include "a-namet.h"
#include "a-snames.h"
#include "a-string.h"
#include "a-uintp.h"
#include "a-gtran3.h"
#include "a-trans.h"
#include "a-trans3.h"
#include "a-trans4.h"
#include "a-misc.h"

static tree find_common_type		PROTO((tree, tree));
static tree compare_arrays		PROTO((tree, tree, tree));
static tree nonbinary_modular_operation	PROTO((enum tree_code, tree,
					       tree, tree));

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR or other logical
   operation.

   This preparation consists of taking the ordinary representation of
   an expression expr and producing a valid tree boolean expression
   describing whether expr is nonzero. We could simply always do

      build_binary_op (NE_EXPR, expr, integer_zero_node, 1),

   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be the same as the input type.
   This function is simpler than the corresponding C version since
   the only possible operands will be things of Boolean type.  */

tree
truthvalue_conversion (expr)
     tree expr;
{
  register enum tree_code code;
  tree type = TREE_TYPE (expr);

  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:  case NE_EXPR: case LE_EXPR: case GE_EXPR:
    case LT_EXPR:  case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case ERROR_MARK:
      return expr;

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold (build (COND_EXPR, type, TREE_OPERAND (expr, 0),
			  truthvalue_conversion (TREE_OPERAND (expr, 1)),
			  truthvalue_conversion (TREE_OPERAND (expr, 2))));
    }

  return build_binary_op (NE_EXPR, type, expr,
			  convert (type, integer_zero_node));
}

/* Return the base type of TYPE.  */

tree
get_base_type (type)
     tree type;
{
  if (TREE_CODE (type) == RECORD_TYPE
      && TYPE_LEFT_JUSTIFIED_MODULAR_P (type))
    type = TREE_TYPE (TYPE_FIELDS (type));

  while (TREE_TYPE (type) != 0
	 && (TREE_CODE (type) == INTEGER_TYPE
	     || TREE_CODE (type) == REAL_TYPE))
    type = TREE_TYPE (type);

  return type;
}

/* Likewise, but only return types known to the Ada source.  */
tree
get_ada_base_type (type)
     tree type;
{
  while (TREE_TYPE (type) != 0
	 && (TREE_CODE (type) == INTEGER_TYPE
	     || TREE_CODE (type) == REAL_TYPE)
	 && ! TYPE_EXTRA_SUBTYPE_P (type))
    type = TREE_TYPE (type);

  return type;
}

/* We have a comparison or assignment operation on two types, T1 and T2,
   which are both either array types or both record types.
   Return the type that both operands should be converted to, if any.
   Otherwise return zero.  */

static tree
find_common_type (t1, t2)
     tree t1, t2;
{
  /* If either type is non-BLKmode, use it.  Note that we know that we will
     not have any alignment problems since if we did the non-BLKmode
     type could not have been used.  */
  if (TYPE_MODE (t1) != BLKmode)
    return t1;
  else if (TYPE_MODE (t2) != BLKmode)
    return t2;

  /* Otherwise, return the type that has a constant size.  */
  if (TREE_CONSTANT (TYPE_SIZE (t1)))
    return t1;
  else if (TREE_CONSTANT (TYPE_SIZE (t2)))
    return t2;

  /* In this case, both types have variable size.  It's probably
     best to leave the "type mismatch" because changing it could
     case a bad self-referential reference.  */
  return 0;
}

/* Return an expression tree representing an equality comparison of
   A1 and A2, two objects of ARRAY_TYPE.  The returned expression should
   be of type RESULT_TYPE

   Two arrays are equal in one of two ways: (1) if both have zero length
   in some dimension (not necessarily the same dimension) or (2) if the
   lengths in each dimension are equal and the data is equal.  We perform the
   length tests in as efficient a manner as possible.  */

static tree
compare_arrays (result_type, a1, a2)
     tree a1, a2;
     tree result_type;
{
  tree t1 = TREE_TYPE (a1);
  tree t2 = TREE_TYPE (a2);
  tree result = convert (result_type, integer_one_node);
  tree a1_is_null = convert (result_type, integer_zero_node);
  tree a2_is_null = convert (result_type, integer_zero_node);
  int length_zero_p = 0;

  /* Process each dimension separately and compare the lengths.  If any
     dimension has a size known to be zero, set SIZE_ZERO_P to 1 to
     suppress the comparison of the data.  */
  while (TREE_CODE (t1) == ARRAY_TYPE && TREE_CODE (t2) == ARRAY_TYPE)
    {
      tree lb1 = TYPE_MIN_VALUE (TYPE_DOMAIN (t1));
      tree ub1 = TYPE_MAX_VALUE (TYPE_DOMAIN (t1));
      tree lb2 = TYPE_MIN_VALUE (TYPE_DOMAIN (t2));
      tree ub2 = TYPE_MAX_VALUE (TYPE_DOMAIN (t2));
      tree bt = get_base_type (TREE_TYPE (lb1));
      tree length1 = fold (build (MINUS_EXPR, bt, ub1, lb1));
      tree length2 = fold (build (MINUS_EXPR, bt, ub2, lb2));
      tree nbt;
      tree tem;
      tree comparison;

      /* If the length of the first array is a constant, swap our operands
	 unless the length of the second array is the constant zero.  
	 Note that we have set the `length' values to the length - 1.  */
      if (TREE_CODE (length1) == INTEGER_CST
	  && ! integer_zerop (fold (build (PLUS_EXPR, bt, length2,
					   convert (bt, integer_one_node)))))
	{
	  tem = a1, a1 = a2, a2 = tem;
	  tem = t1, t1 = t2, t2 = tem;
	  tem = lb1, lb1 = lb2, lb2 = tem;
	  tem = ub1, ub1 = ub2, ub2 = tem;
	  tem = length1, length1 = length2, length2 = tem;
	  tem = a1_is_null, a1_is_null = a2_is_null, a2_is_null = tem;
	}

      /* If the length of this dimension in the second array is the constant
	 zero, we can just go inside the original bounds for the first
	 array and see if last < first.  */
      if (integer_zerop (fold (build (PLUS_EXPR, bt, length2,
				      convert (bt, integer_one_node)))))
	{
	  tree ub = TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t1)));
	  tree lb = TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t1)));

	  comparison = build_binary_op (LT_EXPR, result_type, ub, lb);

	  if (contains_placeholder_p (comparison))
	    comparison = build (WITH_RECORD_EXPR, result_type,
				comparison, a1);
	  if (contains_placeholder_p (length1))
	    length1 = build (WITH_RECORD_EXPR, bt, length1, a1);

	  length_zero_p = 1;
	}

      /* If the length is some other constant value, we know that the
	 this dimension in the first array cannot be superflat, so we
	 can just use its length from the actual stored bounds.  */
      else if (TREE_CODE (length2) == INTEGER_CST)
	{
	  ub1 = TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t1)));
	  lb1 = TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t1)));
	  ub2 = TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t2)));
	  lb2 = TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (t2)));
	  nbt = get_base_type (TREE_TYPE (ub1));

	  comparison
	    = build_binary_op (EQ_EXPR, result_type, 
			       build_binary_op (MINUS_EXPR, nbt, ub1, lb1),
			       build_binary_op (MINUS_EXPR, nbt, ub2, lb2));

	  /* Note that we know that UB2 and LB2 are constant and hence
	     cannot contain a PLACEHOLDER_EXPR.  */

	  if (contains_placeholder_p (comparison))
	    comparison = build (WITH_RECORD_EXPR, result_type, comparison, a1);
	  if (contains_placeholder_p (length1))
	    length1 = build (WITH_RECORD_EXPR, bt, length1, a1);
	}

      /* Otherwise compare the computed lengths.  */
      else
	{
	  if (contains_placeholder_p (length1))
	    length1 = build (WITH_RECORD_EXPR, bt, length1, a1);
	  if (contains_placeholder_p (length2))
	    length2 = build (WITH_RECORD_EXPR, bt, length2, a2);

	  comparison
	    = build_binary_op (EQ_EXPR, result_type, length1, length2);
	}

      result = build_binary_op (TRUTH_ANDIF_EXPR, result_type,
				result, comparison);
      a1_is_null
	= build_binary_op (TRUTH_ORIF_EXPR, result_type,
			   build_binary_op (LT_EXPR, result_type, length1,
					    convert (bt, integer_zero_node)),
			   a1_is_null);
      a2_is_null
	= build_binary_op (TRUTH_ORIF_EXPR, result_type,
			   build_binary_op (LT_EXPR, result_type, length2,
					    convert (bt, integer_zero_node)),
			   a2_is_null);

      t1 = TREE_TYPE (t1);
      t2 = TREE_TYPE (t2);
    }

  /* Unless the size of some bound is known to be zero, compare the
     data in the array.  */
  if (! length_zero_p)
    {
      tree type = find_common_type (TREE_TYPE (a1), TREE_TYPE (a2));

      if (type != 0)
	a1 = convert (type, a1), a2 = convert (type, a2);


      result
	= build_binary_op (TRUTH_ANDIF_EXPR, result_type, result,
			   build (EQ_EXPR, result_type, a1, a2));
    }

  result = build_binary_op (TRUTH_ORIF_EXPR, result_type,
			    build_binary_op (TRUTH_ANDIF_EXPR, result_type,
					     a1_is_null, a2_is_null),
			    result);

  /* If either array has side-effects, they must contains SAVE_EXPRs, which
     have to be evaluated before starting the comparison above since
     the place it would be otherwise evaluated would be wrong.  */
  if (TREE_SIDE_EFFECTS (a1))
    result = build (COMPOUND_EXPR, result_type, a1, result);
  if (TREE_SIDE_EFFECTS (a2))
    result = build (COMPOUND_EXPR, result_type, a2, result);

  return result;
}

/* Compute the result of applying OP_CODE to LHS and RHS, where both are of
   type TYPE.  We know that TYPE is a modular type with a nonbinary
   modulus.  */

static tree
nonbinary_modular_operation (op_code, type, lhs, rhs)
     enum tree_code op_code;
     tree type;
     tree lhs, rhs;
{
  tree modulus = TYPE_MODULUS (type);
  int needed_precision
    = (TREE_INT_CST_HIGH (modulus) != 0
       ? HOST_BITS_PER_WIDE_INT + floor_log2 (TREE_INT_CST_HIGH (modulus)) + 1
       : TREE_INT_CST_LOW (modulus) == 0 ? 1
       : floor_log2 (TREE_INT_CST_LOW (modulus)) + 1);
  int precision;
  int unsignedp = 1;
  tree op_type = type;
  tree result;

  /* If this is an addition of a constant, convert it to a subtraction
     of a constant since we can do that faster.  */
  if (op_code == PLUS_EXPR && TREE_CODE (rhs) == INTEGER_CST)
    rhs = fold (build (MINUS_EXPR, type, modulus, rhs)), op_code = MINUS_EXPR;

  /* For the logical operations, we only need PRECISION bits.  For
     addition and subraction, we need one more and for multiplication we
     need twice as many.  But we never want to make a size smaller than
     our size. */
  if (op_code == PLUS_EXPR || op_code == MINUS_EXPR)
    needed_precision += 1;
  else if (op_code == MULT_EXPR)
    needed_precision *= 2;

  precision = MAX (needed_precision, TYPE_PRECISION (op_type));

  /* Unsigned will do for everything but subtraction.  */
  if (op_code == MINUS_EXPR)
    unsignedp = 0;

  /* If our type is the wrong signedness or isn't wide enough, make a new
     type and convert both our operands to it.  */
  if (TYPE_PRECISION (op_type) < precision
      || TREE_UNSIGNED (op_type) != unsignedp)
    {
      /* Copy the node so we ensure it can be modified to make it modular.  */
      op_type = copy_node (type_for_size (precision, unsignedp));
      modulus = convert (op_type, modulus);
      TYPE_MODULUS (op_type) = modulus;
      TYPE_MODULAR_P (op_type) = 1;
      lhs = convert (op_type, lhs);
      rhs = convert (op_type, rhs);
    }

  /* Do the operation, then we'll fix it up.  */
  result = fold (build (op_code, op_type, lhs, rhs));

  /* For multiplication, we have no choice but to do a full modulus
     operation.  However, we want to do this in the narrowest
     possible size.  */
  if (op_code == MULT_EXPR)
    {
      tree div_type = copy_node (type_for_size (needed_precision, 1));
      modulus = convert (div_type, modulus);
      TYPE_MODULUS (div_type) = modulus;
      TYPE_MODULAR_P (div_type) = 1;
      result = convert (op_type,
			fold (build (TRUNC_MOD_EXPR, div_type,
				     convert (div_type, result), modulus)));
    }

  /* For subtraction, add the modulus back if we are negative.  */
  else if (op_code == MINUS_EXPR)
    {
      result = save_expr (result);
      result = fold (build (COND_EXPR, op_type,
			    build (LT_EXPR, integer_type_node, result,
				   convert (op_type, integer_zero_node)),
			    fold (build (PLUS_EXPR, op_type,
					 result, modulus)),
			    result));
    }

  /* For the other operations, subtract the modulus if we are >= it.  */
  else
    {
      result = save_expr (result);
      result = fold (build (COND_EXPR, op_type,
			    build (GE_EXPR, integer_type_node,
				   result, modulus),
			    fold (build (MINUS_EXPR, op_type,
					 result, modulus)),
			    result));
    }

  return convert (type, result);
}

/* Make a binary operation of kind OP_CODE.  RESULT_TYPE is the type
   desired for the result.  Usually the operation is to be performed
   in that type.  For MODIFY_EXPR and ARRAY_REF, RESULT_TYPE may be 0
   in which case the type to be used will be derived from the operands.

   This function is very much unlike the ones for C and C++ since we
   have already done any type conversion and matching required.  All we
   have to do here is validate the work done by SEM and handle subtypes.  */

tree
build_binary_op (op_code, result_type, left_operand, right_operand)
     enum tree_code op_code;
     tree result_type;
     tree left_operand;
     tree right_operand;
{
  tree left_type  = TREE_TYPE (left_operand);
  tree right_type = TREE_TYPE (right_operand);
  tree left_base_type = get_base_type (left_type);
  tree right_base_type = get_base_type (right_type);
  tree operation_type = result_type;
  tree best_type;
  tree modulus;
  tree result;
  int has_side_effects = 0;

  if (operation_type != 0
      && TREE_CODE (operation_type) == RECORD_TYPE
      && TYPE_LEFT_JUSTIFIED_MODULAR_P (operation_type))
    operation_type = TREE_TYPE (TYPE_FIELDS (operation_type));

  if (operation_type != 0
      && ! AGGREGATE_TYPE_P (operation_type)
      && TYPE_EXTRA_SUBTYPE_P (operation_type))
    operation_type = get_base_type (operation_type);

  modulus = (operation_type != 0 && TYPE_MODULAR_P (operation_type)
	     ? TYPE_MODULUS (operation_type) : 0);

  switch (op_code)
    {
    case MODIFY_EXPR:
      /* If there were any integral or pointer conversions on LHS, remove
	 them; we'll be putting them back below if needed.  Likewise for
	 conversions between array and record types.  But don't do this if
	 the right operand is not BLKmode (for packed arrays)
	 unless we are not changing the mode.  */
      while ((TREE_CODE (left_operand) == CONVERT_EXPR
	      || TREE_CODE (left_operand) == NOP_EXPR
	      || TREE_CODE (left_operand) == UNCHECKED_CONVERT_EXPR)
	     && (((INTEGRAL_TYPE_P (left_type)
		   || POINTER_TYPE_P (left_type))
		  && (INTEGRAL_TYPE_P (TREE_TYPE
				       (TREE_OPERAND (left_operand, 0)))
		      || POINTER_TYPE_P (TREE_TYPE
					 (TREE_OPERAND (left_operand, 0)))))
		 || (((TREE_CODE (left_type) == RECORD_TYPE
		       /* Don't remove conversions to left-justified modular
			  types. */
		       && ! TYPE_LEFT_JUSTIFIED_MODULAR_P (left_type))
		      || TREE_CODE (left_type) == ARRAY_TYPE)
		     && ((TREE_CODE (TREE_TYPE
				     (TREE_OPERAND (left_operand, 0)))
			  == RECORD_TYPE)
			 || (TREE_CODE (TREE_TYPE
					(TREE_OPERAND (left_operand, 0)))
			     == ARRAY_TYPE))
		     && (TYPE_MODE (right_type) == BLKmode
			 || (TYPE_MODE (left_type)
			     == TYPE_MODE (TREE_TYPE
					   (TREE_OPERAND
					    (left_operand, 0))))))))
	{
	  left_operand = TREE_OPERAND (left_operand, 0);
	  left_type = TREE_TYPE (left_operand);
	}

      if (operation_type == 0)
	operation_type = left_type;

      /* If the RHS has a conversion between record and array types and
	 an inner type is no worse, use it.  Note we cannot do this for
	 modular types or types with TYPE_ALIGN_OK_P, since the latter
	 might indicate a conversion between a root type and a class-wide
	 type, which we must not remove.  */
      while (TREE_CODE (right_operand) == UNCHECKED_CONVERT_EXPR
	     && ((TREE_CODE (right_type) == RECORD_TYPE
		  && ! TYPE_LEFT_JUSTIFIED_MODULAR_P (right_type)
		  && ! TYPE_ALIGN_OK_P (right_type))
		 || TREE_CODE (right_type) == ARRAY_TYPE)
	     && ((TREE_CODE (TREE_TYPE (TREE_OPERAND (right_operand, 0)))
		  == RECORD_TYPE)
		 || (TREE_CODE (TREE_TYPE (TREE_OPERAND (right_operand, 0)))
		     == ARRAY_TYPE))
	     && (right_type
		 != find_common_type (right_type,
				      TREE_TYPE (TREE_OPERAND
						 (right_operand, 0)))))
	{
	  right_operand = TREE_OPERAND (right_operand, 0);
	  right_type = TREE_TYPE (right_operand);
	}

      /* If we are copying one array or record to another, find the best type
	 to use.  */
      if (((TREE_CODE (left_type) == ARRAY_TYPE
	    && TREE_CODE (right_type) == ARRAY_TYPE)
	   || (TREE_CODE (left_type) == RECORD_TYPE
	       && TREE_CODE (right_type) == RECORD_TYPE))
	  && (best_type = find_common_type (left_type, right_type)) != 0)
	operation_type = best_type;

      /* If a class-wide type may be involved, force use of the RHS type.  */
      if (TREE_CODE (right_type) == RECORD_TYPE
	  && TYPE_ALIGN_OK_P (right_type))
	operation_type = right_type;

      /* After we strip off any COMPONENT_REF or ARRAY_REF from the lhs, we
	 must have either an INDIRECT_REF or a decl. Allow
	 UNCHECKED_CONVERT_EXPRs, but set TREE_ADDRESSABLE to show they are
	 in an LHS.  Finally, allow NOP_EXPR if both types are the same tree
	 code and mode because we know these will be nops.  */
      for (result = left_operand;
	   TREE_CODE (result) == COMPONENT_REF
	   || TREE_CODE (result) == ARRAY_REF
	   || TREE_CODE (result) == REALPART_EXPR
	   || TREE_CODE (result) == IMAGPART_EXPR
	   || TREE_CODE (result) == UNCHECKED_CONVERT_EXPR
	   || (TREE_CODE (result) == NOP_EXPR
	       && (TREE_CODE (TREE_TYPE (result))
		   == TREE_CODE (TREE_TYPE (TREE_OPERAND (result, 0))))
	       && (TYPE_MODE (TREE_TYPE (TREE_OPERAND (result, 0)))
		   == TYPE_MODE (TREE_TYPE (result))));
	   result = TREE_OPERAND (result, 0))
	if (TREE_CODE (result) == UNCHECKED_CONVERT_EXPR)
	  TREE_ADDRESSABLE (result) = 1;

      if (TREE_CODE (result) != INDIRECT_REF
	  && TREE_CODE (result) != NULL_EXPR
	  && TREE_CODE_CLASS (TREE_CODE (result)) != 'd')
	gigi_abort (516);

      /* Convert the right operand to the operation type unless
	 it is either already of the correct type or if the type
	 involves a placeholder, since the RHS may not have the same
	 record type.  */
      if (operation_type != right_type
	  && (! (TREE_CODE (TYPE_SIZE (operation_type)) != INTEGER_CST
		 && contains_placeholder_p (TYPE_SIZE (operation_type)))))
	{
	  right_operand = convert (operation_type, right_operand);
	  right_type = operation_type;
	}

      /* If the modes differ, make up a bogus type and convert the RHS to
	 it.  This can happen with packed types.  */
      if (TYPE_MODE (left_type) != TYPE_MODE (right_type))
	{
	  tree new_type = copy_node (left_type);

	  TYPE_SIZE (new_type) = TYPE_SIZE (right_type);
	  TYPE_MAIN_VARIANT (new_type) = new_type;
	  right_operand = convert (new_type, right_operand);
	}

      has_side_effects = 1;
      modulus = 0;
      break;

    case ARRAY_REF:
      if (operation_type == 0)
	operation_type = TREE_TYPE (left_type);

      /* First convert the right operand to its base type.  This will
	 prevent unneed signedness conversions when sizetype is wider than
	 integer.  */
      right_operand = convert (right_base_type, right_operand);
      right_operand = convert (TYPE_DOMAIN (left_type), right_operand);

      if (! TREE_CONSTANT (right_operand)
	  || ! TREE_CONSTANT (TYPE_MIN_VALUE (right_type)))
	mark_addressable (left_operand);

      modulus = 0;
      break;

    case GE_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case LT_EXPR:
      if (TREE_CODE (left_type) == POINTER_TYPE)
	gigi_abort (501);

      /* ... fall through ... */

    case EQ_EXPR:
    case NE_EXPR:
      /* If either operand is a NULL_EXPR, just return a new one.  */
      if (TREE_CODE (left_operand) == NULL_EXPR
	  || TREE_CODE (right_operand) == NULL_EXPR)
	return build (op_code, result_type,
		      build1 (NULL_EXPR, integer_type_node,
			      TREE_OPERAND (left_operand, 0)),
		      integer_zero_node);

      /* If either object is a left-justified modular types, get the
	 fields from within.  */
      if (TREE_CODE (left_type) == RECORD_TYPE
	  && TYPE_LEFT_JUSTIFIED_MODULAR_P (left_type))
	{
	  left_operand = convert (TREE_TYPE (TYPE_FIELDS (left_type)),
				  left_operand);
	  left_type = TREE_TYPE (left_operand);
	  left_base_type = get_base_type (left_type);
	}

      if (TREE_CODE (right_type) == RECORD_TYPE
	  && TYPE_LEFT_JUSTIFIED_MODULAR_P (right_type))
	{
	  right_operand = convert (TREE_TYPE (TYPE_FIELDS (right_type)),
				  right_operand);
	  right_type = TREE_TYPE (right_operand);
	  right_base_type = get_base_type (right_type);
	}

      /* If both objects are arrays, compare them specially.  */
      if ((TREE_CODE (left_type) == ARRAY_TYPE
	   || (TREE_CODE (left_type) == INTEGER_TYPE
	       && TYPE_HAS_ACTUAL_BOUNDS_P (left_type)))
	  && (TREE_CODE (right_type) == ARRAY_TYPE
	      || (TREE_CODE (right_type) == INTEGER_TYPE
		  && TYPE_HAS_ACTUAL_BOUNDS_P (right_type))))
	{
	  result = compare_arrays (result_type, left_operand, right_operand);

	  if (op_code == EQ_EXPR)
	    ;
	  else if (op_code == NE_EXPR)
	    result = invert_truthvalue (result);
	  else
	    gigi_abort (502);

	  return result;
	}

      /* Otherwise, the base types must be the same unless the objects are
	 records.  If we have records, use the best type and convert both
	 operands to that type.  */
      if (left_base_type != right_base_type)
	{
	  if (TREE_CODE (left_base_type) == RECORD_TYPE
	      && TREE_CODE (right_base_type) == RECORD_TYPE)
	    {
	      /* The only way these are permitted to be the same is if both
		 types have the same name.  In that case, one of them must
		 not be self-referential.  Use that one as the best type.
		 Even better is if one is of fixed size.  */
	      best_type = 0;

	      if (TYPE_NAME (left_base_type) == 0
		  || TYPE_NAME (left_base_type) != TYPE_NAME (right_base_type))
		gigi_abort (503);

	      if (TREE_CONSTANT (TYPE_SIZE (left_base_type)))
		best_type = left_base_type;
	      else if (TREE_CONSTANT (TYPE_SIZE (right_base_type)))
		best_type = right_base_type;
	      else if (! contains_placeholder_p (TYPE_SIZE (left_base_type)))
		best_type = left_base_type;
	      else if (! contains_placeholder_p (TYPE_SIZE (right_base_type)))
		best_type = right_base_type;
	      else
		gigi_abort (504);

	      left_operand = convert (best_type, left_operand);
	      right_operand = convert (best_type, right_operand);
	    }
	  else
	    gigi_abort (505);
	}

      /* If we are comparing a fat pointer against zero, we need to 
	 just compare the data pointer.  */
      else if (TYPE_FAT_POINTER_P (left_base_type)
	       && TREE_CODE (right_operand) == CONSTRUCTOR
	       && integer_zerop (TREE_VALUE (TREE_OPERAND (right_operand, 1))))
	{
	  right_operand = build_component_ref (left_operand, NULL_TREE,
					       TYPE_FIELDS (left_base_type));
	  left_operand = convert (TREE_TYPE (right_operand),
				  integer_zero_node);
	}
      else
	{
	  left_operand = convert (left_base_type, left_operand);
	  right_operand = convert (right_base_type, right_operand);
	}

      modulus = 0;
      break;

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      /* In these, the result type and the left operand type should be the
	 same.  Do the operation in the base type of those and convert the
	 right operand (which is an integer) to that type.

	 Note that these operations are only used in loop control where
	 we guarantee that no overflow can occur.  So nothing special need
	 be done for modular types.  */

      if (left_type != result_type)
	gigi_abort (506);

      operation_type = get_base_type (result_type);
      left_operand = convert (operation_type, left_operand);
      right_operand = convert (operation_type, right_operand);
      has_side_effects = 1;
      modulus = 0;
      break;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* The RHS of a shift can be any type.  Also, ignore any modulus
	 (we used to abort, but this is needed for unchecked conversion
	 to modular types).  Otherwise, processing is the same as normal.  */
      if (operation_type != left_base_type)
	gigi_abort (514);

      modulus = 0;
      left_operand = convert (operation_type, left_operand);
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      left_operand = truthvalue_conversion (left_operand);
      right_operand = truthvalue_conversion (right_operand);
      goto common;

    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      /* For binary modulus, if the inputs are in range, so are the
	 outputs.  */
      if (modulus != 0 && integer_pow2p (modulus))
	modulus = 0;

      goto common;

    case COMPLEX_EXPR:
      if (TREE_TYPE (result_type) != left_type
	  || TREE_TYPE (result_type) != right_type)
	gigi_abort (515);
      break;

    case TRUNC_DIV_EXPR:   case TRUNC_MOD_EXPR:
    case CEIL_DIV_EXPR:    case CEIL_MOD_EXPR:
    case FLOOR_DIV_EXPR:   case FLOOR_MOD_EXPR:
    case ROUND_DIV_EXPR:   case ROUND_MOD_EXPR:
      /* These always produce results lower than either operand.  */
      modulus = 0;
      goto common;

    default:
    common:
      /* The result type should be the same as the base types of the
	 both operands (and they should be the same).  Convert
	 everything to the result type.  */

      if (operation_type != left_base_type
	  || left_base_type != right_base_type)
	gigi_abort (507);

      left_operand = convert (operation_type, left_operand);
      right_operand = convert (operation_type, right_operand);
    }

  if (modulus != 0 && ! integer_pow2p (modulus))
    {
      result = nonbinary_modular_operation (op_code, operation_type,
					    left_operand, right_operand);
      modulus = 0;
    }
  else
    result = fold (build (op_code, operation_type,
			  left_operand, right_operand));

  TREE_SIDE_EFFECTS (result) |= has_side_effects;
  TREE_CONSTANT (result)
    = (TREE_CONSTANT (left_operand) & TREE_CONSTANT (right_operand)
       && op_code != ARRAY_REF);

  /* If we are working with modular types, perform the MOD operation
     if something above hasn't eliminated the need for it.  */
  if (modulus != 0)
    result = fold (build (FLOOR_MOD_EXPR, operation_type, result,
			  convert (operation_type, modulus)));

  if (result_type != 0 && result_type != operation_type)
    result = convert (result_type, result);

  return result;
}

/* Similar, but for unary operations.  */

tree
build_unary_op (op_code, result_type, operand)
     enum tree_code op_code;
     tree result_type;
     tree operand;
{
  tree type = TREE_TYPE (operand);
  tree base_type = get_base_type (type);
  tree operation_type = result_type;
  tree result;
  int side_effects = 0;

  if (operation_type != 0
      && TREE_CODE (operation_type) == RECORD_TYPE
      && TYPE_LEFT_JUSTIFIED_MODULAR_P (operation_type))
    operation_type = TREE_TYPE (TYPE_FIELDS (operation_type));

  if (operation_type != 0
      && ! AGGREGATE_TYPE_P (operation_type)
      && TYPE_EXTRA_SUBTYPE_P (operation_type))
    operation_type = get_base_type (operation_type);

  switch (op_code)
    {
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      if (operation_type == 0)
	result_type = operation_type = TREE_TYPE (type);
      else if (result_type != TREE_TYPE (type))
	gigi_abort (513);

      result = fold (build1 (op_code, operation_type, operand));
      break;

    case TRUTH_NOT_EXPR:
      if (result_type != base_type)
	gigi_abort (508);

      result = invert_truthvalue (truthvalue_conversion (operand));
      break;

    case ADDR_EXPR:
      switch (TREE_CODE (operand))
	{
	case INDIRECT_REF:
	case UNCONSTRAINED_ARRAY_REF:
	  result = TREE_OPERAND (operand, 0);
	  break;

	case NULL_EXPR:
	  result = operand;
	  TREE_TYPE (result) = type = build_pointer_type (type);
	  break;

	case TRANSFORM_EXPR:
	  TREE_TRANSFORM_ADDR (operand) = 1;
	  result = operand;

	  if (type != error_mark_node)
	    {
	      type = build_pointer_type (type);
	      TREE_TYPE (result) = type;
	    }
	  break;

	case ARRAY_REF:
	  {
	    /* Compute address of array element by adding address of array
	       to index times size of element.  */
	    tree array = TREE_OPERAND (operand, 0);
	    tree array_adr = build1 (ADDR_EXPR,
				     build_pointer_type (type), array);
	    tree low_bound = TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (array)));
	    tree index = TREE_OPERAND (operand, 1);
	    tree size = size_in_bytes (type);

	    mark_addressable (array);

	    if (TREE_CODE (low_bound) != INTEGER_CST
		&& contains_placeholder_p (low_bound))
	      low_bound = build (WITH_RECORD_EXPR, sizetype,
				 low_bound, operand);

	    /* Convert the integer argument to a type the same size as a
	       pointer so the arithmetic won't overflow spuriously.  */
	    if (TYPE_PRECISION (TREE_TYPE (index)) != POINTER_SIZE)
	      index = convert (type_for_size (POINTER_SIZE, 0), index);

	    index = fold (build (MINUS_EXPR, TREE_TYPE (index), index,
				 convert (TREE_TYPE (index), low_bound)));

	    index = fold (build (MULT_EXPR, TREE_TYPE (index),
				 index, convert (TREE_TYPE (index), size)));

	    result = fold (build (PLUS_EXPR, TYPE_POINTER_TO (type), array_adr,
				  convert (TYPE_POINTER_TO (type), index)));
	  }
	  break;

	default:

	  if (type != error_mark_node)
	    operation_type = build_pointer_type (type);

	  mark_addressable (operand);
	  result = fold (build1 (op_code, operation_type, operand));
	}

      TREE_CONSTANT (result) = staticp (operand) || TREE_CONSTANT (operand);
      break;

    case INDIRECT_REF:
      /* If we want to refer to an entire unconstrained array,
	 make up an expression to do so.  This will never survive to
	 the backend.  If TYPE is a thin pointer, first convert the
	 operand to a fat pointer.  */
      if (TYPE_THIN_POINTER_P (type)
	  && TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (type)) != 0)
	{
	  operand
	    = convert (TREE_TYPE (TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (type))),
		       operand);
	  type = TREE_TYPE (operand);
	}

      if (TYPE_FAT_POINTER_P (type))
	result = build1 (UNCONSTRAINED_ARRAY_REF,
			 TYPE_UNCONSTRAINED_ARRAY (type), operand);
      else
	{
	  result = fold (build1 (op_code, TREE_TYPE (type), operand));
	  TREE_READONLY (result) = TREE_STATIC (result)
	    = TREE_READONLY (TREE_TYPE (type));
	}

      side_effects = flag_volatile 
	|| (! TYPE_FAT_POINTER_P (type) && TYPE_VOLATILE (TREE_TYPE (type)));
      break;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
      {
	tree modulus = (operation_type && TYPE_MODULAR_P (operation_type)
			? TYPE_MODULUS (operation_type) : 0);
	int mod_pow2 = modulus != 0 && integer_pow2p (modulus);

	/* If this is a modular type, there are various possibilities
	   depending on the operation and whether the modulus is a
	   power of two or not.  */

	if (modulus != 0)
	  {
	    if (operation_type != base_type)
	      gigi_abort (509);

	    operand = convert (operation_type, operand);

	    /* The fastest in the negate case for binary modulus is
	       the straightforward code; the TRUNC_MOD_EXPR below
	       is an AND operation.  */
	    if (op_code == NEGATE_EXPR && mod_pow2)
	      result = fold (build (TRUNC_MOD_EXPR, operation_type,
				    fold (build1 (NEGATE_EXPR, operation_type,
						  operand)),
				    modulus));

	    /* For nonbinary negate case, return zero for zero operand,
	       else return the modulus minus the operand.  If the modulus
	       is a power of two minus one, we can do the subtraction
	       as an XOR since it is equivalent and faster on most machines. */
	    else if (op_code == NEGATE_EXPR && ! mod_pow2)
	      {
		if (integer_pow2p (fold (build (PLUS_EXPR, operation_type,
						modulus,
						convert (operation_type,
							 integer_one_node)))))
		  result = fold (build (BIT_XOR_EXPR, operation_type,
					operand, modulus));
		else
		  result = fold (build (MINUS_EXPR, operation_type,
					modulus, operand));

		result = fold (build (COND_EXPR, operation_type,
				      fold (build (NE_EXPR, integer_type_node,
						   operand,
						   convert (operation_type,
							    integer_zero_node))),
				      result, operand));
	      }
	    else
	      {
		/* For the NOT cases, we need a constant equal to
		   the modulus minus one.  For a binary modulus, we
		   XOR against the constant and subtract the operand from
		   that constant for nonbinary modulus.  */

		tree cnst = fold (build (MINUS_EXPR, operation_type, modulus,
					 convert (operation_type,
						  integer_one_node)));

		if (mod_pow2)
		  result = fold (build (BIT_XOR_EXPR, operation_type,
					operand, cnst));
		else
		  result = fold (build (MINUS_EXPR, operation_type,
					cnst, operand));
	      }

	    break;
	  }
      }

      /* ... fall through ... */

    default:
      if (operation_type != base_type)
	gigi_abort (509);

      result = fold (build1 (op_code, operation_type, convert (operation_type,
							       operand)));
    }

  if (side_effects)
    {
      TREE_SIDE_EFFECTS (result) = 1;
      if (TREE_CODE (result) == INDIRECT_REF)
	TREE_THIS_VOLATILE (result) = TYPE_VOLATILE (TREE_TYPE (result));
    }

  if (result_type != 0 && TREE_TYPE (result) != result_type)
    result = convert (result_type, result);

  return result;
}

/* Similar, but for COND_EXPR.  */

tree
build_cond_expr (result_type, condition_operand, true_operand, false_operand)
     tree result_type;
     tree condition_operand;
     tree true_operand;
     tree false_operand;
{
  /* Front-end verifies that result, true and false operands have same base
     type. Convert everything to the result type.  */

  true_operand  = convert (result_type, true_operand);
  false_operand = convert (result_type, false_operand);

  return fold (build (COND_EXPR, result_type, condition_operand,
		      true_operand, false_operand));
}


/* Build a CALL_EXPR to call FUNDECL with one argument, ARG.  Return
   the CALL_EXPR.  */
tree
build_call_1_expr (fundecl, arg)
     tree fundecl;
     tree arg;
{
  tree call = build (CALL_EXPR, TREE_TYPE (TREE_TYPE (fundecl)),
		     build_unary_op (ADDR_EXPR, NULL_TREE, fundecl),
		     chainon (NULL_TREE, build_tree_list (NULL_TREE, arg)),
		     NULL_TREE);

  TREE_SIDE_EFFECTS (call) = 1;

  return call;
}

/* Likewise to call FUNDECL with no arguments.  */

tree
build_call_0_expr (fundecl)
     tree fundecl;
{
  tree call = build (CALL_EXPR, TREE_TYPE (TREE_TYPE (fundecl)),
		     build_unary_op (ADDR_EXPR, NULL_TREE, fundecl),
		     NULL_TREE, NULL_TREE);

  TREE_SIDE_EFFECTS (call) = 1;

  return call;
}


/* Return a CONSTRUCTOR of TYPE whose list is LIST.  */

tree
build_constructor (type, list)
     tree type;
     tree list;
{
  tree elmt;
  int allconstant = 1;
  int side_effects = 0;
  tree result;

  for (elmt = list; elmt; elmt = TREE_CHAIN (elmt))
    {
      if (! TREE_CONSTANT (TREE_VALUE (elmt))
	  || (TREE_CODE (type) == RECORD_TYPE
	      && DECL_BIT_FIELD (TREE_PURPOSE (elmt))
	      && TREE_CODE (TREE_VALUE (elmt)) != INTEGER_CST))
	allconstant = 0;

      if (TREE_SIDE_EFFECTS (TREE_VALUE (elmt)))
	side_effects = 1;
    }

  /* If TYPE is a RECORD_TYPE and the fields are not in the
     same order as their bit position, don't treat this as constant
     since varasm.c can't handle it.  */
  if (allconstant && TREE_CODE (type) == RECORD_TYPE)
    {
      tree last_pos = size_zero_node;
      tree field;

      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (DECL_FIELD_BITPOS (field)) != INTEGER_CST
	      || tree_int_cst_lt (DECL_FIELD_BITPOS (field), last_pos))
	    {
	      allconstant = 0;
	      break;
	    }

	  last_pos = DECL_FIELD_BITPOS (field);
	}
    }

  result = build (CONSTRUCTOR, type, NULL_TREE, list);
  TREE_CONSTANT (result) = allconstant;
  TREE_STATIC (result) = allconstant;
  TREE_SIDE_EFFECTS (result) = side_effects;
  TREE_READONLY (result) = TREE_READONLY (type);

  return result;
}

/* Return a COMPONENT_REF to access a field that is given by COMPONENT,
   an IDENTIFIER_NODE giving the name of the field, FIELD, a FIELD_DECL,
   for the field, or both.

   We also handle the fact that we might have been passed a pointer to the
   actual record and know how to look for fields in variant parts.  */

tree
build_simple_component_ref (record_variable, component, field)
     tree record_variable;
     tree component;
     tree field;
{
  tree record_type = TYPE_MAIN_VARIANT (TREE_TYPE (record_variable));
  tree ref;

  /* Handle added pointer for pass-by-reference values.  */
  if (TREE_CODE (record_type) == POINTER_TYPE)
    {
      record_variable
	= build_unary_op (INDIRECT_REF, NULL_TREE, record_variable);
      record_type = TREE_TYPE (record_variable);
    }

  if ((TREE_CODE (record_type) != RECORD_TYPE
       && TREE_CODE (record_type) != UNION_TYPE
       && TREE_CODE (record_type) != QUAL_UNION_TYPE)
      || TYPE_SIZE (record_type) == 0)
    gigi_abort (510);

  if (field == 0 || DECL_CONTEXT (field) != record_type)
    /* Check if there is a field with name COMPONENT in the record.  */
    {
      if (component == 0)
	gigi_abort (511);

      /* ??? Explore later if we can use the TYPE_LANG_SPECIFIC optimization
	 that appears in C version of this function.  */

      for (field = TYPE_FIELDS (record_type); field;
	   field = TREE_CHAIN (field))
	{
	  if (DECL_NAME (field) == component)
	    break;
	  else if (DECL_INTERNAL_P (field))
	     {
	      tree field_ref
		= build_simple_component_ref (record_variable, 
					      NULL_TREE, field);
	      ref = build_simple_component_ref (field_ref, 
						component, NULL_TREE);

	      if (ref != 0)
		return ref;
	    }
	}
    }

  if (!field)
    return 0;

  /* It would be nice to call "fold" here, but that can lose a type
     we need to tag a PLACEHOLDER_EXPR with, so we can't do it.  */
  ref = build (COMPONENT_REF, TREE_TYPE (field), record_variable, field);

  if (TREE_READONLY (record_variable) || TREE_READONLY (field))
    TREE_READONLY (ref) = 1;
  if (TREE_THIS_VOLATILE (record_variable) || TREE_THIS_VOLATILE (field))
    TREE_THIS_VOLATILE (ref) = 1;

  return ref;
}

/* Like build_simple_component_ref, except that we give an error if the
   reference could not be found.  */

tree
build_component_ref (record_variable, component, field)
     tree record_variable;
     tree component;
     tree field;
{
  tree ref = build_simple_component_ref (record_variable, component, field);

  if (ref != 0)
    return ref;

  /* If FIELD was specified, assume this is an invalid user field so
     raise constraint error.  Otherwise, we can't find the type to return, so
     abort.  */

  else if (field != 0)
    return build1 (NULL_EXPR, TREE_TYPE (field),
		   build_call_0_expr (raise_constraint_error_decl));

  else
    gigi_abort (512);
}

/* Build a GCC tree to call an allocation or deallocation function.
   If GNU_OBJ is nonzero, it is an object to deallocate.  Otherwise,
   generate an allocator.

   GNU_SIZE is the size of the object and ALIGN is the alignment.
   GNAT_PROC, if present is a procedure to call and GNAT_POOL is the
   storage pool to use.  If not preset, malloc and free will be used.  */

tree
build_call_alloc_dealloc (gnu_obj, gnu_size, align, gnat_proc, gnat_pool)
     tree gnu_obj;
     tree gnu_size;
     int align;
     Entity_Id gnat_proc;
     Entity_Id gnat_pool;
{
  tree gnu_align = size_int (align / BITS_PER_UNIT);

  gnu_size = size_binop (CEIL_DIV_EXPR, gnu_size, size_int (BITS_PER_UNIT));

  if (TREE_CODE (gnu_size) != INTEGER_CST && contains_placeholder_p (gnu_size))
    gnu_size = build (WITH_RECORD_EXPR, sizetype, gnu_size,
		      build_unary_op (INDIRECT_REF, NULL_TREE, gnu_obj));

  if (Present (gnat_proc))

    /* The storage pools are obviously always tagged types, but the 
       secondary stack uses the same mechanism and is not tagged */

    if (Is_Tagged_Type (Etype (gnat_pool)))
      {
	/* The size is the third parameter; the alignment is the same type.  */
	Entity_Id gnat_size_type
	  = Etype (Next_Formal (Next_Formal (First_Formal (gnat_proc))));
	tree gnu_size_type = gnat_to_gnu_type (gnat_size_type);
	tree gnu_proc = gnat_to_gnu_entity (gnat_proc, NULL_TREE, 0);
	tree gnu_proc_addr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_proc);
	tree gnu_pool = gnat_to_gnu_entity (gnat_pool, NULL_TREE, 0);
	tree gnu_pool_addr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_pool);
	tree gnu_args = NULL_TREE;
	tree gnu_call;

	/* The first arg is always the address of the storage pool; next
	   comes the address of the object, for a deallocator, then the
	   size and alignment.  */

	gnu_args
	  = chainon (gnu_args, build_tree_list (NULL_TREE, gnu_pool_addr));

	if (gnu_obj)
	  gnu_args = chainon (gnu_args, build_tree_list (NULL_TREE, gnu_obj));

	gnu_args = 
	  chainon (gnu_args,
		   build_tree_list (NULL_TREE,
				    convert (gnu_size_type, gnu_size)));
	gnu_args = 
	  chainon (gnu_args,
		   build_tree_list (NULL_TREE, 
				    convert (gnu_size_type, gnu_align)));

	gnu_call = build (CALL_EXPR, TREE_TYPE (TREE_TYPE (gnu_proc)),
			  gnu_proc_addr, gnu_args, NULL_TREE);
	TREE_SIDE_EFFECTS (gnu_call) = 1;
	return gnu_call;
      }

    /* Secondary stack case.  */

    else
      {
	/* The size is the second parameter */
	Entity_Id gnat_size_type 
	  = Etype (Next_Formal (First_Formal (gnat_proc)));
	tree gnu_size_type = gnat_to_gnu_type (gnat_size_type);
	tree gnu_proc = gnat_to_gnu_entity (gnat_proc, NULL_TREE, 0);
	tree gnu_proc_addr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_proc);
	tree gnu_pool = gnat_to_gnu_entity (gnat_pool, NULL_TREE, 0);
	tree gnu_pool_addr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_pool);
	tree gnu_args = NULL_TREE;
	tree gnu_call;

	/* The first arg is the address of the object, for a
	   deallocator, then the size */

	if (gnu_obj)
	  gnu_args = chainon (gnu_args, build_tree_list (NULL_TREE, gnu_obj));

	gnu_args = 
	  chainon (gnu_args,
		   build_tree_list (NULL_TREE,
				    convert (gnu_size_type, gnu_size)));

	gnu_call = build (CALL_EXPR, TREE_TYPE (TREE_TYPE (gnu_proc)),
			  gnu_proc_addr, gnu_args, NULL_TREE);
	TREE_SIDE_EFFECTS (gnu_call) = 1;
	return gnu_call;
      }

  else if (gnu_obj)
    return build_call_1_expr (free_decl, gnu_obj);
  else
    return build_call_1_expr (malloc_decl, gnu_size);
}

/* Build a GCC tree to correspond to allocating an object of TYPE whose
   initial value is INIT, if INIT is nonzero.  Convert the expression to
   RESULT_TYPE, which must be some type of pointer.  Return the tree.
   GNAT_PROC and GNAT_POOL optionally give the procedure to call and
   the storage pool to use.  */

tree
build_allocator (type, init, result_type, gnat_proc, gnat_pool)
     tree type;
     tree init;
     tree result_type;
     Entity_Id gnat_proc;
     Entity_Id gnat_pool;
{
  /* Counts number of allocators we were able to do by statically allocating
     memory when at top level.  */
  static int alloc_var_index = 0;
  tree size = TYPE_SIZE (type);
  tree ptr_type;
  tree result;

  /* If RESULT_TYPE is a fat or thin pointer, set SIZE to be the sum of the
     sizes of the object and its template.  Allocate the whole thing and
     fill in the parts that are known.  */
  if (TYPE_FAT_OR_THIN_POINTER_P (result_type))
    {
      tree template_type
	= (TYPE_FAT_POINTER_P (result_type)
	   ? TREE_TYPE (TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (result_type))))
	   : TREE_TYPE (TYPE_FIELDS (TREE_TYPE (result_type))));
      tree storage_type
	= build_unc_object_type (template_type, type,
				 get_identifier ("ALLOC"));
      tree storage_ptr_type = build_pointer_type (storage_type);
      tree storage;
      tree template_cons = NULL_TREE;

      size = TYPE_SIZE (storage_type);

      if (TREE_CODE (size) != INTEGER_CST
	  && contains_placeholder_p (size))
	size = build (WITH_RECORD_EXPR, sizetype, size, init);

      storage = build_call_alloc_dealloc (NULL_TREE, size,
					  TYPE_ALIGN (storage_type),
					  gnat_proc, gnat_pool);
      storage = convert (storage_ptr_type, storage);

      if (TREE_CODE (type) == RECORD_TYPE && TYPE_IS_PADDING_P (type))
	{
	  type = TREE_TYPE (TYPE_FIELDS (type));

	  if (init != 0)
	    init = convert (type, init);
	}

      /* If there is an initializing expression, make a constructor for
	 the entire object including the bounds and copy it into the
	 object.  If there is no initializing expression, just set the
	 bounds.  */
      if (init != 0)
	{
	  template_cons = tree_cons (TREE_CHAIN (TYPE_FIELDS (storage_type)),
				     init, NULL_TREE);
	  template_cons = tree_cons (TYPE_FIELDS (storage_type),
				     build_template (template_type, type,
						     init),
				     template_cons);

	  return convert
	    (result_type,
	     build_unary_op
	     (ADDR_EXPR, NULL_TREE,
	      build_binary_op
	      (MODIFY_EXPR, storage_type,
	       build_unary_op (INDIRECT_REF, NULL_TREE,
			       convert (storage_ptr_type, storage)),
	       build_constructor (storage_type, template_cons))));
	}
      else
	return build
	  (COMPOUND_EXPR, result_type,
	   build_binary_op
	   (MODIFY_EXPR, template_type,
	    build_component_ref
	    (build_unary_op (INDIRECT_REF, NULL_TREE,
			     convert (storage_ptr_type, storage)),
	     NULL_TREE, TYPE_FIELDS (storage_type)),
	    build_template (template_type, type, NULL_TREE)),
	   convert (result_type, convert (storage_ptr_type, storage)));
    }

  /* If we have an initializing expression, see if its size is simpler
     than the size from the type.  */
  if (init != 0 && TYPE_SIZE (TREE_TYPE (init)) != 0
      && (TREE_CODE (TYPE_SIZE (TREE_TYPE (init))) == INTEGER_CST
	  || (TREE_CODE (size) != INTEGER_CST
	      && contains_placeholder_p (size))))
    size = TYPE_SIZE (TREE_TYPE (init));

  /* If the size is still self-referential, reference the initializing
     expression, if it is present.  If not, this must have been a
     call to allocate a library-level object, in which case we use
     the maximum size.  */
  if (TREE_CODE (size) != INTEGER_CST && contains_placeholder_p (size))
    {
      if (init == 0)
	size = max_size (size, 1);
      else
	size = build (WITH_RECORD_EXPR, sizetype, size, init);
    }

  /* If we are at top-level and SIZE is a constant, we can actually
     allocate an object of TYPE and point to it.
     ??? At some point, we should make an attempt to do this above.  */
  if (global_bindings_p () && TREE_CODE (size) == INTEGER_CST
      && ! TREE_OVERFLOW (size) && TREE_INT_CST_HIGH (size) == 0
      && TREE_INT_CST_LOW (size) < 200000
      && 0)
    {
      char name[20];

      type = maybe_pad_type (type, size, 0, Empty, NULL_PTR, 0);
      sprintf (name, "__V%d", alloc_var_index++);
      result = create_var_decl (get_identifier (name), NULL_TREE, type, init,
				0, 0, 0, 1, NULL_PTR);
      result = build_unary_op (ADDR_EXPR, NULL_TREE, result);
      init = 0;
    }
  else
    {
      /* If the size overflows, pass -1 so the allocator will raise
	 storage error.  */
      if (TREE_CODE (size) == INTEGER_CST && TREE_OVERFLOW (size))
	size = convert (sizetype, build_int_2 (-BITS_PER_UNIT, ~0));

      result = convert (result_type,
			build_call_alloc_dealloc (NULL_TREE, size,
						  TYPE_ALIGN (type),
						  gnat_proc, gnat_pool));
    }

  /* If we have an initial value, put the new address into a SAVE_EXPR, assign
     the value, and return the address.  Do this with a COMPOUND_EXPR.  */

  if (init)
    {
      result = save_expr (result);
      result
	= build (COMPOUND_EXPR, TREE_TYPE (result),
		 build_binary_op
		 (MODIFY_EXPR, TREE_TYPE (TREE_TYPE (result)),
		  build_unary_op (INDIRECT_REF, TREE_TYPE (TREE_TYPE (result)),
				  result),
		  init),
		 result);
    }

  return convert (result_type, result);
}

/* Fill in a VMS descriptor for EXPR and return a constructor for it. 
   GNAT_FORMAL is how we find the descriptor record.  */

tree
fill_vms_descriptor (expr, gnat_formal)
     tree expr;
     Entity_Id gnat_formal;
{
  tree record_type = TREE_TYPE (TREE_TYPE (get_gnu_tree (gnat_formal)));
  tree field;
  tree const_list = 0;

  expr = maybe_unconstrained_array (expr);

  for (field = TYPE_FIELDS (record_type); field; field = TREE_CHAIN (field))
    {
      tree init = DECL_INITIAL (field);

      if (TREE_CODE (init) != INTEGER_CST
	  && contains_placeholder_p (init))
	init = build (WITH_RECORD_EXPR, TREE_TYPE (init), init, expr);

      const_list = tree_cons (field, convert (TREE_TYPE (field), init),
			      const_list);
    }

  return build_constructor (record_type, nreverse (const_list));
}

/* Indicate that we need to make the address of EXPR_NODE and it therefore
   should not be allocated in a register. Return 1 if successful.  */

int
mark_addressable (expr_node)
     tree expr_node;
{
  while (1)
    switch (TREE_CODE (expr_node))
      {
      case ADDR_EXPR:
      case COMPONENT_REF:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	expr_node = TREE_OPERAND (expr_node, 0);
	break;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (expr_node) = 1;
	return 1;

      case VAR_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	put_var_into_stack (expr_node);
	TREE_ADDRESSABLE (expr_node) = 1;
	return 1;

      case FUNCTION_DECL:
	TREE_ADDRESSABLE (expr_node) = 1;
	return 1;

      default:
	return 1;
    }
}
