/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                              A - T R A N S                               */
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*                            $Revision: 1.439 $                            */
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
#include "real.h"
#include "flags.h"
#include "a-ada.h"
#include "a-types.h"
#include "a-atree.h"
#include "a-nlists.h"
#include "a-elists.h"
#include "a-uintp.h"
#include "a-sinfo.h"
#include "a-einfo.h"
#include "a-namet.h"
#include "a-snames.h"
#include "a-string.h"
#include "a-urealp.h"
#include "a-trans.h"
#include "a-trans3.h"
#include "a-gtran3.h"
#include "a-trans4.h"
#include "a-misc.h"
#include "a-rtree.h"

int max_gnat_nodes;
int number_names;
struct Node *Nodes_Ptr;
Node_Id *Next_Node_Ptr;
Node_Id *Prev_Node_Ptr;
struct Elist_Header *Elists_Ptr;
struct Elmt_Item *Elmts_Ptr;
/* struct Name_Entry *Names_Ptr; */
struct String_Entry * Strings_Ptr;
Char_Code * String_Chars_Ptr;
struct List_Header *List_Headers_Ptr;
/* char * Name_Chars_Ptr; */

/* First_Actual and Next_Actual functions from Sem_Util */
#define First_Actual sem_util__first_actual
Node_Id First_Actual (Node_Id Node);
#define Next_Actual sem_util__next_actual
Node_Id Next_Actual (Node_Id Actual_Id);
#define Range sinfo__discrete_range
#define Is_Rewrite_Substitution atree__is_rewrite_substitution
#define Original_Node atree__original_node

/* List of TREE_LIST nodes representing a block stack.  TREE_VALUE
   of each gives the variable used for the setjmp buffer in the current
   block, if any.  */

tree gnu_block_stack;

/* Map GNAT tree codes to GCC tree codes for simple expressions.  */

static enum tree_code gnu_codes[Number_Node_Kinds];

/* Current node being treated, in case gigi_abort called */
static Node_Id error_gnat_node;

/* Variable that stores the address of the raised exception.  
   Nonzero means we are in an exception handler.  */
static tree gnu_except_ptr_decl;

static tree tree_transform		PROTO((Node_Id));
static void process_freeze_entity	PROTO((Node_Id));
static void process_inlined_subprograms	PROTO((Node_Id));
static void process_decls		PROTO((List_Id, List_Id, Node_Id,
					       int, int));
static tree emit_access_check		PROTO((tree));
static tree emit_discriminant_check	PROTO((tree, Node_Id));
static tree emit_range_check		PROTO((tree, Node_Id));
static tree emit_index_check		PROTO((tree, tree, tree, tree));
static tree emit_check			PROTO((tree, tree));
static tree convert_with_check		PROTO((Entity_Id, tree,
					       int, int, int));
static tree assoc_to_constructor	PROTO((Node_Id, tree));
static tree extract_values		PROTO((tree, tree));
static tree pos_to_constructor		PROTO((Node_Id, tree, Entity_Id));
static tree maybe_implicit_deref	PROTO((tree));
static tree gnat_stabilize_reference_1	PROTO((tree, int));
static int build_package_elab		PROTO((Entity_Id, int, tree, Node_Id));
static int build_subprogram_elab	PROTO((Entity_Id, int));

/* Constants for +0.5 and -0.5 for float-to-integer rounding.  */
static REAL_VALUE_TYPE dconstp5;
static REAL_VALUE_TYPE dconstmp5;

/* This is the main program of the back-end.  It sets up all the table
   structures and then generates code.  */

void
gigi (gnat_root, max_gnat_node, number_name,
      nodes_ptr, next_node_ptr, prev_node_ptr,
      elists_ptr, elmts_ptr,
      names_ptr,       /* no longer used -- tjq */
      strings_ptr, string_chars_ptr, list_headers_ptr,
      name_chars_ptr,  /* no longer used -- tjq */
      number_units, file_info_ptr)
     Node_Id gnat_root;
     int max_gnat_node;
     int number_name;

     struct Node *nodes_ptr;
     Node_Id *next_node_ptr;
     Node_Id *prev_node_ptr;
     struct Elist_Header *elists_ptr;
     struct Elmt_Item *elmts_ptr;
     struct Name_Entry *names_ptr;
     struct String_Entry *strings_ptr;
     Char_Code *string_chars_ptr;
     struct List_Header *list_headers_ptr;
     char *name_chars_ptr;
     Int number_units;
     struct Needed_File_Info *file_info_ptr;
{
  max_gnat_nodes = max_gnat_node;
  number_names =   number_name;
  Nodes_Ptr = nodes_ptr - First_Node_Id;
  Next_Node_Ptr = next_node_ptr - First_Node_Id;
  Prev_Node_Ptr = prev_node_ptr - First_Node_Id;
  Elists_Ptr = elists_ptr - First_Elist_Id;
  Elmts_Ptr = elmts_ptr - First_Elmt_Id;
/* Names_Ptr = names_ptr - First_Name_Id; */
  Strings_Ptr = strings_ptr - First_String_Id;
  String_Chars_Ptr = string_chars_ptr;
  List_Headers_Ptr = list_headers_ptr - First_List_Id;
/*  Name_Chars_Ptr = name_chars_ptr; */

  if (Nkind (gnat_root) != N_Compilation_Unit)
    gigi_abort (301);

  set_lineno (gnat_root, 0);

  /* Initialize ourselves.  */
  init_gnat_to_gnu ();
  init_dummy_type ();
  init_code_table ();

  dconstp5 = REAL_VALUE_ATOF ("0.5", DFmode);
  dconstmp5 = REAL_VALUE_ATOF ("-0.5", DFmode);

  gnat_to_code (gnat_root);
}


/* This function is the driver of the GNAT to GCC tree transformation process.
   GNAT_NODE is the root of some gnat tree.  It generates code for that
   part of the tree.  */

void
gnat_to_code (gnat_node)
     Node_Id gnat_node;
{
  tree gnu_root;

  /* Save node number in case error */
  error_gnat_node = gnat_node;

  gnu_root = tree_transform (gnat_node);

  /* This should just generate code, not return a value.  If it returns
     a value, something is wrong.  */
  if (gnu_root != error_mark_node)
    gigi_abort (302);
}

/* GNAT_NODE is the root of some GNAT tree.  Return the root of the
   GCC tree corresponding to that GNAT tree.  Normally, no code is generated;
   we just return an equivalent tree which is used elsewhere to generate
   code.  */

tree
gnat_to_gnu (gnat_node)
     Node_Id gnat_node;
{
  tree gnu_root;

  /* Save node number in case error */
  error_gnat_node = gnat_node;

  gnu_root = tree_transform (gnat_node);

  /* If we got no code as a result, something is wrong.  */
  if (gnu_root == error_mark_node)
    gigi_abort (303);

  return gnu_root;
}

/* This function is the driver of the GNAT to GCC tree transformation process.
   It is the entry point of the tree transformer.  GNAT_NODE is the root of
   some GNAT tree.  Return the root of the corresponding GCC tree or
   error_mark_node to signal that there is no GCC tree to return.

   The latter is the case if only code generation actions have to be performed
   like in the case of if statements, loops, etc.  This routine is wrapped
   in the above two routines for most purposes.  */

static tree
tree_transform (gnat_node)
     Node_Id gnat_node;
{
  tree gnu_result = error_mark_node; /* Default to no value. */
  tree gnu_result_type = void_type_node;
  tree gnu_expr;
  tree gnu_lhs, gnu_rhs;
  enum tree_code gnu_code;
  Node_Id gnat_temp;
  Entity_Id gnat_temp_type;

  /* Set input_file_name and lineno from the Sloc in the GNAT tree. */
  set_lineno (gnat_node, 0);

  /* If this is a Statement and we are at top level, we add the statement
     as an elaboration for a null tree.  That will cause it to be placed
     in the elaboration procedure.  */
  if (global_bindings_p ()
      && ((IN (Nkind (gnat_node), N_Statement)
	   && Nkind (gnat_node) != N_Null_Statement)
	  || Nkind (gnat_node) == N_Procedure_Call_Statement
	  || (Nkind (gnat_node) == N_Raise_Constraint_Error
	      && (Ekind (Etype (gnat_node)) == E_Void))))
    {
      add_pending_elaborations (NULL_TREE,
				make_transform_expr (gnat_node,
						     error_mark_node));
      return error_mark_node;
    }

 switch (Nkind (gnat_node))
    {
      /********************************/
      /* Chapter 2: Lexical Elements: */
      /********************************/

    case N_Identifier:
    case N_Expanded_Name:
    case N_Operator_Symbol:
      /* If the Etype of this node does not equal the Etype of the
	 Entity, something is wrong with the entity map, probably
         in generic instantiation. However, this does not apply to
         types. Since we sometime have strange Ekind's, just do
         this test for objects. Also, if the Etype of the Entity
         is private, the Etype of the N_Identifier is allowed to be the
         full type and also we consider a packed array type to be the
         same as the original type. Finally, if the types are Itypes,
         one may be a copy of the other, which is also legal. */

      gnat_temp = Entity (gnat_node);
      gnat_temp_type = Etype (gnat_temp);

      if (Etype (gnat_node) != gnat_temp_type
          && ! (Is_Packed (gnat_temp_type)
                && Etype (gnat_node) == Packed_Array_Type (gnat_temp_type))
          && ! (IN (Ekind (gnat_temp_type), Private_Kind)
                && Present (Full_View (gnat_temp_type))
                && ((Etype (gnat_node) == Full_View (gnat_temp_type))
                    || (Is_Packed (Full_View (gnat_temp_type))
                        && Etype (gnat_node) ==
                             Packed_Array_Type (Full_View (gnat_temp_type)))))
          && (!Is_Itype (Etype (gnat_node)) || !Is_Itype (gnat_temp_type))
          && (Ekind (gnat_temp) == E_Variable
	      || Ekind (gnat_temp) == E_Component
	      || Ekind (gnat_temp) == E_Constant
	      || Ekind (gnat_temp) == E_Loop_Parameter
	      || IN (Ekind (gnat_temp), Formal_Kind)))
	gigi_abort (304);

      /* Expand the type of this identitier first, in case it is
	 an enumeral literal, which only get made when the type
	 is expanded.  There is no order-of-elaboration issue here.  */

      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result = gnat_to_gnu_entity (Entity (gnat_node), NULL_TREE, 0);

      /* The GNAT tree has the type of a function as the type of its result. */
      if (TREE_CODE (TREE_TYPE (gnu_result)) == FUNCTION_TYPE)
	gnu_result_type = TREE_TYPE (gnu_result);

      /* If we are in an exception handler, force this variable into memory
	 to ensure optimization does not remove stores that appear
	 redundant but are actually needed in case an exception occurs.  */
      if (gnu_except_ptr_decl != 0)
	mark_addressable (gnu_result);

      /* Some objects (such as parameters passed by reference, globals of
	 variable size, and renamed objects) actually represent the address
	 of the object.  In that case, we must do the dereference.  Likewise,
	 deal with parameters to foreign convention subprograms.  Call fold
	 here since GNU_RESULT may be a CONST_DECL.  */
      if (TREE_CODE_CLASS (TREE_CODE (gnu_result)) == 'd'
	  && (DECL_BY_REF_P (gnu_result)
	      || DECL_BY_COMPONENT_PTR_P (gnu_result)))
	{
	  int ro = DECL_POINTS_TO_READONLY_P (gnu_result);

	  if (DECL_BY_COMPONENT_PTR_P (gnu_result))
	    gnu_result = convert (build_pointer_type (gnu_result_type),
				  gnu_result);

	  gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE,
				       fold (gnu_result));
	  TREE_READONLY (gnu_result) = TREE_STATIC (gnu_result) = ro;
	}

      /* We always want to return the underlying INTEGER_CST for an
	 enumeration literal to avoid the need to call fold in lots
	 of places.  */
      if (TREE_CODE (gnu_result) == CONST_DECL)
	gnu_result = DECL_INITIAL (gnu_result);

      break;

    case N_Integer_Literal:
      {
	tree gnu_type;

	gnu_type = gnu_result_type = get_unpadded_type (Etype (gnat_node));

	if (TREE_CODE (gnu_type) == RECORD_TYPE
	    && TYPE_LEFT_JUSTIFIED_MODULAR_P (gnu_type))
	  gnu_type = TREE_TYPE (TYPE_FIELDS (gnu_type));

	gnu_result = UI_To_gnu (Intval (gnat_node), gnu_type);
	if (TREE_CONSTANT_OVERFLOW (gnu_result)
#if 0
	    || (TREE_CODE (TYPE_MIN_VALUE (gnu_result_type)) == INTEGER_CST
		&& tree_int_cst_lt (gnu_result,
				    TYPE_MIN_VALUE (gnu_result_type)))
	    || (TREE_CODE (TYPE_MAX_VALUE (gnu_result_type)) == INTEGER_CST
		&& tree_int_cst_lt (TYPE_MAX_VALUE (gnu_result_type),
				    gnu_result))
#endif
	    )
	  gigi_abort (305);
      }
      break;

    case N_Character_Literal:
      /* If a Entity is present, it means that this was one of the
	 literals in a user-defined character type.  In that case,
	 just return the value in the CONST_DECL.  Otherwise, use the
	 character code.  In that case, the base type should be an
	 INTEGER_TYPE, but we won't bother checking for that.  */
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      if (Present (Entity (gnat_node)))
	gnu_result = DECL_INITIAL (get_gnu_tree (Entity (gnat_node)));
      else
	gnu_result = build_int_2 (Char_Literal_Value (gnat_node), 0);
      break;

    case N_Real_Literal:
      /* If this is of a fixed-point type, the value we want is the
	 value of the corresponding integer.  */
      if (IN (Ekind (Underlying_Type (Etype (gnat_node))), Fixed_Point_Kind))
	{
	  gnu_result_type = get_unpadded_type (Etype (gnat_node));
	  gnu_result = UI_To_gnu (Corresponding_Integer_Value (gnat_node),
				  gnu_result_type);
	  if (TREE_CONSTANT_OVERFLOW (gnu_result)
#if 0
	      || (TREE_CODE (TYPE_MIN_VALUE (gnu_result_type)) == INTEGER_CST
		  && tree_int_cst_lt (gnu_result,
				      TYPE_MIN_VALUE (gnu_result_type)))
	      || (TREE_CODE (TYPE_MAX_VALUE (gnu_result_type)) == INTEGER_CST
		  && tree_int_cst_lt (TYPE_MAX_VALUE (gnu_result_type),
				      gnu_result))
#endif
	      )
	    gigi_abort (305);
	}
      else
	{
	  /* We want to avoid doing explicit host machine arithmetic.  The
	     best way to do this is when EXPON_EXPR is implemented, but the
	     kludge that follows is the best way for now. Note that we always
	     convert the literal to the largest floating-point type and do
	     the arithmetic there, then convert to the result type (at the
	     end of this function). We handle negative values by building a
	     separate negation node, this ensures minus zero is preserved */

	  tree gnu_numerator = UI_To_gnu (Norm_Num (Realval (gnat_node)),
					  longest_float_type_node);
	  tree gnu_denominator = UI_To_gnu (Norm_Den (Realval (gnat_node)),
					    longest_float_type_node);

	  gnu_result = build_binary_op (RDIV_EXPR, longest_float_type_node,
					gnu_numerator, gnu_denominator);
	  gnu_result_type = get_unpadded_type (Etype (gnat_node));

	  if (UR_Is_Negative (Realval (gnat_node)))
	    gnu_result = build_unary_op (NEGATE_EXPR, longest_float_type_node,
					 gnu_result);
	}

      break;

    case N_String_Literal:
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      if (TYPE_PRECISION (TREE_TYPE (gnu_result_type)) == 8)
	{
	  /* We assume here that all strings are of type standard.string.
	     "Weird" types of string have been converted to an aggregate
	     by the expander. */
	  String_Id gnat_string = Strval (gnat_node);
	  int length = String_Length (gnat_string);
	  char *string = (char *) alloca (length + 1);
	  int i;

	  /* Build the string with the characters in the literal and
	     end it with a null.  Note that Ada strings are 1-origin.  */
	  for (i = 0; i < length; i++)
	    string[i] = Get_String_Char (gnat_string, i + 1);

	  string[i] = 0;
	  gnu_result = build_string (length + 1, string);

	  /* Strings in GCC don't normally have types, but we want
	     this to not be converted to the array type.  */
	  TREE_TYPE (gnu_result) = gnu_result_type;
	}
      else
	{
	  /* Build a list consisting of each character, then make
	     the aggregate.  */
	  String_Id gnat_string = Strval (gnat_node);
	  int length = String_Length (gnat_string);
	  int i;
	  tree gnu_list = NULL_TREE;

	  for (i = 0; i < length; i++)
	    gnu_list
	      = tree_cons (NULL_TREE,
			   convert (TREE_TYPE (gnu_result_type),
				    build_int_2 (Get_String_Char (gnat_string,
								  i + 1),
						 0)),
			   gnu_list);

	  gnu_result
	    = build_constructor (gnu_result_type, nreverse (gnu_list));
	}
      break;

    case N_Pragma:
      switch (Get_Pragma_Id (Chars (gnat_node)))
	{
	case Pragma_Inspection_Point:
	  set_lineno (gnat_node, 1);
	  for (gnat_temp = First (Pragma_Argument_Associations (gnat_node));
	       Present (gnat_temp);
	       gnat_temp = Next (gnat_temp))
	    {
	      gnu_expr = build1 (USE_EXPR, void_type_node,
				 gnat_to_gnu (Expression ((gnat_temp))));
	      TREE_SIDE_EFFECTS (gnu_expr) = 1;
	      expand_expr_stmt (gnu_expr);
	    }
	  break;

	case Pragma_Optimize:
	  switch (Chars (Expression
			 (First (Pragma_Argument_Associations (gnat_node)))))
	    {
	    case Name_Time:  case Name_Space:
	      if (optimize == 0)
		post_error ("insufficient -O value?", gnat_node);
	      break;

	    case Name_Off:
	      if (optimize != 0)
		post_error ("must specify -O0?", gnat_node);
	      break;

	    default:
	      gigi_abort (331);
	      break;
	    }
	  break;

	case Pragma_Reviewable:
	  if (write_symbols == NO_DEBUG)
	    post_error ("must specify -g?", gnat_node);
	  break;
	}
      break;

    /**************************************/
    /* Chapter 3: Declarations and Types: */
    /**************************************/

    case N_Subtype_Declaration:
    case N_Full_Type_Declaration:
    case N_Incomplete_Type_Declaration:
    case N_Private_Type_Declaration:
    case N_Private_Extension_Declaration:
    case N_Task_Type_Declaration:
      process_type (Defining_Identifier (gnat_node));
      break;

    case N_Object_Declaration:
    case N_Exception_Declaration:
      gnat_temp = Defining_Identifier (gnat_node);

      if (Present (Expression (gnat_node)) 
	  && !(Nkind (gnat_node) == N_Object_Declaration 
	       && No_Default_Init (gnat_node)))
	{
	  gnu_expr = gnat_to_gnu (Expression (gnat_node));
	  if (Do_Range_Check (Expression (gnat_node)))
	    gnu_expr = emit_range_check (gnu_expr, Etype (gnat_temp));

	  /* If this object has its elaboration delayed, we must force
	     evaluation of GNU_EXPR right now and save it for when the object
	     is frozen.  */
	  if (Present (Freeze_Node (gnat_temp)))
	    {
	      if ((Is_Public (gnat_temp) || global_bindings_p ())
		  && ! TREE_CONSTANT (gnu_expr))
		gnu_expr
		  = create_var_decl (create_concat_name (gnat_temp, "init"),
				     NULL_TREE, TREE_TYPE (gnu_expr), gnu_expr,
				     0, Is_Public (gnat_temp), 0, 0, NULL_PTR);
	      else
		gnu_expr = maybe_variable (gnu_expr, Expression (gnat_node));

	      save_gnu_tree (gnat_node, gnu_expr, 1);
	    }
	}
      else
	gnu_expr = NULL_TREE;

      if (No (Freeze_Node ((gnat_temp))))
	gnat_to_gnu_entity (gnat_temp, gnu_expr, 1);
      break;

    case N_Object_Renaming_Declaration:

      gnat_temp = Defining_Identifier (gnat_node);

      /* Don't do anything if this renaming handled by the front end */

      if (! Is_Renaming_Of_Object (gnat_temp))
        {
          gnu_expr = gnat_to_gnu (Renamed_Object (gnat_temp));
          gnat_to_gnu_entity (gnat_temp, gnu_expr, 1);
        }
      break;

    case N_Implicit_Label_Declaration:
      gnat_to_gnu_entity (Defining_Identifier (gnat_node), NULL_TREE, 1);
      break;

    case N_Subprogram_Renaming_Declaration:
    case N_Package_Renaming_Declaration:
    case N_Exception_Renaming_Declaration:
    case N_Number_Declaration:
      /* These are fully handled in the front end.  */
      break;

    /*************************************/
    /* Chapter 4: Names and Expressions: */
    /*************************************/

    case N_Explicit_Dereference:
      gnu_result = gnat_to_gnu (Prefix (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));

      /* Emit access check if necessary */
      if (Do_Access_Check (gnat_node))
	gnu_result = emit_access_check (gnu_result);

      gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_result);
      break;

    case N_Indexed_Component:
      {
	tree gnu_array_object = gnat_to_gnu (Prefix (gnat_node));
	tree gnu_type;
	int ndim;
	int i;
	Node_Id *gnat_expr_array;

	/* Emit access check if necessary */
	if (Do_Access_Check (gnat_node))
	  gnu_array_object = emit_access_check (gnu_array_object);

	gnu_array_object = maybe_implicit_deref (gnu_array_object);
	gnu_array_object = maybe_unconstrained_array (gnu_array_object);
	gnu_result = gnu_array_object;

	/* First compute the number of dimensions of the array, then
	   fill the expression array, the order depending on whether
	   this is a Convention_Fortran array or not.  */
	for (ndim = 1, gnu_type = TREE_TYPE (gnu_array_object);
	     TREE_CODE (TREE_TYPE (gnu_type)) == ARRAY_TYPE
	     && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_type));
	     ndim++, gnu_type = TREE_TYPE (gnu_type))
	  ;

	gnat_expr_array = (Node_Id *) alloca (ndim * sizeof (Node_Id));

	if (TYPE_CONVENTION_FORTRAN_P (TREE_TYPE (gnu_array_object)))
	  for (i = ndim - 1, gnat_temp = First (Expressions (gnat_node));
	       i >= 0;
	       i--, gnat_temp = Next (gnat_temp))
	    gnat_expr_array[i] = gnat_temp;
	else
	  for (i = 0, gnat_temp = First (Expressions (gnat_node));
	       i < ndim;
	       i++, gnat_temp = Next (gnat_temp))
	    gnat_expr_array[i] = gnat_temp;

	for (i = 0, gnu_type = TREE_TYPE (gnu_array_object);
	     i < ndim; i++, gnu_type = TREE_TYPE (gnu_type))
	  {
	    if (TREE_CODE (gnu_type) != ARRAY_TYPE)
	      gigi_abort (307);

	    gnat_temp = gnat_expr_array[i];
	    gnu_expr = gnat_to_gnu (gnat_temp);

	    if (Do_Range_Check (gnat_temp))
	      gnu_expr
		= emit_index_check
		  (gnu_array_object, gnu_expr,
		   TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))),
		   TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))));

	    gnu_result = build_binary_op (ARRAY_REF, NULL_TREE,
					  gnu_result, gnu_expr);
	  }
      }

      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Slice:
      {
        tree gnu_type;
        tree gnu_expr_l, gnu_expr_h;
        Node_Id gnat_range_node = Range (gnat_node);

        gnu_result = gnat_to_gnu (Prefix (gnat_node));
        gnu_result_type = get_unpadded_type (Etype (gnat_node));

        /* Emit access check if necessary */
        if (Do_Access_Check (gnat_node))
          gnu_result = emit_access_check (gnu_result);

        /* Reference the desired first component of Prefix, where the first
           index is the low bound of our type.  Then takes its address and
           convert it to a pointer to our type.  Finally, indirectly
           reference that.  Note that most of these operations are just
           performed to get the types correct and will go away.  */
        gnu_result = maybe_implicit_deref (gnu_result);
        gnu_result = maybe_unconstrained_array (gnu_result);
        gnu_type = TREE_TYPE (gnu_result);
        if (Do_Range_Check (gnat_range_node)) 
          {
            /* Get the bounds of the slice. */
	    tree gnu_index_type
	      = TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_result_type));
            tree gnu_min_expr = TYPE_MIN_VALUE (gnu_index_type);
            tree gnu_max_expr = TYPE_MAX_VALUE (gnu_index_type);
            tree gnu_expr_l, gnu_expr_h, gnu_expr_type;

            /* Check to see that the minimum slice value is in range */
            gnu_expr_l
	      = emit_index_check
		(gnu_result, gnu_min_expr,
		 TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))),
		 TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))));

            /* Check to see that the maximum slice value is in range */
            gnu_expr_h
	      = emit_index_check
		(gnu_result, gnu_max_expr,
		 TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))),
		 TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))));

            /* Derive a good type to convert everything too */
            gnu_expr_type = get_base_type (TREE_TYPE (gnu_expr_l));

            /* Build a compound expression that does the range checks */
            gnu_expr
              = build_binary_op (COMPOUND_EXPR, gnu_expr_type,
                                 convert (gnu_expr_type, gnu_expr_h),
                                 convert (gnu_expr_type, gnu_expr_l));

            /* Build a conditional expression that returns the range checks
               expression if the slice range is not null (max >= min) or
               returns the min if the slice range is null */
            gnu_expr
              = build (COND_EXPR, gnu_expr_type,
		       build_binary_op (GE_EXPR, gnu_expr_type,
					convert (gnu_expr_type, gnu_max_expr),
					convert (gnu_expr_type, gnu_min_expr)),
		       gnu_expr, gnu_min_expr);
          }
        else
          gnu_expr = TYPE_MIN_VALUE (TYPE_DOMAIN (gnu_result_type));

        gnu_result 
          = build_binary_op (ARRAY_REF, NULL_TREE, gnu_result, gnu_expr);
        gnu_result = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_result);
        gnu_result 
          = convert (build_pointer_type (gnu_result_type), gnu_result);
        gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_result);
      }
      break;

    case N_Selected_Component:
      {
	tree gnu_prefix = gnat_to_gnu (Prefix (gnat_node));
	Entity_Id gnat_field = Entity (Selector_Name (gnat_node));
	Entity_Id gnat_pref_type = Etype (Prefix (gnat_node));
	char *field_name;
	tree gnu_field;

	/* For discriminant references in tagged types always substitute the
	   corresponding discriminant as the actual selected component. */

	if (Is_Tagged_Type (gnat_pref_type))
	  while (Present (Corresponding_Discriminant (gnat_field)))
	    gnat_field = Corresponding_Discriminant (gnat_field);

	/* For discriminant references of untagged types always substitute the
	   corresponding girder discriminant. */

	else if (Present (Corresponding_Discriminant (gnat_field)))
	  gnat_field = Original_Record_Component (gnat_field);

	/* Handle extracting the real or imaginary part of a complex.
	   The real part is the first field and the imaginary the last.  */
	if (TREE_CODE (TREE_TYPE (gnu_prefix)) == COMPLEX_TYPE)
	  gnu_result = build_unary_op (Present (Next_Entity (gnat_field))
				       ? REALPART_EXPR : IMAGPART_EXPR,
				       NULL_TREE, gnu_prefix);
	else
	  {
	    gnu_field = gnat_to_gnu_entity (gnat_field, NULL_TREE, 0);
	    field_name = Get_Name_String (Chars (gnat_field));

	    /* If there are discriminants, the prefix might be evaluated more
	       than once, which is a problem if it has side-effects.  */
	    if (Has_Discriminants (Prefix (gnat_node))
		&& TREE_SIDE_EFFECTS (gnu_prefix))
	      gnu_prefix = make_save_expr (gnu_prefix);

	    /* Emit access and discriminant check if necessary.  */
	    if (Do_Access_Check (gnat_node))
	      gnu_prefix = emit_access_check (gnu_prefix);
	    if (Do_Discriminant_Check (gnat_node))
	      gnu_prefix = emit_discriminant_check (gnu_prefix,
						    gnat_node);
	    gnu_result
	      = build_component_ref (gnu_prefix, get_identifier (field_name),
				     gnu_field);
	  }

	if (gnu_result == 0)
	  gigi_abort (308);

	gnu_result_type = get_unpadded_type (Etype (gnat_node));
      }
      break;

    case N_Attribute_Reference:
      {
        /* The attribute designator (like an enumeration value). */
        int attribute = Get_Attribute_Id (Attribute_Name (gnat_node));
	int prefix_unused = 0;
	tree gnu_prefix;
	tree gnu_type;

	/* The Elab_Spec and Elab_Body attributes are special in that
	   Prefix is a unit, not an object with a GCC equivalent.  */
	if (attribute == Attr_Elab_Body || attribute == Attr_Elab_Spec)
	  return
	    create_subprog_decl
	      (create_concat_name (Entity (Prefix (gnat_node)),
				   attribute == Attr_Elab_Body
				   ? "elabb" : "elabs"),
	       NULL_TREE, void_ftype, NULL_TREE, 0, 1, 1, NULL_PTR);

	gnu_prefix = gnat_to_gnu (Prefix (gnat_node));
	gnu_type = TREE_TYPE (gnu_prefix);

        switch (attribute)
          {
	  case Attr_Pos:
	  case Attr_Val:
	    /* These are just conversions until since representation
	       clauses for enumerations are handled in the front end.  */
	    {
	      int check_p = Do_Range_Check (First (Expressions (gnat_node)));

	      gnu_result = gnat_to_gnu (First (Expressions (gnat_node)));
	      gnu_result_type = get_unpadded_type (Etype (gnat_node));
	      gnu_result = convert_with_check (Etype (gnat_node), gnu_result,
					       check_p, check_p, 1);
	    }
	    break;

	  case Attr_Pred:
	  case Attr_Succ:
	    /* These just add or subject the constant 1.  Representation
	       clauses for enumerations are handled in the front-end.  */
	    gnu_expr = gnat_to_gnu (First (Expressions (gnat_node)));
	    gnu_result_type = get_unpadded_type (Etype (gnat_node));

	    if (Do_Range_Check (First (Expressions (gnat_node))))
	      {
		gnu_expr = make_save_expr (gnu_expr);
		gnu_expr
		  = emit_check
		    (build_binary_op (EQ_EXPR, integer_type_node,
				      gnu_expr,
				      attribute == Attr_Pred
				      ? TYPE_MIN_VALUE (gnu_result_type)
				      : TYPE_MAX_VALUE (gnu_result_type)),
		     gnu_expr);
	      }

	    gnu_result
	      = build_binary_op (attribute == Attr_Pred
				 ? MINUS_EXPR : PLUS_EXPR,
				 gnu_result_type, gnu_expr,
				 convert (gnu_result_type, integer_one_node));
	    break;

	  case Attr_Address:

	    /* Conversions don't change something's address but can cause
	       us to miss the COMPONENT_REF case below, so strip them off.  */
	    while (TREE_CODE (gnu_prefix) == NOP_EXPR
		   || TREE_CODE (gnu_prefix) == CONVERT_EXPR)
	      gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

	    /* If we are taking 'Address of an unconstrained object,
	       this is the pointer to the underlying array.  */
	    gnu_prefix = maybe_unconstrained_array (gnu_prefix);

	    /* If we are taking 'Adddress of a component reference, get the
	       offset to the byte containing the bitfield and add
	     it to the address of the underlying object.  */
	    if (TREE_CODE (gnu_prefix) == COMPONENT_REF)
	      {
		tree gnu_offset = size_zero_node;
		tree gnu_ptr_type = build_pointer_type (char_type_node);
		tree gnu_inner = gnu_prefix;

		while (TREE_CODE (gnu_inner) == COMPONENT_REF)
		  {
		    tree gnu_this_offset 
		      = DECL_FIELD_BITPOS (TREE_OPERAND (gnu_inner, 1));

		    /* If OFFSET is a PLUS_EXPR, the first operand is
		       the byte offset.  */
		    if (TREE_CODE (gnu_this_offset) == PLUS_EXPR)
		      gnu_this_offset = TREE_OPERAND (gnu_this_offset, 0);

		    gnu_offset = size_binop (PLUS_EXPR, gnu_offset,
					     gnu_this_offset);
		    gnu_inner = TREE_OPERAND (gnu_inner, 0);
		  }

		if (TREE_CODE (gnu_offset) != INTEGER_CST
		    && contains_placeholder_p (gnu_offset))
		  gnu_offset = build (WITH_RECORD_EXPR, sizetype, gnu_offset,
				      TREE_OPERAND (gnu_prefix, 0));

		mark_addressable (gnu_inner);
		gnu_result_type = get_unpadded_type (Etype (gnat_node));
		gnu_result
		  = fold (build
			  (PLUS_EXPR, gnu_ptr_type,
			   build_unary_op (ADDR_EXPR, gnu_ptr_type, gnu_inner),
			   convert (gnu_ptr_type,
				    size_binop (FLOOR_DIV_EXPR,
						gnu_offset,
						size_int (BITS_PER_UNIT)))));
		break;
	      }

	    /* ... fall through ... */

          case Attr_Access:
          case Attr_Unchecked_Access:
          case Attr_Unrestricted_Access:

	    gnu_result_type = get_unpadded_type (Etype (gnat_node));
	    gnu_result = build_unary_op (ADDR_EXPR, gnu_result_type,
					 gnu_prefix);
	    break;

	  case Attr_Size:
	  case Attr_Object_Size:
	  case Attr_Value_Size:
	  case Attr_Max_Size_In_Storage_Elements:
	    /* In all these cases, we don't want the padding stripped off.  */
	    if (TREE_CODE (gnu_prefix) == COMPONENT_REF
		&& (TYPE_IS_PADDING_P
		    (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0)))))
	      gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

	    /* We can ignore any conversion here.  */
	    while (TREE_CODE (gnu_prefix) == UNCHECKED_CONVERT_EXPR
		   || TREE_CODE (gnu_prefix) == NOP_EXPR
		   || TREE_CODE (gnu_prefix) == CONVERT_EXPR)
	      gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

	    prefix_unused = 1;
	    gnu_type = TREE_TYPE (gnu_prefix);

	    /* Replace an unconstrained array type with the type of the
	       underlying array.  We can't do this with a call to
	       maybe_unconstrained_array since we may have a TYPE_DECL.  */

	    if (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE)
	      gnu_type
		= TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_type))));

	    /* If we are looking for the size of a type or the Value_Size,
	       the result is the RM_Size of the type.  For Object_Size,
	       it is the field size, for a bitfield, or the GCC size of
	       the type in other case.  */
	    if (attribute == Attr_Value_Size
		|| (attribute != Attr_Object_Size
		    && TREE_CODE (gnu_prefix) == TYPE_DECL))
	      gnu_result = rm_size (gnu_type);
	    else if (TREE_CODE (gnu_prefix) == COMPONENT_REF)
	      gnu_result = DECL_SIZE (TREE_OPERAND (gnu_prefix, 1));
	    else
	      gnu_result = TYPE_SIZE (gnu_type);

	    /* Deal with a self-referential size by returning the maximum
	       size for a type and by qualifying the size with
	       the object for 'Size of an object.  */

	    if (TREE_CODE (gnu_result) != INTEGER_CST
		&& contains_placeholder_p (gnu_result))
	      {
		if (TREE_CODE (gnu_prefix) != TYPE_DECL)
		  gnu_result = build (WITH_RECORD_EXPR, TREE_TYPE (gnu_result),
				      gnu_result, gnu_prefix);
		else
		  gnu_result = max_size (gnu_result, 1);
	      }

	    gnu_result_type = get_unpadded_type (Etype (gnat_node));

	    if (attribute == Attr_Max_Size_In_Storage_Elements)
	      gnu_result = size_binop (CEIL_DIV_EXPR, gnu_result,
				       size_int (BITS_PER_UNIT));

	    break;

	  case Attr_Alignment:
	    if (TREE_CODE (gnu_prefix) == COMPONENT_REF
		&& (TYPE_IS_PADDING_P
		    (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0)))))
	      gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

	    gnu_type = TREE_TYPE (gnu_prefix);
	    gnu_result_type = get_unpadded_type (Etype (gnat_node));
	    prefix_unused = 1;

	    if (TREE_CODE (gnu_prefix) == COMPONENT_REF)
	      gnu_result
		= size_int (DECL_ALIGN (TREE_OPERAND (gnu_prefix, 1)));
	    else
	      gnu_result = size_int (TYPE_ALIGN (gnu_type) / BITS_PER_UNIT);
	    break;

	  case Attr_First:
	  case Attr_Last:
	  case Attr_Range_Length:
	    if (TREE_CODE (gnu_prefix) == COMPONENT_REF
		&& (TYPE_IS_PADDING_P
		    (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0)))))
	      gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

	    gnu_type = TREE_TYPE (gnu_prefix);
	    prefix_unused = 1;

	    if (INTEGRAL_TYPE_P (gnu_type)
		|| TREE_CODE (gnu_type) == REAL_TYPE)
	      {
		gnu_result_type = get_unpadded_type (Etype (gnat_node));

		if (attribute == Attr_First)
		  gnu_result = TYPE_MIN_VALUE (gnu_type);
		else if (attribute == Attr_Last)
		  gnu_result = TYPE_MAX_VALUE (gnu_type);
		else
		  gnu_result
		    = build_binary_op
		      (MAX_EXPR, get_base_type (gnu_result_type),
		       build_binary_op
		       (PLUS_EXPR, get_base_type (gnu_result_type),
			build_binary_op (MINUS_EXPR,
					 get_base_type (gnu_result_type),
					 convert (gnu_result_type,
						  TYPE_MAX_VALUE (gnu_type)),
					 convert (gnu_result_type,
						  TYPE_MIN_VALUE (gnu_type))),
			convert (gnu_result_type, integer_one_node)),
		       convert (gnu_result_type, integer_zero_node));

		break;
	      }
	    /* ... fall through ... */
	  case Attr_Length:
	    {
	      int Dimension
		= (Present (Expressions (gnat_node))
		   ? UI_To_Int (Intval (First (Expressions (gnat_node))))
		   : 1);

	      /* Emit access check if necessary */
	      if (Do_Access_Check (gnat_node))
		gnu_prefix = emit_access_check (gnu_prefix);

	      /* Make sure any implicit dereference gets done.  */
	      gnu_prefix = maybe_implicit_deref (gnu_prefix);
	      gnu_prefix = maybe_unconstrained_array (gnu_prefix);

	      if (TREE_CODE (gnu_prefix) == COMPONENT_REF
		  && (TYPE_IS_PADDING_P
		      (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0)))))
		gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

	      gnu_type = TREE_TYPE (gnu_prefix);
	      prefix_unused = 1;
	      gnu_result_type = get_unpadded_type (Etype (gnat_node));

	      if (TYPE_CONVENTION_FORTRAN_P (gnu_type))
		{
		  int ndim;
		  tree gnu_type_temp;

		  for (ndim = 1, gnu_type_temp = gnu_type;
		       TREE_CODE (TREE_TYPE (gnu_type_temp)) == ARRAY_TYPE
		       && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_type_temp));
		       ndim++, gnu_type_temp = TREE_TYPE (gnu_type_temp))
		    ;

		  Dimension = ndim + 1 - Dimension;
		}

	      for (; Dimension > 1; Dimension--)
		gnu_type = TREE_TYPE (gnu_type);

	      if (TREE_CODE (gnu_type) != ARRAY_TYPE)
		gigi_abort (309);

	      if (attribute == Attr_First)
		gnu_result
		  = TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type)));
	      else if (attribute == Attr_Last)
		gnu_result
		  = TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type)));
	      else
		/* 'Length or 'Range_Length.  */
		{
		  tree gnu_compute_type
		    = signed_type (get_base_type (gnu_result_type));

		  gnu_result
		  = build_binary_op
		    (MAX_EXPR, gnu_compute_type,
		     build_binary_op
		     (PLUS_EXPR, gnu_compute_type,
		      build_binary_op 
                      (MINUS_EXPR, gnu_compute_type,
		       convert (gnu_compute_type,
				TYPE_MAX_VALUE
				(TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type)))),
		       convert (gnu_compute_type,
				TYPE_MIN_VALUE
				(TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))))),
		      convert (gnu_compute_type, integer_one_node)),
		     convert (gnu_compute_type, integer_zero_node));
		}

	      /* If this has a PLACEHOLDER_EXPR, qualify it by the object
		 we are handling.  Note that these attributes could not
		 have been used on an unconstrained array type.  */
	      if (TREE_CODE (gnu_result) != INTEGER_CST
		  && contains_placeholder_p (gnu_result))
		gnu_result = build (WITH_RECORD_EXPR, TREE_TYPE (gnu_result),
				    gnu_result, gnu_prefix);

	      break;
	    }

	  case Attr_Position:
	  case Attr_First_Bit:
	  case Attr_Last_Bit:
	  case Attr_Bit:
	    {
	      int bitsize, bitpos;
	      tree offset;
	      enum machine_mode mode;
	      int unsignedp, volatilep, alignment;

	      if (TREE_CODE (gnu_prefix) == COMPONENT_REF
		  && (TYPE_IS_PADDING_P
		      (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0)))))
		gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

	      /* We can ignore any conversion here.  */
	      while (TREE_CODE (gnu_prefix) == UNCHECKED_CONVERT_EXPR
		     || TREE_CODE (gnu_prefix) == NOP_EXPR
		     || TREE_CODE (gnu_prefix) == CONVERT_EXPR)
		gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

	      if (TREE_CODE (gnu_prefix) != COMPONENT_REF
		  && attribute != Attr_Bit)
		gigi_abort (310);

	      get_inner_reference (gnu_prefix, &bitsize, &bitpos, &offset,
				   &mode, &unsignedp, &volatilep, &alignment);

	      prefix_unused = 1;

	      if (offset)
		offset = size_binop (PLUS_EXPR, offset, size_int (bitpos));
	      else
		offset = size_int (bitpos);

	      if (attribute == Attr_Position)
		gnu_result
		  = size_binop (TRUNC_DIV_EXPR,
				DECL_FIELD_BITPOS
				(TREE_OPERAND (gnu_prefix, 1)),
				size_int (BITS_PER_UNIT));
	      else if (attribute == Attr_First_Bit || attribute == Attr_Bit)
		gnu_result = size_binop (TRUNC_MOD_EXPR, offset,
					 size_int (BITS_PER_UNIT));
	      else if (attribute == Attr_Last_Bit)
		gnu_result = size_binop (PLUS_EXPR,
					 size_binop (TRUNC_MOD_EXPR, offset,
						     size_int (BITS_PER_UNIT)),
					 size_int (bitsize - 1));
	    }

	    gnu_result_type = get_unpadded_type (Etype (gnat_node));
	    break;

	  case Attr_Min:
	  case Attr_Max:
	    gnu_lhs = gnat_to_gnu (First (Expressions (gnat_node)));
	    gnu_rhs =  gnat_to_gnu (Next (First (Expressions (gnat_node))));

	    gnu_result_type = get_unpadded_type (Etype (gnat_node));
	    gnu_result = build_binary_op (attribute == Attr_Min
					  ? MIN_EXPR : MAX_EXPR,
					  gnu_result_type, gnu_lhs, gnu_rhs);
	    break;

	  case Attr_Passed_By_Reference:
	    gnu_result = size_int (default_pass_by_ref (gnu_type));
	    gnu_result_type = get_unpadded_type (Etype (gnat_node));
	    break;

	  case Attr_Component_Size:
	    if (TREE_CODE (gnu_prefix) == COMPONENT_REF
		&& (TYPE_IS_PADDING_P
		    (TREE_TYPE (TREE_OPERAND (gnu_prefix, 0)))))
	      gnu_prefix = TREE_OPERAND (gnu_prefix, 0);

	    gnu_prefix = maybe_implicit_deref (gnu_prefix);
	    gnu_type = TREE_TYPE (gnu_prefix);

	    if (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE)
	      gnu_type
		= TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_type))));

	    while (TREE_CODE (gnu_type) == ARRAY_TYPE
		   && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_type)))
	      gnu_type = TREE_TYPE (gnu_type);

	    if (TREE_CODE (gnu_type) != ARRAY_TYPE)
	      gigi_abort (330);

	    /* Note this size cannot be self-referential.  */
	    gnu_result = TYPE_SIZE (TREE_TYPE (gnu_type));
	    gnu_result_type = get_unpadded_type (Etype (gnat_node));
	    prefix_unused = 1;
	    break;

	  case Attr_Null_Parameter:
	    /* This is just a zero cast to the pointer type for
	       our prefix and dereferenced.  */
	    gnu_result_type = get_unpadded_type (Etype (gnat_node));
	    gnu_result
	      = build_unary_op (INDIRECT_REF, NULL_TREE,
				convert (build_pointer_type (gnu_result_type),
					 integer_zero_node));
	    TREE_PRIVATE (gnu_result) = 1;
	    break;

	  case Attr_Mechanism_Code:
	    {
	      int code;
	      tree gnu_obj;
	      Entity_Id gnat_obj = Entity (Prefix (gnat_node));

	      prefix_unused = 1;
	      gnu_result_type = get_unpadded_type (Etype (gnat_node));
	      if (Present (Expressions (gnat_node)))
		{
		  int i = UI_To_Int (Intval (First (Expressions (gnat_node))));

		  for (gnat_obj = First_Formal (gnat_obj); i > 1;
		       i--, gnat_obj = Next_Formal (gnat_obj))
		    ;
		}

	      code = Mechanism (gnat_obj);
	      if (code == Default)
		code = ((present_gnu_tree (gnat_obj)
			 && (DECL_BY_REF_P (get_gnu_tree (gnat_obj))
			     || (DECL_BY_COMPONENT_PTR_P
				 (get_gnu_tree (gnat_obj)))))
			? By_Reference : By_Copy);
	      gnu_result = convert (gnu_result_type, size_int (- code));
	    }
	  break;

          default:
	    /* Say we have an unimplemented attribute.  Then set the
	       value to be returned to be a zero and hope that's something
	       we can convert to the type of this attribute.  */

	    post_error ("unimplemented attribute", gnat_node);
	    gnu_result_type = get_unpadded_type (Etype (gnat_node));
	    gnu_result = integer_zero_node;
	    break;
          }

	/* If the result is a constant that overflows, raise
	   constraint error.  */
	if (TREE_CODE (gnu_result) == INTEGER_CST
	    && TREE_CONSTANT_OVERFLOW (gnu_result))
	  {
	    post_error ("Constraint_Error will be raised at run-time?",
			gnat_node);
	    gnu_result
	      = build1 (NULL_EXPR, gnu_result_type,
			build_call_0_expr (raise_constraint_error_decl));
	  }

	/* If this is an attribute where the prefix was unused,
	   force a use of it if it has a side-effect.  */
	if (prefix_unused && TREE_SIDE_EFFECTS (gnu_prefix))
	  gnu_result = fold (build (COMPOUND_EXPR, TREE_TYPE (gnu_result),
				    gnu_prefix, gnu_result));
      }
      break;

    case N_Reference:
      /* Like 'Access as far as we are concerned.  */
      gnu_result = gnat_to_gnu (Prefix (gnat_node));
      gnu_result = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_result);
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Aggregate:
    case N_Extension_Aggregate:
      {
	tree gnu_aggr_type;

	/* ??? It is wrong to evaluate the type now, but there doesn't
	   seem to be any other practical way of doing it.  */

	gnu_aggr_type = gnu_result_type
	  = get_unpadded_type (Etype (gnat_node));

	if (TREE_CODE (gnu_result_type) == RECORD_TYPE
	    && TYPE_CONTAINS_TEMPLATE_P (gnu_result_type))
	  gnu_aggr_type
	    = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (gnu_result_type)));

	if (Null_Record_Present (gnat_node))
	  gnu_result = build_constructor (gnu_aggr_type, NULL_TREE);

	else if (TREE_CODE (gnu_aggr_type) == RECORD_TYPE)
	  gnu_result
	    = assoc_to_constructor (First (Component_Associations (gnat_node)),
				    gnu_aggr_type);
	else if (TREE_CODE (gnu_aggr_type) == UNION_TYPE)
	  {
	    /* The first element is the discrimant, which we ignore.  The
	       next is the field we're building.  Convert the expression
	       to the type of the field and then to the union type.  */
	    Node_Id gnat_assoc
	      = Next (First (Component_Associations (gnat_node)));
	    Entity_Id gnat_field = Entity (First (Choices (gnat_assoc)));
	    tree gnu_field_type
	      = TREE_TYPE (gnat_to_gnu_entity (gnat_field, NULL_TREE, 0));

	    gnu_result = convert (gnu_field_type,
				  gnat_to_gnu (Expression (gnat_assoc)));
	  }
	else if (TREE_CODE (gnu_aggr_type) == ARRAY_TYPE)
	  gnu_result = pos_to_constructor (First (Expressions (gnat_node)),
					   gnu_aggr_type,
					   Component_Type (Etype (gnat_node)));
	else if (TREE_CODE (gnu_aggr_type) == COMPLEX_TYPE)
	  gnu_result
	    = build_binary_op
	      (COMPLEX_EXPR, gnu_aggr_type,
	       gnat_to_gnu (Expression (First
					(Component_Associations (gnat_node)))),
	       gnat_to_gnu (Expression
			    (Next
			     (First (Component_Associations (gnat_node))))));
	else
	  gigi_abort (312);

	gnu_result = convert (gnu_result_type, gnu_result);
      }
      break;

    case N_Null:
      gnu_result = null_pointer_node;
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Type_Conversion:
    case N_Qualified_Expression:
      /* Get the operand expression.  */
      gnu_result = gnat_to_gnu (Expression (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));

      gnu_result
	= convert_with_check (Etype (gnat_node), gnu_result,
			      Do_Overflow_Check (gnat_node),
			      Do_Range_Check (Expression (gnat_node)),
			      Nkind (gnat_node) == N_Type_Conversion
			      && Float_Truncate (gnat_node));
      break;

    case N_Unchecked_Type_Conversion:
      gnu_result = gnat_to_gnu (Expression (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result = unchecked_convert (gnu_result_type, gnu_result);
      break;

    case N_In:
    case N_Not_In:
      {
	tree gnu_object = gnat_to_gnu (Left_Opnd (gnat_node));
	Node_Id gnat_range = Right_Opnd (gnat_node);
	tree gnu_low;
	tree gnu_high;

	/* GNAT_RANGE is either an N_Range node or an identifier
	   denoting a subtype.  */
	if (Nkind (gnat_range) == N_Range)
	  {
	    gnu_low = gnat_to_gnu (Low_Bound (gnat_range));
	    gnu_high = gnat_to_gnu (High_Bound (gnat_range));
	  }
	else if (Nkind (gnat_range) == N_Identifier
              || Nkind (gnat_range) == N_Expanded_Name)
	  {
	    tree gnu_range_type = get_unpadded_type (Entity (gnat_range));

	    gnu_low = TYPE_MIN_VALUE (gnu_range_type);
	    gnu_high = TYPE_MAX_VALUE (gnu_range_type);
	  }
	else
	  gigi_abort (313);

	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	/* If LOW and HIGH are identical, perform an equality test.
	   Otherwise, ensure that GNU_OBJECT is only evaluated once
	   and perform a full range test.  */
	if (operand_equal_p (gnu_low, gnu_high, 0))
	  gnu_result = build_binary_op (EQ_EXPR, gnu_result_type,
					gnu_object, gnu_low);
	else
	  {
	    gnu_object = make_save_expr (gnu_object);
	    gnu_result
	      = build_binary_op (TRUTH_ANDIF_EXPR, gnu_result_type,
				 build_binary_op (GE_EXPR, gnu_result_type,
						  gnu_object, gnu_low),
				 build_binary_op (LE_EXPR, gnu_result_type,
						  gnu_object, gnu_high));
	  }

	if (Nkind (gnat_node) == N_Not_In)
	  gnu_result = invert_truthvalue (gnu_result);
      }
      break;

    case N_Op_Divide:
      gnu_lhs = gnat_to_gnu (Left_Opnd (gnat_node));
      gnu_rhs = gnat_to_gnu (Right_Opnd (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result = build_binary_op (FLOAT_TYPE_P (gnu_result_type)
				    ? RDIV_EXPR
				    : (Rounded_Result (gnat_node)
				       ? ROUND_DIV_EXPR : TRUNC_DIV_EXPR),
				    gnu_result_type, gnu_lhs, gnu_rhs);
      break;

    case N_And_Then: case N_Or_Else:
      {
	enum tree_code code = gnu_codes[Nkind (gnat_node)];
	tree gnu_rhs_side;

	/* The elaboration of the RHS may generate code.  If so,
	   we need to make sure it gets executed after the LHS.  */
	gnu_lhs = gnat_to_gnu (Left_Opnd (gnat_node));
	clear_last_expr ();
	gnu_rhs_side = expand_start_stmt_expr ();
	gnu_rhs = gnat_to_gnu (Right_Opnd (gnat_node));
	expand_end_stmt_expr (gnu_rhs_side);
	gnu_result_type = get_unpadded_type (Etype (gnat_node));

	if (RTL_EXPR_SEQUENCE (gnu_rhs_side) != 0)
	  gnu_rhs = build (COMPOUND_EXPR, gnu_result_type, gnu_rhs_side,
			   gnu_rhs);

	gnu_result = build_binary_op (code, gnu_result_type, gnu_lhs, gnu_rhs);
      }
      break;

    case N_Op_Or:    case N_Op_And:      case N_Op_Xor:
      /* These can either be operations on booleans or on modular types.
	 Fall through for boolean types since that's the way GNU_CODES is
	 set up.  */
      if (IN (Ekind (Underlying_Type (Etype (gnat_node))),
	      Modular_Integer_Kind))
	{
	  enum tree_code code
	    = (Nkind (gnat_node) == N_Op_Or ? BIT_IOR_EXPR
	       : Nkind (gnat_node) == N_Op_And ? BIT_AND_EXPR
	       : BIT_XOR_EXPR);

	  gnu_lhs = gnat_to_gnu (Left_Opnd (gnat_node));
	  gnu_rhs = gnat_to_gnu (Right_Opnd (gnat_node));
	  gnu_result_type = get_unpadded_type (Etype (gnat_node));
	  gnu_result = build_binary_op (code, gnu_result_type,
					gnu_lhs, gnu_rhs);
	  break;
	}

      /* ... fall through ... */

    case N_Op_Eq:    case N_Op_Ne:	 case N_Op_Lt:
    case N_Op_Le:    case N_Op_Gt:       case N_Op_Ge:
    case N_Op_Add:   case N_Op_Subtract: case N_Op_Multiply:
    case N_Op_Mod:   case N_Op_Rem:	 case N_Op_Expon:
    case N_Op_Rotate_Left:
    case N_Op_Rotate_Right:
    case N_Op_Shift_Left:
    case N_Op_Shift_Right:
    case N_Op_Shift_Right_Arithmetic:
      {
	enum tree_code code = gnu_codes[Nkind (gnat_node)];
	tree gnu_type;

	gnu_lhs = gnat_to_gnu (Left_Opnd (gnat_node));
	gnu_rhs = gnat_to_gnu (Right_Opnd (gnat_node));
	gnu_type = gnu_result_type = get_unpadded_type (Etype (gnat_node));

	/* If this is a comparison operator, convert any references to
	   an unconstrained array value into a reference to the
	   actual array.  */
	if (TREE_CODE_CLASS (code) == '<')
	  {
	    gnu_lhs = maybe_unconstrained_array (gnu_lhs);
	    gnu_rhs = maybe_unconstrained_array (gnu_rhs);
	  }

	/* If this is a shift whose count is not guaranteed to be correct,
	   we need to adjust the shift count.  */
	if (IN (Nkind (gnat_node), N_Op_Shift)
	    && ! Shift_Count_OK (gnat_node))
	  {
	    tree gnu_count_type = get_base_type (TREE_TYPE (gnu_rhs));
	    tree gnu_max_shift
	      = convert (gnu_count_type, TYPE_SIZE (gnu_type));

	    if (Nkind (gnat_node) == N_Op_Rotate_Left
		|| Nkind (gnat_node) == N_Op_Rotate_Right)
	      gnu_rhs = build_binary_op (TRUNC_MOD_EXPR, gnu_count_type,
					 gnu_rhs, gnu_max_shift);
	    else if (Nkind (gnat_node) == N_Op_Shift_Right_Arithmetic)
	      gnu_rhs
		= build_binary_op
		  (MIN_EXPR, gnu_count_type,
		   build_binary_op (MINUS_EXPR,
				    gnu_count_type,
				    gnu_max_shift,
				    convert (gnu_count_type,
					     integer_one_node)),
		   gnu_rhs);
	  }

	/* For right shifts, the type says what kind of shift to do,
	   so we may need to choose a different type.  */
	if (Nkind (gnat_node) == N_Op_Shift_Right
	    && ! TREE_UNSIGNED (gnu_type))
	  gnu_type = unsigned_type (gnu_type);
	else if (Nkind (gnat_node) == N_Op_Shift_Right_Arithmetic
		 && TREE_UNSIGNED (gnu_type))
	  gnu_type = signed_type (gnu_type);

	if (gnu_type != gnu_result_type)
	  {
	    gnu_lhs = convert (gnu_type, gnu_lhs);
	    gnu_rhs = convert (gnu_type, gnu_rhs);
	  }

	gnu_result = build_binary_op (code, gnu_type, gnu_lhs, gnu_rhs);

	/* If this is a logical shift with the shift count not verified,
	   we must return zero if it is too large.  We cannot compensate
	   above in this case.  */
	if ((Nkind (gnat_node) == N_Op_Shift_Left
	     || Nkind (gnat_node) == N_Op_Shift_Right)
	    && ! Shift_Count_OK (gnat_node))
	  gnu_result
	    = build_cond_expr
	      (gnu_type, 
	       build_binary_op (GT_EXPR, integer_type_node,
				gnu_rhs,
				convert (TREE_TYPE (gnu_rhs),
					 TYPE_SIZE (gnu_type))),
	       convert (gnu_type, integer_zero_node),
	       gnu_result);
      }
      break;

    case N_Conditional_Expression:
      {
        tree gnu_cond = gnat_to_gnu (First (Expressions (gnat_node)));
        tree gnu_true = gnat_to_gnu (Next (First (Expressions (gnat_node))));
        tree gnu_false
          = gnat_to_gnu (Next (Next (First (Expressions (gnat_node)))));

	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	gnu_result = build_cond_expr (gnu_result_type,
				      truthvalue_conversion (gnu_cond),
				      gnu_true, gnu_false);
      }
      break;

    case N_Op_Plus:
      gnu_result = gnat_to_gnu (Right_Opnd (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      break;

    case N_Op_Not:
      /* This case can apply to a boolean or a modular type.
	 Fall through for a boolean operand since GNU_CODES is set
	 up to handle this.  */
      if (IN (Ekind (Etype (gnat_node)), Modular_Integer_Kind))
	{
	  gnu_expr = gnat_to_gnu (Right_Opnd (gnat_node));
	  gnu_result_type = get_unpadded_type (Etype (gnat_node));
	  gnu_result = build_unary_op (BIT_NOT_EXPR, gnu_result_type,
				       gnu_expr);
	  break;
	}

      /* ... fall through ... */

    case N_Op_Minus:  case N_Op_Abs:
      gnu_expr = gnat_to_gnu (Right_Opnd (gnat_node));
      gnu_result_type = get_unpadded_type (Etype (gnat_node));
      gnu_result = build_unary_op (gnu_codes[Nkind (gnat_node)],
				   gnu_result_type, gnu_expr);
      break;

    case N_Allocator:
      {
	tree gnu_init = 0;
	Entity_Id gnat_type;
	tree gnu_type;

	gnat_temp = Expression (gnat_node);

	/* The Expression operand can either be an N_Identifier or
	   Expanded_Name, which must represent a type, or a
	   N_Qualified_Expression, which contains both the object type and an
	   initial value for the object.  */
	if (Nkind (gnat_temp) == N_Identifier
	    || Nkind (gnat_temp) == N_Expanded_Name)
	  gnu_type = gnat_to_gnu_type (Entity (gnat_temp));
	else if (Nkind (gnat_temp) == N_Qualified_Expression)
	  {
	    Entity_Id gnat_desig_type
	      = Designated_Type (Underlying_Type (Etype (gnat_node)));

	    gnu_init = gnat_to_gnu (Expression (gnat_temp));
	    gnu_init = maybe_unconstrained_array (gnu_init);
            if (Do_Range_Check (Expression (gnat_temp)))
              gnu_init = emit_range_check (gnu_init, gnat_desig_type);

	    if (Is_Elementary_Type (gnat_desig_type)
		|| Is_Constrained (gnat_desig_type))
	      {
		gnu_type = gnat_to_gnu_type (gnat_desig_type);
		gnu_init = convert (gnu_type, gnu_init);
	      }
	    else
	      gnu_type = TREE_TYPE (gnu_init);
	  }
	else
	  gigi_abort (315);

	gnu_result_type = get_unpadded_type (Etype (gnat_node));
	return build_allocator (gnu_type, gnu_init, gnu_result_type,
				Procedure_To_Call (gnat_node),
				Storage_Pool (gnat_node));
      }
      break;

    /***************************/
    /* Chapter 5: Statements:  */
    /***************************/

    case N_Label:
      expand_label (gnat_to_gnu (Identifier (gnat_node)));
      break;

    case N_Null_Statement:
      break;

    case N_Assignment_Statement:
      /* Get the LHS and RHS of the statement and convert any reference to an
	 unconstrained array into a reference to the underlying array.  */
      gnu_lhs = maybe_unconstrained_array (gnat_to_gnu (Name (gnat_node)));
      gnu_rhs
	= maybe_unconstrained_array (gnat_to_gnu (Expression (gnat_node)));

      /* If range check is needed, emit code to generate it */
      if (Do_Range_Check (Expression (gnat_node)))
	gnu_rhs = emit_range_check (gnu_rhs, Etype (Name (gnat_node)));

      set_lineno (gnat_node, 1);
      expand_expr_stmt (build_binary_op (MODIFY_EXPR, NULL_TREE,
					 gnu_lhs, gnu_rhs));
      break;

    case N_If_Statement:
      /* Start an IF statement giving the condition.  */
      gnu_expr = gnat_to_gnu (Condition (gnat_node));
      set_lineno (gnat_node, 1);
      expand_start_cond (gnu_expr, 0);

      push_momentary ();

      /* Generate code for the statements to be executed if the condition
	 is true.  */

      for (gnat_temp = First (Then_Statements (gnat_node));
	   Present (gnat_temp);
	   gnat_temp = Next (gnat_temp))
	{
	  gnat_to_code (gnat_temp);
	  clear_momentary();
	}

      /* Generate each of the "else if" parts.  */
      if (Present (Elsif_Parts (gnat_node)))
	{
	  for (gnat_temp = First (Elsif_Parts (gnat_node));
	       Present (gnat_temp);
	       gnat_temp = Next (gnat_temp))
	    {
	      Node_Id gnat_statement;

	      expand_start_else ();

	      /* Set up the line numbers for each condition we test.  */
	      set_lineno (Condition (gnat_temp), 1);
	      expand_elseif (gnat_to_gnu (Condition (gnat_temp)));

	      for (gnat_statement = First (Then_Statements (gnat_temp));
		   Present (gnat_statement);
		   gnat_statement = Next (gnat_statement))
		{
		  gnat_to_code (gnat_statement);
		  clear_momentary ();
		}
	    }
	}

      /* Finally, handle any statements in the "else" part.  */
      if (Present (Else_Statements (gnat_node)))
	{
	  expand_start_else ();

	  for (gnat_temp = First (Else_Statements (gnat_node));
	       Present (gnat_temp);
	       gnat_temp = Next (gnat_temp))
	    {
	      gnat_to_code (gnat_temp);
	      clear_momentary ();
	    }
	}

      pop_momentary ();
      expand_end_cond ();
      break;

    case N_Case_Statement:
      {
	Node_Id gnat_when;
	Node_Id gnat_choice;
	tree gnu_label;
	Node_Id gnat_statement;

	gnu_expr = gnat_to_gnu (Expression (gnat_node));
	set_lineno (gnat_node, 1);
	expand_start_case (1, gnu_expr, TREE_TYPE (gnu_expr), "case");

	push_momentary();

	for (gnat_when = First_Non_Pragma (Alternatives (gnat_node));
	     Present (gnat_when);
	     gnat_when = Next_Non_Pragma (gnat_when))
	  {
	    /* First compile all the different case choices for the  current
	       WHEN alternative.  */

	    for (gnat_choice = First (Discrete_Choices (gnat_when));
		 Present (gnat_choice); gnat_choice = Next (gnat_choice))
              {
 	        gnu_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

		set_lineno (gnat_choice, 1);
		switch (Nkind (gnat_choice))
		  {
		  case N_Range:
		    pushcase_range
		      (gnat_to_gnu (Low_Bound (gnat_choice)),
		       gnat_to_gnu (High_Bound (gnat_choice)),
		       convert, gnu_label, NULL_PTR);
		    break;

		  case N_Subtype_Indication:
		    pushcase_range
		      (gnat_to_gnu (Low_Bound
		        (Range_Expression (Constraint (gnat_choice)))),
		       gnat_to_gnu (High_Bound
		        (Range_Expression (Constraint (gnat_choice)))),
		       convert, gnu_label, NULL_PTR);
		    break;

		  case N_Identifier:
                  case N_Expanded_Name:
		    /* This represents either a subtype range or a static value
		       of some kind; Ekind says which.  If a static value,
		       fall through to the next case.  */
		    if (IN (Ekind (Entity (gnat_choice)), Type_Kind))
		      {
			tree type = get_unpadded_type (Entity (gnat_choice));

			pushcase_range (fold (TYPE_MIN_VALUE (type)),
					fold (TYPE_MAX_VALUE (type)),
					convert, gnu_label, NULL_PTR);
			break;
		      }
		    /* ... fall through ... */
		  case N_Character_Literal:
		  case N_Integer_Literal:
		    pushcase (gnat_to_gnu (gnat_choice), convert,
			      gnu_label, NULL_PTR);
		    break;

		  case N_Others_Choice:
		    pushcase (NULL_TREE, convert, gnu_label, NULL_PTR);
		    break;

		  default:
		    gigi_abort (316);
		  }
	      }

	    /* After compiling the choices attached to the WHEN compile the
	       body of statements that have to be executed, should the
	       "WHEN ... =>" be taken.  */
	    for (gnat_statement = First (Statements (gnat_when));
		 Present (gnat_statement);
		 gnat_statement = Next (gnat_statement))
	      {
		gnat_to_code (gnat_statement);
		clear_momentary ();
	      }

	    /* Communicate to GCC that we are done with the current WHEN,
	       i.e. insert a "break" statement.  */
	    expand_exit_something ();
	  }

	pop_momentary();
	expand_end_case (gnu_expr);
      }
      break;

    case N_Loop_Statement:
      {
	/* The loop variable in GCC form, if any. */
	tree gnu_loop_var = NULL_TREE;
	/* PREINCREMENT_EXPR or PREDECREMENT_EXPR.  */
	enum tree_code gnu_update;
	/* Used if this is a named loop for so EXIT can work.  */
	struct nesting *loop_id;
	/* Condition to continue loop tested at top of loop.  */
	tree gnu_top_condition = integer_one_node;
	/* Similar, but tested at bottom of loop.  */
	tree gnu_bottom_condition = integer_one_node;
	Node_Id gnat_statement;
	Node_Id gnat_iter_scheme = Iteration_Scheme (gnat_node);
	Node_Id gnat_top_condition = Empty;
	int enclosing_if_p = 0;

	/* Set the condition that under which the loop should continue.
	   For "LOOP .... END LOOP;" the condition is always true.  */
	if (No (gnat_iter_scheme))
	  ;
	/* The case "WHILE condition LOOP ..... END LOOP;" */
	else if (Present (Condition (gnat_iter_scheme)))
	  gnat_top_condition = Condition (gnat_iter_scheme);
        else
	  {
	    /* We have an iteration scheme.  */
	    Node_Id gnat_loop_spec
	      = Loop_Parameter_Specification (gnat_iter_scheme);
	    Entity_Id gnat_loop_var = Defining_Identifier (gnat_loop_spec);
	    Entity_Id gnat_type = Etype (gnat_loop_var);
	    tree gnu_type = get_unpadded_type (gnat_type);
	    tree gnu_low = TYPE_MIN_VALUE (gnu_type);
	    tree gnu_high = TYPE_MAX_VALUE (gnu_type);
	    int reversep = Reverse_Present (gnat_loop_spec);
	    tree gnu_first = reversep ? gnu_high : gnu_low;
	    tree gnu_last = reversep ? gnu_low : gnu_high;
	    enum tree_code end_code = reversep ? GE_EXPR : LE_EXPR;
	    tree gnu_base_type = get_base_type (gnu_type);
	    tree gnu_limit
	      = (reversep ? TYPE_MIN_VALUE (gnu_base_type)
		 : TYPE_MAX_VALUE (gnu_base_type));

	    /* We know the loop variable will not overflow if GNU_LAST is
	       a constant and is not equal to GNU_LIMIT.  If it might
	       overflow, we have to move the limit test to the end of
	       the loop.  In that case, we have to test for an
	       empty loop outside the loop.  */
	    if (TREE_CODE (gnu_last) != INTEGER_CST
		|| TREE_CODE (gnu_limit) != INTEGER_CST
		|| tree_int_cst_equal (gnu_last, gnu_limit))
	      {
		gnu_expr = build_binary_op (LE_EXPR, integer_type_node,
					    gnu_low, gnu_high);
		set_lineno (gnat_loop_spec, 1);
		expand_start_cond (gnu_expr, 0);
		enclosing_if_p = 1;
	      }


	    /* Open a new nesting level that will surround the loop to declare
	       the loop index variable.  */
	    pushlevel (0);
	    expand_start_bindings (0);

	    /* Declare the loop index and set it to its initial value.  */
	    gnu_loop_var = gnat_to_gnu_entity (gnat_loop_var, gnu_first, 1);

	    /* The loop variable might be a padded type, so use `convert' to
	       get a reference to the inner variable if so.  */
	    gnu_loop_var = convert (get_base_type (gnu_type), gnu_loop_var);

	    /* Set either the top or bottom exit condition as
	       appropriate depending on whether we know an overflow
	       cannot occur or not. */
	    if (enclosing_if_p)
	      gnu_bottom_condition
		= build_binary_op (NE_EXPR, integer_type_node,
				   gnu_loop_var, gnu_last);
	    else
	      gnu_top_condition
		= build_binary_op (end_code, integer_type_node,
				   gnu_loop_var, gnu_last);

	    gnu_update = reversep ? PREDECREMENT_EXPR : PREINCREMENT_EXPR;
	  }

	set_lineno (gnat_node, 1);
	if (gnu_loop_var)
	  loop_id = expand_start_loop_continue_elsewhere (1);
	else
	  loop_id = expand_start_loop (1);

	/* If the loop was named, have the name point to this loop.  In this
	   case, the association is not a ..._DECL node; in fact, it isn't
	   a GCC tree node at all.  Since this name is referenced inside
	   the loop, do it before we process the statements of the loop.  */
        if (Present (Identifier (gnat_node)))
	  save_gnu_tree (Entity (Identifier (gnat_node)),
			 (tree) loop_id, 1);

	set_lineno (gnat_node, 1);

	/* We must evaluate the condition after we've entered the
	   loop so that any expression actions get done in the right
	   place.  */
	if (Present (gnat_top_condition))
	  gnu_top_condition = gnat_to_gnu (gnat_top_condition);

	expand_exit_loop_if_false (NULL_PTR, gnu_top_condition);
	push_momentary ();

	for (gnat_statement = First (Statements (gnat_node));
	     Present (gnat_statement);
	     gnat_statement = Next (gnat_statement))
	  {
	    gnat_to_code (gnat_statement);
	    clear_momentary ();
	  }

	pop_momentary ();
	set_lineno (gnat_node, 1);
	expand_exit_loop_if_false (NULL_PTR, gnu_bottom_condition);

	if (gnu_loop_var)
	  {
	    expand_loop_continue_here ();
	    gnu_expr = build_binary_op (gnu_update, TREE_TYPE (gnu_loop_var),
					gnu_loop_var,
					convert (TREE_TYPE (gnu_loop_var),
						 integer_one_node));
	    set_lineno (gnat_iter_scheme, 1);
	    expand_expr_stmt (gnu_expr);
	  }

	set_lineno (gnat_node, 1);
	expand_end_loop ();

	if (gnu_loop_var)
	  {
	    /* Close the nesting level that sourround the loop that was used to
	       declare the loop index variable.   */
	    set_lineno (gnat_node, 1);
	    expand_end_bindings (getdecls (), 1, 0);
	    poplevel (1, 1, 0);
	  }

	if (enclosing_if_p)
	  {
	    set_lineno (gnat_node, 1);
	    expand_end_cond ();
	  }
      }
      break;

    case N_Block_Statement:
      pushlevel (0);
      gnu_block_stack = tree_cons (NULL_TREE, NULL_TREE, gnu_block_stack);
      expand_start_bindings (0);
      process_decls (Declarations (gnat_node), Empty, Empty, 1, 1);
      gnat_to_code (Handled_Statement_Sequence (gnat_node));
      expand_end_bindings (getdecls (), kept_level_p (), 0);
      poplevel (kept_level_p (), 1, 0);
      gnu_block_stack = TREE_CHAIN (gnu_block_stack);
      break;

    case N_Exit_Statement:
      {
	/* Which loop to exit, NULL if the current loop.   */
	struct nesting *loop_id = NULL_PTR;
	/* The GCC version of the optional GNAT condition node attached to the
	   exit statement. Exit the loop if this is false.  */
	tree gnu_cond = integer_zero_node;

	if (Present (Name (gnat_node)))
	  loop_id
	    = (struct nesting *) get_gnu_tree (Entity (Name (gnat_node)));

	if (Present (Condition (gnat_node)))
	  gnu_cond
	    = invert_truthvalue
	      (truthvalue_conversion (gnat_to_gnu (Condition (gnat_node))));

	set_lineno (gnat_node, 1);
	expand_exit_loop_if_false (loop_id, gnu_cond);
      }
      break;

    case N_Return_Statement:
      {
	/* The gnu function type of the subprogram currently processed.  */
	tree gnu_subprog_type = TREE_TYPE (current_function_decl);
	/* The return value from the subprogram.  */
	tree gnu_ret_val = 0;

	/* If we are dealing with a "return;" from an Ada procedure with
	   parameters passed by copy in copy out, we need to return a record
	   containing the final values of these parameters.  If the list
	   contains only one entry, return just that entry.

	   For a full description of the copy in copy out parameter mechanism,
	   see the part of the gnat_to_gnu_entity routine dealing with the
	   translation of subprograms. */
	if (TYPE_CI_CO_LIST (gnu_subprog_type) != NULL_TREE)
	  {
	    if (list_length (TYPE_CI_CO_LIST (gnu_subprog_type)) == 1)
	      gnu_ret_val = TREE_VALUE (TYPE_CI_CO_LIST (gnu_subprog_type));
	    else
	      gnu_ret_val
		= build_constructor (TREE_TYPE (gnu_subprog_type),
				     TYPE_CI_CO_LIST (gnu_subprog_type));
	  }

	/* If the Ada subprogram is a function, we just need to return the
	   expression.   If the subprogram returns an unconstrained
	   array, we have to allocate a new version of the result and
	   return it.  If we return by reference, return a pointer.  */

	else if (Present (Expression (gnat_node)))
	  {
	    gnu_ret_val  = gnat_to_gnu (Expression (gnat_node));

	    if (TYPE_RETURNS_BY_REF_P (gnu_subprog_type) 
		|| By_Ref (gnat_node))
	      gnu_ret_val = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_ret_val);

	    else if (TYPE_RETURNS_UNCONSTRAINED_P (gnu_subprog_type))
	      {
		gnu_ret_val = maybe_unconstrained_array (gnu_ret_val);
		gnu_ret_val
		  = build_allocator (TREE_TYPE (gnu_ret_val), gnu_ret_val,
				     TREE_TYPE (gnu_subprog_type),
				     Procedure_To_Call (gnat_node),
				     Storage_Pool (gnat_node));
	      }
  }

	set_lineno (gnat_node, 1);
	if (gnu_ret_val)
	  expand_return (build_binary_op (MODIFY_EXPR, NULL_TREE,
					  DECL_RESULT (current_function_decl),
					  gnu_ret_val));
	else
	  expand_null_return ();

      }
      break;

    case N_Goto_Statement:
      gnu_expr = gnat_to_gnu (Name (gnat_node));
      TREE_USED (gnu_expr) = 1;
      set_lineno (gnat_node, 1);
      expand_goto (gnu_expr);
      break;

    case N_Raise_Statement:

      set_lineno (gnat_node, 1);

      /* Normal raise statement */
      if (Present (Name (gnat_node)))
	expand_expr_stmt 
	  (build_call_1_expr 
	   (raise_decl, build_unary_op (ADDR_EXPR, NULL_TREE,
					gnat_to_gnu (Name (gnat_node)))));
      else
	/* Re-raise. */
        expand_expr_stmt
          (build_call_1_expr (raise_with_msg_decl, gnu_except_ptr_decl));

      break;

    /****************************/
    /* Chapter 6: Subprograms:  */
    /****************************/

    case N_Subprogram_Declaration:
      gnat_to_code (Specification (gnat_node));
      break;

    case N_Abstract_Subprogram_Declaration:
      break;

    case N_Function_Specification:
    case N_Procedure_Specification:

      /* Consider this a "definition" even though we won't actually be
	 making code for the subprogram here.  This is because if we
	 see the spec and are actually generating code, we know the body
	 must be in this same file.  */
      if (No (Freeze_Node (Defining_Unit_Name (gnat_node))))
	gnat_to_gnu_entity (Defining_Unit_Name (gnat_node), NULL_TREE, 1);

      break;

    case N_Defining_Program_Unit_Name:
      /* For a child unit identifier go up a level to get the
         specificaton.  We get this when we try to find the spec of
	 a child unit package that is the compilation unit being compiled. */
      gnat_to_code (Parent (gnat_node));
      break;

    case N_Subprogram_Body:
      {
	/* Definining identifier of a parameter to the subprogram.  */
        Entity_Id gnat_param;
        /* The declared entity currently being processed in the declarative
	   part of the subprogram body.  */
        Entity_Id gnat_entity;
	/* The defining identifier for the subprogram body. Note that if a
	   specification has appeared before for this body, then the identifier
	   occurring in that specification will also be a defining identifier
	   and all the calls to this subprogram will point to that
	   specification.  */
	Entity_Id gnat_subprog_id
	  = (Present (Corresponding_Spec (gnat_node))
	     ? Corresponding_Spec (gnat_node)
	     : Defining_Unit_Name (Specification (gnat_node)));

	/* The FUNCTION_DECL node corresponding to the subprogram spec.   */
	tree gnu_subprog_decl;
	/* The FUNCTION_TYPE node corresponding to the subprogram spec.  */
	tree gnu_subprog_type;
	tree gnu_cico_list;

        /* If the subprogram is a child unit,  retrieve simple name */
        gnat_subprog_id
           = (Nkind (gnat_subprog_id) == N_Defining_Program_Unit_Name
             ? Defining_Identifier (gnat_subprog_id)
             : gnat_subprog_id);

	/* If this is a generic object, ignore it.  */
	if (Ekind (gnat_subprog_id) == E_Generic_Procedure
	    || Ekind (gnat_subprog_id) == E_Generic_Function)
	  break;

	gnu_subprog_decl = gnat_to_gnu_entity (gnat_subprog_id, NULL_TREE, 1);
	gnu_subprog_type = TREE_TYPE (gnu_subprog_decl);

	/* Set the line number in the decl to correspond to that of
	   the body so that the line number notes are written 
	   correctly.  */
	set_lineno (gnat_node, 0);
	DECL_SOURCE_FILE (gnu_subprog_decl) = input_filename;
	DECL_SOURCE_LINE (gnu_subprog_decl) = lineno;

	begin_subprog_body (gnu_subprog_decl);
	set_lineno (gnat_node, 1);

	/* If an ABE check is needed, add one.  */
	if (present_gnu_tree (Parent (Declaration_Node (gnat_subprog_id))))
	  {
	    expand_start_cond
	      (build_binary_op
	       (EQ_EXPR, integer_type_node,
		get_gnu_tree (Parent (Declaration_Node (gnat_subprog_id))),
		integer_zero_node),
	       0);
	    expand_expr_stmt (build_call_0_expr (raise_program_error_decl));
	    expand_end_cond ();
	  }

	pushlevel (0);
	gnu_block_stack = tree_cons (NULL_TREE, NULL_TREE, gnu_block_stack);
  	expand_start_bindings (0);

	/* See if there are any parameters for which we don't yet have
	   GCC entities.  These must be for OUT parameters for which we
	   will be making VAR_DECL nodes here.  Fill them in to
	   TYPE_CI_CO_LIST, which must contain the empty entry as well.
	   We can match up the entries because TYPE_CI_CO_LIST is in the
	   order of the parameters.

	   If we make any new nodes here, make sure that they are in
	   the object that the function declaration's type is in because we
	   will be using them in the context of the caller.  */

	push_obstacks (TYPE_OBSTACK (gnu_subprog_type),
		       TYPE_OBSTACK (gnu_subprog_type));

	gnu_cico_list = TYPE_CI_CO_LIST (gnu_subprog_type);
	for (gnat_param = First_Formal (gnat_subprog_id);
	     Present (gnat_param);
	     gnat_param = Next_Formal_With_Extras (gnat_param))
	  if (! present_gnu_tree (gnat_param))
	    {
	      /* Skip any entries that have been already filled in; they
		 must correspond to IN OUT parameters.  */
	    for (; gnu_cico_list != 0 && TREE_VALUE (gnu_cico_list) != 0;
		 gnu_cico_list = TREE_CHAIN (gnu_cico_list))
	      ;

	    /* Do any needed references for padded types.  */
	    TREE_VALUE (gnu_cico_list)
	      = convert (TREE_TYPE (TREE_PURPOSE (gnu_cico_list)),
			 gnat_to_gnu_entity (gnat_param, NULL_TREE, 1));
	  }

	pop_obstacks ();

	process_decls (Declarations (gnat_node), Empty, Empty, 1, 1);

	/* Generate the code of the subprogram itself.  A return statement
	   will be present and any OUT parameters will be handled there.  */
	gnat_to_code (Handled_Statement_Sequence (gnat_node));

	expand_end_bindings (getdecls (), kept_level_p (), 0);
	poplevel (kept_level_p (), 1, 0);
	gnu_block_stack = TREE_CHAIN (gnu_block_stack);
	end_subprog_body ();

	/* Throw away any VAR_DECLs we made for OUT parameters; they must
	   not be seen when we call this function and will be in
	   unallocated memory anyway.  Also throw away DECL_RTL in
	   any PARM_DECLs unless this function was saved for inline, in
	   which case the DECL_RTLs are in preserved memory.  */
	for (gnat_param = First_Formal (gnat_subprog_id);
	     Present (gnat_param);
	     gnat_param = Next_Formal_With_Extras (gnat_param))
	  {
	    tree gnu_param = get_gnu_tree (gnat_param);

	    if (TREE_CODE (gnu_param) == VAR_DECL)
	      save_gnu_tree (gnat_param, NULL_TREE, 0);
	    else if (TREE_CODE (gnu_param) == PARM_DECL
		     && DECL_SAVED_INSNS (gnu_subprog_decl) == 0)
	      DECL_RTL (gnu_param) = DECL_INCOMING_RTL (gnu_param) = 0;
	  }

	/* Similarly, discard DECL_RTL of the return value.  */
	if (DECL_SAVED_INSNS (gnu_subprog_decl) == 0)
	  DECL_RTL (DECL_RESULT (gnu_subprog_decl))
	    = DECL_INCOMING_RTL (DECL_RESULT (gnu_subprog_decl)) = 0;
      }
      break;

    case N_Function_Call:
    case N_Procedure_Call_Statement:
      {
	/* The GCC node corresponding to the GNAT subprogram name.  This can
	   either be a FUNCTION_DECL node if we are dealing with a standard
	   subprogram call, or an indirect reference expression (an
	   INDIRECT_REF node) pointing to a subprogram.  */
	tree gnu_subprog_node = gnat_to_gnu (Name (gnat_node));
	/* The FUNCTION_TYPE node giving the GCC type of the subprogram.  */
	tree gnu_subprog_type = TREE_TYPE (gnu_subprog_node);
	tree gnu_subprog_addr
	  = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_subprog_node);
	Entity_Id gnat_formal;
	Node_Id gnat_actual;
	tree gnu_actual_list = NULL_TREE;
	tree gnu_name_list = NULL_TREE;
	tree gnu_subprog_call;

	if (TREE_CODE (gnu_subprog_type) != FUNCTION_TYPE)
	  gigi_abort (317);

	/* The only way we can be making a call via an access type is
	   if Name is an explicit dereference.  In that case, get the
	   list of formal args from the type the access type is pointing
	   to.  Otherwise, get the formals from entity being called.  */
	if (Nkind (Name (gnat_node)) == N_Explicit_Dereference)
	  gnat_formal = First_Formal (Etype (Name (gnat_node)));
	else if (Nkind (Name (gnat_node)) == N_Attribute_Reference)
	  /* Assume here that this must be 'Elab_Body or 'Elab_Spec.  */
	  gnat_formal = 0;
	else
	  gnat_formal = First_Formal (Entity (Name (gnat_node)));

	/* Create the list of the actual parameters as GCC expects it, namely
	   a chain of TREE_LIST nodes in which the TREE_VALUE field of each
	   node is a parameter-expression and the TREE_PURPOSE field is
	   null.  Skip OUT parameters that are not passed by reference.  */

        for (gnat_actual = First_Actual (gnat_node);
             Present (gnat_actual);
             gnat_formal = Next_Formal_With_Extras (gnat_formal),
             gnat_actual = Next_Actual (gnat_actual))
	  {
	    tree gnu_formal_type = gnat_to_gnu_type (Etype (gnat_formal));
	    Node_Id gnat_name
	      = ((Nkind (gnat_actual) == N_Unchecked_Type_Conversion)
		? Expression (gnat_actual) : gnat_actual);
	    tree gnu_name = gnat_to_gnu (gnat_name);
	    tree gnu_actual;

	    /* If it's possible we may need to use this expression twice,
	       make sure than any side-effects are handled via SAVE_EXPRs. 
	       Likewise if we need to force side-effects before the call. 
	       ??? This is more conservative than we need since we don't
	       need to do this for pass-by-ref with no conversion.  */
	    if (Ekind (gnat_formal) != E_In_Parameter)
	      gnu_name = gnat_stabilize_reference (gnu_name, 0);

	    gnu_actual = gnu_name;

	    if (Ekind (gnat_formal) != E_Out_Parameter
		&& Nkind (gnat_actual) != N_Unchecked_Type_Conversion
		&& Do_Range_Check (gnat_actual))
	      gnu_actual = emit_range_check (gnu_actual, Etype (gnat_formal));

	    /* Do any needed conversions.  We need only check for
	       unchecked conversion since normal conversions will be handled
	       by just converting to the formal type.  */
	    if (Nkind (gnat_actual) == N_Unchecked_Type_Conversion)
	      gnu_actual
		= unchecked_convert (gnat_to_gnu_type (Etype (gnat_actual)),
				     gnu_actual);

	    gnu_actual = convert (gnu_formal_type, gnu_actual);

	    /* If we have not saved a GCC object for the formal, it means
	       it is an OUT parameter not passed by reference.  Otherwise,
	       look at the PARM_DECL to see if it is passed by reference. */
	    if (present_gnu_tree (gnat_formal)
		&& DECL_BY_REF_P (get_gnu_tree (gnat_formal)))
	      {
		/* The symmetry of the paths to the type of an entity is
		   broken here since arguments don't know that they will
		   be passed by ref. */
		gnu_formal_type = TREE_TYPE (get_gnu_tree (gnat_formal));
		gnu_actual = build_unary_op (ADDR_EXPR, gnu_formal_type,
					     gnu_actual);
	      }
	    else if (present_gnu_tree (gnat_formal)
		     && DECL_BY_COMPONENT_PTR_P (get_gnu_tree (gnat_formal)))
	      {
		gnu_formal_type = TREE_TYPE (get_gnu_tree (gnat_formal));
		gnu_actual = maybe_implicit_deref (gnu_actual);
		gnu_actual = maybe_unconstrained_array (gnu_actual);

		if (TREE_CODE (gnu_formal_type) == RECORD_TYPE
		    && TYPE_IS_PADDING_P (gnu_formal_type))
		  {
		    gnu_formal_type
		      = TREE_TYPE (TYPE_FIELDS (gnu_formal_type));
		    gnu_actual = convert (gnu_formal_type, gnu_actual);
		  }

		/* Take the address of the object and convert to the
		   proper pointer type.  We'd like to actually compute
		   the address of the beginning of the array using 
		   an ADDR_EXPR of an ARRAY_REF, but there's a possibility
		   that the ARRAY_REF might return a constant and we'd
		   be getting the wrong address.  Neither approach is
		   exactly correct, but this is the most likely to work
		   in all cases.  */
		gnu_actual = convert (gnu_formal_type,
				      build_unary_op (ADDR_EXPR, NULL_TREE,
						      gnu_actual));
	      }
	    else if (present_gnu_tree (gnat_formal)
		     && DECL_BY_DESCRIPTOR_P (get_gnu_tree (gnat_formal)))
	      {
		/* If arg is 'Null_Parameter, pass zero descriptor.  */
		if (TREE_CODE (gnu_actual) == INDIRECT_REF
		    && TREE_PRIVATE (gnu_actual))
		  gnu_actual
		    = convert (DECL_ARG_TYPE (get_gnu_tree (gnat_formal)),
			       integer_zero_node);
		else
		  gnu_actual
		    = build_unary_op (ADDR_EXPR, NULL_TREE,
				      fill_vms_descriptor (gnu_actual,
							   gnat_formal));
	      }
	    else
	      {
		if (Ekind (gnat_formal) != E_In_Parameter)
		  gnu_name_list
		    = chainon (gnu_name_list,
			       build_tree_list (NULL_TREE, gnu_name));

		if (! present_gnu_tree (gnat_formal)
		    || TREE_CODE (get_gnu_tree (gnat_formal)) != PARM_DECL)
		  continue;

		/* If this is 'Null_Parameter, pass a zero even though we are
		   dereferencing it.  */
		if (TREE_CODE (gnu_actual) == INDIRECT_REF
		    && ! AGGREGATE_TYPE_P (TREE_TYPE (gnu_actual))
		    && TREE_PRIVATE (gnu_actual))
		  gnu_actual = integer_zero_node;

		gnu_actual
		  = convert (DECL_ARG_TYPE (get_gnu_tree (gnat_formal)),
			     gnu_actual);
	      }

	    gnu_actual_list
	      = chainon (gnu_actual_list,
			 build_tree_list (NULL_TREE, gnu_actual));
	  }

	gnu_subprog_call = build (CALL_EXPR, TREE_TYPE (gnu_subprog_type),
				  gnu_subprog_addr, gnu_actual_list,
				  NULL_TREE);
	TREE_SIDE_EFFECTS (gnu_subprog_call) = 1;

	/* If it is a function call, the result is the call expression.  */
	if (Nkind (gnat_node) == N_Function_Call)
	  {
	    gnu_result = gnu_subprog_call;

	    /* If the function returns an unconstrained array or by reference,
	       we have to de-dereference the pointer.  */
	    if (TYPE_RETURNS_UNCONSTRAINED_P (gnu_subprog_type)
		|| TYPE_RETURNS_BY_REF_P (gnu_subprog_type))
	      gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE,
					   gnu_result);

	    gnu_result_type = get_unpadded_type (Etype (gnat_node));
	    break;
	  }

	/* If this is the case where the GNAT tree contains a procedure call
	   but the Ada procedure has copy in copy out parameters, the special
	   parameter passing mechanism must be used.  */
	else if (TYPE_CI_CO_LIST (gnu_subprog_type) != NULL_TREE)
	  {
	    /* List of FIELD_DECLs associated with the PARM_DECLs of the copy
	       in copy out parameters.  */
	    tree scalar_return_list = TYPE_CI_CO_LIST (gnu_subprog_type);
	    int length = list_length (scalar_return_list);

	    if (length > 1)
	      {
		tree gnu_name;

		gnu_subprog_call = make_save_expr (gnu_subprog_call);

		/* If any of the names had side-effects, ensure they are
		   all evaluated before the call.  */
		for (gnu_name = gnu_name_list; gnu_name;
		     gnu_name = TREE_CHAIN (gnu_name))
		  if (TREE_SIDE_EFFECTS (TREE_VALUE (gnu_name)))
		    gnu_subprog_call
		      = build (COMPOUND_EXPR, TREE_TYPE (gnu_subprog_call),
			       TREE_VALUE (gnu_name), gnu_subprog_call);
	      }

	    if (Nkind (Name (gnat_node)) == N_Explicit_Dereference)
	      gnat_formal = First_Formal (Etype (Name (gnat_node)));
	    else
	      gnat_formal = First_Formal (Entity (Name (gnat_node)));

	    for (gnat_actual = First_Actual (gnat_node);
		 Present (gnat_actual);
		 gnat_formal = Next_Formal_With_Extras (gnat_formal),
		 gnat_actual = Next_Actual (gnat_actual))
	      /* If we are dealing with a copy in copy out parameter, we must
		 retrieve its value from the record returned in the function
		 call.  */
	      if (! (present_gnu_tree (gnat_formal)
		     && (DECL_BY_REF_P (get_gnu_tree (gnat_formal))
			 || (DECL_BY_COMPONENT_PTR_P 
			     (get_gnu_tree (gnat_formal)))
			 || DECL_BY_DESCRIPTOR_P (get_gnu_tree (gnat_formal))))
		  && Ekind (gnat_formal) != E_In_Parameter)
		{
		  /* Get the value to assign to this OUT or IN OUT
		     parameter.  It is either the result of the function if
		     there is only a single such parameter or the appropriate
		     field from the record returned.  */
		  tree gnu_result
		    = length == 1 ? gnu_subprog_call
		      : build_component_ref
			(gnu_subprog_call, NULL_TREE,
			 TREE_PURPOSE (scalar_return_list));
		  int unchecked_conversion
		    = Nkind (gnat_actual) == N_Unchecked_Type_Conversion;
		  /* If the actual is a conversion, get the inner expression,
		     which will be the real destination, and convert the
		     result to the type of the actual parameter.  */
		  tree gnu_actual
		    = maybe_unconstrained_array (TREE_VALUE (gnu_name_list));

		  /* If the result is a padded type, remove the padding.  */
		  if (TREE_CODE (TREE_TYPE (gnu_result)) == RECORD_TYPE
		      && TYPE_IS_PADDING_P (TREE_TYPE (gnu_result)))
		    gnu_result
		      = convert (TREE_TYPE (TYPE_FIELDS
					    (TREE_TYPE (gnu_result))),
				 gnu_result);

		  /* If the result is a type conversion, do it.  */
		  if (Nkind (gnat_actual) == N_Type_Conversion)
		    gnu_result
		      = convert_with_check
			(Etype (Expression (gnat_actual)), gnu_result,
			 Do_Overflow_Check (gnat_actual),
			 Do_Range_Check (Expression (gnat_actual)),
			 Float_Truncate (gnat_actual));

		  else if (unchecked_conversion)
		    gnu_result
		      = unchecked_convert (TREE_TYPE (gnu_actual), gnu_result);
		  else
		    {
		      if (Do_Range_Check (gnat_actual))
			gnu_result = emit_range_check (gnu_result,
						       Etype (gnat_actual));

		      if (! (! TREE_CONSTANT (TYPE_SIZE
					      (TREE_TYPE (gnu_actual)))
			     && TREE_CONSTANT (TYPE_SIZE
					       (TREE_TYPE (gnu_result)))))
			gnu_result = convert (TREE_TYPE (gnu_actual),
					      gnu_result);
		    }

		  set_lineno (gnat_node, 1);
		  expand_expr_stmt (build_binary_op (MODIFY_EXPR, NULL_TREE,
						     gnu_actual, gnu_result));
		  scalar_return_list = TREE_CHAIN (scalar_return_list);
		  gnu_name_list = TREE_CHAIN (gnu_name_list);
		}

	    break;
	  }

	set_lineno (gnat_node, 1);
	expand_expr_stmt (gnu_subprog_call);
      }
      break;

    /*************************/
    /* Chapter 7: Packages:  */
    /*************************/

    case N_Package_Declaration:
      /* The only time we actually see this node is if it is not the top-level
	 unit.  So just expand the specification.  */
      gnat_to_code (Specification (gnat_node));
      break;

    case N_Package_Specification:

      process_decls (Visible_Declarations (gnat_node),
		     Private_Declarations (gnat_node), Empty, 1, 1);
      break;

    case N_Package_Body:

      /* If this is the body of a generic package - do nothing */
      if (Ekind (Corresponding_Spec (gnat_node)) == E_Generic_Package)
	break;

      /* The only time we get here is if we are not processing this
	 package body at the top level.  So just process all declarations and
	 statements.  Package declarations have the same persistence as those
	 in the containing object, so don't push a binding level here.

	 ??? This means that declarations and statements will be intermixed,
	 which might be trouble when we deal with cleanups, but worry about
	 it then.  */
      process_decls (Declarations (gnat_node), Empty, Empty, 1, 1);

      /* If we are at the top level, record that we must generate code for
	 any statements that actually belong in the body.  Otherwise,
	 generate code now.  */
      if (Present (Handled_Statement_Sequence (gnat_node)))
	{
	  if (global_bindings_p ())
	    add_pending_elaborations
	      (NULL_TREE,
	       make_transform_expr (Handled_Statement_Sequence (gnat_node),
				    void_type_node));
	  else
	    {
	      gnu_block_stack
		= tree_cons (NULL_TREE, NULL_TREE, gnu_block_stack);
	      gnat_to_code (Handled_Statement_Sequence (gnat_node));
	      gnu_block_stack = TREE_CHAIN (gnu_block_stack);
	    }
	}

      break;

    /*********************************/
    /* Chapter 8: Visibility Rules:  */
    /*********************************/

    case N_Use_Package_Clause:
    case N_Use_Type_Clause:
      /* Nothing to do here - but these may appear in list of declarations */
      break;

    /***********************/
    /* Chapter 9: Tasks:   */
    /***********************/

    case N_Protected_Type_Declaration:
      break;

    case N_Single_Task_Declaration:
      gnat_to_gnu_entity (Defining_Identifier (gnat_node), NULL_TREE, 1);
      break;

    /***********************************************************/
    /* Chapter 10: Program Structure and Compilation Issues:   */
    /***********************************************************/

    case N_Compilation_Unit:
      /* If we have a package body, process and make an elaboration
	 routine for the spec and then do the same for the body.  */
      if (Nkind (Unit (gnat_node)) == N_Package_Body)
	{
	  gnat_to_code (Declaration_Node (Corresponding_Spec
					  (Unit (gnat_node))));

	  Set_Has_No_Elab_Code
	    (Library_Unit (gnat_node),
	     build_package_elab (Corresponding_Spec (Unit (gnat_node)), 0,
				 get_pending_elaborations (), 0));

	  process_inlined_subprograms (gnat_node);
	  process_decls (Declarations (Unit (gnat_node)), Empty, Empty, 1, 1);
	  Set_Has_No_Elab_Code
	    (gnat_node,
	     build_package_elab (Defining_Unit_Name (Unit (gnat_node)), 1,
				 get_pending_elaborations (),
				 Handled_Statement_Sequence
				 (Unit (gnat_node))));
	}

      /* If we have a package spec, handle the declarations and
	 elaboration function for it.  We can't do anything with any
	 inlined functions here due to order of elaboration concerns.  */
      else if (Nkind (Unit (gnat_node)) == N_Package_Declaration)
	{
	  Entity_Id gnat_spec = Specification (Unit (gnat_node));

	  gnat_to_code (gnat_spec);

	  Set_Has_No_Elab_Code
	    (gnat_node,
	     build_package_elab (Defining_Unit_Name (gnat_spec), 0,
				 get_pending_elaborations (), 0));
	}

      /* For library level subprogram bodies create elaboration functions for
	 spec and body, if needed.  */
      else if (Nkind (Unit (gnat_node)) == N_Subprogram_Body
	       && Present (Corresponding_Spec (Unit (gnat_node))))
	{
	  Entity_Id gnat_subprog = Corresponding_Spec (Unit (gnat_node));

	  process_inlined_subprograms (gnat_node);

	  if (Ekind (gnat_subprog) != E_Generic_Procedure
	      && Ekind (gnat_subprog) != E_Generic_Function
	      && Comes_From_Source (gnat_subprog)
	      && ! Suppress_Elaboration_Checks (gnat_subprog)
	      && ! Is_Intrinsic_Subprogram (gnat_subprog)
	      && ! Is_Imported (gnat_subprog))
	    save_gnu_tree (Parent (Declaration_Node (gnat_subprog)),
			   create_var_decl
			   (create_concat_name (gnat_subprog, "ABE"),
			    NULL_TREE, integer_type_node,
			    integer_zero_node, 0, 1, 0, 0, NULL_PTR),
			   1);

	  Set_Has_No_Elab_Code (Library_Unit (gnat_node),
				build_subprogram_elab (gnat_subprog, 0));

	  Set_Has_No_Elab_Code (gnat_node,
				build_subprogram_elab (gnat_subprog, 1));

	  gnat_to_code (Unit (gnat_node));
	}

      /* Otherwise, process whatever unit we are called with.  */
      else
	{
	  Set_Has_No_Elab_Code (gnat_node, 1);
	  process_inlined_subprograms (gnat_node);
	  gnat_to_code (Unit (gnat_node));
	}

      /* Process any pragmas following the unit.  */
      if (Present (Pragmas_After (gnat_node)))
	for (gnat_temp = First (Pragmas_After (gnat_node));
	     gnat_temp; gnat_temp = Next (gnat_temp))
	  gnat_to_code (gnat_temp);

      break;

    case N_Subprogram_Body_Stub:
    case N_Package_Body_Stub:
    case N_Protected_Body_Stub:
    case N_Task_Body_Stub:
      /* Simply process whatever unit is being inserted.  */
      gnat_to_code (Library_Unit (gnat_node));
      break;

    case N_Subunit:
      gnat_to_code (Proper_Body (gnat_node));
      break;

    /***************************/
    /* Chapter 11: Exceptions: */
    /***************************/

    case N_Handled_Sequence_Of_Statements:
      /* If there are exception handlers, start a new binding level that
	 we can exit (since each exception handler will do so).  Then
	 declare a variable to save the old __gnat_jmpbuf value and a
	 variable for our jmpbuf.  Call setjmp and handle each of the
	 possible exceptions if it returns one.

	 ??? We have a short-term kludge where we allow an N_Identifier
	 of a procedure to be in Identifier.  In that case, exception
	 handlers aren't allowed.  We make an exception handler that
	 calls the specified function and does a re-raise.  We also
	 put a call to that function as a cleanup action for that block.  */

      if (Present (Exception_Handlers (gnat_node))
	  || Present (Identifier (gnat_node)))
	{
	  tree gnu_jmpsave_decl;
	  tree gnu_jmpbuf_decl;
	  tree gnu_old_except_ptr_decl = gnu_except_ptr_decl;
	  tree gnu_cleanup_call = 0;
	  tree gnu_cleanup_decl;
	  int moment = suspend_momentary ();

	  pushlevel (0);
	  expand_start_bindings (1);

	  gnu_jmpsave_decl
	    = create_var_decl (get_identifier ("jmpbuf_save"), NULL_TREE,
			       jmpbuf_ptr_type,
			       build_call_0_expr (get_jmpbuf_decl), 0, 0, 0,
			       0, NULL_PTR);

	  gnu_jmpbuf_decl = create_var_decl (get_identifier ("jmp_buf"),
					     NULL_TREE, jmpbuf_type,
					     NULL_TREE, 0, 0, 0, 0, NULL_PTR);
	  TREE_VALUE (gnu_block_stack) = gnu_jmpbuf_decl;

	  /* See if we are to call a function when exiting this block.  */
	  if (Present (Identifier (gnat_node)))
	    {
	      gnu_cleanup_call
		= build_call_0_expr (gnat_to_gnu (Identifier (gnat_node)));

	      gnu_cleanup_decl
		= create_var_decl (get_identifier ("cleanup"), NULL_TREE,
				   integer_type_node, NULL_TREE, 0, 0, 0, 0,
				   NULL_PTR);

	      expand_decl_cleanup (gnu_cleanup_decl, gnu_cleanup_call);
	    }

	  /* When we exit this block, restore the saved value.  */
	  expand_decl_cleanup (gnu_jmpsave_decl,
			       build_call_1_expr (set_jmpbuf_decl,
						  gnu_jmpsave_decl));

	  resume_momentary (moment);

	  /* Call setjmp and handle exceptions if it returns one.  */
	  set_lineno (gnat_node, 1);
	  expand_start_cond
	    (build_call_1_expr (setjmp_decl,
				build_unary_op (ADDR_EXPR, NULL_TREE,
						gnu_jmpbuf_decl)),
	     0);

	  /* Restore our incoming longjmp value before we do anything.  */
	  expand_expr_stmt (build_call_1_expr (set_jmpbuf_decl,
					       gnu_jmpsave_decl));

	  /* If we have a cleanup to do, emit it now.  */
	  if (gnu_cleanup_call)
	    expand_expr_stmt (gnu_cleanup_call);

	  pushlevel (0);
	  expand_start_bindings (0);

	  gnu_except_ptr_decl
	    = create_var_decl (get_identifier ("except_ptr"), NULL_TREE,
			       build_pointer_type (except_type_node),
			       build_call_0_expr (get_excptr_decl),
			       0, 0, 0, 0, NULL_PTR);

	  /* Generate code for each exception handler.  The code at
	     N_Exception_Handler below does the real work.  */
	  if (Present (Exception_Handlers (gnat_node)))
	    for (gnat_temp =
	           First_Non_Pragma (Exception_Handlers (gnat_node));
		 Present (gnat_temp);
	         gnat_temp = Next_Non_Pragma (gnat_temp))
	      gnat_to_code (gnat_temp);

	  /* If none of the exception handlers did anything, re-raise
             but do not defer abortion.  */
	  set_lineno (gnat_node, 1);
	  expand_expr_stmt (build_call_1_expr (raise_nodefer_decl,
					       gnu_except_ptr_decl));

	  gnu_except_ptr_decl = gnu_old_except_ptr_decl;
	  expand_end_bindings (getdecls (), kept_level_p (), 0);
	  poplevel (kept_level_p (), 1, 0);

	  /* End the "if" on setjmp.  Note that we have arranged things so
	     control never returns here.  */
	  expand_end_cond ();

	  /* This is now immediately before the body proper.  Set
	     our jmp_buf as the current buffer.  */
	  expand_expr_stmt
	    (build_call_1_expr (set_jmpbuf_decl,
				build_unary_op (ADDR_EXPR, NULL_TREE,
						gnu_jmpbuf_decl)));
	}

      /* Generate code and declarations for the prefix of this block, 
	 if any.  */
      if (Present (First_Real_Statement (gnat_node)))
	process_decls (Statements (gnat_node), Empty,
		       First_Real_Statement (gnat_node), 1, 1);

      /* Generate code for each statement in the block.  */
      push_momentary ();
      for (gnat_temp = (Present (First_Real_Statement (gnat_node))
			? First_Real_Statement (gnat_node)
			: First (Statements (gnat_node)));
	   Present (gnat_temp); gnat_temp = Next (gnat_temp))
	{
	  gnat_to_code (gnat_temp);
	  clear_momentary ();
	}

      pop_momentary ();

      /* If we have handlers, close the block we made.  */
      if (Present (Identifier (gnat_node))
	  || Present (Exception_Handlers (gnat_node)))
	{
	  expand_end_bindings (getdecls (), kept_level_p (), 0);
	  poplevel (kept_level_p (), 1, 0);
	}

      break;

    case N_Exception_Handler:
      {
	/* Unless this is "Others", make an "if" statement to select
	   the proper exceptions.  For "Others", exclude exceptions whose
	   data block is nonzero; this is used for Abort.  */
	tree gnu_choice = integer_zero_node;

	for (gnat_temp = First (Exception_Choices (gnat_node));
	     gnat_temp; gnat_temp = Next (gnat_temp))
	  {
	    tree this_choice;

	    if (Nkind (gnat_temp) == N_Others_Choice)
	      this_choice
		= build_binary_op (EQ_EXPR, integer_type_node,
				   build_unary_op (INDIRECT_REF, NULL_TREE,
						   gnu_except_ptr_decl),
				   convert (char_type_node,
					    integer_zero_node));

	    else if (Nkind (gnat_temp) == N_Identifier
                     || Nkind (gnat_temp) == N_Expanded_Name)
	      this_choice
		= build_binary_op 
		  (EQ_EXPR, integer_type_node,
		   gnu_except_ptr_decl,
		   convert (TREE_TYPE (gnu_except_ptr_decl), 
			    build_unary_op (ADDR_EXPR, NULL_TREE,
					    gnat_to_gnu (gnat_temp))));

	    else
	      gigi_abort (318);

	    gnu_choice = build_binary_op (TRUTH_ORIF_EXPR, integer_type_node,
					  gnu_choice, this_choice);
	  }

	set_lineno (gnat_node, 1);
	expand_start_cond (gnu_choice, 0);

	/* Generate code for the exception handler.  If we have a
	   Choice_Parameter, make a block to put its definition in,
	   define it, and initialize to the exception occurrence. */
	if (Present (Choice_Parameter (gnat_node)))
	  {
	    tree gnu_except_occ;
	    tree gnu_except_id;
	    tree gnu_max_length;

	    pushlevel (0);
	    expand_start_bindings (0);

	    /* ??? Implementation of variable-length messages:
	       The type provided by the front-end for the choice_parameter is
	       the unconstrained type Exception_Occurrence, Gigi should 
	       constrain this type with the value of the actual message length
	       wich can be obtained by calling 
	       System.Task_Specific_Data.Get_Message_Length.  */

	    gnu_except_occ = gnat_to_gnu_entity (Choice_Parameter (gnat_node),
						 NULL_TREE, 1);

	    /* Max_Length is the first field in the exception occurrence.  */
	    gnu_max_length
	      = TYPE_FIELDS (get_unpadded_type
			     (Etype (Choice_Parameter (gnat_node))));

	    /* ??? Implementation of variable-length messages:
	       The discriminant Max_Length should be initialized with the 
	       actual message length (see previous comment). For now set it 
	       to the somewhat arbitrary value 200.  */

	    expand_expr_stmt 
	      (build_binary_op 
	       (MODIFY_EXPR, NULL_TREE,
		build (COMPONENT_REF, TREE_TYPE (gnu_max_length),
		       gnu_except_occ, gnu_max_length),
		convert (TREE_TYPE (gnu_max_length), build_int_2 (200, 0))));

	    /* The exception ID is the second field of the exception
	       occurrence. */
	    gnu_except_id = TREE_CHAIN (gnu_max_length);

	    /* The exception identity is copied to the exception
	       occurrence.  */
	    expand_expr_stmt 
	      (build_binary_op 
	       (MODIFY_EXPR, NULL_TREE,
		build (COMPONENT_REF, TREE_TYPE (gnu_except_id),
		       gnu_except_occ, gnu_except_id),
		convert (TREE_TYPE (gnu_except_id), gnu_except_ptr_decl)));

	    /* The exception message is copied in the exception occurrence 
	       via a library call.  */
	    expand_expr_stmt 
	      (build_call_1_expr (set_except_occ_decl, 
				  build_unary_op (ADDR_EXPR, 
						  NULL_TREE,
						  gnu_except_occ)));
	  }

	push_momentary ();
	for (gnat_temp = First (Statements (gnat_node));
	     gnat_temp; gnat_temp = Next (gnat_temp))
	  {
	    gnat_to_code (gnat_temp);
	    clear_momentary ();
	  }

	pop_momentary ();

	if (Present (Choice_Parameter (gnat_node)))
	  {
	    expand_end_bindings (getdecls (), 1, 0);
	    poplevel (1, 1, 0);
	  }

	/* At the end of the handler, exit the block.  We made this block
	   in N_Handled_Sequence_Of_Statements.  */
	expand_exit_something ();
	expand_end_cond ();
      }
      break;

    /*******************************/
    /* Chapter 12: Generic Units:  */
    /*******************************/

    case N_Generic_Function_Renaming_Declaration:
    case N_Generic_Package_Renaming_Declaration:
    case N_Generic_Procedure_Renaming_Declaration:
    case N_Generic_Package_Declaration:
    case N_Generic_Subprogram_Declaration:
    case N_Package_Instantiation:
    case N_Procedure_Instantiation:
    case N_Function_Instantiation:
      /* These nodes can appear on a declaration list but there is nothing to
	 to be done with them.  */
      break;


    /***************************************************/
    /* Chapter 13: Representation Clauses and	       */
    /*             Implementation-Dependent Features:  */
    /***************************************************/

    case N_Attribute_Definition_Clause:

      /* The only one we need deal with is for 'Address.  For the others, SEM
	 puts the information elsewhere.  We need only deal with 'Address
	 if the object has a Freeze_Node (which it never will currently).  */
      if (Get_Attribute_Id (Chars (gnat_node)) != Attr_Address
	  || No (Freeze_Node (Entity (Name (gnat_node)))))
	break;

      /* Get the value to use as the address and save it as the
	 equivalent for GNAT_TEMP.  When the object is frozen,
	 gnat_to_gnu_entity will do the right thing. */
      gnu_expr = gnat_to_gnu (Expression (gnat_node));
      save_gnu_tree (Entity (Name (gnat_node)), gnu_expr, 1);
      break;

    case N_Enumeration_Representation_Clause:
    case N_Record_Representation_Clause:
    case N_At_Clause:
      /* We do nothing with these.  SEM puts the information elsewhere.  */
      break;

    case N_Code_Statement:
      {
	tree gnu_template = gnat_to_gnu (Asm_Template (gnat_node));
	tree gnu_input_list = 0, gnu_output_list = 0, gnu_orig_out_list = 0;
	tree gnu_clobber_list = 0;
	char *clobber;

	/* First process inputs, then outputs, then clobbers.  */
	Setup_Asm_Inputs (gnat_node);
	while (Present (gnat_temp = Asm_Input_Value ()))
	  {
	    gnu_input_list = tree_cons (gnat_to_gnu (Asm_Input_Constraint ()),
					gnat_to_gnu (gnat_temp),
					gnu_input_list);
	    Next_Asm_Input ();
	  }

	Setup_Asm_Outputs (gnat_node);
	while (Present (gnat_temp = Asm_Output_Variable ()))
	  {
	    tree gnu_value = gnat_to_gnu (gnat_temp);
	    tree gnu_constr = gnat_to_gnu (Asm_Output_Constraint ());

	    gnu_orig_out_list
	      = tree_cons (gnu_constr, gnu_value, gnu_orig_out_list);
	    gnu_output_list
	      = tree_cons (gnu_constr, gnu_value, gnu_output_list);
	    Next_Asm_Output ();
	  }

	Clobber_Setup (gnat_node);
	while ((clobber = Clobber_Get_Next ()) != 0)
	  gnu_clobber_list
	    = tree_cons (NULL_TREE, 
			 build_string (strlen (clobber) + 1, clobber),
			 gnu_clobber_list);

	expand_asm_operands (gnu_template, nreverse (gnu_output_list),
			     nreverse (gnu_input_list), gnu_clobber_list,
			     Is_Asm_Volatile (gnat_node),
			     input_filename, lineno);

	/* Copy all the intermediate outputs into the specified outputs.  */
	for (; gnu_output_list;
	     (gnu_output_list = TREE_CHAIN (gnu_output_list),
	      gnu_orig_out_list = TREE_CHAIN (gnu_orig_out_list)))
	  if (TREE_VALUE (gnu_orig_out_list) != TREE_VALUE (gnu_output_list))
	    {
	      expand_expr_stmt
		(build_binary_op (MODIFY_EXPR, NULL_TREE,
				  TREE_VALUE (gnu_orig_out_list),
				  TREE_VALUE (gnu_output_list)));
	      free_temp_slots ();
	    }
      }
      break;

    /***************************************************/
    /* Added Nodes	                               */
    /***************************************************/

    case N_Freeze_Entity:
      process_freeze_entity (gnat_node);
      process_decls (Actions (gnat_node), Empty, Empty, 1, 1);
      break;

    case N_Itype_Reference:
      if (! present_gnu_tree (Itype (gnat_node)))
	process_type (Itype (gnat_node));
      break;

    case N_Free_Statement:
      {
	tree gnu_ptr = gnat_to_gnu (Expression (gnat_node));
	tree gnu_obj_size;
	int align;

	/* If this is an unconstrained array, we know the object must
	   have been allocated with the template in front of the object.
	   So pass the template address, but get the total size.  */
	if (TYPE_FAT_POINTER_P (TREE_TYPE (gnu_ptr)))
	  {
	    tree gnu_ptr_template_field
	      = TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (gnu_ptr)));
	    tree gnu_template_type
	      = TREE_TYPE (TREE_TYPE (gnu_ptr_template_field));
	    tree gnu_array_type
	      = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_ptr))));

	    gnu_obj_size = size_binop (PLUS_EXPR,
				       TYPE_SIZE (gnu_template_type),
				       TYPE_SIZE (gnu_array_type));
	    align = MAX (TYPE_ALIGN (gnu_template_type),
			 TYPE_ALIGN (gnu_array_type));

	    gnu_ptr = build_component_ref (gnu_ptr,  NULL_TREE,
					   gnu_ptr_template_field);
	  }
	else
	  {
	    tree gnu_obj_type = TREE_TYPE (TREE_TYPE (gnu_ptr));

	    gnu_obj_size = TYPE_SIZE (gnu_obj_type);
	    align = TYPE_ALIGN (gnu_obj_type);

	    if (TREE_CODE (gnu_obj_type) == RECORD_TYPE
		&& TYPE_CONTAINS_TEMPLATE_P (gnu_obj_type))
	      {
		tree gnu_char_ptr_type = build_pointer_type (char_type_node);
		tree gnu_pos
		  = DECL_FIELD_BITPOS (TYPE_FIELDS (gnu_obj_type));
		tree gnu_byte_offset
		  = convert (gnu_char_ptr_type,
			     size_binop (MINUS_EXPR, size_zero_node,
					 size_binop
					 (EXACT_DIV_EXPR, gnu_pos,
					  size_int (BITS_PER_UNIT))));

		gnu_ptr = convert (gnu_char_ptr_type, gnu_ptr);
		gnu_ptr = build_binary_op (MINUS_EXPR, gnu_char_ptr_type,
					   gnu_ptr, gnu_byte_offset);
	      }
	  }

	set_lineno (gnat_node, 1);
	expand_expr_stmt
	  (build_call_alloc_dealloc (gnu_ptr, gnu_obj_size, align,
				     Procedure_To_Call (gnat_node),
				     Storage_Pool (gnat_node)));
      }
      break;

    case N_Raise_Constraint_Error:
      gnu_result = build_call_0_expr (raise_constraint_error_decl);
      gnu_result_type = get_unpadded_type (Etype (gnat_node));

      /* If the type is VOID, this is a statement, so we need to 
	 generate the code for the call.  Handle a Condition, if there
	 is one.  */
      if (TREE_CODE (gnu_result_type) == VOID_TYPE)
	{
	  set_lineno (gnat_node, 1);

	  if (Present (Condition (gnat_node)))
	    expand_start_cond (gnat_to_gnu (Condition (gnat_node)), 0);

	  expand_expr_stmt (gnu_result);
	  if (Present (Condition (gnat_node)))
	    expand_end_cond ();
	  gnu_result = error_mark_node;
	}
      else
	gnu_result = build1 (NULL_EXPR, gnu_result_type, gnu_result);
      break;

    case N_Validate_Unchecked_Conversion:
      {
	tree gnu_in_type = gnat_to_gnu_type (Source_Type (gnat_node));
	tree gnu_out_type = gnat_to_gnu_type (Target_Type (gnat_node));

	if (TREE_CODE (gnu_in_type) != UNCONSTRAINED_ARRAY_TYPE
	    && TREE_CODE (TYPE_SIZE (gnu_in_type)) == INTEGER_CST
	    && TREE_CODE (gnu_out_type) != UNCONSTRAINED_ARRAY_TYPE
	    && TREE_CODE (TYPE_SIZE (gnu_out_type)) == INTEGER_CST
	    && ! operand_equal_p (rm_size (gnu_in_type),
				  rm_size (gnu_out_type), 0))
	  post_error ("types for unchecked conversion have different sizes?",
		      gnat_node);
	}
      break;

    case N_Op_Concat:
    case N_Component_Association:
    case N_Task_Body:
    default:
      gigi_abort (321);
    }

  /* Now convert the result to the proper type.  If the type is void or if
     we have no result, return error_mark_node to show we have no result.
     If the type of the result is correct or if we have a label (which doesn't
     have any well-defined type), return our result.  Also don't do the
     conversion if the "desired" type involves a PLACEHOLDER_EXPR in its size
     since those are the cases where the front end may have the type wrong due
     to "instantiating" the unconstrained record with discriminant values
     or if this is a FIELD_DECL.  If this is the Name of an assignment
     statement, return what we have since the RHS has to be converted
     to our type there in that case, unless GNU_RESULT_TYPE has a simpler
     size.  Otherwise, convert the result to the proper type.  */

  if (TREE_CODE (gnu_result) == LABEL_DECL
      || TREE_CODE (gnu_result) == FIELD_DECL
      || (TYPE_SIZE (gnu_result_type) != 0
	  && TREE_CODE (TYPE_SIZE (gnu_result_type)) != INTEGER_CST
	  && contains_placeholder_p (TYPE_SIZE (gnu_result_type)))
      || (Present (Parent (gnat_node))
	  && Nkind (Parent (gnat_node)) == N_Assignment_Statement
	  && Name (Parent (gnat_node)) == gnat_node
	  && ! (TYPE_SIZE (gnu_result_type) != 0
		&& TYPE_SIZE (TREE_TYPE (gnu_result)) != 0
		&& ((TREE_CODE (TYPE_SIZE (gnu_result_type)) == INTEGER_CST
		     && (TREE_CODE (TYPE_SIZE (TREE_TYPE (gnu_result)))
			 != INTEGER_CST))
		    || (TREE_CODE (TYPE_SIZE (gnu_result_type)) != INTEGER_CST
			&& (TREE_CODE (TYPE_SIZE (TREE_TYPE (gnu_result)))
			    != INTEGER_CST)
			&& ! (contains_placeholder_p
			      (TYPE_SIZE (gnu_result_type)))
			&& (contains_placeholder_p
			    (TYPE_SIZE (TREE_TYPE (gnu_result)))))))))
    {
      /* Remove any padding record, but do nothing more in this case.  */
      if (TREE_CODE (TREE_TYPE (gnu_result)) == RECORD_TYPE
	  && TYPE_IS_PADDING_P (TREE_TYPE (gnu_result)))
	gnu_result = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_result))),
			      gnu_result);
    }
  else if (gnu_result == error_mark_node
	   || gnu_result_type == void_type_node)
    gnu_result =  error_mark_node;
  else if (gnu_result_type != TREE_TYPE (gnu_result))
    gnu_result = convert (gnu_result_type, gnu_result);

  /* We don't need any NOP_EXPR or NON_LVALUE_EXPR on GNU_RESULT.  */
  while ((TREE_CODE (gnu_result) == NOP_EXPR
	  || TREE_CODE (gnu_result) == NON_LVALUE_EXPR)
	 && TREE_TYPE (TREE_OPERAND (gnu_result, 0)) == TREE_TYPE (gnu_result))
    gnu_result = TREE_OPERAND (gnu_result, 0);

  /* If our result has side-effects and is of an unconstrained type,
     make a SAVE_EXPR so that we can be sure it will only be referenced
     once.  */
  if (TREE_SIDE_EFFECTS (gnu_result)
      && (TREE_CODE (gnu_result_type) == UNCONSTRAINED_ARRAY_TYPE
	  || (TREE_CODE (TYPE_SIZE (gnu_result_type)) != INTEGER_CST
	      && contains_placeholder_p (TYPE_SIZE (gnu_result_type)))))
    gnu_result = gnat_stabilize_reference (gnu_result, 0);

  return gnu_result;
}

/* Do the processing of N_Freeze_Entity, GNAT_NODE.  */

static void
process_freeze_entity (gnat_node)
     Node_Id gnat_node;
{
  Entity_Id gnat_entity = Entity (gnat_node);
  tree gnu_old;
  tree gnu_new;
  tree gnu_init
    = (Nkind (Declaration_Node (gnat_entity)) == N_Object_Declaration
       && present_gnu_tree (Declaration_Node (gnat_entity)))
    ? get_gnu_tree (Declaration_Node (gnat_entity)) : NULL_TREE;

  /* If this is a package, need to generate code for the package.  */
  if (Ekind (gnat_entity) == E_Package)
    {
      insert_code_for
	(Parent (Corresponding_Body
		 (Parent (Declaration_Node (gnat_entity)))));
      return;
    }

  /* Check for old definition after the above call.  This Freeze_Node
     might be for one its Itypes.  */
  gnu_old
    = present_gnu_tree (gnat_entity) ? get_gnu_tree (gnat_entity) : 0;

  /* If this entity has an Address representation clause, GNU_OLD is the
     address, so discard it here.  */
  if (Present (Address_Clause (gnat_entity)))
    gnu_old = 0;

  /* Don't do anything for class-wide types they are always transformed
     into their root type.  */
  if (Is_Class_Wide_Type (gnat_entity))
    return;

  /* If we have a non-dummy type old tree, we have nothing to do.   Unless
     this is the public view of a private type whose full view was not
     delayed, this node was never delayed as it should have been.  */
  if (gnu_old != 0
      && ! (TREE_CODE (gnu_old) == TYPE_DECL
	    && TYPE_IS_DUMMY_P (TREE_TYPE (gnu_old))))
    {
      if (IN (Ekind (gnat_entity), Incomplete_Or_Private_Kind)
	  && Present (Full_View (gnat_entity))
	  && No (Freeze_Node (Full_View (gnat_entity))))
	return;
      else
	{
	  /* ??? For now, don't abort if this is a Concurrent_Type.  */
	  if (Is_Concurrent_Type (gnat_entity))
	    return;

	  gigi_abort (320);
	}
    }

  /* Reset the saved tree, if any, and elaborate the object or type for real.
     If there is a full declaration, elaborate it and copy the type to
     GNAT_ENTITY.  */
  if (gnu_old != 0)
    {
      save_gnu_tree (gnat_entity, NULL_TREE, 0);
      if (IN (Ekind (gnat_entity), Incomplete_Or_Private_Kind)
	  && Present (Full_View (gnat_entity))
	  && present_gnu_tree (Full_View (gnat_entity)))
	save_gnu_tree (Full_View (gnat_entity), NULL_TREE, 0);
    }

  if (IN (Ekind (gnat_entity), Incomplete_Or_Private_Kind)
      && Present (Full_View (gnat_entity)))
    {
      gnu_new = gnat_to_gnu_entity (Full_View (gnat_entity), NULL_TREE, 1);
      save_gnu_tree (gnat_entity, gnu_new, 0);
    }
  else
    gnu_new = gnat_to_gnu_entity (gnat_entity, gnu_init, 1);

  /* If we've made any pointers to the old version of this type, we have
     to update them.  */
  if (gnu_old != 0)
    update_pointer_to (TREE_TYPE (gnu_old), TREE_TYPE (gnu_new));
}

/* Process the list of inlined subprograms of GNAT_NODE, which is an
   N_Compilation_Unit.  */

static void
process_inlined_subprograms (gnat_node)
     Node_Id gnat_node;
{
  Entity_Id gnat_entity;
  Node_Id gnat_body;

  /* If we can inline, generate RTL for all the inlined subprograms.
     Define the entity first so we set DECL_EXTERNAL.  */
  if (optimize > 0 && ! flag_no_inline)
    for (gnat_entity = First_Inlined_Subprogram (gnat_node);
	 Present (gnat_entity);
	 gnat_entity = Next_Inlined_Subprogram (gnat_entity))
      {
	gnat_body = Parent (Declaration_Node (gnat_entity));

	if (Nkind (gnat_body) != N_Subprogram_Body)
	  {
	    /* ??? This really should always be Present.  */
	    if (No (Corresponding_Body (gnat_body)))
	      continue;

	    gnat_body
	      = Parent (Declaration_Node (Corresponding_Body (gnat_body)));
	  }

	if (Present (gnat_body))
	  {
	    gnat_to_gnu_entity (gnat_entity, NULL_TREE, 0);
	    gnat_to_code (gnat_body);
	  }
      }
}

/* Elaborate decls in the lists GNAT_DECLS and GNAT_DECLS2, if present.
   We make two passes, one to elaborate anything other than bodies (but
   we declare a function if there was no spec).  The second pass
   elaborates the bodies.

   GNAT_END_LIST gives the element in the list past the end.  Normally,
   this is Empty, but can be First_Real_Statement for a
   Handled_Sequence_Of_Statements.

   We make a complete pass through both lists if PASS1P is true, then make
   the second pass over both lists if PASS2P is true.  The lists usually
   correspond to the public and private parts of a package.  */

static void
process_decls (gnat_decls, gnat_decls2, gnat_end_list, pass1p, pass2p)
     List_Id gnat_decls, gnat_decls2;
     Node_Id gnat_end_list;
     int pass1p, pass2p;
{
  List_Id gnat_decl_array[2];
  Node_Id gnat_decl;
  int i;

  gnat_decl_array[0] = gnat_decls, gnat_decl_array[1] = gnat_decls2;

  if (pass1p)
    for (i = 0; i <= 1; i++)
      if (Present (gnat_decl_array[i]))
	for (gnat_decl = First (gnat_decl_array[i]);
	     gnat_decl != gnat_end_list; gnat_decl = Next (gnat_decl))
	  {
	    set_lineno (gnat_decl, 0);

	    /* For package specs, we recurse inside the declarations,
	       thus taking the two pass approach inside the boundary.  */
	    if (Nkind (gnat_decl) == N_Package_Declaration
		&& (Nkind (Specification (gnat_decl)
			   == N_Package_Specification)))
	      process_decls (Visible_Declarations (Specification (gnat_decl)),
			     Private_Declarations (Specification (gnat_decl)),
			     Empty, 1, 0);

	    /* Similarly for any declarations in the actions of a
	       freeze node.  */
	    else if (Nkind (gnat_decl) == N_Freeze_Entity)
	      {
		process_freeze_entity (gnat_decl);
		process_decls (Actions (gnat_decl), Empty, Empty, 1, 0);
	      }

	    /* Package bodies with freeze nodes get their elaboration deferred
	       until the freeze node, but the code must be placed in the right
	       place, so record the code position now.  */
	    else if (Nkind (gnat_decl) == N_Package_Body
		     && Present (Freeze_Node (Corresponding_Spec (gnat_decl))))
	      record_code_position (gnat_decl);

	    /* For subprogram specs, we define a variable for ABE processing
	       if the subprogram comes from source and the check has not
	       been disabled.  */
	    else if (Nkind (gnat_decl) == N_Subprogram_Declaration)
	      {
		Entity_Id gnat_subprog
		  = Defining_Unit_Name (Specification (gnat_decl));

		if (Ekind (gnat_subprog) != E_Generic_Procedure
		    && Ekind (gnat_subprog) != E_Generic_Function
		    && Comes_From_Source (gnat_subprog)
		    && ! Suppress_Elaboration_Checks (gnat_subprog)
		    && ! Is_Intrinsic_Subprogram (gnat_subprog)
		    && ! Is_Imported (gnat_subprog))
		  save_gnu_tree (gnat_decl,
				 create_var_decl
				 (create_concat_name (gnat_subprog, "ABE"),
				  NULL_TREE, integer_type_node,
				  integer_zero_node, 0, 1, 0, 0, NULL_PTR),
				 1);

		gnat_to_code (gnat_decl);
	      }

	    /* We defer most subprogram bodies to the second pass.
	       However, Init_Proc subprograms cannot be defered, but luckily
	       don't need to be.  */
	    else if ((Nkind (gnat_decl) == N_Subprogram_Body
		      && ! ((Nkind (Defining_Unit_Name
				    (Specification (gnat_decl)))
			     == N_Defining_Identifier)
			    && (Chars (Defining_Unit_Name
				       (Specification (gnat_decl)))
				== Name_uInit_Proc)))
		     || Nkind (gnat_decl) == N_Subprogram_Body_Stub)
	      {
		Node_Id gnat_real_decl = gnat_decl;

		if (Nkind (gnat_real_decl) == N_Subprogram_Body_Stub)
		  gnat_real_decl
		    = Proper_Body (Unit (Library_Unit (gnat_real_decl)));

		if (No (Corresponding_Spec (gnat_real_decl)))
		  {
		    Node_Id gnat_subprog_id
		      = Defining_Unit_Name (Specification (gnat_real_decl));

		    if ((Nkind (gnat_subprog_id)
			 == N_Defining_Program_Unit_Name))
		      gnat_subprog_id = Defining_Identifier (gnat_subprog_id);

		    if (Ekind (gnat_subprog_id) != E_Generic_Procedure
			&& Ekind (gnat_subprog_id) != E_Generic_Function)
		      gnat_to_gnu_entity (gnat_subprog_id, NULL_TREE, 1);
		  }
		else if (present_gnu_tree
			 (Parent (Declaration_Node (Corresponding_Spec
						    (gnat_real_decl)))))
		  {
		    tree gnu_abevar
		      = get_gnu_tree
		      (Parent (Declaration_Node (Corresponding_Spec
						 (gnat_real_decl))));

		    if (global_bindings_p ())
		      add_pending_elaborations (gnu_abevar, integer_one_node);
		    else
		      expand_expr_stmt
			(build_binary_op (MODIFY_EXPR, integer_type_node,
					  gnu_abevar, integer_one_node));
		  }
	      }
	    else
	      gnat_to_code (gnat_decl);
	  }

  /* Here we elaborate everything we deferred above except for package bodies,
     which are elaborated at their freeze nodes.  Note that we must also
     go inside things (package specs and freeze nodes) the first pass did.  */
  if (pass2p)
    for (i = 0; i <= 1; i++)
      if (Present (gnat_decl_array[i]))
	for (gnat_decl = First (gnat_decl_array[i]);
	     gnat_decl != gnat_end_list; gnat_decl = Next (gnat_decl))
	  {
	    if ((Nkind (gnat_decl) == N_Subprogram_Body
		 && ! ((Nkind (Defining_Unit_Name (Specification (gnat_decl)))
			== N_Defining_Identifier)
		       && (Chars (Defining_Unit_Name
				  (Specification (gnat_decl)))
			   == Name_uInit_Proc)))
		|| Nkind (gnat_decl) == N_Subprogram_Body_Stub)
	      gnat_to_code (gnat_decl);

	    else if (Nkind (gnat_decl) == N_Package_Declaration
		     && (Nkind (Specification (gnat_decl)
				== N_Package_Specification)))
	      process_decls (Visible_Declarations (Specification (gnat_decl)),
			     Private_Declarations (Specification (gnat_decl)),
			     Empty, 0, 1);

	    else if (Nkind (gnat_decl) == N_Freeze_Entity)
	      process_decls (Actions (gnat_decl), Empty, Empty, 0, 1);
	  }
}

/* Emits an access check. GNU_EXPR is the expression that needs to be
   checked against the NULL pointer. */

static tree
emit_access_check (gnu_expr)
     tree gnu_expr;
{
  tree gnu_type = TREE_TYPE (gnu_expr);

  /* This only makes sense if GNU_TYPE is a pointer of some sort.  */
  if (TREE_CODE (gnu_type) != POINTER_TYPE
      && ! TYPE_FAT_POINTER_P (gnu_type))
    gigi_abort (322);

  /* Checked expressions must be evaluated only once. */
  gnu_expr = make_save_expr (gnu_expr);

  return emit_check (build_binary_op (EQ_EXPR, integer_type_node,
				      gnu_expr,
				      convert (TREE_TYPE (gnu_expr),
					       integer_zero_node)),
		     gnu_expr);
}

/* Emits a discriminant check. GNU_EXPR is the expression to be checked and
   GNAT_NODE a N_Selected_Component node. */

static tree
emit_discriminant_check (gnu_expr, gnat_node)
     tree gnu_expr;
     Node_Id gnat_node;
{
  Entity_Id gnat_discr_fct
    = Discriminant_Checking_Func (Original_Record_Component 
				  (Entity (Selector_Name (gnat_node))));
  tree gnu_discr_fct;
  Entity_Id gnat_discr;
  tree gnu_actual_list = NULL_TREE;
  tree gnu_cond;
  Entity_Id gnat_pref_type = Etype (Prefix (gnat_node));

  if (! Present (gnat_discr_fct))
    return gnu_expr;

  gnu_discr_fct = gnat_to_gnu_entity (gnat_discr_fct, NULL_TREE, 0);

  /* Checked expressions must be evaluated only once. */
  gnu_expr = make_save_expr (gnu_expr);

  /* Create the list of the actual parameters as GCC expects it.
     This list is the list of the discriminant fields of the
     record expression to be discriminant checked. For documentation
     on what is the GCC format for this list see under the
     N_Function_Call case */

 while (IN (Ekind (gnat_pref_type), Incomplete_Or_Private_Kind)
	|| IN (Ekind (gnat_pref_type), Access_Kind))
   {
     if (IN (Ekind (gnat_pref_type), Incomplete_Or_Private_Kind)) 
       gnat_pref_type = Underlying_Type (gnat_pref_type);
     else if (IN (Ekind (gnat_pref_type), Access_Kind))
       gnat_pref_type = Designated_Type (gnat_pref_type);
   }

  for (gnat_discr = First_Discriminant (gnat_pref_type);
       Present (gnat_discr); gnat_discr = Next_Discriminant (gnat_discr))
    {
      tree gnu_discr = gnat_to_gnu_entity (gnat_discr, NULL_TREE, 0);
      /* The IDENTIFIER_NODE whose name is that of the discriminant */
      tree gnu_discr_id_node
	= get_identifier (Get_Name_String (Chars (gnat_discr)));

      gnu_actual_list
	= chainon (gnu_actual_list,
		   build_tree_list (NULL_TREE,
				    build_component_ref (gnu_expr,
							 gnu_discr_id_node,
							 gnu_discr)));
    }

  gnu_cond = build (CALL_EXPR,
		    TREE_TYPE (TREE_TYPE (gnu_discr_fct)),
		    build_unary_op (ADDR_EXPR,
				    NULL_TREE,
				    gnu_discr_fct),
		    gnu_actual_list,
		    NULL_TREE);
  TREE_SIDE_EFFECTS (gnu_cond) = 1;

  return build_unary_op (INDIRECT_REF, NULL_TREE,
			 emit_check (gnu_cond,
				     build_unary_op (ADDR_EXPR, NULL_TREE,
						     gnu_expr)));
}

/* Emit code for a range check. GNU_EXPR is the expression to be checked,
   GNAT_RANGE_TYPE the gnat type or subtype containing the bounds against
   which we have to check. */

static tree
emit_range_check (gnu_expr, gnat_range_type)
     tree gnu_expr;
     Entity_Id gnat_range_type;
{
  tree gnu_range_type = gnat_to_gnu_type (gnat_range_type);
  tree gnu_low  = TYPE_MIN_VALUE (gnu_range_type);
  tree gnu_high = TYPE_MAX_VALUE (gnu_range_type);
  tree gnu_compare_type = get_base_type (TREE_TYPE (gnu_expr));

  /* If GNU_EXPR has an integral type that is narrower than GNU_RANGE_TYPE,
     we can't do anything since we might be truncating the bounds.  No
     check is needed in this case.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (gnu_expr))
      && (TYPE_PRECISION (gnu_compare_type)
	  < TYPE_PRECISION (get_base_type (gnu_range_type))))
    return gnu_expr;

  /* Checked expressions must be evaluated only once. */
  gnu_expr = make_save_expr (gnu_expr);

  /* There's no good type to use here, so we might as well use
     integer_type_node.  */
  return emit_check
    (build_binary_op (TRUTH_ORIF_EXPR, integer_type_node,
		      build_binary_op (LT_EXPR, integer_type_node,
				       convert (gnu_compare_type, gnu_expr),
				       convert (gnu_compare_type, gnu_low)),
		      build_binary_op (GT_EXPR, integer_type_node,
				       convert (gnu_compare_type, gnu_expr),
				       convert (gnu_compare_type,
						gnu_high))),
     gnu_expr);
}

/* Emit code for an index check. GNU_ARRAY_OBJECT is the array object
   which we are about to index, GNU_EXPR is the index expression to be
   checked, GNU_LOW and GNU_HIGH are the lower and upper bounds
   against which GNU_EXPR has to be checked. Note that for index
   checking we cannot use the emit_range_check function (although very
   similar code needs to be generated in both cases) since for index
   checking the array type against which we are checking the indeces
   may be unconstrained and consequently we need to retrieve the
   actual index bounds from the array object itself
   (GNU_ARRAY_OBJECT). The place where we need to do that is in
   subprograms having unconstrained array formal parameters */

static tree
emit_index_check (gnu_array_object, gnu_expr, gnu_low, gnu_high)
     tree gnu_array_object;
     tree gnu_expr;
     tree gnu_low;
     tree gnu_high;
{
  /* Checked expressions must be evaluated only once. */
  gnu_expr = make_save_expr (gnu_expr);

  /* If GNU_LOW or GNU_HIGH are a PLACEHOLDER_EXPR, qualify them by
     the object we are handling. */
  if (TREE_CODE (gnu_low) != INTEGER_CST && contains_placeholder_p (gnu_low))
    gnu_low = build (WITH_RECORD_EXPR, TREE_TYPE (gnu_low),
		     gnu_low, gnu_array_object);

  if (TREE_CODE (gnu_high) != INTEGER_CST && contains_placeholder_p (gnu_high))
    gnu_high = build (WITH_RECORD_EXPR, TREE_TYPE (gnu_high),
		      gnu_high, gnu_array_object);

  /* There's no good type to use here, so we might as well use
     integer_type_node.   */
  return emit_check
    (build_binary_op (TRUTH_ORIF_EXPR, integer_type_node,
		      build_binary_op (LT_EXPR, integer_type_node,
				       gnu_expr,
				       convert (TREE_TYPE (gnu_expr),
						gnu_low)),
		      build_binary_op (GT_EXPR, integer_type_node,
				       gnu_expr,
				       convert (TREE_TYPE (gnu_expr),
						gnu_high))),
     gnu_expr);
}

/* Given GNU_COND which contains the condition corresponding to an access,
   discriminant or range check, of value GNU_EXPR, build a COND_EXPR
   that returns GNU_EXPR if GNU_COND is false and raises a
   CONSTRAINT_ERROR if GNU_COND is true.  */

static tree
emit_check (gnu_cond, gnu_expr)
     tree gnu_cond;
     tree gnu_expr;
{
  tree gnu_call = build (CALL_EXPR, void_type_node,
			 build_unary_op (ADDR_EXPR, NULL_TREE,
					 raise_constraint_error_decl),
			 NULL_TREE, NULL_TREE);

  TREE_SIDE_EFFECTS (gnu_call) = 1;

  /* Use an outer COMPOUND_EXPR to make sure that GNU_EXPR will
     get evaluated in front of the comparison in case it ends
     up being a SAVE_EXPR.  Put the whole thing inside its own
     SAVE_EXPR do the inner SAVE_EXPR doesn't leak out.  */

  return make_save_expr (build (COMPOUND_EXPR, TREE_TYPE (gnu_expr), gnu_expr,
				fold (build (COND_EXPR, TREE_TYPE (gnu_expr),
					     gnu_cond,
					     build (COMPOUND_EXPR,
						    TREE_TYPE (gnu_expr),
						    gnu_call, gnu_expr),
					     gnu_expr))));
}

/* Return an expression that converts GNU_EXPR to GNAT_TYPE, doing
   overflow checks if OVERFLOW_P is nonzero and range checks if
   RANGE_P is nonzero.  GNAT_TYPE is known to be an integral type.
   If TRUNCATE_P is nonzero, do a float to integer conversion with
   truncation; otherwise round.  */

static tree
convert_with_check (gnat_type, gnu_expr, overflow_p, range_p, truncate_p)
     Entity_Id gnat_type;
     tree gnu_expr;
     int overflow_p;
     int range_p;
     int truncate_p;
{
  tree gnu_type = get_unpadded_type (gnat_type);
  tree gnu_in_type = TREE_TYPE (gnu_expr);
  tree gnu_in_basetype = get_base_type (gnu_in_type);
  tree gnu_base_type = get_base_type (gnu_type);
  tree gnu_ada_base_type = get_ada_base_type (gnu_type);
  tree gnu_in_lb = TYPE_MIN_VALUE (gnu_in_basetype);
  tree gnu_in_ub = TYPE_MAX_VALUE (gnu_in_basetype);
  tree gnu_out_lb = TYPE_MIN_VALUE (gnu_base_type);
  tree gnu_out_ub = TYPE_MAX_VALUE (gnu_base_type);
  tree gnu_result = gnu_expr;

  /* If we are not doing any checks, the output is an integral type, and
     the input is not a floating type, just do the conversion.  This
     shortcut is required to avoid problems with packed array types
     and simplifies code in all cases anyway.   */
  if (! range_p && ! overflow_p && INTEGRAL_TYPE_P (gnu_type)
      && ! FLOAT_TYPE_P (gnu_in_type))
    return convert (gnu_type, gnu_expr);

  /* First convert the expression to its base type.  This
     will never generate code, but makes the tests below much simpler. 
     But don't do this if converting from an integer type to an unconstrained
     array type since then we need to get the bounds from the original
     (unpacked) type.  */
  if (TREE_CODE (gnu_type) != UNCONSTRAINED_ARRAY_TYPE)
    gnu_result = convert (gnu_in_basetype, gnu_result);

  /* If overflow checks are requested,  we need to be sure the result will
     fit in the output base type.  But don't do this if the input
     is integer and the output floating-point.  */
  if (overflow_p
      && ! (FLOAT_TYPE_P (gnu_base_type) && INTEGRAL_TYPE_P (gnu_in_basetype)))
    {
      /* Ensure GNU_EXPR only gets evaluated once.  */
      tree gnu_input = make_save_expr (gnu_result);
      tree gnu_cond = integer_zero_node;

      /* Convert the lower bounds to signed types, so we're sure we're
	 comparing them properly.  Likewise, convert the upper bounds
	 to unsigned types.  */
      if (INTEGRAL_TYPE_P (gnu_in_basetype) && TREE_UNSIGNED (gnu_in_basetype))
	gnu_in_lb = convert (signed_type (gnu_in_basetype), gnu_in_lb);

      if (INTEGRAL_TYPE_P (gnu_in_basetype)
	  && ! TREE_UNSIGNED (gnu_in_basetype))
	gnu_in_ub = convert (unsigned_type (gnu_in_basetype), gnu_in_ub);

      if (INTEGRAL_TYPE_P (gnu_base_type) && TREE_UNSIGNED (gnu_base_type))
	gnu_out_lb = convert (signed_type (gnu_base_type), gnu_out_lb);

      if (INTEGRAL_TYPE_P (gnu_base_type) && ! TREE_UNSIGNED (gnu_base_type))
	gnu_out_ub = convert (unsigned_type (gnu_base_type), gnu_out_ub);

      /* Check each bound separately and only if the result bound
	 is tighter than the bound on the input type.  Note that all the
	 types are base types, so the bounds must be constant. Also,
	 the comparison is done in the base type of the input, which
	 always has the proper signedness.  First check for input
	 integer (which means output integer), output float (which means
	 both float), or mixed, in which case we always compare.  */
      if (INTEGRAL_TYPE_P (gnu_in_basetype)
	  ? tree_int_cst_lt (gnu_in_lb, gnu_out_lb)
	  : (FLOAT_TYPE_P (gnu_base_type)
	     ? REAL_VALUES_LESS (TREE_REAL_CST (gnu_in_lb),
				 TREE_REAL_CST (gnu_out_lb))
	     : 1))
	gnu_cond = build_binary_op (LT_EXPR, integer_type_node,
				    gnu_input, convert (gnu_in_basetype,
				    gnu_out_lb));

      if (INTEGRAL_TYPE_P (gnu_in_basetype)
	  ? tree_int_cst_lt (gnu_out_ub, gnu_in_ub)
	  : (FLOAT_TYPE_P (gnu_base_type)
	     ? REAL_VALUES_LESS (TREE_REAL_CST (gnu_out_ub),
				 TREE_REAL_CST (gnu_in_lb))
	     : 1))
	gnu_cond
	  = build_binary_op (TRUTH_ORIF_EXPR, integer_type_node, gnu_cond,
			     build_binary_op (GT_EXPR, integer_type_node,
					      gnu_input,
					      convert (gnu_in_basetype,
						       gnu_out_ub)));

      if (! integer_zerop (gnu_cond))
	gnu_result = emit_check (gnu_cond, gnu_input);
    }

  /* Now convert to the result base type.  If this is a non-truncating
     float-to-integer conversion, round.  */
  if (INTEGRAL_TYPE_P (gnu_ada_base_type) && FLOAT_TYPE_P (gnu_in_basetype)
      && ! truncate_p)
    {
      tree gnu_point_5 = build_real (gnu_in_basetype, dconstp5);
      tree gnu_minus_point_5 = build_real (gnu_in_basetype, dconstmp5);
      tree gnu_zero = convert (gnu_in_basetype, integer_zero_node);
      tree gnu_saved_result = save_expr (gnu_result);
      tree gnu_comp = build (GE_EXPR, integer_type_node,
			     gnu_saved_result, gnu_zero);
      tree gnu_adjust = build (COND_EXPR, gnu_in_basetype, gnu_comp,
			       gnu_point_5, gnu_minus_point_5);

      gnu_result
	= build (PLUS_EXPR, gnu_in_basetype, gnu_saved_result, gnu_adjust);
    }

  if (TREE_CODE (gnu_ada_base_type) == INTEGER_TYPE
      && TYPE_HAS_ACTUAL_BOUNDS_P (gnu_ada_base_type)
      && TREE_CODE (gnu_result) == UNCONSTRAINED_ARRAY_REF)
    gnu_result = unchecked_convert (gnu_ada_base_type, gnu_result);
  else
    gnu_result = convert (gnu_ada_base_type, gnu_result);

  /* Finally, do the range check if requested.  Note that if the
     result type is a modular type, the range check is actually
     an overflow check.  */

  if (range_p || (TYPE_MODULAR_P (gnu_base_type) && overflow_p))
    gnu_result = emit_range_check (gnu_result, gnat_type);

  return convert (gnu_type, gnu_result);
}

/* Do the processing for the declaration of a GNAT_ENTITY, a type.  If
   a separate Freeze node exists, delay the bulk of the processing.  Otherwise
   make a GCC type for GNAT_ENTITY and set up the correspondance.  */

void
process_type (gnat_entity)
     Entity_Id gnat_entity;
{
  tree gnu_old
    = present_gnu_tree (gnat_entity) ? get_gnu_tree (gnat_entity) : 0;
  tree gnu_new;

  /* If we are to delay elaboration of this type, just do any
     elaborations needed for expressions within the declaration and
     make a dummy type entry for this node and its Full_View (if
     any) in case something points to it.  Don't do this if it
     has already been done (the only way that can happen is if
     the private completion is also delayed).  */
  if (Present (Freeze_Node (gnat_entity))
      || (IN (Ekind (gnat_entity), Incomplete_Or_Private_Kind)
	  && Present (Full_View (gnat_entity))
	  && Freeze_Node (Full_View (gnat_entity))))
    {
      elaborate_entity (gnat_entity);

      if (gnu_old == 0)
        {
	  tree gnu_entity_name
	    = get_identifier (Get_Name_String (Chars (gnat_entity)));
	  tree gnu_type = make_dummy_type (gnat_entity);
	  tree gnu_decl
	    = create_type_decl (gnu_entity_name, gnu_type, NULL_PTR);

	  save_gnu_tree (gnat_entity, gnu_decl, 0);
	  if (IN (Ekind (gnat_entity), Incomplete_Or_Private_Kind)
	      && Present (Full_View (gnat_entity)))
	    save_gnu_tree (Full_View (gnat_entity), gnu_decl, 0);
	}

      return;
    }

  /* If we saved away a dummy type for this node it means that this
     made the type that corresponds to the full type of an incomplete
     type.  Clear that type for now and then update the type in the
     pointers.  */
  if (gnu_old != 0)
    {
      if (TREE_CODE (gnu_old) != TYPE_DECL
	  || ! TYPE_IS_DUMMY_P (TREE_TYPE (gnu_old)))
	gigi_abort (323);

      save_gnu_tree (gnat_entity, NULL_TREE, 0);
    }

  /* Now fully elaborate the type.  */
  gnu_new = gnat_to_gnu_entity (gnat_entity, NULL_TREE, 1);
  if (TREE_CODE (gnu_new) != TYPE_DECL)
    gigi_abort (324);

  /* If we have an old type and we've made pointers to this type,
     update those pointers.  */
  if (gnu_old != 0)
    update_pointer_to (TREE_TYPE (gnu_old), TREE_TYPE (gnu_new));

  /* If this is a record type corresponding to a task or protected type 
     that is a completion of an incomplete type, perform a similar update
     on the type.  */
  /* ??? Including protected types here is a guess. */

  if (IN (Ekind (gnat_entity), Record_Kind)
      && Is_Concurrent_Record_Type (gnat_entity)
      && present_gnu_tree (Corresponding_Concurrent_Type (gnat_entity)))
    {
      tree gnu_task_old
	= get_gnu_tree (Corresponding_Concurrent_Type (gnat_entity));

      save_gnu_tree (Corresponding_Concurrent_Type (gnat_entity),
		     NULL_TREE, 0);
      save_gnu_tree (Corresponding_Concurrent_Type (gnat_entity),
		     gnu_new, 0);

      update_pointer_to (TREE_TYPE (gnu_task_old), TREE_TYPE (gnu_new));
    }
}

/* GNAT_ASSOC is the front of the Component_Associations of an N_Aggregate.
   GNU_TYPE is the GCC type of the corresponding record. 

   Return a CONSTRUCTOR to build the record.  */

static tree
assoc_to_constructor (gnat_assoc, gnu_type)
     Node_Id gnat_assoc;
     tree gnu_type;
{
  tree gnu_field, gnu_list, gnu_result;

  /* We test for GNU_FIELD being empty in the case where a variant
     was the last thing since we don't take things off GNAT_ASSOC in
     that case.  We check GNAT_ASSOC in case we have a variant, but it
     has no fields.  */

  for (gnu_list = NULL_TREE; Present (gnat_assoc);
       gnat_assoc = Next (gnat_assoc))
    {
      Node_Id gnat_field = First (Choices (gnat_assoc));
      tree gnu_field = gnat_to_gnu (gnat_field);
      tree gnu_expr = gnat_to_gnu (Expression (gnat_assoc));

      /* The expander is supposed to put a single component selector name
	 in every record component association */
      if (Next (gnat_field))
	gigi_abort (328);

      /* Before assigning a value in an aggregate make sure range checks
	 are done if required.  Then convert to the type of the field.  */
      if (Do_Range_Check (Expression (gnat_assoc)))
	gnu_expr = emit_range_check (gnu_expr, Etype (gnat_field));

      gnu_expr = convert (TREE_TYPE (gnu_field), gnu_expr);

      /* Add the field and expression to the list.  */
      gnu_list = tree_cons (gnu_field, gnu_expr, gnu_list);
    }

  gnu_result = extract_values (gnu_list, gnu_type);

  /* Verify every enty in GNU_LIST was used.  */
  for (gnu_field = gnu_list; gnu_field; gnu_field = TREE_CHAIN (gnu_field))
    if (! TREE_ADDRESSABLE (gnu_field))
      gigi_abort (311);

  return gnu_result;
}

/* Builds a possibly nested constructor for array aggregates. GNAT_EXPR
   is the first element of an array aggregate. It may itself be an
   aggregate (an array or record aggregate). GNU_ARRAY_TYPE is the gnu type
   corresponding to the array aggregate. GNAT_COMPONENT_TYPE is the type
   of the array component. It is needed for range checking. */

static tree
pos_to_constructor (gnat_expr, gnu_array_type, gnat_component_type)
     Node_Id gnat_expr;
     tree gnu_array_type;
     Entity_Id gnat_component_type;
{
  tree gnu_expr;
  tree gnu_expr_list = NULL_TREE;

  for ( ; Present (gnat_expr); gnat_expr = Next (gnat_expr))
    {
      /* If the expression is itself an array aggregate then first build the
	 innermost constructor if it is part of our array (multi-dimensional
	 case).  */

      if (Nkind (gnat_expr) == N_Aggregate
	  && TREE_CODE (TREE_TYPE (gnu_array_type)) == ARRAY_TYPE
	  && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_array_type)))
	gnu_expr = pos_to_constructor (First (Expressions (gnat_expr)),
				       TREE_TYPE (gnu_array_type),
				       gnat_component_type);
      else
	{
	  gnu_expr = gnat_to_gnu (gnat_expr);

	  /* before assigning the element to the array make sure it is
	     in range */
	  if (Do_Range_Check (gnat_expr))
	    gnu_expr = emit_range_check (gnu_expr, gnat_component_type);
	}
      gnu_expr_list = tree_cons (NULL_TREE, gnu_expr, gnu_expr_list);
    }

  return build_constructor (gnu_array_type, nreverse (gnu_expr_list));
}

/* Subroutine of assoc_to_constructor: VALUES is a list of field associations,
   some of which are from RECORD_TYPE.  Return a CONSTRUCTOR consisting
   of the associations that are from RECORD_TYPE.  If we see an internal
   record, make a recursive call to fill it in as well.  */

static tree
extract_values (values, record_type)
     tree values;
     tree record_type;
{
  tree result = NULL_TREE;
  tree field, tem;

  for (field = TYPE_FIELDS (record_type); field; field = TREE_CHAIN (field))
    {
      tree value = 0;

      /* _Parent is an internal field, but may have values in the aggregate,
	 so check for values first.  */
      if ((tem = purpose_member (field, values)) != 0)
	{
	  value = TREE_VALUE (tem);
	  TREE_ADDRESSABLE (tem) = 1;
	}

      else if (DECL_INTERNAL_P (field))
	{
	  value = extract_values (values, TREE_TYPE (field));
	  if (TREE_CODE (value) == CONSTRUCTOR
	      && CONSTRUCTOR_ELTS (value) == 0)
	    value = 0;
	}
      else
	/* If we have a record subtype, the names will match, but not the
	   actual FIELD_DECLs.  */
	for (tem = values; tem; tem = TREE_CHAIN (tem))
	  if (DECL_NAME (TREE_PURPOSE (tem)) == DECL_NAME (field))
	    {
	      value = convert (TREE_TYPE (field), TREE_VALUE (tem));
	      TREE_ADDRESSABLE (tem) = 1;
	    }

      if (value == 0)
	continue;

      result = tree_cons (field, value, result);
    }

  return build_constructor (record_type, nreverse (result));
}

/* EXP is to be treated as an array or record.  Handle the cases when it is
   an access object and perform the required dereferences.  */

static tree
maybe_implicit_deref (exp)
     tree exp;
{
  /* If the type is a pointer, dereference it.  */

  if (TREE_CODE (TREE_TYPE (exp)) == POINTER_TYPE
      || TYPE_FAT_POINTER_P (TREE_TYPE (exp)))
    exp = build_unary_op (INDIRECT_REF, NULL_TREE, exp);

  /* If we got a padded type, remove it too.  */
  if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
      && TYPE_IS_PADDING_P (TREE_TYPE (exp)))
    exp = convert (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (exp))), exp);

  return exp;
}

/* Surround EXP with a SAVE_EXPR, but handle unconstrained objects specially
   since it doesn't make any sense to put them in a SAVE_EXPR.  */

tree
make_save_expr (exp)
     tree exp;
{
  tree type = TREE_TYPE (exp);

  /* If this is an aggregate type, take its address, make a
     SAVE_EXPR of that, then do the indirect reference.  Note that
     for an unconstrained array, the effect will be to make a SAVE_EXPR
     of the fat pointer.  */
  if (TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE
      || AGGREGATE_TYPE_P (type))
    return build_unary_op (INDIRECT_REF, type,
			   save_expr (build_unary_op (ADDR_EXPR,
						      NULL_TREE, exp)));
  /* Otherwise, just do the usual thing.  */
  return save_expr (exp);
}

/* This is equivalent to stabilize_reference in GCC's tree.c, but we know
   how to handle our new nodes and we take an extra argument that says 
   whether to force evaluation of everything.  */

tree
gnat_stabilize_reference (ref, force)
     tree ref;
     int force;
{
  register tree type = TREE_TYPE (ref);
  register enum tree_code code = TREE_CODE (ref);
  register tree result;

  switch (code)
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      /* No action is needed in this case.  */
      return ref;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
    case UNCHECKED_CONVERT_EXPR:
      result
	= build1 (code, type,
		  gnat_stabilize_reference (TREE_OPERAND (ref, 0), force));
      break;

    case INDIRECT_REF:
    case UNCONSTRAINED_ARRAY_REF:
      result = build1 (code, type,
		       gnat_stabilize_reference_1 (TREE_OPERAND (ref, 0),
						   force));
      break;

    case COMPONENT_REF:
      result = build (COMPONENT_REF, type,
		      gnat_stabilize_reference (TREE_OPERAND (ref, 0),
						force),
		      TREE_OPERAND (ref, 1));
      break;

    case BIT_FIELD_REF:
      result = build (BIT_FIELD_REF, type,
		      gnat_stabilize_reference (TREE_OPERAND (ref, 0), force),
		      gnat_stabilize_reference_1 (TREE_OPERAND (ref, 1),
						     force),
		      gnat_stabilize_reference_1 (TREE_OPERAND (ref, 2),
						  force));
      break;

    case ARRAY_REF:
      result = build (ARRAY_REF, type,
		      gnat_stabilize_reference (TREE_OPERAND (ref, 0), force),
		      gnat_stabilize_reference_1 (TREE_OPERAND (ref, 1),
						  force));
      break;

    case COMPOUND_EXPR:
      result = build (COMPOUND_EXPR, type,
		      gnat_stabilize_reference_1 (TREE_OPERAND (ref, 0),
						  force),
		      gnat_stabilize_reference (TREE_OPERAND (ref, 1),
						force));
      break;

    case RTL_EXPR:
      result = build1 (INDIRECT_REF, type,
		       save_expr (build1 (ADDR_EXPR,
					  build_pointer_type (type), ref)));
      break;

      /* If arg isn't a kind of lvalue we recognize, make no change.
	 Caller should recognize the error for an invalid lvalue.  */
    default:
      return ref;

    case ERROR_MARK:
      return error_mark_node;
    }

  TREE_READONLY (result) = TREE_READONLY (ref);
  return result;
    }

/* Similar to stabilize_reference_1 in tree.c, but supports an extra
   arg to force a SAVE_EXPR for everything.  */

static tree
gnat_stabilize_reference_1 (e, force)
     tree e;
     int force;
{
  register enum tree_code code = TREE_CODE (e);
  register tree type = TREE_TYPE (e);
  register tree result;

  /* We cannot ignore const expressions because it might be a reference
     to a const array but whose index contains side-effects.  But we can
     ignore things that are actual constant or that already have been
     handled by this function.  */

  if (TREE_CONSTANT (e) || code == SAVE_EXPR)
    return e;

  switch (TREE_CODE_CLASS (code))
    {
    case 'x':
    case 't':
    case 'd':
    case 'b':
    case '<':
    case 's':
    case 'e':
    case 'r':
      if (TREE_SIDE_EFFECTS (e) || force)
	return save_expr (e);
      return e;

    case 'c':
      /* Constants need no processing.  In fact, we should never reach
	 here.  */
      return e;

    case '2':
      /* Division is slow and tends to be compiled with jumps,
	 especially the division by powers of 2 that is often
	 found inside of an array reference.  So do it just once.  */
      if (code == TRUNC_DIV_EXPR || code == TRUNC_MOD_EXPR
	  || code == FLOOR_DIV_EXPR || code == FLOOR_MOD_EXPR
	  || code == CEIL_DIV_EXPR || code == CEIL_MOD_EXPR
	  || code == ROUND_DIV_EXPR || code == ROUND_MOD_EXPR)
	return save_expr (e);
      /* Recursively stabilize each operand.  */
      result = build (code, type,
		      gnat_stabilize_reference_1 (TREE_OPERAND (e, 0), force),
		      gnat_stabilize_reference_1 (TREE_OPERAND (e, 1), force));
      break;

    case '1':
      /* Recursively stabilize each operand.  */
      result = build1 (code, type,
		       gnat_stabilize_reference_1 (TREE_OPERAND (e, 0),
						   force));
      break;

    default:
      abort ();
    }

  TREE_READONLY (result) = TREE_READONLY (e);
  return result;
}

/* GNAT_UNIT is the Defining_Identifier for some package, either a
   spec or a body, BODY_P says which.  If needed, make a function to be the
   elaboration routine for that object and perform the elaborations
   in GNU_ELAB_LIST.

   Return 1 if we didn't need an elaboration function, zero otherwise.  */

static int
build_package_elab (gnat_unit, body_p, gnu_elab_list, gnat_statements)
     Entity_Id gnat_unit;
     int body_p;
     tree gnu_elab_list;
     Node_Id gnat_statements;
{
  tree gnu_decl;
  struct elab_list *elab;

  /* If we have nothing to do, return.  */
  if (gnu_elab_list == 0 && No (gnat_statements))
    return 1;

  /* Set our file and line number to that of the object and set up the
     elaboration routine.  */
  gnu_decl = create_subprog_decl (create_concat_name (gnat_unit,
						      body_p ?
						      "elabb" : "elabs"),
				  NULL_TREE, void_ftype, NULL_TREE, 0, 1, 0, 
				  NULL_PTR);

  begin_subprog_body (gnu_decl);
  set_lineno (gnat_unit, 1);
  pushlevel (0);
  gnu_block_stack = tree_cons (NULL_TREE, NULL_TREE, gnu_block_stack);
  expand_start_bindings (0);

  /* Emit the assignments for the elaborations we have to do.  If there
     is no destination, this is just a call to execute some statement
     that was placed within the declarative region.  */
  for (; gnu_elab_list; gnu_elab_list = TREE_CHAIN (gnu_elab_list))
    if (TREE_PURPOSE (gnu_elab_list) == NULL_TREE)
      expand_expr_stmt (TREE_VALUE (gnu_elab_list));
    else
      {
	input_filename = DECL_SOURCE_FILE (TREE_PURPOSE (gnu_elab_list));
	lineno = DECL_SOURCE_LINE (TREE_PURPOSE (gnu_elab_list));

	emit_line_note (input_filename, lineno);
	expand_expr_stmt (build_binary_op (MODIFY_EXPR, NULL_TREE,
					   TREE_PURPOSE (gnu_elab_list),
					   TREE_VALUE (gnu_elab_list)));
      }

  /* Now generate code for any actual statements in the body.  */
  if (Present (gnat_statements))
    gnat_to_code (gnat_statements);

  expand_end_bindings (getdecls (), 0, 0);
  poplevel (0, 0, 0);
  gnu_block_stack = TREE_CHAIN (gnu_block_stack);
  end_subprog_body ();

  return 0;
}

/* GNAT_UNIT is the Defining_Identifier for some subprogram.
   Make elaboration functions for the spec or body of it.
   Currently the function just returns -- eventually it may
   have some elaboration order checks in it.

   Return 1 if we don't need any elaboration functions, zero otherwise.  */

static int
build_subprogram_elab (gnat_unit, body_p)
     Entity_Id gnat_unit;
     int body_p;
{
  tree gnu_abevar;
  tree gnu_decl;

  /* We only have to do something if we need to set the ABE variable 
     to one.  */

  if (! body_p || ! present_gnu_tree (Parent (Declaration_Node (gnat_unit))))
    return 1;

  /* Set our file and line number to that of the object and set up the
     elaboration routine.  */
  gnu_decl
    = create_subprog_decl (create_concat_name (gnat_unit,
					       body_p ? "elabb" : "elabs"),
			   NULL_TREE, void_ftype, NULL_TREE,
			   0, 1, 0, NULL_PTR);
  begin_subprog_body (gnu_decl);
  set_lineno (gnat_unit, 1);
  pushlevel (0);
  gnu_block_stack = tree_cons (NULL_TREE, NULL_TREE, gnu_block_stack);
  expand_start_bindings (0);

  gnu_abevar = get_gnu_tree (Parent (Declaration_Node (gnat_unit)));
  expand_expr_stmt (build_binary_op (MODIFY_EXPR, integer_type_node,
				     gnu_abevar, integer_one_node));

  expand_end_bindings (getdecls (), 0, 0);
  poplevel (0, 0, 0);
  gnu_block_stack = TREE_CHAIN (gnu_block_stack);
  end_subprog_body ();

  return 0;
}

/* Define names for some functions in sinput.adb we call from here.  */

#define Full_Ref_Name		sinput__full_ref_name
#define Get_Source_File_Index	sinput__get_source_file_index
#define Get_Line_Number		sinput__get_line_number

/* Determine the input_filename and the lineno from the source location
   (Sloc) of GNAT_NODE node.  Set the global variable input_filename and
   lineno.  If WRITE_NOTE_P is true, emit a line number note.  */

void
set_lineno (gnat_node, write_note_p)
     Node_Id gnat_node;
     int write_note_p;
{
  Source_Ptr source_location = Sloc (gnat_node);

  /* If node not from source code, ignore.  */
  if (source_location < 0)
    return;

  input_filename
    = Get_Name_String
      (Full_Ref_Name (Get_Source_File_Index (source_location)));
  lineno = Get_Line_Number (source_location);

  if (write_note_p)
    emit_line_note (input_filename, lineno);
}

/* Post an error message.  MSG is the error message, properly annotated.
   NODE is the node at which to post the error and the node to use for the
   "&" substitution.  */

void
post_error (msg, node)
     char *msg;
     Node_Id node;
{
  struct template {int first, last; } temp = {1, strlen (msg)};
  struct fat_pointer { char *array; struct template *temp; } fp = {msg, &temp};

  if (Present (node))
    errout__error_msg_n (fp, node);
}

/* Similar, but NODE is the node at which to post the error and ENT
   is the node to use for the "&" substitution.  */

void
post_error_ne (msg, node, ent)
     char *msg;
     Node_Id node;
     Entity_Id ent;
{
  struct template {int first, last; } temp = {1, strlen (msg)};
  struct fat_pointer { char *array; struct template *temp; } fp = {msg, &temp};

  if (Present (node))
    errout__error_msg_ne (fp, node, ent);
}

extern Uint errout__error_msg_uint_1;

/* Similar, but NODE is the node at which to post the error, ENT is the node
   to use for the "&" substitution, and N is the number to use for the ^.  */

void
post_error_ne_num (msg, node, ent, n)
     char *msg;
     Node_Id node;
     Entity_Id ent;
     int n;
{
  struct template {int first, last; } temp = {1, strlen (msg)};
  struct fat_pointer { char *array; struct template *temp; } fp = {msg, &temp};

  errout__error_msg_uint_1 = UI_From_Int (n);
  if (Present (node))
    errout__error_msg_ne (fp, node, ent);
}

/* Similar to post_error_ne_num, but T is a GCC tree representing the number
   to write.  If the tree represents a constant that fits within a
   host integer, the text inside curly brackets in MSG will be output
   (presumably including a '^').  Otherwise that text will not be output
   and the text inside square brackets will be output instead.  */

void
post_error_ne_tree (msg, node, ent, t)
     char *msg;
     Node_Id node;
     Entity_Id ent;
     tree t;
{
  char *newmsg = alloca (strlen (msg));
  struct template {int first, last; } temp = {1, 0};
  struct fat_pointer { char *array; struct template *temp; } fp
    = {newmsg, &temp};
  char start_yes, end_yes, start_no, end_no;
  char *p, *q;

  if (TREE_CODE (t) == INTEGER_CST
      && ! TREE_CONSTANT_OVERFLOW (t)
      && TREE_INT_CST_HIGH (t) == 0
#if HOST_BITS_PER_WIDE_INT > HOST_BITS_PER_INT
      && TREE_INT_CST_LOW (t) < (1 << (HOST_BITS_PER_INT - 2))
#endif
      )
    {
      errout__error_msg_uint_1 = UI_From_Int (TREE_INT_CST_LOW (t));
      start_yes = '{', end_yes = '}', start_no = '[', end_no = ']';
    }
  else
    start_yes = '[', end_yes = ']', start_no = '{', end_no = '}';

  for (p = msg, q = newmsg; *p != 0; p++)
    {
      if (*p == start_yes)
	for (p++; *p != end_yes; p++)
	  *q++ = *p;
      else if (*p == start_no)
	for (p++; *p != end_no; p++)
	  ;
      else
	*q++ = *p;
    }

  *q = 0;

  temp.last = strlen (newmsg);
  if (Present (node))
    errout__error_msg_ne (fp, node, ent);
}

/* Signal abort, with "Gigi abort" as the error label, and error_gnat_node
   as the relevant node that provides the location info for the error */

void
gigi_abort (code)
     int code;
{
  struct template {int first, last; } temp = {1, 10};
  struct fat_pointer { char *array; struct template *temp; }
                                            fp = {"Gigi abort", &temp};
  extern comperr__compiler_abort PROTO((struct fat_pointer, int))
    __attribute__ ((noreturn));
  extern Node_Id debug__fatal_error_node;

  debug__fatal_error_node = error_gnat_node;
  comperr__compiler_abort (fp, code);
}

/* Initialize the table that maps GNAT codes to GCC codes for simple
   binary and unary operations.  */

void
init_code_table ()
{
  gnu_codes[N_And_Then] = TRUTH_ANDIF_EXPR;
  gnu_codes[N_Or_Else] = TRUTH_ORIF_EXPR;

  gnu_codes[N_Op_And] = TRUTH_AND_EXPR;
  gnu_codes[N_Op_Or] = TRUTH_OR_EXPR;
  gnu_codes[N_Op_Xor] = TRUTH_XOR_EXPR;
  gnu_codes[N_Op_Eq] = EQ_EXPR;
  gnu_codes[N_Op_Ne] = NE_EXPR;
  gnu_codes[N_Op_Lt] = LT_EXPR;
  gnu_codes[N_Op_Le] = LE_EXPR;
  gnu_codes[N_Op_Gt] = GT_EXPR;
  gnu_codes[N_Op_Ge] = GE_EXPR;
  gnu_codes[N_Op_Add] = PLUS_EXPR;
  gnu_codes[N_Op_Subtract] = MINUS_EXPR;
  gnu_codes[N_Op_Multiply] = MULT_EXPR;
  gnu_codes[N_Op_Mod] = FLOOR_MOD_EXPR;
  gnu_codes[N_Op_Rem] = TRUNC_MOD_EXPR;
  gnu_codes[N_Op_Expon] = EXPON_EXPR;
  gnu_codes[N_Op_Minus] = NEGATE_EXPR;
  gnu_codes[N_Op_Abs] = ABS_EXPR;
  gnu_codes[N_Op_Not] = TRUTH_NOT_EXPR;
  gnu_codes[N_Op_Rotate_Left] = LROTATE_EXPR;
  gnu_codes[N_Op_Rotate_Right] = RROTATE_EXPR;
  gnu_codes[N_Op_Shift_Left] = LSHIFT_EXPR;
  gnu_codes[N_Op_Shift_Right] = RSHIFT_EXPR;
  gnu_codes[N_Op_Shift_Right_Arithmetic] = RSHIFT_EXPR;
}
