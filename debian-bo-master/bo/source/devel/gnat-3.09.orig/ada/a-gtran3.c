/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             A - G T R A N 3                              */
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

#include <ctype.h>
#include "config.h"
#include "tree.h"
#include "obstack.h"
#include "flags.h"
#include "convert.h"

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
#include "a-trans.h"
#include "a-trans3.h"
#include "a-trans4.h"
#include "a-gtran3.h"
#include "a-misc.h"
#include "a-rtree.h"

/* Setting this to 1 suppresses hashing of types.  */
extern int debug_no_type_hash;

static struct attrib *build_attr_list	PROTO((Entity_Id));
static tree maybe_placeholder		PROTO((tree));
static tree elaborate_expression	PROTO((Node_Id, Entity_Id, tree,
					       int, int));
static tree gnat_to_gnu_field		PROTO((Entity_Id, tree, int));
static void components_to_record	PROTO((tree, Node_Id, tree, int,
					       int, tree *));
static int compare_field_bitpos		PROTO((tree *, tree *));
static void annotate_rep		PROTO((Entity_Id, tree));
static tree annotate_rep_1		PROTO((tree, tree, int));
static tree create_enum_initializer	PROTO((Entity_Id, tree));
static tree validate_size		PROTO((Uint, tree, Entity_Id,
					       enum tree_code, int));
static tree make_type_from_size		PROTO((tree, tree, int));
static int validate_alignment		PROTO((Node_Id, Entity_Id, int));
static void check_ok_for_atomic		PROTO((tree, Entity_Id));
static tree get_entity_name		PROTO((Entity_Id));
static void compute_qualified_name	PROTO((Entity_Id));

/* Given GNAT_ENTITY, an entity in the incoming GNAT tree, return a
   GCC type corresponding to that entity.  GNAT_ENTITY is assumed to
   refer to an Ada type.  */

tree
gnat_to_gnu_type (gnat_entity)
     Entity_Id gnat_entity;
{
  tree gnu_decl;

  /* Convert the ada entity type into a GCC TYPE_DECL node.  */
  gnu_decl = gnat_to_gnu_entity (gnat_entity, NULL_TREE, 0);
  if (TREE_CODE (gnu_decl) != TYPE_DECL)
    gigi_abort (101);

  return TREE_TYPE (gnu_decl);
}

/* These two variables are used to defer recursively expanding incomplete
   types while we are processing a record or subprogram type.  */

static int defer_incomplete_level = 0;
static struct incomplete
{
  struct incomplete *next;
  tree old_type;
  Entity_Id full_type;
} *defer_incomplete_list = 0;

/* Given GNAT_ENTITY, a GNAT defining identifier node, which denotes some Ada
   entity, this routine returns the equivalent GCC tree for that entity
   (an ..._DECL node) and associates the ..._DECL node with the input GNAT
   defining identifier.

   If GNAT_ENTITY is a variable or a constant declaration, GNU_EXPR gives its
   initial value (in GCC tree form). This is optional for variables.
   For renamed entities, GNU_EXPR gives the object being renamed.

   DEFINITION is nonzero if this call is intended for a definition.  This is
   used for separate compilation where it necessary to know whether an
   external declaration or a definition should be created if the GCC equivalent
   was not created previously.  The value of 1 is normally used for a non-zero
   DEFINITION, but a value of 2 is used in special circumstances, defined in
   the code.  */

tree
gnat_to_gnu_entity (gnat_entity, gnu_expr, definition)
     Entity_Id gnat_entity;
     tree gnu_expr;
     int definition;
{
  tree gnu_entity_id;
  tree gnu_type;
  /* Contains the gnu XXXX_DECL tree node which is equivalent to the input
     GNAT tree. This node will be associated with the GNAT node by calling
     the save_gnu_tree routine at the end of the `switch' statement.  */
  tree gnu_decl = 0;
  /* Nonzero if we have already saved gnu_decl as a gnat association.  */
  int saved = 0;
  /* Nonzero if we were already in permanent allocation.  */
  int was_permanent = ! allocation_temporary_p ();
  /* Nonzero if we were in momentary allocation.  */
  int was_momentary;
  /* Nonzero if we incremented defer_incomplete_level.  */
  int this_deferred = 0;
  /* Nonzero if we should check to see if elaborated during processing.  */
  int maybe_present = 0;
  /* Nonzero if we made GNU_DECL and its type here.  */
  int this_made_decl = 0;
  struct attrib *attr_list = 0;
  Entity_Kind kind = Ekind (gnat_entity);
  Entity_Id gnat_temp;
  int esize
    = ((Present (Esize (gnat_entity))
	&& UI_Is_In_Int_Range (Esize (gnat_entity)))
       ? MIN (UI_To_Int (Esize (gnat_entity)),
	      IN (kind, Float_Kind)
	      ? LONG_DOUBLE_TYPE_SIZE
	      : IN (kind, Access_Kind) ? POINTER_SIZE * 2
	      : LONG_LONG_TYPE_SIZE)
       : LONG_LONG_TYPE_SIZE);
  tree gnu_size = 0;
  int align = 0;

  /* Since a use of an Itype is a definition, process it as such if it is
     not in a with'ed unit.  */
  if (! definition && Is_Itype (gnat_entity)
      && ! present_gnu_tree (gnat_entity)
      && Entity_Is_In_Main_Unit (gnat_entity))
    {
      process_type (gnat_entity);
      return get_gnu_tree (gnat_entity);
    }

  /* If this is entity 0, something went badly wrong.  */
  if (gnat_entity == 0)
    gigi_abort (102);

  /* If we've already processed this entity, return what we got last time.
     If we are defining the node, we should not have already processed it.
     In that case, we will abort below when we try to save a new GCC tree for
     this object.

     We make an exception here for subprograms since we may have processed
     both the spec and body, depending on the circumstances.  This is a
     bit of a kludge, but we are only using the kludge to disable an error
     check, so it's not too bad.

     We also need to handle the case of getting a dummy type when a
     Full_View exists.  */

  if ((! definition || kind == E_Function || kind == E_Procedure)
      && present_gnu_tree (gnat_entity))
    {
      gnu_decl = get_gnu_tree (gnat_entity);

      if (TREE_CODE (gnu_decl) == TYPE_DECL
	  && TYPE_IS_DUMMY_P (TREE_TYPE (gnu_decl))
	  && IN (kind, Incomplete_Or_Private_Kind)
	  && Present (Full_View (gnat_entity)))
	{
	  gnu_decl = get_gnu_tree (Full_View (gnat_entity));
	  save_gnu_tree (gnat_entity, NULL_TREE, 0);
	  save_gnu_tree (gnat_entity, gnu_decl, 0);
	}

      return gnu_decl;
    }

  /* If this is a numeric or enumeral type, or an access type, a nonzero
     Esize must be specified unless it was specified by the programmer.  */
  if ((IN (kind, Numeric_Kind) || IN (kind, Enumeration_Kind)
       || (IN (kind, Access_Kind)
	   && kind != E_Access_Protected_Subprogram_Type
	   && kind != E_Access_Subtype))
      && (No (Esize (gnat_entity)) || esize == 0)
      && ! Has_Size_Clause (gnat_entity))
    gigi_abort (109);

  /* Get the name of the entity and set up the line number and filename of
     the original definition for use in any decl we make.  */

  gnu_entity_id = get_entity_name (gnat_entity);
  set_lineno (gnat_entity, 0);

  /* If we get here, it means we have not yet done anything with this
     entity.  If we are not defining it here, it must be external,
     otherwise we should have defined it already.  */
  if (! definition && ! Is_Public (gnat_entity)
      && kind != E_Discriminant && kind != E_Component
      && ! (kind == E_Constant && Present (Full_View (gnat_entity)))
#if 1
      && NOTIN (kind, Type_Kind)
#endif
      )
    gigi_abort (116);

  /* If we are not defining this node, it is external and must be
     permanently allocated.  If we are not already in permanent
     allocation, go there now.  Likewise if it is imported.  */
  if ((! definition || Is_Imported (gnat_entity)) && ! was_permanent) 
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();

      if (Is_Public (gnat_entity))
	/* When computing sizes, treat us as being at global level.  */
	force_global++;
    }

  /* Make sure objects we allocate aren't in the momentary obstack.  */
  was_momentary = suspend_momentary ();

  /* Handle any attributes.  */
  if (Has_Gigi_Rep_Item (gnat_entity))
    attr_list = build_attr_list (gnat_entity);

  switch (kind)
    {
    case E_Constant:
      /* If this is a use of a deferred constant, get its full
	 declaration.  */
      if (! definition && Present (Full_View (gnat_entity)))
	{
	  gnu_decl = gnat_to_gnu_entity (Full_View (gnat_entity),
					 gnu_expr, definition);
	  saved = 1;
	  break;
	}

      /* If we have an external constant that we are not defining,
	 get the expression that is was defined to represent.  We
	 may throw that expression away later if it is not a
	 constant.  */
      if (! definition 
	  && Present (Expression (Declaration_Node (gnat_entity)))
	  && ! No_Default_Init (Declaration_Node (gnat_entity)))

	gnu_expr = gnat_to_gnu (Expression (Declaration_Node (gnat_entity)));

      /* Ignore deferred constant definitions; they are processed fully in the
	 front-end.  For deferred constant references, get the full
         definition.  On the other hand, constants that are renamings are
	 handled like variable renamings.  If No_Default_Init is set, this is
	 not a deferred constant but a constant whose value is built
	 manually.  */

      if (definition && gnu_expr == 0
	  && ! No_Default_Init (Declaration_Node (gnat_entity))
	  && No (Renamed_Object (gnat_entity)))
	{
	  gnu_decl = error_mark_node;
	  saved = 1;
          break;
	}
      else if (! definition && IN (kind, Incomplete_Or_Private_Kind)
	       && Present (Full_View (gnat_entity)))
	{
	  gnu_decl =  gnat_to_gnu_entity (Full_View (gnat_entity),
					  NULL_TREE, 0);
	  saved = 1;
	  break;
	}

      goto object;

    case E_Discriminant:
    case E_Component:
      {
	/* The gnat record where the component was defined. */
	Entity_Id gnat_record = Scope (gnat_entity);

	/* If the variable is an inherited record component (in the case of
	   extended record types), just return the inherited entity, which
	   must be a FIELD_DECL.  Likewise for discriminants.
	   For discriminants of untagged records which have explicit
	   girder discriminants, return the entity for the corresponding
	   girder discriminant.  */

	if (Present (Original_Record_Component (gnat_entity))
	    && Original_Record_Component (gnat_entity) != gnat_entity)
	  {
	    gnu_decl
	      = gnat_to_gnu_entity (Original_Record_Component (gnat_entity),
				    gnu_expr, definition);
	    saved = 1;
	    break;
	  }

	/* If the enclosing record has explicit girder discriminants,
	   then it is an untagged record.  If the Corresponding_Discriminant
	   is not empty then this must be a renamed discriminant and its
	   Original_Record_Component must point to the corresponding explicit
	   girder discriminant (i.e., we should have taken the previous
	   branch).  */

	else if (Present (Corresponding_Discriminant (gnat_entity))
		 && (Is_Tagged_Type (gnat_record)))
	  {
	    /* A tagged record has no explicit girder discriminants. */

	    if (First_Discriminant (gnat_record)
		!= First_Girder_Discriminant (gnat_record))
	      gigi_abort (119);

	    gnu_decl
	      = gnat_to_gnu_entity (Corresponding_Discriminant (gnat_entity),
				    gnu_expr, definition);
	    saved = 1;
	    break;
	  }

	/* If the enclosing record has explicit girder discriminants,
	   then it is an untagged record. If the Corresponding_Discriminant
	   is not empty then this must be a renamed discriminant and its
	   Original_Record_Component must point to the corresponding explicit
	   girder discriminant (i.e., we should have taken the first
	   branch).  */

	else if (Present (Corresponding_Discriminant (gnat_entity))
		 && (First_Discriminant (gnat_record)
		     != First_Girder_Discriminant (gnat_record)))
	  gigi_abort (120);

	/* Otherwise, if we are not defining this and we have no GCC type
	   for the containing record, make one for it.  Then we should
	   have made our own equivalent.  Otherwise, abort.  */
	else if (! definition && ! present_gnu_tree (gnat_record))
	  {
	    gnat_to_gnu_entity (Scope (gnat_entity), NULL_TREE, 0);
	    gnu_decl = get_gnu_tree (gnat_entity);
	    saved = 1;
	    break;
	  } 

	else
	  gigi_abort (103);
      }

    case E_Loop_Parameter:
    case E_Out_Parameter:
    case E_Exception:
    case E_Variable:

      /* Simple variables, loop variables, OUT parameters, and exceptions.  */
    object:
      {
	tree gnu_type;
	int used_by_ref = 0;
	int const_flag
	  = (kind == E_Constant
	     && Not_Assigned (gnat_entity)
	     && ! Is_Aliased (gnat_entity)
	     && ! Is_Aliased (Etype (gnat_entity))
	     && ! Address_Taken (gnat_entity)
	     && (((Nkind (Declaration_Node (gnat_entity))
		   == N_Object_Declaration)
		  && Present (Expression (Declaration_Node (gnat_entity))))
		 || Present (Renamed_Object (gnat_entity))));
	int inner_const_flag = const_flag;
	tree gnu_ext_name = NULL_TREE;

	/* If GNU_EXPR may be in the momentary obstack, make sure we don't
	   free it if this is a constant or a renaming.  */
	if (was_momentary && gnu_expr != 0
	    && (const_flag || Present (Renamed_Object (gnat_entity))))
	  preserve_momentary ();

	if (Present (Renamed_Object (gnat_entity)) && ! definition)
	  {
	    if (kind == E_Exception)
	      gnu_expr = gnat_to_gnu_entity (Renamed_Entity (gnat_entity), 
					     NULL_TREE, 0);
	    else
	      gnu_expr = gnat_to_gnu (Renamed_Object (gnat_entity));
	  }

	/* Get the type after elaborating the renamed object.  */
	gnu_type = gnat_to_gnu_type (Etype (gnat_entity));

	/* If this is a loop variable, its type should be the base type.
	   This is because the code for processing a loop determines whether
	   a normal loop end test can be done by comparing the bounds of the
	   loop against those of the base type, which is presumed to be the
	   size used for computation.  But this is not correct when the size
	   of the subtype is smaller than the type.  */
	if (kind == E_Loop_Parameter)
	  gnu_type = get_base_type (gnu_type);

	/* Reject non-renamed objects whose types are unconstrained arrays or 
	   any object whose type is a dummy type or VOID_TYPE. */

	if ((TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE
	     && No (Renamed_Object (gnat_entity)))
	    || TYPE_IS_DUMMY_P (gnu_type)
	    || TREE_CODE (gnu_type) == VOID_TYPE)
	  gigi_abort (104);

	/* If we are defining the object, see if it has a Size value and
	   validate it if so.  Then get the new type, if any.  */
	if (definition)
	  gnu_size = validate_size (Esize (gnat_entity), gnu_type,
				    gnat_entity, VAR_DECL, 0);

	if (gnu_size != 0)
	  {
	    gnu_type
	      = make_type_from_size (gnu_type, gnu_size,
				     Has_Biased_Representation (gnat_entity));

	    if (operand_equal_p (TYPE_SIZE (gnu_type), gnu_size, 0))
	      gnu_size = 0;
	  }

	/* If this object has self-referential size, it must be a record with
	   a default value.  We are supposed to allocate an object of the
	   maximum size in this case unless it is a constant with an
	   initializing expression, in which case we can get the size from
	   that.  Note that the resulting size may still be a variable, so
	   this may end up with an indirect allocation.  */

	if (No (Renamed_Object (gnat_entity))
	    && TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST
	    && contains_placeholder_p (TYPE_SIZE (gnu_type)))
	  {
	    if (gnu_expr != 0 && kind == E_Constant)
	      {
		gnu_size = TYPE_SIZE (TREE_TYPE (gnu_expr));
		if (TREE_CODE (gnu_size) != INTEGER_CST
		    && contains_placeholder_p (gnu_size))
		  {
		    tree gnu_temp = gnu_expr;

		    /* Strip off any conversions in GNU_EXPR since
		       they can't be changing the size to allocate.  */
		    while (TREE_CODE (gnu_temp) == UNCHECKED_CONVERT_EXPR)
		      gnu_temp = TREE_OPERAND (gnu_temp, 0);

		    gnu_size = TYPE_SIZE (TREE_TYPE (gnu_temp));
		    if (TREE_CODE (gnu_size) != INTEGER_CST
			&& contains_placeholder_p (gnu_size))
		      gnu_size = build (WITH_RECORD_EXPR, sizetype, gnu_size,
					gnu_temp);
		  }
	      }

	    /* We may have no GNU_EXPR because No_Default_Init is
	       set even though there's an Expression.  */
	    else if (kind == E_Constant
		     && (Nkind (Declaration_Node (gnat_entity))
			 == N_Object_Declaration)
		     && Present (Expression (Declaration_Node (gnat_entity))))
	      gnu_size
		= TYPE_SIZE (gnat_to_gnu_type
			     (Etype
			      (Expression (Declaration_Node (gnat_entity)))));
	    else
	      gnu_size = max_size (TYPE_SIZE (gnu_type), 1);
	  }

	/* If the size is zero bytes, make it one byte since some linkers
	   have trouble with zero-sized objects.  */
	if ((gnu_size != 0 && integer_zerop (gnu_size))
	    || (TYPE_SIZE (gnu_type) != 0
		&& integer_zerop (TYPE_SIZE (gnu_type))))
	  gnu_size = size_int (BITS_PER_UNIT);

	/* If an alignment is specified, use it if valid.   */
	if (Present (Alignment_Clause (gnat_entity)))
	  align
	    = validate_alignment (Alignment_Clause (gnat_entity),
				  gnat_entity, TYPE_ALIGN (gnu_type));

	/* If this is an atomic object with no specified size and alignment,
	   but where the size of the type is a constant smaller than the
	   word size, set the alignment to the lowest power of two greater
	   than the size.  */

	if (Is_Atomic (gnat_entity) && gnu_size == 0 && align == 0
	    && TREE_CODE (TYPE_SIZE (gnu_type)) == INTEGER_CST)
	  {
	    if (TREE_INT_CST_HIGH (TYPE_SIZE (gnu_type)) != 0
		|| (TREE_INT_CST_LOW (TYPE_SIZE (gnu_type))
		    >= BIGGEST_ALIGNMENT))
	      align = BIGGEST_ALIGNMENT;
	    else
	      align = ((HOST_WIDE_INT) 1
		       << (floor_log2 (TREE_INT_CST_LOW
				       (TYPE_SIZE (gnu_type)) - 1)
			   + 1));
	  }

#ifdef MINIMUM_ATOMIC_ALIGNMENT
	/* If the size is a constant and no alignment is specified, force
	   the alignment to be the minimum valid atomic alignment.  The
	   restriction on constant size avoids problems with variable-size
	   temporaries; if the size is variable, there's no issue with
	   atomic access.  Also don't do this for a constant, since it isn't
	   necessary and can interfere with constant replacement.  Finally,
	   do not do it for Out parameters since that creates an 
	   size inconsistency with In parameters.  */
	if (align == 0 && MINIMUM_ATOMIC_ALIGNMENT > TYPE_ALIGN (gnu_type)
	    && ! const_flag && No (Renamed_Object (gnat_entity))
	    && kind != E_Out_Parameter
	    && (gnu_size != 0 ? TREE_CODE (gnu_size) == INTEGER_CST
		: TREE_CODE (TYPE_SIZE (gnu_type)) == INTEGER_CST))
	  align = MINIMUM_ATOMIC_ALIGNMENT;
#endif

	/* Make a new type with the desired size and alignment, if needed. */
	gnu_type = maybe_pad_type (gnu_type, gnu_size, align,
				   gnat_entity, "PAD", 0);

	/* Make a volatile version of this object's type if we are to
	   make the object volatile.  Note that 13.3(19) says that we
	   should treat other types of objects as volatile as well.  */
	if ((Is_Volatile (gnat_entity)
	     || Is_Exported (gnat_entity)
	     || Is_Imported (gnat_entity)
	     || Present (Address_Clause (gnat_entity)))
	    && ! TYPE_VOLATILE (gnu_type))
	  gnu_type = build_type_variant (gnu_type, 0, 1);

	/* Convert the expression to the type of the object except in the
	   case where the object's type is unconstrained.  In that case,
	   doing that will generate unnecessary evaluations of the
	   CONSTRUCTOR to compute the size.  */
	if (gnu_expr != 0
	    && ! (TYPE_SIZE (gnu_type) != 0
		  && TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST
		  && contains_placeholder_p (TYPE_SIZE (gnu_type))))
	  gnu_expr = convert (gnu_type, gnu_expr);

	/* See if this is a renaming.  If this is a constant renaming,
	   treat it as a normal variable whose initial value is what
	   is being renamed.  However, if the initial value is a constant,
	   we can just return it.  We cannot do this if the type is
	   unconstrained or class-wide.

	   Otherwise, if what we are renaming is a reference, we can simply
	   return a stabilized version of that reference, after forcing
	   any SAVE_EXPRs to be evaluated.  But, if this is at global level,
	   we can only do this if we know no SAVE_EXPRs will be made.  
	   Otherwise, make this into a constant pointer to the object we are
	   to rename.  */

	if (Present (Renamed_Object (gnat_entity)))
	  {
	    /* If the renamed object had padding, strip off the reference
	       to the inner object and reset our type.  */
	    if (TREE_CODE (gnu_expr) == COMPONENT_REF
		&& (TYPE_IS_PADDING_P
		    (TREE_TYPE (TREE_OPERAND (gnu_expr, 0)))))
	      {
		gnu_expr = TREE_OPERAND (gnu_expr, 0);
		gnu_type = TREE_TYPE (gnu_expr);
	      }

	    if (const_flag
		&& TREE_CODE (gnu_type) != UNCONSTRAINED_ARRAY_TYPE
		&& (TREE_CODE (TYPE_SIZE (gnu_type)) == INTEGER_CST
		    || ! contains_placeholder_p (TYPE_SIZE (gnu_type)))
		&& Ekind (Etype (gnat_entity)) != E_Class_Wide_Type)
	      {
		if (TREE_CONSTANT (gnu_expr))
		  {
		    gnu_decl = gnu_expr;
		    save_gnu_tree (gnat_entity, gnu_decl, 1);
		    saved = 1;
		    break;
		  }
	      }
	    else if ((TREE_CODE_CLASS (TREE_CODE (gnu_expr)) == 'd'
		      || TREE_CODE_CLASS (TREE_CODE (gnu_expr)) == 'r')
		     && (! global_bindings_p () || staticp (gnu_expr)))
	      {
		gnu_decl = gnat_stabilize_reference (gnu_expr, 1);
		save_gnu_tree (gnat_entity, gnu_decl, 1);
		saved = 1;
		expand_expr_stmt (gnu_decl);
		break;
	      }
	    else
	      {
		inner_const_flag = TREE_READONLY (gnu_expr);
		const_flag = 1;
		gnu_expr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_expr);
		gnu_type = build_pointer_type (gnu_type);
		gnu_size = 0;
		used_by_ref = 1;
	      }
	  }

	/* If this is an aliased object whose nominal subtype is unconstrained,
	   the object is a record that contains both the template and
	   the object.  If there is an initializer, it will have already
	   been converted to the right type, but we need to create the
	   template if there is no initializer.  */
	else if (definition && TREE_CODE (gnu_type) == RECORD_TYPE
		 && TYPE_CONTAINS_TEMPLATE_P (gnu_type)
		 && gnu_expr == 0)
	  gnu_expr
	    = build_constructor
	      (gnu_type,
	       tree_cons
	       (TYPE_FIELDS (gnu_type),
		build_template
		(TREE_TYPE (TYPE_FIELDS (gnu_type)),
		 TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (gnu_type))),
		 NULL_TREE),
		NULL_TREE));

	/* If we are defining the object and it has an Address clause we must
	   get the address expression from the saved GCC tree for the
	   object if the object has a Freeze_Node.  Otherwise, we elaborate
	   the address expression here since the front-end has guaranteed
	   in that case that the elaboration has no effects.  Note that
	   only the latter mechanism is currently in use.  */
	if (definition && Present (Address_Clause (gnat_entity)))
	  {
	    tree gnu_address
	      = (present_gnu_tree (gnat_entity) ? get_gnu_tree (gnat_entity)
		: gnat_to_gnu (Expression (Address_Clause (gnat_entity))));

	    save_gnu_tree (gnat_entity, NULL_TREE, 0);

	    /* Ignore the size.  It's either meaningless or was handled
	       above.  */
	    gnu_size = 0;
	    gnu_type = build_pointer_type (gnu_type);
	    gnu_address = convert (gnu_type, gnu_address);
	    used_by_ref = 1;
	    const_flag = ! Is_Public (gnat_entity);

	    /* If we don't have an initializing expression for the underlying
	       variable, the initializing expression for the pointer is the
	       specified address.  Otherwise, we have to make a COMPOUND_EXPR
	       to assign both the address and the initial value.  */
	    if (gnu_expr == 0)
	      gnu_expr = gnu_address;
	    else
	      gnu_expr
		= build (COMPOUND_EXPR, gnu_type,
			 build_binary_op
			 (MODIFY_EXPR, NULL_TREE,
			  build_unary_op (INDIRECT_REF, NULL_TREE,
					  gnu_address),
			  gnu_expr),
			 gnu_address);
	  }

	/* If it has an address clause and we are not defining it, mark it
	   as an indirect object.  Likewise for Stdcall objects that are
	   imported.  */
	if ((! definition && Present (Address_Clause (gnat_entity)))
	    || (Is_Imported (gnat_entity)
		&& Convention (gnat_entity) == Convention_Stdcall))
	  {
	    gnu_type = build_pointer_type (gnu_type);
	    gnu_size = 0;
	    used_by_ref = 1;
	    const_flag = 0;
	  }

	/* If we are at top level and this object is of variable size,
	   make the actual type a hidden pointer to the real type and
	   make the initializer be a memory allocation and initialization.
	   Likewise for objects we aren't defining (presumed to be
	   external references from other packages), but there we do
	   not set up an initialization.

	   If the object's size overflows or if it is larger than a
	   reasonable size for a stack frame, make an allocator too.  */

	if (((global_bindings_p () || ! definition)
	     && TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST
	     && ! (gnu_size != 0 && TREE_CODE (gnu_size) == INTEGER_CST))
	    || (TREE_CODE (TYPE_SIZE (gnu_type)) == INTEGER_CST
		&& (TREE_CONSTANT_OVERFLOW (TYPE_SIZE (gnu_type))
		    || TREE_INT_CST_HIGH (TYPE_SIZE (gnu_type)) != 0
		    || (TREE_INT_CST_LOW (TYPE_SIZE (gnu_type))
			> 10000000 * BITS_PER_UNIT)))
	    || (gnu_size != 0
		&& TREE_CODE (gnu_size) == INTEGER_CST
		&& (TREE_CONSTANT_OVERFLOW (gnu_size)
		    || TREE_INT_CST_HIGH (gnu_size) != 0
		    || (TREE_INT_CST_LOW (gnu_size)
			> 10000000 * BITS_PER_UNIT))))
	  {
	    gnu_type = build_pointer_type (gnu_type);
	    gnu_size = 0;
	    used_by_ref = 1;

	    /* Get the data port of GNU_EXPR in case this was a
	       aliased object whose nominal subtype is unconstrained.
	       In that case the pointer above will be a thin pointer and
	       build_allocator will automatically make the template and
	       constructor already made above.  */

	    if (definition)
	      {
		tree gnu_alloc_type = TREE_TYPE (gnu_type);

		if (TREE_CODE (gnu_alloc_type) == RECORD_TYPE
		    && TYPE_CONTAINS_TEMPLATE_P (gnu_alloc_type))
		  {
		    gnu_alloc_type
		      = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (gnu_alloc_type)));
		    gnu_expr
		      = build_component_ref
			(gnu_expr, NULL_TREE,
			 TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (gnu_expr))));
		  }

		if (TREE_CODE (TYPE_SIZE (gnu_alloc_type)) == INTEGER_CST
		    && TREE_CONSTANT_OVERFLOW (TYPE_SIZE (gnu_alloc_type))
		    && ! Is_Imported (gnat_entity))
		  post_error ("Storage_Error will be raised at run-time?",
			      gnat_entity);

		gnu_expr = build_allocator (gnu_alloc_type, gnu_expr,
					    gnu_type, 0, 0);
	      }
	    else
	      gnu_expr = 0;
	  }

	/* If this is a pointer and it does not have an initializing
	   expression, initialize it to NULL.  */
	if (definition
	    && (TREE_CODE (gnu_type) == POINTER_TYPE
		|| TYPE_FAT_POINTER_P (gnu_type))
	    && gnu_expr == 0)
	  gnu_expr = integer_zero_node;

	/* Convert the expression to the type of the object except in the
	   case where the object's type is unconstrained.  In that case,
	   doing that will generate unnecessary evaluations of the
	   CONSTRUCTOR to compute the size.  */
	if (gnu_expr != 0
	    && ! (TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST
		  && contains_placeholder_p (TYPE_SIZE (gnu_type))))
	  gnu_expr = convert (gnu_type, gnu_expr);

	if (Present (Interface_Name (gnat_entity))
	    || (Is_Public (gnat_entity)
		&& (! Is_Imported (gnat_entity) || Is_Exported (gnat_entity))))
	  gnu_ext_name = create_concat_name (gnat_entity, NULL_PTR);

	set_lineno (gnat_entity, 0);
	gnu_decl = create_var_decl (gnu_entity_id, gnu_ext_name, gnu_type,
				    gnu_expr, const_flag,
				    Is_Public (gnat_entity),
				    Is_Imported (gnat_entity) || !definition,
				    Is_Statically_Allocated (gnat_entity),
				    attr_list);

	DECL_BY_REF_P (gnu_decl) = used_by_ref;
	DECL_POINTS_TO_READONLY_P (gnu_decl) = used_by_ref && inner_const_flag;

	if (definition && DECL_SIZE (gnu_decl) != 0
	    && TREE_CODE (DECL_SIZE (gnu_decl)) != INTEGER_CST
	    && gnu_block_stack != 0
	    && TREE_VALUE (gnu_block_stack) != 0)
	  update_setjmp_buf (TREE_VALUE (gnu_block_stack));

	/* If this is an exported constant or we are compiling without
	   optimization and we're not making a VAR_DECL for it, make one
	   just for export or debugger use.  */
	if (definition && TREE_CODE (gnu_decl) == CONST_DECL && ! used_by_ref
	    && (Is_Exported (gnat_entity) || optimize == 0))
	  create_var_decl (gnu_entity_id, gnu_ext_name,
			   gnu_type, gnu_expr, 0, 1, 0, 0, NULL_PTR);

	if (Is_Atomic (gnat_entity))
	  check_ok_for_atomic (gnu_decl, gnat_entity);

	/* If this is declared in a block that contains an block with an
	   exception handler, we must force this variable in memory to
	   suppress an invalid optimization.  */
	if (Has_Nested_Block_With_Handler (Scope (gnat_entity)))
	  mark_addressable (gnu_decl);
      }
      break;

    case E_Named_Integer:
    case E_Named_Real:
      /* These should not be present in any part of the tree we look at.  */
      gigi_abort (106);

    case E_Void:
      /* Return a TYPE_DECL for "void" that we previously made.  */
      gnu_decl = void_type_decl_node;
      break;

    case E_Enumeration_Type:
      /* A special case, for the types Character and Wide_Character in
         Standard, we do not list all the literals. So if the literals
         are not specified, make this an unsigned type.  */
      if (No (First_Literal (gnat_entity)))
	{
	  gnu_type = make_unsigned_type (esize);
	  break;
	}

      /* Normal case of non-character type, or non-Standard character type */
      {
	/* Here we have a list of enumeral constants in First_Literal.
	   We make a CONST_DECL for each and build into GNU_LITERAL_LIST
	   the list to be places into TYPE_FIELDS.  Each node in the list
	   is a TREE_LIST node whose TREE_VALUE is the literal name
	   and whose TREE_PURPOSE is the value of the literal.

	   Esize contains the number of bits needed to represent the enumeral
	   type, Type_Low_Bound also points to the first literal and
	   Type_High_Bound points to the last literal.  */

	Entity_Id gnat_literal;
	tree gnu_literal_list = NULL_TREE;

	if (Is_Unsigned_Type (gnat_entity))
	  gnu_type = make_unsigned_type (esize);
	else
	  gnu_type = make_signed_type (esize);

	TREE_SET_CODE (gnu_type, ENUMERAL_TYPE);

	for (gnat_literal = First_Literal (gnat_entity);
	     Present (gnat_literal);
	     gnat_literal = Next_Literal (gnat_literal))
	  {
	    tree gnu_value = UI_To_gnu (Enumeration_Rep (gnat_literal),
					gnu_type);
	    tree gnu_literal
	      = create_var_decl (get_entity_name (gnat_literal),
				 0, gnu_type, gnu_value, 1, 0, 0, 0, NULL_PTR);

	    save_gnu_tree (gnat_literal, gnu_literal, 0);
	    gnu_literal_list = tree_cons (DECL_NAME (gnu_literal),
					  gnu_value, gnu_literal_list);
	  }

	TYPE_FIELDS (gnu_type) = nreverse (gnu_literal_list);

	/* Note that the bounds are updated at the end of this function
	   because to avoid an infinite recursion when we get the bounds of
	   this type, since those bounds are objects of this type.    */
      }
      break;

    case E_Signed_Integer_Type:
    case E_Ordinary_Fixed_Point_Type:
    case E_Decimal_Fixed_Point_Type:
      /* For integer types, just make a signed type the appropriate number
	 of bits.  */
      gnu_type = make_signed_type (esize);
      break;

    case E_Modular_Integer_Type:
      /* For modular types, make the unsigned type of the proper number of
	 bits and then set up the modulus, if required.  */
      {
	enum machine_mode mode;
	tree gnu_modulus;
	tree gnu_high = 0;

	if (Is_Packed_Array_Type (gnat_entity))
	  esize = UI_To_Int (RM_Size (gnat_entity));

	/* Find the smallest mode at least ESIZE bits wide and make a class
	   using that mode.  */

	for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	     GET_MODE_BITSIZE (mode) < esize;
	     mode = GET_MODE_WIDER_MODE (mode))
	  ;

	gnu_type = make_unsigned_type (GET_MODE_BITSIZE (mode));
	TYPE_PACKED_ARRAY_TYPE_P (gnu_type)
	  = Is_Packed_Array_Type (gnat_entity);

	/* Get the modulus in this type.  If it overflows, assume it is because
	   it is equal to 2**Esize.  Note that there is no overflow checking
	   done on unsigned type, so we detect the overflow by looking for
	   a modulus of zero, which is otherwise invalid.  */
	gnu_modulus = UI_To_gnu (Modulus (gnat_entity), gnu_type);

	if (! integer_zerop (gnu_modulus))
	  {
	    TYPE_MODULAR_P (gnu_type) = 1;
	    TYPE_MODULUS (gnu_type) = gnu_modulus;
	    gnu_high = fold (build (MINUS_EXPR, gnu_type, gnu_modulus,
				    convert (gnu_type, integer_one_node)));
	  }

	/* If we have to set TYPE_PRECISION different from its natural value,
	   make a subtype to do do.  Likewise if there is a modulus and
	   it is not one greater than TYPE_MAX_VALUE.  */
	if (TYPE_PRECISION (gnu_type) != esize
	    || (TYPE_MODULAR_P (gnu_type)
		&& ! tree_int_cst_equal (TYPE_MAX_VALUE (gnu_type), gnu_high)))
	  {
	    tree gnu_subtype = make_node (INTEGER_TYPE);

	    TREE_TYPE (gnu_subtype) = gnu_type;
	    TYPE_MIN_VALUE (gnu_subtype) = TYPE_MIN_VALUE (gnu_type);
	    TYPE_MAX_VALUE (gnu_subtype)
	      = TYPE_MODULAR_P (gnu_type)
		? gnu_high : TYPE_MAX_VALUE (gnu_type);
	    TYPE_PRECISION (gnu_subtype) = esize;
	    TREE_UNSIGNED (gnu_subtype) = 1;
	    TYPE_EXTRA_SUBTYPE_P (gnu_subtype) = 1;
	    TYPE_PACKED_ARRAY_TYPE_P (gnu_subtype)
	      = Is_Packed_Array_Type (gnat_entity);
	    layout_type (gnu_subtype);

	    gnu_type = gnu_subtype;
	  }
      }
      break;

    case E_Signed_Integer_Subtype:
    case E_Enumeration_Subtype:
    case E_Modular_Integer_Subtype:
    case E_Ordinary_Fixed_Point_Subtype:
    case E_Decimal_Fixed_Point_Subtype:

      /* For integral subtypes, we make a new INTEGER_TYPE.  Note
	 that we do not want to call build_range_type since we would
	 like each subtype node to be distinct.  This will be important
	 when memory aliasing is implemented.

	 The TREE_TYPE field of the INTEGER_TYPE we make points to the
	 parent type; this fact is used by the arithmetic conversion
	 functions.  */

      gnu_type = make_node (INTEGER_TYPE);
      if (Is_Packed_Array_Type (gnat_entity))
	{
	  esize = UI_To_Int (RM_Size (gnat_entity));
	  TYPE_PACKED_ARRAY_TYPE_P (gnu_type) = 1;
	}

      TYPE_PRECISION (gnu_type) = esize;
      TREE_TYPE (gnu_type) = get_unpadded_type (Etype (gnat_entity));

      TYPE_MIN_VALUE (gnu_type)
	= convert (TREE_TYPE (gnu_type),
		   elaborate_expression (Type_Low_Bound (gnat_entity),
					 gnat_entity,
					 get_identifier ("L"), definition, 1));

      TYPE_MAX_VALUE (gnu_type)
	= convert (TREE_TYPE (gnu_type),
		   elaborate_expression (Type_High_Bound (gnat_entity),
					 gnat_entity,
					 get_identifier ("U"), definition, 1));

      /* One of the above calls might have caused us to be elaborated,
	 so don't blow up if so.  */
      if (present_gnu_tree (gnat_entity))
	{
	  maybe_present = 1;
	  break;
	}

      TYPE_BIASED_REPRESENTATION_P (gnu_type)
	= Has_Biased_Representation (gnat_entity); 

     /* This should be an unsigned type if the lower bound is constant
	 and non-negative or if the base type is unsigned; a signed type
	 otherwise.    */
      TREE_UNSIGNED (gnu_type)
	= (TREE_UNSIGNED (TREE_TYPE (gnu_type))
	   || (TREE_CODE (TYPE_MIN_VALUE (gnu_type)) == INTEGER_CST
	       && TREE_INT_CST_HIGH (TYPE_MIN_VALUE (gnu_type)) >= 0)
	   || TYPE_BIASED_REPRESENTATION_P (gnu_type)
	   || Is_Unsigned_Type (gnat_entity));

      layout_type (gnu_type);

      if (Is_Packed_Array_Type (gnat_entity) && BYTES_BIG_ENDIAN)
	{
	  tree gnu_field_type = gnu_type;

	  TYPE_RM_SIZE_INT (gnu_field_type)
	    = UI_To_gnu (RM_Size (gnat_entity), sizetype);
	  gnu_type = make_node (RECORD_TYPE);
	  TYPE_NAME (gnu_type) = create_concat_name (gnat_entity, "LJM");
	  finish_record_type (gnu_type,
			      create_field_decl (get_identifier ("OBJECT"),
						 gnu_field_type,
						 gnu_type, 1, 0, 0),
			      0, 0);
	  TYPE_LEFT_JUSTIFIED_MODULAR_P (gnu_type) = 1;
	  TYPE_ADA_SIZE (gnu_type) = size_int (esize);
	}

      break;

    case E_Floating_Point_Type:
      /* The type of the Low and High bounds can be our type if this is
	 a type from Standard, so set them at the end of the function.  */
      gnu_type = make_node (REAL_TYPE);
      TYPE_PRECISION (gnu_type) = esize;
      layout_type (gnu_type);
      break;

    case E_Floating_Point_Subtype:
      {
	enum machine_mode mode;

	for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT);
	     (GET_MODE_WIDER_MODE (mode) != VOIDmode
	      && GET_MODE_BITSIZE (GET_MODE_WIDER_MODE (mode)) <= esize);
	     mode = GET_MODE_WIDER_MODE (mode))
	  ;

	gnu_type = make_node (REAL_TYPE);
	TREE_TYPE (gnu_type) = get_unpadded_type (Etype (gnat_entity));
	TYPE_PRECISION (gnu_type) = UI_To_Int (GET_MODE_BITSIZE (mode));

	TYPE_MIN_VALUE (gnu_type)
	  = convert (TREE_TYPE (gnu_type),
		     elaborate_expression (Type_Low_Bound (gnat_entity),
					   gnat_entity, get_identifier ("L"),
					   definition, 1));

	TYPE_MAX_VALUE (gnu_type)
	  = convert (TREE_TYPE (gnu_type),
		     elaborate_expression (Type_High_Bound (gnat_entity),
					   gnat_entity, get_identifier ("U"),
					   definition, 1));

	/* One of the above calls might have caused us to be elaborated,
	   so don't blow up if so.  */
	if (present_gnu_tree (gnat_entity))
	  {
	    maybe_present = 1;
	    break;
	  }

	layout_type (gnu_type);
      }
    break;

      /* Array and String Types and Subtypes

	 Unconstrained array types are represented by E_Array_Type and
	 constrained array types are represented by E_Array_Subtype.  There
	 are no actual objects of an unconstrained array type; all we have
	 are pointers to that type.

	 The following fields are defined on array types and subtypes:

		Component_Type     Component type of the array.
		Number_Dimensions  Number of dimensions (an int).
		First_Index	   Type of first index.  */

    case E_String_Type:
    case E_Array_Type:
      {
	tree gnu_template_fields = NULL_TREE;
	tree gnu_template_type = make_node (RECORD_TYPE);
	tree gnu_ptr_template = build_pointer_type (gnu_template_type);
	tree gnu_fat_type = make_node (RECORD_TYPE);
	int ndim = Number_Dimensions (gnat_entity);
	int firstdim
	  = (Convention (gnat_entity) == Convention_Fortran) ? ndim - 1 : 0;
	int nextdim
	  = (Convention (gnat_entity) == Convention_Fortran) ? - 1 : 1;
	tree *gnu_index_types = (tree *) alloca (ndim * sizeof (tree *));
	tree *gnu_temp_fields = (tree *) alloca (ndim * sizeof (tree *));
	tree gnu_comp_size = 0;
	int index;
	Entity_Id gnat_ind_subtype;
	tree gnu_template_reference;
	tree tem;

	TYPE_NAME (gnu_template_type) = get_identifier ("BOUNDS");
	TYPE_NAME (gnu_fat_type) = gnu_entity_id;
	TYPE_IS_FAT_POINTER_P (gnu_fat_type) = 1;
	TREE_READONLY (gnu_template_type) = 1;

	/* Make a node for the array.  If we are not defining the array
	   suppress expanding incomplete types and save the node as the type
	   for GNAT_ENTITY.  */
	gnu_type = make_node (UNCONSTRAINED_ARRAY_TYPE);
	if (! definition)
	  {
	    defer_incomplete_level++;
	    this_deferred = 1;
	    gnu_decl = create_type_decl (gnu_entity_id, gnu_type, attr_list);
	    save_gnu_tree (gnat_entity, gnu_decl, 0);
	    saved = 1;
	  }

	/* Build the fat pointer type.  Use a "void *" object instead of
	   a pointer to the array type since we don't have the array type
	   yet (it will reference the fat pointer via the bounds).  */
	tem = chainon (chainon (NULL_TREE,
				create_field_decl (get_identifier ("P_ARRAY"),
						   ptr_void_type_node,
						   gnu_fat_type, 0, 0, 0)),
		       create_field_decl (get_identifier ("P_BOUNDS"),
					  gnu_ptr_template,
					  gnu_fat_type, 0, 0, 0));

	/* Make sure we can put this into a register.  */
	TYPE_ALIGN (gnu_fat_type) = MIN (BIGGEST_ALIGNMENT, 2 * POINTER_SIZE);
	finish_record_type (gnu_fat_type, tem, 0, 1);

	/* Build a reference to the template from a PLACEHOLDER_EXPR that
	   is the fat pointer.  This will be used to access the individual
	   fields once we build them.  */
	tem = build (COMPONENT_REF, gnu_ptr_template,
		     build (PLACEHOLDER_EXPR, gnu_fat_type),
		     TREE_CHAIN (TYPE_FIELDS (gnu_fat_type)));
	TREE_READONLY (tem) = 1;

	gnu_template_reference
	  = build_unary_op (INDIRECT_REF, gnu_template_type, tem);

	/* Now create the GCC type for each index and add the fields for
	   that index to the template.  */
	for (index = firstdim, gnat_ind_subtype = First_Index (gnat_entity);
	     index < ndim && index >= 0;
	     index += nextdim,
	     gnat_ind_subtype = Next_Index (gnat_ind_subtype))
	  {
	    char field_name[10];
	    tree gnu_ind_subtype
	      = get_unpadded_type (Base_Type (Etype (gnat_ind_subtype)));
	    tree gnu_min_field, gnu_max_field, gnu_min, gnu_max;

	    /* Make the FIELD_DECLs for the minimum and maximum of this
	       type and then make extractions of that field from the
	       template.  */
	    set_lineno (gnat_entity, 0);
	    sprintf (field_name, "LB%d", index);
	    gnu_min_field = create_field_decl (get_identifier (field_name),
					       gnu_ind_subtype,
					       gnu_template_type, 0, 0, 0);
	    field_name[0] = 'U';
	    gnu_max_field = create_field_decl (get_identifier (field_name),
					       gnu_ind_subtype,
					       gnu_template_type, 0, 0, 0);

	    gnu_temp_fields[index] = chainon (gnu_min_field, gnu_max_field);

	    /* We can't use build_component_ref here since the template
	       type isn't complete yet.  */
	    gnu_min = build (COMPONENT_REF, gnu_ind_subtype,
			     gnu_template_reference, gnu_min_field);
	    gnu_max = build (COMPONENT_REF, gnu_ind_subtype,
			     gnu_template_reference, gnu_max_field);
	    TREE_READONLY (gnu_min) = TREE_READONLY (gnu_max) = 1;

	    /* Make a range type with the new ranges, but using
	       the Ada subtype.  Then we convert to sizetype.  */
	    gnu_index_types[index]
	      = create_index_type (convert (sizetype, gnu_min),
				   convert (sizetype, gnu_max),
				   build_range_type (gnu_ind_subtype,
						     gnu_min, gnu_max));
	  }

	for (index = 0; index < ndim; index++)
	  gnu_template_fields
	    = chainon (gnu_template_fields, gnu_temp_fields[index]);

	/* Install all the fields into the template.  */
	finish_record_type (gnu_template_type, gnu_template_fields, 0, 0);
	TREE_READONLY (gnu_template_type) = 1;

	/* Now make the array of arrays and update the pointer to the array
	   in the fat pointer.  Note that it is the first field.  */

	tem = gnat_to_gnu_type (Component_Type (gnat_entity));

	/* Get and validate any specified Component_Size, but if Packed,
	   ignore it since the front end will have taken care of it.  Also,
	   allow sizes not a multiple of Storage_Unit if packed.  */
	gnu_comp_size = validate_size (Component_Size (gnat_entity), tem,
				       gnat_entity,
				       (Is_Bit_Packed_Array (gnat_entity)
					? TYPE_DECL : VAR_DECL), 1);

	/* If the component type is a RECORD_TYPE that has a self-referential
	   size, use the maxium size.  */
	if (gnu_comp_size == 0 && TREE_CODE (tem) == RECORD_TYPE
	    && TREE_CODE (TYPE_SIZE (tem)) != INTEGER_CST
	    && contains_placeholder_p (TYPE_SIZE (tem)))
	  gnu_comp_size = max_size (TYPE_SIZE (tem), 1);

	if (! Is_Bit_Packed_Array (gnat_entity) && gnu_comp_size != 0)
	  {
	    tem = make_type_from_size (tem, gnu_comp_size, 0);
	    tem = maybe_pad_type (tem, gnu_comp_size, 0, gnat_entity,
				  "C_PAD", 0);
	  }

	if (Has_Volatile_Components (gnat_entity))
	  tem = build_type_variant (tem, 0, 1);

	for (index = ndim - 1; index >= 0; index--)
	  {
	    tem = build_array_type (tem, gnu_index_types[index]);
	    TYPE_MULTI_ARRAY_P (tem) = (index > 0);
	  }

	/* If an alignment is specified, use it if valid.   */
	if (Present (Alignment_Clause (gnat_entity)))
	  TYPE_ALIGN (tem)
	    = validate_alignment (Alignment_Clause (gnat_entity),
				  gnat_entity, TYPE_ALIGN (tem));

	TYPE_ALIGN_OK_P (tem) = Is_Packed (gnat_entity);
	TYPE_CONVENTION_FORTRAN_P (tem)
	  = (Convention (gnat_entity) == Convention_Fortran);
	TREE_TYPE (TYPE_FIELDS (gnu_fat_type)) = build_pointer_type (tem);
	rest_of_type_compilation (gnu_fat_type, global_bindings_p ());

	/* The result type is an UNCONSTRAINED_ARRAY_TYPE that indicates the
	   corresponding fat pointer.  */
	TREE_TYPE (gnu_type) = TYPE_POINTER_TO (gnu_type) = gnu_fat_type;
	TYPE_MODE (gnu_type) = BLKmode;
	TYPE_ALIGN (gnu_type) = TYPE_ALIGN (tem);
	TYPE_UNCONSTRAINED_ARRAY (gnu_fat_type) = gnu_type;

	/* Create a record type for the object and its template and
	   set the template at a negative offset.  */
	tem = build_unc_object_type (gnu_template_type, tem,
				     create_concat_name (gnat_entity, "OR"));
	DECL_FIELD_BITPOS (TYPE_FIELDS (tem))
	  = size_binop (MINUS_EXPR, size_zero_node,
			DECL_FIELD_BITPOS (TREE_CHAIN (TYPE_FIELDS (tem))));
	DECL_FIELD_BITPOS (TREE_CHAIN (TYPE_FIELDS (tem))) = size_zero_node;
	TYPE_UNCONSTRAINED_ARRAY (tem) = gnu_type;
	TYPE_OBJECT_RECORD_TYPE (gnu_type) = tem;
      }
      break;

    case E_String_Subtype:
    case E_Array_Subtype:
      /* This is the actual data type for array variables.  Multidimensional
	 arrays are implemented in the gnu tree as arrays of arrays.  Note
	 that for the moment arrays which have sparse enumeration subtypes as
	 index components create sparse arrays, which is obviously space
	 inefficient but so much easier to code for now.

	 Also note that the subtype never refers to the unconstrained
	 array type, which is somewhat at variance with Ada semantics.

	 First check to see if this is simply a renaming of the array
	 type.  If so, the result is the array type.  */

      gnu_type = gnat_to_gnu_type (Etype (gnat_entity));
      if (! Is_Constrained (gnat_entity))
	break;
      else if (Present (Packed_Array_Type (gnat_entity)))
	{
	  Entity_Id gnat_index;
	  tree gnu_inner_type;

	  /* Define ourself here so that the elaboration below
	     doesn't come back and elaborate this type; it may do so
	     incorrectly.  */
	  save_gnu_tree (gnat_entity,
			 create_type_decl (gnu_entity_id,
					   make_dummy_type (gnat_entity),
					   attr_list),
			 0);

	  gnu_inner_type = gnu_type
	    = gnat_to_gnu_type (Packed_Array_Type (gnat_entity));
	  save_gnu_tree (gnat_entity, NULL_TREE, 0);

	  if (TREE_CODE (gnu_inner_type) == RECORD_TYPE
	      && (TYPE_LEFT_JUSTIFIED_MODULAR_P (gnu_inner_type)
		  || TYPE_IS_PADDING_P (gnu_inner_type)))
	    gnu_inner_type = TREE_TYPE (TYPE_FIELDS (gnu_inner_type));

	  /* We need to point the type we just made to our index type so
	     the actual bounds can be put into a template.  */

	  if ((TREE_CODE (gnu_inner_type) == ARRAY_TYPE
	       && TYPE_ACTUAL_BOUNDS (gnu_inner_type) == 0)
	      || (TREE_CODE (gnu_inner_type) == INTEGER_TYPE
		  && ! TYPE_HAS_ACTUAL_BOUNDS_P (gnu_inner_type)))
	    {
	      if (TREE_CODE (gnu_inner_type) == INTEGER_TYPE)
		{
		  /* The TYPE_ACTUAL_BOUNDS field is also used for the modulus.
		     If it is, we need to make another type.  */
		  if (TYPE_MODULAR_P (gnu_inner_type))
		    {
		      tree gnu_subtype;

		      push_obstacks (TYPE_OBSTACK (gnu_type),
				     TYPE_OBSTACK (gnu_type));
		      gnu_subtype = make_node (INTEGER_TYPE);

		      TREE_TYPE (gnu_subtype) = gnu_inner_type;
		      TYPE_MIN_VALUE (gnu_subtype)
			= TYPE_MIN_VALUE (gnu_inner_type);
		      TYPE_MAX_VALUE (gnu_subtype)
			= TYPE_MAX_VALUE (gnu_inner_type);
		      TYPE_PRECISION (gnu_subtype)
			= TYPE_PRECISION (gnu_inner_type);
		      TREE_UNSIGNED (gnu_subtype)
			= TREE_UNSIGNED (gnu_inner_type);
		      TYPE_EXTRA_SUBTYPE_P (gnu_subtype) = 1;
		      layout_type (gnu_subtype);
		      pop_obstacks ();

		      gnu_inner_type = gnu_subtype;
		    }

		  TYPE_HAS_ACTUAL_BOUNDS_P (gnu_inner_type) = 1;
		}

	      push_obstacks (TYPE_OBSTACK (gnu_inner_type),
			     TYPE_OBSTACK (gnu_inner_type));
	      TYPE_ACTUAL_BOUNDS (gnu_inner_type) = NULL_TREE;

	      for (gnat_index = First_Index (gnat_entity);
		   Present (gnat_index); gnat_index = Next_Index (gnat_index))
		TYPE_ACTUAL_BOUNDS (gnu_inner_type)
		  = tree_cons (NULL_TREE,
			       get_unpadded_type (Etype (gnat_index)),
			       TYPE_ACTUAL_BOUNDS (gnu_inner_type));

	      if (Convention (gnat_entity) != Convention_Fortran)
		TYPE_ACTUAL_BOUNDS (gnu_inner_type)
		  = nreverse (TYPE_ACTUAL_BOUNDS (gnu_inner_type));

	      TYPE_ALIGN_OK_P (gnu_inner_type) = 1;
	      pop_obstacks ();

	      if (TREE_CODE (gnu_type) == RECORD_TYPE
		  && TYPE_LEFT_JUSTIFIED_MODULAR_P (gnu_type))
		TREE_TYPE (TYPE_FIELDS (gnu_type)) = gnu_inner_type;
	    }
	}
      else
	{
	  int index;
	  int array_dim = Number_Dimensions (gnat_entity);
	  int first_dim
	    = ((Convention (gnat_entity) == Convention_Fortran)
	       ? array_dim - 1 : 0);
	  int next_dim
	    = (Convention (gnat_entity) == Convention_Fortran) ? -1 : 1;
	  Entity_Id gnat_ind_subtype;
	  Entity_Id gnat_ind_base_subtype;
	  tree *gnu_index_type = (tree *) alloca (array_dim * sizeof (tree *));
	  tree gnu_comp_size = 0;
	  tree gnu_max_size = size_one_node;

	  /* First create the gnu types for each index.  */

	  for (index = first_dim, gnat_ind_subtype = First_Index (gnat_entity),
	       gnat_ind_base_subtype
		 = First_Index (Implementation_Base_Type (gnat_entity));
	       index < array_dim && index >= 0;
	       index += next_dim,
	       gnat_ind_subtype = Next_Index (gnat_ind_subtype),
	       gnat_ind_base_subtype = Next_Index (gnat_ind_base_subtype))
	    {
	      tree gnu_index_subtype
		= get_unpadded_type (Etype (gnat_ind_subtype));
	      tree gnu_min
		= convert (sizetype, TYPE_MIN_VALUE (gnu_index_subtype));
	      tree gnu_max
		= convert (sizetype, TYPE_MAX_VALUE (gnu_index_subtype));
	      tree gnu_base_subtype
		= get_unpadded_type (Etype (gnat_ind_base_subtype));
	      tree gnu_base_min
		= convert (sizetype, TYPE_MIN_VALUE (gnu_base_subtype));
	      tree gnu_base_max
		= convert (sizetype, TYPE_MAX_VALUE (gnu_base_subtype));
	      tree gnu_high;

	      /* If the minimum and maximum values both overflow in
		 SIZETYPE, but the difference in the original type
		 does not overflow in SIZETYPE, ignore the overflow
		 indications.  */
	      if ((TYPE_PRECISION (gnu_index_subtype)
		   > TYPE_PRECISION (sizetype))
		  && TREE_CODE (gnu_min) == INTEGER_CST
		  && TREE_CODE (gnu_max) == INTEGER_CST
		  && TREE_OVERFLOW (gnu_min) && TREE_OVERFLOW (gnu_max)
		  && (! TREE_OVERFLOW
		      (size_binop (MINUS_EXPR,
				   TYPE_MAX_VALUE (gnu_index_subtype),
				   TYPE_MIN_VALUE (gnu_index_subtype)))))
		TREE_OVERFLOW (gnu_min) = TREE_OVERFLOW (gnu_max)
		  = TREE_CONSTANT_OVERFLOW (gnu_min)
		  = TREE_CONSTANT_OVERFLOW (gnu_max) = 0;

	      /* Similarly, if the range is null, use bounds of 1..0 for
		 the sizetype bounds.  */
	      else if ((TYPE_PRECISION (gnu_index_subtype)
			> TYPE_PRECISION (sizetype))
		  && TREE_CODE (gnu_min) == INTEGER_CST
		  && TREE_CODE (gnu_max) == INTEGER_CST
		       && (TREE_OVERFLOW (gnu_min) || TREE_OVERFLOW (gnu_max))
		       && tree_int_cst_lt (TYPE_MAX_VALUE (gnu_index_subtype),
					   TYPE_MIN_VALUE (gnu_index_subtype)))
		gnu_min = size_int (1), gnu_max = size_zero_node;

	      gnu_high = size_binop (MINUS_EXPR, gnu_min, size_int (1));

	      /* If the above subtraction overflowed, GNU_MIN must have
		 been the lowest negative number. So GNU_MAX is clearly
		 the one to use.  Otherwise, use the max of it and GNU_MIN.  */
	      if (TREE_CODE (gnu_high) == INTEGER_CST
		  && TREE_OVERFLOW (gnu_high))
		gnu_high = gnu_max;
	      else
		gnu_high = size_binop (MAX_EXPR, gnu_max, gnu_high);

	      gnu_index_type[index]
		= create_index_type (gnu_min, gnu_high, gnu_index_subtype);

	      /* Update the maximum size of the array, in elements. */
	      gnu_max_size
		= size_binop (MULT_EXPR, gnu_max_size,
			      size_binop (PLUS_EXPR, size_one_node,
					  size_binop (MINUS_EXPR, gnu_base_max,
						      gnu_base_min)));
	    }

	  /* Then flatten: create the array of arrays.  */

	  gnu_type = gnat_to_gnu_type (Component_Type (gnat_entity));

	  /* One of the above calls might have caused us to be elaborated,
	     so don't blow up if so.  */
	  if (present_gnu_tree (gnat_entity))
	    {
	      maybe_present = 1;
	      break;
	    }

	  /* Get and validate any specified Component_Size, but if Packed,
	     ignore it since the front end will have taken care of it.  Also,
	     allow sizes not a multiple of Storage_Unit if packed.  */
	  gnu_comp_size = validate_size (Component_Size (gnat_entity),
					 gnu_type, gnat_entity,
					 (Is_Bit_Packed_Array (gnat_entity)
					  ? TYPE_DECL : VAR_DECL),
					 1);

	  /* If the component type is a RECORD_TYPE that has a self-referential
	     size, use the maxium size.  */
	  if (gnu_comp_size == 0 && TREE_CODE (gnu_type) == RECORD_TYPE
	      && TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST
	      && contains_placeholder_p (TYPE_SIZE (gnu_type)))
	    gnu_comp_size = max_size (TYPE_SIZE (gnu_type), 1);

	  if (! Is_Bit_Packed_Array (gnat_entity) && gnu_comp_size != 0)
	    {
	      gnu_type = make_type_from_size (gnu_type, gnu_comp_size, 0);
	      gnu_type = maybe_pad_type (gnu_type, gnu_comp_size, 0,
					 gnat_entity, "C_PAD", 0);
	    }

	  if (Has_Volatile_Components (Base_Type (gnat_entity)))
	    gnu_type = build_type_variant (gnu_type, 0, 1);

	  gnu_max_size
	    = size_binop (MULT_EXPR, gnu_max_size, TYPE_SIZE (gnu_type));

	  /* We don't want any array types shared for two reasons: first,
	     we want to keep differently-named types distinct; second,
	     setting TYPE_MULTI_ARRAY_TYPE of one type can clobber
	     another.  */
	  debug_no_type_hash = 1;
	  for (index = array_dim - 1; index >= 0; index --)
	    {
	      gnu_type = build_array_type (gnu_type, gnu_index_type[index]);
	      TYPE_MULTI_ARRAY_P (gnu_type) = (index > 0);
	    }

	  debug_no_type_hash = 0;
	  TYPE_ALIGN_OK_P (gnu_type) = Is_Packed (gnat_entity);
	  TYPE_CONVENTION_FORTRAN_P (gnu_type)
	    = (Convention (gnat_entity) == Convention_Fortran);

	  /* If our size depends on a placeholder and the maximum size doesn't
	     overflow, use it.  */
	  if (! TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST
	      && contains_placeholder_p (TYPE_SIZE (gnu_type))
	      && TREE_CODE (gnu_max_size) == INTEGER_CST
	      && ! TREE_OVERFLOW (gnu_max_size))
	    TYPE_SIZE (gnu_type) = size_binop (MIN_EXPR, gnu_max_size,
					       TYPE_SIZE (gnu_type));
	}
      break;

    case E_String_Literal_Subtype:
      /* Create the type for a string literal. */
      {
	tree gnu_string_type = get_unpadded_type (Etype (gnat_entity));
	tree gnu_string_array_type
	  = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_string_type))));
	tree gnu_string_index_type
	  = TREE_TYPE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_string_array_type)));
	tree gnu_lower_type
	  = get_unpadded_type (Etype (First_Index (Etype (gnat_entity))));
	tree gnu_lower_bound
	  = convert (integer_type_node,
		     TYPE_MIN_VALUE (gnu_lower_type));
	int length = UI_To_Int (String_Literal_Length (gnat_entity));
	tree gnu_upper_bound
	  = fold (build (PLUS_EXPR, integer_type_node,
			 fold (build (MINUS_EXPR, integer_type_node,
				      build_int_2 (length, 0),
				      integer_one_node)),
			 gnu_lower_bound));
	tree gnu_range_type
	  = build_range_type (gnu_string_index_type,
			      convert (gnu_string_index_type,
				       gnu_lower_bound),
			      convert (gnu_string_index_type,
				       gnu_upper_bound));
	tree gnu_index_type
	  = create_index_type (convert (sizetype,
					TYPE_MIN_VALUE (gnu_range_type)),
			       convert (sizetype,
					TYPE_MAX_VALUE (gnu_range_type)),
			       gnu_range_type);

	gnu_type
	  = build_array_type (gnat_to_gnu_type (Component_Type (gnat_entity)),
			      gnu_index_type);

	/* ??? We compute the size incorrectly (it overflow) if the
	   length is zero, so correct it here.  */
	if (length == 0)
	  TYPE_SIZE (gnu_type) = size_zero_node;
      }
      break;

    case E_Enum_Table_Type:
      /* Create the type for an enumeration literal table.  */
      {
	tree gnu_high_bound = gnat_to_gnu (Table_High_Bound (gnat_entity));
	tree gnu_range_type
	  = build_range_type (TREE_TYPE (gnu_high_bound),
			      convert (TREE_TYPE (gnu_high_bound),
				       integer_zero_node),
			      gnu_high_bound);
	tree gnu_index_type
	  = create_index_type (convert (sizetype,
					TYPE_MIN_VALUE (gnu_range_type)),
			       convert (sizetype,
					TYPE_MAX_VALUE (gnu_range_type)),
			       gnu_range_type);

	/* The table type is an array of thin pointers.  */
	gnu_type = gnat_to_gnu_type (Component_Type (gnat_entity));
	gnu_type
	  = build_pointer_type
	    (TYPE_OBJECT_RECORD_TYPE (TYPE_UNCONSTRAINED_ARRAY (gnu_type)));
	gnu_type = build_array_type (gnu_type, gnu_index_type);
      }
      break;

    /* Record Types and Subtypes

       The following fields are defined on record types:

		Has_Discriminants	True if the record has discriminants
                First_Discriminant      Points to head of list of discriminants
		First_Entity		Points to head of list of fields
		Is_Tagged_Type		True if the record is tagged

       Implementation of Ada records and discriminated records:

       A record type definition is transformed into the equivalent of a C
       struct definition.  The fields that are the discriminants which are
       found in the Full_Type_Declaration node and the elements of the
       Component_List found in the Record_Type_Definition node.  The
       Component_List can be a recursive structure since each Variant of
       the Variant_Part of the Component_List has a Component_List.

       Processing of a record type definition comprises starting the list of
       field declarations here from the discriminants and the calling the
       function components_to_record to add the rest of the fields from the
       component list and return the gnu type node. The function
       components_to_record will call itself recursively as it traverses
       the tree.  */

    case E_Record_Type:
      {
	Entity_Id gnat_impl_type;
        Node_Id full_definition = Declaration_Node (gnat_entity);
        Node_Id record_definition = Type_Definition (full_definition);
	Entity_Id gnat_field;
        tree gnu_field;
	char *field_id;
	tree gnu_field_type;
	tree gnu_field_list = NULL_TREE;
	tree gnu_get_parent;
	int packed = Is_Packed (gnat_entity);
	int has_rep = Has_Specified_Layout (gnat_entity);
	int is_extension
	  = (Is_Tagged_Type (gnat_entity)
	     && Nkind (record_definition) == N_Derived_Type_Definition);

	/* If this is a record extension, go a level further to find the
	   record definition.  Also, verify we have a Parent_Subtype.  */
	if (is_extension)
	  {
	    record_definition = Record_Extension_Part (record_definition);
	    if (No (Parent_Subtype (gnat_entity)))
	      gigi_abort (121);
	  }

	/* Make a node for the record.  If we are not defining the record,
	   suppress expanding incomplete types and save the node as the type
	   for GNAT_ENTITY.  */
	gnu_type = make_dummy_type (gnat_entity);
	TYPE_ALIGN (gnu_type) = 0;
	TYPE_PACKED (gnu_type) = packed | has_rep;

	if (! definition)
	  {
	    defer_incomplete_level++;
	    this_deferred = 1;
	    set_lineno (gnat_entity, 0);
	    gnu_decl = create_type_decl (gnu_entity_id, gnu_type, attr_list);
	    save_gnu_tree (gnat_entity, gnu_decl, 0);
	    this_made_decl = saved = 1;
	  }

	/* If both a size and rep clause was specified, put the size in
	   the record type now so that it can get the proper mode.  */
	if (has_rep && esize != 0)
	  TYPE_SIZE (gnu_type) = UI_To_gnu (Esize (gnat_entity), sizetype);

	/* Always set the alignment here so that it can be used to
	   set the mode, if it is making the alignment stricter.  If
	   it is invalid, it will be checked again below.  If this is to
	   be Atomic, choose a default alignment of a word.  Otherwise,
	   if the size is specified and less than 32, set the alignment
	   to the smallest power of two containing that size.  This makes
	   access to such structures more efficient.  */

	if (Present (Alignment_Clause (gnat_entity)))
	  TYPE_ALIGN (gnu_type)
	    = validate_alignment (Alignment_Clause (gnat_entity),
				  gnat_entity, 0);
	else if (Is_Atomic (gnat_entity))
	  TYPE_ALIGN (gnu_type) = BITS_PER_WORD;
	else if (esize != 0 && esize < 32)
	  TYPE_ALIGN (gnu_type)
	    = MIN (BIGGEST_ALIGNMENT, 1 << (floor_log2 (esize - 1) + 1));

	/* If we have a Parent_Subtype, make a field for the parent.  If
	   this record has rep clauses, force the position to zero.  */
	if (Present (Parent_Subtype (gnat_entity)))
	  {
	    tree gnu_parent;

	    /* A major complexity here is that the parent subtype will
	       reference our discriminants.  But those must reference
	       the parent component of this record.  So here we will
	       initialize each of those components to a COMPONENT_REF.
	       The first operand of that COMPONENT_REF is another
	       COMPONENT_REF which will be filled in below, once
	       the parent type can be safely built.  */

	    gnu_get_parent = build (COMPONENT_REF, NULL_TREE,
				    build (PLACEHOLDER_EXPR, gnu_type),
				    build_decl (FIELD_DECL, NULL_TREE,
						NULL_TREE));

	    if (Has_Discriminants (gnat_entity))
	      for (gnat_field = First_Girder_Discriminant (gnat_entity);
		   Present (gnat_field);
		   gnat_field = Next_Girder_Discriminant (gnat_field))
		if (Present (Corresponding_Discriminant (gnat_field)))
		  save_gnu_tree
		    (gnat_field,
		     build (COMPONENT_REF,
			    get_unpadded_type (Etype (gnat_field)),
			    gnu_get_parent,
			    gnat_to_gnu_entity (Corresponding_Discriminant
						(gnat_field),
						NULL_TREE, 0)),
		     1);

	    gnu_parent = gnat_to_gnu_type (Parent_Subtype (gnat_entity));

	    gnu_field_list
	      = create_field_decl (get_identifier ("PARENT"),
				   gnu_parent, gnu_type, 0,
				   has_rep ? TYPE_SIZE (gnu_parent) : 0,
				   has_rep ? size_zero_node : 0);
	    DECL_INTERNAL_P (gnu_field_list) = 1;

	    TREE_TYPE (gnu_get_parent) = gnu_parent;
	    TREE_OPERAND (gnu_get_parent, 1) = gnu_field_list;
	  }

	/* Add the fields for the discriminants into the record.  */
        if (! Is_Unchecked_Union (gnat_entity)
	    && Has_Discriminants (gnat_entity))
	  for (gnat_field = First_Girder_Discriminant (gnat_entity);
	       Present (gnat_field);
	       gnat_field = Next_Girder_Discriminant (gnat_field))
	    {
	      /* If this is a record extension and this discriminant
		 is the renaming of another discriminant, we've already
		 handled the discriminant above.  */
	      if (Present (Parent_Subtype (gnat_entity))
		  && Present (Corresponding_Discriminant (gnat_field)))
		continue;

	      gnu_field = gnat_to_gnu_field (gnat_field, gnu_type, packed);
	      DECL_DISCRIMINANT_P (gnu_field) = 1;

	      /* Associate the FIELD_DECL node just created with the
		 corresponding gnat defining identifier and add to fields.  */
	      save_gnu_tree (gnat_field, gnu_field, 0);
	      TREE_CHAIN (gnu_field) = gnu_field_list;
	      gnu_field_list = gnu_field;
	    }

	/* Put the discriminants into the record (backwards), so we can
	   know the appropriate discriminant to use for the names of the
	   variants.  */
	TYPE_FIELDS (gnu_type) = gnu_field_list;

	/* Add the listed fields into the record and finish up.  */
	components_to_record (gnu_type, Component_List (record_definition),
			      gnu_field_list, packed, definition, NULL_PTR);

	annotate_rep (gnat_entity, gnu_type);
	TYPE_DUMMY_P (gnu_type) = 0;
	TYPE_VOLATILE (gnu_type) = Is_Volatile (gnat_entity);
	TYPE_BY_REFERENCE_P (gnu_type) = Is_By_Reference_Type (gnat_entity);

	/* If this is an extension type, reset the tree for any
	   inherited discriminants.  */
	if (Present (Parent_Subtype (gnat_entity))
	    && Has_Discriminants (gnat_entity))
	  for (gnat_field = First_Girder_Discriminant (gnat_entity);
	       Present (gnat_field);
	       gnat_field = Next_Girder_Discriminant (gnat_field))
	    if (Present (Corresponding_Discriminant (gnat_field)))
	      save_gnu_tree (gnat_field, NULL_TREE, 0);

	/* If it is a tagged record force the type to BLKmode to insure
	   that these objects will always be placed in memory. Do the
	   same thing for limited record types. */

	if (Is_Tagged_Type (gnat_entity) || Is_Limited_Record (gnat_entity))
	  TYPE_MODE (gnu_type) = BLKmode;
      }
      break;

    case E_Record_Subtype:

      /* Create the gnu subtype from the gnu type by calling
	 substitute_in_type for each discriminant expresion.  This function
	 returns a new tree from the type tree by substituting the discriminant
	 expression for the subtype for the occurences of the discriminant in
	 the base type definition.  We don't see any difference between
	 private and nonprivate type here since derivations from types should
	 have been deferred until the completion of the private type.  */
      {
	Node_Id gnat_discriminant_expr;
	Entity_Id gnat_field;
	Entity_Id gnat_nonprivate_type;
	Entity_Id gnat_base_type;

	if (! definition)
	  defer_incomplete_level++, this_deferred = 1;

	/* Find the innermost nonprivate type, that is not the immediate
	   Full_View of this subtype.  The immediate Full_View follows the
	   subtype and thus hasn't been processed yet.  In essence this
	   Full_View front-end-only. */
	gnat_nonprivate_type = gnat_entity;
	while (1)
	  {
	    if (NOTIN (Ekind (gnat_nonprivate_type),
		       Incomplete_Or_Private_Kind))
	      break;

	    gnat_nonprivate_type
	     = (gnat_nonprivate_type == gnat_entity
		|| No (Full_View (gnat_nonprivate_type))
		? Etype (gnat_nonprivate_type)
		: Full_View (gnat_nonprivate_type));
	  }

	/* Find the gnat_nonprivate_type's innermost nonprivate base type. */
	gnat_base_type = gnat_nonprivate_type;
	while (1)
	  {
	    gnat_base_type = Base_Type (gnat_base_type);

	    if (NOTIN (Ekind (gnat_base_type), Incomplete_Or_Private_Kind))
	      break;

	    gnat_base_type
	      = ((No (Full_View (gnat_base_type)))
		 ? Etype (gnat_base_type)
		 : Full_View (gnat_base_type));
	  }

	/* Get the gnu_type for the underlying type */
	gnu_type
	  = get_unpadded_type (IN (Ekind (gnat_base_type), Record_Kind)
			       ? gnat_base_type
			       : gnat_nonprivate_type);
	if (present_gnu_tree (gnat_entity))
	  {
	    maybe_present = 1;
	    break;
	  }

	/* When the type has discriminants, and these discriminants
	   affect the shape of what it built, factor them in.

	   If we are making a subtype of an Unchecked_Union (must be an
	   Itype), just return the type.

	   We can't just use Is_Constrained because private subtypes without
	   discriminants of full types with discriminants with default
	   expressions are Is_Constrained but aren't constrained!  */

	if (IN (Ekind (gnat_base_type), Record_Kind)
	    && (kind != E_Private_Subtype
		|| ! Is_For_Access_Subtype (gnat_entity))
	    && ! Is_Unchecked_Union (gnat_base_type)
	    && Is_Constrained (gnat_entity)
            && Girder_Constraint (gnat_entity) != No_Elist
            && Present (Discriminant_Constraint (gnat_entity)))
          for (gnat_field
	       = First_Girder_Discriminant (gnat_base_type),
               gnat_discriminant_expr
               = First_Elmt (Girder_Constraint (gnat_entity));
               Present (gnat_field);
               gnat_field = Next_Girder_Discriminant (gnat_field),
               gnat_discriminant_expr = Next_Elmt (gnat_discriminant_expr))
	    /* Ignore access discriminants.  */
	    if (! Is_Access_Type (Etype (Node (gnat_discriminant_expr))))
	      gnu_type
		= gnat_substitute_in_type
		  (gnu_type, gnat_to_gnu_entity (gnat_field, NULL_TREE, 0),
		   elaborate_expression (Node (gnat_discriminant_expr),
					 gnat_entity,
					 get_entity_name (gnat_field),
					 definition, 1));

	/* Define any undefined Is_Itype types in the entity chain.  */
	for (gnat_temp = First_Entity (gnat_entity);
	     Present (gnat_temp); gnat_temp = Next_Entity (gnat_temp))
	  if ((Ekind (gnat_temp) == E_Component
	       || Ekind (gnat_temp) == E_Discriminant)
	      && Is_Itype (Etype (gnat_temp))
	      && ! present_gnu_tree (Etype (gnat_temp)))
	    process_type (Etype (gnat_temp));
      }
      break;

    case E_Access_Subprogram_Type:
      /* If we are not defining this entity, and we have incomplete
	 entities being processed above us, make a dummy type and
	 fill it in later.  */
      if (! definition && defer_incomplete_level != 0)
	{
	  struct incomplete *p
	    = (struct incomplete *) oballoc (sizeof (struct incomplete));

	  gnu_type
	    = build_pointer_type
	      (make_dummy_type (Directly_Designated_Type (gnat_entity)));
	  gnu_decl = create_type_decl (gnu_entity_id, gnu_type, attr_list);
	  save_gnu_tree (gnat_entity, gnu_decl, 0);
	  this_made_decl = saved = 1;

	  p->old_type = TREE_TYPE (gnu_type);
	  p->full_type = Directly_Designated_Type (gnat_entity);
	  p->next = defer_incomplete_list;
	  defer_incomplete_list = p;
	  break;
	}

      /* ... fall through ... */

    case E_Allocator_Type:
    case E_Access_Type:
    case E_Anonymous_Access_Type:
    case E_General_Access_Type:
      {
	Entity_Id gnat_desig_type = Directly_Designated_Type (gnat_entity);
	Entity_Id gnat_desig_full
	  = ((IN (Ekind (Etype (gnat_desig_type)),
		  Incomplete_Or_Private_Kind))
	     ? Full_View (gnat_desig_type) : 0);
	/* We want to know if we'll be seeing the freeze node for any
	   incomplete type we may be pointing to.  */
	int in_main_unit
	  = (Present (gnat_desig_full)
	     ? Entity_Is_In_Main_Unit (gnat_desig_full)
	     : Entity_Is_In_Main_Unit (gnat_desig_type));
	int got_fat_p = 0;
	int made_dummy = 0;

	if (No (gnat_desig_full) && Is_Class_Wide_Type (gnat_desig_type))
	  {
	    if (Present (Equivalent_Type (gnat_desig_type))
		&& IN (Ekind (Equivalent_Type (gnat_desig_type)),
		       Incomplete_Or_Private_Kind))
	      gnat_desig_full = Full_View (Equivalent_Type (gnat_desig_type));
	    else if (IN (Ekind (Root_Type (gnat_desig_type)),
			 Incomplete_Or_Private_Kind))
	      gnat_desig_full = Full_View (Root_Type (gnat_desig_type));
	  }

	if (Present (gnat_desig_full) && Is_Concurrent_Type (gnat_desig_full))
	  gnat_desig_full = Corresponding_Record_Type (gnat_desig_full);

	/* If we are pointing to an incomplete type whose completion is an
	   unconstrained array, make a fat pointer type instead of a pointer
	   to VOID.  The two types in our fields will be pointers to VOID and
	   will be replaced in update_pointer_to.  Similiarly, if the type
	   itself is a dummy type or an unconstrained array.  Also make
	   a dummy TYPE_OBJECT_RECORD_TYPE in case we have any thin
	   pointers to it.  */

	if ((Present (gnat_desig_full)
	     && Is_Array_Type (gnat_desig_full)
	     && ! Is_Constrained (gnat_desig_full))
	    || (present_gnu_tree (gnat_desig_type)
		&& TYPE_IS_DUMMY_P (TREE_TYPE
				     (get_gnu_tree (gnat_desig_type)))
		&& Is_Array_Type (gnat_desig_type)
		&& ! Is_Constrained (gnat_desig_type))
	    || (present_gnu_tree (gnat_desig_type)
		&& (TREE_CODE (TREE_TYPE (get_gnu_tree (gnat_desig_type)))
		    == UNCONSTRAINED_ARRAY_TYPE)
		&& (TYPE_POINTER_TO (TREE_TYPE
				     (get_gnu_tree (gnat_desig_type)))
		    == 0))
	    || (No (gnat_desig_full) && ! in_main_unit
		&& defer_incomplete_level != 0
		&& ! present_gnu_tree (gnat_desig_type)
		&& Is_Array_Type (gnat_desig_type)
		&& ! Is_Constrained (gnat_desig_type)))
	  {
	    tree gnu_old
	      = (present_gnu_tree (gnat_desig_type)
		 ? gnat_to_gnu_type (gnat_desig_type)
		 : make_dummy_type (gnat_desig_type));
	    tree fields;

	    /* Show the dummy we get will be a fat pointer.  */
	    got_fat_p = made_dummy = 1;

	    /* If the call above got something that has a pointer, that
	       pointer is our type.  This could have happened either
	       because the type was elaborated or because somebody
	       else executed the code below.  */
	    gnu_type = TYPE_POINTER_TO (gnu_old);
	    if (gnu_type == 0)
	      {
		gnu_type = make_node (RECORD_TYPE);
		TYPE_UNCONSTRAINED_ARRAY (gnu_type) = gnu_old;
		TYPE_POINTER_TO (gnu_old) = gnu_type;

		set_lineno (gnat_entity, 0);
		fields
		  = chainon (chainon (NULL_TREE,
				      create_field_decl
				      (get_identifier ("P_ARRAY"),
				       ptr_void_type_node, gnu_type, 0, 0, 0)),
			     create_field_decl (get_identifier ("P_BOUNDS"),
						ptr_void_type_node,
						gnu_type, 0, 0, 0));

		/* Make sure we can place this into a register.  */
		TYPE_ALIGN (gnu_type)
		  = MIN (BIGGEST_ALIGNMENT, 2 * POINTER_SIZE);
		TYPE_IS_FAT_POINTER_P (gnu_type) = 1;
		finish_record_type (gnu_type, fields, 0, 1);

		TYPE_OBJECT_RECORD_TYPE (gnu_old) = make_node (VOID_TYPE);
		TYPE_DUMMY_P (TYPE_OBJECT_RECORD_TYPE (gnu_old)) = 1;
	      }
	  }

	/* If we already know what the full type is, use it.  */
	else if (present_gnu_tree (gnat_desig_full))
	  gnu_type
	    = build_pointer_type (TREE_TYPE (get_gnu_tree (gnat_desig_full)));

	/* Get the type of the thing we are to point to and build a pointer
	   to it.  If it is a reference to an incomplete or private type with a
	   full view that is a record, make a dummy type node and get the
	   actual type later when we have verified it is safe.  */
	else if (! in_main_unit
		 && ! present_gnu_tree (gnat_desig_type)
		 && Present (gnat_desig_full)
		 && ! present_gnu_tree (gnat_desig_full)
		 && Is_Record_Type (gnat_desig_full))
	  {
	    gnu_type = build_pointer_type (make_dummy_type (gnat_desig_type));
	    made_dummy = 1;
	  }

	/* Likewise if we are pointing to a record or array and we are to defer
	   elaborating incomplete types.  We do this since this access type
	   may be the full view of some private type.  Note that the 
	   unconstrained array case is handled above. */
	else if (! in_main_unit && defer_incomplete_level != 0
		 && ! present_gnu_tree (gnat_desig_type)
		 && (Is_Record_Type (gnat_desig_type)
		     || Is_Array_Type (gnat_desig_type)))
	  {
	    gnu_type = build_pointer_type (make_dummy_type (gnat_desig_type));
	    made_dummy = 1;
	  }
	else if (gnat_desig_type == gnat_entity)
	  {
	    gnu_type = build_pointer_type (make_node (VOID_TYPE));
	    TREE_TYPE (gnu_type) = TYPE_POINTER_TO (gnu_type) = gnu_type;
	  }
	else
	  gnu_type = build_pointer_type (gnat_to_gnu_type (gnat_desig_type));

	/* It is possible that the above call to gnat_to_gnu_type resolved our
	   type.  If so, just return it.  */
	if (present_gnu_tree (gnat_entity))
	  {
	    maybe_present = 1;
	    break;
	  }

	/* If we are not defining this object and we made a dummy pointer,
	   save our current definition, evaluate the actual type, and replace
	   the tentative type we made with the actual one.  If we are to defer
	   actually looking up the actual type, make an entry in the
	   deferred list.  */

	if (! in_main_unit && made_dummy)
	  {
	    tree gnu_old_type
	      = TYPE_FAT_POINTER_P (gnu_type)
		? TYPE_UNCONSTRAINED_ARRAY (gnu_type) : TREE_TYPE (gnu_type);

	    if (esize < POINTER_SIZE * 2
		&& (got_fat_p || TYPE_FAT_POINTER_P (gnu_type)))
	      gnu_type
		= build_pointer_type
		  (TYPE_OBJECT_RECORD_TYPE
		   (TYPE_UNCONSTRAINED_ARRAY (gnu_type)));

	    gnu_decl = create_type_decl (gnu_entity_id, gnu_type, attr_list);
	    save_gnu_tree (gnat_entity, gnu_decl, 0);
	    this_made_decl = saved = 1;

	    if (defer_incomplete_level == 0)
	      update_pointer_to
		(gnu_old_type, gnat_to_gnu_type (gnat_desig_full));
	    else
	      {
		struct incomplete *p
		  = (struct incomplete *) oballoc (sizeof (struct incomplete));

		p->old_type = gnu_old_type;
		p->full_type = gnat_desig_type;
		p->next = defer_incomplete_list;
		defer_incomplete_list = p;
	      }
	  }

	else
	  {
	    /* For unconstrained arrays we always got a fat pointer above, so
	       if we need a thin pointer, get it now.  Note this works for both
	       incomplete and complete types.  */
	    if (esize < POINTER_SIZE * 2
		&& (got_fat_p || TYPE_FAT_POINTER_P (gnu_type)))
	      gnu_type
		= build_pointer_type (TYPE_OBJECT_RECORD_TYPE
				      (TYPE_UNCONSTRAINED_ARRAY (gnu_type)));
	  }
      }
      break;

    case E_Access_Protected_Subprogram_Type:
      /* The runtime representation is the equivalent type. */
      gnu_type = gnat_to_gnu_type (Equivalent_Type (gnat_entity));
      break;

    case E_Access_Subtype:
      /* We treat this as identical to its base type; any constraint is
	 meaningful only to the front end.  */
      gnu_type = gnat_to_gnu_type (Etype (gnat_entity));
      if (Is_Itype (Directly_Designated_Type (gnat_entity))
	  && ! present_gnu_tree (Directly_Designated_Type (gnat_entity))
	  && No (Freeze_Node (gnat_entity)))
	process_type (Directly_Designated_Type (gnat_entity));

      maybe_present = 1;
      break;

    /* Subprogram Entities

       The following access functions are defined for subprograms (functions
       or procedures):

		First_Formal	The first formal parameter.
		Is_Imported     Indicates that the subprogram has appeared in
				an INTERFACE or IMPORT pragma. For now we
				assume that the external language is C.
		Is_Inlined      True if the subprogram is to be inlined.

       In addition for function subprograms we have:

		Etype       	Return type of the function.

       Each parameter is first checked by calling must_pass_by_ref on its
       type to determine if it is passed by reference.  For parameters which
       are copied in, if they are Ada IN OUT or OUT parameters, their return
       value becomes part of a record which becomes the return type of the
       function (C function - note that this applies only to Ada procedures
       so there is no Ada return type). Additional code to store back the
       parameters will be generated on the caller side.  This transformation
       is done here, not in the front-end.

       The intended result of the transformation can be seen from the
       equivalent source rewritings that follow:

                                                   struct temp {int a,b};
       procedure P (A,B: IN OUT ...) is            temp P (int A,B) {
        ..                                            ..
       end P;                                        return {A,B};
                                                   }
                              procedure call

                                              {
                                                  temp t;
       P(X,Y);                                    t = P(X,Y);
                                                  X = t.a , Y = t.b;
                                              }

       For subprogram types we need to perform mainly the same conversions to
       GCC form that are needed for procedures and function declarations.  The
       only difference is that at the end, we make a type declaration instead
       of a function declaration.  */

    case E_Subprogram_Type:
    case E_Function:
    case E_Procedure:
      {
	/* The first GCC parameter declaration (a PARM_DECL node).  The
	   PARM_DECL nodes are chained through the TREE_CHAIN field, so this
	   actually is the head of this parameter list.  */
	tree gnu_param_list = NULL_TREE;
	/* The type returned by a function. If the subprogram is a procedure
	   this type should be void_type_node.  */
	tree gnu_return_type = void_type_node;
        /* List of fields in return type of procedure with copy in copy out
	   parameters.  */
        tree gnu_field_list = NULL_TREE;
	/* Non-null for subprograms containing  parameters passed by copy in
	   copy out (Ada IN OUT or OUT parameters not passed by reference),
	   in which case it is the list of nodes used to specify the values of
	   the in out/out parameters that are returned as a record upon
	   procedure return.  The TREE_PURPOSE of an element of this list is
	   a field of the record and the TREE_VALUE is the PARM_DECL
	   corresponding to that field.  This list will be saved in the
	   TYPE_CI_CO_LIST field of the FUNCTION_TYPE node we create.  */
	tree gnu_return_list = NULL_TREE;
	Entity_Id gnat_param;
	int inline_flag = Is_Inlined (gnat_entity);
	int public_flag = Is_Public (gnat_entity);
	int extern_flag
	  = ((Is_Public (gnat_entity) && !definition)
	     || Is_Imported (gnat_entity));
	int pure_flag = Is_Pure (gnat_entity);
	int returns_by_ref = 0;
	int returns_unconstrained = 0;
	tree gnu_ext_name = NULL_TREE;
	int has_copy_in_out = 0;
	tree machine_attr = NULL_TREE;
	int parmnum;

	if (kind == E_Subprogram_Type && ! definition)
	  /* A parameter may refer to this type, so defer completion
	     of any incomplete types.  */
	  defer_incomplete_level++, this_deferred = 1;

	/* If the subprogram has an alias, it is probably inherited, so
	   we can use the original one */
	if (Present (Alias (gnat_entity)))
	  {
	    gnu_decl = gnat_to_gnu_entity (Alias (gnat_entity),
					   gnu_expr, 0);
	    break;
	  }

	if (kind == E_Function || kind == E_Subprogram_Type)
	  gnu_return_type = gnat_to_gnu_type (Etype (gnat_entity));

	/* If this function returns by reference, make the actual
	   return type of this function the pointer and mark the decl.  */
	if (Returns_By_Ref (gnat_entity))
	  {
	    returns_by_ref = 1;

	    gnu_return_type = build_pointer_type (gnu_return_type);
	  }

	/* If we are supposed to return an unconstrained array,
	   actually return a fat pointer and make a note of that.  Return
	   a pointer to an unconstrained record of variable size.  */
	else if (TREE_CODE (gnu_return_type) == UNCONSTRAINED_ARRAY_TYPE)
	  {
	    gnu_return_type = TREE_TYPE (gnu_return_type);
	    returns_unconstrained = 1;
	  }

        /* If the type requires a transient scope, the result is allocated
           on the secondary stack, so the result type of the function is
           just a pointer.  */
	else if (Requires_Transient_Scope (Etype (gnat_entity)))
	  {
	    gnu_return_type = build_pointer_type (gnu_return_type);
	    returns_unconstrained = 1;
	  }

	/* If the type is a padded type and the underlying type would not
	   be passed by reference or this function has a foreign convention,
	   return the underlying type.  */
	else if (TREE_CODE (gnu_return_type) == RECORD_TYPE
		 && TYPE_IS_PADDING_P (gnu_return_type)
		 && (! default_pass_by_ref (TREE_TYPE
					    (TYPE_FIELDS (gnu_return_type)))
		     || Has_Foreign_Convention (gnat_entity)))
	  gnu_return_type = TREE_TYPE (TYPE_FIELDS (gnu_return_type));

	/* Look at all our parameters and get the type of
	   each.  While doing this, build a copy-out structure if
	   we need one.  */

	for (gnat_param = First_Formal (gnat_entity), parmnum = 0;
	     Present (gnat_param);
	     gnat_param = Next_Formal_With_Extras (gnat_param), parmnum++)
	  {
	    tree gnu_param_name = get_entity_name (gnat_param);
	    tree gnu_param_type = gnat_to_gnu_type (Etype (gnat_param));
	    tree gnu_param, gnu_field;
	    int by_ref_p = 0;
	    int by_descr_p = 0;
	    int by_component_ptr_p = 0;
	    int copy_in_copy_out_flag = 0;
	    int req_by_copy = 0, req_by_ref = 0;

	    /* See if a Mechanism was supplied that forced this
	       parameter to be passed one way or another.  */
	    if (Is_Valued_Procedure (gnat_entity) && parmnum == 0)
	      req_by_copy = 1;
	    else if (Mechanism (gnat_param) == Default)
	      ;
	    else if (Mechanism (gnat_param) == By_Copy)
	      req_by_copy = 1;
	    else if (Mechanism (gnat_param) == By_Reference)
	      req_by_ref = 1;
	    else if (Mechanism (gnat_param) <= By_Descriptor)
	      by_descr_p = 1;
	    else if (Mechanism (gnat_param) > 0)
	      {
		if (TREE_CODE (gnu_param_type) == UNCONSTRAINED_ARRAY_TYPE
		    || TREE_CODE (TYPE_SIZE (gnu_param_type)) != INTEGER_CST
		    || TREE_INT_CST_HIGH (TYPE_SIZE (gnu_param_type)) != 0
		    || (TREE_INT_CST_LOW (TYPE_SIZE (gnu_param_type))
			> Mechanism (gnat_param)))
		  req_by_ref = 1;
		else
		  req_by_copy = 1;
	      }
	    else
	      post_error ("unsupported mechanism for&", gnat_param);

	    /* If this is either a foreign function or if the
	       underlying type won't be passed by refererence, strip off
	       possible padding type.  */
	    if (TREE_CODE (gnu_param_type) == RECORD_TYPE
		&& TYPE_IS_PADDING_P (gnu_param_type)
		&& (req_by_ref || Has_Foreign_Convention (gnat_entity)
		    || ! must_pass_by_ref (TREE_TYPE (TYPE_FIELDS
						      (gnu_param_type)))))
	      gnu_param_type = TREE_TYPE (TYPE_FIELDS (gnu_param_type));

	    /* If this is an IN parameter it is read-only, so make a variant
	       of the type that is read-only.

	       ??? However, if this is an unconstrained array, that type can
	       be very complex.  So skip it for now.  Likewise for any other
	       self-referential type.  */
	    if (Ekind (gnat_param) == E_In_Parameter
		&& TREE_CODE (gnu_param_type) != UNCONSTRAINED_ARRAY_TYPE
		&& ! (TYPE_SIZE (gnu_param_type) != 0
		      && TREE_CODE (TYPE_SIZE (gnu_param_type)) != INTEGER_CST
		      && contains_placeholder_p (TYPE_SIZE (gnu_param_type))))
	      gnu_param_type
		= build_type_variant (gnu_param_type, 1,
				      TYPE_VOLATILE (gnu_param_type));

	    /* For foreign conventions, pass arrays as a pointer to the
	       underlying type.  First check for unconstrained array and get
	       the underlying array.  Then get the component type and build
	       a pointer to it.  */
	    if (Has_Foreign_Convention (gnat_entity)
		&& TREE_CODE (gnu_param_type) == UNCONSTRAINED_ARRAY_TYPE)
	      gnu_param_type
		= TREE_TYPE (TREE_TYPE (TYPE_FIELDS
					(TREE_TYPE (gnu_param_type))));

	    if (by_descr_p)
	      gnu_param_type
		= build_pointer_type
		  (build_vms_descriptor (gnu_param_type,
					 Mechanism (gnat_param),
					 gnat_entity));

	    else if (Has_Foreign_Convention (gnat_entity)
		     && ! req_by_copy
		     && TREE_CODE (gnu_param_type) == ARRAY_TYPE)
	      {
		/* Strip off any multi-dimensional entries, then strip
		   off the last array to get the component type.  */
		while (TREE_CODE (TREE_TYPE (gnu_param_type)) == ARRAY_TYPE
		       && TYPE_MULTI_ARRAY_P (TREE_TYPE (gnu_param_type)))
		  gnu_param_type = TREE_TYPE (gnu_param_type);

		by_component_ptr_p = 1;
		gnu_param_type = TREE_TYPE (gnu_param_type);

		if (Ekind (gnat_param) == E_In_Parameter)
		  gnu_param_type
		    = build_type_variant (gnu_param_type, 1,
					  TYPE_VOLATILE (gnu_param_type));

		gnu_param_type = build_pointer_type (gnu_param_type);
	      }

	    /* Fat pointers are passed as thin pointers for foreign
	       conventions.  */
	    else if (Has_Foreign_Convention (gnat_entity)
		     && TYPE_FAT_POINTER_P (gnu_param_type))
	      gnu_param_type
		= make_type_from_size (gnu_param_type,
				       size_int (POINTER_SIZE), 0);

            else if (must_pass_by_ref (gnu_param_type)
		     || (Has_Foreign_Convention (gnat_entity)
			 && ! req_by_copy
			 && (Ekind (gnat_param) != E_In_Parameter
			     || AGGREGATE_TYPE_P (gnu_param_type)))
		     || (Convention (gnat_entity) == Convention_Fortran
			 && (INTEGRAL_TYPE_P (gnu_param_type)
			     || FLOAT_TYPE_P (gnu_param_type)))
		     || req_by_ref
		     /* For convention Ada, see if we pass by reference
			by default.  */
		     || (! req_by_copy
			 && ! Has_Foreign_Convention (gnat_entity)
			 && default_pass_by_ref (gnu_param_type)))
	      {
		gnu_param_type = build_pointer_type (gnu_param_type);
		by_ref_p = 1;
	      }

            else if (Ekind (gnat_param) != E_In_Parameter)
	      copy_in_copy_out_flag = 1;

	    if (req_by_copy && (by_ref_p || by_component_ptr_p))
	      post_error ("? cannot pass & by copy", gnat_param);

	    /* If this is an OUT parameter that isn't passed by reference
	       and isn't a pointer or aggregate, we don't make a PARM_DECL
	       for it.  Instead, it will be a VAR_DECL created when we process
	       the procedure.  */
	    if (Ekind (gnat_param) == E_Out_Parameter && ! by_ref_p
		&& ! by_descr_p
		&& ! POINTER_TYPE_P (gnu_param_type)
		&& ! AGGREGATE_TYPE_P (gnu_param_type))
	      gnu_param = 0;
	    else
	      {
		set_lineno (gnat_param, 0);
		gnu_param
		  = create_param_decl
		    (gnu_param_name, gnu_param_type,
		     by_ref_p || by_component_ptr_p
		     || Ekind (gnat_param) == E_In_Parameter);

		DECL_BY_REF_P (gnu_param) = by_ref_p;
		DECL_BY_COMPONENT_PTR_P (gnu_param) = by_component_ptr_p;
		DECL_BY_DESCRIPTOR_P (gnu_param) = by_descr_p;
		DECL_POINTS_TO_READONLY_P (gnu_param)
		  = (Ekind (gnat_param) == E_In_Parameter
		     && (by_ref_p || by_component_ptr_p));
		save_gnu_tree (gnat_param, gnu_param, 0);
		gnu_param_list = chainon (gnu_param, gnu_param_list);

		/* If a parameter is a pointer, this function may modify
		   memory through it and thus shouldn't be considered
		   a pure function.  */
		if ((TREE_CODE (gnu_param_type) == POINTER_TYPE
		     ||  TYPE_FAT_POINTER_P (gnu_param_type))
		    && ! DECL_POINTS_TO_READONLY_P (gnu_param))
		  pure_flag = 0;
	      }

            if (copy_in_copy_out_flag)
	      {
		if (! has_copy_in_out)
		  {
		    if (TREE_CODE (gnu_return_type) != VOID_TYPE)
		      gigi_abort (111);

		    gnu_return_type = make_node (RECORD_TYPE);
		    TYPE_NAME (gnu_return_type) = get_identifier ("RETURN");
		    has_copy_in_out = 1;
		  }

		set_lineno (gnat_param, 0);
		gnu_field = create_field_decl (gnu_param_name, gnu_param_type,
					       gnu_return_type, 0, 0, 0);
		TREE_CHAIN (gnu_field) = gnu_field_list;
		gnu_field_list = gnu_field;
		gnu_return_list = tree_cons (gnu_field, gnu_param,
					     gnu_return_list);
	      }
	  }

	if (gnu_field_list != 0)
	  finish_record_type (gnu_return_type, nreverse (gnu_field_list),
			      0, 0);

	/* If we have a CICO list but it has only one entry, we convert
	   this function into a function that simply returns that one
	   object.  */
	if (list_length (gnu_return_list) == 1)
	  gnu_return_type = TREE_TYPE (TREE_PURPOSE (gnu_return_list));


	if (Convention (gnat_entity) == Convention_Stdcall)
	  {
	    struct attrib *attr
	      = (struct attrib *) oballoc (sizeof (struct attrib));

	    attr->next = attr_list;
	    attr->type = ATTR_MACHINE_ATTRIBUTE;
	    attr->name = get_identifier ("stdcall");
	    attr->arg = NULL_TREE;
	    attr->error_point = gnat_entity;
	    attr_list = attr;
	  }

	/* Both lists ware built in reverse.  */
	gnu_param_list = nreverse (gnu_param_list);
	gnu_return_list = nreverse (gnu_return_list);

	gnu_type
	  = create_subprog_type (gnu_return_type, gnu_param_list,
				 gnu_return_list, returns_unconstrained,
				 returns_by_ref);

	/* We can't call build_type_variant here since it doesn't know
	   about the way we handle types in create_subprog_type, but
	   we don't need the main type being separate anyway.  */
	TYPE_READONLY (gnu_type) = pure_flag;
	TYPE_VOLATILE (gnu_type) = No_Return (gnat_entity);

	/* Top-level or external functions need to have an assembler name.
	   This is passed to create_subprog_decl through the ext_name argument.
	   For Pragma Interface subprograms with no Pragma Interface_Name, the
	   simple name already in entity_name is correct, and this is what is
	   gotten when ext_name is NULL.  If Interface_Name is specified, then
	   the name is extracted from the N_String_Literal node containing the
	   string specified in the Pragma.  If there is no Pragma Interface,
	   then the Ada fully qualified name is created. */

	if (Present (Interface_Name (gnat_entity))
	    || ! (Is_Imported (gnat_entity) || Is_Exported (gnat_entity)))
	  gnu_ext_name = create_concat_name (gnat_entity, NULL_PTR);

	set_lineno (gnat_entity, 0);
        if (kind == E_Subprogram_Type)
          gnu_decl = create_type_decl (gnu_entity_id, gnu_type, attr_list);
        else
	  gnu_decl = create_subprog_decl (gnu_entity_id, gnu_ext_name,
					  gnu_type, gnu_param_list,
					  inline_flag, public_flag,
					  extern_flag, attr_list);
      }
      break;

    case E_Incomplete_Type:
    case E_Private_Type:
    case E_Limited_Private_Type:
    case E_Record_Type_With_Private:
    case E_Private_Subtype:
    case E_Limited_Private_Subtype:
    case E_Record_Subtype_With_Private:

      /* If this type does not have a full view in the unit we are
	 compiling, then just get the type from its Etype.  */
      if (No (Full_View (gnat_entity)))
	{
	  /* If this is an incomplete type with no full view, it must
	     be a Taft Amendement type, so just return a dummy type.  */
	  if (kind == E_Incomplete_Type)
	    gnu_type = make_dummy_type (gnat_entity);
	  else
	    {
	      gnu_type = gnat_to_gnu_type (Etype (gnat_entity));
	      maybe_present = 1;
	    }

	  break;
	}

      /* Otherwise, if we are not defining the type now, get the
	 type from the full view. But always get the type from the full
	 view for define on use types, since otherwise we won't see them! */

      else if (! definition
	       || (Is_Itype (Full_View (gnat_entity))
		   && No (Freeze_Node (gnat_entity)))
	       || (Is_Itype (gnat_entity)
		   && No (Freeze_Node (Full_View (gnat_entity)))))
	{
	  gnu_decl = gnat_to_gnu_entity (Full_View (gnat_entity),
					 NULL_TREE, 0);

	  if (Is_Itype (gnat_entity))
	    maybe_present = 1;
	  else
	    saved = 1;
	  break;
	}

      /* For incomplete types, make a dummy type entry which will be
	 replaced later.  */
      gnu_type = make_dummy_type (gnat_entity);

      /* Save this type as the full declaration's type so we can do any needed
	 updates when we see it.  */
      set_lineno (gnat_entity, 0);
      gnu_decl = create_type_decl (gnu_entity_id, gnu_type, attr_list);
      save_gnu_tree (Full_View (gnat_entity), gnu_decl, 0);
      break;

      /* Simple class_wide types and subtypes are always viewed as their
	 root_type by Gigi unless an Equivalent_Type is specified.  */
    case E_Class_Wide_Type:
    case E_Class_Wide_Subtype:
      if (Present (Equivalent_Type (gnat_entity)))
	gnu_type = gnat_to_gnu_type (Equivalent_Type (gnat_entity));
      else
	gnu_type = gnat_to_gnu_type (Root_Type (gnat_entity));

      maybe_present = 1;
      break;

    case E_Task_Type:
    case E_Task_Subtype:
    case E_Protected_Type:
    case E_Protected_Subtype:
      gnu_type = gnat_to_gnu_type (Corresponding_Record_Type (gnat_entity));

      /* Define any Is_Itype types in the entity chain.

	 ??? Try doing this for Protected types, but not for task types and
	 only for Itypes whose associated node is an
	 N_Entry_Index_Specification.  */
      if (IN (kind, Protected_Kind))
	for (gnat_temp = First_Entity (gnat_entity);
	     Present (gnat_temp); gnat_temp = Next_Entity (gnat_temp))
	  if (IN (Ekind (gnat_temp), Type_Kind)
	      && Is_Itype (gnat_temp)
	      && ! present_gnu_tree (gnat_temp)
	      && (Nkind (Associated_Node_For_Itype (gnat_temp))
		  == N_Entry_Index_Specification))
	    process_type (gnat_temp);

      maybe_present = 1;
      break;

    case E_Label:
      gnu_decl = create_label_decl (gnu_entity_id);
      break;

    case E_Block:
    case E_Loop:
      /* Nothing at all to do here, so just return an ERROR_MARK and claim
	 we've already saved it, so we don't try to.  */
      gnu_decl = error_mark_node;
      saved = 1;
      break;

    default:
      gigi_abort (113);
    }

  /* If we had a case where we evaluated another type and it might have
     defined this one, handle it here.  */
  if (maybe_present && present_gnu_tree (gnat_entity))
    {
      gnu_decl = get_gnu_tree (gnat_entity);
      saved = 1;
    }

  /* If we are processing a type and there is either no decl for it or
     we just made one, do some common processing for the type, such as
     handling alignment and possible padding.  */

  if ((gnu_decl == 0 || this_made_decl) && IN (kind, Type_Kind))
    {
      if (Is_Tagged_Type (gnat_entity))
        TYPE_ALIGN_OK_P (gnu_type) = 1;

      if (AGGREGATE_TYPE_P (gnu_type) && Is_By_Reference_Type (gnat_entity))
	TYPE_BY_REFERENCE_P (gnu_type) = 1;

      if (TREE_CODE (gnu_type) == INTEGER_TYPE
	  && Is_Discrete_Or_Fixed_Point_Type (gnat_entity))
	TYPE_RM_SIZE_INT (gnu_type) 
	  = UI_To_gnu (RM_Size (gnat_entity), sizetype);
      else if (TREE_CODE (gnu_type) == ENUMERAL_TYPE)
	TYPE_RM_SIZE_ENUM (gnu_type) 
	  = UI_To_gnu (RM_Size (gnat_entity), sizetype);

      if (gnu_size == 0)
	gnu_size = validate_size (Esize (gnat_entity), gnu_type, gnat_entity,
				  TYPE_DECL, 0);

      /* If the alignment hasn't already been specified, see if there's
	 an alignment clause.  If not, we pick a default alignment for
	 atomic objects.  */
      if (align != 0)
	;
      else if (Present (Alignment_Clause (gnat_entity)))
	  align
	    = validate_alignment (Alignment_Clause (gnat_entity),
				  gnat_entity, TYPE_ALIGN (gnu_type));
      else if (Is_Atomic (gnat_entity) && gnu_size == 0
	       && integer_pow2p (TYPE_SIZE (gnu_type))
	       && TREE_INT_CST_HIGH (TYPE_SIZE (gnu_type)) == 0)
	align = MIN (BIGGEST_ALIGNMENT,
		     TREE_INT_CST_LOW (TYPE_SIZE (gnu_type)));
      else if (Is_Atomic (gnat_entity) && gnu_size != 0
	       && integer_pow2p (gnu_size)
	       && TREE_INT_CST_HIGH (gnu_size) == 0)
	align = MIN (BIGGEST_ALIGNMENT, TREE_INT_CST_LOW (gnu_size));

      gnu_type = maybe_pad_type (gnu_type, gnu_size, align, 
				 gnat_entity, "PAD", 1);
      gnu_type = build_type_variant (gnu_type, 0, Is_Volatile (gnat_entity));

      if (Is_Atomic (gnat_entity))
	check_ok_for_atomic (gnu_type, gnat_entity);

      /* If this is the type used for an aliased variable whose nominal subtype
	 is unconstrained, make a record that contains the template.  */
      if (Is_Constr_Subt_For_UN_Aliased (gnat_entity)
	  && (kind == E_Array_Subtype || kind == E_String_Subtype))
	{
	  tree gnu_fat = TREE_TYPE (gnat_to_gnu_type
				    (Base_Type (Etype (gnat_entity))));
	  tree gnu_temp_type
	    = TREE_TYPE (TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (gnu_fat))));

	  gnu_type = build_unc_object_type (gnu_temp_type, gnu_type,
					    gnu_entity_id);
	}

      if (gnu_decl == 0)
	{
	  set_lineno (gnat_entity, 0);
	  gnu_decl = create_type_decl (gnu_entity_id, gnu_type, attr_list);
	}
      else
	TREE_TYPE (gnu_decl) = gnu_type;
    }

  if (! Comes_From_Source (gnat_entity)
      && TREE_CODE_CLASS (TREE_CODE (gnu_decl)) == 'd')
    DECL_ARTIFICIAL (gnu_decl) = 1;

  /* If we haven't already, associate the ..._DECL node that we just made with
     the input GNAT entity node. */
  if (! saved)
    save_gnu_tree (gnat_entity, gnu_decl, 0);

  /* If this is an enumeral or floating-point type, we were not able to set
     the bounds since they refer to the type.  These bounds are always static.

     For enumeration types, also write debugging information and declare the
     enumeration literal  table, if needed.  */

  if ((kind == E_Enumeration_Type && Present (First_Literal (gnat_entity)))
      || kind == E_Floating_Point_Type)
    {
      TYPE_MIN_VALUE (gnu_type) = gnat_to_gnu (Type_Low_Bound (gnat_entity));
      TYPE_MAX_VALUE (gnu_type) = gnat_to_gnu (Type_High_Bound (gnat_entity));

      if (kind == E_Enumeration_Type)
	{
	  TYPE_STUB_DECL (gnu_type) = gnu_decl;
	  rest_of_type_compilation (gnu_type, global_bindings_p ());

	  if (definition && Present (Lit_Name_Table (gnat_entity)))
	    gnat_to_gnu_entity
	      (Lit_Name_Table (gnat_entity),
	       create_enum_initializer
	       (gnat_entity,
		gnat_to_gnu_type (Etype (Lit_Name_Table (gnat_entity)))),
	       1);
	}
    }

  /* If we deferred processing of incomplete types, re-enable it.  If there
     were no other disables and we have some to process, do so.  */
  if (this_deferred && --defer_incomplete_level == 0
      && defer_incomplete_list != 0)
    {
      struct incomplete *p = defer_incomplete_list;

      defer_incomplete_list = 0;
      for (; p; p = p->next)
	update_pointer_to (p->old_type, gnat_to_gnu_type (p->full_type));
    }

  /* Restore our previous allocation, if not previously permanent and we
     changed it.  */
  if ((! definition || Is_Imported (gnat_entity)) && ! was_permanent)
    {
      pop_obstacks ();

      if (Is_Public (gnat_entity))
	force_global--;
    }

  resume_momentary (was_momentary);

  if (Is_Packed_Array_Type (gnat_entity)
      && Is_Itype (Associated_Node_For_Itype (gnat_entity))
      && ! present_gnu_tree (Associated_Node_For_Itype (gnat_entity)))
    process_type (Associated_Node_For_Itype (gnat_entity));

  return gnu_decl;
}

/* Given GNAT_ENTITY, elaborate all expressions that are required to
   be elaborated at the point of its definition, but do nothing else.  */

void
elaborate_entity (gnat_entity)
     Entity_Id gnat_entity;
{
  switch (Ekind (gnat_entity))
    {
    case E_Signed_Integer_Subtype:
    case E_Modular_Integer_Subtype:
    case E_Enumeration_Subtype:
    case E_Ordinary_Fixed_Point_Subtype:
    case E_Decimal_Fixed_Point_Subtype:
    case E_Floating_Point_Subtype:
      {
	Node_Id gnat_lb = Type_Low_Bound (gnat_entity);
	Node_Id gnat_hb = Type_High_Bound (gnat_entity);

	/* ??? tests for avoiding static constaint error expression
	   is needed until the front stops to generate bogus conversions 
	   on bounds of real types */

	if (! Raises_Constraint_Error (gnat_lb))
	  elaborate_expression (gnat_lb, gnat_entity, get_identifier ("L"),
				1, 0);
	if (! Raises_Constraint_Error (gnat_lb))
	  elaborate_expression (gnat_hb, gnat_entity, get_identifier ("U"),
				1, 0);
      break;
      }

    case E_Record_Type:
      {
        Node_Id full_definition = Declaration_Node (gnat_entity);
	Node_Id record_definition = Type_Definition (full_definition);
	Entity_Id gnat_impl_type;

	/* If this is a record extension, go a level further to find the
	   record definition */
	if (Nkind (record_definition) == N_Derived_Type_Definition)
	  record_definition = Record_Extension_Part (record_definition);
      }
      break;

    case E_Record_Subtype:
    case E_Private_Subtype:
    case E_Limited_Private_Subtype:
    case E_Record_Subtype_With_Private:
      if (Is_Constrained (gnat_entity)
          && Has_Discriminants (Base_Type (gnat_entity))
          /* beware private types without discrims of full types with */
	  && Present (Discriminant_Constraint (gnat_entity)))
	{
	  Node_Id gnat_discriminant_expr;
	  Entity_Id gnat_field;

	  for (gnat_field = First_Discriminant (Base_Type (gnat_entity)),
	       gnat_discriminant_expr
	       = First_Elmt (Discriminant_Constraint (gnat_entity));
	       Present (gnat_field);
	       gnat_field = Next_Discriminant (gnat_field),
	       gnat_discriminant_expr = Next_Elmt (gnat_discriminant_expr))
	    /* ??? For now, ignore access discriminants.  */
	    if (! Is_Access_Type (Etype (Node (gnat_discriminant_expr))))
	      elaborate_expression (Node (gnat_discriminant_expr),
				    gnat_entity,
				    get_entity_name (gnat_field), 1, 0);
	}
      break;

    }
}

/* For the following two functions: for each GNAT entity, the GCC
   tree node used as a dummy for that entity, if any.  */

static tree *dummy_node_table;

/* Initialize the above table.  */

void
init_dummy_type ()
{
  Node_Id gnat_node;

  dummy_node_table = (tree *) xmalloc (max_gnat_nodes * sizeof (tree));

  for (gnat_node = 0; gnat_node < max_gnat_nodes; gnat_node++)
    dummy_node_table[gnat_node] = NULL_TREE;

  dummy_node_table -= First_Node_Id;
}

/* Make a dummy type corresponding to GNAT_TYPE.  */

tree
make_dummy_type (gnat_type)
     Entity_Id gnat_type;
{
  Entity_Id gnat_underlying;
  tree gnu_type;
  int was_permanent = ! allocation_temporary_p ();

  /* Find a full type for GNAT_TYPE, taking into account any class wide
     types.  */
  if (Is_Class_Wide_Type (gnat_type))
    gnat_type = (Present (Equivalent_Type (gnat_type))
		 ? Equivalent_Type (gnat_type)
		 : Root_Type (gnat_type));

  for (gnat_underlying = gnat_type;
       (IN (Ekind (gnat_underlying), Incomplete_Or_Private_Kind)
	&& Present (Full_View (gnat_underlying)));
       gnat_underlying = Full_View (gnat_underlying))
    ;

  /* If it there already a dummy type, use that one.  Else make one.  */
  if (dummy_node_table[gnat_underlying])
    return dummy_node_table[gnat_underlying];

  if (! was_permanent && Is_Public (gnat_underlying))
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();
    }

  /* If this is a record, make this a RECORD_TYPE or UNION_TYPE; else make
     it a VOID_TYPE.  */
  if (Is_Record_Type (gnat_underlying))
    gnu_type = make_node (Is_Unchecked_Union (gnat_underlying)
			  ? UNION_TYPE : RECORD_TYPE);
  else
    gnu_type = make_node (VOID_TYPE);

  TYPE_NAME (gnu_type) = get_entity_name (gnat_type);
  if (AGGREGATE_TYPE_P (gnu_type))
    TYPE_STUB_DECL (gnu_type)
      = pushdecl (build_decl (TYPE_DECL, NULL_TREE, gnu_type));

  TYPE_DUMMY_P (gnu_type) = 1;
  dummy_node_table[gnat_underlying] = gnu_type;

  if (! was_permanent && Is_Public (gnat_underlying))
    pop_obstacks ();

  return gnu_type;
}

/* Return a list of attributes for GNAT_ENTITY, if any.  */

static struct attrib *
build_attr_list (gnat_entity)
     Entity_Id gnat_entity;
{
  struct attrib *attr_list = 0;
  Node_Id gnat_temp;

  for (gnat_temp = First_Rep_Item (gnat_entity); Present (gnat_temp);
       gnat_temp = Next_Rep_Item (gnat_temp))
    if (Nkind (gnat_temp) == N_Pragma)
      {
	struct attrib *attr;
	tree gnu_arg0 = 0, gnu_arg1 = 0;
	Node_Id gnat_assoc = Pragma_Argument_Associations (gnat_temp);
	enum attr_type etype;

	if (Present (gnat_assoc) && Present (First (gnat_assoc))
	    && Present (Next (First (gnat_assoc)))
	    && (Nkind (Expression (Next (First (gnat_assoc))))
		== N_String_Literal))
	  {
	    gnu_arg0 = get_identifier (TREE_STRING_POINTER
				       (gnat_to_gnu
					(Expression (Next
						     (First (gnat_assoc))))));
	    if (Present (Next (Next (First (gnat_assoc))))
		&& (Nkind (Expression (Next (Next (First (gnat_assoc)))))
		    == N_String_Literal))
	      gnu_arg1 = get_identifier (TREE_STRING_POINTER
					 (gnat_to_gnu
					  (Expression
					   (Next (Next
						  (First (gnat_assoc)))))));
	  }

	switch (Get_Pragma_Id (Chars (gnat_temp)))
	  {
	  case Pragma_Machine_Attribute:
	    etype = ATTR_MACHINE_ATTRIBUTE;
	    break;

	  case Pragma_Linker_Alias:
	    etype = ATTR_LINK_ALIAS;
	    break;

	  case Pragma_Linker_Section:
	    etype = ATTR_LINK_SECTION;
	    break;

	  case Pragma_Weak_External:
	    etype = ATTR_WEAK_EXTERNAL;
	    break;

	  default:
	    continue;
	  }

	attr = (struct attrib *) oballoc (sizeof (struct attrib));
	attr->next = attr_list;
	attr->type = etype;
	attr->name = gnu_arg0;
	attr->arg = gnu_arg1;
	attr->error_point
	  = Present (Next (First (gnat_assoc)))
	    ? Expression (Next (First (gnat_assoc))) : gnat_temp;
	attr_list = attr;
      }

  return attr_list;
}

/* EXP may be a FIELD_DECL.  If so, make the appropriate COMPONENT_REF
   involving a PLACEHOLDER_EXPR.

   This function must be called whenever we have something that is allowed to
   be a discriminant.  */

static tree
maybe_placeholder (exp)
     tree exp;
{
  if (TREE_CODE (exp) == FIELD_DECL)
    return build (COMPONENT_REF, TREE_TYPE (exp),
		  build (PLACEHOLDER_EXPR, DECL_CONTEXT (exp)),
		  exp);

  return exp;
}

/* Get the unpadded version of a GNAT type.  */

tree
get_unpadded_type (gnat_entity)
     Entity_Id gnat_entity;
{
  tree type = gnat_to_gnu_type (gnat_entity);

  if (TREE_CODE (type) == RECORD_TYPE && TYPE_IS_PADDING_P (type))
    type = TREE_TYPE (TYPE_FIELDS (type));

  return type;
}

/* Called when we need to protect a variable object using a save_expr.  */

tree
maybe_variable (gnu_operand, gnat_node)
     tree gnu_operand;
     Node_Id gnat_node;
{
  if (TREE_CONSTANT (gnu_operand))
    return gnu_operand;

  /* If we will be generating code, make sure we are at the proper
     line number.  */
  if (! global_bindings_p () && ! TREE_CONSTANT (gnu_operand)
      && ! contains_placeholder_p (gnu_operand))
  set_lineno (gnat_node, 1);

  if (TREE_CODE (gnu_operand) == UNCONSTRAINED_ARRAY_REF)
    return build1 (UNCONSTRAINED_ARRAY_REF, TREE_TYPE (gnu_operand),
		   variable_size (TREE_OPERAND (gnu_operand, 0)));
  else
    return variable_size (gnu_operand);
}

/* Given a GNAT tree GNAT_EXPR, for an expression which is a value within a
   type definition (either a bound or a discriminant value) for GNAT_ENTITY,
   return the GCC tree to use for that expression.  GNU_NAME is the
   qualification to use if an external name is appropriate and DEFINITION is
   nonzero if this is a definition of GNAT_ENTITY.  If NEED_VALUE is nonzero,
   we need a result.  Otherwise, we are just elaborating this for
   side-effects.  */

static tree
elaborate_expression (gnat_expr, gnat_entity, gnu_name, definition, need_value)
     Node_Id gnat_expr;
     Entity_Id gnat_entity;
     tree gnu_name;
     int definition;
     int need_value;
{
  tree gnu_expr;

  /* If we already elaborated this expression (e.g., it was involved
     in the definition of a private type), use the old value.  */
  if (present_gnu_tree (gnat_expr))
    return get_gnu_tree (gnat_expr);

  /* If we don't need a value and this is static or a discriment, we
     don't need to do anything.  */
  else if (! need_value
      && (Is_Static_Expression (gnat_expr)
	  || (Nkind (gnat_expr) == N_Identifier
	      && Ekind (Entity (gnat_expr)) == E_Discriminant)))
    return 0;

  /* Otherwise, convert this tree to its GCC equivalant, handling any
     references to a discriminant.  */
  gnu_expr = maybe_placeholder (gnat_to_gnu (gnat_expr));

  /* If this entity is defined at top level and a bound or discriminant
     value isn't a constant or a reference to a discriminant, replace the
     bound by a variable that will be initialized to contain the bound when
     the package containing the definition is elaborated.  Note that we rely
     here on the fact that an expression cannot contain both the discriminant
     and some other variable.  */

  if ((Is_Public (gnat_entity) || global_bindings_p ())
      && ! TREE_CONSTANT (gnu_expr) && ! contains_placeholder_p (gnu_expr))
    gnu_expr
      = create_var_decl (create_concat_name (gnat_entity,
					     IDENTIFIER_POINTER (gnu_name)),
			 NULL_PTR, TREE_TYPE (gnu_expr), gnu_expr, 1,
			 Is_Public (gnat_entity), ! definition, 0, NULL_PTR);
  else
    gnu_expr = maybe_variable (gnu_expr, gnat_expr);

  /* Save the expression in case we try to elaborate this entity again.
     Since this is not a DECL, don't check it.  If this is a constant,
     don't save it since GNAT_EXPR might be used more than once.  Also,
     don't save if it's a discriminant.  */
  if (! TREE_CONSTANT (gnu_expr) && ! contains_placeholder_p (gnu_expr))
    save_gnu_tree (gnat_expr, gnu_expr, 1);

  return gnu_expr;
}

/* Ensure that TYPE has SIZE and ALIGN.  Make and return a new padded type
   if needed.  We have already verified that SIZE and TYPE are large enough.

   GNAT_ENTITY and NAME_TRAILER are used to name the resulting record and
   to issue a warning.

   IN_PLACE is nonzero if we can modify the type in place; otherwise
   we must make a new one to make any changes to the size and alignment.  */

tree
maybe_pad_type (type, size, align, gnat_entity, name_trailer, in_place)
     tree type;
     tree size;
     int align;
     Entity_Id gnat_entity;
     char *name_trailer;
     int in_place;
{
  tree type_size = rm_size (type);
  tree orig_size = TYPE_SIZE (type);

  /* If TYPE is a padded type, see if it agrees with any size and alignment
     we were given.  If so, return the original type.  Otherwise, strip
     off the padding, since we will either be returning the inner type
     or repadding it.  */

  if (TREE_CODE (type) == RECORD_TYPE && TYPE_IS_PADDING_P (type))
    {
      if ((size == 0
	   || operand_equal_p (size, round_up (TYPE_SIZE (type),
					       MAX (align, TYPE_ALIGN (type))),
			       0))
	  && (align == 0 || align == TYPE_ALIGN (type)))
	return type;

      type = TREE_TYPE (TYPE_FIELDS (type));
      orig_size = TYPE_SIZE (type);
    }

  /* If the size is either not being changed or is being made smaller (which
     is not done here (and is only valid for bitfields anyway), show the size
     isn't changing.  Likewise, clear the alignment if it isn't being
     changed.  Then return if we aren't doing anything.  */

  if (size != 0
      && (operand_equal_p (size, TYPE_SIZE (type), 0)
	  || (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
	      && tree_int_cst_lt (size, TYPE_SIZE (type)))))
    size = 0;

  if (align == TYPE_ALIGN (type))
    align = 0;

  if (align == 0 && size == 0)
    return type;

  /* Handle the case where we can just modify this in place.  This is only
     for records and, to keep it simple, only if the record has BLKmode.  */

  if (in_place && size != 0 && TREE_CODE (type) == RECORD_TYPE
      && ! TYPE_IS_FAT_POINTER_P (type)
      && ! TYPE_LEFT_JUSTIFIED_MODULAR_P (type)
      && TYPE_MODE (type) == BLKmode)
    {
      if (align != 0)
	TYPE_ALIGN (type) = align;

      if (size != 0)
	{
	  TYPE_ADA_SIZE (type) = size;

#ifdef ROUND_TYPE_SIZE
	  TYPE_SIZE (type)
	    = ROUND_TYPE_SIZE (type, TYPE_ADA_SIZE (type), TYPE_ALIGN (type));
#else
	  TYPE_SIZE (type)
	    = round_up (TYPE_ADA_SIZE (type), TYPE_ALIGN (type));
#endif
	}
    }
  else
    {
      tree record = make_node (RECORD_TYPE);
      tree field = create_field_decl (get_identifier ("F"), type, record, 0,
				      NULL_TREE, size_zero_node);

      if (Present (gnat_entity))
	TYPE_NAME (record) = create_concat_name (gnat_entity, name_trailer);

      TYPE_SIZE (record) = size != 0 ? size : type_size;
      TYPE_ALIGN (record) = align;
      TYPE_IS_PADDING_P (record) = 1;
      finish_record_type (record, field, 1, 0);

      if (size != 0)
	TYPE_ADA_SIZE (record) = size;
      else
	/* We made this extension for alignment purposes, so set the Ada size
	   value to what we would have had were it not for the
	   alignment. */
	TYPE_ADA_SIZE (record) = type_size;

      type = record;
    }

  if (TREE_CODE (orig_size) != INTEGER_CST
      && contains_placeholder_p (orig_size))
    orig_size = max_size (orig_size, 1);

  /* If the size was widened explicitly, maybe give a warning.  */
  if (size != 0 && Present (gnat_entity)
      && ! operand_equal_p (size, orig_size, 0))
    {
      Node_Id gnat_error_node = Empty;

      if (Is_Packed_Array_Type (gnat_entity))
	gnat_entity = Associated_Node_For_Itype (gnat_entity);

      if ((Ekind (gnat_entity) == E_Component
	   || Ekind (gnat_entity) == E_Discriminant)
	  && Present (Component_Clause (gnat_entity)))
	gnat_error_node = Last_Bit (Component_Clause (gnat_entity));
      else if (Present (Size_Clause (gnat_entity)))
	gnat_error_node = Expression (Size_Clause (gnat_entity));

      /* Generate message only for entities that come from source, since
	 if we have an entity created by expansion, the message will be
	 generated for some other corresponding source entity.  */
      if (Comes_From_Source (gnat_entity) && Present (gnat_error_node))
	post_error_ne_tree ("{^ }bits of & unused?", gnat_error_node,
			    gnat_entity,
			    size_binop (MINUS_EXPR, size, orig_size));

      else if (*name_trailer == 'C' && ! Is_Internal (gnat_entity))
	post_error_ne_tree ("component of& padded{ by ^ bits}?",
			    gnat_entity, gnat_entity,
			    size_binop (MINUS_EXPR, size, orig_size));
    }

  return type;
}

/* Given a GNU tree and a GNAT list of choices, generate an expression to test
   the value passed against the list of choices.  If STRING is non-zero,
   concatenate a discription of the choice to it.  Set STRING[0] to zero
   if we can't.  */

tree
choices_to_gnu (operand, choices, string)
     tree operand;
     Node_Id choices;
     char *string;
{
  Node_Id choice;
  Node_Id gnat_temp;
  tree result = integer_zero_node;
  tree this_test, low = 0, high = 0, single = 0;
  char *prefix;

  for (choice = First (choices); Present (choice); choice = Next (choice))
    {
      switch (Nkind (choice))
	{
	case N_Range:
	  low = gnat_to_gnu (Low_Bound (choice));
	  high = gnat_to_gnu (High_Bound (choice));

	  /* There's no good type to use here, so we might as well use
	     integer_type_node.  */
	  this_test
	    = build_binary_op (TRUTH_ANDIF_EXPR, integer_type_node,
			       build_binary_op (GE_EXPR, integer_type_node,
						operand, low),
			       build_binary_op (LE_EXPR, integer_type_node,
						operand, high));

	  break;

        case N_Subtype_Indication:
	  gnat_temp = Range_Expression (Constraint (choice));
	  low = gnat_to_gnu (Low_Bound (gnat_temp));
	  high = gnat_to_gnu (High_Bound (gnat_temp));

	  this_test
	    = build_binary_op (TRUTH_ANDIF_EXPR, integer_type_node,
			       build_binary_op (GE_EXPR, integer_type_node,
						operand, low),
			       build_binary_op (LE_EXPR, integer_type_node,
						operand, high));
          break;

	case N_Identifier:
        case N_Expanded_Name:
	  /* This represents either a subtype range, an enumeration
	     literal, or a constant  Ekind says which.  If an enumeration 
             literal or constant, fall through to the next case.  */
	  if (Ekind (Entity (choice)) != E_Enumeration_Literal
              && Ekind (Entity (choice)) != E_Constant)
	    {
	      tree type = gnat_to_gnu_type (Entity (choice));

	      low = TYPE_MIN_VALUE (type);
	      high = TYPE_MAX_VALUE (type);

	      this_test
		= build_binary_op (TRUTH_ANDIF_EXPR, integer_type_node,
				   build_binary_op (GE_EXPR, integer_type_node,
						    operand, low),
				   build_binary_op (LE_EXPR, integer_type_node,
						    operand, high));
	      break;
	    }
	  /* ... fall through ... */
	case N_Character_Literal:
	case N_Integer_Literal:
	  single = gnat_to_gnu (choice);
	  this_test = build_binary_op (EQ_EXPR, integer_type_node, operand,
				       single);
	  break;

	case N_Others_Choice:
	  this_test = integer_one_node;
	  break;

	default:
	  gigi_abort (114);
	}

      result = build_binary_op (TRUTH_ORIF_EXPR, integer_type_node,
				result, this_test);

      /* Make up the name for the string corresponding to this choice.
	 Concatenate all previous options.  If too long, say we can't
	 give the name.  */
      if (string != 0 && strlen (string)  > 600)
	{
	  string[0] = 0;
	  string = 0;
	}

      if (string)
	{
	  char *prefix = (char *) alloca (strlen (string) + 1);
	  strcpy (prefix, string);

	  if (single) single = fold (single);
	  if (low) low = fold (low);
	  if (high) high = fold (high);

	  if (single != 0 && TREE_CODE (single) == INTEGER_CST
	      && (TREE_INT_CST_HIGH (single) == 0
		  || TREE_INT_CST_HIGH (single) == -1))
	    sprintf (string,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		     "%sS%u",
#else
		     "%sS%lu",
#endif
		     prefix, TREE_INT_CST_LOW (single));
	  else if (low != 0 && high != 0 && TREE_CODE (low) == INTEGER_CST
		   && TREE_CODE (high) == INTEGER_CST
		   && (TREE_INT_CST_HIGH (low) == 0
		       || TREE_INT_CST_HIGH (low) == -1)
		   && (TREE_INT_CST_HIGH (high) == 0
		       || TREE_INT_CST_HIGH (high) == -1))
	    sprintf (string,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		     "%sR%uT%u",
#else
		     "%sR%luT%lu",
#endif
		     prefix, TREE_INT_CST_LOW (low), TREE_INT_CST_LOW (high));
	  else if (single == 0 && low == 0 && high == 0)
	    sprintf (string, "%sO", prefix);
	  else
	    {
	      string[0] = 0;
	      string = 0;
	    }
	}
    }

  return result;
}

/* Return a GCC tree for a field corresponding to GNAT_FIELD to be
   placed in GNU_RECORD_TYPE.

   PACKED is nonzero if the enclosing record is packed.  */

static tree
gnat_to_gnu_field (gnat_field, gnu_record_type, packed)
     Entity_Id gnat_field;
     tree gnu_record_type;
     int packed;
{
  tree gnu_field_id = get_entity_name (gnat_field);
  tree gnu_field_type = gnat_to_gnu_type (Etype (gnat_field));
  tree gnu_pos = 0;
  tree gnu_size = 0;
  int needs_strict_alignment
    = Is_Aliased (gnat_field) || Strict_Alignment (Etype (gnat_field))
      || Is_Volatile (gnat_field);

  /* If this field requires strict alignment pretend it isn't packed.  */
  if (needs_strict_alignment)
    packed = 0;

  /* For packed records, this is one of the few occasions on which we use
     the official RM size for discrete or fixed-point components, instead
     of the normal GNAT size stored in Esize. See description in Einfo:
     "Handling of Type'Size Values" for further details.  */

  if (packed && Is_Discrete_Or_Fixed_Point_Type (Etype (gnat_field)))
    gnu_size = validate_size (RM_Size (Etype (gnat_field)), gnu_field_type,
				gnat_field, FIELD_DECL, 0);

  else if (Present (Esize (gnat_field)))
    gnu_size = validate_size (Esize (gnat_field), gnu_field_type,
			      gnat_field, FIELD_DECL, 0);

  if (Present (Component_First_Bit (gnat_field)))
    {
      gnu_pos = UI_To_gnu (Component_First_Bit (gnat_field), sizetype);
      gnu_size = validate_size (Esize (gnat_field), gnu_field_type,
				gnat_field, FIELD_DECL, 0);

      /* Ensure the position does not overlap with the parent subtype,
	 if there is one.  */
      if (Present (Parent_Subtype (Underlying_Type (Scope (gnat_field)))))
	{
	  tree gnu_parent
	    = gnat_to_gnu_type (Parent_Subtype
				(Underlying_Type (Scope (gnat_field))));

	  if (TREE_CODE (TYPE_SIZE (gnu_parent)) == INTEGER_CST
	      && tree_int_cst_lt (gnu_pos, TYPE_SIZE (gnu_parent)))
	    {
	      post_error_ne_tree
		("offset of& must be beyond parent{, minimum allowed is ^}",
		 Position (Component_Clause (gnat_field)), gnat_field,
		 size_binop (CEIL_DIV_EXPR, TYPE_SIZE (gnu_parent),
			     size_int (BITS_PER_UNIT)));
	    }
	}

      /* If this field needs strict alignment, ensure the record is
	 sufficiently aligned and that that position and size are 
	 consistent with the alignment.  */
      if (needs_strict_alignment)
	{
	  tree gnu_min_size = round_up (rm_size (gnu_field_type),
					TYPE_ALIGN (gnu_field_type));

	  TYPE_ALIGN (gnu_record_type)
	    = MAX (TYPE_ALIGN (gnu_record_type), TYPE_ALIGN (gnu_field_type));

	  if ((Is_Atomic (gnat_field) || Is_Atomic (Etype (gnat_field)))
	      && ! operand_equal_p (gnu_size, TYPE_SIZE (gnu_field_type), 0))
	    {
	      post_error_ne_tree
		("atomic field& must be natural size of type{ (^)}",
		 Last_Bit (Component_Clause (gnat_field)), gnat_field,
		 TYPE_SIZE (gnu_field_type));

	      gnu_size = gnu_pos = 0;
	    }

	  else if (! integer_zerop (size_binop (TRUNC_MOD_EXPR, gnu_pos,
						size_int (TYPE_ALIGN
							  (gnu_field_type)))))
	    {
	      if (Is_Aliased (gnat_field))
		post_error_ne_num
		  ("position of aliased field& must be multiple of ^ bits",
		   Component_Clause (gnat_field), gnat_field,
		   TYPE_ALIGN (gnu_field_type));

	      else if (Is_Volatile (gnat_field))
		post_error_ne_num
		  ("position of volatile field& must be multiple of ^ bits",
		   Component_Clause (gnat_field), gnat_field,
		   TYPE_ALIGN (gnu_field_type));

	      else
		post_error_ne_num
  ("position of field& with aliases components must be multiple of ^ bits",
		   Component_Clause (gnat_field), gnat_field,
		   TYPE_ALIGN (gnu_field_type));

	      gnu_size = gnu_pos = 0;
	    }

	  else if (tree_int_cst_lt (gnu_size, gnu_min_size))
	    {
	      if (Is_Aliased (gnat_field))
		post_error_ne_tree
		  ("size of aliased field& too small{, minimum required is ^}",
		   Last_Bit (Component_Clause (gnat_field)), gnat_field,
		   gnu_min_size);

	      else if (Is_Volatile (gnat_field))
		post_error_ne_tree
 ("size of volatile field& too small{, minimum required is ^}",
		   Last_Bit (Component_Clause (gnat_field)), gnat_field,
		   gnu_min_size);

	      else
		post_error_ne_tree
 ("size of field& with aliased components too small{, minimum required is ^}",
		   Last_Bit (Component_Clause (gnat_field)), gnat_field,
		   gnu_min_size);

	      gnu_size = 0;
	    }
	}

      if (Is_Atomic (gnat_field))
	check_ok_for_atomic (gnu_field_type, gnat_field);

      if (TYPE_MODE (gnu_field_type) == BLKmode
	  && (! integer_zerop (size_binop (TRUNC_MOD_EXPR, gnu_pos,
					   size_int (BITS_PER_UNIT)))))
	{
	  post_error_ne ("fields of& must start at storage unit boundary",
			 gnat_field, Etype (gnat_field));
	  gnu_pos = 0;
	}
    }

  /* If the record has rep clauses and this is the tag field, make a rep
     clause for it as well.  */
  else if (Has_Specified_Layout (Scope (gnat_field))
	   && Chars (gnat_field) == Name_uTag)
    {
      gnu_pos = size_zero_node;
      gnu_size = TYPE_SIZE (gnu_field_type);
    }

  /* We need to make the size the maximum for the type if it is
     self-referential and an unconstrained type.  */
  else if (TREE_CODE (gnu_field_type) == RECORD_TYPE
	   && ! TREE_CONSTANT (TYPE_SIZE (gnu_field_type))
	   && contains_placeholder_p (TYPE_SIZE (gnu_field_type))
	   && ! Is_Constrained (Underlying_Type (Etype (gnat_field))))
    gnu_size = max_size (TYPE_SIZE (gnu_field_type), 1);

  /* If no size is specified (or if there was an error), don't specify a 
     position.  */
  if (gnu_size == 0)
    gnu_pos = 0;
  else
    {
      /* Unless this field is aliased, we can remove any left-justified
	 modular type since it's only needed in the unchecked conversion
	 case, which doesn't apply here.  */
      if (! needs_strict_alignment
	  && TREE_CODE (gnu_field_type) == RECORD_TYPE
	  && TYPE_LEFT_JUSTIFIED_MODULAR_P (gnu_field_type))
	gnu_field_type = TREE_TYPE (TYPE_FIELDS (gnu_field_type));

      gnu_field_type
	= make_type_from_size (gnu_field_type, gnu_size,
			       Has_Biased_Representation (gnat_field));
      gnu_field_type = maybe_pad_type (gnu_field_type, gnu_size, 0,
				       gnat_field, "PAD", 0);
    }

  if (TREE_CODE (gnu_field_type) == RECORD_TYPE
      && TYPE_CONTAINS_TEMPLATE_P (gnu_field_type))
    gigi_abort (118);

  set_lineno (gnat_field, 0);
  return create_field_decl (gnu_field_id, gnu_field_type, gnu_record_type,
			    packed, gnu_size, gnu_pos);
}

/* Return a GCC tree for a record type given a GNAT Component_List and a chain
   of GCC trees for fields that are in the record and have already been
   processed.  When called from gnat_to_gnu_entity during the processing of a
   record type definition, the GCC nodes for the discriminants will be on
   the chain.  The other calls to this function are recursive calls from
   itself for the Component_List of a variant and the chain is empty.

   PACKED is nonzero if this field is for a record with "pragma pack".

   FINISH_RECORD is nonzero if this call will supply all of the remaining
   fields of the record.

   P_GNU_REP_LIST, if nonzero, is a pointer to a list to which each field
   with a rep clause is to be added.  If it is nonzero, that is all that
   should be done with such fields.

   The processing of the component list fills in the chain with all of the
   fields of the record and then the record type is finished.  */

static void
components_to_record (gnu_record_type, component_list, gnu_field_list, packed,
		      definition, p_gnu_rep_list)
     tree gnu_record_type;
     Node_Id component_list;
     tree gnu_field_list;
     int definition;
     tree *p_gnu_rep_list;
{
  Node_Id component_decl;
  Entity_Id gnat_field;
  Node_Id variant_part;
  Node_Id variant;
  tree gnu_our_rep_list = NULL_TREE;
  tree gnu_field, gnu_last;
  int layout_with_rep = 0;

  /* For each variable within each component declaration create a GCC field
     and add it to the list, skipping any pragmas in the list.  */

  if (Present (Component_Items (component_list)))
    for (component_decl = First (Component_Items (component_list));
	 Present (component_decl);
	 component_decl = Next (component_decl))
      if (Nkind (component_decl) != N_Pragma)
        {
	  gnat_field = Defining_Identifier (component_decl);

	  if (Chars (gnat_field) == Name_uParent)
	    gnu_field = tree_last (TYPE_FIELDS (gnu_record_type));
	  else
	    {
	      gnu_field
		= gnat_to_gnu_field (gnat_field, gnu_record_type, packed);

	      /* If this is the _Tag field, put it before any discriminants,
		 instead of after them as is the case for all other fields.  */
	      if (Chars (gnat_field) == Name_uTag)
		gnu_field_list = chainon (gnu_field_list, gnu_field);
	      else
		{
		  TREE_CHAIN (gnu_field) = gnu_field_list;
		  gnu_field_list = gnu_field;
		}
	    }

	  save_gnu_tree (gnat_field, gnu_field, 0);
        }

  /* At the end of the component list there may be a variant part.  */
  variant_part = Variant_Part (component_list);

  /* If this is an unchecked union, each variant must have exactly one
     component, each of which becomes one component of this union.  */
  if (TREE_CODE (gnu_record_type) == UNION_TYPE && Present (variant_part))
    for (variant = First_Non_Pragma (Variants (variant_part));
	 Present (variant);
	 variant = Next_Non_Pragma (variant))
      {
	component_decl
	  = First_Non_Pragma (Component_Items (Component_List (variant)));
	gnat_field = Defining_Identifier (component_decl);
	gnu_field = gnat_to_gnu_field (gnat_field, gnu_record_type, packed);
	TREE_CHAIN (gnu_field) = gnu_field_list;
	gnu_field_list = gnu_field;
	save_gnu_tree (gnat_field, gnu_field, 0);
      }

  /* We create a QUAL_UNION_TYPE for the variant part since the variants are
     mutually exclusive and should go in the same memory.  To do this we need
     to treat each variant as a record whose elements are created from the
     component list for the variant.  So here we create the records from the
     lists for the variants and put them all into the QUAL_UNION_TYPE.  */
  else if (Present (variant_part))
    {
      tree gnu_disc_var = gnat_to_gnu (Name (variant_part));
      tree gnu_discriminant = maybe_placeholder (gnu_disc_var);
      Node_Id variant;
      tree gnu_union_type = make_node (QUAL_UNION_TYPE);
      tree gnu_union_field;
      tree gnu_variant_list = NULL_TREE;
      int var_idx = 0;
      char var_name[1000];
      int field_idx;

      /* Get the field index of the discriminant to use as the name for
	 the variants for debugging purposes.  If we can't find it or
	 it is a nested structure, set an invalid index number.  */
      if (TREE_CODE (gnu_disc_var) != FIELD_DECL)
	field_idx = list_length (gnu_field_list) + 1;
      else
	{
	  for (field_idx = 0,
	       gnu_field = TYPE_FIELDS (DECL_CONTEXT (gnu_disc_var));
	       gnu_field != gnu_disc_var && gnu_field != 0;
	       field_idx++, gnu_field = TREE_CHAIN (gnu_field))
	    ;

	  field_idx
	    = (list_length (TYPE_FIELDS (DECL_CONTEXT (gnu_disc_var)))
	       - field_idx);
	}

      for (variant = First_Non_Pragma (Variants (variant_part));
           Present (variant);
	   variant = Next_Non_Pragma (variant))
	{
	  tree gnu_variant_type = make_node (RECORD_TYPE);
	  tree gnu_qual;

	  TYPE_ALIGN (gnu_variant_type) = TYPE_ALIGN (gnu_record_type);
	  components_to_record (gnu_variant_type, Component_List (variant),
				NULL_TREE, packed, definition,
				&gnu_our_rep_list);

	  sprintf (var_name, "V%d", field_idx);
	  gnu_qual = choices_to_gnu (gnu_discriminant,
				     Discrete_Choices (variant), var_name);
	  if (var_name[0] == '\0')
	    sprintf (var_name, "V%d", var_idx++);

	  gnu_field = create_field_decl (get_identifier (var_name),
					 gnu_variant_type,
					 gnu_union_type, 0, 0, 0);
	  DECL_INTERNAL_P (gnu_field) = 1;
	  DECL_QUALIFIER (gnu_field) = gnu_qual;
	  TREE_CHAIN (gnu_field) = gnu_variant_list;
	  gnu_variant_list = gnu_field;
	}

      /* We can delete any empty variants from the end.  This may leave none
	 left.  Note we cannot delete variants from anywhere else.  */
      while (gnu_variant_list != 0
	     && TYPE_FIELDS (TREE_TYPE (gnu_variant_list)) == 0)
	gnu_variant_list = TREE_CHAIN (gnu_variant_list);

      /* Only make the QUAL_UNION_TYPE if there are any non-empty variants.  */
      if (gnu_variant_list != 0)
	{
	  finish_record_type (gnu_union_type, nreverse (gnu_variant_list),
			      0, 0);

	  gnu_union_field = create_field_decl (get_identifier ("VARIANTS"),
					       gnu_union_type, gnu_record_type,
					       packed, 0, 0);

	  DECL_INTERNAL_P (gnu_union_field) = 1;
	  TREE_CHAIN (gnu_union_field) = gnu_field_list;
	  gnu_field_list = gnu_union_field;
	}
    }

  /* Scan GNU_FIELD_LIST and see if any fields have rep clauses.  If they
     do, pull them out and put them into GNU_OUR_REP_LIST.  We have to do this
     in a separate pass since we want to handle the discriminants but can't
     play with them until we've used them in debugging data above.

     ??? Note: if we then reorder them, debugging information will be wrong,
     but there's nothing that can be done about this at the moment.  */

  for (gnu_field = gnu_field_list, gnu_last = 0; gnu_field; )
    {
      if (DECL_FIELD_BITPOS (gnu_field) != 0)
	{
	  tree gnu_next = TREE_CHAIN (gnu_field);

	  if (gnu_last == 0)
	    gnu_field_list = gnu_next;
	  else
	    TREE_CHAIN (gnu_last) = gnu_next;

	  TREE_CHAIN (gnu_field) = gnu_our_rep_list;
	  gnu_our_rep_list = gnu_field;
	  gnu_field = gnu_next;
	}
      else
	{
	  gnu_last = gnu_field;
	  gnu_field = TREE_CHAIN (gnu_field);
	}
    }

  /* If we have any items in our rep'ed field list and P_REP_LIST is nonzero,
     set it and ignore the items.  Otherwise, sort the fields by bit position
     and put them into their own record if we have any fields without
     rep clauses. */
  if (gnu_our_rep_list != 0 && p_gnu_rep_list != 0)
    *p_gnu_rep_list = chainon (*p_gnu_rep_list, gnu_our_rep_list);
  else if (gnu_our_rep_list != 0)
    {
      tree gnu_rep_type
	= gnu_field_list == 0 ? gnu_record_type : make_node (RECORD_TYPE);
      int len = list_length (gnu_our_rep_list);
      tree *gnu_arr = (tree *) alloca (sizeof (tree) * len);
      int i;

      for (i = 0, gnu_field = gnu_our_rep_list; gnu_field;
	   gnu_field = TREE_CHAIN (gnu_field), i++)
	gnu_arr[i] = gnu_field;

      qsort (gnu_arr, len, sizeof (tree), compare_field_bitpos);

      /* Put the fields in the list in order of increasing position, which
	 means we start from the end.  */
      gnu_our_rep_list = NULL_TREE;
      for (i = len - 1; i >= 0; i--)
	{
	  TREE_CHAIN (gnu_arr[i]) = gnu_our_rep_list;
	  gnu_our_rep_list = gnu_arr[i];
	  DECL_CONTEXT (gnu_arr[i]) = gnu_rep_type;
	}

      if (gnu_field_list != 0)
	{
	  /* If an alignment was specified for the type, that becomes the
	     alignment for the rep'ed part.  */
	  TYPE_ALIGN (gnu_rep_type) = TYPE_ALIGN (gnu_record_type);
	  finish_record_type (gnu_rep_type, gnu_our_rep_list, 1, 0);
	  gnu_field = create_field_decl (get_identifier ("REP"), gnu_rep_type,
					 gnu_record_type, 0, 0, 0);
	  DECL_INTERNAL_P (gnu_field) = 1;
	  gnu_field_list = chainon (gnu_field_list, gnu_field);

	  /* Clear out any alignment and size pre-set into GNU_RECORD_TYPE
	     in the case of partial rep clauses.  In that case, we choose the
	     alignment and size by normal mechanisms.  */
	  TYPE_SIZE (gnu_record_type) = TYPE_ADA_SIZE (gnu_record_type) = 0;
	  TYPE_ALIGN (gnu_record_type) = 0;
	}
      else
	{
	  layout_with_rep = 1;
	  gnu_field_list = nreverse (gnu_our_rep_list);
	}
    }

  finish_record_type (gnu_record_type, nreverse (gnu_field_list),
		      layout_with_rep, 0);
}

/* Called via qsort from the above.  Returns -1, 0, or 1, depending on the
   bit position of the two fields.  */

static int
compare_field_bitpos (t1, t2)
     tree *t1, *t2;
{
  if (tree_int_cst_equal (DECL_FIELD_BITPOS (*t1), DECL_FIELD_BITPOS (*t2)))
    return 0;
  else if (tree_int_cst_lt (DECL_FIELD_BITPOS (*t1), DECL_FIELD_BITPOS (*t2)))
    return -1;
  else
    return 1;
}
/* Given GNAT_ENTITY, a record type, and GNU_TYPE, its corresponding
   GCC type, set Component_First_Bit and Esize to the position and size
   used by Gigi.  */

static void
annotate_rep (gnat_entity, gnu_type)
     Entity_Id gnat_entity;
     tree gnu_type;
{
  tree gnu_list;
  tree gnu_entry;
  Entity_Id gnat_field;

  /* We operate by first making a list of all field and their positions
     (we can get the sizes easily at any time) by a recursive call
     and then update all the sizes into the tree.  */
  gnu_list = annotate_rep_1 (gnu_type, NULL_TREE, 0);

  for (gnat_field = First_Entity (gnat_entity); Present (gnat_field);
       gnat_field = Next_Entity (gnat_field))
    if ((Ekind (gnat_field) == E_Component
	 || (Ekind (gnat_field) == E_Discriminant
	     && ! Is_Unchecked_Union (Scope (gnat_field))))
	&& 0 != (gnu_entry = purpose_member (gnat_to_gnu_entity (gnat_field,
								 NULL_TREE, 0),
					     gnu_list)))
      {
	Set_Component_First_Bit (gnat_field,
				 UI_From_Int
				 (TREE_INT_CST_LOW (TREE_VALUE (gnu_entry))));
	if (TREE_CODE (DECL_SIZE (TREE_PURPOSE (gnu_entry))) == INTEGER_CST)
	  Set_Esize (gnat_field,
		     UI_From_Int
		     (TREE_INT_CST_LOW
		      (DECL_SIZE (TREE_PURPOSE (gnu_entry)))));
      }
}

/* Subroutine of above.  Scan all field in GNU_TYPE and build entries
   where TREE_PURPOSE is the FIELD_DECL and TREE_VALUE is the bit
   position.  POS is to be added to the position and GNU_LIST is the
   entries so far.  */

static tree
annotate_rep_1 (gnu_type, gnu_list, pos)
     tree gnu_type;
     tree gnu_list;
     int pos;
{
  tree gnu_field;
  tree gnu_result = gnu_list;
  tree gnu_entry;

  for (gnu_field = TYPE_FIELDS (gnu_type); gnu_field;
       gnu_field = TREE_CHAIN (gnu_field))
    {
      if (TREE_CODE (DECL_FIELD_BITPOS (gnu_field)) != INTEGER_CST)
	break;

      if (DECL_INTERNAL_P (gnu_field))
	gnu_result
	  = annotate_rep_1 (TREE_TYPE (gnu_field), gnu_result,
			    TREE_INT_CST_LOW (DECL_FIELD_BITPOS (gnu_field)));
      else
	gnu_result = tree_cons (gnu_field, DECL_FIELD_BITPOS (gnu_field),
				gnu_result);
    }

  return gnu_result;
}

/* Create a CONSTRUCTOR for the enumeration literal table of
   GNAT_ENUM_TYPE.  The GCC type of the literal table is GNU_TABLE_TYPE.  */

static tree
create_enum_initializer (gnat_enum_type, gnu_table_type)
     Entity_Id gnat_enum_type;
     tree gnu_table_type;
{
  tree gnu_a_string_type = TREE_TYPE (gnu_table_type);
  tree gnu_unc_array_type
    = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (gnu_a_string_type))));
  tree gnu_char_type = TREE_TYPE (gnu_unc_array_type);
  tree gnu_char_domain_type = TYPE_DOMAIN (gnu_unc_array_type);
  tree gnu_size_1 = size_int (1);
  tree gnu_list = NULL_TREE;
  Entity_Id gnat_literal;

  /* Make a STRING_CST for each literal and add it to the CONSTRUCTOR.  */
  for (gnat_literal = First_Literal (gnat_enum_type);
       Present (gnat_literal);
       gnat_literal = Next_Literal (gnat_literal))
    {
      char *name = Get_Upper_Decoded_Name_String (Chars (gnat_literal));
      char *encoded_name = Get_Name_String (Chars (gnat_literal));
      int length = strlen (name);
      tree gnu_lit_range = build_range_type (gnu_char_domain_type,
					     convert (gnu_char_domain_type,
						      integer_one_node),
					     convert (gnu_char_domain_type,
						      build_int_2 (length,
								   0)));
      tree gnu_lit_index
	= create_index_type (convert (sizetype,
				      TYPE_MIN_VALUE (gnu_lit_range)),
			     convert (sizetype,
				      TYPE_MAX_VALUE (gnu_lit_range)),
			     gnu_lit_range);
      tree gnu_lit_type = build_array_type (gnu_char_type, gnu_lit_index);
      tree gnu_literal;
      tree gnu_temp_type
	= TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_a_string_type)));
      tree gnu_temp = build_template (gnu_temp_type, gnu_lit_type, NULL_TREE);
      tree gnu_record_type
	= build_unc_object_type (gnu_temp_type, gnu_lit_type,
				 get_identifier (encoded_name));

      gnu_literal = build_string (length, name);
      TREE_TYPE (gnu_literal) = gnu_lit_type;
      gnu_literal
	= build_constructor
	  (gnu_record_type,
	   tree_cons (TYPE_FIELDS (gnu_record_type), gnu_temp,
		      tree_cons (TREE_CHAIN (TYPE_FIELDS (gnu_record_type)),
				 gnu_literal, NULL_TREE)));
      gnu_list = tree_cons (NULL_TREE,
			    convert (gnu_a_string_type,
				     build_unary_op (ADDR_EXPR, NULL_TREE,
						     gnu_literal)),
			    gnu_list);
    }

  return build_constructor (gnu_table_type, nreverse (gnu_list));
}

/* UINT_SIZE is a Uint giving the specified size for an object of GNU_TYPE
   corresponding to GNAT_OBJECT.  If size is valid, return a tree corresponding
   to its value.  Otherwise return 0.  KIND is VAR_DECL is we are specifying
   the size for an object, TYPE_DECL for the size of a type, and FIELD_DECL
   for the size of a field.  COMPONENT_P is true if we are being called
   to process the Component_Size of GNAT_OBJECT.  This is used for error
   message handling.  */

static tree
validate_size (uint_size, gnu_type, gnat_object, kind, component_p)
     Uint uint_size;
     tree gnu_type;
     Entity_Id gnat_object;
     enum tree_code kind;
     int component_p;
{
  Node_Id gnat_error_node;
  tree type_size = rm_size (gnu_type);
  tree size;

  if (type_size != 0 && TREE_CODE (type_size) != INTEGER_CST
      && contains_placeholder_p (type_size))
    type_size = max_size (type_size, 1);

  if (TYPE_FAT_POINTER_P (gnu_type))
    type_size = size_int (POINTER_SIZE);

  if ((Ekind (gnat_object) == E_Component
       || Ekind (gnat_object) == E_Discriminant)
      && Present (Component_Clause (gnat_object)))
    gnat_error_node = Last_Bit (Component_Clause (gnat_object));
  else if (Present (Size_Clause (gnat_object)))
    gnat_error_node = Expression (Size_Clause (gnat_object));
  else
    gnat_error_node = gnat_object;

  /* Don't give errors on packed array types; we'll be giving the error on
     the type itself soon enough.  */
  if (Is_Packed_Array_Type (gnat_object))
    gnat_error_node = Empty;

  /* Get the size as a tree.  Return 0 if none was specified, either
     because Esize was not Present or if the specified size was zero.
     Give an error if a size was specified, but cannot be represented as
     in sizetype or if this is a bitfield and the size is larger than
     that of HOST_WIDE_INT, since that's how bit DECL_FIELD_BITPOS is.  */

  if (No (uint_size))
    return 0;

  size = UI_To_gnu (uint_size, sizetype);
  if (TREE_OVERFLOW (size)
      || (kind == FIELD_DECL && TREE_INT_CST_HIGH (size) != 0))
    {
      if (component_p)
	post_error_ne ("component size of & is too large",
		       gnat_error_node, gnat_object);
      else
	post_error_ne ("size of & is too large", gnat_error_node, gnat_object);

      return 0;
    }
  else if (integer_zerop (size))
    return 0;

  /* The size of objects is always a multiple of a byte.  */
  if (kind == VAR_DECL
      && ! integer_zerop (size_binop (TRUNC_MOD_EXPR, size,
				      size_int (BITS_PER_UNIT))))
    {
      if (component_p)
	post_error_ne ("component size for& is not a multiple of Storage_Unit",
		       gnat_error_node, gnat_object);
      else
	post_error_ne ("size for& is not a multiple of Storage_Unit",
		       gnat_error_node, gnat_object);
      return 0;
    }

  /* If this is an integral type, the front-end has verified the size, so we
     need not do it here (which would entail checking against the bounds). 
     However, if this is an aliased object, it may not be smaller than the
     type of the object.  */
  if (INTEGRAL_TYPE_P (gnu_type)
      && ! (kind == VAR_DECL && Is_Aliased (gnat_object)))
    return size;

  /* If the size of the object is a constant, the new size must not be
     smaller.  */
  if (TREE_CODE (type_size) != INTEGER_CST
      || TREE_OVERFLOW (type_size)
      || tree_int_cst_lt (size, type_size))
    {
      if (component_p)
	post_error_ne_tree
	  ("component size for& too small{, minimum allowed is ^}",
	   gnat_error_node, gnat_object, type_size);
      else
	post_error_ne_tree ("size for& too small{, minimum allowed is ^}",
			    gnat_error_node, gnat_object, type_size);

      if (INTEGRAL_TYPE_P (gnu_type))
	post_error_ne ("size would be legal if & were not aliased!",
		       gnat_error_node, gnat_object);

      return 0;
    }

  return size;
}	 

/* Given a type TYPE, return a new type whose size is appropriate for SIZE.
   If TYPE is the best type, return it.  Otherwise, make a new type.  We
   only support new integral and pointer types.  BIASED_P is nonzero if
   we are making a biased type.  */

static tree
make_type_from_size (type, size_tree, biased_p)
     tree type;
     tree size_tree;
     int biased_p;
{
  tree new_type;
  HOST_WIDE_INT size;

  /* If size indicates an error, just return TYPE to avoid propagating the
     error.  Likewise if it's too large to represent.  */
  if (size_tree == 0
      || TREE_OVERFLOW (size_tree)
      || TREE_INT_CST_HIGH (size_tree) != 0)
    return type;

  size = TREE_INT_CST_LOW (size_tree);
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
      /* Only do something if the type is not already the proper size and is
	 not a packed array type.  */
      if (TYPE_PACKED_ARRAY_TYPE_P (type)
	  || (TYPE_PRECISION (type) == size
	      && TYPE_BIASED_REPRESENTATION_P (type) == biased_p))
	break;

      new_type = make_signed_type (MIN (size, LONG_LONG_TYPE_SIZE));
      TREE_TYPE (new_type)
	= TREE_TYPE (type) != 0 ? TREE_TYPE (type) : type;
      TYPE_MIN_VALUE (new_type)
	= convert (TREE_TYPE (new_type), TYPE_MIN_VALUE (type));
      TYPE_MAX_VALUE (new_type)
	= convert (TREE_TYPE (new_type), TYPE_MAX_VALUE (type));
      TYPE_BIASED_REPRESENTATION_P (new_type)
	= TYPE_BIASED_REPRESENTATION_P (type) | biased_p;
      TREE_UNSIGNED (new_type)
	= TREE_UNSIGNED (type) | TYPE_BIASED_REPRESENTATION_P (new_type);
      return new_type;

    case RECORD_TYPE:
      /* Do something if this is a fat pointer, in which case we
	 may need to return the thin pointer.  */
      if (TYPE_IS_FAT_POINTER_P (type) && size < POINTER_SIZE * 2)
	return
	  build_pointer_type
	    (TYPE_OBJECT_RECORD_TYPE (TYPE_UNCONSTRAINED_ARRAY (type)));
      break;

    case POINTER_TYPE:
      /* Only do something if this is a thin pointer, in which case we
	 may need to return the fat pointer.  */
      if (TYPE_THIN_POINTER_P (type) && size >= POINTER_SIZE * 2)
	return
	  build_pointer_type (TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (type)));

      break;
    }

  return type;
}

/* GNAT_ALIGNMENT is an N_Alignment_Clause that is specified for GNAT_ENTITY,
   a type or object whose present alignment is ALIGN.  If this alignment is
   valid, return it.  Otherwise, give an error and return ALIGN.  */

static int
validate_alignment (gnat_alignment, gnat_entity, align)
     Node_Id gnat_alignment;
     Entity_Id gnat_entity;
     int align;
{
  int new_align;

#ifndef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT BIGGEST_ALIGNMENT
#endif

  /* Within GCC, an alignment is an integer, so we must make sure a
     value is specified that fits in that range.  Also, alignments of
     more than MAX_OFILE_ALIGNMENT can't be supported.  */

  if (! UI_Is_In_Int_Range (Expr_Value (Expression (gnat_alignment)))
      || ((new_align = UI_To_Int (Expr_Value (Expression (gnat_alignment))))
	  > MAX_OFILE_ALIGNMENT / BITS_PER_UNIT))
    post_error_ne_num ("?largest supported alignment for& is ^",
		       Expression (gnat_alignment), gnat_entity,
		       MAX_OFILE_ALIGNMENT / BITS_PER_UNIT);
  else if (! From_At_Mod (gnat_alignment)
	   && new_align * BITS_PER_UNIT < align)
    post_error_ne_num ("alignment for& must be at least ^",
		       Expression (gnat_alignment), gnat_entity,
		       align / BITS_PER_UNIT);
  else
    align = MAX (align, new_align == 0 ? 1 : new_align * BITS_PER_UNIT);

  return align;
}

/* Verify that OBJECT, a type or decl, is something we can implement
   atomically.  If not, give an error for GNAT_ENTITY.  */

static void
check_ok_for_atomic (object, gnat_entity)
     tree object;
     Entity_Id gnat_entity;
{
  enum machine_mode mode
    = TREE_CODE_CLASS (TREE_CODE (object)) == 'd'
      ? DECL_MODE (object) : TYPE_MODE (object);
  int align = (TREE_CODE_CLASS (TREE_CODE (object)) == 'd'
	       ? DECL_ALIGN (object) : TYPE_ALIGN (object));
  tree size = (TREE_CODE_CLASS (TREE_CODE (object)) == 'd'
	       ? DECL_SIZE (object) : TYPE_SIZE (object));
  Node_Id gnat_error_point = gnat_entity;
  Node_Id gnat_node;

  /* Consider all floating-point types atomic and any types that that are
     represented by integers no wider than a machine word.  */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      || ((GET_MODE_CLASS (mode) == MODE_INT
	   || GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
	  && GET_MODE_BITSIZE (mode) <= BITS_PER_WORD))
    return;

  /* For the moment, also allow anything that has an alignment equal
     to its size and which is smaller than a word.  */
  if (TREE_CODE (size) == INTEGER_CST && TREE_INT_CST_HIGH (size) == 0
      && TREE_INT_CST_LOW (size) == align
      && align <= BITS_PER_WORD)
    return;

  for (gnat_node = First_Rep_Item (gnat_entity); Present (gnat_node);
       gnat_node = Next_Rep_Item (gnat_node))
    if (Nkind (gnat_node) == N_Pragma
	&& Get_Pragma_Id (Chars (gnat_node)) == Pragma_Atomic)
      gnat_error_point = First (Pragma_Argument_Associations (gnat_node));

  post_error_ne ("atomic access to & cannot be guaranteed",
		 gnat_error_point, gnat_entity);
}

/* Given a type T, a FIELD_DECL F, and a replacement value R,
   return a new type with all size expressions that contain F
   updated by replacing F with R.  This is identical to GCC's
   substitute_in_type except that it knows about TYPE_INDEX_TYPE. 
   If F is NULL_TREE, always make a new RECORD_TYPE, even if nothing has
   changed.  */

tree
gnat_substitute_in_type (t, f, r)
     tree t, f, r;
{
  tree new = t;
  tree tem;

  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
    case VOID_TYPE:
      return t;

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
      if ((TREE_CODE (TYPE_MIN_VALUE (t)) != INTEGER_CST
	   && contains_placeholder_p (TYPE_MIN_VALUE (t)))
	  || (TREE_CODE (TYPE_MAX_VALUE (t)) != INTEGER_CST
	      && contains_placeholder_p (TYPE_MAX_VALUE (t))))
	{
	  tree low = substitute_in_expr (TYPE_MIN_VALUE (t), f, r);
	  tree high = substitute_in_expr (TYPE_MAX_VALUE (t), f, r);

	  if (low == TYPE_MIN_VALUE (t) && high == TYPE_MAX_VALUE (t))
	    return t;

	  new = build_range_type (t, low, high);
	  if (TYPE_INDEX_TYPE (t))
	    TYPE_INDEX_TYPE (new)
	      = gnat_substitute_in_type (TYPE_INDEX_TYPE (t), f, r);
	  return new;
	}

      return t;

    case REAL_TYPE:
      if ((TYPE_MIN_VALUE (t) != 0
	   && TREE_CODE (TYPE_MIN_VALUE (t)) != REAL_CST
	   && contains_placeholder_p (TYPE_MIN_VALUE (t)))
	  || (TYPE_MAX_VALUE (t) != 0
	      && TREE_CODE (TYPE_MAX_VALUE (t)) != REAL_CST
	      && contains_placeholder_p (TYPE_MAX_VALUE (t))))
	{
	  tree low = 0, high = 0;

	  if (TYPE_MIN_VALUE (t))
	    low = substitute_in_expr (TYPE_MIN_VALUE (t), f, r);
	  if (TYPE_MAX_VALUE (t))
	    high = substitute_in_expr (TYPE_MAX_VALUE (t), f, r);

	  if (low == TYPE_MIN_VALUE (t) && high == TYPE_MAX_VALUE (t))
	    return t;

	  t = copy_type (t);
	  TYPE_MIN_VALUE (t) = low;
	  TYPE_MAX_VALUE (t) = high;
	}
      return t;

    case COMPLEX_TYPE:
      tem = gnat_substitute_in_type (TREE_TYPE (t), f, r);
      if (tem == TREE_TYPE (t))
	return t;

      return build_complex_type (tem);

    case OFFSET_TYPE:
    case METHOD_TYPE:
    case REFERENCE_TYPE:
    case FILE_TYPE:
    case SET_TYPE:
    case FUNCTION_TYPE:
    case LANG_TYPE:
      /* Don't know how to do these yet.  */
      abort ();

    case ARRAY_TYPE:
      {
	tree component = gnat_substitute_in_type (TREE_TYPE (t), f, r);
	tree domain = gnat_substitute_in_type (TYPE_DOMAIN (t), f, r);

	if (component == TREE_TYPE (t) && domain == TYPE_DOMAIN (t))
	  return t;

	new = build_array_type (component, domain);
	TYPE_SIZE (new) = 0;
	TYPE_MULTI_ARRAY_P (new) = TYPE_MULTI_ARRAY_P (t);
	TYPE_CONVENTION_FORTRAN_P (new) = TYPE_CONVENTION_FORTRAN_P (t);
	layout_type (new);
	TYPE_ALIGN (new) = TYPE_ALIGN (t);
	return new;
      }

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree field_type, field;
	int changed_field
	  = (f == NULL_TREE && ! TREE_CONSTANT (TYPE_SIZE (t)));
	int field_has_rep = 0;
	tree last_field = 0;

	tree new = copy_type (t);

	/* Start out with no fields, make new fields, and chain them
	   in.  If we haven't actually changed the type of any field,
	   discard everything we've done and return the old type.  */

	TYPE_FIELDS (new) = 0;
	TYPE_SIZE (new) = 0;

	for (field = TYPE_FIELDS (t); field;
	     field = TREE_CHAIN (field))
	  {
	    tree new_field = copy_node (field);

	    TREE_TYPE (new_field)
	      = gnat_substitute_in_type (TREE_TYPE (new_field), f, r);

	    if (TREE_TYPE (new_field) != TREE_TYPE (field)
		&& ! DECL_HAS_REP_P (field))
	      changed_field = 1;

	    if (DECL_HAS_REP_P (field))
	      field_has_rep = 1;

	    /* If this is an internal field and the type of this field is
	       a UNION_TYPE or RECORD_TYPE with no elements, ignore it.  If
	       the type just has one element, treat that as the field.
	       But don't do this if we are processing a QUAL_UNION_TYPE.  */
	    if (TREE_CODE (t) != QUAL_UNION_TYPE
		&& DECL_INTERNAL_P (new_field)
		&& (TREE_CODE (TREE_TYPE (new_field)) == UNION_TYPE
		    || TREE_CODE (TREE_TYPE (new_field)) == RECORD_TYPE))
	      {
		if (TYPE_FIELDS (TREE_TYPE (new_field)) == 0)
		  continue;

		if (TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (new_field))) == 0)
		  {
		    tree next_new_field
		      = copy_node (TYPE_FIELDS (TREE_TYPE (new_field)));

		    /* Make sure omitting the union doesn't change
		       the layout.  */
		    DECL_ALIGN (next_new_field) = DECL_ALIGN (new_field);
		    new_field = next_new_field;
		  }
	      }

	    DECL_CONTEXT (new_field) = new;

	    /* If the size of the old field was set at a constant,
	       propagate the size in case the type's size was variable.
	       (This occurs in the case of a variant or discriminated
	       record with a default size used as a field of another
	       record.)  */
	    DECL_SIZE (new_field)
	      = TREE_CODE (DECL_SIZE (field)) == INTEGER_CST
		? DECL_SIZE (field) : 0;

	    if (TREE_CODE (t) == QUAL_UNION_TYPE)
	      {
		tree new_q = substitute_in_expr (DECL_QUALIFIER (field), f, r);

		if (new_q != DECL_QUALIFIER (new_field))
		  changed_field = 1;

		/* Do the substitution inside the qualifier and if we find
		   that this field will not be present, omit it.  */
		DECL_QUALIFIER (new_field) = new_q;

		if (integer_zerop (DECL_QUALIFIER (new_field)))
		  continue;
	      }

	    if (last_field == 0)
	      TYPE_FIELDS (new) = new_field;
	    else
	      TREE_CHAIN (last_field) = new_field;

	    last_field = new_field;

	    /* If this is a qualified type and this field will always be
	       present, we are done.  */
	    if (TREE_CODE (t) == QUAL_UNION_TYPE
		&& integer_onep (DECL_QUALIFIER (new_field)))
	      break;
	  }

	/* If this used to be a qualified union type, but we now know what
	   field will be present, make this a normal union.  */
	if (TREE_CODE (new) == QUAL_UNION_TYPE
	    && (TYPE_FIELDS (new) == 0
		|| integer_onep (DECL_QUALIFIER (TYPE_FIELDS (new)))))
	  TREE_SET_CODE (new, UNION_TYPE);
	else if (! changed_field)
	  return t;

	if (field_has_rep)
	  gigi_abort (117);

	layout_type (new);

	/* If the size was originally a constant use it.  */
	if (TYPE_SIZE (t) != 0 && TREE_CODE (TYPE_SIZE (t)) == INTEGER_CST
	    && TREE_CODE (TYPE_SIZE (new)) != INTEGER_CST)
	  {
	    TYPE_SIZE (new) = TYPE_SIZE (t);
	    TYPE_ADA_SIZE (new) = TYPE_ADA_SIZE (t);
	  }

	return new;
      }
    }

  return t;
}

/* Return the "RM size" of GNU_TYPE.  This is the actual number of bits
   needed to represent the object.  */

tree
rm_size (gnu_type)
     tree gnu_type;
{
  /* For integer types, this is the precision.  For record types, we store
     the size explicitly.  For other types, this is just the size.  */

  if (INTEGRAL_TYPE_P (gnu_type) && TYPE_RM_SIZE (gnu_type) != 0)
    return TYPE_RM_SIZE (gnu_type);
  else if ((TREE_CODE (gnu_type) == RECORD_TYPE
	    || TREE_CODE (gnu_type) == UNION_TYPE)
	   && ! TYPE_IS_FAT_POINTER_P (gnu_type)
	   && TYPE_ADA_SIZE (gnu_type) != 0)
    return TYPE_ADA_SIZE (gnu_type);
  else
    return TYPE_SIZE (gnu_type);
}

/* The external name of an entity is the specified Interface_Name, if any.
   Otherwise it is:

    The string "_ada_", if the entity is a library subprogram, followed by
    the name of any enclosing scope (each followed by "__") followed by
    the name of the entity followed by
    the string "__" followed by homonym number for overloaded subprograms. */

static struct obstack ext_name_obstack;
static char *ext_name_firstobj;

/* Return an identifier representing the external name to be used for
   GNAT_ENTITY.  If STRING is specified, the name is followed by "___"
   and the specified string.  */

tree
create_concat_name (gnat_entity, string)
     Entity_Id gnat_entity;
     char *string;
{
  /* Initialize the obstack we are using to construct the name.  */
  if (!ext_name_firstobj)
    {
      gcc_obstack_init (&ext_name_obstack);
      ext_name_firstobj = obstack_alloc (&ext_name_obstack, 1);
    }
  else
    {
      obstack_free (&ext_name_obstack, ext_name_firstobj);
      ext_name_firstobj = obstack_alloc (&ext_name_obstack, 1);
    }

  /* If this is a child unit, we want the child.  */
  if (Nkind (gnat_entity) == N_Defining_Program_Unit_Name)
    gnat_entity = Defining_Identifier (gnat_entity);

  if ((Ekind (gnat_entity) == E_Procedure || Ekind (gnat_entity) == E_Function
       || Ekind (gnat_entity) == E_Constant
       || Ekind (gnat_entity) == E_Variable)
      && Present (Interface_Name (gnat_entity))
      && string == 0)
    {
      String_Id gnat_string = Strval (Interface_Name (gnat_entity));
      int length = String_Length (gnat_string);
      int i;

      for (i = 0; i < length; i++)
	obstack_1grow (&ext_name_obstack,
		       Get_String_Char (gnat_string, i + 1));

      if (Convention (gnat_entity) == Convention_Stdcall
	  && Ekind (gnat_entity) == E_Variable)
	obstack_grow (&ext_name_obstack, "_dll", 4);
    }
  else
    {
      /* If this is a a main subprogram, we prepend a prefix to avoid clashes
	 with external C names as main or C library names. A main subprogram
	 is recognized by the fact that its scope is Standard.  We don't
	 do this if we want to concatenate a string.  */
      if (No (Scope (Scope (gnat_entity)))
	  && Is_Subprogram (gnat_entity)
	  && string == 0)
	obstack_grow (&ext_name_obstack, "_ada_", 5);

      compute_qualified_name (gnat_entity);

      if (Has_Homonym (gnat_entity))
	{
	  Entity_Id e;
	  int number;
	  char buf[10];

	  for (e = Homonym (gnat_entity), number = 1;
	       Present (e); e = Homonym (e))
	    if (Scope (e) == Scope (gnat_entity))
		number ++;

	  sprintf (buf, "%d", number);
	    if (number != 1)
	      {
#ifdef NO_DOLLAR_IN_LABEL
		obstack_grow (&ext_name_obstack, "__", 2);
#else
		obstack_grow (&ext_name_obstack, "$", 1);
#endif
		obstack_grow (&ext_name_obstack, buf, strlen (buf));
	      }
	}
    }

  if (string)
    {
      obstack_grow (&ext_name_obstack, "___", 3);
      obstack_grow (&ext_name_obstack, string, strlen (string));
    }

  obstack_1grow (&ext_name_obstack, 0);

  return get_identifier ((char *) obstack_base (&ext_name_obstack));
}

/* Return the name to be used for GNAT_ENTITY.  If a type, create a 
   fully-qualified name, possibly with type information encoding.
   Otherwise, return the name.  */

static tree
get_entity_name (gnat_entity)
     Entity_Id gnat_entity;
{
  return (Is_Type (gnat_entity)
	  ? create_concat_name (gnat_entity,
				Get_Encoded_Type_Name
				(gnat_entity)
				? Name_Buffer : NULL_PTR)
	  : get_identifier (Get_Name_String (Chars (gnat_entity))));
}

static void
compute_qualified_name (gnat_entity)
     Entity_Id gnat_entity;
{
  char *name;

  /* If the entity is a child package, its name is not a Defining_Identifier,
     but a Defining_Program_Unit_Name, which does not have a chars field.
     Its simple name is the final identifier, which is the name to use. */

  if (Nkind (gnat_entity) == N_Defining_Program_Unit_Name)
    gnat_entity = Defining_Identifier (gnat_entity);

  if (Scope (Scope (gnat_entity)))
    {
      compute_qualified_name (Scope (gnat_entity));
      obstack_grow (&ext_name_obstack, "__", 2);
    }

  /* Now get the name of the entity */
  name = Get_Name_String (Chars (gnat_entity));
  obstack_grow (&ext_name_obstack, name, strlen (name));
}
