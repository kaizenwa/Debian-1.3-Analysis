/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             A - T R A N S 3                              */
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*                            $Revision: 1.187 $                            */
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
#include "defaults.h"
#include "a-ada.h"
#include "a-types.h"
#include "a-atree.h"
#include "a-nlists.h"
#include "a-elists.h"
#include "a-sinfo.h"
#include "a-einfo.h"
#include "a-namet.h"
#include "a-string.h"
#include "a-uintp.h"
#include "a-trans.h"
#include "a-gtran3.h"
#include "a-trans3.h"
#include "a-trans4.h"
#include "a-misc.h"
#include "a-rtree.h"
#include "convert.h"

#undef  NULL
#define NULL 0

/* If nonzero, pretend we are allocating at global level.  */

int force_global;

/* Global Variables for the various types we create.  */ 

tree error_mark_node;
tree integer_type_node;
tree unsigned_type_node;
tree char_type_node;
tree longest_float_type_node;
tree void_type_node;
tree void_type_decl_node;
tree ptr_void_type_node;
tree void_ftype;
tree ptr_void_ftype;

tree except_type_node;
tree malloc_decl;
tree free_decl;
tree jmpbuf_type;
tree jmpbuf_ptr_type;
tree get_jmpbuf_decl;
tree set_jmpbuf_decl;
tree get_excptr_decl;
tree set_except_occ_decl;
tree raise_decl;
tree raise_with_msg_decl;
tree raise_nodefer_decl;
tree setjmp_decl;
tree raise_constraint_error_decl;
tree raise_program_error_decl;

tree integer_zero_node;
tree integer_one_node;
tree null_pointer_node;

tree current_function_decl = NULL;

static int contains_placeholder_except_p	PROTO((tree, tree));
static tree convert_to_fat_pointer		PROTO((tree, tree));
static tree convert_to_thin_pointer		PROTO((tree, tree));

/* Routines to Associate and Retrieve GCC Nodes with Gnat Nodes: */

/* Associates a GNAT tree node to a GCC tree node. It is used in
   `save_gnu_tree', `get_gnu_tree' and `present_gnu_tree'. See documentation
   of `save_gnu_tree' for more info.  */
static tree *associate_gnat_to_gnu;

/* This listhead is used to record any global objects that need elaboration.
   TREE_PURPOSE is the variable to be elaborated and TREE_VALUE is the
   initial value to assign.  */

static tree pending_elaborations;

/* This stack allows us to momentarily switch to generating elaboration
   lists for an inner context.  */

static struct e_stack {struct e_stack *next; tree elab_list; } *elist_stack;

extern struct obstack *saveable_obstack;

#ifndef MAX_FIXED_MODE_SIZE
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (DImode)
#endif

static tree make_descriptor_field PROTO((char *,tree, tree, tree));

/* Initialize the association of GNAT nodes to GCC trees.  */

void
init_gnat_to_gnu ()
{
  Node_Id gnat_node;

  associate_gnat_to_gnu   = (tree *) xmalloc (max_gnat_nodes * sizeof (tree));

  for (gnat_node = 0; gnat_node < max_gnat_nodes; gnat_node++)
    associate_gnat_to_gnu [gnat_node]   = NULL_TREE;

  associate_gnat_to_gnu   -= First_Node_Id;

  pending_elaborations = build_tree_list (NULL_TREE, NULL_TREE);
}

/* GNAT_ENTITY is a GNAT tree node for an entity.   GNU_DECL is the GCC tree
   which is to be associated with GNAT_ENTITY. Such GCC tree node is always
   a ..._DECL node.  If NO_CHECK is nonzero, the latter check is suppressed.

   If GNU_DECL is zero, a previous association is to be reset.  */

void
save_gnu_tree (gnat_entity, gnu_decl, no_check)
     Entity_Id gnat_entity;
     tree gnu_decl;
     int no_check;
{
  if (gnu_decl
      && (associate_gnat_to_gnu [gnat_entity]
	  || (! no_check && TREE_CODE_CLASS (TREE_CODE (gnu_decl)) != 'd')))
    gigi_abort (401);

  associate_gnat_to_gnu [gnat_entity] = gnu_decl;
}

/* GNAT_ENTITY is a GNAT tree node for a defining identifier.
   Return the ..._DECL node that was associated with it.  If there is no tree
   node associated with GNAT_ENTITY, abort.

   In some cases, such as delayed elaboration or expressions that need to
   be elaborated only once, GNAT_ENTITY is really not an entity.  */

tree
get_gnu_tree (gnat_entity)
     Entity_Id gnat_entity;
{
  if (! associate_gnat_to_gnu [gnat_entity])
    gigi_abort (402);

  return associate_gnat_to_gnu [gnat_entity];
}

/* Return nonzero if a GCC tree has been associated with GNAT_ENTITY.  */

int
present_gnu_tree (gnat_entity)
     Entity_Id gnat_entity;
{
  return (associate_gnat_to_gnu [gnat_entity] != NULL_TREE);
}

/* For each binding contour we allocate a binding_level structure which records
   the entities defined or declared in that contour. Contours include:

	the global one
	one for each subprogram definition
	one for each compound statement (declare block)

   Binding contours are used to create GCC tree BLOCK nodes.  */

struct binding_level
{
  /* A chain of ..._DECL nodes for all variables, constants, functions,
     parameters and type declarations.  These ..._DECL nodes are chained
     through the TREE_CHAIN field. Note that these ..._DECL nodes are stored
     in the reverse of the order supplied to be compatible with the
     back-end.  */
  tree names;
  /* For each level (except the global one), a chain of BLOCK nodes for all
     the levels that were entered and exited one level down from this one.  */
  tree blocks;
  /* The back end may need, for its own internal processing, to create a BLOCK
     node. This field is set aside for this purpose. If this field is non-null
     when the level is popped, i.e. when poplevel is invoked, we will use such
     block instead of creating a new one from the 'names' field, that is the
     ..._DECL nodes accumulated so far.  Typically the routine 'pushlevel'
     will be called before setting this field, so that if the front-end had
     inserted ..._DECL nodes in the current block they will not be lost.   */
  tree block_created_by_back_end;
  /* The binding level containing this one (the enclosing binding level). */
  struct binding_level *level_chain;
};

/* The binding level currently in effect.  */
static struct binding_level *current_binding_level = NULL;

/* A chain of binding_level structures awaiting reuse.  */
static struct binding_level *free_binding_level = NULL;

/* The outermost binding level. This binding level is created when the
   compiler is started and it will exist through the entire compilation.  */
static struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */
static struct binding_level clear_binding_level = {NULL, NULL, NULL, NULL};

/* Return non-zero if we are currently in the global binding level.  */

int
global_bindings_p ()
{
  return (force_global != 0 || current_binding_level == global_binding_level
	  ? -1 : 0);
}

/* Return the list of declarations in the current level. Note that this list
   is in reverse order (it has to be so for back-end compatibility).  */

tree
getdecls ()
{
  return current_binding_level->names;
}

/* Nonzero if the current level needs to have a BLOCK made.  */

int
kept_level_p ()
{
  return (current_binding_level->names != 0);
}

/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */

void
pushlevel (ignore)
     int ignore;
{
  struct binding_level *newlevel = NULL;

  /* Reuse a struct for this binding level, if there is one.  */
  if (free_binding_level)
    {
      newlevel = free_binding_level;
      free_binding_level = free_binding_level->level_chain;
    }
  else
    newlevel =
      (struct binding_level *) xmalloc (sizeof (struct binding_level));

  *newlevel = clear_binding_level;

  /* Add this level to the front of the chain (stack) of levels that are
     active.  */
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree
poplevel (keep, reverse, functionbody)
     int keep;
     int reverse;
     int functionbody;
{
  /* Points to a GCC BLOCK tree node. This is the BLOCK node construted for the
     binding level that we are about to exit and which is returned by this
     routine.  */
  tree block_node = NULL_TREE;
  tree decl_chain;
  tree decl_node;
  tree subblock_chain = current_binding_level->blocks;
  tree subblock_node;
  tree block_created_by_back_end;

  /* Reverse the list of XXXX_DECL nodes if desired.  Note that the ..._DECL
     nodes chained through the `names' field of current_binding_level are in
     reverse order except for PARM_DECL node, which are explicitely stored in
     the right order.  */
  decl_chain = (reverse) ? nreverse (current_binding_level->names)
                         : current_binding_level->names;

  /* Output any nested inline functions within this block which must be
     compiled because their address is needed. */
  for (decl_node = decl_chain; decl_node; decl_node = TREE_CHAIN (decl_node))
    if ((TREE_CODE (decl_node) == FUNCTION_DECL)
	&& ! TREE_ASM_WRITTEN (decl_node)
	&& (DECL_INITIAL (decl_node) != 0)
	&& TREE_ADDRESSABLE (decl_node))
      {
	push_function_context ();
	output_inline_function (decl_node);
	pop_function_context ();
      }

  block_created_by_back_end = current_binding_level->block_created_by_back_end;
  if (block_created_by_back_end != 0)
    {
      block_node = block_created_by_back_end;

      /* Update decls and chain into the block the back end made.  */
      if ((keep || functionbody) && (decl_chain || subblock_chain))
	{
	  BLOCK_VARS (block_node) = keep? decl_chain : 0;
	  BLOCK_SUBBLOCKS (block_node) = subblock_chain;
	}
    }

  /* If there were any declarations in the current binding level, or if this
     binding level is a function body, or if there are any nested blocks then
     create a BLOCK node to record them for the life of this function.  */
  else if (keep || functionbody)
    block_node = build_block (keep ? decl_chain : 0, 0, subblock_chain, 0, 0);

  /* Record the BLOCK node just built as the subblock its enclosing scope.  */
  for (subblock_node = subblock_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    BLOCK_SUPERCONTEXT (subblock_node) = block_node;

  /* Clear out the meanings of the local variables of this level.  */

  for (subblock_node = decl_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    if (DECL_NAME (subblock_node) != 0)
      /* If the identifier was used or addressed via a local extern decl,  
	 don't forget that fact.   */
      if (DECL_EXTERNAL (subblock_node))
	{
	  if (TREE_USED (subblock_node))
	    TREE_USED (DECL_NAME (subblock_node)) = 1;
	  if (TREE_ADDRESSABLE (subblock_node))
	    TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (subblock_node)) = 1;
	}

  {
    /* Pop the current level, and free the structure for reuse.  */
    struct binding_level *level = current_binding_level;
    current_binding_level = current_binding_level->level_chain;
    level->level_chain = free_binding_level;
    free_binding_level = level;
  }

  if (functionbody)
    {
      /* This is the top level block of a function. The ..._DECL chain stored
	 in BLOCK_VARS are the function's parameters (PARM_DECL nodes). Don't
	 leave them in the BLOCK because they are found in the FUNCTION_DECL
	 instead.  */
      DECL_INITIAL (current_function_decl) = block_node;
      BLOCK_VARS (block_node) = 0;
    }
  else if (block_node)
    {
      if (block_created_by_back_end == NULL)
	current_binding_level->blocks
	  = chainon (current_binding_level->blocks, block_node);
    }

  /* If we did not make a block for the level just exited, any blocks made for
     inner levels (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks of something
     else.  */
  else if (subblock_chain)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblock_chain);
  if (block_node)
    TREE_USED (block_node) = 1;

  return block_node;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (block)
     tree block;
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (block)
     tree block;
{
  current_binding_level->block_created_by_back_end = block;
}

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */

tree
pushdecl (decl)
     tree decl;
{
  struct binding_level *b = current_binding_level;

  /* External objects aren't nested, other objects may be.  */
  if (DECL_EXTERNAL (decl))
    {
      DECL_CONTEXT (decl) = 0;
      b = global_binding_level;
    }
  else
    DECL_CONTEXT (decl) = current_function_decl;

  /* Put the declaration on the list.  The list of declarations is in reverse
     order. The list will be reversed later if necessary.  This needs to be
     this way for compatibility with the back-end.

     Don't put TYPE_DECLs for UNCONSTRAINED_ARRAY_TYPE into the list.  They
     will cause trouble with the debugger and aren't needed anyway.  */
  if (TREE_CODE (decl) != TYPE_DECL
      || TREE_CODE (TREE_TYPE (decl)) != UNCONSTRAINED_ARRAY_TYPE)
    {
      TREE_CHAIN (decl) = b->names;
      b->names = decl;
    }

  /* For the declaration of a type, set its name if it either is not already
     set or is an implicit type name.  We'd rather have the type named with a
     real name and all the pointer types to the same object have the same
     POINTER_TYPE node.  Code in this function in c-decl.c makes a copy
     of the type node here, but that may cause us trouble with incomplete
     types, so let's not try it (at least for now).  Ensure we don't set a
     name if TYPE is not in the same obstack as DECL would have been placed. */

  if (TREE_CODE (decl) == TYPE_DECL
      && DECL_NAME (decl) != 0
      && (TYPE_NAME (TREE_TYPE (decl)) == 0
	  || (TREE_CODE (TYPE_NAME (TREE_TYPE (decl))) == IDENTIFIER_NODE
	      && *(IDENTIFIER_POINTER (TYPE_NAME (TREE_TYPE (decl)))) == 'T'))
      && (TREE_PERMANENT (decl)
	  || saveable_obstack == TYPE_OBSTACK (TREE_TYPE (decl))))
    TYPE_NAME (TREE_TYPE (decl)) = decl;

  return decl;
}

/* Create the predefined scalar types such as `integer_type_node' needed 
   in the gcc back-end and initialize the global binding level.  */

void
init_decl_processing ()
{
  tree endlink;

  /* The structure `tree_identifier' is the GCC tree data structure that holds
     IDENTIFIER_NODE nodes. We need to call `set_identifier_size' to tell GCC
     that we have not added any language specific fields to IDENTIFIER_NODE
     nodes.  */
  set_identifier_size (sizeof (struct tree_identifier));

  lineno = 0;

  /* incomplete_decl_finalize_hook is defined in toplev.c. It needs to be set
     by each front end to the appropriate routine that handles incomplete 
     VAR_DECL nodes. This routine will be invoked by compile_file when a  
     VAR_DECL node of DECL_SIZE zero is encountered.  */
  incomplete_decl_finalize_hook = finish_incomplete_decl;

  /* Make the binding_level structure for global names.  */
  current_function_decl = 0;
  current_binding_level = 0;
  free_binding_level = 0;
  pushlevel (0);
  global_binding_level = current_binding_level;

  /* In Ada, we use a signed type for SIZETYPE.  Use the signed type
     corresponding to the size of Pmode.  */
  sizetype = type_for_size (GET_MODE_BITSIZE (Pmode), 0);
  pushdecl (build_decl (TYPE_DECL, get_identifier (SIZE_TYPE), sizetype));

  integer_type_node = type_for_size (INT_TYPE_SIZE, 0) ;
  pushdecl (build_decl (TYPE_DECL, get_identifier ("int"), integer_type_node));
  unsigned_type_node = type_for_size (INT_TYPE_SIZE, 1);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned int"),
			unsigned_type_node));
  char_type_node = type_for_size (CHAR_TYPE_SIZE, 1);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned char"),
			char_type_node));

  longest_float_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (longest_float_type_node) = LONG_DOUBLE_TYPE_SIZE;
  layout_type (longest_float_type_node);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("longest float type"),
			longest_float_type_node));

  error_mark_node = make_node (ERROR_MARK);
  TREE_TYPE (error_mark_node) = error_mark_node;

  integer_zero_node = build_int_2 (0, 0);
  integer_one_node = build_int_2 (1, 0);

  size_zero_node = build_int_2 (0, 0);
  TREE_TYPE (size_zero_node) = sizetype;
  size_one_node = build_int_2 (1, 0);
  TREE_TYPE (size_one_node) = sizetype;

  void_type_node = make_node (VOID_TYPE);
  layout_type (void_type_node);
  TYPE_ALIGN (void_type_node) = BITS_PER_UNIT;
  void_type_decl_node
    = pushdecl (build_decl (TYPE_DECL, get_identifier ("void"),
			    void_type_node));

  ptr_void_type_node = build_pointer_type (void_type_node);

  null_pointer_node = build_int_2 (0, 0);
  TREE_TYPE (null_pointer_node) = ptr_void_type_node;
  layout_type (TREE_TYPE (null_pointer_node));

  void_ftype = build_function_type (void_type_node, NULL_TREE);
  ptr_void_ftype = build_pointer_type (void_ftype);

  /* Now declare runtime functions. */
  endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);

  /* malloc is a function declaration tree for a function to allocate
     memory.  */
  malloc_decl = create_subprog_decl (get_identifier ("__gnat_malloc"),
				     NULL_TREE,
				     build_function_type (ptr_void_type_node,
							  tree_cons (NULL_TREE,
								     sizetype,
								     endlink)),
				     NULL_TREE, 0, 1, 1, NULL_PTR);

  /* free is a function declaration tree for a function to free memory.  */

  free_decl
    = create_subprog_decl (get_identifier ("__gnat_free"), NULL_TREE,
			   build_function_type (void_type_node,
						tree_cons (NULL_TREE,
							   ptr_void_type_node,
							   endlink)),
			   NULL_TREE, 0, 1, 1, NULL_PTR);

/* set_except_occ is a function that initialize the current exception 
   occurrence of a handler with the message saved in the task-specific data */

  set_except_occ_decl
    = create_subprog_decl (get_identifier ("__set_except_occ"), NULL_TREE,
			   build_function_type (void_type_node,
						tree_cons (NULL_TREE,
							   ptr_void_type_node,
							   endlink)),
			   NULL_TREE, 0, 1, 1, NULL_PTR);

  /* Make the types and functions used for exception processing.    */
  jmpbuf_type
    = build_array_type (type_for_mode (Pmode, 0),
			build_index_type (build_int_2 (5, 0)));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("jmpbuf_t"), jmpbuf_type));
  jmpbuf_ptr_type = build_pointer_type (jmpbuf_type);

  /* Functions to get and set the jumpbuf pointer for the current thread.  */
  get_jmpbuf_decl
    = create_subprog_decl
    (get_identifier ("system__task_specific_data__get_jmpbuf_address"),
     NULL_TREE, build_function_type (jmpbuf_ptr_type, NULL_TREE),
     NULL_TREE, 0, 1, 1, NULL_PTR);

  set_jmpbuf_decl
    = create_subprog_decl
    (get_identifier ("system__task_specific_data__set_jmpbuf_address"),
     NULL_TREE,
     build_function_type (void_type_node, 
			  tree_cons (NULL_TREE, jmpbuf_ptr_type, endlink)),
     NULL_TREE, 0, 1, 1, NULL_PTR);

  /* Right now the type of an exception is a byte.  We need to
     get the actual type from the front end eventually.  */
  except_type_node = char_type_node;

  /* Function to get the current exception.  */
  get_excptr_decl
    = create_subprog_decl
    (get_identifier ("system__task_specific_data__get_gnat_exception"),
     NULL_TREE,
     build_function_type (build_pointer_type (except_type_node), NULL_TREE),
     NULL_TREE, 0, 1, 1, NULL_PTR);

  /* Functions that raise exceptions.  */
  raise_with_msg_decl
    = create_subprog_decl
      (get_identifier ("__gnat_raise_with_msg"), NULL_TREE,
       build_function_type (void_type_node,
			    tree_cons (NULL_TREE,
				       build_pointer_type (except_type_node),
				       endlink)),
       NULL_TREE, 0, 1, 1, NULL_PTR);

  raise_decl
    = create_subprog_decl
      (get_identifier ("__gnat_raise"), NULL_TREE,
       build_function_type (void_type_node,
			    tree_cons (NULL_TREE,
				       build_pointer_type (except_type_node),
				       endlink)),
       NULL_TREE, 0, 1, 1, NULL_PTR);

  raise_nodefer_decl
    = create_subprog_decl
      (get_identifier ("__gnat_raise_nodefer_with_msg"), NULL_TREE,
       build_function_type (void_type_node,
			    tree_cons (NULL_TREE,
				       build_pointer_type (except_type_node),
				       endlink)),
       NULL_TREE, 0, 1, 1, NULL_PTR);

  /* __gnat_raise_constraint_error takes no operands and never returns.  */
  raise_constraint_error_decl
    = create_subprog_decl
      (get_identifier ("__gnat_raise_constraint_error"), NULL_TREE,
       build_function_type (void_type_node, endlink),
       NULL_TREE, 0, 1, 1, NULL_PTR);

  TREE_THIS_VOLATILE (raise_constraint_error_decl) = 1;
  TREE_SIDE_EFFECTS (raise_constraint_error_decl) = 1;

  /* Likewise for __gnat_raise_program_error.  */
  raise_program_error_decl
    = create_subprog_decl
      (get_identifier ("__gnat_raise_program_error"), NULL_TREE,
       build_function_type (void_type_node, endlink),
       NULL_TREE, 0, 1, 1, NULL_PTR);

  /* Indicate that these never return.  */
  TREE_THIS_VOLATILE (raise_decl) = 1;
  TREE_THIS_VOLATILE (raise_nodefer_decl) = 1;
  TREE_THIS_VOLATILE (raise_constraint_error_decl) = 1;
  TREE_THIS_VOLATILE (raise_program_error_decl) = 1;
  TREE_TYPE (raise_decl) = build_type_variant (TREE_TYPE (raise_decl), 0, 1);
  TREE_TYPE (raise_nodefer_decl)
    = build_type_variant (TREE_TYPE (raise_nodefer_decl), 0, 1);
  TREE_TYPE (raise_constraint_error_decl)
    = build_type_variant (TREE_TYPE (raise_constraint_error_decl), 0, 1);
  TREE_TYPE (raise_program_error_decl)
    = build_type_variant (TREE_TYPE (raise_program_error_decl), 0, 1);

  /* setjmp returns an integer and has one operand, which is a pointer to
     a jmpbuf.  */
  setjmp_decl
    = create_subprog_decl
      (get_identifier ("setjmp"), NULL_TREE,
       build_function_type (integer_type_node,
			    tree_cons (NULL_TREE,  jmpbuf_ptr_type, endlink)),
       NULL_TREE, 0, 1, 1, NULL_PTR);

  DECL_BUILT_IN (setjmp_decl) = 1;
  DECL_FUNCTION_CODE (setjmp_decl) = BUILT_IN_SETJMP;
}

/* This routine is called in tree.c to print an error message for invalid use
   of an incomplete type.  */

void
incomplete_type_error (dont_care_1, dont_care_2)
     tree dont_care_1, dont_care_2;
{
  gigi_abort (404);
}

/* This function is called indirectly from toplev.c to handle incomplete 
   declarations, i.e. VAR_DECL nodes whose DECL_SIZE is zero.  To be precise,
   compile_file in toplev.c makes an indirect call through the function pointer
   incomplete_decl_finalize_hook which is initialized to this routine in
   init_decl_processing.  */

void
finish_incomplete_decl (dont_care)
     tree dont_care;
{
  gigi_abort (405);
}

/* Given a record type (RECORD_TYPE) and a chain of FIELD_DECL
   nodes (FIELDLIST), finish constructing the record or union type. 
   If HAS_REP is nonzero, this record has a rep clause; don't call
   layout_type but merely set the size and alignment ourselves. 
   If DEFER_DEBUG is nonzero, do not call the debugging routines
   on this type; it will be done later. */

void
finish_record_type (record_type, fieldlist, has_rep, defer_debug)
     tree record_type;
     tree fieldlist;
     int has_rep;
     int defer_debug;
{
  tree field;

  TYPE_FIELDS (record_type) = fieldlist;

  if (TYPE_NAME (record_type) != 0
      && TREE_CODE (TYPE_NAME (record_type)) == TYPE_DECL)
    TYPE_STUB_DECL (record_type) = TYPE_NAME (record_type);
  else
    TYPE_STUB_DECL (record_type)
      = pushdecl (build_decl (TYPE_DECL, TYPE_NAME (record_type),
			      record_type));

  /* If we had a rep clause, compute the size from the highest ending position
     plus one and the alignment from the highest actual alignment.  Otherwise,
     let GCC lay out the type.  */
  if (has_rep)
    {
      int must_be_blkmode = 0;

      TYPE_ALIGN (record_type) = MAX (BITS_PER_UNIT, TYPE_ALIGN (record_type));
      TYPE_MODE (record_type) = BLKmode;
      if (TYPE_SIZE (record_type) == 0)
	TYPE_SIZE (record_type) = size_zero_node;

      for (field = fieldlist; field; field = TREE_CHAIN (field))
	{
	  tree end_bit = size_binop (PLUS_EXPR, DECL_FIELD_BITPOS (field),
				     DECL_SIZE (field));
	  HOST_WIDE_INT bitpos = TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field));

	  /* If END_BIT is not a constant, the only way we could have been
	     called here is from maybe_pad_type when we are making the record
	     to hold a maxium-sized object.  In that case, our size is known
	     to be correct, so use it.  */
	  if (TREE_CODE (end_bit) == INTEGER_CST
	      && tree_int_cst_lt (TYPE_SIZE (record_type), end_bit))
	    TYPE_SIZE (record_type) = end_bit;

	  if (! DECL_BIT_FIELD (field))
	    TYPE_ALIGN (record_type)
	      = MAX (TYPE_ALIGN (record_type), DECL_ALIGN (field));

	  /* A record which has any BLKmode members must itself be BLKmode; it
	     can't go in a register unless the member is BLKmode only because
	     it isn't aligned.  */
	  if ((TYPE_MODE (TREE_TYPE (field)) == BLKmode
	       && ! TYPE_NO_FORCE_BLK (TREE_TYPE (field)))
	      /* Must be BLKmode if any field crosses a word boundary,
		 since extract_bit_field can't handle that in registers.  */
	      || (DECL_BIT_FIELD (field)
		  && (bitpos / BITS_PER_WORD
		      != ((TREE_INT_CST_LOW (DECL_SIZE (field)) + bitpos - 1)
			  / BITS_PER_WORD))
		  /* But there is no problem if the field is entire words.  */
		  && (TREE_INT_CST_LOW (DECL_SIZE (field))
		      % BITS_PER_WORD == 0)))
	    must_be_blkmode = 1;
	}

      /* Store the minimum size (or the specified size) as the Ada size.
	 Then round the size to the required alignment of the type.  */
      TYPE_ADA_SIZE (record_type) = TYPE_SIZE (record_type);

#ifdef ROUND_TYPE_SIZE
      TYPE_SIZE (record_type)
	= ROUND_TYPE_SIZE (record_type, TYPE_SIZE (record_type),
			   TYPE_ALIGN (record_type));
#else
      TYPE_SIZE (record_type) = round_up (TYPE_SIZE (record_type),
					  TYPE_ALIGN (record_type));
#endif

      if (! must_be_blkmode)
	TYPE_MODE (record_type)
	  = mode_for_size (TREE_INT_CST_LOW (TYPE_SIZE (record_type)),
			   MODE_INT, 1);

      /* If structure's known alignment is less than what the scalar mode
	 would need, and it matters, then stick with BLKmode.  */
      if (STRICT_ALIGNMENT
	  && ! (TYPE_ALIGN (record_type) >= BIGGEST_ALIGNMENT
		|| (TYPE_ALIGN (record_type)
		    >= TREE_INT_CST_LOW (TYPE_SIZE (record_type)))))
	{
	  if (TYPE_MODE (record_type) != BLKmode)
	    /* If this is the only reason this type is BLKmode,
	       then don't force containing types to be BLKmode.  */
	    TYPE_NO_FORCE_BLK (record_type) = 1;
	  TYPE_MODE (record_type) = BLKmode;
	}
    }
  else
    {
      /* Ensure there isn't a size already set.  There can be in an error
	 case where there is a rep clause but all fields have errors and
	 no longer have a position.  */
      TYPE_SIZE (record_type) = 0;
      layout_type (record_type);

      /* If the size is a constant, set the Ada size from the last used bit.
	 If the last field is a record, determine the last used bit from
	 the Ada size of the last field.  This is the size we can pack to.
	 Don't do this for fat pointers or records pointed to by thin
	 pointers since the field means something differently there.  */
      if (fieldlist != 0
	  && ! TYPE_IS_FAT_POINTER_P (record_type)
	  && ! TYPE_CONTAINS_TEMPLATE_P (record_type)
	  && TREE_CODE (TYPE_SIZE (record_type)) == INTEGER_CST)
	{
	  for (field = TYPE_FIELDS (record_type);
	       TREE_CHAIN (field);
	       field = TREE_CHAIN (field))
	    ;

	  if (TREE_CODE (DECL_FIELD_BITPOS (field)) == INTEGER_CST)
	    {
	      tree gnu_field_size = DECL_SIZE (field);
	      tree gnu_field_type = TREE_TYPE (field);

	      if (((TREE_CODE (gnu_field_type) == RECORD_TYPE
		    && ! TYPE_IS_FAT_POINTER_P (gnu_field_type))
		   || TREE_CODE (gnu_field_type) == UNION_TYPE)
		  && TYPE_ADA_SIZE (gnu_field_type) != 0)
		gnu_field_size = size_binop (MIN_EXPR, gnu_field_size,
					     TYPE_ADA_SIZE (gnu_field_type));

	      if (TREE_CODE (gnu_field_size) == INTEGER_CST)
		TYPE_ADA_SIZE (record_type)
		  = size_binop (PLUS_EXPR, DECL_FIELD_BITPOS (field),
				DECL_SIZE (field));
	    }
	}
    }

  if (! defer_debug)
    rest_of_type_compilation (record_type, global_bindings_p ());
}

/* Return a FUNCTION_TYPE node. RETURN_TYPE is the type returned by the
   subprogram. If it is void_type_node, then we are dealing with a procedure,
   otherwise we are dealing with a function. PARAM_DECL_LIST is a list of
   PARM_DECL nodes that are the subprogram arguments.  CICO_LIST is the
   copy-in/copy-out list to be stored into TYPE_CICO_LIST.
   RETURNS_UNCONSTRAINED is nonzero if the function returns an unconstrained
   object.  RETURNS_BY_REF is nonzero if the function returns by reference.  */

tree
create_subprog_type (return_type, param_decl_list, cico_list,
		     returns_unconstrained, returns_by_ref)
     tree return_type;
     tree param_decl_list;
     tree cico_list;
     int returns_unconstrained, returns_by_ref;
{
  /* A chain of TREE_LIST nodes whose TREE_VALUEs are the data type nodes of
     the subprogram formal parameters. This list is generated by traversing the
     input list of PARM_DECL nodes.  */
  tree param_type_list = NULL;
  tree param_decl;
  tree type;

  for (param_decl = param_decl_list; param_decl;
       param_decl = TREE_CHAIN (param_decl))
    param_type_list = tree_cons (NULL_TREE, TREE_TYPE (param_decl),
				 param_type_list);

  /* The list of the function parameter types has to be terminated by the void
     type to signal to the back-end that we are not dealing with a variable
     parameter subprogram, but that the subprogram has a fixed number of
     parameters.  */
  param_type_list = tree_cons (NULL_TREE, void_type_node, param_type_list);

  /* The list of argument types has been created in reverse
     so nreverse it.   */
  param_type_list = nreverse (param_type_list);

  type = build_function_type (return_type, param_type_list);

  /* TYPE may have been shared since GCC hashes types.  If it has a CICO_LIST
     or the new type should, make a copy of TYPE.  Likewise for
     RETURNS_UNCONSTRAINED and RETURNS_BY_REF.  */
  if (TYPE_CI_CO_LIST (type) != 0 || cico_list != 0
      || TYPE_RETURNS_UNCONSTRAINED_P (type) != returns_unconstrained
      || TYPE_RETURNS_BY_REF_P (type) != returns_by_ref)
    {
      push_obstacks (TYPE_OBSTACK (type), TYPE_OBSTACK (type));
      type = copy_node (type);
      pop_obstacks ();
    }

  TYPE_CI_CO_LIST (type) = cico_list;
  TYPE_RETURNS_UNCONSTRAINED_P (type) = returns_unconstrained;
  TYPE_RETURNS_BY_REF_P (type) = returns_by_ref;
  return type;
}

/* Return a copy of TYPE, in the same obstack as it was, but safe to modify
   in any way.  */

tree
copy_type (type)
     tree type;
{
  tree new;

  push_obstacks (TYPE_OBSTACK (type), TYPE_OBSTACK (type));
  new = copy_node (type);
  pop_obstacks ();

  TYPE_POINTER_TO (new) = 0;
  TYPE_REFERENCE_TO (new) = 0;
  TYPE_MAIN_VARIANT (new) = new;
  TYPE_NEXT_VARIANT (new) = 0;

  return new;
}

/* Return an INTEGER_TYPE of SIZETYPE with range MIN to MAX and whose
   TYPE_INDEX_TYPE is INDEX.  */

tree
create_index_type (min, max, index)
     tree min, max;
     tree index;
{
  /* First build a type for the desired range.  */
  tree type = build_index_2_type (min, max);

  /* If this type has the TYPE_INDEX_TYPE we want, return it.  Otherwise, if it
     doesn't have TYPE_INDEX_TYPE set, set it to INDEX.  If TYPE_INDEX_TYPE
     is set, but not to INDEX, make a copy of this type with the requested
     index type.  Note that we have no way of sharing these types, but that's
     only a small hole.  */
  if (TYPE_INDEX_TYPE (type) == index)
    return type;
  else if (TYPE_INDEX_TYPE (type) != 0)
    type = copy_type (type);

  TYPE_INDEX_TYPE (type) = index;
  return type;
}

/* Return a TYPE_DECL node. TYPE_NAME gives the name of the type (a character
   string) and TYPE is a ..._TYPE node giving its data type.  */

tree
create_type_decl (type_name, type, attr_list)
     tree type_name;
     tree type;
     struct attrib *attr_list;
{
  tree type_decl = build_decl (TYPE_DECL, type_name, type);
  enum tree_code code = TREE_CODE (type);

  process_attributes (type_decl, attr_list);

  /* Add this decl to the current binding level.  */
  type_decl = pushdecl (type_decl);

  /* Pass type declaration information to the debugger unless this is an
     UNCONSTRAINED_ARRAY_TYPE, which the debugger does not support,
     and ENUMERAL_TYPE or RECORD_TYPE which is handled separately,
     or a dummy type, which will be completed later.  */
  if (code == UNCONSTRAINED_ARRAY_TYPE || TYPE_IS_DUMMY_P (type))
    DECL_IGNORED_P (type_decl) = 1;
  else if (code != ENUMERAL_TYPE && code != RECORD_TYPE
      && ! (code == POINTER_TYPE && TYPE_IS_DUMMY_P (TREE_TYPE (type))))
    rest_of_decl_compilation (type_decl, NULL, global_bindings_p (), 0);

  return type_decl;
}

/* Returns a GCC VAR_DECL node. VAR_NAME gives the name of the variable.
   ASM_NAME is its assembler name (if provided).  TYPE is its data type
   (a GCC ..._TYPE node).  VAR_INIT is the GCC tree for an optional initial
   expression; NULL_TREE if none.

   CONST_FLAG is nonzero if this variable is constant.

   PUBLIC_FLAG is nonzero if this definition is to be made visible outside of
   the current compilation unit. This flag should be set when processing the
   variable definitions in a package specification.  EXTERN_FLAG is nonzero 
   when processing an external variable declaration (as opposed to a
   definition: no storage is to be allocated for the variable here). 
   STATIC_FLAG is only relevant when not at top level.  In that case
   it indicates whether to always allocate storage to the variable.   */

tree
create_var_decl (var_name, asm_name, type, var_init, const_flag, public_flag,
		 extern_flag, static_flag, attr_list)
     tree var_name;
     tree asm_name;
     tree type;
     tree var_init;
     int const_flag;
     int public_flag;
     int extern_flag;
     int static_flag;
     struct attrib *attr_list;
{
  tree var_decl
    = build_decl ((const_flag && var_init && TREE_CONSTANT (var_init)
		   /* Only make a CONST_DECL for sufficiently-small objects.
		      We consider complex double "sufficiently-small"  */
		   && TYPE_SIZE (type)
		   && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
		   && TREE_INT_CST_HIGH (TYPE_SIZE (type)) == 0
		   && (TREE_INT_CST_LOW (TYPE_SIZE (type))
		       <= GET_MODE_BITSIZE (DCmode))
		   /* For the moment, don't do this for records
		      due to the Assignment_OK flag.  */
		   && ! AGGREGATE_TYPE_P (type))
		  ? CONST_DECL : VAR_DECL, var_name, type);
  tree assign_init = 0;

  /* If this is external, throw away any initializations unless this is a
     CONST_DECL (meaning we have a constant); they will be done elsewhere.  If
     we are defining a global here, leave a constant initialization and save
     any variable elaborations for the elaboration routine.  Otherwise, if
     the initializing expression is not the same as TYPE, generate the
     initialization with an assignment statement, since it knows how
     to do the required adjustents.  */

  if (extern_flag && TREE_CODE (var_decl) != CONST_DECL)
    var_init = 0;

  if (global_bindings_p () && var_init != 0 && ! TREE_CONSTANT (var_init))
    {
      add_pending_elaborations (var_decl, var_init);
      var_init = 0;
    }
  else if (var_init != 0 && TREE_TYPE (var_init) != type)
    assign_init = var_init, var_init = 0;

  DECL_COMMON   (var_decl) = 1;
  DECL_INITIAL  (var_decl) = var_init;
  TREE_READONLY (var_decl) = const_flag;
  DECL_EXTERNAL (var_decl) = extern_flag;
  TREE_PUBLIC   (var_decl) = public_flag || extern_flag;
  TREE_CONSTANT (var_decl) = TREE_CODE (var_decl) == CONST_DECL;
  TREE_THIS_VOLATILE (var_decl) = TREE_SIDE_EFFECTS (var_decl)
    = TYPE_VOLATILE (type);

  /* At the global binding level we need to allocate static storage for the
     variable if and only if its not external. If we are not at the top level
     we allocate automatic storage unless requested not to.  */
  TREE_STATIC (var_decl) = global_bindings_p () ? !extern_flag : static_flag;

  if (asm_name != 0)
    DECL_ASSEMBLER_NAME (var_decl) = asm_name;

  process_attributes (var_decl, attr_list);

  /* Add this decl to the current binding level and generate any
     needed code and RTL. */
  var_decl = pushdecl (var_decl);
  expand_decl (var_decl);
  expand_decl_init (var_decl);

  /* If this is volatile, force it into memory.  */
  if (TREE_SIDE_EFFECTS (var_decl))
    mark_addressable (var_decl);

  if (TREE_CODE (var_decl) != CONST_DECL)
    rest_of_decl_compilation (var_decl, 0, global_bindings_p (), 0);

  if (assign_init != 0)
    expand_expr_stmt (build_binary_op (MODIFY_EXPR, NULL_TREE,
				       var_decl, assign_init));

  return var_decl;
}

/* Returns a FIELD_DECL node. FIELD_NAME the field name, FIELD_TYPE is its
   type, and RECORD_TYPE is the type of the parent.  PACKED is nonzero if
   this field is in a record type with a "pragma pack".  If SIZE is nonzero
   it is the specified size for this field.  If POS is nonzero, it is the bit
   position.  */

tree
create_field_decl (field_name, field_type, record_type, packed, size, pos)
     tree field_name;
     tree field_type;
     tree record_type;
     int packed;
     tree size, pos;
{
  tree field_decl = build_decl (FIELD_DECL, field_name, field_type);

  DECL_CONTEXT (field_decl) = record_type;
  TREE_READONLY (field_decl) = TREE_READONLY (field_type);

  /* If FIELD_TYPE is BLKmode, we must ensure this is aligned to at least
     a byte boundary since GCC cannot handle less-aligned BLKmode bitfields.
     Just handle the packed case here; we will disallow non-aligned rep
     clauses elsewhere.  */
  if (packed && TYPE_MODE (field_type) == BLKmode)
    DECL_ALIGN (field_decl) = BITS_PER_UNIT;

  /* If a size is specified, use it.  Otherwise, see if we have a size
     to use that may differ from the natural size of the object.  */
  if (size != 0)
    ;
  else if (packed)
    {
      size = rm_size (field_type);
      if (operand_equal_p (size, TYPE_SIZE (field_type), 0))
	size = 0;

      /* For a constant size larger than MAX_FIXED_MODE_SIZE, round up to
	 byte.  */
      if (size != 0 && TREE_CODE (size) == INTEGER_CST
	  && (TREE_INT_CST_HIGH (size) != 0
	      || TREE_INT_CST_LOW (size) > MAX_FIXED_MODE_SIZE))
	size = round_up (size, BITS_PER_UNIT);
    }

  /* Make a bitfield if a size is specific for two reasons: first if the size
     differs from the natural size.  Second, if the alignment is insufficient.
     There are a number of ways the latter can be true.  */
  if (size != 0 && TREE_CODE (size) == INTEGER_CST
      && (! operand_equal_p (TYPE_SIZE (field_type), size, 0)
	  || (pos != 0
	      && ! integer_zerop (size_binop (TRUNC_MOD_EXPR, pos,
					      size_int (TYPE_ALIGN
							(field_type)))))
	  || packed
	  || (TYPE_ALIGN (record_type) != 0
	      && TYPE_ALIGN (record_type) < TYPE_ALIGN (field_type))))
    {
      DECL_BIT_FIELD (field_decl) = 1;
      DECL_FIELD_SIZE (field_decl) = TREE_INT_CST_LOW (size);
      if (! packed && pos == 0)
	DECL_ALIGN (field_decl)
	  = (TYPE_ALIGN (record_type) != 0
	     ? MIN (TYPE_ALIGN (record_type), TYPE_ALIGN (field_type))
	     : TYPE_ALIGN (field_type));
    }

  DECL_PACKED (field_decl) = pos != 0 ? DECL_BIT_FIELD (field_decl) : packed;
  DECL_ALIGN (field_decl)
    = MAX (DECL_ALIGN (field_decl),
	   DECL_BIT_FIELD (field_decl)
	   ? 1 : TYPE_ALIGN (TREE_TYPE (field_decl)));

  if (pos != 0)
    {
      /* We need to pass in the alignment the DECL is known to have.
	 This is the lowest-order bit set in POS, but no more than
	 the alignment of the record, if one is specified.  Note
	 that an alignment of 0 is taken as infinite.  */
      int known_align = TREE_INT_CST_LOW (pos) & - TREE_INT_CST_LOW (pos);

      if (TYPE_ALIGN (record_type)
	  && (known_align == 0 || known_align > TYPE_ALIGN (record_type)))
	known_align = TYPE_ALIGN (record_type);

      layout_decl (field_decl, known_align);
      DECL_FIELD_BITPOS (field_decl) = pos;
      DECL_HAS_REP_P (field_decl) = 1;
    }

  return field_decl;
}

/* Returns a PARM_DECL node. PARAM_NAME is the name of the parameter,
   PARAM_TYPE is its type.  READONLY is nonzero if the parameter is
   readonly (either an IN parameter or an address of a pass-by-ref
   parameter). */

tree
create_param_decl (param_name, param_type, readonly)
     tree param_name;
     tree param_type;
     int readonly;
{
  tree param_decl = build_decl (PARM_DECL, param_name, param_type);

#ifdef PROMOTE_PROTOTYPES
  if ((TREE_CODE (param_type) == INTEGER_TYPE
       || TREE_CODE (param_type) == ENUMERAL_TYPE)
      && TYPE_PRECISION (param_type) < TYPE_PRECISION (integer_type_node))
    param_type = integer_type_node;
#endif

  DECL_ARG_TYPE (param_decl) = param_type;
  TREE_READONLY (param_decl) = readonly;
  return param_decl;
}

/* Given a DECL and ATTR_LIST, process the listed attributes.  */

void
process_attributes (decl, attr_list)
     tree decl;
     struct attrib *attr_list;
{
  for (; attr_list; attr_list = attr_list->next)
    switch (attr_list->type)
      {
      case ATTR_MACHINE_ATTRIBUTE:
	if (! valid_machine_attribute (attr_list->name, attr_list->arg,
				       decl, TREE_TYPE (decl)))
	  post_error ("specified attribute unknown on this target",
		      attr_list->error_point);
	break;

      case ATTR_LINK_ALIAS:
	assemble_alias (decl, attr_list->name);
	break;

      case ATTR_WEAK_EXTERNAL:
	if (SUPPORTS_WEAK)
	  declare_weak (decl);
	else
	  post_error ("?weak declarations not supported on this target",
		      attr_list->error_point);
	break;

      case ATTR_LINK_SECTION:
#ifdef ASM_OUTPUT_SECTION_NAME
	DECL_SECTION_NAME (decl)
	  = build_string (IDENTIFIER_LENGTH (attr_list->name),
			  IDENTIFIER_POINTER (attr_list->name));
	DECL_COMMON (decl) = 0;
#else
	post_error ("?section attributes are not supported for this target",
		    attr_list->error_point);
#endif
	break;
      }
}

/* Add some pending elaborations on the list.  */

void 
add_pending_elaborations (var_decl, var_init)
     tree var_decl;
     tree var_init;
{
  pending_elaborations
    = chainon (pending_elaborations, build_tree_list (var_decl, var_init));
}

/* Obtain any pending elaborations and clear the old list.  */

tree
get_pending_elaborations ()
{
  /* Each thing added to the list went on the end; we want it on the
     beginning.  */
  tree result = TREE_CHAIN (pending_elaborations);

  TREE_CHAIN (pending_elaborations) = 0;
  return result;
}

/* Save a copy of the current pending elaboration list and make a new
   one.  */

void
push_pending_elaborations ()
{
  struct e_stack *p = (struct e_stack *) oballoc (sizeof (struct e_stack));

  p->next = elist_stack;
  p->elab_list = pending_elaborations;
  elist_stack = p;
  pending_elaborations = build_tree_list (NULL_TREE, NULL_TREE);
}

/* Pop the stack of pending elaborations.  */

void
pop_pending_elaborations ()
{
  pending_elaborations = elist_stack->elab_list;
  elist_stack = elist_stack->next;
}

/* Return the current position in pending_elaborations so we can insert
   elaborations after that point.  */

tree
get_elaboration_location ()
{
  return tree_last (pending_elaborations);
}

/* Insert the current elaborations after ELAB, which is in some elaboration
   list.  */

void
insert_elaboration_list (elab)
     tree elab;
{
  tree next = TREE_CHAIN (elab);

  if (TREE_CHAIN (pending_elaborations))
    {
      TREE_CHAIN (elab) = TREE_CHAIN (pending_elaborations);
      TREE_CHAIN (tree_last (pending_elaborations)) = next;
      TREE_CHAIN (pending_elaborations) = 0;
    }
}

/* Returns a LABEL_DECL node for LABEL_NAME.  */

tree
create_label_decl (label_name)
     tree label_name;
{
  tree label_decl = build_decl (LABEL_DECL, label_name, void_type_node);

  DECL_CONTEXT (label_decl)     = current_function_decl;
  DECL_MODE (label_decl)        = VOIDmode;
  DECL_SOURCE_LINE (label_decl) = lineno;
  DECL_SOURCE_FILE (label_decl) = input_filename;

  return label_decl;
}

/* Returns a FUNCTION_DECL node.  SUBPROG_NAME is the name of the subprogram,
   ASM_NAME is its assembler name, SUBPROG_TYPE is its type (a FUNCTION_TYPE
   node), PARAM_DECL_LIST is the list of the subprogram arguments (a list of
   PARM_DECL nodes chained through the TREE_CHAIN field).

   INLINE_FLAG, PUBLIC_FLAG, and EXTERN_FLAG are used to set the appropriate
   fields in the FUNCTION_DECL.  */

tree
create_subprog_decl (subprog_name, asm_name, subprog_type, param_decl_list,
		     inline_flag, public_flag, extern_flag, attr_list)
     tree subprog_name;
     tree asm_name;
     tree subprog_type;
     tree param_decl_list;
     int inline_flag;
     int public_flag;
     int extern_flag;
     struct attrib *attr_list;
{
  tree return_type  = TREE_TYPE (subprog_type);
  tree subprog_decl = build_decl (FUNCTION_DECL, subprog_name, subprog_type);

  DECL_EXTERNAL (subprog_decl)  = extern_flag;
  TREE_PUBLIC (subprog_decl)    = public_flag;
  DECL_INLINE (subprog_decl)    = inline_flag;
  TREE_READONLY (subprog_decl)  = TYPE_READONLY (subprog_type);
  TREE_THIS_VOLATILE (subprog_decl) = TYPE_VOLATILE (subprog_type);
  DECL_ARGUMENTS (subprog_decl) = param_decl_list;
  DECL_RESULT (subprog_decl)    = build_decl (RESULT_DECL, 0, return_type);

  if (asm_name != 0)
    DECL_ASSEMBLER_NAME (subprog_decl) = asm_name;

  process_attributes (subprog_decl, attr_list);

  /* Add this decl to the current binding level.  */
  subprog_decl = pushdecl (subprog_decl);

  /* Output the assembler code and/or RTL for the declaration.  */
  rest_of_decl_compilation (subprog_decl, 0, global_bindings_p (), 0);

  return subprog_decl;
}

/* Count how deep we are into nested functions.  This is because
   we shouldn't call the backend function context routines unless we
   are in a nested function.  */

static int function_nesting_depth;

/* Set up the framework for generating code for SUBPROG_DECL, a subprogram
   body. This routine needs to be invoked before processing the declarations
   appearing in the subprogram.  */

void
begin_subprog_body (subprog_decl)
     tree subprog_decl;
{
  tree param_decl_list;
  tree param_decl;
  tree next_param;

  if (function_nesting_depth++ != 0)
    push_function_context ();

  announce_function (subprog_decl);

  /* Make this field nonzero so further routines know that this is not
     tentative. error_mark_node is replaced below (in poplevel) with the
     adequate BLOCK.  */
  DECL_INITIAL (subprog_decl)  = error_mark_node;

  /* This function exists in static storage. This does not mean `static' in
     the C sense!  */
  TREE_STATIC (subprog_decl)   = 1;

  /* Enter a new binding level.  */
  temporary_allocation ();
  current_function_decl = subprog_decl;
  pushlevel (0);

  make_function_rtl (subprog_decl);

  /* Push all the PARM_DECL nodes onto the current scope (i.e. the scope of the
     subprogram body) so that they can be recognized as local variables in the
     subprogram. 

     The list of PARM_DECL nodes is stored in the right order in
     DECL_ARGUMENTS.  Since ..._DECL nodes get stored in the reverse order in
     which they are transmitted to `pushdecl' we need to reverse the list of
     PARM_DECLs if we want it to be stored in the right order. The reason why
     we want to make sure the PARM_DECLs are stored in the correct order is
     that this list will be retrieved in a few lines with a call to `getdecl'
     to store it back into the DECL_ARGUMENTS field.  */
    param_decl_list = nreverse (DECL_ARGUMENTS (subprog_decl));

    for (param_decl = param_decl_list; param_decl; param_decl = next_param)
      {
	next_param = TREE_CHAIN (param_decl);
	TREE_CHAIN (param_decl) = NULL;
	pushdecl (param_decl);
      }

  /* Store back the PARM_DECL nodes. They appear in the right order. */
  DECL_ARGUMENTS (subprog_decl) = getdecls ();

  init_function_start   (subprog_decl, input_filename, lineno);
  expand_function_start (subprog_decl, 0);
}


/* Finish the definition of the current subprogram and compile it all the way
   to assembler language output.  */

void
end_subprog_body (void)
{
  poplevel (1, 0, 1);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (current_function_decl))
    = current_function_decl;

  /* Mark the RESULT_DECL as being in this subprogram. */
  DECL_CONTEXT (DECL_RESULT (current_function_decl)) = current_function_decl;

  expand_function_end (input_filename, lineno, 0);
  rest_of_compilation (current_function_decl);

  if (DECL_SAVED_INSNS (current_function_decl) == 0)
    {
      /* Stop pointing to the local nodes about to be freed.  */
      /* But DECL_INITIAL must remain nonzero so we know this
	 was an actual function definition.  */
      DECL_INITIAL (current_function_decl) = error_mark_node;
      DECL_ARGUMENTS (current_function_decl) = 0;
    }

  /* If we are not at the bottom of the function nesting stack, pop up to
     the containing function.  Otherwise show we aren't in any function
     and switch back to permanent allocation.  */
  if (--function_nesting_depth != 0)
    pop_function_context ();
  else
    {
      current_function_decl = 0;
      permanent_allocation (1);
    }
}

/* Return 1 if EXP contains a PLACEHOLDER_EXPR for any type other than T;
   i.e., if it represents a size or offset that depends on a field within a
   record other than the RECORD_TYPE denoted by T.  If T is zero,
   return 1 for any PLACEHOLDER_EXPR.

   Note that we only allow such expressions within simple arithmetic
   or a COND_EXPR.  */

static int
contains_placeholder_except_p (exp, t)
     tree exp;
     tree t;
{
  register enum tree_code code = TREE_CODE (exp);
  tree inner;

  /* If we have a WITH_RECORD_EXPR, it "cancels" any PLACEHOLDER_EXPR
     in it since it is supplying a value for it.  */
  if (code == WITH_RECORD_EXPR)
    return 0;

  switch (TREE_CODE_CLASS (code))
    {
    case 'r':
      for (inner = TREE_OPERAND (exp, 0);
	   TREE_CODE_CLASS (TREE_CODE (inner)) == 'r';
	   inner = TREE_OPERAND (inner, 0))
	;
      return (TREE_CODE (inner) == PLACEHOLDER_EXPR
	      && TREE_TYPE (inner) != t);

    case '1':
    case '2':  case '<':
    case 'e':
      switch (tree_code_length[(int) code])
	{
	case 1:
	  return contains_placeholder_except_p (TREE_OPERAND (exp, 0), t);
	case 2:
	  return (code != RTL_EXPR
		  && code != CONSTRUCTOR
		  && ! (code == SAVE_EXPR && SAVE_EXPR_RTL (exp) != 0)
		  && code != WITH_RECORD_EXPR
		  && (contains_placeholder_except_p (TREE_OPERAND (exp, 0), t)
		      || contains_placeholder_except_p (TREE_OPERAND (exp, 1),
							t)));
	case 3:
	  return
	    (code == COND_EXPR
	     && (contains_placeholder_except_p (TREE_OPERAND (exp, 0), t)
		 || contains_placeholder_except_p (TREE_OPERAND (exp, 1), t)
		 || contains_placeholder_except_p (TREE_OPERAND (exp, 2), t)));
	}
    }

  return 0;
}

#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD  BITS_PER_WORD
#endif

/* This variable keeps a table for types for each precision so that we only 
   allocate each of them once. Signed and unsigned types are kept separate.

   Note that these types are only used when fold-const requests something
   special.  Perhaps we should NOT share these types; we'll see how it
   goes later.  */
static tree signed_and_unsigned_types[MAX_BITS_PER_WORD + 1][2];

/* Return an integer type with the number of bits of precision given by  
   PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   it is a signed type.  */

tree
type_for_size (precision, unsignedp)
     unsigned precision;
     int unsignedp;
{
  tree t;
  int moment;

  if (precision <= MAX_BITS_PER_WORD
      && signed_and_unsigned_types[precision][unsignedp] != 0)
    return signed_and_unsigned_types[precision][unsignedp];

  /* Since we will keep these types around, they must be permanent.  */
  moment = suspend_momentary ();
  push_obstacks_nochange ();
  end_temporary_allocation ();

 if (unsignedp)
    t = signed_and_unsigned_types[precision][1]
      = make_unsigned_type (precision);
  else
    t = signed_and_unsigned_types[precision][0]
      = make_signed_type (precision);

  pop_obstacks ();
  resume_momentary (moment);

  return t;
}

/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */

tree
type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  return type_for_size (GET_MODE_BITSIZE (mode), unsignedp);
}

/* Return the unsigned version of a TYPE_NODE, a scalar type.  */

tree
unsigned_type (type_node)
     tree type_node;
{
  tree type = type_for_size (TYPE_PRECISION (type_node), 1);

  if (TYPE_MODULAR_P (type_node))
    {
      type = copy_node (type);
      TREE_TYPE (type) = type_node;
    }
  else if (TREE_TYPE (type_node) != 0
	   && TYPE_MODULAR_P (TREE_TYPE (type_node)))
    {
      type = copy_node (type);
      TREE_TYPE (type) = TREE_TYPE (type_node);
    }

  return type;
}

/* Return the signed version of a TYPE_NODE, a scalar type.  */

tree
signed_type (type_node)
     tree type_node;
{
  tree type = type_for_size (TYPE_PRECISION (type_node), 0);

  if (TYPE_MODULAR_P (type_node))
    {
      type = copy_node (type);
      TREE_TYPE (type) = type_node;
    }
  else if (TREE_TYPE (type_node) != 0
	   && TYPE_MODULAR_P (TREE_TYPE (type_node)))
    {
      type = copy_node (type);
      TREE_TYPE (type) = TREE_TYPE (type_node);
    }

  return type;
}

/* Return a type the same as TYPE except unsigned or signed according to
   UNSIGNEDP.  */

tree
signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (! INTEGRAL_TYPE_P (type) || TREE_UNSIGNED (type) == unsignedp)
    return type;
  else
    return type_for_size (TYPE_PRECISION (type), unsignedp);
}

/* EXP is an expression for the size of an object.  If this size contains
   discriminant references, replace them with the maximum (if MAX_P) or
   minimum (if ! MAX_P) possible value of the discriminant.  */

tree
max_size (exp, max_p)
     tree exp;
     int max_p;
{
  enum tree_code code = TREE_CODE (exp);
  tree type = TREE_TYPE (exp);

  switch (TREE_CODE_CLASS (code))
    {
    case 'd':
    case 'c':
      return exp;

    case 'r':
      /* If this contains a PLACEHOLDER_EXPR, it is the thing we want to
	 modify.  Otherwise, we abort since it is something we can't
	 handle.  */
      if (! contains_placeholder_p (exp))
	gigi_abort (406);

      type = TREE_TYPE (TREE_OPERAND (exp, 1));
      return
	max_size (max_p ? TYPE_MAX_VALUE (type) : TYPE_MIN_VALUE (type), 1);

    case '<':
      return max_p ? size_one_node : size_zero_node;

    case '1':
    case '2':
    case 'e':
      switch (tree_code_length[(int) code])
	{
	case 1:
	  return
	    fold (build1 (code, type,
			  max_size (TREE_OPERAND (exp, 0),
				    code == NEGATE_EXPR ? ! max_p : max_p)));

	case 2:
	  if (code == RTL_EXPR)
	    gigi_abort (407);

	  {
	    tree lhs = max_size (TREE_OPERAND (exp, 0), max_p);
	    tree rhs = max_size (TREE_OPERAND (exp, 1),
				 code == MINUS_EXPR ? ! max_p : max_p);

	    /* Special-case wanting the maximum value of a MIN_EXPR.
	       In that case, if one side overflows, return the other.
	       sizetype is signed, but we know sizes are non-negative.  */
	    if (max_p && code == MIN_EXPR && TREE_OVERFLOW (rhs))
	      return lhs;
	    else if (max_p && code == MIN_EXPR && TREE_OVERFLOW (lhs))
	      return rhs;
	    else
	      return fold (build (code, type, lhs, rhs));
	  }

	case 3:
	  if (code == SAVE_EXPR)
	    return exp;
	  else if (code == COND_EXPR)
	    return fold (build (MAX_EXPR, type,
				max_size (TREE_OPERAND (exp, 1), max_p),
				max_size (TREE_OPERAND (exp, 2), max_p)));
	}
    }

  gigi_abort (408);
}

/* Build a template of type TEMPLATE_TYPE from the array bounds of ARRAY_TYPE.
   EXPR is an expression that we can use to locate any PLACEHOLDER_EXPRs.
   Return a constructor for the template.  */

tree
build_template (template_type, array_type, expr)
     tree template_type;
     tree array_type;
     tree expr;
{
  tree template_elts = NULL_TREE;
  tree bound_list = NULL_TREE;
  tree field;

  if (TREE_CODE (array_type) == RECORD_TYPE
      && TYPE_LEFT_JUSTIFIED_MODULAR_P (array_type))
    array_type = TREE_TYPE (TYPE_FIELDS (array_type));

  if (TREE_CODE (array_type) == ARRAY_TYPE
      || (TREE_CODE (array_type) == INTEGER_TYPE
	  && TYPE_HAS_ACTUAL_BOUNDS_P (array_type)))
    bound_list = TYPE_ACTUAL_BOUNDS (array_type);

  /* First make the list for a CONSTRUCTOR for the template.   Go down the
     field list of the template instead of the type chain because this
     array might be an Ada array of arrays and we can't tell where the
     nested arrays stop being the underlying object.  */

  for (field = TYPE_FIELDS (template_type); field;
       (bound_list != 0
	? (bound_list = TREE_CHAIN (bound_list))
	: (array_type = TREE_TYPE (array_type))),
       field = TREE_CHAIN (TREE_CHAIN (field)))
    {
      tree bounds, min, max;

      /* If we have a bound list, get the bounds from there.  Likewise
	 for an ARRAY_TYPE.  Otherwise, if expr is a PARM_DECL with
	 DECL_BY_COMPONENT_PTR_P, use the bounds of the field in the template.
	 This will give us a maximum range.  */
      if (bound_list != 0)
	bounds = TREE_VALUE (bound_list);
      else if (TREE_CODE (array_type) == ARRAY_TYPE)
	bounds = TYPE_INDEX_TYPE (TYPE_DOMAIN (array_type));
      else if (expr != 0 && TREE_CODE (expr) == PARM_DECL
	       && DECL_BY_COMPONENT_PTR_P (expr))
	bounds = TREE_TYPE (field);
      else
	gigi_abort (411);

      min = convert (TREE_TYPE (TREE_CHAIN (field)), TYPE_MIN_VALUE (bounds));
      max = convert (TREE_TYPE (field), TYPE_MAX_VALUE (bounds));

      /* If either MIN or MAX involve a PLACEHOLDER_EXPR, we must
	 surround them with a WITH_RECORD_EXPR giving EXPR as the
	 OBJECT.  */
      if (! TREE_CONSTANT (min) && contains_placeholder_p (min))
	min = build (WITH_RECORD_EXPR, TREE_TYPE (min), min, expr);
      if (! TREE_CONSTANT (max) && contains_placeholder_p (max))
	max = build (WITH_RECORD_EXPR, TREE_TYPE (max), max, expr);

      template_elts = tree_cons (TREE_CHAIN (field), max,
				 tree_cons (field, min, template_elts));
    }

  return build_constructor (template_type, nreverse (template_elts));
}

/* Build a VMS descriptor from a Mechanism_Type, which must specify
   a descriptor type, and the GCC type of an object.  Each FIELD_DECL
   in the type contains in its DECL_INITIAL the expression to use when
   a constructor is made for the type.  GNAT_ENTITY is a gnat node used
   to print out an error message if the mechanism cannot be applied to
   an object of that type and also for the name.  */

tree
build_vms_descriptor (type, mech, gnat_entity)
     tree type;
     Mechanism_Type mech;
     Entity_Id gnat_entity;
{
  tree record_type = make_node (RECORD_TYPE);
  tree field_list = 0;
  int descriptor_size = 32;	/* Always make 32 bit forms for now. */
  int dtype;

  /* If TYPE is an unconstrained array, use the underlying array type.  */
  if (TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE)
    type = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (type))));

  if (INTEGRAL_TYPE_P (type))
    {
      switch (GET_MODE_BITSIZE (TYPE_MODE (type)))
	{
	case 8:
	  dtype = TREE_UNSIGNED (type) ? 2 : 6;
	  break;
	case 16:
	  dtype = TREE_UNSIGNED (type) ? 3 : 7;
	  break;
	case 32:
	  dtype = TREE_UNSIGNED (type) ? 4 : 8;
	  break;
	case 64:
	  dtype = TREE_UNSIGNED (type) ? 5 : 9;
	  break;
	case 128:
	  dtype = TREE_UNSIGNED (type) ? 25 : 26;
	  break;
	}
    }
  else if (FLOAT_TYPE_P (type))
    dtype = GET_MODE_BITSIZE (TYPE_MODE (type)) == 32 ? 52 : 53;
  else if (TREE_CODE (type) == ARRAY_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == INTEGER_TYPE
	   && TYPE_PRECISION (TREE_TYPE (type)) == CHAR_TYPE_SIZE)
    dtype = 14;
  else
    dtype = 0;

  if (mech == By_Descriptor)
    mech = By_Descriptor_S;
  else if (mech != By_Descriptor_S && mech != By_Descriptor_SB)
    {
      post_error ("unsupported descriptor type for &", gnat_entity);
      mech = By_Descriptor_S;
    }

  /* Make the type for a descriptor for VMS.  There are two versions,
     depending on whether this is a 32-bit machine or a 64-bit machine.  */

  if (descriptor_size == 64)
    field_list = chainon (field_list,
			  make_descriptor_field ("MBO", type_for_size (16, 1),
						 record_type, size_int (1)));
  else
    field_list = chainon (field_list,
			  make_descriptor_field ("LENGTH",
						 type_for_size (16, 0),
						 record_type,
						 size_in_bytes (type)));

  field_list = chainon (field_list,
			make_descriptor_field ("DTYPE", char_type_node,
					       record_type, size_int (dtype)));
  field_list = chainon (field_list,
			make_descriptor_field ("CLASS", char_type_node,
					       record_type, size_int (1)));

  if (descriptor_size == 64)
    {
      field_list = chainon (field_list,
			    make_descriptor_field ("MBNO", integer_type_node,
						   record_type,
						   build_int_2 (-1, -1)));
      field_list = chainon (field_list,
			    make_descriptor_field ("LENGTH",
						   type_for_size (64, 0),
						   record_type,
						   size_in_bytes (type)));
    }

  field_list
    = chainon (field_list,
	       make_descriptor_field ("POINTER",
				      type_for_size (descriptor_size, 1),
				      record_type,
				      build1 (ADDR_EXPR,
					      build_pointer_type (type),
					      build (PLACEHOLDER_EXPR,
						     type))));

  if (mech == By_Descriptor_SB)
    {
      field_list
	= chainon (field_list,
		   make_descriptor_field 
		   ("SB_L1", type_for_size (descriptor_size, 0),
		    record_type,
		    TREE_CODE (type) == ARRAY_TYPE
		    ? TYPE_MIN_VALUE (TYPE_DOMAIN (type)) : size_zero_node));
      field_list
	= chainon (field_list,
		   make_descriptor_field
		   ("SB_L2", type_for_size (descriptor_size, 0),
		    record_type,
		    TREE_CODE (type) == ARRAY_TYPE
		    ? TYPE_MAX_VALUE (TYPE_DOMAIN (type)) : size_zero_node));
    }

  finish_record_type (record_type, field_list, 0, 1);
  pushdecl (build_decl (TYPE_DECL, create_concat_name (gnat_entity, "DESC"),
			record_type));

  return record_type;
}

/* Utility routine for above code to make a field.  */

static tree
make_descriptor_field (name, type, rec_type, initial)
     char *name;
     tree type;
     tree rec_type;
     tree initial;
{
  tree field
    = create_field_decl (get_identifier (name), type, rec_type, 0, 0, 0);

  DECL_INITIAL (field) = initial;
  return field;
}

/* Build a type to be used to represent an aliased object whose nominal
   type is an unconstrained array.  This consists of a RECORD_TYPE containing
   a field of TEMPLATE_TYPE and a field of OBJECT_TYPE, which is an
   ARRAY_TYPE.  If ARRAY_TYPE is that of the unconstrained array, this
   is used to represent an arbitrary unconstrained object.  Use NAME
   as the name of the record.  */

tree
build_unc_object_type (template_type, object_type, name)
     tree template_type;
     tree object_type;
     tree name;
{
  tree type = make_node (RECORD_TYPE);
  tree template_field = create_field_decl (get_identifier ("BOUNDS"),
					   template_type, type, 0, 0, 0);
  tree array_field = create_field_decl (get_identifier ("ARRAY"), object_type,
					type, 0, 0, 0);

  TYPE_NAME (type) = name;
  TYPE_CONTAINS_TEMPLATE_P (type) = 1;
  finish_record_type (type,
		      chainon (chainon (NULL_TREE, template_field),
			       array_field),
		      0, 0);

  return type;
}

/* Update anything previously pointing to OLD_TYPE to point to NEW_TYPE.  In
   the normal case this is just two adjustments, but we have more to do
   if NEW is an UNCONSTRAINED_ARRAY_TYPE.  */

void
update_pointer_to (old_type, new_type)
     tree old_type;
     tree new_type;
{
  tree ptr = TYPE_POINTER_TO (old_type);

  if (ptr == 0)
    return;

  /* First handle the simple case.  */
  if (TREE_CODE (new_type) != UNCONSTRAINED_ARRAY_TYPE)
    {
      TREE_TYPE (ptr) = new_type;
      TYPE_POINTER_TO (new_type) = ptr;

      if (TYPE_NAME (ptr) != 0 && TREE_CODE (TYPE_NAME (ptr)) == TYPE_DECL
	  && TREE_CODE (new_type) != ENUMERAL_TYPE)
	rest_of_decl_compilation (TYPE_NAME (ptr), NULL,
				  global_bindings_p (), 0);
    }

  /* Now deal with the unconstrained array case. In this case the "pointer"
     is actually a RECORD_TYPE where the types of both fields are
     pointers to void.  In that case, copy the field list from the
     old type to the new one and update the fields' context. */
  else if (TREE_CODE (ptr) != RECORD_TYPE || ! TYPE_IS_FAT_POINTER_P (ptr))
    gigi_abort (412);

  else
    {
      tree ptr_temp_type;

      TYPE_FIELDS (ptr) = TYPE_FIELDS (TYPE_POINTER_TO (new_type));
      DECL_CONTEXT (TYPE_FIELDS (ptr)) = ptr;
      DECL_CONTEXT (TREE_CHAIN (TYPE_FIELDS (ptr))) = ptr;

      /* Rework the PLACEHOLDER_EXPR inside the reference to the
	 template bounds.  */
      ptr_temp_type = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (ptr)));
      update_pointer_to 
	(TREE_TYPE (TREE_TYPE (TYPE_FIELDS (ptr))),
	 gnat_substitute_in_type (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (ptr))),
				  TREE_CHAIN (TYPE_FIELDS (ptr)),
				  build (COMPONENT_REF, ptr_temp_type,
					 build (PLACEHOLDER_EXPR, ptr),
					 TREE_CHAIN (TYPE_FIELDS (ptr)))));

      TYPE_UNCONSTRAINED_ARRAY (ptr) = new_type;
      TYPE_POINTER_TO (new_type) = TREE_TYPE (new_type) = ptr;
      rest_of_type_compilation (ptr, global_bindings_p ());

      update_pointer_to (TYPE_OBJECT_RECORD_TYPE (old_type),
			 TYPE_OBJECT_RECORD_TYPE (new_type));
    }
}

/* Convert a pointer to a constrained array into a pointer to a fat
   pointer.  This involves making or finding a template.  */

static tree
convert_to_fat_pointer (type, expr)
     tree type;
     tree expr;
{
  tree template_type = TREE_TYPE (TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (type))));
  tree template, template_addr;
  tree etype = TREE_TYPE (expr);

  /* If EXPR is a constant of zero, we make a fat pointer that has a null
     pointer to the template and array.  */
  if (integer_zerop (expr))
    return
      build_constructor
	(type,
	 tree_cons (TYPE_FIELDS (type),
		    convert (TREE_TYPE (TYPE_FIELDS (type)), expr),
		    tree_cons (TREE_CHAIN (TYPE_FIELDS (type)),
			       convert (build_pointer_type (template_type),
					expr),
			       NULL_TREE)));

  /* If EXPR is a thin pointer, the template and data from the record.  */

  else if (TYPE_THIN_POINTER_P (etype))
    {
      tree fields = TYPE_FIELDS (TREE_TYPE (etype));

      expr = save_expr (expr);
      expr = build1 (INDIRECT_REF, TREE_TYPE (etype), expr);
      template = build_component_ref (expr, NULL_TREE, fields);
      expr = build_unary_op (ADDR_EXPR, NULL_TREE,
			     build_component_ref (expr, NULL_TREE,
						  TREE_CHAIN (fields)));
    }
  else
    /* Otherwise, build the constructor for the template.  */
    template = build_template (template_type, TREE_TYPE (etype), expr);

  template_addr = build_unary_op (ADDR_EXPR, NULL_TREE, template);

  /* The result is a CONSTRUCTOR for the fat pointer.  */
  return
    build_constructor (type,
		       tree_cons (TYPE_FIELDS (type), expr,
				  tree_cons (TREE_CHAIN (TYPE_FIELDS (type)),
					     template_addr, NULL_TREE)));
}

/* Convert to a thin pointer type, TYPE.  The only thing we know how to convert
   is something that is a fat pointer, so convert to it first if it EXPR
   is not already a fat pointer.  */

static tree
convert_to_thin_pointer (type, expr)
     tree type;
     tree expr;
{
  if (! TYPE_FAT_POINTER_P (TREE_TYPE (expr)))
    expr
      = convert_to_fat_pointer
	(TREE_TYPE (TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (type))), expr);

  /* We get the pointer to the data and use a NOP_EXPR to make it the
     proper GCC type.  */
  expr = build_component_ref (expr, NULL_TREE, TYPE_FIELDS (TREE_TYPE (expr)));
  expr = build1 (NOP_EXPR, type, expr);

  return expr;
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (type, expr)
     tree type, expr;
{
  enum tree_code code = TREE_CODE (type);
  tree etype = TREE_TYPE (expr);
  enum tree_code ecode = TREE_CODE (etype);
  tree tem;

  /* If EXPR is already the right type, we are done.  */
  if (type == etype)
    return expr;

  /* If the input is a biased type, adjust first.  */
  if (ecode == INTEGER_TYPE && TYPE_BIASED_REPRESENTATION_P (etype))
    return convert (type, fold (build (PLUS_EXPR, TREE_TYPE (etype),
				       fold (build1 (GNAT_NOP_EXPR,
						     TREE_TYPE (etype), expr)),
				       TYPE_MIN_VALUE (etype))));

  /* If the input is a left-justified modular type, we need to extract
     the actual object before converting it to any other type with the
     exception of an unconstrained array.  */
  if (ecode == RECORD_TYPE && TYPE_LEFT_JUSTIFIED_MODULAR_P (etype)
      && code != UNCONSTRAINED_ARRAY_TYPE)
    return convert (type, build_component_ref (expr, NULL_TREE,
						   TYPE_FIELDS (etype)));

  /* If converting a type that does not contain a template into one
     that does, convert to the data type and then build the template. */
  if (code == RECORD_TYPE && TYPE_CONTAINS_TEMPLATE_P (type)
      && ! (ecode == RECORD_TYPE && TYPE_CONTAINS_TEMPLATE_P (etype)))
    {
      tree obj_type = TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (type)));

      return
	build_constructor
	  (type,
	   tree_cons (TYPE_FIELDS (type),
		      build_template (TREE_TYPE (TYPE_FIELDS (type)),
				      obj_type, NULL_TREE),
		      tree_cons (TREE_CHAIN (TYPE_FIELDS (type)),
				 convert (obj_type, expr), NULL_TREE)));
    }

  /* If the input type has padding, remove it by doing a component reference
     to the field.  If the output type has padding, make a constructor
     to build the record.  */
  if (ecode == RECORD_TYPE && TYPE_IS_PADDING_P (etype))
    return convert (type, build_component_ref (expr, NULL_TREE,
					       TYPE_FIELDS (etype)));
  else if (code == RECORD_TYPE && TYPE_IS_PADDING_P (type))
    return
      build_constructor (type,
			 tree_cons (TYPE_FIELDS (type),
				    convert (TREE_TYPE (TYPE_FIELDS (type)),
					     expr),
				    NULL_TREE));

  /* There are some special cases of expressions that we process
     specially.  */
  switch (TREE_CODE (expr))
    {
    case ERROR_MARK:
      return expr;

    case TRANSFORM_EXPR:
    case NULL_EXPR:
      /* Just set its type here.  For TRANSFORM_EXPR, we will do the actual
	 conversion in gnat_expand_expr.  NULL_EXPR does not represent
	 and actual value, so no conversion is needed.  */
      TREE_TYPE (expr) = type;
      return expr;

    case STRING_CST:
    case CONSTRUCTOR:
      /* If we are converting a STRING_CST to another constrained array type,
	 just make a new one in the proper type.  Likewise for a
	 CONSTRUCTOR.  */
      if (code == ecode && AGGREGATE_TYPE_P (etype)
	  && ! (TREE_CODE (TYPE_SIZE (etype)) == INTEGER_CST
		&& TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST))
	{
	  expr = copy_node (expr);
	  TREE_TYPE (expr) = type;
	  return expr;
	}
      break;

    case UNCONSTRAINED_ARRAY_REF:
      /* Convert this to the type of the inner array by getting the address of
	 the array from the template.  */
      expr = build_unary_op (INDIRECT_REF, NULL_TREE,
			     build_component_ref (TREE_OPERAND (expr, 0),
						  get_identifier ("P_ARRAY"),
						  NULL_TREE));
      break;

    case UNCHECKED_CONVERT_EXPR:
      if (AGGREGATE_TYPE_P (type) && AGGREGATE_TYPE_P (etype))
	return convert (type, TREE_OPERAND (expr, 0));
      break;
    }

  /* Check for converting to a pointer to an unconstrained array.  */
  if (TYPE_FAT_POINTER_P (type) && ! TYPE_FAT_POINTER_P (etype))
    return convert_to_fat_pointer (type, expr);

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (etype)
      || (code == INTEGER_CST && ecode == INTEGER_CST
	  && (type == TREE_TYPE (etype) || etype == TREE_TYPE (type))))
    return fold (build1 (NOP_EXPR, type, expr));

  switch (code)
    {
    case VOID_TYPE:
      return build1 (CONVERT_EXPR, type, expr);

    case INTEGER_TYPE:
      if (TYPE_HAS_ACTUAL_BOUNDS_P (type)
	  && (ecode == ARRAY_TYPE || ecode == UNCONSTRAINED_ARRAY_TYPE))
	return unchecked_convert (type, expr);
      else if (TYPE_BIASED_REPRESENTATION_P (type))
	return fold (build1 (CONVERT_EXPR, type,
			     fold (build (MINUS_EXPR, TREE_TYPE (type),
					  convert (TREE_TYPE (type), expr),
					  TYPE_MIN_VALUE (type)))));

      /* ... fall through ... */

    case ENUMERAL_TYPE:
      return fold (convert_to_integer (type, expr));

    case POINTER_TYPE:
      /* If converting between two pointers to records denoting
	 both a template and type, adjust if needed to account
	 for any differing offsets, since one might be negative.  */
      if (TYPE_THIN_POINTER_P (etype) && TYPE_THIN_POINTER_P (type))
	{
	  tree bit_diff
	    = size_binop (MINUS_EXPR,
			  DECL_FIELD_BITPOS (TYPE_FIELDS (TREE_TYPE (etype))),
			  DECL_FIELD_BITPOS (TYPE_FIELDS (TREE_TYPE (type))));
	  tree byte_diff = size_binop (CEIL_DIV_EXPR, bit_diff,
				       size_int (BITS_PER_UNIT));

	  expr = build1 (NOP_EXPR, type, expr);
	  if (integer_zerop (byte_diff))
	    return expr;

	  return build_binary_op (PLUS_EXPR, type, expr,
				  convert_to_pointer (type, byte_diff));
	}

      /* If converting to a thin pointer, handle specially.  */
      if (TYPE_THIN_POINTER_P (type)
	  && TYPE_UNCONSTRAINED_ARRAY (TREE_TYPE (type)) != 0)
	return convert_to_thin_pointer (type, expr);

      /* If converting fat pointer to normal pointer, get the pointer to the
	 array and then convert it.  */
      else if (TYPE_FAT_POINTER_P (etype))
	expr = build_component_ref (expr, get_identifier ("P_ARRAY"),
				    NULL_TREE);

      return fold (convert_to_pointer (type, expr));

    case REAL_TYPE:
      return fold (convert_to_real (type, expr));

    case RECORD_TYPE:
      if (TYPE_LEFT_JUSTIFIED_MODULAR_P (type))
	return
	  build_constructor
	    (type, tree_cons (TYPE_FIELDS (type),
			      convert (TREE_TYPE (TYPE_FIELDS (type)), expr),
			      NULL_TREE));

      /* ... fall through ... */

    case ARRAY_TYPE:
      /* In these cases, assume the front-end has validated the conversion.
	 If the conversion is valid, it will bit a bit-wise conversion, so
	 it can be viewed as an unchecked conversion.  */
      expr = unchecked_convert (type, expr);

      /* If our result has side-effects and is of an unconstrained type,
	 make a SAVE_EXPR so that we can be sure it will only be referenced
	 once.  */
      if (TREE_SIDE_EFFECTS (expr)
	  && (TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE
	      || (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
		  && contains_placeholder_p (TYPE_SIZE (type)))))
	expr = make_save_expr (expr);
      return expr;

    case UNION_TYPE:
      /* Just validate that the type is indeed that of a field
	 of the type.  Then make the simple conversion.  */
      for (tem = TYPE_FIELDS (type); tem; tem = TREE_CHAIN (tem))
	if (TREE_TYPE (tem) == etype)
	  return build1 (CONVERT_EXPR, type, expr);

      gigi_abort (413);

    case UNCONSTRAINED_ARRAY_TYPE:
      /* If EXPR is a constrained array, take its address, convert it to a
	 fat pointer, and then dereference it.  Likewise if EXPR is a
	 record containing both a template and a constrained array.
	 Note that a record representing a left justified modular type
	 always represents a packed constrained array.  */
      if (ecode == ARRAY_TYPE
	  || (ecode == INTEGER_TYPE && TYPE_HAS_ACTUAL_BOUNDS_P (etype))
	  || (ecode == RECORD_TYPE && TYPE_CONTAINS_TEMPLATE_P (etype))
	  || (ecode == RECORD_TYPE && TYPE_LEFT_JUSTIFIED_MODULAR_P (etype)))
	return
	  build_unary_op
	    (INDIRECT_REF, NULL_TREE,
	     convert_to_fat_pointer (TREE_TYPE (type),
				     build_unary_op (ADDR_EXPR,
						     NULL_TREE, expr)));

      /* Do something very similar for converting one unconstrained
	 array to another.  */
      else if (ecode == UNCONSTRAINED_ARRAY_TYPE)
	return
	  build_unary_op (INDIRECT_REF, NULL_TREE,
			  convert (TREE_TYPE (type),
				   build_unary_op (ADDR_EXPR,
						   NULL_TREE, expr)));
      else
	gigi_abort (409);

    case COMPLEX_TYPE:
      return fold (convert_to_complex (type, expr));

    default:
      gigi_abort (410);
    }
}

/* If EXP's type is an UNCONSTRAINED_ARRAY_TYPE, return an expression that
   refers to the underlying array.  If its type has TYPE_CONTAINS_TEMPLATE_P,
   likewise return an expression pointing to the underlying array.  */

tree
maybe_unconstrained_array (exp)
     tree exp;
{
  tree new;

  switch (TREE_CODE (TREE_TYPE (exp)))
    {
    case UNCONSTRAINED_ARRAY_TYPE:
      if (TREE_CODE (exp) == UNCONSTRAINED_ARRAY_REF)
	{
	  new
	    = build_unary_op (INDIRECT_REF, NULL_TREE,
			      build_component_ref (TREE_OPERAND (exp, 0),
						   get_identifier ("P_ARRAY"),
						   NULL_TREE));
	  TREE_READONLY (new) = TREE_STATIC (new) = TREE_READONLY (exp);
	  return new;
	}

      else if (TREE_CODE (exp) == NULL_EXPR)
	return build1 (NULL_EXPR,
		       TREE_TYPE (TREE_TYPE (TYPE_FIELDS
					     (TREE_TYPE (TREE_TYPE (exp))))),
		       TREE_OPERAND (exp, 0));

    case RECORD_TYPE:
      if (TYPE_CONTAINS_TEMPLATE_P (TREE_TYPE (exp)))
	return
	  build_component_ref (exp, NULL_TREE,
			       TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (exp))));
      break;
    }

  return exp;
}

/* Return an expression that does an unchecked converstion of EXPR to TYPE.  */

tree
unchecked_convert (type, expr)
     tree type;
     tree expr;
{
  tree etype = TREE_TYPE (expr);

  /* If the expression is already the right type, we are done.  */
  if (etype == type)
    return expr;

  /* If both types types are integral just do a normal conversion.
     Likewise for a conversion to an unconstrained array.  */
  if (((INTEGRAL_TYPE_P (type)
	|| (TREE_CODE (type) == POINTER_TYPE && ! TYPE_THIN_POINTER_P (type))
	|| (TREE_CODE (type) == RECORD_TYPE
	    && TYPE_LEFT_JUSTIFIED_MODULAR_P (type)))
       && (INTEGRAL_TYPE_P (etype)
	   || (TREE_CODE (etype) == POINTER_TYPE
	       && ! TYPE_THIN_POINTER_P (etype))
	   || (TREE_CODE (etype) == RECORD_TYPE
	       && TYPE_LEFT_JUSTIFIED_MODULAR_P (etype))))
      || TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE)
    {
      tree rtype = type;

      if (INTEGRAL_TYPE_P (etype) && TYPE_BIASED_REPRESENTATION_P (etype))
	{
	  tree ntype = copy_type (etype);

	  TYPE_BIASED_REPRESENTATION_P (ntype) = 0;
	  TYPE_MAIN_VARIANT (ntype) = ntype;
	  expr = build1 (GNAT_NOP_EXPR, ntype, expr);
	}

      if (INTEGRAL_TYPE_P (type) && TYPE_BIASED_REPRESENTATION_P (type))
	{
	  rtype = copy_type (type);
	  TYPE_BIASED_REPRESENTATION_P (rtype) = 0;
	  TYPE_MAIN_VARIANT (rtype) = rtype;
	}

      expr = convert (rtype, expr);
      if (type != rtype)
	expr = build1 (GNAT_NOP_EXPR, type, expr);
    }

  /* If we are converting TO an integral type whose precision is not the
     same as its size, first unchecked convert to a record that contains
     an object of the output type.  Then extract the field. */
  else if (INTEGRAL_TYPE_P (type) && TYPE_RM_SIZE (type) != 0
	   && (TREE_INT_CST_LOW (TYPE_RM_SIZE (type))
	       != GET_MODE_BITSIZE (TYPE_MODE (type))))
    {
      tree rec_type = make_node (RECORD_TYPE);
      tree field = create_field_decl (get_identifier ("OBJ"), type, 
				      rec_type, 1, 0, 0);

      TYPE_FIELDS (rec_type) = field;
      layout_type (rec_type);

      expr = unchecked_convert (rec_type, expr);
      expr = build_component_ref (expr, NULL_TREE, field);
    }

  /* Similarly for integral input type whose precision is not equal to its
     size.  */
  else if (INTEGRAL_TYPE_P (etype) && TYPE_RM_SIZE (etype) != 0
      && (TREE_INT_CST_LOW (TYPE_RM_SIZE (etype))
	  != GET_MODE_BITSIZE (TYPE_MODE (etype))))
    {
      tree rec_type = make_node (RECORD_TYPE);
      tree field
	= create_field_decl (get_identifier ("obj"), etype, rec_type,
			     1, 0, 0);

      TYPE_FIELDS (rec_type) = field;
      layout_type (rec_type);

      expr = build_constructor (rec_type, build_tree_list (field, expr));
      expr = unchecked_convert (type, expr);
    }

  /* We have a special case when we are converting between two
     unconstrained array types.  In that case, take the address,
     convert the fat pointer types, and dereference.  */
  else if (TREE_CODE (etype) == UNCONSTRAINED_ARRAY_TYPE
	   && TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE)
    expr = build_unary_op (INDIRECT_REF, NULL_TREE,
			   build1 (UNCHECKED_CONVERT_EXPR, TREE_TYPE (type),
				   build_unary_op (ADDR_EXPR, NULL_TREE,
						   expr)));

  else
    expr = build1 (UNCHECKED_CONVERT_EXPR, type,
		   maybe_unconstrained_array (expr));

  /* If the result is an integral type whose size is not equal to
     the size of the underlying machine type, sign- or zero-extend
     the result.  We need not do this in the case where the input is
     an integral type of the same precision and signedness or if the output
     is a biased type.  */
  if (INTEGRAL_TYPE_P (type) && TYPE_RM_SIZE (type) != 0
      && ! TYPE_BIASED_REPRESENTATION_P (type)
      && (TREE_INT_CST_LOW (TYPE_RM_SIZE (type))
	  != GET_MODE_BITSIZE (TYPE_MODE (type)))
      && ! (INTEGRAL_TYPE_P (etype)
	    && TREE_UNSIGNED (type) == TREE_UNSIGNED (etype)
	    && operand_equal_p (TYPE_RM_SIZE (type),
				(TYPE_RM_SIZE (etype) != 0
				 ? TYPE_RM_SIZE (etype) : TYPE_SIZE (etype)),
				0)))
    {
      tree base_type = type_for_mode (TYPE_MODE (type), TREE_UNSIGNED (type));
      tree shift_expr
	= convert (base_type,
		   size_binop (MINUS_EXPR,
			       size_int (GET_MODE_BITSIZE (TYPE_MODE (type))),
			       TYPE_RM_SIZE (type)));
      expr
	= convert (type,
		   build_binary_op (RSHIFT_EXPR, base_type,
				    build_binary_op (LSHIFT_EXPR, base_type,
						     convert (base_type, expr),
						     shift_expr),
				    shift_expr));
    }

  return expr;
}
