/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                               A - M I S C                                */
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*                             $Revision: 1.123 $                           */
/*                                                                          */
/*   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  */
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
/* As a  special  exception,  if you  link  this file  with other  files to */
/* produce an executable,  this file does not by itself cause the resulting */
/* executable to be covered by the GNU General Public License. This except- */
/* ion does not  however invalidate  any other reasons  why the  executable */
/* file might be covered by the  GNU Public License.                        */
/*                                                                          */
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

#include "config.h"
#include <stdio.h>
#include <string.h>
#include "tree.h"
#include "rtl.h"
#include "expr.h"
#include "insn-flags.h"
#include "insn-config.h"
#include "recog.h"
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
#include "a-gtran3.h"
#include "a-trans.h"
#include "a-trans3.h"
#include "a-trans4.h"
#include "a-misc.h"
#include "a-rtree.h"
#include "flags.h"

extern char *xmalloc ();
extern char *main_input_filename;

/* Tables describing GCC tree codes used only by GNAT.  

   Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See cp-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

char *gnat_tree_code_type[] = {
  "x",
#include "a-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

int gnat_tree_code_length[] = {
  0,
#include "a-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

char *gnat_tree_code_name[] = {
  "@@dummy",
#include "a-tree.def"
};
#undef DEFTREECODE

/* gnat standard argc argv */

extern int gnat_argc;
extern char **gnat_argv;

/* Global Variables Expected by gcc: */

char *language_string = "GNU Ada";
int current_function_returns_null;
int flag_traditional;		/* Used by dwarfout.c.  */

/* Routines Expected by gcc:  */

/* For most front-ends, this is the parser for the language.  For us, we
   process the GNAT tree.  */

int
yyparse ()
{
  /* Make up what Gigi uses as a jmpbuf.  */
  size_t jmpbuf[10];

  /* call the target specific initializations */
  __gnat_initialize();

  /* Call the front-end elaboration procedures */
  adainit ();

  /* Set up to catch unhandled exceptions.  */
  if (__builtin_setjmp (jmpbuf))
    abort ();

  system__task_specific_data__set_jmpbuf_address (jmpbuf);

  immediate_size_expand = 1;

  /* Call the front end */
  _ada_gnat1drv ();

  return 0;
}

/* init gnat_argc and gnat_argv */

void 
init_gnat_args ()
{
  extern int save_argc;

  /* initialize gnat_argv with save_argv size */
  gnat_argv = (char **) malloc ((save_argc + 1) * sizeof (gnat_argv[0])); 

  /* leave the 2 first slots in gnat_argv for the program name and 
     the main source name */

  gnat_argc = 2;
}

/* Decode all the language specific options that cannot be decoded by GCC.
   The option decoding phase of GCC calls this routine on the flags that
   it cannot decode. This routine returns 1 if it is successful, otherwise
   it returns 0. */

int
lang_decode_option (p)
     char *p;
{
  if (!gnat_argc) init_gnat_args ();

  if (!strncmp (p, "-I", 2))
    {
      /* pass the -I switches as-is */
      gnat_argv[gnat_argc] = p;
      gnat_argc ++;
      return 1;
    }

  if (!strncmp (p, "-gnat", 5))
    {
      /* recopy the switches without the 'gnat' prefix */

      gnat_argv[gnat_argc] =  (char *) malloc (strlen (p) - 3);
      gnat_argv[gnat_argc][0] = '-';
      strcpy (gnat_argv[gnat_argc] + 1, p + 5);
      gnat_argc ++;
      return 1;
    }

  return 0;
}

/* Perform all the initialization steps that are language-specific.  */

void
lang_init ()
{
  extern char **save_argv;

  if (!gnat_argc) init_gnat_args ();

  gnat_argv [0] = save_argv[0];     /* name of the command */ 
  gnat_argv [1] = input_filename;   /* name of the main source */
  gnat_argv [gnat_argc] = 0;      /* end of argv */

  main_input_filename = input_filename;

}

/* Perform all the finalization steps that are language-specific.  */

void
lang_finish ()
{}

/* Return a short string identifying this language to the debugger.  */

char *
lang_identify ()
{
  return "ada";
}

/* If DECL has a cleanup, build and return that cleanup here.
   This is a callback called by expand_expr.  */

tree
maybe_build_cleanup (decl)
     tree decl;
{
  /* There are no cleanups in C.  */
  return NULL_TREE;
}

/* Print any language-specific compilation statistics.  */

void
print_lang_statistics ()
{}

/* integrate_decl_tree calls this function, but since we don't use the
   DECL_LANG_SPECIFIC field, this is a no-op.  */

void
copy_lang_decl (node)
     tree node;
{
}

/* Hooks for print-tree.c:  */

void
print_lang_decl (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{}

void
print_lang_type (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  switch (TREE_CODE (node))
    {
    case FUNCTION_TYPE:
      print_node (file, "ci_co_list", TYPE_CI_CO_LIST (node), indent + 4);
      break;

    case ENUMERAL_TYPE:
      print_node (file, "RM size", TYPE_RM_SIZE_ENUM (node), indent + 4);
      break;

    case INTEGER_TYPE:
      if (TYPE_MODULAR_P (node))
	print_node (file, "modulus", TYPE_MODULUS (node), indent + 4);
      else if (TYPE_HAS_ACTUAL_BOUNDS_P (node))
	print_node (file,"actual bounds", TYPE_ACTUAL_BOUNDS (node),
		    indent + 4);
      else
	print_node (file, "index type", TYPE_INDEX_TYPE (node), indent + 4);

      print_node (file, "RM size", TYPE_RM_SIZE_INT (node), indent + 4);
      break;

    case ARRAY_TYPE:
      print_node (file,"actual bounds", TYPE_ACTUAL_BOUNDS (node), indent + 4);
      break;

    case RECORD_TYPE:
      if (TYPE_IS_FAT_POINTER_P (node) || TYPE_CONTAINS_TEMPLATE_P (node))
	print_node (file, "unconstrained array",
		    TYPE_UNCONSTRAINED_ARRAY (node), indent + 4);
      else
	print_node (file, "Ada size", TYPE_ADA_SIZE (node), indent + 4);
      break;
    }
}

void
print_lang_identifier (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{}

/* Expands GNAT-specific GCC tree nodes.  The only ones we support here are
   TRANSFORM_EXPR, UNCHECKED_CONVERT_EXPR, and NULL_EXPR.  */

static rtx
gnat_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  tree type = TREE_TYPE (exp);
  tree new;
  rtx result;

  /* Update EXP to be the new expression to expand.  */

  switch (TREE_CODE (exp))
    {
    case TRANSFORM_EXPR:
      /* If we will ignore our result, just generate code.  Otherwise,
	 expand it.  */
      if (target == const0_rtx || TREE_CODE (type) == VOID_TYPE)
	{
	  gnat_to_code (TREE_COMPLEXITY (exp));
	  return target;
	}

      new = gnat_to_gnu (TREE_COMPLEXITY (exp));

      /* If we were to take the address of this node, do it now.  */
      if (TREE_TRANSFORM_ADDR (exp))
	new = build_unary_op (ADDR_EXPR, NULL_TREE, new);

      /* If convert was called on this TRANSFORM_EXPR, it will now have a type,
	 so we must do the conversion now.  */
      if (type != error_mark_node)
	new = convert (type, new);
      break;

    case UNCHECKED_CONVERT_EXPR:
      /* If we're converting between an aggregate and non-aggregate type
	 and we have a MEM TARGET, we can't use it, since MEM_IN_STRUCT_P
	 would be set incorrect.  */
      if (target != 0 && GET_CODE (target) == MEM
	  && (MEM_IN_STRUCT_P (target)
	      != AGGREGATE_TYPE_P (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	target = 0;

      /* If the input and output are both the same mode (usually BLKmode),
	 just return the expanded input since we want just the bits.  But
	 we can't do this if the output is more strictly aligned than
	 the input.  */
      if (TYPE_MODE (type) == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))
	  && ((TYPE_ALIGN (type)
	       <= TYPE_ALIGN (TREE_TYPE (TREE_OPERAND (exp, 0))))
	      || (TYPE_ALIGN (TREE_TYPE (TREE_OPERAND (exp, 0)))
		  >= BIGGEST_ALIGNMENT)))
	new = TREE_OPERAND (exp, 0);

      /* If either mode is BLKmode, memory will be involved, so do this
	 via pointer punning.  Likewise, this doesn't work if there
	 is an alignment issue.  But we must do it for types that are known
	 to be aligned properly.  */
      else if ((TYPE_MODE (type) == BLKmode
		|| TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == BLKmode)
	       && ((TYPE_ALIGN (type)
		    <= TYPE_ALIGN (TREE_TYPE (TREE_OPERAND (exp, 0))))
		   || (TYPE_ALIGN (TREE_TYPE (TREE_OPERAND (exp, 0)))
		       >= BIGGEST_ALIGNMENT)
		   || TYPE_ALIGN_OK_P (type)
		   || TYPE_ALIGN_OK_P (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	{
	  new = build_unary_op (INDIRECT_REF, NULL_TREE,
				convert
				(build_pointer_type (type),
				 build_unary_op (ADDR_EXPR, NULL_TREE,
						 TREE_OPERAND (exp, 0))));
	  result = expand_expr (new, target, tmode, modifier);
	  if (GET_CODE (result) != MEM)
	    gigi_abort (204);

	  /* Since this is really the underlying object, set the flags from
	     the underlying type.  */
	  MEM_VOLATILE_P (result) = TREE_THIS_VOLATILE (TREE_OPERAND (exp, 0));
	  RTX_UNCHANGING_P (result) = TREE_READONLY (TREE_OPERAND (exp, 0));
	  MEM_IN_STRUCT_P (result)
	    = AGGREGATE_TYPE_P (TREE_TYPE (TREE_OPERAND (exp, 0)));
	  return result;
	}

      /* Otherwise make a union of the two types, convert to the union, and
	 extract the other value.  */
      else
	{
	  tree in_type, union_type, in_field, out_field;

	  /* If this is inside the LHS of an assignment, this would generate
	     bad code, so abort.  */
	  if (TREE_ADDRESSABLE (exp))
	    gigi_abort (202);

	  in_type = TREE_TYPE (TREE_OPERAND (exp, 0));
	  union_type = make_node (UNION_TYPE);
	  in_field = create_field_decl (get_identifier ("in"),
					in_type, union_type, 0, 0, 0);
	  out_field = create_field_decl (get_identifier ("out"),
					 type, union_type, 0, 0, 0);

	  TYPE_FIELDS (union_type) = chainon (in_field, out_field);
	  layout_type (union_type);

	  /* Though this is a "union", we can treat its size as that of
	     the output type in case the size of the input type is variable.
	     If the output size is a variable, use the input size.  */
	  TYPE_SIZE (union_type) = TYPE_SIZE (type);
	  if (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	      && TREE_CODE (TYPE_SIZE (in_type)) == INTEGER_CST)
	    TYPE_SIZE (union_type) = TYPE_SIZE (in_type);

	  new = build (COMPONENT_REF, type,
		       build1 (CONVERT_EXPR, union_type,
			       TREE_OPERAND (exp, 0)),
		       out_field);
	}

      break;

    case NULL_EXPR:
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);

      /* Now make a temporary RTL the same as expr.c does.  For
	 now, don't support variable-sized objects.  */
      if (TYPE_MODE (type) == BLKmode || TREE_ADDRESSABLE (type))
	{
	  int size = int_size_in_bytes (type);
	  rtx tem;

	  /* We really can't handle variable-sized objects here, but we're
	     not going to do anything with it, so just allocate 1 byte.  */
	  if (size == -1)
	    size = 1;

	  tem = assign_stack_temp (TYPE_MODE (type), size, 0);
	  MEM_IN_STRUCT_P (tem) = AGGREGATE_TYPE_P (type);
	  return tem;
	}
      else
	{
	  int unsignedp = TREE_UNSIGNED (type);

	  return gen_reg_rtx (promote_mode (type, TYPE_MODE  (type),
					    &unsignedp, 0));
	}

    case USE_EXPR:
      if (target != const0_rtx)
	gigi_abort (203);

      /* First write a volatile ASM_INPUT to prevent anything from being
	 moved.  */
      result = gen_rtx (ASM_INPUT, VOIDmode, "");
      MEM_VOLATILE_P (result) = 1;
      emit_insn (result);

      result = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode,
			    modifier);
      emit_insn (gen_rtx (USE, VOIDmode, result));
      return target;

    case GNAT_NOP_EXPR:
      return expand_expr (build1 (NOP_EXPR, type, TREE_OPERAND (exp, 0)),
			  target, tmode, modifier);

    case UNCONSTRAINED_ARRAY_REF:
      /* If we are evaluating just for side-effects, just evaluate our
	 operand.  Otherwise, abort since this code should never appear
	 in a tree to be evaluated (objects aren't unconstrained).  */
      if (target == const0_rtx || TREE_CODE (type) == VOID_TYPE)
	return expand_expr (TREE_OPERAND (exp, 0), const0_rtx,
			    VOIDmode, modifier);

      /* ... fall through ... */

    default:
      gigi_abort (201);
    }

  return expand_expr (new, target, tmode, modifier);
}

/* Make a TRANSFORM_EXPR to later expand GNAT_NODE into an object
   of GNU_TYPE.  */

tree
make_transform_expr (gnat_node, gnu_type)
     Node_Id gnat_node;
     tree gnu_type;
{
  tree gnu_result = build (TRANSFORM_EXPR, gnu_type);

  TREE_SIDE_EFFECTS (gnu_result) = 1;
  TREE_COMPLEXITY (gnu_result) = gnat_node;
  return gnu_result;
}

/* Update the setjmp buffer BUF with the current stack pointer.  We assume
   here that a __builtin_setjmp was done to BUF.  */

void
update_setjmp_buf (buf)
     tree buf;
{
  enum machine_mode sa_mode = Pmode;
  rtx stack_save;

#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    sa_mode = insn_operand_mode[(int) CODE_FOR_save_stack_nonlocal][0];
#endif

  stack_save
    = gen_rtx (MEM, sa_mode,
	       memory_address
	       (sa_mode,
		plus_constant (expand_expr (build_unary_op (ADDR_EXPR,
							    NULL_TREE, buf),
					    NULL_RTX, VOIDmode, 0),
			       2 * GET_MODE_SIZE (Pmode))));

#ifdef HAVE_setjmp
  if (HAVE_setjmp)
    emit_insn (gen_setjmp ());
#endif

  emit_stack_save (SAVE_NONLOCAL, &stack_save, NULL_RTX);
}

/* Record the current code position in GNAT_NODE.  */

void
record_code_position (gnat_node)
     Node_Id gnat_node;
{
  if (global_bindings_p ())
    save_gnu_tree (gnat_node, get_elaboration_location (), 1);
  else
    /* Always emit another insn in case marking the last insn
       addressable needs some fixups.  */
    save_gnu_tree (gnat_node,
		   (tree) emit_note (NULL_PTR, NOTE_INSN_DELETED), 1);
}

/* Insert the code for GNAT_NODE at the position saved for that node.  */

void
insert_code_for (gnat_node)
     Node_Id gnat_node;
{
  if (global_bindings_p ())
    {
      push_pending_elaborations ();
      gnat_to_code (gnat_node);
      insert_elaboration_list (get_gnu_tree (gnat_node));
      pop_pending_elaborations ();
    }
  else
    {
      rtx insns;

      start_sequence ();
      mark_all_temps_used ();
      gnat_to_code (gnat_node);
      insns = get_insns ();
      end_sequence ();
      emit_insns_after (insns, (rtx) get_gnu_tree (gnat_node));
    }
}

/* Performs whatever initialization steps needed by the language-dependent
   lexical analyzer.

   Define the additional tree codes here.  This isn't the best place to put
   it, but it's where g++ does it.  */

void
init_lex ()
{
  lang_expand_expr = gnat_expand_expr;

  tree_code_type
    = (char **) realloc (tree_code_type,
			 sizeof (char *) * LAST_GNAT_TREE_CODE);
  tree_code_length
    = (int *) realloc (tree_code_length,
		       sizeof (int) * LAST_GNAT_TREE_CODE);
  tree_code_name
    = (char **) realloc (tree_code_name,
			 sizeof (char *) * LAST_GNAT_TREE_CODE);

  bcopy ((char *) gnat_tree_code_type,
	 (char *) (tree_code_type + (int) LAST_AND_UNUSED_TREE_CODE),
	 ((LAST_GNAT_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	  * sizeof (char *)));

  bcopy ((char *)gnat_tree_code_length,
	 (char *) (tree_code_length + (int) LAST_AND_UNUSED_TREE_CODE),
	 ((LAST_GNAT_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	  * sizeof (int)));

  bcopy ((char *) gnat_tree_code_name,
	 (char *) (tree_code_name + (int) LAST_AND_UNUSED_TREE_CODE),
	 ((LAST_GNAT_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	  * sizeof (char *)));
}

/* Sets some debug flags for the parsed. It does nothing here.  */

void
set_yydebug (value)
     int value;
{}


/* Override the regular abort to call gigi_abort since it gives more useful
   crash error messages.  If abort is a macro, we can't do this.  */

#ifndef abort

void
abort ()
{
  gigi_abort (999);
}
#endif

/* Return the alignment for GNAT_TYPE.  */

int
get_type_alignment (gnat_type)
     Entity_Id gnat_type;
{
  return TYPE_ALIGN (gnat_to_gnu_type (gnat_type)) / BITS_PER_UNIT;
}

/* GNU_TYPE is a type. Determine if it should be passed by reference by
   default.  */

int
default_pass_by_ref (gnu_type)
     tree gnu_type;
{
  /* We pass aggregates by reference if they are sufficiently large.
     The choice of constant here is somewhat arbitrary.  */
  return (AGGREGATE_TYPE_P (gnu_type)
	  && (TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST
	      || TREE_INT_CST_HIGH (TYPE_SIZE (gnu_type)) != 0
	      || (TREE_INT_CST_LOW (TYPE_SIZE (gnu_type))
		  > (HOST_WIDE_INT) 8 * BITS_PER_WORD)
	      || TREE_OVERFLOW (TYPE_SIZE (gnu_type))));
}

/* GNU_TYPE is the type of a subprogram parameter.  Determine from the type if
   it should be passed by reference. */

int
must_pass_by_ref (gnu_type)
     tree gnu_type;
{
  /* We pass only unconstrained objects, those required by the language
     to be passed by reference, and objects of variable size.  The latter
     is more efficient, avoids problems with variable size temporaries,
     and does not produce compatibility problems with C, since C does
     not have such objects.  */
  return (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE
	  || (AGGREGATE_TYPE_P (gnu_type) && TYPE_BY_REFERENCE_P (gnu_type))
	  || (TYPE_SIZE (gnu_type) != 0
	      && TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST));
}
