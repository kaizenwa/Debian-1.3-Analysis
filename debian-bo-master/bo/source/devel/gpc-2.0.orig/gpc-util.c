/* Misc routines & definitions for the GPC 
   Copyright (C) 1987, 1988, 1989 Free Software Foundation, Inc.

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
 *
 */

#include "config.h"
#include <stdio.h>

#include "tree.h"
#include "c-tree.h"
#include "input.h"
#include "rtl.h"
#include "obstack.h"
#include "flags.h"

#include "gpc-parse.h"
#include "gpc-defs.h"
#include "rts/rts-types.h"

/* Use this as default now */
#define LAZY_IO_TEXT

#ifdef LAZY_IO_TEXT
# define LAZY_IO
#endif

/* This is not used. It's required to compile gpc-decl.c */
tree ridpointers[1];

/* move to tree.h */
extern rtx  emit_string_move PROTO ((tree, tree, tree));
extern void emit_string_pad  PROTO ((rtx, rtx, int));

/* Required to allocate space for modified program name.
 * see get_main_program_name().
 */
extern struct obstack permanent_obstack;

static void init_any PROTO((tree, int));

tree pascal_array_type_nelts PROTO((tree));

/* This is TRUE if we are in 'TYPE' declaration part
 * It is written by the parser,
 * checked in build_pascal_pointer_type().
 */

extern int defining_types;

/* A list of types already defined in this type_declaration_part */

extern tree current_type_list;

/*
 * Name of the main program, usually "main"
 */
char *gpc_main = "main";

/* Option -fsetlimit:NNNNN changes the upper bound of "unbound"
 * integer type sets (default upper bound = DEFAULT_SET_SIZE)
 */

/* While reading options we can't make tree nodes */
int  requested_set_size = 0;
tree integer_set_size   = NULL_TREE;

/* Default set type for unbounded integer sets, size is
 * integer_set_size
 */
tree integer_set_type_node;

/* a pointer to integer type */
tree integer_ptr_type_node;

/* Size of the file control block in bits */
/* This is referenced by stor-layout.c */
tree size_of_file_type;

/* BOOLEAN_TYPE node */
tree boolean_type_node;

/* TEXT and FILE type nodes */
/* TEXT is a predefined file type in ISO Pascal */
tree text_type_node;

/* COMPLEX type node */
tree complex_type_node;

/* pascal nodes */
tree boolean_false_node;
tree boolean_true_node;
tree integer_maxint_node;
tree identifier_output;
tree identifier_input;
tree global_input_file_node;
tree global_output_file_node;

tree real_zero_node;

/* index type for character arrays (strings) */
tree char_array_index_type_node;

tree string_schema_proto_type;

/* GPC extension: String parameter to a C routine */
tree cstring_type_node;

tree empty_arglist;

/* Extended Pascal nodes */
tree complex_zero_node;
tree real_max_node;
tree real_min_node;
tree real_eps_node;
tree char_max_node;

/* Required implementation dependent canonical-string-types
 * for TIME and DATE return values,
 * TIMESTAMP and BINDINGTYPE records.
 */
tree gpc_type_TIME;
tree gpc_type_DATE;
tree gpc_type_TIMESTAMP;
tree gpc_type_BINDINGTYPE;

/*
 * these *_id's are used to tell
 * grokdeclarator what and how we are currently defining things.
 */
tree const_id;
tree type_id;
tree inline_id;
tree varparm_id;
tree volatile_id;
tree auto_id;
tree extern_id;
tree static_id;

/* If we have module contructors */   
tree collect_constructors_now = NULL_TREE;

/* Shorthands used in building integer range types
 * A is a tree type low_bound and B is an INT value of the high_bound
 */
#define INT_RANGE_TYPE(a,b) \
	build_range_type (integer_type_node, a, build_int_2 (b, 0))

/* Makes life easier with required record types */
#define DO_FIELD(name, type) 				     \
	grokfield (input_filename, 0, get_identifier (name), \
		   build_tree_list (NULL_TREE, type), NULL_TREE)

/*
 * Generate unique identifier_nodes of the form "_%s_%d_" if
 */
tree
get_unique_identifier (lead, valid)
char *lead;
int  valid;
{
  char idbuf [ 256 ];
  static int idcount = 0;
  
  if (valid)
    sprintf (idbuf, "%s_%d", lead, idcount++);
  else
    sprintf (idbuf, "_%s_%d_", lead, idcount++);
  return get_identifier (idbuf);
}

tree
error_level (str)
char *str;
{
    error ("Level 0 ISO Pascal does not implement `%s'", str);
    return error_mark_node;
}

/* Clear the target set */
clear_set (the_set)
     tree the_set;
{
  tree type  = TREE_TYPE (the_set);
  rtx target = expand_expr (the_set,
			    NULL_TREE,
			    TYPE_MODE (type),
			    0);
  clear_storage (target, expr_size (the_set));
}

/*
  Construct one set member from the LOWER and UPPER as range bounds.
  If UPPER is NULL_TREE, then the member is not a closed interval,
  but rather a single member.

  Returns a TREE_LIST node with TREE_PURPOSE as the lower bound and
  TREE_VALUE as the upper bound of each set member.

 */

tree
construct_set_member (lower, upper)
tree lower, upper;
{
  tree t1;

  if (lower == error_mark_node || upper == error_mark_node)
    return build_tree_list (NULL_TREE, NULL_TREE);

  t1 = TREE_TYPE (lower);

  if (! ORDINAL_TYPE (TREE_CODE (t1)))
    {
      error ("Set constructor elements must be of ordinal type");
      return build_tree_list (NULL_TREE, NULL_TREE);
    }
    
  if (upper == NULL_TREE)
    return build_tree_list (lower, upper);
  else
    {
      if (TREE_CODE (t1) != TREE_CODE (TREE_TYPE (upper)))
	{
	  error ("set range upper and lower bound are of incompatible type");
	  return build_tree_list (NULL_TREE, NULL_TREE);
	}
      if (TREE_CODE (lower) == INTEGER_CST &&
	  TREE_CODE (upper) == INTEGER_CST &&
	  tree_int_cst_lt (upper, lower))
	{
	  warning ("specified set member range is empty");
	  return NULL_TREE;
	}
    }
  return build_tree_list (lower, upper);
}

/*
 * Build a constructor node for set elements.
 * This is later converted to a set with construct_set ()
 *
 */
tree
build_set_constructor (members)
tree members;
{
  tree node;
  tree m;
  int konstant = 1;

  if (members == error_mark_node)
    return error_mark_node;

  for (m = members; m ; m = TREE_CHAIN(m))
    {
      if (!TREE_PURPOSE (m) && !TREE_VALUE (m))
	return error_mark_node;			/* Error in elements */
      else if (! TREE_CONSTANT (TREE_PURPOSE (m))
	       || (TREE_VALUE (m) && ! TREE_CONSTANT (TREE_VALUE (m))))
	konstant = 0;
    }
  
  node = make_node (CONSTRUCTOR);

  /* Mark it as a SET_TYPE constructor */
  /* The correct type is not known yet, so this is as good as any other */
  TREE_TYPE (node) =
    build_set_type (build_tree_list (void_type_node,
				     INT_RANGE_TYPE (integer_zero_node, -1)));

  CONSTRUCTOR_ELTS (node) = members;
  TREE_CONSTANT (node)    = konstant;
  TREE_ADDRESSABLE (node) = 1;
  return node;
}

/* Build a SET_TYPE node from a set constructor.

   CONSTRUCTOR is a CONSTRUCTOR type node whose CONSTRUCTOR_ELTS
   contains a TREE_LIST of the elements(/pairs) of the set.

   If ARG_TYPE == 0, TARGET_OR_TYPE is a VAR_DECL node where we should
   	construct our set.
   If ARG_TYPE == 1, TARGET_OR_TYPE is a SET_TYPE node which we should
   	model our new set after.
   If ARG_TYPE == 2, TARGET_OR_TYPE is a SET_TYPE node passed as a
        parameter to a function. (Special case for empty set constructor
	passing)

   TARGET_OR_TYPE is NULL_TREE if we don't know the destination
   where we should put the constructed set, nor the type we should
   be cloning to our constructed set.

   Return NULL_TREE if we assigned the set to the existing
   TARGET_SET, else return the constructor whose TREE_TYPE
   type we now set
 */

tree
construct_set (constructor, target_or_type, arg_type)
tree constructor;
tree target_or_type;
int  arg_type;
{
  tree elements;
  tree elem;
  tree result = NULL_TREE;
  tree set_low, set_high;
  tree type;
  tree basetype;
  rtx target;

  tree this_set_type;

  if (constructor == error_mark_node)
    return error_mark_node;

  elements = CONSTRUCTOR_ELTS (constructor);
  
  if (! elements && arg_type != 2)
    if (arg_type == 0 && target_or_type) /* Clear storage in the target */
      {
	clear_set (target_or_type);
	return NULL_TREE;
      }
    else
      return constructor;

  if (arg_type == 0 && target_or_type)
    {
      this_set_type = TREE_TYPE (target_or_type);
      set_low  = TYPE_MIN_VALUE (this_set_type);
      set_high = TYPE_MAX_VALUE (this_set_type);
    }
  else
    {
      int  set_is_ready = 0;
      tree type;
      enum tree_code tcode;

      if (elements)
	type = TREE_TYPE (TREE_PURPOSE (elements));
      else if (! target_or_type)
	/* Empty set constructor to unknown set? */
	abort ();
      else
	type = TREE_TYPE (target_or_type);

      tcode = TREE_CODE (type);

      if (target_or_type)
	{
	  this_set_type = target_or_type;
	  set_low  = TYPE_MIN_VALUE (this_set_type);
	  set_high = TYPE_MAX_VALUE (this_set_type);
	  set_is_ready = 1;
	}
      else if (tcode == INTEGER_TYPE && ! TREE_TYPE (type))
	{
	  /* avoids [ -maxint .. maxint ] storage request :-) */
	  tree min = NULL_TREE;
	  tree max = NULL_TREE;
	  int varies = 0;
	  
	  /* Scan for the min and max values */
	  for (elem = elements; elem; elem = TREE_CHAIN (elem))
	    {
	      tree min_c = TREE_PURPOSE (elem);
	      tree max_c = TREE_VALUE (elem);
	      
	      /* If this is a single element, not a range */
	      if (! max_c)
		max_c = min_c;
	      
	      if (TREE_CODE (min_c) != INTEGER_CST)
		varies++;
	      else
		if (!min || tree_int_cst_lt (min_c, min))
		  min = min_c;
	      
	      if (TREE_CODE (max_c) != INTEGER_CST)
		varies++;
	      else
		if (!max || tree_int_cst_lt (max, max_c))
		  max = max_c;
	    }
	      
	  /* @@@ I do not yet check if the type of the variable part
	     is a subrange whose allowed ranges would
	     change the calculated set bounds, or prevent
	     the warnings since it could not be outside bounds.
	   */
	      
	  if (!varies)  /* Constant low bound */
	    {
	      set_low  = min;
	      set_high = max;
		  
	      /* If constant bounds, but too big */
	      if (max && tree_int_cst_lt
		  	    (integer_set_size,
			     fold
			     (build
			      (PLUS_EXPR, integer_type_node,
			       integer_one_node,
			       fold
			       (build (MINUS_EXPR,
				       integer_type_node,
				       max, min))))))
		max = NULL_TREE;
	      
	      if (!max)
		{
		  set_high = fold (build (PLUS_EXPR, integer_type_node,
					  min, integer_set_size));
		  warning ("constructing limited integer set `%d..%d'",
			   TREE_INT_CST_LOW (set_low),
			   TREE_INT_CST_LOW (set_high));
		}
	    }
	  else
	    {
	      this_set_type = integer_set_type_node;
		  
	      set_low  = TYPE_MIN_VALUE (TYPE_DOMAIN (this_set_type));
	      set_high = TYPE_MAX_VALUE (TYPE_DOMAIN (this_set_type));
	      set_is_ready = 1;
	      warning ("constructing limited integer set `%d..%d'",
		       TREE_INT_CST_LOW (set_low),
		       TREE_INT_CST_LOW (set_high));
	    }
	      
	  if (varies)
	    warning ("The limited integer set contains variable elements");
	}
      else
	{
	  if (tcode == INTEGER_TYPE) /* subrange type */
	    {
	      set_low  = TYPE_MIN_VALUE (TREE_TYPE (type));
	      set_high = TYPE_MAX_VALUE (TREE_TYPE (type));
	    }
	  else
	    {
	      set_low  = TYPE_MIN_VALUE (type);
	      set_high = TYPE_MAX_VALUE (type);
	    }
	}

      if (! set_is_ready)
	this_set_type =
	  build_set_type
	    (build_tree_list
	     (type,
	      build_range_type (TREE_TYPE (set_low),
				set_low, set_high)));
	  
      if (int_size_in_bytes (type) == -1)
	abort ();   /* non-constant bounds not yet supported (Why???) */
    }

  /* Now we know the type of the target set, so we switch the constructor
   * type to be the correct type
   */
  TREE_TYPE (constructor) = this_set_type;

  basetype = TREE_TYPE (this_set_type);

  /* Check for ordinal subranges */
  if (TREE_CODE (basetype) == INTEGER_TYPE
      && TREE_TYPE (basetype)
      && TREE_CODE (TREE_TYPE (basetype)) != INTEGER_TYPE)
    basetype = TREE_TYPE (basetype);

  /* Check that the constructor elements are of valid type */
  
  for (elem = elements; elem; elem = TREE_CHAIN (elem))
    {
      tree p = TREE_PURPOSE (elem);
      tree v = TREE_VALUE   (elem);
      enum tree_code pcode = TREE_CODE (TREE_TYPE (p));
      enum tree_code vcode;

      /* @@@ Must be more strict */
      if (v == NULL_TREE)
	{
	  if (! (pcode == TREE_CODE (basetype)
		 || (pcode == INTEGER_TYPE
		     && TREE_TYPE (TREE_TYPE (p))
		     && TREE_CODE (TREE_TYPE (TREE_TYPE (p)))
		            == TREE_CODE (basetype))))
	    error ("Type of set constructor element must be the set basetype");
	}
      else
	{
	  vcode = TREE_CODE (TREE_TYPE (v));

	  if (! (pcode == TREE_CODE (basetype)
		 || (pcode == INTEGER_TYPE
		     && TREE_CODE (TREE_TYPE (TREE_TYPE (p)))
		     	== TREE_CODE (basetype))))
	    error ("Type of set constructor range lower bound is not set basetype");
	  if (! (vcode == TREE_CODE (basetype)
		 || (vcode == INTEGER_TYPE
		     && TREE_CODE (TREE_TYPE (TREE_TYPE (p)))
		     	== TREE_CODE (basetype))))
	    error ("Type of set constructor range upper bound is not set basetype");
	}
    }

  return constructor;
}

tree
check_set_bounds (type, range)
     tree type, range;
{
  tree lo = TYPE_MIN_VALUE (range);
  tree hi = TYPE_MAX_VALUE (range);

  /* Check if it is a nonlimited integer type, like "set of integer" */
  if (TREE_CODE (type) == INTEGER_TYPE
      && !TREE_TYPE (type)		 /* Allows huge subrange sets */
      && TREE_CODE (lo) == INTEGER_CST
      && TREE_CODE (hi) == INTEGER_CST
      && TREE_INT_CST_LOW (integer_set_size) < (TREE_INT_CST_LOW (hi)
						- TREE_INT_CST_LOW (lo) + 1))
    {
      warning ("Integer set size limited to `%d' elements from low bound",
	       TREE_INT_CST_LOW (integer_set_size));
      
      warning ("Use -fsetlimit:NUMBER to change the limit at compile time");
      
      range = build_range_type
		(integer_type_node,
		 lo,
		 build_int_2 (TREE_INT_CST_LOW (lo) +
			      TREE_INT_CST_LOW (integer_set_size) - 1, 0));
    }

  return range;
}

/* tree.c:
   Construct, lay out and return the 'SET OF type' with the number of
   elements specified by the range of values of MEMBERS.

   MEMBERS is a TREE_LIST node whose TREE_PURPOSE is the type of the
   set and TREE_VALUE is the range of values it has.

   The SET_TYPE has the same fields as the ARRAY_TYPE.

   The TREE_TYPE of the SET_TYPE node is the type of the set members.
   (If that is subrange, then TREE_TYPE(TREE_TYPE(type)) is the
    base type of the set)
 */

tree
build_set_type (members)
tree members;
{
  tree d     = TREE_VALUE (members);
  tree dlow  = TYPE_MIN_VALUE (d);
  tree dhigh = TYPE_MAX_VALUE (d); 
  tree type;

  if (! ORDINAL_TYPE (TREE_CODE (TREE_PURPOSE (members)))
      && TREE_PURPOSE (members) != void_type_node)
    {
      error ("Set base type must be an ordinal type");
      return integer_set_type_node;
    }

  type		     = make_node (SET_TYPE);
  TREE_TYPE (type)   = TREE_PURPOSE (members);
  TYPE_DOMAIN (type) = d;

  /* @@ hash set types here... */

  if (TYPE_SIZE (type) == 0)
    layout_type (type);

  return type;
}

/*
 * Returns the base type of an ordinal subrange, or the type
 * itself if it is not a subrange
 */
tree
base_type (type)
     tree type;
{
  /* Check for ordinal subranges */
  if (TREE_CODE (type) == INTEGER_TYPE
      && TREE_TYPE (type)
#if 0
      /* If it is an integer subrange return the range */
      && TREE_CODE (TREE_TYPE (type)) != INTEGER_TYPE
#endif
      )
    type = TREE_TYPE (type);

  return type;
}

/* FILETYPE is the file component type.
 * If INDEX_TYPE == NULL_TREE, this is a normal sequential access file.
 * If INDEX_TYPE != NULL_TREE, then it is a TREE_LIST with:
 *
 *   TREE_PURPOSE: index type name or new index type (unused)
 *   TREE_VALUE:   domain of the index type
 */

tree
build_file_type (filetype, index_type)
     tree filetype;
     tree index_type;
{
    tree File = make_node (FILE_TYPE);

    /* This file is not a TEXT type file */
    TYPE_FILE_TEXT (File)  = NULL_TREE;
    TYPE_DOMAIN (File)     = NULL_TREE;

    /* For a direct access file type just store the indices */
    if (index_type)
      if (TREE_VALUE (index_type) != error_mark_node)
	TYPE_DOMAIN (File) = TREE_VALUE (index_type);
      else
	error ("File treated as sequential access file");

    TREE_TYPE (File) = filetype;

    layout_type (File);
    if (!TYPE_SIZE (File))
      TYPE_SIZE (File) = size_of_file_type;

    return File;
}

/* information in variant field into tree list node:
 *
 * VARSEL:
 *  TREE_LIST(PURPOSE (PURPOSE(selector_id), VALUE(selector_type)),
 *	      VALUE   (PURPOSE(file),        VALUE(line)))
 *            
 *
 * FIELDLIST:
 *  TREE_LIST(PURPOSE   (case_list),
 *	      VALUE	(field_decl_node))
 */
/* @@@@ Extended Pascal: Implement ranges as case selectors */
/*
 * @@@@ Where is the problem?  I just implemented it in gpc-parse.y, 
 * and it worked immediately.  (But this is beyoud Extended Pascal.)  -- PG
 */
tree
build_record_variant_part (file, line, varsel, fieldlist)
char *file;
int line;
tree varsel, fieldlist;
{
    tree ret, type;
    tree link, fields = NULL_TREE;

    /* @@@@ check here that the case labels are of type
       TREE_PURPOSE (varsel) */

    for (link = fieldlist; link; link = TREE_CHAIN (link))
	fields = chainon (fields, TREE_VALUE (link));

    type = finish_struct (start_struct (UNION_TYPE, NULL_TREE),
			  fields,
			  NULL_TREE);

    ret = grokfield (file, line,
		     NULL_TREE, /* field name */
		     build_tree_list (NULL_TREE, type),
		     NULL_TREE); /* field width */
    return ret;
}

/*
 * Set the packed flag of the type.
 *
 * PACKED packs all immediate ARRAY_TYPE components of TYPE.
 *
 * PACKED ARRAY [ 5..10,1..20 ] OF CHAR; is actually two arrays
 * which should both be packed.
 *
 * @@@ Hmm, maybe PACKED ARRAY [ 1..10 ] of array_type_name;
 * @@@ should not pack array_type_name???
 * @@@ If so, grok_packed requires info of the levels to pack
 * @@@ from the parser. there is no way to find it out here.
 */
tree
grok_packed (type)
     tree type;
{
  return pascal_type_variant (type, TYPE_QUALIFIER_PACKED, 0);
}

/* Assign a (char,string-type) to (char,string-type)
 * String may be either a fixed or variable length string.
 *
 * @@@ No checks are made for capacity violations.
 * @@@@ Not even trivial checks.
 */
void
assign_string (target, source)
     tree target;
     tree source;
{
  tree length = NULL_TREE;
  tree t_type = TREE_TYPE (target);
  tree s_type = TREE_TYPE (source);

  switch (TREE_CODE (s_type)) {
  case CHAR_TYPE:
    /* target must be a string-type since source is a char */
    expand_expr_stmt (build_modify_expr (build_array_ref (PASCAL_STRING_VALUE (target),
							  integer_one_node),
					 NOP_EXPR,
					 source));
    length = integer_one_node;
    break;

  case RECORD_TYPE: /* String schema */
  case STRING_CST:
  case ARRAY_TYPE:
    if (is_string_type (source)
	&& (is_string_type (target) || TREE_CODE (t_type) == CHAR_TYPE))
      {
	/* @@@@ Check if source longer than target!!! */
	if (TREE_CODE (t_type) == CHAR_TYPE)
	  expand_expr_stmt (build_modify_expr
			    (target,
			     NOP_EXPR,
			     (build_array_ref (PASCAL_STRING_VALUE (source),
					       integer_one_node))));
	else
	  {
	    length = PASCAL_STRING_LENGTH (source);

	    /* The target needs to be an lvalue, but the
	       source might be e.g. an array returned by a
	       function or whatever.

	       The conversions here are only made so that
	       the string copy can copy the characters of
	       possibly different MODE strings to each other
	     */
	    emit_string_move (build_unary_op (ADDR_EXPR,
					      PASCAL_STRING_VALUE (target), 0),
			      build1(ADDR_EXPR,
				     string_type_node,
				     PASCAL_STRING_VALUE (source)),
			      length);
	  }
	break;
      }
    
    /* FALLTHROUGH */
  default:
    error("Only pascal string-type and char type are assignment compatible with a string");
    return;
  }

  if (is_variable_string_type (t_type))
    expand_expr_stmt
      (build_modify_expr (PASCAL_STRING_LENGTH (target), NOP_EXPR, length));
  else if (length)
    {
      tree t_length = TYPE_MAX_VALUE (TYPE_DOMAIN (t_type));
      
      if (TREE_CODE (length) != INTEGER_CST
	  || TREE_CODE (t_length) != INTEGER_CST
	  || !tree_int_cst_equal (length, t_length))
	{
	  /* Blank pad the fixed string target */
	  
	  emit_string_pad
	    (expand_expr (build_unary_op (ADDR_EXPR,
					  build_array_ref (target,
							   build_binary_op (PLUS_EXPR,
									    integer_one_node,
									    length, 0)),
					  0),
			  NULL_RTX, Pmode, 0),
	     expand_expr (build_binary_op (MINUS_EXPR, t_length, length, 0),
			  NULL_RTX, VOIDmode, 0),
	     ' ');
	}
    }
}

/*
 * Create a new variable of type TYPE with a name constructed from
 * template+unique number return the new variable.
 */
tree
make_new_variable (name_template, type)
     char *name_template;
     tree type;
{
  tree var = start_decl (get_unique_identifier (name_template, 1),
			 build_tree_list (NULL_TREE, type),
			 0, NULL_TREE, NULL_TREE);
  finish_decl (var, NULL_TREE, NULL_TREE);

  return var;
}


/*
 * Create a new VAR_DECL of variable string type model.
 * Copy the value of DATA to the new VAR_DECL.
 *
 * If MODEL is NULL_TREE, the string capacity is the same
 * as the char or string type node in DATA. If DATA is a string schema,
 * the capacity will be the length of the string in DATA currently.
 *
 * If DATA is NULL_TREE, don't copy anything, just create a new string
 * variable.
 *
 * If MODEL and DATA are both NULL_TREE, abort().
 *
 * Return the new string.
 */
tree
new_string_by_model (model, data, copy)
tree model;
tree data;
int  copy;
{
  tree type;
  tree length;
  tree new_string;

  if (! model)
    {
      if (! data)
	abort ();

      length = PASCAL_STRING_LENGTH (data);
  
      type = build_pascal_string_schema (length);
    }
  else
    type = model;
  
  new_string = make_new_variable ("_GPC_TMP_STRING_", type);

  /* Assign the DATA value to the new string schema */
  if (data && copy)
    assign_string (new_string, data);

  /* Initialize the capacity schema discriminant field to length of string */
  if (PASCAL_TYPE_STRING (type))
    init_any (new_string, 0);

  return new_string;
}

/* Declare variables.
 *
 * In Extended Pascal the initial value is associated with the TYPE like:
 * var i,j,k,l : integer value 0;
 *
 * From an interface module we only output external references
 * to variables.
 *
 * These are similar to C variable definition:
 * extern int foo;
 */
void
declare_vars (var_list, type, initial_value, qualifiers, interface)
tree  var_list, type, initial_value, qualifiers;
int   interface;
{
    tree v;
    tree d;
    tree tt_normal = build_tree_list (NULL_TREE, type);
    tree tt_static = NULL_TREE;
    tree tt_extern;
    tree pending   = NULL_TREE;
    
    if (initial_value &&
	TREE_CODE (TREE_TYPE (initial_value)) != TREE_CODE (type))
      {
	error ("initial value is of wrong type");
	initial_value = NULL_TREE;
      }

    if (qualifiers)
      {
	if (pedantic)
	  warning ("ISO Pascal does not support storage class qualifiers");

	tt_normal = chainon (tt_normal, qualifiers);
	
      }

    if (interface)
      {
	/* The variables get declared in the implementation part,
	 * not in the interface.
	 */

	tt_extern = build_tree_list (NULL_TREE, type);
	if (qualifiers)
	  tt_extern = chainon (tt_extern, qualifiers);

	tt_extern = chainon (tt_extern,
			     build_tree_list (NULL_TREE, extern_id));
      }

    for (v = var_list; v; v = TREE_CHAIN(v))
      {
        tree name = TREE_VALUE (v);
	tree tt   = interface ? tt_extern : tt_normal;
	tree init = interface ? NULL_TREE : initial_value;
        tree q;

        if (interface && ! we_are_loading_a_gpi_file)
          handle_autoexport (name);

#if 1 /* Peter */
        /*
         * When making everything in the main program `static',
         * it makes all top-level variables of the main program
         * go into the stack segment, so they are limited in size
         * on many systems.  This means that you are forced either
         * to use modules or to allocate these variables on the
         * heap, even if no dynamical allocation is needed.
         *
         * More concrete: If the following does not work:
         *
         *   Program Test;
         *
         *   Const
         *     n = 1024;
         *
         *   Var
         *     A, B, C: array [ 1..n, 1..n ] of Real;
         *
         *   ...
         *
         * most scientists won't use the compiler (because they
         * don't dare to use such exotic things as pointers :-).
         */
	if (! current_module->main_program
	    && top_level_p (0)
	    && ! (name_exported_p (name) || we_are_loading_a_gpi_file))
#else /* not Peter */
	if (top_level_p (0) 
            && ! (name_exported_p (name) || we_are_loading_a_gpi_file))
#endif /* Peter */
	  {
	    /* Don't make unexported variables visible */
	    if (interface)
	      continue;

	    if (! tt_static)
	      {
		tt_static = build_tree_list (NULL_TREE, type);
		if (qualifiers)
		  tt_static = chainon (tt_static, qualifiers);

		tt_static = maybe_make_static (tt_static);
	      }
	    tt = tt_static;
	  }

	d = start_decl (name, tt, init != NULL_TREE, NULL_TREE, NULL_TREE);

	/* In case we have any user qualifiers for the storage class,
	 * undo the capitalization for the name.
	 */
	if (qualifiers)
	  DECL_ASSEMBLER_NAME (d) = de_capitalize (name);

	/* In case we have an __asmname__ qualifier,
         * use the specified name.
         */
        for (q = qualifiers; q; q = TREE_CHAIN (q))
          if (TREE_VALUE (q) == extern_id && TREE_PURPOSE (q))
            {
              if (TREE_CODE (TREE_PURPOSE (q)) == IDENTIFIER_NODE)
                DECL_ASSEMBLER_NAME (d) = TREE_PURPOSE (q);
              else
                DECL_ASSEMBLER_NAME (d) = get_identifier (
                                            TREE_STRING_POINTER (
                                              TREE_PURPOSE (q)));
            }

	finish_decl (d, init, NULL_TREE);
      }

    /* Store the unexported variable declarations of the interface module
     * to the current_module so that the implementation module can
     * declare them as local variables.
     *
     * The list contains also pending function decl nodes, but for them
     * the first TREE_PURPOSE field is void_type_node
     *
     * TREE_LIST = pending decl in the list
     *   TREE_PURPOSE : NULL_TREE
     *   TREE_VALUE   : TREE_LIST
     *                    TREE_PURPOSE	: type
     *                    TREE_VALUE    : chain of pending var names
     *			  TREE_CHAIN	: TREE_LIST
     *					    TREE_PURPOSE : initial_value
     *					    TREE_VALUE   : qualifiers
     *   TREE_CHAIN   : next pending decl in the list
     *		
     */
    if (interface && ! we_are_loading_a_gpi_file)
      {
	tree this_one = build_tree_list (NULL_TREE,
					 tree_cons (type,
						    var_list,
						    build_tree_list (initial_value, qualifiers)));
	current_module->pending_decls = chainon (current_module->pending_decls,
						 this_one);
      }
}


/*
 * Each conformant array argument is passed as follows:
 *
 * The array and it's conformant bounds are in the following
 * argument order:
 *
 *    1   2      n-1  n  n+1     n+2     n+3
 *   LO1 HI1 ... LOn HIn CARRAY0 CARRAY1 CARRAY2
 *
 * Where the indices always appear paired:
 * LO1 HI1  - Top level indices of CARRAYs
 * ...
 * LOn HIn  - lowest level indices of CARRAYs
 *
 * CARRAYx  - the n/2-dimension arrays.
 *
 * All of the indice's types are flagged with
 * PASCAL_TYPE_CONFORMANT_INDEX. 
 *
 * The first thing after indices is the conformant
 * array type with variable sized bounds.
 *
 * If an ARRAY_TYPE is not preceedeed by a list of indices,
 * its indices are the same as the immediately preceeding
 * conformant array's. (If some of this is not true, abort())
 */
void
handle_formal_conf_array_param_list (idlist, type, varparm, protected)
tree idlist;
tree type;
int  varparm;
int  protected;
{
  tree link;
  int  packed     = TREE_PURPOSE (type) != NULL_TREE;
  tree index_list = TREE_PURPOSE (TREE_VALUE (type));
  tree carray     = TREE_VALUE (TREE_VALUE (type));
  tree parm;

  /*
   * Build array with variable indices. The type is NOT laid
   * out. These are handled specially when actual parameters
   * are passed to this kind of formal in convert_arguments.
   *
   * The TREE_LIST node of the indices part of the conformant
   * array schema is flagged; it tells grokdeclarator to
   * take special care of it.
   */
  for (link = index_list; link; link = TREE_CHAIN (link))
    {
      /* Build a new type-node so that we can safely flag it */
      tree ctype = pascal_type_variant (TREE_VALUE (link),
					TYPE_QUALIFIER_CONFORMANT, 1);
      tree lo = TREE_PURPOSE (TREE_PURPOSE (link));
      tree hi = TREE_VALUE   (TREE_PURPOSE (link));
      tree index_type = build_tree_list (NULL_TREE, ctype);
      tree itype;

      /* Push lower bound */
      parm = build_tree_list (build_tree_list (index_type, lo),
			      build_tree_list (NULL_TREE, NULL_TREE));
      push_parm_decl (parm);
      
      /* Push upper bound */
      parm = build_tree_list (build_tree_list (index_type, hi),
			      build_tree_list (NULL_TREE, NULL_TREE));
      push_parm_decl (parm);

      {
	tree lodecl = lookup_name (lo);
	tree hidecl = lookup_name (hi);

	if (! lodecl || ! hidecl ||
	    TREE_CODE (lodecl) != PARM_DECL ||
	    TREE_CODE (hidecl) != PARM_DECL)
	  abort ();

	itype = build_range_type (TREE_TYPE (lodecl), lodecl, hidecl);
      }

      {
	/* Prevent conformant array size calculation until
	 * we are enter the function body
	 */
	int old_value = immediate_size_expand;
	immediate_size_expand = 0;
	carray = build_array_type (carray, itype);
	C_TYPE_VARIABLE_SIZE (carray) = 1;
	immediate_size_expand = old_value;
      }
    }

  if (packed)
    grok_packed (carray);

  /* If this is a protected parameter, make it read-only */
  if (protected)
    type = build_type_variant (carray, 1, 0);

  if (varparm)
    {
      carray = build_reference_type (carray);
      C_TYPE_VARIABLE_SIZE (carray) = 1;
    }

  /* add conformant array parameters */
  for (link = idlist; link; link = TREE_CHAIN(link))
    {
      parm = build_tree_list (build_tree_list ((build_tree_list (NULL_TREE, carray)),
					       TREE_VALUE (link)),
			      build_tree_list (NULL_TREE, NULL_TREE));
      push_parm_decl (parm);
    }
}

void
handle_formal_param_list (idlist, type, varparm, protected)
tree idlist;
tree type;
int varparm;
int protected;
{
  tree link;
  tree parm;

  if (TREE_CODE (idlist) == CALL_EXPR)
    {
      tree p;
      /* idlist == CALL_EXPR
       * type   == TREE_LIST (NULL_TREE, type_node)
       */
      parm  = build_tree_list (build_tree_list (type, idlist),
			       build_tree_list (NULL_TREE, NULL_TREE));

      push_parm_decl (parm);

      p = lookup_name (TREE_OPERAND (idlist, 0));

      if (!p || TREE_CODE (p) != PARM_DECL)
	abort ();

      /* This is a standard routine parameter. Auto-call it on reference. */
      PASCAL_PROCEDURE_PARAMETER(p) = 1;
    }
  else
    {
      /* If this is a protected parameter, make it read-only */
      if (protected)
	type = build_type_variant (type, 1, 0);

      if (varparm && type != error_mark_node)
	type = build_reference_type (type);
      
      type = build_tree_list (NULL_TREE, type);

      for (link = idlist; link; link = TREE_CHAIN(link))
	{
	  parm = build_tree_list (build_tree_list (type, TREE_VALUE (link)),
				  build_tree_list (NULL_TREE, NULL_TREE));
	  push_parm_decl (parm);
	}
    }
}

tree
p_grokfields (list, type)
tree list, type;
{
    tree link, ret = NULL_TREE;

    type = build_tree_list (NULL_TREE, type);

    for (link = list; link; link = TREE_CHAIN (link))
	ret = chainon(ret, grokfield ("", 0, 
				      TREE_VALUE(link), type, NULL_TREE));

    return ret;
}

/* no params to function */
tree ptype_void;

/* single param rts calls */
tree ptype_int;
tree ptype_char;
tree ptype_bool;
tree ptype_double;
tree ptype_complex;

/* any number of any parms */
tree ptype_vararg;

/* Some rts function formal parameters are constructed
   by this routine. */
tree
do_ptype(type, var, last)
tree type;
int  var;	/* 0 : use type; 1 : build_reference; 2 : build_pointer */
int  last;
{
    tree temp;

    if (var == 1)
	type = build_reference_type (type);
    else if (var == 2)
	type = build_pointer_type (type);

    temp = build_tree_list (NULL_TREE, type);

    /* True if this is the last parameter to the function */
    if (last)
	temp = chainon (temp, ptype_void);

    return temp;
}

/* Declares some useful rts call types */
void
declare_rts_types ()
{
    ptype_void   = build_tree_list (NULL_TREE, void_type_node);
    ptype_vararg = NULL_TREE;

    ptype_int    = do_ptype (integer_type_node, 0, 1);
    ptype_char   = do_ptype (char_type_node, 0, 1);

    ptype_bool   = do_ptype (boolean_type_node, 0, 1);
    ptype_double = do_ptype (double_type_node, 0, 1);
    ptype_complex = do_ptype (complex_type_node, 0, 1);
}

/* inline functions are not here */
static struct
  {
      int  val;
      char *name;
      rtx  symref;
      tree fun;
  } rts[] = {
    { r_WRITE,	"_p_write",	NULL_RTX, NULL_TREE},
    { r_READ,	"_p_read",	NULL_RTX, NULL_TREE},
    { r_INITFDR,"_p_initfdr",	NULL_RTX, NULL_TREE},
    { r_LAZYGET,"_p_lazyget",	NULL_RTX, NULL_TREE},
    { r_COLLECT,"_p_collect",	NULL_RTX, NULL_TREE},

    { p_ARCTAN, "_p_arctan",	NULL_RTX, NULL_TREE},
    { p_COS,	"_p_cos", 	NULL_RTX, NULL_TREE},
    { p_EXP,	"_p_exp", 	NULL_RTX, NULL_TREE},
    { p_LN,	"_p_ln", 	NULL_RTX, NULL_TREE},
    { p_SIN,	"_p_sin", 	NULL_RTX, NULL_TREE},
    { p_SQRT,	"_p_sqrt", 	NULL_RTX, NULL_TREE},

    { p_DISPOSE,"_p_dispose", 	NULL_RTX, NULL_TREE},
    { p_EOF,	"_p_eof", 	NULL_RTX, NULL_TREE},
    { p_EOLN,	"_p_eoln", 	NULL_RTX, NULL_TREE},
    { bp_FREEMEM, "_p_dispose",	NULL_RTX, NULL_TREE},
    { p_GET,	"_p_get", 	NULL_RTX, NULL_TREE},
    { bp_GETMEM, "_p_new",	NULL_RTX, NULL_TREE},
    { p_NEW,	"_p_new", 	NULL_RTX, NULL_TREE},
    { p_PAGE,	"_p_page", 	NULL_RTX, NULL_TREE},
    { p_PUT,	"_p_put", 	NULL_RTX, NULL_TREE},
    { p_RESET,	"_p_reset", 	NULL_RTX, NULL_TREE},
    { p_REWRITE,"_p_rewrite", 	NULL_RTX, NULL_TREE},
    { p_MARK,	"_p_mark", 	NULL_RTX, NULL_TREE},
    { p_RELEASE,"_p_release", 	NULL_RTX, NULL_TREE},
    { p_CLOSE,	"_p_close",	NULL_RTX, NULL_TREE},
    { r_POW,	"_p_pow",	NULL_RTX, NULL_TREE},
    { r_EXPON,	"_p_expon",	NULL_RTX, NULL_TREE},

    /* Extended Pascal libcalls */

    /* Functions with COMPLEX_TYPE parameters */
    { z_ARCTAN, "_p_z_arctan",	NULL_RTX, NULL_TREE},
    { z_COS,	"_p_z_cos", 	NULL_RTX, NULL_TREE},
    { z_EXP,	"_p_z_exp", 	NULL_RTX, NULL_TREE},
    { z_LN,	"_p_z_ln", 	NULL_RTX, NULL_TREE},
    { z_SIN,	"_p_z_sin", 	NULL_RTX, NULL_TREE},
    { z_SQRT,	"_p_z_sqrt", 	NULL_RTX, NULL_TREE},
    { z_POW,	"_p_z_pow",	NULL_RTX, NULL_TREE},
    { z_EXPON,	"_p_z_expon",	NULL_RTX, NULL_TREE},

    /* Should inline p_POLAR */
    { p_POLAR,	"_p_polar",	NULL_RTX, NULL_TREE},
    { p_ARG,	"_p_arg",	NULL_RTX, NULL_TREE},

    { p_GETTIMESTAMP, "_p_gettimestamp", NULL_RTX, NULL_TREE},
    { p_DATE,   "_p_date",	NULL_RTX, NULL_TREE},
    { p_TIME,   "_p_time",	NULL_RTX, NULL_TREE},
    { p_HALT,   "_p_halt",	NULL_RTX, NULL_TREE},

    { p_EMPTY,  "_p_empty",	NULL_RTX, NULL_TREE},
    { p_EXTEND, "_p_extend",	NULL_RTX, NULL_TREE},
    { p_UPDATE, "_p_update",    NULL_RTX, NULL_TREE},
    { p_POSITION,  "_p_position",NULL_RTX, NULL_TREE},
    { p_LASTPOSITION,"_p_lastposition",NULL_RTX, NULL_TREE},
    { p_SEEKWRITE, "_p_seekwrite", NULL_RTX, NULL_TREE},
    { p_SEEKREAD,  "_p_seekread",  NULL_RTX, NULL_TREE},
    { p_SEEKUPDATE,"_p_seekupdate",NULL_RTX, NULL_TREE},

    /* string functions */
    { p_LENGTH, "_p_string", NULL_RTX, NULL_TREE},
    { p_INDEX,  "_p_string", NULL_RTX, NULL_TREE},
    { p_SUBSTR, "_p_string", NULL_RTX, NULL_TREE},
    { p_TRIM,   "_p_string", NULL_RTX, NULL_TREE},
    /* lexicographic string comparisons */
    { p_EQ,     "_p_string", NULL_RTX, NULL_TREE},
    { p_LT,     "_p_string", NULL_RTX, NULL_TREE},
    { p_GT,	"_p_string", NULL_RTX, NULL_TREE},
    { p_NE,	"_p_string", NULL_RTX, NULL_TREE},
    { p_LE,	"_p_string", NULL_RTX, NULL_TREE},
    { p_GE,	"_p_string", NULL_RTX, NULL_TREE},
    /* string comparisons with space padding */
    { '=',      "_p_string", NULL_RTX, NULL_TREE},
    { NEQ,	"_p_string", NULL_RTX, NULL_TREE},
    { '<',      "_p_string", NULL_RTX, NULL_TREE},
    { LTE,	"_p_string", NULL_RTX, NULL_TREE},
    { '>',	"_p_string", NULL_RTX, NULL_TREE},
    { GTE,	"_p_string", NULL_RTX, NULL_TREE},

    /* Read from string, write to string */
    { p_READSTR, "_p_readstr", NULL_RTX, NULL_TREE},
    { p_WRITESTR,"_p_writestr",NULL_RTX, NULL_TREE},

    /* Binding routines */
    { p_BIND,    "_p_bind",    NULL_RTX, NULL_TREE},
    { p_BINDING, "_p_binding", NULL_RTX, NULL_TREE},
    { p_UNBIND,  "_p_unbind",  NULL_RTX, NULL_TREE},
    
    /* GPC extensions. Pax used to have this. */
    { p_DEFINESIZE,"_p_definesize",NULL_RTX, NULL_TREE},

    { 0,        NULL,		NULL_RTX, NULL_TREE}
};

/*
   RTS_ID : What routine we are calling
   VALUE_TYPE : return value from calling NAME
   FORMAL_PARAMS : List of formal parameters; NULL_TREE means varargs.
   PARAMS is a list--a chain of TREE_LIST nodes--in which the
          TREE_VALUE of each node is a parameter-expression.
*/

static tree
rts_call (rts_id, value_type, formal_params, params)
int rts_id;
tree value_type;
tree formal_params;
tree params;
{
    register int value;
    int index;
    tree fun;
    
    for (index = 0; (value = rts[index].val) != rts_id; index++)
      ASSERT (value, "no such run time system routine");

    fun = build_decl (FUNCTION_DECL,
		      get_identifier(rts[index].name),
		      build_function_type (value_type, formal_params));

    DECL_EXTERNAL (fun) = 1;
    TREE_PUBLIC (fun) = 1;

    DECL_SOURCE_FILE (fun) = "Pascal run time library";
    DECL_SOURCE_LINE (fun) = 1;

    rest_of_decl_compilation (fun, NULL_PTR, 0, 0);

    /* @@@ Can't reuse the fun later because the argument types
     * may vary in each call (i.e. the _p_new takes any pointer)
     *
     * Should check if could use 'void *' et. al. for these cases...
     */
    if (! rts[index].fun)
      {
	if (! rts[index].symref)
	  abort ();

	/* If this machine requires an external definition for library
	   functions, write one out.  */

	/* HP-UX .import FOO directives are done here.
	 * I don't think this needs to be called but once per routine
	 */
	assemble_external_libcall (rts[index].symref);
	rts[index].fun = fun;
      }

    return build_function_call (fun, params);
}

/*
 * Read from FILEs and STRINGs.
 */
void
rts_read (rts_id, params)
int rts_id;
tree params;
{
    tree parm;
    int length;
    tree arglist = NULL_TREE;
    tree string_curlen = NULL_TREE;
    tree string_length = NULL_TREE;

    if (rts_id == p_READSTR)
      {
	tree string;
	tree curlen;

	if (! params || !is_string_type (TREE_VALUE (params)))
	  {
	    error ("First arg to `readstr' must be the string to read from");
	    return;
	  }

	string = TREE_VALUE (params);
	params = TREE_CHAIN (params);

	/* find out the number of args we are writing */
	length = list_length (params);
	
	/* If a variable length string, pass a pointer to the current length
	 *
	 * Note that the string does not need to be an lvalue.
	 */
	if (is_variable_string_type (TREE_TYPE (string)))
	  curlen = build1 (ADDR_EXPR, 
			   integer_ptr_type_node,
			   PASCAL_STRING_LENGTH (string));
	else
	  curlen = null_pointer_node;

	/* First four args:
	 *  string pointer, pointer to curlen, string maxlen, number of args
	 *
	 * Note that the string does not need to be an lvalue.
	 */
	arglist =
	  tree_cons
	    (NULL_TREE, build1 (ADDR_EXPR, string_type_node, PASCAL_STRING_VALUE (string)),
	     tree_cons (NULL_TREE, curlen,
			tree_cons (NULL_TREE,
				   pascal_array_type_nelts (TREE_TYPE (PASCAL_STRING_VALUE (string))),
				   build_tree_list (NULL_TREE,
						    size_int (length)))));
      }
    else
      {
	tree file;
	int is_text;

	if (params
	    && TREE_CODE (TREE_TYPE (TREE_VALUE (params))) == FILE_TYPE)
	  {
	    file = TREE_VALUE (params);
	    params = TREE_CHAIN (params);
	  }
	else
	  file = get_standard_input ();

	is_text = TYPE_FILE_TEXT (TREE_TYPE (file)) != NULL_TREE;
	
	if (rts_id == p_READLN)
	  if (! is_text)
	    {
	      error ("`Readln' is allowed only when reading from files of type `Text'");
	      return;
	    }
    
	if (rts_id == p_READ)
	  {
	    if (params == NULL_TREE)
	      {
		error ("Too few parameters to predefined procedure `Read'");
		return;
	      }

	    /* Non TEXT file reads */
	    if (! is_text) {
	      tree the_file = build_tree_list (NULL_TREE, file);
	      tree buffer = build_buffer_ref (file);
	      for (parm = params; parm; parm = TREE_CHAIN (parm))
		{
		  /* @@@@ Should check that the types match! */

		  /* Move the contents of the file buffer to the read param */
		  expand_assignment (TREE_VALUE (parm), buffer, 0, 0);
		
		  /* Do a get from the file */
		  build_rts_call (p_GET, the_file);
		}
	      return;
	    }
	  }
	/* find out the number of args we are reading */
	length = list_length (params);
	length += rts_id == p_READLN;	/* add P_LINE */
	
	arglist = tree_cons (NULL_TREE, build_unary_op (ADDR_EXPR, file, 0),
			     build_tree_list (NULL_TREE, size_int (length)));
      }

    for (parm = params; parm; parm = TREE_CHAIN (parm))
      {
	tree p = TREE_VALUE (parm);
	enum tree_code code = TREE_CODE (TREE_TYPE (p));
	int what;

	length--;
	switch (code)
	  {
	  case INTEGER_TYPE :
	    what = P_INT;
	    break;
	  case CHAR_TYPE :
	    what = P_CHAR;
	    break;
	  case REAL_TYPE :
	    what = P_REAL;
	    break;

	  case RECORD_TYPE: /* String schema */ 
	  case ARRAY_TYPE:  /* fixed length strings */
	    switch (is_string_type (p)) {
	    case 0:
	      error ("Only packed arrays of char may be read from TEXT files");
	      continue;
	    case 1:
	      break;
	    case 2:
	      /* @@@ Should generate a runtime check for conformant arrays.
	       * (low index has to be 1 for arrays to be valid string-type)
	       */
	      /* for now, just read them */
	      break;
	    default:
	      abort ();
	    }

	    if (pedantic)
	      pedwarn ("ISO pascal does not allow reading of strings from textfiles");

	    /* run time system expects another argument
	       before max length: a pointer to int where
	       it stores the current length of the string

	       If that is null_pointer, the string is a fixed length string.

	       This needs to be an lvalue if not NULL_TREE.
	     */
	    if (is_variable_string_type (TREE_TYPE (p)))
	      string_curlen = build_unary_op (ADDR_EXPR, PASCAL_STRING_LENGTH (p), 0);
	    else
	      string_curlen = null_pointer_node;

	    /* The char store */
	    p = PASCAL_STRING_VALUE (p);

	    /* String max length */
	    string_length = pascal_array_type_nelts (TREE_TYPE (p));
	    what          = P_STRING;
	    break;
	    
	  default :
	    error ("parameter of Read/Readln from TEXT file is of wrong type");
	    return;
	  }
	
	/* Inform the run time system of the next arg type */
	arglist = chainon (arglist,
			   build_tree_list (NULL_TREE, size_int (what)));
	
	/* Pass the address of the variable we want to read */
	arglist = chainon (arglist, 
			   build_tree_list (NULL_TREE,
					    build_unary_op (ADDR_EXPR, p, 0)));
	if (what == P_STRING)
	  arglist = chainon (arglist, 
			     tree_cons (NULL_TREE,
					string_curlen,
					build_tree_list (NULL_TREE,
							 string_length)));
      }
    
    if (rts_id == p_READLN)
      {
	/* Inform the run time system that we should do a readln */
	arglist = chainon (arglist, build_tree_list (NULL_TREE,
						     size_int (P_LINE)));
	length--;
      }
    
    if (length)
      abort ();
    
    expand_expr_stmt (rts_call ((rts_id == p_READSTR) ? p_READSTR : r_READ,
				void_type_node, NULL_TREE, arglist));
}


void
rts_write(rts_id, params)
int rts_id;
tree params;
{
    tree parm;
    tree arglist;
    int length;
    
    if (rts_id == p_WRITESTR)
      {
	tree string;
	tree curlen;

	if (! params || !is_string_type (TREE_VALUE (params)))
	  {
	    error ("First arg to `writestr' must be the string to write to");
	    return;
	  }

	string = TREE_VALUE (params);

	/* Don't allow writes to a constant string */
	if (really_constant_p (string) || TREE_READONLY (string))
	  readonly_warning (string, "writestr: modification");

	params = TREE_CHAIN (params);

	/* find out the number of args we are writing */
	length = list_length (params);

	/* If a variable length string, pass a pointer to the current length.
	 */
	if (is_variable_string_type (TREE_TYPE (string)))
	  curlen = build_unary_op (ADDR_EXPR, PASCAL_STRING_LENGTH (string), 0);
	else
	  curlen = null_pointer_node;

	/* First four args:
	 *  string pointer, pointer to curlen, string maxlen, number of args
	 *
	 * Note that this string needs to be an lvalue.
	 */
	arglist = tree_cons (NULL_TREE, build_unary_op (ADDR_EXPR,
							PASCAL_STRING_VALUE (string),
							0),
			     tree_cons (NULL_TREE, curlen,
					tree_cons (NULL_TREE,
						   pascal_array_type_nelts (TREE_TYPE (PASCAL_STRING_VALUE (string))),
						   build_tree_list (NULL_TREE, size_int (length)))));
      }
    else
      {
	tree file;
	int is_text;

	if (params
	    && TREE_CODE (TREE_TYPE (TREE_VALUE (params))) == FILE_TYPE)
	  {
	    file = TREE_VALUE (params);
	    params = TREE_CHAIN (params);
	  }
	else
	  file = get_standard_output ();
	
	is_text = TYPE_FILE_TEXT (TREE_TYPE (file)) != NULL_TREE;
	
	if (rts_id == p_WRITELN)
	  if (! is_text)
	    {
	      error ("WRITELN is allowed only when writing to files of type TEXT");
	      return;
	    }
    
	if (rts_id == p_WRITE)
	  {
	    if (params == NULL_TREE)
	      {
		error ("Too few parameters to predefined procedure WRITE");
		return;
	      }

	  /* Non TEXT file writes */
	    if (! is_text)
	      {
		tree the_file = build_tree_list (NULL_TREE, file);
		tree buffer = build_buffer_ref (file);
		for (parm = params; parm; parm = TREE_CHAIN (parm))
		  {
		    
		    /* @@@@ Should check that the types match! */
		    
		    /* Move the contents of the write parm to the file buffer */
		    expand_assignment (buffer, TREE_VALUE (parm), 0, 0);
		    
		    /* Do a put to the file */
		    build_rts_call (p_PUT, the_file);
		  }
		return;
	      }
	  }
	/* find out the number of args we are writing */
	length = list_length (params);
	length += rts_id == p_WRITELN;	/* add P_LINE */
	
	arglist = tree_cons (NULL_TREE, build_unary_op (ADDR_EXPR, file, 0),
			     build_tree_list (NULL_TREE, size_int (length)));
      }

    for (parm = params; parm; parm = TREE_CHAIN (parm)) {
	tree field1;
	tree field2;
	int what;
	tree p = TREE_VALUE (parm);
	enum tree_code code = TREE_CODE (TREE_TYPE (p));

	length--;

	if (TREE_PURPOSE (parm)) {
	    field1 = TREE_VALUE (TREE_PURPOSE (parm));
	    field2 = TREE_PURPOSE (TREE_PURPOSE (parm));
	} else {
	    field1 = NULL_TREE;
	    field2 = NULL_TREE;
        }

	if (field1)
	  field1 = convert (integer_type_node, field1);

	if (field2)
	  field2 = convert (integer_type_node, field2);

	switch (code) {
	case INTEGER_TYPE :
	    if (field1)
		what = P_FIX_INT;
	    else
		what = P_INT;
	    break;
	case BOOLEAN_TYPE :
	    if (field1)
		what = P_FIX_BOOL;
	    else
		what = P_BOOL;
	    break;
	case CHAR_TYPE :
	    if (field1)
		what = P_FIX_CHAR;
	    else
		what = P_CHAR;
	    break;
	case REAL_TYPE :
	    if (field1)
		if (field2)
		    what = P_FIX2_REAL;
		else
		    what = P_FIX1_REAL;
	    else
		what = P_REAL;
	    break;

	case RECORD_TYPE:
	case ARRAY_TYPE:
	    switch (is_string_type (p)) {
	    case 0:
	      error ("Only packed arrays of char with low index 1 may be written to text files");
	      continue;
	    case 1:
	      break;
	    case 2:
	      /* @@@ Should generate a runtime check for conformant arrays.
	       * (low index has to be 1 for arrays to be valid string-type)
	       */
	      /* for now, just write them out
	       * Note that the field1 below will be incorrect if confarrays
	       * don't start from 1
	       */
	      break;
	    default:
	      abort ();
	    }
	    
	    if (field1)
	      {
		what = P_FIX_STRING;
		if (field2)
		  warning ("second field width allowed only when writing REAL type");
	      }
	    else
	      {
		what = P_STRING;

		/* If a variable length string, pass a pointer to the current length
		 *
		 * Note that this does not have to be an lvalue.
		 */
		if (is_variable_string_type (TREE_TYPE (p)))
		  field1 = build1 (ADDR_EXPR, integer_ptr_type_node,
				   PASCAL_STRING_LENGTH (p));
		else
		  field1 = null_pointer_node;
	      }

	    field2 = PASCAL_STRING_LENGTH (p);
	    p = PASCAL_STRING_VALUE (p);

	    /* pass the address of the string */
	    p = build1 (ADDR_EXPR, string_type_node, p);
	    break;
	    ;

	default :
	    error ("parameter to Write/Writeln to Textfile is of wrong type");
	    return;
	}
	if (field2
	    && what != P_FIX2_REAL
	    && what != P_FIX_STRING
	    && what != P_STRING)
	  warning ("second field width allowed only when writing REAL type");
	
	/* Inform the run time system of the next arg type */
	arglist = chainon (arglist,
			   build_tree_list (NULL_TREE, size_int (what)));
	
	/* Pass the variable we want to write */
	arglist = chainon (arglist, build_tree_list (NULL_TREE, p));

	if (field1)
	    arglist = chainon (arglist, build_tree_list (NULL_TREE, field1));

	if (field2)
	    arglist = chainon (arglist, build_tree_list (NULL_TREE, field2));
    }

    if (rts_id == p_WRITELN) {
	/* Inform the run time system that we should do a writeln */
	arglist = chainon (arglist, build_tree_list (NULL_TREE,
						     size_int (P_LINE)));
	length--;
    }

    if (length)
	abort ();

    expand_expr_stmt (rts_call ((rts_id == p_WRITESTR) ? p_WRITESTR : r_WRITE,
				void_type_node, NULL_TREE, arglist));
}


tree
rts_string (rts_id, params)
int rts_id;
tree params;
{
    int  opcode;
    tree arglist       = NULL_TREE;
    tree string_curlen = NULL_TREE;
    tree string_length = NULL_TREE;

    int parm_types = 0;	/* Both are strings (default) */

    /* params != NULL, verified by the caller */
    tree parm1 = TREE_VALUE (params);
    tree parm2;

    tree arg1 = NULL_TREE;
    tree len1;
    tree arg2 = NULL_TREE;
    tree len2;
    tree arg3 = NULL_TREE;
    tree len3;
    tree sub_from;
    tree sub_length;
    tree return_type;
    tree funcall;

    /* @@ Hmmph, identify the routines with incorrect parameters someday */
    if (TREE_CODE (TREE_TYPE (parm1)) == CHAR_TYPE)
      {
	parm_types |= P_STR_FIRST_IS_CHAR;	/* First arg is CHAR_TYPE */
	len1 = integer_one_node;
	arg1 = parm1;
      }
    else if (is_string_type (parm1))
      len1 = PASCAL_STRING_LENGTH (parm1);
    else
      {
	error ("First parameter to string routines must be of char or string type");
	return error_mark_node;
      }

    if (!arg1)
      arg1 = build1 (ADDR_EXPR, string_type_node, PASCAL_STRING_VALUE (parm1));

    if (rts_id != p_TRIM)
      {
	parm2 = TREE_VALUE (TREE_CHAIN (params));

	if (rts_id == p_SUBSTR)
	  {
	    /* Substr may have 3rd parameter, check later */
	    if (TREE_CODE (TREE_TYPE (parm2)) != INTEGER_TYPE)
	      {
		error ("Second parameter to `substr' must be of integer type");
		return error_mark_node;
	      }
	    sub_from = parm2;
	  }
	else
	  {
	    if (TREE_CODE (TREE_TYPE (parm2)) == CHAR_TYPE)
	      {
		parm_types |= P_STR_SECOND_IS_CHAR;	/* Second arg is CHAR_TYPE */
		len2 = integer_one_node;
		arg2 = parm2;
	      }
	    else if (is_string_type (parm2))
	      len2 = PASCAL_STRING_LENGTH (parm2);
	    else
	      {
		error ("Second parameter to this string routine must be of char or string type");
		return error_mark_node;
	      }
	    
	    parm2 = PASCAL_STRING_VALUE (parm2);

	    if (!arg2)
	      arg2 = build1 (ADDR_EXPR, string_type_node, parm2);
	  }
      }

    return_type = boolean_type_node;
    switch (rts_id) {
      /* Lexicographic string comparison. two args returns boolean */
    case p_EQ: opcode = R_EQ; break;
    case p_LT: opcode = R_LT; break;
    case p_LE: opcode = R_LE; break;
    case p_GE: opcode = R_GE; break;
    case p_GT: opcode = R_GT; break;
    case p_NE: opcode = R_NE; break;

      /* comparisons padded with spaces. two args returns boolean */
    case '=': opcode = R_eq; break;
    case '<': opcode = R_lt; break;
    case '>': opcode = R_gt; break;
    case LTE: opcode = R_le; break;
    case GTE: opcode = R_ge; break;
    case NEQ: opcode = R_ne; break;

      /* index (s1, s2) : first position where s1 contains s2, 0 if none. */
    case p_INDEX: opcode = R_INDEX; return_type = integer_type_node; break; /* two args */

      /* substr(s,i) or substr(s,i,j)
       * (if j is missing, it is calculated as: length(s)-i+1)
       *
       * Return a substring of s from index i to index j.
       */
    case p_SUBSTR:	/* two or three args */
      opcode = R_SUBSTR;
      if (TREE_CHAIN (TREE_CHAIN (params)))
	{
	  sub_length = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (params)));
	  if (TREE_CODE (TREE_TYPE (sub_length)) != INTEGER_TYPE)
	    {
	      error ("Substring start position must be of integer type");
	      return error_mark_node;
	    }
	}
      else
	sub_length = build_binary_op
	  		(PLUS_EXPR,
			 integer_one_node,
			 build_binary_op (MINUS_EXPR, len1, sub_from, 1),
			 1);
      /* FALLTHROUGH */

    case p_TRIM:
      if (rts_id == p_TRIM)
	opcode = R_TRIM;
      return_type = void_type_node;
      /* Allocate a new string, pass that and the length field
	 as a pointer to run time system.

	 <*string,*len> to arg2, len2.
       */
      parm2 = new_string_by_model (NULL_TREE, parm1, 0);
      arg2  = build_unary_op (ADDR_EXPR, PASCAL_STRING_VALUE (parm2), 0);
      len2  = build_unary_op (ADDR_EXPR, PASCAL_STRING_LENGTH (parm2), 0);
      break;

    default:
      abort ();
    }

    arglist = tree_cons (NULL_TREE, size_int (opcode),
	       tree_cons (NULL_TREE, size_int (parm_types),
		tree_cons (NULL_TREE, arg1, NULL_TREE)));

    /* Length of first string; nothing if char */
    if ((parm_types & 1) == 0)
      arglist = chainon (arglist, build_tree_list (NULL_TREE, len1));

    /* Second <*string,length> or just <char>, 
     * or <*string, *length> for p_TRIM and p_SUBSTR
     */
    arglist = chainon (arglist, build_tree_list (NULL_TREE, arg2));
    if ((parm_types & 2) == 0)
      arglist = chainon (arglist, build_tree_list (NULL_TREE, len2));
		 
    if (rts_id == p_SUBSTR)
      {
	/* Substring start */
	arglist = chainon (arglist, build_tree_list (NULL_TREE, sub_from));
	/* Substring length */
	arglist = chainon (arglist, build_tree_list (NULL_TREE, sub_length));
      }

    funcall = rts_call (rts_id, return_type, NULL_TREE, arglist, 0);

    if (return_type != void_type_node)
      return funcall;

    expand_expr_stmt (funcall);
    if (rts_id != p_TRIM && rts_id != p_SUBSTR)
      abort ();

    return parm2;
}

/*
 * @@@. Sigh. I just noticed that array_type_nelts thinks that
 * the array type is max-min long. However, I assume that arrays are
 * max-min+1 long, so I had to make yet another routine...
 */

/* Return, as an INTEGER_CST node, the number of elements for
   TYPE (which is an ARRAY_TYPE).  */
tree
pascal_array_type_nelts (type)
     tree type;
{
  tree index_type = TYPE_DOMAIN (type);
  tree len =
    tree_int_cst_equal (TYPE_MIN_VALUE (index_type),
			integer_zero_node)
      ? TYPE_MAX_VALUE (index_type)
      : fold (build (MINUS_EXPR, integer_type_node,
		     TYPE_MAX_VALUE (index_type),
		     TYPE_MIN_VALUE (index_type)));

  return fold (build (PLUS_EXPR, integer_type_node,
		      len, integer_one_node));
}

tree
object_size (type)
tree type;
{
  if (TREE_CODE (type) != ARRAY_TYPE)
    return size_in_bytes (type);
  else
    return build_binary_op (MULT_EXPR,
			    pascal_array_type_nelts (type),
			    object_size (TREE_TYPE (type)),
			    1);
}

/*
 * Implement pascal pack and unpack transfer procedures.
 */
tree
pascal_unpack_and_pack (unpack, unpacked, packed, ustart)
int  unpack;			/* Nonzero if UNPACK */
tree unpacked;
tree packed;
tree ustart;
{
  tree utype;
  tree ptype;
  tree elem_size;
  tree len;
  tree pmin, pmax, umin, umax;
  tree check;			/* Check of arguments */

  rtx x,y;
  rtx target;
  rtx source;
  rtx length;
  
  if (unpacked == error_mark_node || packed == error_mark_node)
    return error_mark_node;

  utype = TREE_TYPE (unpacked);
  ptype = TREE_TYPE (packed);
  pmin  = TYPE_MIN_VALUE (TYPE_DOMAIN (ptype));
  pmax  = TYPE_MAX_VALUE (TYPE_DOMAIN (ptype));
  umin  = TYPE_MIN_VALUE (TYPE_DOMAIN (utype));
  umax  = TYPE_MAX_VALUE (TYPE_DOMAIN (utype));

  /* Elements must be same size, get size in bytes */
  elem_size = size_in_bytes (TREE_TYPE (utype));
  
  x = expand_expr
    (build_indirect_ref
     (build_unary_op (ADDR_EXPR, build_array_ref (unpacked, ustart), 0),
      "unpacked"),
     0, BLKmode, 0);
  
  y = expand_expr
    (build_indirect_ref
     (build_unary_op (ADDR_EXPR, packed, 0),
      "packed"),
     0, BLKmode, 0);

  if (unpack)
    {
      target = x;
      source = y;
    }
  else
    {
      target = y;
      source = x;
    }

  /* Length we copy is the length of the packed array */
  len = fold (build (PLUS_EXPR, integer_type_node,
		     fold (build (MINUS_EXPR, integer_type_node, pmax, pmin)),
		     integer_one_node));
      
  /* Sanity check */
  /* Check that (ustart >= umin) and (ustart..umax >= pmin..pmax) */
  check =
    fold (build (TRUTH_AND_EXPR, boolean_type_node,
		 fold (build (GE_EXPR, boolean_type_node,
			      ustart, umin)),
		 fold (build (GE_EXPR, boolean_type_node,
			      fold (build (PLUS_EXPR, integer_type_node,
					   fold (build (MINUS_EXPR,
							integer_type_node,
							umax, ustart)),
					   integer_one_node)),
			      len))));

  if (! TREE_CONSTANT (check))
    /* @@@ Runtime check here variable size array */;
  else if (integer_zerop (check))
    {
      error ("Invalid arguments to '%s'", unpack ? "Unpack" : "Pack");
      return error_mark_node;
    }

  length = expand_expr
    	       (fold (build (MULT_EXPR, integer_type_node, elem_size, len)),
		0, SImode, 0);
  
  if (GET_CODE (source) == MEM && GET_CODE (target) == MEM)
    {
      extern rtx change_address ();

      if (GET_MODE (target) != BLKmode)
	target = change_address (target, BLKmode, 0);

      if (GET_MODE (source) != BLKmode)
	source = change_address (source, BLKmode, 0);

      emit_block_move
	(target, source, length, TYPE_ALIGN (ptype) / BITS_PER_UNIT);
    }
  else
    abort ();

  return NULL_TREE;
}

int
number_of_schema_discriminants (type)
     tree type;
{
  /* @@@ How is this done in the general case? */
  if (PASCAL_TYPE_STRING (type) && type == string_schema_proto_type)
    return 1;

  /* @@@ No other schemas implemented currently */
  return 0;
}


/*
 * routine constructs Pascal run time system calls with correct arguments.
 *
 * RTS_ID is the %token number of the run time system routine to call.
 * APAR is a TREE_LIST chain of arguments; args are in the TREE_VALUE field.
 * if there is something in the TREE_PURPOSE field, it is a TREE_LIST
 * node of write | writeln output field length expressions, the first 
 * expression is in TREE_VALUE and the second one is in TREE_PURPOSE
 * i.e. actual_parameter : expression : expression
 *
 */

tree
build_rts_call (rts_id, apar)
int rts_id;
register tree apar;   /* actual parameters of the routine */
{
  tree fpar = NULL_TREE;       /* formal parameters, default to varargs */
  tree rval = void_type_node;  /* return value; void_type_node if !function */
  tree actual_return_value = NULL_TREE; /* Value to return for a procedure call if any */

  tree un_init_this = NULL_TREE; /* If nonzero, try to un-initalize the beast */

  tree post_conversion = NULL_TREE; /* for integer "a POW b": should be inlined */
  tree val  = NULL_TREE;
  tree val2 = NULL_TREE;
  enum tree_code code  = MINUS_EXPR; /* Something that is impossible */
  enum tree_code code2 = MINUS_EXPR; /* Something that is impossible */
  tree type, type2;
  int length;		       /* length of the actual parameter list */
  int schema_ids   = 0;
  int schema_size  = 0;
  int rts_inline   = 0;	       /* set to 1 if this is compiled inline */
  int wins         = 1;	       /* Will be zeroed if formal params are not
				  constants */
  tree temp;

  char *errstr = (char *)NULL;
  tree retval   = NULL_TREE;

  length = list_length (apar);

#define INLINE_RTS_LENGTH(nam, len) \
 { if (length != len) {						\
    error ("incorrect number of parameters to `%s'", nam); 	\
    return error_mark_node; } 					\
   else rts_inline = 1; }

  if (apar) {
      val  = TREE_VALUE (apar);
      type = TREE_TYPE (val);
      if (type)
        code = TREE_CODE (type);
      if (TREE_CHAIN (apar))
	{
	  val2  = TREE_VALUE (TREE_CHAIN (apar));
	  type2 = TREE_TYPE (val2);
          if (type2)
            code2 = TREE_CODE (type2);
	}
      /* @@@ wins is not used */
      for (temp = apar; temp && wins; temp = TREE_CHAIN (temp))
	  wins = TREE_CONSTANT (TREE_VALUE (temp));
  }

  switch (rts_id) {

  case p_CARD:
    INLINE_RTS_LENGTH ("card", 1);
    if (code == SET_TYPE)
      {
	if (TREE_CODE (val) != CONSTRUCTOR
	    && TREE_CODE (TREE_TYPE (type)) == VOID_TYPE)
	  retval = integer_zero_node;
	else
	  retval = build1 (CARD_EXPR, integer_type_node, val);
      }
    else
      errstr = "SET type required";
    break;

  /* rts_inline functions */
  case p_ABS:
      INLINE_RTS_LENGTH("abs", 1);
      if (INT_REAL(code) || code == COMPLEX_TYPE)
          retval = build_unary_op (ABS_EXPR, val, 0);
      else 
	  errstr = "argument to `abs' must be of integer, real or complex type";
      break;

  case p_SQR:
      INLINE_RTS_LENGTH("sqr", 1);
      if (INT_REAL(code) || code == COMPLEX_TYPE)
	  retval = build_binary_op(MULT_EXPR, val, val,1);
      else
	  errstr = "argument to `sqr' must be of integer, real  or complex type";
      break;

  case p_TRUNC:	/* check the return value; maybe wrong TRUNC_EXPR used */
      INLINE_RTS_LENGTH("trunc", 1);
      if (code == REAL_TYPE)
	  retval = convert (integer_type_node, val);
      else
	  errstr = "argument to `trunc' must be of real type";
      break;

  case p_ROUND:
      INLINE_RTS_LENGTH("round", 1);
      if (code == REAL_TYPE) {
	  retval = fold (build1 (FIX_ROUND_EXPR, integer_type_node, val));
      } else 
	  errstr = "argument to `round' must be of real type";
      break;

  case p_SUCC:		/* rts_inline */
      if (length != 1 && length != 2)
	{
	  error ("incorrect number of parameters to `succ'");
	  return error_mark_node;
	}

      rts_inline = 1;

      if (ORDINAL_TYPE (code))
	{
	  tree increment = integer_one_node;
	  if (length == 2)
	    if (TREE_CODE (type2) != INTEGER_TYPE)
	      errstr = "second arg to `succ' must be of integer type";
	    else
	      increment = val2;
	  if (! errstr)
	    retval = convert (type,
			      build_binary_op (PLUS_EXPR, val, increment, 1));
        }
      else 
	errstr = "argument to `succ' must be of ordinal type";
      break;

  case p_PRED:		/* rts_inline */
      if (length != 1 && length != 2)
	{
	  error ("incorrect number of parameters to `pred'");
	  return error_mark_node;
	}

      rts_inline = 1;

      if (ORDINAL_TYPE (code))
	{
	  tree increment = integer_one_node;
	  if (length == 2)
	    if (TREE_CODE (type2) != INTEGER_TYPE)
	      errstr = "second arg to `pred' must be of integer type";
	    else
	      increment = val2;
	  if (! errstr)
	    retval = convert (type,
			      build_binary_op (MINUS_EXPR, val, increment, 1));
        }
      else 
	errstr = "argument to `pred' must be of ordinal type";
      break;

  case bp_INC:		/* rts_inline */
      if (pedantic)
        warning ("ISO Pascal does not define `inc'");
      if (length != 1 && length != 2)
	{
	  error ("incorrect number of parameters to `inc'");
	  return error_mark_node;
	}
      rts_inline = 1;
      retval = NULL_TREE;
      if (ORDINAL_TYPE (code))
	{
	  tree increment = integer_one_node;
	  if (length == 2)
	    if (TREE_CODE (type2) != INTEGER_TYPE)
	      errstr = "second arg to `inc' must be of integer type";
	    else
	      increment = val2;
	  if (! errstr)
            expand_expr_stmt (build_modify_expr (convert (integer_type_node, 
                                                          val),
                                                 PLUS_EXPR, increment));
        }
      else 
	errstr = "argument to `inc' must be of ordinal type";
      break;

  case bp_DEC:		/* rts_inline */
      if (pedantic)
        warning ("ISO Pascal does not define `dec'");
      if (length != 1 && length != 2)
	{
	  error ("incorrect number of parameters to `dec'");
	  return error_mark_node;
	}
      rts_inline = 1;
      retval = NULL_TREE;
      if (ORDINAL_TYPE (code))
	{
	  tree increment = integer_one_node;
	  if (length == 2)
	    if (TREE_CODE (type2) != INTEGER_TYPE)
	      errstr = "second arg to `dec' must be of integer type";
	    else
	      increment = val2;
	  if (! errstr)
            expand_expr_stmt (build_modify_expr (convert (integer_type_node, 
                                                          val),
                                                 MINUS_EXPR, increment));
        }
      else 
	errstr = "argument to `dec' must be of ordinal type";
      break;

  case AND:		/* rts_inline */
      if (pedantic)
        warning ("GPC specific use of `and'");
      if (length != 2)
	{
	  error ("incorrect number of parameters to `and'");
	  return error_mark_node;
	}
      rts_inline = 1;
      retval = NULL_TREE;
      if (TREE_CODE (type) == INTEGER_TYPE
          && TREE_CODE (type2) == INTEGER_TYPE)
	{
	  if (! errstr)
            expand_expr_stmt (build_modify_expr (val, BIT_AND_EXPR, val2));
        }
      else 
	errstr = "arguments to `and' must be of integer type";
      break;

  case OR:		/* rts_inline */
      if (pedantic)
        warning ("GPC specific use of `or'");
      if (length != 2)
	{
	  error ("incorrect number of parameters to `or'");
	  return error_mark_node;
	}
      rts_inline = 1;
      retval = NULL_TREE;
      if (TREE_CODE (type) == INTEGER_TYPE
          && TREE_CODE (type2) == INTEGER_TYPE)
	{
	  if (! errstr)
            expand_expr_stmt (build_modify_expr (val, BIT_IOR_EXPR, val2));
        }
      else 
	errstr = "arguments to `or' must be of integer type";
      break;

  case NOT:		/* rts_inline */
      if (pedantic)
        warning ("GPC specific use of `not'");
      if (length != 1)
	{
	  error ("incorrect number of parameters to `not'");
	  return error_mark_node;
	}
      rts_inline = 1;
      retval = NULL_TREE;
      if (TREE_CODE (type) == INTEGER_TYPE)
	{
	  if (! errstr)
            expand_expr_stmt (build_modify_expr (val, NOP_EXPR,
              build_pascal_unary_op (BIT_NOT_EXPR, val, 0)));
        }
      else 
	errstr = "argument to `not' must be of integer type";
      break;

  case BP_XOR:		/* rts_inline */
      if (pedantic)
        warning ("GPC specific use of `xor'");
      if (length != 2)
	{
	  error ("incorrect number of parameters to `xor'");
	  return error_mark_node;
	}
      rts_inline = 1;
      retval = NULL_TREE;
      if (TREE_CODE (type) == INTEGER_TYPE
          && TREE_CODE (type2) == INTEGER_TYPE)
	{
	  if (! errstr)
            expand_expr_stmt (build_modify_expr (val, BIT_XOR_EXPR, val2));
        }
      else 
	errstr = "arguments to `xor' must be of integer type";
      break;

  case p_ORD:		/* rts_inline */
      INLINE_RTS_LENGTH("ord", 1);
      if (ORDINAL_TYPE(code))
	  retval = convert (integer_type_node, val);
      else 
	  errstr = "argument to `ord' must be of ordinal type";
      break;

  case p_CHR:		/* rts_inline */
      INLINE_RTS_LENGTH("chr", 1);
      if (code == INTEGER_TYPE)
	  retval = convert (char_type_node, val); 
      else 
	  errstr = "argument to `chr' must be of integer type";
      break;

  case p_ODD:		/* rts_inline */ 
      INLINE_RTS_LENGTH("odd", 1);
      if (code == INTEGER_TYPE)
	  retval = convert (boolean_type_node,
			    build_binary_op (BIT_AND_EXPR, val, integer_one_node, 1));
      else 
	  errstr = "argument to `odd' must be of integer type";
      break;

  case p_LENGTH:	/* rts_inline */
      INLINE_RTS_LENGTH("length", 1);
      if (code == CHAR_TYPE || is_string_type (val))
	  retval = PASCAL_STRING_LENGTH (val);
      else
	  errstr = "argument to `length' must be a string or char type";
      break;

  case p_CMPLX:		/* rts_inline */
      INLINE_RTS_LENGTH("cmplx", 2);
      if (! INT_REAL (code) || ! INT_REAL (TREE_CODE (TREE_TYPE (val2))))
	  errstr = "Wrong type arguments to `cmplx'";
      else {
	  tree complex = TREE_TYPE (complex_type_node);

	  if (type != complex)
	      val = convert (complex, val);

	  if (TREE_TYPE (val2) != complex)
	      val2 = convert (complex, val2);

	  retval = build (COMPLEX_EXPR, complex_type_node, val, val2);
      }
      break;

  case p_IM:
      INLINE_RTS_LENGTH("im", 1);
      if (code == COMPLEX_TYPE)
	  retval = build_unary_op (IMAGPART_EXPR, val, 1);
      else 
	  errstr = "argument to `im' must be of complex type";
      break;
      
  case p_RE:
      INLINE_RTS_LENGTH("re", 1);
      if (code == COMPLEX_TYPE)
	  retval = build_unary_op (REALPART_EXPR, val, 1);
      else 
	  errstr = "argument to `re' must be of complex type";
      break;

  case gpc_MAX:
  case gpc_MIN:
      INLINE_RTS_LENGTH (rts_id == gpc_MAX ? "max" : "min", 2);
      if (code == REAL_TYPE || code2 == REAL_TYPE)
        {
          if (code == INTEGER_TYPE && code2 == REAL_TYPE)
            {
              val = build_c_cast (type2, val);
              type = type2;
            }
          else if (code == REAL_TYPE && code2 == INTEGER_TYPE)
            val2 = build_c_cast (type, val2);
        }
      else if (! ORDINAL_TYPE (code) || ! ORDINAL_TYPE (code2))
        {
          errstr = "arguments to `max' or `min' must be of ordinal or real type";
          break;
        }
      else if (code != code2)
        {
          errstr = "both arguments to `max' or `min' must have the same type";
          break;
        }
      retval = build_c_cast (type, 
                 build_binary_op (rts_id == gpc_MAX ? MAX_EXPR : MIN_EXPR,
                                  val, val2, 1));
      break;

  case p_PACK:
      emit_line_note (input_filename, lineno);
      INLINE_RTS_LENGTH ("pack", 3);
      { tree val3  = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (apar)));
	tree type3 = TREE_TYPE (val3);
	tree unpacked_domain = TYPE_DOMAIN (type);

	if (TREE_CODE (type3) != ARRAY_TYPE || !PASCAL_TYPE_PACKED (type3))
	  error ("Third argument to PACK must be a packed array");
	else if (TREE_CODE (type) != ARRAY_TYPE || PASCAL_TYPE_PACKED (type))
	  error ("First argument to PACK must be an unpacked array");
	else if (TREE_CODE (type2) != TREE_CODE (unpacked_domain) &&
		 (TREE_CODE (unpacked_domain) != INTEGER_TYPE ||
		  TREE_CODE (type2) != TREE_CODE(TREE_TYPE (unpacked_domain))))
	  error ("Second argument of PACK must be of unpacked array index type");
	else if (TREE_CODE (TREE_TYPE (type)) != TREE_CODE (TREE_TYPE (type3)))
	  error ("Source and destination arrays in PACK are not of same type");
	else
	  return pascal_unpack_and_pack (0, val, val3, val2);
      }
      return error_mark_node;

  case p_UNPACK:
      emit_line_note (input_filename, lineno);
      INLINE_RTS_LENGTH ("unpack", 3);
      { tree val3  = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (apar)));
	tree type3 = TREE_TYPE (val3);
	tree unpacked_domain = TYPE_DOMAIN (type2);

	/* I just wonder why on venus they had to shuffle these */
	if (TREE_CODE (type2) != ARRAY_TYPE || PASCAL_TYPE_PACKED (type2))
	  error ("Second argument to UNPACK must be an unpacked array");
	else if (TREE_CODE (type) != ARRAY_TYPE || !PASCAL_TYPE_PACKED (type))
	  error ("First argument to UNPACK must be a packed array");
	else if (TREE_CODE (type3) != TREE_CODE (unpacked_domain) &&
		 (TREE_CODE (unpacked_domain) != INTEGER_TYPE ||
		  TREE_CODE (type3) != TREE_CODE(TREE_TYPE (unpacked_domain))))
	  error ("Third argument of UNPACK must be of unpacked array index type");
	else if (TREE_CODE (TREE_TYPE (type)) != TREE_CODE (TREE_TYPE (type2)))
	  error ("Source and destination arrays in UNPACK are not of same type");
	else
	  return pascal_unpack_and_pack (1, val2, val, val3);
      }
      return error_mark_node;

  /* calls to run time system */

  /* statements */
  case p_NEW:
  case bp_GETMEM:
      /*
       * There are a lot of allowed call styles for New:
       *
       *   New ( PtrVar );                   (* Standard  *)
       *   New ( SchemaVar, TagFields );     (* Extended  *)
       *   New ( PtrVar, ConstructorCall );  (* BP object *)
       *
       *   PtrVar:= New ( PtrType );         (* BP ordinary pointer  *)
       *
       *   PtrVar:= New ( PtrType,           (* BP pointer to object *)
       *                  ConstructorCall );
       *
       *  GetMEM (Ptr, Size);		     (* BP *)
       *  Ptr := GetMEM (Size);		     (* BP *)
       *
       * Just to maximize confusion, we allow GetMem to be called
       * as a "function" as well as New.
       *
       * We call New and GetMem as functions.  If called as a procedure,
       * let us do the assignment "inline".  This make code more efficient
       * than having a RTS procedure and doing function calls via a temp.
       */
      if (rts_id == bp_GETMEM)
        {
          if (pedantic)
            warning ("ISO Pascal does not allow `GetMem'");
          if (length > 2)	/* @@ Parser prevents this */
            errstr = "too many arguments to `GetMem'";
          else if (length == 2)
            {
              if (code != POINTER_TYPE)
                errstr = "POINTER type required for `GetMem'";
              else if (TREE_CODE (type2) != INTEGER_TYPE)
                errstr = "size argument to `GetMem' must be of INTEGER type";
            }
          else if (TREE_CODE (type) != INTEGER_TYPE)
            errstr = "size argument to `GetMem' must be of INTEGER type";
        }
      else if (code != POINTER_TYPE)
	errstr = "POINTER type required for `New'";

      if (errstr)
	break;

      if (rts_id == p_NEW && TREE_CODE (TREE_TYPE (type)) == RECORD_TYPE)
	{
          /* Schemas look like records */
	  schema_ids = number_of_schema_discriminants (TREE_TYPE (type));

	  /* If this is a schema type, NEW does two things:
	   *  - it selects the type of the object
	   *  - and allocates the required space for the object.
	   *
	   * So we must create a new type for the object with the
	   * schema discriminants filled with the values 
	   */
	  if (schema_ids)
	    {
	      if (length < schema_ids + 1)
		{
		  error ("`New' applied to this schema type requires %d discriminant value%s",
			 schema_ids, schema_ids > 1 ? "s" : "");
		  return error_mark_node;
		}

	      /* @@@ only works for strings currently */
	      if (! PASCAL_TYPE_STRING (TREE_TYPE (type)))
		abort ();

	      type = build_pointer_type (build_pascal_string_schema (val2));

	      /* Force the type of the variable to be a pointer to
	       * the discriminated schema type instead of a pointer
	       * to the schema type
	       */
	      TREE_TYPE (val) = type;
	    }
	}

#if 1 /* Peter */
      /*
       * Since NEW is now (again) a function, call it 
       * and do the assignment of return value inline.
       * But later.  For now take the first argument
       * out of game (the pointer where the result is assigned).
       *
       * Now the only parameter to the run time system is always the
       * size of the object being created (an integer).
       */
      fpar = ptype_int;

      if (rts_id == bp_GETMEM && length == 1)
        type = ptr_type_node;
      else
        apar = TREE_CHAIN (apar);
#else
      /* Pass a reference to this pointer */
      fpar = chainon (do_ptype (type, 1, 0),
		      ptype_int);
#endif
      if (rts_id == p_NEW)
	{
	  if (length != 1)
	    {
	      /*
	       * Object constructor calls are handled by the parser
	       * so the rest of the values are for tag fields.
	       */
	      length -= schema_ids;
	      
	      if (length != 1)
		{
		  static int informed = 0;
		  if (! informed)
		    {
		      warning ("tag fields ignored in `New'");
		      informed++;
		    }
		}
	    }
	  apar = build_tree_list (NULL_TREE,
				  object_size (TREE_TYPE (type)));
	}

      if (TREE_CODE (val) == TYPE_DECL
          || (rts_id == bp_GETMEM && length == 1))
        {
          /* function-style call */
          rval = type;
        }
      else
        {
          /* procedure-style call */
          retval = rts_call (rts_id, type, fpar, apar);
          expand_expr_stmt (build_modify_expr (val, NOP_EXPR, retval));
          retval = void_type_node;
          rts_inline = 1;

	  /* Un-initialize the object we get from new */
	  init_any (build_indirect_ref (val, "new"), 0);
        }
      break;

  case p_DISPOSE:
      if (code != POINTER_TYPE)
	{
	  errstr = "POINTER type required for `Dispose'";
	  break;
	}
      fpar = chainon (do_ptype (type, 0, 0),
                      ptype_int);
      if (length > 1)
	{
	  static int informed = 0;
	  if (! informed)
	    {
	      warning ("tag fields ignored in `Dispose'");
	      informed++;
	    }
	  apar = copy_node (apar);
	}
      apar = chainon (apar, build_tree_list (NULL_TREE,
					     object_size (TREE_TYPE (val))));
      break;

  case bp_FREEMEM:
      if (pedantic)
        warning ("ISO Pascal does not allow `FreeMem'");
      if (code != POINTER_TYPE)
	{
          errstr = "POINTER type required for `FreeMem'";
	  break;
	}
      if (length > 2)
        {
          errstr = "too many arguments to `FreeMem'";
          break;
        }
      if (length == 2)
        {
          if (TREE_CODE (type2) != INTEGER_TYPE)
            {
              errstr = "second argument to `FreeMem' must be of INTEGER type";
              break;
            }
          /*
           * @@@ Perhaps we should do a run-time check with second parameter?
           */
         }
      else
        {
          fpar = chainon (do_ptype (type, 0, 0), ptype_int);
          apar = chainon (apar, build_tree_list (NULL_TREE,
                                  object_size (TREE_TYPE (val))));
          /*
           * GNU extension (or bug):  Second parameter to FreeMem
           * may be omitted.  In this case it works identically to
           * (the standard version of) Dispose but is named symmetri-
           * cally to GetMem. :-)
           *
           * @@@ Borland Pascal allows to FreeMem parts of an allocated
           * dynamic variable: "GetMem ( p, 1024 ); FreeMem ( p, 512 );".
           * The rest may be FreeMem'ed later using another pointer ...
           */
         }
      break;

  case p_POSITION:
  case p_LASTPOSITION:
      rval = integer_type_node;
      if (code != FILE_TYPE)
	  errstr = "FILE type required";
      else
	{
	  fpar = do_ptype (type, 1, 1);
	  if (TYPE_DOMAIN (type))
	    {
	      retval = build_binary_op (PLUS_EXPR,
					rts_call (rts_id, rval, fpar, apar),
					convert (rval,
						 TYPE_MIN_VALUE (TYPE_DOMAIN (type))),
					0);

	      rts_inline = 1; /* Fake it is inline */
	    }
	  else
	    if (pedantic)
	      warning ("GPC extension: direct access routine applied to a normal file");
	}
      break;

  case p_CLOSE:
  case p_UPDATE:
  case p_PUT:
  case p_GET:
  case p_PAGE:
      if (code != FILE_TYPE)
	  errstr = "FILE type required";
      else
	  fpar = do_ptype (type, 1, 1);
      break;

  case p_HALT:
      if (length == 0)
	  apar = build_tree_list (NULL_TREE, integer_zero_node);
      else if (code != INTEGER_TYPE) {
	  errstr = "HALT requires integer type parameter";
	  break;
      }
      fpar = do_ptype (TREE_TYPE (TREE_VALUE (apar)), 0, 1);
      break;

  case p_BINDING:
      if (pedantic)
	warning ("ISO Pascal does not define `Binding'");

        /* Do not check for FILE_TYPE here; it is enough if p_BIND does it */
	{
	  tree stype = gpc_type_BINDINGTYPE;

	  /* Pass a reference to a temporary variable; run time system
	   * fills it it instead of returning a record type.
	   */
    	  actual_return_value = make_new_variable ("_GPC_binding", stype);

	  fpar = chainon (do_ptype (type, 1, 0),
			  do_ptype (stype,1, 1));
	  
	  apar = chainon (apar, build_tree_list (NULL_TREE, actual_return_value));
	}
      break;

  case p_UNBIND:
      if (pedantic)
	warning ("ISO Pascal does not define `Unbind'");

      /* Do not check for FILE_TYPE here; it is enough if p_BIND does it */
      fpar = do_ptype (type, 1, 1);
      break;

  case p_BIND:
    {
      int bound_type = 0;
      if (pedantic)
	warning ("ISO Pascal does not define `Bind'");

      if (TREE_CODE (type) != FILE_TYPE)
	errstr = "Gpc supports binding only for file type objects (bind)";
      else if (type2 != gpc_type_BINDINGTYPE)
	errstr = "Second argument to `Bind' must be of type BindinType";
#if 0
      /* Why not??? */
      else if (! PASCAL_EXTERNAL_OBJECT (apar))
	errstr = "GPC supports `Bind' only to external files";
#endif
      /* @@@@@@ I'm too tired to do this now:
	 For variable parameters the bindability is determined by
	 the type of the actual parameter, at least when it is a file
	 type.
	 Now I allow binding of all var file parameters, not only
	 those that are declared bindable.
	 @@@@@ FIXME
       */
      else if (! PASCAL_TYPE_BINDABLE (type)
	       && !(TREE_CODE (val) == INDIRECT_REF
		    && (TREE_CODE (TREE_OPERAND (val, 0)) == PARM_DECL
			|| TREE_CODE (TREE_OPERAND (val, 0)) == CONVERT_EXPR
			   && TREE_CODE (TREE_OPERAND (TREE_OPERAND (val, 0), 0))
				== PARM_DECL)))
	errstr = "Type has not been declared `bindable'";
      else
	{
	  /* Only files are currently bindable */
	  bound_type = RTS_BIND_FILE;

	  fpar = chainon (do_ptype (type, 1,0),
			  chainon (do_ptype (type2,1,0),
				   ptype_int));

	  /* Pass the type to run time system as enum tree_code */
	  apar = chainon (apar, build_tree_list (NULL_TREE,
						 build_int_2 (bound_type, 0)));
	}
    }
    break;

  case p_DATE:
  case p_TIME:
      if (pedantic)
	warning ("ISO Pascal does not define `Date' or `Time'");

      if (type != gpc_type_TIMESTAMP)
	errstr = "Timestamp record type parameter required";
      else
	{
	  /* A REFERENCE_TYPE to val and an additional parameter
	   * that is the location where run time system stores
	   * DATE/TIME conversion results.
	   */

	  tree stype = (rts_id == p_DATE) ? gpc_type_DATE : gpc_type_TIME;

    	  actual_return_value = make_new_variable ("_GPC_times", stype);
	  
	  fpar = chainon (do_ptype (type, 1, 0),
			  do_ptype (stype, 1, 1));

	  apar = chainon (apar, build_tree_list (NULL_TREE, actual_return_value));

	  rval = void_type_node;
	}
      break;

  case p_GETTIMESTAMP:
      if (pedantic)
	  warning ("ISO Pascal does not define `Gettimestamp'");
      if (type != gpc_type_TIMESTAMP)
	  errstr = "Timestamp record type parameter required";
      else
	  fpar = do_ptype (type, 1, 1);
      break;

  case p_SEEKUPDATE:
  case p_SEEKREAD:
  case p_SEEKWRITE:
      if (pedantic)
	  warning ("ISO Pascal does not define seek operations");
      if (code != FILE_TYPE)
	  errstr = "FILE type required for seek operations";
      else
	{
	  fpar = chainon (do_ptype (type, 1, 0), ptype_int);

	  if (val2 && ORDINAL_TYPE (code2))
	    {
	      if (TYPE_DOMAIN (type))
		if (base_type (type2)
		    == base_type (TREE_TYPE (TYPE_DOMAIN (type))))
		  {
		    val2 = build_binary_op (MINUS_EXPR,
					    val2,
					    TYPE_MIN_VALUE (TYPE_DOMAIN (type)),
					    0);

		    TREE_VALUE (TREE_CHAIN (apar)) = val2;
		  }
	        else
		  errstr = "Index type does not match direct access file range type";
	      else
		if (pedantic)
		  warning ("GPC extension: Direct access file open applied to a reqular file");
	    }
	  else
	    errstr = "Second argument must be of ordinal type";
	}
      break;

  /* functions */
  case p_EOF:
      rval = boolean_type_node;

      if (length == 0)
	{
	  tree std_input = get_standard_input ();
	  if (std_input)
	    apar = build_tree_list (NULL_TREE, std_input);
	}
      else if (code != FILE_TYPE)
	{
	  errstr = "EOF may only be applied to FILE type objects";
	  break;
	}
      if (apar)
	  fpar = do_ptype (TREE_TYPE (TREE_VALUE (apar)), 1, 1);
      break;

  case p_EOLN:
      rval = boolean_type_node;

      if (length == 0)
	{
	  tree std_input = get_standard_input ();
	  if (std_input)
	    apar = build_tree_list (NULL_TREE, std_input);
	}
      else if (code != FILE_TYPE || TYPE_FILE_TEXT (TREE_TYPE (val)) == NULL_TREE)
	{
	  errstr = "EOLN may only be applied to TEXT type objects";
	  break;
	}
      if (apar)
	  fpar = do_ptype (TREE_TYPE (TREE_VALUE (apar)), 1, 1);
      break;

  case p_POLAR:
      if (code == REAL_TYPE && code2 == REAL_TYPE)
	{
	  rval = complex_type_node;
	  fpar = chainon (do_ptype (type, 0, 0),
			  do_ptype (type2, 0, 1));
	}
      else
	errstr = "The two arguments to `polar' must be of real type";
      break;

  case p_SQRT:
      if (code == COMPLEX_TYPE)
	{
	  rval = complex_type_node;
	  fpar = ptype_complex;
	  rts_id = z_SQRT;
	}
      else
	{
	  rval = double_type_node;
	  if (code == INTEGER_TYPE || code == REAL_TYPE)
	    fpar = ptype_double;
	  else
	    errstr = "argument to `sqrt' must be of integer type or real type";
	}
      break;

  case p_SIN:
  case p_COS:
  case p_EXP:
  case p_LN:
  case p_ARCTAN:
      if (code == COMPLEX_TYPE)
	{
	  rval = complex_type_node;
	  fpar = ptype_complex;
	  switch (rts_id)
	    {
	    case p_SIN:	  rts_id = z_SIN;    break;
	    case p_COS:   rts_id = z_COS;    break;
	    case p_EXP:   rts_id = z_EXP;    break;
	    case p_LN: 	  rts_id = z_LN;     break;
	    case p_ARCTAN:rts_id = z_ARCTAN; break;
	    default: abort ();
	    }
	}
      else
	{
	  rval = double_type_node;
	  fpar = ptype_double;
	}
      break;

  case p_ARG:
      if (code == COMPLEX_TYPE)
	{
	  fpar = ptype_complex;
	  rval = double_type_node;
	}
      else
	errstr = "ARG requires one complex type argument";
      break;

  case r_EXPON:
    	/* Exponent type is checked in the parser... */

      rval = double_type_node;
      if (code == COMPLEX_TYPE)
	{
	  rts_id = z_EXPON;
	  rval   = complex_type_node;
	}
      else if (code == INTEGER_TYPE)
	val  = convert (double_type_node, val);
      else if (code != REAL_TYPE)
	errstr = "left operand of `**' must be of integer, real or complex type";

      if (! errstr)
	fpar = chainon (do_ptype (rval, 0, 0),
			do_ptype (type2, 0, 1));
      break;


  case r_POW:
    	/* Exponent type is checked in the parser... */

      rval = type; /* Type is same as left operand type */
      if (code == COMPLEX_TYPE)
	rts_id = z_POW;
      else if (code == INTEGER_TYPE)
	{
	  /* @@@ Calculate in double type, then convert back to int */
	  /* @@@@ Should be inlined */
	  post_conversion = integer_type_node;
	  rval = double_type_node;
	  val  = convert (double_type_node, val);
	}
      else if (code != REAL_TYPE)
	errstr = "left operand of `pow' must be of integer, real or complex type";

      if (! errstr)
	fpar = chainon (do_ptype (rval, 0, 0),
			do_ptype (type2, 0, 1));
      break;

  case p_EXTEND:
      if (pedantic)
	  warning ("ISO Pascal does not define `Extend'");
  case p_RESET:
  case p_REWRITE:
      if (code != FILE_TYPE)
	  errstr = "FILE type required";
      else {
	  tree size = integer_zero_node;

	  if (val2 != null_pointer_node)
	    if (is_string_type (val2))
	      {
		size = PASCAL_STRING_LENGTH (val2);

		if (is_variable_string_type (type2))
		  {
		    /* Use the string, not the schema type.
		     * Converted to a reference by fpar.
		     */
		    val2 = PASCAL_STRING_VALUE (val2);

		    /* Change the second parameter of the call to be val2
		     * i.e. the array of characters in the string schema.
		     */
		    TREE_VALUE (TREE_CHAIN (apar)) = val2;
		  }
		
		fpar = chainon (do_ptype (type, 1, 0),
				chainon (do_ptype (TREE_TYPE(val2), 1, 0),
					 ptype_int));
	      }
	    else
	      errstr = "Optional file name parameter to REWRITE/RESET/EXTEND must be a string";
	  else
	      fpar = chainon (do_ptype (type, 1, 0),
			      chainon (do_ptype (char_array_type_node, 2, 0),
				       ptype_int));
	  apar = chainon (apar, build_tree_list (NULL_TREE, size));
      }
      break;

  case p_WRITESTR:
      if (pedantic)
	  warning ("ISO Pascal does not define `writestr'");
      /* FALLTHROUGH */

  case p_WRITE:
  case p_WRITELN:
      emit_line_note (input_filename, lineno);
      rts_write (rts_id, apar);
      return NULL_TREE;

  case p_READSTR:
      if (pedantic)
	  warning ("ISO Pascal does not define `readstr'");
      /* FALLTHROUGH */

  case p_READ:
  case p_READLN:
      emit_line_note (input_filename, lineno);
      rts_read (rts_id, apar);
      return NULL_TREE;

  /* extensions */
  case p_MARK:
      if (pedantic)
	  warning ("ISO Pascal does not define `Mark'");
      if (code != POINTER_TYPE)
	  errstr = "Pointer type required for `Mark'";
      else
	  fpar = do_ptype (type, 1, 1);
      break;

  case p_RELEASE:
      if (pedantic)
	  warning ("ISO Pascal does not define `Release'");
      if (code != POINTER_TYPE)
	  errstr = "Pointer type required for `Release'";
      else
	  fpar = do_ptype (type, 0, 1);
      break;

  case p_DEFINESIZE:
      if (pedantic)
	  warning ("ISO Pascal does not define `Definesize'");
      if (code != FILE_TYPE)
	  errstr = "FILE type required for `Definesize'";
      else
	  fpar = chainon (do_ptype (type, 1, 0), ptype_int);
      break;

  case p_EMPTY:
      if (pedantic)
	  warning ("ISO Pascal does not define `Empty'");
      if (code != FILE_TYPE)
	  errstr = "FILE type required for `Empty'";
      else
	  fpar = do_ptype (type, 1, 1);
      rval = boolean_type_node;
      break;

  /* String comparisons */
  case '=':
  case NEQ:
  case '<':
  case LTE:
  case '>':
  case GTE:
  /* Additional extended pascal string functions */
  case p_EQ:
  case p_LT:
  case p_LE:
  case p_GE:
  case p_GT:
  case p_NE:
  case p_TRIM:
  case p_INDEX:
  case p_SUBSTR:
      if (pedantic)
	  warning ("ISO Pascal does not define string functions");

      if (val && (val2 || rts_id == p_TRIM))
	{
	  emit_line_note (input_filename, lineno);
	  return rts_string (rts_id, apar);
	}
      else
	errstr = "Incorrect number of parameters to a string routine";
      break;

  default:
      ASSERT (0, "no such run time system routine");
  }

  if (errstr) {
      error (errstr);      
      return error_mark_node;
  }

  /* Output a RTL line note */
  emit_line_note (input_filename, lineno);

  /* construct a call to the run time system if not compiled inline */

  if (! rts_inline)
    {
      retval = rts_call (rts_id, rval, fpar, apar);

      /* if this is a statement, built rtl from it */
      /* otherwise let the caller do whatever it likes to do with it */
      if (rval == void_type_node)
	{
	  expand_expr_stmt (retval);

	  /* If we need to return something, like a string that the
	   * RTS wrote into in p_DATE/p_TIME
	   */
	  if (actual_return_value)
	    retval = actual_return_value;
	}
      else
	if (post_conversion)
	  retval = convert (post_conversion, retval);

      /* Un-initialize at run-time the node in UN_INIT_THIS */
      if (un_init_this)
	  init_any (un_init_this, 0);
    }      
  return retval;
}

/*
 * Each variable length string looks like:
 *
 * string = RECORD
 *		Capacity : Integer;
 *		length   : Integer;
 *		string   : packed array [ 1 .. Capacity ] of char;
 *          END;
 *
 * PASCAL_TYPE_STRING (string) = 1;
 */

tree
build_pascal_string_schema (capacity)
     tree capacity;
{
  tree string;
  tree fields;
  /* If the string index type is altered,
     verify integer_ptr_type_node usage */
  tree string_range = build_range_type (integer_type_node,
					integer_one_node,
					capacity);

  fields = chainon (grokfield (input_filename, lineno,
			       get_identifier ("Capacity"),
			       build_tree_list (NULL_TREE,
						integer_type_node),
			       NULL_TREE),
		    chainon (grokfield (input_filename, lineno,
					get_identifier ("length"),
					build_tree_list (NULL_TREE,
							 integer_type_node),
					NULL_TREE),
			     grokfield (input_filename, lineno,
					get_identifier ("string"),
					build_tree_list (NULL_TREE,
							 grok_packed (build_array_type
								      (char_type_node,
								       string_range))),
					NULL_TREE)));

  string = finish_struct (start_struct (RECORD_TYPE, NULL_TREE),
			  fields, NULL_TREE); 

  /* Flag this as a variable-string-type */
  PASCAL_TYPE_STRING (string) = 1;

  return  string;
}

/* Constructors for a pointer that points to an unknown
 * type.
 */

tree
build_pascal_pointer_type (type)
     tree type;
{
  int temporary;
  tree t;
  tree name = type;

  if (TREE_CODE (name) != IDENTIFIER_NODE)
    if (defining_types)
      {
	tree scan;
	tree decl = TYPE_NAME (name);

	/* If the type has no name, it might be a
	 * function pointer or string schema
	 */
	if (! decl)
	  return build_pointer_type (type);

	if (TREE_CODE (decl) != TYPE_DECL)
	  abort ();
	name = DECL_NAME (decl);

	for (scan = current_type_list; scan; scan = TREE_CHAIN (scan))
	  {
	    /* If there already is a local type declaration
	     * in this scope, use that.
	     */
	    
	    if (TREE_VALUE (scan) == name)
	      {
		tree lv = IDENTIFIER_LOCAL_VALUE (TREE_VALUE (scan));
		if (! lv)
		  {
		    if (global_bindings_p())
		      lv = IDENTIFIER_GLOBAL_VALUE (TREE_VALUE (scan));

		    if (! lv)
		      lv = error_mark_node;
		  }
		if (TREE_CODE (lv) != TYPE_DECL)
		  {
		    error ("Pointer domain `%s' is not a type",
			   IDENTIFIER_POINTER (name));
		    break;
		  }
		return build_pointer_type (TREE_TYPE (lv));
	      }
	  }
      }
    else
      return build_pointer_type (type);

  /* The type is an identifier, which is a reference to
   * a type that is unknown. We have no knowledge what
   * the type might be. In C, the
   *   typedef struct unknown *ptr;
   * specifies that the pointer will be to a struct.
   *
   */
  if (! defining_types)
    {
      error ("Pointer domain type undefined");
      return TYPE_POINTER_TO (integer_type_node);
    }

  type = xref_tag (LANG_TYPE, name);

#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    {
#if 0
      /* Don't bother, it is documented in PROBLEMS file */
      static int informed = 0;
      if (!informed) {
	warning("@@@ Option -g used with a program using forward");
	warning("    pointers in a machine with SDB debug symbols.");
	informed++;
      }
#endif /* 0 */
    }
#endif /* SDB_DEBUGGING_INFO */

#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
  /* Avoid writing incorrect dbx info */
  if (write_symbols == DBX_DEBUG)
    dbxout_set_type_status (type, 1);
#endif /* defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO) */
  
  temporary = allocation_temporary_p ();

  push_obstacks_nochange ();

  if (global_bindings_p() && temporary)
    end_temporary_allocation ();

  t = make_node (POINTER_TYPE);

  /* The type where the pointer points to is in
     TREE_TYPE (t).

     In the forward case it is a LANG_TYPE node which will
     be patched later.
   */
  TREE_TYPE (t) = type;

  /* Remember the pointer type that points to type.
   * Uses the LANG_TYPE field pointer_to.
   */
  TYPE_POINTER_TO (type) = t;

  layout_type (t);

  pop_obstacks ();

  return t;
}


/* the ISO 7185 standard says that the two ways, abbreviated (when
 * giving several index types) and full (when listing each index type
 * separately), of specifying and accessing arrays are equivalent.
 *
 * So we represent arrays internally in their full form.
 */

/* the ELT_TYPE is the user specified element type.
 *
 * the INDEX_LIST is a list of TREE_LIST nodes of index types,
 * in forward order, and thus the ELT_TYPE is the type for the
 * last element in the INDEX_LIST.
 *
 * Returns the type of the complete array specified by the user.
 */
tree
build_pascal_array_type (elt_type, index_list)
tree elt_type;
tree index_list;
{
    tree link;
    for (link = nreverse (index_list); link; link = TREE_CHAIN (link))
      elt_type = build_array_type (elt_type, TREE_VALUE(link));

    return elt_type;
}

/*
 * Build a reference to an array slice
 *
 * ISO-10206 requires ARR to be a string-type, but gpc allows
 * slice accesses to all arrays.
 *
 * SLICE_INX is a TREE_LIST node whose TREE_VALUE is the lower bound
 * and TREE_PURPOSE is the higher bound of the slice range.
 */
static tree
build_array_slice_ref (arr, slice_inx)
     tree arr;
     tree slice_inx;
{
  tree start;
  tree slice;
  tree sub_ptr;
  tree sub_type;
  tree component_type = char_type_node;
  int  is_string = is_string_type (arr);
  
  if (! is_string)
    {
      if (TREE_CODE (TREE_TYPE (arr)) != ARRAY_TYPE)
	{
	  error ("Array slice access requires an array object");
	  return error_mark_node;
	}

      if (pedantic)
	warning ("ISO Pascal restricts subarray accesses to string types");

      component_type = TREE_TYPE (TREE_TYPE (arr));
    }
  
  /* Build a array type that is the same length
   * as the sub-array access is, but the new array always starts
   * from index 1.
   */
  sub_type = build_pascal_array_type
    		(component_type,
		 build_tree_list
		  (NULL_TREE,
		   build_range_type
		    (integer_type_node,
		     integer_one_node,
		     build_binary_op
		      (PLUS_EXPR,
		       integer_one_node,
		       build_binary_op (MINUS_EXPR,
					TREE_PURPOSE (slice_inx),
					TREE_VALUE (slice_inx),
					1),
		       1))));

  /* If the array is packed or a string, so is the subarray */
  if (is_string || PASCAL_TYPE_PACKED (TREE_TYPE (arr)))
    sub_type = grok_packed (sub_type);

  sub_ptr = build_pointer_type (sub_type);

  /* Form a pointer to the start of the subarray */
  start = build1 (ADDR_EXPR,
		  sub_ptr,
		  build_array_ref (PASCAL_STRING_VALUE (arr),
				   TREE_VALUE (slice_inx)));
	  
  /* Form an access to the slice and return the access */
  return build1 (INDIRECT_REF, sub_type, start);
}

/*
 * Constructs a reference to a pascal array. The VAR_ACCESS is
 * a VAR_DECL node and the INDEX_LIST is a TREE_LIST chain of
 * expressions to index the array with. This INDEX_LIST is
 * passed in forward order.
 */
tree
build_pascal_array_ref (var_access, index_list)
     tree var_access;
     tree index_list;
{
    tree link;

    if (TREE_CODE (TREE_TYPE (var_access)) != ARRAY_TYPE
	&& ! PASCAL_TYPE_STRING (TREE_TYPE (var_access)))
      {
	error ("subscripted object is not array or string type");
	return error_mark_node;
      }

    for (link = index_list; link; link = TREE_CHAIN (link))
      {
	if (TREE_PURPOSE (link))
	  var_access = build_array_slice_ref (var_access, link);
	else
	  {
	    if (PASCAL_TYPE_STRING (TREE_TYPE (var_access)))
	      var_access = PASCAL_STRING_VALUE (var_access);
	    
	    var_access = build_array_ref (var_access, TREE_VALUE(link));
	  }
	if (var_access == error_mark_node)
	  break;
      }
    return var_access;
}

static int
guess_precision (minval, maxval)
tree minval, maxval;
{
    if (int_fits_type_p (minval, signed_char_type_node) &&
	int_fits_type_p (maxval, signed_char_type_node))
	return TYPE_PRECISION (signed_char_type_node);
    else if (int_fits_type_p (minval, short_integer_type_node) &&
	     int_fits_type_p (maxval, short_integer_type_node))
	return TYPE_PRECISION (short_integer_type_node);
    else if (int_fits_type_p (minval, integer_type_node) &&
	     int_fits_type_p (maxval, integer_type_node))
	return TYPE_PRECISION (integer_type_node);
    else if (int_fits_type_p (minval, long_integer_type_node) &&
	     int_fits_type_p (maxval, long_integer_type_node))
	return TYPE_PRECISION (long_integer_type_node);
    else {
	error ("range will not fit in the largest integer type available");
	return TYPE_PRECISION (long_integer_type_node);
    }
}

/* Create and return a type for signed integers of PRECISION bits, if
   the bounds are constant, otherwise just return a new range type with
   variable bounds. */
tree
make_signed_range (minval, maxval)
tree minval;
tree maxval;
{
  register tree type = make_node (INTEGER_TYPE);

  /* set the upper and lower limits of the range */

  TYPE_MIN_VALUE (type) = minval;
  TYPE_MAX_VALUE (type) = maxval;

  if (TREE_CODE (minval) == INTEGER_CST && TREE_CODE (maxval) == INTEGER_CST) {

      /* Set these things only if the bounds are constant */

      TYPE_PRECISION (type) = guess_precision (minval, maxval);
      
      /* Lay out the type: set its alignment, size, etc.  */
      
      TYPE_SIZE (type) = 0;
      layout_type (type);
  }      
  return type;
}

/*
 * The First word of the FILE_TYPE BLKmode variable is
 * a pointer to the file buffer we want to access.
 *
 *  So we need to generate:
 *  *(*(&file))
 *
 * If symbol LAZY_IO is defined, each buffer access needs to check
 * that the buffer is valid. If not, we need to do a get before
 * accessing the buffer.
 *
 *  If LAZY_IO is defined, we can select at compile time if we want to use
 * it or not; run time system should support both that and the old method.
 *
 * Lazy I/O helps to overcome the biggest problem of Pascal: It's I/O system.
 *
 * When we do a reset or read something from a file, the old method
 * needs to read new contents to the buffer before the data is
 * actually needeed. This is annoying if you do interactive programs,
 * the output to terminal asking for input comes after you have
 * already given the input to the program, or then you have to code
 * things differently for terminals and files, which is also annoying.
 *
 * The old method GPC uses is the same implementation dependent feature
 * that the Pax compiler uses. The run time system checks each file when
 * it's reset, and if it's a terminal, it sets EOLN implicitely on and places
 * a space in the file buffer. This is valid according to the standard, since
 * it explicitely states that handling of INPUT and OUTPUT is implementation
 * dependent.
 *
 * Lazy I/O means that we must do a PUT as early as we can, and GET as
 * late as we can. The first condition is satisfied either by not
 * buffering output at all, or then flushing output to terminals
 * before each get; the second condition is fulfilled when we check
 * that the buffer is valid each time we generate buffer references.
 */

tree
build_buffer_ref (file)
tree file;
{
  register tree ft = TREE_TYPE (file); 	/* type of variable */
  tree lazy_undefined = size_int (FiUnd);
  
  if (TREE_CODE (ft) == FILE_TYPE) {
      tree t = TREE_TYPE (ft);		/* type of file component */
      register tree ref;

      tree file_addr = build1 (ADDR_EXPR, ft, file);

#ifdef LAZY_IO
      /* Fill file buffer if it is undefined */
      /* @@@@ To use LAZY_IO implement code that does a "get" if the file buffer is undefined */
      /* @@@@ Easy way out: call run time routine _p_lazyget(File); */
#ifdef LAZY_IO_TEXT
      if (TYPE_FILE_TEXT (TREE_TYPE (file)))
#endif
	expand_expr_stmt (rts_call (r_LAZYGET,
				    void_type_node,
				    do_ptype (TREE_TYPE (file), 1, 0),
				    build_tree_list (NULL_TREE, file)));
#endif /* LAZY_IO */

      ref = build1 (INDIRECT_REF, TYPE_MAIN_VARIANT (t),	       /* type we point to */
 		    build1 (INDIRECT_REF, build_pointer_type (t),      /* Through this pointer */
			    file_addr));			       /* Address of file */
      TREE_READONLY (ref) = TREE_READONLY (t);
      TYPE_VOLATILE (ref) = TYPE_VOLATILE (t) || TYPE_VOLATILE (file);
      TREE_THIS_VOLATILE (ref) = TYPE_VOLATILE (t);
      return ref;
  } else if (file != error_mark_node && ft != error_mark_node)
      error ("file buffer referencing requires a FILE type argument");
  return error_mark_node;
}

/*
 * Return standard input/output node of current module. 
 * If not found, return global_in/output_file_node and warn
 * about ISO violation.  Never return NULL.
 */
tree
get_standard_input ()
{
  if (!current_module->input_file_node)
    {
      current_module->input_file_node = global_input_file_node;
      if (pedantic)
        if (current_module->main_program)
          warning ("file `Input' was not mentioned in program header");
        else
          warning ("`StandardInput' not imported by module");
    }
  return current_module->input_file_node;
}

tree
get_standard_output ()
{
  if (!current_module->output_file_node)
    {
      current_module->output_file_node = global_output_file_node;
      if (pedantic)
        if (current_module->main_program)
          warning ("file `Output' was not mentioned in program header");
        else
          warning ("`StandardOutput' not imported by module");
    }
  return current_module->output_file_node;
}


/* Internal routine to un_initialize_block.

   This one does the initialization of the object
   (or a component of an object) specified in decl.
   This type is a BASIC type, i.e. not a structured type
   that has been exploded up by init_any ()
 */
static void
init_simple (decl, the_end)
tree decl;
int the_end;
{
    tree type = TREE_TYPE (decl);
    enum tree_code code = TREE_CODE (type);

    /* Clean up only local files */
    if (the_end && code != FILE_TYPE)
	return;	/* Forget them! */

    switch (code) {
    case RECORD_TYPE:
    case ARRAY_TYPE:
	abort ();
    case FILE_TYPE :
	{
	    char *temp;
	    tree fname;
	    int  byte_size = 0;
	    tree file_size;
	    tree file_kind;
	    int lazyfile = 0;
	    tree file_type = TREE_TYPE (type);
	    
	    if (the_end)
	      {
		/* Close the files on exit */
		build_rts_call (p_CLOSE,
				build_tree_list (NULL_TREE, decl));
		break;
	      }
	    
	    /* Always pass the internal name to the run time system,
	     * not only for external files.
	     */
	    if (TREE_CODE (decl) == VAR_DECL)
	      temp  = IDENTIFIER_POINTER (DECL_NAME (decl));
	    else if (TREE_CODE (decl) == INDIRECT_REF)
	      temp = "<Allocated from heap>";
	    else
	      temp = "<Name unknown>";

	    fname = build_string (strlen (temp)+1, temp);
	    TREE_TYPE (fname) = char_array_type_node;
	    fname = build_tree_list (NULL_TREE,
				     build_unary_op (ADDR_EXPR, fname, 0));

	    if (pedantic && PASCAL_TYPE_PACKED (type))
	      warning ("Word `PACKED' has no effect on external file format");
	    
	    /* Make a TREE_LIST node of the size of the file buffer */
	    /* the size is in bits, if the file is packed, else bytes */
	    /* (the run time system does not pack files yet, so it converts */
	    /*  sizes back to bytes) */
	    
#if 0
	    /* @@@ This feature does not work...
	     *
	     * Check if it is a subrange
	     * @@ Only constant bounds accepted.
	     *
	     * Tells the run time system that the file buffer
	     * actually fits in one byte, even if the file
	     * element size is larger (this is used for
	     * integer subranges, e.g: 0..255
	     */
	    if (TREE_CODE (file_type) == INTEGER_TYPE
		&& TREE_TYPE (file_type)
		&& TREE_CODE (TREE_TYPE (file_type)) == INTEGER_TYPE)
	      {
		tree lo = TYPE_MIN_VALUE (file_type);
		tree hi = TYPE_MAX_VALUE (file_type);

		if (TREE_CODE (lo) == INTEGER_CST
		    && TREE_CODE (hi) == INTEGER_CST
		    && TREE_INT_CST_LOW (lo) >= 0)
		  {
		    int h = TREE_INT_CST_LOW (hi);

		    if (h <= (1 << BITS_PER_UNIT))
		      byte_size = 1;
		  }
	      }
#endif /* 0 */

	    file_size =
	      build_tree_list
		(NULL_TREE,
		 (! PASCAL_TYPE_PACKED (type) ?
		  size_in_bytes (file_type) :
		  fold (build (MULT_EXPR, sizetype,
			       size_in_bytes (file_type),
			       size_int (BITS_PER_UNIT)))));

#ifdef LAZY_IO_TEXT
	    lazyfile = TYPE_FILE_TEXT (type) != NULL_TREE;
#else
#if LAZY_IO
	    /* There is no advantage for lazy files that are not text files
	     * but there is the overhead of validating the file buffer
	     * before it is referenced
	     */
	    if (flag_lazy_io)
	      lazyfile = TYPE_FILE_TEXT (type) != NULL_TREE;
#endif
#endif
	    
	    file_kind = 
		build_tree_list
		  (NULL_TREE,
		   size_int (  !!TYPE_FILE_TEXT (type) 	     << fkind_TEXT
			     | PASCAL_TYPE_PACKED (type)     << fkind_PACKED
			     | PASCAL_EXTERNAL_OBJECT (decl) << fkind_EXTERN
			     | lazyfile		             << fkind_LAZY
			     | !!TYPE_DOMAIN (type) 	     << fkind_DIRECT
			     | byte_size		     << fkind_BYTE));

	    /* Call and construct parameters to the run time system
	     * routine that initializes a file buffer to a known state
	     */
	    expand_expr_stmt
	      (rts_call (r_INITFDR,
			 void_type_node,
			 do_ptype (type, 1, 0),
			 tree_cons (NULL_TREE, decl,
				    chainon (fname,
					     chainon (file_size, file_kind)))));
	    break;
	}
	default :
	    /* nothing yet!!! */ ;		    
    }
}

/* Subroutine of init_any */
static void
init_record (thing, the_end, base)
tree thing;
int the_end;
tree base;
{
    tree field;

    for (field = TYPE_FIELDS (TREE_TYPE (thing));
	 field;
	 field = TREE_CHAIN (field)) {
	if (DECL_NAME (field))
	    init_any (build_component_ref (base, DECL_NAME (field)),
		      the_end);
	else {
	    tree inner_type = TREE_TYPE (field);
	    if (TREE_CODE (inner_type) == RECORD_TYPE || 
		TREE_CODE (inner_type) == UNION_TYPE)
		init_record (field, the_end, base);
	    else
		warning ("strange unnamed field in record");
	}
    }
}

/* Nonzero if TYPE contains a FILE_TYPEor a schema
 * that needs to be initialized
 */
static int
contains_file_or_schema_p (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);
  switch (code) {
  case FILE_TYPE:
    return 1;
  case ARRAY_TYPE:
    return contains_file_or_schema_p (TREE_TYPE (type));
  case RECORD_TYPE:
    {
      tree field;
      
      /* @@@ Only string schemas implementad currently */
      if (PASCAL_TYPE_STRING (type))
	return 1;

      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  tree inner_type = TREE_TYPE (field);
	  
	  if (!DECL_NAME (field) &&
	      (TREE_CODE (inner_type) != RECORD_TYPE
	       && TREE_CODE (inner_type) != UNION_TYPE))
	    abort ();
	  
	  if (contains_file_or_schema_p (inner_type))
	    return 1;
	}
    }
    break;

  default:
    break;
  }

  return 0;
}

/* Subroutine of un_initialize_block
 *
 * Recursively propagate through a structured type
 * un_initializing all simple types with init_simple ()
 */
static void
init_any (thing, the_end)
tree thing;
int the_end;
{
    tree temp1;
    tree type = TREE_TYPE (thing);
    enum tree_code code = TREE_CODE (type);

    /* Only FILE_TYPE and STRING SCHEMA variables are initialized now */
    /* This check is good for huge arrays that don't contain those :-) */
    if (!contains_file_or_schema_p (type))
      return;

    switch (code) {
    case FILE_TYPE :
	init_simple (thing, the_end);
	break;
    case ARRAY_TYPE :
      temp1 = TYPE_DOMAIN (type);

      if (TREE_CODE (TYPE_MIN_VALUE (temp1)) == INTEGER_CST
	  && TREE_CODE (TYPE_MAX_VALUE (temp1)) == INTEGER_CST)
	{
	  int i;
	  int maxint = TREE_INT_CST_LOW (integer_maxint_node);
	  int max = TREE_INT_CST_LOW (TYPE_MAX_VALUE (temp1));
	  
	  /* Avoid wraparound if i is maxint */
	  for (i = TREE_INT_CST_LOW (TYPE_MIN_VALUE (temp1));
	       i <= max && i <  maxint; i++)
	    init_any (build_array_ref (thing,
				       build_int_2 (i, 0)),
		      the_end);
	}
      else if (pedantic)
	warning ("Dynamic arrays are not initialized properly");
	break;

    case RECORD_TYPE :
      if (PASCAL_TYPE_STRING (type))
	{
	  /* String schema discriminant identifier initializing */
	  if (! the_end)
	    expand_expr_stmt
	      (build_modify_expr
	       (PASCAL_STRING_CAPACITY (thing),
		NOP_EXPR,
		TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (PASCAL_STRING_VALUE (thing))))));
	  break;
	}
	/* FALLTHROUGH */

    case UNION_TYPE :	/* In Pascal only in variant records */
	init_record (thing, the_end, thing);
	break;
    }
}

/*
 * This code will (some day) un-initialize all variables
 * declared in the current block level.
 *
 * It does not do it yet, however, except for FILE_TYPE
 * nodes that won't work if not un-initialized.
 *
 * THE_END is nonzero if this is called to clean up after
 * the block's statements have been executed.
 *
 *  This also initializes files in
 *   1) components of structured types and
 *   2) parts of dynamic objects.
 *
 */
void
un_initialize_block (names, the_end)
tree names;
int the_end;
{
    tree decl;
    for (decl = names; decl; decl = TREE_CHAIN (decl))
	if (TREE_CODE (decl) == VAR_DECL)
	  init_any (decl, the_end);
}

/* @@@ range needs to be the correct type!!!! */
tree
convert_type_to_range (type)
tree type;
{
    enum tree_code code = TREE_CODE (type);
    tree min, max;
    tree itype;

    if (! ORDINAL_TYPE (code)) {
	error ("ordinal type expected");
	return error_mark_node;
    }
    min = TYPE_MIN_VALUE (type);
    max = TYPE_MAX_VALUE (type);

    if (TREE_TYPE (min) != TREE_TYPE (max)) {
	error ("range limits are not of the same type");
	return error_mark_node;
    }
    itype = make_signed_range (TYPE_MIN_VALUE (type),
			       TYPE_MAX_VALUE (type));
    
    /* see comment in tree.def in INTEGER_TYPE */
    TREE_TYPE (itype) = type;
    return itype;
}

/*
 * Enables/disables procedure/function evaluation when
 * the routines have no parameters. 
 * Reason:
 *   1) When parsing an IDENTIFIER it may be either a routine
 *	name or a variable name.
 *   2) When passing such an item to a routine, it may have to be
 *	evaluated or not depending of the corresponding FORMAL
 *	parameter type; if this is a procedural parameter, the
 *	routine should be passed, if not, the routine should be
 *	evaluated, and the result passed.
 */
static int evaluate_function_calls = 1;

/*
 * disables the evaluation of procedure/function calls with no parameters
 * and returns the old value of the enable/disable flag.
 *
 * They need to be stacked, because routine calls can be nested :-)
 */
int
suspend_function_calls ()
{
    int old = evaluate_function_calls;
    evaluate_function_calls = 0;
    return old;
}

/*
 *  Resumes routine calls for routines with no parameters
 *  if STATE is nonzero.
 *
 *  STATE should be the value previously returned by
 *  suspend_function_calls ().
 */
void
resume_function_calls (state)
int state;
{
    evaluate_function_calls = state;
}

/* A problem with the function calls again...
 * If a forward declared/external function is used in an expression that is
 * part of some function arguments it will not be called by the routine
 * maybe_call_function()
 *
 * The function probably_call_function() is used when we know that the function
 * is not a function parameter but rather should be evaluated.
 */
tree
probably_call_function (fun)
     tree fun;
{
  tree t = TREE_TYPE (fun);

  /* If this is a function without parameters, call it */
  if (TREE_CODE (fun) == FUNCTION_DECL
      && TREE_CODE (t) == FUNCTION_TYPE
      && TREE_CODE (TREE_TYPE (t)) != VOID_TYPE
      && TYPE_ARG_TYPES (t)
      && TREE_CODE (TREE_VALUE (TYPE_ARG_TYPES (t))) == VOID_TYPE)
    {
      fun = build_function_call (fun, NULL_TREE);
    }

  return fun;
}

/*
 * Maybe call the function, or pass it as a routine parameter.
 * The problem is that the corresponding argument type is not known
 * when the factor is parsed. Neither is it known if this is part
 * of an expression...
 */
tree
maybe_call_function (fun, args)
     tree fun;
     tree args;
{
    tree temp = fun;

    /*
       1) this is a procedure statement without parameters
       2) this is an assignment from function with no parameters
          to some variable, or passing the RESULT of such function
	  to another routine. @@@@ it may also have args, for
	  function pointer calls. @@@@
       3) this procedure or function is being passed to a procedural
          parameter list.

       The problem is this:
         in 1 & 2 above the procedure/function should be CALLED;
	 in 3 it should NOT be called, but rather passed as a function
	 definition.
     */

    if (evaluate_function_calls)
      if (TREE_CODE (fun) == FUNCTION_DECL
	  || args
	  || PASCAL_PROCEDURE_PARAMETER(fun))
	{
	  if (args == empty_arglist)
	    args = NULL_TREE;
	  
	  temp = build_function_call (fun, args);
	}

    return temp;
}


/* Get a FIELD_DECL node of structured type OBJ.
 * This is only applied for structures with no variant part,
 * so it is much simpler than find_field.
 */
static tree
simple_get_field (name, obj, errmsg)
     register tree name;
     tree obj;
     char *errmsg;
{
  register tree field = TYPE_FIELDS (obj);
  while (field && DECL_NAME (field) != name)
    field = TREE_CHAIN (field);
  if ((! field) && errmsg)
    error (errmsg, IDENTIFIER_POINTER (name));
  return field;
}

/* Build a method call out of a COMPONENT_REF */
tree
call_method (cref, args)
     tree cref;
     tree args;
{
  tree obj = TREE_OPERAND (cref, 0);
  tree fun = TREE_OPERAND (cref, 1);
  register tree type;

  if (! IS_OBJECT_TYPE (TREE_TYPE (obj)))
    {
      error ("calling method of something not an object");
      return error_mark_node;
    }
  /* If this is a method, call it */
  if (TREE_CODE (fun) == FUNCTION_DECL
      && TREE_CODE (TREE_TYPE(fun)) == FUNCTION_TYPE)
    {
      int is_virtual = PASCAL_VIRTUAL_METHOD (fun);
      if (TREE_CODE (obj) == TYPE_DECL)
        {
          /*
           * This is an explicit call to a parent's method.
           * In this case, no constructor code is generated.
           */
          if (defining_methods)
            {
              fun = simple_get_field (DECL_NAME (fun), TREE_TYPE (obj),
                                      "method not found");
              if (! fun)
                return error_mark_node;
              obj = lookup_name (self_name_node);
            }
          else
            {
              error ("trying to call inherited method outside a method");
              return error_mark_node;
            }
        }
      else if (is_virtual
          || PASCAL_CONSTRUCTOR_METHOD (fun))
        {
          tree vmt_field = simple_get_field (get_identifier ("vmt"),
                             TREE_TYPE (obj),
                             "Internal GPC error: no implicit VMT field");
          if (PASCAL_CONSTRUCTOR_METHOD (fun))
            {
              /* Generate constructor code in the caller
               */
              char vmt_name [256];
              tree vmt;
              tree obj_type_name = TYPE_LANG_SPECIFIC (TREE_TYPE (obj))->elts [0];
            
              /* Get the VMT (an external variable) */
              if (! obj_type_name)
                {
                  error ("Internal GPC error: method of unnamed object type called");
                  obj_type_name = get_unique_identifier ("object", 0);
                }
              else if (TREE_CODE (obj_type_name) == TYPE_DECL)
                obj_type_name = DECL_NAME (obj_type_name);
              sprintf (vmt_name, "vmt_%s", IDENTIFIER_POINTER (obj_type_name));
              vmt = lookup_name (get_identifier (vmt_name));
            
              /* Assign the address of VMT to the object's VMT field */
              if (vmt && vmt_field) 
                {
                  tree vmt_addr = build_pascal_unary_op (ADDR_EXPR, vmt, 0);
                  vmt_field = build (COMPONENT_REF, TREE_TYPE (vmt_field),
                                     obj, vmt_field);
                  expand_expr_stmt (build_modify_expr (vmt_field, 
                                                       NOP_EXPR, vmt_addr));
                }
              else
                error ("Internal GPC error: cannot assign VMT to object instance");
            }
          if (is_virtual)
            {
              tree vmt_field = simple_get_field (get_identifier ("vmt"),
                               TREE_TYPE (obj),
                               "Internal GPC error: no implicit VMT field");
              tree method = NULL_TREE;
              /*
               * @@@ Here a runtime check can be added: does the VMT field
               * of the object TREE_OPERAND (cref, 0) really point to a VMT?
               */
              if (vmt_field)
                method = simple_get_field (DECL_NAME (fun),
                           TREE_TYPE (TREE_TYPE (vmt_field)),
                           "Internal GPC error: virtual method `%s' not in VMT");
              if (method)
                {
                  fun = build (COMPONENT_REF, TREE_TYPE (vmt_field), obj, vmt_field);
                  fun = build_indirect_ref (fun, "virtual method call");
                  fun = build (COMPONENT_REF, TREE_TYPE (method), fun, method);
                  fun = build_indirect_ref (fun, "virtual method call");
                }
              else
                fun = error_mark_node;
            }
        }

      if (!is_virtual)
        {
          /* Not a virtual method.  Use the function definition node rather
           * than the forward declaration inside the object if possible.
           * Otherwise, inline optimizations of methods don't work.
           */
          tree realfun = lookup_name (DECL_ASSEMBLER_NAME (fun));
          if (realfun)
            fun = realfun;
        }

      /* avoid warning about pointer conversion */
      obj = copy_node (obj);
      if ((type = TREE_TYPE (fun))
          && (type = TYPE_ARG_TYPES (type))
          && (type = TREE_VALUE (type))
          && (type = TREE_TYPE (type))
          && IS_OBJECT_TYPE (type))
        TREE_TYPE (obj) = type;
      else
        error ("Internal GPC error: self type not defined in method call");

      /* Check if OBJ is an lvalue and do the call */
      if (lvalue_or_else (obj, "method call"))
        {
          if (args)
            fun = build_function_call (fun,
                    chainon (build_tree_list (NULL_TREE, obj), args));
          else
            fun = build_function_call (fun, build_tree_list (NULL_TREE, obj));
        }
    }
  else
    error ("invalid method call");
  return fun;
}

/* Construct the external name "Myobj_Mymethod" for a method.
 * This won't crash with a user-declared function "Myobj_mymethod"
 * but will be accessable from a C program using a pascal module.
 */
tree
get_method_name (object_name, method_name)
     tree object_name, method_name;
{
  if (object_name && method_name)
    {
      char *object_id = IDENTIFIER_POINTER (object_name);
      char *method_id = IDENTIFIER_POINTER (method_name);
      /* allocate enough space to avoid alignment problems with i386 */
      char *copy = oballoc (IDENTIFIER_LENGTH (object_name)
                            + IDENTIFIER_LENGTH (method_name) + 5);
      sprintf (copy, "%s_%s", object_id, method_id);
      return get_identifier (copy);
    }
  else if (method_name)
    return method_name;
  else
    abort ();
}

/* Push an implicit "Self" reference parameter for method definitions */
void
push_self_parameter (object_name, xref)
     tree object_name;
     int xref;
{
  tree type = lookup_name (object_name);
  /* Enable use of forward referenced type */
  if (type)
    type = TREE_TYPE (type);
  else
    {
      type = xref_tag (LANG_TYPE, object_name);
      if (! xref)
        error ("object type name expected, identifier `%s' given",
               IDENTIFIER_POINTER (object_name));
    }
  handle_formal_param_list (build_tree_list (NULL_TREE, self_name_node),
                            type, 1, 0);
}

/* Make sure that the object type being declared exists as a
 * cross reference.  This is needed for implicit "Self" parameters.
 */
void
check_object_pointer ()
{
  tree p, ptr = NULL_TREE, d, ptr_name_node;
  char ptr_name [256];

  /* In most practical cases the user program will have
   * explicitely specified a pointer type pointing to the
   * object being declared.  Recycle that type, if possible.
   */
  for (p = current_type_list; p; p = TREE_CHAIN (p))
    {
      tree t = TREE_TYPE (lookup_name (TREE_VALUE (p)));
      if (TREE_CODE (t) == POINTER_TYPE
          && TYPE_NAME (TREE_TYPE (t)) == current_type_name)
        {
          ptr = p;
          break;
        }
    }

  /* No user-defined pointer type found.
   * Define it here as "ptr_to_Myobj".
   */
  if (!ptr)
    {
      sprintf (ptr_name, "ptr_to_%s", IDENTIFIER_POINTER (current_type_name));
      ptr = build_pascal_pointer_type (current_type_name);
      ptr_name_node = get_identifier (ptr_name);
      d = start_decl (ptr_name_node, tree_cons (NULL_TREE, ptr,
                        build_tree_list (NULL_TREE, type_id)),
                      0, NULL_TREE, NULL_TREE);
      finish_decl (d, NULL_TREE, NULL_TREE);
      current_type_list = chainon (current_type_list,
                                   build_tree_list (NULL_TREE, ptr_name_node ));
    }
}

/* Make a copy of the object type's name to TYPE_LANG_SPECIFIC.
 * The name is needed when a method is called and may vanish
 * in expressions where a TYPE is replaced by its TYPE_MAIN_VARIANT.
 */
void
store_object_name (obj)
     tree obj;
{
  TYPE_LANG_SPECIFIC (obj) = allocate_type_lang_specific ();
  TYPE_LANG_SPECIFIC (obj)->elts [0] = current_type_name;
  TYPE_LANG_SPECIFIC (obj)->elts [1] = NULL_TREE;  /* base type */
  TYPE_LANG_SPECIFIC (obj)->len = -2;
}

/* Perform inheritance between two already complete object structures */
void
inherit (dest, parent)
     tree dest, parent;
{
  tree dest_fields = TYPE_FIELDS (dest);
  tree parent_fields = copy_list (TYPE_FIELDS (parent));
  register tree df, pf, df0, pf0;
  /* Handle overwriting of methods */
  for (pf0 = NULL_TREE, pf = parent_fields;
       pf; pf0 = pf, pf = TREE_CHAIN (pf))
    {
      for (df0 = NULL_TREE, df = dest_fields;
           df; df0 = df, df = TREE_CHAIN (df))
        if (DECL_NAME (df) == DECL_NAME (pf))
          break;
      if (df)
        if (TREE_CODE (df) == FUNCTION_DECL)
          if (TREE_CODE (pf) == FUNCTION_DECL)
            {
              if (PASCAL_VIRTUAL_METHOD (pf)
                  && ! PASCAL_VIRTUAL_METHOD (df))
                {
                  /* Overridden virtual methods must be virtual. */
                  warning ("method `%s' is virtual",
                           IDENTIFIER_POINTER (DECL_NAME (df)));
                  PASCAL_VIRTUAL_METHOD (df) = 1;
                }
              /* Replace the parent's method with the child's one
               */
              if (df0)
                TREE_CHAIN (df0) = TREE_CHAIN (df);
              else
                dest_fields = TREE_CHAIN (df);
              if (pf0)
                TREE_CHAIN (pf0) = df;
              else
                parent_fields = df;
              TREE_CHAIN (df) = TREE_CHAIN (pf);
              pf = df;
            }
          else
            error ("method `%s' conflicts with data field of parent object",
                   IDENTIFIER_POINTER (DECL_NAME (df)));
        else if (TREE_CODE (pf) == FUNCTION_DECL)
          error ("data field `%s' conflicts with method of parent object",
                 IDENTIFIER_POINTER (DECL_NAME (df)));
        else
          error ("cannot overwrite data field `%s' of parent object",
                 IDENTIFIER_POINTER (DECL_NAME (df)));
    }
  TYPE_FIELDS (dest) = chainon (parent_fields, dest_fields);
  TYPE_SIZE (dest) = 0;
  TYPE_LANG_SPECIFIC (dest)->elts [1] = parent;
  layout_type (dest);
}

/* add an implicit VMT field to an already complete object DEST */ 
void
add_vmt_field (dest)
     tree dest;
{
  tree vmt_pointer_type = build_pointer_type (void_type_node);
  tree vmt_field;
  vmt_field = build_decl (FIELD_DECL, get_identifier ("vmt"),
                          vmt_pointer_type);
  TYPE_FIELDS (dest) = chainon (vmt_field, TYPE_FIELDS (dest));
  TYPE_SIZE (dest) = 0;
  layout_type (dest);
}

/* Construct a virtual method table (VMT) for object DEST */
void
construct_vmt (dest)
     tree dest;
{
  tree object_name_node = TYPE_LANG_SPECIFIC (dest)->elts [0];
  char *object_name = IDENTIFIER_POINTER (object_name_node);
  /* allocate enough space to avoid alignment problems with i386 */
  char *vmt_name = oballoc (IDENTIFIER_LENGTH (object_name_node) + 9);
  char *method_name;
  tree vmt_entry, vmt_type, vmt_init, field, field_name;
  tree vmt_var;
  int mom;

  /* Create a unique name for the VMT
   */
  sprintf (vmt_name, "vmt_%s", object_name);

#if 0
  mom = suspend_momentary ();  /* what does this mean? */
#endif

  /* Allocate all required fields.
   * First field: size of the object (integer)
   */
  vmt_entry = p_grokfields (build_tree_list (NULL_TREE, 
                              get_identifier ("object_size")),
                            integer_type_node);

  /* Second field: bit-complement of the size of the object 
   * (integer; for future runtime checks)
   */
  vmt_entry = chainon (vmt_entry,
                p_grokfields (build_tree_list (NULL_TREE, 
                                get_identifier ("neg_object_size")),
                              integer_type_node));

  /* Create a record type for the VMT.
   * The fields will contain pointers to all virtual methods.
   */
  for (field = TYPE_FIELDS (dest); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == FUNCTION_DECL
          && PASCAL_VIRTUAL_METHOD (field))
        {
          vmt_type = build_pointer_type (TREE_TYPE (field));
          vmt_entry = chainon (vmt_entry,
                        p_grokfields (build_tree_list (NULL_TREE, 
                                        DECL_NAME (field)),
                                      vmt_type));
        }
    }

#if 0
  resume_momentary (mom);
#endif

  vmt_type = start_struct (RECORD_TYPE, NULL_TREE);
  vmt_type = finish_struct (vmt_type, vmt_entry, NULL_TREE);

  /* Build an initializer for the VMT record
   */
  vmt_entry = build_tree_list (NULL_TREE, size_in_bytes (dest));
  vmt_entry = chainon (vmt_entry,
              build_tree_list (NULL_TREE,
                build_pascal_unary_op (BIT_NOT_EXPR, size_in_bytes (dest), 0)));
  for (field = TYPE_FIELDS (dest); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == FUNCTION_DECL
          && PASCAL_VIRTUAL_METHOD (field))
        {
          tree method_name, method, dest2 = dest;
          do
            {
              object_name_node = TYPE_LANG_SPECIFIC (dest2)->elts [0];
              method_name = get_method_name (object_name_node, 
                                             DECL_NAME (field));
              method = lookup_name (method_name);
              dest2 = TYPE_LANG_SPECIFIC (dest2)->elts [1];
            }
          while (!method && dest2);
          if (method)
            method = default_conversion (method);
          else
            error ("Internal GPC error: method not found while constructing VMT");
          vmt_entry = chainon (vmt_entry, build_tree_list (NULL_TREE, method)); 
        } 
    }
  vmt_init = build_pascal_initializer (build_tree_list (NULL_TREE, vmt_type),
                                       vmt_entry);

  /* Now create a global var declaration.
   */
  declare_vars (build_tree_list (NULL_TREE, get_identifier (vmt_name)),
                vmt_type, vmt_init,
                this_is_an_interface_module ? NULL_TREE :
                  build_tree_list (NULL_TREE, static_id),
                this_is_an_interface_module);

#if 0
  /* I had to do this manually rather than using vmt_init above
   * in 2.6.3.  Has this problem vanished in 2.7.2? -- PG
   */
  DECL_INITIAL (lookup_name (get_identifier (vmt_name))) = vmt_init;
#endif

  /* Attach VMT_TYPE to the implicit VMT field of the object
   * (In the moment it still has the inherited type or ^void type.)
   */
  field = simple_get_field (get_identifier ("vmt"), dest,
                            "Internal GPC error: no implicit VMT field");
  if (field && TREE_CODE (TREE_TYPE (field)) == POINTER_TYPE)
    TREE_TYPE (field) = build_pointer_type (vmt_type);
}

/* Mark the last field in the list DEST as a virtual method
 * and return the list.  If the last field is a constructor
 * or a data field, complain.
 */
tree
mark_virtual_method (dest)
     tree dest;
{
  register tree field = dest;
  while (TREE_CHAIN (field))
    field = TREE_CHAIN (field);
  if (TREE_CODE (field) == FUNCTION_DECL)
    if (PASCAL_CONSTRUCTOR_METHOD (field) == 0)
      PASCAL_VIRTUAL_METHOD (field) = 1;
    else
      error ("constructors must not be virtual");
  else
    error ("cannot make data fields virtual");
  return dest;
}

/*
 * Called from gpc-parse.y and gpc-decl.c (shadow_record_fields)
 */
int undeclared_variable_notice = 0; /* 1 if explained undeclared var errors. */

tree
check_identifier (decl, id)
tree decl;
tree id;
{
    /* First: Special case for WITH-shadowed names.
       Environment: We have a with statement inside which we find a
       name. That name may be shadowed by a component reference to the
       fields of the record variable, i.e. ID is an identifier
       whose IDENTIFIER_LOCAL_VALUE is a COMPONENT_REF
       and DECL is also that (found by lookup_name before).

       I admit this is an ugly way. Maybe someone cleans up this.
     */

  /* @@@ Should we mark the record variable TREE_USED? */
    if (decl && TREE_CODE (decl) == COMPONENT_REF &&
	id   && IDENTIFIER_LOCAL_VALUE (id) &&
	TREE_CODE (IDENTIFIER_LOCAL_VALUE (id)) == COMPONENT_REF)
      return decl;

    if (! decl || decl == error_mark_node) {
	if (IDENTIFIER_GLOBAL_VALUE (id) != error_mark_node
	    || IDENTIFIER_ERROR_LOCUS (id) != current_function_decl)
	    error ("undeclared identifier `%s' (first use this function)",
		   IDENTIFIER_POINTER (id));
	if (! undeclared_variable_notice)
	  {
	      error ("(Each undeclared identifier is reported only once");
	      error ("for each function it appears in.)");
	      undeclared_variable_notice = 1;
	  }
	/* Prevent repeated error messages */
	IDENTIFIER_GLOBAL_VALUE (id) = error_mark_node;
	IDENTIFIER_ERROR_LOCUS (id) = current_function_decl;
	return error_mark_node;
    } else if (! TREE_USED (decl))
      {
	if (DECL_EXTERNAL (decl))
	    assemble_external (decl);
	TREE_USED (decl) = 1;
      }

    /* Set TREE_NONLOCAL if var is inherited in nested fcn.  */
    if (DECL_CONTEXT (decl) != 0
	&& DECL_CONTEXT (decl) != current_function_decl)
      {
	DECL_NONLOCAL (decl) = 1;
	mark_addressable (decl);
      }

    /* @@@@ Check if the copy_node is really necessary here! */
    if (TREE_CODE (decl) == CONST_DECL)
	decl = copy_node (DECL_INITIAL (decl)); /* User defined constant */

    return decl;
}

/*
 * Return 1 if the type of the node STRING is a character array node
 *  or it's a string constant, or it's a string schema.
 * Return 2 if it is a valid conformant array whose low bound should
 *  be checked at runtime.
 *
 * Return 0 if we know it's not a valid string.
 */
int
is_string_type (string)
tree string;
{
  if (TREE_CODE (string) == STRING_CST)
    return 1;

  return is_of_string_type (TREE_TYPE (string));
}

int
is_of_string_type (type)
     tree type;
{
  if (PASCAL_TYPE_STRING (type))
    return 1;

  if (TREE_CODE (type) != ARRAY_TYPE
      || TREE_TYPE (type) != char_type_node
      || !PASCAL_TYPE_PACKED (type))
    return 0;

  /* @@@ We should verify at runtime that the low index of the
   * conformant array parameter is 1
   */
  if (C_TYPE_VARIABLE_SIZE (type))
    return 2;

  /* String type low index must be one */
  return tree_int_cst_equal (TYPE_MIN_VALUE (TYPE_DOMAIN (type)),
			     integer_one_node);
}

int
is_variable_string_type (type)
     tree type;
{
  return (TREE_CODE (type) == REFERENCE_TYPE && PASCAL_TYPE_STRING (TREE_TYPE (type))
    	  || PASCAL_TYPE_STRING (type));
}


/*
 * Initialize misc. flags used in here.
 * size_of_file_type is also used in gpc-decl.c (when making TEXT file)
 */
init_util ()
{
  int i;

  size_of_file_type = size_int (FDR_Size * BITS_PER_UNIT);
  
  /* set the default integer set size, if not set with -fsetlimit:NNNNN option
   */
  integer_set_size = requested_set_size > 0
    			? size_int (requested_set_size)
			: size_int (DEFAULT_SET_SIZE);
  
  /* On integer sets of unknown size (i.e. constructors with integers in them)
   * we use the following default set
   */
  integer_set_type_node =
    build_set_type
         (build_tree_list
	       (integer_type_node,
		INT_RANGE_TYPE (integer_zero_node,
				TREE_INT_CST_LOW (integer_set_size) - 1)));

  /* Create symbol refs for Pascal run time routines */
  for (i = 0; rts[i].name; i++)
    rts[i].symref = gen_rtx (SYMBOL_REF, Pmode, rts[i].name);
}


/* Get an "identifier" that starts with a BLANK character. */
tree
get_identifier_with_blank(str)
char *str;
{
  tree result;
  int len = strlen (str);
  char *newstr = (char *) xmalloc (len + 2);

  *newstr = ' ';
  strcpy (newstr + 1, str);
  *(newstr + len + 1) = '\0';
  result = get_identifier (newstr);
  free (newstr);
  return result;
}


/*
 * Construct the following function
 * (This method also makes gcc2 call any special main-kludges, like __main())
 *
 *   void
 *   main(int argc, char **argv, char **envp)
 *   {
 *	_p_initialize (argc, argv, envp);
 *	program_name ();
 *      _p_finalize ();          <-- This calls exit(0);
 *   }
 *
 */

void
output_real_main_program (name)
     tree name;
{
  tree parameters;
  tree declspecs;
  tree declarator;
  tree fun;
  tree param_list;
  tree argc = get_identifier ("argc");
  tree argv = get_identifier ("argv");
  tree envp = get_identifier ("envp");
  tree init_id = get_identifier ("_p_initialize");

  /* This is basically what implicitly_declare() does
   * but does not give warnings...
   */

  push_obstacks_nochange ();
  end_temporary_allocation ();

  fun = build_decl (FUNCTION_DECL, init_id, default_function_type);

  DECL_EXTERNAL (fun) = 1;
  TREE_PUBLIC (fun) = 1;

  /* Record that we have an implicit decl and this is it.  */
  IDENTIFIER_IMPLICIT_DECL (init_id) = fun;

  pushdecl (fun);

  rest_of_decl_compilation (fun, NULL_PTR, 0, 0);

  pop_obstacks ();

  assemble_external (fun);

  /* Construct implicit main program that is invocated as
   *   name(int argc, char **argv, char**envp)
   */
  pushlevel (0);
  clear_parm_order ();
  declare_parm_level (1);

  push_parm_decl (build_tree_list 
		  (build_tree_list (build_tree_list (NULL_TREE,
						     integer_type_node),
				    argc),
		   build_tree_list (NULL_TREE, NULL_TREE)));

  push_parm_decl (build_tree_list
		  (build_tree_list (build_tree_list (NULL_TREE,
						     ptr_type_node),
				    argv),
		   build_tree_list (NULL_TREE, NULL_TREE)));

  push_parm_decl (build_tree_list
		  (build_tree_list (build_tree_list (NULL_TREE,
						     ptr_type_node),
				    envp),
		   build_tree_list (NULL_TREE, NULL_TREE)));

  parameters = get_parm_info (1);
  poplevel (0, 0, 0);

  declspecs = tree_cons (NULL_TREE, void_type_node, NULL_TREE);

  declarator = build_nt (CALL_EXPR, get_identifier (gpc_main), parameters,
			 NULL_TREE);

  if (! start_function (declspecs, declarator, NULL_TREE, NULL_TREE, 0))
    abort ();

  store_parm_decls ();

  pushlevel (0);
  expand_start_bindings (0); 
  pushlevel (0);
  expand_start_bindings (0); 

  param_list = tree_cons (NULL_TREE,
			  lookup_name (argc),
			  tree_cons (NULL_TREE,
				     lookup_name (argv),
				     tree_cons (NULL_TREE,
						lookup_name (envp),
						NULL_TREE)));

  /* Call _p_initialize(argc,argv,envp) */
  expand_expr_stmt (build_function_call (fun, param_list));

  /* Call the Pascal main program. No arguments. */
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, IDENTIFIER_POINTER (name)), 0,
		     VOIDmode, 0);

  /* Call the run time system finalization routine */
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "_p_finalize"), 0,
		     VOIDmode, 0);

  expand_end_bindings (NULL_TREE, 0, 0);
  poplevel (0, 0, 0);
  expand_end_bindings (NULL_TREE, 0, 0);
  poplevel (0, 0, 0);
  
  finish_function (0);
}

tree
no_parameters ()
{
  tree parms;

  pushlevel (0);
  clear_parm_order ();
  declare_parm_level (1);
  parms = get_parm_info (1);
  poplevel (0, 0, 0);

  return parms;
}

/*
 * The standard requires that the program_name is in a separate name
 * space, so we map "Name" to "program_Name".  All other pascal
 * identifiers start with a capital letter and have lowercase otherwise,
 * so clashes cannot occur (except with AsmName).
 * 
 * Return the new NAME.
 */
tree
get_main_program_name (name)
     tree name;
{
  char *newname;
  char *oldname = IDENTIFIER_POINTER (name);

  if (oldname)
    {
      newname = (char *) obstack_alloc (&permanent_obstack,
					strlen (IDENTIFIER_POINTER (name))
                                          + strlen ("program_"));
      strcpy (newname, "program_");
      strcat (newname, oldname);
      name = get_identifier (newname);
    }

  return name;
}

/* This initialization is run once per compilation just when types
 * are declared.
 * 
 * init_util() is too early, so call this from lang_init ().
 * @@@ rs-6000 causes core dumps with -g option when finish_struct() is called
 * there, asm_out_file is uninitialized in routine varasm.c(text_section)
 */
void
initialize_world ()
{
  tree temp;

  declare_rts_types ();

  /* A special node that forces a call through a function pointer that
   * has no arguments.
   */
  empty_arglist = build_tree_list (NULL_TREE, NULL_TREE);

  /* A unique prototype string schema. Used only as a marker. */
  string_schema_proto_type = build_pascal_string_schema (integer_zero_node);

  integer_ptr_type_node = build_pointer_type (integer_type_node);

  /* A canonical-string-type that DATE always returns */
  gpc_type_DATE =grok_packed
    		   (build_pascal_array_type
		    (char_type_node,
		     build_tree_list (NULL_TREE,
				      INT_RANGE_TYPE (integer_one_node,
						      GPC_DATE_LENGTH))));

  /* A canonical-string-type that TIME always returns */
  gpc_type_TIME = grok_packed
    		    (build_pascal_array_type
		     (char_type_node,
		      build_tree_list (NULL_TREE,
				       INT_RANGE_TYPE (integer_one_node,
						       GPC_TIME_LENGTH))));
  
  
  /* Fields of the required RECORD_TYPE TimeStamp */
  temp  =
    chainon
      (DO_FIELD ("Datevalid", boolean_type_node),
       chainon
       (DO_FIELD ("Timevalid", boolean_type_node),
	chainon
	(DO_FIELD ("Year", integer_type_node),
	 chainon
	 (DO_FIELD ("Month", INT_RANGE_TYPE (integer_one_node, 12)),
	  chainon
	  (DO_FIELD ("Day",   INT_RANGE_TYPE (integer_one_node, 31)),
	   chainon
	   (DO_FIELD ("Hour",   INT_RANGE_TYPE (integer_zero_node, 23)),
	    chainon
	    (DO_FIELD ("Minute", INT_RANGE_TYPE (integer_zero_node, 59)),
	     DO_FIELD ("Second", INT_RANGE_TYPE (integer_zero_node, 59)))))))));

  /* Create the packed RECORD_TYPE, we have the fields */
  temp = grok_packed (finish_struct (start_struct (RECORD_TYPE,
						   NULL_TREE),
				     temp, NULL_TREE)); 

  /* Handled like other predefined types in gpc-lex.c */
  gpc_type_TIMESTAMP = temp;

  temp = build_pascal_string_schema (build_int_2 (BINDING_NAME_LENGTH, 0));

  /* Fields of the required RECORD_TYPE BindingType */
  temp  =
    chainon
      (DO_FIELD ("Bound", boolean_type_node),
       chainon
       (DO_FIELD ("Extensions_valid", boolean_type_node),
	chainon
	(DO_FIELD ("Readable", boolean_type_node),
	 chainon
	 (DO_FIELD ("Writable", boolean_type_node),
	  chainon
	  (DO_FIELD ("Existing", boolean_type_node),
	   chainon
	   (DO_FIELD ("Error", integer_type_node),
	    chainon
	    (DO_FIELD ("Size", integer_type_node),
	     DO_FIELD ("Name", temp))))))));

  /* Create the packed RECORD_TYPE, we have the fields */
  temp = grok_packed (finish_struct (start_struct (RECORD_TYPE,
						   NULL_TREE),
				     temp, NULL_TREE)); 

  gpc_type_BINDINGTYPE = temp;

  identifier_output = get_identifier (OUTPUT_FILE_NAME);
  identifier_input  = get_identifier (INPUT_FILE_NAME);

  standard_interface_input  = get_identifier ("Standardinput");
  standard_interface_output = get_identifier ("Standardoutput");

  {
    /* Obtain the input and output files initialized in
     * the run time system
     */
    tree external_text = chainon (build_tree_list (NULL_TREE, text_type_node),
				  build_tree_list (NULL_TREE, extern_id));

    tree iname = get_identifier (RTS_INPUT_FILE_NAME);
    tree oname = get_identifier (RTS_OUTPUT_FILE_NAME);
    tree var;

    var = start_decl (iname, external_text, 0, NULL_TREE, NULL_TREE);
    finish_decl (var, NULL_TREE, NULL_TREE);
    PASCAL_EXTERNAL_OBJECT (var) = 1;
    
    global_input_file_node = var;
    
    var = start_decl (oname, external_text, 0, NULL_TREE, NULL_TREE);
    finish_decl (var, NULL_TREE, NULL_TREE);
    PASCAL_EXTERNAL_OBJECT (var) = 1;
    
    global_output_file_node = var;
  }

  declare_known_ids ();
}


/* Moved here from gpc-lex.c */

/* ISO Pascal does not deny redefinition of predefined type-names
   or routines (e.g. you may define a TYPE INTEGER = CHAR;)
   so the RIDPOINTERS array is not useful in here. */

static void
declare_known_id (name, type, value)
char *name;
tree type;
int value;
{
    tree id = get_identifier (name);
    tree decl;

    if (type) {
	type = build_decl (TYPE_DECL, id, type);
	TYPE_NAME (TREE_TYPE (type)) = type;
    } else {
	/* This prevents core dumps... */
	type = integer_type_node;
    }

    /* It really does not matter which DECL node it is, since
       it has the attribute flag PASCAL_REDEFINABLE_DECL(NODE) set.
       It's not used as a VAR_DECL node anyhow. */
    decl = build_decl (VAR_DECL, id, type);
    PASCAL_REDEFINABLE_DECL (decl) = 1;

    if (value)
      DECL_INITIAL (decl) = build_int_2 (value, 0);

    pushdecl(decl);

    /* Avoid warnings if the standard decls are unused */
    TREE_USED (decl) = 1;
}

/* Pre-declared identifiers; i.e. not type names */
static struct known_id {
    char *name;
    int  value;
} KItable [] = {
    { "Maxint",  p_MAXINT },
    { "False",   p_FALSE },
    { "True",    p_TRUE },
    { "Input",   p_INPUT },
    { "Output",  p_OUTPUT },

    { "Rewrite", p_REWRITE },
    { "Reset", 	 p_RESET },
    { "Put", 	 p_PUT },
    { "Get", 	 p_GET },
    { "Write", 	 p_WRITE },
    { "Read", 	 p_READ },
    { "Writeln", p_WRITELN },
    { "Readln",  p_READLN },
    { "Page", 	 p_PAGE },
    { "New", 	 p_NEW },
    { "Dispose", p_DISPOSE },
    { "Abs", 	 p_ABS },
    { "Sqr", 	 p_SQR },
    { "Sin", 	 p_SIN },
    { "Cos", 	 p_COS },
    { "Exp", 	 p_EXP },
    { "Ln", 	 p_LN },
    { "Sqrt", 	 p_SQRT },
    { "Arctan",  p_ARCTAN },
    { "Trunc", 	 p_TRUNC },
    { "Round", 	 p_ROUND },
    { "Pack", 	 p_PACK },
    { "Unpack",  p_UNPACK },
    { "Ord", 	 p_ORD },
    { "Chr", 	 p_CHR },
    { "Succ", 	 p_SUCC },
    { "Pred", 	 p_PRED },
    { "Odd", 	 p_ODD },
    { "Eof", 	 p_EOF },
    { "Eoln", 	 p_EOLN },

    /* Extended pascal module directives */
    {"Asmname",        D_ASMNAME },
    {"Attribute",      D_ATTRIBUTE },
    {"C",	       D_C },
    {"C_language",     D_C_LANGUAGE},
    {"Forward",	       D_FORWARD },
    {"Extern",	       D_EXTERN },
    {"External",       D_EXTERNAL },

    /* Extended Pascal required module interfaces */
    {"Standardoutput", STANDARD_OUTPUT},
    {"Standardinput",  STANDARD_INPUT},

    /* Object pascal directive */
    {"Override",     D_OVERRIDE},

    /* Extended Pascal required words */
    {"Maxchar",  p_MAXCHAR},
    {"Maxreal",  p_MAXREAL},
    {"Minreal",  p_MINREAL},
    {"Epsreal",  p_EPSREAL},

    /* Extended Pascal required procs & funs */
    {"Gettimestamp",p_GETTIMESTAMP},
    {"Date",	 p_DATE},
    {"Time",	 p_TIME},
    {"Halt",	 p_HALT},
    {"Extend",	 p_EXTEND},
    {"Seekwrite",p_SEEKWRITE},
    {"Seekread", p_SEEKREAD},
    {"Seekupdate",p_SEEKUPDATE},
    {"Empty",	 p_EMPTY},
    {"Update",	 p_UPDATE},
    {"Position", p_POSITION},
    {"Lastposition",p_LASTPOSITION},
    {"Re",	 p_RE},
    {"Im",	 p_IM},
    {"Cmplx",	 p_CMPLX},
    {"Card",	 p_CARD},
    {"Arg",	 p_ARG},

    /* External binding */
    {"Bind",	 p_BIND},
    {"Unbind",	 p_UNBIND},
    {"Binding",	 p_BINDING},

    /* Complex type functions */
    {"Polar",	 p_POLAR},

    /* String functions */
    {"Readstr",	 p_READSTR},
    {"Writestr", p_WRITESTR},
    {"Length",	 p_LENGTH},
    {"Index",	 p_INDEX},
    {"Substr",	 p_SUBSTR},
    {"Trim",	 p_TRIM},
    {"Eq",	 p_EQ},
    {"Lt",	 p_LT},
    {"Gt",	 p_GT},
    {"Ne",	 p_NE},
    {"Le",	 p_LE},
    {"Ge",	 p_GE},

    /* Extended pascal required string schema type generator */
    {"String",   STRING_SCHEMA},

    /* Object pascal */
    {"Copy",         op_COPY },
    {"Null",         op_NULL },
    {"Root",	     op_ROOT },
    {"Textwritable", op_TEXTWRITABLE },
    {"Self",	     op_SELF },

    /* Borland pascal */
    { "Getmem",  bp_GETMEM },
    { "Freemem", bp_FREEMEM },
    { "Inc", bp_INC },
    { "Dec", bp_DEC },
    { "Shl", BP_SHL },
    { "Shr", BP_SHR },
    { "Xor", BP_XOR },

    /* More exotic fruits and birds (gpc extensions) */
    { "Static",	      D_STATIC },
    { "__const__",    G_CONST }, /* Storage class qualifiers */
    { "__external__", EXTERNAL },
    { "__asmname__",  ASMNAME },
    { "__inline__",   INLINE },
    { "__static__",   STATIC },
    { "__volatile__", VOLATILE },
    { "__byte__",     TQ_BYTE }, /* Type qualifiers */
    { "__short__",    TQ_SHORT },
    { "__long__",     TQ_LONG }, 
    { "__longlong__", TQ_LONGLONG },
    { "__unsigned__", TQ_UNSIGNED },

    { "Asm", 	 ASM },
    { "Alignof", ALIGNOF },
    { "Break", 	 BREAK },
    { "Continue",CONTINUE },
    { "Return",  RETURN },
    { "Sizeof",  SIZEOF },
    { "Max",	 gpc_MAX },
    { "Min",	 gpc_MIN },

    { "Conjugate",   CONJUGATE }, /* Complex conjugate */
    { "Mark", 	 p_MARK },	/* mark the heap state */
    { "Release", p_RELEASE },	/* release heap to last mark */

    { "Default", DEFAULT },	/* Case defaults */
    { "Others",  OTHERS },

    { "Close",   p_CLOSE },	/* Close a file */
    { "Definesize", p_DEFINESIZE }, /* random access file size definition */

    { NULL,      0 }
};

/* called from initialize_world() after type nodes have been initialized */
declare_known_ids ()
{
    struct known_id *kptr;

    /* pre-declared type names */
    declare_known_id ("Integer", integer_type_node, 0);
    declare_known_id ("Real",    double_type_node,  0);
    declare_known_id ("Boolean", boolean_type_node, 0);
    declare_known_id ("Char",    char_type_node,    0);
    declare_known_id ("Text",    text_type_node,    0);
    declare_known_id ("Complex", complex_type_node, 0);

    /* GPC Extensions: void type (two spellings) */
    declare_known_id ("__void__", void_type_node,0);
    declare_known_id ("Void",     void_type_node,0);

    /* GPC: Extension: Parameter is a C language string type
     * Causes a string-type parameter to be passed as a pointer even
     * if it is a value parameter; also a string-schema value parameter
     * passes the address of the character array, not the address of the
     * schema object.
     *
     * This is a kludge.
     */
    cstring_type_node = build_type_copy (string_type_node);
    declare_known_id ("__cstring__", cstring_type_node, 0);
    
    /* Extended Pascal: TimeStamp and BindingType */
    declare_known_id ("Timestamp",   gpc_type_TIMESTAMP,   0);
    declare_known_id ("Bindingtype", gpc_type_BINDINGTYPE, 0);

    /* pre-declared identifiers */
    for (kptr = KItable; kptr->name; kptr++)
	declare_known_id (kptr->name, NULL_TREE, kptr->value);
}

void
init_constructor (fun, runid)
     tree fun;
     tree runid;
{
  rtx rfun;

  if (! fun || TREE_CODE (fun) != FUNCTION_DECL)
    abort ();

  /* Do this only if we have module constructors */
  if (collect_constructors_now == NULL_TREE)
    {
      /* Create an external reference to the collect flag */
      tree type = chainon (build_tree_list (NULL_TREE, integer_type_node),
			   build_tree_list (NULL_TREE, extern_id));
      tree d = start_decl (get_identifier (RTS_COLLECT_FLAG),
			   type, 0, NULL_TREE, NULL_TREE);
      finish_decl (d, NULL_TREE, NULL_TREE);

      collect_constructors_now = d;
    }

  /* If the external tree node collect_constructors_now is nonzero
     at runtime, call a library routine that collects constructors.

     The run time system will call a routine that really runs them.
   */
  expand_start_cond (collect_constructors_now, 0);
  expand_expr_stmt
    (rts_call (r_COLLECT, void_type_node,
	       NULL_TREE, chainon (build_tree_list (NULL_TREE,
						    default_conversion (fun)),
				   build_tree_list (NULL_TREE,
						    runid))));
  expand_null_return();
  expand_end_cond ();
}

/* Return a newly-allocated string whose contents concatenate
   those of s1, s2, s3.  */

char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = (char *) xmalloc (len1 + len2 + len3 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  *(result + len1 + len2 + len3) = 0;

  return result;
}

/* Return a newly allocated copy of the string s
 */
char *
save_string (s)
     char *s;
{
  char *result = xmalloc (strlen (s) + 1);
  strcpy (result, s);
  return result;
}

/*
 * For Pascal initialized variables and constants:
 * create an initalizer for an object of type TYPE
 * out of the tree-list INIT.
 * The TREE_VALUEs are the initializers, while the
 * TREE_PURPOSEs are array indices or field names.
 *
 * @@@@@@@@@@ Currently, the TREE_PURPOSEs are
 * ignored completely!  Write any nonsense, and it
 * will be accepted by the compiler!
 */
tree
build_pascal_initializer (type, init)
     tree type, init;
{
  if (!init || init == error_mark_node)
    return error_mark_node;
  if (TREE_CHAIN (init))
    {
      tree link, field;
      type = TREE_VALUE (type);
      if (TREE_CODE (type) == ARRAY_TYPE)
        for (link = TREE_VALUE (init); link; link = TREE_CHAIN (link))
          TREE_VALUE (link) = build_pascal_initializer (TREE_TYPE (type),
                                                        TREE_VALUE (link));
      else if (TREE_CODE (type) == RECORD_TYPE)
        for (link =  TREE_VALUE (init), field = TYPE_FIELDS (type);
             link && field;
             link = TREE_CHAIN (link), field = TREE_CHAIN (field))
          TREE_VALUE (link) = build_pascal_initializer (TREE_TYPE (field), 
                                                        TREE_VALUE (link));
      else
        {
          error ("initial value is of wrong type");
          return error_mark_node;
        }
      init = build (CONSTRUCTOR, type, NULL_TREE, init);
      /*
       * @@@@@ Pretend that everything is constant
       * -- and hope it really is!
       */
      TREE_CONSTANT (init) = 1;
      TREE_STATIC (init) = 1;
    }
  else
    init = fold (TREE_VALUE (init));
  return init;
}
