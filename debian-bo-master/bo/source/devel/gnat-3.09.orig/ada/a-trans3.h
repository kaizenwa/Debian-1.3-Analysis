/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             A - T R A N S 3                              */
/*                                                                          */
/*                              C Header File                               */
/*                                                                          */
/*                            $Revision: 1.104 $                             */
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

/* If nonzero, pretend we are allocating at global level.  */
extern int force_global;

/* Standard data type sizes.  Most of these are not used.  */

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * MIN ((UNITS_PER_WORD + 1) / 2, 2))
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

/* The choice of SIZE_TYPE here is very problematic.  We need a signed
   type whose bit width is Pmode.  Assume "long" is such a type here.  */
#undef SIZE_TYPE
#define SIZE_TYPE "long int"

/* Flags added to GCC type nodes.  */

/* For RECORD_TYPE, nonzero if this is a record being used
   as a fat pointer.  */
#define TYPE_IS_FAT_POINTER_P(NODE) TYPE_LANG_FLAG_0 (NODE)

#define TYPE_FAT_POINTER_P(NODE)  \
  (TREE_CODE (NODE) == RECORD_TYPE && TYPE_IS_FAT_POINTER_P (NODE))

/* For integral types, nonzero if this is a packed array type.  Such
   types should not be extended to a larger size.  */
#define TYPE_PACKED_ARRAY_TYPE_P(NODE) TYPE_LANG_FLAG_0 (NODE)

/* For INTEGER_TYPE, nonzero if this is a modular type with a modulus that
   is not equal to two to the power of its mode's size.  */
#define TYPE_MODULAR_P(NODE) TYPE_LANG_FLAG_1 (NODE)

/* For ARRAY_TYPE, nonzero if this type corresponds to a dimension of
   an Ada array other than the first.  */
#define TYPE_MULTI_ARRAY_P(NODE)  TYPE_LANG_FLAG_1 (NODE)

/* For FUNCTION_TYPE, nonzero if this denotes a function returning an
   unconstrained array or record.  */
#define TYPE_RETURNS_UNCONSTRAINED_P(NODE) TYPE_LANG_FLAG_1 (NODE)

/* For RECORD_TYPE, nonzero if this denotes a left-justified modular type.  */
#define TYPE_LEFT_JUSTIFIED_MODULAR_P(NODE) TYPE_LANG_FLAG_1 (NODE)

/* Nonzero in an arithmetic subtype if this is a subtype not known to the
   front-end.  */
#define TYPE_EXTRA_SUBTYPE_P(NODE) TYPE_LANG_FLAG_2 (NODE)

/* Nonzero for composite types if this is a by-reference type.  */
#define TYPE_BY_REFERENCE_P(NODE) TYPE_LANG_FLAG_2 (NODE)

/* For RECORD_TYPE, nonzero if this is the type for an object whose type
   includes its template in addition to its value.  */
#define TYPE_CONTAINS_TEMPLATE_P(NODE) TYPE_LANG_FLAG_3 (NODE)

/* True if NODE is a thin pointer.  */
#define TYPE_THIN_POINTER_P(NODE)			\
  (TREE_CODE (NODE) == POINTER_TYPE			\
   && TREE_CODE (TREE_TYPE (NODE)) == RECORD_TYPE	\
   && TYPE_CONTAINS_TEMPLATE_P (TREE_TYPE (NODE)))

/* True if TYPE is either a fat or thing pointer to an unconstrained
   array.  */
#define TYPE_FAT_OR_THIN_POINTER_P(NODE) \
  (TYPE_FAT_POINTER_P (NODE) || TYPE_THIN_POINTER_P (NODE))

/* For INTEGER_TYPEs, nonzero if the type has a biased representation.  */
#define TYPE_BIASED_REPRESENTATION_P(NODE) TYPE_LANG_FLAG_4 (NODE)

/* For ARRAY_TYPEs, nonzero if the array type has Convention_Fortran.  */
#define TYPE_CONVENTION_FORTRAN_P(NODE) TYPE_LANG_FLAG_4 (NODE)

/* For FUNCTION_TYPEs, nonzero if the function returns by reference.  */
#define TYPE_RETURNS_BY_REF_P(NODE) TYPE_LANG_FLAG_4 (NODE)

/* For VOID_TYPE, UNION_TYPE, and RECORD_TYPE, nonzero if this is a dummy
   type, made to correspond to a private or incomplete type.  */
#define TYPE_DUMMY_P(NODE) TYPE_LANG_FLAG_4 (NODE)

/* True if TYPE is such a dummy type.  */
#define TYPE_IS_DUMMY_P(NODE) \
  ((TREE_CODE (NODE) == VOID_TYPE || TREE_CODE (NODE) == RECORD_TYPE	\
    || TREE_CODE (NODE) == UNION_TYPE)					\
   && TYPE_DUMMY_P (NODE))

/* Nonzero if this corresponds to a type where alignment is guaranteed
   by other mechanisms (a tagged or packed type).  */
#define TYPE_ALIGN_OK_P(NODE) TYPE_LANG_FLAG_5 (NODE)

/* For an INTEGER_TYPE, nonzero if TYPE_ACTUAL_BOUNDS is present.  */
#define TYPE_HAS_ACTUAL_BOUNDS_P(NODE) TYPE_LANG_FLAG_6 (NODE)

/* For a RECORD_TYPE, nonzero if this was made just to supply needed
   padding or alignment.  */
#define TYPE_IS_PADDING_P(NODE) TYPE_LANG_FLAG_6 (NODE)

/* This field is only defined for FUNCTION_TYPE nodes. If the Ada
   subprogram contains no parameters passed by copy in/copy out then this
   field is 0. Otherwise it points to a list of nodes used to specify the
   return values of the out (or in out) parameters that qualify to be passed
   by copy in copy out.  It is a CONSTRUCTOR.  For a full description of the
   cico parameter passing mechanism refer to the routine gnat_to_gnu_entity. */
#define TYPE_CI_CO_LIST(NODE)   (tree) TYPE_LANG_SPECIFIC (NODE)

/* For an INTEGER_TYPE with TYPE_MODULAR_P, this is the value of the
   modulus. */
#define TYPE_MODULUS(NODE)	(tree) TYPE_LANG_SPECIFIC (NODE)

/* For an INTEGER_TYPE that is the TYPE_DOMAIN of some ARRAY_TYPE, points to
   the type corresponding to the Ada index type.  */
#define TYPE_INDEX_TYPE(NODE)	(tree) TYPE_LANG_SPECIFIC (NODE)

/* For INTEGER_TYPE, stores the RM_Size of the type.  */
#define TYPE_RM_SIZE_INT(NODE)	TYPE_VALUES (NODE)

/* Likewise for ENUMERAL_TYPE.  */
#define TYPE_RM_SIZE_ENUM(NODE)	(tree) TYPE_LANG_SPECIFIC (NODE)

#define TYPE_RM_SIZE(NODE)					\
  (TREE_CODE (NODE) == ENUMERAL_TYPE ? TYPE_RM_SIZE_ENUM (NODE)	\
   : TREE_CODE (NODE) == INTEGER_TYPE ? TYPE_RM_SIZE_INT (NODE)	\
   : 0)

/* For a RECORD_TYPE that is a fat pointer, point to the type for the
   unconstrained object.  Likewise for a RECORD_TYPE that is pointed
   to by a thin pointer.  */
#define TYPE_UNCONSTRAINED_ARRAY(NODE)  (tree) TYPE_LANG_SPECIFIC (NODE)

/* For other RECORD_TYPEs and all UNION_TYPEs and QUAL_UNION_TYPEs, the Ada
   size of the object.  This differs from the GCC size in that it does not
   include any rounding up to the alignment of the type.  */
#define TYPE_ADA_SIZE(NODE)	(tree) TYPE_LANG_SPECIFIC (NODE)

/* For an INTEGER_TYPE with TYPE_HAS_ACTUAL_BOUNDS_P or an ARRAY_TYPE, this is
   the index type that should be used when the actual bounds are required for
   a template.  This is used in the case of packed arrays.  */
#define TYPE_ACTUAL_BOUNDS(NODE)   (tree) TYPE_LANG_SPECIFIC (NODE)

/* In an UNCONSTRAINED_ARRAY_TYPE, points to the record containing both
   the template and object.  */
#define TYPE_OBJECT_RECORD_TYPE(NODE) TYPE_MIN_VALUE (NODE)

/* Nonzero in a FIELD_DECL that represents a discriminant.  */
#define DECL_DISCRIMINANT_P(NODE) DECL_LANG_FLAG_0 (NODE)

/* Nonzero if this decl is always used by reference; i.e., an INDIRECT_REF
   is needed to access the object.  */
#define DECL_BY_REF_P(NODE) DECL_LANG_FLAG_1 (NODE)

/* Nonzero if this decl is a PARM_DECL for an Ada array being passed to a
   foreign convention subprogram.  */
#define DECL_BY_COMPONENT_PTR_P(NODE) DECL_LANG_FLAG_2 (NODE)

/* Nonzero in a FIELD_DECL that is a dummy built for some internal reason.  */
#define DECL_INTERNAL_P(NODE) DECL_LANG_FLAG_3 (NODE)

/* Nonzero if this is a decl for a pointer that points to something which
   is readonly.  Used mostly for fat pointers.  */
#define DECL_POINTS_TO_READONLY_P(NODE) DECL_LANG_FLAG_4 (NODE)

/* Nonzero in a FIELD_DECL if there was a record rep clause.  */
#define DECL_HAS_REP_P(NODE) DECL_LANG_FLAG_5 (NODE)

/* Nonzero in a PARM_DECL if we are to pass by descriptor.  */
#define DECL_BY_DESCRIPTOR_P(NODE) DECL_LANG_FLAG_5 (NODE)

/* Nonzero in a TRANSFORM_EXPR if we are to take its address.  */
#define TREE_TRANSFORM_ADDR(NODE) TREE_LANG_FLAG_0 (NODE)

/* Data structures used to represent attributes.  */

enum attr_type {ATTR_MACHINE_ATTRIBUTE, ATTR_LINK_ALIAS, 
		ATTR_LINK_SECTION, ATTR_WEAK_EXTERNAL};

struct attrib
{
  struct attrib *next;
  enum attr_type type;
  tree name;
  tree arg;
  Node_Id error_point;
};

/* Variables expected by the GCC back-end.  */

/* A node which has tree code ERROR_MARK, and whose type is itself.  */
extern tree error_mark_node;

/* Various standard data types and nodes.  */

extern tree integer_type_node;
extern tree unsigned_type_node;
extern tree char_type_node;
extern tree longest_float_type_node;
extern tree void_type_node;
extern tree void_type_decl_node;
extern tree integer_zero_node;
extern tree integer_one_node;
extern tree null_pointer_node;

/* The type of an exception.  */
extern tree except_type_node;

/* The FUNCTION_DECL node for the function currently being compiled, or 0
   if between functions.  */
extern tree current_function_decl;

/* Variables created for the sole tree translator sake. Their names and
   types can be changed as desired.  */

/* type declaration node  <==> typedef void *T */
extern tree ptr_void_type_node;

/* function type declaration -- void T() */
extern tree void_ftype;

/* type declaration node  <==> typedef void *T() */
extern tree ptr_void_ftype;

/* A function declaration node for a run-time function for allocating memory.
   Ada allocators cause calls to this function to be generated.   */
extern tree malloc_decl;

/* Likewise for freeing memory.  */
extern tree free_decl;

/* Likewise for setting the current exception occurrence */
extern tree set_except_occ_decl;

/* Types and decls used by our temporary exception mechanism.  See
   init_decl_processing for details.  */
extern tree jmpbuf_type;
extern tree jmpbuf_ptr_type;
extern tree get_jmpbuf_decl;
extern tree set_jmpbuf_decl;
extern tree get_excptr_decl;
extern tree raise_decl;
extern tree raise_with_msg_decl;
extern tree raise_nodefer_decl;
extern tree setjmp_decl;
extern tree longjmp_decl;
extern tree raise_constraint_error_decl;
extern tree raise_program_error_decl;

/* Routines expected by the gcc back-end. They must have exactly the same
   prototype and names as below.  */

/* Returns non-zero if we are currently in the global binding level       */
extern int global_bindings_p	PROTO((void));

/* Returns the list of declarations in the current level. Note that this list
   is in reverse order (it has to be so for back-end compatibility).  */
extern tree getdecls			PROTO((void));

/* Nonzero if the current level needs to have a BLOCK made.  */
extern int kept_level_p 		PROTO((void));

/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */
extern void pushlevel			PROTO((int));

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
extern tree poplevel		PROTO((int,int, int));

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */
extern void insert_block		PROTO((tree));

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */
extern void set_block			PROTO((tree));

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */
extern tree pushdecl			PROTO((tree));

/* Create the predefined scalar types such as `integer_type_node' needed 
   in the gcc back-end and initialize the global binding level.  */
extern void init_decl_processing	PROTO((void));

/* Return an integer type with the number of bits of precision given by  
   PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   it is a signed type.  */
extern tree type_for_size		PROTO((unsigned, int));

/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */
extern tree type_for_mode		PROTO((enum machine_mode, int));

/* Return the unsigned version of a TYPE_NODE, a scalar type.  */
extern tree unsigned_type		PROTO((tree));

/* Return the signed version of a TYPE_NODE, a scalar type.  */
extern tree signed_type			PROTO((tree));

/* Return a type the same as TYPE except unsigned or signed according to
   UNSIGNEDP.  */
extern tree signed_or_unsigned_type	PROTO((int, tree));

/* This routine is called in tree.c to print an error message for invalid use
   of an incomplete type.  */
extern void incomplete_type_error	PROTO((tree, tree));

/* This function is called indirectly from toplev.c to handle incomplete 
   declarations, i.e. VAR_DECL nodes whose DECL_SIZE is zero.  To be precise,
   compile_file in toplev.c makes an indirect call through the function pointer
   incomplete_decl_finalize_hook which is initialized to this routine in
   init_decl_processing.  */
extern void finish_incomplete_decl	PROTO((tree));

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */
extern tree convert			PROTO((tree, tree));

/* Routines created solely for the tree translator's sake. Their prototypes
   can be changed as desired.  */

/* GNAT_ENTITY is a GNAT tree node for a defining identifier.
   GNU_DECL is the GCC tree which is to be associated with
   GNAT_ENTITY. Such gnu tree node is always an ..._DECL node.
   If NO_CHECK is nonzero, the latter check is suppressed. 
   If GNU_DECL is zero, a previous association is to be reset.  */
extern void save_gnu_tree		PROTO((Entity_Id, tree, int));

/* GNAT_ENTITY is a GNAT tree node for a defining identifier.
   Return the ..._DECL node that was associated with it.  If there is no tree
   node associated with GNAT_ENTITY, abort.  */
extern tree get_gnu_tree		PROTO((Entity_Id));

/* Return nonzero if a GCC tree has been associated with GNAT_ENTITY.  */
extern int present_gnu_tree	PROTO((Entity_Id));

/* Initialize tables for above routines.  */
extern void init_gnat_to_gnu		PROTO((void));

/* Given a record type (RECORD_TYPE) and a chain of FIELD_DECL
   nodes (FIELDLIST), finish constructing the record or union type. 
   If HAS_REP is nonzero, this record has a rep clause; don't call
   layout_type but merely set the size and alignment ourselves.
   If DEFER_DEBUG is nonzero, do not call the debugging routines
   on this type; it will be done later. */
extern void finish_record_type		PROTO((tree, tree, int, int));

/* Returns a FUNCTION_TYPE node. RETURN_TYPE is the type returned by the
   subprogram. If it is void_type_node, then we are dealing with a procedure,
   otherwise we are dealing with a function. PARAM_DECL_LIST is a list of
   PARM_DECL nodes that are the subprogram arguments.  CICO_LIST is the
   copy-in/copy-out list to be stored into TYPE_CI_CO_LIST. 
   RETURNS_UNCONSTRAINED is nonzero if the function returns an unconstrained
   object.  RETURNS_BY_REF is nonzero if the function returns by reference. */
extern tree create_subprog_type		PROTO((tree, tree, tree, int, int));

/* Return a copy of TYPE, in the same obstack as it was, but safe to modify
   in any way.  */
extern tree copy_type			PROTO((tree));

/* Return an INTEGER_TYPE of SIZETYPE with range MIN to MAX and whose
   TYPE_INDEX_TYPE is INDEX.  */
extern tree create_index_type		PROTO((tree, tree, tree));

/* Returns a TYPE_DECL node. TYPE_NAME gives the name of the type and TYPE
   is a ..._TYPE node giving its data type.  */
extern tree create_type_decl		PROTO((tree, tree, struct attrib *));

/* Returns a GCC VAR_DECL node. VAR_NAME gives the name of the variable.
   ASM_NAME is its assembler name (if provided).  TYPE is
   its data type (a GCC ..._TYPE node).  VAR_INIT is the GCC tree for an
   optional initial expression; NULL_TREE if none.

   CONST_FLAG is nonzero if this variable is constant.

   PUBLIC_FLAG is nonzero if this definition is to be made visible outside of
   the current compilation unit. This flag should be set when processing the
   variable definitions in a package specification.  EXTERN_FLAG is nonzero 
   when processing an external variable declaration (as opposed to a
   definition: no storage is to be allocated for the variable here).
   STATIC_FLAG is only relevant when not at top level.  In that case
   it indicates whether to always allocate storage to the variable.  */
extern tree create_var_decl	PROTO((tree, tree, tree, tree, int, int, int,
				       int, struct attrib *));

/* Given a DECL and ATTR_LIST, apply the listed attributes.  */
extern void process_attributes PROTO((tree, struct attrib *));

/* Obtain any pending elaborations and clear the old list.  */
extern tree get_pending_elaborations PROTO((void));

/* Save a copy of the current pending elaboration list and make a new
   one.  */
extern void push_pending_elaborations PROTO((void));

/* Pop the stack of pending elaborations.  */
extern void pop_pending_elaborations PROTO((void));

/* Return the current position in pending_elaborations so we can insert
   elaborations after that point.  */
extern tree get_elaboration_location PROTO((void));

/* Insert the current elaborations after ELAB, which is in some elaboration
   list.  */
extern void insert_elaboration_list PROTO((tree));

/* Add some pending elaborations to the current list.  */
extern void add_pending_elaborations PROTO((tree, tree));

/* Returns a FIELD_DECL node. FIELD_NAME the field name, FIELD_TYPE is its
   type, and RECORD_TYPE is the type of the parent.  PACKED is nonzero if
   this field is in a record type with a "pragma pack".  If SIZE is nonzero
   it is the specified size for this field.  If POS is nonzero, it is the bit
   position.  */
extern tree create_field_decl	PROTO((tree, tree, tree, int, tree, tree));

/* Returns a PARM_DECL node. PARAM_NAME is the name of the parameter,
   PARAM_TYPE is its type.  READONLY is nonzero if the parameter is
   readonly (either an IN parameter or an address of a pass-by-ref
   parameter). */
extern tree create_param_decl	PROTO((tree, tree, int));

/* Returns a FUNCTION_DECL node.  SUBPROG_NAME is the name of the subprogram,
   ASM_NAME is its assembler name, SUBPROG_TYPE is its type (a FUNCTION_TYPE
   node), PARAM_DECL_LIST is the list of the subprogram arguments (a list of
   PARM_DECL nodes chained through the TREE_CHAIN field).

   INLINE_FLAG, PUBLIC_FLAG, and EXTERN_FLAG are used to set the appropriate
   fields in the FUNCTION_DECL.  */
extern tree create_subprog_decl	PROTO((tree, tree, tree, tree,
				       int, int, int, struct attrib *));

/* Returns a LABEL_DECL node for LABEL_NAME.  */
extern tree create_label_decl	PROTO((tree));

/* Set up the framework for generating code for SUBPROG_DECL, a subprogram
   body. This routine needs to be invoked before processing the declarations
   appearing in the subprogram.  */
extern void begin_subprog_body	PROTO((tree));

/* Finish the definition of the current subprogram and compile it all the way
   to assembler language output.  */
extern void end_subprog_body	PROTO((void));

/* Build a template of type TEMPLATE_TYPE from the array bounds of ARRAY_TYPE.
   EXPR is an expression that we can use to locate any PLACEHOLDER_EXPRs.
   Return a constructor for the template.  */
extern tree build_template	PROTO((tree, tree, tree));

/* Build a VMS descriptor from a Mechanism_Type, which must specify
   a descriptor type, and the GCC type of an object.  Each FIELD_DECL
   in the type contains in its DECL_INITIAL the expression to use when
   a constructor is made for the type.  GNAT_ENTITY is a gnat node used
   to print out an error message if the mechanism cannot be applied to
   an object of that type and also for the name.  */

extern tree build_vms_descriptor  PROTO((tree, Mechanism_Type, Entity_Id));

/* Build a type to be used to represent an aliased object whose nominal
   type is an unconstrained array.  This consists of a RECORD_TYPE containing
   a field of TEMPLATE_TYPE and a field of OBJECT_TYPE, which is an
   ARRAY_TYPE.  If ARRAY_TYPE is that of the unconstrained array, this
   is used to represent an arbitrary unconstrained object.  Use NAME
   as the name of the record.  */
extern tree build_unc_object_type  PROTO((tree, tree, tree));

/* Update anything previously pointing to OLD_TYPE to point to NEW_TYPE.  In
   the normal case this is just two adjustments, but we have more to do
   if NEW is an UNCONSTRAINED_ARRAY_TYPE.  */
extern void update_pointer_to	PROTO((tree, tree));

/* EXP is an expression for the size of an object.  If this size contains
   discriminant references, replace them with the maximum (if MAX_P) or
   minimum (if ! MAX_P) possible value of the discriminant.  */
extern tree max_size		PROTO((tree, int));

/* If EXP's type is an UNCONSTRAINED_ARRAY_TYPE, return an expression that
   refers to the underlying array.  If its type has TYPE_CONTAINS_TEMPLATE_P,
   likewise return an expression pointing to the underlying array.  */
extern tree maybe_unconstrained_array PROTO((tree));

/* Return an expression that does an unchecked converstion of EXPR to TYPE.  */
extern tree unchecked_convert	PROTO((tree, tree));
