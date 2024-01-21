/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                              A - T R A N S                               */
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

#define BAD_GNAT_TREE ((Node_Id) -1)

/* This function isn't in sinfo.h since we don't make the setting functions,
   just the retreival functions.  This is the single exception to the rule
   that Gigi doesn't modify the tree.  */
#define Set_Has_No_Elab_Code sinfo__set_has_no_elab_code
extern void Set_Has_No_Elab_Code	PROTO((Node_Id, Boolean));

/* We will be setting Esize and Component_First_Bit for fields.  */
#define Set_Esize einfo__set_esize
extern void Set_Esize			PROTO((Entity_Id, Uint));
#define Set_Component_First_Bit einfo__set_component_first_bit
extern void Set_Component_First_Bit	PROTO((Entity_Id, Uint));

/* These are definitions from exp_code.ads. */
#define Is_Asm_Volatile exp_code__is_asm_volatile
extern Boolean Is_Asm_Volatile	PROTO((Node_Id));

#define Asm_Template exp_code__asm_template
extern Node_Id Asm_Template	PROTO((Node_Id));

#define Clobber_Setup exp_code__clobber_setup
extern void Clobber_Setup	PROTO((Node_Id));

#define Clobber_Get_Next exp_code__clobber_get_next
extern char *Clobber_Get_Next	PROTO((void));

#define Setup_Asm_Inputs exp_code__setup_asm_inputs
extern void Setup_Asm_Inputs	PROTO((Node_Id));

#define Asm_Input_Constraint exp_code__asm_input_constraint
extern Node_Id Asm_Input_Constraint PROTO((void));

#define Asm_Input_Value exp_code__asm_input_value
extern Node_Id Asm_Input_Value	PROTO((void));

#define Next_Asm_Input exp_code__next_asm_input
extern void Next_Asm_Input	PROTO((void));

#define Setup_Asm_Outputs exp_code__setup_asm_outputs
extern void Setup_Asm_Outputs	PROTO((Node_Id));

#define Asm_Output_Constraint exp_code__asm_output_constraint
extern Node_Id Asm_Output_Constraint PROTO((void));

#define Asm_Output_Variable exp_code__asm_output_variable
extern Node_Id Asm_Output_Variable PROTO((void));

#define Next_Asm_Output exp_code__next_asm_output
extern void Next_Asm_Output PROTO((void));

/* List of TREE_LIST nodes representing a block stack.  TREE_VALUE
   of each gives the variable used for the setjmp buffer in the current
   block, if any.  */
extern tree gnu_block_stack;

/* For most front-ends, this is the parser for the language.  For us, we
   process the GNAT tree.  */
extern int yyparse		PROTO((void));

/* This function is the driver of the GNAT to GCC tree transformation process.
   GNAT_NODE is the root of some gnat tree.  It generates code for that
   part of the tree.  */
extern void gnat_to_code	PROTO((Node_Id));

/* GNAT_NODE is the root of some GNAT tree.  Return the root of the
   GCC tree corresponding to that GNAT tree.  Normally, no code is generated;
   we just return an equivalent tree which is used elsewhere to generate
   code.  */
extern tree gnat_to_gnu		PROTO((Node_Id));

/* Do the processing for the declaration of a GNAT_ENTITY, a type.  If
   a separate Freeze node exists, delay the bulk of the processing.  Otherwise
   make a GCC type for GNAT_ENTITY and set up the correspondance.  */

extern void process_type	PROTO((Entity_Id));

/* Determine the input_filename and the lineno from the source location
   (Sloc) of GNAT_NODE node.  Set the global variable input_filename and
   lineno.  If WRITE_NOTE_P is true, emit a line number note. */
extern void set_lineno		PROTO((Node_Id, int));

/* Post an error message.  MSG is the error message, properly annotated.
   NODE is the node at which to post the error and the node to use for the
   "&" substitution.  */
extern void post_error		PROTO((char *, Node_Id));

/* Similar, but NODE is the node at which to post the error and ENT
   is the node to use for the "&" substitution.  */
extern void post_error_ne	PROTO((char *, Node_Id, Entity_Id));

/* Similar, but NODE is the node at which to post the error, ENT is the node
   to use for the "&" substitution, and N is the number to use for the ^.  */
extern void post_error_ne_num	PROTO((char *, Node_Id, Entity_Id, int));

/* Similar to post_error_ne_num, but T is a GCC tree representing the number
   to write.  If the tree represents a constant that fits within a
   host integer, the text inside curly brackets in MSG will be output
   (presumably including a '^').  Otherwise that text will not be output
   and the text inside square brackets will be output instead.  */
extern void post_error_ne_tree	PROTO((char *, Node_Id, Entity_Id, tree));

/* Surround EXP with a SAVE_EXPR, but handle unconstrained objects specially
   since it doesn't make any sense to put them in a SAVE_EXPR.  */
extern tree make_save_expr	PROTO((tree));

/* Signal abort, with "Gigi abort" as the error label, and error_gnat_node
   as the relevant node that provides the location info for the error.
   The single parameter CODE is an integer code that is included in the
   additional error message generated */
extern void gigi_abort          PROTO((int))  __attribute__ ((noreturn));

/* Initialize the table that maps GNAT codes to GCC codes for simple
   binary and unary operations.  */
extern void init_code_table	PROTO((void));

/* This is equivalent to stabilize_reference in GCC's tree.c, but we know
   how to handle our new nodes and we take an extra argument that says 
   whether to force evaluation of everything.  */

extern tree gnat_stabilize_reference PROTO((tree, int));
