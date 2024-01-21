/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             A - G T R A N 3                              */
/*                                                                          */
/*                              C Header File                               */
/*                                                                          */
/*                            $Revision: 1.32 $                             */
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
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

/*  Definitions to access front-end functions used in gigi */
#define Entity_Is_In_Main_Unit lib__entity_is_in_main_unit
extern Boolean Entity_Is_In_Main_Unit PROTO((Entity_Id));
#define Expr_Value		sem_eval__expr_value
extern Uint Expr_Value		PROTO((Node_Id));
#define Expr_Value_S		sem_eval__expr_value_s
extern Node_Id Expr_Value_S	PROTO((Node_Id));

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
extern tree gnat_to_gnu_entity	PROTO((Entity_Id, tree, int));

/* Given GNAT_ENTITY, an entity in the incoming GNAT tree, return a
   GCC type corresponding to that entity.  GNAT_ENTITY is assumed to
   refer to an Ada type.  */
extern tree gnat_to_gnu_type	PROTO((Entity_Id));

/* Given GNAT_ENTITY, elaborate all expressions that are required to
   be elaborated at the point of its definition, but do nothing else.  */
extern void elaborate_entity	PROTO((Entity_Id));

/* Make a dummy type corresponding to GNAT_TYPE.  */
extern tree make_dummy_type	PROTO((Entity_Id));

/* Get the unpadded version of a GNAT type.  */
extern tree get_unpadded_type	PROTO((Entity_Id));

/* Called when we need to protect a variable object using a save_expr.  */
extern tree maybe_variable	PROTO((tree, Node_Id));

/* Ensure that TYPE has SIZE and ALIGN.  Make and return a new padded type
   if needed.  We have already verified that SIZE and TYPE are large enough.

   GNAT_ENTITY and NAME_TRAILER are used to name the resulting record and
   to issue a warning. 

   IN_PLACE is nonzero if we can modify the type in place; otherwise
   we must make a new one to make any changes to the size and alignment.  */
extern tree maybe_pad_type	PROTO((tree, tree, int, Entity_Id,
				       char *, int));

/* Given a GNU tree and a GNAT list of choices, generate an expression to test
   the value passed against the list of choices.  If STRING is non-zero,
   concatenate a discription of the choice to it.  Set STRING[0] to zero
   if we can't.  */
extern tree choices_to_gnu	PROTO((tree, Node_Id, char *));

/* Given a type T, a FIELD_DECL F, and a replacement value R,
   return a new type with all size expressions that contain F
   updated by replacing F with R.  This is identical to GCC's
   substitute_in_type except that it knows about TYPE_INDEX_TYPE.  */
extern tree gnat_substitute_in_type PROTO((tree, tree, tree));

/* Return the "RM size" of GNU_TYPE.  This is the actual number of bits
   needed to represent the object.  */
extern tree rm_size		PROTO((tree));

/* Return a name for GNAT_ENTITY concatenated with two underscores and
   STRING.  */
extern tree create_concat_name PROTO((Entity_Id, char *));
