/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                               A - M I S C                                */
/*                                                                          */
/*                              C Header File                               */
/*                                                                          */
/*                            $Revision: 1.22 $                             */
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

/* Definitions to access the front-end's character translation tables.  */
extern char csets__fold_lower[], csets__fold_upper[];
#define Fold_Lower(C) csets__fold_lower[C]
#define Fold_Upper(C) csets__fold_upper[C]

/* Variables expected by the gcc back-end for reference or merely linking
   purposes:  */

/* A string identifing the compiler.  */
extern char *language_string;

/* The gcc back-end sets this in jump.c.  */
extern int current_function_returns_null;

/* Ada language-specific GC tree codes.  */
#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) SYM,
enum gnat_tree_code {
  __DUMMY = LAST_AND_UNUSED_TREE_CODE,
#include "a-tree.def"
  LAST_GNAT_TREE_CODE
};
#undef DEFTREECODE

/* Routines expected by the gcc back-end for call-back or merely linking
   purposes. They must have exactly the same prototype and name as expected
   by GCC.  */

/* Decode all the language specific options that cannot be decoded by GCC. The
   option decoding phase of GCC calls this routine on the flags that it cannot
   decode. This routine returns 1 if it is successful, otherwise it
   returns 0. */
extern int lang_decode_option	PROTO((char *));

/* Perform all the initialization steps that are language-specific.  */
extern void lang_init		PROTO((void));

/* Perform all the finalization steps that are language-specific.  */
extern void lang_finish		PROTO((void));

/* Print any language-specific compilation statistics.  */
extern void print_lang_statistics	PROTO((void));

/* Return a short string identifying this language to the debugger.  */
extern char *lang_identify	PROTO((void));

#ifdef BUFSIZ
/* Hooks for `print_node'.  */
extern void print_lang_decl	PROTO((FILE *, tree, int));
extern void print_lang_type	PROTO((FILE *, tree, int));
extern void print_lang_identifier PROTO((FILE *, tree, int));
#endif

/* Record the current code position in GNAT_NODE.  */
extern void record_code_position PROTO((Node_Id));

/* Insert the code for GNAT_NODE at the position saved for that node.  */
extern void insert_code_for	PROTO((Node_Id));

/* Performs whatever initialization steps needed by the language-dependent
   lexical analyzer.  */
extern void init_lex		PROTO((void));

/* Sets some debug flags for the parser. It does nothing here.  */
extern void set_yydebug		PROTO((int));

/* Utility routines created for the tree translator's sake. Their prototypes
   can be changed as desired.  */

/* Make a TRANSFORM_EXPR to later expand GNAT_NODE into an object
   of GNU_TYPE.  */
extern tree make_transform_expr PROTO((Node_Id, tree));

/* Update the setjmp buffer BUF with the current stack pointer.  We assume
   here that a __builtin_setjmp was done to BUF.  */
extern void update_setjmp_buf (tree);

/* GNU_TYPE is the type of a subprogram parameter.  Determine from the type if
   it should be passed by reference.  */
extern int must_pass_by_ref	     PROTO((tree));


/* elaboration routines for the front end */
extern   void elab_all_gnat          PROTO((void));
