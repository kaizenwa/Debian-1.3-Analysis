/*
 * mib.h
 *
 * Copyright (c) 1994, 1995
 *
 * Sven Schmidt, J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#ifndef _MIB_H
#define _MIB_H

/*
 * A structure to store an integer value and a name. This is used
 * to convert enumerated integers to/from readable names.
 */

typedef struct MIB_IntEnum { 
    char *value;		 /* The value of the integer as a string. */
    char *name;			 /* The name of this integer value. */
    struct MIB_IntEnum *nextPtr; /* Next value/name pair in list. */
} MIB_IntEnum;

/*
 * A structure to store the Textual-Convention macros. The 
 * Textual-Convention stores a format string if displayHint is NULL. 
 * If displayHint is not NULL, the Textual-Convention stores an INTEGER 
 * enumeration.
 */

typedef struct MIB_TextConv {
    char	*name;		  /* The name of the Textual Convention. */
    short	syntax;		  /* The ASN.1 syntax used e.g. INTEGER. */
    char	*displayHint;	  /* The display hint, eg. 2d. */
    MIB_IntEnum	*enumList;	  /* The list of enumarations. */
    struct MIB_TextConv *nextPtr; /* Next MIB_TextConv in the list. */
} MIB_TextConv;

/*
 * The following structure is used to hold the MIB tree in memory.
 * Every node is linked with its parent, a list of child nodes and
 * the next node on the current MIB level.
 */

typedef struct MIB_Node {
    char 	 *label;	/* Node's textual name (not always unique). */
    char	 *parentName;	/* Name of parent node during parse.        */
    char         *fileName;	/* The file with the textual description.   */
    int		 fileOffset;	/* Index value for node Textual Description */
    short	 syntax;	/* This node's object type syntax.          */
    u_char	 access;	/* The access mode of the object.	    */
    char	 *index;	/* The list of index nodes in a table entry.*/
    MIB_TextConv *tc;		/* Optional Textual Convention.		    */
    u_int	 subid;		/* This node's integer subidentifier.       */
    struct MIB_Node *parentPtr;	/* The parent of this node.	            */
    struct MIB_Node *childPtr;	/* List of child nodes.		            */
    struct MIB_Node *nextPtr;	/* List of peer nodes.			    */
} MIB_Node;

#define M_NOACCESS	0
#define M_READONLY      1
#define M_READCREATE	2
#define M_READWRITE 	3
#define M_WRITEONLY	4
#define M_FORNOTIFY	5

/*
 * Exported variables:
 */

extern char *mib_FileName;		/* Current MIB file name loaded. */
extern MIB_Node *mib_Tree;		/* The root of the MIB tree.	 */
extern MIB_TextConv *mib_TCList;	/* List of textual conventions.	 */
extern MIB_TextConv *mib_TCSaveMark;	/* The first already saved	 */
					/* element of mib_TCList	 */

/*
 * Exported functions to access the information stored 
 * in the internal MIB tree.
 */

extern char*
MIB_Oid		_ANSI_ARGS_((char *name, int exact));

extern char*
MIB_Name	_ANSI_ARGS_((char *oid,  int exact));

extern char*
MIB_Syntax	_ANSI_ARGS_((char *name, int exact));

extern char*
MIB_Description	_ANSI_ARGS_((char *name, int exact));

extern int
MIB_ASN1	_ANSI_ARGS_((char *name, int exact));

extern char*
MIB_Access	_ANSI_ARGS_((char *name, int exact));

extern char*
MIB_Succ	_ANSI_ARGS_((char *name));

extern char*
MIB_Format	_ANSI_ARGS_((char *name, int exact, char *arg));

extern char*
MIB_Scan	_ANSI_ARGS_((char *name, int exact, char *arg));

extern char*
MIB_TC		_ANSI_ARGS_((char *name, int exact));

extern char*
MIB_File	_ANSI_ARGS_((char *name, int exact));

extern char*
MIB_Index	_ANSI_ARGS_((char *name, int exact));

extern char*
MIB_Parent	_ANSI_ARGS_((char *name, int exact));

extern char*
MIB_DefVal	_ANSI_ARGS_((char *name, int exact));

extern MIB_Node*
MIB_FindNode	_ANSI_ARGS_((char *name, int *offset, int exact));

/*
 * Functions to read a file containing mib definitions.
 */

extern MIB_Node*
MIB_Parse		_ANSI_ARGS_((char *file, char *frozen,
				     MIB_Node *root));
extern MIB_Node*
MIB_ReadFrozenFile	_ANSI_ARGS_((FILE *fp));

extern void
MIB_WriteFrozenFile	_ANSI_ARGS_((FILE *fp, MIB_Node *nodePtr));

/*
 * Functions used by the parser or the frozen file reader to build
 * the MIB tree structure(s).
 */

extern MIB_Node*
MIB_MallocNode		_ANSI_ARGS_((char *label));

extern void
MIB_AddToTree		_ANSI_ARGS_((MIB_Node **rootPtr, MIB_Node *nodePtr));

extern MIB_TextConv*
MIB_AddTC		_ANSI_ARGS_((MIB_TextConv *tcPtr));

extern MIB_TextConv*
MIB_LookupTC		_ANSI_ARGS_((char *name));

/*
 * The Tcl initialization funtion.
 */

extern int
MIB_Init		_ANSI_ARGS_((Tcl_Interp *interp));


#endif /* _MIB_H */
