/*
 * mibFrozen.c
 *
 * Save and load MIB-Definitions in/from a frozen-format file.
 *
 * Copyright (c) 1995
 *
 * E. Schoenfelder, J. Schoenwaelder
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

#include "snmp.h"
#include "mib.h"

/*
 * Strings are collected in hashtable for every parsed mib. This allows
 * us to write all strings in one big chunk so that we do not need to
 * allocate every single string.
 */

static Tcl_HashTable *poolHashTable = 0;
static int p_offset = 0;		/* next string offset (== size) */

#define IDY_MAGIC	"Nase"

/*
 * Forward declarations for procedures defined later in this file:
 */

static void
PoolInit		_ANSI_ARGS_((void));

static void
PoolDelete		_ANSI_ARGS_((void));

static void
PoolAddString		_ANSI_ARGS_((char *string));

static int
PoolGetOffset		_ANSI_ARGS_((char *string));


/*
 * PoolInit() initializes the hash table that is used to create a
 * string pool. The pool is used to eliminate duplicated strings.
 */

static void
PoolInit ()
{
    p_offset = 0;
    if (! poolHashTable) {
	poolHashTable = (Tcl_HashTable *) ckalloc (sizeof (Tcl_HashTable));    
    }
    Tcl_InitHashTable (poolHashTable, TCL_STRING_KEYS);
}

/*
 * PoolDelete() clears the memory used to create the string pool.
 * Very simple and really not worth the comments. Thats why this
 * one is getting a bit longer as usual. :-)
 */

static void
PoolDelete ()
{
    Tcl_DeleteHashTable (poolHashTable);
}

/*
 * PoolAddString() adds a string to the pool if it is not yet there.
 * The value is initialized to mark this entry as used. The total offset
 * is incremented to get total memory required for the string pool.
 */

static void
PoolAddString (s)
     char *s;
{
    Tcl_HashEntry *entryPtr;
    int isnew;

    if (! s) return;

    entryPtr = Tcl_CreateHashEntry (poolHashTable, s, &isnew);
    if (! isnew) {
	return;
    }
    Tcl_SetHashValue (entryPtr, 1);
    p_offset += strlen (s) + 1;
}

/*
 * PoolGetOffset() returns the offset to the given string in the 
 * string pool or 0 if the string is not in the pool.
 */

static int
PoolGetOffset (s)
     char *s;
{
    Tcl_HashEntry *entryPtr;
    if (! s || ! (entryPtr = Tcl_FindHashEntry (poolHashTable, s))) {
	return 0;
    }
    return (int) Tcl_GetHashValue (entryPtr);
}

/*
 * PoolSave() writes the string pool to the given file pointer.
 */

static void
PoolSave (fp)
     FILE *fp;
{
    Tcl_HashSearch searchPtr;
    Tcl_HashEntry *entryPtr;

    /* save size: */
    p_offset += strlen (IDY_MAGIC) + 1;				/* add magic */
    fwrite ((char *) &p_offset, sizeof (int), 1, fp);
    fwrite (IDY_MAGIC, 1, strlen (IDY_MAGIC) + 1, fp);		/* and save */
    /* save strings: */
    p_offset = strlen (IDY_MAGIC) + 1;
    
    entryPtr = Tcl_FirstHashEntry (poolHashTable, &searchPtr);
    while (entryPtr) {
	char *s = Tcl_GetHashKey (poolHashTable, entryPtr);
	int len = strlen (s) + 1;
	Tcl_SetHashValue (entryPtr, p_offset);
	fwrite (s, 1, len, fp);
	p_offset += len;
	entryPtr = Tcl_NextHashEntry (&searchPtr);
    }
}


/*
 * collect strings (added to the pool) and count the number of
 * enums, tcs and nodes to be saved.
 */

static void
collect_data (fp, n_enum, n_tc, n_node, nodePtr)
     FILE *fp;
     int *n_enum, *n_tc, *n_node;
     MIB_Node *nodePtr;
{
    MIB_Node *ptr;
    MIB_IntEnum *e;
    MIB_TextConv *tcPtr;

    *n_enum = *n_tc = *n_node = 0;
    for (ptr = nodePtr; ptr; (*n_node)++, ptr = ptr->nextPtr) {
	PoolAddString (ptr->label);
	PoolAddString (ptr->parentName);
	PoolAddString (ptr->fileName);
	PoolAddString (ptr->index);
	if (ptr->tc) {
	    (*n_tc)++;
	    PoolAddString (ptr->tc->name);
	    PoolAddString (ptr->tc->displayHint);
	    for (e = ptr->tc->enumList; e; (*n_enum)++, e = e->nextPtr) {
		PoolAddString (e->name);
		PoolAddString (e->value);
	    }
	}
    }
    for (tcPtr = mib_TCList; tcPtr != mib_TCSaveMark; tcPtr = tcPtr->nextPtr) {
	(*n_tc)++;
	PoolAddString (tcPtr->name);
	PoolAddString (tcPtr->displayHint);
	for (e = tcPtr->enumList; e; (*n_enum)++, e = e->nextPtr) {
	    PoolAddString (e->name);
	    PoolAddString (e->value);
	}
    }
}


static void
save_enum (e, fp)
     MIB_IntEnum *e;
     FILE *fp;
{
    struct MIB_IntEnum en;
    memcpy ((char *) &en, (char *) e, sizeof (en));
    en.value = (char *) PoolGetOffset (e->value);
    en.name = (char *) PoolGetOffset (e->name);
    en.nextPtr = (MIB_IntEnum *) (e->nextPtr ? 1 : 0);
    fwrite ((char *) &en, sizeof (en), 1, fp);
}

static void
save_tc (tc, i, fp)
     MIB_TextConv *tc;
     int *i;
     FILE *fp;
{
    MIB_TextConv tco;

    memcpy ((char *) &tco, (char *) tc, sizeof (tco));
    tco.name = (char *) PoolGetOffset (tc->name);
    tco.displayHint = (char *) PoolGetOffset (tc->displayHint);
    if (tc->enumList) {
	MIB_IntEnum *e;
	tco.enumList = (MIB_IntEnum *) (*i + 1);
	for (e = tc->enumList; e; (*i)++, e = e->nextPtr) ;
    }
    tco.nextPtr = (MIB_TextConv *) (tc->nextPtr ? 1 : 0);
    fwrite ((char *) &tco, sizeof (tco), 1, fp);
}

static void
save_node (ptr, i, fp)
     MIB_Node *ptr;
     int *i;
     FILE *fp;
{
    struct MIB_Node node;
    memcpy ((char *) &node, (char *) ptr, sizeof (MIB_Node));
    node.label = (char *) PoolGetOffset (ptr->label);
    node.parentName = (char *) PoolGetOffset (ptr->parentName);
    node.fileName = (char *) PoolGetOffset (ptr->fileName);
    node.index = (char *) PoolGetOffset (ptr->index);
    node.childPtr = 0;
    if (ptr->tc)
      node.tc = (MIB_TextConv *) ++(*i);
    node.nextPtr = (MIB_Node *) (ptr->nextPtr ? 1 : 0);
    
    fwrite ((char *) &node, sizeof (node), 1, fp);
}

/*
 * save_data() writes the node list as an index file to fp. 
 *
 *	(the string pool is still written).
 *
 *	int:   #enum elements
 *  	followed by #enum structs
 *
 * 	int:	#tc elements
 *	followed by #tc structs
 *
 *      int:  #node elements
 *	followed by n MIB_Node structs.
 *
 *	string pointer are saved as offsets relative to the pool,
 *	enum pointer are saved as offset to the saved enums (+ 1)
 *	tc pointer dito.
 */

static void
save_data (fp, n_enum, n_tc, n_node, nodePtr)
     FILE *fp;
     int n_enum, n_tc, n_node;
     MIB_Node *nodePtr;
{
    MIB_Node *ptr;
    MIB_IntEnum *e;
    MIB_TextConv *tcPtr;
    int i;
    
    /* save #enum structs: */
    fwrite ((char *) &n_enum, sizeof (int), 1, fp);
    for (ptr = nodePtr; ptr; ptr = ptr->nextPtr)
      if (ptr->tc)
	for (e = ptr->tc->enumList; e; e = e->nextPtr)
	  save_enum (e, fp);
    for (tcPtr = mib_TCList; tcPtr != mib_TCSaveMark; tcPtr = tcPtr->nextPtr)
      for (e = tcPtr->enumList; e; e = e->nextPtr)
	save_enum (e, fp);

    /* save #tc structs: */
    fwrite ((char *) &n_tc, sizeof (int), 1, fp);
    for (i = 0, ptr = nodePtr; ptr; ptr = ptr->nextPtr)
      if (ptr->tc)
	save_tc (ptr->tc, &i, fp);
    for (tcPtr = mib_TCList; tcPtr != mib_TCSaveMark; tcPtr = tcPtr->nextPtr)
      save_tc (tcPtr, &i, fp);
    
    /* save #node structs: */
    fwrite ((char *) &n_node, sizeof (int), 1, fp);
    for (i = 0, ptr = nodePtr; ptr; ptr = ptr->nextPtr)
      save_node (ptr, &i, fp);
}


void
MIB_WriteFrozenFile (fp, nodePtr)
     FILE *fp;
     MIB_Node *nodePtr;
{
    int n_enum, n_tc, n_node; 
    PoolInit ();
    collect_data (fp, &n_enum, &n_tc, &n_node, nodePtr);
    PoolSave (fp);
    save_data (fp, n_enum, n_tc, n_node, nodePtr);
    PoolDelete ();
}

/*
 * ReadFrozenFile() reads an index file that was written by 
 * WriteFrozenFile().
 *
 * we expect:		
 *	int (stringpool size)
 *		pool_size bytes
 *	int (number of enum structs)
 *		#enum structs
 *	int (number of textual conventions)
 *		#tc structs
 *	int (number of nodes)
 *		#node structs
 */

MIB_Node*
MIB_ReadFrozenFile (fp)
     FILE *fp;
{
    MIB_Node *root = NULL;
    int pool_size;
    char *pool;
    int n_enums;
    MIB_IntEnum *enums = 0;
    int n_tcs;
    MIB_TextConv *tcs = NULL;
    int n_nodes;
    MIB_Node *nodes;

    /*
     * read string space: 
     */
    if (1 != fread ((char *) &pool_size, sizeof (int), 1, fp)) {
	fprintf (stderr, "error reading string pool size...\n");
	return 0;
    }
    pool = ckalloc (pool_size);
    if (pool_size != fread (pool, 1, pool_size, fp)) {
	fprintf (stderr, "error reading string pool...\n");
	return 0;
    }

    /*
     * read enums:
     */
    if (1 != fread ((char *) &n_enums, sizeof (int), 1, fp)) {
	fprintf (stderr, "error reading enum counter...\n");
	return 0;
    }
    if (n_enums > 0) {
	MIB_IntEnum *e;
	int i;

	enums = (MIB_IntEnum *) ckalloc (n_enums * sizeof (MIB_IntEnum));
	if (n_enums != fread (enums, sizeof (MIB_IntEnum), n_enums, fp)) {
	    fprintf (stderr, "error reading enums...\n");
	    return 0;
	}
    
	/* adjust string and chain pointers: */
	for (i = 0, e = enums; i < n_enums; i++, e++) {
	    e->value = (int) e->value + pool;
	    e->name = (int) e->name + pool;
	    e->nextPtr = e->nextPtr ? e + 1 : 0;
	}
    }

    /*
     * read tc's: 
     */
    if (1 != fread ((char *) &n_tcs, sizeof (int), 1, fp)) {
	fprintf (stderr, "error reading tc counter...\n");
	return 0;
    }
    if (n_tcs > 0) {
	MIB_TextConv *tc;
	int i;

	tcs = (MIB_TextConv *) ckalloc (n_tcs * sizeof (MIB_TextConv));
	if (n_tcs != fread (tcs, sizeof (MIB_TextConv), n_tcs, fp)) {
	    fprintf (stderr, "error reading tcs...\n");
	    return 0;
	}
	
	/* 
	 * Adjust string and enum pointers: 
	 */

	for (i = 0, tc = tcs; i < n_tcs; i++, tc++) {
	    tc->name = (int) tc->name + pool;
	    if (tc->displayHint) {
		tc->displayHint = (int) tc->displayHint + pool;
	    }
	    if (tc->enumList) {
		tc->enumList = (int) tc->enumList + enums - 1;
	    }
	    if (tc->name[0] != '_') {
		MIB_AddTC (tc);
	    }
	}	
    }

    /*
     * read nodes: 
     */
    if (1 != fread ((char *) &n_nodes, sizeof (int), 1, fp)) {
	fprintf (stderr, "error reading node counter...\n");
	return 0;
    }

    if (n_nodes > 0) {
	MIB_Node *ptr;
	int i;

	nodes = (MIB_Node *) ckalloc (n_nodes * sizeof (MIB_Node));
	if (n_nodes != fread (nodes, sizeof (MIB_Node), n_nodes, fp)) {
	    fprintf (stderr, "error reading nodes...\n");
	    return 0;
	}
	
	/* adjust string and tc pointers: */
	for (i = 0, ptr = nodes; i < n_nodes; i++, ptr++) {
	    ptr->label = (int) ptr->label + pool;
	    ptr->parentName = (int) ptr->parentName + pool;
	    if (ptr->fileName)
	      ptr->fileName = (int) ptr->fileName + pool;
	    if (ptr->index)
	      ptr->index = (int) ptr->index + pool;
	    if (ptr->tc)
	      ptr->tc = (int) ptr->tc + tcs - 1;
	    ptr->nextPtr = ptr->nextPtr ? ptr + 1 : 0;
	}
	root = nodes;
    }
    
    return root;
}
