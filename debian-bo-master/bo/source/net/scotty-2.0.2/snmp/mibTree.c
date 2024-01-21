/*
 * mibTree.c
 *
 * Some utilities to build a tree that contains the data found in MIB
 * definitions.
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

#include "snmp.h"
#include "mib.h"

/*
 * The following table is used to hash nodes by name before building
 * the MIB tree. This has the nice effect that nodes with the same
 * parent can be found in the list that saves the collisions.
 */

#define NODEHASHSIZE	127
static MIB_Node	*nodehashtab[NODEHASHSIZE];

/*
 * Hashtable used to store textual conventions by name. This
 * allows fast lookups. The nodeHashTable is used to lookup
 * MIB names.
 */

static Tcl_HashTable *tcHashTable = NULL;
static Tcl_HashTable *nodeHashTable = NULL;

/*
 * Forward declarations for procedures defined later in this file:
 */

static MIB_Node*
LookupOID		_ANSI_ARGS_((MIB_Node *root, char *label,
				     int *offset, int exact));
static MIB_Node*
LookupLabel		_ANSI_ARGS_((MIB_Node *root, char *start, char *label,
				     int *offset, int exact, int fuzzy));
static void
HashNode		_ANSI_ARGS_((MIB_Node *node));

static MIB_Node*
BuildTree		_ANSI_ARGS_((MIB_Node *nlist));

static void
BuildSubTree		_ANSI_ARGS_((MIB_Node *root));

static void
HashNodeList		_ANSI_ARGS_((MIB_Node *nlist));

static int
HashNodeLabel		_ANSI_ARGS_((char *label));


/*
 * LookupOID() searches for the node given by soid. It converts the
 * oid from string to binary representation and uses to subids to
 * go from the root to the requested node. Returns the node or NULL
 * if the node was not found. The offset to any trailing garbage is
 * written to offset, if offset is not a NULL pointer.
 */

static MIB_Node*
LookupOID (root, label, offset, exact)
     MIB_Node *root;
     char *label;
     int *offset;
     int exact;
{
    ASN1_OID *id;
    int i, len;
    MIB_Node *p, *q = NULL;
    char *s = label;

    if (offset) *offset = -1;

    id = ASN1_Str2Oid (label, &len);

    for (p = root; p ; p = p->nextPtr) {
	if (id[0] == p->subid) break;
    }
    if (!p) return NULL;

    while (offset && s && ispunct(*s)) s++;
    while (offset && s && isdigit(*s)) s++;

    for (q = p, i = 1; i < len; p = q, i++) {
	for (q = p->childPtr; q; q = q->nextPtr) {
	    if (q->subid == id[i]) break;
	}
	if (!q) {
	    if (! exact) {
		if (offset) {
		    *offset = s - label;
		}
		return p;
	    } else {
		return NULL; 
	    }
	}
	while (offset && s && ispunct(*s)) s++;
	while (offset && s && isdigit(*s)) s++;
    }

    return q;
}


/*
 * LookupLabel() extracts the first element (head) of the label
 * argument and recursively searches for a tree node named head. If
 * found, it checks whether the remaining label elements (tail) can be
 * resolved from this node.
 */

static MIB_Node*
LookupLabel (root, start, label, offset, exact, fuzzy)
     MIB_Node *root;
     char *start;
     char *label;
     int *offset;
     int exact;
     int fuzzy;
{
    char head[OID_MAXLEN * 8];
    char *tail = label, *p = head;
    MIB_Node *tp = NULL, *brother;
    int num = 1;

    if (!root) return NULL;

    if (offset) *offset = -1;

    while (*tail && *tail != '.') {
	num = isdigit (*tail) ? num : 0;
	*p++ = *tail++;
    }
    *p = '\0';
    if (*tail == '.') tail++;
    num = num ? atoi (head) : -1;

    for (brother = root; brother; brother = brother->nextPtr) {
	if ((strcmp (head, brother->label) == 0) || (num == brother->subid)) {
	    if (! *tail) {
		tp = brother;
	    } else if (brother->childPtr) {
		tp = LookupLabel (brother->childPtr, start, tail, 
				  offset, exact, 0);
	    } else if (! exact) {
		tp = brother;
	    }
	    if (tp) {
		if (offset && (*offset < tail-start-1) && *offset != -2) {
		    *offset = *tail ? tail-start-1 : -2;
		}
		return tp;
	    }
	}
	if (fuzzy && brother->childPtr) {
	    tp = LookupLabel (brother->childPtr, start, label, 
			      offset, exact, 1);
	    if (tp) {
		if (offset && (*offset < tail-start-1) && *offset != -2) {
		    *offset = *tail ? tail-start-1 : -2;
		}
		return tp;
	    }
	}
    }

    return NULL;
}


/*
 * MIB_FindNode() takes a MIB name and searches for the corresponding
 * node in the MIB tree. The object identifier of the node is written
 * to soid if soid is not NULL. soid must be large enough to hold the
 * complete path. MIB_FindNode() calls LookupOID() if the name is an
 * object identier for fast MIB searches. Otherwise we call slow
 * LookupLabel() to compare the tree node labels.  
 */

MIB_Node*
MIB_FindNode (name, offset, exact)
     char *name;
     int *offset;
     int exact;
{
    MIB_Node *nodePtr = NULL;
    char *expanded = ASN1_Hex2Oid (name);
    if (expanded) name = expanded;

    if (ASN1_IsOid (name)) {
	nodePtr = LookupOID (mib_Tree, name, offset, exact);
    } else {
	Tcl_HashEntry *entryPtr = NULL;
	if (nodeHashTable) {
	    entryPtr = Tcl_FindHashEntry (nodeHashTable, name);
	}
	if (entryPtr) {
	    nodePtr = (MIB_Node *) Tcl_GetHashValue (entryPtr);
	}
	if (! nodePtr) {
	    nodePtr = LookupLabel (mib_Tree, name, name, offset, exact, 1);
	}
    }

    return nodePtr;
}


/*
 * MIB_AddTC() adds a MIB_TextConv structure to the set of known
 * textual conventions by putting it into the hash table. We 
 * return the pointer to the MIB_TextConv structure.
 */

MIB_TextConv *
MIB_AddTC (tcPtr)
     MIB_TextConv *tcPtr;
{
    char *name = tcPtr->name;
    Tcl_HashEntry *entryPtr;
    int isnew;

    if (! tcHashTable) {
	tcHashTable = (Tcl_HashTable *) ckalloc (sizeof (Tcl_HashTable));
	Tcl_InitHashTable (tcHashTable, TCL_STRING_KEYS);
    }
    
    entryPtr = Tcl_CreateHashEntry (tcHashTable, name, &isnew);
    
    if (! isnew) {
	return (MIB_TextConv *) Tcl_GetHashValue (entryPtr);
    }

    tcPtr->nextPtr = mib_TCList;
    mib_TCList = tcPtr;
    
    Tcl_SetHashValue (entryPtr, (ClientData) tcPtr);

    return tcPtr;
}


/*
 * MIB_LookupTC() searches for a textual convention given by name.
 * We return the pointer to the MIB_TextConv structure or NULL if
 * we were not able to find the name in the hash table.
 */

MIB_TextConv *
MIB_LookupTC (name)
     char *name;
{
    Tcl_HashEntry *entryPtr;
    
    if (! tcHashTable) {
	return NULL;
    }

    entryPtr = Tcl_FindHashEntry (tcHashTable, name);
    if (! entryPtr) {
	return NULL;
    }

    return (MIB_TextConv *) Tcl_GetHashValue (entryPtr);
}


/*
 * MIB_MallocNode() allocates a new tree element and does some 
 * basic initialization.
 */

MIB_Node*
MIB_MallocNode (label)
     char *label;
{
    MIB_Node *nodePtr = (MIB_Node *) ckalloc (sizeof (MIB_Node));
    memset ((char *) nodePtr, '\0', sizeof (MIB_Node));
    if (label) {
	nodePtr->label = ckstrdup (label);
    }
    return nodePtr;
}


/*
 * To speedup node lookups, use a hashtable: as long as a node's
 * label is unique, use the tab, else recurse on the tree.
 */

static void
HashNode (nodePtr)
     MIB_Node *nodePtr;
{
    char *name = nodePtr->label;
    Tcl_HashEntry *entryPtr;
    int isnew;
    
    if (! nodeHashTable) {
	nodeHashTable = (Tcl_HashTable *) ckalloc (sizeof (Tcl_HashTable));
	Tcl_InitHashTable (nodeHashTable, TCL_STRING_KEYS);
    }
    
    entryPtr = Tcl_CreateHashEntry (nodeHashTable, name, &isnew);
    
    if (! isnew) {
	if (nodePtr != (MIB_Node *) Tcl_GetHashValue (entryPtr)) {
	    /* leave in hashtable, but mark with NULL ptr: */
	    Tcl_SetHashValue (entryPtr, (ClientData) 0);
	}
	return;
    }

    Tcl_SetHashValue (entryPtr, (ClientData) nodePtr);
}


/*
 * BuildTree() builds a MIB tree. Returns a pointer to root if
 * successful, NULL otherwise.
 */

static MIB_Node*
BuildTree (nodeList)
     MIB_Node *nodeList;
{
    MIB_Node *ccitt, *iso, *joint;
    
    HashNodeList (nodeList);

    /* build the "ccitt" node */

    ccitt = MIB_MallocNode ("ccitt");
    ccitt->parentName = ckstrdup ("(unknown)");
    ccitt->syntax     = ASN1_OBJECT_IDENTIFIER;
    
    /* build the "iso" node */

    iso = MIB_MallocNode ("iso");
    iso->parentName = ckstrdup ("(unknown)");
    iso->subid      = 1;
    iso->syntax     = ASN1_OBJECT_IDENTIFIER;
    ccitt->nextPtr  = iso;

    /* build the "joint-iso-ccitt" node */
    
    joint = MIB_MallocNode ("joint-iso-ccitt");
    joint->parentName = ckstrdup ("(unknown)");
    joint->subid      = 2;
    joint->syntax     = ASN1_OBJECT_IDENTIFIER;
    iso->nextPtr      = joint;
    
    /* build the subtrees */

    BuildSubTree (ccitt);
    BuildSubTree (iso);
    BuildSubTree (joint);

    /*
     * Return the first node in the tree (perhaps "iso" would be
     * better, but this way is consistent)
     */
    
    return ccitt;
}


/*
 * BuildSubTree() finds all the children of root in the list of
 * nodes, link them in the tree and out of the node list. Returns
 * pointer to root if successful, NULL otherwise.
 */

static void
BuildSubTree (root)
     MIB_Node *root;
{
    MIB_Node **np, **ptr;
    int	hash = HashNodeLabel (root->label);

    /*
     * Loop through all nodes whose parent is root. They are all
     * members of the list which starts at the same hash key.
     */

    for (np = &nodehashtab[hash]; *np != NULL; ) {
	
	if (root->label[0] == ((*np)->parentName)[0]
	    && (strcmp (root->label, (*np)->parentName) == 0)) {

	    MIB_Node *thisNode = *np;
	    *np = (*np)->nextPtr;

	    thisNode->fileName = mib_FileName;
/*** XXX: 
	    if (thisNode->parentName) {
		ckfree (thisNode->parentName);
		thisNode->parentName = NULL;
	    }
***/
	    thisNode->parentPtr = root;
	    thisNode->childPtr = NULL;
	    thisNode->nextPtr  = NULL;
	    
	    /* 
	     * Link node in the tree. First skip over all nodes with a
	     * subid less than the new subid. Insert the node if the
	     * node does not already exist. Otherwise free this node.
	     */

	    ptr = &root->childPtr;
	    while (*ptr && (*ptr)->subid < thisNode->subid) {
		ptr = &(*ptr)->nextPtr;
	    }
	    if (*ptr && (*ptr)->subid == thisNode->subid) {
/*** XXX	if (thisNode->label) ckfree ((char *) thisNode->label);
		ckfree ((char *) thisNode);		***/
	    } else {
		thisNode->nextPtr = *ptr;
                *ptr = thisNode;
		HashNode (thisNode);
	    }

	    BuildSubTree (*ptr);			/* recurse on child */

	} else {
	    np = &(*np)->nextPtr;
	}
    }
}


/*
 * AddToTree() add all nodes in nodeList to the MIB tree. It 
 * initializes the hash table and calls BuildSubTree() to move
 * nodes from the hash table into the MIB tree.
 */

void
MIB_AddToTree (rootPtr, nodeList)
     MIB_Node **rootPtr;
     MIB_Node *nodeList;
{
    MIB_Node *nodePtr;
    MIB_Node *tree;
    MIB_Node *root = *rootPtr;
    int i;

    if (! nodeList) return;

    if (! root) {
	*rootPtr = BuildTree (nodeList);
    }

   /*
    * Test if the parent of the first node exists. We expect that we
    * load individual subtrees where the parent of the first node
    * defines the anchor. This must already exist. Note, the first 
    * node is the last node in our nodeList.
    */

    for (nodePtr = nodeList; nodePtr->nextPtr; nodePtr = nodePtr->nextPtr) ;
    tree = MIB_FindNode (nodePtr->parentName, NULL, 1);
    HashNodeList (nodeList);
    if (tree) {
	BuildSubTree (tree);
    }
    
   /*
    * Now the slow part: If there are any nodes left in the hash table,
    * we either have two isolated subtrees or the last node in nodeList
    * was not the root of our tree. We scan for nodes which have known
    * parents in the existing tree and call BuildSubTree for those
    * parents.
    *
    * Perhaps we should check for candidates in the nodelist, as this
    * would allow some local scope. XXX
    */

  repeat:
    for (i = 0; i < NODEHASHSIZE; i++) {
	for (nodePtr = nodehashtab[i]; nodePtr; nodePtr = nodePtr->nextPtr) {
	    tree = MIB_FindNode (nodePtr->parentName, NULL, 1);
	    if (tree) {
		BuildSubTree (tree);
		goto repeat;
	    }
	}
    }

    /* 
     * Finally print out all the node labels that are still in the
     * hashtable. This should allow people to fix the ordering of
     * MIB load commands.
     */
    
    for (i = 0; i < NODEHASHSIZE; i++) {
	for (nodePtr = nodehashtab[i]; nodePtr; nodePtr = nodePtr->nextPtr) {
	    fprintf (stderr, "%s: no parent %s for node %s\n", 
		     mib_FileName, nodePtr->parentName, nodePtr->label);
	}
    }
}


/*
 * HashNodeList() reads the input nodelist and puts them into the
 * node hashtable, where the hash function is build by the parent
 * names. This means, that all nodes with the same parent are in one
 * list.
 */

static void
HashNodeList (nodeList)
     MIB_Node *nodeList;
{
    int	hash;
    MIB_Node *nodePtr, *nextp;

    memset ((char *) nodehashtab,'\0', sizeof (nodehashtab));

    for (nodePtr = nodeList; nodePtr != NULL; ) {
	if (! nodePtr->parentName) {
	    fprintf (stderr, "%s: %s has no parent in the MIB tree!\n",
		     mib_FileName, nodePtr->label);
	    return;
	}

	hash = HashNodeLabel (nodePtr->parentName);

	nextp = nodePtr->nextPtr;
	nodePtr->nextPtr = nodehashtab[hash];
	nodehashtab[hash] = nodePtr;
	nodePtr = nextp;
    }
}


/*
 * Compute the hash value of a node label. Used to group nodes
 * by parents. The conflicts will create a list with all nodes
 * belonging to a parent (plus several others of course).
 */

static int
HashNodeLabel (label)
     char *label;
{
    int hash = 0;
    char *cp;
    
    for (cp = label; *cp; cp++) {
	hash += *cp;
    }

    return (hash % NODEHASHSIZE);
}

