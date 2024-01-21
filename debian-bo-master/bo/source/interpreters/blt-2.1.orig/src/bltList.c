/*
 * bltList.c --
 *
 *	Generic linked list management routines.
 *
 * Copyright 1991-1996 by AT&T Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the
 * names of AT&T Bell Laboratories any of their entities not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data
 * or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 *
 */

#include "bltInt.h"

/*
 *----------------------------------------------------------------------
 *
 * Blt_InitList --
 *
 *	Initialized a linked list.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_InitList(listPtr, type)
    Blt_List *listPtr;
    int type;
{

    listPtr->numEntries = 0;
    listPtr->headPtr = listPtr->tailPtr = (Blt_ListItem *)NULL;
    listPtr->type = type;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_NewList --
 *
 *	Creates a new linked list structure and initializes its pointers
 *
 * Results:
 *	Returns a pointer to the newly created list structure.
 *
 *----------------------------------------------------------------------
 */
Blt_List *
Blt_NewList(type)
    int type;
{
    Blt_List *listPtr;

    listPtr = (Blt_List *)malloc(sizeof(Blt_List));
    if (listPtr != NULL) {
	Blt_InitList(listPtr, type);
    }
    return (listPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_NewItem --
 *
 *	Creates a list entry holder.  This routine does not insert
 *	the entry into the list, nor does it no attempt to maintain
 *	consistency of the keys.  For example, more than one entry
 *	may use the same key.
 *
 * Results:
 *	The return value is the pointer to the newly created entry.
 *
 * Side Effects:
 *	The key is not copied, only the Uid is kept.  It is assumed
 *	this key will not change in the life of the entry.
 *
 *----------------------------------------------------------------------
 */
Blt_ListItem *
Blt_NewItem(key)
    char *key;			/* Unique key to reference object */
{
    register Blt_ListItem *iPtr;

    iPtr = (Blt_ListItem *)malloc(sizeof(Blt_ListItem));
    if (iPtr == (Blt_ListItem *)NULL) {
	Panic("can't allocate list item structure");
    }
    iPtr->keyPtr = key;
    iPtr->clientData = (ClientData)NULL;
    iPtr->nextPtr = iPtr->prevPtr = (Blt_ListItem *)NULL;
    iPtr->listPtr = (Blt_List *)NULL;
    return (iPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_FreeItem --
 *
 *	Free the memory allocated for the entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_FreeItem(iPtr)
    Blt_ListItem *iPtr;
{
    free((char *)iPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ResetList --
 *
 *	Removes all the entries from a list, removing pointers to the
 *	objects and keys (not the objects or keys themselves).
 *	The entry counter is reset to zero.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ResetList(listPtr)
    Blt_List *listPtr;		/* List to clear */
{
    register Blt_ListItem *oldPtr;
    register Blt_ListItem *iPtr = listPtr->headPtr;

    while (iPtr != (Blt_ListItem *)NULL) {
	oldPtr = iPtr;
	iPtr = iPtr->nextPtr;
	Blt_FreeItem(oldPtr);
    }
    Blt_InitList(listPtr, listPtr->type);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_DeleteList
 *
 *     Frees all list structures
 *
 * Results:
 *	Returns a pointer to the newly created list structure.
 *
 *----------------------------------------------------------------------
 */
void
Blt_DeleteList(listPtr)
    Blt_List *listPtr;
{
    Blt_ResetList(listPtr);
    free((char *)listPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_LinkAfter --
 *
 *	Inserts an entry following a given entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_LinkAfter(listPtr, iPtr, afterPtr)
    Blt_List *listPtr;
    Blt_ListItem *iPtr;
    Blt_ListItem *afterPtr;
{
    /*
     * If the list keys are strings, change the key to a Tk_Uid
     */
    if (listPtr->type == TCL_STRING_KEYS) {
	iPtr->keyPtr = Tk_GetUid(iPtr->keyPtr);
    }
    if (listPtr->headPtr == (Blt_ListItem *)NULL) {
	listPtr->tailPtr = listPtr->headPtr = iPtr;
    } else {
	if (afterPtr == (Blt_ListItem *)NULL) {
	    afterPtr = listPtr->tailPtr;
	}
	iPtr->nextPtr = afterPtr->nextPtr;
	iPtr->prevPtr = afterPtr;
	if (afterPtr == listPtr->tailPtr) {
	    listPtr->tailPtr = iPtr;
	} else {
	    afterPtr->nextPtr->prevPtr = iPtr;
	}
	afterPtr->nextPtr = iPtr;
    }
    iPtr->listPtr = listPtr;
    listPtr->numEntries++;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_LinkBefore --
 *
 *	Inserts an entry preceding a given entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_LinkBefore(listPtr, iPtr, beforePtr)
    Blt_List *listPtr;		/* List to contain new entry */
    Blt_ListItem *iPtr;	/* New entry to be inserted */
    Blt_ListItem *beforePtr;	/* Entry to link before */
{
    /*
     * If the list keys are strings, change the key to a Tk_Uid
     */
    if (listPtr->type == TCL_STRING_KEYS) {
	iPtr->keyPtr = Tk_GetUid(iPtr->keyPtr);
    }
    if (listPtr->headPtr == (Blt_ListItem *)NULL) {
	listPtr->tailPtr = listPtr->headPtr = iPtr;
    } else {
	if (beforePtr == (Blt_ListItem *)NULL) {
	    beforePtr = listPtr->headPtr;
	}
	iPtr->prevPtr = beforePtr->prevPtr;
	iPtr->nextPtr = beforePtr;
	if (beforePtr == listPtr->headPtr) {
	    listPtr->headPtr = iPtr;
	} else {
	    beforePtr->prevPtr->nextPtr = iPtr;
	}
	beforePtr->prevPtr = iPtr;
    }
    iPtr->listPtr = listPtr;
    listPtr->numEntries++;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_UnlinkItem --
 *
 *	Unlinks an entry from the given list. The entry itself is
 *	not deallocated, but only removed from the list.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_UnlinkItem(iPtr)
    Blt_ListItem *iPtr;
{
    Blt_List *listPtr;

    listPtr = iPtr->listPtr;
    if (listPtr != NULL) {
	if (listPtr->headPtr == iPtr) {
	    listPtr->headPtr = iPtr->nextPtr;
	}
	if (listPtr->tailPtr == iPtr) {
	    listPtr->tailPtr = iPtr->prevPtr;
	}
	if (iPtr->nextPtr != NULL) {
	    iPtr->nextPtr->prevPtr = iPtr->prevPtr;
	}
	if (iPtr->prevPtr != NULL) {
	    iPtr->prevPtr->nextPtr = iPtr->nextPtr;
	}
	iPtr->listPtr = NULL;
	listPtr->numEntries--;
    }
}

#ifdef notdef
/*
 *----------------------------------------------------------------------
 *
 * Blt_SetItemValue --
 *
 *	Sets the entry data pointer to the given value.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The data is not copied, only the pointer is kept.  It is assumed
 *	this data will remain valid as long as the entry.
 *
 *----------------------------------------------------------------------
 */
void
Blt_SetItemValue(iPtr, clientData)
    Blt_ListItem *iPtr;	/* Pointer to the entry */
    ClientData clientData;	/* Data attached to key */
{
    iPtr->clientData = clientData;
}

#endif


#ifdef notdef
/*
 *----------------------------------------------------------------------
 *
 * Blt_List_Index --
 *
 *	Find the position of an entry in the list.
 *
 * Results:
 *	Returns the index to the entry.  If no entry matching
 *	the key given is found, then -1 is returned.
 *
 *----------------------------------------------------------------------
 */
int
Blt_ItemIndex(listPtr, searchPtr)
    Blt_List *listPtr;		/* List to search */
    Blt_ListItem *searchPtr;	/* Entry to match */
{
    register int count = 0;
    register Blt_ListItem *iPtr;	/* Entry to match */

    for (iPtr = listPtr->headPtr; iPtr != NULL;
	iPtr = iPtr->nextPtr) {
	if (searchPtr == iPtr)
	    return (count);
	count++;
    }
    return (-1);
}

#endif

/*
 *----------------------------------------------------------------------
 *
 * Blt_FindItem --
 *
 *	Find the first entry matching the key given.
 *
 * Results:
 *	Returns the pointer to the entry.  If no entry matching
 *	the key given is found, then NULL is returned.
 *
 *----------------------------------------------------------------------
 */
Blt_ListItem *
Blt_FindItem(listPtr, searchKey)
    Blt_List *listPtr;		/* List to search */
    char *searchKey;		/* Key to match */
{
    register Blt_ListItem *iPtr;
    Tk_Uid newPtr;

    newPtr = searchKey;
    if (listPtr->type == TCL_STRING_KEYS) {
	newPtr = Tk_GetUid(searchKey);
    }
    for (iPtr = listPtr->headPtr; iPtr != NULL;
	iPtr = iPtr->nextPtr) {
	if (newPtr == iPtr->keyPtr)
	    return (iPtr);
    }
    return (Blt_ListItem *) NULL;
}

#ifdef notdef
/*
 *----------------------------------------------------------------------
 *
 * Blt_FirstListItem --
 *
 *	Find the first entry in the list and return its pointer.
 *	In addition, update the given search pointer.
 *
 * Results:
 *	Returns a pointer to the first entry in the list. If the
 *      list is empty, NULL is returned.  The search pointer (used in
 *	subsequent searches) is set to the appropriate value.
 *
 *----------------------------------------------------------------------
 */
Blt_ListItem *
Blt_FirstListItem(listPtr, iPtrPtr)
    Blt_List *listPtr;		/* The list we are searching */
    Blt_ListItem **iPtrPtr;	/* Search pointer to set */
{
    if (listPtr == (Blt_List *)NULL)
	return (Blt_ListItem *) NULL;

    if (listPtr->headPtr == (Blt_ListItem *)NULL)
	return (Blt_ListItem *) NULL;

    if (iPtrPtr != (Blt_ListItem **)NULL) {
	*iPtrPtr = listPtr->headPtr;
    }
    return (listPtr->headPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_LastListItem --
 *
 *	Find the last entry in the list using the given entry as
 *	a cursor to the last entry and return its pointer.
 *	In addition, the cursor position is updated.
 *
 * Results:
 *	A pointer to the last object in the list is returned. If the
 *      list is at end, NULL is returned.  The search pointer (used in
 *	subsequent searches) is set to the appropriate value.
 *
 *----------------------------------------------------------------------
 */
Blt_ListItem *
Blt_LastListItem(iPtrPtr)
    Blt_ListItem **iPtrPtr;	/* Search pointer of current position */
{
    if ((iPtrPtr == (Blt_ListItem **)NULL) ||
	(*iPtrPtr == (Blt_ListItem *)NULL)) {
	return (Blt_ListItem *) NULL;
    }
    *iPtrPtr = (*iPtrPtr)->prevPtr;
    return (*iPtrPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_NextItem --
 *
 *	Find the next entry in the list using the given search pointer
 *	as the current location and return its pointer.
 *	In addition, update the given search pointer.
 *
 * Results:
 *	A pointer to the next object in the list is returned. If the
 *      list is at end, NULL is returned.  The search pointer (used in
 *	subsequent searches) is set to the appropriate value.
 *
 *----------------------------------------------------------------------
 */
Blt_ListItem *
Blt_NextItem(iPtrPtr)
    Blt_ListItem **iPtrPtr;	/* Search pointer indicates current position */
{
    if ((iPtrPtr == NULL) || (*iPtrPtr == NULL)) {
	return (Blt_ListItem *) NULL;
    }
    *iPtrPtr = (*iPtrPtr)->nextPtr;
    return (*iPtrPtr);
}

#endif
/*
 *----------------------------------------------------------------------
 *
 * Blt_DeleteItem --
 *
 *	Find the entry and free the memory allocated for the entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_DeleteItem(iPtr)
    Blt_ListItem *iPtr;
{
    Blt_UnlinkItem(iPtr);
    Blt_FreeItem(iPtr);
}
