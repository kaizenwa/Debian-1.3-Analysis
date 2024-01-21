/*
 * flash.c
 *
 * Some utility functions to implement flashing icons.
 *
 * Copyright (c) 1994, 1995
 *
 * J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include "tkined.h"

typedef struct FlashItem {
    char *id;
    struct FlashItem *nextPtr;
} FlashItem;

static FlashItem *flashList = NULL;

/*
 * Forward declarations for procedures defined later in this file:
 */

static void
FlashProc		_ANSI_ARGS_((ClientData clientData));

/*
 * The FlashProc callback flashes every object in the flashList 
 * and calls it again if there are any jobs left.
 */

static void
FlashProc (clientData)
    ClientData clientData;
{
    Tcl_Interp *interp = (Tcl_Interp *) clientData;
    Tki_Object *object;
    char *color;
    FlashItem *p;

    int max = 0;

    for (p = flashList; p != NULL; p = p->nextPtr) {

	if (p->id == NULL) continue;

	object = Tki_LookupObject (p->id);
	if (object == NULL) continue;

	if (! object->editor->color) {
	    if ((object->flash) % 2) {
		color = "black";
	    } else {
		color = "white";
	    }
	} else {
	    color = object->color;
	    if ((object->flash) % 2) {
		if (strcasecmp(color, "white") == 0) color = "black";
	    } else {
		color = "white";
	    }
	}

	Tcl_VarEval (interp, type_to_string (object->type),
		     "::color ", object->id, " ", color, 
		     (char *) NULL);

	object->flash -= 1;

	if (object->flash == 0) {
	    notrace (m_color, interp, object, 1, &object->color);
	    ckfree (p->id);
	    p->id = NULL;
	}

	max = ( object->flash > max ) ? object->flash : max;
    }

    if (max <= 0) {      /* everything is done - remove the flashList */

	FlashItem *q;

        for (p = flashList; p != NULL; p = q) {
            q = p->nextPtr;
            if (p->id != NULL) ckfree (p->id);
            ckfree ((char *) p);
        }

        flashList = NULL;
    }

    Tcl_Eval (interp, "update");

    if (max > 0) {
	Tk_CreateTimerHandler (500, FlashProc, (ClientData) interp);
    }
}

/*
 * Add a new flash node to our list of flashing objects.
 * Start flash callback if we create the first entry.
 */

void
flash (interp, object)
    Tcl_Interp *interp;
    Tki_Object *object;
{
    FlashItem *p;

    if (flashList == NULL) {
	
        flashList = (FlashItem *) ckalloc (sizeof(FlashItem));
	p = flashList;
	p->id = ckstrdup(object->id);
	p->nextPtr = NULL;
	Tk_CreateTimerHandler (500, FlashProc, (ClientData) interp);

    } else {

	/* 
	 * Move to the end of the list and check if it exists already.
	 */

	for (p = flashList; p->nextPtr != NULL; p = p->nextPtr) {
	    if (p->id && strcmp (p->id, object->id) == 0) return;
	}
	if (p->id && strcmp (p->id, object->id) == 0) {
	    return;
	}

	/* 
	 * Create a new entry for the flash list.
	 */

        p->nextPtr = (FlashItem *) ckalloc (sizeof(FlashItem));
	p = p->nextPtr;
	p->id = ckstrdup(object->id);
	p->nextPtr = NULL;
    }
}
