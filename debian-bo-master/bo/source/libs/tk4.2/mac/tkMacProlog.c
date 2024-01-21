/* 
 * tkMacProlog.c --
 *
 *	Implements a method on the Macintosh to get the prolog
 *	from the resource fork of our application (or the shared
 *	library).
 *
 * Copyright (c) 1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tkMacProlog.c 1.3 96/10/03 14:33:42
 */

#include "tkInt.h"
#include "tclMacInt.h"
#include <Resources.h>

/*
 *--------------------------------------------------------------
 *
 * TkGetNativeProlog --
 *
 *	Locate and load the postscript prolog from the resource
 *	fork of the application.  If it can't be found then we
 *	will try looking for the file in the system folder.
 *
 * Results:
 *	A standard Tcl Result.  If everything is OK the prolog
 *	will be located in the result string of the interpreter.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

int
TkGetNativeProlog(
    Tcl_Interp *interp)		/* Places the prolog in the result. */
{
    Handle resource;
    char *stringPtr;

    resource = TclMacFindResource(interp, "TEXT", "prolog", -1, NULL);
			    
    if (resource != NULL) {
	stringPtr = TclMacConvertTextResource(resource);
	Tcl_SetResult(interp, stringPtr, TCL_DYNAMIC);
	ReleaseResource(resource);
	return TCL_OK;
    } else {
	return TkGetProlog(interp);
    }
}
