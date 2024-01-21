/*
 * tclXlibInit.c
 *
 * Function to add the Extented Tcl library commands into an interpreter. This
 * also sets the Tcl auto_path, tcl_library and tclx_library variable.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1996 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tclXlibInit.c,v 7.0 1996/06/16 05:30:35 markd Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"


/*
 *-----------------------------------------------------------------------------
 * Tclxlib_Init --
 *
 *   Initialize the Extended Tcl library facility commands.
 *-----------------------------------------------------------------------------
 */
int
Tclxlib_Init (interp)
    Tcl_Interp *interp;
{
    TclX_LibraryInit (interp);

    return TCL_OK;
}

