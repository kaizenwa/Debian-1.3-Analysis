/* 
 * tkXAppInit.c --
 *
 *      Provides a default version of the Tcl_AppInit procedure for use with
 *      applications built with Extended Tcl and Tk.  This is based on the
 *      the UCB Tk file tkAppInit.c
 *
 *-----------------------------------------------------------------------------
 * Copyright 1991-1995 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tkXAppInit.c,v 5.0 1995/07/25 06:00:44 markd Rel $
 *-----------------------------------------------------------------------------
 */

#include "tclExtend.h"
#include "tk.h"

/*
 * The following variable is a special hack that insures the tcl
 * version of matherr() is used when linking against shared libraries
 * Even if matherr is not used on this system, there is a dummy version
 * in libtcl.
 */
EXTERN int matherr ();
int (*tclDummyMathPtr)() = matherr;


/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *----------------------------------------------------------------------
 */
#ifdef __cplusplus
int
main (int    argc,
      char **argv)
#else
int
main (argc, argv)
    int    argc;
    char **argv;
#endif
{
    TkX_Main(argc, argv, Tcl_AppInit);
    return 0;			/* Needed only to prevent compiler warning. */
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *----------------------------------------------------------------------
 */
#ifdef __cplusplus
int
Tcl_AppInit (Tcl_Interp *interp)
#else
int
Tcl_AppInit (interp)
    Tcl_Interp *interp;
#endif
{
    Tk_Window main;

    main = Tk_MainWindow(interp);

    /*
     * Call the init procedures for included packages.  Each call should
     * look like this:
     *
     * if (Mod_Init(interp) == TCL_ERROR) {
     *     return TCL_ERROR;
     * }
     *
     * where "Mod" is the name of the module.
     */

    if (TclX_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (main != NULL) {
        if ((TkX_Init(interp) == TCL_ERROR)) {
            return TCL_ERROR;
        }
    }

    /*
     * Call Tcl_CreateCommand for application-specific commands, if
     * they weren't already created by the init procedures called above.
     */

    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  Typically the startup file is "~/.apprc"
     * where "app" is the name of the application.  If this line is deleted
     * then no user-specific startup file will be run under any conditions.
     */

    tcl_RcFileName = "~/.tclrc";
    return TCL_OK;
}
