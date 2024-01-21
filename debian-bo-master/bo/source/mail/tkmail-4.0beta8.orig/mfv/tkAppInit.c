/*
 * tkAppInit.c --
 *
 *      Provides a default version of the Tcl_AppInit procedure for use with
 *      applications built with Extended Tcl and Tk.  This is based on the
 *      the UCB Tk file tkAppInit.c and Extended Tcl file tkXAppInit.c
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
 */

#ifdef HAVE_TCLX
#include "tclExtend.h"
#endif
#include "tk.h"

/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

extern int matherr();
int *tclDummyMathPtr = (int *) matherr;


/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
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
#ifdef HAVE_TCLX
    TkX_Main(argc, argv, Tcl_AppInit);
#else
    Tk_Main(argc, argv, Tcl_AppInit);
#endif
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
 *
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
#ifdef HAVE_TCLX
    Tk_Window main;

    main = Tk_MainWindow(interp);

    if (TclX_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
    if (main != NULL) {
        if ((TkX_Init(interp) == TCL_ERROR)) {
            return TCL_ERROR;
        }
    }
#else
    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (Tcl_InitSignalHandling (interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
#endif
    
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
    
    if (Mfv_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
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

#if (TCL_MINOR_VERSION < 5)
    tcl_RcFileName = "~/.wishrc";
#else
    Tcl_SetVar(interp, "tcl_rcFileName", "~/.wishrc", TCL_GLOBAL_ONLY);
#endif
    return TCL_OK;
}
