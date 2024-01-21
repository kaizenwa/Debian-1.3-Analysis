/* 
 * tkXAppInit.c --
 *
 * Provides a default version of the Tcl_AppInit procedure for use with
 * applications built with Extended Tcl and Tk on Windows 95/NT systems.
 * This is based on the the UCB Tk file tkAppInit.c
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
 * $Id: tkXAppInit.c,v 8.0 1996/11/21 00:25:40 markd Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtend.h"
#include "tk.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <malloc.h>
#include <locale.h>

/*
 * The following declarations refer to internal Tk routines.  These
 * interfaces are available for use, but are not supported.
 */

EXTERN void
TkConsoleCreate (void);

EXTERN int
TkConsoleInit (Tcl_Interp *interp);


/*-----------------------------------------------------------------------------
 * WinMain --
 *
 * This is the main program for the application.
 *-----------------------------------------------------------------------------
 */
int APIENTRY
WinMain(hInstance, hPrevInstance, lpszCmdLine, nCmdShow)
    HINSTANCE hInstance;
    HINSTANCE hPrevInstance;
    LPSTR lpszCmdLine;
    int nCmdShow;
{
    char **argv;
    int argc;
    char buffer [MAX_PATH];

    /*
     * Set up the default locale to be standard "C" locale so parsing
     * is performed correctly.
     */
    setlocale(LC_ALL, "C");

    /*
     * Increase the application queue size from default value of 8.
     * At the default value, cross application SendMessage of WM_KILLFOCUS
     * will fail because the handler will not be able to do a PostMessage!
     * This is only needed for Windows 3.x, since NT dynamically expands
     * the queue.
     */
    SetMessageQueue(64);

    /*
     * Create the console channels and install them as the standard
     * channels.  All I/O will be discarded until TkConsoleInit is
     * called to attach the console to a text widget.
     */
    TkConsoleCreate();

    /*
     * Parse the command line. Since Windows programs don't get passed the
     * command name as the first argument, we need to fetch it explicitly.
     */
    TclX_SplitWinCmdLine (&argc, &argv);
    GetModuleFileName (NULL, buffer, sizeof (buffer));
    argv[0] = buffer;

    TkX_Main(argc, argv, Tcl_AppInit);

    return 0;                   /* Needed only to prevent compiler warning. */
}

/*-----------------------------------------------------------------------------
 * Tcl_AppInit --
 *
 * This procedure performs application-specific initialization. Most
 * applications, especially those that incorporate additional packages, will
 * have their own version of this procedure.
 *
 * Results:
 *    Returns a standard Tcl completion code, and leaves an error message in
 * interp->result if an error occurs.
 *-----------------------------------------------------------------------------
 */
int
Tcl_AppInit (Tcl_Interp *interp)
{
    if (Tcl_Init (interp) == TCL_ERROR) {
        goto errorExit;
    }

    if (Tclx_Init(interp) == TCL_ERROR) {
        goto errorExit;
    }
    Tcl_StaticPackage(interp, "Tclx", Tclx_Init, Tclx_SafeInit);

    if (Tk_Init(interp) == TCL_ERROR) {
        goto errorExit;
    }
    Tcl_StaticPackage(interp, "Tk", Tk_Init, (Tcl_PackageInitProc *) NULL);

    if (Tkx_Init(interp) == TCL_ERROR) {
        goto errorExit;
    }
    Tcl_StaticPackage(interp, "Tkx", Tkx_Init, (Tcl_PackageInitProc *) NULL);

    /*
     * Initialize the console for interactive applications.
     */
    if (TkX_ConsoleInit (interp) == TCL_ERROR)
        goto errorExit;

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
    Tcl_SetVar(interp, "tcl_rcFileName", "~/.wishxrc", TCL_GLOBAL_ONLY);
    return TCL_OK;

  errorExit:
    TkX_Panic ("%s\n", interp->result);
}
