/* 
 * This file contains the main program for "tkined". It is heavily
 * based on main.c from the tk distribution by John Ousterhout.
 *
 * Copyright (c) 1993, 1994, 1995
 *
 * J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
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

extern int Blt_Init _ANSI_ARGS_((Tcl_Interp *interp));

extern char* Editor_tcl;
extern char* Diagram_tcl;
extern char* Command_tcl;
extern char* Tool_tcl;
extern char* Objects_tcl;
extern char* Html_tcl;
extern char* Event_tcl;
extern char* Dialog_tcl;
extern char* Help_tcl;
extern char* Misc_tcl;

/*
 * Global variables. The Interpreter and a window that is not used :-)
 */

Tk_Window w;		        /* The main window for the application.  If
            			 * NULL then the application no longer
			         * exists. */
static Tcl_Interp *interp;	/* Interpreter for this application. */

/*
 * Command-line options:
 */

int synchronize = 0;
int tki_Debug = 0;
char *display = NULL;
char *geometry = NULL;

Tk_ArgvInfo argTable[] = {
    {"-geometry", TK_ARGV_STRING, (char *) NULL, (char *) &geometry,
	"Initial geometry for window"},
    {"-display", TK_ARGV_STRING, (char *) NULL, (char *) &display,
	"Display to use"},
    {"-sync", TK_ARGV_CONSTANT, (char *) 1, (char *) &synchronize,
	"Use synchronous mode for display server"},
    {"-debug", TK_ARGV_CONSTANT, (char *) 1, (char *) &tki_Debug,
	"Turn on the debugging output"},
    {(char *) NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
	(char *) NULL}
};

/*
 * The Tcl_AppInit() function as suggested by J. Ousterhout.
 */

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;
{
    if (Tcl_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }

    if (Tk_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }

#ifdef HAVE_BLT
    if (Blt_Init(interp) != TCL_OK) {
	return TCL_ERROR;
    }
#endif

    if (Tkined_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }

    return TCL_OK;
}

/*
 * Main program for tkined.
 */

int
main(argc, argv)
    int argc;				/* Number of arguments. */
    char **argv;			/* Array of argument strings. */
{
    char buf[20];
    int done = 0;
    char *file;

    /* Create and initialize the interpreter */

    interp = Tcl_CreateInterp();
#ifdef TCL_MEM_DEBUG
    Tcl_InitMemory(interp);
#endif

    /*
     * Parse command-line arguments.
     */

    if (Tk_ParseArgv(interp, (Tk_Window) NULL, &argc, argv, argTable, 0)
	    != TCL_OK) {
	fprintf(stderr, "%s\n", interp->result);
	exit(1);
    }

    /*
     * If a display was specified, put it into the DISPLAY
     * environment variable so that it will be available for
     * any sub-processes created by us.
     */

    if (display != NULL) {
        Tcl_SetVar2(interp, "env", "DISPLAY", display, TCL_GLOBAL_ONLY);
    }

    /*
     * Initialize the Tk application and arrange to map the main window
     * after the startup script has been executed, if any.  This way
     * the script can withdraw the window so it isn't ever mapped
     * at all.
     */

    w = Tk_CreateMainWindow(interp, display, "tkined", "tkined");
    if (w == NULL) {
	fprintf(stderr, "%s\n", interp->result);
	exit(1);
    }
    Tk_SetClass(w, "Tkined");
    if (synchronize) {
	XSynchronize(Tk_Display(w), True);
    }

    /*
     * Set the geometry of the main window, if requested.  Put the
     * requested geometry into the "geometry" variable.
     */

    if (geometry != NULL) {
	int code;
        Tcl_SetVar(interp, "geometry", geometry, TCL_GLOBAL_ONLY);
        code = Tcl_VarEval(interp, "wm geometry . ", geometry, (char *) NULL);
        if (code != TCL_OK) {
            fprintf(stderr, "%s\n", interp->result);
        }
    }

    /*
     * Set up the global tkined variables tkined_version, tkined_path
     * and tkined_debug.
     */

    Tcl_SetVar (interp, "tkined_version", TKINED_VERSION, TCL_GLOBAL_ONLY);
    sprintf (buf, "%d", tki_Debug);
    Tcl_SetVar (interp, "tkined_debug", buf, TCL_GLOBAL_ONLY);
    Tcl_SetVar (interp, "tkined_lib", TKINEDLIB, TCL_GLOBAL_ONLY);
    if (geometry != NULL) {
	Tcl_SetVar (interp, "geometry", geometry, TCL_GLOBAL_ONLY);
    }

    /*
     * Set the "tcl_interactive" variable.
     */

    Tcl_SetVar(interp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);

    if (Tcl_AppInit(interp) != TCL_OK) {
	fprintf (stderr, "%s\n", interp->result);
        return 1;
    }

    /*
     * Initialize the stripchart and barchart canvas objects.
     */

    Tk_CreateItemType((Tk_ItemType *) &TkStripchartType);
    Tk_CreateItemType((Tk_ItemType *) &TkBarchartType);

    /*
     * Execute the initialization scripts.
     */

#ifdef AUTO_LOAD
    if (Tcl_EvalFile(interp, "Editor.tcl") != TCL_OK) 
	    fprintf (stderr, "%s\n", interp->result);
    if (Tcl_EvalFile(interp, "Diagram.tcl") != TCL_OK) 
	    fprintf (stderr, "%s\n", interp->result);
    if (Tcl_EvalFile(interp, "Command.tcl") != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_EvalFile(interp, "Tool.tcl") != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_EvalFile(interp, "Objects.tcl") != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_EvalFile(interp, "Html.tcl") != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_EvalFile(interp, "Event.tcl") != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_EvalFile(interp, "Dialog.tcl") != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_EvalFile(interp, "Help.tcl") != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_EvalFile(interp, "Misc.tcl") != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
#else
    if (Tcl_Eval(interp, Editor_tcl) != TCL_OK) 
	    fprintf (stderr, "%s\n", interp->result);
    if (Tcl_Eval(interp, Diagram_tcl) != TCL_OK) 
	    fprintf (stderr, "%s\n", interp->result);
    if (Tcl_Eval(interp, Command_tcl) != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_Eval(interp, Tool_tcl) != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_Eval(interp, Objects_tcl) != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_Eval(interp, Html_tcl) != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_Eval(interp, Event_tcl) != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_Eval(interp, Dialog_tcl) != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_Eval(interp, Help_tcl) != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
    if (Tcl_Eval(interp, Misc_tcl) != TCL_OK)
            fprintf (stderr, "%s\n", interp->result);
#endif

    if (access ("init.tcl", R_OK) == 0) {
	if (Tcl_EvalFile(interp, "init.tcl") != TCL_OK)
	  fprintf (stderr, "%s\n", interp->result);
    } else {
	buffersize (strlen (TKINEDLIB) + 20);
	strcpy (buffer, TKINEDLIB);
	strcat (buffer, "/init.tcl");
	if (access (buffer, R_OK) == 0) {
	    if (Tcl_EvalFile(interp, buffer) != TCL_OK)
	      fprintf (stderr, "%s\n", interp->result);
	}
    }

    (void) Tcl_Eval(interp, "wm withdraw .");
    (void) Tcl_Eval(interp, "update");

    /*
     * Open a view for every readable file given on the command line.
     */

    for (file = *(++argv); file != NULL; file = *(++argv)) {
	if (   (access(file, R_OK) < 0)
	    && (strncmp (file, "ftp:", 4) != 0)
	    && (strncmp (file, "file:", 5) != 0) ) {
	    fprintf(stderr, "tkined: unable to open %s\n", file);
	    continue;
	}
	(void) Tcl_Eval (interp, "EDITOR");
	(void) Tcl_VarEval (interp, "Command::Open ",
			   interp->result, " ", file, (char *) NULL);
	done++;
    }
    fflush(stdout);

    if (!done) {
	(void) Tcl_Eval(interp, "EDITOR");
    }

    /*
     * Loop infinitely, waiting for commands to execute.  When there
     * are no windows left, Tk_MainLoop returns and we clean up and
     * exit.
     */

    Tk_MainLoop();
    Tcl_DeleteInterp(interp);
    return 0;
}
