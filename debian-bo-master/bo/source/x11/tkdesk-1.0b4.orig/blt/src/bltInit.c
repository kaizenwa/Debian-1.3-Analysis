/*
 * bltInit.c --
 *
 * Copyright 1993-1996 by AT&T Bell Laboratories.
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

#include <bltInt.h>

#ifndef BLT_VERSION
#define BLT_VERSION "2.0"
#endif

#if (TCL_MAJOR_VERSION == 7) && (TCL_MINOR_VERSION < 4)
typedef int (Tcl_AppInitProc) _ANSI_ARGS_((Tcl_Interp *interp));
#endif

#if (TK_MAJOR_VERSION == 3)
/* 
 * No image support for tiling versions of Tk widgets.
 */
#define NO_TILEFRAME
#define NO_TILEBUTTON
#define NO_TILESCROLLBAR
#else 
/* 
 * Remove redundant or unneeded commands
 */
#define NO_CUTBUFFER
#define NO_BELL
#endif /* TK_MAJOR_VERSION == 3 */

/*
 * The inclusion of contributed commands/widgets can be suppressed by
 * defining the respective preprocessor symbol.
 */

/* Exclude commands not needed by TkDesk: */

#define NO_HTEXT
#define NO_GRAPH
#define NO_TABLE
#define NO_WINOP
#define NO_BITMAP
#define NO_DEBUG
#define NO_WATCH
#define NO_VECTOR
#define NO_SPLINE
#define NO_BELL
#define NO_CUTBUFFER
#define NO_TILEFRAME
#define NO_TILEBUTTON
#define NO_TILESCROLLBAR
#ifndef NO_TED
#define NO_TED
#endif

#ifndef NO_HTEXT
extern Tcl_AppInitProc Blt_HtextInit;
#endif
#ifndef NO_GRAPH
extern Tcl_AppInitProc Blt_GraphInit;
#endif
#ifndef NO_TABLE
extern Tcl_AppInitProc Blt_TableInit;
#endif
#ifndef NO_BUSY
extern Tcl_AppInitProc Blt_BusyInit;
#endif
#ifndef NO_WINOP
extern Tcl_AppInitProc Blt_WinOpInit;
#endif
#ifndef NO_BITMAP
extern Tcl_AppInitProc Blt_BitmapInit;
#endif
#ifndef NO_BGEXEC
extern Tcl_AppInitProc Blt_BgExecInit;
#endif
#ifndef NO_DRAGDROP
extern Tcl_AppInitProc Blt_DragDropInit;
#endif
#ifndef NO_DEBUG
extern Tcl_AppInitProc Blt_DebugInit;
#endif
#ifndef NO_WATCH
extern Tcl_AppInitProc Blt_WatchInit;
#endif
#ifndef NO_VECTOR
extern Tcl_AppInitProc Blt_VectorInit;
#endif
#ifndef NO_SPLINE
extern Tcl_AppInitProc Blt_SplineInit;
#endif
#ifndef NO_BELL
extern Tcl_AppInitProc Blt_BellInit;
#endif
#ifndef NO_CUTBUFFER
extern Tcl_AppInitProc Blt_CutbufferInit;
#endif
#ifndef NO_TILEFRAME
extern Tcl_AppInitProc Blt_FrameInit;
#endif
#ifndef NO_TILEBUTTON
extern Tcl_AppInitProc Blt_ButtonInit;
#endif
#ifndef NO_TILESCROLLBAR
extern Tcl_AppInitProc Blt_ScrollbarInit;
#endif
#ifndef NO_TED
extern Tcl_AppInitProc Blt_TedInit;
#endif

static Tcl_AppInitProc *initProcArr[] =
{
#ifndef NO_HTEXT
    Blt_HtextInit,
#endif
#ifndef NO_GRAPH
    Blt_GraphInit,
#endif
#ifndef NO_TABLE
    Blt_TableInit,
#endif
#ifndef NO_BUSY
    Blt_BusyInit,
#endif
#ifndef NO_WINOP
    Blt_WinOpInit,
#endif
#ifndef NO_BITMAP
    Blt_BitmapInit,
#endif
#ifndef NO_BGEXEC
    Blt_BgExecInit,
#endif
#ifndef NO_DRAGDROP
    Blt_DragDropInit,
#endif
#ifndef NO_DEBUG
    Blt_DebugInit,
#endif
#ifndef NO_WATCH
    Blt_WatchInit,
#endif
#ifndef NO_VECTOR
    Blt_VectorInit,
#endif
#ifndef NO_SPLINE
    Blt_SplineInit,
#endif
#ifndef NO_BELL
    Blt_BellInit,
#endif
#ifndef NO_CUTBUFFER
    Blt_CutbufferInit,
#endif
#ifndef NO_TILEFRAME
    Blt_FrameInit,
#endif
#ifndef NO_TILEBUTTON
    Blt_ButtonInit,
#endif
#ifndef NO_TILESCROLLBAR
    Blt_ScrollbarInit,
#endif
#ifndef NO_TED
    Blt_TedInit,
#endif
    (Tcl_AppInitProc *) NULL
};

#ifdef ITCL_NAMESPACES
static char envVar[] = "::env";
#else
static char envVar[] = "env";
#endif

/*LINTLIBRARY*/
int
Blt_Init(interp)
    Tcl_Interp *interp;		/* Interpreter to add extra commands */
{
    char *libDir;
    register Tcl_AppInitProc **procPtrPtr;
#ifdef ITCL_NAMESPACES
    Itcl_Namespace spaceId;	/* Token for "blt" namespace created, used
				 * to delete the namespace on errors. */
#endif

    libDir = Tcl_GetVar2(interp, envVar, "BLT_LIBRARY", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	libDir = BLT_LIBRARY;
    }
    if (Tcl_SetVar(interp, "blt_library", libDir, TCL_GLOBAL_ONLY) == NULL) {
	return TCL_ERROR;
    }
    if (Tcl_SetVar2(interp, "blt_versions", "BLT", BLT_VERSION,
	    TCL_GLOBAL_ONLY) == NULL) {
	return TCL_ERROR;
    }
#ifdef ITCL_NAMESPACES
    if (Itcl_CreateNamesp(interp, "blt", (ClientData)0, (Itcl_DeleteProc *)0,
		  &spaceId) != TCL_OK) {
	return TCL_ERROR;
    }
#endif
    for (procPtrPtr = initProcArr; *procPtrPtr != NULL; procPtrPtr++) {
	if ((**procPtrPtr) (interp) != TCL_OK) {
#ifdef ITCL_NAMESPACES
	    Itcl_DeleteNamesp(spaceId);
#endif
	    return TCL_ERROR;
	}
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_InitCmd --
 *
 *      Given the name of a command, return a pointer to the
 *      clientData field of the command.
 *
 * Results:
 *      A standard TCL result. If the command is found, TCL_OK
 *	is returned and clientDataPtr points to the clientData
 *	field of the command (if the clientDataPtr in not NULL).
 *
 * Side effects:
 *      If the command is found, clientDataPtr is set to the address
 *	of the clientData of the command.  If not found, an error
 *	message is left in interp->result.
 *
 *----------------------------------------------------------------------
 */
int
Blt_InitCmd(interp, specPtr)
    Tcl_Interp *interp;
    Blt_CmdSpec *specPtr;
{
    Tk_Window tkwin;
    Tcl_CmdInfo cmdInfo;
    char cmdPath[200];

#ifdef ITCL_NAMESPACES
    sprintf(cmdPath, "blt::%s", specPtr->name);
#else
    strcpy(cmdPath, specPtr->name);
#endif
    if (Tcl_GetCommandInfo(interp, cmdPath, &cmdInfo)) {
	return TCL_OK;		/* Assume command was already initialized */
    }
    tkwin = Tk_MainWindow(interp);
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "\"", cmdPath, "\" requires Tk", (char *)NULL);
	return TCL_ERROR;
    }
    if (specPtr->clientData == (ClientData)0) {
	specPtr->clientData = (ClientData)tkwin;
    }
    Tcl_SetVar2(interp, "blt_versions", specPtr->name, specPtr->version,
	TCL_GLOBAL_ONLY);
    Tcl_CreateCommand(interp, cmdPath, specPtr->cmdProc, specPtr->clientData, 
	specPtr->cmdDeleteProc);
    return TCL_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * Blt_InitCmds --
 *
 *      Given the name of a command, return a pointer to the
 *      clientData field of the command.
 *
 * Results:
 *      A standard TCL result. If the command is found, TCL_OK
 *	is returned and clientDataPtr points to the clientData
 *	field of the command (if the clientDataPtr in not NULL).
 *
 * Side effects:
 *      If the command is found, clientDataPtr is set to the address
 *	of the clientData of the command.  If not found, an error
 *	message is left in interp->result.
 *
 *----------------------------------------------------------------------
 */
int
Blt_InitCmds(interp, specPtr, numCmds)
    Tcl_Interp *interp;
    Blt_CmdSpec *specPtr;
    int numCmds;
{
    register int i;

    for (i = 0; i < numCmds; i++) {
	if (Blt_InitCmd(interp, specPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	specPtr++;
    }
    return TCL_OK;
}





