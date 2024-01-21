/* 
 * tclXmsgcat.c --
 *
 *      Contains commands for accessing XPG/3 message catalogs.  If real XPG/3
 * message catalogs are not available, the default string is returned.
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
 * $Id: tclXmsgcat.c,v 5.0 1995/07/25 05:42:49 markd Rel $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"

#ifdef HAVE_CATGETS

#include <nl_types.h>

#else

typedef int nl_catd;

#endif /* HAVE_CATGETS */

static int
ParseFailOption _ANSI_ARGS_((Tcl_Interp *interp,
                             CONST char *optionStr,
                             int        *failPtr));

static int
CatOpFailed _ANSI_ARGS_((Tcl_Interp *interp,
                         CONST char *errorMsg));

static void
MsgCatCleanUp _ANSI_ARGS_((ClientData  clientData,
                           Tcl_Interp *interp));

/*
 * Message catalog table is global, so it is shared between all interpreters
 * in the same process.
 */
static void_pt msgCatTblPtr = NULL;

#ifndef HAVE_CATGETS

/*
 *-----------------------------------------------------------------------------
 * catopen --
 *
 *   A stub to use when message catalogs are not available.  Always returns
 * the default string.
 *-----------------------------------------------------------------------------
 */
static nl_catd
catopen (name, oflag)
    char *name;
    int   oflag;
{
    return (nl_catd) -1;
}

/*
 *-----------------------------------------------------------------------------
 * catgets --
 *
 *   A stub to use when message catalogs are not available. Always returns -1.
 *-----------------------------------------------------------------------------
 */
static char *
catgets (catd, set_num, msg_num, defaultStr)
    nl_catd catd;
    int     set_num, msg_num;
    char   *defaultStr;
{
    return defaultStr;
}

/*
 *-----------------------------------------------------------------------------
 * catclose --
 *
 *   A stub to use when message catalogs are not available. Always returns -1.
 *-----------------------------------------------------------------------------
 */
static int
catclose (catd)
    nl_catd catd;
{
    return -1;
}
#endif /* HAVE_CATGETS */

/*
 *-----------------------------------------------------------------------------
 * ParseFailOption --
 *
 *   Parse the -fail/-nofail option, if specified.
 *-----------------------------------------------------------------------------
 */
static int
ParseFailOption (interp, optionStr, failPtr)
    Tcl_Interp *interp;
    CONST char *optionStr;
    int        *failPtr;
{
    if (STREQU ("-fail", ((char *) optionStr)))
        *failPtr = TRUE;
    else if (STREQU ("-nofail", ((char *) optionStr)))
        *failPtr = FALSE;
    else {
        Tcl_AppendResult (interp, "Expected option of `-fail' or ",
                          "`-nofail', got: `", optionStr, "'",
                          (char *) NULL);
        return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 * CatOpFailed --
 *
 *    Handles failures of catopen and catclose.  If message catalogs are
 * available, if returns the supplied message.  If message are not
 * available, it returns a message indicating that message stubs are used.
 * It is not specified by XPG/3 how to get the details of a message catalog
 * open or close failure. Always returns TCL_ERROR;
 *-----------------------------------------------------------------------------
 */
static int
CatOpFailed (interp, errorMsg)
    Tcl_Interp *interp;
    CONST char *errorMsg;
{
#ifdef HAVE_CATGETS

    Tcl_AppendResult (interp, errorMsg, (char *) NULL);

#else

    Tcl_AppendResult (interp, "the message catalog facility is not available,",
                      " default string is always returned", (char *) NULL);

#endif /* HAVE_CATGETS */

    return TCL_ERROR;
}

/*
 *-----------------------------------------------------------------------------
 * Tcl_CatopenCmd --
 *
 *    Implements the TCL echo command:
 *        catopen ?-fail|-nofail? catname
 *-----------------------------------------------------------------------------
 */
static int
Tcl_CatopenCmd (clientData, interp, argc, argv)
    ClientData  clientData;
    Tcl_Interp *interp;
    int         argc;
    char      **argv;
{
    int      fail;
    nl_catd  catDesc;
    nl_catd *catDescPtr;

    if ((argc < 2) || (argc > 3)) {
        Tcl_AppendResult (interp, tclXWrongArgs, argv [0],
                          " ?-fail|-nofail? catname",
                          (char *) NULL);
        return TCL_ERROR;
    }
    if (argc == 3) {
        if (ParseFailOption (interp, argv [1], &fail) != TCL_OK)
            return TCL_ERROR;
    } else
        fail = FALSE;

    catDesc = catopen (argv [argc - 1], 0);
    if ((catDesc == (nl_catd) -1) && fail)
        return CatOpFailed (interp, "open of message catalog failed");

    catDescPtr = (nl_catd *) Tcl_HandleAlloc (msgCatTblPtr, interp->result);
    *catDescPtr = catDesc;

    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 * Tcl_CatgetsCmd --
 *
 *    Implements the TCL echo command:
 *        catgets catHandle setnum msgnum defaultstr
 *-----------------------------------------------------------------------------
 */
static int
Tcl_CatgetsCmd (clientData, interp, argc, argv)
    ClientData  clientData;
    Tcl_Interp *interp;
    int         argc;
    char      **argv;
{
    nl_catd   *catDescPtr;
    int        msgSetNum, msgNum;
    char      *localMsg;

    if (argc != 5) {
        Tcl_AppendResult (interp, tclXWrongArgs, argv [0],
                          " catHandle setnum msgnum ",
                          "defaultstr", (char *) NULL);
        return TCL_ERROR;
    }
    catDescPtr = (nl_catd *) Tcl_HandleXlate (interp, msgCatTblPtr, argv [1]);
    if (catDescPtr == NULL)
        return TCL_ERROR;
    if (Tcl_GetInt (interp, argv [2], &msgSetNum) != TCL_OK)
        return TCL_ERROR;
    if (Tcl_GetInt (interp, argv [3], &msgNum) != TCL_OK)
        return TCL_ERROR;

    localMsg = catgets (*catDescPtr, msgSetNum, msgNum, argv [4]);

    Tcl_SetResult (interp, localMsg, TCL_VOLATILE);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 * Tcl_CatcloseCmd --
 *
 *    Implements the TCL echo command:
 *        catclose ?-fail|-nofail? catHandle
 *-----------------------------------------------------------------------------
 */
static int
Tcl_CatcloseCmd (clientData, interp, argc, argv)
    ClientData  clientData;
    Tcl_Interp *interp;
    int         argc;
    char      **argv;
{
    int      fail;
    nl_catd *catDescPtr;

    if ((argc < 2) || (argc > 3)) {
        Tcl_AppendResult (interp, tclXWrongArgs, argv [0],
                          " ?-fail|-nofail? catHandle",
                          (char *) NULL);
        return TCL_ERROR;
    }
    if (argc == 3) {
        if (ParseFailOption (interp, argv [1], &fail) != TCL_OK)
            return TCL_ERROR;
    } else
        fail = FALSE;

    catDescPtr = (nl_catd *) Tcl_HandleXlate (interp, msgCatTblPtr,
                                              argv [argc - 1]);
    if (catDescPtr == NULL)
        return TCL_ERROR;

    /*
     * NetBSD has catclose of return type void, which is non-standard.
     */
#ifdef BAD_CATCLOSE
    catclose (*catDescPtr);
#else
    if ((catclose (*catDescPtr) < 0) && fail)
        return CatOpFailed (interp, "close of message catalog failed");
#endif
    Tcl_HandleFree (msgCatTblPtr, catDescPtr);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 * MsgCatCleanUp --
 *
 *    Called at interpreter deletion.  Releases all resources when no more
 * interpreters are using the message catalog table.
 *-----------------------------------------------------------------------------
 */
static void
MsgCatCleanUp (clientData, interp)
    ClientData  clientData;
    Tcl_Interp *interp;
{
    nl_catd *catDescPtr;
    int      walkKey;
    
    if (Tcl_HandleTblUseCount (msgCatTblPtr, -1) > 0)
        return;

    walkKey = -1;
    while (TRUE) {
        catDescPtr = (nl_catd *) Tcl_HandleWalk (msgCatTblPtr, &walkKey);
        if (catDescPtr == NULL)
            break;
        catclose (*catDescPtr);
    }
    Tcl_HandleTblRelease (msgCatTblPtr);
    msgCatTblPtr = NULL;
}

/*
 *-----------------------------------------------------------------------------
 * Tcl_InitMsgCat --
 *
 *   Initialize the Tcl XPG/3 message catalog support faility.
 *-----------------------------------------------------------------------------
 */
void
Tcl_InitMsgCat (interp)
    Tcl_Interp *interp;
{
    /*
     * Set up the table.  It is shared between all interpreters, so the use
     * count reflects the number of interpreters.
     */
    if (msgCatTblPtr == NULL) {
        msgCatTblPtr = Tcl_HandleTblInit ("msgcat", sizeof (nl_catd), 6);
    } else {
        (void) Tcl_HandleTblUseCount (msgCatTblPtr, 1);
    }

    Tcl_CallWhenDeleted (interp, MsgCatCleanUp, (ClientData) NULL);

    /*
     * Initialize the commands.
     */

    Tcl_CreateCommand (interp, "catopen", Tcl_CatopenCmd, 
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "catgets", Tcl_CatgetsCmd, 
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "catclose", Tcl_CatcloseCmd,
                       (ClientData) NULL, (void (*)()) NULL);
}

