/*
 * tclXcmdInit.c
 *
 * Function to add the Extented Tcl commands into an interpreter.  The TclX
 * library commands are not added here, to make it easier to build applications
 * that don't use the extended libraries.
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
 * $Id: tclXcmdInit.c,v 7.0 1996/06/16 05:30:07 markd Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"


/*-----------------------------------------------------------------------------
 * Tclxcmd_Init --
 *
 *   Add the Extended Tcl commands to the specified interpreter (except for
 * the library commands that override that standard Tcl procedures).  This
 * does no other startup.
 *-----------------------------------------------------------------------------
 */
int
Tclxcmd_Init (interp)
    Tcl_Interp *interp;
{
    if (Tclxcmd_SafeInit (interp) == TCL_ERROR)
        return TCL_ERROR;

    /*
     * from tclCkalloc.c (now part of the UCB Tcl).
     */
#ifdef TCL_MEM_DEBUG    
    Tcl_InitMemory (interp);
#endif

    /*
     * from tclXchmod.c
     */
    Tcl_CreateCommand (interp, "chgrp", Tcl_ChgrpCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "chmod", Tcl_ChmodCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "chown", Tcl_ChownCmd,
                       (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXcmdloop.c
     */
    Tcl_CreateCommand (interp, "commandloop", Tcl_CommandloopCmd, 
                      (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXdebug.c
     */
    Tcl_InitDebug (interp);

    /*
     * from tclXdup.c
     */
    Tcl_CreateCommand (interp, "dup",  Tcl_DupCmd, 
                       (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXfcntl.c
     */
    Tcl_CreateCommand (interp, "fcntl", Tcl_FcntlCmd,
                       (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXfilecmds.c
     */
    Tcl_CreateCommand (interp, "pipe", Tcl_PipeCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "copyfile", Tcl_CopyfileCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "lgets", Tcl_LgetsCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "frename", Tcl_FrenameCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "ftruncate", Tcl_FtruncateCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "readdir", Tcl_ReaddirCmd,
                       (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXmsgcat.c
     */
    Tcl_InitMsgCat (interp);

    /*
     * from tclXprocess.c
     */
    Tcl_CreateCommand (interp, "execl", Tcl_ExeclCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "fork", Tcl_ForkCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "wait", Tcl_WaitCmd,
                       (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXsignal.c
     */
    Tcl_InitSignalHandling (interp);

    /*
     * from tclXoscmds.c
     */
    Tcl_CreateCommand (interp, "sleep", Tcl_SleepCmd,
                       (ClientData) NULL,(void (*)()) NULL);
    Tcl_CreateCommand (interp, "sync", Tcl_SyncCmd,
                       (ClientData) NULL,(void (*)()) NULL);
    Tcl_CreateCommand (interp, "system", Tcl_SystemCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "umask", Tcl_UmaskCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "unlink", Tcl_UnlinkCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "mkdir", Tcl_MkdirCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "rmdir", Tcl_RmdirCmd,
                       (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXunixcmds.c
     */
    Tcl_CreateCommand (interp, "alarm", Tcl_AlarmCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "chroot", Tcl_ChrootCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "nice", Tcl_NiceCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "times", Tcl_TimesCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "link", Tcl_LinkCmd,
                       (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXxxxSock.c
     */
    TclX_SocketInit (interp);

    return TCL_OK;
}


/*-----------------------------------------------------------------------------
 * Tclxcmd_SafeInit --
 *
 *   Add the safe Extended Tcl commands to the specified interpreter.
 *-----------------------------------------------------------------------------
 */
int
Tclxcmd_SafeInit (interp)
    Tcl_Interp *interp;
{
    TclX_SetAppInfo (TRUE,
                     "TclX",
                     "Extended Tcl",
                     TCLX_FULL_VERSION,
                     TCLX_PATCHLEVEL);


    /*
     * from tclXbsearch.c
     */
    Tcl_CreateCommand (interp, "bsearch", Tcl_BsearchCmd, 
                      (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXfstat.c
     */
    Tcl_CreateCommand (interp, "fstat", Tcl_FstatCmd,
                       (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXflock.c
     */
    Tcl_CreateCommand (interp, "flock", Tcl_FlockCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "funlock", Tcl_FunlockCmd,
                       (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXfilescan.c
     */
    Tcl_InitFilescan (interp);

    /*
     * from tclXgeneral.c
     */
    Tcl_CreateCommand(interp, "echo", Tcl_EchoCmd, 
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "infox", Tcl_InfoxCmd, 
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "loop", Tcl_LoopCmd, 
                     (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXid.c
     */
    Tcl_CreateCommand (interp, "id", Tcl_IdCmd,
                       (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXkeylist.c
     */
    Tcl_CreateCommand(interp, "keyldel", Tcl_KeyldelCmd,
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "keylget", Tcl_KeylgetCmd,
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "keylkeys", Tcl_KeylkeysCmd,
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "keylset", Tcl_KeylsetCmd,
                     (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXlist.c
     */
    Tcl_CreateCommand(interp, "lvarcat", Tcl_LvarcatCmd, 
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "lvarpop", Tcl_LvarpopCmd, 
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "lvarpush", Tcl_LvarpushCmd, 
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "lempty", Tcl_LemptyCmd, 
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "lassign", Tcl_LassignCmd, 
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "lmatch", Tcl_LmatchCmd, 
                     (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXmath.c
     */
    Tcl_InitMath (interp);

    /*
     * from tclXprofile.c
     */
    Tcl_InitProfile (interp);

    /*
     * from tclXselect.c
     */
    Tcl_CreateCommand (interp, "select", Tcl_SelectCmd,
                       (ClientData) NULL, (void (*)()) NULL);

    /*
     * from tclXstring.c
     */
    Tcl_CreateCommand(interp, "cindex", Tcl_CindexCmd, 
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "clength", Tcl_ClengthCmd, 
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "crange", Tcl_CrangeCmd, 
                     (ClientData) TRUE, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "csubstr", Tcl_CrangeCmd, 
                     (ClientData) FALSE, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "ccollate", Tcl_CcollateCmd,
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "replicate", Tcl_ReplicateCmd, 
                     (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "translit", Tcl_TranslitCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "ctype", Tcl_CtypeCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "ctoken", Tcl_CtokenCmd,
                       (ClientData) NULL, (void (*)()) NULL);
    Tcl_CreateCommand (interp, "cequal", Tcl_CequalCmd,
                       (ClientData) NULL, (void (*)()) NULL);

    return TCL_OK;
}
