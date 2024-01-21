/*
 * init.c
 *
 * This is the initialization of the scotty tcl extension with commands
 * to get information about TCP/IP networks.
 *
 * Copyright (c) 1994, 1995
 *
 * J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include "scotty.h"

/*
 * The default directory name where we will find the scotty library
 * files. This is normally overwritten in the Makefile.
 */

#ifndef SCOTTYLIB
#define SCOTTYLIB "/usr/local/lib/scotty"
#endif

/*
 * Initialize a new scotty extended interpreter.
 */

int
Scotty_Init (interp)
    Tcl_Interp *interp;
{
    char *fileName, *machine, *os, *vers, *arch, *path;
    
    path = getenv("SCOTTY_LIBRARY");
    if (! path) {
	path = SCOTTYLIB;
    }

    /*
     * Write the version number to scotty_version. The scotty_library
     * variable will point to the library directory. The scotty_lib
     * variable is still set for backward compatibility.
     */
    
    Tcl_SetVar(interp, "scotty_version", SCOTTY_VERSION, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "scotty_library", path, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "scotty_lib", path, TCL_GLOBAL_ONLY);

    /*
     * Determine the location of machine dependend files in the
     * scotty library.
     */

    machine = Tcl_GetVar2(interp, "tcl_platform", "machine", TCL_GLOBAL_ONLY);
    os = Tcl_GetVar2(interp, "tcl_platform", "os", TCL_GLOBAL_ONLY);
    vers = Tcl_GetVar2(interp, "tcl_platform", "osVersion", TCL_GLOBAL_ONLY);

    if (machine && os && vers) {
	arch = ckalloc(strlen(machine)+strlen(os)+strlen(vers)+3);
	strcpy(arch, machine);
	strcat(arch, "-");
	strcat(arch, os);
        strcat(arch, "-");
	strcat(arch, vers);
    } else {
	arch = ckstrdup("unknown-os");
    }

    Tcl_SetVar(interp, "scotty_arch", arch, TCL_GLOBAL_ONLY);
    ckfree(arch);

    /*
     * The getclock command is also defined in TCLX, but we redefine
     * it here since our version is more powerful and should cause
     * no problem with scripts that expect the TCLX behaviour.
     */

/* #ifndef HAVE_TCLX */
    Tcl_CreateCommand(interp, "getclock", Scotty_GetclockCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
/* #endif */

    Tcl_CreateCommand(interp, "getdate", Scotty_GetdateCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "event", Scotty_EventCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "job", Scotty_JobCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "syslog", Scotty_SyslogCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "icmp", Scotty_IcmpCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "dns", Scotty_DnsCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "ntp", Scotty_NtpCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "udp", Scotty_UdpCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "tcp", Scotty_TcpCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "sunrpc", Scotty_SunrpcCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "rpc", Scotty_RpcCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "http", Scotty_HttpCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "netdb", Scotty_NetdbCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateCommand(interp, "ined", Scotty_InedCmd,
		      (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

#ifdef HAVE_SNMP
    if (Snmp_Init(interp) != TCL_OK) {
        return TCL_ERROR;
    }
#endif

#ifdef HAVE_MSQL
    if (Msqltcl_Init(interp) == TCL_ERROR) {
        return TCL_ERROR;
    }
#endif

#ifdef HAVE_BONES
    Bones_Init(interp);
#endif

#ifdef HAVE_OSIMIS
    if (Cmip_Init(interp) != TCL_OK) {
        return TCL_ERROR;
    }
#endif

#ifdef HAVE_GDMO
    if (Gdmo_Init(interp) != TCL_OK) {
        return TCL_ERROR;
    }
#endif

    /*
     * Evaluate the commands stored in scotty's init.tcl file.
     */

    fileName = ckalloc(strlen(path) + 10);
    strcpy(fileName, path);
    strcat(fileName, "/init.tcl");
    if (access(fileName, R_OK) == 0) {
        if (Tcl_EvalFile(interp, fileName) != TCL_OK) 
	    return TCL_ERROR;
    } else if (access("init.tcl", R_OK) == 0) {
	if (Tcl_EvalFile(interp, "init.tcl") != TCL_OK) 
	    return TCL_ERROR;
    } else if (access("../init.tcl", R_OK) == 0) {
	if (Tcl_EvalFile(interp, "../init.tcl") != TCL_OK) 
	    return TCL_ERROR;
    } else {
        Tcl_AppendResult(interp, "no initialization file: tried ",
			 path, "/init.tcl, init.tcl, and ../init.tcl",
			 (char *) NULL);
        return TCL_ERROR;
    }
    ckfree(fileName);

    /*
     * Load the user specific startup file.
     */

    fileName = Tcl_GetVar(interp, "scotty_RcFileName", TCL_GLOBAL_ONLY);
    if (fileName != NULL && strlen(fileName) > 0) {
        Tcl_DString buffer;
        char *fullName;
        FILE *f;
	
        fullName = Tcl_TildeSubst(interp, fileName, &buffer);
        if (fullName == NULL) {
            fprintf(stderr, "%s\n", interp->result);
        } else {
            f = fopen(fullName, "r");
            if (f != NULL) {
                int code = Tcl_EvalFile(interp, fullName);
                if (code != TCL_OK) {
                    fprintf(stderr, "%s\n", interp->result);
                }
                fclose(f);
            }
        }
        Tcl_DStringFree(&buffer);
    }


    return TCL_OK;
}

