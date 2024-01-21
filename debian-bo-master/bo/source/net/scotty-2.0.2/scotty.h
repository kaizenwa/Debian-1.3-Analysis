/*
 * scotty.h
 *
 * Common definitions for the scotty extensions to tcl.
 *
 * Copyright (c) 1993, 1994, 1995
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

#ifndef _SCOTTY
#define _SCOTTY

#define SCOTTY_VERSION "2.0.2"

#include <config.h>

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>

#ifndef TCL_MSDOS_PORT
#include <sys/time.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef TCL_MSDOS_PORT
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#ifdef __osf__
#include <machine/endian.h>
#endif
#endif

#ifdef TCL_MSDOS_PORT
#include <winsock.h>
#endif

#include <tcl.h>
#include <tk.h>

#define ckstrdup(s)	strcpy (ckalloc (strlen (s) + 1), s)

/*
 * This is the scotty initialization function that may be used
 * to extend other tcl interpreter.
 */

EXTERN int Scotty_Init		_ANSI_ARGS_((Tcl_Interp *interp));

/*
 * The SNMP package and the optional BONES and CMIP extension provide 
 * their own initialization procedure. We define them here to bypass 
 * any problems with finding their include files.
 */

EXTERN int Snmp_Init		_ANSI_ARGS_((Tcl_Interp *interp));

#ifdef HAVE_OSIMIS
EXTERN int Cmip_Init		_ANSI_ARGS_((Tcl_Interp *interp));
#endif

#ifdef HAVE_GDMO
EXTERN int Gdmo_Init		_ANSI_ARGS_((Tcl_Interp *interp));
#endif

#ifdef HAVE_BONES
EXTERN int Bones_Init		_ANSI_ARGS_((Tcl_Interp *interp));
#endif

#ifdef HAVE_TCLX
EXTERN int TclX_Init		_ANSI_ARGS_((Tcl_Interp *interp));
#endif

#ifdef HAVE_MSQL
EXTERN int Msqltcl_Init		_ANSI_ARGS_((Tcl_Interp *interp));
#endif

/*
 * Tcl commands exported by the scotty extension:
 */

EXTERN int Scotty_GetdateCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_GetclockCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_EventCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_JobCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_SyslogCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_UdpCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_TcpCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_RpcCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_HttpCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_DnsCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_NtpCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_SunrpcCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_IcmpCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp,
					     int argc, char **argv));
EXTERN int Scotty_InedCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));
EXTERN int Scotty_NetdbCmd	_ANSI_ARGS_((ClientData clientData, 
					     Tcl_Interp *interp, 
					     int argc, char **argv));

#endif /* _SCOTTY */
