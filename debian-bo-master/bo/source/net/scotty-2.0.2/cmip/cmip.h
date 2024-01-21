/*
 * cmip.h
 *
 * Copyright (c) 1994
 *
 * M. Kernchen
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

#if     defined(__GNUG__)
#include <std.h>
extern "C" { int  strncasecmp (const char*, const char*, int); }

#elif   defined(__cplusplus) && defined(AIXV3)
extern "C" { int  strncasecmp (const char*, const char*, int);
             int  strcasecmp (const char*, const char*); }
#endif

/* (schoenfr) extern "C" wrapper added: */
#if defined(__cplusplus) || defined(__GNUG__)
extern "C" {
#include <isode/tailor.h>
}

#else
#include <isode/tailor.h>
#endif

#include "msap.h"

/*
 * The osimis and isode header files are really a hard trip. We fake
 * everything that break our compiler setup.
 */

#undef T_NULL
#define T_NULL          (TYPE_DATA + 12)

/*
 * The osimis people use header files to define various ASN.1 syntax
 * tables. This is a serious programming error (sorry) as you must
 * make sure to include the header exactly once. A work around is to
 * use a special define to get this header included only in one
 * module. You may say: Well, why not just move the include to this
 * module? You can try, but unfortunately some other bad things will
 * happen. Bad code requires bad work arounds.
 */

#ifdef INCLUDE_SYNTAXES
#include "Syntaxes.h"
#endif

/*
 * Do not move this include to the top where you might expect it.
 * You will get into include hell if you try.
 */

#include <scotty.h>

/*
 * Type definitions used for CMIP handles and outstanding 
 * asynchronous requests.
 */

typedef struct CMIP_Handle {
    char		*name;		/* the name of this cmip handle */
    int			 msd;		/* the association descriptor */
    char		*agent;		/* name of the agent */
    char		*host;		/* name of the host */
    Tcl_HashTable	*req_table;	/* A Tcl-Hashtable for requests */
    unsigned		 req_nr;	/* id number for the next request */
} CMIP_Handle;

typedef struct CMIP_Request {
    char                *name;          /* the request handle */
    unsigned		 request_id;	/* the request id */
    int			type;		/* the request type (see above) */
    char		*reqcallback;
    Tcl_DString		 dStrResult;	/* result that's being built */
} CMIP_Request;

/*
 * Exported function in the CMIP package.
 */

EXTERN int
CMIP_Init		_ANSI_ARGS_((Tcl_Interp *interp));

EXTERN CMIP_Request*
CMIP_MallocRequest	_ANSI_ARGS_((int id, int type, char *callback));

EXTERN void
CMIP_FreeRequest	_ANSI_ARGS_((CMIP_Request *rh));

EXTERN int
CMIP_Get		_ANSI_ARGS_((CMIP_Handle *cmiph, 
				     Tcl_Interp *interp, 
				     int argc, char **argv));

EXTERN int
CMIP_Set		_ANSI_ARGS_((CMIP_Handle *cmiph,
				     Tcl_Interp *interp, 
				     int argc, char **argv));

EXTERN int
CMIP_Action		_ANSI_ARGS_((CMIP_Handle *cmiph,
				     Tcl_Interp *interp, 
				     int argc, char **argv));

EXTERN int
CMIP_Create		_ANSI_ARGS_((CMIP_Handle *cmiph,
				     Tcl_Interp *interp, 
				     int argc, char **argv));

EXTERN int
CMIP_Delete		_ANSI_ARGS_((CMIP_Handle *cmiph,
				     Tcl_Interp *interp, 
				     int argc, char **argv));

EXTERN int
CMIP_CancelGet		_ANSI_ARGS_((CMIP_Handle *cmiph,
				     Tcl_Interp *interp, 
				     int argc, char **argv));

EXTERN int
CMIP_EventReport	_ANSI_ARGS_((CMIP_Handle *cmiph,
				     Tcl_Interp *interp, 
				     int argc, char **argv));

EXTERN int
CMIP_Wait		_ANSI_ARGS_((CMIP_Handle *cmiph,
				     Tcl_Interp *interp,
				     CMIP_Request *rh));

EXTERN int
CMIP_WaitResponse	_ANSI_ARGS_((Tcl_Interp	*interp, CMIP_Handle *cmiph,
				     int type, char *callback));

