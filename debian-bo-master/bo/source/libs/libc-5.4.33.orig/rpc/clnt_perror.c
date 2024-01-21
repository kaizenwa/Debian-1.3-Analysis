/* @(#)clnt_perror.c	2.1 88/07/29 4.0 RPCSRC */
/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)clnt_perror.c 1.15 87/10/07 Copyr 1984 Sun Micro";
#endif

/*
 * clnt_perror.c
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 */
#include <stdio.h>
#include <errno.h>

#include <rpc/rpc.h>

#ifdef __STDC__
#include <stdlib.h>
#endif

#ifdef __linux__
#include <string.h>
#else
extern char *sys_errlist[];
extern char *sprintf();
extern char *strcpy();
#endif

#if NLS
#include "nl_types.h"
#endif

#ifdef __STDC__
static char *_buf               ( void );
char        *clnt_sperror       ( CLIENT *, __const char * );
void         clnt_perror        ( CLIENT *, __const char * );
char        *clnt_sperrno       ( enum clnt_stat );
void         clnt_perrno        ( enum clnt_stat );
char        *clnt_spcreateerror ( __const char * );
void         clnt_pcreateerror  ( __const char * );
static char *auth_errmsg        ( enum auth_stat );
#else
static char *_buf();
char        *clnt_sperror();
void         clnt_perror();
char        *clnt_sperrno();
void         clnt_perrno();
char        *clnt_spcreateerror();
void         clnt_pcreateerror();
static char *auth_errmsg();
#endif

/* Global variable, it could just as well be local to _buf(), vch */
static char *buf;

#ifdef __STDC__
static char *_buf( void )
#else
static char *
_buf()
#endif
{

	if (buf == 0)
		buf = (char *)malloc(256);
	return (buf);
}

/*
 * Print reply error info
 */
#ifdef __STDC__
char *
clnt_sperror( CLIENT *rpch, __const char *s )
#else
char *
clnt_sperror(rpch, s)
	CLIENT *rpch;
	__const char *s;
#endif
{
	struct rpc_err e;
	char *err;
	char *str = _buf();
	char *strstart = str;

	if (str == 0)
		return (0);
	CLNT_GETERR(rpch, &e);

	(void) sprintf(str, "%s: ", s);  
	str += strlen(str);

	(void) strcpy(str, clnt_sperrno(e.re_status));  
	str += strlen(str);

#if NLS
	libc_nls_init();
#endif

	switch (e.re_status) {
	case RPC_SUCCESS:
	case RPC_CANTENCODEARGS:
	case RPC_CANTDECODERES:
	case RPC_TIMEDOUT:     
	case RPC_PROGUNAVAIL:
	case RPC_PROCUNAVAIL:
	case RPC_CANTDECODEARGS:
	case RPC_SYSTEMERROR:
	case RPC_UNKNOWNHOST:
	case RPC_UNKNOWNPROTO:
	case RPC_PMAPFAILURE:
	case RPC_PROGNOTREGISTERED:
	case RPC_FAILED:
		break;

	case RPC_CANTSEND:
	case RPC_CANTRECV:
		(void) sprintf(str,
#if NLS
			       catgets(_libc_cat, ClntMiscSet, ClntMiscErrno,
				       "; errno = %s"),
#else
			       "; errno = %s",
#endif

#if NLS
		    catgets(_libc_cat, ErrorListSet, e.re_errno+1,
		    sys_errlist[e.re_errno]));
#else
		    sys_errlist[e.re_errno]); 
#endif
		str += strlen(str);
		break;

	case RPC_VERSMISMATCH:
		(void) sprintf(str,
#if NLS
		       catgets(_libc_cat, ClntMiscSet, ClntMiscLowHigh,
			       "; low version = %lu, high version = %lu"), 
#else
			"; low version = %lu, high version = %lu", 
#endif
			e.re_vers.low, e.re_vers.high);
		str += strlen(str);
		break;

	case RPC_AUTHERROR:
		err = auth_errmsg(e.re_why);
		(void) sprintf(str,
#if NLS
			       catgets(_libc_cat, ClntMiscSet, ClntMiscWhy,
				       "; why = "));
#else
			       "; why = ");
#endif
		str += strlen(str);
		if (err != NULL) {
			(void) sprintf(str, "%s",err);
		} else {
			(void) sprintf(str,
#if NLS
				catgets (_libc_cat, ClntMiscSet, ClntMiscUnknownAuth,
					 "(unknown authentication error - %d)"),
#else
				"(unknown authentication error - %d)",
#endif
				(int) e.re_why);
		}
		str += strlen(str);
		break;

	case RPC_PROGVERSMISMATCH:
		(void) sprintf(str, 
#if NLS
		       catgets(_libc_cat, ClntMiscSet, ClntMiscLowHigh,
			       "; low version = %lu, high version = %lu"), 
#else
			"; low version = %lu, high version = %lu", 
#endif
			e.re_vers.low, e.re_vers.high);
		str += strlen(str);
		break;

	default:	/* unknown */
		(void) sprintf(str, 
			"; s1 = %lu, s2 = %lu", 
			e.re_lb.s1, e.re_lb.s2);
		str += strlen(str);
		break;
	}
	(void) sprintf(str, "\n");
	return(strstart) ;
}

#ifdef __STDC__
void
clnt_perror( CLIENT *rpch, __const char *s )
#else
void
clnt_perror(rpch, s)
	CLIENT *rpch;
	__const char *s;
#endif
{
	(void) fprintf(stderr,"%s",clnt_sperror(rpch,s));
}


struct rpc_errtab {
	enum clnt_stat status;
	char *message;
};

static struct rpc_errtab  rpc_errlist[] = {
	{ RPC_SUCCESS, 
		"RPC: Success" }, 
	{ RPC_CANTENCODEARGS, 
		"RPC: Can't encode arguments" },
	{ RPC_CANTDECODERES, 
		"RPC: Can't decode result" },
	{ RPC_CANTSEND, 
		"RPC: Unable to send" },
	{ RPC_CANTRECV, 
		"RPC: Unable to receive" },
	{ RPC_TIMEDOUT, 
		"RPC: Timed out" },
	{ RPC_VERSMISMATCH, 
		"RPC: Incompatible versions of RPC" },
	{ RPC_AUTHERROR, 
		"RPC: Authentication error" },
	{ RPC_PROGUNAVAIL, 
		"RPC: Program unavailable" },
	{ RPC_PROGVERSMISMATCH, 
		"RPC: Program/version mismatch" },
	{ RPC_PROCUNAVAIL, 
		"RPC: Procedure unavailable" },
	{ RPC_CANTDECODEARGS, 
		"RPC: Server can't decode arguments" },
	{ RPC_SYSTEMERROR, 
		"RPC: Remote system error" },
	{ RPC_UNKNOWNHOST, 
		"RPC: Unknown host" },
	{ RPC_UNKNOWNPROTO,
		"RPC: Unknown protocol" },
	{ RPC_PMAPFAILURE, 
		"RPC: Port mapper failure" },
	{ RPC_PROGNOTREGISTERED, 
		"RPC: Program not registered"},
	{ RPC_FAILED, 
		"RPC: Failed (unspecified error)"}
};

/*
 * This interface for use by clntrpc
 */
#ifdef __STDC__
char *
clnt_sperrno( enum clnt_stat stat )
#else
char *
clnt_sperrno(stat)
	enum clnt_stat stat;
#endif
{
	int i;

#if NLS
	libc_nls_init();
#endif
	for (i = 0; i < sizeof(rpc_errlist)/sizeof(struct rpc_errtab); i++) {
		if (rpc_errlist[i].status == stat) {
#if NLS
			return catgets(_libc_cat, RpcErrListSet, stat+1, rpc_errlist[i].message);
#else
			return (rpc_errlist[i].message);
#endif
		}
	}
#if NLS
	return catgets(_libc_cat, RpcErrListSet, RpcErrListUnknown,
			"RPC: (unknown error code)");
#else
	return ("RPC: (unknown error code)");
#endif
}

#ifdef __STDC__
void
clnt_perrno( enum clnt_stat num )
#else
void
clnt_perrno(num)
	enum clnt_stat num;
#endif
{
	(void) fprintf(stderr,"%s",clnt_sperrno(num));
}

#ifdef __STDC__
char *
clnt_spcreateerror( __const char *s )
#else
char *
clnt_spcreateerror(s)
	__const char *s;
#endif
{
#ifndef __linux__
	extern int sys_nerr;
	extern char *sys_errlist[];
#endif
	char *str = _buf();

	if (str == 0)
		return(0);
#if NLS
	libc_nls_init();
#endif
	(void) sprintf(str, "%s: ", s);
	(void) strcat(str, clnt_sperrno(rpc_createerr.cf_stat));
	switch (rpc_createerr.cf_stat) {
	case RPC_PMAPFAILURE:
		(void) strcat(str, " - ");
		(void) strcat(str,
		    clnt_sperrno(rpc_createerr.cf_error.re_status));
		break;

	case RPC_SYSTEMERROR:
		(void) strcat(str, " - ");
		if (rpc_createerr.cf_error.re_errno > 0
		    && rpc_createerr.cf_error.re_errno < sys_nerr)
			(void) strcat(str,
#if NLS
		    catgets(_libc_cat, ErrorListSet,
			    rpc_createerr.cf_error.re_errno+1,
			    sys_errlist[rpc_createerr.cf_error.re_errno]));
#else
			    sys_errlist[rpc_createerr.cf_error.re_errno]);
#endif
		else
			(void) sprintf(&str[strlen(str)], "Error %d",
			    rpc_createerr.cf_error.re_errno);
		break;
	default: break;
	}
	(void) strcat(str, "\n");
	return (str);
}

#ifdef __STDC__
void
clnt_pcreateerror( __const char *s )
#else
void
clnt_pcreateerror(s)
	__const char *s;
#endif
{
	(void) fprintf(stderr,"%s",clnt_spcreateerror(s));
}

struct auth_errtab {
	enum auth_stat status;	
	char *message;
};

static struct auth_errtab auth_errlist[] = {
	{ AUTH_OK,
		"Authentication OK" },
	{ AUTH_BADCRED,
		"Invalid client credential" },
	{ AUTH_REJECTEDCRED,
		"Server rejected credential" },
	{ AUTH_BADVERF,
		"Invalid client verifier" },
	{ AUTH_REJECTEDVERF,
		"Server rejected verifier" },
	{ AUTH_TOOWEAK,
		"Client credential too weak" },
	{ AUTH_INVALIDRESP,
		"Invalid server verifier" },
	{ AUTH_FAILED,
		"Failed (unspecified error)" },
};

#ifdef __STDC__
static char *
auth_errmsg( enum auth_stat stat )
#else
static char *
auth_errmsg(stat)
	enum auth_stat stat;
#endif
{
	int i;

#if NLS
	libc_nls_init();
#endif
	for (i = 0; i < sizeof(auth_errlist)/sizeof(struct auth_errtab); i++) {
		if (auth_errlist[i].status == stat) {
#if NLS
			return catgets(_libc_cat, AuthListSet, i+1,
				       auth_errlist[i].message);
#else
			return(auth_errlist[i].message);
#endif
		}
	}
	return(NULL);
}
