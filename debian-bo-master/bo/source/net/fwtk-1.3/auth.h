/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */
/*	RcsId: "Header: auth.h,v 1.4 94/11/01 11:54:21 mjr rel "	*/

/*
 *	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
 */

/*
	auth.h
	interface for authentication server internal routines.
*/

#ifndef	_INCL_AUTH_H
/* default shut account off after this many bad tries */
#define	AUTH_MAXBAD	5

/*
default port and hostname for auth server
if these are set to port=-1 and server = (char *)0
the values will ONLY be accepted from netperm-table.
if they are defined here they will be defaults but
can be overriden by values set in netperm-table.
*/
#define	AUTH_DEFAULTPORT	-1
/* #define	AUTH_DEFAULTPORT	7777 */
#define	AUTH_DEFAULTSERVER	(char *)0
/* #define	AUTH_DEFAULTSERVER	"firewall.yourdomain.com" */


/*
default location of database file for auth server.
if this value is set to (char *)0 the value will
ONLY be accepted from netperm-table. if it is
defined here it will act as a default but can be
overridden by values set in netperm-table.
*/
#define	AUTH_DEFAULTDBFILE	(char *)0


/*
one of these for each auth type (securID, Digital PW, etc, etc)
bindings are in auth/proto.o
*/
typedef	struct	{
	char	typ;		/* short code for proto */
	char	*name;		/* long name of proto */
	int	(*challf)();	/* challenge function (or zero if none) */
	int	(*verf)();	/* verification function */
	int	(*setf)();	/* key setting function */
} AProto;
extern AProto aprototab[];


/*
the next set of options permits you to determine what types of
authentication protocols you have and wish to support. these
are NOT mutually exclusive, I.e.: you may have as many different
protocols as you like, provided there are no linker conflicts.
the master protocol switch is encoded in auth/proto.c
*/

/* define this if you want to use builtin plaintext passwords */
#define	AUTHPROTO_PASSWORD

/* define this if you want to use bellcore's S/Key */
/* #define	AUTHPROTO_SKEY */

/* define this if you want to use Security Dynamics' SecurID */
/* #define	AUTHPROTO_SECURID */

/* define this if you want to use Digital Pathways SNK */
/* #define	AUTHPROTO_SNK */

/* define this if you want to use Enigma Logics' Silver Card */
/* #define	AUTHPROTO_ENIGMA */


/* format of an authorization database entry */
#define	AUTH_USIZ	24
#define	AUTH_GSIZ	24
#define	AUTH_PWSIZ	42	/* if using SNK need room for key */
#define	AUTH_LNSIZ	60
typedef	struct	{
	int	flgs;
	int	bcnt;			/* bad attempt count */
	char	atyp;			/* type of auth to use */
	long	last;			/* last login */
	char	pw[AUTH_PWSIZ];		/* passwrd (or other key stuff) */
	char	gp[AUTH_GSIZ];		/* group */
	char	ln[AUTH_LNSIZ];		/* long name */
} Auth;

#define	AUTHFLG_DIS	0001	/* disabled */
#define	AUTHFLG_WIZ	0002	/* wizard */
#define	AUTHFLG_GWIZ	0004	/* wizard over member group */
#define	AUTHFLG_BSLP	0010	/* sleeping due to bad logins */
#define	AUTHFLG_ONETIME	0020	/* let this login work once only */


extern	int	auth_close();
extern	int	auth_open();
extern	int 	auth_recv();
extern	int	auth_send();

#define	_INCL_AUTH_H
#endif
