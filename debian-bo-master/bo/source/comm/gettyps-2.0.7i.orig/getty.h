/*
**	$Id: getty.h,v 2.0 90/09/19 19:59:15 paul Rel $
**
**	Included by all getty modules
*/

/*
**	Copyright 1989,1990 by Paul Sutcliffe Jr.
**
**	Permission is hereby granted to copy, reproduce, redistribute,
**	or otherwise use this software as long as: there is no monetary
**	profit gained specifically from the use or reproduction or this
**	software, it is not sold, rented, traded or otherwise marketed,
**	and this copyright notice is included prominently in any copy
**	made.
**
**	The author make no claims as to the fitness or correctness of
**	this software for any use whatsoever, and it is provided as is. 
**	Any use of this software is at the user's own risk.
*/

/*
**	$Log:	getty.h,v $
**	Revision 2.0  90/09/19  19:59:15  paul
**	Initial 2.0 release
**	
*/


#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <termio.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <utmp.h>

#include "config.h"
#include "extern.h"
#include "funcs.h"
#include "mytermio.h"		/* SMR - gcc was missing a few definitions */

/*	General purpose defines
 */

#ifndef	FALSE
#define	FALSE	(0)
#endif	/* FALSE */
#ifndef	TRUE
#define	TRUE	(1)
#endif	/* TRUE */

#define OK	(0)

#define SUCCESS	(0)		/* normal return */
#define FAIL	(-1)		/* error return */

#define	STDIN	fileno(stdin)
#define	STDOUT	fileno(stdout)

#define strequal(s1, s2)	(strcmp(s1, s2) == 0)
#define strnequal(s1, s2, n)	(strncmp(s1, s2, n) == 0)
#define	strncopy(s1, s2)	(strncpy(s1, s2, sizeof(s1)))

typedef	struct termios	TERMIO;


/* debug levels
 */
#define	D_OPT	0001		/* option settings */
#define	D_DEF	0002		/* defaults file processing */
#define	D_UTMP	0004		/* utmp/wtmp processing */
#define	D_INIT	0010		/* line initialization (INIT) */
#define	D_GTAB	0020		/* gettytab file processing */
#define	D_RUN	0040		/* other runtime diagnostics */
#define D_RB	0100		/* ringback debugging */
#define	D_LOCK	0200		/* uugetty lockfile processing */
#define D_SCH	0400		/* schedule processing */
#define D_ALL	0777		/* all priorities */

#ifndef	DEBUG
#define	debug()			/* define to nothing, disables debugging */
#endif	/* DEBUG */


/* end of getty.h */
