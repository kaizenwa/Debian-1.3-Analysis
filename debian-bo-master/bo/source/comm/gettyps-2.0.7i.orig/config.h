/*
**	config.h
**
**	Getty configuration.
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


#include "tune.h"			/* defs needed below */

#define	GETTYTAB  "/etc/gettydefs"	/* file used for speed/termio table */

#define	index	  strchr		/* use these instead */
#define	rindex	  strrchr

typedef	void	sig_t;

#ifndef	UTMP_FILE
#define	UTMP_FILE "/etc/utmp"		/* name of the utmp file */
#endif	/* UTMP_FILE */

#ifndef	WTMP_FILE
#define	WTMP_FILE "/var/adm/wtmp"	/* FSSTND compliant wtmp file */
#endif	/* WTMP_FILE */

#if 0
#define TTYTYPE "/etc/ttytype"		/* name of the ttytype file */
					/* use this option at your own risk */
#else
#undef TTYTYPE
#endif

#define	ASCIIPID			/* PID stored in ASCII */
#define	BOTHPID				/* ... or perhaps not */
#define	UUCPID 11			/* uid of UUCP account */
#define	LOCK "/usr/spool/uucp/LCK..%s"	/* lock file name */



/* end of config.h */
