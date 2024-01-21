/*
**	Getty tuneable parameters.
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

#define	boolean	 int			/* does your cc know about boolean? */
#define	DEF_CFL	 (CS8)			/* default word-len/parity */
#define	DEF_CONNECT "CONNECT\\s\\A\r\n"	/* default CONNECT string */


/*  Feature selection
 */
#define RBGETTY				/* include ringback code */
#define SCHED				/* include scheduler code */
#define	DEBUG				/* include debugging code */
#define	LOGUTMP				/* need to update utmp/wtmp files */
#define	MY_CANON			/* use my own ERASE and KILL chars */
#define	SETTERM				/* need to set TERM in environment */
#undef	TELEBIT				/* include Telebit FAST parsing */
#define	WARNCASE			/* warn user if login is UPPER case */
#define	FIDO				/* allow fido logins */
#define SYSLOG				/* log everything via syslog */

/* compiling without DEBUG defined probably will not work */

/*  define your ERASE and KILL characters here
 */
#ifdef	MY_CANON
/* #define	MY_ERASE '\177'			/* 177 = ^?, delete */
#define	MY_ERASE '\10'			/* 010 = ^H, bs */
#define	MY_KILL	 '\025'			/* 025 = ^U, nak */
#endif

/*  define your Telebit FAST speed here
 */
#ifdef	TELEBIT
#define	TB_FAST	 "19200"		/* CONNECT FAST == this speed */
#endif	/* TELEBIT */

/*  Where to find things
 */

#define	CONSOLE	 "/usr/adm/getty.log"	/* error log if not using syslog */
#ifndef FSSTND
#define	DEFAULTS "/etc/default/%s"	/* name of defaults file */
#else
#define	DEFAULTS "/etc/conf.%s"		/* FSSTND compliant defaults file */
#endif
#define	ISSUE	 "/etc/issue"		/* name of the issue file;
					   say "#undef ISSUE" to turn off
					   the issue feature */
#define	LOGIN	 "/bin/login"		/* name of login program */

/*  Parameters for syslog
 */
#define SYSL_FACIL	LOG_AUTH	/* facility to log to */
#define SYSL_ERROR	LOG_ERR		/* facility to log errors to */
#define SYSL_DEBUG	LOG_DEBUG	/* facility to log debugging 
					   output to (undef if you want the
					   debug file) */


/*  You probably shouldn't fool with these
 */

#define	MAXDEF	 100			/* max # lines in defaults file */
#define	MAXLINE	 8192			/* max # chars in a line */
#define	MAXID	 12			/* max # chars in Gtab Id */
#define	MAXLOGIN 80			/* max # chars in Gtab Login */


/* end of tune.h */
