/* @(#) child.h,v 1.3 1992/07/11 11:48:03 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * child.h:
 *	interface file for the child process control functions in child.c.
 */

/* bit values for the flag parameter to open_child */
#define CHILD_DUPERR	0x0001		/* duplicate stdout to stderr */
#define CHILD_DEVNULL	0x0002		/* use "/dev/null" */
#define CHILD_RETRY	0x0004		/* retry the fork() if it failes */
#define CHILD_MINENV	0x0008		/* supply a default minimum env */
#define CHILD_NOCLOSE	0x0010		/* don't close extraneous fd's */

#ifndef	FORK_RETRIES
# define FORK_RETRIES	10		/* default, 10 retries */
#endif	/* FORK_RETRIES */
#ifndef	FORK_INTERVAL
# define FORK_INTERVAL	30		/* default, 30 seconds */
#endif	/* FORK_INTERVAL */
