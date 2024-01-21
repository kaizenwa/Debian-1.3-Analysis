/* $Header: /home/geyer/cvs/procps/skill/conf.h,v 1.1.1.1 1996/06/27 10:56:56 geyer Exp $ */

/*
**  This program may be freely redistributed for noncommercial purposes.
**  This entire comment MUST remain intact.
**
**  Copyright 1994 by Jeff Forys (jeff@forys.cranbury.nj.us)
*/

#include <sys/param.h>
#ifdef	NO_TYPES_INC		/* normally, <sys/param.h> includes this */
#include <sys/types.h>
#endif
#include <sys/stat.h>
#include <sys/signal.h>

/*
 * Set up common typedef's.
 */
#ifdef	NO_UID_T		/* "uid_t" arrived 20 months after 4.2BSD */
typedef	int	uid_T;		/* uids */
#else
typedef	uid_t	uid_T;		/* uids */
#endif
typedef	dev_t	tty_T;		/* ttys (actually, device numbers) */
typedef	int	pid_T;		/* process id's */
typedef	char *	cmd_T;		/* commands */

/*
 * Error codes used by Usage().
 */
#define	E_USAGE	1		/* generic usage error */
#define	E_PRIOR	2		/* priority out of range */
#define	E_SIGNO	3		/* invalid signal number */

/*
 * Error codes used by exit().
 */
#define	EX_OKAY	0		/* success */
#define	EX_UERR	1		/* user error */
#define	EX_SERR	2		/* system error */

/*
 * Miscellaneous #define's.
 */
#define	ROOTUID	0		/* super-user's UID */
#define	ROOTUSR	"root"		/* what the Super-user likes to be called */

#define STREQU(s1,s2)		((*s1 == *s2) && (strcmp(s1,s2) == 0))
#define STRNEQU(s1,s2,n)	((*s1 == *s2) && (strncmp(s1,s2,n) == 0))

/*
 * This is all we need/want to know about a process.  The machine-dependent
 * routine GetProc() returns a "struct ProcInfo" pointer (or NULL if no more
 * processes).
 */
struct ProcInfo {
	int	pi_flags;	/* various flags (see below) */
	cmd_T	pi_cmd;		/* pointer to path-stripped command name */
	pid_T	pi_pid;		/* process id */
	uid_T	pi_uid;		/* user id of process */
	tty_T	pi_tty;		/* controlling tty */
};

/* pi_flags */
#define	PI_CTLTTY	0x01	/* has a controlling tty ("pi_tty" valid) */
#define	PI_ZOMBIE	0x02	/* is a zombie */
#define	PI_SWEXIT	0x04	/* is in the process of exiting */
#define	PI_ASKUSR	0x10	/* check with user before doing anything */

#ifndef	NO_AEXTERN		/* external variables (from "argparse.c") */
extern	tty_T	*TtyList;
extern	uid_T	*UidList;
extern	pid_T	*PidList;
extern	cmd_T	*CmdList;
extern	int	TtyIndx, UidIndx, PidIndx, CmdIndx;
extern	int	Fflag, Iflag, Nflag, Vflag, Wflag;
#endif	/* NO_AEXTERN */

#ifndef	NO_MEXTERN		/* external variables (machine-dependent) */
extern	char	*SigMap[];
extern	int	NSig, Skill, PrioMin, PrioMax, SigPri;
extern	pid_T	MyPid;
extern	uid_T	MyUid;
extern	char	*ProgName;
extern	int	MdepAction();
extern	struct ProcInfo *GetProc();
#endif	/* NO_MEXTERN */

void MdepInit(), ArgParse(), ListSigs(), Usage(), Not(), Exceed();
