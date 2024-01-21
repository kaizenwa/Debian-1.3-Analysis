#ifndef lint
static char rcsid[] = "$Header: /home/geyer/cvs/procps/skill/main.c,v 1.1.1.1 1996/06/27 10:56:56 geyer Exp $";
/*
 * SCCS version release number is manually updated (for what(1), etc).
 * If you use SCCS, please use last extension for version (e.g. "3.6.1.1").
 */
static char sccsid[] = 
	"@(#)skill	3.6.1.0 (jeff@forys.cranbury.nj.us) 7/15/94";
#endif

/*
**  skill - send signals to processes by tty, user name, command or proc id.
**  snice - change process priorities by tty, user name, command or proc id.
**
**  Version 3.6
**
**  This program may be freely redistributed for noncommercial purposes.
**  This entire comment MUST remain intact.
**
**  Copyright 1994 by Jeff Forys (jeff@forys.cranbury.nj.us)
*/

#include "conf.h"

#include <stdio.h>
#include <errno.h>
#include <pwd.h>

/*
 * Processes which could not be checked -- usually due to permission
 * problems (SunOS 4.1, Dunix 3) -- are tallied in "MissedProcCnt".
 * The machine dependent code is responsible for incrementing this;
 * if it does not, we simply assume that all reasonable processes
 * have been investigated (i.e. zombies are not reasonable).
 */
int MissedProcCnt = 0;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *SysErr(), *WhoIs();
	register struct ProcInfo *proc;	/* process we are examining */
	int reterr = -1;		/* -1:matchless, 0:okay, 1:error(s) */
	register int i;

	/*
	 * Ignore SIGHUP (in case we kill our shell).
	 */
	(void) signal(SIGHUP, SIG_IGN);

	/*
	 * Call machine-dependent initialization routine; besides setting
	 * up many global variables, this routine will change our working
	 * directory to where tty devs reside and may raise our priority.
	 *
	 * When machine-dependent initialization is complete, parse the
	 * argument list.
	 */
	MdepInit(argv[0]);
	ArgParse(argc, argv);

	/*
	 * Our friend ArgParse() has painfully categorized the arguments
	 * as ttys, users, commands, or pids. Loop through the list of
	 * currently running processes and find things that match.  In
	 * order for a match, we must have 1 item from each category
	 * (unless a category is empty).  When a match is found, the
	 * process is either sent signal `SigPri' (`Skill' == 1) or has
	 * it's priority is changed to `SigPri' (`Skill' == 0).
	 */
#define	PID	proc->pi_pid		/* process id */
#define	UID	proc->pi_uid		/* process owner */
#define	TTY	proc->pi_tty		/* controlling tty */
#define	CMD	proc->pi_cmd		/* command being executed */
#define	FLAGS	proc->pi_flags		/* various flags (see conf.h) */

	while ((proc = GetProc()) != NULL) {
		if (TtyIndx > 0) {
			if ((FLAGS & PI_CTLTTY) == 0)
				continue;	/* no controlling tty */

			for (i = 0; i < TtyIndx && *(TtyList+i) != TTY; i++)
				;
			if (i == TtyIndx)	/* no matching tty */
				continue;
		}

		if (UidIndx > 0) {
			for (i = 0; i < UidIndx && *(UidList+i) != UID; i++)
				;
			if (i == UidIndx)	/* no matching uid */
				continue;
		}

		if (PidIndx > 0) {
			for (i = 0; i < PidIndx && *(PidList+i) != PID; i++)
				;
			if (i == PidIndx)	/* no matching pid */
				continue;
		}

		if (CmdIndx > 0) {
			for (i = 0; i < CmdIndx; i++) {
				if (STREQU(CMD, *(CmdList + i)))
					break;
			}
			if (i == CmdIndx)	/* no matching cmd  */
				continue;
		}

		if (PID == MyPid || PID == 0)	/* ignore self */
			continue;

		if (Iflag || (!Nflag && (FLAGS & PI_ASKUSR))) {	/* ask user */
			static char yesno[10];

			(void) fseek(stdin, 0L, 0);

			fputs(ProgName, stdout);
			if (Skill)
				printf(": send #%d a %s", PID, SigMap[SigPri]);
			else
				printf(": renice #%d to %s%d", PID,
				       (SigPri >= 0)? "+": "", SigPri);
			printf(" (%s executing %s)? ", WhoIs(UID), CMD);
			(void) fflush(stdout);

			if (fgets(yesno, 10, stdin) == NULL) {	/* EOF */
				(void) putc('\n', stdout);
				return(EX_OKAY);
			}

			if (reterr < 0)
				reterr = 0;

			if (*yesno != 'y' && *yesno != 'Y')
				continue;
		}

		if (FLAGS & PI_ZOMBIE) {	/* zombie process */
			fprintf(stderr, "%d: zombie process\n", PID);
			continue;
		} else if (FLAGS & PI_SWEXIT) {	/* process is exiting */
			fprintf(stderr, "%d: exiting process\n", PID);
			continue;
		}

		/*
		 * Finally do what we came here to do.  First, if
		 * we are only displaying process id's, then do so.
		 * If `Skill' is set, send signal `SigPri' to the
		 * process, otherwise, set priority of process to
		 * `SigPri'.  If either setpriority(2) or kill(2)
		 * return -1, display the system error message.
		 */
		if (Nflag) {
			printf("%d", (int)PID);
			if (Vflag)
				printf(" (%s executing %s)", WhoIs(UID), CMD);
			(void) putc('\n', stdout);
			reterr = 0;
		} else if (MdepAction(PID) < 0) {
			fprintf(stderr, "%d: %s (%s executing %s)\n",
			        PID, SysErr(), WhoIs(UID), CMD);
			reterr = 1;
		} else {			/* success! */
			if (reterr < 0)
				reterr = 0;

			if (Vflag) {
				if (Skill)
					printf("> sent #%d a %s",
					       PID, SigMap[SigPri]);
				else
					printf("> reniced #%d to %s%d", PID,
					       (SigPri >= 0)? "+": "", SigPri);

				if (!Iflag)
					printf(" (%s executing %s)",
					       WhoIs(UID), CMD);
				(void) putc('\n', stdout);
				(void) fflush(stdout);
			}
		}
	}

	if (reterr == -1) {
		fprintf(stderr, "%s: no matching processes", ProgName);
		if (MissedProcCnt > 0)
			fprintf(stderr, " (but %d could not be checked)",
			        MissedProcCnt);
		(void) putc('\n', stderr);
		reterr = 1;
	}

	return(reterr? EX_UERR: EX_OKAY);
}

/*
 * Whois(uid)
 *
 * Given a user id, return its associated user name.
 */
char *
WhoIs(uid)
	uid_T uid;
{
	extern char *strncpy();			/* avoid <string/strings> war */
	static char usrcache[64] = ROOTUSR;	/* user name */
	static uid_T uidcache = ROOTUID;	/* user id */
	struct passwd *pp;

	if (uid == ROOTUID)			/* be consistant w/ROOTUID */
		return(ROOTUSR);

	if (uid == uidcache)			/* lucky break: same person */
		return(usrcache);

	if ((pp=getpwuid((int) uid)) == NULL)	/* entry is gone? */
		(void) sprintf(usrcache, "<uid:%d>", (int)uid);
	else {
		(void) strncpy(usrcache, pp->pw_name, 63);
		usrcache[63] = '\0';
	}

	uidcache = uid;
	return(usrcache);
}

/*
 * Usage(error)
 *
 * The user typed something incorrect; explain their mistake (encoded
 * in `error'), display usage information, and exit.
 */
void
Usage(error)
	int error;
{
	switch (error) {
	    case E_USAGE:
		fprintf(stderr,
		        "Usage: %s [%s] [-ivfwn] {<tty> <user> <pid> <cmd>}\n",
		        ProgName, Skill? "-<signal>": "(+|-)<priority>");
		break;
	    case E_PRIOR:	/* unused... remains for posterity? */
		fprintf(stderr, "%s: no such priority (%d)\n",
		        ProgName, SigPri);
		break;
	    case E_SIGNO:
		fprintf(stderr, "%s: bad signal number (%d)\n",
		        ProgName, SigPri);
		break;
	    default:
		fprintf(stderr, "%s: internal error: Usage(%d)\n",
		        ProgName, error);
		break;
	}
	exit(EX_UERR);
	/*NOTREACHED*/
}

/*
 * SysErr()
 *
 * Return the error message described by `errno'.
 */
char *
SysErr()
{
	extern int errno;
#if (!defined(_FSTDIO) || defined(_ANSI_SOURCE) || defined(__STRICT_ANSI__)) \
      && __GNU_LIBRARY__ -0 < 6
	extern char *sys_errlist[];
	extern int sys_nerr;
#endif

	return((errno > sys_nerr)? "unknown error": (char *)sys_errlist[errno]);
}
