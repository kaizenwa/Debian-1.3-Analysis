#ifndef lint
static char rcsid[] = "$Header: /home/geyer/cvs/procps/skill/machdep/linux.c,v 1.1.1.1 1996/06/27 10:56:56 geyer Exp $";
#endif

/*
**  This program may be freely redistributed for noncommercial purposes.
**  This entire comment MUST remain intact.
**
**  Linux support by Chuck Blake (cblake@bbn.com)
**  Copyright 1994 by Jeff Forys (jeff@forys.cranbury.nj.us)
*/

#define	NO_MEXTERN
#include "conf.h"
#undef	NO_MEXTERN

#include <sys/resource.h>	/* for [get|set]priority() */
#include <linux/fs.h>		/* for MINOR() macro */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "proc/readproc.h"	/* maybe <proc/readproc.h> instead */

extern int MissedProcCnt;

/*
 * Define SigNames, NSig, and TtyDevDir here; they are used by other
 * routines and must be global.  Everyone seems to have their own
 * idea as to what NSIG should be.  Here, `NSig' is the number of
 * signals available, not counting zero.
 */

char *SigMap[] = {
	"0",
	"HUP", "INT", "QUIT", "ILL", "TRAP", "ABRT", "BUS", "FPE", "KILL",
	"USR1", "SEGV", "USR2", "PIPE", "ALRM", "TERM", "STKFLT", "CHLD",
	"CONT", "STOP", "TSTP", "TTIN", "TTOU", "IO", "XCPU", "XFSZ", "VTALRM",
	"PROF", "WINCH", "LOST", "PWR", "UNUSED" 
};

int NSig = 31;

#define	SETCMD(dst,src,maxlen) {			\
	extern char *strrchr();				\
	if (maxlen > 0) src[maxlen] = '\0';		\
	dst = (dst = strrchr(src, '/')) ? ++dst: src;	\
}

static char *TtyDevDir = "/dev";

int	Skill;			/* set 1 if running `skill', 0 if `snice' */
int	PrioMin, PrioMax;	/* min and max process priorities */
int	SigPri;			/* signal to send or priority to set */
pid_T	MyPid;			/* pid of this process */
uid_T	MyUid;			/* uid of this process */
char	*ProgName;		/* program name */

/*
 * This is the machine-dependent initialization routine.
 *
 *   - The following global variables must be initialized:
 *     MyPid, MyUid, ProgName, Skill, PrioMin, PrioMax, SigPri
 *   - The working directory will be changed to that which contains the
 *     tty devices (`TtyDevDir'); this makes argument parsing go faster.
 *   - If possible, this routine should raise the priority of this process.
 */

void skill_getpri(int *low, int *high);

void
MdepInit(pname)
	char *pname;
{
	extern char *rindex(), *SysErr();

	MyPid = (pid_T) getpid();
	MyUid = (uid_T) getuid();
	SETCMD(ProgName, pname, 0)

	/*
	 * If we are running as root, raise our priority to better
	 * catch runaway processes.
	 */
	if (MyUid == ROOTUID)
		(void) setpriority(PRIO_PROCESS, MyPid, PRIO_MIN);

	/*
	 * Determine what we are doing to processes we find.  We will
	 * either send them a signal (skill), or renice them (snice).
	 */
	Skill = (strstr(ProgName, "snice") == NULL);

	/*
	 * Set up minimum and maximum process priorities.
	 * Initialize SigPri to either default signal (`skill') or
	 * default priority (`snice').
	 */
	PrioMin = PRIO_MIN;
	PrioMax = PRIO_MAX;
	SigPri = Skill ? SIGTERM : 4;

	/*
	 * chdir to `TtyDevDir' to speed up tty argument parsing.
	 */
	if (chdir(TtyDevDir) < 0) {
		fprintf(stderr, "%s: chdir(%s): %s\n", ProgName, TtyDevDir,
		        SysErr());
		exit(EX_SERR);
	}
}

/*
 * Carry out an action on a particular process.  If this is `skill',
 * then send the process a signal, otherwise this is `snice' so change
 * it's priority.
 *
 * If 0 is returned, the operation was successful, otherwise -1 is
 * returned and `errno' set.
 */
int
MdepAction(pid)
	pid_T pid;
{
	if (Skill)
		return(kill((int)pid, SigPri));
	else
		return(setpriority(PRIO_PROCESS, (int)pid, SigPri));
}

/*
 * Now, set up everything we need to write a GetProc() routine.
 */

#include <fcntl.h>
char *SysErr();

/*
 * GetProc()
 *
 * Fill in and return a `struct ProcInfo' with information about the
 * next process.  If no processes are left, return NULL.
 */
struct ProcInfo *
GetProc()
{
	static struct ProcInfo procinfo;
	static PROCTAB* ptab = NULL;
	static proc_t p;

	/* If this is our first time here call openproc */
	if (!ptab && !(ptab = openproc(PROC_FILLCMD))) {
		fprintf(stderr, "%s: /proc: %s\n", ProgName, SysErr());
		exit(EX_SERR);
	}
	while (readproc(ptab, &p)) {
		procinfo.pi_uid = p.uid;
		procinfo.pi_pid = p.pid;
		procinfo.pi_cmd = p.cmdline ? p.cmdline[0] : p.cmd;
		procinfo.pi_tty = p.tty;
		return &procinfo;
	}
	(void) closeproc(ptab);
	ptab = NULL;
	return (struct ProcInfo*) NULL;
}
