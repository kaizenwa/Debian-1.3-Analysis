#ifndef lint
static char rcsid[] = "$Header: /home/geyer/cvs/procps/skill/argparse.c,v 1.1.1.1 1996/06/27 10:56:55 geyer Exp $";
#endif

/*
**  This program may be freely redistributed for noncommercial purposes.
**  This entire comment MUST remain intact.
**
**  Copyright 1994 by Jeff Forys (jeff@forys.cranbury.nj.us)
*/

#define	NO_AEXTERN
#include "conf.h"
#undef	NO_AEXTERN

#include <stdio.h>
#include <ctype.h>
#include <pwd.h>

/*
 *  Define/initialize linked lists of user supplied things.
 */
tty_T	*TtyList = NULL;		/* list of ttys */
uid_T	*UidList = NULL;		/* list of user id's */
pid_T	*PidList = NULL;		/* list of process id's */
cmd_T	*CmdList = NULL;		/* list of commands */
int	TtyIndx = 0, UidIndx = 0, PidIndx = 0, CmdIndx = 0;
int	Fflag = 0, Iflag = 0, Nflag = 0, Vflag = 0, Wflag = 0;

/*
 * ArgParse(argc, argv)
 *
 * Destructively parse argv[] into appropriate global variables.
 */
void
ArgParse(argc, argv)
	int argc;
	char *argv[];
{
	extern int atoi();
	extern char *calloc();
	struct passwd *pp;
	struct stat st;
	int argcnt = argc;	/* saved so ALLOC() knows max mem to allocate */
	int haveno = 0;		/* set when we get a signal or priority */
	int help, argloop;

#define	ALLOC(cnt,type,loc,nstr)					\
	if ((loc = (type) calloc((unsigned)cnt,sizeof(type))) == NULL)	\
		Exceed(nstr)

	/*
	 * This is the main argument parsing loop.  At first glance
	 * this may look confusing.  We destructively move across
	 * arguments by incrementing that which "*argv" points to.
	 *
	 * Basically, for each argument, we first check if we have a
	 * flag (flags are preceded by a `-').  If so, then we enter
	 * the flag loop, processing each character until we reach
	 * end of string, or we hit a flag that requires special
	 * processing (i.e. any of "ltucp", "SIGNO", or "PRIORITY").
	 *
	 * Second, we check for the special case flag, "+PRIORITY".
	 * Finally, if we do not have a flag, we simply go on to the
	 * argument classification phase.  This is where we decide
	 * if a string represents a tty, a user name, a process id,
	 * or a command.
	 *
	 * If at any time something incomprehensible is unearthed,
	 * we call Usage(), who in turn calls exit().  Using "-l" also
	 * forces an immediate exit() (through ListSigs() or Usage()).
	 */
	while (--argc > 0) {		/* for each argument */
		help = 0;
		if (**(++argv) == '-') {	/* found a flag */
		    do {
			argloop = 0;
			switch (*++(*argv)) {
			    case 'l':
				if (Skill)
					ListSigs();
				else
					Usage(E_USAGE);
				/*NOTREACHED*/
				break;
			    case 'f':	/* fast mode (if possible) */
				Fflag++;
				argloop++;	/* look for more flags */
				break;
			    case 'i':	/* interactive mode */
				Iflag++;
				argloop++;	/* look for more flags */
				break;
			    case 'v':	/* verbose mode */
				Vflag++;
				argloop++;	/* look for more flags */
				break;
			    case 'w':	/* display warning messages */
				Wflag++;
				argloop++;	/* look for more flags */
				break;
			    case 'n':	/* display pid's (do not act on them) */
				Nflag++;
				argloop++;	/* look for more flags */
				break;
			    case 't':
			    case 'u':
			    case 'c':
			    case 'p':
				help = **argv;
				break;
			    default:
				/*
				 * This code is kind of gnarly.  The user
				 * may specify "-SIGNO" or "-PRIORITY"
				 * (depending on what we are doing).  This
				 * is further complicated by the fact that
				 * SIGNO may be the signal *name*.  The
				 * special case of "+PRIORITY" is handled
				 * outside this switch() statement.
				 */
				if (haveno)
					Usage(E_USAGE);

				if (isdigit(**argv)) {
					if (Skill) {
						SigPri = atoi(*argv);
						if (SigPri < 0 || SigPri > NSig)
							Usage(E_SIGNO);
					} else {
						SigPri = -atoi(*argv);
						if (SigPri < PrioMin)
							SigPri = PrioMin;
					}
					haveno++;
				} else if (!Skill) {
					Usage(E_USAGE);
				} else for (SigPri=0; SigPri <= NSig; SigPri++)
					if (STREQU(SigMap[SigPri], *argv)) {
						haveno++;
						break;
					}
				if (!haveno) {
					fprintf(stderr, "%s: Unknown signal (%s); %s -l lists signals.\n",
					        ProgName, *argv, ProgName);
					exit(EX_UERR);
				}
				break;
			}
		    } while (argloop && *(*argv+1) != '\0');

		    /*
		     * There can be no more flags in this string.  If the
		     * help variable was not set, we can short-circuit the
		     * remainder of the argument processing and move on to
		     * the next argument.
		     */
		    if (!help)			/* go to next arg */
			continue;
		} else if (!Skill && **argv == '+') {	/* "+priority" */
			++(*argv);
			if (isdigit(**argv)) {
				SigPri = atoi(*argv);
				if (SigPri > PrioMax)
					SigPri = PrioMax;
				haveno++;
				continue;	/* go to next arg */
			}
			Usage(E_USAGE);	/* dopey priority: usage error */
		}

		/*
		 * At this point, all we have left are ttys, user names,
		 * processes id's, and commands.  If we have `help', our
		 * job is easy; otherwise we have to hunt for things.
		 */
		if (help && !(*++(*argv) || (--argc && *++argv)))
			Usage(E_USAGE);	/* no arg following flag */

		if (!help || help == 't') {	/* tty? */
			if (stat(*argv,&st)>=0&&(st.st_mode&S_IFMT)==S_IFCHR) {
				if (TtyIndx == 0)
					ALLOC(argcnt, tty_T *, TtyList, "tty");
				*(TtyList + TtyIndx++) = st.st_rdev;
				help = -1;
			} else if (help)
				Not(*argv, "tty");
		}

		if (!help || help == 'u') {	/* user name? */
 			if ((pp=getpwnam(*argv)) != NULL) {
				if (UidIndx == 0)
					ALLOC(argcnt, uid_T *, UidList, "user");
				*(UidList + UidIndx++) = pp->pw_uid;
				help = -1;
			} else if (help)
				Not(*argv, "user name");
		}

		if (!help || help == 'p') {	/* process id? */
			if (isdigit(**argv)) {
				if (PidIndx == 0)
					ALLOC(argcnt, pid_T *, PidList, "pid");
				*(PidList + PidIndx++) = atoi(*argv);
				help = -1;
			} else if (help)
				Not(*argv, "process id");
		}

		if (help > -1) {		/* default: it's a command */
			if (CmdIndx == 0)
				ALLOC(argcnt, cmd_T *, CmdList, "command");
			*(CmdList + CmdIndx++) = *argv;
		}
	}

	if (TtyIndx == 0 && UidIndx == 0 && PidIndx == 0 && CmdIndx == 0)
		Usage(E_USAGE);

	/*
	 *  If this is not the superuser and no specific users were
	 *  mentioned, we assume they only want to deal with their
	 *  own processes.  This cuts down on error messages since
	 *  the general user is not usually allowed to modify the
	 *  state of other users' processes.
	 */
	if (UidIndx == 0 && MyUid != ROOTUID) {
		ALLOC(1, uid_T *, UidList, "user");
		*(UidList + UidIndx++) = MyUid;
	}
#undef	ALLOC
}

/*
 * ListSigs()
 *
 * Display a list of available signals and exit.
 */
void
ListSigs()
{
	register int signo;
	register int didprint = 0;

	for (signo = 1; signo <= NSig; signo++)
		if (!isdigit(*SigMap[signo])) {
			printf("%s%c", SigMap[signo],	/* 16 signals/line */
			       (++didprint % 16) == 0? '\n': ' ');
		}

	if ((didprint % 16) != 0)
		(void) putc('\n', stdout);

	exit(EX_OKAY);
}

/*
 * Not(thought, this)
 *
 * The user incorrectly forced a type (e.g. "-u tty00").
 * Display an error message and exit.
 */
void
Not(thought, this)
	char *thought, *this;
{
	fprintf(stderr, "%s: %s: not a %s\n", ProgName, thought, this);
	exit(EX_UERR);
}

/*
 * Exceed(what)
 *
 * Ran out of memory allocating space for `what'.
 */
void
Exceed(what)
	char *what;
{
	fprintf(stderr, "%s: out of memory (can't alloc %s)\n", ProgName, what);
	exit(EX_SERR);
}
