/*
**	main.c
**
**	main body of program
**
**

	Modified by Jeff Chua (jchua@fedex.com)
	April 25 96
	uugetty sometimes hangs while trying to open() a modem port.
	The last line from /usr/adm/debug will look like ...
		"... D_INIT: opening line /dev/ttyS1"
	Modified so that failure to open the "tty" after 3 attempts will
	terminate the program so that init can restart another uugetty,
	rather than hanging forever!

*/

#define MAIN
#include "main.h"
#include "release.h"
#include <sys/time.h>		/* Jeff Chua, required by setitimer */

#ifdef UUGETTY
#include "uufuncs.h"
#endif /* UUGETTY */

#include "debug.h"

/* forward function declarations
*/

sig_t	timeout();
sig_t	schedalarm();
sig_t	rbalarm();
sig_t	shangup();
sig_t	squit();
sig_t	sterm();
sig_t	ssegv();
int	tputc();
void	exit_usage();
void	initvalues();
void	defs();
void	initline();
void	opentty();
void	dologin();

#ifdef DEBUG
void	debugstart();
#endif /* DEBUG */


/* trivial globals
*/

char	buf[MAXLINE+1];
FILE	*fp;
int	fd;
char	tbuf[64];


#define Perror(s)		{ \
					debug(D_INIT, \
						"Line %d: %s: Error %d: %s\n", \
						__LINE__, s, errno, \
						sys_errlist[errno]); \
					exit(errno); \
				}


#define start_ntimer(n)		sleep_msec(n)
#define start_timer()		sleep_msec(10000)	/* 10 seconds */
#define cancel_timer()		sleep_msec(0)

void sleep_msec(long msec);
void action(int signum);

void sleep_msec(long msec)
{
	struct itimerval rttimer;
	struct itimerval old_rttimer;
	long sec, usec;
	struct sigaction act;

        act.sa_handler = action;
        act.sa_mask = 0;
        act.sa_flags = SA_NOMASK;	/* allow own signal to be received */
        if(sigaction(SIGALRM, &act, NULL))		/* called by pause() */
		Perror("sigaction");

	sec = msec / 1000;
	usec = (msec % 1000) * 1000;

	/* set timer to wait once for "msec" seconds */
	/* note that interval must be 0 to wait once only ... */
	/* setting interval to non-zero will repeat forever until cancelled */
	rttimer.it_value.tv_sec		= sec;	/* initial wait, second */
	rttimer.it_value.tv_usec	= usec; /* initial wait, microsecond */
	rttimer.it_interval.tv_sec	= sec;	/* wait interval, second */
	rttimer.it_interval.tv_usec	= usec;	/* wait interval, microsecond */

	if(setitimer(ITIMER_REAL, &rttimer, &old_rttimer))
		Perror("setitimer");
}

void action(int signum)
{
	/*
	static count = 0;
	*/

	switch(signum) {
		case SIGALRM:
				/* called by setitimer */
				/*
				debug(D_INIT, "%s (%d)", "timeout!", count++);
				*/
				break;
		default:
				debug(D_INIT, "unknown signal %d", signum);
				break;
	}
}



/*
** main
*/

void main(argc, argv)
int 	argc;
char 	**argv;
{
	initvalues();		/* initialize all runtime variables */
	defs(argc, argv);	/* parse command line and defaults file */

#ifdef UUGETTY
	waitlocks();		/* hold on 'til the lockfiles are gone */
#endif /* UUGETTY */

	start_timer();
	initline();		/* initialize the line */
	cancel_timer();

	opentty();		/* open & initialize the tty */
	dologin();		/* get username and start login */
}


/* 
** initvalues
**
** initialize all runtime stuff to default values
*/

void initvalues()
{
	(void) signal(SIGINT,  SIG_IGN);	/* ignore ^C */
	(void) signal(SIGQUIT, squit);
	(void) signal(SIGTERM, sterm);
	(void) signal(SIGSEGV, ssegv);
	(void) signal(SIGHUP, shangup);
	(void) signal(SIGPIPE, SIG_IGN);

	strcpy(term, 	"unknown");		/* tty type */
	Device =	"unknown";		/* tty device */
	InitDevice =	Device;			/* device to init */
	LineD =		(char *) NULL;		/* lined */
	AutoBaud = 	FALSE;			/* no autobauding */
	AutoRate[0] = 	'\0';
	Check = 	FALSE;			/* ! check files and exit */
	CheckFile =	(char *) NULL;
	GtabId =	(char *) NULL;
	NoHangUp =	FALSE;			/* hangup the line first */
	TimeOut =	0;			/* no timeout */
	Version =	"/proc/version";	/* version for @V subst */
	delay = 	0;			/* delay before prompt */
	speed =		(char *) NULL;		
	clear =		TRUE;			/* clear the screen */
	login =		LOGIN;			/* login program */
	waitchar =	FALSE;			/* don't wait for a char */
	waitfor =	(char *) NULL;		/* no waitfor string */
	Connect =	(char *) NULL;		/* no connect string */
	defname =	(char *) NULL;		/* no defaults file */

#ifdef ISSUE
	issue =		ISSUE;			/* login banner */
#endif /* ISSUE */

#ifdef FIDO
	fido =		(char *) NULL;		/* fido program */
	emsi =		(char *) NULL;		/* emsi yes or no */
#endif /* FIDO */

#ifdef SCHED
	allow =		TRUE;			/* no scheduling */
#endif /* SCHED */

#ifdef DEBUG
	Debug =		0;			/* no debugging */
	Dfp =		NULL;			/* no debugging file */
#endif /* DEBUG */

#ifdef WARNCASE
	WarnCase =	TRUE;			/* moan about all caps */
#endif /* WARNCASE */

#ifdef RBGETTY
	minrbtime = 	8;			/* min time to call back */
	maxrbtime =	60;			/* max time to call back */
	interring =	6;			/* time between rings */
	minrings = 	1;			/* min rings to set off rb */
	maxrings =	3;			/* max rings to set off rb */
	rbmode = 	FALSE;			/* off by default */
#endif /* RBGETTY */

#ifdef UUGETTY
	MyName =	"uugetty";		/* hello... my name is */
#else
	MyName =	"getty";
#endif /* UUGETTY */
}


#ifdef SCHED
/*
** SetSched
**
** parse the SCHED information
*/

#define TS2SEC(a)	((a).tm_sec + (60 * (a).tm_min) + \
			(3600 * (a).tm_hour) + (86400 * (a).tm_wday))
#define CMP_TIME(a,b)	(TS2SEC(a) - TS2SEC(b))

void setsched(p)
char	*p;
{
	time_t		t_cur;
	struct tm	*cur, tb, te;
	int		count;
	char 		*s;

/* set up the time base
*/
	(void) time(&t_cur);
	cur = localtime(&t_cur);
	debug(D_SCH, "current time: %d:%d:%d", cur->tm_wday, cur->tm_hour,
		cur->tm_min);

/* parse the sched line
*/
	count = 0;
	allow = FALSE;
	tb.tm_sec = te.tm_sec = 0;
	while(sscanf((s = nextword(p, &count)), "%d:%d:%d-%d:%d:%d",
		     &tb.tm_wday, &tb.tm_hour, &tb.tm_min, 
		     &te.tm_wday, &te.tm_hour, &te.tm_min) == 6) {
		p += count;
		debug(D_SCH, "processing field %s", s);

/* hadle week overlaps
*/

		if(CMP_TIME(te, tb) < 0) {
			if(CMP_TIME(*cur, te) < 0)
				tb.tm_wday -= 7;
			else
				te.tm_wday += 7;
			debug(D_SCH, 
			 "week overlap detected.  New range: %d:%d:%d-%d:%d:%d",
			 tb.tm_wday, tb.tm_hour, tb.tm_min,
			 te.tm_wday, te.tm_hour, te.tm_min);
		}

/* set alarm based on times
*/
		if((CMP_TIME(tb, *cur) > 0) && 
		  ((TS2SEC(tb) < alrm) || (alrm == 0))) alrm = TS2SEC(tb);
		if((CMP_TIME(te, *cur) > 0) && 
		  ((TS2SEC(te) < alrm) || (alrm == 0))) alrm = TS2SEC(te);

/* set allow based on this
*/
		if ((CMP_TIME(tb, *cur) < 0) && 
		    (CMP_TIME(*cur, te) < 0)) allow = TRUE;
	}
	alrm -= TS2SEC(*cur);

	debug(D_SCH, "Alarm set to: %u", alrm);
	debug(D_SCH, "Allow: %s", ((allow) ? "TRUE" : "FALSE"));
}
#endif /* SCHED */


/*
** defs
**
** parse the command line and the defaults file
*/

void defs(count, args)
int	count;
char	**args;
{
	register int	c;
	DEF		**def;
	char		*p;
	char		termcap[1024];

/* first, the command line
*/

	while((c = getopt(count, args, "RC:D:ac:d:hr:t:w:")) != EOF) {
		switch(c) {
#ifdef RBGETTY
			case 'R':
				rbmode = TRUE;
				break;
#endif /* RBGETTY */
			case 'C':
				Connect = optarg;
				break;
			case 'D':
#ifdef DEBUG
				(void) sscanf(optarg, "%o", &Debug);
				if(Debug && Dfp == 0) debugstart();
#else
				logerr("DEBUG not compiled in");
#endif /* DEBUG */
				break;
			case 'a':
				InitDevice = optarg;
				break;
			case 'c':
				Check = TRUE;
				CheckFile = optarg;
				break;
			case 'd':
				defname = optarg;
				break;
			case 'h':
				NoHangUp = TRUE;
				break;
			case 'r':
				waitchar = TRUE;
				delay = (unsigned) atoi(optarg);
				break;
			case 't':
				TimeOut = (unsigned) atoi(optarg);
				break;
			case 'w':
				waitchar = TRUE;
				waitfor = optarg;
				break;
			case '?':
				exit_usage(2);
		}
	}

	
/* if we're just checking, exit here
*/

	if(Check) {
		(void) signal(SIGINT, SIG_DFL);
		(void) gtabvalue((char *) NULL, G_CHECK);
		exit(0);
	}


/* get line, speed, tty type, lined
*/

	if(optind < count)
		Device = args[optind++];
	else {
		logerr("no line given");
		exit_usage(2);
	}
	if(optind < count) GtabId = args[optind++]; 
	if(optind < count) strncpy(term, args[optind++], sizeof(term));
	if(optind < count) LineD = args[optind++];


/* check /etc/ttytype if term was not set on the command line
*/

#ifdef TTYTYPE
	if (strequal(term, "unknown")) {
		if(!(fp = fopen(TTYTYPE, "r"))) {
			logerr("open on %s failed: %s", TTYTYPE, 
				strerror(errno));
		} else {
			while((fscanf(fp, "%s %s", name, line)) != EOF) {
				if (strequal(line, Device)) {
					(void) strncpy(term, name, 
						sizeof(term));
					break;
				}
			}
			(void) fclose(fp);
		}
	}
#endif
      
	
/* now, get all that info in the defaults file
*/

	def = defbuild(defname);
#ifdef DEBUG
	if((p = defvalue(def, "DEBUG"))) (void) sscanf(p, "%o", &Debug);
	if(Debug) debugstart();
#endif /* DEBUG */
	if(! (SysName = defvalue(def, "SYSTEM"))) SysName = getuname();
	if((Version = defvalue(def, "VERSION")) && (*Version == '/')) {
		if((fp = fopen(Version, "r"))) {
			(void) fgets(buf, sizeof(buf), fp);
			(void) fclose(fp);
			buf[strlen(buf)-1] = '\0';
			Version = strdup(buf);
		}
	}
	if((p = defvalue(def, "LOGIN"))) login = p;
	if((p = defvalue(def, "ISSUE"))) issue = p;
	if((p = defvalue(def, "CLEAR")) && (strequal(p, "NO"))) 
		clear = FALSE;
	if((p = defvalue(def, "HANGUP")) && (strequal(p, "NO"))) 
		NoHangUp = TRUE;
	if((p = defvalue(def, "WAITCHAR")) && (strequal(p, "YES")))
		waitchar = TRUE;
	if((p = defvalue(def, "DELAY"))) delay = (unsigned) atoi(p);
	if((p = defvalue(def, "TIMEOUT"))) TimeOut = atoi(p);
	if((p = defvalue(def, "CONNECT"))) Connect = p;
	if((p = defvalue(def, "WAITFOR"))) {
		waitchar = TRUE;
		waitfor = p;
	}
	if((p = defvalue(def, "INIT"))) init = p;  
	if((p = defvalue(def, "INITLINE"))) InitDevice = p;
#ifdef FIDO
	fido = defvalue(def, "FIDO");
	emsi = defvalue(def, "EMSI");
#endif /* FIDO */

#ifdef SCHED
	if((p = defvalue(def, "SCHED"))) {
		setsched(p);

		if((! allow)) init = defvalue(def, "OFF");
	}
#endif /* SCHED */

#ifdef RBGETTY
	if((p = defvalue(def, "MINRBTIME"))) minrbtime = atoi(p);
	if((p = defvalue(def, "MAXRBTIME"))) maxrbtime = atoi(p);
	if((p = defvalue(def, "INTERRING"))) interring = atoi(p);
	if((p = defvalue(def, "MINRINGS"))) minrings = atoi(p);
	if((p = defvalue(def, "MAXRINGS"))) maxrings = atoi(p);
	if((p = defvalue(def, "RINGBACK")) && (strequal(p, "YES"))) rbmode = TRUE;
#endif /* RBGETTY */

/* find out how on earth to clear the screen
*/
	clrscr="";     /* initialize clrscr in case term=unknown or not specified in inittab */
	if(! strequal(term, "unknown")) {
		p = tbuf;
		if((tgetent(termcap, term) == 1)
		  && (! (clrscr = tgetstr("cl", &p)))) clrscr = "";
	}

/* construct /dev/ names for the lines
*/
	(void) sprintf(devname, "/dev/%s", Device);
	if(strequal(InitDevice, "unknown")) InitDevice = Device;
	(void) sprintf(initdevname, "/dev/%s", InitDevice);

#ifdef UUGETTY
	(void) sprintf(buf, LOCK, Device);
	lock = strdup(buf);
	if((p = defvalue(def, "ALTLOCK"))) {
		(void) sprintf(buf, LOCK, p);
		altlock = strdup(buf);
	} else if(! strequal(Device, InitDevice)) {
		(void) sprintf(buf, LOCK, InitDevice);
		altlock = strdup(buf);
	}

	debug(D_LOCK, "lock = (%s), altlock = (%s)", lock, altlock);
#endif /* UUGETTY */
}


#ifdef DEBUG
/*
** debugstart
**
** open up the debug file
*/

void debugstart()
{
	time_t	clock;

#if !defined(SYSLOG) || !defined(SYSL_DEBUG)
	(void) sprintf(buf, "/tmp/%s:%s", MyName, Device);
	if(! (Dfp = fopen(buf, "a+"))) {
		logerr("open debug file \"/tmp/%s:%s\" failed: %s",
		       MyName, Device, strerror(errno));
		exit(FAIL);
	} 
	if (fileno(Dfp) < 3) {
		if((fd = fcntl(fileno(Dfp), F_DUPFD, 3)) > 2) {
			(void) fclose(Dfp);
			Dfp = fdopen(fd, "a+");
		}
	}
	debug(D_ALL, "----------");
#endif
	(void) time(&clock);
	debug(D_ALL, "%s version %s started at %s", MyName, 
	      RELEASE, ctime(&clock));
}
#endif /* DEBUG */



#ifdef LOGUTMP
/*
** doutmp
**
** update the utmp and wtmp files
*/

void doutmp()
{
	int 		pid;
	time_t 		clock;
	struct utmp	*utp;
	struct utmp	uts;
	struct flock	fl;

	debug(D_UTMP, "update utmp/wtmp files");

	pid = getpid();
	(void) time(&clock);

	while((utp = getutent()) && utp->ut_pid != pid)
		continue;

	strncpy(uts.ut_line, Device, 12);
	if (utp)
		strncpy(uts.ut_id, utp->ut_id, sizeof(uts.ut_id));
	/*
	Leave ut_id alone, it should contain the 2-character key at the
	front of each inittab line, not an abbreviation for the tty line,
	because ttyS20 and ttyS2 both get the same id.
	And the pututline function locates the utmp entry to be updated by
	comparing ut_id's with the one passed in.
	Alan Wendt 7/26/95
	*/
	else
		strncpy(uts.ut_id, Device+3, 2);
	uts.ut_host[0] = '\0';
	uts.ut_addr = 0;
	uts.ut_user[0] = '\0';
	uts.ut_pid = pid;
	uts.ut_type = LOGIN_PROCESS;
	uts.ut_time = clock;
	
	debug(D_UTMP, "adding utmp entry: type: %d, pid: %d, line: %s, id: %c%c, time: %d, user: %s, host: %s, addr: %d",
		uts.ut_type, uts.ut_pid, uts.ut_line,
		(uts.ut_id[0] ? uts.ut_id[0] : ' '), 
		(uts.ut_id[1] ? uts.ut_id[1] : ' '), 
		uts.ut_time, uts.ut_user, uts.ut_host, uts.ut_addr);
	
	rewriteutent(&uts);

	if((fp = fopen(WTMP_FILE, "a"))) {
		debug(D_UTMP, "locking %s", WTMP_FILE);
		fl.l_type = F_WRLCK;
		fl.l_whence = fl.l_len = fl.l_start = 0;
		if (fcntl (fileno (fp), F_SETLKW, &fl) >= 0) {
		  (void) fseek(fp, 0L, 2);
		  debug(D_UTMP, "adding wtmp entry");
		  (void) fwrite((char *)&uts, sizeof(struct utmp), 
				1, fp);
		} else
		  logerr("flock of %s failed: %s", WTMP_FILE, strerror(errno));
		(void) fclose(fp);
	}
	endutent();
}
#endif /* LOGUTMP */


/*
** initline
**
** initialize the line, do waitchar, waitfor
*/

void initline()
{
	struct stat	st;
	TERMIO		termio;
	char		ch;
	int		flags;
	char *		wait = waitfor;

	int		busys = 0;

/* set the line owned by root
*/

	(void) chmod(devname, 0600);
	if (! stat(devname, &st)) (void) chown(devname, 0, st.st_gid);


/* update utmp/wtmp
*/

#ifdef LOGUTMP
	doutmp();
#endif /* LOGUTMP */


/* close any old stdin, stdout, stderr
*/

	(void) close(0);
	(void) close(1);
	(void) close(2);


/* vhangup the line
*/
	debug(D_INIT, "opening line %s", devname);

	while(((fd = open(devname, O_RDWR | O_NDELAY)) < 0)) {
		switch(errno) {
			case EBUSY:
				if(++busys > 3)	/* try for 3 times */
					exit(FAIL);
				debug(D_INIT, "%s busy %d", devname, busys);
				pause();
				break;
			case EAGAIN:
				logerr("open failed on \"%s\": %s", devname, 
				strerror(errno));
				exit(FAIL);
			default:
				Perror("open");
		}
	}

	ioctl(fd, TIOCSCTTY, 1);
	(void) ioctl(fd, TCGETS, &termio);
	if(NoHangUp) {
		termio.c_cflag &= ~HUPCL;
	} else {
		termio.c_cflag &= ~CBAUD;
		termio.c_cflag |= B0;
	}
	(void) ioctl(fd, TCSETSF, &termio);
	if(! NoHangUp) sleep(2);
	gtab = gtabvalue(GtabId, G_FORCE);
	settermio(&(gtab->itermio), INITIAL);
#ifndef GDB_FRIENDLY
	(void) signal(SIGHUP, SIG_IGN);
	vhangup();
	(void) signal(SIGHUP, shangup);
#endif
	close(fd);


/* now, the init device is opened ONLY if INIT or WAITCHAR is requested
*/

	if((init) || (waitchar)) {
		debug(D_INIT, "opening init line: %s", initdevname);
		if((fd = open(initdevname, O_RDWR | O_NDELAY)) < 0)
			if((errno == EBUSY) || (errno == EAGAIN)) {
				debug(D_INIT, 
				  "line in use... exiting to reinit");
				exit(0);
			} else {
				logerr("open failed on \"%s\": %s",
				       initdevname, strerror(errno));
				exit(FAIL);
			}
		if(fd != 0) {
			logerr("open fd != 0, please report this bug [1]");
			exit(FAIL);
		}
		if(dup(0) != 1) {
			logerr("dup fd != 1, please report this bug [1]");
			exit(FAIL);
		}
		if(dup(0) != 2) {
			logerr("dup fd != 2, please report this bug [1]");
			exit(FAIL);
		}

		setbuf(stdin, (char *) NULL);
		setbuf(stdout, (char *) NULL);
		setbuf(stderr, (char *) NULL);

		settermio(&(gtab->itermio), INITIAL);

		flags = fcntl(STDIN, F_GETFL, 0);
		(void) fcntl(STDIN, F_SETFL, flags & ~O_NDELAY);

/* init the line
*/

		if(init) {
			debug(D_INIT, "initializing line");

/* set CLOCAL and other stuff for init, so ATZ works right
*/
			(void) ioctl(STDIN, TCGETS, &termio);
			termio.c_cflag |= CLOCAL;
			termio.c_iflag &= ~(ICRNL);
			termio.c_lflag &= ~(ICANON);
			(void) ioctl(STDIN, TCSETSF, &termio);

			if(chat(init) == FAIL) {
				debug(D_INIT, "init failed... aborting");
				logerr("warning: INIT sequence failed on %s",
				       initdevname);
				exit(FAIL);
			}

			settermio(&(gtab->itermio), INITIAL);
		}
	}

/* set the alarm if requested
*/

#ifdef SCHED
	if(alrm) {
		(void) signal(SIGALRM, schedalarm);
		(void) alarm(alrm);
		debug(D_SCH, "SCHED alarm set");
	}
#endif /* SCHED */

/* wait for a char
*/
#ifdef SCHED
	if(allow)
#endif /* SCHED */
	  if(waitchar) {
		debug(D_INIT, "waiting for a character...");

		(void) ioctl(STDIN, TCFLSH, 0);
		(void) read(STDIN, &ch, 1);		/* blocks */
		debug(D_INIT, "got it");

#ifdef UUGETTY
/* check the lockfiles
*/
		if (checklock(lock) || (altlock && checklock(altlock))) {
			(void) signal(SIGHUP, SIG_IGN);
			ioctl(STDIN, TIOCNOTTY);
			exit(0);
		}
#endif /* UUGETTY */

		if(wait) {
			if(ch == *wait) wait++;
			if((*wait) && (expect(wait) == FAIL)) {
				debug(D_INIT, "WAITFOR match failed");
				exit(0);
			}
		}
	}
}


#ifdef RBGETTY
/*
** dorb
**
** watch the pattern of incoming rings (defined by the WAITFOR
** string) to do a ringback connect
**
** contributed by: Shane Alderton (shanea@extra.ucc.su.oz.au)
*/

void dorb()
{
	time_t	lasttime = 0;
	time_t	currenttime = 0;
	time_t	elapsed;
	int	ringcount = 1;
	boolean success = FALSE;
	char *	wait;
	char	ch;

	while(! success) {
		(void) time(&lasttime);
		(void) signal(SIGALRM, rbalarm);
		(void) alarm(maxrbtime);

		debug(D_RB, "waiting for another ring...");
		(void) ioctl(STDIN, TCFLSH, 0);
		(void) read(STDIN, &ch, 1);		/* this blocks */
		debug(D_RB, "got something");

		(void) alarm(0);
		(void) signal(SIGALRM, SIG_DFL);
		wait = waitfor;
		if(wait) {
			if(ch == *wait) wait++;
			if((*wait) && (expect(wait) == FAIL)) {
				debug(D_RB, "WAITFOR match failed");
				exit(0);
			}
			debug(D_RB, "WAITFOR string matched");
		}
		
		(void) time(&currenttime);
		elapsed = currenttime - lasttime;

		if(elapsed <= interring) {
			ringcount++;
			debug(D_RB, "got ring number %d.", ringcount);
		} else if (elapsed < minrbtime) {
			debug(D_RB, "%d rings then one too quickly... ",
				ringcount);
			debug(D_RB, "treating as a new call");
			ringcount = 1;
		} else if (ringcount > maxrings) {
			debug(D_RB, "%d rings... too many", ringcount);
			debug(D_RB, "... exiting to reinit");
			exit(0);
		} else if (ringcount < minrings) {
			debug(D_RB, "%d rings... too few", ringcount);
			debug(D_RB, "... exiting to reinit");
			exit(0);
		} else { 
			debug(D_RB, "got ringback...");
			success = TRUE;
		}
	}
	debug(D_RB, "ringback finished");
}
#endif /* RBGETTY */
		


/*
** opentty
**
** open the tty device.
** the tty is opened in blocking mode, unless WAITFOR is specified
*/

void opentty()
{
	int	flags, i, cbaud, nspeed;
	char	ch;
	GTAB	*gt;

/* close any old stdin, stdout, stderr
*/
	(void) close(0);
	(void) close(1);
	(void) close(2);

#ifdef SCHED
/* handle allow
*/

	debug(D_SCH, (allow ? "not sleeping" : "sleeping"));
	while(! allow) pause();
#endif /* SCHED */

	debug(D_RUN, "opening line: %s", devname);

	if((fd = open(devname, (waitfor) ? 
	  (O_RDWR | O_NDELAY) : (O_RDWR))) < 0) {
		if (errno == EAGAIN) {
			debug(D_RUN, 
			  "open got EAGAIN... exiting to reinitialize");
			exit(0);
		}
		logerr("open failed on \"%s\": %s", devname, strerror(errno));
		exit(FAIL);
	}
	if(fd != 0) {
		logerr("open fd != 0, please report this bug [2]");
		exit(FAIL);
	}
	if(dup(0) != 1) {
		logerr("dup fd != 1, please report this bug [2]");
		exit(FAIL);
	}
	if(dup(0) != 2) {
		logerr("dup fd != 2, please report this bug [2]");
		exit(FAIL);
	}

#ifdef UUGETTY
/* now we can lock the line
*/
	lockline();
#endif /* UUGETTY */

	ioctl(fd, TIOCSCTTY, 1);
#ifndef GDB_FRIENDLY
	setpgrp();
#endif

	setbuf(stdin, (char *) NULL);
	setbuf(stdout, (char *) NULL);
	setbuf(stderr, (char *) NULL);

	flags = fcntl(STDIN, F_GETFL, 0);
	(void) fcntl(STDIN, F_SETFL, flags & ~O_NDELAY);

	gtab = gtabvalue(GtabId, G_FORCE);
	settermio(&(gtab->itermio), INITIAL);

	if(delay) {
		debug(D_RUN, "delay(%d)", delay);

		(void) sleep(delay);
		(void) fcntl(STDIN, F_SETFL, flags | O_NDELAY);
		while (read(STDIN, &ch, 1) == 1);
		(void) fcntl(STDIN, F_SETFL, flags & ~O_NDELAY);
	}

#ifdef RBGETTY
	if(rbmode) dorb();
#endif /* RBGETTY */

	if(Connect) {
		debug(D_RUN, "performing connect sequence");

		cbaud = 0;
		if(strequal(Connect, "DEFAULT")) Connect = DEF_CONNECT;
		if((chat(Connect)) == FAIL) {
			logerr("warning: CONNECT sequence failed");
			debug(D_RUN, "connect sequence failed... aborting");
			exit(FAIL);
		}
		if(AutoBaud) {
			debug(D_RUN, "AutoRate = (%s)", AutoRate);
#ifdef TELEBIT
			if(strequal(AutoRate, "FAST")) 
				(void) strcpy(AutoRate, TB_FAST);
#endif /* TELEBIT */
			if((nspeed = atoi(AutoRate)) > 0)
				for (i=0; speedtab[i].nspeed; i++)
					if(nspeed == speedtab[i].nspeed) {
						cbaud = speedtab[i].cbaud;
						speed = speedtab[i].speed;
						break;
					}
		}
		if(cbaud) {
			debug(D_RUN, "setting speed to %s", speed);
			if((gt = gtabvalue(speed, G_FIND)) && 
			  strequal(gt->cur_id, speed)) 
				gtab = gt;
			else {
				gtab->itermio.c_cflag = 
				  (gtab->itermio.c_cflag & ~CBAUD) | cbaud;
				gtab->ftermio.c_cflag = 
				  (gtab->ftermio.c_cflag & ~CBAUD) | cbaud;
			}
			settermio(&(gtab->itermio), INITIAL);
		}
	}
}
				

/*
** dologin
**
** print the login banner, get the user's name, and exec login
*/

void dologin()
{
	struct utmp	*utmp;
	int		cbaud, i;
	TERMIO		termio;
	

	for(;;) {
		Nusers = 0;
		setutent();
		while((utmp = getutent())) 
			if(utmp->ut_type == USER_PROCESS) Nusers++;
		endutent();

		cbaud = gtab->itermio.c_cflag & CBAUD;
		for(i=0; speedtab[i].cbaud != cbaud; i++);
		Speed = speedtab[i].speed;

#ifdef ISSUE
		if (clear && *clrscr) {
			(void) tputs(clrscr, 1, tputc);
		}
		fputc('\r', stdout);

		if(*issue != '/') {
			(void) Fputs(issue, stdout);
			(void) fputs("\r\n", stdout);
		} else if((fp = fopen(issue, "r"))) {
			while(fgets(buf, sizeof(buf), fp))
			  (void) Fputs(buf, stdout);
			(void) fclose(fp);
		}
#endif /* ISSUE */

login_prompt:
		(void) ioctl(STDIN, TCFLSH, 0);
#ifdef FIDO
	if (emsi && (strcmp(emsi,"yes") == 0)) 
		(void) Fputs("**EMSI_REQA77E\r", stdout);
#endif
		(void) Fputs(gtab->login, stdout);
		if(TimeOut > 0) {
			(void) signal(SIGALRM, timeout);
			(void) alarm((unsigned) TimeOut);
		}

		switch(getlogname(&termio, buf, MAXLINE)) {
#ifdef FIDO
			case FIDOCALL:
				(void) signal(SIGALRM, SIG_DFL);
				(void) alarm(0);

				login = fido;
				logerr("Fido Call Detected");
#endif /* FIDO */
			case SUCCESS:
				if(buf[0] == '-') {
					printf("\n'-' not allowed as first character of login name\n");
					sleep(5);
					break;
				}
				(void) signal(SIGALRM, SIG_DFL);
				(void) alarm(0);

				if(TimeOut > 0) {
					(void) alarm((unsigned) 0);
					(void) signal(SIGALRM, SIG_DFL);
				}

				termio.c_iflag |= gtab->ftermio.c_iflag;
				termio.c_oflag |= gtab->ftermio.c_oflag;
				termio.c_cflag |= gtab->ftermio.c_cflag;
				termio.c_lflag |= gtab->ftermio.c_lflag;
				termio.c_line |= gtab->ftermio.c_line;
				settermio(&termio, FINAL);
#ifdef SETTERM
				setenv("TERM", term, TRUE);
#endif /* SETTERM */
				debug(D_RUN, "execing login");
				(void) execl(login, 
				  "login", buf, (char *)NULL);
				debug(D_RUN, 
				  "exec failed: %s; trying with /bin/sh",
				  strerror(errno));
				(void) execl("/bin/sh", "sh", "-c", login,
				  buf, (char *) NULL);
				logerr("exec of %s failed: %s", login,
				       strerror(errno));
				exit(FAIL);

			case BADSPEED:
				GtabId = gtab->next_id;
				gtab = gtabvalue(GtabId, G_FORCE);
				settermio(&(gtab->itermio), INITIAL);
				break;

#ifdef WARNCASE
			case BADCASE:
				for(i=0; bad_case[i] != (char *) NULL; i++)
					(void) fputs(bad_case[i], stdout);
				goto login_prompt;
#endif /* WARNCASE */

			case NONAME:
				break;
		}
	}
}


/*
** exit_usage
**
** moan about bad command line options
*/

void exit_usage(code)
int code;
{
	logerr(USAGE, MyName);
	exit(code);
}


/*
** timeout
**
** the user types way too damn slow
*/

sig_t timeout()
{
	TERMIO termio;

	(void) sprintf(MsgBuf, "\nTimed out after %d seconds.\n", TimeOut);
	(void) Fputs(MsgBuf, stdout);

	(void) ioctl(STDIN, TCGETS, &termio);
	termio.c_cflag &= ~CBAUD;
	termio.c_cflag |= B0;
	(void) ioctl(STDIN, TCSETAF, &termio);
	sleep(5);

	exit(1);
}


/*
** tputc
**
** just because
*/

int tputc(c)
char c;
{
	return(fputc(c, stdout));
}


#ifdef SCHED
/*
** schedalarm
**
** signal handler for SCHED
*/

sig_t schedalarm()
{
	debug(D_SCH, "alarm clock caught");
	debug(D_SCH, "... exiting to reinit");
	exit(0);
}
#endif /* SCHED */


#ifdef RBGETTY
/*
** rbalarm
**
** signal handler for RINGBACK
*/

sig_t rbalarm()
{
	debug(D_RB, "alarm clock caught... waited too long for ringback");
	debug(D_RB, "... exiting to reinit");
	exit(0);
}
#endif /* RBGETTY */



/* 
** a bunch of signal handlers for better logging
*/

sig_t shangup()
{
	debug(D_RUN, "Caught HANGUP signal");
	logerr("exiting on HANGUP signal");
	exit(0);
}

sig_t ssegv()
{
	debug(D_RUN, "Caught SEGV signal");
	logerr("Segmentation fault");
	exit(1);
}

sig_t squit()
{
	debug(D_RUN, "Caught QUIT signal");
	logerr("exiting on QUIT signal");
	exit(1);
}

sig_t sterm()
{
	debug(D_RUN, "Caught TERM signal");
	logerr("exiting on TERM signal");
	exit(1);
}
