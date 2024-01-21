/*
 * init.h	Several defines and declarations to be
 *		included by all modules of the init program.
 *
 * Version:	@(#)init.h  1.00  04-Feb-1997  miquels@cistron.nl
 *
 */

/* Standard configuration */
#define CHANGE_WAIT 0			/* Do not change runlevel while
					   waiting for a process to exit */
/* Debug and test modes */
#define DEBUG	   0			/* Debug code off */
#define INITDEBUG  0			/* Fork at startup to debug init. */
#undef  ROOTFS	   "/dev/hda2"		/* Root fs to use */

/* Some constants */
#define INITPID	   1			/* pid of first process */
#define PIPE_FD    10			/* Fileno of initfifo. */

/* Failsafe configuration */
#define MAXSPAWN   10			/* Max times respawned in.. */
#define TESTTIME   120			/* this much seconds */
#define SLEEPTIME  300			/* Disable time */

/* Default path inherited by every child if it's not set. */
#define PATH_DFL   "PATH=/usr/local/sbin:/sbin:/bin:/usr/sbin:/usr/bin"


/* Prototypes. */
void write_utmp_wtmp(char *user, char *id, int pid, int type, char *line);
void write_wtmp(char *user, char *id, int pid, int type, char *line);
void log(int loglevel, char *fmt, ...);
void set_term(int how);
void print(char *fmt);

/* Actions to be taken by init */
#define RESPAWN			1
#define WAIT			2
#define ONCE			3
#define	BOOT			4
#define BOOTWAIT		5
#define POWERFAIL		6
#define POWERWAIT		7
#define POWEROKWAIT		8
#define CTRLALTDEL		9
#define OFF		       10
#define	ONDEMAND	       11
#define	INITDEFAULT	       12
#define SYSINIT		       13
#define POWERFAILNOW           14
#define KBREQUEST               15

/* Information about a process in the in-core inittab */
typedef struct _child_ {
  int flags;			/* Status of this entry */
  int exstat;			/* Exit status of process */
  int pid;			/* Pid of this process */
  time_t tm;			/* When respawned last */
  int count;			/* Times respawned in the last 2 minutes */
  char id[8];			/* Inittab id (must be unique) */
  char rlevel[12];		/* run levels */
  int action;			/* what to do (see list below) */
  char process[128];		/* The command line */
  struct _child_ *new;		/* New entry (after inittab re-read) */
  struct _child_ *next;		/* For the linked list */
} CHILD;

/* Values for the 'flags' field */
#define RUNNING			2	/* Process is still running */
#define KILLME			4	/* Kill this process */
#define DEMAND			8	/* "runlevels" a b c */
#define FAILING			16	/* process respawns rapidly */
#define WAITING			32	/* We're waiting for this process */
#define ZOMBIE			64	/* This process is already dead */
#define XECUTED		128	/* Set if spawned once or more times */

/* Log levels. */
#define L_CO	1		/* Log on the console. */
#define L_SY	2		/* Log with syslog() */
#define L_VB	(L_CO|L_SY)	/* Log with both. */

#ifndef NO_PROCESS
#  define NO_PROCESS 0
#endif
#ifndef SIGPWR
#  define SIGPWR SIGUSR1
#endif

/*
 *	Global variables.
 */
extern CHILD *family;
extern int wrote_reboot;

