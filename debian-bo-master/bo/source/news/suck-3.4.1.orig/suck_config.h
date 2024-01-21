#ifndef _SUCK_CONFIG_H
#define _SUCK_CONFIG_H 1

/* Config Options */

/*  DEBUG Options */
/*  DEBUG1 = debug both.c, particularly sgetline() and fileargs.c */
/*  DEBUG2 = debug suck.c */
/*  DEBUG3 = debug rpost.c  */
/*  dont define DEBUG2 and DEBUG3 at same time  */
/*  warning if you have DEBUG1 defined when you compile suck.c, you will get */
/*  every line of every article in the debug output file twice, this can  */
/*  create a debug file many megs in size */
/* #define DEBUG1 1 */
/* #define DEBUG2 1 */
/* #define DEBUG3 1 */

#define DEFAULT_NNRP_PORT 119 	/* port to talk to on remote machine */

#ifdef HAVE_GETTIMEOFDAY	/* need gettimeofday() function */
/*  TIMER Option */
/* To see the approximate BPS while downloading articles, uncomment this */
/* If you don't like it, or want a tiny speed increase, comment this out */
#define TIMER 1
#endif /* HAVE_GETTIMEOFDAY */

/* If you don't EVER plan to use the KILLFILE stuff, comment this out for a slight speed increase */
/* If you want to, or plan to, use the KILLFILE stuff, uncomment this */
#define KILLFILE 1

/* If you want the kill/keep file routine to keep articles that are both flagged as */
/* delete and keep (either by matching both a delete group and a keep group, or by not matching */
/* both a delete group and keep group), then uncomment this next one.  If you want them deleted, */
/* then comment this out */
#define KILL_TIEBREAKER_KEEP 1

/* If your system doesn't have regex.h, you can't use this */
/* If you don't want to use regex for the killfile stuff comment */
/* out the #define USE_REGEX 1 line */
#ifdef HAVE_REGEX_H
#define USE_REGEX 1
#endif

/* This is the character used in killfiles to tell if we should do a case sensitive or a */
/* case insensitive compare.  It can be overridden in a killfile. */
#define KILLFILE_QUOTE '"'

#ifdef HAVE_SELECT	/* if the system doesn't support select, then you can't use this */
/* if you want to timeout if no read after X seconds define this   */
/* as number of seconds before timeout 				   */
/* else, comment it out.  If you comment it out, if link goes down */
/* we'll just sit, twiddling our bits, until whenever.             */
#define TIMEOUT 300
#endif /* HAVE_SELECT */

/* signal which will interrupt us DON'T USE SIGKILL OR SIGSTOP */
/* if you don't want to be able to abort, and a miniscule speed */
/* increase, comment this out */
/* WARNING: if you plan to use the killprg part of the killfile routines */
/* then leave this in.  If you don't, and the child program dies unexpectedly */
/* suck will also die. */
#define MYSIGNAL SIGTERM
#define PAUSESIGNAL SIGUSR1

/* If you want to have suck check in your history file for articles */
/* that you already have, uncomment this.			    */
/* If you use DBM, DBZ, or NDBM then you need to edit the Makefile  */
/* to show which DB scheme you use. 				    */
/* If you have a flat file, you need to edit the Makefile so suck   */
/* knows where to find the history file 			    */
#define CHECK_HISTORY 1

/* To use the old, slow flat file check history routines, uncomment this and */
/* CHECK_HISTORY above.  If you use DBM, DBZ, or NDBM, this doesn't apply */
/* #define CHECK_HISTORY_OLD 1 */

/* don't comment this out */
#ifndef HISTORY_FILE
#define HISTORY_FILE "/usr/lib/news/history"
#endif

/* If your system doesn't like the lock file stuff in suck.c  */
/* comment this out */
#define LOCKFILE 1

/* FULL PATH of error log used if -e option specifed to any of the programs */
/* can be overridden at the command line with -E option */
#define ERROR_LOG "./suck.errlog"

/* FULL PATH of status messages log if -s option specified to any of the programs */
/* can be overridden at the command line with -S option */
#define STATUS_LOG "/dev/null"		/* /dev/null = silent mode */

/* FIRST Character used in args to signify read arguments from a file */
#define FILE_CHAR '@'

/* Comment character in arg file, everything after this on a line is ignored */
#define FILE_ARG_COMMENT '#'

/*  FILE NAMES */
/* Data files read in */
#define N_OLDRC 	"sucknewsrc"		/* what newsgroups to download			 */
#define N_KILLFILE 	"suckkillfile"		/* Parameter file for which articles s NOT to download */
#define N_SUPPLEMENTAL 	"suckothermsgs"		/* list of article nrs to add to our list to download */

/* TEMP FILES created */
#define N_NEWRC "suck.newrc"
#define N_SORTED "suck.sorted"
#define N_RESTART "suck.restart"
#define N_KILLLOG "suck.killlog"
#define N_LOCKFILE "suck.lock"
#define N_PHRASES "suck.phrases"

/* Old copy of N_OLDRC (saved just in case) */
#define N_OLD_OLDRC    "sucknewsrc.old"

/* Various DIRECTORY PATHS, these can be overriden from command line */
#define N_TMPDIR "."		/* location of Temp Files */
#define N_DATADIR "."		/* location of Data Files */
#define N_MSGDIR "./Msgs"	/*location of articles produced by suck, if multifile option selected */

/* Argument substition strings for rpost */
#define RPOST_FILTER_IN "$$i"
#define RPOST_FILTER_OUT "$$o"

/* Max nr of args to the rpost filter */
#define RPOST_MAXARGS 128

/* extension added by rpost to the batch file when */
/* a message upload fails and we have to write out */
/* a failed file */
#define RPOST_FAIL_EXT ".fail"

/* RNEWS program called by lpost */
#define RNEWS "/usr/lib/news/rnews"

/* character used as a comment in sucknewsrc */
#define SUCKNEWSRC_COMMENT_CHAR '#'

/* character used as a separate in the PATH environment variable */
#define PATH_SEPARATOR ':'

/* maximum number of args to killprg */
#define KILLPRG_MAXARGS 128

/* length of string sent to killprg (min length must be 8) */
#define KILLPRG_LENGTHLEN 8 

/*  Internal buffer size shouldn't need to change */
#define MAXLINLEN 4096

/* Internal buffer size for print_phrases */
#define PHRASES_BLOCK_SIZE 1024

/* Character that is used to separate variables (front and back) in phrases */
#define PHRASES_SEPARATOR '%'
#define PHRASES_VAR_CHAR 'v'

/* the following should be handled by the auto-config stuff */
/* shouldn't need to touch below here */

#ifndef HAVE_MEMMOVE 	/* if you don't have memmove, try bcopy */
#define memmove(a,b,c) bcopy(b,a,c)
#endif

#ifndef PATH_MAX	/* in case you don't have it */
#define PATH_MAX _POSIX_PATH_MAX
#endif

#endif /* _SUCK_CONFIG_H */
