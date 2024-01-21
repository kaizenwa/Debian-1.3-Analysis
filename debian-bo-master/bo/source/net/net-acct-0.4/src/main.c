/* 
 * Network accounting
 * main.c - main module
 * (C) 1994 Ulrich Callmeier
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <strings.h>
#include <signal.h>
#include "netacct.h"

char *rcs_revision_main_c = "$Revision: 1.13be $";

/* globals */
char *progname;
struct config *cfg;
volatile int debug_level;
struct dev2line *dev2line;
FILE *dbg_file;

static char *fname = NULL;
static int debug = 0;
static int daem = 1;

void usage(void)
{
   fprintf(stderr, "Usage: %s [-dD] [-c filename]\n\n\t-d\tSwitch on debugging\n", progname);
   fprintf(stderr, "\t-c\tSpecify alternative configuration file\n");
   fprintf(stderr, "\t-D\tDon't detach (for inittab)\n\n");
}

void process_options(int argc, char *argv[])
{
   int c;

   fname = strdup(DEF_ACCTTAB);
   
   while ((c = getopt( argc, argv, "c:dD" )) != EOF)
       {
	   switch (c)
	       {
	       case 'c':
		   free(fname);
		   fname = strdup(optarg);
		   break;
	       case 'd':
		   debug = 1;
		   break;
	       case 'D':
	           daem = 0;
	           break;
	       case '?':
	       default:
		   usage();
		   exit(1);
	       }
       }
   
    argc -= optind;
    argv += optind;

    if (argc > 1)
	{
	    usage();
	    exit(1);
	}
}

int do_pid_file(void)
/* return 1 if file could be created */
/* return 0 if daemon already running */
/* this is by no means clean of races, if we take it serious we should do it with
   some well thought out atomic operations */
{
    FILE *f;

    if(access(PID_FILE,F_OK)==0)
	{
	    char buff[80];
	    int pid;
	    /* file exists */

	    f = fopen(PID_FILE, "r");
	    fgets(buff, sizeof(buff), f);
	    fclose(f);

	    pid = atoi(buff);

	    syslog(LOG_INFO, "found pid-file with pid %d\n", pid);

	    if(kill(pid, 0) == -1)
	    {
		syslog(LOG_INFO, "process %d doesn't exist anymore\n", pid);
	    }
	    else
	    {
		syslog(LOG_INFO, "process %d is still running.\n", pid);
		return 0;
	    }

	}

    f = fopen(PID_FILE, "w");
    fprintf(f, "%d\n", (int) getpid());
    fclose(f);
    
    return 1;
}


/* Set a signal handler. */
#define SETSIG(sig, fun, fla)   sa.sa_handler = fun; \
                                sa.sa_flags = fla; \
                                sigaction(sig, &sa, NULL);

void signal_setup(void)
{
    int i;
    struct sigaction sa;

    for (i= 1; i < NSIG; ++i)
	signal(i, signal_ignore);

    /* these stop the program */
    SETSIG(SIGINT, daemon_stop, 0);
    SETSIG(SIGKILL, daemon_stop, 0);
    SETSIG(SIGTERM, daemon_stop, 0);
    
    /* this one does the scheduling of write processes and handles the internal clock */
    SETSIG(SIGALRM, alarm_handler, 0);
    
    /* handles notification about child exits */
    SETSIG(SIGCHLD, child_finished, 0);

    /* manipulating the level of debug output */
    SETSIG(SIGUSR1, signal_debug, 0); /* increase debugging level */
    SETSIG(SIGUSR2, signal_debug, 0); /* turn off debugging */

    /* the following signals are used in a nonstandard sense */

    /* to see what version is running */
    SETSIG(SIGWINCH, signal_debug, 0); /* print version number */

    /* in case the program stops receiving packets (due to a kernel bug) */
    SETSIG(SIGIOT, signal_debug, 0); /* reopen socket */

    /* to cleanly move logfiles */
    SETSIG(SIGTSTP, signal_debug, 0); /* stop writing to file */
    SETSIG(SIGCONT, signal_debug, 0); /* continue writing to file */

    /* ignore, but notify */
    SETSIG(SIGHUP, signal_ignore, 0);

}

void save_dumpfile(char *fname)
{
    char *s;
    
    if(access(fname,F_OK)==0)
	{
	    syslog(LOG_DEBUG,"found old dumpfile (%s)\n",fname);
	    s = malloc(strlen(fname)+3);
	    strcpy(s, fname);
	    strcat(s, ".o");
	    save_dumpfile(s);
	    rename(fname, s);
	    free(s);
	}
    else
	{
	    if(errno != ENOENT)
		{
		    syslog(LOG_ERR, "error accessing dumpfile: %m\n");
		}
	    else
		{
		    syslog(LOG_DEBUG,"no old dumpfile (%s) exists\n",fname);
		}
	}
}

int main(int argc, char *argv[])
{
    progname = argv[0];

    if(geteuid() != 0)
	{
	    syslog(LOG_ERR, "must be superuser to run nacctd\n");
	    exit(1);
	}

    /* process user options */
    process_options(argc, argv);

    openlog("nacctd", 0, LOG_DAEMON);
    syslog(LOG_INFO, "net accounting daemon started");

    /* read config file */
    cfg = read_config(fname);
    if(cfg == NULL)
	{
	    syslog(LOG_ERR, "error reading config file\n");
	    syslog(LOG_INFO, "net accounting daemon aborting\n");
	    exit(1);
	}

    save_dumpfile(cfg->dumpname);

    if(!debug && daem)
	{
	    /* start daemon */
	    if(daemon_start()!=-1)
		{
		    openlog("nacctd", 0, LOG_DAEMON);
		    syslog(LOG_INFO, "net accounting daemon forked\n");
		}
	    else
		{
		    syslog(LOG_ERR, "couldn't fork: %m\n");
		    syslog(LOG_INFO, "net accounting daemon aborting\n");
		    exit(1);
		}
	}

    dbg_file = fopen(cfg->debugname, "a");
    if(dbg_file==NULL)
      {
	syslog(LOG_ERR, "error opening debug file: %m\n");
	syslog(LOG_INFO, "net accounting daemon aborting\n");
	exit(1);
      }
    
    setvbuf(dbg_file, NULL, _IONBF, BUFSIZ);

    /* check and create /var/run/nacctd.pid */
    if(!do_pid_file())
	{
	    syslog(LOG_ERR, "daemon already running or stale pid-file\n");
	    exit(1);
	}

    /* signal setup */
    signal_setup();

    /* init capturing */
    init_capture();
    
    /* start being useful */
    do_acct();

    fclose(dbg_file);
    return 0;
}

