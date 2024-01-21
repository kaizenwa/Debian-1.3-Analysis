/* -*- c -*-
 *
 * Author:      James Brister <brister@vix.com> -- berkeley-unix --
 * Start Date:  Mon, 15 Jan 1996 17:31:58 +1100
 * Project:     INN -- innfeed
 * File:        main.c
 * RCSId:       $Id: main.c,v 1.19 1996/12/07 01:38:53 brister Exp $
 *
 * Copyright:   Copyright (c) 1996 by Internet Software Consortium
 *
 *              Permission to use, copy, modify, and distribute this
 *              software for any purpose with or without fee is hereby
 *              granted, provided that the above copyright notice and this
 *              permission notice appear in all copies.
 *
 *              THE SOFTWARE IS PROVIDED "AS IS" AND INTERNET SOFTWARE
 *              CONSORTIUM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 *              SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *              MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL INTERNET
 *              SOFTWARE CONSORTIUM BE LIABLE FOR ANY SPECIAL, DIRECT,
 *              INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *              WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *              WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 *              TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 *              USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Description: Main routines for the innfeed program.
 * 
 */

#if ! defined (lint)
static const char *rcsid = "$Id: main.c,v 1.19 1996/12/07 01:38:53 brister Exp $" ;
static void use_rcsid (const char *rid) {   /* Never called */
  use_rcsid (rcsid) ; use_rcsid (rid) ;
}
#endif


#include "config.h"             /* system specific configuration */

#include <stdlib.h>
#include <syslog.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <string.h>
#include <sys/socket.h>
#include <errno.h>
#include <fcntl.h>
#include <assert.h>
#include <math.h>

#if defined (DO_HAVE_UNISTD)
#include <unistd.h>
#endif

#include <sys/wait.h>
#include <signal.h>
#include <stdio.h>
#include <ctype.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "misc.h"
#include "tape.h"
#include "article.h"
#include "msgs.h"
#include "buffer.h"
#include "connection.h"


#if defined(DO_HAVE_UNIX_DOMAIN)
#include <sys/un.h>
#endif  /* defined(DO_HAVE_UNIX_DOMAIN) */


#include "endpoint.h"
#include "host.h"
#include "innlistener.h"


/* exports */
extern int debugWrites ;
int sigFlag ;
char *InputFile ;
const char *configFile = CONFIG_FILE ;
bool RollInputFile ;
char *pidFile = PID_FILE ;

/* imports */
extern char *versionInfo ;
#if defined (sun)
extern char *optarg ;           /* needed for Solaris */
extern int optind;
#endif

extern void openInputFile (void);

/* privates */
static bool talkToSelf ;
static char *logFile ;
static char *newsspool = NEWSSPOOL;

static void sigalrm (int sig) ;
static void sigchld (int sig) ;
static void sighup (int sig) ;
static void sigterm (int sig) ;
static void sigusr (int sig) ;
static void usage (int) ;
static void gprintinfo (void) ;
static void openLogFile (void) ;
static void writePidFile (void) ;

int main (int argc, char **argv)
{
  EndPoint ep ;
  InnListener listener ;
  int optVal ;
  const char *subProgram = NULL ;
  bool seenV = false ;
  bool dynamicPeers = false ;
  time_t now = theTime() ;
  char dateString [30] ;

  strcpy (dateString,ctime(&now)) ;
  dateString [24] = '\0' ;

  if ((program = strrchr (argv [0],'/')) == NULL)
    program = argv [0] ;
  else
    program++ ;

  gPrintInfo = gprintinfo ;


#define OPT_STRING "zymvhxb:l:c:d:s:o:p:a:"
  
  while ((optVal = getopt (argc,argv,OPT_STRING)) != EOF)
    {
      switch (optVal) 
        {
          case 'a':
            newsspool = optarg ;
            break ;
            
          case 'd':
            loggingLevel = atoi (optarg) ;
            break ;

          case 'z':
            hostLogConnectionStats (true) ;
            break ;
            
          case 'x':
            talkToSelf = true ;
            break ;
            
          case 's':
            subProgram = optarg ;
            break ;

          case 'b':
            setTapeDirectory (optarg) ;
            break ;

          case 'l':
            logFile = optarg ;
            break ;

          case 'm':
            artLogMissingArticles (true) ;
            break ;

          case 'o':
            artSetMaxBytesInUse (atoi (optarg)) ;
            break ;

          case 'p':
            pidFile = optarg ;
            break ;

          case 'c':
            configFile = optarg ;
            break ;

          case 'h':
            usage (0) ;

          case 'v':
            seenV = true ;
            break ;

          case 'y':
            dynamicPeers = true ;
            break ;
	
          default:
             	usage (1) ;	
        }
    }

  argc -= optind;
  argv += optind;

  if (argc > 1)
    usage (1) ;
  else if (argc == 1)
    InputFile = *argv;

  if (subProgram == NULL && talkToSelf == false)
    {
      struct stat buf ;

      if (fstat (0,&buf) < 0)
        {
          syslog (LOG_ERR,FSTAT_FAILURE,"stdin") ;
          exit (1) ;
        }
      else if (S_ISREG (buf.st_mode))
          InputFile = "";
    }
  
  if (subProgram != NULL && (talkToSelf == true || InputFile))
    {
      dprintf (0,"Cannot specify '-s' with '-x' or an input file\n") ;
      usage (1) ;
    }

  if (loggingLevel == 0 && fileExistsP (DEBUG_FILE))
    loggingLevel = 1 ;

    /* XXX this need to be an option */
  artBitFiddleContents (true) ; 
 
  if (logFile == NULL && ! isatty ( fileno (stderr) ) )
    logFile = LOG_FILE ;

  if (logFile)
    openLogFile () ;

  if (seenV)
    {
      warn ("%s version: %s\n",program, versionInfo) ;
      exit (0) ;
    }

  openlog (program,(int)(L_OPENLOG_FLAGS|LOG_PID),LOG_NEWS) ;
  syslog (LOG_NOTICE,STARTING_PROGRAM,versionInfo,dateString) ;

  openfds = 4 ;                 /* stdin, stdout, stderr and syslog */
  
  if ( !isDirectory (getTapeDirectory()) )
    {
      syslog (LOG_ERR,NOT_A_DIR,getTapeDirectory()) ;
      dprintf (1,"Not a directory : %s\n",getTapeDirectory()) ;
      exit (1) ;
    }
  
  /* XXX this pid file should be done better to allow for multiple
     XXX concurrent innfeed processes */
  writePidFile ();

  if (subProgram != NULL)
    {
      int fds [2] ;
      int pid ;

      if (pipe (fds) < 0)
        {
          syslog (LOG_CRIT,PIPE_FAILURE) ;
          exit (1) ;
        }

      if ((pid = fork ()) < 0)
        {
          syslog (LOG_CRIT,FORK_FAILURE) ;
          exit (1) ;
        }
      else if (pid == 0)
        {                       /* child */
          close (fds[0]) ;
          close (0) ;
          close (1) ;
          close (2) ;
          dup2 (fds[1],1) ;
          dup2 (fds[1],2) ;
          execlp ("sh","sh", "-c", subProgram, NULL) ;
          perror ("execlp") ;
          exit (1) ;
        }
      else
        {                       /* parent */
          close (0) ;
          dup2 (fds[0],0) ;
          close (fds[1]) ;
          signal(SIGCHLD,sigchld) ;
          openfds++ ;
        }
    }
  else  if (talkToSelf)
    {
        /* We're not really getting information from innd or a subprogram,
           but are just processing backlog files. We set up a pipe to ourself
           that we never write to, to simulate an idle innd. */
      int pipefds [2] ;

      if (pipe (pipefds) != 0)
        {
          syslog (LOG_ERR,PIPE_FAILURE) ;
          exit (1) ;
        }

      close (0) ;
      dup2 (pipefds [0], 0) ;

      openfds++ ;
      openfds++ ;
    }

  if (chdir (newsspool) != 0)
    {
      syslog (LOG_ERR,CD_FAILED,newsspool) ;
      exit (1) ;
    }


    /* hook up the endpoint to the source of new article information (usually
       innd). */
  ep = newEndPoint (0) ;        /* fd 0, i.e. stdin */

    /* now arrange for this endpoint to always be the first one checked for
       possible activity. */
  setMainEndPoint (ep) ;

  tapeSetCheckpointPeriod (TAPE_CHECKPOINT_PERIOD) ;

  listener = newListener (ep, talkToSelf,dynamicPeers) ;
  mainListener = listener ;
  
  configHosts (talkToSelf) ;

  if (InputFile && *InputFile) {
    openInputFile () ;
  }

  /* handle signal to shutndown */
  signal (SIGTERM,sigterm) ;

  /* handle signal to reload config */
  signal (SIGHUP,sighup) ;

  /* handle signal to roll log file */
  signal (SIGALRM,sigalrm) ;

  /* we can increment and decrement logging levels by sending SIGUSR{1,2} */
  signal (SIGUSR1,sigusr) ;
  signal (SIGUSR2,sigusr) ;

  Run () ;
  
  exit (0) ;
}


static void usage (int val)
{
  fprintf (stderr,"usage: %s [ options ]\n\n",
           program) ;
  fprintf (stderr,"Version: %s\n\n",versionInfo) ;
  fprintf (stderr,"Config file: %s\n",CONFIG_FILE) ;
  fprintf (stderr,"Backlog directory: %s\n",TAPE_DIRECTORY) ;
  fprintf (stderr,"\n\nLegal options are:\n") ;
  fprintf (stderr,"\t-d num      set the logging level to num (an integer).\n");
  fprintf (stderr,"\t            Larger value means more logging. 0 means no\n");
  fprintf (stderr,"\t            logging. The default is 0\n");
  fprintf (stderr,"\t-l file     redirect stderr and stdout to the given file.\n");
  fprintf (stderr,"\t            When run under INN they normally are redirected to\n");
  fprintf (stderr,"\t            /dev/null. This is needed if using '-d'.\n");
  fprintf (stderr,"\t-s command  run the given command in a subprocess and use\n");
  fprintf (stderr,"\t            its output as article information instead of\n");
  fprintf (stderr,"\t            running under innd\n");
  fprintf (stderr,"\t-x          Do not read any articles information off stdin,\n");
  fprintf (stderr,"\t            but simply process backlog files and then exit\n");
  fprintf (stderr,"\t            when done\n");
  fprintf (stderr,"\t-c file     Use the given file as the config file instead of the\n");
  fprintf (stderr,"\t            default of %s\n",CONFIG_FILE);
  fprintf (stderr,"\t-z          have each of the connections issue their own stats\n");
  fprintf (stderr,"\t            whenever they close, or whenever their controller\n");
  fprintf (stderr,"\t            issues its own stats\n");
  fprintf (stderr,"\t-y          Add peers dynamically. If an unrecognized peername\n");
  fprintf (stderr,"\t            is received from innd, then it is presumed to als\n");
  fprintf (stderr,"\t            be the ip name and a new peer binding is set up\n");
  fprintf (stderr,"\t-m          Log information on all missing articles\n");
  fprintf (stderr,"\t-v          print version information\n");
  fprintf (stderr,"\t-h          print this message\n");
  fprintf (stderr,"\t-b dir      Use the given directory as the the storage\n");
  fprintf (stderr,"\t            place for backlog (tape) files and lock files.\n");

  exit (val) ;
}

static void sigterm (int sig)
{
  syslog(LOG_NOTICE, SHUTDOWN_SIGNAL);
  shutDown (mainListener) ;

  signal (sig,sigterm) ;
}

static void sighup (int sig)
{
  /* XXX Just set variable and roll this into Run () ? */
  syslog(LOG_NOTICE, CONFIG_RELOAD, configFile);
  configHosts (talkToSelf) ;

  signal (sig,sighup) ;
}

static void sigalrm (int sig)
{
  RollInputFile = true;
  syslog(LOG_NOTICE, "ME preparing to roll %s", InputFile);

  signal (sig,sigalrm) ;
}

static void sigchld (int sig)
{

  (void) sig ;                  /* keep lint happy */
  
#if 0
  wait (&status) ;              /* we don't care */
#endif

  signal (sig,sigchld) ;
}


  /* SIGUSR1 increments logging level. SIGUSR2 decrements. */
static void sigusr (int sig)
{
  if (sig == SIGUSR1) {
    syslog(LOG_NOTICE, INCR_LOGLEVEL, loggingLevel);
    loggingLevel++ ;
  } else if (sig == SIGUSR2 && loggingLevel > 0) {
    syslog(LOG_NOTICE, DECR_LOGLEVEL, loggingLevel);
    loggingLevel-- ;
  }    

  signal (sig,sigusr) ;
}

static void openLogFile ()
{
  FILE *fpr ;

  fpr = freopen (logFile,"a",stdout) ;
  if (fpr != stdout)
    die ("freopen (%s, \"a\", stdout): %s", logFile, strerror (errno)) ;

  fpr = freopen (logFile,"a",stderr) ;
  if (fpr != stderr)
    die ("freopen (%s, \"a\", stderr): %s", logFile, strerror (errno)) ;

#if defined (DO_HAVE_SETBUFFER)
setbuffer (stdout, NULL, 0) ;
setbuffer (stderr, NULL, 0) ;
#else
setbuf (stdout, NULL) ;
setbuf (stderr, NULL) ;
#endif
}

static void writePidFile ()
{
  FILE *F;
  int pid;

  /* Record our PID. */
  pid = getpid();
  if ((F = fopen(pidFile, "w")) == NULL)
    {
      syslog(LOG_ERR, "ME cant fopen %s %m", pidFile);
    }
  else
    {
      if (fprintf(F, "%ld\n", (long)pid) == EOF || ferror(F))
	{
	  syslog(LOG_ERR, "ME cant fprintf %s %m", pidFile);
        }
      if (fclose(F) == EOF)
	{
	  syslog(LOG_ERR, "ME cant fclose %s %m", pidFile);
        }
      if (chmod(pidFile, 0664) < 0)
	{
	  syslog(LOG_ERR, "ME cant chmod %s %m", pidFile);
        }
    }
}

static void gprintinfo (void)
{
  FILE *fp = fopen (SNAPSHOT_FILE,"a") ;
  time_t now = theTime() ;

  if (fp == NULL)
    {
      syslog (LOG_ERR,NO_SNAPSHOT,SNAPSHOT_FILE) ;
      return ;
    }

#if defined (DO_HAVE_SETBUFFER)
  setbuffer (fp, NULL, 0) ;
#else
  setbuf (fp, NULL) ;
#endif


  fprintf (fp,"----------------------------System snaphot taken at: %s\n",
           ctime (&now)) ;
  gPrintListenerInfo (fp,0) ;
  fprintf (fp,"\n\n\n\n") ;
  gPrintHostInfo (fp,0) ;
  fprintf (fp,"\n\n\n\n") ;
  gPrintCxnInfo (fp,0) ;
  fprintf (fp,"\n\n\n\n") ;
  gPrintArticleInfo (fp,0) ;
  fprintf (fp,"\n\n\n\n") ;
  gPrintBufferInfo (fp,0) ;
  fprintf (fp,"\n\n\n\n") ;
  fclose (fp) ;
}

