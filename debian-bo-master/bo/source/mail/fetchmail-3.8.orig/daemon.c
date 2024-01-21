/*
 * daemon.c -- turn a process into a daemon under POSIX, SYSV, BSD.
 *
 * For license terms, see the file COPYING in this directory.
 */

#include <config.h>

#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/fcntl.h>	/* Solaris 2.5 requires this */

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if defined(STDC_HEADERS)
#include <stdlib.h>
#endif

#if defined(QNX)
#include <unix.h>
#endif

#if defined(HAVE_SGTTY_H)
#  include <sgtty.h>		/* for TIOCNOTTY under NEXTSTEP */
#endif

/* BSD portability hack */
#if !defined(SIGCHLD) && defined(SIGCLD)
#define SIGCHLD	SIGCLD
#endif

#include "fetchmail.h"
#include "tunable.h"

RETSIGTYPE
sigchld_handler (int sig)
/* process SIGCHLD to obtain the exit code of the terminating process */
{
  pid_t pid;

#if defined(HAVE_UNION_WAIT)
  union wait status;
#else
  int status;
#endif

#if 	defined(HAVE_WAIT3)
  while ((pid = wait3(&status, WNOHANG, (struct rusage *) 0)) > 0)
    ; /* swallow 'em up. */
#elif 	defined(HAVE_WAITPID)
  while ((pid = waitpid(-1, &status, WNOHANG)) > 0)
    ; /* swallow 'em up. */
#else	/* Zooks! Nothing to do but wait(), and hope we don't block... */
  wait(&status);
#endif

}

int
daemonize (const char *logfile, void (*termhook)(int))
/* detach from control TTY, become process group leader, catch SIGCHLD */
{
  int fd;
  pid_t childpid;
  RETSIGTYPE sigchld_handler();

  /* if we are started by init (process 1) via /etc/inittab we needn't 
     bother to detach from our process group context */

  if (getppid() == 1) 
    goto nottyDetach;

  /* Ignore BSD terminal stop signals */
#ifdef 	SIGTTOU
  signal(SIGTTOU, SIG_IGN);
#endif
#ifdef	SIGTTIN
  signal(SIGTTIN, SIG_IGN);
#endif
#ifdef	SIGTSTP
  signal(SIGTSTP, SIG_IGN);
#endif

  /* In case we were not started in the background, fork and let
     the parent exit.  Guarantees that the child is not a process
     group leader */

  if ((childpid = fork()) < 0) {
    error(0, errno, "fork");
    return(PS_IOERR);
  }
  else if (childpid > 0) 
    exit(0);  /* parent */

  
  /* Make ourselves the leader of a new process group with no
     controlling terminal */

#if	defined(HAVE_SETSID)		/* POSIX */
  /* POSIX makes this soooo easy to do */
  if (setsid() < 0) {
    error(0, errno, "setsid");
    return(PS_IOERR);
  }
#elif	defined(SIGTSTP)		/* BSD */
  /* change process group */
  setpgrp(0, getpid());

  /* lose controlling tty */
  if ((fd = open("/dev/tty", O_RDWR)) >= 0) {
    ioctl(fd, TIOCNOTTY, (char *) 0);
    close(fd);
  }
#else					/* SVR3 and older */
  /* change process group */
  setpgrp();
  
  /* lose controlling tty */
  signal(SIGHUP, SIG_IGN);
  if ((childpid = fork()) < 0) {
    error(0, errno, "fork");
    return(PS_IOERR);
  }
  else if (childpid > 0) {
    exit(0); 	/* parent */
  }
#endif

nottyDetach:

  /* Close any/all open file descriptors */
#if 	defined(HAVE_GETDTABLESIZE)
  for (fd = getdtablesize()-1;  fd >= 0;  fd--)
#elif	defined(NOFILE)
  for (fd = NOFILE-1;  fd >= 0;  fd--)
#else		/* make an educated guess */
  for (fd = 19;  fd >= 0;  fd--)
#endif
  {
    close(fd);
  }

  /* Reopen stdin descriptor on /dev/null */
  if ((fd = open("/dev/null", O_RDWR)) < 0) {   /* stdin */
    error(0, errno, "open: /dev/null");
    return(PS_IOERR);
  }

  if (logfile)
    fd = open(logfile, O_CREAT|O_WRONLY|O_APPEND, 0666);	/* stdout */
  else
    if (dup(fd) < 0) {				/* stdout */
      error(0, errno, "dup");
      return(PS_IOERR);
    }
  if (dup(fd) < 0) {				/* stderr */
    error(0, errno, "dup");
    return(PS_IOERR);
  }

  /* move to root directory, so we don't prevent filesystem unmounts */
  chdir("/");

  /* set our umask to something reasonable (we hope) */
#if defined(DEF_UMASK)
  umask(DEF_UMASK);
#else
  umask(022);
#endif

  /* set up to catch child process termination signals */ 
  signal(SIGCHLD, sigchld_handler); 
#if defined(SIGPWR)
  signal(SIGPWR, sigchld_handler); 
#endif

  return(0);
}

/* daemon.c ends here */
