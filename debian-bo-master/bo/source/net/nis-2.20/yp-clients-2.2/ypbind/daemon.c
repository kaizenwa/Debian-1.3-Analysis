/* 
 * $Log: daemon.c,v $
 * Revision 1.1.1.1  1997/01/23 20:48:30  hjl
 * Import ypbind 3.0.
 *
 * Revision 1.2  1996/10/08 09:36:13  swen
 * Reordered include files.
 *
 * Revision 1.1  1995/07/25  14:27:34  swen
 * ypbind version 2.0.
 *
 * Revision 2.3  1995/01/24  12:24:20  swen
 * Added RCS keywords.
 *
 */

static char rcsid[] = "$Id: daemon.c,v 1.1.1.1 1997/01/23 20:48:30 hjl Exp $" ;

/*
 * Initialize a daemon process.
 */

#include	<sys/param.h>
#include	<sys/file.h>
#include	<sys/ioctl.h>
#include	<sys/types.h>
#include    <sys/stat.h>
#include	<stdio.h>
#include	<signal.h>
#include    <unistd.h>
#include    <fcntl.h>
#include	<errno.h>
#include	"ourhdr.h"

extern int	errno;

/*
 * Detach a daemon process from login session context.
 */

void
daemon_start(void)
   	          	/* nonzero -> handle SIGCLDs so zombies don't clog */
{
  register int	childpid, fd;
  
      /*
       * If we were started by init (process 1) from the /etc/inittab file
       * there's no need to detach.
       * This test is unreliable due to an unavoidable ambiguity
       * if the process is started by some other process and orphaned
       * (i.e., if the parent process terminates before we are started).
       */
  
  if (getppid() == 1)
    goto out;
  
      /*
       * Ignore the terminal stop signals (BSD).
       */
  
#ifdef SIGTTOU
  signal(SIGTTOU, SIG_IGN);
#endif
#ifdef SIGTTIN
  signal(SIGTTIN, SIG_IGN);
#endif
#ifdef SIGTSTP
  signal(SIGTSTP, SIG_IGN);
#endif
  
      /*
       * If we were not started in the background, fork and
       * let the parent exit.  This also guarantees the first child
       * is not a process group leader.
       */
  
  if ( (childpid = fork()) < 0)
    log_sys("can't fork first child");
  else if (childpid > 0)
    exit(0);	/* parent */
  
      /*
       * First child process.
       *
       * Disassociate from controlling terminal and process group.
       * Ensure the process can't reacquire a new controlling terminal.
       */
  
  if (setsid() == -1)
    log_sys("setsid");
  
  if ( (childpid = fork()) < 0)
    log_sys("can't fork second child");
  else if (childpid > 0)
    exit(0);	/* first child */
  
      /* second child */
  
  out:
      /*
       * Close any open files descriptors.
       */
  
  for (fd = open_max()-1; fd >= 0; fd--)
    close(fd);
  
  errno = 0;		/* probably got set to EBADF from a close */
  
      /*
       * Move the current directory to root, to make sure we
       * aren't on a mounted filesystem.
       */
  
  chdir("/");
  
      /*
       * Clear any inherited file mode creation mask.
       */
  
  umask(0);

      /*
       * stdin, stdout and stderr -> /dev/null
       */
  
  fd = open("/dev/null", O_RDWR);
  dup(fd);
  dup(fd);
  
}
