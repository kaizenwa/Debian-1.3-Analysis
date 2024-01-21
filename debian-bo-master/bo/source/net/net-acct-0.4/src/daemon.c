/* 
 * Network accounting
 * daemon.c - Utilities for a daemon process.
 * (C) 1994 Ulrich Callmeier
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/param.h>
#include <errno.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include "netacct.h"

char *rcs_revision_daemon_c = "$Revision: 1.8 $";

int daemon_start(void)
{
    int i;
    pid_t pid;
    
    if( (pid = fork()) < 0)
        return(-1);
    else if (pid!=0)
        exit(0);

    closelog();

    for(i=0; i<FD_SETSIZE; i++)
        close(i);

    setsid();

    return 0;
}

void daemon_stop(int sig)
{
  DEBUG(DBG_STATE, sprintf(dbg, "entering daemon_stop\n"));

  write_log(1);

  DEBUG(DBG_STATE, sprintf(dbg, "wrote final log\n"));
    
  unlink(PID_FILE);

  DEBUG(DBG_STATE, sprintf(dbg, "unlinked PID_FILE\n"));

  syslog(LOG_INFO, "net accounting daemon terminating (%d)\n",sig);

  DEBUG(DBG_STATE, sprintf(dbg, "did syslog message\n"));
    
  exit_capture();

  DEBUG(DBG_STATE, sprintf(dbg, "cleaned up capture\n"));

  closelog();

  DEBUG(DBG_STATE, sprintf(dbg, "closed syslog\n"));

  fclose(dbg_file);
  
  exit(1);
}







