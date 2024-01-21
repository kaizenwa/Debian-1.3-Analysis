/*
 * gather.c		- gather and report process info.
 *
 * Michael Hamilton (michael@actrix.gen.nz).
 * Copyright (c) 1995
 *
 * Snarfed and HEAVILY modified from top in process ps
 * by Branko Lankester and Roger Binns.
 *
 */


#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <sys/ioctl.h>
#include <pwd.h>
#include <linux/sched.h>
#include <linux/tty.h>
#include <termcap.h>
#include <termios.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <ctype.h>
#include <setjmp.h>

#include "sysinfo.h"
#include "readproc.h"
#include "whattime.h"
#include "signals.h"

#define MAX_HEIGHT 10.0

static unsigned **memory_info = NULL;

void show_process(proc_t *info, double up_seconds, time_t now)
{
  int pid;
  double cpu_utilisation;	/* cpu_time / total_time */
  double memory_utilisation;
  double resident;
  double total_time;		/* elapsed_time / system_uptime */
  double cpu_ticks;
  double elapsed_ticks;

  if (!info) {
    return ;
  }

  cpu_ticks = (info->utime + info->stime) ;

  /* Start_time is in herz from boot time - dispite what any other comment says. */

				/* Percent of uptime of a processes existance. */
  elapsed_ticks = up_seconds * HZ - info->start_time;

  total_time = MAX_HEIGHT * (elapsed_ticks / (double) HZ) / up_seconds; 

  cpu_utilisation = MAX_HEIGHT * cpu_ticks / elapsed_ticks ;

  memory_utilisation = 
    MAX_HEIGHT * (double) info->size * 4096 / memory_info[meminfo_main][meminfo_used];

  resident = 
    MAX_HEIGHT * (double) info->resident * 4096 / memory_info[meminfo_main][meminfo_used] ;

  printf("% 5d\n%s\n %f %f %f %f\n",
	 info->pid,
	 strcmp(info->user, "root") == 0 ? "{ root }" : info->user,
	 cpu_utilisation,
	 resident,
	 memory_utilisation,
	 total_time
	 ) ;
  
}


void show_all_processes ()
{
  double up ;
  double idle ;

  double location ;
  time_t now;

  char host[80] ;

  int count, i;

  proc_t **p_table = NULL;
  const int proc_flags=PROC_FILLMEM|PROC_FILLCMD|PROC_FILLTTY|PROC_FILLUSR;

  uptime(&up, &idle) ;

  p_table=readproctab(proc_flags, NULL);

  for (count=0; p_table[count]; count++) {} ;

  if (count == 0) {
    fprintf(stderr, "No processes available\n");
    sleep(1);
  }
  else {
    gethostname(host, 79);
    now = time(NULL);
    printf("Process View: %s %s", host, ctime(&now));
    printf("%d %d CPU RSS Mem Time\n", count, 4) ;

    for (i = 0, location = -(count / 2) ;  i < count ; ++i, ++location) {
      show_process(p_table[i], up, now) ;
    }
  }
  freeproctab(p_table);
}

void usage(void) 
{
  printf("Usage: gather [-sleep n]\n");
}

int
main(int argc, char **argv)
{
  /* loop, collecting process info and sleeping */
  
  int i, seconds = 2;    

  for (i = 1; i < argc; ++i) {

    if (!strcmp("-sleep", argv[i])) {
      seconds = strtol(argv[++i],NULL,0);

    } else {
      usage();
    }
  }

  setvbuf(stdout,NULL,_IONBF,0);

  memory_info = meminfo();

  for (;;) {
    show_all_processes() ;
    sleep(seconds) ;
  }
}



