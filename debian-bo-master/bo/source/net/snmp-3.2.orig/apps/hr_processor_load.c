/*
  Linux specific code to maintain and compute the hrProcessorLoad
  MIB value.

  Copyright (C) Patrick Weemeeuw, 1996.
  
  This code is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published
  by the Free Software Foundation; either version 2 of the License, or (at
  your option) any later version.
     
  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.
     
  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
   

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "hr_processor_load.h"


/*
 * well, at least with 12 samples per minute, the hr_processor_load
 * drops to 0 with my 90 mhz box at home. so i would say, don't care
 * about using samples > 2 -- (schoenfr)
 */

#define NROFSAMPLES 4		/* number of samples per minute;
				   minimum 2 */

static int interval = 60/NROFSAMPLES;
static long int idleJiffies[NROFSAMPLES+1];
				/* cyclic array with idle jiffies sampled */
static time_t sampleTimes[NROFSAMPLES+1]; /* time of each sample */
static int index;		/* index of the most recent sample */

static void update_HrProcessorLoad();

static long int get_IdleJiffies()
{
  FILE *in;
  static long int idle;

  if((in = fopen("/proc/stat", "r")))
    {
      (void) fscanf (in, "cpu %*d %*d %*d %ld", &idle);
      fclose (in);
    }
  else
    {
      fprintf (stderr, "snmpd: cannot open /proc/stat - please make sure /proc is mounted.\n");
    }
  return idle;
}

void init_HrProcessorLoad()
{
  int i;
  time_t t = time(0);
  long int ij = get_IdleJiffies();
  struct sigaction action;
    
  for(i=0;i<=NROFSAMPLES;i++)
    {
      idleJiffies[i] = ij;
      sampleTimes[i] = t;
    }
  index = 0;

  action.sa_handler = update_HrProcessorLoad;
  sigemptyset(&action.sa_mask);
  action.sa_flags = 0;
  if(sigaction(SIGALRM, &action, NULL) != 0) {
    perror("snmpd: sigaction failed");
    /** exit(1); **/
  }
  (void) alarm(interval);	/* schedule first timer interrupt */
}

static void update_HrProcessorLoad()
{
  index = (index+1)%(NROFSAMPLES+1);
  sampleTimes[index] = time((time_t *) 0);
  idleJiffies[index] = get_IdleJiffies();

  /* reschedule for next interrupt */
  (void) alarm(interval);
}

int get_HrProcessorLoad()
{
  long int elapsedJiffies, nonIdleJiffies;
  sigset_t currentBlocked, previousBlocked;
  int oldestIndex = (index+1)%(NROFSAMPLES+1);

  /* prevent signal handler from clobbering the data structures */
  (void) sigemptyset(&currentBlocked);
  (void) sigaddset(&currentBlocked, SIGALRM);
  sigprocmask(SIG_BLOCK, &currentBlocked, &previousBlocked);

  /* actual computation */
  elapsedJiffies = (sampleTimes[index] - sampleTimes[oldestIndex])*100;
  nonIdleJiffies =
    elapsedJiffies - (idleJiffies[index] - idleJiffies[oldestIndex]);

  /* restore signal mask */
  sigprocmask(SIG_SETMASK, &previousBlocked, NULL);
  
  /*
    printf("non idle: %d elapsed: %d\n", nonIdleJiffies, elapsedJiffies);
   */

  if (elapsedJiffies > 0)
    return (int) ((100 * nonIdleJiffies) / elapsedJiffies + 0.5);
  else
    return 0;
}
