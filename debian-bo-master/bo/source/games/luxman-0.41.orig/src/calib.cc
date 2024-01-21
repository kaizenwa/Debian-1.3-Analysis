/*
   calib.cc

   This file is part of LuxMan.
   
   Copyright (C) 1994,1995 Frank McIngvale (frankm@nuance.com)
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdio.h>
#include "globals.h"
#include "vts.h"

/* Try to determine amount of overhead in usleep() call */

#include <signal.h>
static int got;

static void handle( int s )
{
  got = 1;
}

void calibrate_usleep( int nsecs )
{
  int sleep_val;
  int r;
  struct timeval t1, t2;
  int reps;
  long usec;

  /* This probably doesn't really help */
  while( vt_switched_out );
  
restart:
  sleep_val = 1000000 / gb_frames_per_second;
  r = sleep_val % 10000;	/* Assume best resolution of 10ms */
  sleep_val = sleep_val / 10000;
  sleep_val = sleep_val * 10000;
  if ( r >= 5000 )
	sleep_val += 10000;

  gettimeofday( &t1, NULL );

  reps = 0;
  got = 0;
  
  signal( SIGALRM, handle );
  alarm( nsecs );
  
  while( !got )
	{
	  usleep( sleep_val );
	  ++reps;

	  if ( vt_switched_out )
		{
		  while( vt_switched_out );
		  
		  printf("\nGot VT switch -- Restarting calibration.\n");
		  printf("Calibrating delay...");
		  fflush( stdout );
		  goto restart;
		}
	}

  gettimeofday( &t2, NULL );

  if ( t2.tv_usec < t1.tv_usec )
	{
	  --t2.tv_sec;
	  t2.tv_usec += 1000000;
	}

  /* How long did that take? */
  usec = (t2.tv_sec - t1.tv_sec) * 1000000 - (t2.tv_usec - t1.tv_usec );

  /* Should have taken (reps*sleep_val) usecs; calc diff */
  usec = usec - (reps*sleep_val);
  
  /* Overhead per rep therefore... */
  usec = usec / reps;

  gb_usleep_time = (int)usec;
  
  printf("%d\n", (int)usec );
}

