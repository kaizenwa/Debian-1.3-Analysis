/* apm.c -- Simple APM interface
 * Created: Mon Jan  8 10:28:16 1996 by r.faith@ieee.org
 * Revised: Sun Apr 21 16:24:28 1996 by r.faith@ieee.org
 * Copyright 1996 Rickard E. Faith (r.faith@ieee.org)
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id: apm.c,v 1.8 1996/04/21 20:49:40 faith Exp $
 * 
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <getopt.h>
#include "apm.h"

static int verbose = 0;

void change_state( int suspend )
{
   int    fd;
   time_t then, now;
   int    error;

   if ((fd = apm_open()) < 0) {
      fprintf( stderr, "Cannot open APM device: %s\n", strerror( errno ) );
      exit( 1 );
   }
   time( &then );
   if (suspend) error = apm_suspend( fd );
   else         error = apm_standby( fd );
   if (error != 0)
      perror( "apm" );
   else {
      time( &now );

#if 0
/*
 * This currently doesn't work as we should wait for a resume before
 * trying to time this. It is possible for the above calls to return
 * before the machine enters the desired state if some other apm-aware
 * application (e.g., apmd) is running. -- sfr
 */
      if (verbose) {
         printf( "%s for %s\n",
	         suspend ? "Suspended" : "On standby",
	         apm_delta_time( then, now ) );
      }
#endif
   }
   apm_close( fd );
}

void usage( void )
{
   fprintf( stderr,
	    "usage: apm [-vmsS] [--verbose] [--minutes] [--suspend] [--standby]\n" );
   exit( 1 );
}

int main( int argc, char **argv )
{
   apm_info i;
   int      suspend        = 0;
   int      standby        = 0;
   int      debug          = 0;
   int      displayMinutes = 0;
   int      c;
   struct option longopts[] = {
      { "version", 0, 0, 'V' },
      { "verbose", 0, 0, 'v' },
      { "suspend", 0, 0, 's' },
      { "standby", 0, 0, 'S' },
      { "debug",   0, 0, 'd' },
      { "minutes", 0, 0, 'm' },
   };

   while ((c = getopt_long( argc, argv, "VvsSdm", longopts, NULL )) != -1)
      switch (c) {
      case 'V':
	 fprintf( stderr, "apm version %s\n", VERSION );
	 exit( 0 );
	 break;
      case 'v': ++verbose;          break;
      case 's': ++suspend;          break;
      case 'S': ++standby;          break;
      case 'd': ++debug; ++verbose; break;
      case 'm': ++displayMinutes;   break;
      default:  usage();            break;
      }

   switch (apm_exists()) {
   case 1: fprintf( stderr, "No APM support in kernel\n" );  exit( 1 );
   case 2: fprintf( stderr, "Old APM support in kernel\n" ); exit( 2 );
   }

   if (suspend && standby) usage();
   if (suspend || standby) {
      change_state( suspend );
      exit( 0 );
   }

   if (apm_read( &i )) {
      fprintf( stderr, "Cannot read APM information\n" );
      exit( 1 );
   }

   if (verbose)
      printf( "APM BIOS %d.%d (kernel driver %s)\n",
	      i.apm_version_major, i.apm_version_minor, i.driver_version );
   
   if (!(i.apm_flags & 0x02)) {
      fprintf( stderr, "32-bit APM interface not supported\n" );
      exit( 1 );
   }

   if (verbose && (i.apm_flags & 0x10))
      printf( "APM BIOS Power Management is currently disabled\n" );
   if (verbose && (i.apm_flags & 0x20))
      printf( "APM BIOS Power Management is currently disengaged\n" );
   
   switch (i.ac_line_status) {
   case 0: printf( "AC off-line" );     break;
   case 1: printf( "AC on-line" );      break;
   case 2: printf( "On backup power" ); break;
   }
   if (i.battery_flags != 0xff) { /* Have a 1.1 BIOS and a system battery. */
      if (i.battery_flags & 0x80) {
	 printf( ", no system battery" );
      } else {
	 switch (i.battery_status) {
	 case 0: printf( ", battery status high" ); break;
	 case 1: printf( ", battery status low" ); break;
	 case 2: printf( ", battery status critical" ); break;
	 case 3: printf( ", battery charging" ); break;
	 }
	 if (i.battery_percentage >= 0)
	    printf( ": %d%%", i.battery_percentage );
	 if (i.battery_time >= 0) {
	    if (displayMinutes) {
	       printf( " (%d %s)",
		       i.battery_time, i.using_minutes ? "min" : "sec" );
	    } else {
	       int seconds = i.using_minutes
			     ? i.battery_time * 60 : i.battery_time;
	       
	       printf( " (%s)", apm_time_nosec( seconds ) );
	    }
	 }
      }
   }
   printf( "\n" );

   if (debug) {
      printf( "Using device 0x%04x\n", apm_dev() );
      printf( "APM Flags =      0x%02x; AC Line Status = 0x%02x\n",
	      i.apm_flags, i.ac_line_status );
      printf( "Battery Status = 0x%02x; Battery Flags =  0x%02x\n",
	      i.battery_status, i.battery_flags );
   }

   return 0;
}
