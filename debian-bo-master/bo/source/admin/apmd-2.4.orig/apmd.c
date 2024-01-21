/* apmd.c -- APM event-monitoring daemon
 * Created: Mon Jan  8 14:29:18 1996 by r.faith@ieee.org
 * Revised: Sun Apr 21 16:37:44 1996 by r.faith@ieee.org
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
 * $Id: apmd.c,v 1.6 1996/04/21 20:49:56 faith Exp $
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>
#include <syslog.h>
#include <signal.h>
#include <paths.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include "apm.h"


#define PID_FILE _PATH_VARRUN "apmd.pid"

#define DEBUG        0		/* Special debugging: 0 = off; 1 = on */
#define MAX_EVENTS   8		/* Maximum events from APM BIOS */
#define RESUME_HOURS 6		/* How many hours before resume loss calc. */

static int    verbose = 0;
static int    wall    = 0;
static time_t then;
static time_t now;
static int    percent_change =  5; /* log every 5 percent change */
static int    warn_level     = 10; /* start warning at 10% remaining */

#ifndef abs
#define abs(a) ((a)<0?(-(a)):(a))
#endif

void usage( void )
{
   fprintf( stderr,
	    "usage: apmd [-VvW] [-p percent] [-w percent]\n" );
   exit( 1 );
}

void warn( const char *message )
{
   FILE *str;

   syslog( LOG_ALERT, "%s", message );
   if (wall) {
      str = popen( "wall", "w" );
      fprintf( str, "%s\n", message );
      pclose( str );
   }
}

void check_power( int resume_flag, int low_battery )
{
   apm_info      i;
   time_t        tmp;
   time_t        delta_time;
   int           delta_percentage;
   int           seconds;
   double        rate;
   int           force           = 0;
   char          buf[512];
   int           len;
   static time_t t;
   static int    percentage;
   static int    charging;
   static int    snap_percentage = 0;
   static int    initialized     = 0;
   static int    last_percentage = 0;
   static int    done_full       = 0;
   static int    done_empty      = 0;

   if (apm_read( &i )) return;

   if (i.using_minutes) seconds = i.battery_time * 60;
   else                 seconds = i.battery_time;

   if (!initialized)
      syslog( LOG_INFO, "Version %s (APM BIOS %d.%d, Linux driver %s)",
	      VERSION,
	      i.apm_version_major, i.apm_version_minor,
	      i.driver_version );
   
   if (!initialized
       || (!charging && (i.battery_status == 3))
       || (charging && (i.battery_status != 3))) {
      time( &t );
      last_percentage = percentage = i.battery_percentage;
      charging = (i.battery_status == 3);
      syslog( LOG_INFO, "%s: * * * (%d%% %s)",
	      charging ? "Charge" : "Battery",
	      i.battery_percentage, apm_time_nosec( seconds ) );
   }
   initialized = 1;

   if (resume_flag) {
      int percent_change = i.battery_percentage - snap_percentage;
      int seconds_change = now - then;
      
      time( &t );
      last_percentage = percentage = i.battery_percentage;
      charging = (i.battery_status == 3);
      if (then)
	 len = sprintf( buf, "Resume after %s", apm_time( seconds_change ) );
      else
	 len = sprintf( buf, "Resume" );
      if (seconds_change > 60 * 60 * RESUME_HOURS
	  && snap_percentage > 0
	  && percent_change < 0
	  && !charging) {
	 len += sprintf( buf + len, ", %.2f%%/day",
			 ((double)percent_change / (double)seconds_change)
			 * 60.0 * 60.0 * 24.0 );
      }
      sprintf( buf + len, " (%d%%%% %s)",
	      i.battery_percentage, apm_time_nosec( seconds ) );
      syslog( LOG_INFO, buf );
      
   }

   if (low_battery) {
      sprintf( buf, "Battery Low Notification from APM BIOS (%d%% %s)",
	       i.battery_percentage, apm_time_nosec( seconds ) );
      warn( buf );
   }

   snap_percentage = i.battery_percentage;
   if (i.battery_percentage < 0 || seconds < 0) return;
   
   time( &tmp );
   delta_time       = tmp - t;
   delta_percentage = percentage - i.battery_percentage;

#if DEBUG
   printf( "%d %d %d %d %d\n",
	   delta_time, delta_percentage, i.battery_percentage,
	   done_full, force );
#endif
   
   if (!charging && i.battery_percentage <= warn_level
       && (abs( last_percentage - i.battery_percentage ) > 0
	   || !i.battery_percentage)) {
      sprintf( buf, "Battery warning (%d%% %s)",
	       i.battery_percentage, apm_time_nosec( seconds ) );
      warn( buf );
   }
      
   if (!delta_time || !delta_percentage) return;
   if (i.battery_percentage == 100) {
      if (!done_full) ++force;
      done_full = 1;
   } else
      done_full = 0;
   
   if (!i.battery_percentage) {
      if (!done_empty) ++force;
      done_empty = 1;
   } else
      done_empty = 0;

#if DEBUG
   printf( "%d %d %d %d %d\n",
	   delta_time, delta_percentage, i.battery_percentage,
	   done_full, force );
#endif

   if (!force && abs(last_percentage - i.battery_percentage)
       < (percent_change > 1 ? percent_change : 1))
      return;

   printf( "%d\n", force );
   last_percentage = i.battery_percentage;
   
   rate = (double)delta_percentage / (double)delta_time;

   len = sprintf( buf, "%s: %f",
		  charging ? "Charge" : "Battery",
		  rate * 60.0 );
   
   if (!charging && percentage == 100)
      len += sprintf( buf + len, " %s", apm_time_nosec( delta_time ) );
   else if (charging && percentage == 0)
      len += sprintf( buf + len, " %s", apm_time_nosec( delta_time ) );
   else
      len += sprintf( buf + len, " (%s)", apm_time_nosec( delta_time ) );
   
   if (charging) {
      len += sprintf( buf + len, " %s",
		     apm_time_nosec( (int)((100.0 - i.battery_percentage)
				     / -rate ) ) );
   } else {
      len += sprintf( buf + len, " %s",
		      apm_time_nosec( (int)(i.battery_percentage / rate) ) );
   }

   sprintf( buf + len, " (%d%%%% %s)",
	    i.battery_percentage, apm_time_nosec( seconds ) );
   syslog( LOG_INFO, buf );
}

static void sig_handler( int sig )
{
   syslog( LOG_INFO, "Exiting" );
   unlink( PID_FILE );
   exit( 0 );
}

int main( int argc, char **argv )
{
   int           debug   = 0;
   int           c;
   int           fd;
   int           pid;
   FILE          *str;
   apm_event_t   events[MAX_EVENTS];
   struct option longopts[] = {
      { "verbose",    0, 0, 'v' },
      { "version",    0, 0, 'V' },
      { "debug",      0, 0, 'd' },
      { "percentage", 1, 0, 'p' },
      { "warn",       1, 0, 'w' },
      { "wall",       0, 0, 'W' },
   };

   switch (apm_exists()) {
   case 1: fprintf( stderr, "No APM support in kernel\n" );  exit( 1 );
   case 2: fprintf( stderr, "Old APM support in kernel\n" ); exit( 2 );
   }

   if (getuid()) {
      fprintf( stderr, "apmd: must be run as root\n" );
      exit( 1 );
   }

   while ((c = getopt_long( argc, argv, "Vvdp:w:W", longopts, NULL )) != -1)
      switch (c) {
      case 'V':
	 fprintf( stderr, "apmd version %s\n", VERSION );
	 exit( 0 );
	 break;
      case 'v': ++verbose;                       break;
      case 'd': ++debug;                         break;
      case 'p': percent_change = atoi( optarg ); break;
      case 'w': warn_level = atoi( optarg );     break;
      case 'W': ++wall;                          break;
      default:  usage();                         break;
      }

   if (!access( PID_FILE, R_OK )) {
      if ((str = fopen( PID_FILE, "r" ))) {
	 fscanf( str, "%d", &pid );
	 fclose( str );
	 fprintf( stderr, "An apmd is already running as process %d\n", pid );
	 fprintf( stderr,
		  "If it is no longer running, remove %s\n", PID_FILE );
	 exit( 1 );
      }
   }

   openlog( "apmd", (debug?LOG_PERROR:0)|LOG_PID|LOG_CONS, LOG_DAEMON );
   if (signal( SIGINT, SIG_IGN ) != SIG_IGN)  signal( SIGINT, sig_handler );
   if (signal( SIGQUIT, SIG_IGN ) != SIG_IGN) signal( SIGQUIT, sig_handler );
   if (signal( SIGTERM, SIG_IGN ) != SIG_IGN) signal( SIGTERM, sig_handler );
   
   if (!debug) {		/* detach */
      if ((pid = vfork())) {	/* parent */
	 if ((str = fopen( PID_FILE, "w" ))) {
	    fprintf( str, "%d\n", pid );
	    fclose( str );
	 }
	 exit( 0 );
      }
				/* child */
      if (pid < 0) {
	 syslog( LOG_INFO, "fork() failed: %m" );
	 unlink( PID_FILE );
	 exit( 1 );
      }
				/* Child.  Follow the daemon rules in
                                   W. Richard Stevens. Advanced Programming
                                   in the UNIX Environment (Addison-Wesley
                                   Publishing Co., 1992). Page 417.). */
      if (setsid() < 0) {
	 syslog( LOG_INFO, "setsid() failed: %m" );
	 unlink( PID_FILE );
	 exit( 1 );
      }
      chdir( "/" );
      umask( 0 );
   }
	    
   if ((fd = apm_open()) < 0) {
      syslog( LOG_INFO, "apm_open() failed: %m" );
      unlink( PID_FILE );
      exit( 1 );
   }

   check_power( 0, 0 );
   for (;;) {
      int n = apm_get_events( fd, -1, events, MAX_EVENTS );
      int i;

      for (i = 0; i < n; i++) {
	 if (verbose)
	    syslog( LOG_INFO, "Event 0x%04x: %s",
		    events[i], apm_event_name( events[i] ) );
	 switch (events[i]) {
	 case APM_SYS_STANDBY:
	 case APM_USER_STANDBY:
	    time( &then );
	    apm_standby( fd );
	    break;
	 case APM_SYS_SUSPEND:
	 case APM_USER_SUSPEND:
	    time( &then );
	    apm_suspend( fd );
	    break;
	 case APM_CRITICAL_SUSPEND:
	    time( &then );
				/* As fast as possible */
	    ioctl( fd, APM_IOC_SUSPEND, NULL );
	    break;
	 case APM_NORMAL_RESUME:
	 case APM_STANDBY_RESUME:
	    time( &now );
	    check_power( 1, 0 );
	    break;
	 case APM_UPDATE_TIME:
	 case APM_CRITICAL_RESUME:
	    time( &then );
	    system( "clock -s" );
	    time( &now );
	    check_power( 1, 0 );
	    break;
	 case APM_POWER_STATUS_CHANGE:
	    check_power( 0, 0 );
	    break;
	 case APM_LOW_BATTERY:
	    check_power( 0, 1 );
	    break;
	 }
      }
   }
   
   return 0;
}
