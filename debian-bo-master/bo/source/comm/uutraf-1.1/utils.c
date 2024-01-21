/*****************************************************************************
 * utils.c -- Part of UUCP Traffic analyzer and cost estimator               *
 *                                                                           *
 * Reads an Taylor-UUCP Log-/Statsfile, and generates a report out of it     *
 *                                                                           *
 * utils.c is (c) David Frey, 1995    					     *
 *								             *
 * This program is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU General Public License as published by the     *
 * Free Software Foundation; either version 2 of the License, or (at your    *
 * option) any later version.                                                *
 *									     *
 * This program is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of                *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                      *
 * See the GNU General Public License for more details.                      *
 *									     *
 * You should have received a copy of the GNU General Public License along   *
 * with this program; if not, write to the Free Software Foundation, Inc.,   *
 * 675 Mass Ave, Cambridge, MA 02139, USA.                                   *
 *****************************************************************************/

#include <stdio.h>
#include <time.h>

/* $Log: utils.c,v $
 * Revision 1.0  1996/01/01 16:47:32  david
 * Initial revision
 * */

static char RCSId[]="$Id: utils.c,v 1.0 1996/01/01 16:47:32 david Rel david $";

char timestr[7];

time_t unixtime(struct tm time)
{
  time.tm_mon--; time.tm_year -= 1900; /* Month 0..11, Year 1900... */
  time.tm_isdst = -1;                  /* compute DST */
  return mktime(&time);
}

time_t statstime(struct tm time, time_t sec100)
{
  time.tm_mon--; time.tm_year -= 1900; /* Month 0..11, Year 1900... */
  time.tm_isdst = -1;                  /* compute DST */
  return mktime(&time)*100+sec100;
}

char *sec2str(time_t secs)
{
  int min;
  
  min = secs/  60; secs -=   min*60;
  sprintf(timestr,"%03d:%02ld",min,secs);
  return timestr;
}



