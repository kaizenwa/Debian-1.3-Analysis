/*****************************************************************************
 * stats.c -- Part of uutraf.c, an UUCP Traffic analyzer and cost estimator  *
 *                                                                           *
 * Reads an Taylor-UUCP Log-/Statsfile, and generates a report out of it     *
 *                                                                           *
 * stats.c is (c) David Frey, 1995    					     *
 *								             *
 * Modifications by Yves Arrouye, 1996                                       *
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

/* $Log: stats.c,v $
 * Revision 1.0  1996/01/01 16:44:17  david
 * Initial revision
 * */

static char RCSId[]="$Id: stats.c,v 1.0 1996/01/01 16:44:17 david Rel $";
 
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <limits.h>

#include "uutraf.h"

void ReadStatsfile(char *statsfilename)
/*
 * Reads in the stats file and builds a linked list of lines, containing
 * the original information.
 * This list is used by AnalyseLogfile, since it is not possible to get
 * the size of a mail without the information in the stats file.
 * Moreover, when connecting to an non-Taylor UUCP-site, the size of a
 * received file is not recorded in the log file.
 *
 */
{
  FILE *statsfile;
  statsentry *ptr;

  char hostname[HOSTNAMELENGTH+1];
  hostentry *host;
  char statsline[LINE_MAX+1];
  char *text;
  char text0[LINE_MAX+1];
  struct tm time;
  short int received;
  float duration;
  int sec100;

  statsfile=fopen(statsfilename,"r"); 
  if (statsfile == NULL)
  {
    fprintf(stderr,"%s: can't open %s: %s\n",
	    progname,statsfilename,strerror(errno));
    exit(1);
  }

  /* Read and parse the Statsfile */
  while (fgets(statsline,LINE_MAX,statsfile) != NULL)
  {
    ptr = calloc(1,sizeof(statsentry));
    if (ptr != NULL)
    {
      sscanf(statsline,"%*s %8s (%d-%d-%d %d:%d:%d.%d) %[ -~]",
	     hostname, &time.tm_year,&time.tm_mon, &time.tm_mday,
	               &time.tm_hour,&time.tm_min, &time.tm_sec, 
	               &sec100,text0);

      ptr->when = statstime(time,sec100);
      text = &text0[0];

      if (strncmp(text,FAILED,sizeof(FAILED)-1) == 0)
        text += (sizeof(FAILED)-1);
      
      ptr->received = received
	= (strncmp(text,RECEIVED,sizeof(RECEIVED)-1) == 0);
      if (received) text += (sizeof(RECEIVED)-1);
      else          text += (sizeof(SENT)-1);

      ptr->size = atol(text);          
      while (isdigit(*text))  text++;
      while (!isdigit(*text)) text++;  
      duration = atof(text);

      host = gethostentry(hostname);
      if (received) 
      { 
	host->datarecnum++;  host->datarecbytes  += ptr->size; 
	host->rectime += duration; 
      }
      else          
      { 
	host->datasentnum++; host->datasentbytes += ptr->size; 
	host->senttime += duration; 
      }
      ptr->next = host->statslist; host->statslist = ptr;
    }
    else
    {
      fprintf(stderr,"%s: Out of memory! (ReadStatsfile)\n",progname);
      exit(1);
    }
  }
  fclose(statsfile);
}

statsentry *GetStatsEntry(statsentry *statslist, int received,
    off_t size, etime_t when)
{
  statsentry *ptr, *prevptr, *prevprevptr;
  statsentry *statptr = 0;

  ptr = statslist; prevptr = NULL;
  while ((ptr != NULL) && when < ptr->when
    && (!statptr || !prevptr || prevptr->received != received))
  {
    if ((ptr->received == received) && ((ptr->size == size) || (size==0))) {
      statptr = ptr;
    }
    prevprevptr = prevptr; prevptr = ptr; ptr = ptr->next;
  }

  return statptr ? statptr : prevptr;
}
