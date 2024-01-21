/*****************************************************************************
 * config.c -- Part of uutraf.c, an UUCP Traffic analyzer and cost estimator *
 *                                                                           *
 * Reads an Taylor-UUCP Log-/Statsfile, and generates a report out of it     *
 *                                                                           *
 * config.c is (c) David Frey, 1995    					     *
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

/* $Log: config.c,v $
 * Revision 1.0  1996/01/01 16:42:07  david
 * Initial revision
 * */

static char RCSId[]="$Id: config.c,v 1.0 1996/01/01 16:42:07 david Rel david $"; 

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <malloc.h>
#include <string.h>
#include <time.h>
#include <limits.h>

#include "uutraf.h"

#ifndef DEFAULT_PROVFEEPERKBMULT
#define DEFAULT_PROVFEEPERKBMULT 1
#endif

hostentry *BuildHostTable(char *cfgfilename)
/* 
 *  Reads in the config file, which is a table containing the entries 
 *      host cost/min fee/mail fee/(mail*kB) cost/call cost/min
 * , and builds a linked list of host entries.
 */
{
  FILE *cfgfile;
  char cfgline[LINE_MAX+1];
  int  lineno;
  hostentry *hosttab, *ptr;

  hosttab = NULL;
  cfgfile = fopen(cfgfilename,"r");
  if (cfgfile == NULL)
    fprintf(stderr,
	    "%s: no costs will be calculated due to missing '%s'\n",
	    progname, cfgfilename);
  else
  {
    lineno=1;
    while (fgets(cfgline,LINE_MAX,cfgfile) != NULL)
    {
      char* line;

      for (line = cfgline; isspace(*line); ++line);

      if (line[0] && line[0] != '#') 
      {
	ptr = calloc(1,sizeof(hostentry));
	if (ptr != NULL)
	{
	  int nfields;
	  /* host cost/min fee/mail fee/(mail*kB) cost/call cost/min */

	  ptr->provfeeperkbmult = DEFAULT_PROVFEEPERKBMULT;
	  ptr->provfeeperfile = ptr->provfeeperfilekb = 0;

	  if ((nfields = sscanf(line,"%8s %f %f %f %f %f %f %f %f %f %f",
		     ptr->hostname, &ptr->provlinecostpermin,
		     &ptr->provfeeperfile, &ptr->provfeeperfilekb,
		     &ptr->provfeepermail, &ptr->provfeepermailkb,
		     &ptr->provfeepernews, &ptr->provfeepernewskb,
		     &ptr->provfeeperkbmult,
		     &ptr->pttcostpercall, &ptr->pttlinecostpermin))
		< 6 || nfields > 11)
	  {
	    fprintf(stderr,"%s: Parse error line %d of %s.\n",
		    progname,lineno,cfgfilename);
	  }
	  else
	  {
	    switch (nfields) {
	      case 7:
		ptr->provfeeperkbmult = ptr->provfeepernews;
		ptr->provfeepernews = 0;

	      case 6:
		ptr->pttlinecostpermin = ptr->provfeepermailkb;
		ptr->pttcostpercall = ptr->provfeepermail;

		ptr->provfeepermailkb = ptr->provfeeperfilekb;
		ptr->provfeepermail = ptr->provfeeperfile;

		ptr->provfeeperfilekb = ptr->provfeeperfile = 0;
		break;

	      case 9:
	      case 8:
		ptr->pttlinecostpermin = ptr->provfeepernewskb;
		ptr->pttcostpercall = ptr->provfeepernews;
	
		ptr->provfeepernewskb = ptr->provfeepernews = 0;
		break;

	     case 10:
		ptr->pttlinecostpermin = ptr->pttcostpercall;
		ptr->pttcostpercall = ptr->provfeeperkbmult;

		ptr->provfeeperkbmult = DEFAULT_PROVFEEPERKBMULT;
		break;
	    }
	    ptr->next = hosttab; hosttab = ptr;
	  }
	}
	else
	{
	  fprintf(stderr,"%s: Out of memory! (BuildHostTable)\n",progname);
	  exit(1);
	}

      }
      lineno++;
    }
    fclose(cfgfile);
  }
  return hosttab;
}

hostentry *gethostentry(char *hostname)
/*
 * Try to find HOSTNAME and to return its pointer.
 * Create a new entry, when HOSTNAME was not found (and issue a warning,
 * since the host is not recorded in the config file). 
 *
 */
{
  hostentry *ptr, *prevptr;

  ptr = hosttab; prevptr = NULL;
  while ((ptr != NULL) && (strcmp(ptr->hostname,hostname) != 0))
  {
    prevptr = ptr; ptr = ptr->next; 
  }

  if (ptr == NULL)    /* HOSTNAME not found */
  {
    ptr = calloc(1,sizeof(hostentry));
    if (ptr != NULL)
    {
      strcpy(ptr->hostname,hostname);
      ptr->next = hosttab; hosttab = ptr;

      fprintf(stderr,
	  "%s: no entry for %s, no costs will be calculated\n",
	  progname, hostname);
    }
    else
    {
      fprintf(stderr,"%s: out of memory! (in GetHostEntry())\n",progname);
      exit(1);
    }
  }

  return ptr;
}
