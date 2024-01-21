/* filename: rlpr-dbfile.c
 * project: rlpr
 * author: meem  --  meem@sherilyn.wustl.edu
 * version: $Id: rlpr-dbfile.c,v 1.8 1996/11/19 01:15:48 meem Exp $
 * contents: procedures for the rlpr database file
 * 
 * Time-stamp: <1996/11/16 19:44 -- meem@sherilyn.wustl.edu>
 */

/* copyright (c) 1996 meem, meem@gnu.ai.mit.edu
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

#include "config.h"

#include <stdio.h>		      /* for FILE*, etc */
#include <string.h>
#include <stdlib.h>		      /* for getenv() */
#include "rlpr-common.h"
#include "rlpr-dbfile.h"
#include "rlpr-client.h"	      /* common to parts of the rlpr client */

static char *dbfilename = NULL;
static FILE *dbp;

void opendbfile(void) {
  char *home;

  if (!(home = getenv("HOME")))
    rlpr_msg(FATAL, NO_ERRNO, "getenv: $HOME not set!");

  dbfilename = rlpr_malloc(strlen(home) + strlen(DBNAME) + 2);
  sprintf(dbfilename, "%s/%s", home, DBNAME);

  if ((dbp = fopen(dbfilename, "r")) == NULL)
    /* no local one, lets look for a systemwide one */
    if ((dbp = fopen(DEFAULT_SYS_DBNAME, "r")) == NULL)
      rlpr_msg(FATAL, ERRNO, "%s - cannot open", DEFAULT_SYS_DBNAME);
}

void closedbfile(void) {
  if (fclose(dbp) < 0)
    rlpr_msg(FATAL, ERRNO, "%s - cannot close", dbfilename);

  free(dbfilename);
}

char * getqfromhost(char *printhost) {
  char *queue;			      /* what this returns */
  opendbfile();

  if ((queue = db_search(printhost, FINDQ)) == NULL)
    rlpr_msg(FATAL, NO_ERRNO, 
	     "host \"%s\" has no printers, check %s", printhost, dbfilename);
  closedbfile();
  return queue;
}

char * gethostfromq(char *queue) {
  char *printhost;		      /* returned by db_search */
  opendbfile();  

  if ((printhost = db_search(queue, FINDHOST)) == NULL)
    rlpr_msg(FATAL, NO_ERRNO,
	     "no host has printer \"%s\", check %s", queue, dbfilename);

  closedbfile();		      /* close database file */
  return printhost;
}

/* main engine */

char * db_search(char *sstr, int type) {
  char line[DB_LINE_LEN];
  char *cloc, *hloc, *qloc;	      /* ptr to colon, host, q in line */
  char *result = NULL;		      /* our result */
  char *linep = line;		      /* pointer to line */
  int sz;			      /* ubiquitous size variable */

  while ((fgets(line, DB_LINE_LEN, dbp))) {
    if (*line == '\n' || *line == '#') continue;    /* comment line */
    else if (!(cloc = strchr(line, ':'))) continue; /* invalid line */

    if (type == FINDQ) {
      strlower(sstr);		      /* lowercase the hostname */

      if ((hloc = strstr(line, sstr)) == NULL) continue;
      if ((strchr(" \t:", hloc[strlen(sstr)])) == NULL) continue;
      if (hloc >= cloc) continue;
 
      while (*(++cloc) == ' ');	      /* chop off whitespace */
      sz = strcspn(cloc, "\n! ");     /* grab next "word" */
      result = rlpr_malloc(sz + 1);
      strncpy(result, cloc, sz);
      result[sz] = '\0';	      /* add null byte */

    } else /* FINDHOST */ {

      if ((qloc = strstr(cloc, sstr)) == NULL) continue;
      if ((strchr(" !\n", qloc[strlen(sstr)])) == NULL) continue;

      while (*linep == ' ') linep++;  /* chop off whitespace */
      sz = strcspn(linep, " :");      /* grab next word */
      if (result) free(result);	      /* old failed attempt? */
      result = rlpr_malloc(sz + 1);
      strncpy(result, linep, sz);
      result[sz] = '\0';	      /* add null byte */
      if (qloc[strlen(sstr)] == '!') return result;
    }
  }
  return result;
}
