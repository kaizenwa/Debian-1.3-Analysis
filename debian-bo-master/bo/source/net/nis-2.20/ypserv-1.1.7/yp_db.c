/*
** yp_db.c - database functions for the maps
**
** This file is part of the NYS YP Server.
**
** The NYS YP Server is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS YP Server is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** General Public License for more details.
** 
** You should have received a copy of the GNU General Public
** License along with the NYS YP Server; see the file COPYING.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Thorsten Kukuk <kukuk@uni-paderborn.de>
*/

static char rcsid[] = "$Id: yp_db.c,v 1.7 1997/03/07 15:27:31 kukuk Exp $";

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "system.h"

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <sys/param.h>

#include "ypserv.h"
#include "yp_msg.h"
#include "yp_db.h"
#include "yp.h"

#if defined(HAVE_LIBGDBM)
#include <gdbm.h>
#endif

/* repleace "field" with x, if field 3 != user id and user id != 0 */
static inline int 
mangle_field (datum * val, int field)
{
  int i, j, k, anz;
  char *p;

  /* allocate a little bit more memory, it's safer, because the 
     field could be empty */
  if ((p = malloc (val->dsize + 3)) == NULL)
    {
      yp_msg ("ERROR: could not allocate enough memory! [%s|%d]\n", __FILE__, __LINE__);
      return -1;
    }

  anz = 0;
  for (i = 0; i < val->dsize; i++)
    {
      if (val->dptr[i] == ':')
	anz++;
      if (anz + 1 == field)
	{
	  anz = i;
	  strncpy (p, val->dptr, anz);
	  p[anz] = 0;
	  /* if field == 1, we don't need a beginning ":" */
	  if (field == 1)
	    {
	      strcat (p, "x");
	      anz += 1;
	    }
	  else
	    {
	      strcat (p, ":x");
	      anz += 2;
	    }
	  for (j = anz; j < val->dsize && val->dptr[j] != ':'; j++);
	  for (k = j; k < val->dsize; k++)
	    {
	      p[anz] = val->dptr[k];
	      anz++;
	    }
	  free (val->dptr);
	  val->dptr = p;
	  val->dsize = anz;
	  return 0;
	}
    }
  free (p);
  return 0;
}

#if defined(HAVE_LIBGDBM)

/* Open a GDBM database */
GDBM_FILE 
_db_open (const char *domain, const char *map)
{
  GDBM_FILE dbp;
  char buf[MAXPATHLEN + 2];

  if (debug_flag)
    yp_msg ("\tdb_open(\"%s\", \"%s\")\n", domain, map);

  if (map[0] == '.' || strchr (map, '/'))
    {
      if (debug_flag)
	yp_msg ("\t\t->Returning 0\n");
      return 0;
    }

  if (strlen (domain) + strlen (map) < MAXPATHLEN)
    {
      sprintf (buf, "%s/%s", domain, map);

      dbp = gdbm_open (buf, 0, GDBM_READER, 0, NULL);

      if (debug_flag && dbp == NULL)
	yp_msg ("gdbm_open: GDBM Error Code #%d\n", gdbm_errno);
      else if (debug_flag)
	yp_msg ("\t\t->Returning OK!\n");
    }
  else
    {
      dbp = NULL;
      yp_msg ("Path to long: %s/%s\n", domain, map);
    }

  return dbp;
}

int 
_db_close (GDBM_FILE file)
{
  gdbm_close (file);
  return 0;
}

/* Get a record from a GDBM database. */
int 
ypdb_read (GDBM_FILE dbp, const datum * ikey, datum * okey,
	   datum * dval, int flags, int mangle)
{
  int first_flag = 0;
  datum nkey, ckey;

  if (ikey == NULL || ikey->dptr == NULL)
    {
      if (debug_flag)
	yp_msg ("\tread_database(), gdbm_firstkey()\n");

      ckey = gdbm_firstkey (dbp);
      first_flag = 1;
    }
  else
    {
      if (debug_flag)
	yp_msg ("\tread_database(), gdbm_nextkey()\n");

      if ((flags & F_NEXT))
	ckey = gdbm_nextkey (dbp, *ikey);
      else
	ckey = *ikey;
    }

  if (ckey.dptr == NULL)
    {
      return (flags & F_NEXT) ? YP_NOMORE : YP_NOKEY;
    }

  while (1)
    {
      *dval = gdbm_fetch (dbp, ckey);
      if (dval->dptr == NULL)
	{
	  /* Free key, unless it comes from the caller! */
	  if (ikey == NULL || ckey.dptr != ikey->dptr)
	    free (ckey.dptr);

	  if (ikey && ikey->dptr != NULL)
	    {
	      return YP_NOKEY;
	    }
	  else if (first_flag)
	    return YP_BADDB;
	  else
	    return YP_FALSE;
	}

      if ((flags & F_ALL) || strncmp (ckey.dptr, "YP_", 3) != 0)
	{
	  if (okey)
	    *okey = ckey;
	  else if (ikey == NULL || ikey->dptr != ckey.dptr)
	    free (ckey.dptr);

	  if (mangle)
	    if (mangle_field (dval, mangle) < 0)
	      return YP_YPERR;

	  return YP_TRUE;
	}

      /* Free old value */
      free (dval->dptr);

      nkey = gdbm_nextkey (dbp, ckey);

      /* Free old key, unless it comes from the caller! */
      if (ikey == NULL || ckey.dptr != ikey->dptr)
	free (ckey.dptr);

      if (ckey.dptr == NULL || nkey.dptr == NULL)
	return YP_NOMORE;

      ckey = nkey;
    }
}

#else

void 
ypdb_open ()
{
}

#endif

int
ypdb_close_all (void)
{
  return 0;
}

int 
ypdb_close (DB_FILE file)
{
  return _db_close (file);
}

DB_FILE 
ypdb_open (const char *domain, const char *map)
{
  return _db_open (domain, map);
}
