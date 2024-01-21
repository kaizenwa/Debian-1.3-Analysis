/* Copyright (C) 1991 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>
#include <grp.h>

#ifdef YP
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
extern int __yp_check(char **) ;
extern struct group * __nis_parsegroupdata(char *, void *);
extern struct group * __nis_getgrnam(const char *, char **, void *);
static struct group * __nis_getgrgid(gid_t, void *);
#endif

/* Search for an entry with a matching group ID.  */
struct group *
DEFUN(getgrgid, (gid), register gid_t gid)
{
  static PTR info = NULL;
  register FILE *stream;
  register struct group *g;
  
  if (info == NULL)
    {
      info = __grpalloc();
      if (info == NULL)
        return NULL;
    }
  
  stream = __grpopen();
  if (stream == NULL)
    return NULL;
  
  while ((g = __grpread(stream, info)) != NULL)
    {
#ifdef YP
          /*
           * Handle +group/-group entries.
           */
      if ('-' == g->gr_name[0] && '\0' != g->gr_name[1])
        {
          g = __nis_getgrnam(g->gr_name + 1, NULL, info);
          if (NULL != g && g->gr_gid == gid)
            return NULL;
          continue;
        }
      if ('+' == g->gr_name[0] && '\0' != g->gr_name[1])
        {
          struct group *grp = g;
          g = __nis_getgrnam(g->gr_name + 1, g->gr_mem, info);
          if (NULL != g && g->gr_gid == gid)
            {
              if (NULL != grp->gr_passwd && '\0' != grp->gr_passwd[0])
                g->gr_passwd = grp->gr_passwd;
              break;
            }
          continue;
        }
      if (0 == strcmp(g->gr_name, "+"))
        {
          g = __nis_getgrgid(gid, info);
          break;
        }
#endif
      if ( NULL != g && g->gr_gid == (gid_t) gid)
        break;
    }
  (void) fclose(stream);
  return g;
}

#ifdef YP
static struct group *
__nis_getgrgid(gid_t gid, void *info)
{
  static char *nisdomain = NULL;
  char *outkey;
  int status, outkeylen ;
  struct group *gptr = NULL;
  
  if (1 == __yp_check(NULL))
    {
      char gidbuf[20] ;
      if (NULL == nisdomain)
        yp_get_default_domain(&nisdomain);
      sprintf(gidbuf, "%d", gid) ;
      status = yp_match(nisdomain, "group.bygid",
                        gidbuf, strlen(gidbuf),
                        &outkey, &outkeylen) ;
      if (0 != status)
        return NULL;
      gptr = __nis_parsegroupdata(outkey, info) ;
      free (outkey);
      return (gptr);
    }
  return NULL;
}
#endif
