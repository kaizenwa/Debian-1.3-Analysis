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
#include <string.h>
#include <grp.h>

#ifdef YP
#include <rpc/rpc.h>
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
extern int __yp_check(char **) ;
extern struct group * __nis_parsegroupdata(char *, void *);
struct group *__nis_getgrnam(char *, char **, void *);
#endif

/* Search for an entry with a matching name.  */
struct group *
DEFUN(getgrnam, (name), register CONST char *name)
{
  static PTR info = NULL;
  register FILE *stream;
  register struct group *g;
  
  if (NULL == info)
    {
      info = __grpalloc();
      if (NULL == info)
        return NULL;
    }
  
  stream = __grpopen();
  if (NULL == stream)
    return NULL;
  
  while ((g = __grpread(stream, info)) != NULL)
    {
#ifdef YP
          /*
           * Handle +group/-group entries.
           * Bug: gets full entry from NIS, overwriting of some fields
           * not supported (yet). --Swen
           */
      if (('-' == g->gr_name[0]) && (0 == strcmp(g->gr_name + 1, name)))
        return NULL;
      if ('+' == g->gr_name[0] && 0 == strcmp(g->gr_name + 1, name))
        {
          struct group *grp = g;
          g = __nis_getgrnam(g->gr_name + 1, grp->gr_mem, info);
          if (NULL != g && NULL != grp->gr_passwd && '\0' != grp->gr_passwd[0])
            g->gr_passwd = grp->gr_passwd;
          break;
        }
      if (0 == strcmp(g->gr_name, "+"))
        {
          g = __nis_getgrnam(name, NULL, info);
          break;
        }
#endif;
      if (!strcmp(g->gr_name, name))
        break;
    }
  (void) fclose(stream);
  return(g);
}

#ifdef YP
/* 'char *name' was 'const char *name' but gcc discarded 'const' - vch */
struct group *
__nis_getgrnam(char *name, char **more_members, void *info)
{
  static char *nisdomain = NULL;
  char *outkey, *outtmp, *c;
  int status, outkeylen ;
  struct group *gptr = NULL;
  int i;
    
  if (1 == __yp_check(NULL))
    {
      if (NULL == nisdomain)
        yp_get_default_domain(&nisdomain);
      status = yp_match(nisdomain, "group.byname",
                        name, strlen(name),
                        &outkey, &outkeylen) ;
      if (0 != status)
        return NULL;
      if (NULL != more_members)
        {
          i = strlen(outkey);
          outtmp = malloc(i+1);
          strcpy(outtmp, outkey);
          free(outkey);
          if ('\n' == outtmp[i-1])
            outtmp[i-1] = '\0';
          i = 0;
          c = strrchr(outtmp, ':');
          *(c+1) = '\0';
          while (more_members[i] != NULL)
            {
              outtmp = realloc(outtmp, strlen(outtmp)
										 + strlen(more_members[i]) + 3);
              strcat(outtmp, more_members[i]);
              strcat(outtmp, ",");
              i++;
            }
          *(strrchr(outtmp, ',')) = '\0';
          strcat(outtmp, "\n");
          outkey = outtmp;
        }
      gptr = __nis_parsegroupdata(outkey, info) ;
      free (outkey);
    }
  return (gptr);
}
#endif
