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
#include <grp.h>

#ifdef YP
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
extern int __yp_check(char **);
extern struct group * __nis_parsegroupdata(char *, void *);
extern struct group * __nis_getgrnam(const char *, char **, void *);
extern struct group * __nis_getgrent(int, void *);
static int ypmode = 0;
static int inkeylen;
static char *inkey = NULL;
#endif

static FILE *stream = NULL;

/* Rewind the stream.  */
void
DEFUN_VOID(setgrent)
{
  if (NULL != stream)
    rewind(stream);
#ifdef YP
  ypmode = 0;
  if(NULL != inkey)
    {
      free (inkey);
      inkey = NULL;
    }
#endif
}


/* Close the stream.  */
void
DEFUN_VOID(endgrent)
{
  if (NULL != stream)
    {
      (void) fclose(stream);
      stream = NULL;
    }
#ifdef YP
  ypmode = 0;
  if(NULL != inkey)
    {
      free (inkey);
      inkey = NULL;
    }
#endif
}


/* Read an entry from the stream.  */
struct group *
DEFUN_VOID(getgrent)
{
#ifdef YP
  struct group* grtmp;
#endif
  static PTR info = NULL;
  if (NULL == info)
    {
      info = __grpalloc();
      if (NULL == info)
        return(NULL);
    }
  
  if (NULL == stream)
    {
      stream = __grpopen();
      if (NULL == stream)
        return(NULL);
    }
  
#ifdef YP
  if (0 == ypmode) 
    {
      again:
          /* disable -group entries */
      do
        {
          grtmp = __grpread(stream, info);
        }
      while (NULL != grtmp && '-' == grtmp->gr_name[0]);
      if (NULL == grtmp)
        return NULL;
          /*
           * Handle +group entries.
           */
      if ('+' == grtmp->gr_name[0] && '\0' != grtmp->gr_name[1])
        {
          struct group *grp = grtmp;
          grtmp = __nis_getgrnam(grtmp->gr_name + 1, grp->gr_mem, info);
          if (NULL == grtmp)
            goto again;
          if (NULL != grp->gr_passwd && '\0' != grp->gr_passwd[0])
            grtmp->gr_passwd = grp->gr_passwd;
        }
      
      if (0 == strcmp(grtmp->gr_name, "+"))
        {
          ypmode = 1;
          grtmp = __nis_getgrent(1, info);
        }
    }
  else
    {
      grtmp = __nis_getgrent(0, info);
    }
  return(grtmp);
#else
  return(__grpread(stream, info));
#endif
}

#ifdef YP
struct group *
__nis_getgrent(int first, void *info)
{
  static char *nisdomain = NULL;
  char *outkey, *outval;
  int outkeylen, outvallen, status;
  struct group *grptr;
  
  if (1 == __yp_check(NULL))
    {
      if (NULL == nisdomain)
        yp_get_default_domain(&nisdomain);
      if (1 == first)
        {
          status = yp_first(nisdomain, "group.byname",
                            &outkey, &outkeylen,
                            &outval, &outvallen);
          if (0 != status)
            return NULL;
          inkey = outkey;
          inkeylen = outkeylen;
              /* Assumption: No comment lines in NIS maps */
          grptr = __nis_parsegroupdata(outval, info);
          free (outval);
          return (grptr);
        }
      else
        {
          status = yp_next(nisdomain, "group.byname",
                           inkey, inkeylen,
                           &outkey, &outkeylen,
                           &outval, &outvallen);
          if (0 != status)
            {
              free (inkey);
              inkey = NULL;
              return NULL;
            }
          free (inkey);
          inkey = outkey;
          inkeylen = outkeylen;
              /* Assumption: No comment lines in NIS maps */
          grptr = __nis_parsegroupdata(outval, info);
          free (outval);
          return (grptr);
        }
    }
  return NULL;
}
#endif
