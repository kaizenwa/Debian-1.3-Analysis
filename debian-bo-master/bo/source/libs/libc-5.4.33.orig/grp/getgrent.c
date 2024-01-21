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
/* Structure for remembering -group members ... */
#define BLACKLIST_INITIAL_SIZE 128
#define BLACKLIST_INCREMENT 128
struct _blacklist
{
  char *data;
  int current;
  int size;
};
static struct _blacklist blacklist = { NULL, 0, 0 };
static void __blacklist_store_name(char *);
static int __in_blacklist(char *);
#endif /* YP */

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
  blacklist.current = 0;
  if (NULL != blacklist.data)
    blacklist.data[0] = '\0';
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
  blacklist.current = 0;
  if (NULL != blacklist.data)
    blacklist.data[0] = '\0';
#endif
}


/* Read an entry from the stream.  */
struct group *
DEFUN_VOID(getgrent)
{
#ifdef YP
  struct group *grp, *grtmp;
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
  if (1 == ypmode) 
    {
      grtmp = __nis_getgrent(0, info);
      if (NULL != grtmp)
        return grtmp;
      else
        ypmode = 0;
    }
  
  again:
  grtmp = __grpread(stream, info);
  if (NULL == grtmp)
    return NULL;
      /* disable -group entries */
  if ('-' == grtmp->gr_name[0] && '\0' != grtmp->gr_name[1])
    {
      __blacklist_store_name(&grtmp->gr_name[1]);
      goto again;
    }
      /*
       * Handle +group entries.
       */
  if ('+' == grtmp->gr_name[0] && '\0' != grtmp->gr_name[1])
    {
      grp = grtmp;
      grtmp = __nis_getgrnam(grtmp->gr_name + 1, grp->gr_mem, info);
      if (NULL == grtmp)
        goto again;
      if (NULL != grp->gr_passwd && '\0' != grp->gr_passwd[0])
        grtmp->gr_passwd = grp->gr_passwd;
    }
  
  if (0 == strcmp(grtmp->gr_name, "+"))
    {
      ypmode = 1;
      grp = grtmp;
      grtmp = __nis_getgrent(1, info);
      if (NULL != grp->gr_passwd && '\0' != grp->gr_passwd[0])
        grtmp->gr_passwd = grp->gr_passwd;
    }
  if (grtmp && grtmp->gr_name && __in_blacklist(grtmp->gr_name))
    goto again;
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
/*
 * Support routines for remembering -group entries.
 * The names are stored in a single string with \n as separator.
 */
static void
__blacklist_store_name(char *name)
{
  int namelen;
  char *tmp;
  
  if (__in_blacklist(name))
    return;   /* no duplicates */
  
  namelen = strlen(name);
      /* first call, setup cache */
  if (0 == blacklist.size)
    {
      blacklist.data = malloc(BLACKLIST_INITIAL_SIZE);
      if (NULL == blacklist.data)
        return;
      blacklist.size = BLACKLIST_INITIAL_SIZE;
      blacklist.current = 0;
    }
  
  if (blacklist.current + namelen + 1 >= blacklist.size)
    {
      tmp = realloc(blacklist.data, blacklist.size + BLACKLIST_INCREMENT);
      if (NULL == tmp)
        {
          free(blacklist.data);
          blacklist.size = 0;
          return;
        }
      blacklist.size += BLACKLIST_INCREMENT;
      blacklist.data = tmp;
    }
  
  strncpy(blacklist.data + blacklist.current, name, namelen);
  tmp = blacklist.data + blacklist.current + namelen;
  *tmp++ = '\n';
  *tmp = '\0';
  blacklist.current += namelen + 1 ;
  
  return;
}

/* returns TRUE if blacklist contains name, else FALSE */
static int
__in_blacklist(char *name)
{
  if (NULL != blacklist.data)
    return (NULL != strstr(blacklist.data, name));
  return 0;
}
#endif
