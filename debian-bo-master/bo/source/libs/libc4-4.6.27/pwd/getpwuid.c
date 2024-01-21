/*  LAST EDIT: Wed Mar 31 06:07:51 1993 by Swen Thümmler (swen)  */
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
#include <pwd.h>
#include <sys/types.h>

#ifdef YP
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
extern int __yp_check(char **) ;
extern struct passwd * __nis_parsepwddata(char *, void *);
extern void setnetgrent(const char *);
extern void endnetgrent(void);
extern int getnetgrent(char **, char **, char **);
extern int innetgr(const char *, const char *, const char *, const char *);
extern struct passwd * __nis_getpwnam(const char *, void *);

static struct passwd * __nis_getpwuid(uid_t, void *);
static struct passwd * __netgroup_getpwuid(const char *, uid_t, void *);
#endif

/* Search for an entry with a matching uid.  */
struct passwd *
DEFUN(getpwuid, (uid), register uid_t uid)
{
#ifdef YP
  static void *info_nis = NULL;
#endif
  static PTR info = NULL;
  register FILE *stream;
  register struct passwd *p;

  if (info == NULL)
    {
      info = __pwdalloc();
      if (info == NULL)
	return(NULL);
    }

  stream = __pwdopen();
  if (stream == NULL)
    return(NULL);

  while ((p = __pwdread(stream, info)) != NULL)
    {
      if (p->pw_uid == uid)
        break;
#ifdef YP
          /* Handle -@netgroup entries */
      if ('-' == p->pw_name[0]
          && '@' == p->pw_name[1]
          && '\0' != p->pw_name[2])
        {
          p = __netgroup_getpwuid(&p->pw_name[2], uid, info);
          if (NULL != p)
            return NULL;
          break;
        }

          /* Handle +@netgroup entries */
      if ('+' == p->pw_name[0]
          && '@' == p->pw_name[1]
          && '\0' != p->pw_name[2])
        {
          p = __netgroup_getpwuid(&p->pw_name[2], uid, info);
          if (NULL != p)
            break;
          continue;
        }
      
          /*
           * Handle +user/-user entries.
           * Bug: gets full entry from NIS, overwriting of some fields
           *      not supported (yet). --Swen
           */
      if ('-' == p->pw_name[0] && '\0' != p->pw_name[1])
        {
          p = __nis_getpwnam(p->pw_name + 1, info);
          if (p->pw_uid == uid)
            return NULL;
          continue;
        }
      if ('+' == p->pw_name[0] && '\0' != p->pw_name[1])
        {
          struct passwd *pwd = p;
          if (NULL == info_nis)
            {
              info_nis = __pwdalloc();
              if (NULL == info_nis)
                return(NULL);
            }
          p = __nis_getpwnam(p->pw_name + 1, info_nis);
          if (p->pw_uid == uid)
            {
              if (NULL != pwd->pw_gecos && '\0' != *pwd->pw_gecos)
                p->pw_gecos = pwd->pw_gecos;
              if (NULL != pwd->pw_dir && '\0' != *pwd->pw_dir)
                p->pw_dir   = pwd->pw_dir  ;
              if (NULL != pwd->pw_shell && '\0' != *pwd->pw_shell)
                p->pw_shell = pwd->pw_shell;
              break;
            }
          continue;
        }
      if (0 == strcmp(p->pw_name, "+"))
        {
          p = __nis_getpwuid(uid, info);
          break;
        }
#endif
    }
  (void) fclose(stream);
  return(p);
}

#ifdef YP
static struct passwd *
__nis_getpwuid(uid_t uid, void *info)
{
  static char *nisdomain = NULL;
  char *outkey;
  int status, outkeylen ;
  struct passwd *pwptr = NULL;
  
  if (1 == __yp_check(NULL))
    {
      char uidbuf[20] ;
      if (NULL == nisdomain)
        yp_get_default_domain(&nisdomain);
      sprintf(uidbuf, "%d", uid) ;
      status = yp_match(nisdomain, "passwd.byuid",
                        uidbuf, strlen(uidbuf),
                        &outkey, &outkeylen) ;
      if (0 != status)
        return NULL;
      pwptr = __nis_parsepwddata(outkey, info) ;
      free (outkey);
      return (pwptr);
    }
  return NULL;
}

static struct passwd *
__netgroup_getpwuid(const char *netgr, uid_t uid, void *info)
{
  int status;
  static char *nisdomain = NULL;
  char *host, *user, *domain;
  struct passwd *p = NULL;
    
  if (NULL == nisdomain)
    yp_get_default_domain(&nisdomain);
  
  setnetgrent(netgr);
  while(1)
    {
      while (1 == (status = getnetgrent(&host, &user, &domain))
             && NULL == user
             && NULL != domain
             && 0 != strcmp(domain, nisdomain))
        ;
      
      if (0 == status || NULL == user)
        {
          p = NULL;
          break;
        }
      
      p = __nis_getpwnam(user, info);
      if (p->pw_uid == uid)
        break;
      }
  endnetgrent();
  return p;
}
#endif
