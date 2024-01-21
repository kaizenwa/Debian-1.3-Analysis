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
#include <pwd.h>

#ifdef YP
#include <rpc/rpc.h>
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
extern void setnetgrent(const char *);
extern void endnetgrent(void);
extern int getnetgrent(char **, char **, char **);
extern int innetgr(const char *, const char *, const char *, const char *);
extern int __yp_check(char **);
extern struct passwd * __nis_parsepwddata(char *, void *);
extern struct passwd * __nis_getpwnam(const char *, void *);
#endif


/* Search for an entry with a matching name.  */
struct passwd *
DEFUN(getpwnam, (name), register CONST char *name)
{
#ifdef YP
  static void *info_nis = NULL;
#endif
  static PTR info = NULL;
  register FILE *stream;
  register struct passwd *p;

#ifdef YP
  if (name[0] == '-' || name[0] == '+')
    return NULL;
#endif
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
      if (!strcmp(p->pw_name, name))
        break;
#ifdef YP
          /* Handle -@netgroup entries */
      if ('-' == p->pw_name[0]
          && '@' == p->pw_name[1]
          && '\0' != p->pw_name[2]
          && 1 == innetgr(&p->pw_name[2], NULL, name, NULL))
        return NULL ;
      
          /* Handle +@netgroup entries */
      if ('+' == p->pw_name[0]
          && '@' == p->pw_name[1]
          && '\0' != p->pw_name[2])
        {
          if (1 == innetgr(&p->pw_name[2], NULL, name, NULL))
            {
              p = __nis_getpwnam(name, info) ;
              break;
            }
          continue;
        }
        
          /*
           * Handle +user/-user entries.
           * Bug: overwriting of pw_passwd field
           * not supported (yet). --Swen
           */
      if (('-' == p->pw_name[0]) && (0 == strcmp(p->pw_name + 1, name)))
        return NULL;
      if (('+' == p->pw_name[0]) && (0 == strcmp(p->pw_name + 1, name)))
        {
          struct passwd *pwd = p;
          if (NULL == info_nis)
            {
              info_nis = __pwdalloc();
              if (NULL == info_nis)
                return(NULL);
            }
          p = __nis_getpwnam(name, info_nis);
          if (NULL != pwd->pw_gecos && '\0' != *pwd->pw_gecos)
            p->pw_gecos = pwd->pw_gecos;
          if (NULL != pwd->pw_dir && '\0' != *pwd->pw_dir)
            p->pw_dir   = pwd->pw_dir  ;
          if (NULL != pwd->pw_shell && '\0' != *pwd->pw_shell)
            p->pw_shell = pwd->pw_shell;
          break;
        }
      if (0 == strcmp(p->pw_name, "+"))
        {
          p = __nis_getpwnam(name, info);
          break;
        }
#endif;
    }
  (void) fclose(stream);
  return(p);
}

#ifdef YP
struct passwd *
__nis_getpwnam(const char *name, void *info)
{
  static char *nisdomain = NULL;
  char *outkey;
  int status, outkeylen ;
  struct passwd *pwptr = NULL;
  
  if (1 == __yp_check(NULL))
    {
      if (NULL == nisdomain)
        yp_get_default_domain(&nisdomain);
      status = yp_match(nisdomain, "passwd.byname",
                        name, strlen(name),
                        &outkey, &outkeylen) ;
      if (0 != status)
        return NULL;
      pwptr = __nis_parsepwddata(outkey, info) ;
      free (outkey);
    }
  return (pwptr);
}
#endif
