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
extern struct passwd * __nis_alloc_pwd_args(void);
extern void __nis_copy_pwd_args(struct passwd *, struct passwd *, int);
extern void __nis_clear_pwd_args(struct passwd *);
static struct passwd * __nis_getpwuid(uid_t, void *);
static char *nisdomain = NULL;
#endif

/* Search for an entry with a matching uid.  */
struct passwd *
DEFUN(getpwuid, (uid), register uid_t uid)
{
#ifdef YP
  static void *info_nis = NULL;
  static struct passwd *stored_pwd = NULL;
  struct passwd *nis_pwd = NULL;
  char *netgroups = NULL;
  int nis_failed = 0;
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
#ifdef YP
      if (NULL == stored_pwd)
	stored_pwd = __nis_alloc_pwd_args();

          /* Handle -@netgroup/+@netgroup entries */
      if (('-' == p->pw_name[0] || '+' == p->pw_name[0])
          && '@' == p->pw_name[1]
          && '\0' != p->pw_name[2])
	{
	  char *s, *t;

	  if (nis_failed)
	    continue; /* We failed once so do not try again */

	  if (! netgroups)
	    {
	      if (! info_nis)
		{
		  info_nis = __pwdalloc();
		  if (NULL == info_nis)
		    return(NULL);
		}
	      nis_pwd = __nis_getpwuid(uid, info_nis);
	      if (! nis_pwd)
		{
		  nis_failed = 1;
		  continue;
		}
	      else
		{
		  /* __nis_getpwuid already initialized nisdomain */
		  int status, outkeylen;
		  int l = strlen(nis_pwd->pw_name);
		  char namebuf[l + strlen(nisdomain) + 3];

		  memcpy(namebuf, nis_pwd->pw_name, l);
		  namebuf[l] = '.';
		  strcpy(namebuf + l + 1, nisdomain);
		  status = yp_match(nisdomain, "netgroup.byuser",
				    namebuf, l + strlen(nisdomain) + 1,
				    &netgroups, &outkeylen);
		  if (status)
		    {
		      namebuf[l+1] = '*';
		      namebuf[l+2] = '\0';
		      status = yp_match(nisdomain, "netgroup.byuser",
					namebuf, l + 2,
					&netgroups, &outkeylen);
		      if (status)
			{
			  nis_failed = 1;
			  continue;
			}
		    }

		  netgroups[outkeylen] = ',';
		  netgroups[outkeylen+1] = '\0';
		}
	    }

	  t = s = netgroups;
	  while (*s && *t)
	    {
	      t = p->pw_name + 2;
	      while (*s != ',' && *t++ == *s++);
	      while (*s++ != ',');
	    }

	  if (! *t)
	    {
	      if ('-' == p->pw_name[0])
		{
		  p = NULL;
		  break;
		}

              __nis_copy_pwd_args(p, nis_pwd, 1);
	      p = nis_pwd;
	      break;
            }
	  continue;
	}

          /*
           * Handle +user/-user entries.
           */
      if ('-' == p->pw_name[0] && '\0' != p->pw_name[1])
        {
          p = __nis_getpwnam(p->pw_name + 1, info);
          if (p && p->pw_uid == uid)
	    {
	      p = NULL;
	      break;
	    }
          continue;
        }
      if ('+' == p->pw_name[0] && '\0' != p->pw_name[1])
        {
	  __nis_clear_pwd_args(stored_pwd);
	  __nis_copy_pwd_args(p, stored_pwd, 0);
          p = __nis_getpwnam(p->pw_name + 1, info);
          if (p && p->pw_uid == uid)
	    {
	      __nis_copy_pwd_args(stored_pwd, p, 1);
	      break;
	    }
	  continue;
	}
      if (0 == strcmp(p->pw_name, "+"))
        {
	  __nis_clear_pwd_args(stored_pwd);
	  __nis_copy_pwd_args(p, stored_pwd, 0);
          p = __nis_getpwuid(uid, info);
	  __nis_copy_pwd_args(stored_pwd, p, 1);
          break;
        }
#endif
      if (p && p->pw_uid == uid)
        break;
    }
#ifdef YP		/* Added by vch */
  if (netgroups)
    free(netgroups);
#endif
  (void) fclose(stream);
  return(p);
}

#ifdef YP
static struct passwd *
__nis_getpwuid(uid_t uid, void *info)
{
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
#endif
