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
extern void   setnetgrent           (const char *);
extern void   endnetgrent           (void);
extern int    getnetgrent           (char **, char **, char **);
extern int    innetgr               (const char *, const char *,
                                     const char *, const char *);
extern int    __yp_check            (char **);
extern struct passwd
             *__nis_parsepwddata    (char *, void *);
extern struct passwd
             *__nis_getpwnam        (const char *, void *);
extern struct passwd
             *__nis_alloc_pwd_args  (void);
extern void   __nis_clear_pwd_args  (struct passwd *);
extern void   __nis_copy_pwd_args   (struct passwd *, struct passwd *, int);
static char *nisdomain = NULL;
#endif


/* Search for an entry with a matching name.  */
struct passwd *
DEFUN(getpwnam, (name), register CONST char *name)
{
#ifdef YP
  static void *info_nis = NULL;
  static struct passwd *stored_pwd = NULL;
  char *netgroups = NULL;
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
#ifdef YP
      if (NULL == stored_pwd)
	stored_pwd = __nis_alloc_pwd_args();

          /* Handle -@netgroup/+@netgroup entries */
      if (('-' == p->pw_name[0] || '+' == p->pw_name[0])
          && '@' == p->pw_name[1]
          && '\0' != p->pw_name[2])
	{
	  char *s, *t;

	  if (NULL == nisdomain)
	    yp_get_default_domain(&nisdomain);
	  if (! netgroups)
	    {
	      int status, outkeylen;
	      int l = strlen(name);
	      char namebuf[l + strlen(nisdomain) + 3];

	      memcpy(namebuf, name, l);
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
				    namebuf, l + 2, &netgroups, &outkeylen);
		  if (status)
		    continue;
		}

	      netgroups[outkeylen] = ',';
	      netgroups[outkeylen+1] = '\0';
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

	      __nis_clear_pwd_args(stored_pwd);
	      __nis_copy_pwd_args(p, stored_pwd, 0);
	      p = __nis_getpwnam(name, info) ;
              __nis_copy_pwd_args(stored_pwd, p, 1);
	      break;
            }
          continue;
        }
        
          /*
           * Handle +user/-user entries.
           */
      if (('-' == p->pw_name[0]) && (0 == strcmp(p->pw_name + 1, name)))
	{
	  p = NULL;
	  break;
	}
      if (('+' == p->pw_name[0]) && (0 == strcmp(p->pw_name + 1, name)))
        {
	  __nis_clear_pwd_args(stored_pwd);
	  __nis_copy_pwd_args(p, stored_pwd, 0);
	  if (NULL == info_nis)
            {
              info_nis = __pwdalloc();
              if (NULL == info_nis)
                return(NULL);
            }
          p = __nis_getpwnam(name, info_nis);
	  __nis_copy_pwd_args(stored_pwd, p, 1);
          break;
        }
      if (0 == strcmp(p->pw_name, "+"))
        {
	  __nis_clear_pwd_args(stored_pwd);
	  __nis_copy_pwd_args(p, stored_pwd, 0);
	  p = __nis_getpwnam(name, info);
	  __nis_copy_pwd_args(stored_pwd, p, 1);
          break;
        }
#endif;
      if (0 == strcmp(p->pw_name, name))
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

#define USE_PASSWD_ADJUNCT
#ifdef USE_PASSWD_ADJUNCT
  /* Look up NAME if the RESULT from a regular password lookup has 
     an entry starting with '##' in the encrypted section, then
     lookup in passwd.adjunct, and merge that into the data already
     found, returning a parsed password entry.
     [if your nis server uses some other name for the database,
      simply change your servers /var/yp/makefile to emit it under
      passwd.adjunct too!   I dont know of any standard other than
      sun's ..  we could also use some secure file to store this name...
      ]
   
     Otherwise return NULL.
  */     
     
static
struct passwd *
__nis_lookup_password_adjunct(char *nisdomain,  char *name,const char *result,void *info)
{char *tem;
  struct passwd * pw = 0;
  char buf[500];
 char *result1;
 int len;
  if (result && (tem=strchr(result,':')) && tem[1]=='#' && tem[2]=='#' &&
      yp_match(nisdomain, "passwd.adjunct.byname", name, strlen(name), &result1, &len) == 0)
    /* this looks like a shadow entry  and we found an entry in passwd.adjunct */
    {
      /* merge the result of the regular password with the actual encrypted password */
      char * encrypted = strchr(result1,':');
      if (encrypted)
	{ char *p ;
	  p = strchr(encrypted+1,':');
	  if (p)
	    {  *p = 0;
	       tem =  strchr(tem+1,':');
	       if (tem)
		 { sprintf(buf,"%s:%s:%s",name,encrypted+1,tem+1);
		   pw = __nis_parsepwddata(buf , info);
		 }
	     }
	}
      free(result1);
	
   }
 return pw;
}
#endif

struct passwd *
__nis_getpwnam(const char *name, void *info)
{
  char *outkey;
  int status, outkeylen ;
  struct passwd *pwptr = NULL;
  if (name == 0) abort();
  if (1 == __yp_check(NULL))
    {
      if (NULL == nisdomain)
        yp_get_default_domain(&nisdomain);
      status = yp_match(nisdomain, "passwd.byname",
                        name, strlen(name),
                        &outkey, &outkeylen) ;
      if (0 != status)
        return NULL;

#ifdef USE_PASSWD_ADJUNCT
     if(0 == (pwptr=__nis_lookup_password_adjunct(nisdomain,
																  name,outkey,info)))
#endif      
      pwptr = __nis_parsepwddata(outkey, info) ;
      free (outkey);
    }
  return (pwptr);
}


#endif
