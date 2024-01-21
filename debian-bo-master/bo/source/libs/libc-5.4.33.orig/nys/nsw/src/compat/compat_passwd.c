/*
** compat_passwd.c           COMPAT Passwd map access routines
*/
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

#include "config.h"

#ifdef ENABLE_COMPAT


#include <ansidecl.h>
#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <time.h>
#include <sys/types.h>
#define USE_YP_ALL   1  /* Don't define this if you're short of memory */
#include <rpc/rpc.h>
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
extern int __yp_check(char **);
extern void setnetgrent(const char *);
extern void endnetgrent(void);
extern int getnetgrent(char **, char **, char **);
extern int innetgr(const char *, char *, char *, char *);
extern ssize_t __getline(char **, size_t *, FILE *);
static void * __pwdalloc(void);
static FILE * __pwdopen(void);
static struct passwd * __pwdread(FILE *, void * const);
static struct passwd * __nis_parsepwddata(char *, void *);
static struct passwd * __nis_getpwnam(const char *, void *);
static struct passwd * __nis_alloc_pwd_args(void);
static void __nis_copy_pwd_args(struct passwd *, struct passwd *, int);
static void __nis_clear_pwd_args(struct passwd *);
static struct passwd * __nis_getpwent(int, void *);
static struct passwd * __nis_getpwuid(uid_t, void *);
static struct passwd * __netgroup_getpwent(const char *, int, void *);
static struct passwd * __netgroup_getpwuid(const char *, uid_t, void *);
static int ypmode = 0;
static int netgroupmode = 0;
static int inkeylen;
static char *inkey = NULL;
static struct passwd *stored_pwd = NULL;
static char *nisdomain = NULL;

/* This is the function that all the others are based on.
   The format of the password file is known only here.  */

/* Structure containing info kept by each __pwdread caller.  */
typedef struct
  {
    char *buf;
    size_t buflen;
    struct passwd p;
  } pwdread_info;


#if USE_YP_ALL
#define NIS_CACHE_TIMEOUT 60
/*
 * These values should be adjusted to the actual size of the password
 * file to avoid extra mallocs.
 */
#define NIS_CACHE_INITIAL_SIZE 64*1024
#define NIS_CACHE_INCREMENT 16*1024
struct _nis_cache 
{
  char *data;
  int current;
  int size;
  time_t update;
};

static struct _nis_cache nis_cache = { NULL, 0, 0, 0 };
static int __nis_store_map(int, char *, int, char *, int, char *);
static struct passwd *__nis_get_first_cached_entry(void *);
static struct passwd *__nis_get_next_cached_entry(void *);
#endif /* USE_YP_ALL */
/* Structure for remembering -@netgroup and -user members ... */
#define BLACKLIST_INITIAL_SIZE 512
#define BLACKLIST_INCREMENT 256
struct _blacklist
{
  char *data;
  int current;
  int size;
};
static struct _blacklist blacklist = { NULL, 0, 0 };
static void __blacklist_store_name(char *);
static int __in_blacklist(char *);

static FILE *stream = NULL;

/* Rewind the stream.  */
void
_compat_setpwent(void)
{
  if (stream != NULL)
    rewind(stream);
  ypmode = netgroupmode = 0;
  if(NULL != inkey)
    {
      free (inkey);
      inkey = NULL;
    }
  blacklist.current = 0;
  if (NULL != blacklist.data)
    blacklist.data[0] = '\0';
}


/* Close the stream.  */
void
_compat_endpwent(void)
{
  if (stream != NULL)
    {
      (void) fclose(stream);
      stream = NULL;
    }
  ypmode = netgroupmode = 0;
  if(NULL != inkey)
    {
      free (inkey);
      inkey = NULL;
    }
  blacklist.current = 0;
  if (NULL != blacklist.data)
    blacklist.data[0] = '\0';
}


/* Return one entry from the password file.  */
struct passwd *
_compat_getpwent(void)
{
  struct passwd *pwtmp;
  static void *info_nis = NULL;
  static void *info = NULL;
  if (NULL == info)
    {
      info = __pwdalloc();
      if (NULL == info)
        return(NULL);
    }
  
  if (NULL == stream)
    {
      stream = __pwdopen();
      if (NULL == stream)
        return(NULL);
    }
  if (NULL == stored_pwd)
    stored_pwd = __nis_alloc_pwd_args();
  if ( 1 == netgroupmode)
    {
      pwtmp = __netgroup_getpwent(NULL, 0, info);
      __nis_copy_pwd_args(stored_pwd, pwtmp, 1);
      if (NULL != pwtmp)
        return pwtmp;
      else
        {
          netgroupmode = 0;
          __nis_clear_pwd_args(stored_pwd);
        }
    }
  if ( 1 == ypmode )
    {
      pwtmp = __nis_getpwent(0, info);
      __nis_copy_pwd_args(stored_pwd, pwtmp, 1);
      if (NULL != pwtmp)
        return pwtmp;
      else
        {
          ypmode = 0;
          __nis_clear_pwd_args(stored_pwd);
        }
    }
  
  again:
  pwtmp = __pwdread(stream, info);
  if (NULL == pwtmp)
    return NULL;
      /* Handle -@netgroup entries */
  if ('-' == pwtmp->pw_name[0]
      && '@' == pwtmp->pw_name[1]
      && '\0' != pwtmp->pw_name[2])
    {
      pwtmp = __netgroup_getpwent(&pwtmp->pw_name[2], 1, info);
      while (NULL != pwtmp)
        {
          __blacklist_store_name(pwtmp->pw_name);
          pwtmp = __netgroup_getpwent(pwtmp->pw_name, 0, info);
        }
      goto again;
    }
      /* Handle +@netgroup entries */
  if ('+' == pwtmp->pw_name[0]
      && '@' == pwtmp->pw_name[1]
      && '\0' != pwtmp->pw_name[2])
    {
      __nis_clear_pwd_args(stored_pwd);
      __nis_copy_pwd_args(pwtmp, stored_pwd, 0);
      netgroupmode = 1;
      pwtmp = __netgroup_getpwent(&pwtmp->pw_name[2], 1, info);
      if (NULL == pwtmp) 
        {
          netgroupmode = 0;
          __nis_clear_pwd_args(stored_pwd);
          goto again;
        }
      __nis_copy_pwd_args(stored_pwd, pwtmp, 1);
    }
      /* disable -user entries */
  if ('-' == pwtmp->pw_name[0]
      && '\0' != pwtmp->pw_name[1])
    {
      __blacklist_store_name(&pwtmp->pw_name[1]);
      goto again;
    }
      /*
       * Handle +user entries.
       */
  if ('+' == pwtmp->pw_name[0] 
      && '@' != pwtmp->pw_name[1] 
      && '\0' != pwtmp->pw_name[1])
    {
      __nis_copy_pwd_args(pwtmp, stored_pwd, 0);
      if (NULL == info_nis)
        {
          info_nis = __pwdalloc();
          if (NULL == info_nis)
            return(NULL);
        }
      pwtmp = __nis_getpwnam(pwtmp->pw_name + 1, info_nis);
      if (NULL == pwtmp)
        goto again;
      __nis_copy_pwd_args(stored_pwd, pwtmp, 1);
    }
  
  if (0 == strcmp(pwtmp->pw_name, "+"))
    {
      ypmode = 1;
      __nis_clear_pwd_args(stored_pwd);
      __nis_copy_pwd_args(pwtmp, stored_pwd, 0);
      pwtmp = __nis_getpwent(1, info);
      __nis_copy_pwd_args(stored_pwd, pwtmp, 1);
    }
  if (pwtmp && pwtmp->pw_name && __in_blacklist(pwtmp->pw_name))
    goto again;
  return(pwtmp);
}


/* Search for an entry with a matching name.  */
struct passwd *
_compat_getpwnam(register const char *name)
{
  static void *info_nis = NULL;
  static void *info = NULL;
  static struct passwd *stored_pwd = NULL;
  register FILE *stream;
  register struct passwd *p;
  char *netgroups = NULL;

  if ('-' == name[0] || '+' == name[0])
    return NULL;

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
      if (NULL == stored_pwd)
	stored_pwd = __nis_alloc_pwd_args();
      
          /* Handle -@netgroup entries */
      if (('-' == p->pw_name[0]  || '+' == p->pw_name[0])
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
      if (0 == strcmp(p->pw_name, name))
        break;
    }
  if (netgroups)
    free(netgroups);
  (void) fclose(stream);
  return(p);
}

/* Search for an entry with a matching uid.  */
struct passwd *
_compat_getpwuid(register uid_t uid)
{
  static void *info_nis = NULL;
  static void *info = NULL;
  static struct passwd *stored_pwd = NULL;
  struct passwd *nis_pwd = NULL;
  char *netgroups = NULL;
  int nis_failed = 0;
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
      if (p && p->pw_uid == uid)
        break;
    }
  if (netgroups)
    free(netgroups);
  (void) fclose(stream);
  return(p);
}

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

static struct passwd *
__nis_getpwent(int first, void *info)
{
  static char *nisdomain = NULL;
  char *outkey, *outval;
  int outkeylen, outvallen, status;
  struct passwd *pwptr;
  
  if (1 == __yp_check(NULL))
    {
      if (NULL == nisdomain)
        yp_get_default_domain(&nisdomain);
      if (1 == first)
        {
#if USE_YP_ALL
          if ((time(NULL) - nis_cache.update ) > NIS_CACHE_TIMEOUT )
            {
              struct ypall_callback ypcb;
              nis_cache.current = 0;
              ypcb.foreach = __nis_store_map;
              ypcb.data = NULL;
              status = yp_all(nisdomain, "passwd.byname", &ypcb);
              if (0 != status)
                return NULL;
              nis_cache.update = time(NULL);
            }
          pwptr = __nis_get_first_cached_entry(info);
          return(pwptr);
#endif /* USE_YP_ALL */
          status = yp_first(nisdomain, "passwd.byname",
                            &outkey, &outkeylen,
                            &outval, &outvallen);
          if (0 != status)
            return NULL;
          inkey = outkey;
          inkeylen = outkeylen;
              /* Assumption: No comment lines in NIS maps */
          pwptr = __nis_parsepwddata(outval, info);
          free (outval);
          return (pwptr);
        }
      else
        {
#if USE_YP_ALL
          pwptr = __nis_get_next_cached_entry(info);
          return(pwptr);
#endif /* USE_YP_ALL */
          status = yp_next(nisdomain, "passwd.byname",
                           inkey, inkeylen,
                           &outkey, &outkeylen,
                           &outval, &outvallen);
          if (0 != status)
            {
              free(inkey);
              inkey = NULL;
              return NULL;
            }
          free (inkey);
          inkey = outkey;
          inkeylen = outkeylen;
              /* Assumption: No comment lines in NIS maps */
          pwptr = __nis_parsepwddata(outval, info);
          free (outval);
          return (pwptr);
        }
    }
  return NULL;
}

static struct passwd *
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

static struct passwd *
__netgroup_getpwent(const char *netgr, int first, void *info)
{
  char *host, *user, *domain;
  int status;
  
  if (NULL == nisdomain)
    yp_get_default_domain(&nisdomain);
  
  if (1 == first)
    setnetgrent(netgr);
  
  
  while (1 == (status = getnetgrent(&host, &user, &domain))
         && NULL == user
         && NULL != domain
         && 0 != strcmp(domain, nisdomain))
    ;
  
  if (0 == status || NULL == user)
    {
      endnetgrent();
      netgroupmode = 0;
      __nis_clear_pwd_args(stored_pwd);
      return NULL;
    }
  return __nis_getpwnam(user, info);
}

/*
 * Support routines for remembering -@netgroup and -user entries.
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



/* Return a chunk of memory containing a pre-initialized `pwdread_info'.  */
static void *
__pwdalloc(void)
{
  pwdread_info *info = malloc (sizeof(pwdread_info));
  if (info == NULL)
    return NULL;
  info->buf = NULL;
  info->buflen = 0;
  return info;
}

/* Read a password entry from STREAM, filling in P.  */
static struct passwd *
__pwdread(FILE *stream, void * const p)
{
  register pwdread_info *const info = (pwdread_info *) p;
  char *start, *end;
  int is_nis_entry = 0;
  
  /* Idiocy checks.  */
  if (stream == NULL)
    {
      errno = EINVAL;
      return NULL;
    }

  do
    if (__getline (&info->buf, &info->buflen, stream) == -1)
      return NULL;
  while (info->buf[0] == '#');

  start = info->buf;
  if ('+' == *start || '-' == *start)
    {
      is_nis_entry = 1;
      info->p.pw_passwd = info->p.pw_gecos = NULL;
      info->p.pw_dir = info->p.pw_shell = NULL;
    }
  end = strchr (start, ':');
  info->p.pw_name = start;
  if (end == NULL)
    {
      if (!is_nis_entry)
        return NULL;
      end = strchr(info->p.pw_name, '\n');
      if (NULL != end)
        *end = '\0';
      return &info->p;
    }
  *end = '\0';

  start = end + 1;
  end = strchr (start, ':');
  if (end == NULL)
    return ( is_nis_entry ? &info->p : NULL );
  *end = '\0';
  info->p.pw_passwd = start;
  info->p.pw_uid = (uid_t) strtol (end + 1, &end, 10);
  if (*end != ':')
    return ( is_nis_entry ? &info->p : NULL );
  info->p.pw_gid = (gid_t) strtol (end + 1, &end, 10);
  if (*end != ':')
    return ( is_nis_entry ? &info->p : NULL );

  start = end + 1;
  info->p.pw_gecos = start;
  end = strchr (start, ':');
  if (end == NULL)
    {
      if (!is_nis_entry)
        return NULL;
      end = strchr(info->p.pw_gecos, '\n');
      if (NULL != end)
        *end = '\0';
      return &info->p;
    }
  *end = '\0';

  start = end + 1;
  info->p.pw_dir = start;
  end = strchr (start, ':');
  if (end == NULL)
    {
      if (!is_nis_entry)
        return NULL;
      end = strchr(info->p.pw_dir, '\n');
      if (NULL != end)
        *end = '\0';
      return &info->p;
    }
  *end = '\0';

  start = end + 1;
  info->p.pw_shell = start;
  end = strchr (start, '\n');
  if (end == NULL)
    return ( is_nis_entry ? &info->p : NULL );
  *end = '\0';

  return &info->p;
}

static struct passwd *
__nis_parsepwddata (char *line, void *p)
{
  register pwdread_info *const info = (pwdread_info *)p;
  struct passwd *pw = &info->p;
  char *start, *end;
  register size_t i;
  
  i = strlen(line) + 1;
  if (NULL == info->buf || info->buflen < i)
    {
      start = (NULL == info->buf) ? malloc(i) : realloc(info->buf, i);
      if (NULL == start)
        return NULL;
      info->buf = start;
      info->buflen = i;
    }
  strcpy(info->buf, line);

  start = info->buf ;
  end = strchr(start, ':') ;
  if (end == NULL)
      return NULL ;
  *end = '\0' ;
  pw->pw_name = start ;
  start = end + 1 ;
  end = strchr(start, ':') ;
  if (end == NULL)
      return NULL ;
  *end = '\0' ;
  pw->pw_passwd = start ;
  start = end + 1 ;
  end = strchr(start, ':') ;
  if (end == NULL)
      return NULL ;
  *end = '\0' ;
  pw->pw_uid = atoi(start);
  start = end + 1 ;
  end = strchr(start, ':') ;
  if (end == NULL)
      return NULL ;
  *end = '\0' ;
  pw->pw_gid = atoi(start);
  start = end + 1 ;
  end = strchr(start, ':') ;
  if (end == NULL)
      return NULL ;
  *end = '\0' ;
  pw->pw_gecos = start ;
  start = end + 1 ;
  end = strchr(start, ':') ;
  if (end == NULL)
      return NULL ;
  *end = '\0' ;
  pw->pw_dir = start ;
  start = end + 1 ;
  end = strchr(start, '\n') ;
  if (end != NULL)
    *end = '\0' ;
  pw->pw_shell = start ;
  return pw;
}

static void
__nis_clear_pwd_args(struct passwd *pwd)
{
  pwd->pw_passwd[0] = pwd->pw_gecos[0] = '\0';
  pwd->pw_dir[0] = pwd->pw_shell[0] = '\0';
}

static struct passwd *
__nis_alloc_pwd_args(void)
{
  struct passwd *pwd;

  /* FIXME: no check for return value from malloc() yet */
  pwd = (struct passwd *)malloc(sizeof (struct passwd));
  pwd->pw_passwd = malloc(260);
  pwd->pw_gecos = pwd->pw_passwd + 20;
  pwd->pw_dir = pwd->pw_gecos + 80;
  pwd->pw_shell = pwd->pw_dir + 80;
  __nis_clear_pwd_args(pwd);
  return pwd;
}

/* 
 * copy some fields of a passwd struct
 * if flag == 0 use strcpy
 * else use assignment
 */
static void
__nis_copy_pwd_args(struct passwd *from, struct passwd *to, int flag)
{
  if (NULL != to && NULL != from)
    {
      if (NULL != from->pw_passwd && '\0' != from->pw_passwd[0])
	to->pw_passwd = (flag) ? from->pw_passwd : strcpy(to->pw_passwd, from->pw_passwd);
      if (NULL != from->pw_gecos && '\0' != from->pw_gecos[0])
	to->pw_gecos = (flag) ? from->pw_gecos : strcpy(to->pw_gecos, from->pw_gecos);
      if (NULL != from->pw_dir && '\0' != from->pw_dir[0])
	to->pw_dir = (flag) ? from->pw_dir : strcpy(to->pw_dir, from->pw_dir);
      if (NULL != from->pw_shell && '\0' != from->pw_shell[0])
	to->pw_shell = (flag) ? from->pw_shell : strcpy(to->pw_shell, from->pw_shell);
    }
}


/* Return a new stream open on the password file.  */
FILE *
__pwdopen(void)
{
  return(fopen("/etc/passwd", "r"));
}


#if USE_YP_ALL
static int
__nis_store_map(int instatus, char *inkey, int inkeylen,
                char *inval, int invallen, char *indata)
{
  char *tmp;
  if (YP_TRUE != instatus)
    {
      return 1;
    }
  
      /* first call, setup cache */
  if (0 == nis_cache.size)
    {
      nis_cache.data = malloc(NIS_CACHE_INITIAL_SIZE);
      if (NULL == nis_cache.data)
        return 1;
      nis_cache.size = NIS_CACHE_INITIAL_SIZE;
      nis_cache.current = 0;
    }
  
  if (nis_cache.current + invallen + 2 >= nis_cache.size)
    {
      tmp = realloc(nis_cache.data, nis_cache.size + NIS_CACHE_INCREMENT);
      if (NULL == tmp)
        {
          free(nis_cache.data);
          nis_cache.size = 0;
          return 1;
        }
      nis_cache.size += NIS_CACHE_INCREMENT;
      nis_cache.data = tmp;
    }
  
  strncpy(nis_cache.data + nis_cache.current, inval, invallen);
  tmp = nis_cache.data + nis_cache.current + invallen;
  *tmp++ = '\n';
  *tmp++ = '\0';
  nis_cache.current += invallen + 2 ;
  
  return 0;
}

static struct passwd *
__nis_get_first_cached_entry(void *info)
{
  struct passwd *pwd;
  pwd = __nis_parsepwddata(nis_cache.data, info);
  nis_cache.current = strlen(nis_cache.data) + 1;
  return pwd;
}

static struct passwd *
__nis_get_next_cached_entry(void *info)
{
  struct passwd *pwd;
  pwd = __nis_parsepwddata(nis_cache.data + nis_cache.current, info);
  nis_cache.current += strlen(nis_cache.data + nis_cache.current) + 1;
  return pwd;
}
#endif /* USE_YP_ALL */

#endif /* ENABLE_COMPAT */
