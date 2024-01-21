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

#ifdef YP
#include <time.h>
#define USE_NETGROUP 1
/* Don't define this if you're short of memory */
#define USE_YP_ALL 1
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
extern int __yp_check(char **);
extern struct passwd * __nis_parsepwddata(char *, void *);
extern struct passwd * __nis_getpwnam(const char *, void *);
static struct passwd * __nis_getpwent(int, void *);
#if USE_NETGROUP
extern void setnetgrent(const char *);
extern void endnetgrent(void);
extern int getnetgrent(char **, char **, char **);
extern int innetgr(const char *, char *, char *, char *);
static struct passwd * __netgroup_getpwent(const char *, int, void *);
#endif /* USE_NETGROUP */
static int ypmode = 0;
static int netgroupmode = 0;
static int inkeylen;
static char *inkey = NULL;
#if USE_YP_ALL
#define NIS_CACHE_TIMEOUT 60
/*
 * These values should be adjusted to the actual size of the password
 * file to avoid unnessecary mallocs.
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
#endif /* YP */

static FILE *stream = NULL;

/* Rewind the stream.  */
void
DEFUN_VOID(setpwent)
{
  if (stream != NULL)
    rewind(stream);
#ifdef YP
  ypmode = netgroupmode = 0;
  if(NULL != inkey)
    {
      free (inkey);
      inkey = NULL;
    }
#endif /* YP */
}


/* Close the stream.  */
void
DEFUN_VOID(endpwent)
{
  if (stream != NULL)
    {
      (void) fclose(stream);
      stream = NULL;
    }
#ifdef YP
  ypmode = netgroupmode = 0;
  if(NULL != inkey)
    {
      free (inkey);
      inkey = NULL;
    }
#endif /* YP */
}


/* Return one entry from the password file.  */
struct passwd *
DEFUN_VOID(getpwent)
{
#ifdef YP
  struct passwd *pwtmp;
  static void *info_nis = NULL;
#endif /* YP */
  static PTR info = NULL;
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
#ifdef YP
#if USE_NETGROUP
  if ( 1 == netgroupmode)
    {
      pwtmp = __netgroup_getpwent(NULL, 0, info);
      if (NULL != pwtmp)
        return pwtmp;
    }
#endif /* USE_NETGROUP */
  if ( 1 == ypmode )
    {
      pwtmp = __nis_getpwent(0, info);
      if (NULL != pwtmp)
        return pwtmp;
    }
  
  again:
      /* disable -user entries */
  do
    {
      pwtmp = __pwdread(stream, info);
    }
  while(NULL != pwtmp && '-' == pwtmp->pw_name[0]) ;
  if (NULL == pwtmp)
    return NULL;
#if USE_NETGROUP
      /* Handle +@netgroup entries */
  if ('+' == pwtmp->pw_name[0]
      && '@' == pwtmp->pw_name[1]
      && '\0' != pwtmp->pw_name[2])
    {
      netgroupmode = 1;
      pwtmp = __netgroup_getpwent(&pwtmp->pw_name[2], 1, info);
      if (NULL == pwtmp) 
        {
          netgroupmode = 0;
          goto again;
        }
    }
#endif /* USE_NETGROUP */      
      /*
       * Handle +user entries.
       * Bug: overwriting of pw_passwd field
       *      not supported (yet). --Swen
       */
  if ('+' == pwtmp->pw_name[0] && '\0' != pwtmp->pw_name[1])
    {
      struct passwd *pwd = pwtmp;
      if (NULL == info_nis)
        {
          info_nis = __pwdalloc();
          if (NULL == info_nis)
            return(NULL);
        }
      pwtmp = __nis_getpwnam(pwtmp->pw_name + 1, info_nis);
      if (NULL == pwtmp)
        goto again;
      if (NULL != pwd->pw_gecos && '\0' != *pwd->pw_gecos)
        pwtmp->pw_gecos = pwd->pw_gecos;
      if (NULL != pwd->pw_dir && '\0' != *pwd->pw_dir)
        pwtmp->pw_dir   = pwd->pw_dir  ;
      if (NULL != pwd->pw_shell && '\0' != *pwd->pw_shell)
        pwtmp->pw_shell = pwd->pw_shell;
    }
  
  if (0 == strcmp(pwtmp->pw_name, "+"))
    {
      ypmode = 1;
      pwtmp = __nis_getpwent(1, info);
    }
  
  return(pwtmp);
#else /* !YP */
  return(__pwdread(stream, info));
#endif /* YP */
}

#ifdef YP
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

#if USE_NETGROUP
static struct passwd *
__netgroup_getpwent(const char *netgr, int first, void *info)
{
  static char *nisdomain = NULL;
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
      return NULL;
    }
  return __nis_getpwnam(user, info);
}
#endif /* USE_NETGROUP */

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
#endif /* YP */
