/* Copyright (C) 1991, 1992 Free Software Foundation, Inc.
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
#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <sys/types.h>

#ifndef NO_SHADOW
#include "shadow.h"
#endif

#ifdef YP
extern ssize_t __getline(char **, size_t *, FILE *);
extern struct passwd * __nis_parsepwddata(char *, void *);
extern struct passwd * __nis_alloc_pwd_args(void);
extern void __nis_clear_pwd_args(struct passwd *);
extern void __nis_copy_pwd_args(struct passwd *, struct passwd *, int);
#endif

/* This is the function that all the others are based on.
   The format of the password file is known only here.  */

/* Structure containing info kept by each __pwdread caller.  */
typedef struct
  {
    char *buf;
    size_t buflen;
    struct passwd p;
  } pwdread_info;


/* Return a chunk of memory containing a pre-initialized `pwdread_info'.  */
PTR
DEFUN_VOID(__pwdalloc)
{
  pwdread_info *info = (PTR) malloc (sizeof(pwdread_info));
  if (info == NULL)
    return NULL;
  info->buf = NULL;
  info->buflen = 0;
  return info;
}

/* Read a password entry from STREAM, filling in P.  */
struct passwd *
DEFUN(__pwdread, (stream, p), FILE *stream AND PTR CONST p)
{
  register pwdread_info *CONST info = (pwdread_info *) p;
  char *start, *end;
  int is_nis_entry = 0;
  
  /* Idiocy checks.  */
  if (stream == NULL)
    {
      errno = EINVAL;
      return NULL;
    }

/* We use it to skip a bad entry */
restart:

  do
    if (__getline (&info->buf, &info->buflen, stream) == -1)
      return NULL;
  while (info->buf[0] == '#');

  start = info->buf;
#ifdef YP
  if ('+' == *start || '-' == *start)
    {
      is_nis_entry = 1;
      info->p.pw_passwd = info->p.pw_gecos = NULL;
      info->p.pw_dir = info->p.pw_shell = NULL;
    }
#endif
  end = strchr (start, ':');
  info->p.pw_name = start;
  if (end == NULL)
    {
      if (!is_nis_entry)
#ifdef NO_SKIP_BAD
        return NULL;
#else
	goto restart;
#endif
      end = strchr(info->p.pw_name, '\n');
      if (NULL != end)
        *end = '\0';
      return &info->p;
    }
  *end = '\0';

  start = end + 1;
  end = strchr (start, ':');
  if (end == NULL)
#ifdef NO_SKIP_BAD
    return ( is_nis_entry ? &info->p : NULL );
#else
    if (is_nis_entry)
      return &info->p;
    else
      goto restart;
#endif
  *end = '\0';
  info->p.pw_passwd = start;

#ifndef NO_SHADOW
  {
    struct spwd *spw = getspnam (info->p.pw_name);

    if (spw)
      info->p.pw_passwd = spw->sp_pwdp;
  }
#endif

  info->p.pw_uid = (uid_t) strtol (end + 1, &end, 10);
  if (*end != ':')
#ifdef NO_SKIP_BAD
    return ( is_nis_entry ? &info->p : NULL );
#else
    if (is_nis_entry)
      return &info->p;
    else
      goto restart;
#endif
  info->p.pw_gid = (gid_t) strtol (end + 1, &end, 10);
  if (*end != ':')
#ifdef NO_SKIP_BAD
    return ( is_nis_entry ? &info->p : NULL );
#else
    if (is_nis_entry)
      return &info->p;
    else
      goto restart;
#endif

  start = end + 1;
  info->p.pw_gecos = start;
  end = strchr (start, ':');
  if (end == NULL)
    {
      if (!is_nis_entry)
#ifdef NO_SKIP_BAD
        return NULL;
#else
	goto restart;
#endif
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
#ifdef NO_SKIP_BAD
        return NULL;
#else
	goto restart;
#endif
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
#ifdef NO_SKIP_BAD
    return ( is_nis_entry ? &info->p : NULL );
#else
    if (is_nis_entry)
      return &info->p;
    else
      goto restart;
#endif
  *end = '\0';

  return &info->p;
}

#ifdef YP
struct passwd *
__nis_parsepwddata (char *line, void *p)
{
  register pwdread_info *const info = (pwdread_info *)p;
  struct passwd *pw = &info->p;
  char *start, *end;
  register int i;
  
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

void
__nis_clear_pwd_args(struct passwd *pwd)
{
  pwd->pw_passwd[0] = pwd->pw_gecos[0] = '\0';
  pwd->pw_dir[0] = pwd->pw_shell[0] = '\0';
}

struct passwd *
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
void
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
#endif
