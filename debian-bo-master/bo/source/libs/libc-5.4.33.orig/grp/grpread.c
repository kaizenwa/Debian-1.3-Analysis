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
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <grp.h>

#ifdef YP
/* The ifdef is confusing since __getline is used even when YP is
 * undefined. see line 94. vch */
extern ssize_t __getline(char **, size_t *, FILE *);
extern struct group * __nis_parsegroupdata(char *, void *);
#else		/* added by vch */
extern ssize_t __getline(char **, size_t *, FILE *);
#endif

/* This is the function that all the others are based on.
   The format of the group file is known only here.  */

/* Structure containing info kept by each __grpread caller.  */
typedef struct
{
  char *buf;
  size_t buflen;
  size_t max_members;
  char **members;
  struct group g;
} grpread_info;


/* Return a chunk of memory containing a pre-initialized `grpread_info'.  */
PTR
DEFUN_VOID(__grpalloc)
{
  grpread_info *info = (PTR) malloc (sizeof(grpread_info));
  if (info == NULL)
    return NULL;
  
  info->buf = NULL;
  info->buflen = 0;
  
  info->max_members = 5;
  info->members = (char **) malloc (5 * sizeof(char *));
  if (info->members == NULL)
    {
      free ((PTR) info);
      return NULL;
    }
  
  return info;
}

/* Read a group entry from STREAM, filling in G.  */
struct group *
DEFUN(__grpread, (stream, g), FILE *stream AND PTR CONST g)
{
  register grpread_info *CONST info = (grpread_info *) g;
  char *start, *end;
  register size_t i;
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
    {
      if (__getline (&info->buf, &info->buflen, stream) == -1)
        return NULL;
    } while (info->buf[0] == '#');
  
  start = info->buf;
#ifdef YP
  if ('+' == *start || '-' == *start)
    {
      is_nis_entry = 1;
      info->g.gr_passwd = NULL;
    }
#endif
  end = strchr (start, ':');
  info->g.gr_name = start;
  if (end == NULL)
    {
      if(!is_nis_entry)
#ifdef NO_SKIP_BAD
        return NULL;
#else
	goto restart;
#endif
      end = strchr(info->g.gr_name, '\n');
      if (NULL != end)
        *end = '\0';
      return &info->g;
    }
  *end = '\0';
    
  start = end + 1;
  end = strchr (start, ':');
  info->g.gr_passwd = start;
  if (end == NULL)
    {
      if(!is_nis_entry)
#ifdef NO_SKIP_BAD
        return NULL;
#else
	goto restart;
#endif
      end = strchr(info->g.gr_passwd, '\n');
      if (NULL != end)
        *end = '\0';
      return &info->g;
    }
  *end = '\0';
  
  info->g.gr_gid = (gid_t) strtol (end + 1, &end, 10);
  if (*end != ':')
#ifdef NO_SKIP_BAD
    return ( is_nis_entry ? &info->g : NULL );
#else
    if (is_nis_entry)
      return &info->g;
    else
      goto restart;
#endif
  
  i = 0;
  do
    {
      start = end + 1;
      end = strchr (start, ',');
      if (end == NULL)
        {
          end = strchr (start, '\n');
          if (end == start)
            break;
          if (end == NULL)
#ifdef NO_SKIP_BAD
            return NULL;
#else
	    goto restart;
#endif
          *end = '\0';
          end = NULL;
        }
      else
        *end = '\0';
      
      if (i == info->max_members - 2)
        {
          info->max_members += 5;
          info->members = (char **)
            realloc ((PTR) info->members, info->max_members * sizeof (char *));
          if (info->members == NULL)
#ifdef NO_SKIP_BAD
            return NULL;
#else
	    goto restart;
#endif
        }
      
      info->members[i++] = start;
    } while (end != NULL);
  info->members[i] = NULL;
  info->g.gr_mem = info->members;
  
  return &info->g;
}

#ifdef YP
struct group *
__nis_parsegroupdata(char *line, void *g)
{
  register grpread_info *const info = (grpread_info *)g;
  char *start, *end;
  char **memtmp;
  struct group *grptr = &info->g ;
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
  
  start = info->buf;
  end = strchr (start, ':');
  if (end == NULL)
    return NULL;
  *end = '\0';
  
  grptr->gr_name = start;
  
  start = end + 1;
  end = strchr (start, ':');
  if (end == NULL)
      return NULL;
  *end = '\0';
  grptr->gr_passwd = start;
  
  grptr->gr_gid = (gid_t) strtol (end + 1, &end, 10);
  if (*end != ':')
      return NULL;

  i = 0;
  do
    {
      start = end + 1;
      end = strchr (start, ',');
      if (NULL == end)
        {
          end = strchr (start, '\n');
          if (end == start)
            break;
          if (NULL == end)
              return NULL;
          *end = '\0';
          end = NULL;
        }
      else
        *end = '\0';
      if (i == info->max_members - 2)
        {
          info->max_members += 5;
          memtmp = (char **)
            realloc ((PTR) info->members, info->max_members * sizeof (char *));
          if (NULL == memtmp)
              return NULL;
          info->members = memtmp;
        }
      info->members[i++] = start;
    } while (end != NULL);
  info->members[i] = NULL;
  grptr->gr_mem = info->members;
  
  return grptr;
}
#endif
