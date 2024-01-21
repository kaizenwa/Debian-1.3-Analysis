/*
** compat_group.c           COMPAT group map access routines
*/

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

#include "config.h"

#ifdef ENABLE_COMPAT

#include <ansidecl.h>
#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <grp.h>
#include <rpc/rpc.h>
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>

extern ssize_t __getline(char **, size_t *, FILE *);
extern int __yp_check(char **);

static FILE * __grpopen(void);
static void * __grpalloc(void);
static struct group * __grpread(FILE *, void * const);
static struct group * __nis_getgrnam(const char *, char **, void *);
static struct group * __nis_getgrent(int, void *);
static struct group * __nis_getgrgid(gid_t, void *);
static struct group * __nis_parsegroupdata(char *, void *);
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

static FILE *stream = NULL;


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



/* Rewind the stream.  */
void
_compat_setgrent(void)
{
  if (NULL != stream)
    rewind(stream);
  ypmode = 0;
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
_compat_endgrent(void)
{
  if (NULL != stream)
    {
      (void) fclose(stream);
      stream = NULL;
    }
  ypmode = 0;
  if(NULL != inkey)
    {
      free (inkey);
      inkey = NULL;
    }
  blacklist.current = 0;
  if (NULL != blacklist.data)
    blacklist.data[0] = '\0';
}


/* Read an entry from the stream.  */
struct group *
_compat_getgrent(void)
{
  struct group *grp, *grtmp;
  static void *info = NULL;
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
}




/* Search for an entry with a matching group ID.  */
struct group *
_compat_getgrgid(register gid_t gid)
{
  static void *info = NULL;
  register FILE *stream;
  register struct group *g;
  
  if (info == NULL)
    {
      info = __grpalloc();
      if (info == NULL)
        return NULL;
    }
  
  stream = __grpopen();
  if (stream == NULL)
    return NULL;
  
  while ((g = __grpread(stream, info)) != NULL)
    {
          /*
           * Handle +group/-group entries.
           */
      if ('-' == g->gr_name[0] && '\0' != g->gr_name[1])
        {
          g = __nis_getgrnam(g->gr_name + 1, NULL, info);
          if (NULL != g && g->gr_gid == gid)
            return NULL;
          continue;
        }
      if ('+' == g->gr_name[0] && '\0' != g->gr_name[1])
        {
          struct group *grp = g;
          g = __nis_getgrnam(g->gr_name + 1, g->gr_mem, info);
          if (NULL != g && g->gr_gid == gid)
            {
              if (NULL != grp->gr_passwd && '\0' != grp->gr_passwd[0])
                g->gr_passwd = grp->gr_passwd;
              break;
            }
          continue;
        }
      if (0 == strcmp(g->gr_name, "+"))
        {
          g = __nis_getgrgid(gid, info);
          break;
        }
      if ( NULL != g && g->gr_gid == (gid_t) gid)
        break;
    }
  (void) fclose(stream);
  return g;
}


/* Search for an entry with a matching name.  */
struct group *
_compat_getgrnam(register const char *name)
{
  static void *info = NULL;
  register FILE *stream;
  register struct group *g;
  
  if (NULL == info)
    {
      info = __grpalloc();
      if (NULL == info)
        return NULL;
    }
  
  stream = __grpopen();
  if (NULL == stream)
    return NULL;
  
  while ((g = __grpread(stream, info)) != NULL)
    {
          /*
           * Handle +group/-group entries.
           * Bug: gets full entry from NIS, overwriting of some fields
           * not supported (yet). --Swen
           */
      if (('-' == g->gr_name[0]) && (0 == strcmp(g->gr_name + 1, name)))
        return NULL;
      if ('+' == g->gr_name[0] && 0 == strcmp(g->gr_name + 1, name))
        {
          struct group *grp = g;
          g = __nis_getgrnam(g->gr_name + 1, grp->gr_mem, info);
          if (NULL != g && NULL != grp->gr_passwd && '\0' != grp->gr_passwd[0])
            g->gr_passwd = grp->gr_passwd;
          break;
        }
      if (0 == strcmp(g->gr_name, "+"))
        {
          g = __nis_getgrnam(name, NULL, info);
          break;
        }
      if (!strcmp(g->gr_name, name))
        break;
    }
  (void) fclose(stream);
  return(g);
}


/* Return a chunk of memory containing a pre-initialized `grpread_info'.  */
static void *
__grpalloc(void)
{
  grpread_info *info = malloc (sizeof(grpread_info));
  if (info == NULL)
    return NULL;
  
  info->buf = NULL;
  info->buflen = 0;
  
  info->max_members = 5;
  info->members = (char **) malloc (5 * sizeof(char *));
  if (info->members == NULL)
    {
      free (info);
      return NULL;
    }
  
  return info;
}

/* Read a group entry from STREAM, filling in G.  */
static struct group *
__grpread(FILE *stream, void * const g)
{
  register grpread_info *const info = (grpread_info *) g;
  char *start, *end;
  register size_t i;
  int is_nis_entry = 0;
  
      /* Idiocy checks.  */
  if (stream == NULL)
    {
      errno = EINVAL;
      return NULL;
    }
  
  do
    {
      if (__getline (&info->buf, &info->buflen, stream) == -1)
        return NULL;
    } while (info->buf[0] == '#');
  
  start = info->buf;
  if ('+' == *start || '-' == *start)
    {
      is_nis_entry = 1;
      info->g.gr_passwd = NULL;
    }
  end = strchr (start, ':');
  info->g.gr_name = start;
  if (end == NULL)
    {
      if(!is_nis_entry)
        return NULL;
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
        return NULL;
      end = strchr(info->g.gr_passwd, '\n');
      if (NULL != end)
        *end = '\0';
      return &info->g;
    }
  *end = '\0';
  
  info->g.gr_gid = (gid_t) strtol (end + 1, &end, 10);
  if (*end != ':')
    return ( is_nis_entry ? &info->g : NULL );
  
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
            return NULL;
          *end = '\0';
          end = NULL;
        }
      else
        *end = '\0';
      
      if (i == info->max_members - 2)
        {
          info->max_members += 5;
          info->members = (char **)
            realloc (info->members, info->max_members * sizeof (char *));
          if (info->members == NULL)
            return NULL;
        }
      
      info->members[i++] = start;
    } while (end != NULL);
  info->members[i] = NULL;
  info->g.gr_mem = info->members;
  
  return &info->g;
}

/* Return a new stream open on the group file.  */
static FILE *
__grpopen(void)
{
  return fopen("/etc/group", "r");
}

static struct group *
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

static struct group *
__nis_getgrgid(gid_t gid, void *info)
{
  static char *nisdomain = NULL;
  char *outkey;
  int status, outkeylen ;
  struct group *gptr = NULL;
  
  if (1 == __yp_check(NULL))
    {
      char gidbuf[20] ;
      if (NULL == nisdomain)
        yp_get_default_domain(&nisdomain);
      sprintf(gidbuf, "%d", gid) ;
      status = yp_match(nisdomain, "group.bygid",
                        gidbuf, strlen(gidbuf),
                        &outkey, &outkeylen) ;
      if (0 != status)
        return NULL;
      gptr = __nis_parsegroupdata(outkey, info) ;
      free (outkey);
      return (gptr);
    }
  return NULL;
}

struct group *
__nis_getgrnam(const char *name, char **more_members, void *info)
{
  static char *nisdomain = NULL;
  char *outkey, *outtmp, *c;
  int status, outkeylen ;
  struct group *gptr = NULL;
  int i;
    
  if (1 == __yp_check(NULL))
    {
      if (NULL == nisdomain)
        yp_get_default_domain(&nisdomain);
      status = yp_match(nisdomain, "group.byname",
                        name, strlen(name),
                        &outkey, &outkeylen) ;
      if (0 != status)
        return NULL;
      if (NULL != more_members)
        {
          i = strlen(outkey);
          outtmp = malloc(i+1);
          strcpy(outtmp, outkey);
          free(outkey);
          if ('\n' == outtmp[i-1])
            outtmp[i-1] = '\0';
          i = 0;
          c = strrchr(outtmp, ':');
          *(c+1) = '\0';
          while (more_members[i] != NULL)
            {
              outtmp = realloc(outtmp, strlen(outtmp) + strlen(more_members[i]) + 3);
              strcat(outtmp, more_members[i]);
              strcat(outtmp, ",");
              i++;
            }
          *(strrchr(outtmp, ',')) = '\0';
          strcat(outtmp, "\n");
          outkey = outtmp;
        }
      gptr = __nis_parsegroupdata(outkey, info) ;
      free (outkey);
    }
  return (gptr);
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

struct group *
__nis_parsegroupdata(char *line, void *g)
{
  register grpread_info *const info = (grpread_info *)g;
  char *start, *end;
  char **memtmp;
  struct group *grptr = &info->g ;
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
            realloc (info->members, info->max_members * sizeof (char *));
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

#endif /* ENABLE_COMPAT */
