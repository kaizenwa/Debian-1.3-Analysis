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
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include "checker.h"

#if defined(__linux__) || defined(__sun__)
#define d_namlen d_reclen
#define d_fileno d_ino
#define STAT(x,y) lstat((x),(y))
#else
#define STAT(x,y) stat((x),(y))
#endif

/* Get the pathname of the current working directory,
   and put it in SIZE bytes of BUF.  Returns NULL if the
   directory couldn't be determined or SIZE was too small.
   If successful, returns BUF.  In GNU, if BUF is NULL,
   an array is allocated with `malloc'; the array is SIZE
   bytes long, unless SIZE <= 0, in which case it is as
   big as necessary.  */
/* Changes by Tristan Gingold:
   never call malloc(). so getcwd(0,x) returns NULL */

char *
chkr_getcwd(char *buf, size_t size)
{
  static const char dots[]
    = "../../../../../../../../../../../../../../../../../../../../../../../\
../../../../../../../../../../../../../../../../../../../../../../../../../../\
../../../../../../../../../../../../../../../../../../../../../../../../../..";
  const char *dotp, *dotlist;
  size_t dotsize;
  dev_t rootdev, thisdev;
  ino_t rootino, thisino;
  char *path;
  register char *pathp;
  struct stat st;

  if (size == 0 || buf == NULL)
  {
    chkr_errno = EINVAL;
    return NULL;
  }

  path = buf;
	
  pathp = path + size;
  *--pathp = '\0';

  if (STAT (".", &st) < 0)
    {
      return NULL;
    }
  thisdev = st.st_dev;
  thisino = st.st_ino;

  if (STAT ("/", &st) < 0)
    {
      return NULL;
    }
  rootdev = st.st_dev;
  rootino = st.st_ino;

  dotsize = sizeof (dots) - 1;
  dotp = &dots[sizeof (dots)];
  dotlist = dots;
  while (!(thisdev == rootdev && thisino == rootino))
    {
      register DIR *dirstream;
      register struct dirent *d;
      dev_t dotdev;
      ino_t dotino;
      char mount_point;

      /* Look at the parent directory.  */
      if (dotp == dotlist)
	{
	  /* My, what a deep directory tree you have, Grandma.  */
	  return NULL;	/* should never happen */
#if 0	  
	  char *new;
	  if (dotlist == dots)
	    {
	      new = malloc (dotsize * 2 + 1);
	      if (new == NULL)
	        {
		  return NULL;
		}
	      memcpy (new, dots, dotsize);
	    }
	  else
	    {
	      new = realloc ((PTR) dotlist, dotsize * 2 + 1);
	      if (new == NULL)
		goto lose;
	    }
	  memcpy (&new[dotsize], new, dotsize);
	  dotp = &new[dotsize];
	  dotsize *= 2;
	  new[dotsize] = '\0';
	  dotlist = new;
#endif	  
	}

      dotp -= 3;

      /* Figure out if this directory is a mount point.  */
      if (STAT (dotp, &st) < 0)
	goto lose;
      dotdev = st.st_dev;
      dotino = st.st_ino;
      mount_point = dotdev != thisdev;

      /* Search for the last directory.  */
      dirstream = opendir (dotp);
      if (dirstream == NULL)
	goto lose;
      while ((d = readdir (dirstream)) != NULL)
	{
	  if (d->d_name[0] == '.' &&
	      (d->d_namlen == 1 || (d->d_namlen == 2 && d->d_name[1] == '.')))
	    continue;
	    
	  if (mount_point || d->d_fileno == thisino)
	    {
	      char *name = alloca (dotlist + dotsize - dotp +
				     1 + d->d_namlen + 1);
	      memcpy (name, dotp, dotlist + dotsize - dotp);
	      name[dotlist + dotsize - dotp] = '/';
	      memcpy (&name[dotlist + dotsize - dotp + 1],
		      d->d_name, d->d_namlen + 1);
	      if (STAT (name, &st) < 0)
		{
		  (void) closedir (dirstream);
		  goto lose;
		}
	      if (st.st_dev == thisdev && st.st_ino == thisino)
		break;
	    }
	}
      if (d == NULL)
	{
	  (void) closedir (dirstream);
	  goto lose;
	}
      else
	{
	  if (pathp - path < d->d_namlen + 1)
	    {
	      (void) closedir (dirstream);
	      chkr_errno = ERANGE;
	      goto lose;
	    }
	  pathp -= d->d_namlen;
	  (void) memcpy (pathp, d->d_name, d->d_namlen);
	  (void) closedir (dirstream);
	  *--pathp = '/';
	}

      thisdev = dotdev;
      thisino = dotino;
    }

  if (pathp == &path[size - 1])
    *--pathp = '/';

  return memmove (path, pathp, path + size - pathp);

 lose:
  return NULL;
}
