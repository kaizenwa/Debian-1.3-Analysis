/* Copyright (C) 1995 Free Software Foundation, Inc.
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
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

/* Return the pathname of the terminal FD is open on, or NULL on errors.
   The returned storage is good only until the next call to this function.  */
char *ttyname_r (	int, char *, size_t );

char *
DEFUN(ttyname_r, (fd, name, namelen),
	int fd AND char *name AND size_t namelen)
{
  static CONST char dev[] = "/dev";
  struct stat st;
  dev_t mydev;
  ino_t myino;
  DIR *dirstream;
  struct dirent *d;
  int save = errno;
  int d_namlen;

  if (!name || !namelen) return NULL;

  if (isatty (fd) && fstat (fd, &st) < 0)
    return NULL;
  mydev = st.st_dev;
  myino = st.st_ino;

  dirstream = opendir (dev);
  if (dirstream == NULL)
    return NULL;

  (void) memcpy (name, dev, sizeof (dev) - 1);
  name[sizeof (dev) - 1] = '/';
  
  while ((d = readdir (dirstream)) != NULL)
    if (d->d_ino == myino)
      {
	d_namlen = strlen (d->d_name) + 1;
	/* Do we need this? */
	if (sizeof (dev) + d_namlen > namelen)
	  {
	    return NULL;
	  }
	(void) memcpy (&name[sizeof (dev)], d->d_name, d_namlen);
	if (stat (name, &st) == 0 && st.st_dev == mydev)
	  {
	    (void) closedir (dirstream);
	    errno = save;
	    return name;
	  }
      }

  (void) closedir (dirstream);
  errno = save;
  return NULL;
}
