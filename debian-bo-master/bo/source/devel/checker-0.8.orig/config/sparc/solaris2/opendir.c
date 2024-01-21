/* opendir() function.
   Copyright 1995 Tristan Gingold
		  Written June 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "checker.h"
#include <sys/types.h>
#include <sys/dirent.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <errno.h>
#include <unistd.h>

/*
 * opendir just makes an open() call - it return NULL if it fails
 * (open sets errno), otherwise it returns a DIR * pointer.
 */
DIR *
opendir (const char * name)
{
  int fd;
  struct stat statbuf;
  struct dirent *buf;
  DIR *ptr;

  if (stat (name, &statbuf))
    return NULL;
  if (!S_ISDIR (statbuf.st_mode))
    {
      chkr_errno = ENOTDIR;
      return NULL;
    }
  if ((fd = open (name,O_RDONLY)) < 0)
    return NULL;
    
  /* According to POSIX, directory streams should be closed when
   * exec. From "Anna Pluzhnikov" <besp@midway.uchicago.edu>.
   */
  if (fcntl (fd, F_SETFD, FD_CLOEXEC) < 0)
    return NULL;
    
  if (!(ptr = sys_malloc (sizeof (*ptr))))
    {
      close (fd);
      chkr_errno = ENOMEM;
      return NULL;
    }
    
  if (!(buf = sys_malloc (MAXNAMLEN)))
    {
      close (fd);
      sys_free (ptr);
      chkr_errno = ENOMEM;
      return NULL;
    }
    
  ptr->dd_fd = fd;
  ptr->dd_loc = ptr->dd_size = 0;
  ptr->dd_buf = (char*)buf;
  return ptr;
}
