/* readdir() function.
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
#define getdents(a,b,c) chkr_getdents(a,b,c)
#include <sys/types.h>
#include <sys/dirent.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>      /* for constant O_RDONLY */
#include <errno.h>
#include <limits.h>

extern int chkr_errno;

struct dirent *
readdir (DIR * dir)
{
  int result;
  int count = MAXNAMLEN;
  struct dirent *de;

  if (!dir)
    {
      chkr_errno = EBADF;
      return NULL; 
    }

  if (dir->dd_size <= dir->dd_loc)
    {
      result = getdents (dir->dd_fd, (struct dirent*)dir->dd_buf, count);
      if (result <= 0)
        {
          chkr_errno = -result;
          return NULL;
        }

    dir->dd_size = result;
    dir->dd_loc = 0;
  } 

  de = (struct dirent*)(((char*)dir->dd_buf) + dir->dd_loc);
  
  /* d_reclen seems better than d_off.  FIXME.  Solaris2.4 bug ?? */
  dir->dd_loc += de->d_reclen; /* de->d_off; */
  
  /* Very stange... */
  de->d_reclen = strlen (de->d_name);
  
  return de;
}
