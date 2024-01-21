/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2, or (at your option)
     any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <McTools/McInfoRequest.h>
#include "mkdirhier.h"

#define DEBUG_MKDIRHIER

/*
 * mkdirhier(const char *path)
 *
 * create a complete directory hirarchy to hold file specified by path
 *
 * On success, zero is returned.
 * On failure -1 is returned, errno is set accordingly and *path is
 * modified to reflect the place of failure.
 *
 */
int mkdirhier(char *path) {
  char *p, *p2;
  struct stat st;
  int umsk;

  if (!path || !strlen(path)) {
    errno=EINVAL;
    return -1;
  }

#ifdef DEBUG_MKDIRHIER
  printf("mkdirhier(\"%s\")\n", path);
#endif

  /* First check if the directory already exists. */
  if ((p=strrchr(path, '/'))) {
    *p=0;
    if (!stat(path, &st)) {
      if ((st.st_mode&S_IFMT)==S_IFDIR) {
	*p='/';
	return 0;
      }
    }
    *p='/';
  }

  /* If not, scan each path component and try to create it if it is
   * missing.
   * Be sure to use the users umask for directory creation.
   */

  umsk = umask(077); umask(umsk); /* There should be a way to read it
				   * without changing it. */

  if (*path=='/') p2=path+1; else p2=path;

  while ((p=strchr(p2, '/'))) {
    *p=0;
    if (stat(path, &st)) {
#ifdef DEBUG_MKDIRHIER
      printf("mkdir(\"%s\")\n", path);
#endif
      if (mkdir(path, 0755 & (~umsk))) return -1;
    } else {
      if ((st.st_mode&S_IFMT)!=S_IFDIR) {
	errno=ENOTDIR;
	return -1;
      }
    }
    *p='/';
    p2=p+1;
  }

  return 0;
}





