/* Disp a memory map of a /proc/file.
   Copyright 1995 Tristan Gingold
		  Written Juny 1995 by Tristan Gingold

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

#include <fcntl.h>
#include <sys/types.h>
#include <sys/procfs.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int
main(int argc, char *argv[])
{
 int fd, fd1;
 prmap_t *prmap;
 struct stat dev_zero;
 struct stat stat_fd;
 int n;
 int i;
 
 if (argc != 2)
   {
     printf("Usage: %s /proc/file\n", argv[0]);
     exit(2);
   }
 stat("/dev/zero", &dev_zero);
 fd = open(argv[1], O_RDONLY);
 if (fd == -1)
   {
     perror("open");
     exit(1);
   }
 ioctl(fd, PIOCNMAP, &n);
 prmap = (prmap_t*)malloc(n * sizeof(prmap_t));
 ioctl(fd, PIOCMAP, prmap);
 for (i = 0; i < n; i++)
   {
     fd1 = ioctl(fd, PIOCOPENM, &(prmap[i].pr_vaddr));
     if (fd1 == -1)
       {
         stat_fd.st_ino = 0;
         stat_fd.st_dev = 0;
       }
     else
       {
         fstat(fd1, &stat_fd);
         close(fd1);
       }
     printf("0x%08x-0x%08x %c%c%c%c%c%c inode:%d dev:%d %s\n",
     	prmap[i].pr_vaddr,
     	prmap[i].pr_vaddr + prmap[i].pr_size,
     	prmap[i].pr_mflags & MA_READ ? 'r' : '-',
     	prmap[i].pr_mflags & MA_WRITE ? 'w' : '-',
     	prmap[i].pr_mflags & MA_EXEC ? 'x' : '-',
	prmap[i].pr_mflags & MA_SHARED ? 's' : 'p',
	prmap[i].pr_mflags & MA_BREAK ? 'b' : '-',
	prmap[i].pr_mflags & MA_STACK ? 's' : '-',
	stat_fd.st_ino,
	stat_fd.st_dev,
	stat_fd.st_ino == dev_zero.st_ino ? "(/dev/zero)" : "");
   }
 exit(0);
}

