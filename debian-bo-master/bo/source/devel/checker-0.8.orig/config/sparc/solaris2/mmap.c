/* mmap() function.
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
#include <sys/mman.h>
#include <sys/syscall.h>

extern int chkr_errno;
void *chkr_mmap (void *addr, size_t len, int flags, int prot, int fd, off_t offset);

void *
chkr_mmap (void *addr, size_t len, int flags, int prot, int fd, off_t offset)
{
  void *res;
  
  /* This line is *very* important.  */
  prot |= _MAP_NEW;
  
  asm ( "mov %2, %%g1\n\t"
	"mov %3, %%o0\n\t"
	"mov %4, %%o1\n\t"
	"mov %5, %%o2\n\t"
	"mov %6, %%o3\n\t"
	"mov %7, %%o4\n\t"
	"mov %8, %%o5\n\t"
	"ta 8\n\t"
	"bcc,a 1f\n\t"
	"mov %%o0, %0\n\t"
	"mov -1, %0\n\t"
	"mov %%o0, %1\n\t"
	"1:\n\t"
	: "=r" (res), "=r" (chkr_errno)
	: "I" (SYS_mmap), "r" (addr), "r" (len), "r" (flags), "r" (prot), 
	  "r" (fd), "r" (offset)
	: "g1", "o0", "o1", "o2", "o3", "o4", "o5");

  return res;
}
