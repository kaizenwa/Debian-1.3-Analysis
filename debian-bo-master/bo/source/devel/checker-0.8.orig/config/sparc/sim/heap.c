/* Sparc simulator for Checker.
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

#include "instr.h"
#include "checker.h"

#undef calloc
#undef free
#undef malloc
#undef valloc
#undef memalign
#undef realloc
#undef sbrk

#define CALLOC_N 1
#define FREE_N 2
#define MALLOC_N 3
#define VALLOC_N 4
#define MEMALIGN_N 5
#define REALLOC_N 6
#define SBRK_N 7

#define DISP_RIGHT 20
#define SET_DISASSEMBLE 40

extern PTR stub_calloc (size_t nmemb, size_t size);
extern void stub_free (PTR ptr);
extern PTR stub_malloc (size_t real_size);
extern PTR stub_valloc (size_t size);
extern PTR stub_memalign (size_t alignment, size_t size);
extern PTR stub_realloc (PTR ptr, size_t size);
extern PTR stub_sbrk (int incr);
extern int disassemble_level;
extern uint disassemble_flag;

PTR 
calloc (size_t nmemb, size_t size)
{
  PTR res;
  asm (
  	"mov %1, %%o0\n\t"
  	"mov %2, %%o1\n\t"
  	"mov %3, %%g1\n\t"
  	"ta 121\n\t"
  	"mov %%o0, %0\n\t"
  	: "=r" (res)
  	: "r" (nmemb), "r" (size), "I" (CALLOC_N));
  return res;
}
  	
void
free (PTR ptr)
{
  asm (
  	"mov %0, %%o0\n\t"
  	"mov %1, %%g1\n\t"
  	"ta 121\n\t"
  	: /* no output */
  	: "r" (ptr), "I" (FREE_N));
}

PTR
malloc (size_t real_size)
{
  PTR res;
  asm (
  	"mov %1, %%o0\n\t"
  	"mov %2, %%g1\n\t"
  	"ta 121\n\t"
  	"mov %%o0, %0\n\t"
  	: "=r" (res)
  	: "r" (real_size), "I" (MALLOC_N));
  return res;
}

PTR
valloc (size_t size)
{
  PTR res;
  asm (
  	"mov %1, %%o0\n\t"
  	"mov %2, %%g1\n\t"
  	"ta 121\n\t"
  	"mov %%o0, %0\n\t"
  	: "=r" (res)
  	: "r" (size), "I" (VALLOC_N));
  return res;
}

PTR
memalign (size_t alignment, size_t size)
{
  PTR res;
  asm (
  	"mov %1, %%o0\n\t"
  	"mov %2, %%o1\n\t"
  	"mov %3, %%g1\n\t"
  	"ta 121\n\t"
  	"mov %%o0, %0\n\t"
  	: "=r" (res)
  	: "r" (alignment), "r" (size), "I" (MEMALIGN_N));
  return res;
}

PTR
realloc (PTR ptr, size_t size)
{
  PTR res;
  asm (
  	"mov %1, %%o0\n\t"
  	"mov %2, %%o1\n\t"
  	"mov %3, %%g1\n\t"
  	"ta 121\n\t"
  	"mov %%o0, %0\n\t"
  	: "=r" (res)
  	: "r" (ptr), "r" (size), "I" (REALLOC_N));
  return res;
}

PTR
sbrk (int incr)
{
  PTR res;
  asm (
  	"mov %1, %%o0\n\t"
  	"mov %2, %%g1\n\t"
  	"ta 121\n\t"
  	"mov %%o0, %0\n\t"
  	: "=r" (res)
  	: "r" (incr), "I" (SBRK_N));
  return res;
}

void
chkr_disp_right (PTR ptr, int size)
{
  asm (
  	"mov %0, %%o0\n\t"
  	"mov %1, %%o1\n\t"
  	"mov %2, %%g1\n\t"
  	"ta 121\n\t"
  	:
  	: "r" (ptr), "r" (size), "I" (DISP_RIGHT));
}

void
chkr_set_disassemble_level (uint level)
{
  asm (
  	"mov %0, %%o0\n\t"
  	"mov %1, %%g1\n\t"
  	"ta 121\n\t"
  	:
  	: "r" (level), "I" (SET_DISASSEMBLE));
}

void
do_trap_121 (void)
{
  switch (regs[G1])
    {
  case CALLOC_N:
      regs[O0] = (uint) stub_calloc (regs[O0], regs[O1]);
      break;
  case MALLOC_N:
      regs[O0] = (uint) stub_malloc (regs[O0]);
      break;
  case FREE_N:
      stub_free ((PTR)regs[O0]);
      break;
  case REALLOC_N:
      regs[O0] = (uint) stub_realloc ((PTR)regs[O0], regs[O1]);
      break;
  case MEMALIGN_N:
      regs[O0] = (uint) stub_memalign (regs[O0], regs[O1]);
      break;
  case SBRK_N:
      regs[O0] = (uint) stub_sbrk (regs[O0]);
      break;
  case DISP_RIGHT:
      __chkr_disp_right ((PTR) regs[O0], regs[O1]);
      break;
  case SET_DISASSEMBLE:
      if (regs[O0])
        {
          disassemble_level = regs[O0];
          disassemble_flag = 1;
        }
      else
        disassemble_flag = 0;
      break;
  default:
      chkr_abort();
    }
}
