/* startup.
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
void ___chkr_init_chkr (int linked, int nlibs, char **libs,
		  int argc, char *argv[], char *argp[]);
		  
#include "checker.h"
#include <sys/frame.h>
#include <elf.h>
#include <sys/auxv.h>
#include <link.h>

extern char **environ;


#if 0
/* Switch to the .init section.  cc1 is not aware of this. */
asm(".section \".init\"\n");
#endif

#define SEND_STDERR(mes) chkr_write(2,mes,strlen(mes))

/* Such portable code is nice... */
#if defined(__PIC__)
#define MAGIC_ADD 0
#else
#define MAGIC_ADD 32
#endif

#ifdef GCCCHECKER
void startup (int argc, char *argv[], char *envp[]) asm ("main");
#define STARTUP_ARGS int argc, char *argv[], char *envp[]
int chkr$main (int, char **, char **);
#endif

#ifdef SIMCHECKER
#define STARTUP_ARGS void
extern void run_sim (void);
#endif

void
startup (STARTUP_ARGS)
{
  Elf32_Phdr *phdr;
#ifdef SIMCHECKER
  struct frame *fp;
  struct frame *frames;
  char **argv;
  char **envp;
  int argc;
#endif
  
#if 0
  chkr_printf ("enter in .init\n");
#endif

#ifdef SIMCHECKER
  /* Save the frames.  Do not call raw_save_frames() because it can be
   * defined for the simulator. */
  asm ("ta 0x3\n"
       "\tmov %%sp, %0" : "=r" (frames));
      
  /* Seek the first frame. */
  fp = frames->fr_savfp;
  while (fp->fr_savfp)
    fp = fp->fr_savfp;
  
  /* These magic numbers can be found in gcc-2.x.x/config/sparc/sol2-c1.asm */
  argc = * ((int*) ((char*)fp + MAGIC_ADD + 64));
  argv = (char**) ((char*)fp + MAGIC_ADD + 68);
  envp = argv + argc + 1;
#endif

#if 0  
  chkr_printf ("fp: 0x%08x\n", fp);
#endif
  
  /* Search AT_PHDR */
  {
    int *tmp;
    auxv_t *auxv;
    
    tmp = (int*)argv;
    /* Skip over the argv pointers. */
    while (*tmp)
      tmp++;
    /* Skip the null */
    tmp++;
    /* Skip over the envp pointers. */
    while (*tmp)
      tmp++;
    /* Skip the null. */
    tmp++;
    phdr = (Elf32_Phdr*)0;
    for (auxv = (auxv_t*)tmp; auxv->a_type; auxv++)
      if (auxv->a_type == AT_PHDR)
        {
          phdr = (Elf32_Phdr*)auxv->a_un.a_val;
          break;
        }
#if 0
    chkr_printf ("auxv: 0x%08x\n", auxv);
    chkr_printf ("phdr: 0x%08x\n", phdr);
#endif

  }
  
  /* Initialize the environ */
  environ = envp;
  
#if 0
  chkr_printf("Call initialize\n");
#endif
  
  /* Initialize Checker */
  ___chkr_init_chkr (1, 0, (char**) phdr, argc, argv, envp);

#if 0
  chkr_printf ("Leave .init\n");
#endif

#ifdef SIMCHECKER
  /* Run the simulator.  It does not really return: after this call, all the
   *  instructions are run under the simulator.
   */
  run_sim ();
#endif
#ifdef GCCCHECKER
  chkr$main (argc, argv, envp);
#endif
}

#ifdef SIMCHECKER
#ifdef __PIC__
asm(".section \".init\"\n"
    "\t.type _init,#function\n"
    "\t.globl _init\n"
    "_init:\tsave %sp, -96, %sp\n\t"
#if 0
    "mov 4, %g1\n\t"
    "mov 2, %o0\n\t"
    "mov 123, %o1\n\t"
    "mov 0, %o2\n\t"
    "ta 8\n\t"
#endif
    "call startup\n"
    "\tnop\n"
    "\tret\n"
    "\trestore\n"
    "\t.size _init,.-_init\n");
    
asm(".section \".fini\"\n"
    "\t.type _fini,#function\n"
    "\t.globl _fini\n"
    "_fini:\tsave %sp, -96, %sp\n"
    "call chkr_do_end\n"
    "\tnop\n"
    "\tret\n"
    "\trestore\n"
    "\t.size _fini,.-_fini\n");
#else
/* GCC is too bugged to handle .init section.  So use a ctor.
 * It is a bad story. */
asm(".section \".ctors\",#alloc,#execinstr");
asm(".word startup");
#endif
#endif
