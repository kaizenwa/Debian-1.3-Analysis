/* startup for gccchecker.
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

#ifndef USE_BI_JUMP
int startup_call_main (int argc, char *argv[], char *envp[]);
#else
/* The entry point for the user program.  */
int chkr$main (int, char **, char **);
#endif

#include "checker.h"
#include <elf.h>

typedef struct
{
  int	a_type;
  union{
    long a_val;
    void *p_ptr;
    void (*a_fcn)(void);
  } a_un;
} auxv_t;

#ifndef AT_NULL
#define AT_NULL		0
#define AT_IGNORE	1
#define AT_EXECFD	2
#define AT_PHDR		3
#define AT_PHENT	4
#define AT_PHNUM	5
#define AT_PAGESZ	6
#define AT_BASE		7
#define AT_FLAGS       	8
#define AT_ENTRY	9
#endif

extern char **environ;

#define SEND_STDERR(mes) chkr_write(2,mes,strlen(mes))

/*  The assembly name of this function is 'main'.  This means that crt0.o
 * calls this function.  Its purpose is to initialize Checker before running
 * the program.  This function also collects informations about the shared
 * libraries.
 */
int startup (int argc, char *argv[], char *envp[]) asm ("main");

int
startup (int argc, char *argv[], char *envp[])
{
  Elf32_Phdr *phdr;
  int num;
  
  /* Search AT_PHDR. */
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
    phdr = (Elf32_Phdr*) 0;
    num = 0;
    for (auxv = (auxv_t*)tmp; auxv->a_type; auxv++)
      {
        if (auxv->a_type == AT_PHDR)
          phdr = (Elf32_Phdr*)auxv->a_un.a_val;
        if (auxv->a_type == AT_PHNUM)
          num = auxv->a_un.a_val;
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
  
  /* Initialize Checker.  */
  ___chkr_init_chkr (1, num, (char**) phdr, argc, argv, envp);

  /* Run the user program.  A trampoline is used to set the right of the
     args. */
#ifndef USE_BI_JUMP
  return startup_call_main (argc, argv, envp);
#else
  chkr_set_right (&argc, sizeof (argc), CHKR_RW);
  chkr_set_right (&argv, sizeof (argv), CHKR_RW);
  chkr_set_right (&envp, sizeof (envp), CHKR_RW);
  __builtin_jump (chkr$main);
#endif
}
