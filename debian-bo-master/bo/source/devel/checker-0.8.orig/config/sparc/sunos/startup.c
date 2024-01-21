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

#include <sys/types.h>
#include <link.h>
#include "checker.h"

/* The entry point for the user program.  */
int chkr$main (int, char **, char **);
		  
extern char **environ;

extern struct link_dynamic _DYNAMIC;

/*  The assembly name of this function is 'main'.  This means that crt0.o
 * calls this function.  Its purpose is to initialize Checker before running
 * the program.  This function also collects informations about the shared
 * libraries.
 */
int startup (int argc, char *argv[], char *envp[]) asm ("_main");

int
startup (int argc, char *argv[], char *envp[])
{
  int nbr_libs;
  void *libs;
  
#if 0
  chkr_printf("Call initialize\n");
#endif
  
  /* Initialize the environ.  */
  environ = envp;

  /* Handle shared libraries.  */
  nbr_libs = 0;
  if (&_DYNAMIC)
    {
      struct link_map *lm;
      struct link_dynamic_2 *v2;
      
      v2 = _DYNAMIC.ld_un.ld_2;
      if (v2)
        for (lm = v2->ld_loaded; lm; lm = lm->lm_next)
          nbr_libs++;
      libs = v2->ld_loaded;
    }
    
  /* Initialize Checker.  */
  ___chkr_init_chkr (1, nbr_libs, libs, argc, argv, envp);
  
  /* Run the user program.  */
  return chkr$main (argc, argv, envp);
}
