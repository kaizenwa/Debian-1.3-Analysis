/* generic.c Functions to handle the symbol table of an unknown exec file.
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
#include "machine.h"
#include "message.h"

/* Load the symbols in memory.  No symbols to handle.  */
void
chkr_load_symtab (void)
{
  return;
}

void
chkr_show_addr (PTR ptr)
{
  chkr_printf (M_PC__NOSYMTAB, ptr);
}

int
same_history (char **funcs, int nbr_funcs)
{
  return 0;
}

/* Show all the symbol table.  Nothing to do.  */
void
__chkr_dump_symtab (void)
{
  return;
}

/* Initialize the objects structure: seek etext, edata, end.
 * If LINKED is true, ETEXT are good for the program
 * eg: if checker is a module, ETEXT is for checker, not for your program.
 */
extern etext (void);
extern char *edata, *end;
void
init_main_object (int linked, char *argv0, int nlibs, char **libs)
{
  objects = (struct object*) sys_malloc (2 * sizeof (struct object));
  
  /* The main program.  */
  objects[0].path = chkr_prog_path ? chkr_prog_path : "Main program";
  objects[0].name = ".text";
  objects[0].org = 0;		/* bad */
  objects[0].end = (uint)&etext;
  objects[0].rights = OBJECT_TEXT | OBJECT_READ | OBJECT_EXEC;
  objects[0].next = &objects[1];
  
  objects[1].path = objects[0].path;
  objects[1].name = ".data";
  objects[1].org = (uint)&etext;	/* bad */
  objects[1].end = (uint)&end;
  objects[1].rights = OBJECT_DATA | OBJECT_READ | OBJECT_WRIT;
  objects[1].next = (struct object*)0;
}
