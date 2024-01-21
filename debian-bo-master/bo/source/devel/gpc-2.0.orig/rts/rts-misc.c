/* Copyright (C) 1991 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Miscallenous routines for extended pascal support

The GNU Pascal Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU Pascal Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU Pascal Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include "rts.h"

/* Extended Pascal HALT procedure for terminating the program.
   Gpc extension: return the VALUE as the exit status of the program.
   		  This may not work for systems other than UN*X */
void
_p_halt (value)
int value;
{
  _p_warning ("Call to predefined procedure `Halt'");
  
  /* Remove this */
  if (Gpc_debug != 123456789)
    exit(value);
}

/* function return FALSE if the requested parameter could not be accessed;
 * Program exits if the command line arg does not fit in the string STR.
 * Return TRUE if the var parameter STR contains the requested arg.
 */
int
_p_paramstr (num, str)
     int num;
     STRING *str;
{
  int length;

  if (num < 0 || num >= Gpc_argc)
    return FALSE;
  
  length = strlen (Gpc_argv[ num ]);

  if (str->Capacity < length)
    {
      _p_error (ABORT,
		"Command line argument %d does not fit in the string parameter",
		num);
      return FALSE;
    }

  str->length = length;
  strncpy (str->string, Gpc_argv [ num ], length);
  return TRUE;
}

/* Return the number of arguments passed to the program */
int
_p_paramcount ()
{
  return Gpc_argc;
}

/* This is called by the compiler generated code from pascal module
 * initializers, each pass their own ADDRESS and a RUN_ID as argument.
 *
 * The constructor with the smallest RUN_ID will be first in the list.
 * If two constructors have same run id, the one that called this first
 * is called before.
 *
 * Negative RUN_ID's are reserved for Pascal internal use.
 */
void
_p_collect (fun, run_id)
     void (* fun)();
     int run_id;
{
  CONSTRUCTOR *scan;
  CONSTRUCTOR *c = (CONSTRUCTOR *)_p_malloc(sizeof(CONSTRUCTOR));

  /* Gpc_debug is not yet set, so no debugging info is printed... */
  D(1, fprintf (stderr, "Collecting constructor at 0x%lx with run id %d\n",
		(long)fun, run_id));

  if (! c)
    _p_error(ABORT, "Could not allocate storage for constructor 0x%lx", fun);

  c->fun    = fun;
  c->run_id = run_id;
  c->next   = (CONSTRUCTOR *)NULL;

  if (! Gpc_c_list || run_id < Gpc_c_list->run_id)
    {
      /* Make this the first constructor */
      c->next = Gpc_c_list;
      Gpc_c_list = c;
    }
  else
    for (scan = Gpc_c_list; scan; scan = scan->next)
      {
	if (fun == scan->fun)
	  _p_error (ABORT, "Constructor at 0x%lx already in the list", fun);

	if (scan->run_id > run_id || !scan->next)
	  { /* Append to current node */
	    c->next = scan->next;
	    scan->next = c;
	    break;
	  }
      }
}

/* Run the constructors collected and sorted to Gpc_c_list
 */
void
_p_run_constructors ()
{
  CONSTRUCTOR *scan;
  int count = 0;

  _p_collect_flag = 0;

  for (scan = Gpc_c_list; scan; scan = scan->next, count++)
    {
      D(1, fprintf (stderr, "Running constructor %d at 0x%lx with run id %d\n",
		    count, (long)scan->fun, scan->run_id));
      (*(scan->fun))();
    }
}
