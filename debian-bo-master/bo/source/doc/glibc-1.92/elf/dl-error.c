/* Error handling for runtime dynamic linker.
Copyright (C) 1995, 1996 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <stddef.h>
#include <link.h>
#include <setjmp.h>
#include <string.h>

/* This structure communicates state between _dl_catch_error and
   _dl_signal_error.  */
struct catch
  {
    const char *errstring, *objname; /* Error detail filled in here.  */
    jmp_buf env;		/* longjmp here on error.  */
  };

/* This points to such a structure during a call to _dl_catch_error.
   During implicit startup and run-time work for needed shared libraries,
   this is null.  */
static struct catch *catch;


void
_dl_signal_error (int errcode,
		  const char *objname,
		  const char *errstring)
{
  if (! errstring)
    errstring = "DYNAMIC LINKER BUG!!!";

  if (catch)
    {
      /* We are inside _dl_catch_error.  Return to it.  */
      catch->errstring = errstring;
      catch->objname = objname;
      longjmp (catch->env, errcode ?: -1);
    }
  else
    {
      /* Lossage while resolving the program's own symbols is always fatal.  */
      extern char **_dl_argv;	/* Set in rtld.c at startup.  */
      _dl_sysdep_fatal (_dl_argv[0] ?: "<program name unknown>",
			": error in loading shared libraries\n",
			objname ?: "", objname ? ": " : "",
			errstring, errcode ? ": " : "",
			errcode ? strerror (errcode) : "", "\n", NULL);
    }
}

int
_dl_catch_error (const char **errstring,
		 const char **objname,
		 void (*operate) (void))
{
  int errcode;
  struct catch c = { errstring: NULL, objname: NULL };

  errcode = setjmp (c.env);
  if (errcode == 0)
    {
      catch = &c;
      (*operate) ();
      catch = NULL;
      *errstring = *objname = NULL;
      return 0;
    }

  /* We get here only if we longjmp'd out of OPERATE.  */
  *errstring = c.errstring;
  *objname = c.objname;
  return errcode == -1 ? 0 : errcode;
}
