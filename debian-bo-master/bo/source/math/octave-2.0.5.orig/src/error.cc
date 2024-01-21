/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdarg>
#include <cstring>

#include <string>

#include <strstream.h>

#include "defun.h"
#include "error.h"
#include "pager.h"
#include "oct-obj.h"
#include "utils.h"
#include "ov.h"
#include "variables.h"

// TRUE means that Octave will try to beep obnoxiously before printing
// error messages.
static bool Vbeep_on_error;

// Current error state.
int error_state = 0;

// Tell the error handler whether to print messages, or just store
// them for later.  Used for handling errors in eval() and
// the `unwind_protect' statement.
int buffer_error_messages;

// The message buffer
ostrstream *error_message_buffer = 0;

static void
verror (const char *name, const char *fmt, va_list args)
{
  flush_octave_stdout ();

  bool to_beep_or_not_to_beep_p = Vbeep_on_error && ! error_state;

  ostrstream output_buf;

  if (to_beep_or_not_to_beep_p)
    output_buf << "\a";
  if (name)
    output_buf << name << ": ";
  output_buf.vform (fmt, args);
  output_buf << endl << ends;

  char *msg = output_buf.str ();

  if (buffer_error_messages)
    {
      char *ptr = msg;

      if (! error_message_buffer)
	{
	  error_message_buffer = new ostrstream;

	  // XXX FIXME XXX -- this is ugly, but it prevents
	  //
	  //   eval ("error (\"msg\")", "error (__error_text__)");
	  //
	  // from printing `error: ' twice.  Assumes that the NAME we
	  // have been given doesn't contain `:'.

	  ptr = strchr (msg, ':') + 2;
	  ptr = ptr ? ptr : msg;	  
	}

      *error_message_buffer << ptr;
    }
  else
    {
      octave_diary << msg;
      cerr << msg;
    }

  delete [] msg;
}

// Note that we don't actually print any message if the error string
// is just "" or "\n".  This allows error ("") and error ("\n") to
// just set the error state.

static void
error_1 (const char *name, const char *fmt, va_list args)
{
  if (error_state != -2)
    {
      if (fmt)
	{
	  if (*fmt)
	    {
	      int len = strlen (fmt);
	      if (fmt[len - 1] == '\n')
		{
		  if (len > 1)
		    {
		      char *tmp_fmt = strsave (fmt);
		      tmp_fmt[len - 1] = '\0';
		      verror (name, tmp_fmt, args);
		      delete [] tmp_fmt;
		    }

		  error_state = -2;
		}
	      else
		verror (name, fmt, args);
	    }
	}
      else
	panic ("error_1: invalid format");

      if (! error_state)
	error_state = 1;
    }
}

void
message (const char *name, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror (name, fmt, args);
  va_end (args);
}

void
usage (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_state = -1;
  verror ("usage", fmt, args);
  va_end (args);
}

void
warning (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror ("warning", fmt, args);
  va_end (args);
}

void
error (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_1 ("error", fmt, args);
  va_end (args);
}

void
parse_error (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_1 (0, fmt, args);
  va_end (args);
}

void
panic (const char *fmt, ...)
{
  flush_octave_stdout ();

  va_list args;
  va_start (args, fmt);
  verror ("panic", fmt, args);
  va_end (args);
  abort ();
}

typedef void (*error_fun)(const char *, ...);

extern octave_value_list Fsprintf (const octave_value_list&, int);

static octave_value_list
handle_message (error_fun f, const char *msg, const octave_value_list& args)
{
  octave_value_list retval;

  string tstr;

  int nargin = args.length ();

  if (nargin > 0)
    {
      octave_value arg = ((nargin > 1) ? Fsprintf (args, 1) : args) (0);

      if (arg.is_defined ())
	{
	  if (arg.is_string ())
	    {
	      tstr = arg.string_value ();
	      msg = tstr.c_str ();
	      
	      if (! msg)
		return retval;
	    }
	  else if (arg.is_empty ())
	    return retval;
	}
    }

// Ugh.

  int len = strlen (msg);
  if (msg[len - 1] == '\n')
    {
      if (len > 1)
	{
	  char *tmp_msg = strsave (msg);
	  tmp_msg[len - 1] = '\0';
	  f ("%s\n", tmp_msg);
	  delete [] tmp_msg;
	}
    }
  else
    f ("%s", msg);

  return retval;
}

DEFUN (error, args, ,
  "error (FMT, ...): print message according to FMT and set error state.\n\
\n\
This should eventually take us up to the top level, possibly\n\
printing traceback messages as we go.\n\
\n\
If the resulting error message ends in a newline character, traceback
messages are not printed.\n\
\n\
See also: printf") 
{
  return handle_message (error, "unspecified error", args);
}

DEFUN (warning, args, ,
  "warning (FMT, ...): print a warning message according to FMT.\n\
\n\
See also: error, printf")
{
  return handle_message (warning, "unspecified warning", args);
}

DEFUN (usage, args, ,
  "usage (FMT, ...): print a usage message according to FMT.\n\
\n\
See also: error, printf")
{
  return handle_message (usage, "unknown", args);
}

static int
beep_on_error (void)
{
  Vbeep_on_error = check_preference ("beep_on_error");

  return 0;
}

void
symbols_of_error (void)
{
  DEFVAR (beep_on_error, 0.0, 0, beep_on_error,
    "if true, beep before printing error messages");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
