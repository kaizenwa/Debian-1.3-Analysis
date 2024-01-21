/* error.c -- error handler for noninteractive utilities
   Copyright (C) 1990, 91, 92, 93, 94, 95, 96 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* Written by David MacKenzie <djm@gnu.ai.mit.edu>.
 * Heavily modified by Dave Bodenstab and ESR.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <errno.h>
#if defined(HAVE_SYSLOG)
#include <syslog.h>
#endif
#if defined(HAVE_ALLOCA_H)
#include <alloca.h>
#endif

#if HAVE_VPRINTF || HAVE_DOPRNT || _LIBC
# if __STDC__
#  include <stdarg.h>
#  define VA_START(args, lastarg) va_start(args, lastarg)
# else
#  include <varargs.h>
#  define VA_START(args, lastarg) va_start(args)
# endif
#else
# define va_alist a1, a2, a3, a4, a5, a6, a7, a8
# define va_dcl char *a1, *a2, *a3, *a4, *a5, *a6, *a7, *a8;
#endif

#if STDC_HEADERS || _LIBC
# include <stdlib.h>
# include <string.h>
#else
void exit ();
#endif

#include "fetchmail.h"

#ifndef _
# define _(String) String
#endif

/* If NULL, error will flush stdout, then print on stderr the program
   name, a colon and a space.  Otherwise, error will call this
   function without parameters instead.  */
void (*error_print_progname) (
#if __STDC__ - 0
			      void
#endif
			      );

/* Used by error_build() and error_complete() to accumulate partial messages.  */
static unsigned int partial_message_size = 0;
static unsigned int partial_message_size_used = 0;
static char *partial_message;
static unsigned use_stderr;

/* This variable is incremented each time `error' is called.  */
unsigned int error_message_count;

#ifdef _LIBC
/* In the GNU C library, there is a predefined variable for this.  */

# define program_name program_invocation_name
# include <errno.h>

#else

/* The calling program should define program_name and set it to the
   name of the executing program.  */
extern char *program_name;

# if HAVE_STRERROR
#  ifndef strerror		/* On some systems, strerror is a macro */
char *strerror ();
#  endif
# else
static char *
private_strerror (errnum)
     int errnum;
{
  extern char *sys_errlist[];
  extern int sys_nerr;

  if (errnum > 0 && errnum <= sys_nerr)
    return sys_errlist[errnum];
  return _("Unknown system error");
}
#  define strerror private_strerror
# endif	/* HAVE_STRERROR */
#endif	/* _LIBC */

/* Print the program name and error message MESSAGE, which is a printf-style
   format string with optional args.
   If ERRNUM is nonzero, print its corresponding system error message.
   Exit with status STATUS if it is nonzero.  */
/* VARARGS */

#if !defined(HAVE_VSYSLOG) && !defined(VA_START)
int vsyslog(priority, message, va_alist)
int priority;
char *message;
va_dcl
{
    va_list args;
  
    char *string;
  
    string = (char *)malloc(LINELEN);
 
    va_start(args);
    vsprintf(string, message, args);
    va_end(args);
 
    syslog(priority, string);
    free(string);
}
#endif

void
#if defined(VA_START) && __STDC__
error (int status, int errnum, const char *message, ...)
#else
error (status, errnum, message, va_alist)
     int status;
     int errnum;
     char *message;
     va_dcl
#endif
{
#ifdef VA_START
  va_list args;
#endif

  /* If a partially built message exists, print it now so it's not lost.  */
  if (partial_message_size_used != 0)
    {
      partial_message_size_used = 0;
      error (0, 0, "%s (message incomplete)", partial_message);
    }

#if defined(HAVE_SYSLOG)
  if (use_syslog)
    {
      int priority;

# ifdef VA_START
      VA_START (args, message);
# endif
      priority = status? LOG_ALERT : errnum? LOG_ERR : LOG_INFO;

      if (errnum)
        {
	  char *msg = alloca (strlen (message) + 5);

	  strcpy (msg, message);
	  strcat (msg, ": %m");

	  errno = errnum;
# ifdef VA_START
	  vsyslog (priority, msg, args);
	  va_end (args);
# else
	  syslog (priority, msg, a1, a2, a3, a4, a5, a6, a7, a8);
# endif
	}
      else
        {
# ifdef VA_START
	  vsyslog (priority, message, args);
	  va_end (args);
# else
	  syslog (priority, message, a1, a2, a3, a4, a5, a6, a7, a8);
# endif
	}
    }
  else
#endif
    {
      if (error_print_progname)
	(*error_print_progname) ();
      else
	{
	  fflush (stdout);
	  if ( *message == '\n' )
	    {
	      fputc( '\n', stderr );
	      ++message;
	    }
	  fprintf (stderr, "%s: ", program_name);
	}

#ifdef VA_START
      VA_START (args, message);
# if HAVE_VPRINTF || _LIBC
      vfprintf (stderr, message, args);
# else
      _doprnt (message, args, stderr);
# endif
      va_end (args);
#else
      fprintf (stderr, message, a1, a2, a3, a4, a5, a6, a7, a8);
#endif

      if (errnum)
	fprintf (stderr, ": %s", strerror (errnum));
      putc ('\n', stderr);
      fflush (stderr);
    }
  ++error_message_count;
  if (status)
    exit (status);
}

/*
 * Calling error_init(TRUE) causes error_build and error_complete to write
 * to stderr without buffering.  This is needed for the ticker dots to
 * work correctly.
 */
void error_init(foreground)
int foreground;
{
    use_stderr = foreground;
}

/* Build an error message by appending MESSAGE, which is a printf-style
   format string with optional args, to the existing error message (which may
   be empty.)  The completed error message is finally printed (and reset to
   empty) by calling error_complete().
   If an intervening call to error() occurs when a partially constructed
   message exists, then, in an attempt to keep the messages in their proper
   sequence, the partial message will be printed as-is (with a trailing newline)
   before error() prints its message.
/* VARARGS */

void
#if defined(VA_START) && __STDC__
error_build (const char *message, ...)
#else
error_build (message, va_alist)
     char *message;
     va_dcl
#endif
{
#ifdef VA_START
  va_list args;
  int n;
#endif

  /* Make an initial guess for the size of any single message fragment.  */
  if (partial_message_size == 0)
    {
      partial_message_size_used = 0;
      partial_message_size = 512;
      partial_message = xmalloc (partial_message_size);
    }
  else
    if (partial_message_size - partial_message_size_used < 256)
      {
        partial_message_size += 512;
        partial_message = xrealloc (partial_message, partial_message_size);
      }

#if defined(VA_START)
  VA_START (args, message);
#if HAVE_VSNPRINTF || _LIBC
  for ( ; ; )
    {
      n = vsnprintf (partial_message + partial_message_size_used,
		     partial_message_size - partial_message_size_used,
		     message, args);

      if (n < partial_message_size - partial_message_size_used)
        {
	  partial_message_size_used += n;
	  break;
	}

      partial_message_size += 512;
      partial_message = xrealloc (partial_message, partial_message_size);
    }
#else
  partial_message_size_used += vsprintf (partial_message + partial_message_size_used, message, args);

  /* Attempt to catch memory overwrites... */
  if (partial_message_size_used >= partial_message_size)
    {
      partial_message_size_used = 0;
      error (PS_UNDEFINED, 0, "partial error message buffer overflow");
    }
#endif
  va_end (args);
#else
#if HAVE_SNPRINTF
  for ( ; ; )
    {
      n = snprintf (partial_message + partial_message_size_used,
		    partial_message_size - partial_message_size_used,
		    message, a1, a2, a3, a4, a5, a6, a7, a8);

      if (n < partial_message_size - partial_message_size_used)
        {
	  partial_message_size_used += n;
	  break;
	}

      partial_message_size += 512;
      partial_message = xrealloc (partial_message, partial_message_size);
    }
#else
  sprintf (partial_message + partial_message_size_used, message, a1, a2, a3, a4, a5, a6, a7, a8);

  /* Attempt to catch memory overwrites... */
  if ((partial_message_size_used = strlen (partial_message)) >= partial_message_size)
    {
      partial_message_size_used = 0;
      error (PS_UNDEFINED, 0, "partial error message buffer overflow");
    }
#endif
#endif

  if (use_stderr && partial_message_size_used != 0)
    {
      partial_message_size_used = 0;
      fputs(partial_message, stderr);
    }
}

/* Complete an error message by appending MESSAGE, which is a printf-style
   format string with optional args, to the existing error message (which may
   be empty.)  The completed error message is then printed (and reset to
   empty.)
/* VARARGS */

void
#if defined(VA_START) && __STDC__
error_complete (int status, int errnum, const char *message, ...)
#else
error_complete (status, errnum, message, va_alist)
     int status;
     int errnum;
     char *message;
     va_dcl
#endif
{
#ifdef VA_START
  va_list args;
  int n;
#endif

  /* Make an initial guess for the size of any single message fragment.  */
  if (partial_message_size == 0)
    {
      partial_message_size_used = 0;
      partial_message_size = 512;
      partial_message = xmalloc (partial_message_size);
    }
  else
    if (partial_message_size - partial_message_size_used < 256)
      {
        partial_message_size += 512;
        partial_message = xrealloc (partial_message, partial_message_size);
      }

#if defined(VA_START)
  VA_START (args, message);
#if HAVE_VSNPRINTF || _LIBC
  for ( ; ; )
    {
      n = vsnprintf (partial_message + partial_message_size_used,
		     partial_message_size - partial_message_size_used,
		     message, args);

      if (n < partial_message_size - partial_message_size_used)
        {
	  partial_message_size_used += n;
	  break;
	}

      partial_message_size += 512;
      partial_message = xrealloc (partial_message, partial_message_size);
    }
#else
  partial_message_size_used += vsprintf (partial_message + partial_message_size_used, message, args);

  /* Attempt to catch memory overwrites... */
  if (partial_message_size_used >= partial_message_size)
    {
      partial_message_size_used = 0;
      error (PS_UNDEFINED, 0, "partial error message buffer overflow");
    }
#endif
  va_end (args);
#else
#if HAVE_SNPRINTF
  for ( ; ; )
    {
      n = snprintf (partial_message + partial_message_size_used,
		    partial_message_size - partial_message_size_used,
		    message, a1, a2, a3, a4, a5, a6, a7, a8);

      if (n < partial_message_size - partial_message_size_used)
        {
	  partial_message_size_used += n;
	  break;
	}

      partial_message_size += 512;
      partial_message = xrealloc (partial_message, partial_message_size);
    }
#else
  sprintf (partial_message + partial_message_size_used, message, a1, a2, a3, a4, a5, a6, a7, a8);

  /* Attempt to catch memory overwrites... */
  if ((partial_message_size_used = strlen (partial_message)) >= partial_message_size)
    {
      partial_message_size_used = 0;
      error (PS_UNDEFINED, 0, "partial error message buffer overflow");
    }
#endif
#endif

  /* Finally... print it.  */
  partial_message_size_used = 0;

  if (use_stderr)
    {
      fputs(partial_message, stderr);

      if (errnum)
	fprintf (stderr, ": %s", strerror (errnum));

      putc ('\n', stderr);
      fflush (stderr);

      ++error_message_count;

      if (status)
	  exit(status);
    }
  else
    error (status, errnum, "%s", partial_message);
}

/* Sometimes we want to have at most one error per line.  This
   variable controls whether this mode is selected or not.  */
int error_one_per_line;

void
#if defined(VA_START) && __STDC__
error_at_line (int status, int errnum, const char *file_name,
	       unsigned int line_number, const char *message, ...)
#else
error_at_line (status, errnum, file_name, line_number, message, va_alist)
     int status;
     int errnum;
     const char *file_name;
     unsigned int line_number;
     char *message;
     va_dcl
#endif
{
#ifdef VA_START
  va_list args;
#endif

  if (error_one_per_line)
    {
      static const char *old_file_name;
      static unsigned int old_line_number;

      if (old_line_number == line_number &&
	  (file_name == old_file_name || !strcmp (old_file_name, file_name)))
	/* Simply return and print nothing.  */
	return;

      old_file_name = file_name;
      old_line_number = line_number;
    }

  if (error_print_progname)
    (*error_print_progname) ();
  else
    {
      fflush (stdout);
      if ( *message == '\n' )
	{
	  fputc( '\n', stderr );
	  ++message;
	}
      fprintf (stderr, "%s:", program_name);
    }

  if (file_name != NULL)
    fprintf (stderr, "%s:%d: ", file_name, line_number);

#ifdef VA_START
  VA_START (args, message);
# if HAVE_VPRINTF || _LIBC
  vfprintf (stderr, message, args);
# else
  _doprnt (message, args, stderr);
# endif
  va_end (args);
#else
  fprintf (stderr, message, a1, a2, a3, a4, a5, a6, a7, a8);
#endif

  ++error_message_count;
  if (errnum)
    fprintf (stderr, ": %s", strerror (errnum));
  putc ('\n', stderr);
  fflush (stderr);
  if (status)
    exit (status);
}
