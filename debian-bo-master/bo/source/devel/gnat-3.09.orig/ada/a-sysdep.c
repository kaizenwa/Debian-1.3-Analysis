/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                              A - S Y S D E P                             */
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*                            $Revision: 1.24 $                             */
/*                                                                          */
/*   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms of the  GNU General Public License as published  by the Free Soft- */
/* ware  Foundation;  either version 2,  or (at your option) any later ver- */
/* sion.  GNAT is distributed in the hope that it will be useful, but WITH- */
/* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/* for  more details.  You should have  received  a copy of the GNU General */
/* Public License  distributed with GNAT;  see file COPYING.  If not, write */
/* to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/* MA 02111-1307, USA.                                                      */
/*                                                                          */
/* As a  special  exception,  if you  link  this file  with other  files to */
/* produce an executable,  this file does not by itself cause the resulting */
/* executable to be covered by the GNU General Public License. This except- */
/* ion does not  however invalidate  any other reasons  why the  executable */
/* file might be covered by the  GNU Public License.                        */
/*                                                                          */
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

/* This file contains system dependent symbols that are referenced in the
   GNAT Run Time Library */

#include "config.h"
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>

/*
   mode_read_text
   open text file for reading
   rt for DOS and Windows NT, r for Unix

   mode_write_text
   truncate to zero length or create text file for writing
   wt for DOS and Windows NT, w for Unix

   mode_append_text
   append; open or create text file for writing at end-of-file
   at for DOS and Windows NT, a for Unix

   mode_read_binary
   open binary file for reading
   rb for DOS and Windows NT, r for Unix

   mode_write_binary
   truncate to zero length or create binary file for writing
   wb for DOS and Windows NT, w for Unix

   mode_append_binary
   append; open or create binary file for writing at end-of-file
   ab for DOS and Windows NT, a for Unix

   mode_read_text_plus
   open text file for update (reading and writing)
   r+t for DOS and Windows NT, r+ for Unix

   mode_write_text_plus
   truncate to zero length or create text file for update
   w+t for DOS and Windows NT, w+ for Unix

   mode_append_text_plus
   append; open or create text file for update, writing at end-of-file
   a+t for DOS and Windows NT, a+ for Unix

   mode_read_binary_plus
   open binary file for update (reading and writing)
   r+b for DOS and Windows NT, r+ for Unix

   mode_write_binary_plus
   truncate to zero length or create binary file for update
   w+b for DOS and Windows NT, w+ for Unix

   mode_append_binary_plus
   append; open or create binary file for update, writing at end-of-file
   a+b for DOS and Windows NT, a+ for Unix

   Notes:

   (1) Opening a file with read mode fails if the file does not exist or
   cannot be read.

   (2) Opening a file with append mode causes all subsequent writes to the
   file to be forced to the then current end-of-file, regardless of
   intervening calls to the fseek function.

   (3) When a file is opened with update mode, both input and output may be
   performed on the associated stream.  However, output may not be directly
   followed by input without an intervening call to the fflush function or
   to a file positioning function (fseek, fsetpos, or rewind), and input
   may not be directly followed by output without an intervening call to a
   file positioning function, unless the input operation encounters
   end-of-file.

   The other target dependent declarations here are for the two functions
   set_binary_mode and set_text_mode:

      void set_binary_mode (int handle);
      void set_text_mode   (int handle);

   These functions have no effect in Unix (or similar systems where there is
   no distinction between binary and text files), but in DOS (and similar
   systems where text mode does CR/LF translation), these functions allow
   the mode of the stream with the given handle (fileno can be used to get
   the handle of a stream) to be changed dynamically. The returned result
   is 0 if no error occurs and -1 if an error occurs.

   Finally there is a boolean (character) variable

      char text_translation_required;

   which is zero (false) in Unix mode, and one (true) in DOS mode, with a
   true value indicating that text translation is required on text files
   and that fopen supports the trailing t and b modifiers.

*/

#if defined(WINNT) || defined (MSDOS) || defined (__EMX__)

  const char *mode_read_text = "rt";
  const char *mode_write_text = "wt";
  const char *mode_append_text = "at";
  const char *mode_read_binary = "rb";
  const char *mode_write_binary = "wb";
  const char *mode_append_binary = "ab";
  const char *mode_read_text_plus = "r+t";
  const char *mode_write_text_plus = "w+t";
  const char *mode_append_text_plus = "a+t";
  const char *mode_read_binary_plus = "r+b";
  const char *mode_write_binary_plus = "w+b";
  const char *mode_append_binary_plus = "a+b";
  const char text_translation_required = 1;

  /* for now these functions do nothing, must be fixed later ??? */
  void set_binary_mode (int handle) { ; }
  void set_text_mode   (int handle) { ; }

#else
  const char *mode_read_text = "r";
  const char *mode_write_text = "w";
  const char *mode_append_text = "a";
  const char *mode_read_binary = "r";
  const char *mode_write_binary = "w";
  const char *mode_append_binary = "a";
  const char *mode_read_text_plus = "r+";
  const char *mode_write_text_plus = "w+";
  const char *mode_append_text_plus = "a+";
  const char *mode_read_binary_plus = "r+";
  const char *mode_write_binary_plus = "w+";
  const char *mode_append_binary_plus = "a+";
  const char text_translation_required = 0;

  /* these functions do nothing in non-DOS systems */
  void set_binary_mode (FILE *stream) { ; }
  void set_text_mode   (FILE *stream) { ; }

#endif

#if defined (linux) || defined (sun) || defined (sgi) || defined (__EMX__) \
    || defined (__osf__) || defined (WINNT)
#include <termios.h>
#include <fcntl.h>
#endif

/* Implements the common processing for getc_immediate and
   getc_immediate_nowait. */
void
getc_immediate_common
  (FILE *stream, int *ch, int *end_of_file, int *avail, int waiting);

/* Called by Get_Immediate (Foo); */
void
getc_immediate
  (FILE *stream, int *ch, int *end_of_file)
{
  int avail;
  getc_immediate_common (stream, ch, end_of_file, &avail, 1);
}

/* Called by Get_Immediate (Foo, Available); */
void
getc_immediate_nowait
  (FILE *stream, int *ch, int *end_of_file, int *avail)
{
  getc_immediate_common (stream, ch, end_of_file, avail, 0);
}

/* Called by getc_immediate () and getc_immediate_nowait () */
void
getc_immediate_common
  (FILE *stream, int *ch, int *end_of_file, int *avail, int waiting)
{
#if defined (linux) || defined (sun) || defined (sgi) || defined (__EMX__) \
    || defined (__osf__) || defined (WINNT)
  char c;
  int nread;
  int flags;
  int good_one = 0;
  int eof_ch = 4; /* Ctrl-D */
  int fd = fileno (stream);
  struct termios otermios_rec, termios_rec;

  if (isatty (fd))
    {
      tcgetattr (fd, &termios_rec);
      memcpy (&otermios_rec, &termios_rec, sizeof (struct termios));
      while (!good_one)
        {
          /* Set RAW mode */
          termios_rec.c_lflag = termios_rec.c_lflag & ~ICANON;
#if defined(sgi) || defined (sun) || defined (__EMX__) || defined (__osf__)
          /* If waiting (i.e. Get_Immediate (Char)), set MIN = 1 and wait for
             a character forever. This doesn't seem to effect Ctrl-Z or
             Ctrl-C processing except on OS/2 where Ctrl-C won't work right
             unless we do a read loop. Luckily we can delay a bit between
             iterations. If not waiting (i.e. Get_Immediate (Char, Available)),
             don't wait for anything but timeout immediately. */
#ifdef __EMX__
          termios_rec.c_cc[VMIN] = 0;
          termios_rec.c_cc[VTIME] = waiting;
#else
          termios_rec.c_cc[VMIN] = waiting;
          termios_rec.c_cc[VTIME] = 0;
#endif
          eof_ch = termios_rec.c_cc[VEOF];
#endif
          tcsetattr (fd, TCSANOW, &termios_rec);

#if defined (linux)
          /* If we don't set this mode on Linux, Ctrl-C and Ctrl-Z processing
             get messed up */
          flags = fcntl (fd, F_GETFL);
          fcntl (fd, F_SETFL, O_NONBLOCK | flags);
#endif

          /* Read() is used here instead of fread(), because fread() doesn't
             work on Solaris5 and Sunos4 in this situation.  Maybe because we
             are mixing calls that use file descriptors and streams. */

          nread = read (fd, &c, 1);
          if (nread > 0)
            {
              /* On Unix terminals, Ctrl-D (EOT) is an End of File. */
              if (c == eof_ch)
                {
                  *avail = 0;
                  *end_of_file = 1;
                  good_one = 1;
                }
              /* Everything else is ok */
              else if (c != eof_ch)
                {
                  *avail = 1;
                  *end_of_file = 0;
                  good_one = 1;
                }
            }
          else if (!waiting)
            {
              *avail = 0;
              *end_of_file = 0;
              good_one = 1;
            }
          else
            {
              good_one = 0;
            }
        }
#if defined(linux)
      fcntl (fd, F_SETFL, flags);
#endif
      tcsetattr (fd, TCSANOW, &otermios_rec);
      *ch = c;
    }
  else
#endif
    /* If we're not on a terminal, then we don't need any fancy processing */
    /* Also this is the only thing that's left if we're not on one of the
       supported systems. */
    {
      *ch = fgetc (stream);
      if (feof (stream))
        {
          *end_of_file = 1;
          *avail = 0;
        }
      else
        {
          *end_of_file = 0;
          *avail = 1;
        }
    }
}


/* the following definitions are provided in NT to support Windows based
   Ada programs */

#ifdef WINNT
#include <windows.h>

/* Provide functions to echo the values passed to WinMain (windows bindings
   will want to import these).  We use the same names as the routines used
   by AdaMagic for compatibility.  */
char *rts_get_hInstance     (void) { return (GetModuleHandleA (0)); }
char *rts_get_hPrevInstance (void) { return (0); }
char *rts_get_lpCommandLine (void) { return (GetCommandLineA ()); }
int   rts_get_nShowCmd      (void) { return (1); }

#endif /* WINNT */

#ifdef VMS
/* This gets around a problem with using the old threads library on VMS 7.0 */
#include <time.h>
long
get_gmtoff ()
{
  time_t t;
  struct tm *ts;

  t = time ((time_t) 0);
  ts = localtime (&t);
  return ts->tm_gmtoff;
}
#endif
