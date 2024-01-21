/* Copyright (C) 1991,1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Routines to read various things from files. There are two entry points
   that the compiler should call: _p_get and _p_read.
   (_p_eof, _p_eoln, _p_lazyget are also here)

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

/*
 * Author: Jukka Virtanen <jtv@hut.fi>
 */

#include "rts.h"

/* File Descriptor Record definition */
#include "rts-fdr.h"
#include "varargs.h"

#undef GPC_STRINGS
/* Fetch the subroutines that parse what we need
 * (If GPC_STRINGS is undefined, they are reading from a file.)
 */
#include "rts-rdsub.c"

static void _p_GetByt();

/* Routine reads various things from TEXT files by
 * calling the subroutines in rts-readsub.c to do the actual job.
 * If these routines consume m_FILBUF, they must set_LGET
 */

/* Pre-assertion:  same as GET below
 * Post-assertion: (f.M = f1.M) and
 *		   (f.L~f.R = f0.L~f0.R) and
 *		   (f.R = t~u) and
 *		   (f^ = f.R.first)
 */

void
_p_read (va_alist)
va_dcl
{
    register va_list p;
    FDR File;

    int length;
    int failed = 0;

    /* Parameter required by subroutines, not used for files */
    int count = 0;

    va_start (p);
    File   = va_arg (p, FDR);	/* First arg is the file we read from */

    /* If you have non Pascal subroutines that output to terminal,
       but don't fflush, you might want to define READ_FLUSH in the
       Makefile to see what they write before you read from tty.

       It affects only the way how LAZY I/O flushes terminal output.
     */

#if defined(READ_FLUSH)
    if (tst_TTY(File))
#else
    if (tst_TTY(File) && !tst_LAZY(File))
#endif
	_p_fflush(TRUE);

    if (!ok_READ(File))
	failed = 1;

    /* Second is the number of remaining codes */
    for (length = va_arg (p, int); length > 0; length--) {
	int code = va_arg (p, int);	/* Type we are reading */

	/* If buffer is undefined, read in new contents */
	if (tst_LGET(File))
	  _p_GetByt (File);

	/* File buffer is now valid or EOF is set */

	switch (code) {
	case P_INT:
	    { int *i = va_arg (p, int *);
	      if (failed || !ok_EOF(File))
		*i = 0;
	      else
		_p_readi(File, i, &count);
	      break;
	    }
	case P_CHAR:
	    { char *ch = va_arg (p, char *);
	      if (failed || !ok_EOF(File))
		  *ch = ' ';
	      else
		{
		  *ch = m_FILBUF(File);
		  set_LGET(File);
		}
	      break;
	    }
	case P_REAL:
	    { double *d = va_arg (p, double *);
	      float  *f = (float *) NULL;  /* @@ maybe make selectable later */
	      if (failed || !ok_EOF(File))
		  *d = 0.0;
	      else
		_p_readr (File, d, f, 1, &count);
	      break;
	    }
	case P_LINE:
	    if (length != 1)
		_p_error (ABORT, "Compiler calls READLN incorrectly");
	    if (failed || !ok_EOF(File)) 
		break;

	    if (!tst_EOLN(File)) {
		_p_GetByt(File);
		while(!tst_EOLN(File))
		    _p_GetByt(File);
	    }
	    /* Now EOLN is not true, because we just read it off */
	    clr_EOLN(File);
	    set_LGET(File);
	    set_EOFOK(File);
	    break;

	case P_STRING:
	    { char *str    = va_arg (p, char *); /* pointer to string */
	      int  *curlen = va_arg (p, int *);  /* current string length */
	      int  maxlen  = va_arg (p, int);    /* length to be read */
	      if (failed || !ok_EOF(File))
		{
		  *str = '\000';
		  if (curlen)
		    *curlen = 0;
		}
	      else
		_p_reads (File, str, curlen, maxlen, &count);
	      break;
	    }

	default:
	    _p_error (ABORT, "unknown code in _p_read");
	}

	/* If this is not a lazy file, validate the buffer contents on exit
	 * if we consumed the value.
	 */
	if (tst_LGET (File) && !tst_LAZY(File))
	  _p_GetByt(File);

    }
    va_end (p);
}

/* Gets ONE BYTE from the file. */
static void
_p_GetByt(File)
FDR	File;
{
  if (ok_EOF(File))
    {
      int temp;
      int eof_now = tst_EOFOK(File);

      if (tst_TTY(File))
	{
	  _p_fflush(TRUE);

	  /* First get from the terminal input device
	   * This is done to take care of an EOLN test
	   * before anything is read in. Otherwise we
	   * would have to read in a character to test.
	   *
	   * @@ Document: If INPUT is RESET, the file buffer
	   * is set UNDEFINED and when nothing is read in yet:
	   *
	   * EOF(input) = False
	   *
	   *   (This is according to standard, because EOLN must be on
	   *    before EOF can be TRUE).
	   *
	   * EOLN(Input)
	   *
	   *   If it is TESTED it is TRUE.
	   *   If it is NOT TESTED it is FALSE
	   */
	  if (tst_EMPTY(File) && tst_UND(File) && tst_LGET(File)) 
	    clr_EMPTY(File);
	}

      clr_UND(File);
      clr_LGET(File);
      clr_EOFOK(File);

      m_FILBUF(File) = temp = getc(m_FILNUM(File));
      while (1)
	if (temp != EOF)
	  break;
	else
	  {
	    if (stdin != current_stdin && m_FILNUM(File) == current_stdin)
	      {
		/* If this is the end of current_stdin, we are reading
		 * from the options file. Continue with the original stdin
		 * instead of quitting.
		 */
		if (_p_restore_stdin(File))
		  {
		    m_FILBUF(File) = temp = getc(m_FILNUM(File));
		    continue;
		  }
	      }

	    if (tst_TXT(File) && !eof_now
		&& !tst_EOLN(File) && !tst_EMPTY(File))
	      {

		/* When reading from TEXT file EOLN is always true
		   just before EOF, even if there is no end of line
		   at the end of the file */

		set_EOLN(File);
		m_FILBUF(File) = ' ';
	      }
	    else
	      {
		set_EOF(File);
		clr_EOLN(File);
		set_UND(File);
	      }
	    return;
	  }

      if (tst_TXT(File) && m_FILBUF(File) == NEWLINE)
	{
	  set_EOLN(File);
	  m_FILBUF(File) = ' ';
	}
      else
	clr_EOLN(File);
    }
}

/* Gets m_SIZ bytes (> 1) from the file. */
static void
_p_GetN(File)
     FDR File;
{
  int n;

  if (ok_EOF(File))
    {
      clr_UND(File);
      if ((n = fread(m_FILBPTR(File),1,m_SIZ(File),m_FILNUM(File))) < m_SIZ(File))
	{
	  if (n != 0)
	    _p_error(REPORT, "GET partial record");
	  else
	    {
	      set_EOF(File);
	      clr_EOLN(File);
	    }
	  set_UND (File);
	}
    }
}

/* GET
 * pre-assertion: (f0.M = Inspection or f0.M = Update) and
 *		  (neither f0.L nor f0.R is undefined) and
 *		  (f0.R <> S())
 * post-assertion:(f.M = f0.M) and (f.L = f0.L~S(f0.R.first)) and
 *		  (f.R = f0.R.rest)) and
 * 		  (if (f.R = S()) then
 *		      (f^ is undefined)
 *		    else
 *		      (f^ = f.R.first))
 *
 */
void
_p_get(File)
FDR	File;
{
  /* If a lazy file buffer is undefined we need to get
   * twice to synchronize the buffer.
   */
  if (tst_LGET(File))
    _p_GetByt(File);

  if (ok_READ(File))
    if (m_SIZ(File) == 1) /* No files are packed yet. */
      _p_GetByt(File);
    else
      _p_GetN(File);
}


/* The standard requires that EOLN be set before EOF in text files.
 *
 * Based on this I do not validate an undefined buffer for text files
 * when reading from a terminal if EOLN is not set.
 */
int
_p_eof (File)
FDR	File;
{
    if (m_STATUS(File) == FiNOP)
	_p_generic(40);

    if (tst_LGET (File) && is_READABLE (File))
      /* If we do not have EOLN or EOFOK when reading from terminal text file,
       * this can't be eof
       */
      if (tst_TTY(File) && tst_TXT(File) && !(tst_EOLN(File) || tst_EOFOK(File)))
	return 0;
      else
	_p_GetByt (File);

    return tst_EOF (File);
}

int
_p_eoln (File)
     FDR File;
{
  if (m_STATUS(File) == FiNOP)
    _p_generic(41);

  if (!tst_TXT (File))
    _p_generic (61);

  if (tst_LGET (File) && is_READABLE (File))
    {
      if (tst_UND (File))
	{
	  /* If EOLN is tested in TERMINAL DEVICE where nothing has
	   * been read yet, return TRUE
	   * If it is not tested, it is FALSE.
	   *
	   * EMPTY is a special flag in this case, set before anything
	   * is read. On direct access files it means what it says.
	   */
	  if (tst_TTY (File) && tst_EMPTY(File))
	    {
	      m_FILBUF (File) = ' ';
	      set_EOLN (File);
	      clr_LGET (File);
	      clr_UND  (File);
	      clr_EMPTY(File);
	      
	      return TRUE;
	    }
	}
      
      _p_GetByt (File);
    }

  if (tst_EOF(File))
    _p_generic(42);
  
  return tst_EOLN (File);
}

/* This is the buffer referencing routine, that the compiler
 * should do inline. Nothing is actually done, if tst_LGET(File)
 * is not on. Compiler should dereference the file buffer address
 * to get the value from the buffer.
 *
 * Only TEXT files may get the lazy attribute.
 * If something else has it, the routines won't work.
 */
void
_p_lazyget(File)
FDR File;
{
#if 0
  /* @@@ This is called also for "buffer^ := VAL;"
   * So it must not blindly trap the reference
   *
   * Compiler should clear the UND bit for these...
   */
  if (tst_UND (File) && !tst_LGET (File))
    _p_error (ABORT, "Reference to a file buffer variable with undefined value `%s^'",
	      m_NAM (File));
#endif  

  /* If the file buffer contents is lazy, validate it */
  if (! tst_LGET(File))
    return;

  _p_GetByt (File);
}
