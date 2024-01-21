/* Copyright (C) 1991,1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Routines to output various things.
   Three entry points: _p_write, _p_page & _p_put.

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

/* str is not currently used */
#define LEN_CHK(str, val) if (val < 0) _p_generic(60)

void
_p_page(File)
FDR	File;
{

    if (ok_WRITE(File))
	fprintf(m_FILNUM(File),"%c",NEWPAGE);
}

/* Routine writes various objects to TEXT files */

void
_p_write (va_alist)
va_dcl
{
    register va_list p;
    FDR File;

    int length;
    FILE *out;

    va_start (p);
    File   = va_arg (p, FDR);	/* First arg is the file we write to */
    
    if (!ok_WRITE (File)) {
	va_end (p);
	return;
    }

    out = m_FILNUM(File);

    /* Next comes the number of remaining codes */
    for (length = va_arg (p, int); length > 0; length--) {
	int code = va_arg (p, int);	/* Type we are writing */
	switch (code) {
	case P_INT:
	    fprintf(out, "%*d", INT_OUT_WIDTH, va_arg (p, int));
	    break;
	case P_INT2:
	    {
	      int type = va_arg (p, int);
	      unsigned long val = 0;
	      char *fmt = "%*d";
	      switch (type) {
	      case P_S_BYTE:
		val = va_arg (p, int);
		val &= 0xff;
		break;
	      case P_S_SHORT:
		val = va_arg (p, int);
		val &= 0xffff;
		break;
	      case P_S_INT:
		val = va_arg (p, int);
		break;
	      case P_S_LONG:
		val = va_arg (p, long);
		fmt = "%*l";
		break;
	      case P_U_BYTE:
		val = va_arg (p, unsigned int);
		fmt = "%*u";
		break;
	      case P_U_SHORT:
		val = va_arg (p, unsigned int);
		fmt = "%*u";
		break;
	      case P_U_INT:
		val = va_arg (p, unsigned int);
		fmt = "%*u";
		break;
	      case P_U_LONG:
		val = va_arg (p, unsigned long);
		fmt = "%*lu";
		break;
	      default:
		_p_error (ABORT, "type arg of P_INT2 is invalid");
		val = 0;
		break;
	      }
	      fprintf(out, fmt, INT_OUT_WIDTH, val);
	      break;
	    }
	case P_CHAR:
	    {
	      int ch = va_arg (p, int);

	      /* Problem: Wide character support? */
	      ch &= 0xff;

	      fprintf (out, "%c", ch);
	      break;
	    }
	case P_REAL:
	    { double num = va_arg (p, double);
	      fprintf(out, (num < 0.0) ? "%.*e" : " %.*e",
		      REAL_OUT_WIDTH-7, num);
	      break;
	    }
	case P_LINE:
	    if (length != 1)
		_p_error (ABORT, "Compiler calls `WRITELN' incorrectly");
	    fprintf(out, "%c",NEWLINE);
	    break;
	case P_FIX_INT:
	    { int this = va_arg (p, int);
	      int len  = va_arg (p, int);
	      LEN_CHK ("fixed integer", len);
	      if (len)
		fprintf(out, "%*d", len, this);
	      break;
	    }
	case P_FIX1_REAL:
	    { double d = va_arg (p, double);
	      int len  = va_arg (p, int);
	      LEN_CHK ("fixed real length", len);
	      if (len < REAL_MIN_WIDTH)
		len = REAL_MIN_WIDTH;	/* Minimum width */
	      if (len)
		fprintf(out, (d < 0.0) ? "%.*e" : " %.*e",
			len-7, d);
	      break;
	    }
	case P_FIX2_REAL:
	    { double d = va_arg (p, double);
	      int mlen = va_arg (p, int);
	      int dlen = va_arg (p, int);
	      LEN_CHK ("fixed real total", mlen);
	      LEN_CHK ("fixed length real fraction", dlen);
	      if (mlen)
		fprintf(out, "%*.*f", mlen, dlen, d);
	      break;
	    }
	case P_STRING:
	    { char *str    = va_arg (p, char *);
	      int  *curlen = va_arg (p, int *);
	      int  len     = va_arg (p, int);
	      LEN_CHK ("Compiler error: string width", len);
	      /* Use fwrite instead of fprintf, because the
	       * string should be written as is, e.g. NULL
	       * chars must not alter the output format.
	       */
	      if (len)
		if (curlen)
		  {
		    if (*curlen <= 0)
		      break;
		    LEN_CHK ("???Current string length < 0", *curlen);
		    fwrite(str, 1, *curlen, out);
		  }
		else
		  fwrite(str, 1, len, out);
	      break;
	    }
	case P_FIX_STRING:
	    { char *str   = va_arg (p, char *);
	      int  wanted = va_arg (p, int);
	      int  len    = va_arg (p, int);
	      LEN_CHK ("Compiler error: fixed string width", len);
	      LEN_CHK ("fixed string", wanted);
	      if (!len || !wanted)
		break;
	      if (len >= wanted)
		len = wanted;
	      else
		/* Output WANTED-LEN spaces in front of the string */
		fprintf(out, "%*c",wanted-len,' ');

	      fwrite(str, 1, len, out);
	      break;
	    }
	case P_FIX_CHAR:
	    { int ch = va_arg (p, int);
	      int len = va_arg (p, int);

	      /* Problem: Wide character support? */
	      ch &= 0xff;

	      LEN_CHK("fixed char", len);
	      if (len)
		fprintf (out, "%*c", len, ch);
	      break;
	    }
	case P_BOOL:
	    { int val = va_arg (p, int);
	      val &= 0xff;
	      fprintf(out, "%*s",
		      BOOL_OUT_WIDTH, val ? TRUE_str : FALSE_str);
	      break;
	    }
	case P_FIX_BOOL:
	    { int val = va_arg (p, int);
	      int len = va_arg (p, int);
	      val &= 0xff;
	      LEN_CHK ("fixed boolean", len);
	      if (len)
		fprintf(out, "%*.*s",len,len, val ? TRUE_str : FALSE_str);
	      break;
	    }
	default:
	    _p_error (ABORT, "unknown code in _p_write");
	}
    }
    va_end (p);

    if (tst_LAZY(File) && tst_TTY(File))
      fflush(m_FILNUM(File));		/* Do a put on first opportunity */
}

/* PUT
 * pre-assertion: (f0.M = Generation or f0.M = Update) and
 *		  (neither f0.L nor f0.R is undefined) and
 *		  (f0.R = S() or f is a direct access file type) and
 *		  (f0^ is not undefined)
 * post-assertion:(f.M = f0.M) and (f.L = f0.L~S(f0^)) and
 *		  (if f0.R = S() then
 *		     (f.R = S())
 *		   else
 *		     (f.R = f0.R.rest)) and
 * 		   (if (f.R = S()) or (f0.M = Generation) then
 *		      (f^ is undefined)
 *		    else
 *		      (f^ = f.R.first))
 *
 */
void
_p_put(File)
     FDR File;
{
  int	n;
	
  if (ok_WRITE(File))
    {
      register FILE *filep = m_FILNUM(File);

      if (tst_TXT(File) || m_SIZ(File) == 1)
	n = (putc(m_FILBUF(File), filep)) != EOF;
      else
	n = fwrite(m_FILBPTR(File),1,m_SIZ(File),filep);

      if (n == 0)
	_p_error(ABORT, "PUT failed - nothing written");
      else if (n != m_SIZ(File))
	_p_error(ABORT, "PUT failed - partial record written");
      
      /* Lazy i/o put is flushed immediately */
      if (tst_DIRECT (File) || (tst_LAZY(File) && tst_TTY(File)))
	fflush (filep);

      /* f^ set undefined if eof or mode is generation */
      if (tst_EOF(File) || !TST_STATUS(File, FiRND))
	set_UND (File);
    }
}
