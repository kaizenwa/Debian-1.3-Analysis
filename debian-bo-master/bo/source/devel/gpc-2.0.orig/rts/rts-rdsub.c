/* Copyright (C) 1991 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Subroutines for READ and READSTR.

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

/*
 * This file is #included by rts-read.c and rts-string.c
 *
 * Keep all routines "static". They are modified with
 * defines.
 */

/*
 * Define ISO7185_ONLY if you don't want to accept Extended Pascal
 * real number format extension: 
 *  [ sign ] (digit-sequence "." | "." fractional-part) [ "e" scale-factor ]
 *
 * By default it's accepted.
 */

#ifdef GPC_STRINGS

#define NEXTCHAR(ch, from, count) 					\
  do { if ((*count)++ > _p_maxchars) 					\
	 _p_error (ABORT, "Read past end of string in `readstr'"); 	\
       ch = *++from; } while (0)
#define SOURCETYPE	     char
#define INTYPE		     char *

#else

#define NEXTCHAR(ch, from, count)   do { ch = getc(from); } while (0)
#define SOURCETYPE	     FILE
#define INTYPE		     FDR

#endif /* GPC_STRINGS */

# define ROK     (0)
# define RSYNTAX (-1)
# define RFATAL  (-2)

/* Scanf does not parse the floating point numbers
   like the standard requires. So, I do the syntax
   analysis here and give the result to sscanf.

   Integers are also read here; this is to trap
   overflows... (someday this may even work)
 */

/* this reads one integer */
static int
getint (f, ch, count)
SOURCETYPE *f;
int ch;
int *count;
{
    int num  = 0;
    int skip = 0;

    /* ch is a digit on entry */

    /*
     * Here I used to enable integer overflow traps but that was a
     * long time ago for another compiler far away...
     */

    do {
      if (! skip)
	{
	  if (num > READ_INT_TRAP)
	    skip = 1;
	  else
	    {
	      num = num*10+(ch-'0');
	      if (num < 0)
		skip = 1;
	    }
	}
      NEXTCHAR (ch, f, count);
    } while (isdigit(ch));

    if (skip)
      {
	_p_error (ABORT, "Integer overflow");
	num = INT_MAX;
      }

#ifndef GPC_STRINGS
    if (ch != EOF)	  /* Push back last non-digit, unless it was eof */
      ungetc (ch, f);
#endif

    return num;
}


static char *where;

static void
store_int (what)
     int what;
{
  if (what > 9)
    store_int (what/10);

  *where++ = (what%10)+'0';
}

/* parse a floating point number */
static int
_p_rdfloat (Source, res, count)
INTYPE	Source;
char   *res; 		/* stores characters in here */
int    *count;
{
    SOURCETYPE *f;
    register int  ch;
    int nonzero    = 0;
    int require_fractional = 0;
    int adjust_exp = 0;
    int expon = 0;
    int esign = 1;

#ifdef GPC_STRINGS
    f  = Source;
    ch = *f;
#else
    /* file buffer is JUNK after this till return to _p_read */

    f = m_FILNUM(Source);

    ch = m_FILBUF(Source);	       /* Get the buffered chraco */
    set_LGET (Source);
#endif /* GPC_STRINGS */

    while(isspace_nl(ch))
      NEXTCHAR (ch, f, count);

#ifdef ISO7185_ONLY
    if (!isdigit(ch) && ch != '+' && ch != '-')
#else
    if (!isdigit(ch) && ch != '+' && ch != '-' && ch != '.')
#endif
      {
	_p_generic(605); /* Sign or digit expected */
	strcpy (res, "0");
	return(ROK);
      }
    else
      {
	if (ch=='+' || ch=='-')
	  {
	    *res++ = ch;
	    NEXTCHAR (ch, f, count);
	    
	    /* Skip spaces between sign and digit (or '.') */
	    while(isspace_nl(ch))
	      NEXTCHAR (ch, f, count);
	  }
      }

#ifdef ISO7185_ONLY
    if (!isdigit(ch))
      {
	_p_generic(604); /* Digit expected after sign */
	strcpy (res, "0");
	return(ROK);
      }

    require_fractional = 1;
#else
    if (!isdigit(ch) && ch != '.')
      {
	_p_error (ABORT, "Digit or '.' expexted after sign");
	strcpy (res, "0");
	return(ROK);
      }
    
    require_fractional = !isdigit (ch);
#endif

    /* Read the mantissa. ch is now a digit (or '.') */
    while (isdigit(ch))
      {
	if (ch != '0' || nonzero)
	  {
	    *res++ = ch;
	    nonzero = 1;
	  }
	NEXTCHAR (ch, f, count);
      }
    
    /* No significant digits, and there is no decimal point
     *  --> mantissa is zero
     */
    if (! nonzero && ch != '.')
      *res++ = '0';

    /* read the fractional part */
    if (ch == '.')
      { /* Read the fractional part */
	if (nonzero)
	  *res++ = ch;	/* Store decimal point if mantissa nonzero */
	else
	  adjust_exp = -1;
	
	NEXTCHAR (ch, f, count);
	
	if (require_fractional && !isdigit(ch))
	  {
	    _p_generic(607); /* Digit expected after decimal point */
	    ch = '0';
	  }

	while(isdigit(ch))
	  {
	    if (nonzero || ch != '0')
	      {
		*res++ = ch;
		if (! nonzero)
		  {
		    *res++ = '.';
		    nonzero = 2;
		  }
	      }
	    else
	      adjust_exp -= 1;
	  
	    NEXTCHAR (ch, f, count);
	  }
      
	if (!nonzero)
	  {
	    *res++ = '0';
	    *res++ = '.';
	    *res++ = '0';
	    adjust_exp = 0;
	  }
      }

    /* read the exponent */
    if (ch=='e' || ch=='E') {

      if (nonzero)
	*res++ = ch;

      NEXTCHAR (ch, f, count);
      if (ch == '+' || ch == '-') {
	if (nonzero)
	  *res++ = ch;
	if (ch == '-')
	  esign = -1;
	NEXTCHAR (ch, f, count);
      }

      if (!isdigit(ch)) {
	_p_generic(608); /* Digit expected while reading exponent */
	ch = '0';
      }

      do {
	expon = 10*expon+(ch-'0');
	NEXTCHAR (ch, f, count);
      } while(isdigit(ch));
    }

    if (nonzero && (adjust_exp || expon))
      {
	if (expon)
	  /* Add the adjust value */
	  expon = esign*expon+adjust_exp;
	else
	  {
	    expon = adjust_exp;
	    *res++ = 'E';
	    if (expon < 0)
	      {
		esign  = -1;
		*res++ = '-';
	      }
	  }

	if (expon > DBL_MAX_10_EXP || expon < DBL_MIN_10_EXP)
	  _p_generic(609);

	expon = esign*expon;
	where = res;
	store_int (expon);
	res = where;
      }

    *res++ = '\000';

#ifndef GPC_STRINGS
    if (ch != EOF)
      ungetc (ch, f); /* Push back the char not belonging to the number */
#endif

    return(ROK);
}

static void
_p_readi (Source, p, count)
INTYPE	Source;
int    *p;
int    *count;
{
    SOURCETYPE *f;
    int negative, num;
    int ch;

#ifdef GPC_STRINGS
    f  = Source;
    ch = *f;
#else
    /* file buffer is JUNK after this till return to _p_read */

    f = m_FILNUM(Source);

    ch = m_FILBUF(Source);	       /* Get the buffered chraco */
    set_LGET (Source);
#endif /* GPC_STRINGS */

    negative = FALSE;
    while(isspace_nl(ch))
      NEXTCHAR (ch, f, count);

    if (!isdigit(ch) && ch != '+' && ch != '-')
      {
	_p_generic(605); /* Sign or digit expected */
	ch = '0';
      }
    else
      {
	if (ch=='+' || ch=='-')
	  {
	    if (ch == '-')
	      negative = TRUE;
	    NEXTCHAR (ch, f, count);
	    if (!isdigit(ch)) {
	      _p_generic(604); /* Digit expected after sign */
	      ch = '0';       /* If returned, set it to something */
	    }
	  }
      }
    
    /* Now the 'ch' contains the first digit. Get the integer */
    num = getint (f, ch, count);

    *p  = (negative? -num : num);
}

/* read a string up to the max length or newline, whichever comes first.
 * The number of characters read is stored in curlen, if that is not null.
 * If curlen is NULL, then this is a fixed length string.
 */
static void
_p_reads (Source, str, curlen, maxlen, count)
INTYPE	Source;
char   *str;
int    *curlen;
int     maxlen;
int    *count;
{
  int length = 0;

  if (maxlen < 0)
    _p_error (ABORT, "String length can not be negative");

  /* If EOLN(Source) is on, nothing is read and string is filled with
   * proper values. Length is left zero.
   */
  if (! tst_EOLN (Source))
    {
#ifdef GPC_STRINGS
      char *f = Source;
      int  ch = *f;
#else
      FILE *f = m_FILNUM(Source);
      int ch  = m_FILBUF(Source);	       /* Get the buffered chraco */
#endif /* GPC_STRINGS */

      /* Like TEXT files, `end of line' is always set before `end of file' */
      for (; ch != NEWLINE
#ifdef GPC_STRINGS
	   && *count < _p_maxchars
#else
	   && ch != EOF
#endif
	   && length < maxlen; length++, str++)
	{
	  *str = ch;
	  NEXTCHAR (ch, f, count);
	}
      
#ifndef GPC_STRINGS
      if (ch != EOF)
	ungetc (ch, f);
      set_LGET (Source);
#endif
    }

  if (curlen)
    *curlen = length;

  /* variable length strings are filled with null chars
   * fixed length strings are filled with spaces
   */
  while (length++ < maxlen)
    *str++ = curlen ? '\000' : ' ';
}

static void
_p_readr (Source, real_d, real_f, double_precision, count)
INTYPE	Source;
double *real_d;
float  *real_f;
int     double_precision;
int    *count;
{
    char buffer[ 256 ];
	
    switch(_p_rdfloat(Source, buffer, count))
    {
    case ROK:
	/* Use scanf to convert to binary */
        /* @@ check how GCC real.c could be used instead */
	if (double_precision)
	  sscanf(buffer, "%lf", real_d);
	else
	  sscanf(buffer, "%f",  real_f);
	break;
    case RSYNTAX:
	_p_generic(601);
	if (double_precision)
	  *real_d = 0.0;
	else
	  *real_f = 0.0;
	break;
    }
}
