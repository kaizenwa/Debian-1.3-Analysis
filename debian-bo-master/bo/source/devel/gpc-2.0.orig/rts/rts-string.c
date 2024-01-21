/* Copyright (C) 1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   String handling routines

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
 * Author: Juki <jtv@hut.fi>
 *
 * Sun Oct 24 21:36:05 1993
 *
 * Note: These routines are under construction, most of them have
 * never been tested.
 *
 * Entry points: _p_readstr, _p_writestr and _p_string
 * 
 */

#include "rts.h"
#include "rts-types.h"

#include "varargs.h"

/* str is not currently used */
#define LEN_CHK(str, val) \
	    if (val < 0) _p_error (ABORT, "Field length must be positive")

static int
_p_substr (src, curlen, i, j, dest)
     char *src;
     int curlen;
     int i;
     int j;
     char *dest;
{
  int count = 0;

  if (i <= 0)
    _p_error (ABORT, "Substring can not start from positions less than 1");

  if (j < 0)
    _p_error (ABORT, "Substring length can not be negative");

  if (i+j-1 > curlen)
    _p_error (ABORT, "Substring must terminate before end of string");

  *dest = '\000';

  /* Return empty string */
  if (j == 0)
    return 0;
  
  for (; count < j; count++)
    dest[ count ] = src[ i-1+count ];

  /* @@ I don't know if this is correct */
  /* terminate the string with null. */
  if (count < curlen)
    dest[ count+1 ] = '\000';

  return count;
}

/* Pascal string relations: =, <
 * = and < are used to compute values of:  >, <=, >=
 *
 * these pad the shorter string with spaces.
 */

/* compare strings for equality padding the shorter string with spaces
 * if necessary.
 */
static int
_p_str_eq (s1, len1, s2, len2)
     char *s1;
     int len1;
     char *s2;
     int len2;
{
  int result;
  if (len2 < len1)
    {
      char *ts;
      int  tl;
      ts = s1; s1 = s2; s2 = ts;
      tl = len1; len1 = len2; len2 = tl;
    }

  /* shorter string is now s1 */

  result = strncmp (s1, s2, len1) == 0;

  if (result && len1 != len2)
    {
      int count = len2 - len1;
      char *ptr = &s2[ len1 ];
      for (; result && count > 0; count--)
	result = *ptr++ == ' ';
    }

  return result;
}

static int
_p_str_lt (s1, len1, s2, len2)
     char *s1;
     int len1;
     char *s2;
     int len2;
{
  int   result;
  int   count;
  char *ptr;
  int   s_len = len1;
  int   l_len = len2;
  char *l_str = s2;
  int   s1_is_longer = 0;

  /* string s2 is shorter? */
  if (len2 < len1)
    {
      s_len = len2;
      l_len = len1;
      l_str = s1;
      s1_is_longer = 1;
    }

  /* string compare: zero if equal, negative if s1 < s2, positive if s1 > s2 */
  result = __gcc_bcmp (s1, s2, s_len);
  if (result || len1 == len2)
    return result < 0;

  /* Start scanning the longer string */
  ptr = &l_str[ s_len ];

  /* Decide on the first non-space character of the rest of the longer string:
   *    return TRUE if character < ' ' iff s1 is LONGER
   *    return TRUE if character > ' ' iff s1 is SHORTER
   *    return FALSE if (shorter = TRIM (longer))
   */
  for (count = l_len - s_len; count > 0; count--, ptr++)
    if (*ptr != ' ')
      if (s1_is_longer)
	return *ptr < ' ';
      else
	return *ptr > ' ';
  
  return 0;		        /* padded strings appear equal */
}

/*
 * String relations: EQ, LT
 * NE, LE, GT, GE are implemented with EQ and LT
 *
 * Don't pad with spaces.
 */

#define EQ(s1,len1,s2,len2)   ((len1) == (len2) && !strncmp (s1, s2, len1))

/* LT(s1,s2) == if n1 < n2 this is equivalent to
 *		   s1v <= substr (s2v, 1, n1)
 *              else it is equivalent to
 *		   substr (s1v, 1, n2) < s2v
 */
static int
_p_lt (s1, len1, s2, len2)
     char *s1;
     int  len1;
     char *s2;
     int  len2;
{
  if (len1 < len2)
    return __gcc_bcmp (s1, s2, len1) <= 0;

  return __gcc_bcmp (s1, s2, len2) < 0;
}

/* Generic string function */
int
_p_string (va_alist)
va_dcl
{
    register va_list p;

    char *s1, *s2 = (char *)NULL;
    int  len1, len2 = 0;
    char c1, c2;

    int code;
    int argument_mask;
    int retval = 0;

    va_start (p);
    code = va_arg (p, int);     /* First arg is the opcode */

    argument_mask = va_arg (p, int); /* mask of argument types */

    /* First arg is always a string or char */
    if (argument_mask & P_STR_FIRST_IS_CHAR)
      {
	c1   = va_arg (p, char);
	s1   = &c1;
	len1 = 1;
      }
    else
      {
	/* It's a string */
	s1   = va_arg (p, char *);
	len1 = va_arg (p, int);
      }

    /* two string args, either may be a string or char */
    if (code != R_LENGTH && code != R_TRIM && code != R_SUBSTR)
      if (argument_mask & P_STR_SECOND_IS_CHAR)
	{
	  c2   = va_arg (p, char);
	  s2   = &c2;
	  len2 = 1;
	}
      else
	{
	  /* It's a string */
	  s2   = va_arg (p, char *);
	  len2 = va_arg (p, int);
	}

    switch (code) {
      /* two args, either may be a string or char */

      /* lexicographic relations */
    case R_EQ:
      retval =  EQ (s1, len1, s2, len2); break;
    case R_LT:
      retval = _p_lt (s1, len1, s2, len2); break;
    case R_GT:
      retval = !EQ (s1, len1, s2, len2) && !_p_lt (s1, len1, s2, len2); break;
    case R_NE:
      retval = !EQ (s1, len1, s2, len2); break;
    case R_LE:
      retval =  EQ (s1, len1, s2, len2) || _p_lt (s1, len1, s2, len2); break;
    case R_GE:
      retval = !_p_lt (s1, len1, s2, len2); break;

      /* relations padding with space if necessary */
    case R_eq:
      retval =  _p_str_eq (s1, len1, s2, len2); break;
    case R_lt:
      retval =  _p_str_lt (s1, len1, s2, len2); break;
    case R_ge:
      retval = !_p_str_lt (s1, len1, s2, len2); break;
    case R_ne:
      retval = !_p_str_eq (s1, len1, s2, len2); break;
    case R_le:
      retval =   _p_str_eq (s1, len1, s2, len2)
	      || _p_str_lt (s1, len1, s2, len2); break;
    case R_gt:
      retval =   !_p_str_eq (s1, len1, s2, len2)
	      && !_p_str_lt (s1, len1, s2, len2); break;

    case R_INDEX:
      if (len2 < 0 || len1 < 0)
	_p_error (ABORT, "Negative string lengths. Must be joking...?");

      /* null string (s2) is contained in all strings at position 1 */
      if (len2 == 0)
	return 1;
      
      /* NULL string (s1) does not contain s2 */
      if (len1 == 0)
	return 0;

      /* index (char, <string,char>) */
      if (len1 == 1)
	return (len2 == 1 && *s1 == *s2);

      /* index (string, char) */
      if (len2 == 1)
	{
	  int where;
	  for (where = 0; where < len1; where++)
	    if (s1[ where ] == *s2)
	      return where + 1;
	  break;
	}

      /* index (string, string) */
      {
	int first;

	for (first = 0; first+len2 <= len1; first++)	/* does it fit?     */
	  if (s1[ first ] == *s2			/* first char match */
	      && s2[ len2-1 ] == s1 [ first+len2-1 ])	/* last char match  */
	    {
	      int scan;
	      int match = TRUE;
	      for (scan = 1; scan < len2-1 && match; scan++)
		match = s1 [ first + scan ] == s2 [ scan ];

	      if (match)				/* Gotta light, buddy? */
		return first + 1;			/* Second?	       */
	    }
      }
      break;			       /* No match found, take a break instead */

    case R_SUBSTR:
      {
	char *dest = va_arg (p, char *);
	int *dlen  = va_arg (p, int *);
	int from   = va_arg (p, int);
	int length = va_arg (p, int);

	int curlen = _p_substr (s1, len1, from, length, dest);

	if (dlen)
	  *dlen = curlen;

	/* return value not used */
	break;
      }

    case R_TRIM:
      {
	char *dest = va_arg (p, char *);
	int *dlen  = va_arg (p, int *);
	int inx;

	/* Copy the whole string first */
	(void) strncpy (dest, s1, len1);

	for (inx = len1; inx > 0; inx--)
	  if (dest[ inx-1 ] != ' ')
	    break;			/* nonspace */
	  else
	    dest[ inx-1 ] = '\000';	/* Convert trailing spaces to nulls */

	if (dlen)
	  *dlen = inx;

	/* return value not used */
	break;
      }

    case R_LENGTH:
      _p_error (ABORT, "Oh my! LENGTH should be inline code -- compiler error");

    default:
      _p_error (ABORT, "Unknown string function called -- compiler error");
      break;
    };

    va_end (p);

    return retval;
}


/* routine that writes to a string */
/* @@@@ MODIFY: use temp buffer for output, then strcat them together */
void
_p_writestr (va_alist)
va_dcl
{
    register va_list p;

    /* Destination string description */
    char *s1;
    int *len1;
    int maxlen1;

    char *out;

    int length;
    int total_printed  = 0;
    int printed;

    va_start (p);
    s1      = va_arg (p, char *);  /* First arg is the string we write to */
    len1    = va_arg (p, int *);
    maxlen1 = va_arg (p, int);
    
    out = s1;			/* the string we write to */

#define TRAP_STRING_OVERFLOW				\
    		if (total_printed + printed > maxlen1)	\
  		   _p_error(ABORT, "Attempt to write past end of string in `writestr'")

    /* Next comes the number of remaining codes */
    for (length = va_arg (p, int); length > 0; length--) {
	int code = va_arg (p, int);	/* Type we are writing */

	printed = 0;
	switch (code) {
	case P_INT:
	  printed = INT_OUT_WIDTH;
	  TRAP_STRING_OVERFLOW;
	  (void )sprintf(out, "%*d", INT_OUT_WIDTH, va_arg (p, int));
	  break;
	case P_CHAR:
	  {
	    int ch = va_arg (p, int);

	    /* Problem: Wide character support? */
	    ch &= 0xff;

	    printed = 1;
	    TRAP_STRING_OVERFLOW;
	    (void) sprintf(out, "%c", ch);
	    break;
	  }
	case P_REAL:
	  { double num = va_arg (p, double);
	    printed = sprintf(out, (num < 0.0) ? "%.*e" : " %.*e",
			      REAL_OUT_WIDTH-7, num);
	    /* Writes first */
	    TRAP_STRING_OVERFLOW;
	    break;
	  }
	case P_FIX_INT:
	  { int this = va_arg (p, int);
	    int len  = va_arg (p, int);
	    LEN_CHK ("fixed integer", len);
	    if (! len)
	      break;
	    printed = len;
	    TRAP_STRING_OVERFLOW;
	    (void) sprintf(out, "%*d", len, this);
	    break;
	  }
	case P_FIX1_REAL:
	  { double d = va_arg (p, double);
	    int len  = va_arg (p, int);
	    LEN_CHK ("fixed real length", len);
	    if (! len)
	      break;

	    if (len < REAL_MIN_WIDTH)
	      len = REAL_MIN_WIDTH;	/* Minimum width */
	    printed = sprintf(out, (d < 0.0) ? "%.*e" : " %.*e",
			      len-7, d);
	    /* Writes first */
	    TRAP_STRING_OVERFLOW;
	    break;
	  }
	case P_FIX2_REAL:
	  { double d = va_arg (p, double);
	    int mlen = va_arg (p, int);
	    int dlen = va_arg (p, int);
	    LEN_CHK ("fixed real total", mlen);
	    LEN_CHK ("fixed length real fraction", dlen);
	    if (! mlen)
	      break;
	    printed = sprintf(out, "%*.*f", mlen, dlen, d);
	    /* Writes first */
	    TRAP_STRING_OVERFLOW;
	    break;
	  }
	case P_STRING:
	  { char *str    = va_arg (p, char *);
	    int  *curlen = va_arg (p, int *);
	    int  len     = va_arg (p, int);
	    LEN_CHK ("Compiler error: string width", len);
	    if (len)
	      if (curlen)
		{
		  LEN_CHK ("Current string length < 0", *curlen);
		  if (! *curlen)
		    break;
		  printed = *curlen;
		  TRAP_STRING_OVERFLOW;
		  (void) sprintf(out, "%*.*s",*curlen, *curlen, str);
		}
	      else
		{
		  printed = len;
		  TRAP_STRING_OVERFLOW;
		  (void) sprintf(out, "%*.*s",len,len,str);
		}
	    break;
	  }
	case P_FIX_STRING:
	  { char *str = va_arg (p, char *);
	    int  len  = va_arg (p, int);
	    int  wanted = va_arg (p, int);
	    LEN_CHK ("Compiler error: fixed string length", len);
	    LEN_CHK ("fixed string", wanted);
	    if (!len || !wanted)
	      break;
	    if (len > wanted)
	      len = wanted;
	    printed = wanted;
	    TRAP_STRING_OVERFLOW;
	    (void) sprintf(out, "%*.*s",wanted,len,str);
	    break;
	  }
	case P_FIX_CHAR:
	  { int ch = va_arg (p, int);
	    int len = va_arg (p, int);
	    
	    /* Problem: Wide character support? */
	    ch &= 0xff;
	    
	    LEN_CHK("fixed char", len);
	    if (! len)
	      break;
	    printed = len;
	    TRAP_STRING_OVERFLOW;
	    (void) sprintf (out, "%*c", len, ch);
	    break;
	  }
	case P_BOOL:
	  { int val = va_arg (p, int);
	    val &= 0xff;
	    printed = BOOL_OUT_WIDTH;
	    TRAP_STRING_OVERFLOW;
	    (void) sprintf(out, "%*s",
			   BOOL_OUT_WIDTH, val ? TRUE_str : FALSE_str);
	    break;
	  }
	case P_FIX_BOOL:
	  { int val = va_arg (p, int);
	    int len = va_arg (p, int);
	    val &= 0xff;
	    LEN_CHK ("fixed boolean", len);
	    if (! len)
	      break;
	    printed = len;
	    TRAP_STRING_OVERFLOW;
	    (void) sprintf(out, "%*.*s",len,len, val ? TRUE_str : FALSE_str);
	    break;
	  }
	default:
	  _p_error (ABORT, "unknown code in `writestr'");
	}

	total_printed += printed;
	out += printed;
      }
    /* Save the current string length to the variable string length location if it exists */
    if (len1)
      *len1 = total_printed;

    va_end (p);
}

#define GPC_STRINGS
#define tst_EOLN(string)	(*(string) == NEWLINE)

static int _p_maxchars = 0;

#include "rts-rdsub.c"

/* read from a string using routines from rts-readsub.c */
void
_p_readstr (va_alist)
va_dcl
{
    register va_list p;
    char *ptr;
    char *string;
    int  *curlen;
    int   maxlen;

    int length;
    int failed = FALSE;

    /* Number of bytes consumed from the string */
    int count = 0;

    va_start (p);
    string = va_arg (p, char *); /* First arg is the string we read from    */
    curlen = va_arg (p, int *);  /* String curlen or null if fixed length   */
    maxlen = va_arg (p, int);    /* Max number of chars in string           */

    /* for subroutines so they can check reads past end of string */
    _p_maxchars = curlen ? *curlen : maxlen;

    /* then is the number of remaining opcodes */
    for (length = va_arg (p, int); length > 0; length--) {
	int code = va_arg (p, int);	/* Type we are reading */

	if (count >= _p_maxchars)
	  _p_error (ABORT,"Attempt to read past end of string with 'readstr'");

	/* COUNT contains the number of chars consumed from start of string */
	ptr = string + count;

	switch (code) {
	case P_INT:
	    { int *i = va_arg (p, int *);
	      if (failed)
		*i = 0;
	      else
		_p_readi (ptr, i, &count);
	      break;
	    }
	case P_CHAR:
	    { char *ch = va_arg (p, char *);
	      if (failed)
		  *ch = ' ';
	      else
		{
		  *ch = *ptr;
		  count++;
		}
	      break;
	    }
	case P_REAL:
	    { double *d = va_arg (p, double *);
	      float  *f = (float *) NULL;  /* @@ maybe make selectable later */
	      if (failed)
		  *d = 0.0;
	      else
		_p_readr (ptr, d, f, 1, &count);
	      break;
	    }

	case P_STRING:
	    { char *str     = va_arg (p, char *); /* pointer to string */
	      int  *curlen1 = va_arg (p, int *);  /* current string length */
	      int  maxlen1  = va_arg (p, int);    /* length to be read */
	      if (failed)
		{
		  *str = '\000';
		  if (curlen1)
		    *curlen1 = 0;
		}
	      else
		_p_reads (ptr, str, curlen1, maxlen1, &count);
	      break;
	    }

	default:
	    _p_error (ABORT, "unknown opcode in _p_readstr");
	}
	failed = count > _p_maxchars;
    }
    va_end (p);
}
