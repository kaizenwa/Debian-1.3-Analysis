/*****************************************************************************
 *
 * Mark Nagel <nagel@ics.uci.edu>
 * 20 July 1989
 *
 * $Revision: 1.1 $
 * Tom Kloos <tk@sequent.com>  Fixed a number of problems that actually
 *	caused bad output conversions for certain data values with certain
 *	valid format strings.  Note that there still isn't good (any?) error
 *	checking of the format string -- A bad format may print wrong values
 *	without warning (ie. ".#" won't show overflows).   28 Sep 92
 *
 * bool
 * format(fmt, num, buf, buflen)
 *  char *fmt;
 *  double num;
 *  char buf[];
 *  int buflen;
 *
 * The format function will produce a string representation of a number
 * given a _format_ (described below) and a double value.  The result is
 * written into the passed buffer -- if the resulting string is too
 * long to fit into the passed buffer, the function returns FALSE.
 * Otherwise the function returns TRUE.
 *
 * The fmt parameter contains the format to use to convert the number.
 *
 *  #	Digit placeholder.  If the number has fewer digits on either
 *      side of the decimal point than  there are '#' characters in
 *      the format, the extra '#' characters are ignored.  The number
 *      is rounded to the number of digit placeholders as there are
 *      to the right of the decimal point.  If there are more digits
 *      in the number than there are digit placeholders on the left
 *      side of the decimal point, then those digits are displayed.
 *
 *  0	Digit placeholder.  Same as for '#' except that the number
 *      is padded with zeroes on either side of the decimal point.
 *      The number of zeroes used in padding is determined by the
 *      number of digit placeholders after the '0' for digits on
 *      the left side of the decimal point and by the number of
 *      digit placeholders before the '0' for digits on the right
 *      side of the decimal point.
 *
 *  .	Decimal point.  Determines how many digits are placed on
 *      the right and left sides of the decimal point in the number.
 *      Note that numbers smaller than 1 will begin with a decimal
 *      point if the left side of the decimal point contains only
 *      a '#' digit placeholder.  Use a '0' placeholder to get a
 *      leading zero in decimal formats.
 *
 *  %	Percentage.  For each '%' character in the format, the actual
 *      number gets multiplied by 100 (only for purposes of formatting
 *      -- the original number is left unmodified) and the '%' character
 *      is placed in the same position as it is in the format.
 *
 *  ,	Thousands separator.  The presence of a ',' in the format
 *      (multiple commas are treated as one) will cause the number
 *      to be formatted with a ',' separating each set of three digits
 *      in the integer part of the number with numbering beginning
 *      from the right end of the integer.
 *
 *  \	Quote.  This character causes the next character to be
 *      inserted into the formatted string directly with no
 *      special interpretation.
 *
 *  E- E+ e- e+
 *	Scientific format.  Causes the number to formatted in scientific
 *	notation.  The case of the 'E' or 'e' given is preserved.  If
 *      the format uses a '+', then the sign is always given for the
 *	exponent value.  If the format uses a '-', then the sign is
 *	only given when the exponent value is negative.  Note that if
 *	there is no digit placeholder following the '+' or '-', then
 *	that part of the formatted number is left out.  In general,
 *	there should be one or more digit placeholders after the '+'
 *	or '-'.
 *
 *  ;	Format selector.  Use this character to separate the format
 *	into two distinct formats.  The format to the left of the
 *	';' character will be used if the number given is zero or
 *	positive.  The format to the right of the ';' character is
 *      used if the number given is negative.
 *    
 *  Any
 *	Self insert.  Any other character will be inserted directly
 *	into the formatted number with no change made to the actual
 *      number.
 *
 *****************************************************************************/

/*****************************************************************************/

#include <config.h>

#include <stdio.h>
#include <sys/types.h>
#include <math.h>
#include "sc.h"

#define bool	int
#define EOS	'\0'
#define MAXBUF	256


static char *	fmt_int PROTO((char *, char *, bool, bool));
static char *	fmt_frac PROTO((char *, char *));
static char *	fmt_exp PROTO((int, char *));
static void	reverse PROTO((char *));

#ifndef __STDC__
extern char *	strcpy();
extern char *	strcat();
#endif	/* __STDC__ */

/*****************************************************************************/

bool
format(fmt, val, buf, buflen)
  char *fmt;
  double val;
  char *buf;
  int buflen;
{
  register char *cp;
  char *tmp, *tp;
  bool comma = FALSE, negative = FALSE;
  char *integer = NULL, *decimal = NULL;
  char *exponent = NULL;
  int exp_val = 0;
  int width;
  char prtfmt[32];
  static char		*mantissa = NULL;
  static char		*tmpfmt1 = NULL, *tmpfmt2 = NULL, *exptmp = NULL;
  static unsigned	mantlen = 0, fmtlen = 0;
  char *fraction = NULL;
  int zero_pad = 0;

  if (fmt == NULL)
    return(TRUE);

  if (strlen(fmt) + 1 > fmtlen)
  {	fmtlen = strlen(fmt) + 40;
	tmpfmt1 = scxrealloc(tmpfmt1, fmtlen);
	tmpfmt2 = scxrealloc(tmpfmt2, fmtlen);
	exptmp = scxrealloc(exptmp, fmtlen);
  }
  fmt = strcpy(tmpfmt1, fmt);
  if (buflen + 1 > mantlen)
  {	mantlen = buflen + 40;
	mantissa = scxrealloc(mantissa, mantlen);
  }

  /*
   * select positive or negative format if necessary
   */
  for (cp = fmt; *cp != ';' && *cp != EOS; cp++)
  {
    if (*cp == '\\')
      cp++;
  }
  if (*cp == ';')
  {
    if (val < 0.0)
    {
      val = -val;     /* format should provide sign if desired */
      fmt = cp + 1;
    }
    else
    {
      *cp = EOS;
    }
  }
  
  /*
   * extract other information from format and produce a
   * format string stored in tmpfmt2 also scxmalloc()'d above
   */
  tmp = tmpfmt2;
  for (cp = fmt, tp = tmp; *cp != EOS; cp++)
  {
    switch (*cp)
    {
      case '\\':
        *tp++ = *cp++;
        *tp++ = *cp;
	break;

      case ',':
        comma = TRUE;
	break;

      case '.':
        if (decimal == NULL)
	  decimal = tp;
	*tp++ = *cp;
	break;
	
      case '%':
        val *= 100.0;
	*tp++ = *cp;
	break;
	
      default:
        *tp++ = *cp;
	break;
    }
  }
  *tp = EOS;
  fmt = tmpfmt2;

  if (val < 0.0)
  {	negative = TRUE;
	val = -val;
  }
  /*
   * extract the exponent from the format if present
   */
  for (cp = fmt; *cp != EOS; cp++)
  { if (*cp == '\\')
    {
      cp++;
    }
    else if (*cp == 'e' || *cp == 'E')
    {
      if (cp[1] == '+' || cp[1] == '-')
      {
	exponent = strcpy(exptmp, cp);
	*cp = EOS;
	exp_val = 0;
	if (val!=0.0) {
	  while (val < 1.0)
	  {
	    val *= 10.0;
	    exp_val--;
	  }
	  while (val >= 10.0)
	  {
	    val /= 10.0;
	    exp_val++;
	  }
	}
	break;
      }
    }
  }

  /*
   * determine maximum decimal places and use sprintf
   * to build initial character form of formatted value.
   */
  width = 0;
  if (decimal)
  {
    *decimal++ = EOS;
    for (cp = decimal; *cp != EOS; cp++)
    {
	if (*cp == '\\')
		cp++;
	else
	if (*cp == '#')
		width++;
	else
	if (*cp == '0')
		zero_pad = ++width;
	else
		break;
    }
    zero_pad = width - zero_pad;	/* Now really zeros to zap! */
  }
  (void) sprintf(prtfmt, "%%.%dlf", width);
  (void) sprintf(mantissa, prtfmt, val);
  for (cp = integer = mantissa; *cp != '.' && *cp != EOS; cp++)
  {
    if (*integer == '0')
      integer++;
  }
  if (*cp == '.')
  {
    fraction = cp + 1;
    *cp = EOS;
    cp = fraction + strlen(fraction) - 1;
    while ((zero_pad-- > 0) && (*cp == '0'))
	*cp-- = EOS;
  }

  /*
   * format the puppy
   */
  {
    static	char *citmp = NULL, *cftmp = NULL;
    static	unsigned cilen = 0, cflen = 0;
    char *ci, *cf, *ce;
    int len_ci, len_cf, len_ce;
    bool ret = FALSE;
    
    ci = fmt_int(integer, fmt, comma, negative);
    len_ci = strlen(ci);
    if (len_ci >= cilen)
    {	cilen = len_ci + 40;
	citmp = scxrealloc(citmp, cilen);
    }
    ci = strcpy(citmp, ci);

    cf = (decimal) ? fmt_frac(fraction, decimal) : "";
    len_cf = strlen(cf);
    if (len_cf >= cflen)
    {	cflen = len_cf + 40;
	cftmp = scxrealloc(cftmp, cilen);
    }
    cf = strcpy(cftmp, cf);

    ce = (exponent) ? fmt_exp(exp_val, exponent) : "";
    len_ce = strlen(ce);
/*
 * Skip copy assuming sprintf doesn't call our format functions
 *   ce = strcpy(scxmalloc((unsigned)((len_ce = strlen(ce)) + 1)), ce);
 */
    if (len_ci + len_cf + len_ce < buflen)
    {
      (void) sprintf(buf, "%s%s%s", ci, cf, ce);
      ret = TRUE;
    }

    return (ret);
  }
}

/*****************************************************************************/

static char *
fmt_int(val, fmt, comma, negative)
  char *val;	    /* integer part of the value to be formatted */
  char *fmt;	    /* integer part of the format */
  bool comma;	    /* TRUE if we should comma-ify the value */
  bool negative;    /* TRUE if the value is actually negative */
{
  int digit, f, v;
  int thousands = 0;
  char *cp;
  static char buf[MAXBUF];
  char *bufptr = buf;

  /*
   * locate the leftmost digit placeholder
   */
  for (cp = fmt; *cp != EOS; cp++)
  {
    if (*cp == '\\')
      cp++;
    else if (*cp == '#' || *cp == '0')
      break;
  }
  digit = (*cp == EOS) ? -1 : cp - fmt;

  /*
   * format the value
   */
  f = strlen(fmt) - 1;
  v = (digit >= 0) ? strlen(val) - 1 : -1;
  while (f >= 0 || v >= 0)
  {
    if (f > 0 && fmt[f-1] == '\\')
    {
      *bufptr++ = fmt[f--];
    }
    else if (f >= 0 && (fmt[f] == '#' || fmt[f] == '0'))
    {
      if (v >= 0 || fmt[f] == '0')
      {
        *bufptr++ = v < 0 ? '0' : val[v];
	if (comma && (thousands = (thousands + 1) % 3) == 0 && v > 0)
	{
	  *bufptr++ = ',';
	}
	v--;
      }
    }
    else if (f >= 0)
    {
      *bufptr++ = fmt[f];
    }
    if (v >= 0 && f == digit)
    {
      continue;
    }
    f--;
  }
    
  if (negative && digit >= 0)
    *bufptr++ = '-';
  *bufptr = EOS;
  reverse(buf);

  return (buf);
}

/*****************************************************************************/

static char *
fmt_frac(val, fmt)
  char *val;	    /* fractional part of the value to be formatted */
  char *fmt;	    /* fractional portion of format */
{
  static char buf[MAXBUF];
  register char *bufptr = buf;
  register char *fmtptr = fmt, *valptr = val;

  *bufptr++ = '.';
  while (*fmtptr != EOS)
  {
    if (*fmtptr == '\\')
    {
      *bufptr++ = *++fmtptr;
    }
    else if (*fmtptr == '#' || *fmtptr == '0')
    {
      if (*valptr != EOS || *fmtptr == '0')
      {
        *bufptr++ = (*valptr != EOS) ? *valptr++ : *fmtptr;
      }
    }
    else
    {
      *bufptr++ = *fmtptr;
    }
    fmtptr++;
  }
  *bufptr = EOS;

  return (buf);
}

/*****************************************************************************/

static char *
fmt_exp(val, fmt)
  int val;	    /* value of the exponent */
  char *fmt;	    /* exponent part of the format */
{
  static char buf[MAXBUF];
  register char *bufptr = buf;
  char valbuf[64];
  bool negative = FALSE;
  
  *bufptr++ = *fmt++;
  if (*fmt == '+')
    *bufptr++ = (val < 0) ? '-' : '+';
  else if (val < 0)
    *bufptr++ = '-';
  fmt++;
  *bufptr = EOS;

  if (val < 0)
  {
    val = -val;
    negative = FALSE;
  }
  (void) sprintf(valbuf, "%d", val);
  
  (void) strcat(buf, fmt_int(valbuf, fmt, FALSE, negative));
  return (buf);
}

/*****************************************************************************/

static void
reverse(buf)
  register char *buf;
{
  register char *cp = buf + strlen(buf) - 1;
  register char tmp;

  while (buf < cp)
  {
    tmp = *cp;
    *cp-- = *buf;
    *buf++ = tmp;
  }
}

/*****************************************************************************
 *  
 * Tom Anderson    <toma@hpsad.hp.com>
 * 10/14/90
 * David Fox - added a date format
 * Philemon W. Johnson <pjohnson@itd.nrl.navy.mil>
 * added exponent format, 19 July 92
 *
 * This routine takes a value and formats it using fixed, scientific,
 * engineering notation, date, or exponent (modulo 3).  The format command
 * 'f' determines which format is used.
 * The formats are:                          example
 *    0:   Fixed point (default)             0.00010
 *    1:   Scientific                        1.00E-04
 *    2:   Engineering                     100.00u
 *    3:   Date                              05/15/92
 *    4:   Exponent, modulo 3              100.E-06
 *
 * The format command 'f' now uses three values.  The first two are the
 * width and precision, and the last one is the format value 0 to 4 as
 * described above.  The format value is passed in the variable fmt.
 *
 * This formatted value is written into the passed buffer.  if the
 * resulting string is too long to fit into the passed buffer, the
 * function returns FALSE.  Otherwise the function returns true.
 *
 * When a number is formatted as engineering and is outside of the range
 * of typically used engineering exponents, the format reverts to
 * scientific.
 *
 * To preserve compatability with old spreadsheet files, the third value
 * may be missing, and the default will be fixed point (format 0).
 *
 * When an old style sheet is saved, the third value will be stored.
 *
 *****************************************************************************/

bool
engformat (fmt, width, lprecision, val, buf, buflen)
     int fmt, width, lprecision, buflen;
     double val;
     char *buf;
{
  static char engmult[] = "afpnum kMGT";

  int engind = 0;
  double engabs, engexp, engmant;

  if (buflen >= width)
  {
      switch (fmt)
      {
	case REFMTFLT:
	  (void) sprintf (buf, "%*.*E", width, lprecision, val);
	  break;
	case REFMTENG:
	  if (val == 0e0)	/* Hack to get zeroes to line up in engr fmt */
	      (void) sprintf (buf, "%*.*f ", width - 1, lprecision, val);
	  else
	  {
	      engabs = (val < 0e0) ? -val : val;
	      if ((1e-18 <= engabs) && (engabs < 1e-15)) engind = 0;
	      if ((1e-15 <= engabs) && (engabs < 1e-12)) engind = 1;
	      if ((1e-12 <= engabs) && (engabs < 1e-9)) engind = 2;
	      if ((1e-9 <= engabs) && (engabs < 1e-6)) engind = 3;
	      if ((1e-6 <= engabs) && (engabs < 1e-3)) engind = 4;
	      if ((1e-3 <= engabs) && (engabs < 1)) engind = 5;
	      if ((1 <= engabs) && (engabs < 1e3)) engind = 6;
	      if ((1e3 <= engabs) && (engabs < 1e6)) engind = 7;
	      if ((1e6 <= engabs) && (engabs < 1e9)) engind = 8;
	      if ((1e9 <= engabs) && (engabs < 1e12)) engind = 9;
	      if ((1e12 <= engabs) && (engabs < 1e15)) engind = 10;
	      if ((engabs < 1e-18) || (1e15 <= engabs))
		{
		  /* Revert to floating point */
		  (void) sprintf (buf, "%*.*E", width, lprecision, val);
		}
	      else
		{
		  engexp = (double) (engind - 6) * 3;
		  engmant = val / pow (10.0e0, engexp);
		  (void) sprintf (buf, "%*.*f%c", width - 1,
				  lprecision, engmant, engmult[engind]);
		}
	    }
	  break;
	case REFMTDATE:
	  {
	    int i;
	    char *temp_time;
	    long int secs;
	    /*
	     * sure hope that this is an 8 bit per character machine
	     * a defense against machines which do not cast
 	     * double to long properly
	     */
	    secs = ~(1 << ((8 * (long) sizeof (long)) - 1));
	    if (buflen < 9 || val < (double) -secs || (double) secs < val)
	      {
		for (i = 0; i < width; i++)
		  buf[i] = '*';
		buf[i] = '\0';
	      }
	    else
	      {
		secs = (time_t) val;
		temp_time = ctime (&secs);
		buf[0] = temp_time[8];
		buf[1] = temp_time[9];
		buf[2] = ' ';
		buf[3] = temp_time[4];
		buf[4] = temp_time[5];
		buf[5] = temp_time[6];
		buf[6] = ' ';
		buf[7] = temp_time[22];
		buf[8] = temp_time[23];
		for (i = 9; i < width; i++)
		  buf[i] = ' ';
		buf[i] = '\0';
	      }
	  }
	  break;
	case REFMTEXP:
	  /*
	   * sprintf the value val to the output buffer buf
	   */
	  (void) sprintf (buf, "%*.*E", width, lprecision, val);
	  if (lprecision > 1)	/* need 2 signifcant digits for this format */
	    {
	      int exponent;
	      char *pdecimal, *pexponent;
	      /*
	       * locate the decimal point and the exponent in buf
	       */
	      pdecimal = strchr (buf, '.');
	      pexponent = strrchr (buf, 'E');
	      pexponent++;
	      exponent = atoi (pexponent);	/* convert pexponent to int */
	      /*
	       * determine how many places to shift the decimal point
	       * handle negative exponents as a special case
	       * exponent will be reduced by 1 for each decimal shift
	       */
	      switch ((exponent < 0) ? -abs (exponent) % 3 : exponent % 3)
		{
		case -1:
		case 2:
		  /*
		   * need to shift decimal two places
		   */
		  *pdecimal = *(pdecimal + 1);
		  pdecimal++;
		  exponent--;
		case -2:
		case 1:
		  /*
		   * need to shift decimal one place
		   */
		  *pdecimal = *(pdecimal + 1);
		  *++pdecimal = 056;	/* write in a new decimal point */
		  exponent--;
		  /*
		   * write the adjusted exponent to buf
		   * verify if exponent is two or three digits
		   */
		  if (*(pexponent + 3) == '\0')	/* JEFFB compiler warning below */
		    (void) sprintf (pexponent, "%0+3.0d", exponent);
		  else
		    (void) sprintf (pexponent, "%0+4.0d", exponent);
		  break;
		case 0:
		default:
		  /*
		   * do not need to shift at all, just exit
		   */
		  break;
		}
	    }
	  break;
	case REFMTFIX:
	default:
	  (void) sprintf (buf, "%*.*f", width, lprecision, val);
	  break;
      }
      return (TRUE);
  }
  else
	return (FALSE);
}
