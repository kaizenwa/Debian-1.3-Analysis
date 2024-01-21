#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	readfith.c (Read FITS Header)
 * Purpose:	Parse FITS header for specific parameter fields
 * Subroutine:	read_fitsheader()			returns: void
 * Subroutine:	no_fitscomment()			returns: void
 * Copyright:	1989,1990,1995 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	      23 January 1989
 *		{1} Tim Cornwell (NRAO) not reject BITPIX < 0	   1 Feb 1990
 *              {1} MVH BSDonly strings.h compatability           19 Feb 1990
 *              {2} Doug Mink (SAO) initialize WCS                 7 Jul 1995
 *              {3} Doug Mink (SAO) print WCS info                16 Oct 1995
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* get stderr, NULL, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, strcat, strcpy, rindex */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define MIN, MAX, etc. */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structure */

/*
 * Subroutine:	read_fitsheader
 * Purpose:	Find important FITS parameters in FITS header
 * Returns:	1 if success, else 0
 */
int read_fitsheader ( header, length, bitpix, naxis, naxes, scale, bias )
     char *header;
     int length;
     int *bitpix;
     int *naxis;
     int *naxes;
     float *scale;
     float *bias;
{
  static int get_keyint(), get_keyfloat();
  struct WorldCoor *wcsinit();
  char *header0;
  header0 = header;

  /* first card is assumed to be SIMPLE = T, so skip it */
  header += 80;
  if( (*bitpix = get_keyint(header,"BITPIX  ", 80, 1)) == 0 )
    return(0);
  if( (*bitpix % 8) != 0 ) {
    (void)fprintf(stderr,
		  "BITPIX = %d: not 8, 16, 32, -16, -32, or -64\n", *bitpix);
    return(0);
  }
  header += 80;
  if( (*naxis = get_keyint(header, "NAXIS   ", 80, 1)) <= 0 )
    return(0);
  header += 80;
  if( (naxes[0] = get_keyint(header, "NAXIS1  ", 80, 1)) <= 0 )
    return(0);
  header += 80;
  if( (naxes[1] = get_keyint(header, "NAXIS2  ", 80, 1)) <= 0 )
    return(0);
  header += 80;
  length -= 400;
  if( *naxis > 2 ) {
    if( (naxes[2] = get_keyint(header, "NAXIS3  ", 80, 1)) <= 0 )
      return(0);
    header += 80;
    length -= 80;
  }
  if( get_keyfloat(header, "BZERO   ", length, bias, 0) == 0 )
    *bias = 0.0;
  if( get_keyfloat(header, "BSCALE  ", length, scale, 0) == 0 )
    *scale = 1.0;
  wcs = wcsinit (header0);
  if (control.verbose)
    wcscent (wcs);
  wcscominit (wcs, &wcscommand);
  wcsoutinit (wcs, &wcscoor);
  return(1);
}

/*
 * Subroutine:	get_keyint
 * Purpose:	Return the int value in the data field for a given FITS
 *		header keyword.  If key not found, return 0.
 */
static int get_keyint ( header, keyword, length, report_error)
     char *header;	/* buffer start */
     char *keyword;	/* keyword to match */
     int length;	/* if zero, search up to "END" keyword */
     int report_error;	/* if > 0, fatal if key not found */
{
  int key_not_end, val;
  int i;
  void no_fitscomment();

  key_not_end = (strncmp(keyword, "END     ", 8) != 0);
  for( i=0; i<length; i+=80 ) {
    /* check for END keyword marking end of header, unless END is the key */
    if( key_not_end && strncmp(header+i,"END     ",8) == 0 )
      break;
    /* check for desired keyword */
    if( strncmp(header+i,keyword,8) == 0 ) {
      no_fitscomment (header+i+10, 20);
      if( (sscanf(header+i+10,"%d", &val) != 1) || (val == 0) ) {
	(void)fprintf(stderr,
		      "Bad integer value for %s keyword in FITS header\n",
		      keyword);
	return( 0 );
      }
      return( val );
    }
  }
  if( report_error )
    (void)fprintf(stderr, "No `%s' keyword in FITS header\n", keyword);
  return( 0 );
}

/*
 * Subroutine:	get_keyfloat
 * Purpose:	Return the float value in the data field for a given FITS
 *		header keyword.  If key not found, return 0.
 */
static int get_keyfloat ( header, keyword, length, val, report_error)
     char *header;	/* buffer start */
     char *keyword;	/* keyword to match */
     int length;	/* if zero, search up to "END" keyword */
     float *val;	/* float to recieve value if found */
     int report_error;	/* if > 0, fatal if key not found */
{
  int key_not_end;
  int i;
  static void fix_exponent();
  void no_fitscomment();

  key_not_end = (strncmp(keyword, "END     ", 8) != 0);
  for( i=0; i<length; i+=80  ) {
    /* check for END keyword marking end of header, unless END is the key */
    if( key_not_end && strncmp(header+i,"END     ",8) == 0 )
      break;
    /* check for desired keyword */
    if( strncmp(header+i,keyword,8) == 0 ) {
      no_fitscomment (header+i+10, 20);
      fix_exponent (header+i+10, 20);
      if( sscanf(header+i+10,"%f", val) != 1 ) {
	(void)fprintf(stderr,
		      "Bad numerical value for %s keyword in FITS header\n",
		      keyword);
	return( 0 );
      }
      return( 1 );
    }
  }
  if( report_error )
    (void)fprintf(stderr, "No `%s' keyword in FITS header\n", keyword);
  return( 0 );
}

/*
 * Subroutine:	no_fitscomment
 * Purpose:	Terminate the data field at an comment indicator.  The FITS
 *		standard allows comments immediately after any data.
 */
void no_fitscomment ( line, count )
     char *line;
     int count;
{
  int i;
  for( i=0; i<count; i++ )
    if( line[i] == '/' ) line[i] = '\0';
}

/*
 * Subroutine:	fix_exponent
 * Purpose:	Change misguided exponents using D to use E.  This problem
 *		reportedly appears in output from some IRAF tasks.
 */
static void fix_exponent ( line, count )
     char *line;
     int count;
{
  int i;
  for( i = 0; (i < count) && (line[i] != '\0'); i++ ) {
    if( line[i] == 'D' ) line[i] = 'E';
    if( line[i] == 'd' ) line[i] = 'e';
  }
}
