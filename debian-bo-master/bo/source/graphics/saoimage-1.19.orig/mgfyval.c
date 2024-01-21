#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mgfyval.c (Magnify Value)
 * Purpose:	Get file pixel value and make a formatted string to show it
 * Subroutine:	get_pixel_val()		returns: int
 * Subroutine:	integer_string()	returns: void
 * Subroutine:	real_string()		returns: void
 * Copyright:	1990 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Note:	Table based on code from Bill Wyatt's showimg
 * Modified:	{0} Michael VanHilst	initial version	     21 February 1990
 *		{1} MVH improved G formatter (c)MVH		   1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/*  Get stderr  */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/*  strlen, strcat, strcpy, strrchr  */
#else
#include <strings.h>		/*  strlen, strcat, strcpy, rindex  */
#define strchr index		/*  Needed for a few unenlightened BSD's  */
#define strrchr rindex
#endif
#else
#include <string.h>		/*  strlen, strcat, strcpy, strrchr  */
#endif

#include <X11/Xlib.h>		/*  X window stuff  */
#include <X11/Xutil.h>		/*  X window manager stuff  */
#include "hfiles/struct.h"	/*  Declare structure types  */
#include "hfiles/constant.h"	/*  Define ARR_ codes  */
#include "hfiles/extern.h"	/*  Extern main parameter structures  */

#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

int		get_pixel_val(	int bufx, int bufy,
				int* ival, double* dval, int* clip);
void		integer_string(	int ival, int clip, char *string, int width);
void		real_string(	double val, char *string, int width);
static void	mark_overflow(	char *string, int string_width,
				char* label, int label_width);

#else

  void i_transform();
  int get_pixel_val();
  void integer_string(), real_string();
  static void mark_overflow();

#endif


/*  Subroutine:	get_pixel_val
 *  Purpose:	Return pixel value and code representing best format
 *  Returns:	1 if value is an integer, else 0
 *  Note:	Puts integer val in ival
 *  Note:	If float, double or scaled, puts float val in dval
 *  Note:	Sets clip if buffer value is at clipped value edge (IRAF)
 */
#ifdef ANSIC
int get_pixel_val( int bufx, int bufy, int* ival, double* dval, int* clip )
#else
int get_pixel_val ( bufx, bufy, ival, dval, clip )
     int bufx, bufy;	/* shortbuf ("buf") coordinates */
     int *ival;		/* gets integer value */
     double *dval;	/* gets floating point value (unless just integer) */
     int *clip;		/* -1 if at clipping min, 1 if at clipping max */
#endif
{
  int val;

  *clip = 0;
  if( img.fiscaled == 0 ) {
    *ival = buffer.shortbuf[(bufy * coord.buf.width) + bufx];
    return( 1 );
  } else {
    if( (buffer.filebuf == NULL) ||
        (buffer.filebuf == (char *)buffer.shortbuf) ) {
      /* values scaled, originals not available */
      val = buffer.shortbuf[bufx + (bufy * coord.buf.width)];
      *dval = ((double)val * img.fiscale) + img.fibias;
      /*  Print strings with spaces padding out the end  */
      if( val <= buffer.clipmin )
	*clip = -1;
      else if( val >= buffer.clipmax )
	*clip = 1;
    } else {
      /*  Values scaled, originals in filebuf  */
      float fbX, fbY;
      i_transform(&coord.buftofbuf, bufx, bufy, &fbX, &fbY);
      if( img.storage_type == ARR_I4 ) {
	*dval = (double)
	  *((int *)(buffer.filebuf +
		    (((int)fbX + ((int)fbY * coord.fbuf.width)) *
		     sizeof(int))));
      } else if( img.storage_type == ARR_R4 ) {
	*dval = (double)
	  *((float *)(buffer.filebuf +
		      (((int)fbX + ((int)fbY * coord.fbuf.width)) *
		       sizeof(float))));
      } else if( img.storage_type == ARR_R8 ) {
	*dval = *((double *)(buffer.filebuf +
			    (((int)fbX + ((int)fbY * coord.fbuf.width)) *
			     sizeof(double))));
      } else
	*dval = 0.0;
      if( img.fscaled )
	*dval = img.fbias + (*dval * img.fscale);
    }
    *ival = (int)(*dval);
    if( ((double)(*ival)) == *dval )
      return( 1 );
    else
      return( 0 );
  }
}


static char *iform[16] = { "x", "%d", "%2d", "%3d", "%4d", "%5d", "%6d",
			     "%7d", "%8d", "%9d", "%10d", "%11d", "%12d",
			     "%13d", "%14d", "%15d" };
/*  Subroutine:	integer_string
 *  Purpose:	Create an integer string, include clipping mark if appropriate
 *  Note:	to be called with full column width
 *  Note:	2<=width<=15
 */
#ifdef ANSIC
void integer_string( int ival, int clip, char *string, int width )
#else
void integer_string ( ival, clip, string, width )
     int ival;
     int clip;
     char *string;
     int width;
#endif
{
  char *edge;

  (void)sprintf(string, iform[width], ival);
  if( clip ) {
    if( string[0] != ' ' ) {
      if( width > 8 )
	real_string((double)ival, &string[1], width-1);
      else
	mark_overflow(string, width, "CLIP", 4);
      edge = string;
    } else
      edge = strrchr(string, ' ');
    /*  If value known to be clipped, indicate true value is beyond this  */
    if( clip > 0 )
      *edge = '>';
    else
      *edge = '<';
  } else {
    if( string[width] != '\0' ) {
      if( width > 8 ) {
	real_string((double)ival, &string[1], width-1);
	*string = ' ';
      } else
	mark_overflow(string, width, "ovfl", 4);
    }
  }
}


/*  Subroutine:	mark_overflow
 *  Purpose:	Print overflow message in place of value
 */
#ifdef ANSIC
static void mark_overflow( char *string, int string_width,
			   char* label, int label_width )
#else
static void mark_overflow ( string, string_width, label, label_width )
     char *string;
     int string_width;
     char *label;
     int label_width;
#endif
{
  int i;

  *string = ' ';
  for( i=0; i<label_width; i++ )
    string[i+1] = label[i];
  for( i=label_width+1; i<string_width; i++ )
    string[i] = ' ';
  string[string_width] = '\0';
}


static double emax[17] = { 1.0, 10.0, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8,
			   1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e17 };
static double emin[18] = {  0.0,  -1.0, -10.0,  -1e2,  -1e3,  -1e4,
			   -1e5,  -1e6,  -1e7,  -1e8,  -1e9, -1e10,
			  -1e11, -1e12, -1e13, -1e14, -1e15, -1e17 };
static char *fform[16] = { "%.0f", "%.1f", "%.2f", "%.3f", "%.4f", "%.5f",
			     "%.6f", "%.7f", "%.8f", "%.9f", "%.10f",
			     "%.11f", "%.12f", "%.13f", "%.14f", "%.15f" };
static char *eform[16] = { "%.0e", "%.1e", "%.2e", "%.3e", "%.4e", "%.5e",
			     "%.6e", "%.7e", "%.8e", "%.9e", "%.10e",
			     "%.11e", "%.12e", "%.13e", "%.14e", "%.15e" };
/*  Subroutine:	real_string
 *  Purpose:	Print the real value in best format for character space given
 *  Note:	8<=width<=15 (7 makes -1e-10\0, 16 goes beyond lookup arrays)
 *  Note:	width characters beyond string[width] my get messed up
 *  Method:	For speed, compares and formats get values through lookups
 */
#ifdef ANSIC
void real_string( double val, char *string, int width )
#else
void real_string ( val, string, width )
     double val;	/* value to print */
     char *string;	/* string to recieve number (see note 2 above) */
     int width;		/* i: number of characters to use */
#endif
{
  if( val >= 0.0 ) {
    if( (val < 0.001) || (val >= emax[width-1]) ) {
      /*  Outside range for simple %f format  */
      if( (val >= 1e-9) && (val < 1e10) ) {
	/*  Inside range for 1 digit exponent (move digit 2 over)  */
	if( val >= 1.0 ) {
	  /*  Positive number with 1 digit positive exponent  */
	  (void)sprintf(string, eform[width-4], val);
	  string[width-1] = string[width+1];
	} else {
	  /*  Positive number with 1 digit negative exponent  */
	  (void)sprintf(string, eform[width-5], val);
	  string[width-1] = string[width];
	}
      } else if( val >= 1.0 ) {
	/*  Positive number with 2 digit positive exponent  */
	(void)sprintf(string, eform[width-5], val);
	string[width-2] = string[width-1];
	string[width-1] = string[width];
      } else
	/*  Positive number with 2 digit negative exponent  */
	(void)sprintf(string, eform[width-6], val);
    } else
      /*  Positive number in fixed point notation  */
      (void)sprintf(string, fform[width], val);
  } else {
    if( (val > -0.001) || (val <= emin[width]) ) {
      if( (val <= -1e-9) && (val > -1e10) ) {
	/*  Inside range for 1 digit exponent (move digit 2 over)  */
	if( val > -1.0 ) {
	  /*  Negative number with 1 digit negative exponent  */
	  (void)sprintf(string, eform[width-6], val);
	  string[width-1] = string[width];
	} else {
	  /*  Negative number with 1 digit positive exponent  */
	  (void)sprintf(string, eform[width-5], val);
	  string[width-1] = string[width+1];
	}
      } else if( val <= -1.0 ) {
	/*  Negative number with 2 digit positive exponent  */
	(void)sprintf(string, eform[width-6], val);
	string[width-2] = string[width-1];
	string[width-1] = string[width];
      } else
	/*  Negative number with 2 digit negative exponent  */
	(void)sprintf(string, eform[width-7], val);
    } else
      /*  Negative number in fixed point notation  */
      (void)sprintf(string, fform[width], val);
  }
  string[width] = '\0';
}
