#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	sclmap.c (Scale Map)
 * Purpose:	Map image values to display screen values using various
 *		functions
 * Subroutine:	make_scalemap()			returns: void
 * Copyright:	1993 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  26 May 1989
 *              {2} Doug Mink    find range from histogram   10 November 1993
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* get stderr, NULL, etc. */
#include <math.h>		/* get the math definitions */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes  */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/scale.h"	/* define scaling constants */

#ifndef SUN
#define expm1(a)  (exp(a)-1)
#endif

/*
 * Subroutine:	make_scalemap
 * Purpose:	Make scale map according to type selected
 * Note:	Map goes from image value to hardware value (through ideal
 *		map value using pixels array from XAllocColors)
 */
void make_scalemap ( image_min, image_max )
     int image_min, image_max;	/* i: range of mapping for image data input */
{
  void histogram_equalize();
  static void linear_scale(), wrap_scale(), sqrt_scale(), log_scale();

  /* note the range of data for which mapping is to be calculated */
  if( image_max == image_min )
    ++image_max;
  if( image_max < image_min ) {
    int temp;
    temp = image_max;
    image_max = image_min;
    image_min = temp;
  }
  {
    /* fill in map below min with mincolor */
    register int imageval;
    register int pixval;
    register unsigned char *lookup;	/* l: scalemap base (signed index) */
    lookup = buffer.scalemap + SCALEOFF;
    pixval = color.pixvalmap[0];
    for( imageval = -32768; imageval < image_min; imageval++ )
      lookup[imageval] = pixval;
  }
  /* create the image map straight or with some form of scaling */
  switch( color.scale.mode ) {
  case SOP_HistEq:
    /* if there are levels to distribute, call the routine, else do the next */
    if( color.ncolors < (image_max - image_min) ) {
      histogram_equalize(buffer.scalemap + SCALEOFF,
			 buffer.histogram + SCALEOFF, buffer.hist_area,
			 image_min, image_max,
			 color.ncolors, color.pixvalmap);
      break;
    }
    /* fall through to linear if there is nothing to distribute */
  case SOP_Linear:
    linear_scale(image_min, image_max);
    break;
  case SOP_Wrap:
    wrap_scale(image_min, image_max);
    /* don't fill in the top of map */
    return;
  case SOP_Sqrt:
    sqrt_scale(image_min, image_max);
    break;
  case SOP_Log:
    log_scale(image_min, image_max);
    break;
  default:
    (void)fprintf(stderr,"Unknown scaling type request!\n");
  }
  {
    /* fill in top of map with maxcolor */
    register int imageval;
    register int pixval;
    register unsigned char *lookup;	/* l: scalemap base (signed index) */
    lookup = buffer.scalemap + SCALEOFF;
    pixval = color.pixvalmap[color.ncolors - 1];
    for( imageval = image_max; imageval < 32768; imageval++ )
      lookup[imageval] = pixval;
  }
}

/*
 * Subroutine:	linear_scale
 * Purpose:	Distribute color levels in the map evenly
 */
static void linear_scale ( image_min, image_max )
  int image_min, image_max;		/* i: limits of values in display */
{
  double scale;
  double upper_bound;
  int maxcolor;
  int level;
  int range_min, range_max;
  unsigned long *pixels;		/* l: ideal byte to hardare byte map */
  register unsigned char *lookup;	/* l: scalemap base (signed offsets) */
  register int imageval;
  register int pixval;
  register int imagelim;
  static void get_histogram_range();

  range_min = image_min;
  range_max = image_max;
  if (buffer.cmdMax <= buffer.cmdMin && !img.imtool_200) {
    get_histogram_range (buffer.histogram, &range_min, &range_max);
    }
  lookup = buffer.scalemap + SCALEOFF;
  maxcolor = color.ncolors - 1;
  pixels = color.pixvalmap;
  /* input range / output range yields input cells per ouput cell */
  scale = (double)(range_max - range_min + 1) / (double)color.ncolors;
  imageval = image_min;
  pixval = pixels[0];

  /* fill in at bottom if range_min > image_min */
  while( imageval < range_min )
    lookup[imageval++] = pixval;

  /* upper bound is ideal edge between colors (offset for rounding) */
  upper_bound = range_min + 0.5;
  level = 0;
  while( level++ < maxcolor ) {
    upper_bound += scale;
    imagelim = (int)upper_bound;
    while( imageval < imagelim )
      lookup[imageval++] = pixval;
    /* level was inc'd after loop test, make pixval for next round */
    pixval = pixels[level];
  }

  /* fill in at top if range_max  < image_max */
  while( imageval <= image_max )
    lookup[imageval++] = pixval;
}

/*
 * Subroutine:	wrap_scale
 * Purpose:	Create the image map with a repeating linear scale
 * Note:	Levels below image_min are not mapped
 */
static void wrap_scale ( image_min, image_max )
  int image_min, image_max;	/* i: limits of values in display */
{
  double scale;
  double range;
  int maxcolor;
  int level;
  unsigned long *pixels;		/* l: ideal byte to hardare byte map */
  register unsigned char *lookup;	/* l: scalemap base (signed offsets) */
  register int imageval;
  register int pixval;
  register int imagelim;

  lookup = buffer.scalemap + SCALEOFF;
  maxcolor = color.ncolors - 1;
  pixels = color.pixvalmap;
  /* input range / output range yields input cells per ouput cell */
  imagelim = (image_max - image_min + 1) / color.scale.wrap_cnt;
  scale = (double)imagelim / (double)color.ncolors;
  /* start at image_min with color level = 0 */
  imageval = image_min;
  level = 0;
  /* use range as floating point boundary between levels */
  range = (double)imageval;
  while( imageval < 32768 ) {
    do {
      /* level was inc'd after loop test, make pixval for next round */
      pixval = pixels[level];
      range += scale;
      imagelim = (int)(range + 0.5);
      if( imagelim >= 32768 ) 
	imagelim = 32767;
      do {
	lookup[imageval] = pixval;
      } while( ++imageval <= imagelim );
      if( imageval >= 32768 )
	return;
    } while( ++level <= maxcolor );
    /* pixval at level 0 lowest color again */
    level = 0;
  }
}

/*
 * Subroutine:	sqrt_scale
 * Purpose:	Distribute color levels in the map by a root or power function.
 * Method:	(level / maxlevel) ranges from 0 to 1.  Raise to a power.
 *		Result curves from 0 to 1.  Map result directly back to
 *		pixel value.
 */
static void sqrt_scale ( image_min, image_max )
  int image_min, image_max;		/* i: limits of values in display */
{
  double range;
  double power;
  double ncolors;
  int maxcolor;
  int level;
  unsigned long *pixels;		/* l: ideal byte to hardare byte map */
  register unsigned char *lookup;	/* l: scalemap base (signed offsets) */
  register int imageval;
  register int pixval;
  register int imagelim;

  lookup = buffer.scalemap + SCALEOFF;
  maxcolor = color.ncolors - 1;
  pixels = color.pixvalmap;
  power = color.scale.root_power;
  ncolors = (double)color.ncolors;
  range = image_max - image_min + 1;
  imageval = image_min;
  level = 0;
  /* pixval at level 0 is pixoffset (lshifted 0 is still 0) */
  pixval = pixels[0];
  while( level++ < maxcolor ) {
    imagelim = image_min + (int)
      ((pow(((double)level / ncolors), power) * range) + 0.5);
    /* limit map range to image values */
    if( imagelim > image_max )
      imagelim = image_max;
    while( imageval < imagelim )
      lookup[imageval++] = pixval;
    /* level was inc'd after loop test, make pixval for next round */
    pixval = pixels[level];
  }
  /* fill in at top if short of image_max */
  while( imageval <= image_max )
    lookup[imageval++] = pixval;
}

/*
 * Subroutine:	log_scale
 * Purpose:	Distribute color levels in the map by a logorithmic or
 *		exponential curve (powers of e).
 */
static void log_scale ( image_min, image_max )
  int image_min, image_max;		/* i: limits of values in display */
{
  double scale;
  double expo;
  double ncolors;
  int maxcolor;
  int level;
  unsigned long *pixels;		/* l: ideal byte to hardare byte map */
  register unsigned char *lookup;	/* l: scalemap base (signed offsets) */
  register int imageval;
  register int pixval;
  register int imagelim;

  lookup = buffer.scalemap + SCALEOFF;
  maxcolor = color.ncolors - 1;
  pixels = color.pixvalmap;
  expo = color.scale.log_expo;
  ncolors = (double)color.ncolors;
  /* base distribution on e**n as n goes from 0 to expo (expm1 is exp()-1) */
  if( color.scale.log_expo >= 0 ) {
    scale = (double)(image_max - image_min + 1) / expm1(expo);
  } else {
    /* negative exponents allocate more levels toward the high values */
    scale = (double)(image_max - image_min + 1) / (1.0 - exp(expo));
  }
  imageval = image_min;
  level = 0;
  /* pixval at level 0 is pixoffset (lshifted 0 is still 0) */
  pixval = pixels[0];
  while( level++ < maxcolor ) {
    if( expo > 0 ) {
      imagelim = image_min + (int)
	((expm1(((double)level / ncolors) * expo) * scale) + 0.5);
    } else {
      imagelim = image_min + (int)
	((1.0-exp(((double)level / ncolors) * expo) * scale) + 0.5);
    }
    /* limit map range to image values */
    if( imagelim > image_max )
      imagelim = image_max;
    while( imageval < imagelim )
      lookup[imageval++] = pixval;
    /* level was inc'd after loop test, make pixval for next round */
    pixval = pixels[level];
  }
  /* fill in at top if short of image_max */
  while( imageval <= image_max )
    lookup[imageval++] = pixval;
}

/*
 * Subroutine:	get_histogram_range
 * Purpose:	Find a range from the peak of the pixel value distribution
 */
static void get_histogram_range ( histogram, imin, imax)
     int *histogram;
     int *imin;		/* minimum pixel value */
     int *imax;		/* maximum pixel value */
{
  register int *hist;
  register int vmin;
  register int i;
  register int minhist, maxhist;
  float lfrac;
  int vpeak;
  int ipeak, ppeak;
  int pmin, pmax;
  int hwidth,hoff;

  if (buffer.lfrac <= 0 || buffer.lfrac >= 1.)
    return;

  /* offset hist to zero (+32768 or 0x8000)  */
  hist = histogram + SCALEOFF;

  /* find histogram peak */
  maxhist = SCALEMAX;
  minhist = SCALEMIN;
  vpeak = 0;
  ipeak = 0;
  for( i = minhist; (i <= maxhist); i++ ) {
    if (hist[i] > vpeak) {
      ipeak = i;
      vpeak = hist[i];
      }
    }

  /* Convert peak from histogram bin to pixel value */
  if ( img.fiscaled )
    ppeak = ((float) ipeak * img.fiscale) + img.fibias;
  else
    ppeak = ipeak;

  /* Find first pixel value at peak of histogram - lfrac */
  i = ipeak;
  if ( buffer.lfrac > 0 )
    lfrac = buffer.lfrac;
  else
    lfrac = 0.0;
  vmin = hist[ipeak] * lfrac;
  *imin = minhist;
  for (i = ipeak; i > minhist; i--) {
    if (hist[i] > 0 && hist[i] < vmin) {
      *imin = i+1;
      break;
      }
    }

  /* Convert range minimum from histogram bin to pixel value */
  if ( img.fiscaled )
    pmin = ((float) *imin * img.fiscale) + img.fibias;
  else
    pmin = *imin;

  /* Find last pixel value at peak of histogram - lfrac */
  *imax = maxhist;
  for (i = ipeak; i < maxhist; i++) {
    if (hist[i] > 0 && hist[i] < vmin) {
      *imax = i - 1;
      break;
      }
    }

  /* Convert range maximum from histogram bin to pixel value */
  if ( img.fiscaled )
    pmax = ((float) *imax * img.fiscale) + img.fibias;
  else
    pmax = *imax;

  if (control.verbose) {
    (void)printf("Linear display autoscaled from %d to %d\n", pmin, pmax);
    (void)printf("Limits at %5.3f of most common value = %d\n", lfrac, ppeak);
    }

  return;
}
