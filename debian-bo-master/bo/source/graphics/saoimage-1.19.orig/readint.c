#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	readint.c (Read Integer)
 * Purpose:	Scale fixed point image data to fit in a short (int*2) buffer
 * Subroutine:	scale_data_u1()			returns: void
 * Subroutine:	scale_data_i2()			returns: void
 * Subroutine:	scale_data_u2()			returns: void
 * Subroutine:	scale_data_i4()			returns: void
 * Xlib calls:	none
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	      31 October 1988
 *		{1} Juha Salo (kali@altair.utu.fi) scale_data_u1  17 Oct 1989
 *              {2} Stephan Jansen scale_data_i4 (overflow fix)    8 Dec 1989
 *                         jansen%madraf.decnet@vms.macc.wisc.edu
 *		{3} Martin Bly (Starlink) scale_data_i4 for OSF/1 31 Jan 1995
 *                          (ussc@star.rl.ac.uk)
 *		{4} Doug Mink (SAO) change ALPHA flag to LONG64    5 May 1995
 *		{5} Doug Mink (SAO) use bswap4 instead of nhtol   18 Jun 1995
 *		{n} <who> -- <does what> -- <when>
 */

#include <sys/types.h>		/* needed for ntohl (net to host long) */
#ifndef VMS
#include <netinet/in.h>		/* needed for ntohl (net to host long) */
#endif
#include "hfiles/scale.h"	/* define SCALEWIDTH, etc. */
#include "hfiles/image.h"	/* image struct */

static void byte_swap();

/*
 * Subroutine:	scale_data_u1
 * Purpose:	Read an 8-bit array into a 16-bit buffer
 * Method:	Copy from end so byte data may occupy beginning of same buf
 */

void
scale_data_u1 ( image, imgbuf, databuf, vals )
     struct imageRec *image;
     register short *imgbuf;
     register unsigned char *databuf;
     int vals;
{
  databuf += (vals - 1);
  imgbuf += (vals - 1);
  /* and pad the picture */
  while( vals-- > 0 )
    *imgbuf-- = (short)*databuf--;
  image->fiscaled = 0;
  image->fibias = 0.0;
  image->fiscale = 1.0;
}

/*
 * Subroutine:	scale_data_i2
 * Purpose:	Scale 16-bit signed short data into the (short) img array
 * Method:	Data is assumed to be already in the buffer.  Only byte-swap
 *		might be needed.
 */

void
scale_data_i2 ( image, imgbuf, databuf, vals )
     struct imageRec *image;
     short *imgbuf;
     short *databuf;
     int vals;
{

  /* do byte swap if called for */
  if( image->byte_swap )
    byte_swap((char *)databuf, (char *)imgbuf, vals * sizeof(short));
  image->fiscaled = 0;
  image->fibias = 0.0;
  image->fiscale = 1.0;
}

/*
 * Subroutine:	scale_data_u2
 * Purpose:	Scale 16 bit unsigned short data into the (short) img array
 */

void
scale_data_u2 ( image, imgbuf, databuf, vals )
     struct imageRec *image;
     register short *imgbuf;
     register unsigned short *databuf;
     int vals;
{
  register unsigned short *dataend;
  register int bias;

  bias = -SCALEOFF;
  /* do byte swap if called for */
  if( image->byte_swap )
    byte_swap((char *)databuf, (char *)imgbuf, vals * sizeof(short));
  /* start from back end since data may occupy beginning of buf */
  dataend = databuf + vals;
  do {
    *imgbuf++ = (short)((int)*databuf + bias);
  } while( ++databuf < dataend );
  image->fiscaled = 1;
  image->fibias = -(double)bias;
  image->fiscale = 1.0;
}

/*
 * Subroutine:	scale_data_i4
 * Purpose:	Scale 32 bit integer data into the (short) img array
 */

void
scale_data_i4 ( image, imgbuf, databuf, vals, verbose )
     struct imageRec *image;
     short *imgbuf;
#ifdef LONG64
     int *databuf;
#else
     long *databuf;
#endif
     int vals;
     int verbose;		/* whether to print explanatory messages */
{
#ifdef LONG64
  register int *lbuf, *lbufend;
#else
  register long *lbuf, *lbufend;
#endif
  int datamin, datamax;

  {
    register int lmin, lmax;
    lbuf = databuf;
    lbufend = lbuf + vals;
    /* find the min and the max (byteswap if needed) */
    if( image->byte_swap ) {
      bswap4 ((char *)lbuf);
      lmin = lmax = *lbuf;
      /* skip the first val since we just used it */
      while( ++lbuf < lbufend ) {
        bswap4 ((char *)lbuf);
	if( *lbuf < lmin )
	  lmin = *lbuf;
	else if( *lbuf > lmax )
	  lmax = *lbuf;
      }
    } else {
      lmin = lmax = *lbuf;
      while( ++lbuf < lbufend ) {
	if( *lbuf < lmin )
	  lmin = *lbuf;
	else if( *lbuf > lmax )
	  lmax = *lbuf;
      }
    }
    datamin = lmin;
    datamax = lmax;
  }
  /* make announcement if requested */
  if( verbose ) {
    (void)printf("min and max as read: %d, %d\n", datamin, datamax);
    if( image->fimin < image->fimax ) {
      (void)printf("using given limits: %d, %d\n",
		   (int)image->fimin, (int)image->fimax);
    }
  }
  /* apply preset limits if given */
  if( image->fimin < image->fimax ) {
    if( image->fimin < 0.0 )
      datamin = (int)(image->fimin - 0.5);
    else
      datamin = (int)(image->fimin + 0.5);
    if( image->fimax < 0.0 )
      datamax = (int)(image->fimax - 0.5);
    else
      datamax = (int)(image->fimax + 0.5);
  }  
  {
    register short *sbuf;
    register long bias;
    double scale;
    register int smin, smax;

    /*
     *  I inserted a number of (float) conversions to stop the problem
     *  with overflow.
     *  Stephan Jansen
     */

    smin = SCALEMIN;
    smax = SCALEMAX;
    /* reset buf pointer for another pass through lbuf */
    lbuf = databuf;
    sbuf = imgbuf;
    if( ((double)datamax - (double)datamin) <= (double)SCALEWIDTH ) {
      /* if a simple offset suffices */
      register long ltemp;
      if( (datamin >= SCALEMIN) && (datamax <= SCALEMAX) ) {
	/* if possible to truncate to short without bias, do so. */
	bias = 0;
	image->fiscaled = 0;
	image->fibias = 0.0;
      } else {
	/* offset by average to center within range */
	bias = -((datamin + datamax) / 2);
	image->fiscaled = 1;
	image->fibias = (double)(-bias);
      }
      image->fiscale = 1.0;
      do {
	ltemp = *lbuf + bias;
	if( ltemp < smin ) {
	  *sbuf++ = smin;
	} else if( ltemp > smax ) {
	  *sbuf++ = smax;
	} else
	  *sbuf++ = (short)ltemp;
      } while( ++lbuf < lbufend );
    } else {
      /* full-up scaling required. (+/- (tmax-tmin)/2) */
      double dtemp, dmin, dmax, dbias;
      /* offset values to be zero centered */
      dbias = -(((double)datamin + (double)datamax) * 0.5);
      /* get the scale factor (no divide by zero gets past prior test) */
      scale = (double)SCALEWIDTH / ((double)datamax - (double)datamin);
      /* use min and max to mark out limits as float values */
      dmin = (double)SCALEMIN;
      dmax = (double)SCALEMAX;
      /* scale the picture */
      do {
	dtemp = ((double)(*lbuf) + dbias) * scale;
	if( dtemp < 0.0 ) {
	  dtemp -= 0.5;
	  if( dtemp < dmin )
	    *sbuf++ = smin;
	  else
	    *sbuf++ = (short)dtemp;
	} else {
	  dtemp += 0.5;
	  if( dtemp > dmax )
	    *sbuf++ = smax;
	  else
	    *sbuf++ = (short)dtemp;
	}
      } while( ++lbuf < lbufend );
      image->fiscaled = 1;
      image->fibias = -dbias;
      image->fiscale = 1.0 / scale;
    }
  }
}

/*
 * Subroutine:	byte_swap
 * Purpose:	Equivalent of UNIX swab, but guaranteed to work in place
 *		and usable under VMS
 */

static void
byte_swap( inbuf, outbuf, nbytes )
     char *inbuf;
     char *outbuf;
     int nbytes;
{
  register char *from, *to, *last_byte;
  register unsigned temp;

  from = inbuf;
  to = outbuf;
  last_byte = from + nbytes;
  while( from < last_byte ) {
    temp = *from;
    ++from;
    *to = *from;
    ++to;
    *to = temp;
    ++to;
    ++from;
  }
}
