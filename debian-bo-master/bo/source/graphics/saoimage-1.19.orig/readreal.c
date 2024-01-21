#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	readreal.c (Read Real)
 * Purpose:	Scale real image data to fit in a short (int*2) buffer
 * Subroutine:	scale_data_r4()			returns: void
 * Subroutine:	scale_data_r8()			returns: void
 * Xlib calls:	none
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     31 October 1988
 *		{1} Doug Mink		added byte swapping  May 2, 1994 
 *		{2} Robert Wilson	Put in code for NaN  July 7, 1995
 *		{n} <who> -- <does what> -- <when>
 */

#include "hfiles/image.h"	/* image struct */
#include "hfiles/scale.h"	/* define SCALEWIDTH, etc. */

/*
 * Subroutine:	scale_data_r4
 * Purpose:	scale 32 bit real data into the (short) img array
 */
void scale_data_r4 ( image, imgbuf, databuf, vals, verbose )
     struct imageRec *image;
     short *imgbuf;
     float *databuf;
     int vals;
     int verbose;		/* whether to print explanatory messages */
{
  register float *fbuf, *fbufend;
  float fmin, fmax;
  int swap = image->byte_swap, started = 0;

  /* set buf start and end pointers for a pass through databuf.
   * This pass will do the byte swapping if needed and find fmin and fmax
   */
  fbuf = databuf;
  fbufend = fbuf + vals;
  for(; fbuf < fbufend; fbuf++) {
    if(swap)
      bswap4 ((char *)fbuf);
    if(! started) {
      if(*fbuf == *fbuf) {  /* A NaN doesn't equal anything, not even itself */
	fmin = fmax = *fbuf;
	started = 7;
      }
    } else if(*fbuf == *fbuf) {
      if( *fbuf < fmin )
        fmin = *fbuf;
      else if( *fbuf > fmax )
        fmax = *fbuf;
    }
  }
  if(!started) {
    printf("No valid pixels in this image\n");
    fmin = fmax = 0.0;
  }

  /* make announcement if requested */
  if( verbose ) {
    (void)printf("Data min and max as read: %g, %g\n", fmin, fmax);
    if( image->fimin < image->fimax ) {
      (void)printf("Using given limits: %g, %g\n", image->fimin, image->fimax);
    }
  }

  /* apply preset limits if given */
  if( image->fimin < image->fimax ) {
    fmin = image->fimin;
    fmax = image->fimax;
  }  
  {
    float scale, bias;
    float ftemp;
    register short *sbuf;

    /* set bias to offset values to zero center the range */
    bias = -((fmin + fmax) / (float)2.0);
    /* get the scale factor */
    if( (fmax - fmin) > 0.0 ) {
      scale = (float)SCALEWIDTH / (fmax - fmin);
    } else {
      scale = 1.0;
    }
    /* reset buf for another pass through fbuf */
    /* while loop changed to current form from:
     * buf = databuf;
     * while( buf < bufend )
     *   *sbuf RND((*buf++ + bias) * scale;
     * because Sun compiler was inc'ing buf by 8 bytes */
    fbuf = databuf;
    sbuf = imgbuf;
    /* use min and max to mark out limits */
    fmin = (float)SCALEMIN;
    fmax = (float)SCALEMAX;
    /* scale the picture */
    do {
      if(*fbuf == *fbuf) {	/* Pixel is not NaN */
	ftemp = (*fbuf + bias) * scale;
	if( ftemp < 0.0 ) {
	  ftemp -= 0.5;
	  if( ftemp < fmin )
	    *sbuf++ = fmin;
	  else
	    *sbuf++ = (short)ftemp;
        } else {
	  ftemp += 0.5;
	  if( ftemp > fmax )
	    *sbuf++ = fmax;
	  else
	    *sbuf++ = (short)ftemp;
        }
      } else {			/* Pixel is NaN */
	*sbuf++ = -SCALEOFF;
      }
    } while( ++fbuf < fbufend );
    image->fiscaled = 1;
    image->fibias = (double)(-bias);
    image->fiscale = 1.0 / (double)scale;
  }
}

/*
 * Subroutine:	scale_data_r8
 * Purpose:	Scale 64 bit real data into the (short) img array
 */
void scale_data_r8 ( image, imgbuf, databuf, vals, verbose )
     struct imageRec *image;
     short *imgbuf;
     double *databuf;
     int vals;
     int verbose;		/* whether to print explanatory messages */
{
  register double *dbuf, *dbufend;
  double dmin, dmax;
  int swap = image->byte_swap, started = 0;

  /* set buf start and end pointers for a pass through databuf.
   * This pass will do the byte swapping if needed and find fmin and fmax
   */
  dbuf = databuf;
  dbufend = dbuf + vals;
  for(; dbuf < dbufend; dbuf++) {
    if(swap)
      bswap8 ((char *)dbuf);
    if(! started) {
      if(*dbuf == *dbuf) {  /* A NaN doesn't equal anything, not even itself */
	dmin = dmax = *dbuf;
	started = 7;
      }
    } else if(*dbuf == *dbuf) {
      if( *dbuf < dmin )
        dmin = *dbuf;
      else if( *dbuf > dmax )
        dmax = *dbuf;
    }
  }
  if(!started) {
    printf("No valid pixels in this image\n");
    dmin = dmax = 0.0;
  }

  /* make announcement if requested */
  if( verbose ) {
    (void)printf("Data min and max as read: %g, %g\n", dmin, dmax);
    if( image->fimin < image->fimax ) {
      (void)printf("Using given limits: %g, %g\n", image->fimin, image->fimax);
    }
  }
  /* apply preset limits if given */
  if( image->fimin < image->fimax ) {
    dmin = image->fimin;
    dmax = image->fimax;
  }  
  {
    double scale, bias;
    double dtemp;
    register short *sbuf;

    /* set bias to offset values to zero center the range */
    bias = -((dmin + dmax) / (double)2.0);
    /* get the scale factor */
    if( (dmax - dmin) > 0.0 ) {
      scale = (double)SCALEWIDTH / (dmax - dmin);
    } else {
      scale = 1.0;
    }
    /* reset buf for another pass through dbuf */
    dbuf = databuf;
    sbuf = imgbuf;
    /* use min and max to mark out limits */
    dmin = (double)SCALEMIN;
    dmax = (double)SCALEMAX;
    /* scale the picture */
    do {
      if(*dbuf == *dbuf) {	/* Pixel is not NaN */
	dtemp = (*dbuf + bias) * scale;
	if( dtemp < 0.0 ) {
	  dtemp -= 0.5;
	  if( dtemp < dmin )
	    *sbuf++ = dmin;
	  else
	    *sbuf++ = (short)dtemp;
	} else {
	  dtemp += 0.5;
	  if( dtemp > dmax )
	    *sbuf++ = dmax;
	  else
	    *sbuf++ = (short)dtemp;
	}
      } else {			/* Pixel is NaN */
	*sbuf++ = -SCALEOFF;
      }
    } while( ++dbuf < dbufend );
    image->fiscaled = 1;
    image->fibias = -bias;
    image->fiscale = 1.0 / scale;
  }
}

bswap4 (string)

/*   Reverse bytes of 4-byte variable
 *
 *	string	Address of 4-byte variable
 */

char *string;

{
char temp0, temp1, temp2, temp3;

	temp3 = string[0];
	temp2 = string[1];
	temp1 = string[2];
	temp0 = string[3];
	string[0] = temp0;
	string[1] = temp1;
	string[2] = temp2;
	string[3] = temp3;

	return;
}
 

bswap8 (string)

/*   Reverse bytes of 8-byte number
 *
 *	string	Address of 8-byte number
 */

char *string;

{
char temp[8];

	temp[7] = string[0];
	temp[6] = string[1];
	temp[5] = string[2];
	temp[4] = string[3];
	temp[3] = string[4];
	temp[2] = string[5];
	temp[1] = string[6];
	temp[0] = string[7];
	string[0] = temp[0];
	string[1] = temp[1];
	string[2] = temp[2];
	string[3] = temp[3];
	string[4] = temp[4];
	string[5] = temp[5];
	string[6] = temp[6];
	string[7] = temp[7];
	return;
}
