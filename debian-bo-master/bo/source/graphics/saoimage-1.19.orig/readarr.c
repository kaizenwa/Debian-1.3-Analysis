#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	readarr.c (Read Array)
 * Purpose:	Read in raster line array images
 * Subroutine:	read_array()			returns: void
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     31 October 1988
 * 		{1} Doug Mink  add skip over images          18 October 1995
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* define stderr */
#include <X11/Xlib.h>		/* needed for control.h */
#include "hfiles/constant.h"	/* define data type codes */
#include "hfiles/control.h"	/* define IOP codes */
#include "hfiles/image.h"

/*
 * Subroutine:	read_array
 * Purpose:	Read array data from a file
 * Note:	Assumes file was tested benignly, exits here if trouble
 */
void read_array ( fd, img, imgbuf, filebuf, width, height, X, Y, block,
		  verbose )
     int fd;			/* if >=0 handle to open & ready image file */
     struct imageRec *img;	/* record describing image file and its use */
     short *imgbuf;		/* buffer to receive i*2 data */
     char *filebuf;		/* buffer to receive raw data */
     int width, height;		/* width and height of buffer */
     int X, Y;			/* starting point in image file (no support) */
     int block;			/* blocking factor (not yet supported */
     int verbose;		/* whether to print explanatory messages */
{
  int vals;
  static int read_data();
  int image_start;
  int open_disk(), lseek_disk();
  void close_disk();
  void say_goodbye(), scale_data_u1(), scale_data_i2(), scale_data_u2();
  void scale_data_i4(), scale_data_r4(), scale_data_r8();

  if( (X!=0) || (Y!=0) || (block!=1) ) {
    (void)fprintf(stderr, "Error: no subsection support yet\n");
    return;
  }
  /* if not passed an open file, open it and move past the header */
  if( fd == -1 ) {
    /* open the image file */
    if( (fd = open_disk(img->filename, IOP_Read, 0)) < 0 )
      return;
    /* skip header if necessary */
    if( img->headersize > 0 ){
       image_start = img->headersize +
              ((img->nimage-1) * img->filerows * img->filecols * img->bytepix);
      if( lseek_disk(fd, image_start, img->filename) < 0 ) {
	close_disk(fd, img->filename);
	return;
      }
    }
  }
  /* FOR NOW, READ ARRAY CANNOT HANDLE OVERSIZED ARRAYS */
  if( (width != img->filecols) ||
      (height != img->filerows) ) {
    (void)fprintf(stderr, "Error: cannot handle %d x %d array\n",
		  img->filecols, img->filerows);
    return;
  }
  /* read in the data */
  vals = read_data(fd, img, filebuf);
  /* read the image into the picture buffer */
  switch( img->storage_type ) {
  case ARR_U1:
    scale_data_u1(img, imgbuf, (unsigned char *)filebuf, vals);
    break;
  case ARR_I2:
    scale_data_i2(img, imgbuf, (short *)filebuf, vals);
    break;
  case ARR_U2:
    scale_data_u2(img, imgbuf, (unsigned short *)filebuf, vals);
    break;
  case ARR_I4:
    scale_data_i4(img, imgbuf, (long *)filebuf, vals, verbose);
    break;
  case ARR_R4:
    scale_data_r4(img, imgbuf, (float *)filebuf, vals, verbose);
    break;
  case ARR_R8:
    scale_data_r8(img, imgbuf, (double *)filebuf, vals, verbose);
    break;
  default:
    (void)fprintf(stderr, "illegal array type: %d\n", img->storage_type);
    exit(1);
  }
  /* adjust buffer scale and bias to include that of original file */
  if( img->fscaled ) {
    if( img->fiscaled ) {
      img->fiscale *= img->fscale;
      img->fibias = (img->fscale * img->fibias) + img->fbias;
    } else {
      img->fiscaled = img->fscaled;
      img->fiscale = img->fscale;
      img->fibias = img->fbias;
    }
  }
  /* close the file */
  close_disk(fd, img->filename);
}

/*
 * Subroutine:	read_data
 * Purpose:	Read in the array data
 * UNIX calls:	read
 */
static int read_data ( fd, img, databuf )
     int fd;
     struct imageRec *img;
     char *databuf;
{
  int vals, nbytes;
  int read_disk();

  /* find size of image */
  vals = img->filecols * img->filerows;
  nbytes = vals * img->bytepix;
  /* read the file */
  if( read_disk(fd, databuf, nbytes, 1, img->filename, "data") != nbytes )
    return( 0 );
  else
    return( vals );
}
