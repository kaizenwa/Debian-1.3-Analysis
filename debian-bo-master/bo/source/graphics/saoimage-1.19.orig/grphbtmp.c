#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grphbtmp.c (Color Graph Bitmap)
 * Purpose:	Create the halftone colorbar
 * Subroutine:	make_halftone_colorbar()	returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 7 July 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"
#include "hfiles/constant.h"	/* define codes */

extern struct colorRec color;

/*
 * Subroutine:	make_halftone_panimage
 * Purpose:	Make halftone bitmap for pan window (panbox)
 */
void make_halftone_colorbar ( bytedata, bitdata,
			      width, height, bytes_per_line )
     unsigned char *bytedata;
     unsigned char *bitdata;
     int width, height;
     int bytes_per_line;
{
  static void byte_dither_sample(), byte_diffuse_sample();

  bzero((char *)bitdata, bytes_per_line * height);
  if( color.halftone.mode == BOP_Dither ) {
    byte_dither_sample(bytedata, width, 1, bitdata, bytes_per_line, 0, 0,
		       width, height, color.pixvalmap, color.halftone.matrix);
  } else if( color.halftone.mode == BOP_Diffuse ) {
    byte_diffuse_sample(bytedata, width, 1, bitdata, bytes_per_line, 0, 0,
			width, height, color.pixvalmap,
			color.halftone.errbuf);
  }
}

/*
 * Subroutine:	byte_dither_sample
 * Purpose:	Perform subsampling and dithering to produce a bitmap image
 *		from 16 bit signed data through a scaling lookup table.
 *		Used when bitmap is same size or smaller than data image.
 * Method:	Sample every zoom'th data element in each direction
 * Note:	This is a copy of the disp dither code but for byte data
 */
static void byte_dither_sample ( byte_data, data_width, zoom,
				 bitmap, bytes_per_line,
				 x, y, width, height, lookup, dither_matrix )
     unsigned char *byte_data;
     int data_width;
     int zoom;
     unsigned char *bitmap;
     int bytes_per_line;
     int x, y, width, height;
     register unsigned long *lookup;
     short *dither_matrix;
{
  register unsigned char *data;		/* ptr to current data input */
  register short *matrix;		/* ptr to current dither value */
  register unsigned char *bitmap_byte;	/* ptr to current output byte */
  register int bitmap_bit;	/* current bit in current output byte */
  unsigned char *data_row;
  short *matrix_row;
  short *matrix_row_end;
  short *matrix_end;
  unsigned char *data_row_end;
  unsigned char *bitmap_row;
  short x_step;
  short y_step;
  short bitmap_first_bit;
  short i;

  /* point register pointer to data buffer */
  data_row = byte_data;
  bitmap_row = bitmap + (y * bytes_per_line) + (x / 8);
  bitmap_first_bit = x & 7;	/* same as %8 */
  matrix_row = dither_matrix;
  matrix_row_end = matrix_row + 16;
  matrix_end = matrix_row + 256;
  x_step = zoom;
  y_step = data_width * x_step;
  /* process one row at a time */
  if( color.halftone.inverse ) {
    for( i = 0; i < height; i++ ) {
      matrix = matrix_row;
      data = data_row;
      data_row_end = data + width;
      bitmap_byte = bitmap_row;
      bitmap_bit = bitmap_first_bit;
      /* process through the row */
      while( data < data_row_end ) {
	if( lookup[*data] <= *matrix )
	  *bitmap_byte |= 1 << bitmap_bit;
	data += x_step;
	++matrix;
	/* check for next byte */
	if( ++bitmap_bit == 8 ) {
	  bitmap_bit = 0;
	  ++bitmap_byte;
	  if( matrix >= matrix_row_end )
	    matrix = matrix_row;
	}
      }
      bitmap_row += bytes_per_line;
      data_row += y_step;
      matrix_row = matrix_row_end;
      matrix_row_end += 16;
      if( matrix_row >= matrix_end ) {
	matrix_row = matrix;
	matrix_row_end = matrix_row + 16;
      }
    }
  } else {
    for( i = 0; i < height; i++ ) {
      matrix = matrix_row;
      data = data_row;
      data_row_end = data + width;
      bitmap_byte = bitmap_row;
      bitmap_bit = bitmap_first_bit;
      /* process through the row */
      while( data < data_row_end ) {
	if( lookup[*data] > *matrix )
	  *bitmap_byte |= 1 << bitmap_bit;
	data += x_step;
	++matrix;
	/* check for next byte */
	if( ++bitmap_bit == 8 ) {
	  bitmap_bit = 0;
	  ++bitmap_byte;
	  if( matrix >= matrix_row_end )
	    matrix = matrix_row;
	}
      }
      bitmap_row += bytes_per_line;
      data_row += y_step;
      matrix_row = matrix_row_end;
      matrix_row_end += 16;
      if( matrix_row >= matrix_end ) {
	matrix_row = matrix;
	matrix_row_end = matrix_row + 16;
      }
    }
  }
}

/*
 * Subroutine:	byte_diffuse_sample
 * Purpose:	Perform subsampling and error diffusion to produce a bitmap
 *		image from 16 bit signed data through a scaling lookup table.
 *		Used when bitmap is same size or smaller than data image.
 * Method:	Sample every zoom'th data element in each direction
 * Note:	This is a copy of the DispDiffuse code but for byte data
 */
static void byte_diffuse_sample ( byte_data, data_width, zoom,
				  bitmap, bytes_per_line,
				  x, y, width, height, lookup, errbuf )
     unsigned char *byte_data;
     int data_width;
     int zoom;
     unsigned char *bitmap;
     int bytes_per_line;
     int x, y, width, height;
     register unsigned long *lookup;
     short *errbuf;
{
  register int val;
  register short *error;
  register unsigned char *data;
  register unsigned char *bitmap_byte;
  register int maxval;
  unsigned char *bitmap_row;
  unsigned char *data_row;
  unsigned char *data_row_end;
  short error0;
  short bitmap_bit;
  short bitmap_first_bit;
  short x_step, y_step;
  short i;

  data_row = byte_data;
  bitmap_row = bitmap + (y * bytes_per_line) + (x / 8);
  bitmap_first_bit = x & 7;	/* same as %8 */
  /* halftone standard of 256 levels */
  maxval = 256;
  error0 = 0;
  /* subsample line skipping */
  x_step = zoom;
  y_step = data_width * zoom;
  if( color.halftone.inverse ) {
    /* go through each row of the bitmap */
    for( i = 0; i < height; i++ ) {
      error = errbuf;
      data = data_row;
      data_row_end = data + width;
      bitmap_byte = bitmap_row;
      bitmap_bit = bitmap_first_bit;
      while( data < data_row_end ) {
	/* val is data value plus average of error remnants */
	val = lookup[*data] + ((*error + error0 + error[1] + error[2]) >> 2);
	/* save earlier error */
	error0 = *(++error);
	/* replace old error with this error */
	if( val < maxval ) {
	  *bitmap_byte |= 1 << bitmap_bit;
	  *error = val;
	} else
	  *error = val - maxval;
	data += x_step;
	/* update word and bit pointers */
	if( ++bitmap_bit > 7 ) {
	  bitmap_byte++;
	  bitmap_bit = 0;
	}
      }
      data_row += y_step;
      /* advance to next line in bitmap_byte */
      bitmap_row += bytes_per_line;
    }
  } else {
    /* go through each row of the bitmap */
    for( i = 0; i < height; i++ ) {
      error = errbuf;
      data = data_row;
      data_row_end = data + width;
      bitmap_byte = bitmap_row;
      bitmap_bit = bitmap_first_bit;
      while( data < data_row_end ) {
	/* val is data value plus average of error remnants */
	val = lookup[*data] + ((*error + error0 + error[1] + error[2]) >> 2);
	/* save earlier error */
	error0 = *(++error);
	/* replace old error with this error */
	if( val >= maxval ) {
	  *bitmap_byte |= 1 << bitmap_bit;
	  *error = val - maxval;
	} else
	  *error = val;
	data += x_step;
	/* update word and bit pointers */
	if( ++bitmap_bit > 7 ) {
	  bitmap_byte++;
	  bitmap_bit = 0;
	}
      }
      data_row += y_step;
      /* advance to next line in bitmap_byte */
      bitmap_row += bytes_per_line;
    }
  }
}
