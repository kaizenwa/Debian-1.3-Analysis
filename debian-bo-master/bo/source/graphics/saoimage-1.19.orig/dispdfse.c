#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	dispdfse.c (Display Error Diffusion)
 * Purpose:	Map 16 bit data to a single plane bitmap using error diffusion
 * Subroutine:	diffuse_sample()		returns: void
 * Subroutine:	diffuse_replicate()		returns: void
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * History:	Algorithm from Bill Wyatt's linodots & John Tonry's halftone
 * Modified:	{0} Michael VanHilst	initial version	         7 July 1987
 *		{1} MVH fixed zoom rep code			21 June 1991
 *		{n} <who> -- <does what> -- <when>
 */

/*
 * Subroutine:	diffuse_sample
 * Purpose:	Perform subsampling and error diffusion to produce a bitmap
 *		image from 16 bit signed data through a scaling lookup table.
 *		Used when bitmap is same size or smaller than data image.
 * Method:	Sample every zoom'th data element in each direction
 */
void diffuse_sample ( short_data, data_width, zoom, bitmap, bytes_per_line,
		      x, y, width, height, lookup, errbuf, inverse )
     short *short_data;
     int data_width;
     int zoom;
     char *bitmap;
     int bytes_per_line;
     int x, y, width, height;
     register unsigned char *lookup;
     short *errbuf;
     int inverse;
{
  register int val;
  register short *error;
  register short *data;
  register char *bitmap_byte;
  register int maxval;
  char *bitmap_row;
  short *data_row;
  short *data_row_end;
  short error0;
  short bitmap_bit;
  short bitmap_first_bit;
  short x_step, y_step;
  short i;

  data_row = short_data;
  bitmap_row = bitmap + (y * bytes_per_line) + (x / 8);
  bitmap_first_bit = x & 7;	/* same as %8 */
  /* halftone standard of 256 levels */
  maxval = 256;
  error0 = 0;
  /* subsample line skipping */
  x_step = zoom;
  y_step = data_width * zoom;
  if( inverse ) {
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

/*
 * Subroutine:	diffuse_replicate
 * Purpose:	Perform replication and error diffusion to produce a bitmap
 *		image from 16 bit signed data through a scaling lookup table.
 *		Used when bitmap is larger than data image.
 * Method:	Repeat data element zoom times in each direction
 */
void diffuse_replicate ( short_data, data_width, zoom, bitmap, bytes_per_line,
			 first_x_rep, first_y_rep, x, y, width, height,
			 lookup, errbuf, inverse )
     short *short_data;
     int data_width;
     int zoom;
     char *bitmap;
     int bytes_per_line;
     int first_x_rep;
     int first_y_rep;
     int x, y, width, height;
     register unsigned char *lookup;
     short *errbuf;
     int inverse;
{
  register int val;
  register short *error;
  register short *data;
  register char *bitmap_byte;
  register int maxval;
  short *data_row;
  char *bitmap_row;
  char *bitmap_row_end;
  int error0;
  int bitmap_bit;
  int bitmap_first_bit;
  int bitmap_last_bit;
  int bitmap_bytes_per_row;
  int x_step, y_step;
  int i;

  data_row = short_data;
  bitmap_row = bitmap + (y * bytes_per_line) + (x / 8);
  bitmap_first_bit = x & 7;	/* same as %8 */
  bitmap_last_bit = (width + bitmap_first_bit - 1) & 7;
  bitmap_bytes_per_row = (x + width + 7) / 8;
  if( bitmap_bytes_per_row >= bytes_per_line )
    bitmap_bytes_per_row = bytes_per_line - ((x / 8) + 1);
  else
    bitmap_bytes_per_row -= ((x / 8) + 1);
  /* halftone standard of 256 levels */
  maxval = 256;
  error0 = 0;
  /* convert first rep to first step toward zoom reps */
  y_step = 1 + zoom - first_y_rep;
  first_x_rep = 1 + zoom - first_x_rep;
  if( inverse ) {
    /* go through each row of the bitmap */
    for( i = 0; i < height; i++ ) {
      error = errbuf;
      x_step = first_x_rep;
      data = data_row;
      bitmap_byte = bitmap_row;
      bitmap_row_end = bitmap_byte + bitmap_bytes_per_row;
      bitmap_bit = bitmap_first_bit;
      while( (bitmap_byte < bitmap_row_end) ||
	     ((bitmap_byte == bitmap_row_end) &&
	      (bitmap_bit <= bitmap_last_bit)) ) {
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
	if( ++bitmap_bit > 7 ) {
	  bitmap_byte++;
	  bitmap_bit = 0;
	}
	/* step forward after replication */
	if( ++x_step > zoom ) {
	  data++;
	  x_step = 1;
	}
      }
      /* advance one line after replication, else repeat this line */
      if( ++y_step > zoom ) {
	data_row += data_width;
	y_step = 1;
      }
      bitmap_row += bytes_per_line;
    }
  } else {
    /* go through each row of the bitmap */
    for( i = 0; i < height; i++ ) {
      error = errbuf;
      x_step = first_x_rep;
      data = data_row;
      bitmap_byte = bitmap_row;
      bitmap_row_end = bitmap_byte + bitmap_bytes_per_row;
      bitmap_bit = bitmap_first_bit;
      while( (bitmap_byte < bitmap_row_end) ||
	     ((bitmap_byte == bitmap_row_end) &&
	      (bitmap_bit <= bitmap_last_bit)) ) {
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
	if( ++bitmap_bit > 7 ) {
	  bitmap_byte++;
	  bitmap_bit = 0;
	}
	/* step forward after replication */
	if( ++x_step > zoom ) {
	  data++;
	  x_step = 1;
	}
      }
      /* advance one line after replication, else repeat this line */
      if( ++y_step > zoom ) {
	data_row += data_width;
	y_step = 1;
      }
      bitmap_row += bytes_per_line;
    }
  }
}
