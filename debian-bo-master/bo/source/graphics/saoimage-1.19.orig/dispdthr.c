#ifndef lint
static char SccsId[] = "@(#)dispdither.c	1.2  8/12/89";
#endif

/* Module:	dispdthr.c (Display Dither)
 * Purpose:	Map 16 bit data to a single plane bitmap using dithering
 * Subroutine:	dither_sample()			returns: void
 * Subroutine:	dither_replicate()		returns: void
 * Xlib calls:	none
 * Method:	Image dithering based on 16x16 square of values from 0 to 255.
 * Note:	Matrix is short to allow compare with 256 (not a uchar val)
 * Note:	Lookup is used to rescale input data to 0-255 range
 * Note:	Most loops check by pointer val, saving indexing overhead
 * Note:	Seperate code used for forward and reverse video rather than
 *		switching foreground and background val, because non-image
 *		area of display must not change
 * Copyright:	1989, 1995 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     7 July 1989
 *		{1} MVH added ULedge partial replicate	     3  Feb 1990
 *		{2} MVH replaced zoom rep code		    21 June 1991
 *		{3} Doug Mink  cast lookup table for comp     4 May 1995
 *		{n} <who> -- <does what> -- <when>
 */

static unsigned char bit_mask[8] = {
  0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80 };

/*
 * Subroutine:	zoom_dither_sample
 * Purpose:	Perform subsampling and dithering to produce a bitmap image
 *		from 16 bit signed data through a scaling lookup table.
 *		Used when bitmap is same size or smaller than data image.
 * Method:	Sample every zoom'th data element in each direction
 * Note:	x, y, width, and height define a subsection within output.
 *		Bits outside of subsection are unchanged.
 */
void dither_sample ( short_data, data_width, zoom, bitmap, bytes_per_line,
		     x, y, width, height, lookup, dither_matrix, inverse )
     short *short_data;			/* i: image of signed short data */
     int data_width;			/* i: row width of short image */
     int zoom;				/* i: zoom factor for subsampling */
     unsigned char *bitmap;		/* o: output bitmap buffer */
     int bytes_per_line;		/* i: bytes per line of bitmap */
     int x, y, width, height;		/* i: dimensions to fill */
     register unsigned char *lookup;	/* i: short to uchar lookup table */
     short *dither_matrix;		/* i: 16x16 matrix for dithering */
     int inverse;			/* i: make 0 on 1, else 1 on 0 */
{
  register short *data;			/* l: ptr to current data input */
  register short *matrix;		/* l: ptr to current dither value */
  register unsigned char *bitmap_byte;	/* l: ptr to current output byte */
  register unsigned char *bitmap_bit;	/* l: current bit in output byte */
  short *data_row;			/* l: beginning of current input row */
  short *matrix_row;			/* l: 1st entry in matrix row */
  short *matrix_row_end;		/* l: last short in matrix row */
  short *matrix_end;			/* l: last short in matrix */
  short *data_row_end;			/* l: last short in input data row */
  unsigned char *bitmap_row;		/* l: first byte of output row */
  unsigned char *bitmap_first_bit;	/* l: first byte to actually write */
  unsigned char *bitmap_last_bit;	/* l: last byte to be affected */
  short x_step;				/* l: sampling step along row */
  short y_step;				/* l: pointer offset to advance rows */
  short i;

  /* point register pointer to data buffer */
  data_row = short_data;
  bitmap_row = bitmap + (y * bytes_per_line) + (x / 8);
  bitmap_first_bit = bit_mask + (x & 7);	/* same as %8 */
  bitmap_last_bit = &(bit_mask[7]);
  /* initialize matrix pointers for use and loop tests */
  matrix_row = dither_matrix;
  matrix_row_end = matrix_row + 16;
  matrix_end = matrix_row + 256;
  /* initial sampling step size for both directions */
  x_step = zoom;
  y_step = data_width * x_step;
  /* width becomes count of data traversed each row */
  width *= zoom;
  /* if high vals are to be white */
  if( inverse ) {
    /* process one row at a time */
    for( i = 0; i < height; i++ ) {
      matrix = matrix_row;
      data = data_row;
      data_row_end = data + width;
      bitmap_byte = bitmap_row;
      bitmap_bit = bitmap_first_bit;
      /* process through the row */
      while( data < data_row_end ) {
	/* set bit by comparing val to matrix entry */
	if( (short)lookup[*data] <= *matrix )
	  *bitmap_byte |= *bitmap_bit;
	data += x_step;
	++matrix;
	/* check for next byte */
	if( ++bitmap_bit > bitmap_last_bit ) {
	  bitmap_bit = bit_mask;
	  ++bitmap_byte;
	  /* matrix row wrap-around happens every other byte edge */
	  if( matrix >= matrix_row_end )
	    matrix = matrix_row;
	}
      }
      /* advance to next row of everything */
      bitmap_row += bytes_per_line;
      data_row += y_step;
      matrix_row = matrix_row_end;
      matrix_row_end += 16;
      /* check for matrix column wrap-around */
      if( matrix_row >= matrix_end ) {
	matrix_row = dither_matrix;
	matrix_row_end = matrix_row + 16;
      }
    }
    /* else foreground is to be black */
  } else {
    /* process one row at a time */
    for( i = 0; i < height; i++ ) {
      matrix = matrix_row;
      data = data_row;
      data_row_end = data + width;
      bitmap_byte = bitmap_row;
      bitmap_bit = bitmap_first_bit;
      /* process through the row */
      while( data < data_row_end ) {
	/* set bit by comparing val to matrix entry */
	if( (short)lookup[*data] > *matrix )
	  *bitmap_byte |= *bitmap_bit;
	data += x_step;
	++matrix;
	/* check for next byte */
	if( ++bitmap_bit > bitmap_last_bit ) {
	  bitmap_bit = bit_mask;
	  ++bitmap_byte;
	  /* matrix row wrap-around happens every other byte edge */
	  if( matrix >= matrix_row_end )
	    matrix = matrix_row;
	}
      }
      /* advance to next row of everything */
      bitmap_row += bytes_per_line;
      data_row += y_step;
      matrix_row = matrix_row_end;
      matrix_row_end += 16;
      /* check for matrix column wrap-around */
      if( matrix_row >= matrix_end ) {
	matrix_row = dither_matrix;
	matrix_row_end = matrix_row + 16;
      }
    }
  }
}

/*
 * Subroutine:	zoom_dither_replicate
 * Purpose:	Perform replication and dithering to produce a bitmap image
 *		from 16 bit signed data through a scaling lookup table.
 *		Used when bitmap is larger than data image.
 * Method:	Repeat data element zoom times in each direction
 */
void dither_replicate ( short_data, data_width, zoom, bitmap, bytes_per_line,
		        first_x_rep, first_y_rep, x, y, width, height,
		        lookup, dither_matrix, inverse )
     short *short_data;			/* i: image of signed short data */
     int data_width;			/* i: row width of short image */
     int zoom;				/* i: zoom factor for replicating */
     unsigned char *bitmap;		/* o: output bitmap buffer */
     int bytes_per_line;		/* i: bytes per line of bitmap */
     int first_x_rep;			/* i: number of reps in 1st column */
     int first_y_rep;			/* i: num of reps in 1st row */
     int x, y, width, height;		/* i: area of output to fill */
     register unsigned char *lookup;	/* i: short to uchar lookup table */
     short *dither_matrix;		/* i: 16x16 matrix for dithering */
     int inverse;			/* i: make 0 on 1, else 1 on 0 */
{
  register short *data;			/* l: ptr to current data input */
  register short *matrix;		/* l: ptr to current dither value */
  register unsigned char *bitmap_byte;	/* l: ptr to current output byte */
  register int bitmap_bit;		/* l: current bit in output byte */
  short *data_row;			/* l: beginning of current input row */
  short *matrix_row;			/* l: 1st entry in matrix row */
  short *matrix_row_end;		/* l: last short in matrix row */
  short *matrix_end;			/* l: last short in matrix */
  short *data_row_end;			/* l: last short in input data row */
  unsigned char *bitmap_row;		/* l: first byte of output row */
  unsigned char *bitmap_row_end;	/* l: first byte in bitmap */
  int bitmap_first_bit;			/* l: 1st bit of first byte affected */
  int bitmap_last_bit;			/* l: last bit of last byte affected */
  int bitmap_bytes_per_row;		/* l: length of output line in bytes */
  int x_step;				/* l: repetition step along row */
  int y_step;				/* l: pointer offset to repeat rows */
  int i;

  /* point register pointer to data buffer */
  data_row = short_data;
  bitmap_row = bitmap + (y * bytes_per_line) + (x / 8);
  /* which bit does this x fall on? ((x & 7) is same as (x % 8)) */
  bitmap_first_bit = x & 7;
  /* which bit will last x fall on */
/*
  bitmap_last_bit = (width + bitmap_first_bit) & 7;
  bitmap_bytes_per_row = (width + bitmap_first_bit + 7) / 8;
*/
  bitmap_last_bit = (width + bitmap_first_bit - 1) & 7;
  bitmap_bytes_per_row = (x + width + 7) / 8;
  if( bitmap_bytes_per_row >= bytes_per_line )
    bitmap_bytes_per_row = bytes_per_line - ((x / 8) + 1);
  else
    bitmap_bytes_per_row -= ((x / 8) + 1);

  /* get matrix pointer and loop references ready */
  matrix_row = dither_matrix;
  matrix_row_end = matrix_row + 16;
  matrix_end = matrix_row + 256;
  /* convert first rep to first step toward zoom reps */
  y_step = 1 + zoom - first_y_rep;
  first_x_rep = 1 + zoom - first_x_rep;
  if( inverse ) {
    /* do one row at a time */
    for( i = 0; i < height; i++ ) {
      /* beginning of row initialization */
      data = data_row;
      bitmap_byte = bitmap_row;
      bitmap_row_end = bitmap_byte + bitmap_bytes_per_row;
      bitmap_bit = bitmap_first_bit;
      /* align matrix with output bytes */
      matrix = matrix_row + bitmap_bit;
      x_step = first_x_rep;
      /* go through the row */
      while( (bitmap_byte < bitmap_row_end) ||
	     ((bitmap_byte == bitmap_row_end) &&
	      (bitmap_bit <= bitmap_last_bit)) ) {
	/* set bit by comparing val to matrix entry */
	if( (short)lookup[*data] <= *matrix )
	  *bitmap_byte |= 1 << bitmap_bit;
	++matrix;
	/* check for next bitmap byte and end of matrix row */
	if( ++bitmap_bit == 8 ) {
	  bitmap_bit = 0;
	  ++bitmap_byte;
	  /* wrap matrix row around on output byte edges */
	  if( matrix >= matrix_row_end )
	    matrix = matrix_row;
	}
	/* move to next val when done with replication */
	if( ++x_step > zoom ) {
	  ++data;
	  x_step = 1;
	}
      }
      bitmap_row += bytes_per_line;
      matrix_row = matrix_row_end;
      matrix_row_end += 16;
      /* check for matrix column wrap-around */
      if( matrix_row >= matrix_end ) {
	matrix_row = dither_matrix;
	matrix_row_end = matrix_row + 16;
      }
      /* advance data row only after zoom repetitions */
      if( ++y_step > zoom ) {
	data_row += data_width;
	y_step = 1;
      }
    }
  } else {
    /* do one row at a time */
    for( i = 0; i < height; i++ ) {
      data = data_row;
      bitmap_byte = bitmap_row;
      bitmap_row_end = bitmap_byte + bitmap_bytes_per_row;
      bitmap_bit = bitmap_first_bit;
      /* align matrix with output bytes */
      matrix = matrix_row + bitmap_bit;
      x_step = first_x_rep;
      /* go through the row */
      while( (bitmap_byte < bitmap_row_end) ||
	     ((bitmap_byte == bitmap_row_end) &&
	      (bitmap_bit <= bitmap_last_bit)) ) {
	/* set bit by comparing val to matrix entry */
	if( (short)lookup[*data] > *matrix )
	  *bitmap_byte |= 1 << bitmap_bit;
	++matrix;
	/* check for next bitmap byte and end of matrix row */
	if( ++bitmap_bit == 8 ) {
	  bitmap_bit = 0;
	  ++bitmap_byte;
	  /* wrap matrix row around on output byte edges */
	  if( matrix >= matrix_row_end )
	    matrix = matrix_row;
	}
	/* move to next val when done with replication */
	if( ++x_step > zoom ) {
	  ++data;
	  x_step = 1;
	}
      }
      bitmap_row += bytes_per_line;
      matrix_row = matrix_row_end;
      matrix_row_end += 16;
      /* check for matrix column wrap-around */
      if( matrix_row >= matrix_end ) {
	matrix_row = dither_matrix;
	matrix_row_end = matrix_row + 16;
      }
      /* advance data row only after zoom repetitions */
      if( ++y_step > zoom ) {
	data_row += data_width;
	y_step = 1;
      }
    }
  }
}
