#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mgfytabl.c (Magnify Table)
 * Purpose:	print a table of image values on the screen
 * Subroutine:	print_table()		returns: void
 * Copyright:	1989, 1990 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Note:	Based on code by Bill Wyatt's showimg
 * Modified:	{0} Michael VanHilst	initial version	    29 September 1988
 *		{1} MVH redone for modularity, was ImageTable.c	  21 Feb 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* get stderr */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */

/*
 * Subroutine:	print_table
 * Purpose:	Print out the values of memory image given the event
 *		coordinates
 * Note:	Uses event coords in control struct (control.event.xkey)
 */
void print_table ()
{
  int bufx, bufy;
  int filex, filey;
  int xarray[16];
  int yarray[16];
  int table_size;
  int col_width;
  int rot;
  static int get_key_buf_coords(), set_table_params();
  static void output_column_labels(), output_pixval_table();

  /* determine the buffer coordinates of the event */
  if( get_key_buf_coords(&control.event.xkey, &bufx, &bufy) == 0 ) {
    (void)printf("SORRY: portion of image not in buffer.\n");
    return;
  }
  /* note if there is something special about the values */
  if( buffer.shortbuf_summing > 1 ) {
    (void)printf("Image buffer has summed values:\n");
    (void)printf("Each pixel summed from %d by %d square of pixels in file\n",
	    coord.fb.block, coord.fb.block);
    (void)printf("Square starts at file coordinates shown in table\n");
  }
  /* set a table size that fits with the word size needed */
  if( img.fiscaled ) {
    table_size = 7;
    col_width = 10;
  } else {
    table_size = 11;
    col_width = 6;
  }
  /* decide which pixels and get the relevant file coordinates */
  rot = set_table_params(bufx, bufy, table_size, table_size, xarray, yarray);
  filex = xarray[table_size];
  filey = yarray[table_size];
  /* identify the pixel and note any coordinate oddities */
  (void)printf("\n\nFile Column: %d, Row: %d\n", filex, filey);
  if( rot > 1 ) {
    (void)printf("WARNING: image coordinates rotated from file\n");
    (void)printf(" Table labels give image buffer offsets\n");
    filex = 0;
    filey = 0;
  } else if( rot > 0 ) {
    (void)printf("Image is orthogonally rotated from file\n");
    (void)printf(" File rows read across, file columns read up and down\n");
  }
  /* make the table header line */
  output_column_labels(xarray, table_size, col_width, filex);
  /* print a table of values in a table_size x table_size picture region */
  output_pixval_table(xarray, yarray, table_size, table_size, col_width,
		      xarray[table_size + 1], yarray[table_size + 1],
		      filex, filey);
}

/*
 * Subroutine:	get_key_buf_coords
 * Purpose:	Determine the buffer coordinates of the pixel identified
 *		with a key event.
 * Returns:	1 if the pixel is in the image data buffer, else 0.
 */
static int get_key_buf_coords ( xkey, bufX, bufY )
     XKeyEvent *xkey;
     int *bufX, *bufY;
{
  float x, y;
  void i_transform(), d_transform();

  /* translate event to buffer coordinates */
  if( xkey->window == dispbox.ID ) {
    i_transform(&coord.disptobuf, xkey->x, xkey->y, &x, &y);
  } else if( xkey->window == panbox.ID ) {
    i_transform(&coord.pantoimg, xkey->x, xkey->y, &x, &y);
    d_transform(&coord.imgtobuf, (double)x, (double)y, &x, &y);
  } else
    return;
  *bufX = (int)x;
  *bufY = (int)y;
  /* check if within buffer */
  if ((*bufX < 0) || (*bufX >= coord.buf.width) ||
      (*bufY < 0) || (*bufY >= coord.buf.height) )
    return( 0 );
  return( 1 );
}

/*
 * Subroutine:	output_column_labels
 * Purpose:	Print a line of column indexes and underline it.
 */
static void output_column_labels ( xarray, cols, col_width, filex )
     int xarray[];	/* array of column labels */
     int cols;		/* number of cols */
     int col_width;	/* width of each column */
     int filex;		/* given coord to be highlighted */
{
  int i;
  char format[32];

  /* space down and start next line over from row labels */
  (void)printf("\n       ");
  /* print the column numbers */
  for( i=0; i<cols; i++ ) {
    /* special treatment (reverse video) for the chosen pixel column */
    if( xarray[i] == filex ) {
      sprintf(format, "  %%c[7m%%%dd%%c[0m", col_width - 2);
      (void)printf(format, 27, xarray[i], 27);
    } else {
      sprintf(format, "  %%%dd", col_width - 2);
      (void)printf(format, xarray[i]);
    }
  }
  /* carriage return and underline the column labels */
  (void)printf("\n       ");
  for( i=2; i<col_width; i++ )
    format[i] = '-';
  format[col_width] = '\0';
  for( i=0; i<cols; i++)
    (void)printf(format);
}

/*
 * Subroutine:	output_pixval_table
 * Purpose:	Print a table of pixel values
 * Note:	Uses VT100 escape sequence for reverse video (seems to work)
 * Note:	Uses global image and buffer structures
 */
static void output_pixval_table ( xarray, yarray, cols, rows, col_width,
				  bufx, bufy, filex, filey )
     int xarray[], yarray[];	/* array of labels */
     int cols, rows;		/* dimensions of table */
     int col_width;		/* width of each column in output line */
     int bufx, bufy;		/* buffer coord of upper left in table */
     int filex, filey;		/* label vals of highlighted pixel */
{
  double dval;
  int ival;
  int clip;
  int i, j;
  char string[16];
  int get_pixel_val();
  void real_string(), integer_string();

  for( j=0; j<rows; j++ ) {
    /* print <nl> and the row label (reverse video if chosen row) */
    if( yarray[j] == filey )
      (void)printf("\n %c[7m%4d%c[0m |", 27, yarray[j], 27);
    else
      (void)printf("\n%5d |", yarray[j]);
    /* print the data */
    for( i=0; i<cols; i++ ) {
      if( get_pixel_val((bufx + i), (bufy + j), &ival, &dval, &clip) ) {
	/* value is an integer */
	integer_string(ival, clip, string, col_width);
      } else {
	real_string(dval, &string[1], col_width - 1);
	if( clip ) {
	  if( clip > 0 )
	    *string = '>';
	  else
	    *string = '<';
	} else
	  *string = ' ';
      }
      if( (xarray[i] == filex) && (yarray[j] == filey) )
	(void)printf("%c[7m%s%c[0m", 27, string, 27);
      else
	(void)printf("%s", string);
    }
  }
  (void)printf("\n");
}

/*
 * Subroutine:	set_table_params
 * Purpose:	Put pixval table parameters in the x and y arrays
 *		0 to (dim-1) has file coords,
 *		_array[dim]: main file coord, _array[dim+1]: starting buf coord
 */
static int set_table_params ( bufx, bufy, xdim, ydim, xarray, yarray )
     int bufx, bufy;
     int xdim, ydim;
     int *xarray, *yarray;
{
  float x0, x1, x2, y0, y1, y2;
  int xinc, yinc;
  int fx0, fx1, fx2, fy0, fy1, fy2;
  int bufx0, bufy0, bufoff;
  int i, rot;
  void i_transform();
  
  /* determine starting buffer position */
  bufoff = (xdim - 1) / 2;
  bufx0 = bufx - bufoff;
  if( bufx0 < 0 )
    bufx0 = 0;
  else if( (bufoff = bufx0 + xdim - coord.buf.width) > 0 )
    bufx0 -= bufoff;
  bufoff = (ydim - 1) / 2;
  bufy0 = bufy - bufoff;
  if( bufy0 < 0 )
    bufy0 = 0;
  else if( (bufoff = bufy0 + ydim - coord.buf.height) > 0 )
    bufy0 -= bufoff;
  /* store the coordinates of the first buffer element */
  xarray[xdim+1] = bufx0;
  yarray[ydim+1] = bufy0;
  /* set buf0 to offset from focus pixel */
  bufx0 = bufx0 - bufx; 
  bufy0 = bufy0 - bufy; 
  /* determine file coordinates of focus and two other pixels */
  i_transform (&coord.buftofile, bufx, bufy, &x0, &y0);
  i_transform (&coord.buftofile, bufx+1, bufy, &x1, &y1);
  i_transform (&coord.buftofile, bufx, bufy+1, &x2, &y2);
  if( coord.file.ioff > 0.1 ) {
    fx0 = (int)x0;
    fy0 = (int)y0;
    fx1 = (int)x1;
    fy1 = (int)y1;
    fx2 = (int)x2;
    fy2 = (int)y2;
  } else {
    fx0 = (int)(x0 + 0.5);
    fy0 = (int)(y0 + 0.5);
    fx1 = (int)(x1 + 0.5);
    fy1 = (int)(y1 + 0.5);
    fx2 = (int)(x2 + 0.5);
    fy2 = (int)(y2 + 0.5);
  }
  /* store the focus coordinates */
  xarray[xdim] = fx0;
  yarray[ydim] = fy0;
  /* determine the file increments along both axes and rotation */
  if( (fx1 != fx0) && (fy1 == fy0) ) {
    xinc = fx1 - fx0;
    yinc = fy2 - fy0;
    rot = 0;
  } else if( (fx2 != fx0) && (fy2 = fy0) ) {
    xinc = fx2 - fx0;
    yinc = fy1 - fy0;
    rot = 1;
  } else {
    for( i=0; i<xdim; i++ )
      xarray[i] = bufx0++;
    for( i=0; i<ydim; i++ )
      yarray[i] = bufy0++;
    return( 2 );
  }
  /* install the file coordinates */
  xarray[0] = fx0 + (bufx0 * xinc);
  for( i=1; i<xdim; i++ )
    xarray[i] = xarray[i-1] + xinc;
  yarray[0] = fy0 + (bufy0 * yinc);
  for( i=1; i<ydim; i++ )
    yarray[i] = yarray[i-1] + yinc;
  return( rot );
}
