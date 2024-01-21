#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	imgflip.c (Image Flip)
 * Purpose:	Rotate buffer 180 degrees on any orthogonal axis
 * Subroutine:	xflip_buf()			returns: void
 * Subroutine:	yflip_buf()			returns: void
 * Subroutine:	zflip_buf()			returns: void
 * Subroutine:	transfer_buf()			returns: void
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     8 December 1988
 *		{n} <who> -- <does what> -- <when>
 */


/*  Subroutine:	xflip_buf
 *  Purpose:	Flip buf to make Y coordinates run in opposite direction
 *		(upside down)
 */
#ifdef ANSIC
void xflip_buf ( short *buf, int cols, int rows )
#else
void xflip_buf ( buf, cols, rows )
     short *buf;
     int cols, rows;
#endif
{
  register int bytes; 
  short *linebuf;
  short *toprow, *botrow;
  char *calloc_errchk();

  linebuf = (short *)calloc_errchk(cols, sizeof(short), "Temp line");
  toprow = buf;
  botrow = buf + ((rows - 1) * cols);
  bytes = cols * sizeof(short);
  while( toprow < botrow ) {
#ifdef ANSIC
    (void)memcpy((void *)linebuf, (void *)botrow, (size_t)bytes);
    (void)memcpy((void *)botrow, (void *)toprow, (size_t)bytes);
    (void)memcpy((void *)toprow, (void *)linebuf, (size_t)bytes);
#else
    bcopy((char *)botrow, (char *)linebuf, bytes);
    bcopy((char *)toprow, (char *)botrow, bytes);
    bcopy((char *)linebuf, (char *)toprow, bytes);
#endif
    toprow += cols;
    botrow -= cols;
  }
  free((char *)linebuf);
}


/*  Subroutine:	yflip_buf
 *  Purpose:	Mirror buf to make x coordinates run in opposite direction
 */
#ifdef ANSIC
void yflip_buf ( short *buf, int cols, int rows )
#else
void yflip_buf ( buf, cols, rows )
     short *buf;
     int cols, rows;
#endif
{
  short *row;
  register short *right, *left;
  int wdth;
  int i;
  short temp;

  row = buf;
  wdth = cols - 1;
  for( i=0; i<rows; i++ ) {
    right = row;
    left = right + wdth;
    do {
      temp = *right;
      *right = *left;
      *left = temp;
    } while( ++right < --left );
    row += cols;
  }
}


/* Subroutine:	zflip_buf
 * Purpose:	Turn buf upside down (180 degree rotation on image plane)
 */
#ifdef ANSIC
void zflip_buf ( short *buf, int cols, int rows )
#else
void zflip_buf ( buf, cols, rows )
     short *buf;
     int cols, rows;
#endif
{
  register short *top, *bottom;
  short temp;

  top = buf;
  bottom = buf + (cols * rows) - 1;
  do {
    temp = *top;
    *top = *bottom;
    *bottom = temp;
  } while( ++top < --bottom );
}


/* Subroutine:	transfer_buf
 * Purpose:	Move short values to another buffer with optional reorientation
 */
#ifdef ANSIC
void transfer_buf ( register short *src, register short *dst,
		    int cols, int rows, int rotcode )
#else
void transfer_buf ( src, dst, cols, rows, rotcode )
     register short *src;
     register short *dst;
     int cols, rows;
     int rotcode;
#endif
{
  register short *src_row_end;
  short *src_buf_end;
  int dst_inc;
  int dst_next_row;

  switch( rotcode ) {
  case 0:
#ifdef ANSIC
    (void)memcpy((void *)dst, (void *)src,
		 (size_t)(sizeof(short) * (cols * rows)));
#else
    bcopy((char *)src, (char *)dst, sizeof(short) * (cols * rows));
#endif
    return;
  case 1:
    /*  Top right corner, down  */
    dst += (rows - 1);
    dst_inc = rows;
    dst_next_row = -(1 + (cols * rows));
    break;
  case 2:
    /*  Lower right corner, back  */
    dst += ((rows * cols) - 1);
    dst_inc = -1;
    dst_next_row = 0;
    break;
  case 3:
    /*  Lower left corner, up  */
    dst += ((cols - 1) * rows);
    dst_inc = -rows;
    dst_next_row = 1 + (cols * rows);
    break;
  case 4:
    /*  Top right corner, back  */
    dst += (cols - 1);
    dst_inc = -1;
    dst_next_row = rows + rows - 1;
    break;
  case 5:
    /*  Lower right corner, up  */
    dst += ((cols * rows) - 1);
    dst_inc = -rows;
    dst_next_row = (cols * rows) - 1;
    break;
  case 6:
    /*  Lower left corner, forward  */
    dst += ((rows - 1) * cols);
    dst_inc = 1;
    dst_next_row = 1 - (rows + rows);
    break;
  case 7:
    /*  Upper left, down  */
    dst_inc = rows;
    dst_next_row = 1 -(cols * rows);
    break;
  default:
    return;
  } 
  src_buf_end = src + (cols * rows);
  src_row_end = src + cols;
  while( src < src_buf_end ) {
    do {
      *dst = *src;
      dst += dst_inc;
    } while( ++src < src_row_end );
    src_row_end += cols;
    dst += dst_next_row;
  }
}
