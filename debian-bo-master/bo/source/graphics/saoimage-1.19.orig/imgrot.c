#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	imgrot.c (Image Rotate)
 * Purpose:	Routines to rotate the img buffer 90 degrees
 * Subroutine:	cwturn_buf()			returns: void
 * Subroutine:	ccwturn_buf()			returns: void
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     3 December 1988
 *		{n} <who> -- <does what> -- <when>
 */

/*
 * Subroutine:	cwturn_buf
 * Purpose:	Rotate a square buffer 90 degrees clockwise
 */
void cwturn_buf ( buf, bufdim )
     short *buf;
     int bufdim;
{
  register short *row, *rowend, *col;
  register int bufwdth = bufdim;
  short *linebuf, *toprow, *botrow;
  int linedim;
  int ring, center;
  char *calloc_errchk();

  /* allocate space for temporary storage for two lines */
  linebuf = (short *)calloc_errchk(bufwdth, sizeof(short), "Temp line");
  /* perform rotations from outer ring in toward center */
  center = bufwdth / 2;
  linedim = bufwdth - 1;
  for( ring = 0; ring < center; ring++ ) {
    /* store top row of ring */
    toprow = buf + (ring * (bufwdth + 1));
    botrow = toprow + (linedim * bufwdth) + 1;
    bcopy((char *)toprow, (char *)linebuf, linedim * sizeof(short));
    /* copy left column onto top row */
    row = toprow + linedim - 1;
    rowend = toprow;
    col = toprow + bufwdth;
    do {
      *row = *col;
      col += bufwdth;
    } while( --row >= rowend );
    /* copy bottom row onto left column */
    row = botrow;
    rowend = row + linedim;
    col = toprow + bufwdth;
    do {
      *col = *row;
      col += bufwdth;
    } while( ++row < rowend );
    /* copy right column onto bottom row */
    row--;
    rowend = botrow;
    col = toprow + linedim;
    do {
      *row = *col;
      col += bufwdth;
    } while( --row >= rowend );
    /* copy saved top line onto right column */
    row = linebuf;
    rowend = row + linedim;
    col = toprow + linedim;
    do {
      *col = *row;
      col += bufwdth;
    } while( ++row < rowend );
    linedim -= 2;
  }
  /* free work space */
  free((char *)linebuf);
}

/*
 * Subroutine:	ccwturn_buf
 * Purpose:	Rotate a square buffer 90 degrees counter-clockwise
 */
void ccwturn_buf ( buf, bufdim )
     short *buf;
     int bufdim;
{
  register short *row, *rowend, *col;
  register int bufwdth = bufdim;
  short *linebuf, *toprow, *botrow;
  int linedim;
  int ring, center;
  char *calloc_errchk();

  /* allocate space for temporary storage for two lines */
  linebuf = (short *)calloc_errchk(bufwdth, sizeof(short), "Temp line");
  /* perform rotations from outer ring in toward center */
  center = bufwdth / 2;
  linedim = bufwdth - 1;
  for( ring = 0; ring < center; ring++ ) {
    /* store top row of ring */
    toprow = buf + (ring * (bufwdth + 1));
    botrow = toprow + (linedim * bufwdth) + 1;
    bcopy((char *)toprow, (char *)linebuf, linedim * sizeof(short));
    /* copy right column onto top row */
    row = toprow;
    rowend = toprow + linedim;
    col = toprow + linedim;
    do {
      *row = *col;
      col += bufwdth;
    } while( ++row < rowend );
    /* copy bottom row onto right column */
    rowend = botrow;
    row = rowend + linedim - 1;
    col = toprow + linedim;
    do {
      *col = *row;
      col += bufwdth;
    } while( --row >= rowend );
    /* copy left column onto bottom row */
    ++row;
    rowend = row + linedim;
    col = toprow + bufwdth;
    do {
      *row = *col;
      col += bufwdth;
    } while( ++row < rowend );
    /* copy saved top line onto left column */
    rowend = linebuf;
    row = rowend + linedim - 1;
    col = toprow + bufwdth;
    do {
      *col = *row;
      col += bufwdth;
    } while( --row >= rowend );
    linedim -= 2;
  }
  /* free work space */
  free((char *)linebuf);
}
