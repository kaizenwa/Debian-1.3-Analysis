#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	pancopy.c (Pan Copy)
 * Purpose:	Copy data between short integer image buffers in different
 *		flavors of blocking (used to fill pan buffer)
 * Subroutine:	copy_buf_subsample()			returns: void
 * Subroutine:	copy_buf_replicate()			returns: void
 * Subroutine:	copy_buf_sum()				returns: void
 * Subroutine:	copy_buf_max()				returns: void
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	      3 December 1988
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* define stderr, NULL, etc. */
#include "hfiles/define.h"		/* define MIN, MAX, etc. */

/*
 * Subroutine:	copy_buf_replicate
 * Purpose:	Copy data from main buffer to pan buffer, zooming UP by
 *		replicating. (for zoom > 1)
 * Exception:	Assumes a perfect fit (edge to edge, all pixels mapped)
 */
void copy_buf_replicate ( ibuf, obuf, dupe, iwidth, owidth, oheight )
     short *ibuf;		/* pointer to input buffer */
     register short *obuf;	/* pointer in output buffer */
     register int dupe;		/* imbuf to panbuf duplicating count */
     int iwidth;		/* width of input buffer */
     int owidth, oheight;	/* dimensions of output buffer */
{
  register short *ptr;		/* loop pointer in either buffer */
  register short *rowend;	/* loop pointer to end of a row */
  short *obufend;		/* pointer at end of output buffer */
  short *thisband;		/* pointer to line to replicate for band */
  short *nextband;		/* pointer to end of replicated band */
  int bandsz;			/* pixels between bands */

#ifdef DEBUG
  if( dupe <= 0 ) {
    (void)fprintf(stderr, "pan zoom error\n");
    return;
  }
#endif
  obufend = obuf + (owidth * oheight);
  bandsz = dupe * owidth;
  /* quit when obuf is full */
  while( obuf < obufend ) {
    /* mark end of row replication */
    thisband = obuf;
    nextband = obuf + bandsz;
    /* first make one row with column replication */
    ptr = ibuf;
    rowend = ibuf + iwidth;
    do {
      register int rep;
      rep = 0;
      while( rep++ < dupe )
	*obuf++ = *ptr;
    } while( ++ptr < rowend );
    /* replicate this row as needed */
    rowend = obuf;
    while( obuf < nextband ) {
      ptr = thisband;      
      while( ptr < rowend )
	*obuf++ = *ptr++;
    }
    ibuf += iwidth;
  }
}

/*
 * Subroutine:	copy_buf_subsample
 * Purpose:	Copy data between buffers, zooming down by sub-sampling
 * Exception:	Assumes a perfect fit
 */
void copy_buf_subsample ( ibuf, obuf, subsample, iwidth, owidth, oheight )
     short *ibuf;		/* pointer to input buffer */
     register short *obuf;	/* pointer in output buffer */
     register int subsample;	/* in to out sub-sampling increment */
     int iwidth;		/* width of input buffer */
     int owidth, oheight;	/* dimensions of output buffer */
{
  register short *iptr;		/* pointer within ibuf */
  register int iptr_advance;	/* to advance iptr to start next row */
  register short *rowend;	/* loop reference pointer to end of obuf row */
  register short *obufend;	/* loop reference pointer to end of obuf */
  int bufoff;			 /* subsample offset into sample block */

#ifdef DEBUG
  if( subsample <= 0 ) {
    (void)fprintf(stderr, "pan zoom error\n");
    return;
  }
#endif
  /* initialize parameter values */
  /* choose offsets to center the sample in block (not just start at 0) */
  bufoff = MAX (0, ((subsample - 1) / 2));
  /* center area of data by splitting remainder */
  iptr = ibuf + ((bufoff * iwidth) + bufoff);
  /* advance zoom lines minus what was ticked off (i*z - o*z) */
  iptr_advance = (iwidth - owidth) * subsample;
  /* quit when obuf is full */
  obufend = obuf + (owidth * oheight);
  /* loop through sub-sampling */
  while( obuf < obufend ) {
    /* MAKE A SUB-SAMPLED LINE */
    rowend = obuf + owidth;
    while( obuf < rowend ) {
      *obuf++ = *iptr;
      iptr += subsample;
    }
    iptr += iptr_advance;
  }
}

/*
 * Subroutine:	copy_buf_sum
 * Purpose:	Copy data between buffers, zoom down by summing or averaging
 * Exception:	Assumes a perfect fit
 */
void copy_buf_sum ( ibuf, obuf, xblock, yblock, av, iwidth, owidth, oheight )
     short *ibuf;		/* pointer to input buffer */
     register short *obuf;	/* pointer in output buffer */
     register int xblock;	/* width blocking factor */
     int yblock;		/* height blocking factor */
     int av;			/* flag to select averaging */
     int iwidth;		/* width of input buffer */
     int owidth, oheight;	/* dimensions of output buffer */
{
  register short *iptr, *inextsum, *onextline;
  short *isumline;
  int blocksq, halfbsq;
  int iadvance;
  int oline;

#ifdef DEBUG
  if (xblock <= 0 || yblock <= 0 ) {
    (void)fprintf(stderr, "pan zoom error block factors %d %d\n",xblock,yblock);
    return;
  }
#endif
  if( av ) {
    blocksq = xblock * yblock;
    halfbsq = blocksq / 2;
  }
  iadvance = yblock * iwidth;

  /* count the output lines */
  for( oline=0; oline<oheight; oline++ ) {
    bzero((char *)obuf, owidth);
    isumline = ibuf + iadvance;
    onextline = obuf + owidth;

    /* make block passes with same out line, summing */
    while( ibuf < isumline ) {
      /* remember ibuf at start of this line */
      iptr = ibuf;

      /* go through output line yblock times */
      while( obuf < onextline ) {

	/* sum for xblock piece of line */
	inextsum = iptr + xblock;
	while( iptr < inextsum )
	  *obuf += *iptr++;
	obuf++;

      }
      /* reset outline, advance inline */
      obuf -= owidth;
      ibuf += iwidth;
    }

    /* if averaging, divide by block squared (rounding) */
    if( av ) {
      /* make one more pass through outline, dividing */
      /* put inc where compiler is sure to have it sequenced right */
      do {
	*obuf = (*obuf + halfbsq) / blocksq;
      } while( ++obuf < onextline );
    } else {
      obuf = onextline;
    }
  }
}

/*
 * Subroutine:	copy_buf_max
 * Purpose:	Copy data between buffers, zoom down by taking the max
 * Exception:	Assumes a perfect fit
 */
void copy_buf_max ( ibuf, obuf, xblock, yblock, iwidth, owidth, oheight )
     short *ibuf;		/* pointer to input buffer */
     register short *obuf;	/* pointer in output buffer */
     register int xblock;	/* width blocking factor */
     int yblock;		/* height blocking factor */
     int iwidth;		/* width of input buffer */
     int owidth, oheight;	/* dimensions of output buffer */
{
  register short *iptr, *inextsum, *onextline;
  short *isumline;
  int iadvance;
  int oline;
  short absmin;

#ifdef DEBUG
  if (xblock <= 0 || yblock <= 0 ) {
    (void)fprintf(stderr, "pan zoom error block factors %d %d\n",xblock,yblock);
    return;
  }
#endif
  absmin = -32767;
  iadvance = yblock * iwidth;

  /* count the output lines */
  for( oline=0; oline<oheight; oline++ ) {

    /* set line to lowest possible value (use isumline as temp storage) */
    onextline = obuf + owidth;
    isumline = obuf;
    do {
      *obuf = absmin;
    } while( ++obuf < onextline );
    obuf = isumline;
    isumline = ibuf + iadvance;

    /* make block passes with same out line, summing */
    while( ibuf < isumline ) {
      /* remember ibuf at start of this line */
      iptr = ibuf;
      /* go through output line block times */
      while( obuf < onextline ) {
	/* sum for block piece of line */
	inextsum = iptr + xblock;
	do {
	  if( *iptr > *obuf )
	    *obuf = *iptr;
	} while( ++iptr < inextsum );
	obuf++;
      }

      /* reset outline, advance inline */
      obuf -= owidth;
      ibuf += iwidth;
    }
    /* go on to next line */
    obuf = onextline;
  }
}
