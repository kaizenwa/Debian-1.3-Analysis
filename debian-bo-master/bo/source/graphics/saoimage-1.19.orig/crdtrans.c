#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	crdtrans.c (Coordinate Transformation)
 * Purpose:	Transform x,y, coordinates through transformation matrix
 * Subroutine:	d_transform()			returns: void
 * Subroutine:	i_transform()			returns: void
 * Xlib calls:	none
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	        16 March 1988
 *		{n} <who> -- <does what> -- <when>
 */

#include "hfiles/coord.h"		/* coord structs */

/*
 * Subroutine:	d_transform
 * Purpose:	Perform coordinate translation on floating point
 *		x and y values
 */
void d_transform ( trans, xin, yin, xout, yout )
     Transform *trans;
     double xin, yin;
     float *xout, *yout;
{
  if( trans->no_rot ) {
    *xout = (trans->inx_outx * (float)xin) + trans->add_outx;
    *yout = (trans->iny_outy * (float)yin) + trans->add_outy;
  } else {
    *xout = trans->add_outx +
      (trans->inx_outx * (float)xin) + (trans->iny_outx * (float)yin);
    *yout = trans->add_outy +
      (trans->inx_outy * (float)xin) + (trans->iny_outy * (float)yin);
  }
}

/*
 * Subroutine:	i_transform
 * Purpose:	Perform coordinate translation using integer input values
 * Note:	Does some checking for computational short-cuts
 */
void i_transform ( trans, xin, yin, xout, yout )
     Transform *trans;
     int xin, yin;
     float *xout, *yout;
{

  /* if operation is orthogonal and can be performed in integer math */
  if( trans->int_math ) {
    /* if x and y axes are flipped */
    if( trans->flip ) {
      /* if not zoom 1 (no zoom) */
      if( trans->zoom ) {
	if( trans->multiply ) {
	  /* multiply means integer multiply */
	  *xout = (float)(yin * trans->ixzoom) + trans->iadd_outx;
	  *yout = (float)(xin * trans->iyzoom) + trans->iadd_outy;
	} else {
	  /* else divide */
	  *xout = ((float)yin / (float)trans->ixzoom) + trans->iadd_outx;
	  *yout = ((float)xin / (float)trans->iyzoom) + trans->iadd_outy;
	}
      } else {
	/* no zoom is zoom 1 */
	*xout = (float)yin + trans->iadd_outx;
	*yout = (float)xin + trans->iadd_outx;
      }
    } else {
      if( trans->zoom ) {
	if( trans->multiply ) {
	  /* multiply means integer multiply */
	  *xout = (float)(xin * trans->ixzoom) + trans->iadd_outx;
	  *yout = (float)(yin * trans->iyzoom) + trans->iadd_outy;
	}
	else {
	  /* else divide */
	  *xout = ((float)xin / (float)trans->ixzoom) + trans->iadd_outx;
	  *yout = ((float)yin / (float)trans->iyzoom) + trans->iadd_outy;
	}
      } else {
	/* no zoom is zoom 1 */
	*xout = (float)xin + trans->iadd_outx;
	*yout = (float)yin + trans->iadd_outy;
      }
    }
  } else {
    /* else translation must be computed with floating point operations */
    register float Xin, Yin;

    Xin = xin;
    Yin = yin;
    *xout =
      (trans->inx_outx * Xin) + (trans->iny_outx * Yin) + trans->iadd_outx;
    *yout =
      (trans->inx_outy * Xin) + (trans->iny_outy * Yin) + trans->iadd_outy;
  }
}
