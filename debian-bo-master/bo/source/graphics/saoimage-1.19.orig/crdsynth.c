#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	crdsynth.c (Coordinate Synthesis)
 * Purpose:	Support the computing of transform matrices and shortcuts
 * Subroutine:	invert_transform()		returns: void
 * Subroutine:	compute_iadd()			returns: void
 * Subroutine:	combine_transform()		returns: void
 * Subroutine:	set_trans_speed()		returns: void
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

#include <stdio.h>		/* stderr, NULL, etc */
#include <math.h>		/* get trig functions, sqrt, and fabs */
#include "hfiles/coord.h"	/* coord structs */

/*
 * Subroutine:	invert_transform
 * Purpose:	Create matrix to perform opposite transformation
 *		Given AtoB (old) and Bsys ioff, compute BtoA (new)
 */
void invert_transform ( new, old, ioff )
     Transform *new, *old;	/* pointers for new and original transforms */
     float ioff;		/* integer coord to real translation factor */
{
  void invert_matrix(), compute_iadd_invert(), set_trans_speed();  

#ifdef DEBUG
  void exit_errmsg();
  /* check for undifined coordinate transforms */
  if( ((old->inx_outx ==0) && (old->iny_outx ==0)) ||
      ((old->inx_outy ==0) && (old->iny_outy ==0)) ) {
    exit_errmsg("indeterminate coordinate transform!");
  }
#endif
  /* check for simplified cases of orthogonal transform */
  if( (old->iny_outx ==0) && (old->inx_outy ==0) ) {
    new->iny_outx = 0.0;
    new->inx_outy = 0.0;
    new->inx_outx = 1.0 / old->inx_outx;
    new->iny_outy = 1.0 / old->iny_outy;
    new->add_outx = -old->add_outx * new->inx_outx;
    new->add_outy = -old->add_outy * new->iny_outy;
    new->iadd_outx = (-old->add_outx + ioff) * new->inx_outx;
    new->iadd_outy = (-old->add_outy + ioff) * new->iny_outy;
  } else if( (old->inx_outx ==0) && (old->iny_outy ==0) ) {
    new->inx_outx = 0.0;
    new->iny_outy = 0.0;
    new->iny_outx = 1.0 / old->inx_outy;
    new->inx_outy = 1.0 / old->iny_outx;
    new->add_outx = -old->add_outy * new->iny_outx;
    new->add_outy = -old->add_outx * new->inx_outy;
    new->iadd_outx = (-old->add_outy + ioff) * new->iny_outx;
    new->iadd_outy = (-old->add_outx + ioff) * new->inx_outy;
  } else {
    /* compute parameters by matrix inversion */
    invert_matrix(old, new);
    compute_iadd_invert(old, new, ioff);
  }
  /* set rotation flag */
  new->no_rot = old->no_rot;
  /* set parameters for speedy integer calculations */
  set_trans_speed(new);
}

#ifdef NOTNEEDED /* %% not currently needed */
/*
 * Subroutine:	compute_iadd
 * Purpose:	Compute the offsets used for integer transforms
 */
void compute_iadd ( old, new, ioff )
     Transform *old, *new;
     float ioff;
{
  void compute_iadd_invert();

  /* check for simplified cases of orthogonal transform */
  if( (new->iny_outx ==0) && (new->inx_outy ==0) ) {
    new->iadd_outx = new->add_outx + (ioff * new->inx_outx);
    new->iadd_outy = new->add_outy + (ioff * new->iny_outy);
  } else if( (new->inx_outx ==0) && (new->iny_outy ==0) ) {
    new->iadd_outx = new->add_outx + (ioff * new->iny_outx);
    new->iadd_outy = new->add_outy + (ioff * new->inx_outy);
  } else {
  /* if no simple solution, carry offset through from inverse transform */
    compute_iadd_invert(old, new, ioff);
  }
}
#endif

/*
 * Subroutine:	combine_transform
 * combine two sets of transform parameters into a single set
 * first and second apply to sequence starting with the input values
 * algorithm is 3x3 matrix multiplication (3rd row is 0 0 1)
 * combination[i][j] is sum of products of column j of 1st and row i of 2nd
 */
void combine_transform ( new, first, second )
     Transform *new, *first, *second;
{
  void set_trans_speed();

  new->inx_outx = ((second->inx_outx * first->inx_outx) +
		   (second->iny_outx * first->inx_outy));
  new->iny_outx = ((second->inx_outx * first->iny_outx) +
		   (second->iny_outx * first->iny_outy));
  new->add_outx = ((second->inx_outx * first->add_outx) +
		   (second->iny_outx * first->add_outy) +
		   second->add_outx);
  new->inx_outy = ((second->inx_outy * first->inx_outx) +
		   (second->iny_outy * first->inx_outy));
  new->iny_outy = ((second->inx_outy * first->iny_outx) +
		   (second->iny_outy * first->iny_outy));
  new->add_outy = ((second->inx_outy * first->add_outx) +
		   (second->iny_outy * first->add_outy) +
		   second->add_outy);
  /* compute offset factor for transform starting with an integer value */
  /* use integer input offset of first and float offset of second */
  new->iadd_outx = ((second->inx_outx * first->iadd_outx) +
		    (second->iny_outx * first->iadd_outy) +
		    second->add_outx);
  new->iadd_outy = ((second->inx_outy * first->iadd_outx) +
		    (second->iny_outy * first->iadd_outy) +
		    second->add_outy);

  /* set rotation flag, most cases don't have cross x-y relationships */
  new->no_rot = first->no_rot && second->no_rot;
  /* set up parameters needed for speedy calculation of transform */
  /* uses integer multiply or divide on orthogonal rotations */
  set_trans_speed(new);
  return;
}

#define EPSILON 0.0001
#define NEGONE -0.9999
#define CLOSE(a,b) (fabs((double)((a)-((float)(b))))<EPSILON)
/*
 * Subroutine:	set_trans_speed
 * Purpose:	Set parameters for fast integer computation
 */
void set_trans_speed ( trans )
     Transform *trans;
{
  int xzm, yzm;
  static int integer_test();

  trans->ixzoom = 0;
  trans->iyzoom = 0;
  /* is it an unflipped transform? */
  if( CLOSE(trans->iny_outx, 0) && CLOSE(trans->inx_outy, 0) ) {
    /* straight forward x->x, y->y transform */
    trans->no_rot = 1;
    trans->flip = 0;
    xzm = integer_test(trans->inx_outx, &trans->ixzoom);
    yzm = integer_test(trans->iny_outy, &trans->iyzoom);
  } else {
    trans->no_rot = 0;
    if( CLOSE(trans->inx_outx, 0) && CLOSE(trans->iny_outy, 0) ) {
      trans->flip = 1;
      xzm = integer_test(trans->iny_outx, &trans->ixzoom);
      yzm = integer_test(trans->inx_outy, &trans->iyzoom);
    } else {
      trans->int_math = 0;
      return;
    }
  }
  /* if both zooms can be handled by integer operations ... */
  /* ... when given integer inputs, use faster operations */
  if( trans->ixzoom && trans->iyzoom ) {
    if( (trans->ixzoom == 1) && (trans->iyzoom == 1) ) {
      /* if both are zoom 1 */
      trans->int_math = 1;
      trans->zoom = 0;
    } else if( (xzm >= 0) && (yzm >= 0) ) {
      /* if both zooms can be integer multiplies, use integer multiply */
      trans->int_math = 1;
      trans->multiply = 1;
      trans->zoom = 1;
    } else if( (xzm <= 0) && (yzm <= 0) ) {
      /* if both zooms can be integer divides, use float divide */
      trans->int_math = 1;
      trans->multiply = 0;
      trans->zoom = 1;
    } else {
      /* if its not so simple (mixed operations) don't bother using shortcut */
      trans->int_math = 0;
    }
  } else {
    trans->int_math = 0;
  }
}

/*
 * Subroutine:	integer_test
 * Returns:	0 if not possible, 1 for zoom up, -1 for zoom down
 * Purpose:	Test zoom to determine whether it could be computed
 *		with integer math
 */
static int integer_test ( fzoom, izoom )
     double fzoom;	/* i: zoom factor (>1, ==1, or <1) */
     int *izoom;	/* o: integer zoom factor (>1 or ==1) */
{
  int ival;

  /* test for unity scaling */
  if( CLOSE(fzoom, 1) ) {
    *izoom = 1;
    return( 0 );
  }
  /* test for even integer multiplies */
  if( fzoom > 1.0 ) {
    /* round and test for close match */
    ival = fzoom + 0.5;
    if( CLOSE(fzoom, ival) ) {
      *izoom = ival;
      return( 1 );
    }
  } else if( fzoom < NEGONE ) {
    ival = fzoom - 0.5;
    if( CLOSE(fzoom, ival) ) {
      *izoom = ival;
      return( 1 );
    }
  } else if( fzoom != 0.0 ) {
    /* test for even integer divides */
    fzoom = 1.0 / fzoom;
    if( fzoom > 0 ) {
      /* round and test for close match */
      ival = fzoom + 0.5;
      if( CLOSE(fzoom, ival) ) {
	*izoom = ival;
	return( -1 );
      }
    } else {
      ival = fzoom - 0.5;
      if( CLOSE(fzoom, ival) ) {
	*izoom = ival;
	return( -1 );
      }
    }
  }
#ifdef DEBUG
  else
    (void)fprintf(stderr, "Warning: Zero zoom factor (CoordSynth.c)\n");
#endif
  *izoom = 0;
  return( 0 );
}
