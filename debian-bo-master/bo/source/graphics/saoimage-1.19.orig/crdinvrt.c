#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	crdinvrt.c (Coordinate Invert)
 * Purpose:	Compute reverse transform and unknown transform parameters
 * Subroutine:	invert_matrix()			returns: void
 * Subroutine:	compute_iadd_invert()		returns: void
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
#include "hfiles/coord.h"	/* Transform and other coord structs */

#define N 3	/* all matrices are 3x3 */

/*
 * Subroutine:	invert_matrix
 * Purpose:	Compute parameters of the inverse transform
 * Method:	Uses LU decomposition method
 */
void invert_matrix ( old, new )
     Transform *old, *new;
{
  float scratch[3][3], column[3];
  int pivots[3];
  static void lu_decompose(), lu_backsub();

  scratch[0][0] = old->inx_outx;
  scratch[1][0] = old->iny_outx;
  scratch[2][0] = old->add_outx;
  scratch[0][1] = old->inx_outy;
  scratch[1][1] = old->iny_outy;
  scratch[2][1] = old->add_outy;
  scratch[0][2] = scratch[1][2] = 0.0;
  scratch[2][2] = 1.0;
  lu_decompose(scratch, pivots);
  /* solve for out X */
  column[1] = column[2] = 0.0;
  column[0] = 1.0;
  lu_backsub(scratch, pivots, column);
  new->inx_outx = column[0];
  new->iny_outx = column[1];
  new->add_outx = column[2];
  /* solve for out Y */
  column[0] = column[2] = 0.0;
  column[1] = 1.0;
  lu_backsub(scratch, pivots, column);
  new->inx_outy = column[0];
  new->iny_outy = column[1];
  new->add_outy = column[2];
}

/*
 * Subroutine:	compute_iadd_invert
 * Purpose:	Compute the offsets used for integer transforms
 * Method:	Uses matrix inversion
 */
void compute_iadd_invert ( old, new, ioff )
     Transform *old, *new;
     float ioff;
{
  float scratch[3][3], column[3];
  int pivots[3];
  static void lu_decompose(), lu_backsub();

  /* set transform equations in matrix form */
  scratch[0][0] = old->inx_outx;
  scratch[1][0] = old->iny_outx;
  scratch[2][0] = old->add_outx + ioff;
  scratch[0][1] = old->inx_outy;
  scratch[1][1] = old->iny_outy;
  scratch[2][1] = old->add_outy + ioff;
  scratch[0][2] = scratch[1][2] = 0.0;
  scratch[2][2] = 1.0;
  lu_decompose(scratch, pivots);
  /* solve for out X */
  column[1] = column[2] = 0.0;
  column[0] = 1.0;
  lu_backsub(scratch, pivots, column);
  new->iadd_outx = column[2];
  /* solve for out Y */
  column[0] = column[2] = 0.0;
  column[1] = 1.0;
  lu_backsub(scratch, pivots, column);
  new->iadd_outy = column[2];
}

/*
 * Subroutine:	lu_decompose
 * Purpose:	lower upper decompose matrix (in place) for matrix inversion
 */
static void lu_decompose ( mtrx, pivots )
     float mtrx[3][3];
     int pivots[3];
{
  int i, j, k, imax;
  double sum, mag, column_max;
  float merit;
  float rescale[3];

  /* find the maximum values in each column */
  for( i=0; i<N; i++ ) {
    column_max = 0.0;
    for( j=0; j<N; j++ ) {
      mag = fabs((double)mtrx[i][j]);
      if( mag > column_max ) column_max = mag;
    }
    /* save scaling value */
    rescale[i] = 1.0 / column_max;
  }
  /* loop through columns for Crouts method */
  for( j=0; j<N; j++ ) {
    if( j > 0 ) {
      for( i=0; i<j; i++ ) {
	sum = mtrx[i][j];
	if( i > 0 ) {
	  for( k=0; k<i; k++ ) {
	    sum = sum - (mtrx[i][k] * mtrx[k][j]);
	  }
	  mtrx[i][j] = sum;
	}
      }
    }
    /* initialize search for largest pivot element */
    column_max = 0.0;
    for( i=j; i<N; i++ ) {
      sum = mtrx[i][j];
      if( j > 0 ) {
	for( k=0; k<j; k++ ) {
	  sum = sum - (mtrx[i][k] * mtrx[k][j]);
	}
	mtrx[i][j] = sum;
      }
      /* measurement of merit for pivot element */
      merit = rescale[i] * fabs(sum);
      if( merit >= column_max ) {
	column_max = merit;
	imax = i;
      }
    }
    /* need to interchange rows? */
    if( j != imax ) {
      for( k=0; k<N; k++ ) {
	merit = mtrx[imax][k];
	mtrx[imax][k] = mtrx[j][k];
	mtrx[j][k] = merit;
      }
      /* also interchange scale factor */
      rescale[imax] = rescale[j];
    }
    pivots[j] = imax;
    /* divide by pivot element */
    if( j != N ) {
      merit = 1.0 / mtrx[j][j];
      for( i=j+1; i<N; i++ ) {
	mtrx[i][j] = mtrx[i][j] * merit;
      }
    }
  } /* repeat for next column */
}

/*
 * Subroutine:	lu_backsub
 * Purpose:	LowerUpper backsubstitute one column to solve matrix inversion
 */
static void lu_backsub ( lumtrx, pivots, result )
     float lumtrx[N][N];
     int pivots[N];
     float result[N];
{
  int index, pivot;
  double sum;
  int i, j;

  index = (-1);
  /* do forward substitution and unscramble permutations */
  for( i=0; i<N; i++ ) {
    pivot = pivots[i];
    sum = result[pivot];
    result[pivot] = result[i];
    if( index >=0 ) {
      for( j=index; j<i; j++ ) {
	sum = sum - (lumtrx[i][j] * result[j]);
      }
    }
    /* start summing after first non-zero element */
    else if( sum != 0 ) index = i;
    result[i]= sum;
  }
  /* do back substitution */
  for( i=N-1; i>=0; i-- ) {
    sum = result[i];
    if( i < (N-1) ) {
      for( j=i+1; j<N; j++ ) {
	sum = sum - (lumtrx[i][j] * result[j]);
      }
    }
    /* store component of solution vector */
    result[i] = sum / lumtrx[i][i];
  }
}

#ifdef SAMPLE
/*
 * invert a transform matrix, gives equations for opposite transform
 */
void invert_matrix ( mtrx, inverse )
     float mtrx[N][N], inverse[N][N];
{
  float scratch[N][N], column[N];
  int pivots[N];
  int i, j;

  /* set up identity matrix and copy of matrix a */
  for( i=0; i<N; i++ ) {
    for( j=0; j<N; j++ ) {
      inverse[i][j] = 0.0;
      scratch[i][j] = mtrx[i][j];
    }
    inverse[i][i] = 1.0;
  }
  /* lower/upper decompose matrix just once */
  lu_decompose ( scratch, pivots );
  /* find inverse column by column */
  for( j=0; j<N; j++ ) {
    for( i=0; i<N; i++ ) {
      column[i] = inverse[i][j];
    }
    /* backsubstitute one column at a time */
    lu_backsub( scratch, pivots, column );
    for( i=0; i<N; i++ ) {
      inverse[i][j] = column[i];
    }
  }
}
#endif
