#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	crdrot.c (Coordinate Rotate)
 * Purpose:	Add a rotation to be applied through the transform
 * Subroutine:	rotate_transform		returns: void
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

#include "hfiles/coord.h"	/* coord structs */

/*
 * rotation matrix for coordinates (rotate on perpendicular (Z) axis)
 *
 *    |  cos() sin() 0 |  cos(0)= 1, cos(90)= 0, cos(180)=-1, cos(270)= 0
 *    | -sin() cos() 0 |  sin(0)= 0, sin(90)= 1, sin(180)= 0, sin(270)=-1
 *    |   0     0    1 |
 *
 * mirror matrix flips coordinates on 0,0 X or Y axis
 *
 * X: | -1  0  0 |    Y: |  1  0  0 |
 *    |  0  1  0 |       |  0 -1  0 |
 *    |  0  0  1 |       |  0  0  1 |
 *
 * translation matrix to move 0,0 position
 *
 *    |   1     0    0 |  distance to move 0,0 to center
 *    |   0     1    0 |  ((float)([width]or[height] + 1) / 2.0) - ioff
 *    | xmove ymove  1 |  center of first pixel is 0.5(ioff=0.5), 1.0(ioff=0)
 */

/*
 * Subroutine:	rotate_transform
 * Purpose:	Perform requested transformation of image coordinates
 * Method:	Transform img coords to what they would be without rotation
 * Parameter:	rotcode: 0 = no change
 *		 1, 2, 3 = rotation of 90, 180, and 270 degrees
 *		 4 = flip the Y coordinates (top becomes bottom
 *		 5, 6, 7 = rotate the flipped image 90, 180, and 270 degrees
 * Notes:
 * Sequence is applied before conversions already in imgtofile
 *  - starting with img coords,
 *   A. shift origin of img buffer coordinates to exact center of buffer
 *   B. undo rotation of img buffer (about origin - now at center of buffer)
 *   C. shift origin to where it would be in unrotated buffer
 * As each transform is applied to the front, we apply them in reverse order
 *  the result is, from img->A,B,C,imgtofile(as given)->file
 */
void rotate_transform ( img, imgtofile, flip, rotcode )
     Coordsys *img;
     Transform *imgtofile;
     int flip;
     int rotcode;
{
  double xmove, ymove;
  int angle;
  static void move_mtrx(), flip_mtrx(), turn_mtrx();

  /* if no rotation, do nothing */
  if( (rotcode == 0) && (flip == 0) ) return;
  /* determine offset from real edge to real center */
  xmove = (double)img->width / 2.0;
  ymove = (double)img->height / 2.0;
  /* shift origin from buffer center (for rotation) to buffer edge */
  move_mtrx(imgtofile, xmove, ymove, 0.5, 1);
  /* flip Y coordinates about 0,0 (center), if requested */
  if( flip )
    flip_mtrx (imgtofile, 1);
  /* rotate image about 0,0 (center), use -angle to undo rotation of buffer */
  /* shift origin to 0,0 in rotated buffer */
  if( rotcode != 0 ) {
    angle = rotcode * -90;
    turn_mtrx(imgtofile, angle, 1);
  }
  /* shift origin from buffer corner to buffer center (center of rotation) */
  move_mtrx(imgtofile, -xmove, -ymove, 0.5, 1);
}

/*
 * Subroutine:	move_mtrx
 * Purpose:	Apply a given linear translation to the passed Transform
 * Parameter:	prior: 1: move, do transform, move, 0: do transform, move
 * Parameter:	ioff: offset of integer coord (i.e. 0.5), needed for prior=1
 * Note:	ioff is not used for turn or flip, as it is assumed that both
 * 		will be bracketed between two moves (to place origin at center)
 */
static void move_mtrx ( mtrx, xtran, ytran, ioff, prior )
     Transform *mtrx;
     double xtran, ytran;
     double ioff;
     int prior;
{
  Transform translate;
  static void mult_mtrx();

  bzero((char *)(&translate), sizeof(Transform));
  translate.inx_outx = 1.0;
  translate.iny_outy = 1.0;
  translate.add_outx = (float)xtran;
  translate.add_outy = (float)ytran;
  if( prior ) {
    translate.iadd_outx = (float)(xtran + ioff);
    translate.iadd_outy = (float)(ytran + ioff);
    mult_mtrx(mtrx, &translate, mtrx);
  } else
    mult_mtrx(mtrx, mtrx, &translate);
}

/*
 * Subroutine:	flip_mtrx
 * Purpose:	Apply a mirror reflection to the passed Transform
 */
static void flip_mtrx ( mtrx, prior )
     Transform *mtrx;
     int prior;		/* i: 1: move, transform, move, 0: transform, move */
{
  Transform reflect;
  static void mult_mtrx();

  bzero((char *)(&reflect), sizeof(Transform));
  reflect.inx_outx = 1.0;
  reflect.iny_outy = -1.0;
  if( prior )
    mult_mtrx(mtrx, &reflect, mtrx);
  else
    mult_mtrx(mtrx, mtrx, &reflect);
}

/*
 * Subroutine:	turn_mtrx
 * Purpose:	Apply given angular rotation to the passed Transform
 * Note:	the pivot is 0,0 of the passed Transform's coordinate system
 * Exception:	only multiples of 90 degrees are allowed
 */
static void turn_mtrx ( mtrx, angle, prior )
     Transform *mtrx;
     int angle;
     int prior;		/* i: 1: move, transform, move, 0: transform, move */
{
  Transform rotate;
  void exit_errmsg();
  static void mult_mtrx();

  while( angle >= 360 ) angle -= 360;
  while( angle < 0 ) angle += 360;
  bzero((char *)(&rotate), sizeof(Transform));
  switch( angle ) {
  case 0:
    rotate.inx_outx = 1.0;
    rotate.iny_outy = 1.0;
    rotate.no_rot = 1;
    break;
  case 90:
    rotate.inx_outy = 1.0;
    rotate.iny_outx = -1.0;
    rotate.no_rot = 0;
    break;
  case 180:
    rotate.inx_outx = -1.0;
    rotate.iny_outy = -1.0;
    rotate.no_rot = 1;
    break;
  case 270:
    rotate.inx_outy = -1.0;
    rotate.iny_outx = 1.0;
    rotate.no_rot = 0;
    break;
  default:
    exit_errmsg("Non-orthogonal rotate (90,180,270)");
    break;
  }
  if( prior )
    mult_mtrx(mtrx, &rotate, mtrx);
  else
    mult_mtrx(mtrx, mtrx, &rotate);
}

/*
 * Subroutine:	mult_mtrx
 * Purpose:	Multiply two sets of transformation matrices to produce a
 *		new combined matrix
 * Method:	algorithm is 3x3 matrix multiplication (3rd row is 0 0 1)
 *		[i][j] is sum of products of column j of 1st and row i of 2nd
 * Note:	iadd is computed to be able to transform from integer coords
 *  calculation uses integer input offset of first and float offset of second
 */
static void mult_mtrx ( new, first, second )
     Transform *new;	/* i/o: gets result, can be same as first or second */
     Transform *first;	/* i: matrices in non-commutative multiply */
     Transform *second; /* i: as above (new = 1st * 2nd) */
{
  Transform temp;

  temp.inx_outx = ((second->inx_outx * first->inx_outx) +
	      (second->iny_outx * first->inx_outy));
  temp.iny_outx = ((second->inx_outx * first->iny_outx) +
	      (second->iny_outx * first->iny_outy));
  temp.add_outx = ((second->inx_outx * first->add_outx) +
	      (second->iny_outx * first->add_outy) + second->add_outx);
  temp.iadd_outx = ((second->inx_outx * first->iadd_outx) +
	       (second->iny_outx * first->iadd_outy) + second->add_outx);
  temp.inx_outy = ((second->inx_outy * first->inx_outx) +
		   (second->iny_outy * first->inx_outy));
  temp.iny_outy = ((second->inx_outy * first->iny_outx) +
		   (second->iny_outy * first->iny_outy));
  temp.add_outy = ((second->inx_outy * first->add_outx) +
		   (second->iny_outy * first->add_outy) + second->add_outy);
  temp.iadd_outy = ((second->inx_outy * first->iadd_outx) +
		    (second->iny_outy * first->iadd_outy) + second->add_outy);
  bcopy((char *)&temp, (char *)new, sizeof(Transform));
}
