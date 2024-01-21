#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	rotate.c (Rotate Pixmap Image)
 * Subroutine:	btn_RotateImage()	returns: char *
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

/*
 * Subroutine:	btn_RotateImage
 * Purpose:	given a Z image, rotate it 90 degrees according to the sign
 * Returns:	rotated copy of given image
 */
char *btn_RotateImage ( image, width, height, sign )
     char *image;
     int *width, *height;
     int sign;
{
  char *rimage;
  char *flip, *img;
  int i, j, inc;
  int wdth, hght;
  int offset;
  char *btn_Alloc();

  wdth = *width;
  hght = *height;
  offset = wdth * hght;
  rimage = btn_Alloc(offset, 1, "rotated image");
  flip = rimage;
  offset -= wdth;
  if( sign > 0 )
    inc = -wdth;
  else
    inc = wdth;
  for( i=0; i<wdth; i++ ) {
    flip = rimage + (hght * i);
    if( sign > 0 )
      img = (image + i) + offset;
    else
      img = image + (wdth - i) - 1;
    for( j=0; j<hght; j++ ) {
      *flip++ = *img;
      img += inc;
    }
  }
  *height = wdth;
  *width = hght;
  free(image);
  return( rimage );
}
