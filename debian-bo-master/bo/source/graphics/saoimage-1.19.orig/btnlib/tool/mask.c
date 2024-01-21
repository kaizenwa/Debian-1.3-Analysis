#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mask.c (Image Mask)
 * Subroutine:	btn_MakeLabelMask()		returns: char *
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
 * Subroutine:	btn_MakeLabelMask
 * Purpose:	make a mask that surrounds the given image
 * Note:	mask extends 1 pixel in every direction beyond given image
 */
char *btn_MakeLabelMask ( icon, width, height )
     char *icon;
     int width, height;
{
  char *mask;
  char *icon_left, *icon_pix, *icon_right;
  char *mask_above, *mask_pix, *mask_below;
  char *above, *pix, *below;
  char *icon_next;
  int i, j;
  char *btn_Alloc();

  /* allocate mask space */
  mask = btn_Alloc(width*height, 1, "mask map");
  height--;
  icon_next = icon;
  mask_pix = mask;
  mask_below = mask;
  for( i=0; i<=height; i++ ) {
    /* set up mask pointers for this line */
    mask_above = mask_pix;
    mask_pix = mask_below;
    if( i < height )
      mask_below += width;
    above = mask_above;
    pix = mask_pix;
    below = mask_below;
    /* set up icon pointers for this (next) line */
    icon_pix = icon_next;
    icon_left = icon_pix;
    icon_right = icon_pix + 1;
    icon_next = icon_pix + width;
    for( j=0; j<width; j++ ) {
      if( (*icon_left != 0) || (*icon_pix != 0) || (*icon_right != 0) ) {
	*above = 1;
	*pix = 1;
	*below = 1;
      }
      icon_left = icon_pix;
      icon_pix = icon_right;
      if( ++icon_right >= icon_next )
	icon_right--;
      above++;
      pix++;
      below++;
    }
  }
  return( mask );
}
