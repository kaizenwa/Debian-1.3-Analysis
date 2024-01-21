#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	image.c (Make XImage)
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Stencil label icons onto blank (or prepared) button border
 *		bitmaps and install as XImage's in the button record
 * Subroutines:	btn_MakeXImages()		returns: void
 * Subroutines:	static btn_AddLabel()		returns: void
 * Subroutines:	static btn_LabelX()		returns: int
 * Subroutines:	static btn_LabelY()		returns: int
 * Xlib calls:	XCreateImage()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		18 March 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* define NULL */
#include <X11/Xlib.h>
#include "buttons.h"

/*
 * Subroutine:	btn_MakeXImages
 * Purpose:	Create XImage bitmaps for four states of a button
 * Returns:	void
 * Called by:	btn_LabelButtons() in MakeBox.c
 * Uses:	btn_Alloc() in BtnAlloc.c
 * Uses:	btn_AddLabel() below;
 * Xlib calls:	XCreateImage() with XYBitmap constant.
 * Pre-state:	four model bitmaps with the border pattern installed
 * Post-state:	off_out, off_in, on_out, on_in XImage's set in button record.
 * Exception:	Only labels present are installed.
 */
void btn_MakeXImages ( button, width, height, byte_width, visual,
		       off_out, off_in, on_out, on_in, mapsz, off, on, motf )
     ButtonRecord *button;	/* i: main structure describing button */
     int width;			/* i: width in pixels of button */
     int height;		/* i: height in pixels of button */
     int byte_width;		/* i: width in bytes of the button bitmap */
     Visual *visual;		/* i: visual for XCreateImage() */
     unsigned char *off_out;	/* i: filled bitmap with only border pattern */
     unsigned char *off_in;	/* i: filled bitmap with only border pattern */
     unsigned char *on_out;	/* i: filled bitmap with only border pattern */
     unsigned char *on_in;	/* i: filled bitmap with only border pattern */
     int mapsz;			/* i: size of button bitmap in bytes */
     int off, on;		/* i: reverse video when in on or off state */
     int motf;			/* i: don't use occupied labels */
{
  unsigned char *data;		/* l: pointer to bitmap currently processing */
  XImage *ximage;
  char *btn_Alloc();
  static void btn_AddLabel();

#ifdef ALLIANT
  extern int nbutton;
#endif

  /* put the dimensions in the button record */
  button->width = (unsigned int)width;
  button->height = (unsigned int)height;
  /* make bitmap for unoccupied off state */
  data = (unsigned char *)btn_Alloc(mapsz, sizeof(char), "button bitmap");
  bcopy((char *)off_out, (char *)data, mapsz);
  if( button->look->off_out_1 != NULL )
    btn_AddLabel(button->look->off_out_1,
		 width, height, byte_width, data, off);
  if( button->look->off_out_2 != NULL )
    btn_AddLabel(button->look->off_out_2,
		 width, height, byte_width, data, off);
  ximage = XCreateImage
    (button->display, visual, (unsigned int)1, XYBitmap, 0, (char *)data,
     (unsigned int)width, (unsigned int)height, 8, byte_width);
  /* set for "bitmap" standard order */
  ximage->byte_order = LSBFirst;
  ximage->bitmap_bit_order = LSBFirst;
  button->image[OFF_OUT] = ximage;
  /* make bitmap for occupied off state */
  data = (unsigned char *)btn_Alloc(mapsz, sizeof(char), "button bitmap");
  bcopy((char *)off_in, (char *)data, mapsz);
  if( motf ) {
    if( button->look->off_out_1 != NULL )
      btn_AddLabel(button->look->off_out_1,
		   width, height, byte_width, data, off);
    if( button->look->off_out_2 != NULL )
      btn_AddLabel(button->look->off_out_2,
		   width, height, byte_width, data, off);
  } else {
    if( button->look->off_in_1 != NULL )
      btn_AddLabel(button->look->off_in_1,
		   width, height, byte_width, data, off);
    if( button->look->off_in_2 != NULL )
      btn_AddLabel(button->look->off_in_2,
		   width, height, byte_width, data, off);
  }
  ximage = XCreateImage
    (button->display, visual, (unsigned int)1, XYBitmap, 0, (char *)data,
     (unsigned int)width, (unsigned int)height, 8, byte_width);
  /* set for "bitmap" standard order */
  ximage->byte_order = LSBFirst;
  ximage->bitmap_bit_order = LSBFirst;
  button->image[OFF_IN] = ximage;
  /* make bitmap for unoccupied on state */
  data = (unsigned char *)btn_Alloc(mapsz, sizeof(char), "button bitmap");
  bcopy((char *)on_out, (char *)data, mapsz);
  if( motf > 1 ) {
    if( button->look->on_in_1 != NULL )
      btn_AddLabel(button->look->on_in_1, width, height, byte_width, data, on);
    if( button->look->on_in_2 != NULL )
      btn_AddLabel(button->look->on_in_2, width, height, byte_width, data, on);
  } else {
    if( button->look->on_out_1 != NULL )
      btn_AddLabel(button->look->on_out_1,
		   width, height, byte_width, data, on);
    if( button->look->on_out_2 != NULL )
      btn_AddLabel(button->look->on_out_2,
		   width, height, byte_width, data, on);
  }
  ximage = XCreateImage
    (button->display, visual, (unsigned int)1, XYBitmap, 0, (char *)data,
     (unsigned int)width, (unsigned int)height, 8, byte_width);
  /* set for "bitmap" standard order */
  ximage->byte_order = LSBFirst;
  ximage->bitmap_bit_order = LSBFirst;
  button->image[ON_OUT] = ximage;
  /* make bitmap for occupied on state */
  data = (unsigned char *)btn_Alloc(mapsz, sizeof(char), "button bitmap");
  bcopy((char *)on_in, (char *)data, mapsz);
  if( motf == 1 ) {
    if( button->look->on_out_1 != NULL )
      btn_AddLabel(button->look->on_out_1,
		   width, height, byte_width, data, on);
    if( button->look->on_out_2 != NULL )
      btn_AddLabel(button->look->on_out_2,
		   width, height, byte_width, data, on);
  } else {
    if( button->look->on_in_1 != NULL )
      btn_AddLabel(button->look->on_in_1, width, height, byte_width, data, on);
    if( button->look->on_in_2 != NULL )
      btn_AddLabel(button->look->on_in_2, width, height, byte_width, data, on);
  }
  ximage = XCreateImage
    (button->display, visual, (unsigned int)1, XYBitmap, 0, (char *)data,
     (unsigned int)width, (unsigned int)height, 8, byte_width);
  /* set for "bitmap" standard order */
  ximage->byte_order = LSBFirst;
  ximage->bitmap_bit_order = LSBFirst;
  button->image[ON_IN] = ximage;
#ifdef ALLIANT
  button->id[OFF_OUT] = nbutton++;
  button->id[OFF_IN]  = nbutton++;
  button->id[ON_OUT]  = nbutton++;
  button->id[ON_IN]   = nbutton++;
#endif
}

/*
 * Subroutine:	btn_AddLabel
 * Purpose:	Add the label icon to a button's bitmap.
 * Returns:	void
 * Called by:	btn_CreateButtons()
 * Uses:	btn_StencilLabel() in StencilLbl.c
 * Uses:	btn_LabelX() or btn_LabelY() below
 * Xlib calls:	none
 * Pre-state:	bitmap already contains border pattern
 * Post-state:	bitmap has label stenciled on
 * Exception:	Label is not stenciled if one dimension would be zero.
 * Method:	Get basic alignment parameters and call btn_StencilLabel();
 */
static void btn_AddLabel ( label, width, height, byte_width, bitmap, inverse )
     ButtonLabel *label;	/* i: record of label bitmap to stencil in */
     int width;			/* i: width in pixels of button */
     int height;		/* i: height in pixels of button */
     int byte_width;		/* i: width in bytes of the button bitmap */
     unsigned char *bitmap;
     int inverse;		/* i: invert the bits for reverse video look */
{
  int src_x, src_y;	/* l: coords of start of stencil in src (label) */
  int dst_x, dst_y;	/* l: coords of start of stencil in dst (button) */
  int xwdth;		/* l: width (cols) to stencil */
  int yhght;		/* l: number of lines (rows) to stencil */
  static int btn_LabelX(), btn_LabelY();
  void btn_StencilLabel();

  if( ((xwdth = btn_LabelX(label, width, &src_x, &dst_x)) > 0) &&
      ((yhght = btn_LabelY(label, height, &src_y, &dst_y)) > 0) ) {
    btn_StencilLabel(label->label, label->mask, bitmap,
		     label->bytes_per_line, byte_width, src_x, src_y,
		     dst_x, dst_y, xwdth, yhght, inverse);
  }
}

/*
 * Subroutine:	btn_LabelX
 * Purpose:	Determine width and x coordinates to copy label onto the button
 * Returns:	width to copy
 * Called by:	btn_AddLabel() above
 * Uses:	nothing
 * Xlib calls:	none
 * Pre-state:	label structure with 
 * Post-state:	dst bitmap has label stenciled on
 * Method:	If there is enough room, the entire label is copied.  If not,
 *		the x_clip value indicates what to cut off.  If x_clip=0, the
 *		same amount is removed from both sides.  If x_clip!=0, use
 *		the breaks array to select a specific break point (usually
 *		representing the breaks between characters).  If x_clip>0
 *		cut from the left, if x_clip<0 cut from the left.
 *		The postition within the button is determined by x_ratio:
 *		0 is at the left, .5 in the middle, 1 at the right, and then
 *		offset x_offset pixels.  This has no meaning when cutting was
 *		needed, as the label will fill the entire width.
 */
static int btn_LabelX ( label, width, lbl_x, btn_x )
     ButtonLabel *label;
     int width;
     int *lbl_x;
     int *btn_x;
{
  int xwdth;

  /* if full label height fits */
  if( width >= label->width ) {
    *lbl_x = 0;
    xwdth = label->width;
  /* else label must be clipped to fit */
  } else {
    /* clip symmetrically on both ends, use button's width */
    if( label->x_clip == 0 ) {
      *lbl_x = (label->width - width) / 2;
      *btn_x = 0;
      return( width );
    /* clip at given quanta */
    } else {
      /* find the appropriate length */
      int stop;

      stop = label->x_clip;
      if( stop < 0 )
	stop = -stop;
      while( ((--stop) >= 0) && (label->breaks[stop] > width) );
      if( stop < 0 )
	return( 0 );
      else
	xwdth = label->breaks[stop];
      /* set the appropriate starting offset */
      if( label->x_clip > 0 )
	*lbl_x = 0;
      else
	*lbl_x = label->width - xwdth;
    }
  }
  /* determine where to start in button (first ideal, then adjest if needed) */
  *btn_x = ((width - xwdth) * label->x_ratio) + label->x_offset;
  if( *btn_x < 0 ) {
    *btn_x = 0;
  } else if( *btn_x > (width - xwdth) ) {
    *btn_x = width - xwdth;
  }
  return( xwdth );
}

/*
 * Subroutine:	btn_LabelY
 * Purpose:	Determine height and y coordinates to copy label onto button
 * Returns:	height to copy
 * Called by:	btn_AddLabal() above
 * Uses:	nothing
 * Xlib calls:	none
 * Pre-state:	label structure with 
 * Post-state:	dst bitmap has label stenciled on
 * Method:	If there is enough room, the entire label is copied.  If not,
 *		the y_clip value indicates what to cut off.  If y_clip=0, the
 *		same amount is removed from both sides.  If y_clip!=0, use
 *		the breaks array to select a specific break point (usually
 *		representing the breaks between characters).  If y_clip>0
 *		cut from the left, if y_clip<0 cut from the left.
 *		The postition within the button is determined by y_ratio:
 *		0 is at the left, .5 in the middle, 1 at the right, and then
 *		offset y_offset pixels.  This has no meaning when cutting was
 *		needed, as the label will fill the entire height.
 */
static int btn_LabelY ( label, height, lbl_y, btn_y )
     ButtonLabel *label;
     int height;
     int *lbl_y;
     int *btn_y;
{
  int yhght;

  /* if full label height fits */
  if( height >= label->height ) {
    *lbl_y = 0;
    yhght = label->height;
  /* else label must be clipped to fit */
  } else {
    /* clip symmetrically on both ends, use button's height */
    if( label->y_clip == 0 ) {
      *lbl_y = (label->height - height) / 2;
      *btn_y = 0;
      return( height );
    /* clip at given quanta */
    } else {
      /* find the appropriate length */
      int stop;

      stop = label->y_clip;
      if( stop < 0 )
	stop = -stop;
      while( ((--stop) >= 0) && (label->breaks[stop] > height) );
      if( stop < 0 )
	return( 0 );
      else
	yhght = label->breaks[stop];
      /* set the appropriate starting offset */
      if( label->y_clip > 0 )
	*lbl_y = 0;
      else
	*lbl_y = label->height - yhght;
    }
  }
  /* determine where to start in button (first ideal, then adjest if needed) */
  *btn_y = ((height - yhght) * label->y_ratio) + label->y_offset;
  if( *btn_y < 0 ) {
    *btn_y = 0;
  } else if( *btn_y > (height - yhght) ) {
    *btn_y = height - yhght;
  }
  return( yhght );
}
