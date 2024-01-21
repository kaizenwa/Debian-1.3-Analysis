/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:	27.xii.1995
 *
 *	header file for screen_sys
 */

#ifndef _pics_h_
#define _pics_h_

/*
 * dimensions of the various windows (those are fixed!)
 */

#define MkBitmap(bits, w, h) 			XCreateBitmapFromData(	\
			XtDisplay(main_screen),				\
			RootWindowOfScreen(XtScreen(main_screen)),	\
			bits, w, h);


#define MkBitPixmap(bits, w, h, fg, bg) 	XCreatePixmapFromBitmapData( \
			XtDisplay(main_screen),				\
			RootWindowOfScreen(XtScreen(main_screen)),	\
			bits, w, h, fg, bg, 1);
#ifdef XPM
#define MkXpmPixmap(name, pic_st)		\
		(pic_st).attr.valuemask = 0L;	\
		(pic_st).attr.valuemask |= XpmReturnPixels;	\
		(pic_st).attr.valuemask |= XpmColormap;	\
		(pic_st).attr.valuemask |= XpmCloseness;	\
		(pic_st).attr.valuemask |= XpmDepth;	\
		(pic_st).attr.depth = the_depth; \
		(pic_st).attr.colormap = the_colormap; \
		(pic_st).attr.closeness = 0xFFFF; \
		XpmCreatePixmapFromData( \
			XtDisplay(main_screen),				\
			RootWindowOfScreen(XtScreen(main_screen)),	\
			(name), &((pic_st).pic), &((pic_st).picMask), 	\
			&((pic_st).attr));
#endif



/*
 *	function prototypes
 */
void init_bitmaps(void);
void set_icons(void);

#endif /* _pics_h_ */
