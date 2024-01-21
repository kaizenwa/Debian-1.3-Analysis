#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	clrinit.c (Color Initialize)
 * Purpose:	Allocate the needed colors
 * Subroutine:	init_color()			returns: void
 * Xlib calls:	XGetVisualInfo(), XFree()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* color structs */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/control.h"	/* to find verbose */
#include "hfiles/define.h"	/* YES, NO, MIN, MAX and more */
extern struct controlRec control;	/* decide how much to print */

#define MINPLANES 4
#define MAXPLANES 8

static int screen;
static XVisualInfo default_vinfo;	/* info about the default visual */
static XVisualInfo private_vinfo;	/* info about the our own visual */
static Display *display;

/*
 * Subroutine:	init_color
 * Purpose:	Claim and set up colors (and do any needed initial work)
 */
void init_color ( color, init )
     struct colorRec *color;
     int init;
{
  int alloc_colors();
  void exit_errmsg(), init_hard_colors(), init_halftone();
  void free_color_cells(), lookup_cursor_colors(), free_cursor_cell_color();
  void init_overlay_color(), init_cell_color(), init_halftone_color();
  static int init_visual();

  if( init ) {
    display = color->display;
    if( init_visual(color, MINPLANES) == 0 ) {
      color->colormap_mode = VOP_Halftone;
    } else if( color->colormap_mode != VOP_Halftone ) {
      init_hard_colors(color, color->map.default_colormap);
      lookup_cursor_colors(color, color->map.default_colormap, 1);
    }
    /* set pointers for halftone tools */
    init_halftone();
  } else if( color->colors_alloced ) {
    /* free currently alloc'ed colors */
    free_color_cells(color, color->colormap);
    if( (color->old_mode != color->colormap_mode) &&
        (color->colormap_mode == VOP_Cells) )
      /* free read-only cursor colors if we think they won't be needed */
      free_cursor_cell_color(color);
  }
  switch( color->colormap_mode ) {
  case VOP_PseudoColor:
  case VOP_StaticColor:
    if( color->cells.overlay ) {
      if( alloc_colors(color) ) {
	init_overlay_color(color);
	if( control.verbose )
	  (void)printf("%d color levels with overlay reserved\n",
		       color->ncolors);
	color->old_mode = VOP_Overlay;
	break;
      }
      color->cells.overlay = 0;
    }
    if( alloc_colors(color) ) {
      init_cell_color(color);
      if( control.verbose )
	(void)printf("%d color levels reserved\n", color->ncolors);
      color->old_mode = VOP_Cells;
      break;
    }
    color->colormap_mode = VOP_Halftone;
  case VOP_Halftone:
    color->colormap = color->map.default_colormap;
    color->cells.overlay = 0;
    init_halftone_color(color);
    color->old_mode = VOP_Halftone;
    break;
  default:
    exit_errmsg("Unknown color mode");
  }
  color->cursor_overlay = color->cells.overlay;
}

/*
 * Subroutine:	init_visual
 * Purpose:	Get basic info about the color hardware on hand
 */
static int init_visual ( color, mindepth )
     struct colorRec *color;
     int mindepth;		/* i: minimum planes needed for color */
{
  int verify_pseudocolor();
  void exit_errmsg();

  display = color->display;
  if( color->screen < 0 )
    color->screen = DefaultScreen(display);
  screen = color->screen;
  color->screen_depth = DisplayPlanes(display, screen);
  color->hard.std_white = WhitePixel(display, screen);
  color->hard.std_black = BlackPixel(display, screen);
  color->gcset.menu.foreground = color->hard.std_black;
  color->gcset.menu.background = color->hard.std_white;
  color->gcset.black.background = color->hard.std_white;
  /* initially set visual to default visual */
  color->visual = DefaultVisual(display, screen);
  color->map.default_colormap = DefaultColormap(display, screen);
  if( color->screen_depth < mindepth ) {
    color->map.default_enable = NO;
    color->map.private_enable = NO;
    return( 0 );
  } else {
    XVisualInfo *vinfo;
    int vinfo_cnt, vptr;

    color->map.default_vinfo = &default_vinfo;
    default_vinfo.screen = screen;
    default_vinfo.visual = color->visual;
#ifdef XV11R2
    default_vinfo.visualid = default_vinfo.visual->visualid;
#else
    default_vinfo.visualid = XVisualIDFromVisual(default_vinfo.visual);
#endif
    vinfo = XGetVisualInfo(display, VisualIDMask | VisualScreenMask,
			   &default_vinfo, &vinfo_cnt);
    vptr = 0;
    if( vinfo_cnt > 1 ) {
      /* just take the first visual that matches */
      while( (vptr < vinfo_cnt) &&
	     (vinfo[vptr].visual != default_vinfo.visual) )
	vptr++;
    }
    if( vinfo_cnt <= vptr )
      exit_errmsg("Default visual not found");
    default_vinfo.depth = vinfo[vptr].depth;
    default_vinfo.class = vinfo[vptr].class;
    default_vinfo.colormap_size = vinfo[vptr].colormap_size;
    XFree((char *)vinfo);
    switch( default_vinfo.class ) {
    case PseudoColor:
      color->map.default_enable = YES;
      return( 1 );
    case StaticColor:
    case DirectColor:
    case TrueColor:
    case GrayScale:
    case StaticGray:
    default:
      if( color->colormap_mode == VOP_Halftone ) {
	color->map.default_enable = YES;
	color->map.default_permit = YES;
	default_vinfo.depth = 1;
	return( 0 );
      } else {
	color->map.default_enable = NO;
	color->map.default_permit = NO;
	if( verify_pseudocolor(color, 4, 16) ) {
	  color->map.private_enable = YES;
	  return( 1 );
	} else {
	  color->map.private_enable = NO;
	  color->map.private_permit = NO;
	  return( 0 );
	}
      }
    }
  }
}

/*
 * Subroutine:	verify_pseudocolor
 * Purpose:	Verify that server can provide a satisfactory colormap
 * Returns:	1 if yes, else 0
 * PostState:	Fills in visual info in passed vinfo
 * Xlib calls:	XGetVisualInfo(), XFree()
 */
int verify_pseudocolor ( color, min_depth, min_size )
     struct colorRec *color;
     int min_depth;		/* i: minimum required depth */
     int min_size;		/* i: minimum number of map cells */
{
  XVisualInfo *linfo;		/* l: list returned by XGetVisualInfo */
  int cnt, i, ptr;

  private_vinfo.screen = color->screen;
  private_vinfo.class = PseudoColor;
  linfo = XGetVisualInfo(color->display, VisualClassMask | VisualScreenMask,
			 &private_vinfo, &cnt);
  if( cnt == 0 ) {
    color->map.private_enable = NO;
    color->map.private_permit = NO;
    return( 0 );
  }
  ptr = 0;
  if( cnt > 1 ) {
    for( i=0; i<cnt; i++ ) {
      /* look for best we can do */
      if( (linfo[i].depth >= linfo[ptr].depth) &&
	  (linfo[i].colormap_size >= linfo[ptr].colormap_size) )
	ptr = i;
    }
  }
  if( (linfo[ptr].depth < (unsigned int)min_depth) ||
      (linfo[ptr].colormap_size < min_size) ) {
    XFree((char *)linfo);
    color->map.private_enable = NO;
    color->map.private_permit = NO;
    return( 0 );
  } else {
    bcopy((char *)(&linfo[ptr]), (char *)(&private_vinfo),
	  sizeof(XVisualInfo));
    color->map.private_vinfo = &private_vinfo;
    color->map.private_enable = YES;
    XFree((char *)linfo);
    return( 1 );
  }
}
