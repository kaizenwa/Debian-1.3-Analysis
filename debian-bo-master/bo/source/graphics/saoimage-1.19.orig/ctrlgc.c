#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	ctrlgc.c (Control Graphics Context)
 * Purpose:	Create a GC and set the GC params when needed
 * Subroutine:	init_gc()			returns: void
 * Subroutine:	free_gc()			returns: void
 * Subroutine:	set_gc()			returns: GC
 * Subroutine:	set_gc_with_background()	returns: GC
 * Subroutine:	set_text_gc()			returns: GC
 * Subroutine:	set_edit_gc()			returns: GC
 * Subroutine:	get_fontstruct()		returns: XFontStruct *
 * Xlib calls:	XCreateGC(), XSetMask(), XSetFunction(), XSetForeground()
 * Xlib calls:	XChangeGC(), XLoadQueryFont()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  29 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* get stderr, NULL, etc */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define DONT_CARE, U_DONT_CARE */
#include "hfiles/color.h"	/* color and GCspec param structure */
extern struct colorRec color;

static GC main_gc = NULL;
static Display *main_display;
static XGCValues gcvalue;

#define OPTION_COUNT 6
#define NAME_COUNT 17
static char *fontlist[NAME_COUNT] = {
  NULL,				/* user preference 0 */
  NULL,				/* user preference 1 */
  NULL,				/* user preference 2 */
  NULL,						/* 3 */
  "6x10",					/* 4 */
  "6x13",					/* 5 */
  "8x13",					/* 6 */
  "9x15",					/* 7 */
  "*-courier-medium-r-normal-*-10-*",		/* 8 */
  "*-helvetica-medium-r-normal-*-10-*",		/* 9 */
  "*-times-medium-r-normal-*-10-*",		/* 10 */
  "*-courier-medium-r-normal-*-12-*",		/* 11 */
  "*-helvetica-medium-r-normal-*-12-*",		/* 12 */
  "*-times-medium-r-normal-*-12-*",		/* 13 */
  "*-courier-medium-r-normal-*-14-*",		/* 14 */
  "*-helvetica-medium-r-normal-*-14-*",		/* 15 */
  "*-times-medium-r-normal-*-14-*" };		/* 16 */
static XFontStruct *fontstruct[NAME_COUNT] =
  { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL };
/* order to try fonts from list for particular application type */ 
static int preference[3][OPTION_COUNT] = {
  { 0,  5,  9,  10,  4, 8 },	/* small for color graph */
  { 1,  5, 11, 12, 13,  6 },	/* medium for cursor label */
  { 2,  5,  7, 15, 16, 14 } }; 	/* large for popup editor */
static XFontStruct *app_font[3] = { NULL, NULL, NULL };

/*
 * Subroutine:	init_gc()
 * Purpose:	Create the gc and set initial values
 * Xlib calls:	XCreateGC()
 */
void init_gc ( display, window )
     Display *display;
     Window window;
{
  unsigned long valuemask;

  main_display = display;
  gcvalue.function = GXcopy;
  gcvalue.plane_mask = AllPlanes;
  gcvalue.foreground = color.gcset.menu.foreground;
  gcvalue.background = color.gcset.menu.background;
  /* set mask for values which are not same as default */
  valuemask = GCForeground | GCBackground;
  main_gc = XCreateGC(display, window, valuemask, &gcvalue);
}

/*
 * Subroutine:	free_gc
 * Purpose:	free resources loaded by server before exiting
 */
void free_gc ( )
{
  int i;

  for( i=0; i<NAME_COUNT; i++ ) {
    if( fontstruct[i] != NULL )
      XFreeFont (main_display, fontstruct[i]);
  }
  if( main_gc != NULL )
    XFreeGC(main_display, main_gc);
}
  
/*
 * Subroutine:	set_gc()
 * Purpose:	Adjust gc params as specified for simple drawing
 * Xlib calls:	XSetMask(), XSetFunction(), XSetForeground()
 */
GC set_gc ( spec )
     GCspec *spec;
{
  if( spec != NULL ) {
    if( spec->mask != gcvalue.plane_mask ) {
      gcvalue.plane_mask = spec->mask;
      XSetPlaneMask(main_display, main_gc, gcvalue.plane_mask);
    }
    if( spec->func != gcvalue.function ) {
      gcvalue.function = spec->func;
      XSetFunction(main_display, main_gc, gcvalue.function);
    }
    if( (spec->foreground != U_DONT_CARE) &&
       (spec->foreground != gcvalue.foreground) ) {
      gcvalue.foreground = spec->foreground;
      XSetForeground(main_display, main_gc, gcvalue.foreground);
    }
  }
  return( main_gc );
}

/*
 * Subroutine:	set_gc_with_background
 * Purpose:	Adjust gc params as specified for drawing with a background
 * Xlib calls:	XSetMask(), XSetFunction(), XSetForeground(), XSetBackground()
 */
GC set_gc_with_background ( spec, background )
     GCspec *spec;
     unsigned long background;
{
  if( spec->mask != gcvalue.plane_mask ) {
    gcvalue.plane_mask = spec->mask;
    XSetPlaneMask(main_display, main_gc, gcvalue.plane_mask);
  }
  if( spec->func != gcvalue.function ) {
    gcvalue.function = spec->func;
    XSetFunction(main_display, main_gc, gcvalue.function);
  }
  if( (spec->foreground != U_DONT_CARE) &&
      (spec->foreground != gcvalue.foreground) ) {
    gcvalue.foreground = spec->foreground;
    XSetForeground(main_display, main_gc, gcvalue.foreground);
  }
  if( (background != U_DONT_CARE) && (background != gcvalue.background) ) {
    gcvalue.background = background;
    XSetBackground(main_display, main_gc, gcvalue.background);
  }
  return( main_gc );
}

/*
 * Subroutine:	set_text_gc
 * Purpose:	Adjust gc params as specified for basic text
 * Xlib calls:	XChangeGC()
 * Note:	Most gc values are cached, but new font flushes vals to server
 */
GC set_text_gc ( font, foreground, background, func, mask )
     Font font;
     unsigned long foreground;
     unsigned long background;
     int func;
     unsigned long mask;
{
  unsigned long valuemask = 0;

  if( font != gcvalue.font ) {
    gcvalue.font = font;
    valuemask = GCFont;
  }
  if( foreground != gcvalue.foreground ) {
    gcvalue.foreground = foreground;
    valuemask |= GCForeground;
  }
  if( (background != U_DONT_CARE) && (background != gcvalue.background) ) {
    gcvalue.background = background;
    XSetBackground(main_display, main_gc, gcvalue.background);
  }
  if( mask != gcvalue.plane_mask ) {
    gcvalue.plane_mask = mask;
    valuemask |= GCPlaneMask;
  }
  if( func != gcvalue.function ) {
    gcvalue.function = func;
    valuemask |= GCFunction;
  }
  if( valuemask )
    XChangeGC(main_display, main_gc, valuemask, &gcvalue);
  return( main_gc );
}

/*
 * Subroutine:	set_edit_gc
 * Purpose:	Adjust gc params as specified for text with a background
 * Xlib calls:	XChangeGC()
 */
GC set_edit_gc ( font, foreground, background )
     Font font;
     unsigned long foreground;
     unsigned long background;
{
  unsigned long valuemask = 0;

  if( font != gcvalue.font ) {
    gcvalue.font = font;
    valuemask = GCFont;
  }
  if( AllPlanes != gcvalue.plane_mask ) {
    gcvalue.plane_mask = AllPlanes;
    valuemask |= GCPlaneMask;
  }
  if( GXcopy != gcvalue.function ) {
    gcvalue.function = GXcopy;
    valuemask |= GCFunction;
  }
  if( foreground != gcvalue.foreground ) {
    gcvalue.foreground = foreground;
    valuemask |= GCForeground;
  }
  if( background != gcvalue.background ) {
    gcvalue.background = background;
    valuemask |= GCBackground;
  }
  if( valuemask )
    XChangeGC(main_display, main_gc, valuemask, &gcvalue);
  return( main_gc );
}

/*
 * Subroutine:	get_fontstruct
 * Returns:	Pointer to the specified fontstruct
 */
XFontStruct *get_fontstruct ( app_code )
     int app_code;	/* i: see comments above */
{
  static int init_font();

  if( (app_font[app_code] == NULL) && (init_font(app_code) == NULL) )
    return( NULL );
  else
    return( app_font[app_code] );
}

/*
 * Subroutine:	init_font
 * Purpose:	Load the fonts used by this program
 * Returns:	Font on success, else 0
 */
static int init_font ( app_code )
     int app_code;	/* i: font application type index */
{
  int i, name_index;
  static int open_font();

  for( i=0; i<OPTION_COUNT; i++ ) {
    name_index = preference[app_code][i];
    if( (fontstruct[name_index] != NULL) || open_font(name_index) ) {
      app_font[app_code] = fontstruct[name_index];
      return( 1 );
    }
  }
  return( 0 );
}

/*
 * Subroutine:	open_font
 * Purpose:	Load a font or announce failure
 * Returns:	1 on success, else 0
 * Xlib calls:	XLoadQueryFont()
 */
static int open_font ( index )
     int index;		/* i: index of font name in font_name array */
{
  char *name;

  if( (name = fontlist[index]) != NULL ) {
    if( (fontstruct[index] = XLoadQueryFont(main_display, name)) != NULL ) {
      return( 1 );
    } else {
      /* report failure and scratch name off of list */
      (void)fprintf(stderr, "Could not open font: %s\n", name);
      fontlist[index] = NULL;
    }
  }
  return( 0 );
}
