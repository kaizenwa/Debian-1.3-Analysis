/*
 * Programm XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: graphics
 * graphics using X11 standard
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as by published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Intrinsic.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define _GRAPHICS_C

#include "include.h"
#include "mytypes.h"
#include "const.h"
#include "block.h"
#include "sprite.h"
#include "expl.h"
#include "score.h"
#include "graphics.h"
#include "setup.h"
#include "event.h"
#include "map.h"
#include "main.h"

#define BELL_VOLUME 80

#define QUIT_WITH_CONTROL_Q TRUE
#define USE_COPY_AREA FALSE

  /* Some constants */

#define NORMAL_EVENT_MASK ( ExposureMask | StructureNotifyMask | KeyPressMask )

/*
 * global variables
 */
int colorMode[MAX_DISPLAY];
Display *dpy[MAX_DISPLAY] = {NULL, NULL, NULL, NULL, NULL, NULL};

/*
 * local Variables 
 */
static Window win[MAX_DISPLAY];
static Colormap cmap[MAX_DISPLAY];
static int def_depth[MAX_DISPLAY];
static int white_pixel[MAX_DISPLAY];
static int black_pixel[MAX_DISPLAY];

static char *XBlastResName  = "xblast";
static char *XBlastResClass = "XBlast";

static Pixmap pix[MAX_DISPLAY];
static Pixmap pix_block[MAX_DISPLAY][MAX_BLOCK];
static Pixmap pix_sprite_mask[MAX_DISPLAY][MAX_ANIME];
static Pixmap pix_sprite_bits[MAX_DISPLAY][MAX_PLAYER][MAX_ANIME];
static Pixmap pix_bomb_mask[MAX_DISPLAY][MAX_BOMBS];
static Pixmap pix_bomb_bits[MAX_DISPLAY][MAX_BOMBS];
static Pixmap pix_expl_mask[MAX_DISPLAY][MAX_EXPLOSION];
static Pixmap pix_expl_bits[MAX_DISPLAY][MAX_EXPLOSION];
static Pixmap pix_expl_block[MAX_DISPLAY][MAX_EXPLOSION];
static Pixmap pix_leds[MAX_DISPLAY][2];
static Pixmap pix_score[MAX_DISPLAY][MAX_SCORE_TILES];


static GC gc_frompix[MAX_DISPLAY];
static GC gc_window[MAX_DISPLAY];
static GC gc_clearpix[MAX_DISPLAY];
static GC gc_drawblock[MAX_DISPLAY];
static GC gc_addon[MAX_DISPLAY];
static GC gc_sprite_mask[MAX_DISPLAY];
static GC gc_sprite_bits[MAX_DISPLAY];
static GC gc_text_black[MAX_DISPLAY];
static GC gc_text_white[MAX_DISPLAY];

static char *font_name[MAX_DISPLAY][NUM_FONTS];
static XFontStruct *font_struct[MAX_DISPLAY][NUM_FONTS];

static int winX[MAX_DISPLAY],winY[MAX_DISPLAY];
static int winW[MAX_DISPLAY],winH[MAX_DISPLAY];

static XRectangle xrec[MAZE_W*MAZE_H + STAT_W*STAT_H];
static XRectangle *xrec_max = xrec;

static XBColorTriple col_triple[MAX_DISPLAY][MAX_BLOCK];

static int iconified[MAX_DISPLAY] = { FALSE, FALSE, FALSE, FALSE };


/* Default Player Colors */

static PlayerColor player_color [MAX_PLAYER];
/* local variable for color cells */
static PlayerColorCell player_color_cell[MAX_DISPLAY][MAX_PLAYER] = {
  {
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
  },
  {
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
  },
  {
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
  },
  {
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
  },
  {
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
    { 0,0,0,0,0,0},
  },
};

/*
 * color string constants
 */

static DisplayColor display_color[MAX_DISPLAY];

/*
 * local protoptypes
 */

#ifdef __STDC__
static void x_fatal(int disp, char *text1, char *text2);
static void x_warning (int disp, char *text1, char *text2);
static Pixmap create_bitmap(int disp, BitmapStruct *bitmap);
static Pixmap create_black_white_map(int disp, BitmapStruct *bitmap);
static Pixmap create_pixmap(int disp, BitmapStruct *bitmap, int fg, int bg);
static void init_fonts_color (int disp);
static void init_fonts_bw (int disp);
static void init_window_normal(int disp, char *win_title);
static void init_window_override(int disp);
static void init_player_colors(int disp);
static void init_score_tiles_color(int disp);
static void init_pixmap_color(int disp);
static void init_sprites_color(int disp);
static void init_score_tiles_bw(int disp);
static void init_pixmap_bw(int disp);
static void init_sprites_bw(int disp);
static void flush_sync(int disp);
#else
static void x_fatal();
static void x_warning ();
static Pixmap create_bitmap();
static Pixmap create_black_white_map();
static Pixmap create_pixmap();
static void init_fonts_color ();
static void init_fonts_bw ();
static void init_window_normal();
static void init_window_override();
static void init_player_colors();
static void init_score_tiles_color();
static void init_sprites_color();
static void init_pixmap_color();
static void init_score_tiles_bw();
static void init_pixmap_bw();
static void init_sprites_bw();
static void flush_sync();
#endif

/* 
 * Local function : x_fatal 
 */

#ifdef __STDC__
static void 
x_fatal (int disp, 
	 char *text1, 
	 char *text2)
#else
static void 
x_fatal (disp, text1, text2)
     int disp;
     char *text1,*text2;
#endif
{
  fprintf(stderr, "ERROR: Display(%d), %s %s.\n", disp, text1, text2);
  exit_prg(1);
}



/* 
 * Local function : x_warning 
 */

#ifdef __STDC__
static void 
x_warning (int disp, 
	   char *text1, 
	   char *text2)
#else
static void 
x_warning (disp, text1, text2)
     int disp;
     char *text1,*text2;
#endif
{
  fprintf(stderr,"WARNING: Disp(%d), %s %s.\n", disp, text1, text2);
}




/*
 * local function alloc color
 */ 
#ifdef __STDC__
static int 
alloc_color (int disp,
	     char *color_name,
	     int subst)
#else
static int 
alloc_color (disp, color_name, subst)
  int disp;
  char *color_name;
  int subst;
#endif
{
  XColor colorUsed, colorExact;

  /* get color from name */
  if (! XParseColor(dpy[disp], cmap[disp], color_name, &colorExact) ) {
    x_warning(disp, "unknown color ", color_name);
    /* use white instead */
    return subst;
  }

  /* try to alloc color cell */
  colorUsed = colorExact;
  if (XAllocColor(dpy[disp], cmap[disp], &colorUsed) ) {
    return colorUsed.pixel;
  }
  /* if private colormap is in use return white */
  if (cmap[disp] != DefaultColormap(dpy[disp], DefaultScreen(dpy[disp]) ) ) {
    return subst;
  }
  /* create private color map */
  cmap[disp] = XCopyColormapAndFree(dpy[disp], cmap[disp]);
  XSetWindowColormap(dpy[disp], win[disp], cmap[disp]);
  /* alloc again */
  colorUsed = colorExact;
  if (XAllocColor(dpy[disp], cmap[disp], &colorUsed) ) {
    return colorUsed.pixel;
  }
  return subst;

}

/* 
 * local function: alloc_color_or_white
 */
#ifdef __STDC__
static int 
alloc_color_or_white (int disp, 
		      char *color_name)
#else
static int 
alloc_color_or_white (disp, color_name)
     int disp;
     char *color_name;
#endif
{
  return alloc_color(disp, color_name, white_pixel[disp]);
}



/* 
 * local function: alloc_color_or_white
 */
#ifdef __STDC__
static int 
alloc_color_or_black(int disp, 
		     char *color_name)
#else
static int 
alloc_color_or_black (disp, color_name)
     int disp;
     char *color_name;
#endif
{
  return alloc_color(disp, color_name, black_pixel[disp]);
}



/*
 * local_function: create_bitmap
 */
#ifdef __STDC__
static Pixmap 
create_bitmap (int disp, 
	       BitmapStruct *bitmap)
#else
static Pixmap 
create_bitmap (disp, bitmap)
     int disp;
     BitmapStruct *bitmap;
#endif
{
#ifdef DEBUG_X
  Pixmap result;

  result =XCreatePixmapFromBitmapData 
    (dpy[disp], win[disp], (char *)bitmap->data, bitmap->width, bitmap->height,
     1, 0, 1);
  return result;
#else
  return XCreatePixmapFromBitmapData 
    (dpy[disp], win[disp], (char *)bitmap->data, bitmap->width, bitmap->height,
     1, 0, 1);
#endif
}



/*
 * local_function: create_bitmap
 */
#ifdef __STDC__
static Pixmap 
create_black_white_map (int disp, 
			BitmapStruct *bitmap)
#else
static Pixmap 
create_black_white_map (disp, bitmap)
     int disp;
     BitmapStruct *bitmap;
#endif
{
#ifdef DEBUG_X
  Pixmap result;

  if (1 != def_depth[disp])  {
    result = XCreatePixmapFromBitmapData
      (dpy[disp], win[disp], (char *)bitmap->data, 
       bitmap->width, bitmap->height,
       1, 0, 1);
    
  } else {
    result = XCreatePixmapFromBitmapData 
      (dpy[disp], win[disp], (char *)bitmap->data, 
       bitmap->width, bitmap->height,
       black_pixel[disp], white_pixel[disp], 1);
  }
  return result;
#else
  if (1 != def_depth[disp])  {
    return XCreatePixmapFromBitmapData
      (dpy[disp], win[disp], (char *)bitmap->data, 
       bitmap->width, bitmap->height,
       1, 0, 1);
  } else {
    return XCreatePixmapFromBitmapData 
      (dpy[disp], win[disp], (char *)bitmap->data, 
       bitmap->width, bitmap->height,
       black_pixel[disp], white_pixel[disp], 1);
  }
#endif
}



/*
 * local_function: create_pixmap
 */
#ifdef __STDC__
static Pixmap 
create_pixmap(int disp, 
	      BitmapStruct *bitmap, 
	      int fg, 
	      int bg)
#else
static Pixmap 
create_pixmap(disp, bitmap, fg, bg)
     int disp;
     BitmapStruct *bitmap;
     int fg, bg;
#endif
{
  return XCreatePixmapFromBitmapData
    (dpy[disp], win[disp], (char *)bitmap->data, bitmap->width, bitmap->height,
     fg, bg, def_depth[disp]);
}



/*
 * local_functions init_fonts
 */
#ifdef __STDC__
static void
init_fonts_color (int disp) 
#else
static void
init_fonts_color (disp) 
     int disp;
#endif
{
  XGCValues xgcv;
  int i;
  
  /* get resources */
  get_font_resources(disp, font_name[disp]);

  /* gc black text */
  xgcv.fill_style = FillTiled;
  xgcv.tile 
    = create_pixmap(disp, &text_bg_bitmap,
		    alloc_color_or_black(disp, display_color[disp].darktext1),
		    alloc_color_or_black(disp, display_color[disp].darktext2));
  
  xgcv.line_width = 2;
  gc_text_black[disp] = XCreateGC(dpy[disp], pix[disp],
				  GCTile | GCFillStyle | GCLineWidth,
				  &xgcv);
  /* gc white text */
  xgcv.fill_style = FillTiled;
  xgcv.tile 
    = create_pixmap(disp, &text_bg_bitmap,
		    alloc_color_or_white(disp,display_color[disp].lighttext1),
		    alloc_color_or_white(disp,display_color[disp].lighttext2));
  xgcv.line_width = 2;
  gc_text_white[disp] = XCreateGC(dpy[disp], pix[disp],
				  GCTile | GCFillStyle | GCLineWidth,
				  &xgcv);

  for (i=0; i<NUM_FONTS; i++) {
    /* try to load font */
#ifdef DEBUG
    fprintf (stderr, "Loading font \"%s\".\n", font_name[disp][i]);
#endif
    if (NULL == (font_struct[disp][i] 
		 = XLoadQueryFont(dpy[disp], font_name[disp][i]) ) ) {
      x_warning(disp, "could not load font",font_name[disp][i]);
      /* otherwise get default font struct */
      font_struct[disp][i] 
	= XQueryFont( dpy[disp], XGContextFromGC(gc_text_black[disp]) );
    }
  }
}



/*
 * local_functions init_fonts_bbw
 */
#ifdef __STDC__
static void
init_fonts_bw (int disp) 
#else
static void
init_fonts_bw (disp) 
     int disp;
#endif
{
  XGCValues xgcv;
  int black, white;
  int i;

  /* get resources */
  get_font_resources(disp, font_name[disp]);

  /* set black and white */
  if (1 == def_depth[disp]) {
    black = black_pixel[disp];
    white = white_pixel[disp];
  } else {
    black = 1;
    white = 0;
  }

  /* gc black text */
  xgcv.foreground = black;
  xgcv.background = white;
  xgcv.line_width = 3;
  gc_text_black[disp] = XCreateGC(dpy[disp], pix[disp],
				  GCForeground | GCBackground | GCLineWidth,
				  &xgcv);
  /* gc white text */
  xgcv.foreground = white;
  xgcv.background = black;
  xgcv.line_width = 2;
  gc_text_white[disp] = XCreateGC(dpy[disp], pix[disp],
				    GCForeground | GCBackground| GCLineWidth,
				    &xgcv);
  for (i=0; i<NUM_FONTS; i++) {
    /* try to load font */
    if (NULL == (font_struct[disp][i] 
		 = XLoadQueryFont(dpy[disp], font_name[disp][i]) ) ) {
      x_warning(disp, "could not load font",font_name[disp][i]);
      /* otherwise get default font struct */
      font_struct[disp][i] 
	= XQueryFont( dpy[disp], XGContextFromGC(gc_text_black[disp]) );
    }
  }
}



#ifdef __STDC__
static void 
init_player_colors (int disp)
#else
static void 
init_player_colors (disp) 
     int disp;
#endif
{
  int j;
  PlayerColorCell *col = player_color_cell[disp];
  
  /* first get resources from database */
  get_player_color_resources (player_color);

  for (j=0; j<MAX_PLAYER; j++) {
    col[j].helmet     = alloc_color_or_white(disp, player_color[j].helmet);
    col[j].body       = alloc_color_or_white(disp, player_color[j].body);
    col[j].arms_legs  = alloc_color_or_white(disp, player_color[j].arms_legs);
    col[j].face       = alloc_color_or_white(disp, player_color[j].face);
    col[j].hands_feet = alloc_color_or_white(disp, player_color[j].hands_feet);
    col[j].backpack   = alloc_color_or_white(disp, player_color[j].backpack);
  }
}


/*
 * local function init_score_tiles_color
 */
#ifdef __STDC__
static void 
init_score_tiles_color (int disp)
#else
static void 
init_score_tiles_color (disp)
     int disp;
#endif
{
  int i,j;
  int fg, bg;
  Pixmap tmp, pix_R, pix_G;
  PlayerColorCell *col = player_color_cell[disp];

  fg = alloc_color_or_white(disp, display_color[disp].statusbg);
  bg = alloc_color_or_black(disp, display_color[disp].statusfg);
  
  /* init led tiles  */
  for (i=0; i<2; i++) {
    pix_leds[disp][i]	= create_pixmap(disp, &(score_led[i]), fg, bg );
    tmp = create_bitmap (disp, &(score_led_addon[i]));
    XSetStipple(dpy[disp], gc_addon[disp], tmp);
    XSetForeground(dpy[disp], gc_addon[disp],
		   alloc_color_or_white(disp, display_color[disp].statusled) );
    XFillRectangle(dpy[disp], pix_leds[disp][i], gc_addon[disp], 
		   0, 0, LED_WIDTH, LED_HEIGHT);
    XFreePixmap(dpy[disp],tmp);
  }
  
  /* init other score tiles */
  for (i=0; i < SBDead ; i++) {
    pix_score[disp][i] = create_pixmap(disp, &(score_tile[i]), fg,bg);
  }
  
  /* now the player bitmaps */
  for (i=0; i<MAX_SCORE_ADDON; i++) {
    /* first the addons */
    pix_R = create_bitmap(disp, &(score_tile_addon[i][1]) );
    pix_G = create_bitmap(disp, &(score_tile_addon[i][2]) );
    /* addon bitmaps foreach player */
    for (j=0; j<MAX_PLAYER; j++) {
      pix_score[disp][SBDead + i*MAX_PLAYER +j]
	= create_pixmap(disp, &(score_tile_addon[i][0]), fg, bg);
      /* helmet */
      XSetStipple(dpy[disp], gc_addon[disp], pix_R);
      XSetForeground(dpy[disp], gc_addon[disp], col[j].helmet);
      XFillRectangle(dpy[disp], pix_score[disp][SBDead + i*MAX_PLAYER +j],
		     gc_addon[disp], 0, 0, STAT_WIDTH, STAT_HEIGHT);
      /* hand and feet */
      XSetStipple(dpy[disp], gc_addon[disp], pix_G);
      XSetForeground(dpy[disp], gc_addon[disp], col[j].hands_feet);
      XFillRectangle(dpy[disp], pix_score[disp][SBDead + i*MAX_PLAYER +j],
		     gc_addon[disp], 0, 0, STAT_WIDTH, STAT_HEIGHT);
      /* now the face */
      XSetStipple(dpy[disp], gc_addon[disp], pix_R);
      XSetClipMask(dpy[disp], gc_addon[disp], pix_G);
      XSetForeground(dpy[disp], gc_addon[disp], col[j].face );
      XFillRectangle(dpy[disp], pix_score[disp][SBDead + i*MAX_PLAYER +j],
		     gc_addon[disp], 0, 0, STAT_WIDTH, STAT_HEIGHT);
      /* reset gc to no clip mask */
      XSetClipMask(dpy[disp], gc_addon[disp], None);
    }
    XFreePixmap(dpy[disp],pix_R);
    XFreePixmap(dpy[disp],pix_G);
  }    
}



/*
 * local function: init_score_tiles_bw
 */
#ifdef __STDC__
static void 
init_score_tiles_bw (int disp)
#else
static void 
init_score_tiles_bw (disp)
     int disp;
#endif
{
  int i;

  /* init led tiles  */
  for (i=0; i<2; i++) {
    pix_leds[disp][i] = create_black_white_map(disp, &(score_led[i]) );
  }
  /* init normal score tiles */
  for (i=0; i < MAX_SCORE_TILES; i++) {
    pix_score[disp][i] = create_black_white_map(disp, &(score_tile[i]) );
  }
}



/* 
 * local function : init_window 
 */
#ifdef __STDC__
static void 
init_window_normal (int disp,
		    char *win_title)
#else
static void 
init_window_normal (disp, win_title)
  int disp;
  char *win_title;
#endif
{
  XWindowAttributes xwa;
  XSetWindowAttributes xswa;
  XWMHints *wmh;
  XSizeHints *xsh;
  XClassHint *xch;
  XEvent xev;
  XGCValues xgcv;
  int fg, bg;

  fg = black_pixel[disp];
  bg = white_pixel[disp];

  /* Get Root Window Size */
  if ( XGetWindowAttributes(dpy[disp], DefaultRootWindow(dpy[disp]), &xwa)
       == 0 ) {
    x_fatal(disp,"couldn't get root window size","");
  }

  winX[disp] =  0;
  winY[disp] =  0;
  winW[disp] =  PIXW;
  winH[disp] =  PIXH + SCOREH;

  /* Set Window Attributes */
  xswa.event_mask = NORMAL_EVENT_MASK;
  xswa.background_pixel = fg;
  xswa.border_pixel = fg;
  xswa.override_redirect = False;
  xswa.colormap = cmap[disp];

  /* Open the Window */
  win[disp]
    = XCreateWindow(dpy[disp], DefaultRootWindow(dpy[disp]),
                    winX[disp],winY[disp], winW[disp],winH[disp], 0,
                    def_depth[disp],
                    InputOutput,
                    DefaultVisual(dpy[disp], DefaultScreen(dpy[disp]) ),
                    CWEventMask | CWBackPixel | CWBorderPixel 
		    | CWOverrideRedirect | CWColormap,
                    &xswa );

  /* Change Window and icon Title */
  XChangeProperty(dpy[disp], win[disp], XA_WM_NAME, XA_STRING,
                  8, PropModeReplace, (unsigned char *) win_title,
                  strlen(win_title) );

  /* Set Icon */
  if (NULL == (wmh = XAllocWMHints())) {
    x_fatal(disp, "alloc failed", "");
  }
  wmh->flags = IconPixmapHint;
  wmh->icon_pixmap = create_black_white_map (disp, &(sprite_bits[disp][1]) );
  XSetWMHints(dpy[disp], win[disp], wmh);
  
  /* set class */
  if (NULL == (xch = XAllocClassHint())) {
    x_fatal(disp, "alloc failed", "");
  }
  xch->res_name = XBlastResName;
  xch->res_class = XBlastResClass;

  XSetClassHint(dpy[disp], win[disp], xch);

  /* set min and max geometry */
  if (NULL == (xsh = XAllocSizeHints())) {
    x_fatal(disp, "alloc failed", "");
  }
  xsh->flags = PPosition | PSize | PMinSize | PMaxSize;
  xsh->min_width = PIXW;
  xsh->max_width = PIXW;
  xsh->min_height = PIXH+SCOREH;
  xsh->max_height = PIXH+SCOREH;
  XSetWMSizeHints(dpy[disp], win[disp], xsh, XA_WM_NORMAL_HINTS);

  xgcv.foreground = fg;
  xgcv.background = bg;
  gc_window[disp] = XCreateGC(dpy[disp], win[disp], 
			      GCForeground | GCBackground, &xgcv);

  /* Set Cursor */
  XDefineCursor(dpy[disp], win[disp],
                XCreateFontCursor(dpy[disp], XC_trek) );

  /* Map the Window */
  XMapRaised(dpy[disp], win[disp]);

  /* wait for an ExposeEvent */
  do {
    XNextEvent(dpy[disp], &xev);
  } while (xev.type != Expose );

  /* get actual window size */
  if ( XGetWindowAttributes(dpy[disp], win [disp], &xwa) == 0) {
    x_fatal(disp,"could not get window size","");
  }

  winW[disp] = xwa.width;
  winH[disp] = xwa.height;

  if ( (winW[disp] < PIXW) || (winH[disp] <PIXH) ) {
    x_warning(disp,"display is to small","");
  }
}



/* 
 * local function : init_window_override
 */
#ifdef __STDC__
static void 
init_window_override (int disp)
#else
static void 
init_window_override (disp)
     int disp;
#endif
{
  XWindowAttributes xwa;
  XSetWindowAttributes xswa;
  XEvent xev;
  XGCValues xgcv;
  int fg, bg;

  fg = black_pixel[disp];
  bg = white_pixel[disp];

  /* Get Root Window Size */
  if ( XGetWindowAttributes(dpy[disp], DefaultRootWindow(dpy[disp]), &xwa)
       == 0 ) {
    x_fatal(disp,"couldn't get root window size","");
  }

  winX[disp] =  0;
  winY[disp] =  0;
  winW[disp] =  PIXW;
  winH[disp] =  PIXH + SCOREH;

  /* Set Window Attributes */
  xswa.event_mask = NORMAL_EVENT_MASK;
  xswa.background_pixel = fg;
  xswa.border_pixel = fg;
  xswa.override_redirect = True;

  /* Open the Window */
  win[disp]
    = XCreateWindow(dpy[disp], DefaultRootWindow(dpy[disp]),
                    winX[disp],winY[disp], winW[disp],winH[disp], 0,
                    def_depth[disp],
                    InputOutput,
                    DefaultVisual(dpy[disp], DefaultScreen(dpy[disp]) ),
                    CWEventMask | CWBackPixel | CWBorderPixel 
		    | CWOverrideRedirect,
                    &xswa );

  xgcv.foreground = fg;
  xgcv.background = bg;
  gc_window[disp] = XCreateGC(dpy[disp], win[disp], 
			      GCForeground | GCBackground, &xgcv);

  /* Set Cursor */
  XDefineCursor(dpy[disp], win[disp],
                XCreateFontCursor(dpy[disp], XC_trek) );

  /* Map the Window */
  XMapRaised(dpy[disp], win[disp]);

  /* wait for an ExposeEvent */
  do {
    XNextEvent(dpy[disp], &xev);
  } while (xev.type != Expose );

  /* get actual window size */
  if ( XGetWindowAttributes(dpy[disp], win [disp], &xwa) == 0) {
    x_fatal(disp,"could not get window size","");
  }

  winW[disp] = xwa.width;
  winH[disp] = xwa.height;

  if ( (winW[disp] < PIXW) || (winH[disp] <PIXH) ) {
    x_warning(disp,"display is to small","");
  }
}



/* 
 * Local function : init_pixmap_color
 */
#ifdef __STDC__
static void 
init_pixmap_color (int disp)
#else 
static void 
init_pixmap_color (disp)
     int disp;
#endif
{
  XGCValues xgcv;

  /* get resources for various colors */
  get_color_resources(disp, &(display_color[disp]) );

  /* where to draw pixmap */
  pix[disp]
    = XCreatePixmap(dpy[disp], win[disp], PIXW, PIXH+SCOREH, def_depth[disp]);

  /* gc: add on graphics */
  xgcv.fill_style = FillStippled;
  gc_addon[disp] = XCreateGC(dpy[disp], win[disp],
			     GCFillStyle, &xgcv );

  /* gc : copy pixmap to window */
  xgcv.fill_style = FillTiled;
  xgcv.tile = pix[disp];
  gc_frompix[disp] = XCreateGC(dpy[disp], win[disp],
			       GCTile | GCFillStyle, &xgcv );

  /* gc : clear pixmap */
  xgcv.tile = 
    create_pixmap(disp, &title_bitmap, 
		  alloc_color_or_white(disp,display_color[disp].title1),
		  alloc_color_or_black(disp,display_color[disp].title2) ); 
  xgcv.fill_style = FillTiled;
  gc_clearpix[disp] = XCreateGC(dpy[disp], pix[disp], GCFillStyle | GCTile,
				&xgcv );

  /* gc : draw block */
  xgcv.fill_style = FillTiled;
  gc_drawblock[disp] = XCreateGC(dpy[disp], pix[disp], GCFillStyle, &xgcv );

}



/* 
 * Local function : init_pixmap_bw
 */
#ifdef __STDC__
static void 
init_pixmap_bw (int disp)
#else 
static void 
init_pixmap_bw (disp)
     int disp;
#endif
{
  XGCValues xgcv;

  /* where to draw pixmap */
  pix[disp]
    = XCreatePixmap(dpy[disp], win[disp], PIXW, PIXH + SCOREH, 1);

  /* gc : copy pixmap to window */
  if (1 != def_depth[disp]) {
    /* for non monochrome display in bw mode */
    xgcv.foreground = black_pixel[disp];
    xgcv.background = white_pixel[disp];
    xgcv.fill_style = FillOpaqueStippled;
    xgcv.stipple = pix[disp];
    gc_frompix[disp] = XCreateGC(dpy[disp], win[disp],
				 GCForeground | GCBackground | GCStipple 
				 | GCFillStyle, &xgcv );
  } else {
    /* for monochrome displays in bw mode */
    xgcv.fill_style = FillTiled;
    xgcv.tile = pix[disp];
    gc_frompix[disp] = XCreateGC(dpy[disp], win[disp],
				 GCTile | GCFillStyle, 
				 &xgcv );
  }

  /* gc : clear pixmap */
  xgcv.tile = create_black_white_map (disp, &(block_tile[BLScoreFloor]));
  xgcv.fill_style = FillTiled;
  gc_clearpix[disp] = XCreateGC(dpy[disp], pix[disp],
				GCFillStyle | GCTile,
				&xgcv );

  /* gc : draw block */
  xgcv.fill_style = FillTiled;
  gc_drawblock[disp] = XCreateGC(dpy[disp], pix[disp], GCFillStyle, &xgcv );

}



/* 
 * local function init_disp_sprites 
 */

#ifdef __STDC__
static void 
init_sprites_color (int disp)
#else
static void 
init_sprites_color (disp)
     int disp;
#endif
{
  XGCValues xgcv;
  int i,j;
  int width, height;
  int fg, bg, bg2, add;
  Pixmap pix_R, pix_G, pix_B, tmp;
  PlayerColorCell *col;

  fg = black_pixel[disp];
  bg2 = alloc_color_or_white(disp, "AntiqueWhite");
  bg = white_pixel[disp];

  /* gc for drawing mask */
  xgcv.foreground = bg;
  xgcv.fill_style = FillStippled;
  gc_sprite_mask[disp] = XCreateGC(dpy[disp], pix[disp],
				   GCFillStyle | GCForeground, &xgcv);
  /* gc for drawing sprite bits */
  xgcv.fill_style = FillTiled;
  gc_sprite_bits[disp] = XCreateGC(dpy[disp], pix[disp], GCFillStyle, 
				   &xgcv);

  /* alloc colors for players */
  col = player_color_cell[disp];

  /* pixmaps for sprite_masks and bits */

  /* disp sprites */
  for (i = 0; i < MAX_ANIME; i++) {

    /* mask */
    pix_sprite_mask[disp][i] = create_bitmap(disp, sprite_mask + i);

    if ( (NULL == sprite_addon[i][0].data) ) {
      /* normal sprite bits */
      for (j =0; j < MAX_PLAYER; j++) {
	pix_sprite_bits[disp][j][i] = 
	  create_pixmap(disp, sprite_bits[0]+i, fg, bg2 );
      }
    } else {
      /* normal sprite bits */
      for (j =0; j < MAX_PLAYER; j++) {
	pix_sprite_bits[disp][j][i] = 
	  create_pixmap(disp, sprite_addon[i]+3, fg, bg );
      }
    
      /* create three planes of color addons */
      pix_R = create_bitmap(disp, sprite_addon[i]+0);
      pix_G = create_bitmap(disp, sprite_addon[i]+1);
      pix_B = create_bitmap(disp, sprite_addon[i]+2);

      /* set width and height for rectangles */
      width = sprite_bits[0][i].width;
      height = sprite_bits[0][i].height;

      /* first the simple ones */
      
      /* helmet */
      XSetStipple(dpy[disp], gc_addon[disp], pix_R);
      for (j =0; j < MAX_PLAYER; j++) {
	XSetForeground(dpy[disp], gc_addon[disp], col[j].helmet );
	XFillRectangle(dpy[disp], pix_sprite_bits[disp][j][i],
		       gc_addon[disp], 0,0, width, height);
      }
      
      /* body */
      XSetStipple(dpy[disp], gc_addon[disp], pix_G);
      for (j =0; j < MAX_PLAYER; j++) {
	XSetForeground(dpy[disp], gc_addon[disp], col[j].body );
	XFillRectangle(dpy[disp], pix_sprite_bits[disp][j][i],
		       gc_addon[disp], 0,0, width, height);
      }
      
      /* arms & legs */
      XSetStipple(dpy[disp], gc_addon[disp], pix_B);
      for (j =0; j < MAX_PLAYER; j++) {
	XSetForeground(dpy[disp], gc_addon[disp], col[j].arms_legs );
	XFillRectangle(dpy[disp], pix_sprite_bits[disp][j][i],
		       gc_addon[disp], 0,0, width, height);
      }
      
      /* now the trickier ones */
      
      /* face */
      XSetStipple(dpy[disp], gc_addon[disp], pix_R);
      XSetClipMask(dpy[disp], gc_addon[disp], pix_G);
      for (j =0; j < MAX_PLAYER; j++) {
	XSetForeground(dpy[disp], gc_addon[disp], col[j].face );
	XFillRectangle(dpy[disp], pix_sprite_bits[disp][j][i],
		       gc_addon[disp], 0,0, width, height);
      }
      
      /* hands & feet */
      XSetStipple(dpy[disp], gc_addon[disp], pix_B);
      XSetClipMask(dpy[disp], gc_addon[disp], pix_G);
      for (j =0; j < MAX_PLAYER; j++) {
	XSetForeground(dpy[disp], gc_addon[disp], col[j].hands_feet );
	XFillRectangle(dpy[disp], pix_sprite_bits[disp][j][i],
		       gc_addon[disp], 0,0, width, height);
      }
      
      /* backpack */
      XSetStipple(dpy[disp], gc_addon[disp], pix_B);
      XSetClipMask(dpy[disp], gc_addon[disp], pix_R);
      for (j =0; j < MAX_PLAYER; j++) {
	XSetForeground(dpy[disp], gc_addon[disp], col[j].backpack );
	XFillRectangle(dpy[disp], pix_sprite_bits[disp][j][i],
		       gc_addon[disp], 0,0, width, height);
      }

      
      /* reset gc to no clip mask */
      XSetClipMask(dpy[disp], gc_addon[disp], None);
      
      XFreePixmap(dpy[disp],pix_R);
      XFreePixmap(dpy[disp],pix_G);
      XFreePixmap(dpy[disp],pix_B);
    } 
  }

  /* bomb sprites */
  add=alloc_color_or_black(disp, display_color[disp].bomb);
  for (i = 0; i < MAX_BOMBS; i++) {
    /* mask */
    pix_bomb_mask[disp][i] = create_bitmap(disp, &(bomb_mask[i]));
    pix_bomb_bits[disp][i] = create_pixmap(disp, &(bomb_bits[i]),fg, bg);
    tmp = create_bitmap(disp, &(bomb_addon[i]));
    XSetStipple(dpy[disp], gc_addon[disp], tmp);
    XSetForeground(dpy[disp], gc_addon[disp],add);
    XFillRectangle(dpy[disp], pix_bomb_bits[disp][i], gc_addon[disp], 
		   0, 0, BLOCK_WIDTH, BLOCK_HEIGHT);
    XFreePixmap(dpy[disp],tmp);
  }
  
  /* alloc colors for explosions */
  fg = alloc_color_or_black(disp, display_color[disp].expl1) ;
  bg = alloc_color_or_white(disp, display_color[disp].expl2);
  add =alloc_color_or_white(disp, display_color[disp].expl3) ;
  for (i = 0; i < MAX_EXPLOSION; i++) {
    /* mask */
    pix_expl_mask[disp][i] = create_bitmap(disp, &(expl_mask[i]));
    pix_expl_bits[disp][i] = create_pixmap(disp, &(expl_bits[i]),fg,bg);
    tmp = create_bitmap(disp, &(expl_addon[i]));
    XSetStipple(dpy[disp], gc_addon[disp], tmp);
    XSetForeground(dpy[disp], gc_addon[disp],add);
    XFillRectangle(dpy[disp], pix_expl_bits[disp][i], gc_addon[disp], 
		   0, 0, BLOCK_WIDTH, BLOCK_HEIGHT);
    XFreePixmap(dpy[disp],tmp);
  }
}



/* 
 * local function init_sprites_bw 
 */
#ifdef __STDC__
static void 
init_sprites_bw (int disp)
#else
static void 
init_sprites_bw (disp)
     int disp;
#endif
{
  XGCValues xgcv;
  int i,j;
  int fg, bg;

  if (1 == def_depth[disp]) {
    fg = black_pixel[disp];
    bg = white_pixel[disp];
  } else {
    fg = 1;
    bg = 0;
  }

  /* gc for drawing mask */
  xgcv.foreground = bg;
  xgcv.fill_style = FillStippled;
  gc_sprite_mask[disp] = XCreateGC(dpy[disp], pix[disp],
				   GCFillStyle | GCForeground, &xgcv);
  /* gc for drawing bitmap */
  xgcv.fill_style = FillStippled;
  xgcv.foreground = fg;
  gc_sprite_bits[disp] = XCreateGC(dpy[disp], pix[disp],
				   GCFillStyle | GCForeground, &xgcv);

  /* disp sprites */
  for (i = 0; i < MAX_ANIME; i++) {
    /* sprite mask */
    pix_sprite_mask[disp][i] = create_bitmap(disp, &(sprite_mask[i]) );

    /* sprite bits */
    for (j =0; j < MAX_PLAYER; j++) {
      pix_sprite_bits[disp][j][i]
	= create_bitmap(disp, &(sprite_bits[j][i]));
    }
  }

  /* bomb sprites */
  for (i = 0; i < MAX_BOMBS; i++) { 
    /* mask */
    pix_bomb_mask[disp][i] = create_bitmap(disp, &(bomb_mask[i]));
    pix_bomb_bits[disp][i] = create_bitmap(disp, &(bomb_bits[i]));
  }

  for (i = 0; i < MAX_EXPLOSION; i++) {
    /* mask */
    pix_expl_mask[disp][i] = create_bitmap(disp, &(expl_mask[i]));
    pix_expl_bits[disp][i] = create_bitmap(disp, &(expl_bits[i]));
    }
}





/*
 * public function : init_display
 */
#ifdef __STDC__
void 
init_display (int disp, 
	      char *display)
#else
void 
init_display (disp, display)
     int disp;
     char *display;
#endif
{
#ifdef DEBUG
  fprintf(stderr, "Display is \"%s\"\n", display);
#endif
  /* Check Player */
  if ( (disp < 0) || (disp >= MAX_DISPLAY) ) {
    x_fatal(disp,"wrong display","");
  }

  /* open display */
  if ( !(dpy[disp] = XOpenDisplay(display)) ) {
    x_fatal(disp,"Couldn't open Display",display);
  }

  /* init x databases */
  create_display_database(disp);

  /* set depth variable */
  def_depth[disp] = DefaultDepth(dpy[disp], DefaultScreen(dpy[disp]));

  /* set colormap to default */
  cmap[disp] = DefaultColormap(dpy[disp], DefaultScreen(dpy[disp]));

  /* set standard pixel values */
  white_pixel[disp] = WhitePixel(dpy[disp], DefaultScreen(dpy[disp]));
  black_pixel[disp] = BlackPixel(dpy[disp], DefaultScreen(dpy[disp]));

  /* alloc black and white, we still have them if we a use private colormap */
  white_pixel[disp] = alloc_color_or_white(disp, "White");
  black_pixel[disp] = alloc_color_or_black(disp, "Black");
}

#ifdef __STDC__
void
finish_display (int disp)
#else
void
finish_display (disp)
  int disp;
#endif
{
  if (dpy[disp] != NULL) {
    XCloseDisplay(dpy[disp]);
  }
}


/*
 * Public function : init_graphics 
 */
#ifdef __STDC__
void 
init_graphics (int disp,
	       char *win_title)
#else
void 
init_graphics (disp, win_title)
  int disp;
  char *win_title;
#endif
{
  int visual_class;
  XVisualInfo visual_info;

  /* set key tables */
  set_game_keys(disp);

  /* get color mode from visual */
  colorMode[disp] = FALSE;
  if (color_mode_from_database(disp) && (1 != def_depth[disp]) ) {
    visual_class = DirectColor;
    while (!XMatchVisualInfo(dpy[disp], DefaultScreen(dpy[disp]), 
			     def_depth[disp], visual_class--, &visual_info) );
    if (visual_class >= StaticColor) {
      colorMode[disp] = TRUE;
    }
  } 
#ifdef DEBUG
  fprintf(stderr,"Display %d: color mode is %d\n",disp, colorMode[disp]);
#endif

  /* init window, either override or normal */
  if (override_from_database(disp)) {
    init_window_override(disp);
  } else {
    init_window_normal(disp, win_title);
  }
  /* init graphics either color or black an white */
  if (colorMode[disp]) {
    init_player_colors(disp);
    init_pixmap_color(disp);
    init_fonts_color(disp);
    init_score_tiles_color(disp);
    init_sprites_color(disp);
  } else {
    init_pixmap_bw(disp);
    init_fonts_bw(disp);
    init_score_tiles_bw(disp);
    init_sprites_bw(disp);
  }

}


/*
 * local variables for key translations
 */

#define KEYCODE_MAX 255
#define KEYCODE_MIN 8
#define NUM_KEYCODE 256

static KeyPressAction *key_table[MAX_DISPLAY];
static KeySym code2sym[2][NUM_KEYCODE];

/*
 * public link_keysyms
 */
#ifdef __STDC__
void
link_keysyms (int disp,
	      int nelem,
	      KeyPressDefine *keydef)
#else
void
link_keysyms (disp, nelem, keydef)
     int disp;
     int nelem;
     KeyPressDefine *keydef;
#endif
{

  int i;
  KeySym keysym;
  KeyCode keycode;
  char **keyv;
  int keyc;

  /* fill simple translation table */
  for (keycode = KEYCODE_MIN; (keycode<KEYCODE_MAX) && (keycode!=0); keycode++) {
    code2sym[0][keycode] = XKeycodeToKeysym(dpy[disp], keycode, 0);
    code2sym[1][keycode] = XKeycodeToKeysym(dpy[disp], keycode, 1);
  }

  /* alloc key table */
  if (NULL == (key_table[disp] 
	       = calloc (NUM_KEYCODE, sizeof(KeyPressAction) ) ) ) {
    x_fatal(disp, "failed to alloc key table", "");
  }

  for (i=0; i<nelem; i++) {
    keyv = split_string(keydef[i].keysym, &keyc);
    for (--keyc; keyc >= 0; keyc--) {
      if (NULL == keyv[keyc]) {
	x_fatal(disp, "undefined keysymbol", "");
      }
      if (NoSymbol == (keysym = XStringToKeysym(keyv[keyc]))) {
	x_warning(disp, "unknown keysymbol", keyv[keyc]);
	continue;
      }
      /* set entries in table */
      for (keycode=KEYCODE_MIN; (keycode<KEYCODE_MAX)&&(keycode!=0);keycode ++) {
	if ( (keysym == code2sym[0][keycode]) 
	    || (keysym == code2sym[1][keycode])) {
	  key_table[disp][keycode].addr  = keydef[i].addr;
	  key_table[disp][keycode].value = keydef[i].value;
	}
      }
    }
  }

}



/*
 * public function: check_event
 */
#ifdef __STDC__
void 
check_event (int disp, 
	     void (*expose_func)()) 
#else
void 
check_event (disp, expose_func)
     int disp;
     void (*expose_func)();
#endif
{
  XEvent xev;
  int num_events;

  /* first get number of events */
  XSync (dpy[disp], FALSE);
  num_events = XEventsQueued(dpy[disp],QueuedAlready);

  while (num_events !=0) {
    num_events --;

    /* get event */
    XNextEvent(dpy[disp], &xev);
      
    switch (xev.type) {
      /* windows is iconfied */
    case UnmapNotify:
#ifdef DEBUG
      fprintf(stderr, "Window unmapped\n");
#endif
      iconified[disp] = TRUE;
      break;
      
      /* window is mapped again */
    case MapNotify:
#ifdef DEBUG
      fprintf(stderr, "Window mapped\n");
#endif
      iconified[disp] = FALSE;
      break;

      /* part of the window was exposed */
    case Expose:
      (*expose_func)(disp, &xev);
      break;
      
    case KeyPress:
      {
	unsigned keycode;
	KeyPressAction *ptr;
	
	keycode = xev.xkey.keycode;
	ptr = &(key_table[disp][keycode]);
	if (NULL != ptr->addr) {
	  *(ptr->addr) = ptr->value;
	} else {
	  /* dirty quick hack to be changed soon */
	  if (disp == 0) {
	    if (XK_Escape == XLookupKeysym(&xev.xkey, 0)) {
	      (*quit_function)();
	    }
	  }
	  /* first boss key, works only with window manager */
	  if (XK_BackSpace == XLookupKeysym(&xev.xkey, 0)) {
	    XIconifyWindow(dpy[disp],win[disp],DefaultScreen(dpy[disp]));
	  }
	}
      }

      break;
    }
  }
  return;
}


/* public function : init_block */

#ifdef __STDC__
void 
init_block (int disp, 
	    int in_data, 
	    int in_pix,
	    char *fg_name, 
	    char *bg_name, 
	    char *add_name)
#else
void 
init_block (disp, in_data, in_pix, fg_name, bg_name, add_name)
     int disp, in_data, in_pix;
     char *fg_name, *bg_name, *add_name;
#endif
{
  int fg, bg, add;
  Pixmap tmp;

  if (colorMode[disp]) {
    /* color block */
    if (fg_name != NULL) {
      col_triple[disp][in_pix].fg = fg = alloc_color_or_black(disp,fg_name);
      col_triple[disp][in_pix].bg = bg = alloc_color_or_white(disp,bg_name);
    } else {
      fg = black_pixel[disp];
      bg = white_pixel[disp];
    }
    /* set values to be freed after wards */
    col_triple[disp][in_pix].fg  = fg;
    col_triple[disp][in_pix].bg  = bg;
    col_triple[disp][in_pix].add = white_pixel[disp];

    pix_block[disp][in_pix] =
      create_pixmap(disp, &(block_tile[in_data]), fg, bg);
    
    if ( (NULL != add_name) && (NULL != block_addon[in_data].data ) ) {
      /* create temp add on pixmap */
      tmp = create_bitmap(disp, &(block_addon[in_data]));
      /* get color */
      col_triple[disp][in_pix].add = 
	add = alloc_color_or_white (disp, add_name);
      /* draw it */
      XSetStipple(dpy[disp], gc_addon[disp], tmp);
      XSetForeground(dpy[disp], gc_addon[disp], add);
      XFillRectangle(dpy[disp], pix_block[disp][in_pix], gc_addon[disp], 
		     0, 0, BLOCK_WIDTH, BLOCK_HEIGHT);
      XFreePixmap(dpy[disp],tmp);
    }
  } else {
    /* black and white block */
    pix_block[disp][in_pix] =
      create_black_white_map(disp, &(block_tile[in_data]));
  }
}


/* public function: init_explosion_blocks */
#ifdef __STDC__
void 
init_explosion_blocks (int disp)
#else
void 
init_explosion_blocks (disp)
     int disp;
#endif
{
  int i;
  static XGCValues xgcv;
    
  if (colorMode[disp]) {
    for (i=0; i < MAX_EXPLOSION; i++) {
      pix_expl_block[disp][i] =
	XCreatePixmap(dpy[disp], pix[disp], BLOCK_WIDTH, BLOCK_HEIGHT,
		      def_depth[disp]);
      
      xgcv.tile = pix_block[disp][BTFree];
      XChangeGC(dpy[disp], gc_drawblock[disp], GCTile, &xgcv);
      XFillRectangle(dpy[disp], pix_expl_block[disp][i], 
		     gc_drawblock[disp],
		     0, 0, BLOCK_WIDTH, BLOCK_HEIGHT);
      
      xgcv.clip_mask = pix_expl_mask[disp][i];
      xgcv.clip_y_origin = 0;
      xgcv.clip_x_origin = 0;
      xgcv.tile = pix_expl_bits[disp][i];
      xgcv.ts_y_origin = 0;
      xgcv.ts_x_origin = 0;
      XChangeGC(dpy[disp], gc_sprite_bits[disp],
		GCClipMask | GCClipXOrigin |GCClipYOrigin |
		GCTile | GCTileStipXOrigin |GCTileStipYOrigin,
		&xgcv);
      XFillRectangle(dpy[disp], pix_expl_block[disp][i], 
		     gc_sprite_bits[disp],
		     0, 0, BLOCK_WIDTH, BLOCK_HEIGHT);
    }
  } else {
    for (i=0; i < MAX_EXPLOSION; i++) {
      pix_expl_block[disp][i] =
	XCreatePixmap(dpy[disp], pix[disp], BLOCK_WIDTH, BLOCK_HEIGHT, 1);
      
      xgcv.tile = pix_block[disp][BTFree];
      XChangeGC(dpy[disp], gc_drawblock[disp],
		GCTile, &xgcv);
      XFillRectangle(dpy[disp], pix_expl_block[disp][i], 
		     gc_drawblock[disp],
		     0, 0, BLOCK_WIDTH, BLOCK_HEIGHT);
      
      xgcv.stipple = pix_expl_mask[disp][i];
      xgcv.ts_y_origin = 0;
      xgcv.ts_x_origin = 0;
      XChangeGC(dpy[disp], gc_sprite_mask[disp],
		GCStipple | GCTileStipXOrigin |GCTileStipYOrigin,
		&xgcv);
      XFillRectangle(dpy[disp], pix_expl_block[disp][i], 
		     gc_sprite_mask[disp],
		     0, 0, BLOCK_WIDTH, BLOCK_HEIGHT);
      
      xgcv.stipple = pix_expl_bits[disp][i];
      xgcv.ts_y_origin = 0;
      xgcv.ts_x_origin = 0;
      XChangeGC(dpy[disp], gc_sprite_bits[disp],
		GCStipple | GCTileStipXOrigin |GCTileStipYOrigin,
		&xgcv);
      XFillRectangle(dpy[disp], pix_expl_block[disp][i], 
		     gc_sprite_bits[disp],
		     0, 0, BLOCK_WIDTH, BLOCK_HEIGHT);
    }
  }
}



/* public function : free_block */
#ifdef __STDC__
void 
free_block (int disp, 
	    int in_pix)
#else
void 
free_block (disp, in_pix)
     int disp, in_pix;
#endif
{
  int num;
  unsigned long pix_val[3];

  /* free pixmap */
  XFreePixmap(dpy[disp], pix_block[disp][in_pix]);
  /* free read only colors cells */
  if (colorMode[disp]) {
    num=0;
    if (col_triple[disp][in_pix].fg != black_pixel[disp]) {
      pix_val[num++] = (unsigned long) col_triple[disp][in_pix].fg;
    }
    if (col_triple[disp][in_pix].bg != white_pixel[disp]) {
      pix_val[num++] = (unsigned long) col_triple[disp][in_pix].bg;
    }
    if (col_triple[disp][in_pix].add != white_pixel[disp]) {
      pix_val[num++] = (unsigned long) col_triple[disp][in_pix].add;
    }
    if (num >0) {
      XFreeColors(dpy[disp], cmap[disp], pix_val, num, 0);
    }
  }
}



/* public function : free_explosion_blocks */
#ifdef __STDC__
void 
free_explosion_blocks (int disp)
#else
void 
free_explosion_blocks (disp)
     int disp;
#endif
{
  int i;
  
  for (i = 0; i < MAX_EXPLOSION; i ++ ) {
    XFreePixmap(dpy[disp], pix_expl_block[disp][i]);
  }
}


static XRectangle void_list[MAZE_W*MAZE_H];
static XRectangle *void_last = void_list;
static XRectangle block_list[MAX_BLOCK][MAZE_W*MAZE_H];
static XRectangle *block_last[MAX_BLOCK] = {
  block_list[0],
  block_list[1],
  block_list[2],
  block_list[3],
  block_list[4],
  block_list[5],
  block_list[6],
  block_list[7],
  block_list[8],
  block_list[9],
  block_list[10],
};

/* 
 * public function : draw_block 
 */
#ifdef __STDC__
void 
draw_block (int x, 
	    int y, 
	    int block)
#else
void 
draw_block (x, y, block)
     int x, y, block;
#endif
{
#ifdef DEBUG
  if ( (x < 0) || (x >= MAZE_W) 
      || (y < 0) || (y >= MAZE_H)
      || (block<-1) || (block >= MAX_BLOCK) ) {
    fprintf(stderr,"ERROR in draw_block\n");
    fprintf(stderr,"x = %d, y = %d, block =%d\n",x,y,block);
  }
#endif

  if (block >= 0) {
    block_last[block]->x = x*BLOCK_WIDTH;
    block_last[block]->y = y*BLOCK_HEIGHT;
    block_last[block]->width = BLOCK_WIDTH;
    block_last[block]->height = BLOCK_HEIGHT;

    block_last[block] ++;
  } else {
    void_last->x = x*BLOCK_WIDTH;
    void_last->y = y*BLOCK_HEIGHT;
    void_last->width = BLOCK_WIDTH;
    void_last->height = BLOCK_HEIGHT;

    void_last ++;
  }
}


#ifdef __STDC__
void 
draw_block_at (int disp, 
	       int x, int y, 
	       int block)
#else
void 
draw_block_at (disp, x, y, block)
     int disp, x, y, block;
#endif
{
#ifdef DEBUG
  if ( (x < 0) || (x >= MAZE_W) 
      || (y < 0) || (y >= MAZE_H)
      || (block<-1) || (block >= MAX_BLOCK) ) {
    fprintf(stderr,"ERROR in draw_block: DISP =%d\n",disp);
    fprintf(stderr,"x = %d, y = %d, block =%d\n",x,y,block);
  }
#endif

  if (block >= 0) {
    XSetTile(dpy[disp], gc_drawblock[disp], pix_block[disp][block]);
    XFillRectangle(dpy[disp], pix[disp], gc_drawblock[disp],
		   x*BLOCK_WIDTH, y*BLOCK_HEIGHT,
		   BLOCK_WIDTH, BLOCK_HEIGHT );
  } else {
    XFillRectangle(dpy[disp], pix[disp], gc_clearpix[disp],
		   x*BLOCK_WIDTH, y*BLOCK_HEIGHT,
		   BLOCK_WIDTH, BLOCK_HEIGHT );
  }
}

static XRectangle expl_list[MAX_EXPLOSION][MAZE_W*MAZE_H];
static XRectangle *expl_last[MAX_EXPLOSION] = {
  expl_list[0],
  expl_list[1],
  expl_list[2],
  expl_list[3],
  expl_list[4],
  expl_list[5],
  expl_list[6],
  expl_list[7],
  expl_list[8],
  expl_list[9],
  expl_list[10],
  expl_list[11],
  expl_list[12],
  expl_list[13],
  expl_list[14],
  expl_list[15],
};

/* 
 * public function : draw_explosion 
 */
#ifdef __STDC__
void 
draw_explosion (int x,
		int y, 
		int block)
#else
void 
draw_explosion(x, y, block)
     int x, y, block;
#endif
{
#ifdef DEBUG
  if (block < 0) {
    fprintf(stderr,"DRAW EXPLOSION: Negative Block Number %d\n",block);
  }
  if (block >= MAX_EXPLOSION) {
    fprintf(stderr,"DRAW EXPLOSION: Block Number %d to large\n",block);
  }
#endif

    expl_last[block]->x = x*BLOCK_WIDTH;
    expl_last[block]->y = y*BLOCK_HEIGHT;
    expl_last[block]->width = BLOCK_WIDTH;
    expl_last[block]->height = BLOCK_HEIGHT;

    expl_last[block] ++;
}

#if 0
/* 
 * public function : draw_explosion_at 
 */
#ifdef __STDC__
void 
draw_explosion_at (int disp, 
		   int x,
		   int y, 
		   int block)
#else
void 
draw_explosion_at (disp, x, y, block)
     int disp, x, y, block;
#endif
{
#ifdef DEBUG
  if (block < 0) {
    fprintf(stderr,"DRAW EXPLOSION: Negative Block Number %d\n",block);
  }
  if (block >= MAX_EXPLOSION) {
    fprintf(stderr,"DRAW EXPLOSION: Block Number %d to large\n",block);
  }
#endif

  XSetTile(dpy[disp], gc_drawblock[disp], pix_expl_block[disp][block]);
  XFillRectangle(dpy[disp], pix[disp], gc_drawblock[disp],
                 (int)(BLOCK_WIDTH*x), (int)(BLOCK_HEIGHT*y),
                 BLOCK_WIDTH, BLOCK_HEIGHT);
}
#endif

/* public function flush_blocks */
#ifdef __STDC__
void
flush_blocks (int disp, 
	      int flag)
#else
void
flush_blocks (disp, flag)
     int disp, flag;
#endif
{
  int i;

  /* void blocks */
  if (void_last != void_list) {
    XFillRectangles(dpy[disp], pix[disp], gc_clearpix[disp],
		    void_list, void_last - void_list);
    if (flag) {
      void_last = void_list;
    }
  }

  /* normal blocks */
  for (i=0; i<MAX_BLOCK; i++) {
    if (block_last[i] != block_list[i]) {
      XSetTile(dpy[disp], gc_drawblock[disp], pix_block[disp][i]);
      XFillRectangles(dpy[disp], pix[disp], gc_drawblock[disp],
		      block_list[i], block_last[i] - block_list[i]);
      if (flag) {
	block_last[i] = block_list[i];
      }
    }
  }

  /* explosion blocks */
  for (i=0; i<MAX_EXPLOSION; i++) {
    if (expl_last[i] != expl_list[i]) {
      XSetTile(dpy[disp], gc_drawblock[disp], pix_expl_block[disp][i]);
      XFillRectangles(dpy[disp], pix[disp], gc_drawblock[disp],
		      expl_list[i], expl_last[i] - expl_list[i]);
      if (flag) {
	expl_last[i] = expl_list[i];
      }
    }
  }
}


/* public function : draw_explosion */
#ifdef __STDC__
void 
draw_explosion_sprite (int disp, 
		       int x,
		       int y, 
		       int block)
#else
void 
draw_explosion_sprite  (disp, x, y, block)
     int disp, x, y, block;
#endif
{
  if (colorMode[disp]) {
    XSetTSOrigin(dpy[disp], gc_sprite_bits[disp], x*BLOCK_WIDTH, 
		 y*BLOCK_HEIGHT);
    XSetClipOrigin(dpy[disp], gc_sprite_bits[disp], x*BLOCK_WIDTH, 
		   y*BLOCK_HEIGHT);
    XSetClipMask(dpy[disp], gc_sprite_bits[disp], pix_expl_mask[disp][block]);
    XSetTile(dpy[disp], gc_sprite_bits[disp], pix_expl_bits[disp][block]);
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_bits[disp],
		   BLOCK_WIDTH*x, BLOCK_HEIGHT*y,
		   BLOCK_WIDTH, BLOCK_HEIGHT);
  } else {
    XSetTSOrigin(dpy[disp], gc_sprite_mask[disp], x*BLOCK_WIDTH, 
		 y*BLOCK_HEIGHT);
    XSetStipple(dpy[disp], gc_sprite_mask[disp], pix_expl_mask[disp][block]);
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_mask[disp],
		   BLOCK_WIDTH*x, BLOCK_HEIGHT*y,
		   BLOCK_WIDTH, BLOCK_HEIGHT);
    XSetTSOrigin(dpy[disp], gc_sprite_bits[disp], x*BLOCK_WIDTH, 
		 y*BLOCK_HEIGHT);
    XSetStipple(dpy[disp], gc_sprite_bits[disp], pix_expl_bits[disp][block]);
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_bits[disp],
		   BLOCK_WIDTH*x, BLOCK_HEIGHT*y,
		   BLOCK_WIDTH, BLOCK_HEIGHT);
  }
}



/* local function flush_sync */
#ifdef __STDC__
static void 
flush_sync (int disp)
#else
static void 
flush_sync(disp)
     int disp;
#endif
{
#ifdef hpux
  XSync(dpy[disp], FALSE);
#else
  XFlush(dpy[disp]);
#endif
}



/* Public function clear_window */
#ifdef __STDC__
void 
clear_window (int disp)
#else
void 
clear_window (disp)
     int disp;
#endif
{
  XClearWindow(dpy[disp], win[disp]);
}


/* globals for fade routines */

static XSegment line[PIXH+SCOREH];
static int fade_count;
static int fade_max;

#ifdef __STDC__
void 
set_fade_max (int max)
#else
void 
set_fade_max (max)
     int max;
#endif
{
  fade_max = max;
}


/* public function init_fade */
#ifdef __STDC__
void 
init_fade (int step)
#else
void 
init_fade(step)
     int step;
#endif
{
  int i;
  
  if (step == FADE_STEP) {
    fade_count = (fade_max/step);
    for (i=0; i< fade_count; i+=1) {
      line[i].x1 = 0;
      line[i].x2 = PIXW-1;
      line[i].y2 = line[i].y1 = i*step;
    }
  } else {
    fade_count = ((fade_max - step)/step/2);
    for (i=0; i< fade_count; i+=1) {
      line[i].x1 = 0;
      line[i].x2 = PIXW-1;
      line[i].y2 = line[i].y1 = (2*i+1)*step;
    }
  }
}



/* public function fade_out_window */
#ifdef __STDC__
void 
fade_out_window (int disp)
#else
void 
fade_out_window (disp)
     int disp;
#endif
{
  XDrawSegments(dpy[disp], win[disp], gc_window[disp],
                line, fade_count);
  flush_sync(disp);
}



/* public function fade_in_window */
#ifdef __STDC__
void 
fade_in_window (int disp)
#else
void 
fade_in_window (disp)
     int disp;
#endif
{
  XDrawSegments(dpy[disp], win[disp], gc_frompix[disp],
                line, fade_count);
  flush_sync(disp);
}



/* public function : clear_pixmap */
#ifdef __STDC__
void 
clear_pixmap (int disp)
#else
void 
clear_pixmap (disp)
     int disp;
#endif
{
  XFillRectangle(dpy[disp],pix[disp],gc_clearpix[disp], 0,0,PIXW,PIXH+SCOREH);
}


/* public fucntion add_rectangle */
#ifdef __STDC__
void 
add_maze_rectangle (int x,
		    int y)
#else
void 
add_maze_rectangle (x,y)
     int x,y;
#endif
{
  xrec_max->height = BLOCK_HEIGHT;
  xrec_max->x      = x*BLOCK_WIDTH;
  xrec_max->y      = y*BLOCK_HEIGHT;
  xrec_max->width  = BLOCK_WIDTH;

  if (xrec_max != xrec) {
    XRectangle *prev = xrec_max - 1;
    
    if ( (prev->y == xrec_max->y) 
	&& ((xrec_max->x - prev->x) == prev->width) ) {
      prev->width += BLOCK_WIDTH;
      xrec_max = prev;
    }
  }

  xrec_max ++;
}



#ifdef __STDC__
void 
add_stat_rectangle (int x,
		    int y)
#else
void 
add_stat_rectangle (x,y)
     int x,y;
#endif
{
  if (y == 0) {
    xrec_max->height = STAT_HEIGHT;
  } else {
    xrec_max->height = LED_HEIGHT;
  }

  xrec_max->x      = x*STAT_WIDTH;
  xrec_max->y      = MAZE_H*BLOCK_HEIGHT + y*STAT_HEIGHT;
  xrec_max->width  = STAT_WIDTH;

  if (xrec_max != xrec) {
    XRectangle *prev = xrec_max - 1;
    
    if ( (prev->y == xrec_max->y) 
	&& ((xrec_max->x - prev->x) == prev->width) ) {
      prev->width += BLOCK_WIDTH;
      xrec_max = prev;
    }
  }


  xrec_max ++;
}



/* public function flush_score_board */
#ifdef __STDC__
void 
flush_score_board (int disp)
#else
void 
flush_score_board (disp)
     int disp;
#endif
{
  XFillRectangle( dpy[disp], win[disp], gc_frompix[disp],
                 0, PIXH, PIXW, SCOREH);
  flush_sync(disp);
}



/* public function : flush_pixmap */
#ifdef __STDC__
void 
flush_pixmap (int disp, 
	      int num_disp, 
	      int flag)
#else
void 
flush_pixmap (disp, num_disp, flag)
     int disp, num_disp, flag;
#endif
{
  if (!flag) {
    /* Copy Pixmap to Window */
    XFillRectangle( dpy[disp], win[disp], gc_frompix[disp],
                   0, 0, PIXW, PIXH + SCOREH );
  } else {
#ifdef CLEAR_WINDOW
    XClearWindow( dpy[disp], win[disp] );
#endif 
    if (!iconified[disp]) {
      XFillRectangles( dpy[disp], win[disp], gc_frompix[disp], 
		      xrec, xrec_max - xrec );
    }
    if (disp == (num_disp -1)) {
      xrec_max = xrec;
    }
  }
  flush_sync(disp);
}

/*
 * public function win_is_mapped
 */
#ifdef __STDC__
int
win_is_mapped (int disp)
#else
int
win_is_mapped (disp)
     int disp;
#endif
{
  return !iconified[disp];
}

/* public function draw_bomb_sprite */
#ifdef __STDC__
void 
draw_bomb_sprite (int disp, 
		  Sprite *ptr)
#else
void 
draw_bomb_sprite (disp, ptr)
     int disp;
     Sprite *ptr;
#endif
{
  BombSprite *spl = (BombSprite *)ptr;

  /* draw sprite as clipped tile when in color mode */
  if ( !(spl->mode & SPM_MASK_ONLY) && colorMode[disp] ) {
    /* set drawing offset */
    XSetClipOrigin(dpy[disp], gc_sprite_bits[disp], spl->x, spl->y);
    XSetTSOrigin(dpy[disp], gc_sprite_bits[disp], spl->x, spl->y);
    XSetClipMask(dpy[disp], gc_sprite_bits[disp], 
		 pix_bomb_mask[disp][spl->anime] );
    XSetTile(dpy[disp], gc_sprite_bits[disp],
	     pix_bomb_bits[disp][spl->anime] );
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_bits[disp],
		   (int)spl->x, (int)spl->y, BLOCK_WIDTH, BLOCK_HEIGHT);
  } else {
    /* draw sprite mask if not in color mode or mask is needed*/
    XSetTSOrigin(dpy[disp], gc_sprite_mask[disp], spl->x, spl->y);
    XSetStipple(dpy[disp], gc_sprite_mask[disp],
		pix_bomb_mask[disp][spl->anime] );
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_mask[disp],
		   (int)spl->x, (int)spl->y, BLOCK_WIDTH, BLOCK_HEIGHT);
  }
  /* draw sprite as stipple when in monochrome mode */
  if (!colorMode[disp] && !(spl->mode & SPM_MASK_ONLY) ) {
    /* set drawing offset */
    XSetTSOrigin(dpy[disp], gc_sprite_bits[disp], spl->x, spl->y);
    XSetStipple(dpy[disp], gc_sprite_bits[disp], 
		pix_bomb_bits[disp][spl->anime] );
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_bits[disp],
		   (int)spl->x, (int)spl->y, BLOCK_WIDTH, BLOCK_HEIGHT);
    
  }
}


/* public function draw_player_sprite */
#ifdef __STDC__
void 
draw_player_sprite (int disp, Sprite *ptr)
#else
void 
draw_player_sprite (disp, ptr)
     int disp;
     Sprite *ptr;
#endif
{
  PlayerSprite *spl = (PlayerSprite *)ptr;
  
  /* draw sprite as clipped tile when in color mode */
  if ( !(spl->mode & SPM_MASK_ONLY) && colorMode[disp] ) {
    /* set drawing offset */
    XSetClipOrigin(dpy[disp], gc_sprite_bits[disp], spl->x, spl->y);
    XSetTSOrigin(dpy[disp], gc_sprite_bits[disp], spl->x, spl->y);
    XSetClipMask(dpy[disp], gc_sprite_bits[disp], 
		 pix_sprite_mask[disp][spl->anime] );
    XSetTile(dpy[disp], gc_sprite_bits[disp], 
	     pix_sprite_bits[disp][spl->player][spl->anime]);
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_bits[disp],
		   (int)spl->x, (int)spl->y,
		   (spl->anime == WINNER_ANIME) ? WINNER_WIDTH :SPRITE_WIDTH, 
		   (spl->anime == WINNER_ANIME) ? WINNER_HEIGHT:SPRITE_HEIGHT);
  } else {
    /* draw sprite mask if not in color mode or mask is needed*/
    XSetTSOrigin(dpy[disp], gc_sprite_mask[disp], spl->x, spl->y);
    XSetStipple(dpy[disp], gc_sprite_mask[disp],
		pix_sprite_mask[disp][spl->anime] );
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_mask[disp],
		   (int)spl->x, (int)spl->y,
		   (spl->anime == WINNER_ANIME) ? WINNER_WIDTH:SPRITE_WIDTH, 
		   (spl->anime == WINNER_ANIME) ? WINNER_HEIGHT:SPRITE_HEIGHT);
  }
  /* draw sprite as stipple when in monochrome mode */
  if (!colorMode[disp] && !(spl->mode & SPM_MASK_ONLY) ) {
    /* set drawing offset */
    XSetTSOrigin(dpy[disp], gc_sprite_bits[disp], spl->x, spl->y);
    XSetStipple(dpy[disp], gc_sprite_bits[disp], 
		pix_sprite_bits[disp][spl->player][spl->anime]);
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_bits[disp],
		   (int)spl->x, (int)spl->y,
		   (spl->anime == WINNER_ANIME) ? WINNER_WIDTH :SPRITE_WIDTH, 
		   (spl->anime == WINNER_ANIME) ? WINNER_HEIGHT:SPRITE_HEIGHT);
  }
}



/* public function draw_time_led */
#ifdef __STDC__
void 
draw_time_led (int disp, 
	       int x, 
	       int block)
#else
void 
draw_time_led (disp, x, block)
     int disp, x, block;
#endif
{
  XSetTile(dpy[disp], gc_drawblock[disp], pix_leds[disp][block]);
  XFillRectangle(dpy[disp], pix[disp], gc_drawblock[disp],
                 x*LED_WIDTH, MAZE_H*BLOCK_HEIGHT + STAT_HEIGHT, 
		 LED_WIDTH, LED_HEIGHT);
}



/* public function draw_score_block */
#ifdef __STDC__
void 
draw_score_block (int disp, 
		  int x, 
		  int block)
#else
void 
draw_score_block (disp, x, block)
     int disp, x, block;
#endif
{
  XSetTile(dpy[disp], gc_drawblock[disp], pix_score[disp][block]);
  XFillRectangle(dpy[disp], pix[disp], gc_drawblock[disp],
		 x*STAT_WIDTH, MAZE_H*BLOCK_HEIGHT,
		 STAT_WIDTH, STAT_HEIGHT );
}


/* public function draw_score_block */
#ifdef __STDC__
void 
draw_score_block_half (int disp, 
		       int x, 
		       int block,
		       int left)
		       
#else
void 
draw_score_block_half (disp, x, block, left)
     int disp, x, block, left;
#endif
{
  XSetTile(dpy[disp], gc_drawblock[disp], pix_score[disp][block]);
  if (left) {
    XFillRectangle(dpy[disp], pix[disp], gc_drawblock[disp],
		   x*STAT_WIDTH, MAZE_H*BLOCK_HEIGHT,
		   7*STAT_WIDTH/12, STAT_HEIGHT );
  } else {
    XFillRectangle(dpy[disp], pix[disp], gc_drawblock[disp],
		   (12*x+7)*STAT_WIDTH/12, MAZE_H*BLOCK_HEIGHT,
		   5*STAT_WIDTH/12, STAT_HEIGHT );
  }
}



/* public draw_polygon */
#ifdef __STDC__
void 
draw_polygon (int disp,
	      int x, 
	      int y, 
	      int w, 
	      int h,
	      BMPoint *points,
	      int npoints, 
	      int black_white)
#else
void 
draw_polygon (disp, x, y, w, h, points, npoints, black_white )
     int disp;
     int x, y, w, h;
     BMPoint *points;
     int npoints,black_white ;
#endif
{
  XPoint *xp;
  int i;

  xp = (XPoint *)calloc(sizeof(XPoint),npoints+1);
  
  for (i=0; i < npoints; i++) {
    xp[i].x = (int)(x + w*points[i].x);
    xp[i].y = (int)(y + h*points[i].y);
  }
  xp[npoints]=xp[0];

  if (black_white) {
    XFillPolygon(dpy[disp], pix[disp], gc_text_black[disp], xp, npoints, 
		 Complex, CoordModeOrigin);
    XDrawLines(dpy[disp], pix[disp], gc_text_white[disp], xp, npoints+1, 
	       CoordModeOrigin);
  } else {
    XFillPolygon(dpy[disp], pix[disp], gc_text_white[disp], xp, npoints, 
		 Complex, CoordModeOrigin);
    XDrawLines(dpy[disp], pix[disp], gc_text_black[disp], xp, npoints+1, 
	       CoordModeOrigin);
  }

  free(xp);
}     


/*
 * public draw_textbox
 */
#ifdef __STDC__
void
draw_textbox (int disp, 
	      char *text,
	      int flags,
	      BMRectangle *rect)
#else
void
draw_textbox (disp, text, flags, rect)
     int disp;
     char *text;
     int flags;
     BMRectangle *rect;
#endif
{
  XFontStruct *font;
  int y, width, height;
  GC gc_fg, gc_bg;
  XRectangle clip;

  /* first get used font */
  font = font_struct[disp][FM_Size & flags];

  /* set gc for foreground and background */
  if (flags & FM_Color) {
    gc_fg = gc_text_white[disp];
    gc_bg = gc_text_black[disp];
  } else {
    gc_fg = gc_text_black[disp];
    gc_bg = gc_text_white[disp];
  }

  /* draw boxes if needed */
  XSetTSOrigin(dpy[disp], gc_fg, 0, rect->y + (rect->h-BLOCK_HEIGHT)/2);
  if (flags & FM_Boxed) {
    if ( !(flags & FM_Transparent)) {
      XSetTSOrigin(dpy[disp], gc_bg, 0, rect->y + (rect->h-BLOCK_HEIGHT)/2);
      XFillRectangle(dpy[disp], pix[disp], gc_bg, rect->x, rect->y, 
		     rect->w, rect->h);
    } else {
      XGCValues xgcv;

      xgcv.line_width = 1;
      XChangeGC(dpy[disp], gc_bg, GCLineWidth , &xgcv);
      for (y=0; y < rect->h; y+=2) {
	XDrawLine(dpy[disp], pix[disp], gc_bg, rect->x, rect->y + y, 
		  rect->x + rect->w -1, rect->y + y);
      } 
      xgcv.line_width = colorMode[disp] ? 3 : 2;
      XChangeGC(dpy[disp], gc_bg, GCLineWidth , &xgcv);
    }
    XDrawRectangle(dpy[disp], pix[disp], gc_fg, rect->x, rect->y, 
		   rect->w, rect->h);
  }

  /* draw string */
  if (NULL != text) {
    /* set clipping rectangles */
    clip.x = rect->x;
    clip.y = rect->y;
    clip.width = rect->w;
    clip.height = rect->h;
    XSetClipRectangles(dpy[disp], gc_fg, 0, 0, &clip, 1, YXBanded);
    /* dimensions of text */
    width = XTextWidth(font, text, strlen(text) );
    height = font->max_bounds.ascent - font->max_bounds.descent;
    /* draw it */
    XSetFont(dpy[disp], gc_fg, font->fid);
    XDrawString(dpy[disp], pix[disp], gc_fg, 
		rect->x + (rect->w-width)/2, rect->y + (height+rect->h)/2,
		text, strlen(text));
    /* reset clip mask */
    XSetClipMask(dpy[disp], gc_fg, None);
  }
}



/* 
 * public function draw_winner 
 */
#ifdef __STDC__
void 
draw_winner (int disp, 
	     int player, 
	     int x, 
	     int y)
#else
void 
draw_winner (disp, player, x, y)
     int disp, player;
     int x,y;
#endif
{
  if (colorMode[disp]) {
    XSetClipOrigin(dpy[disp], gc_sprite_bits[disp], x, y );
    XSetTSOrigin(dpy[disp], gc_sprite_bits[disp], x, y );
    XSetClipMask(dpy[disp], gc_sprite_bits[disp], 
		 pix_sprite_mask[disp][BIG_ANIME] );
    XSetTile(dpy[disp], gc_sprite_bits[disp], 
	     pix_sprite_bits[disp][player][BIG_ANIME]);
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_bits[disp],
		   x, y, BIG_WIDTH, BIG_HEIGHT );
  } else {
    /* mask */
    XSetTSOrigin(dpy[disp], gc_sprite_mask[disp], x, y );
    XSetStipple(dpy[disp], gc_sprite_mask[disp],
		pix_sprite_mask[disp][BIG_ANIME] );
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_mask[disp],
		   x, y, BIG_WIDTH, BIG_HEIGHT );
    /* bits */
    XSetTSOrigin(dpy[disp], gc_sprite_bits[disp], x, y); 
    XSetStipple(dpy[disp], gc_sprite_bits[disp], 
		pix_sprite_bits[disp][player][BIG_ANIME] );
    XFillRectangle(dpy[disp], pix[disp], gc_sprite_bits[disp],
		   x, y, BIG_WIDTH, BIG_HEIGHT );
  }
}



/* public function draw_circle_from_pixmap */
#ifdef __STDC__
void 
draw_circle_from_pixmap (int disp, 
			 int x, 
			 int y, 
			 int r)
#else
void 
draw_circle_from_pixmap (disp, x, y, r)
     int disp;
     int x,y,r;
#endif
{
  XFillArc(dpy[disp], win[disp], gc_frompix[disp], 
	   x - r , y -r, 2*r, 2*r,
	   0, 360*64);
}



/* public function do_bell */
#ifdef __STDC__
void 
do_bell (int disp)
#else
void 
do_bell (disp)
     int disp;
#endif
{
  XBell(dpy[disp],BELL_VOLUME);
}

#ifdef __STDC__
void 
no_bell (int disp)
#else
void 
no_bell (disp)
     int disp;
#endif
{
}

/*
 * end of file graphics.c
 */
