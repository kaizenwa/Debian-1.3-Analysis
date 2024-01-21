/* ########################################################################

				shadow.c

   File: shadow.c
   Path: /home/fournigault/c/X11/xcoral-2.31/shadow.c
   Description: 
   Created: Fri Jan 27 11:28:54 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:28:55 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#include <X11/Xlib.h>
#include <stdio.h>

#include "shadow.h"
#include "parse.h"

static char gray_bits[] = {0x01, 0x02 };

#define gray_width 		2
#define gray_height 		2
#define UP			0	   
	   
static unsigned long fg_pixel, bg_pixel;	   
static GC relief_gc;
static int shadow_color = True;
static Pixmap pixmap;

/*
**	Function name : CreateRGC
**
**	Description : Creation du contexte graphique pour le relief.
**	Input : Le display.
**	Ouput :
*/
void CreateRGC ( display )
    Display *display;
{
    GC gc;
    XGCValues gcv;
    unsigned long mask;
    int screen, result;

    shadow_color = UseColor ();
    
    screen = DefaultScreen ( display );
    mask = GCBackground | GCForeground | GCFunction;
    gcv.function = GXcopy;
    
    if ( shadow_color == False ) {
	/*
	 * Monochrome
	 */
	pixmap = XCreatePixmapFromBitmapData ( display, 
		      DefaultRootWindow (display),
		      gray_bits, gray_width, gray_height,
		      BlackPixel ( display, screen ),
		      WhitePixel ( display, screen ), 
		      DefaultDepth ( display, screen ));
	
	fg_pixel = WhitePixel ( display, screen );
	bg_pixel = BlackPixel ( display, screen );
	
	gcv.background = bg_pixel;
	gcv.foreground = fg_pixel;
	
	gcv.tile = pixmap;
	gcv.fill_style = FillTiled;
	mask |= GCFillStyle | GCTile;		
    }
    else {
	/*
	 * color
	 */
      
	gcv.foreground = PixelValue ( display, "white", &result);
	fg_pixel = gcv.foreground;
	
	gcv.background = PixelValue ( display, "black", &result);
	bg_pixel = gcv.background;
	
	gcv.fill_style = FillSolid;
	mask |= GCFillStyle;
    }
    gc = XCreateGC ( display, DefaultRootWindow (display), mask, &gcv );
    XSetLineAttributes ( display, gc, 2, LineSolid, CapButt, JoinMiter );
    relief_gc = gc;
}



/*
**	Function name : Display3D
**
**	Description : Affiche le top et le bottom shadow.
**	Input : Le display, la fenetre.
**	Ouput :
*/
void Display3D ( display, window, top, bottom, size, direct )
    Display *display;
    Window window;
    unsigned long top, bottom;
    int size, direct;
{
    XWindowAttributes att;
    XGCValues gcv;
    XRectangle rec [10];
    XPoint points [10];
    unsigned long mask;
    
#ifdef DEBUG
    fprintf ( stderr, "top = %d bot =%d\n", top, bottom );
#endif
    
    if ( size == 0 ) return;
    
    XGetWindowAttributes ( display, window, &att );
    
    if ( size > att.width / 2 ) return;
    
    if ( direct == UP ) {
	/* Les rectangles en haut et a gauche */
	rec [0].x = 0;
	rec [0].y = 0;
	rec [0].width = att.width;
	rec [0].height = size ;
	rec [1].x = 0;
	rec [1].y = size;
	rec [1].width = size ;
	rec [1].height = att.height - size;
    }
    else {
	rec [0].x = 0;
	rec [0].y = att.height - size;
	rec [0].width = att.width;
	rec [0].height = size;
	rec [1].x = att.width - size;
	rec [1].y = 0;
	rec [1].width = size;
	rec [1].height = att.height - size;
    }
    if ( shadow_color == False ) {
	gcv.fill_style = FillTiled;
	gcv.tile = pixmap;
	gcv.foreground = fg_pixel; gcv.background = bg_pixel;
	mask = GCFillStyle | GCTile | GCBackground | GCForeground;
    }
    else {
	gcv.foreground = top; gcv.background = bottom;
	gcv.fill_style = FillSolid;
	mask = GCForeground | GCBackground | GCFillStyle;
    }
    XChangeGC ( display, relief_gc, mask, &gcv );
    
    XFillRectangles ( display, window, relief_gc, rec, 2 );
    
    if ( direct == UP ) { 
	/* le polygone en bas et a droite */
	points [0].x = att.width; points [0].y = 0;
	points [1].x = att.width - size; points [1].y = size;
	points [2].x = att.width - size; points [2].y = att.height - size;
	points [3].x = size; points [3].y = att.height - size;
	points [4].x = 0; points [4].y = att.height;
	points [5].x = att.width; points [5].y = att.height;
    }
    else {
	/* le polygone en haut et a gauche */
	points [0].x = att.width; points [0].y = 0;
	points [1].x = att.width - size; points [1].y = size;
	points [2].x = size; points [2].y = size;
	points [3].x = size; points [3].y = att.height - size;
	points [4].x = 0; points [4].y = att.height;
	points [5].x = 0; points [5].y = 0;
    }
    
    /*
       * On inverse le foreground et le background.
    */
    if ( shadow_color == False ) {
	gcv.fill_style = FillSolid;
	gcv.foreground = bg_pixel; gcv.background = fg_pixel;
	mask = GCFillStyle | GCBackground | GCForeground;
    }
    else {
	gcv.foreground = bottom; gcv.background = top;
	gcv.fill_style = FillSolid;
	mask = GCForeground | GCBackground | GCFillStyle;
    }
    XChangeGC ( display, relief_gc, mask, &gcv );
    
    XFillPolygon ( display,
		  window, relief_gc, points, 6, Nonconvex, CoordModeOrigin );
}


/*
**	Function name : GetShadow
**
**	Description : Calcul des couleurs pour le top et le bottom shadow
**		en fonction d'un couleur donnee.
**	Input : Le display, la structure contenant la valeur en pixel
**		de la couleur dont on veut calculer ts et bs.
**		les valeurs de retour pour ts et bs.
**	Ouput : 
*/
void GetShadow ( display, xcolor, ts, bs )
    Display *display;
    XColor *xcolor;
    unsigned long *ts, *bs;	/* RETURN */
{
    Colormap cmap;
    XColor  top_sh, bot_sh;
    int brightness;
    float factor;
    extern void exit ();
    
    cmap = DefaultColormap ( display, DefaultScreen ( display ));
    
    /* 
     * Monochrome
     */
    if ( shadow_color == False ) {
	*ts = fg_pixel;
	*bs = bg_pixel;
	return;
    }
    
    /*
       * Couleur.
       * Calcul de l'eclat : L'intensite intervient pour 1/4 et
       * la luminosite pour 3/4
    */
    brightness = (0.25 * ((int) (((float) xcolor -> red + (float) xcolor -> green + (float) xcolor -> blue) / 3)) ) +
      ((float) 0.75 * (((float) 0.3 * xcolor -> red) + ((float) 0.59 * xcolor -> green) +((float) 0.11 * xcolor -> blue )));
    
    /*
     * Suivant la valeur de l'eclat par rapport a 2 seuils, on calcul
     * le top et bottom shadow correspondant a la couleur de base.
     */
#define MAX	65535
#define CLAIR	(0.75 * MAX)
#define SOMBRE	(0.15 * MAX)
    
    if ( brightness < SOMBRE ) {
      bot_sh.red = xcolor -> red 
	+ (unsigned short) ((float) (MAX - (float) xcolor -> red) * (float) 0.2 );
	bot_sh.green= xcolor -> green 
	  + (unsigned short) ((float) (MAX - (float) xcolor -> green) * (float) 0.2 );
	bot_sh.blue = xcolor -> blue 
	  + (unsigned short) ((float) (MAX - (float) xcolor -> blue) * (float) 0.2 );
	top_sh.red = xcolor -> red 
	  + (unsigned short) ((float) (MAX - (float) xcolor -> red) * (float) 0.5);
	top_sh.green= xcolor -> green 
	  + (unsigned short) ((float) (MAX - (float) xcolor -> green) * (float) 0.5);
	top_sh.blue = xcolor -> blue 
	  + (unsigned short) ((float) (MAX - (float) xcolor -> blue) * (float) 0.5);
    }
    else if ( brightness > CLAIR ) {
	bot_sh.red = xcolor -> red * 0.2;
	bot_sh.green = xcolor -> green * 0.2;
	bot_sh.blue = xcolor -> blue *  0.2;
	top_sh.red = xcolor -> red * 0.95;
	top_sh.green = xcolor -> green * 0.95;
	top_sh.blue = xcolor -> blue * 0.95;
    }
    else {
	factor =  ((0.60 - (brightness * ( 0.25 ) / MAX))); 
	
	bot_sh.red = xcolor -> red - (xcolor -> red * factor);
	bot_sh.green = xcolor -> green - (xcolor -> green * factor);
	bot_sh.blue = xcolor -> blue -(xcolor -> blue * factor);
	
	factor =  ((0.40 + (brightness * ( 0.30 ) / MAX))); 
	
	top_sh.red = xcolor -> red 
	  + (unsigned short) ((float) (MAX - (float) xcolor -> red) * (float) factor );
	top_sh.green= xcolor -> green 
	  + (unsigned short) ((float) (MAX - (float) xcolor -> green) * (float) factor);
	top_sh.blue = xcolor -> blue 
	  + (unsigned short) ((float)(MAX - (float) xcolor -> blue) * (float) factor );
    }
    
    top_sh.pixel = 0;
    bot_sh.pixel = 0;
    
    if ( XAllocColor ( display, cmap, &top_sh ) == 0 ) {
	(void) fprintf ( stderr, "Warning : XAllocColor error\n" );
	top_sh.pixel = WhitePixel(display, DefaultScreen ( display ));
/*	(void) exit (1); */
    }
    *ts = top_sh.pixel;
    
    if ( XAllocColor ( display, cmap, &bot_sh ) == 0 ) {
	(void) fprintf ( stderr, "Warning : XAllocColor error\n" );
	top_sh.pixel = BlackPixel(display, DefaultScreen ( display ));
/*	(void) exit (1);     */
    }
    *bs = bot_sh.pixel;
}
