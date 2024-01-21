/* ########################################################################

				scroll.c

   File: scroll.c
   Path: /home/fournigault/c/X11/xcoral-2.31/scroll.c
   Description: 
   Created: Fri Jan 27 11:28:01 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:28:02 MET 1995
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


#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>

#ifndef apollo
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif
#endif

#include "scroll.h"
#include "shadow.h"
#include "parse.h"

static char gray [] = { 0x1, 0x02 };

static Pixmap	pix;
static Cursor   cursor;

FCT (static int, myrint, (double x) );

/*
**	Function name : InitScroll
**
**	Description : Pour l'instant, on cree le cursor associe a
**		la barre de scroll.
**
**	Input :  Le display.
**	Ouput :
*/
void InitScroll ( display )
    Display	*display;
{
    pix = XCreatePixmapFromBitmapData ( display, 
				       DefaultRootWindow (display), gray, 2, 2, 
				       BlackPixel ( display, DefaultScreen (display)),
				       WhitePixel ( display, DefaultScreen (display)),
				       DefaultDepth (display, DefaultScreen ( display )));
    
    cursor = XCreateFontCursor ( display, XC_sb_left_arrow );
}


/*
**	Function name : MakeScroll
**
**	Description :  Construction d'une fenetre de scroll avec la
**		scrollbar.
**
**	Input :  Le display, la fenetre parent, position dans la fenetre
**		parent.
**	Ouput : Un pointeur sur SWin
*/
SWin *MakeScroll ( display, parent, x, y )
    Display	*display;
    Window	parent;
    int x, y;
{
    SWin *s;
    int result;
    unsigned long pixel;
    
    s = (SWin *) malloc ( (unsigned) sizeof ( SWin )); 
    
    s -> frame = XCreateSimpleWindow ( display, parent,
				      x, y, SCROLL_WIDTH - 2, DEFAULT_SIZE, 0,
				      BlackPixel ( display, DefaultScreen (display)),
				      WhitePixel ( display, DefaultScreen (display)));
    
    XSelectInput ( display, s -> frame, ButtonPressMask | ButtonMotionMask
		  | ButtonReleaseMask | ExposureMask | PointerMotionHintMask );
    
    s -> fx = x; s -> fy = y;
    
    s -> scroll = XCreateSimpleWindow ( display, s -> frame,
				       2, 2, SCROLL_WIDTH - 4 , DEFAULT_SIZE, 0,
				       BlackPixel ( display, DefaultScreen (display)),
				       WhitePixel ( display, DefaultScreen (display)));
    
    s -> sx = 2; s -> sy = 2;
    s -> line = s -> linepage = 0;
    s -> line_to_scroll = 0;
    s -> rest = 0;
    s -> last_dir = 0;
    
    XSelectInput ( display, s -> scroll, ExposureMask );
    s -> width = SCROLL_WIDTH;
    
    if ( DefaultDepth ( display, DefaultScreen (display)) == 1 ) {
	/* Black and White */
	XSetWindowBackgroundPixmap ( display, s -> scroll, pix );
	s -> bg = BlackPixel ( display, DefaultScreen (display));
	s -> fg = WhitePixel ( display, DefaultScreen (display));
    }
    else {
	/* Color */
	pixel = PixelValue ( display, "gray", &result );
	if ( result == False )
	  pixel = BlackPixel ( display, DefaultScreen (display));
	XSetWindowBackground ( display, s -> scroll, pixel );
	
	pixel = PixelValue ( display, "gray65", &result );
	if ( result == False )
	  pixel = WhitePixel ( display, DefaultScreen (display));
	XSetWindowBackground ( display, s -> frame, pixel );
	
	s -> bg = PixelValue ( display, "gainsboro", &result );
	if ( result == False )
	  s -> bg = PixelValue ( display, "white", &result );
	s -> fg = PixelValue ( display, "dimgray", &result );
	if ( result == False )
	  s -> fg = PixelValue ( display, "black", &result );	
    }
    return ( s );
}


/*
**	Function name : DeleteScroll
**
**	Description : Comme son nom l'indique.
**
**	Input : Le display, la fenetre de scroll
**	Ouput :
*/
void DeleteScroll ( display, swin )
    Display *display;
    SWin *swin;
{
    XDestroyWindow ( display, swin -> frame );
    (void) free ( (char *) swin );
}


/*
**	Function name : SetScrollLine
**
**	Description :  Positionne le nombre de ligne a scroller
**
**	Input :  Le display, nb lignes.
**	Ouput :
*/
void SetScrollLine ( s, n )
    SWin *s;
    int n;
{
    s -> line = n;
    s -> line_to_scroll = n - 1;

#ifdef DEBUG
    (void) fprintf ( stderr, "lines in buf = %d\n", s -> line );
#endif
}


/*
**	Function name : RefreshScroll
**
**	Description :  Ajuste la geometrie de la fenetre de scroll.
**		Positionne la scrollbar au bon endroit. Cette fonction
**		est utilisee apres un resize de la fenetre parent.
**
**	Input : Le display, le scroll, largeur, hauteur, no de la ligne courante.
**	Ouput :
*/
void RefreshScroll ( display, swin, width, height, n )
    Display *display;
    SWin	*swin;
    int 	width, height, n;
{
    swin -> f_height = height;
    swin -> fx = width - SCROLL_WIDTH;
    ShowScrollFrame ( display, swin );
    (void) MoveScrollBar ( display, swin, CURRENT, n );
}


/*
**	Function name : ShowScrollFrame
**
**	Description :  Affiche la fenetre de scroll en fonction de la
**		geometrie courante. Positionne la taille de la scrollbar.
**
**	Input :  Les display, le scroll
**	Ouput :
*/
void ShowScrollFrame ( display, swin )
    Display *display;
    SWin *swin;
{
    XMoveResizeWindow ( display, 
		       swin -> frame,
		       swin -> fx,
		       swin -> fy,
		       swin -> width,
		       swin -> f_height );
    
    XMapWindow ( display, swin -> frame );
    SetScrollBarSize ( display, swin );
}


/*
**	Function name : SetScrollBarSize
**
**	Description :  Calcul de la taille de la scrollbar.
**
**	Input : Le display, le scroll.
**	Ouput :
*/
void SetScrollBarSize ( display, swin )
    Display *display;
    SWin *swin;
{
    double x;
    
    x = ((double) swin -> linepage) 
      / ((double) swin -> line_to_scroll + swin -> linepage);
    
    if ( swin -> line == 0 )
      swin -> s_height = swin -> f_height - 4;
    
    swin -> s_height = (int)( myrint ((double)(swin -> f_height - 4 ) * x));
    
    if ( swin -> s_height < 10 ) {
#ifdef DEBUG
	(void) fprintf ( stderr, "s_height 10\n" );
#endif
	swin -> s_height = 10;
    }
    XResizeWindow ( display, swin -> scroll, SCROLL_WIDTH - 4, swin -> s_height ); 
    
    XMapWindow ( display, swin -> scroll );
    
    if ( swin -> line_to_scroll != 0 )
      swin -> delta = ((double)( swin -> f_height - 4 - swin -> s_height ) 
		       / (double)(swin -> line_to_scroll));
    else
      swin -> delta = 0;	
#ifdef DEBUG
    (void) fprintf ( stderr, "delta = %.2f n = %.2f\n", 
		    swin -> delta, ( swin -> f_height - 4 - swin -> s_height ) / swin -> delta );
#endif
}


/*
**	Function name : ExposeInScroll
**
**	Description : Traitement d'un expose event.
**	Input : Le display, la fenetre exposee, le scroll courant.
**	Ouput : Vrai si le courant est expose faux sinon.
*/
int ExposeInScroll ( dpy, w, swin )
    Display *dpy;
    Window w;
    SWin *swin;
{
    XEvent ev;
    
    if ( w == swin -> frame ) {	
	RefreshScrollFrame ( dpy, swin );
	while ( XCheckWindowEvent ( dpy, w,ExposureMask, &ev ));
	return True;
    }
    if ( w == swin -> scroll ) {
	RefreshScrollBar ( dpy, swin );
	while ( XCheckWindowEvent ( dpy, w, ExposureMask, &ev ));
	return True;
    }
    return False;
}

/*
**	Function name : ButtonPressInScroll
**
**	Description :  Comme son nom l'indique.
**
**	Input : le scroll, le fenetre qui a recu ButtonPress, y position.
**	Ouput : True or False.
*/
int ButtonPressInScroll ( swin, w, y, result )
    SWin *swin;
    Window w;
    int y;
    int *result; /* RETURN */
{
    if ( swin -> frame != w )
      return False;
    
    if ( y < swin -> sy ) 
      *result = PREVIOUS;
    else if ( y > swin -> s_height + swin -> sy ) 
      *result = NEXT;
    else {
	swin -> y_max = swin -> f_height - 2 - ( swin -> sy + swin -> s_height - y );
	swin -> y_min = y - swin -> sy + 2;
	swin -> last_y = y;
#ifdef DEBUG
	(void) fprintf ( stderr, "y_max = %d y_min = %d\n",
			swin -> y_max, swin -> y_min );
#endif
	*result = CURSOR;
    }
    return True;
}


/*
**	Function name : HandleScrollBar
**
**	Description :  Synchronise la scrollbar avec le defilement du texte pendant
**		le ButtonPress.
**		Pas simple mes amis, pas simple. 
**
**	Input :  Le display, le scroll, la callback.
**	Ouput :
*/
void HandleScrollBar ( display, swin, f )
    Display *display;
    SWin *swin;
    void (*f) ();
{
    int n, dy;
    double dn;
    XEvent event;
    Window root, child;
    unsigned int keys_buttons;
    int root_x, root_y, win_x, win_y, stat = 0;
    
    while ( XCheckMaskEvent ( display, PointerMotionMask, &event ));
    XDefineCursor ( display, swin -> frame, cursor );
    
    XGrabPointer ( display, swin -> frame, True, ButtonReleaseMask | PointerMotionMask,
		  GrabModeAsync, GrabModeAsync, swin -> frame, None, CurrentTime );
    
#ifdef DEBUG
    fprintf ( stderr, "delta = %.3f f_height = %d s_height = %d\n",
	     swin -> delta, swin -> f_height, swin -> s_height );
#endif
    dn = swin -> rest;
    for ( ;; ) {
	XNextEvent ( display, &event );
	switch ( event.type ) {
	case ButtonRelease:
	  XUngrabPointer ( display, CurrentTime );
	  XUndefineCursor ( display, swin -> frame );
	  swin -> last_y = 0;
	  swin -> rest = dn;
#ifdef DEBUG
	  fprintf ( stderr, "dn = %.2f rest = %.2f i = %d\n", dn, swin -> rest, i );
#endif
	  return;
	case MotionNotify:
	  while ( XCheckMaskEvent ( display,
				   ButtonMotionMask, &event ));
	  XQueryPointer ( display,
			 event.xmotion.window, &root, &child, &root_x,
			 &root_y, &win_x, &win_y, &keys_buttons );
	  if ( (dy = win_y - swin -> last_y ) == 0 ) continue;
	  if ( dy > 0 ) {
	      if ( stat != BOTTOM ) {
		  if ( (stat = MoveScrollBar ( display,
					      swin, DOWN, win_y - swin -> last_y )) == BOTTOM ) {
		      win_y = swin -> y_max;
		      swin -> rest = 0;
		      dy = win_y - swin -> last_y;
		  }
		  dn += (dy / swin -> delta);
		  n = (int) (myrint ((double)dn));  
		  if ( n > 0 ) {
		      swin -> rest = (double) (dn - n);
		      ( * f ) ( n, swin -> text );
		      dn = swin -> rest;
		  }
		  swin -> last_dir = BOTTOM;
		  swin -> last_y = win_y;
	      }
	      else 
		continue;
	  }
	  else {
	      if ( stat != TOP ) {
		  if ( (stat = MoveScrollBar ( display,
					      swin, UP, swin -> last_y - win_y )) == TOP ) {
		      win_y = swin -> y_min;
		      swin -> rest = 0;
		      dy = win_y - swin -> last_y;
		  }
		  dn += dy / swin -> delta;
		  n = (int) (myrint ((double)dn));
		  if ( n < 0 ) {
		      swin -> rest = (double) (dn - n);
		      ( * f ) ( n, swin -> text );
		      dn = swin -> rest;
		  }
		  swin  -> last_dir = TOP;
		  swin -> last_y = win_y;
	      }
	      else
		continue;
	  }
#ifdef DEBUG
	  (void) fprintf ( stderr, "dy = %d\tlast = %d\n", dy, swin -> last_y );
#endif
	  
#ifdef DEBUG
	  (void) fprintf ( stderr, "dy = %d\tdn = %.2f\tn = %d\t rest = %.2f\n",
			  dy, dn , n, swin -> rest );
#endif
	  break;
	}
    }
}


/*
**	Function name : MoveScrollBar
**
**	Description :  Deplacement de la scrollbar suivant la valeur du
**		flag ( 1er page, page suivante... etc ).
**
**	Input :  Le display, le scroll, flags, deplacement en pixels
**	Ouput : L'etat courant.
*/
int MoveScrollBar ( display, swin, flag, dy )
    Display *display;
    SWin *swin;
    int flag, dy;
{
    int stat = 0;
    int i=0, j, n=0;
    double dn = swin -> rest;
#ifdef DEBUG
    fprintf ( stderr, "dy = %d\n", dy );
#endif
    if ( swin -> sy == 2 ) {
	stat = TOP;
	swin -> rest = 0;
    }
    if ( swin -> sy == swin -> f_height - 2 - swin -> s_height ) {
	stat = BOTTOM;
	swin -> rest = 0;
    }
    switch ( flag ) {
    case UP:
      if ( stat == TOP )
	return stat;
      if ( swin -> sy - dy < 2 ) {
	  swin -> sy = 2;
	  stat = TOP;
      }
      else {
	  swin -> sy -= dy;
	  stat = OTHER;
      }
      break;
    case DOWN:
      if ( stat == BOTTOM )
	return stat;
      if ( (swin -> sy + dy) > (swin -> f_height -2 - swin -> s_height)) {
	  swin -> sy = swin -> f_height - 2 - swin -> s_height;
	  stat = BOTTOM;
      }
      else {
	  swin -> sy += dy;
	  stat = OTHER;
      }
      break;
    case FIRST:
      swin -> sy = 2;
      swin -> rest = 0;
      stat = TOP;
      break;
    case NEXT:
      j = swin -> linepage - 1;
      while ( n < j ) {
	  i++;
	  dn += ( 1 / swin -> delta);
	  n = (int) (myrint ((double)dn));
      }
      swin -> sy += i;
      swin -> rest = (double) (dn - j);
#ifdef DEBUG
      (void) fprintf ( stderr, "i = %d rest = %.3f dn = %.3f n = %d \n",
		      i, swin -> rest, dn, n );
#endif
      if ( swin -> sy > (swin -> f_height -2 - swin -> s_height) ) {
	  swin -> sy = swin -> f_height - 2 - swin -> s_height;
	  swin -> rest = 0;
      }
      stat = OTHER;
      break;
    case PREVIOUS:
      j = swin -> linepage - 1;
      while ( -n < j ) {
	  i ++;
	  dn += ( (-1) / swin -> delta );
	  n = (int) myrint ((double)dn);
      }
      swin -> sy -= i;
      swin -> rest = (double) (dn - (-j));
#ifdef DEBUG
      (void) fprintf ( stderr, "i = %d rest = %.3f dn = %.3f n = %d \n",
		      i, swin -> rest, dn, n );
#endif
      if ( swin -> sy < 2 ) {
	  swin -> sy = 2;
	  swin -> rest = 0;
      }
      stat = OTHER;
      break;
    case CURRENT:
      /* Le nombre de lignes qu'il aurait fallu scroller a partir du debut.  */
      j = dy;
      dn = 0;
#ifdef DEBUG
      (void) fprintf ( stderr,"j = %d\n", j );
#endif
      if ( dy == 0 )
	return 0;
      while ( n < j ) {
	  i++;
	  dn += ( 1 / swin -> delta);
	  n = (int) (myrint ((double)dn));
      }
      swin -> sy = i + 2;
      if ( swin -> sy < 2 ) swin -> sy = 2;
      swin -> rest = (double) (dn - j);
      break;
    }       
    XMoveWindow ( display, swin -> scroll, swin -> sx, swin -> sy );
    XFlush ( display );
    return stat;
}


/*
**	Function name : RefreshScrollBar
**
**	Description :  Affichage du 3D pour la scrollbar.
**
**	Input : Le display, le scroll
**	Ouput :
*/
void RefreshScrollBar ( display, s )
    Display *display;
    SWin *s;
{
    Display3D ( display, s -> scroll, s -> bg , s -> fg , 2, UP );
}

/*
**	Function name : RefreshScrollFrame
**
**	Description :  Affichage du 3D pour la fenetre de scroll.
**
**	Input : Le display, le scroll
**	Ouput :
*/
void RefreshScrollFrame ( display, s )
    Display *display;
    SWin *s;
{
    Display3D ( display, s -> frame, s -> bg , s -> fg , 2, DOWN );
}

/*
**	Function name : myrint
**
**	Description :  Calcul de partie entiere. 
**
**	Input :  un reel signe
**	Ouput : sa partie entiere
*/
static int myrint ( dx )
    double dx;
{
  float x = (float) dx;
  int i = (int) x;
  int sign = (x >= 0) ? 1 : -1;
  float tmp = ( x - i ) ? ( x -i ) : - ( x - i );
    
    if ( (0x1 & i ) == 1 ) {
	/* Impair */
	if ( tmp >= .5 ) i += sign;
    }
    else {
	/* Pair */
	if ( tmp > .5 ) i += sign;
    }
    return i;
}
