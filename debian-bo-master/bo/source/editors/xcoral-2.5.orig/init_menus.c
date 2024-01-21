/* ########################################################################

			      init_menus.c

   File: init_menus.c
   Path: /home/fournigault/c/X11/xcoral-2.31/init_menus.c
   Description: 
   Created: Fri Jan 27 11:08:50 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:08:52 MET 1995
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
#include <X11/Xutil.h>
#ifndef apollo
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif
#endif

#include "init_menus.h"

XContext		SwContext;
XContext		TiContext;
int menus_color = True;
static ResourcesMenu	rm;

FCT (static int, getnbstr, (char **tstr) );
FCT (static char *, getmaxstr, (char **tstr) );
FCT (static void, show_bar, (Display *display, Window w, XYMenu *menu) );
FCT (static VMenu *, Vmenu, (Display *display, Window window,XYMenu *menu, char **item, int x_title, void (**fnt) (), unsigned long bg) );

extern void Display3D ();

/*
**	Function name : InitMenuRes
**
**	Description :  Initialisations des ressources pour les menus.
**		La fonte, le foregreound, le background ainsi que
**		les couleurs utilisees pour le 3D. 
**		Plusieurs contextes graphiques sont mis en oeuvre.
**	
**	Input :  Le display, font, foreground, background, top_shadow et bottom_shadow.
**	Ouput :
*/
void InitMenusRes ( display, font, fg, bg, ts, bs )
    Display		*display;
    XFontStruct	*font;
    unsigned long 	fg, bg, ts, bs;
{
  GC gc;
  XGCValues gcv;
  unsigned long gcm;
  
  gc = DefaultGC ( display, DefaultScreen (display ));
  
  gcv.line_width = 0; gcv.font = font -> fid;
  gcv.background = bg; gcv.foreground = fg;
  gcm = GCLineWidth | GCFont | GCForeground | GCBackground;

  rm.ngc = XCreateGC ( display, DefaultRootWindow ( display ), gcm, &gcv );
  rm.bgc = XCreateGC ( display, DefaultRootWindow ( display ), gcm, &gcv ); 
  rm.ugc = XCreateGC ( display, DefaultRootWindow ( display ), gcm, &gcv );

  gcm = 0;
  gcm |= GCFunction;	gcv.function = GXxor;
  gcm |= GCPlaneMask;	gcv.plane_mask = fg ^ bg;
  gcm |= GCForeground;	gcv.foreground = fg ^ bg;
  gcm |= GCBackground;	gcv.background = bg;
  rm.igc = XCreateGC ( display, DefaultRootWindow ( display ), gcm, &gcv );
  
  rm.top_shadow = ts;
  rm.bot_shadow = bs;
  rm.font = font;
  rm.fg = fg;
  rm.bg = bg;
  
#ifdef NOTDEF
  XCopyGC ( display, gc, (~0), rm.ngc );
  XCopyGC ( display, gc, (~0), rm.igc );
  
  gcm = 0;
  gcm |= GCFunction;	gcv.function = GXxor;
  gcm |= GCPlaneMask;	gcv.plane_mask = fg ^ bg;
  gcm |= GCForeground;	gcv.foreground = fg ^ bg;
  gcm |= GCBackground;	gcv.background = bg;
  
  XChangeGC ( display, rm.igc, gcm, &gcv );
  
  gcv.line_width = 0; gcv.font = font -> fid;
  gcv.background = bg; gcv.foreground = fg;
  
  XChangeGC ( display, rm.ngc, GCLineWidth | GCFont | GCForeground |
	     GCBackground, &gcv );
  
  XCopyGC ( display, rm.ngc, (~0), rm.bgc );
  XCopyGC ( display, rm.ngc, (~0), rm.ugc );
#endif
  
  rm.height_bar = font -> ascent + font -> descent + BAR_SPACE;
  
  TiContext  = XUniqueContext ();
  SwContext = XUniqueContext ();
  menus_color = UseColor ();
}


/*
**	Function name : MakeMenus
**
**	Description : Fabrication des menus pour chacun des titres.
**
**	Input :  Le display, la fenetre parent, les titres, les items et la
**		table des callbacks.
**	Ouput : XYMenu *
*/
XYMenu *MakeMenus ( display, w, title, item, fnt ) 
    Display		*display;
    Window		w;
    char		**title;
    char		***item;
    void		(**fnt []) ();
{
  XYMenu *menu;
  SWindow *sw;
  XWindowAttributes att;
  int i = 0;
  int x, width;
  VMenu	*Vmenu ();
  
  XGetWindowAttributes ( display, w, &att );
  menu = ( XYMenu * ) malloc ( sizeof ( XYMenu ));
  bzero ( (char *) menu, sizeof ( XYMenu ));
  
  menu -> width_relief = DEPTH_WIDTH;
  menu -> font = rm.font;
  menu -> bar_height = rm.height_bar;
  menu -> bar_width 	= att.width + ( menu -> width_relief * 2 );
  menu -> h_item = rm.font->ascent + rm.font->descent + 2 + ( 2 * menu -> width_relief );
  menu -> y_menu = menu -> bar_height + 2;		
  menu -> titlename 	= title;
  menu -> itemname 	= item;
  menu -> y_title = rm.font -> ascent +  1;
  menu -> parent = w;
  menu -> n_last_unmapped = -1;
  menu -> hmax_menu = 0;
  menu -> w_under = w;
  menu -> Ngc = rm.ngc;
  menu -> Igc = rm.igc;
  menu -> Bgc = rm.bgc;
  menu -> Ugc = rm.ugc;
  menu -> top_sh = rm.top_shadow;
  menu -> bot_sh = rm.bot_shadow;
  
  menu -> fg_bar = menu -> fg_menu = rm.fg;
  menu -> bg_bar = menu -> bg_menu = rm.bg;
  
  menu -> w_bar = XCreateSimpleWindow ( display, w, 0, 0, menu -> bar_width,
				       menu -> bar_height, 0, rm.bg, rm.bg );
  XSelectInput ( display, menu->w_bar, ExposureMask | StructureNotifyMask
		| VisibilityChangeMask );
  
  menu->mapped = 0;
  menu -> save = 0;
  x = 2;
  while ( *title != 0 ) {
    width = XTextWidth ( rm.font, *title, strlen ( *title )) + 20;
    *title++;	
    menu->w_title [i] =
      XCreateSimpleWindow ( display, menu->w_bar, x, 2,
			   width,
			   menu -> h_item,
			   0, rm.bg, rm.bg );
    menu->title_width [i] = width;
    /*
       * Ne sert que pour savoir si on est dans la barre des titres.
       * le choix de width n'a pas d'importance.
    */
    XSaveContext ( display, menu->w_title [i], TiContext, (caddr_t) width ) ;
    
    XSelectInput ( display, menu->w_title [i], ExposureMask |
		  EnterWindowMask | ButtonPressMask );
    menu -> no_menu = i;		
    sw = ( SWindow * ) malloc ( sizeof ( SWindow ));
    sw -> w = menu -> w_title [i];
    sw -> type = MENU;
    sw -> no_m = i;
    sw -> no_i = 0;
    if ( XSaveContext ( display, sw -> w, SwContext, (caddr_t) sw ) != 0 )
      (void) fprintf ( stderr, "make_xymenus XSaveContext Error\n" );
    menu->vmenu [i] = Vmenu ( display, w, menu, *item, x, *fnt, rm.bg ); 
    if ( menu -> vmenu [i] -> height > menu -> hmax_menu )
      menu -> hmax_menu = menu -> vmenu [i] -> height;
    x += width;
    i++;
    *item++;
    *fnt++;
    if ( i > 10 )
      break;
  }
  menu->nb_menus = i;
  if ( DoesSaveUnders ( DefaultScreenOfDisplay ( display ) ) != True )
    SetMenuPixmap ( display, menu, menu -> bar_width );
  show_bar ( display, menu -> parent, menu );
  return menu;
}

/*
**	Function name : MouseInMenuBar
**
**	Description :  Comme son nom l'indique.
**
**	Input : Le Display, la fenetre
**	Ouput : Vrai ou Faux.
*/
int MouseInMenuBar ( display, w )
    Display *display;
    Window w;
{
  int m;
  
  if ( XFindContext ( display, w, TiContext, (caddr_t *) &m ) != XCNOENT )
    return True;
  else
    return False;
}


/*
**	Function name : SetMenuPixmap
**
**	Description : Fabrication d'un pixmap pour sauvegarder
**		les parties cachees.
**	Input : Le display, le menu, largeur de la fenetre parent.
**	Ouput :
*/
void SetMenuPixmap ( display, menu, width )
    Display *display;
    XYMenu *menu;
    int width;
{
  
  if ( menu -> save != 0 ) {
#ifdef DEBUG
    fprintf ( stderr, "Pixmap ms = %d\n", menu -> save );
#endif
    XFreePixmap ( display, menu -> save );
  }
  
#ifdef SHADOW
  menu -> save = XCreatePixmap ( display,
				menu -> w_under , width,
				menu -> hmax_menu + SHAD + 10,
				DefaultDepth ( display, DefaultScreen ( display )));
#else
  menu -> save = XCreatePixmap ( display,
				menu -> w_under , width,
				menu -> hmax_menu + 10,
				DefaultDepth ( display, DefaultScreen ( display )));
#endif
  menu -> save_ok = 0;		/* Ya rien  dans la pixmap */
  menu -> pix_width = width;
}


/*
**	Function name : Vmenu
**
**	Description : Fabrication des menus associes a chaque titre
**		principal.
**
**	Input :  Le Display, la fenetre parent, menu, table des items,
**		la fonte, le background.
**	Ouput :
*/
static VMenu *Vmenu ( display, window, menu, item, x_title, fnt, bg )
    Display	*display;
    Window	window;
    XYMenu	*menu;
    char **item;
    int x_title;
    void (**fnt) ();
    unsigned long bg;
{
  VMenu	*vm;
  SWindow	*sw;
  char	*getmaxstr ();
  XSetWindowAttributes att, att_io;
  int x_menu, n;
  char *maxstr;
  int nb_item;
  
  vm = ( VMenu * ) malloc ( sizeof ( VMenu ));
  bzero ( (char *) vm, sizeof ( VMenu ));
  maxstr = getmaxstr ( item );
  nb_item = getnbstr ( item );
  
  vm -> height = ( nb_item * menu -> h_item ) + 6;
  vm -> width = XTextWidth ( menu->font, maxstr, strlen ( maxstr )) + 26
    + ( 2 * menu -> width_relief );
  
  x_menu = x_title;	
  vm -> x = x_menu;
#ifdef SHADOW
  att_io.win_gravity = NorthWestGravity;
  att_io.event_mask = 0;
  att_io.override_redirect = False;
  att_io.do_not_propagate_mask = NoEventMask;
  att_io.cursor = None;
  vm -> trans = XCreateWindow ( display, window, x_menu, menu -> y_menu,
			       vm -> width + SHAD, vm -> height + SHAD, 0, 0,
			       InputOutput, CopyFromParent,
			       CWWinGravity | CWEventMask | CWOverrideRedirect | 
			       CWDontPropagate | CWCursor, 
			       &att_io );
  vm -> shadow = XCreateSimpleWindow ( display, vm -> trans, SHAD,
				      SHAD, vm->width, vm->height, 0, 
				      BlackPixel ( display, DefaultScreen ( display )),
				      BlackPixel ( display, DefaultScreen ( display )));
  vm -> v_frame = XCreateSimpleWindow ( display, vm -> trans, 0,
				       0, vm->width, vm->height, 0, bg, bg );
  XSelectInput ( display, vm->v_frame, ExposureMask | LeaveWindowMask );
  XSelectInput ( display, vm -> trans, ExposureMask | LeaveWindowMask );
#else
  vm->v_frame = XCreateSimpleWindow ( display, window, x_menu,
				     menu->y_menu, vm->width, vm->height, 0, bg, bg );
  XSelectInput ( display, vm->v_frame, ExposureMask | LeaveWindowMask );
#endif
  if ( DoesSaveUnders ( DefaultScreenOfDisplay ( display ) ) == True )
    att.save_under = True;
  else
    att.save_under = False;
#ifdef SHADOW
  XChangeWindowAttributes ( display, vm -> trans, CWSaveUnder, &att );
#else
  XChangeWindowAttributes ( display, vm->v_frame, CWSaveUnder, &att );
#endif
  n = 0;
  vm -> iname = item;
  while ( *item != 0 ) {
    vm->w_item [n] = XCreateSimpleWindow ( display, vm->v_frame,
					  3, (n * menu -> h_item) + 3,
					  vm->width - 6, menu -> h_item, 0,
					  bg, bg );
    XSelectInput ( display, vm->w_item [n], ExposureMask
		  | EnterWindowMask );
    sw = ( SWindow * ) malloc ( sizeof ( SWindow ));
    sw -> w = vm->w_item [n];
    sw -> type = ITEM;
    sw -> no_i = n;
    sw -> no_m = menu -> no_menu;
    if ( XSaveContext ( display, sw -> w, SwContext, (caddr_t) sw ) != 0 )
      (void) fprintf ( stderr,  "make_vertical_menus XSaveContext Error\n" );
    vm -> func [n] = *fnt;
    n++;
    *item++;
    *fnt++;
  }
#ifdef SHADOW
  XMapSubwindows ( display, vm -> trans );
  XMapSubwindows ( display, vm -> v_frame );
#else
  XMapSubwindows ( display, vm -> v_frame );
#endif
  vm->nb_items = n;
  return ( vm );
}


/*
**	Function name : SetHiddenWindow
**
**	Description : Initialise les parametres de la fenetre cachee.
**
**	Input : Le menu, la fenetre cachee, le contexte graphique.
**	Ouput :
*/
void SetHiddenWindow ( menu, w, gc )
    XYMenu	*menu;
    Window	w;
    GC	gc;
{
  menu -> w_under = w;
  menu -> Ugc = gc;
}


/*
**	Function name : getnbstr
**
**	Description : Pour avoir la taille d'un tableau de pointeurs
**
**	Input : La tableau.
**	Ouput : le nombre d'entree non vide dans le tableau.
*/
static int getnbstr ( tstr )
    char **tstr;
{
  int i = 0;
  while ( *tstr != 0 ) {
    i++;
    *tstr++;
    if ( i > 100 ) return ( 0 );
  }
  return ( i );
}


/*
**	Function name : getmaxstr
**
**	Description : Donne la plus longue chaine dans un tableau,
**		en fonction de la fonte utilisee.
**
**	Input : Le tableau
**	Ouput : La plus grande chaine.
*/
static char *getmaxstr ( tstr )
    char **tstr;
{
  char *s;
  int i, j;
  
  i = 0;
  while ( *tstr != 0 ) {
    j = strlen ( *tstr );
    if ( (j=XTextWidth ( rm.font, *tstr, strlen ( *tstr ))) > i ) {
      s = *tstr;
      i = j;
    }
    tstr ++;
  }
  return ( s );
}


/*
**	Function name : RefreshMenuBar
**
**	Description : Redessine la barre des titres.
**
**	Input : Le display, le menu.
**	Ouput :
*/
void RefreshMenuBar ( display, menu )
    Display 	*display;
    XYMenu	*menu;
{
  int i;
  char 	**str = menu->titlename;
  
  XResizeWindow ( display, menu -> w_bar, menu -> bar_width,
		 menu -> bar_height );
  
  Display3D ( display, menu -> w_bar,
	     menu -> top_sh, menu -> bot_sh,
	     menu -> width_relief , DEPTH_UP );
  
  for ( i=0; i < menu->nb_menus; i++ ) {
    XDrawString ( display, menu->w_title [i], menu -> Bgc, 10,
		 menu -> y_title + menu -> width_relief + 1,
		 *str, strlen ( *str ));
    str++;
  }
}


/*
**	Function name : show_bar
**
**	Description : Affichage de la barre des titres.
**
**	Input : Le display, la fenetre mere, le menu.
**	Ouput :
*/
static void show_bar ( display, w, menu )
    Display	*display;
    Window	w;
    XYMenu	*menu;
{
  XEvent	event;
  
  XMapSubwindows ( display, menu->w_bar ); 
  XMapWindow ( display, menu->w_bar ); 
  XMapWindow ( display, w );
  XFlush ( display );
  XWindowEvent ( display, w, ExposureMask, &event );
  XWindowEvent ( display, menu -> w_bar, ExposureMask, &event );
  RefreshMenuBar ( display, menu );
}


/*
**	Function name : ButtonPressInMenu
**
**	Description : Comme son nom l'indique.
**
**	Input : La fenetre de l'evenement, le menu.
**	Ouput : Le numero du menu, sinon -1;
*/
int ButtonPressInMenu ( w, menu )
    Window	w;
    XYMenu	*menu;
{
  int i, n = -1;
  
  for ( i=0; i < menu -> nb_menus; i++) 
    if ( w == menu -> w_title [i] ) {
    n = i;
    break;
  }
  return ( n );
}


/*
**	Function name : DeleteMenu
**
**	Description : Comme son nom l'indique.
**
**	Input : Le display, le menu.
**	Ouput :
*/
void DeleteMenu ( display, menu )
    Display	*display;
    XYMenu	*menu;
{
  int i, j;
  SWindow	*sw;
  
  for ( i = 0; i < menu -> nb_menus; i++ ) {
    for ( j=0; j < menu -> vmenu [i] -> nb_items; j++ ) {
      XFindContext ( display, menu -> vmenu [i] -> w_item [j], SwContext, (caddr_t *) &sw );
      if ( sw != 0 )
	(void) free (( char * ) sw );
      XDeleteContext ( display, menu -> vmenu [i] -> w_item [j], SwContext );
      XDestroyWindow ( display, menu -> vmenu [i] -> w_item [j] ); 
    }
#ifdef SHADOW
    XDestroyWindow ( display, menu -> vmenu [i] -> trans );
#else
    XDestroyWindow ( display, menu -> vmenu [i] -> v_frame );
#endif
    XFindContext ( display, menu -> w_title [i], SwContext, (caddr_t *) &sw );
    if ( sw != 0 )
      (void) free ( ( char * ) sw );
    XDeleteContext ( display, menu -> w_title [i], SwContext );
    if ( menu -> vmenu [i] != 0 )
      free ( ( char * ) menu -> vmenu [i] );
  }
  XDestroyWindow ( display,menu -> w_bar );
  if ( DoesSaveUnders ( DefaultScreenOfDisplay ( display ) ) != True )
    XFreePixmap ( display, menu -> save );
  if ( menu != 0 )
    (void) free ( ( char * ) menu ); 
}
