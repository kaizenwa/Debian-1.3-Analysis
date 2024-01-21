/* ########################################################################

			      new_window.c

   File: new_window.c
   Path: /home/fournigault/c/X11/xcoral-2.31/new_window.c
   Description: 
   Created: Fri Jan 27 11:21:35 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:21:36 MET 1995
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


/*
 * $Log: new_window.c,v $
 * Revision 1.2  1993/12/24  14:21:33  klamer
 * Changed check on used position specification of window from
 * testing on 0 to testing on < 0.
 *
 */
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <string.h>

#include "options.h"
#include "xcoral.h"
#include "new_window.h"
#include "parse.h"
#include "list_box.h"
#include "browser_init.h"
#include "man_box.h"
#include "process.h"
#include "warn_box.h"
#include "dial_box.h"
#include "get_file.h"
#include "macros.h"
#include "logo.bm"

extern Trans st_initial;

FCT (static void, SetProp, (Window w) );
FCT (static int, add_window, (EdWin *e) );

static Pixmap text_icon;

/*
**	Function name : InitIconPixmap
**
**	Description :
**	Input :
**	Output :
*/
void InitIconPixmap()
{
    text_icon = XCreatePixmapFromBitmapData ( dpy, DefaultRootWindow ( dpy ),
		(char *) icon_bits,
		icon_width, icon_height,
		WhitePixel ( dpy, DefaultScreen ( dpy )),
		BlackPixel ( dpy, DefaultScreen ( dpy )),
		DefaultDepth ( dpy, DefaultScreen ( dpy )));
}

/*
**	Function name : CreateWindow
**
**	Description : Fabrication d'une fenetre d'edition.
**	Input : 
**	Ouput : La structure associee.
*/
EdWin *CreateWindow ()
{
    EdWin *edtmp;
    Window root;
    int screen;
    char *malloc ();
    XYMenu *MakeMenus (); 
    Text *MakeTextWindow ();
    MWin *MakeControlPanel ();
    static Atom wm_delete_window;
    int win_id;
    
    screen = DefaultScreen ( dpy );
    root = RootWindow ( dpy, screen );
    
    edtmp = ( EdWin * ) malloc ( sizeof ( EdWin ));
    
    if ( (win_id = add_window ( edtmp )) < 0 ) {
	if ( edtmp != 0 )
	  (void) free ( (char *) edtmp );
	return 0;
    }
    
    edtmp -> w_frame = XCreateSimpleWindow (dpy, root,
					    GetOpGeo ( OP_X ), GetOpGeo ( OP_Y ),
					    GetOpGeo ( OP_WIDTH ), GetOpGeo( OP_HEIGHT ), GetOpBW (),
					    GetOpBD (), GetOpColor ( OP_MENU_BG ));
    
    (void) SetProp ( edtmp -> w_frame ); 
    
    XSelectInput ( dpy, edtmp->w_frame , ExposureMask |
		  ButtonPressMask | ButtonReleaseMask | EnterWindowMask
		  | LeaveWindowMask | KeyPressMask | StructureNotifyMask );
    
    edtmp -> menu = MakeMenus ( dpy, edtmp->w_frame, 
			       menu_names, item_names, func_names );
    
    edtmp -> mwin = MakeControlPanel ( edtmp->w_frame );
    
    edtmp -> scroll = MakeScroll ( dpy, edtmp->w_frame,
				  GetOpGeo ( OP_WIDTH ) - GetScrollWidth () - W_SPACE,
				  HeightMenuBar ( edtmp -> menu -> font ) + 1 + W_SPACE );
    
    SetScrollFont ( edtmp -> scroll, GetOpFont ( OP_TEXT_FONT) );
    
    edtmp -> text = MakeTextWindow ( dpy, edtmp -> w_frame, 
				    W_SPACE, HeightMenuBar ( edtmp -> menu -> font ) + 1 + W_SPACE );
    edtmp -> text -> win_id = win_id;
    
    edtmp -> buf = GetBuffer ( (unsigned) SIZEOF_BUFFER );
    
    edtmp -> text -> buf = edtmp -> buf;
    edtmp -> text -> swin = edtmp -> scroll;
    edtmp -> text -> mwin = edtmp -> mwin;
    edtmp -> scroll -> text = ( char * ) edtmp -> text;
    
    SetHiddenWindow ( edtmp -> menu, edtmp -> text -> window, edtmp -> text -> Cgc );
    
    if ( XSaveContext ( dpy, edtmp->w_frame, EdContext, (caddr_t) edtmp ) != 0 )
      ( void ) printf ("create_env XSaveContext Error\n" );
    
    edtmp -> stat = (ST *) &st_initial;
    edtmp -> width = edtmp -> height = 0;
    (void) strcpy ( edtmp -> text -> filename, "NoName" ); 
    
    wm_delete_window = XInternAtom( dpy, "WM_DELETE_WINDOW", False);
    (void) XSetWMProtocols ( dpy, edtmp -> w_frame, &wm_delete_window, 1);
    
    return edtmp;
}

/*
**	Function name : ConfigWindow
**
**	Description : Positionne la geometrie des fenetres.
**	Input : Le contexte d'edition, largeur er hauteur.
**	Ouput :
*/
void ConfigWindow ( e, width, height )
    EdWin   *e;
    int width, height;
{
    int hbar, hmess, wscroll, i, x;
    
    e -> width = width;
    e -> height = height;
    hbar = HeightMenuBar ( e -> menu -> font );
    hmess = HeightOfMess ();
    wscroll = GetScrollWidth ();
    
#ifdef DEBUG
    fprintf ( stderr, "ConfigWindow width = %d, height = %d\n",
	     width, height );
    fprintf ( stderr, "ConfigWindow hbar = %d, hmess = %d, wscroll = %d\n",
	     hbar, hmess, wscroll );
#endif
    
    x = height - ( hbar + hmess + 2 ) - ( 2 * W_SPACE );
    x -= ( 2 * e -> text -> y_or );
    i = x / e -> text -> font_height;
    
    SetScrollLinePage ( e -> scroll, i );
    SetMenuBarWidth ( e -> menu, width );
    ShowControlPanel ( e -> mwin, width, height );
    ShowWindowText ( dpy, e -> text, width - wscroll - 1 - ( 2 * W_SPACE ) ,
		    height - hbar - hmess - 2 - ( 2 * W_SPACE) );
    
    if ( DoesSaveUnders ( DefaultScreenOfDisplay ( dpy ) ) != True )
      SetMenuPixmap ( dpy, e -> menu, width - wscroll - 1 );
    
    i = e -> text -> no_current_line - e -> text -> n1 - 1;
#ifdef DEBUG
    fprintf ( stderr, "no_current = %d n1 = %d\n", 
	     e -> text -> no_current_line, e -> text -> n1 );
#endif
    RefreshScroll ( dpy, e -> scroll, width - W_SPACE,
		   height - hbar - hmess - 2 - ( 2 * W_SPACE) , i );
}


/*
**	Function name : SetProp
**
**	Description : Initialisation des proprietes pour une fenetre
**		d'edition.
**	Input : La fenetre
**	Ouput :
*/
static void SetProp ( w )
    Window 	w;
{
    XSizeHints sizehints;
    XWMHints wm_hints;
    
    wm_hints.icon_pixmap = text_icon;
    
    sizehints.flags = PMinSize | PMaxSize;
    
    sizehints.height = GetOpGeo ( OP_HEIGHT );
    sizehints.width = GetOpGeo ( OP_WIDTH );
    sizehints.x = GetOpGeo ( OP_X );
    sizehints.y = GetOpGeo ( OP_Y );
    
    if ( sizehints.x >= 0 && sizehints.y >= 0 ) 
      sizehints.flags |=  USPosition;
#ifdef DEBUG
    (void) fprintf ( stderr, "x = %d	y = %d\n", sizehints.x, sizehints.y );
#endif
    sizehints.min_width = (DisplayWidth ( dpy,DefaultScreen ( dpy )) *2 ) / 5; 
    sizehints.min_height = (DisplayHeight ( dpy,DefaultScreen ( dpy )) * 2 ) / 5; 
    sizehints.max_width = DisplayWidth (dpy,DefaultScreen (dpy));
    sizehints.max_height = DisplayHeight (dpy,DefaultScreen (dpy));
    
    wm_hints.flags = InputHint | StateHint | IconPixmapHint;
    wm_hints.input = True;
    wm_hints.initial_state = NormalState;
    
    XSetWMProperties ( dpy, w, 0, 0, 0, 0, &sizehints, &wm_hints, 0 );
}


/*
**	Function name : add_window
**
**	Description : Ajoute une fenetre d'edition dans
**		la table principale.
**	Input : Le contexte d'edition.
**	Ouput : 0 si OK -1 sinon.
*/
static int add_window ( e )
    EdWin *e;
{
    EdWin **t;
    int id = 0;
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) {
	    *t  = e;
	    break;
	}
	id++;
    }
    if ( t == TWin + MAXWIN )
      return (- 1);
    else
      return id;
}


/*
**	Function name : DisplayOpenFiles
**
**	Description : Affiche la liste des fichiers ouverts.
**	Input : Le text courant.
**	Ouput :
*/
void DisplayOpenFiles ( text )
    Text *text;
{
    EdWin **t;
    char *str, *tmp;
    int n;
    
    ClearListBox ();
    OpenFilesInListBox ();
    tmp = (char *) SelectFromListBox ( "Open Files" );
    if ( tmp != 0 ) {
	/* str [2] = 0; */
	str = strrchr ( tmp, '#' );
	str++;
	str [2] = 0;
	if ( (n = atoi (str)) == 0 ) {
	    DisplayMessage ( text -> mwin, "Abort" );
	    return;
	}
	t = TWin; 
	t += (n-1);
	XMapRaised ( dpy,  (*t) -> w_frame );
	if ( tmp != 0 )
	  (void) free ( tmp ); 
    }
    else
      DisplayMessage ( text -> mwin, "Abort" );
}

/*
**	Function name : IsAlreadyLoad
**
**	Description : Pour eviter les ouvertures multiples
**		de fichiers
**	Input : Le nom du fichier.
**	Ouput : Le nombre de buffer ouvert sur le fichier.
*/
int OldIsAlreadyLoad ( s, text )
    char *s;
    Text **text;
{
    EdWin **t;
    Text *tmp = 0;
    int n = 0;
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 )
	  continue;
	if ( strcmp ( s, (*t) -> text -> filename ) == 0 ) {
	    n++;
	    if ( ! tmp )
	      tmp = (*t) -> text;
	}
    }
    if ( tmp )
      *text = tmp;
    return n;
}
/*
**	Function name : IsAlreadyLoad
**
**	Description : Pour eviter les ouvertures multiples
**		de fichiers
**	Input : Le contexte d'origine.
**	Ouput : Le nombre de buffer ouvert sur le fichier.
*/
int IsAlreadyLoad ( filename, from_text, text )
    char *filename;
    Text *from_text;
    Text **text; /* Return */
{
    EdWin **t;
    int n = 0;
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 )
	  continue;
	if ( strcmp ( filename, (*t) -> text -> filename ) == 0 ) {
	    n++;
	    if ( from_text != (*t) -> text )
	      *text = (*t) -> text;
	}
    }
    return n;
}

/*
**	Function name : NewWindow
**
**	Description : Construction d'une fenetre d'edition en
**		tenant compte des proprietes de la fenetre parent.
**	Input : Le text courant.
**	Ouput :
*/
void NewWindow ( text )
    Text *text;
{
    EdWin *ew;
    ew = CreateWindow (); 
    
    if ( ew == 0 ) {
	(void) fprintf ( stderr, "Too many open window\n" );
	return;
    }
    if ( strcmp(text -> current_mode -> name, "Shell") == 0 ) {
	ew -> text -> current_mode = (Mode *) GetMode ("default");
	ew -> text -> mwin -> mode = (Mode *) GetMode ("default");
    }
    else {
	ew -> text -> current_mode = text -> current_mode;
	ew -> text -> mwin -> mode = text -> current_mode;
    }
    if ( ew -> text -> current_mode -> font )
      SetFontText ( dpy, ew -> text, ew -> text -> current_mode -> font );
    
/*    SetBrowserMode ( text -> current_mode ); */
    
    (void) strcpy ( ew -> text -> current_dir,
		   text -> current_dir );
}


/*
**	Function name : DeleteWindow
**
**	Description : Detruit une fenetre d'edition.
**	Input : Le text courant.
**	Ouput : 0 si OK -1 sinon.
*/
int DeleteWindow ( text )
    Text *text;
{
    EdWin	**t, *tmp;
    extern Window window_kill;
    Window the_frame;
    
    if ( GetModif ( text ) == True ) {
	if ( SaveCurrentBuffer ( text, F_MENU ) != True )
	  return -1;
    }
    if ( IsLastWindow ( 1 ) == True ) {
/*
   s = ( char * ) GetStringFromDB ( "Last window. Quit [y/n] : ", False );
	if ( (s == 0) || (strncmp(s,"y",1) == 0) ) {
	  exit(0);
	}
	else if ( (strncmp(s,"n",1 ) == 0) || (strncmp(s,&c,1)==0) ) {
	    DisplayMessage ( text -> mwin, "Abort" );
	    return -1;
	}
*/
      exit(0);
    }
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue;
	if ( text == ( *t ) -> text ) {
	    tmp = *t;
	    break;
	}
    }
    if ( tmp -> text -> shell_id != 0 )
      KillShell ( tmp -> text );

    the_frame = tmp -> w_frame;
    XUnmapWindow ( dpy, tmp -> w_frame ); 
    DeleteMenu ( dpy, tmp -> menu );
    DeleteUndo ( tmp -> text );
    DeleteBuffer ( tmp -> buf );
    DeleteColorList ( tmp -> text );
    DeleteText ( dpy, tmp -> text );
    DeleteControlPanel ( tmp -> mwin );
    DeleteScroll ( dpy, tmp -> scroll );
    DeleteMacro ( tmp -> text );
/*    XSync ( dpy, False ); */
    
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t == 0 ) continue;
	if ( *t == tmp ) { 
	  if ( tmp != 0 ) {
	    window_kill = tmp -> w_frame;
	    (void) free ( (char *) tmp );
	  }
	  *t = 0;
	  break;
	}
    }
    XDestroyWindow ( dpy, the_frame );
    return 0;
}

/*
**	Function name : IsLastWindow
**
**	Description : Comme son l'indique.
**	Input : Le nombre de fenetre.
**	Ouput : Vrai ou faux.
*/
int IsLastWindow ( nb )
    int nb;
{
    EdWin **t;
    int n;
    
    n = 0;
    for ( t = TWin; t < TWin + MAXWIN; t++ ) {
	if ( *t != 0 )
	  n++;
    }
    if ( n != 0 ) {
	if ( nb == n )
	  return True;
	else
	  return False;
    }
    else
      return True;
}


/*
**	Function name : Version
**
**	Description : Affiche la version courante de xcoral.
**	Input : Le text courant
**	Ouput :
*/
void Version ( text )
    Text *text;
{
    char tmp [64];
    
    (void) sprintf ( tmp, "Message : %s", CURRENT_VERSION );
    DisplayWMessage ( tmp, "Version", True );
}


/*
**	Function name : Help
**
**	Description : En attendant d'avoir un help serieux.
**	Input : Le text courant. 
**	Ouput :
*/
void Help ( text )
    Text *text;
{
    DisplayManBox ();
}





