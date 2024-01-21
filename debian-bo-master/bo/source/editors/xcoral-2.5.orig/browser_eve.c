/* ########################################################################

			     browser_eve.c

   File: browser_eve.c
   Path: /home/fournigault/c/X11/xcoral-2.31/browser_eve.c
   Description: 
   Created: Fri Jan 27 10:45:36 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:45:37 MET 1995
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
#include <X11/cursorfont.h>
#if defined (apollo) || defined (__FreeBSD__)
#include <stdlib.h>
#else
#include <malloc.h>
#endif
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "options.h"
#include "xcoral.h"
#include "browser_init.h"
#include "browser_eve.h"
#include "shadow.h"
#include "text_cursor.h"
#include "bm_search.h"
#include "page.h"
#include "get_file.h"
#include "dial_box.h"
#include "fs_box.h"
#include "chars_cmds.h"
#include "main_events.h"
#include "new_window.h"

extern Browser br;
extern char *b_name[];
int flag_visit = False;

FCT (static void, AddFiles, () );
FCT (static void, ButtonEdit, () );
FCT (static void, ButtonInVisitWindow, (BWin *bwin, XButtonEvent *ev) );
FCT (static void, ClassSelect, (BWin *bwin) );
FCT (static void, EditFile, (BWin *bwin, char *name) );
FCT (static void, FileSelect, (BWin *bwin) );
FCT (static void, ManageFiles, (int op) );
FCT (static void, MethodSelect, (BWin *bwin) );
FCT (static void, ParentChildSelect, (BWin *bwin) );
FCT (static void, ProcSelect, (BWin *bwin) );
FCT (static void, RefreshBrowserWindow, (BWin *bwin) );
FCT (static void, RemoveFiles, (char *name) );
FCT (static void, SelectItem, (BWin *bwin, int x, int y, int i) );
FCT (static void, TitleVisit, (char *name) );
FCT (static void, UpdateTitle, (char *name) );

/*
**	Function name : ExposeBrowser
**
**	Description : Traitement d'un 'expose event'
**	Input : L'event.
**	Ouput :
*/
int ExposeBrowser ( ev )
    XEvent *ev;
{
    int i;
    int width, expose_flag;
    
    if ( ev -> xexpose.window == br.title ) {
	if ( br.title_name != 0 )
	  UpdateTitle ( br.title_name );
	else 
	  Display3D ( dpy, br.title, br.ts, br.bs, 1, 0 );	
	return True;
    }
    
    if ( ev -> xexpose.window == br.main ) {
	Display3D ( dpy, ev -> xexpose.window, br.ts, br.bs, 1, 0 );
	return True;
    }
    if ( ev -> xexpose.window == br.rec ) {
	XClearWindow ( dpy, br.rec );
	Display3D ( dpy, ev -> xexpose.window, br.ts, br.bs, 1, 0 );
	if ( br.parse_flag == RECURSIVE )
	  XDrawString ( dpy, ev -> xexpose.window, br.gc,
		       ( br.button_width - XTextWidth ( br.font, "Rec", 3 )) / 2,
		       br.font -> ascent + (B_INTER / 2), "Rec", 3 );
	else 
	  XDrawString ( dpy, ev -> xexpose.window, br.gc, 
		       ( br.button_width - XTextWidth ( br.font, "Dir", 3 )) / 2,
		       br.font -> ascent + (B_INTER / 2), "Dir", 3 );
	return True;
    }
    if ( ev -> xexpose.window == br.dec ) {
	XClearWindow ( dpy, br.dec );
	Display3D ( dpy, ev -> xexpose.window, br.ts, br.bs, 1, 0 );
	if ( br.dec_imp_flag == DEC_MODE )
	  XDrawString ( dpy, ev -> xexpose.window, br.gc,
		       ( br.button_width - XTextWidth ( br.font, "Decl", 4 )) / 2,
		       br.font -> ascent + (B_INTER / 2), "Decl", 4 );
	else 
	  XDrawString ( dpy, ev -> xexpose.window, br.gc, 
		       ( br.button_width - XTextWidth ( br.font, "Impl", 4 )) / 2,
		       br.font -> ascent + (B_INTER / 2), "Impl", 4 );
	return True;
    }
    if ( ev -> xexpose.window == br.close ) {
	Display3D ( dpy, ev -> xexpose.window, br.ts, br.bs, 1, 0 ); 
	XDrawString ( dpy, ev -> xexpose.window, br.gc, 
		     ( br.button_width - XTextWidth ( br.font, "Close", 5 )) / 2,
		     br.font -> ascent + (B_INTER / 2), "Close", 5 );
	return True;
    }
    if ( ev -> xexpose.window == br.add ) {
	Display3D ( dpy, ev -> xexpose.window, br.ts, br.bs, 1, 0 ); 
	XDrawString ( dpy, ev -> xexpose.window, br.gc, 
		     ( br.button_width - XTextWidth ( br.font, "Add", 3 )) / 2,
		     br.font -> ascent + (B_INTER / 2), "Add", 3 );
	return True;
    }
    if ( ev -> xexpose.window == br.del ) {
	Display3D ( dpy, ev -> xexpose.window, br.ts, br.bs, 1, 0 );
	XDrawString ( dpy, ev -> xexpose.window, br.gc, 
		     ( br.button_width - XTextWidth ( br.font, "Del", 3 )) / 2,
		     br.font -> ascent + (B_INTER / 2), "Del", 3 );
	return True;
    }
    if ( ev -> xexpose.window == br.edit ) {
	Display3D ( dpy, ev -> xexpose.window, br.ts, br.bs, 1, 0 ); 
	XDrawString ( dpy, ev -> xexpose.window, br.gc,
		     ( br.button_width - XTextWidth ( br.font, "Edit", 4 )) / 2,
		     br.font -> ascent + (B_INTER / 2), "Edit", 4 );
	return True;
    }
    
    expose_flag = False;
    for ( i = 0; i < 7; i++ ) {
	if ( ev -> xexpose.window == br.tbw[i].frame ) {
	    Display3D ( dpy, ev -> xexpose.window, br.ts, br.bs, 1, 0 );
	    expose_flag = True;
	    break;
	} 
	if ( ev -> xexpose.window == br.tbw[i].title) {
	    Display3D ( dpy, ev -> xexpose.window, br.ts, br.bs, 1, 0 );
	    if ( i == W_VISIT ) {
		TitleVisit ( GetFileName ( br.tbw[W_VISIT].text )); 
	    }
	    else {
		width = XTextWidth ( br.font, b_name [i], strlen (b_name [i]));
		XDrawString ( dpy, ev -> xexpose.window, br.gc, 
			     (br.tbw[i].f_width - width)/2, br.font -> ascent + (B_INTER / 2),            
			     b_name [i], strlen (b_name [i]));                                                            
	    }
	    expose_flag = True;
	    break;
	} 
	if ( ev -> xexpose.window == br.tbw[i].text -> window ) {
	    RefreshBrowserWindow ( &br.tbw[i] ) ;
	    if ( i == W_VISIT ) {
		TextCursorOn ( br.tbw[W_VISIT].text );
	    }
	    expose_flag = True;
	    break;
	} 
	if ( ev -> xexpose.window == br.tbw[i].scroll -> frame ) {
	    RefreshScrollFrame ( dpy,  br.tbw[i].scroll );
	    expose_flag = True;
	    break;
	} 
	if ( ev -> xexpose.window == br.tbw[i].scroll -> scroll ) {
	    RefreshScrollBar ( dpy, br.tbw[i].scroll );
	    expose_flag = True;
	    break;
	} 
    }
    while ( XCheckWindowEvent ( dpy, ev -> xexpose.window, ExposureMask, ev ));
    
    return (expose_flag);
}	

/*
**	Function name : RefreshBrowserWindow
**
**	Description : Comme son nom l'indique.
**	Input : Le contexte
**	Ouput :
*/
static void RefreshBrowserWindow ( bwin )
    BWin *bwin;
{
    XClearWindow ( dpy, bwin -> text -> window );
    Display3D ( dpy, bwin -> text -> window, br.ts, br.bs, 2, 1 ); 
    ClipOn ( bwin -> text, 0 );
    SetLinesTable ( bwin -> text );
    RefreshPage ( bwin -> text );
    ClipOff ( bwin -> text );
    UpdateTextItem ( bwin -> text, bwin -> select );
    XFlush ( dpy );
}


/*
**	Function name : ButtonBrowser
**
**	Description : Traitement d'un 'button press event'
**	Input : L'event.
**	Ouput : Vrai si le button press concerne le browser.
*/
int ButtonBrowser ( ev )
    XButtonEvent *ev;
{
    Window w = ev -> window;
    int i;
    int result;
    
    br.click_time = ev -> time;
    if ( DoubleClick (br.click_time, &br.old_click) == True ) {
	ButtonEdit ();
	return True;
    }
    if ( (w == br.close) 
	||  (w == br.rec) 
	||  (w == br.add) 
	|| ( w == br.del) 
	|| ( w == br.dec) 	    	    
	|| ( w == br.edit ) ) {
	Display3D ( dpy, w, br.ts, br.bs, 1, 1 );
	XFlush ( dpy );
	SmallTime ( (long) 200000 );
	Display3D ( dpy, w, br.ts, br.bs, 1, 0 );
	if ( w == br.rec ) {
	    br.parse_flag = ! br.parse_flag;
	    XClearWindow ( dpy, w );
	    if ( br.parse_flag == RECURSIVE )
	      XDrawString ( dpy, w,br.gc, 
			   (br.button_width - XTextWidth ( br.font, "Rec", 3 )) / 2,
			   br.font -> ascent + (B_INTER / 2), "Rec", 3 );
	    else 
	      XDrawString ( dpy, w, br.gc, 
			   (br.button_width - XTextWidth ( br.font, "Dir", 3 )) / 2,
			   br.font -> ascent + (B_INTER / 2), "Dir", 3 );
	    Display3D ( dpy, w, br.ts, br.bs, 1, 0 );
	}
	if ( w == br.dec ) {
	    br.dec_imp_flag = ! br.dec_imp_flag;
	    XClearWindow ( dpy, w );
	    if ( br.dec_imp_flag == DEC_MODE )
	      XDrawString ( dpy, w,br.gc, 
			   (br.button_width - XTextWidth ( br.font, "Decl", 4 )) / 2,
			   br.font -> ascent + (B_INTER / 2), "Decl", 4 );
	    else 
	      XDrawString ( dpy, w, br.gc, 
			   (br.button_width - XTextWidth ( br.font, "Impl", 4 )) / 2,
			   br.font -> ascent + (B_INTER / 2), "Impl", 4 );
	    Display3D ( dpy, w, br.ts, br.bs, 1, 0 );
	    if ( br.tbw[W_METHOD].select != 0 )
	      MethodSelect ( &br.tbw[W_METHOD] );
	}
	else if ( w == br.edit )
	  ButtonEdit ();
	else if ( w == br.add )
	  ManageFiles ( B_ADD );
	else if ( w == br.del )
	  ManageFiles ( B_DEL );
	else if ( w == br.close )
	  UnmapBrowser ();
	return True;
    }
    for ( i = 0; i< 7; i++ ) {
	if ( w == br.tbw[i].text -> window ) {
	    if ( i != W_VISIT ) {
		SelectItem ( &br.tbw[i], ev -> x, ev -> y, i );
	    }
	    else
	      ButtonInVisitWindow ( &br.tbw[W_VISIT], ev );
#ifdef DEBUG
	    (void) fprintf ( stderr, "Button BrowserText\n" );
#endif
	    
	    return True;
	}
	if ( ButtonPressInScroll ( br.tbw[i].scroll, ev -> window, ev -> y, &result )) {
	    switch ( result ) {
	    case CURSOR:
	      UpdateTextItem ( br.tbw[i].text, br.tbw[i].select );
	      TextCursorOn ( br.tbw[i].text ); 
	      TextCursorOff ( br.tbw[i].text );
#ifdef DEBUG
	      fprintf ( stderr, "Button BrowserScroll\n" );
#endif
	      if ( (TextInBuf ( br.tbw[i].text ) == True) 
		  && ( br.tbw[i].text -> lines_in_buf > 1 )) {
		  HandleScrollBar ( dpy, br.tbw[i].scroll,  ScrollNLine );
		  RefreshScrollBar ( dpy, br.tbw[i].scroll ); 
		  
	      }
	      if ( i != W_VISIT ) 
		UpdateTextItem ( br.tbw[i].text, br.tbw[i].select );
	      else 
		TextCursorOn ( br.tbw[i].text );
	      break;
	    case NEXT:
	      UpdateTextItem ( br.tbw[i].text, br.tbw[i].select );
	      TextCursorOff ( br.tbw[i].text );
	      NextPage ( br.tbw[i].text );
	      TextCursorOff ( br.tbw[i].text );
	      UpdateTextItem ( br.tbw[i].text, br.tbw[i].select );
	      break;
	    case PREVIOUS:
	      UpdateTextItem ( br.tbw[i].text, br.tbw[i].select );
	      TextCursorOff ( br.tbw[i].text );
	      PreviousPage ( br.tbw[i].text );
	      TextCursorOff ( br.tbw[i].text );
	      UpdateTextItem ( br.tbw[i].text, br.tbw[i].select );
	      break;
	    }
	    return True;
	}
    }
    return False;
}


/*
**	Function name : ManageFiles
**
**	Description : Modification de de la base.
**		Ajoute un fichier/directorie ou
**		retire un fichier.
**
**	Input : Le type d'operation a effectuer.
**	Ouput :
*/
static void ManageFiles ( op )
    int op;
{
    int len;
    
    if ( op == B_DEL ) {
	char *del_filename, *tmp, *buf;
	if ( br.tbw[W_FILES].select != 0 )  {
	    HoleToLeft ( br.filebuf );
	    if ( (br.tbw[W_FILES].select - 1) != 0)
	      (void) MoveToLine ( br.filebuf, br.tbw[W_FILES].select - 1 );
	    tmp = GetCurrentLine ( br.filebuf, &len  );
	    *(tmp + len ) = '\0'; /* On vire le newline */
	    del_filename =  (char *) malloc ( (unsigned) len + 2 );
	    (void) strcpy ( del_filename, tmp );
	    buf = (char *) malloc ( (unsigned) len + 32 );
	    (void) sprintf ( buf, "Remove file selected [y/n/all] : "  );
	    tmp = (char *) GetStringFromDB ( buf, False );
	    if ( (tmp == 0) || (strncmp(tmp, "y", 1) == 0 )) 
	      RemoveFiles ( del_filename );
	    else if ( strcmp ( tmp, "all" ) == 0 ) 
	      RemoveFiles ( (char *) 0 );
	    if ( del_filename != 0 )
	      (void) free ( del_filename );
	    if ( buf != 0 )
	      (void) free ( buf );
	}
	else {
	    tmp = (char *) GetStringFromDB ( "Delete all files [y/n] : ", True );
	    if ( (tmp == 0) || (strncmp(tmp, "y", 1) == 0 )
		|| (strncmp(tmp, "all", 3)) == 0 )
	      RemoveFiles ( (char *) 0 );
	}
    }
    else 
      AddFiles ();
}

/*
**	Function name : AddFiles
**
**	Description : Ajoute un fichier ou une directorie.
**	Input : 
**	Ouput :
*/
static void AddFiles ()
{
    char *s;
    struct stat st;
    
    (void) chdir ( br.dir );
    
    s = (char *) SelectFileFromBox ( "Browser : Add Files" );
    
    if ( (s != 0) && (strlen (s) != 0 ) && (*s != '\007') ) {
	(void) stat ( s, &st );
	if ( S_ISDIR(st.st_mode) != 0 ) {
	    XDefineCursor ( dpy, br.frame, 
			   XCreateFontCursor ( dpy, XC_watch ) );
	    XFlush (dpy ); 
#ifdef DEBUG
	    (void) fprintf ( stderr, "dir : %s\n", s );
#endif
	    LoadDir ( s );
	    XUndefineCursor ( dpy, br.frame );
	    SetBrowserDir ( s );
	}
	else if ( S_ISREG(st.st_mode) != 0 ) {
	    if ( GoodSuffix ( s ) == True )
	      parse_file ( s ); 
	}
	(void) RefreshBrowserInfos ();
    }
}

/*
**	Function name : RemoveFiles
**
**	Description : Retire un ficher ou tous les fichiers
**		de la base.
**	Input : Le nom du fichiers.
**	Ouput :
*/
static void RemoveFiles ( name )
    char *name;
{
    char *tmp, *p;
    int i, nbfiles;
    int len;
    
    if ( name != 0 )
      delete_file ( name );
    else {
	HoleToLeft ( br.filebuf );	/* Debut du buffer */
	nbfiles = GetNumberOfLineInBuf ( br.filebuf ); /* Nb fichiers */
	nbfiles--;
	
	for ( i=0; i < nbfiles; i++ ) {
	    p = (char *) GetCurrentLine ( br.filebuf, &len );
	    tmp = (char *) malloc ( (unsigned) (len + 1) );
	    (void) strncpy ( (char *) tmp, p, len );
	    tmp [len] = '\0';
	    (void) MoveToLine ( br.filebuf, 1 );
	    delete_file ( tmp );
	    if ( tmp != 0 )
	      (void) free ( tmp );
	}
    }
    
    /* Refresh windows */
    (void) RefreshBrowserInfos ();
    if ( name == 0) { /* Clear Browser */
	KillText ( dpy, br.tbw[W_VISIT].text );
	ConfigScroll ( &br.tbw[W_VISIT] );
	TitleVisit ( " " );
	UpdateTitle ( " " );
    }
    else { /* Un fichier seulement */
	if  ( strcmp ( br.title_name, name ) == 0 ) {
	    KillText ( dpy, br.tbw[W_VISIT].text );	
	    ConfigScroll ( &br.tbw[W_VISIT] );
	    TitleVisit ( " " );
	}
    }
}

/*
**	Function name : GoodSuffix
**
**	Description : Check filename.
**
**	Input : Le nom du fichier.
**	Ouput : Vrai si .c .h .cc .cxx .C. Faux sinon
*/
int GoodSuffix ( s )
    char *s;
{
    char *end_name;
    
    end_name = (char * ) strrchr ( (char *) s, '.' );
    if ( end_name == 0 )
      return False;
    
    if ( br.mode -> suffixes != 0 ) {
	if ( BmSearch ( br.mode -> suffixes, end_name,
		       strlen (br.mode -> suffixes),1 ) != -1 )
	  return True;
	else 
	  return False;
    }
    
    if ( (strcmp( end_name, ".c") == 0) 
	|| ( strcmp( end_name, ".h") == 0 )
	|| ( strcmp( end_name, ".cc") == 0 )
	|| ( strcmp( end_name, ".cxx") == 0 )
	|| ( strcmp( end_name, ".hxx") == 0 )
	|| ( strcmp( end_name, ".C") == 0 )) 
      return True;
    else
      return False;
}


/*
**	Function name : RefreshBrowserInfos
**
**	Description : Prepare la mise a jour des infos.
**	Input : 
**	Ouput :
*/
void RefreshBrowserInfos ()
{
    int i;
    XEvent event;
    
    SetInfos (); 	/* Classes, Procs, Files */
    br.tbw[W_CLASS].select = br.tbw[W_PROC].select = br.tbw[W_FILES].select = 0;
    br.tbw[W_PARENT].select = br.tbw[W_CHILD].select = br.tbw[W_METHOD].select = 0;
    
    if ( strcmp ( br.mode -> name, "C++mode" ) == 0 ) {
	i = 0;
    }
    else if ( strcmp ( br.mode -> name, "C-mode" ) == 0 ) {
	i = 4;
    }
    else
      return;
    
    if ( br.stat == B_UNMAP )
      return;
    
    for ( ;i < 6; i++ ) {
	while ( XCheckWindowEvent ( dpy,
				   br.tbw[i].text -> window , ExposureMask, &event ));
	switch ( i ) {
	case W_FILES:
	case W_PROC:
	case W_CLASS:
	  RefreshBrowserWindow ( &br.tbw[i] );
	  FirstPage ( br.tbw[i].text );
	  break;
	case W_PARENT:
	case W_CHILD:
	case W_METHOD:
	  KillText ( dpy, br.tbw[i].text ); 
	  break;
	default:
	  break;
	}
	ConfigScroll ( &br.tbw[i] );			
    }
}

/*
**	Function name : ButtonEdit
**
**	Description : Creation d'une fenetre d'edition pour
**		l'objet courant.
**	Input : 
**	Ouput :
*/
static void ButtonEdit ()
{
    EdWin *ew;
    char *filename;
    extern EdWin *CreateWindow ();
    Text *text;
    int line;
    
    filename = GetFileName ( br.tbw[W_VISIT].text ); 
    if ( IsAlreadyLoad ( filename, (Text *) 0,  &text ) ) {
	XMapRaised ( dpy, text -> w_parent );      
	GotoLineNumber ( text, br.tbw[W_VISIT].text -> no_current_line );
	CurrentLineToTop ( text );      
	return;
    }
    
    ew = (EdWin * ) CreateWindow ();
    if ( ew == 0 ) {
	( void ) fprintf ( stderr, "Create_env error\n" );
	return;
    }
    ew -> text -> current_mode = br.mode;
    ew -> text -> mwin -> mode = br.mode;
    
    if ( (filename != 0) && (strcmp(filename, "NoName")!=0) ) {
	(void) strcpy ( ew -> text -> current_dir, br.dir );
	line = br.tbw[W_VISIT].text -> no_current_line;
	if ( LoadFile ( ew -> text, filename, NEW ) != -1 )
	  GotoLineNumber ( ew ->text, line );
	else 
	  return;
    }
    else {
	(void) strcpy ( ew -> text -> current_dir, br.dir );
    }
    XStoreName ( dpy, ew -> w_frame, ew -> text -> filename );
    XMapWindow ( dpy, ew->w_frame );
    XFlush ( dpy );
}

/*
**	Function name : SelectItem
**
**	Description : Un item a ete selectionne dans une
**		des fenetres de texte.
**
**	Input : Le contexte, la position, le type.
**	Ouput :
*/
static void SelectItem ( bwin, x, y, i )
    BWin *bwin;
    int x, y;
    int i;
{
    int n;
    int scroll = bwin -> text -> sl;
    
    if ( ( bwin -> text -> lines_in_buf == 1 ) 
	|| (MoveToXYinTextWindow ( bwin -> text,  x, y ) == -1 ))
      return;
    
    TextCursorOn ( bwin -> text );
    TextCursorOff ( bwin -> text );
    
    if ( (n = bwin -> text -> no_current_line) >= bwin -> text -> lines_in_buf ) {
	return;
    }
    
    if ( bwin -> text -> sl == scroll ) {
	UpdateTextItem ( bwin -> text, bwin -> select );
    }
    UpdateTextItem ( bwin -> text, n );
    bwin -> select = n;
    
    switch ( i ) {
    case W_CLASS:
      ClassSelect ( bwin );
      break;
    case W_PARENT:
    case W_CHILD:
      ParentChildSelect ( &br.tbw[i] );
      break;
    case W_METHOD:
      MethodSelect ( bwin );
      break;
    case W_PROC:
      ProcSelect ( bwin );
      break;
    case W_FILES:
      FileSelect ( bwin );
      break;
    default:
      break;
    }
    br.old_click = br.click_time;
    if ( br.visit_raise == True )
      XMapRaised ( dpy, br.tbw[W_VISIT].frame );
}

/*
**	Function name : ClassSelect
**
**	Description : Selection d'une classe
**	Input : Le contexte
**	Ouput :
*/
static void ClassSelect ( bwin )
    BWin *bwin;
{
    char  *class_name;
    char **t_class;
    int len;
    Position *infos;
    char **tmp;
    
    class_name = (char *) GetCurrentLine ( br.tbw[W_CLASS].buf, &len);
    class_name [len] = '\0';
    UpdateTitle ( class_name );
    infos = get_class_decl ( REMOVE_CLASS_INFO (class_name) );
    
    if ( infos == 0 ) return;
    
/*    ClearBuffer ( br.tbw[W_VISIT].buf ); */
    EditFile ( &br.tbw[W_VISIT], infos -> file_name );
    AtLineDisplayPage ( br.tbw[W_VISIT].text, infos -> line_number );
    if ( infos != 0 )
      (void) free ( (char *) infos );
    
    t_class = br.class_save + (bwin -> select -1);
    
    if ( t_class != 0 ) {
	if ( br.methods_save != 0 )
	  (void) free ( (char *) br.methods_save );
/*	(void) RefreshBrowserInfos (); */ /* Fixed by mtg@csd.uch.gr */
	br.methods_save = ( char **)  get_methods_list ( *t_class);
	if (br.methods_save == 0)
	  KillText(dpy, br.tbw[W_METHOD].text);
	else
	  ExtractInfos ( br.methods_save, W_METHOD );
	ConfigScroll ( &br.tbw[W_METHOD] ); 
	br.tbw[W_METHOD].select = 0;
	ClearPage ( br.tbw[W_METHOD].text );
	FirstPage ( br.tbw[W_METHOD].text ); 
	
	tmp = ( char **)  get_parents_list ( *t_class);
	ExtractInfos ( tmp, W_PARENT );
	if ( tmp != 0 )
	  free ( (char *) tmp );
	ConfigScroll ( &br.tbw[W_PARENT] ); 
	br.tbw[W_PARENT].select = 0;
	ClearPage ( br.tbw[W_PARENT].text );
	FirstPage ( br.tbw[W_PARENT].text ); 
	
	FirstPage ( br.tbw[W_CHILD].text );
	tmp = ( char **)  get_sons_list ( *t_class);
	ExtractInfos ( tmp, W_CHILD );
	if ( tmp != 0 )
	  free ( (char *) tmp );
	ConfigScroll ( &br.tbw[W_CHILD] );
	br.tbw[W_CHILD].select = 0;
	ClearPage ( br.tbw[W_CHILD].text );
	FirstPage ( br.tbw[W_CHILD].text ); 
    }
}

/*
**	Function name : UpdateTitle
**
**	Description : Mise a jour du titre principale.
**
**	Input : Le titre
**	Ouput :
*/
static void UpdateTitle ( name )
    char *name;
{
    if ( name == 0 )
      return;
    XClearWindow ( dpy, br.title );
    XDrawString ( dpy, br.title, br.gc,
		 B_INTER, br.font -> ascent + (B_INTER/2), name, strlen ( name ));
    Display3D ( dpy,br.title , br.ts, br.bs, 1, 0 );
    
    if ( br.title_name != 0 ) {
	if ( strcmp ( br.title_name, name ) == 0 )
	  return;
	if ( br.title_name != 0 )
	  (void) free ( br.title_name );
    }
    br.title_name = (char *) malloc ( (unsigned int) strlen (name) +1 );
    (void) strcpy ( br.title_name, name );
}

/*
**	Function name : ProcSelect
**
**	Description : Selection d'une fonction
**	Input : Le contexte.
**	Ouput :
*/
static void ProcSelect ( bwin )
    BWin *bwin;
{
    char  *proc_name;
    int len;
    Position *infos;
    
    proc_name = (char *) GetCurrentLine ( bwin -> buf, &len);
    proc_name [len] = '\0';
    UpdateTitle ( proc_name );
    infos = get_proc_impl ( proc_name );
    
    if ( infos != 0 ) {
/*	ClearBuffer ( br.tbw[W_VISIT].buf ); */
	EditFile ( &br.tbw[W_VISIT], infos -> file_name );
	AtLineDisplayPage ( br.tbw[W_VISIT].text, infos -> line_number );
    }
    if ( infos != 0 )
      (void) free ( (char *) infos );
}

/*
**	Function name : FileSelect
**
**	Description : Selection d'un fichier
**	Input : Le contexte.
**	Ouput :
*/
static void FileSelect ( bwin )
    BWin *bwin;
{
    char *name, **t_name;
    int len;
    
    HoleToLeft ( br.filebuf );
    
    if ( (bwin -> select - 1) != 0) 
      (void) MoveToLine ( br.filebuf, bwin -> select - 1 );
    
    name = GetCurrentLine ( br.filebuf, &len);
    
    name [len] = '\0';
    UpdateTitle ( name );
    t_name = br.files_save;
    
    while ( t_name != 0 ) {
	if ( (*t_name != 0) && (strcmp(name, *t_name) == 0) ) 
	  break;
	t_name ++;
    }
    
    if ( name != 0 ) {
/*	ClearBuffer ( br.tbw[W_VISIT].buf ); */
	EditFile ( &br.tbw[W_VISIT], *t_name );
	FirstPage ( br.tbw[W_VISIT].text );
	TextCursorOn ( br.tbw[W_VISIT].text );
    }
}

/*
**	Function name : SelectMethod
**
**	Description : Selection d'une methode.
**	Input : Le contexte.
**	Ouput :
*/
static void MethodSelect ( bwin )
    BWin *bwin;
{
    char  *method_name;
    char **t_methods;
    int len;
    Position *infos;
    char *buf;
    
    method_name = (char *) GetCurrentLine ( br.tbw[W_METHOD].buf, &len);
    method_name [len] = '\0';
    t_methods = br.methods_save + ( bwin -> select - 1 );
#ifndef lint
    buf = (char *) malloc ( (unsigned) (strlen (GET_METHOD_CLASS(*t_methods))
					+ strlen ( *t_methods ) + 4) );
    (void) strcpy ( buf, GET_METHOD_CLASS(*t_methods) );
#else 
    buf = 0;
#endif
    (void) strcat ( buf, "::" );
    (void) strcat ( buf, *t_methods );
    UpdateTitle ( buf );
    if ( buf != 0 )
      (void) free ( buf );
    
    if ( br.dec_imp_flag == DEC_MODE ) 
#ifndef lint
      infos =  get_method_decl ( GET_METHOD_CLASS(*t_methods), *t_methods );
    else
      infos =  get_method_impl ( GET_METHOD_CLASS(*t_methods), *t_methods );
#else
    infos = (Position*) t_methods;	/* affecte infos, et utilise t_methods */
#endif
    if ( infos != 0 ) {
/*	ClearBuffer ( br.tbw[W_VISIT].buf ); */
	EditFile ( &br.tbw[W_VISIT], infos -> file_name );
	AtLineDisplayPage ( br.tbw[W_VISIT].text, infos -> line_number );
    }
    if ( infos != 0 )
      (void) free ( (char *) infos );
}

/*
**	Function name : PrentChildSelect
**
**	Description : Selection d'un parent/child
**	Input : Le contexte.
**	Ouput :
*/
static void ParentChildSelect ( bwin )
    BWin *bwin;
{
    char *class_name;
    char **t_class;
    int i = 1;
    int len;
    BWin *b_class = &br.tbw[W_CLASS];
    
    class_name = (char *) GetCurrentLine ( bwin -> buf, &len);
    class_name [len] = '\0';
    
    UpdateTitle ( class_name );
    class_name = REMOVE_CLASS_INFO(class_name);
    
    t_class = br.class_save;
    
    while ( t_class != 0 ) {
	if ( (*t_class == 0) || (strcmp(class_name, *t_class) == 0) ) 
	  break;
	t_class ++;
	i++;
    }
    if ( *t_class == 0 ) {
	UpdateTextItem ( br.tbw[W_PARENT].text, br.tbw[W_PARENT].select );
	UpdateTitle ( " " );
	br.tbw[W_PARENT].select = 0;
	return;
    }
    /* i = no ligne dans la liste des classes */
    UpdateTextItem ( b_class -> text, b_class -> select );
    GotoLineNumber ( b_class -> text, i );
    CurrentLineToTop ( b_class -> text );
    UpdateTextItem ( b_class -> text, b_class -> select );
    SelectItem ( b_class, 5, 5, W_CLASS );
}


/*
**	Function name : EditFile
**
**	Description : Charge un fichier dans la fenetre de visite.
**	Input : Le contexte, le nom du fichier.
**	Ouput :
*/
static void EditFile ( bwin, name )
    BWin *bwin;
    char *name;
{
    TextCursorOff ( bwin -> text );
    (void) strcpy ( bwin -> text -> current_dir, br.dir );
    flag_visit = True;

/*    if(name && (strcmp(name,bwin -> text -> filename)!=0)){ */
      ClearBuffer ( bwin -> buf );
      if ( LoadFile ( bwin ->text, name, NEW ) != -1 ) {
	ShowScrollFrame ( dpy, bwin ->text -> swin );
	FirstPage ( bwin ->text ); 
	TitleVisit ( name );
      }
      else {
	DisplayWMessage ("Load file failed", "Load file failed", 0);
      }
/*    } */
    flag_visit = False;
    TextCursorOn ( bwin ->text );
}


/*
**	Function name : TitleVisit
**
**	Description : Met a jour le titre de la fenetre de visit.
**	Input : Le nom a mettre.
**	Ouput :
*/
static void TitleVisit ( name )
    char *name;
{
    if ( (name != 0) && strlen (name) != 0 ) {
	XClearWindow ( dpy, br.tbw[W_VISIT].title );
	XDrawString ( dpy, br.tbw[W_VISIT].title, br.gc, B_INTER,
		     br.font -> ascent + (B_INTER/2), name, strlen ( name) );
	Display3D ( dpy,br.tbw[W_VISIT].title, br.ts, br.bs, 1, 0 );
    }
}    

/*
**	Function name : ButtonInVisitWindow
**
**	Description : Traitement d'un 'button press' dans
**		la fenetre de visit.
**	Input : Le contexte, l'event.
**	Ouput :
*/
static void ButtonInVisitWindow ( bwin, ev )
    BWin *bwin;
    XButtonEvent *ev;
{
    char *msg;
    
    switch ( ev -> button ) {
    case Button1:
      TextCursorOff ( bwin -> text );
      (void) MoveToXYinTextWindow ( bwin -> text, ev -> x, ev -> y );
      TextCursorOn ( bwin -> text );
      break;
    case Button2:
      msg = (char *) ie_call_function ( (Text *) bwin -> text, "color_buffer", 0, 0);
      if(msg)
	DisplayWMessage (msg, "Color buffer failed", 0);
      else {
	  ClipOn(bwin -> text, 0);
	  RefreshPage(bwin -> text);
	  ClipOff(bwin -> text);
      }
      break;
    case Button3:
      TextCursorOff ( bwin -> text );
      XSync ( dpy, False );
      StoreBytesInCutBuffer ( bwin -> text, ev -> x, ev -> y );
      TextCursorOn ( bwin -> text );
      break;
    default:
      break;
    }
}

/*
**	Function name : KeyPressInBrowser
**
**	Description : Comme son nom l'indique.
**	Input : L'event.
**	Ouput : Vrai si 'Key press' dans une des fenetres
**		du browser.
*/
static FCT(void, HandleKeyPressInBrowser,(XKeyEvent *ev, BWin * bw)	);

int KeyPressInBrowser ( ev )
    XKeyEvent *ev;
{
    int i;
    
    for ( i = 0; i < 7; i++ ) {
	if ( ev -> window == br.tbw [i].text -> window ) {
#ifdef DEBUG				
	    fprintf ( stderr, "Key Press In Browser text\n");
#endif
	    if (i != W_VISIT)
	      HandleKeyPressInBrowser(ev, &br.tbw [i]);
	    return True;
	}
    }
    if ( ev -> window == br.frame ) {
#ifdef DEBUG
	fprintf ( stderr, "Key Press In Browser frame\n");
#endif
	return True;
    }
    else
      return False;
}


static void gotonextchar(buf, p)
    Buf * buf;
    char ** p;
{
  if (++(*p) == buf->l_cur)
    *p = buf->r_cur + 1;
}

static void HandleKeyPressInBrowser(ev, bw)
    XKeyEvent *ev;
    BWin * bw;
{
  char code;
  
  {
    char buf[32];
    static KeySym ksym;
    static XComposeStatus compose;
    
    bzero(buf, 32);
    XLookupString ( ev, buf, 32, &ksym, &compose );
    code = buf[0];
  }
  
  if (((code >= 'a') && (code <= 'z')) ||
      ((code >= 'A') && (code <= 'Z')) ||
      (code == '_')) {
    int numlig;
    char *p1, *p2;
    Buf * buf = bw->buf;
    
    numlig = 1;
    p1 = buf->top;
    if (p1 == buf->l_cur) p1 = buf->r_cur + 1;
    p2 = buf->bottom;
    if (p2 == buf->r_cur) p2 = buf->l_cur - 1;
  
    while (p1 < p2) {
      if (*p1 == '[') {
	do gotonextchar(buf, &p1); while (*p1 != ']');
	do gotonextchar(buf, &p1); while (*p1 == ' ');
      }
      if (*p1 == '~')
	gotonextchar(buf, &p1);
      if (*p1 >= code)
	break;
      do gotonextchar(buf, &p1); while (*p1 != '\n');
      gotonextchar(buf, &p1);
      numlig += 1;
    }
    if ((p1 < p2) && (*p1 == code)) {
      if ( bw->select ) {
	UpdateTextItem ( bw->text, bw->select );
	bw->select = 0;
      }
      AtLineDisplayPage(bw->text, numlig);
      TextCursorOff ( bw->text );
    }
  }
}

