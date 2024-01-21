/* ########################################################################

			      chars_cmds.c

   File: chars_cmds.c
   Path: /home/fournigault/c/X11/xcoral-2.31/chars_cmds.c
   Description: 
   Created: Fri Jan 27 10:52:41 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:52:42 MET 1995
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
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <ctype.h>
#include <string.h>
#include <sys/time.h>
#include <signal.h>

#ifndef apollo
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif
#endif

#include "main_text.h"
#include "chars_cmds.h"
#include "text_cursor.h"
#include "mark_reg.h"
#include "page.h"
#include "kill_buf.h"
#include "list_box.h"
#include "ie_func.h"
#include "get_file.h"

extern Display *dpy;
static int toggle_del = 0;
extern char *ie_call_function();

FCT (static void, donothing, () );

/*
**	Function name : CheckModifAndFilename
**
**	Description : 
**	Input : 
**	Ouput :
*/
void CheckModifAndFilename ( text )
    Text *text;
{
  if ( (text -> modif == False) && (strcmp(text -> filename,"NoName")==0) ) {
    text -> modif = True; 
    if ( text -> swin == 0 )
      return;
    SetScrollLine ( text -> swin , 1 );
    ShowScrollFrame ( dpy, text -> swin );
  }
}

/*
**	Function name : f_impc
**
**	Description : Imprime un caractere ( position courante ). 
**	Input : Le caractere.
**	Ouput :
*/
void f_impc ( text, ch )
    Text *text;
    int ch;
{
  char c = (char) ch;
  
  TextCursorOff ( text );
  (void) UpdateMark ( text, 1, 0 );
  InsertNchar ( text -> buf, &c, 1 );
  
/*  CheckModifAndFilename ( text ); */
  
  /* Pour le Undo */
  if ( text -> u_todo == True )
    (void) StoreInUndo ( text, &c, (char *) 0, 1, 0, U_STD );

  /* Pour la couleur */
  if ( text -> current_ce != 0 )
    (void) UpdateColorList ( text, 1 );
  
  SetCurrentLine ( text ); 
  TextCursorOn ( text );
  SetTextModif ( text );
}

/*
**	Function name : f_tab
**
**	Description : Tabulation.
**	Input : Le text courant. 
**	Ouput :
*/
void f_tab ( text )
    Text *text;
{
  TextCursorOff ( text );
/*  CheckModifAndFilename ( text ); */
  
  UpdateMark ( text, 1, 0 );	
  InsertNchar ( text -> buf, "\t", 1 );
  if ( text -> u_todo == True )
    (void) StoreInUndo ( text, "\t", (char *) 0, 1, 0, U_STD );
  
  /* Pour la couleur */

  if ( text -> current_ce != 0 )
    (void) UpdateColorList ( text, 1 );

  ClipOn ( text, 0 );
  SetCurrentLine ( text );
  ClipOff ( text );
  TextCursorOn ( text );
  SetTextModif ( text );
}


/*
**	Function name : f_return
**
**	Description : Retour chariot.
**	Input : Le text courant.
**	Ouput :
*/
void f_return ( text )
    Text *text;
{
  TextCursorOff ( text );
/*  CheckModifAndFilename ( text ); */
  
  if ( text -> n2 == 0 ) 
    CurrentLineToMiddle ( text );
  
  UpdateMark ( text, 1, 1 );
  InsertNchar ( text -> buf, "\n", 1 );
  text -> n1 ++;
  text -> n2 --;
  text ->  no_current_line ++;
  text -> lines_in_buf ++;
  if ( text -> u_todo == True )
    (void) StoreInUndo ( text, "\n", (char *) 0, 1, 0, U_STD );

  /* Pour la couleur */
  if ( text -> current_ce != 0 )
    (void) UpdateColorList (text, 1);

  
  SetLinesTable ( text );
  ClipOn ( text, text -> n1 -1);
  RefreshPage ( text );
  ClipOff ( text );
  
  SetScrollLine ( text -> swin , text -> lines_in_buf );
  ShowScrollFrame ( dpy, text -> swin );
  (void) MoveScrollBar ( dpy, text -> swin, 
			CURRENT, text -> no_current_line - text -> n1 - 1 );

  TextCursorOn ( text );
  SetTextModif ( text );

}

/*
**	Function name : OpenSpace
**
**	Description : 
**	Input : 
**	Ouput :
*/
void OpenSpace ( text )
    Text *text;
{
  f_return ( text );
  TextCursorOff ( text );			
  BackwardChar ( text );
  
}

/*
**	Function name : f_delete
**
**	Description : Delete ou backspace.
**	Input : Le text courant.
**	Ouput :
*/
void f_delete ( text )
    Text *text;
{
  char c;
  int mark = 0;
  
  if ((TextInBuf ( text ) == False) 
      || (text -> buf -> l_cur == TopBuf(text ->buf))) {
    klaxon ();
    return;
  }
  TextCursorOff ( text );
  GetPrevChar ( text -> buf, &c );
  
  if ( c == '\n' ) {
    if ( text -> n1 == 0 ) 
      CurrentLineToMiddle ( text );
    mark = UpdateMark ( text, -1, -1 );
    DeleteNchar ( text -> buf, 1 );
    if ( mark ) 
      UpdateMarkPos ( text );
    text -> n1 --;
    text -> n2 ++;
    text -> no_current_line--;
    text -> lines_in_buf --;
    SetLinesTable ( text );
    ClipOn ( text, text -> n1 );
    RefreshPage ( text );
    ClipOff ( text );
    SetScrollLine ( text -> swin , text -> lines_in_buf );
    ShowScrollFrame ( dpy, text -> swin );
    if ( text -> no_current_line == 1 ) {
      (void) MoveScrollBar ( dpy, text -> swin, FIRST, 0 );
    }
    else
      (void) MoveScrollBar ( dpy, text -> swin, 
			    CURRENT, text -> no_current_line - text -> n1 - 1 );
  }
  else {
    UpdateMark ( text, -1, 0 );
    DeleteNchar ( text -> buf, 1 );
    ClipOn ( text, 0 );
    SetCurrentLine ( text );
    ClipOff ( text );
  }

  SetTextModif ( text );
  if ( text -> u_todo == True )
    (void) StoreInUndo ( text, &c, (char *) 0, -1, 0, U_STD );
    
    /* Pour la couleur */
  if ( text -> current_ce != 0 ) {
    (void) UpdateColorList ( text, -1 );
    ClipOn ( text, 0 );
    RefreshPage ( text );
    ClipOff ( text );
  }
  TextCursorOn ( text );
}


/*
**	Function name : Controle_D
**
**	Description : Efface le caractere courant.
**	Input : Le text courant.
**	Ouput :
*/
void Control_D ( text )
    Text *text;
{
  if ( TextInBuf ( text ) == False ) {
      klaxon();
    return;
  }
  if ( ForwardChar ( text ) == True ) {
    f_delete ( text );
    SetTextModif ( text );
  }
}


/*
**	Function name : Control_K
**
**	Description : Delete une ou plusieurs lignes
**	Input : Le text courant.
**	Ouput : Le nb de lignes.
*/
void Control_K ( text, n )
    Text *text;
    int n;
{
  char *p;
  int mark = 0;
  int len, dn;

  if ( TextInBuf ( text ) == False ) {
      klaxon();
    return;
  }
  
  mark = UpdateMark ( text, 0, -(n -1) );
  p = (char *) DeleteLines ( text -> buf, n, &len, &dn );
  if ( mark ) 
    UpdateMarkPos (text );
  if ( len == 0 ) {
    klaxon ();
    return;
  }
  
  (void) StoreInUndo ( text, p, (char *) 0, -len ,dn, U_STD );
  
  /* Pour la couleur */
  if ( text -> current_ce != 0 )
    (void) UpdateColorList ( text, -len );

  StoreInKillBuf ( p, len, dn );
#ifdef DEBUG
  fprintf ( stderr, "n = %d Delete lines = %d len = %d\n", n, dn, len );
#endif
  SetLinesTable ( text );
  
  ClipOn ( text, text -> n1 );
  RefreshPage ( text );
  ClipOff ( text );
  
  if ( dn > 0 ) {
    text -> lines_in_buf = GetNumberOfLineInBuf ( text -> buf );
    SetScrollLine ( text -> swin , text -> lines_in_buf );
    ShowScrollFrame ( dpy, text -> swin );
    (void) MoveScrollBar ( dpy, text -> swin, 
			  CURRENT, text -> no_current_line - text -> n1 - 1 );
  }
  SetTextModif ( text );
}


/*
**	Function name : Control_Y
**
**	Description : Restore un portion de texte.
**	Input : Le text courant, le no a restorer.
**	Ouput :
*/
void Control_Y ( text, i )
    Text *text;
    int i;
{
  char *p;
  int len, dn;
  
  TextCursorOff ( text );
  p = (char *) RestoreKillBuf ( i, &len, &dn );
  
  if ( p == 0 ) {
    TextCursorOn ( text );
    return;
  }
#ifdef DEBUG
  fprintf ( stderr, "i = %d len = %d Restore lines = %d\n", i, len, dn );
  write ( 1, p, len );
#endif
  ClipOn ( text, 0 );
  SetTextModif ( text );
  
  if ( dn == 0 ) {
    if ( len == 1 && *p == '\n' ) {  /* ligne vide */
	  f_return ( text );
    }
    else {
      UpdateMark ( text, len, 0 );
      InsertNchar ( text -> buf, p, len );
      (void) StoreInUndo ( text, p, (char *) 0, len ,0, U_STD );    
      SetCurrentLine ( text );
    }
  }
  else {
    (void) InsertLines ( text, p, len, dn );
    (void) StoreInUndo ( text, p, (char *) 0, len ,dn, U_STD ); 
  }
  
    /* Pour la couleur */
  if ( text -> current_ce != 0 ) {
    (void) UpdateColorList ( text, len );
    ClipOn ( text, 0 );
    RefreshPage ( text );
  }

  TextCursorOn ( text );
  ClipOff ( text );
}


/*
**	Function name : DisplayKillBuffer
**
**	Description : Affiche la fenetre des choses tuees.
**		et restore eventuellement.
**	Input : Le text courant.
**	Ouput :
*/
void DisplayKillBuffer ( text ) 
    Text *text;
{
  char *str;
  
  ClearListBox ();
  KillBufferInList ();
  str = (char *) SelectFromListBox ( "Kill Buffers" );
  if ( str != 0 ) {
    /* str += 9; */
    /* On vire le "Select : " */
    /* On ne prend que les 2 premier caracteres */
    *(str+2) = 0;
    /* On restore la nieme entree */
    Control_Y ( text, atoi(str) - 1 );
    if ( str != 0 )
      (void) free ( str ); 
  }
  else
    DisplayMessage ( text -> mwin, "Abort" );
}

/*
**	Function name : GetBytesFromCutBuffer
**
**	Description : Comme son nom l'indique.
**	Input : Le text courant.
**	Ouput :
*/
void GetBytesFromCutBuffer ( text )
    Text *text;
{
  int lines;
  char *s;
  int nbytes;
  
  s = XFetchBuffer ( dpy, &nbytes, 0 );
  
  if ( (s != 0) && (nbytes != 0 )) {
    TextCursorOff ( text );
    ClipOn ( text, 0 );
    lines = GetNewLine ( s, nbytes);
    SetTextModif ( text );
    if ( lines == 0 ) {
      UpdateMark ( text, nbytes, 0 );
      InsertNchar ( text -> buf, s, nbytes);
      SetCurrentLine ( text );
    }
    else {
      (void) InsertLines ( text, s, nbytes, lines );
      RefreshScrollBar ( dpy, text -> swin );
    }
    (void) StoreInUndo ( text, s, (char *) 0, nbytes, lines, U_STD );
    
    /* Pour la couleur */
    if ( text -> current_ce != 0 ) {
      (void) UpdateColorList ( text, nbytes );
      ClipOn ( text, 0 );
      RefreshPage ( text );
    }

    TextCursorOn ( text );
    ClipOff ( text );
  }
}


/*
**	Function name : InsertLines
**
**	Description : Insere une ou plusieurs lignes.
**	Input : Le text courant, la chaine, sa longueur et
**		le nombre de lignes.
**	Ouput :
*/
void InsertLines ( text, s, nbytes, lines )
    Text *text;
    char *s;
    int nbytes;
    int lines;
{
  int mark = 0;
  
  mark = UpdateMark ( text, nbytes, lines );
  InsertNchar ( text -> buf, s, nbytes );      
  text -> no_current_line += lines;
  text -> lines_in_buf = GetNumberOfLineInBuf ( text -> buf );
  SetScrollLine ( text -> swin , text -> lines_in_buf );
  ShowScrollFrame ( dpy, text -> swin );
  
  if ( text -> n2 < lines ) {
    CurrentLineToMiddle ( text );
  }
  else {
    text -> n1 += lines;
    text -> n2 -= lines;
    SetLinesTable ( text );
    RefreshPage ( text );
    (void) MoveScrollBar ( dpy, text -> swin, 
			  CURRENT, text -> no_current_line - text -> n1 - 1 );
  }
  if ( mark ) 
    UpdateMarkPos (text );
}


/*
**	Function name : StoreBytesInCutBuffer
**
**	Description : Selection.
**	Input : Le text, la position du cursor.
**	Ouput :
*/
void StoreBytesInCutBuffer ( text, x, y )
    Text *text;
    int x, y;
{
  char *old_s = 0;
  int old_n = 0;
  int old_x = text -> x_pos;
  int old_y = text -> y_pos;
  char *old_left = (char *) LeftBuf ( text -> buf );
  char *old_right = (char *) RightBuf ( text -> buf );
  char *new_left, *new_right;
  
  (void) MoveToXYinTextWindow ( text, x, y );
  TextCursorOn ( text );
  XSync ( dpy, False );
  
  /* Juste pour voir si ya deja qqch. 
     Si oui, on libere */
  old_s = XFetchBytes ( dpy, &old_n );
  if ( old_s != 0 )
    (void) XFree ( old_s );
  
  new_left = (char *) LeftBuf ( text -> buf );
  new_right = (char *) RightBuf ( text -> buf );

  if ( new_left > old_left )  { /* Deplacement vers la droite du curseur */
	XStoreBytes ( dpy, old_left +1, new_left - old_left );
  }
  else {
	XStoreBytes ( dpy, new_right, old_right - new_right );
  }
  toggle_del = False;
  SmallTime ( (long) 200000 );	/* 0.20 secondes */
  TextCursorOff ( text );
  (void) MoveToXYinTextWindow ( text, old_x, old_y );
  
/*  XSetSelectionOwner (dpy, XA_PRIMARY, text -> window, CurrentTime ); */
}


/*
**	Function name : DeleteBytesFromCutBuffer
**
**	Description : Efface une slection.
**	Input : Le text courant.
**	Ouput :
*/
void DeleteBytesFromCutBuffer ( text )
    Text *text;
{
  int lines;
  char *s, *tmp = 0;
  int mark = 0;
  int nbytes;
  
  s = XFetchBytes ( dpy, &nbytes );
  
  if ( (s != 0) && (nbytes != 0 ) && (toggle_del == False) ) {
    if ( (text -> modif == False) && (strcmp(text -> filename,"NoName")==0) ) {
      return;
    }

    if ( (RightBuf ( text -> buf ) + nbytes) > BottomBuf ( text -> buf ) ) {
      nbytes = BottomBuf ( text -> buf ) - RightBuf ( text -> buf );
      if ( nbytes <= 0 ) {
	klaxon ();
	return;
      }
    }
    tmp = (char *) malloc ( (unsigned) nbytes + 2 );
    (void) bcopy ( RightBuf ( text -> buf ), tmp, nbytes );
    
    TextCursorOff ( text );
    ClipOn ( text, 0 );
    
    lines = GetNewLine ( RightBuf ( text -> buf ), nbytes );
    (void) MoveHole ( text -> buf, nbytes );
    mark = UpdateMark ( text, -nbytes, -lines );
    DeleteNchar ( text -> buf, nbytes);
    if ( mark )
      UpdateMarkPos ( text );
    SetTextModif ( text );
    
    if ( lines == 0 ) {
      SetCurrentLine ( text );
    }
    else {
      text -> lines_in_buf = GetNumberOfLineInBuf ( text -> buf );
      SetScrollLine ( text -> swin , text -> lines_in_buf );
      ShowScrollFrame ( dpy, text -> swin );
      SetLinesTable ( text );
      RefreshPage ( text );
      (void) MoveScrollBar ( dpy, text -> swin, 
			    CURRENT, text -> no_current_line - text -> n1 - 1 );
      RefreshScrollBar ( dpy, text -> swin );
    }
    (void) StoreInUndo ( text, tmp, (char *) 0, -nbytes, lines, U_STD );
    (void) StoreInKillBuf ( tmp, nbytes, lines );
    
    /* Pour la couleur */
    if ( text -> current_ce != 0 ) {
      (void) UpdateColorList ( text, -nbytes );
      ClipOn ( text, 0 );
      RefreshPage ( text );
    }
    TextCursorOn ( text );
    ClipOff ( text );
    toggle_del = True;
    (void) free ( tmp );
  }
}
static int timer_expire = False;
static void donothing () { timer_expire = True; }


/*
**	Function name : SmallTime
**
**	Description : Un reveil
**	Input : Le temps.
**	Ouput :
*/
void SmallTime ( t )
    long t; /* micros-secondes */
{
  struct itimerval value;
#if defined(SVR4) || defined(UNIXWARE)  
  usleep(t);
#else
#ifdef NOTDEF
  value.it_interval.tv_sec = 0;
  value.it_interval.tv_usec = 0;
  value.it_value.tv_sec = 0;
  value.it_value.tv_usec = t;

  timer_expire = False;
  (void) signal ( SIGALRM, (void (*) ()) donothing );
  (void) setitimer ( ITIMER_REAL, &value, (struct itimerval *) 0 );
  while(1) {
    sigblock(sigmask(SIGALRM));
    if ( timer_expire )
      break;
    else
      sigpause(0);
  }
  sigblock(0);
  (void) signal ( SIGALRM, SIG_IGN );
#else
	/* best way to do it... portable, too... */
  struct timeval timeout;

  timeout.tv_sec = 0;
  timeout.tv_usec = t;

  select(0, NULL, NULL, NULL, &timeout);
#endif
#endif
}


/*
**	Function name : FunctionHeader
**
**	Description : 
**	Input : 
**	Ouput :
*/
void FunctionHeader ( text )
    Text *text;
{
  char *msg;
  int nw = text->win_id;
  
  StorePosition ( text );
  msg = (char *) ie_call_function ( text, "function_header", 0, 0);
  text = update_cwd(nw);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
  ie_redisplay ( text );
}


/*
**	Function name : IncludeHeader
**
**	Description : 
**	Input : 
**	Ouput :
*/
void IncludeHeader ( text )
    Text *text;
{
  char *msg;
  int nw = text->win_id;
  
  StorePosition ( text );
  msg = (char *) ie_call_function ( text, "include_header", 0, 0) ;
  text = update_cwd(nw);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
  ie_redisplay ( text );
}

/*
**	Function name : ClassHeader
**
**	Description : 
**	Input : 
**	Ouput :
*/
void ClassHeader ( text )
    Text *text;
{
  char *msg;
  int nw = text->win_id;
    
  StorePosition ( text );
  msg = (char *) ie_call_function ( text, "class_header", 0, 0) ;
  text = update_cwd(nw);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
  ie_redisplay ( text );
}

/*
**	Function name : MethodHeader
**
**	Description : 
**	Input : 
**	Ouput :
*/
void MethodHeader ( text )
    Text *text;
{
  char *msg;
  int nw = text->win_id;

  StorePosition ( text );
  msg = (char *) ie_call_function ( text, "method_header", 0, 0) ;
  text = update_cwd(nw);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
  ie_redisplay ( text );
}

/*
**	Function name : LatexMacros
**
**	Description :
**	Input :
**	Output :
*/
void LatexMacros(text)
    Text *text;
{
  char *msg;
  int nw = text->win_id;

  StorePosition ( text );
  msg = (char *) ie_call_function ( text, "latex_macros", 0, 0) ;
  text = update_cwd(nw);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
  ie_redisplay ( text );
}

/*
**	Function name : HtmlMacros
**
**	Description :
**	Input :
**	Output :
*/
void HtmlMacros(text)
    Text *text;
{
  char *msg;
  int nw = text->win_id;

  StorePosition ( text );
  msg = (char *) ie_call_function ( text, "html_macros", 0, 0) ;
  text = update_cwd(nw);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
  ie_redisplay ( text );
}

/*
**	Function name : MiscCommands
**
**	Description :
**	Input :
**	Output :
*/
void MiscCommands(text)
    Text *text;
{
  char *msg;
  int nw = text->win_id;

  StorePosition ( text );
  msg = (char *) ie_call_function ( text, "misc_commands", 0, 0) ;
  text = update_cwd(nw);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
  ie_redisplay ( text );
}

/*
**	Function name : UserCommands
**
**	Description :
**	Input :
**	Output :
*/
void UserCommands(text)
    Text *text;
{
  char *msg;
  int nw = text->win_id;

  StorePosition ( text );
  msg = (char *) ie_call_function ( text, "user_commands", 0, 0) ;
  text = update_cwd(nw);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
  ie_redisplay ( text );
}


/*
**	Function name : QuotedChar
**
**	Description : Pour inserer un caractere de controle
**	Input :
**	Output :
*/
void QuotedChar( text )
    Text *text;
{
    char buf [32];
    KeySym ksym;
    XComposeStatus compose;
    XEvent ev;
    int n_bytes;
    
    bzero (buf, 32);
    DisplayMessage ( text -> mwin, "Enter quoted char" );
    
    for (;;) {
	XNextEvent ( dpy, &ev );
	switch ( ev.type ) {
	case LeaveNotify:
	case ButtonPress:
	  XPutBackEvent ( dpy, &ev );
	  DisplayMessage ( text -> mwin, " " );
	  return;
	case KeyPress:
	    n_bytes = XLookupString ( (XKeyEvent *) &ev, buf, 32, &ksym, &compose );
	    if ( (n_bytes != 0) && (buf[0] != 0) ) {
		if ( buf[0] == '\n' )
		  f_return ( text );
		else
		  f_impc ( text, (int) buf [0] );
		DisplayMessage ( text -> mwin, " " );
		return;
	    }
	    switch (ksym) {
	    case XK_Control_R:
	    case XK_Control_L:
	      for(;;) {
		  XNextEvent ( dpy, &ev );
		  switch ( ev.type ) {
		  case LeaveNotify:
		  case ButtonPress:
		      XPutBackEvent ( dpy, &ev );
		      DisplayMessage ( text -> mwin, " " );
		      return;
		  case KeyPress:
		      n_bytes = XLookupString ( (XKeyEvent *) &ev, buf, 32, &ksym, &compose );
		      if ( (n_bytes != 0) && (buf[0] != 0) ) {
			  if ( buf[0] == '\r' )
			    f_return ( text );
			  else
			    f_impc (text, (int) buf [0] );
		      }
		      DisplayMessage ( text -> mwin, " " );
		      return;
		  default:
		    break;
		  }
	      }
	    default:
	      break;
	    }
	}
    }
}
#ifdef SVR4
/*
 *      usleep(delay)  --
 *
 *      Possible usleep replacement. Delay in microseconds.
 *      Another possiblity is to use poll(2). On Solaris
 *      2.x, select is just a wrapper for poll, so you
 *      are better off using it directly.  If you use,
 *      poll, note that it uses millisecond resolution,
 *      and is not affected by the O_NDELAY and O_NONBLOCK 
 *      flags.
 *
 *      Note that using nanosleep has portability implications,
 *      even across different versions of Solaris 2.x. In
 *      particular, only Solaris 2.3 has libposix4, and 
 *      hence nanosleep. Select (or poll) is a better option if 
 *      you need portability across those versions.
 *
 *      If you define USE_NANOSLEEP, be sure to link with -lposix4
 *
 *      This function is taken from the Solaris 2.x Porting FAQ
 */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include <sys/param.h>
#include <sys/types.h>
#ifdef  USE_POLL
#include <stropts.h>
#include <poll.h>
#endif  /* USE_POLL */

int usleep(useconds)
    unsigned long int useconds;
{
#ifdef  USE_NANOSLEEP
  struct timespec rqtp;

  rqtp.tv_sec  = useconds / (unsigned long)  1000000;
  rqtp.tv_nsec = (useconds % (unsigned long) 1000000) * 1000 ;

  if (nanosleep(&rqtp,  (struct  timespec *) NULL) == -1)
    perror("nanosleep");
  return (0);

#elif   USE_POLL
  struct pollfd unused;

  if (poll(&unused,0,(useconds/1000)) == -1) 
    perror("poll");
  return(0);
#else  /* default to select */
  struct timeval delay;

  delay.tv_sec = 0;
  delay.tv_usec = useconds;
  if (select(0,
             (fd_set *) NULL,
             (fd_set *) NULL,
             (fd_set *) NULL,
             &delay) == -1)
    perror("select");
  return (0);
#endif /* USE_NANOSLEEP */
}
#endif /* SVR4 */
