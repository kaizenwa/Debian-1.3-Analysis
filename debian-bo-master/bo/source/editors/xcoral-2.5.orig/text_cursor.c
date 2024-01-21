/* ########################################################################

			     text_cursor.c

   File: text_cursor.c
   Path: /home/fournigault/c/X11/xcoral-2.31/text_cursor.c
   Description: 
   Created: Fri Jan 27 11:37:15 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:37:15 MET 1995
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
#if defined(SVR4) || defined(UNIXWARE)
#include <string.h>
#else
#include <strings.h>
#endif
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif

#include "main_text.h"
#include "text_cursor.h"
#include "page.h"

FCT (static void, GetCurrentXY, (Text *text, int *x, int *y, char *c) );
FCT (static int, MyTextWidth, (Text *text,char *str, int i) );

extern Display *dpy;

/*
**	Function name : TextCursorOn
**
**	Description : Allume le curseur.
**
**	Input : Le text courant.
**	Ouput :
*/
void TextCursorOn ( text )
    Text *text;
{
    int x, y;
    char c;
    int width;
    unsigned char sp_c = (unsigned char) BIZ_CHAR;
    unsigned long c_color;
    
    SetLinesTable ( text );
    
    (void) GetCurrentXY ( text, &x, &y, &c );

    if ( (c == '\t') || (c == '\n') || (c == 0) ) 
      width = text -> char_width_ave;
    else {
      width = XTextWidth ( text -> font, &c, 1 );
      if ( width == 0 ) {
	width = XTextWidth ( text -> font, (char *) &sp_c, 1 );
      }
    }
    
    text -> x_pos = x;
    text -> y_pos = y;
    text -> cursor_width = width;
    
    if ( text -> cursor_stat == ON )
      return;
    
    if ( text -> mouse_in == True ) {
	if ( text -> cursor_stat == FREESE )
	  UnFreezeTextCursor ( text );
#ifdef DEBUG
	(void) fprintf ( stderr, "Cursor On\n" );
#endif
	c_color = GetCursorColor(text);
	XSetForeground (dpy, text -> Igc, c_color);
	XSetPlaneMask (dpy, text -> Igc, c_color);

	XFillRectangle ( dpy, text -> window,text -> Igc,
			x, y, text -> cursor_width, text -> font_height );
	
	XSetForeground (dpy, text -> Igc, text -> fg ^ text -> bg );
	XSetPlaneMask (dpy, text -> Igc, text -> fg ^ text -> bg );
	text -> cursor_stat = ON;
    }
    else
      FreezeTextCursor ( text );
}


/*
**	Function name : FreezeTextCursor
**
**	Description : Gele le curseur.
**	Input : Le text courant.
**	Ouput :
*/
void FreezeTextCursor ( text )
    Text *text;
{
    unsigned long c_color;
    
#ifdef DEBUG
    (void) fprintf ( stderr, "FreezeCursor x = %d y = %d\n",
		    text -> x_pos, text -> y_pos);
#endif
    if ( text -> cursor_stat != OFF )
      return;
    
    c_color = GetCursorColor(text);
    XSetForeground (dpy, text -> Igc, c_color);
    XSetPlaneMask (dpy, text -> Igc, c_color);
    
    XDrawRectangle ( dpy, text -> window, text -> Igc,
		    text -> x_pos, text -> y_pos,
		    text -> cursor_width, text -> font_height );

    XSetForeground (dpy, text -> Igc, text -> fg ^ text -> bg );
    XSetPlaneMask (dpy, text -> Igc, text -> fg ^ text -> bg );
    
    text -> cursor_stat = FREESE;
}


/*
**	Function name : UnFreezeTextCursor
**
**	Description : Degele le curseur.
**	Input : Le text courant.
**	Ouput :
*/
void UnFreezeTextCursor ( text )
    Text *text;
{
    unsigned long c_color;
    
#ifdef DEBUG
    (void) fprintf ( stderr, "UnFreezeCursor x = %d y = %d\n",
		    text -> x_pos, text -> y_pos);
#endif
    if ( text -> cursor_stat != FREESE )
      return;
    
    c_color = GetCursorColor(text);
    XSetForeground (dpy, text -> Igc, c_color);
    XSetPlaneMask (dpy, text -> Igc, c_color);

    XDrawRectangle ( dpy, text -> window, text -> Igc,
		    text -> x_pos, text -> y_pos,
		    text -> cursor_width, text -> font_height );

    XSetForeground (dpy, text -> Igc, text -> fg ^ text -> bg );
    XSetPlaneMask (dpy, text -> Igc, text -> fg ^ text -> bg );

    text -> cursor_stat = OFF;
}



/*
**	Function name : TextCursorOff
**
**	Description : Eteint le curseur.
**	Input : Le text courant.
**	Ouput :
*/
void TextCursorOff ( text )
    Text *text;
{
    unsigned long c_color;
    
    if ( text -> cursor_stat == OFF )
      return;
    
    if ( text -> cursor_stat == FREESE ) {
	UnFreezeTextCursor ( text );
	return;
    }
#ifdef DEBUG
    (void) fprintf ( stderr, "Cursor Off x = %d y = %d\n",
		    text  -> x_pos, text -> y_pos);
#endif
    c_color = GetCursorColor(text);
    XSetForeground (dpy, text -> Igc, c_color);
    XSetPlaneMask (dpy, text -> Igc, c_color);

    XFillRectangle ( dpy, text -> window,text -> Igc,
		    text -> x_pos, text -> y_pos, text -> cursor_width,
		    text -> font_height );

    XSetForeground (dpy, text -> Igc, text -> fg ^ text -> bg );
    XSetPlaneMask (dpy, text -> Igc, text -> fg ^ text -> bg );
    
    text -> cursor_stat = OFF;
}


/*
**	Function name : MoveToXYinTextWindow
**
**	Description : Deplace le curseur a la position (x, y)
**		dans la page courante.
**
**	Input : Le text courant, la nouvelle position.
**	Ouput : 0 si Ok -1 sinon
*/
int MoveToXYinTextWindow ( text, x, y )
    Text *text;
    int x, y;
{
    char *s, *sh;
    int i, cur_line, new_line, delta;
    char *HscrollString ();
    int len;
    
    x -= text -> x_or;
    y -= text -> y_or;
    
    cur_line = text -> n1 + 1;
    new_line = (y / text -> font_height) + 1;
    
    if ( new_line > text -> lines_in_page )
      new_line = text -> lines_in_page;
    
    if ( (delta = new_line - cur_line) == 0 ) {  /* meme ligne */
	s = GetCurrentLine ( text -> buf, &len );
	if ( text -> sl )
	  sh = HscrollString ( s, text );
	else
	  sh = s;
	for ( i = 1; i < (len -(sh -s) + 1); i++ ) { 
	    if ( MyTextWidth ( text , sh, i ) > x ) 
	      break;
	}
	i += (sh -s);
	(void) MoveHole ( text -> buf, i - GetNcFromLeft ( text -> buf ) - 1 );
	return 0;
    }
    if ( (delta + text -> no_current_line) > text -> lines_in_buf ) {
	return -1;		
    }
    (void) MoveToLine ( text -> buf, delta); /* On va sur la bonne ligne */
    text -> n1 += delta;
    text -> n2 -= delta;
    text -> no_current_line += delta;
    
    s = GetCurrentLine ( text -> buf, &len );

    if ( text -> sl )
      sh = HscrollString ( s, text );
    else
      sh = s;
    for ( i = 1; i < (len -(sh-s)+1); i++ ) {
	if ( MyTextWidth ( text, sh, i ) > x ) 
	  break;
    }
    i += (sh - s);
    (void) MoveHole ( text -> buf, i - 1 );
    return (0);
}


/*
**	Function name : MoveToBline
**
**	Description : Deplace le curseur au debut de la ligne courante
**	Input : Le text courant.
**	Ouput :
*/
void MoveToBline ( text )
    Text *text;
{
    int n;
    
    if ( text -> sl )
      GotoLeft ( text );
    
    n = GetNcFromLeft ( text -> buf );
    (void) MoveHole ( text -> buf, -n );
}


/*
**	Function name : MoveToEline
**
**	Description : Deplace le curseur a la fin de la ligne courante.
**
**	Input : Le text courant.
**	Ouput :
*/
void MoveToEline ( text )
    Text *text;
{
    int n;
    
    n = GetNcFromRight ( text -> buf );
    (void) MoveHole ( text -> buf, n );
}


/*
**	Function name : UpCursor
**
**	Description : Deplace le curseur sur la ligne
**		precedente.
**	Input : Le text courant.
**	Ouput :
*/
void UpCursor ( text )
    Text *text;
{
#ifdef DEBUG
    (void) fprintf ( stderr, "Up cursor\n" );
#endif
    
    if ( text -> no_current_line == 1 ) {
	klaxon ();
	return;
    }
    
    if ( text -> n1 == 0 )                /* il faut scroller */
      CurrentLineToMiddle ( text );
    
    (void) MoveToXYinTextWindow ( text, text -> x_pos,
				 text -> n1 * text -> font_height );
}


/*
**	Function name : DownCursor
**
**	Description : Delace le curseur sur la ligne suivante.
**	Input : Le text courant.
**	Ouput :
*/
void DownCursor ( text )
    Text *text;
{
    int y;
    
#ifdef DEBUG
    (void) fprintf ( stderr, "Down cursor\n" );
#endif
    
    if ( text -> no_current_line >= text -> lines_in_buf ) {
	klaxon ();
	return;
    }
    
    if ( text -> n2 == 0 ) {
	CurrentLineToMiddle ( text );
	y = text -> y_or + (( text -> n1 + 1 ) * text -> font_height);
	y += 3; /* Pour etre sur d'etre sur la bonne ligne */
    }
    else
      y = text -> y_pos + text -> font_height;
    
    (void) MoveToXYinTextWindow ( text, text -> x_pos, y );
}


/*
**	Function name : ForwardChar
**
**	Description : Deplace le curseur de 1 caractere vers la droite.
**	Input : Le text courant.
**	Ouput : True si Ok, False si fin de buffer.
*/
int ForwardChar ( text )
    Text *text;
{
    char c;

#ifdef DEBUG
    (void) fprintf ( stderr, "Forward char\n" );
#endif

    if ( GetCurrentChar ( text -> buf, &c ) == -1 ) {
	/* Fin du buffer */
	klaxon ();
	return False;
    }
    if ( c == '\n' ) {
	if ( text -> no_current_line > text -> lines_in_buf )
	  return False;
	text -> no_current_line ++;
	(void) MoveToLine ( text -> buf, 1 );
	if ( text -> n2 == 0 ) {
	    CurrentLineToMiddle ( text );
	}
	else {
	    text -> n1 ++;
	    text -> n2 --;
	}
    }
    else 
      (void) MoveHole ( text -> buf, 1 );

    return True;
}


/*
**	Function name : BackwardChar
**
**	Description : Delplace le curseur de 1 caractere vers la gauche.
**	Input : Le text courant.
**	Ouput :
*/
void BackwardChar ( text )
    Text *text;
{
    char c;
#ifdef DEBUG
    (void) fprintf ( stderr, "Backward char\n" );
#endif
    if ( MoveHole ( text -> buf, -1 ) < 0 ) {
	klaxon ();
	return;
    }
    (void) GetCurrentChar ( text -> buf, &c );
    if ( c == '\n' ) {
	text -> no_current_line --;
	if ( text -> n1 == 0 )
	  CurrentLineToMiddle ( text );
	else {
	    text -> n1 --;
	    text -> n2 ++;
	}
    }
}


/*
**	Function name : GetCurrentXY
**
**	Description : Retourne la position courante dans la page,
**		(x, y) ainsi que le caractere courant.
**	Input : Le text courant, la position et la caractere.
**	Ouput :
*/
static void GetCurrentXY ( text, x, y, c )
    Text *text;
    int *x, *y; 	/* RETURN */
    char *c;	/* RETURN */
{
    char *s;  /* Current line */
    char *cl; /* Current line from left */
    int n, n0;
    int len, clipx;
    int nc_l;
    char *HscrollString ();
    
    if ( TextInBuf ( text ) == False ) {
	*x = text -> x_or;
	*y = text -> y_or;
	*c = 0;
	return;
    }
    
    clipx = text -> width - ( 2 * text -> x_or );
    cl = GetCurrentLine ( text -> buf, &len );
    
    if ( text -> page.sline [ text -> n1 + 1 ] == 0 )
      (void) fprintf ( stderr, "Warning page.sline 0\n" );
    
    if ( text -> sl )
      s = HscrollString ( cl, text );
    else
      s = cl;
    
    if ( s == 0 ) 
      (void) fprintf ( stderr, "Internal Error 2...\n" );
    
    *y = text -> y_or + ( text -> n1 * text -> font_height );
    n0 = GetNcFromLeft ( text -> buf );
    n = n0 - ( s - cl );
    
    /*
     *  S'il y a scroll horizontal, on scroll completement a gauche,
     *  pour etre en debut de ligne.
     */   
    if ( (n <= 0) && (text -> sl) ) {   
	text -> sl = 0;
	SetLinesTable ( text );
	s = text -> page.sline [ text -> n1 + 1 ];
	n = n0 - ( s - cl );
	ClipOn ( text, 0 );
	ClearPage ( text );
	RefreshPage ( text );
	ClipOff ( text );
    }
    
    /*
     *  Si le caractere courant est non visible (a droite), on scroll
     *  a droite autant de fois qu'il est necessaire.
     */
    if ( ( *x = text -> x_or + MyTextWidth ( text, s, n )) > clipx ) {
	/* 
	   nc_l est le nombre de caracteres (de deux pixels de large pour
	   etre tranquille) affichables dans la fenetre courante.
	*/
	nc_l = text -> width / 2;
	
	do {
	    if ( (n - nc_l) > 8 ) /* On accelere un peu les choses */
	      text -> sl += (((n - nc_l) / 8) - 1);
	    text -> sl ++;	    
	    if ((cl) && (text -> sl))
	      s = HscrollString ( cl, text );
	    n = n0 - ( s - cl );
	} while (( *x = text -> x_or + MyTextWidth ( text, s, n )) > clipx );

	SetLinesTable ( text );
	s = text -> page.sline [ text -> n1 + 1 ];
	n = n0 - ( s - cl );
	
	ClipOn ( text, 0 );
	ClearPage ( text );
	RefreshPage ( text );
	ClipOff ( text );
    } 
    
    if ( text -> no_current_line > text -> lines_in_buf )
      *x = text -> x_or;
    
    *c = *(s + n);
#ifdef DEBUG
    (void) fprintf ( stderr, "n = %d x = %d c = %c\n", n, *x, *c );
#endif
}


/*
**	Function name : MyTextWidth
**
**	Description : Retourne la longueur en pixel des i premiers
**		caracteres de la chaine s.
**		Cette fonction tient compte des tabs.
**	Input : Le text courant, la chaine, nb caracteres.
**	Ouput : La longueur en pixel.
*/
static int MyTextWidth ( text, str, i )
    Text *text;
    char *str;
    int i;
{
    char *p, *s, *pp;
    int n = 1; /* Caractere en cours de traitement */
    int xx, x, width, count;
    int sizetab = text -> tab_width * text -> blanc_width;
    
    xx = x = width = 0;
    p = s = str;

    if ( str == 0 ) 
      (void) fprintf ( stderr, "Internal Error 3...\n" );
    
    /* 
       Voyons d'abord si la chaine contient ou non des caracteres
       de largeur nulle (caractere de controle). Dans un premier temps,
       on recopie la chaine puis on fait une substitution par un
       caractere peu utilise.
    */
    count = i;
    if ( count == 0 )
      return 0;
    else if ( count > 1 ) {
      pp = (char *) malloc ((unsigned) count + 1);
      (void) bcopy ( str, pp, count );
    }
    else
	pp = str;
	  
    p = pp;
    s = pp;

    while ( count ) {
	if ( (*p != '\t') && (XTextWidth ( text -> font, p, 1 ) == 0) ) {
	    *p = (unsigned char) BIZ_CHAR;
	}
	p++;
	count --;
    }
    p = pp; /* Pour replacer p au debut de la chaine */
    count = i; 
	
    while ( i /* *p != '\n'*/ ) {
      /*if ( i == 0 )
	    break; */
	if ( *p == '\t' ) {
	    xx =  sizetab;
	    width = XTextWidth ( text -> font, s, n - 1);
	    while ( width >= xx ) 	
	      xx += sizetab;
	    x += xx;
	    s = ++p;
	    n = 1;
	}
	else {
	    n++;
	    p++;
	}
	i--;
    }
    x += XTextWidth ( text -> font, s, n - 1 );
    if ( count > 1 )
      (void) free (pp);
    return (x);
}


/*
**	Function name : HscrollString
**
**	Description : Calcul du scroll horizontal.
**	Input : La chaine, le nb de tabulation et la taille de celle-ci
**	Ouput : Le premier caractere de la chaine corrspondant
**		a la tabulation souhaitee.
*/
char *HscrollString ( s, text )
    char *s;
    Text *text;
{
    char *end_ob = BottomBuf ( text -> buf );
    int d = text -> sl * text -> tab_width; 

    while ( d > 0 ) {
	if ( s == end_ob )
	  return 0;
	  
	if ( *s == '\n' )
	  break;
	if ( *s == '\t' ) {
	    d -= text -> tab_width;
	    s++;
	    continue;
	}
	s++;
	d--;
    }
    return s;
}


/*
**	Function name : GotoLeft
**
**	Description : Met le scroll horizontal a zero.
**	Input : Le text courant.
**	Ouput :
*/
void GotoLeft ( text )
    Text *text;
{
    text -> sl = 0;
    ClearPage ( text );
    SetAndDisplayPage ( text );
    DisplayMessage ( text -> mwin, " " );
    MCLEAR ( text -> stat, MESSAGE);
}

