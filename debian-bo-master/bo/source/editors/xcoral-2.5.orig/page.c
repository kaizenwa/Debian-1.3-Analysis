/* ########################################################################

				 page.c

   File: page.c
   Path: /home/fournigault/c/X11/xcoral-2.31/page.c
   Description: 
   Created: Fri Jan 27 11:22:54 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:22:55 MET 1995
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
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <stdlib.h>

#include "main_text.h"
#include "page.h"
#include "text_cursor.h"
#include "shadow.h"
#include "bm_search.h"
#include "color_area.h"

extern Display *dpy;

FCT (static int, DrawLineOfText, (Text *text, int li) );
FCT (static void, ScrollUpFewLines, (Text *text, int n) );
FCT (static void, ScrollDownFewLines, (Text *text, int n) );
FCT (static void, ShiftLinesTable, (Text *text, int dir) );

/*
**	Function name : FirstPage
**
**	Description : Affichage de la premiere page du buffer courant.
**
**	Input : Le text courant.
**	Ouput :
*/
void FirstPage ( text )
    Text *text;
{
    text -> sl = 0;
    HoleToLeft ( text -> buf );
    text -> n1 = 0;
    text -> n2 = text -> lines_in_page - 1;
#ifdef DEBUG
    (void) fprintf ( stderr,"ht = %d\n", text -> height );
#endif
    SetAndDisplayPage ( text );
    
    if ( text -> stat & MESSAGE ) {
	DisplayMessage ( text -> mwin, "  " );
	MCLEAR ( text -> stat, MESSAGE );
    }
    
    (void) MoveScrollBar ( dpy, text -> swin, FIRST, 0 );
    text -> no_current_line = 1;
    
    MSET ( text -> stat, FIRSTPAGE );
    MCLEAR ( text -> stat, LASTPAGE );
    
#ifdef DEBUG
    (void) fprintf ( stderr, "cur_line = %d n1 = %d n2 = %d\n",
		    text -> no_current_line, text -> n1, text -> n2 );
#endif
}


/*
**	Function name : ClearPage
**
**	Description : Comme son nom l'indique.
**
**	Input : Le text courant.
**	Ouput :
*/
void ClearPage ( text )
    Text *text;
{
    XClearArea ( dpy, text -> window,
		text -> x_or, text -> y_or, text -> width - ( 2 * text -> x_or ),
		text -> height - ( 2 * text -> y_or ), False );
}


/*
**	Function name : ClipOn
**
**	Description : Met le clip a partir de la ligne no n
**		de la page courante.
**		La ligne n n'est pas touchee par le clip.
**
**	Input : Le text courant, no de ligne.
**	Ouput :
*/
void ClipOn ( text, line )
    Text *text;
    int line;
{
    XRectangle rect [1];
    
    rect [0].x = text -> x_or;
    rect [0].width = text -> width - ( 2 * text -> x_or );
    
    if ( line <= 1 ) {
	rect [0].y = text -> y_or;
	rect [0].height = text -> height - ( 2 * text -> y_or );
    }
    else {
	rect [0].y = text -> y_or + (line  * text -> font_height);
	rect [0].height = text -> height - ( 2 * text -> y_or )
	  - (line * text -> font_height );
    }
    
    XSetClipRectangles ( dpy, text -> Cgc, 0, 0, rect, 1, False );
}


/*
**	Function name : ClipOff
**
**	Description : Remet la zone de clip par defaut.
**
**	Input : Le text courant.
**	Ouput :
*/
void ClipOff ( text )
    Text *text;
{
    XSetClipMask ( dpy, text -> Cgc, None );
}


/*
**	Function name : ExposePage
**
**	Description : Exposition de la page courante.
**		Deux cas possibles : 
**			-Configure ( Resize )
**			-Expose-Region
**
**	Input : La region exposee, le text courant.
**	Ouput :
*/
void ExposePage ( r, text )
    Region r;
    Text *text;
{
    XRectangle rect [1];
    Region inter, region;
    
    if (  TextInBuf ( text ) != True )
      return;
    
    /* Configure */
    if ( text -> n1 > text -> lines_in_page - 1 ) {
	
	text -> n1 = text -> lines_in_page - 1;
	text -> n2 = 0;
	SetAndDisplayPage ( text );
	(void) MoveScrollBar ( dpy, text -> swin, CURRENT, text -> no_current_line - 1 - text -> n1 );
	return;
    }
    
    /* Expose */
    region = XCreateRegion ();
    inter = XCreateRegion ();
    
    rect [0].x = text -> x_or;
    rect [0].y = text -> y_or;
    rect [0].width = text -> width - ( 2 * text -> x_or );
    rect [0].height = text -> height - ( 2 * text -> y_or );
    
    XUnionRectWithRegion ( rect, region, region );
    
    XIntersectRegion ( r, region, inter );
    XSetRegion ( dpy, text -> Cgc, inter );
    
    (void) SetLinesTable ( text );
    RefreshPage ( text );
    
    XDestroyRegion ( inter );
    XDestroyRegion ( region );
}


/*
**	Function name : SetLinesTable
**
**	Description : Mise a jour de la table des lignes
**		pour la page courante.
**
**	Input : Le text courant.
**	Ouput :
*/
void SetLinesTable ( text )
    Text *text;
{
    int line, n = 0, wl = 1;
    char *s = 0, *hs;
    int len;
    
    /*
     * Si ya rien dans le buffer, il faut absolument
     * virer le pointeur de la ligne courante car
     * celui-ci est global a tous les buffers
     * 
     */
/*    
    if (  TextInBuf ( text ) != True ) {
	text -> page.sline [ text -> n1 + 1 ] = (char *) 0;
	return;
    }
*/    
    
    for ( line = text -> n1; line > 0 ; line-- ) {
	if (! s)
	  s = ( char * ) GetBackwardLine ( text -> buf, -line, &len );
	else
	  while (*s++ != '\n')
	    ;
	if ( s ) {
	    if ( text -> sl )
	      s = HscrollString ( s, text );
	    text -> page.sline [ wl ] = s;
	}
	else {
#ifdef DEBUG
	    (void) fprintf ( stderr,"sline null wl = %d\n", wl );
#endif
	    text -> page.sline [ wl ] = ( char * ) 0;
	    n++;
	    continue;
	}
	wl++;
    }
    for ( line = 0; line < text -> n2 + 1 + n; line++ ) {
	if ( line == 0 )
	  s = ( char * ) GetCurrentLine ( text -> buf, &len );
	else if (line == 1)
	  s = ( char * ) GetForwardLine ( text -> buf, line, &len );
	else if (s) {
	  char * b = text -> buf -> bottom;
	  
	  while (*s++ != '\n')
	    if (s >= b)
	      break;

	  if (s == b) {
	    if (*s != '\n')
	      /* ligne sans cr final, appelle GetForwardLine pour ajouter
	         le cr final (eventuellement dans une ligne static */
	      s = GetForwardLine ( text -> buf, line, &len );
	  }
	  else if (s > b)
	    s = 0;
	}
	
	if ( s ) {
	    if ( text -> sl ) {
	      hs = s;
	      s = HscrollString ( s, text );
	      if ( line == 0 )
		text -> page.hs_delta = s - hs;
	    }
	    else {
		if (line == 0)
		  text -> page.hs_delta = 0;  
	    }
	    text -> page.sline [ wl ] = s;
	}
	else {
#ifdef DEBUG
	    (void) fprintf ( stderr,"sline null wl = %d\n", wl );
#endif
	    text -> page.sline [ wl ] = ( char * ) 0;
	}
	wl ++;
    }
    text -> n1 -= n;
    text -> n2 += n;

    if (  TextInBuf ( text ) != True )
	text -> page.sline [ text -> n1 + 1 ] = (char *) 0;
}


/*
**	Function name : SetCurrentLine
**
**	Description : Affiche la ligne courante.
**
**	Input : Le text courant.
**	Ouput :
*/
void SetCurrentLine ( text )
    Text *text;
{
    char *s, *hs;
    int len;
    int line = text -> n1 + 1;
    int max = text -> width - ( 2 * text -> x_or );
    
    ClearLine ( text, line );
    ClipOn ( text, 0 );
    
    s = ( char * ) GetCurrentLine ( text -> buf, &len ); 
    
    if ( s ) {
	if ( text -> sl ) {
	  hs = s;
	  s = (char *) HscrollString ( s, text );
	  text -> page.hs_delta = s - hs;
	}
	else
	  text -> page.hs_delta = 0;

	text -> page.sline [ line ] = s;
	text -> page.wline [ line ] =  DrawLineOfText ( text, line );
	
	if ( text -> page.wline [ line ] > max )
	  text -> page.wline [ line ] = max;
	
    }
    ClipOff ( text );
}


/*
**	Function name : RefreshPage
**
**	Description : Affiche la page courante.
**
**	Input : Le text courant.
**	Ouput :
*/
void RefreshPage ( text )
    Text *text;
{
    int i = 1;
    int max = text -> width - ( 2 * text -> x_or );

    do {
	ClearLine ( text, i );
	if ( text -> page.sline [ i ] ) {
	    text -> page.wline [i] = DrawLineOfText ( text, i );
	    if ( text -> page.wline [ i ] > max )
	      text -> page.wline [ i ] = max;
	}
	else
	  text -> page.wline [ i ] = 0;
	i++;
    } while ( i < text -> lines_in_page + 1);
}


/*
**	Function name : ClearLine
**
**	Description : Efface la ligne i.
**
**	Input : Le text courant, le no de la ligne.
**	Ouput :
*/
void ClearLine ( text, i )
    Text *text;
    int i;
{
    int width;
    
    XSetForeground ( dpy, text -> Cgc, text -> bg );

    if ( (width = text -> page.wline [i]) != 0 ) {
	XFillRectangle ( dpy, text -> window, text -> Cgc,
			text -> x_or, text -> y_or + ( (i - 1) * text -> font_height ),
			width + 1, text -> font_height + 1 );
	
	text -> cursor_stat = OFF;
    }
    
    XSetForeground ( dpy, text -> Cgc, text -> fg );
}


/*
**	Function name : DrawLineOfText
**
**	Description : Affiche la ligne i en
**		tenant compte des tabs.
**
**	Input : Le text courant, le no de ligne.
**	Ouput :
*/
static int DrawLineOfText ( text, li )
    Text *text;
    int li;
{
    int n = 1;  /* Caracteres en cours de traitement    */
    char  *p, *s, *pp, *ppp;
    int xx, width, count, binary_flag = False;
    int sizetab = text -> tab_width * text -> blanc_width;
    int y = text -> y_or + text -> font -> max_bounds.ascent  
      + ( (li -1) * ( text -> font_height ));
    int x = text -> x_or;
    char *end_of_buf = BottomBuf ( text -> buf ); 
    char *top_of_buf = TopBuf ( text -> buf ); 
    int type = False;
    
    s = p = text -> page.sline [li];
    if ( (p >= top_of_buf) && (p <= end_of_buf) ) 
      type = True;
    
    while ( *p != '\n' ) {
	/* Si les caracteres (de deux pixels de large pour etre 
	   tranquille) a afficher depassent la largeur de la fenetre
	   courante, on arrete les frais.
	*/
	if ( n > (text -> width/2) )
	    break;
	/*
	   Si c'est un caractere de largeur nulle et different du tab
	   (caractere de controle) on le remplace par le caractere 191
	   qui est sans doute peu utilise.
	   Cette substitution se fait uniquement pour l'affichage.
	*/
	if ( binary_flag == False ) {
	    if ( (*p != '\t') && (XTextWidth ( text -> font, p, 1 ) == 0) )
		binary_flag = True;
	}

	if ( (type == True) && (p > end_of_buf) )
	  (void) fprintf ( stderr, "Internal Error 1...\n" );
	if ( *p == '\t' ) {
	    if ( n > 1 ) {
		if ( binary_flag == False ) {
		    if ( (DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1)
		      || (text -> current_ce == 0) )
		    XDrawImageString ( dpy, text -> window, text -> Cgc, x, y, s, n-1 );
		  else 
		    ColorLineInPage ( text, x, y, li, s, n-1 );
		}
		else {
		    pp = (char *) malloc ((unsigned int) n );
		    (void) bcopy ( s, pp, n -1 );
		    count = n - 1;
		    ppp = pp;
		    while ( count ) {
			if ( (*pp != '\t') && (XTextWidth ( text -> font, pp, 1 ) == 0) )
			  *pp = (unsigned char) BIZ_CHAR;
			count --;
			pp ++;
		    }
		    pp = ppp;
		    if (n > 1)
		      XDrawImageString ( dpy, text -> window, text -> Cgc, x, y, pp, n-1 );
		}
	    }	      
	    xx =  sizetab;
	    if ( binary_flag == False )
	      width = XTextWidth ( text -> font, s, n-1);
	    else
	      width = XTextWidth ( text -> font, pp, n-1);
	    while ( width >= xx ) 	
	      xx += sizetab;
	    x += xx;
	    p++;
	    n = 1;
	    if ( (type == True) && (p > end_of_buf) )
	      break;
	    s = p;
	}
	else {
	    n++;
	    p++;
	    if ( (type == True) && (p > end_of_buf) ) 
	      break;
	}
    }
    if ( binary_flag == False ) {
	if (n > 1) {
	    if ( (DefaultDepth ( dpy, DefaultScreen ( dpy )) == 1)
		|| (text -> current_ce == 0) )
	      XDrawImageString ( dpy, text -> window, text -> Cgc, x, y, s, n-1 );
	    else 
	      ColorLineInPage ( text, x, y, li, s, n-1 );
	}
    }
    else {
	pp = (char *) malloc ((unsigned int) n );
	(void) bcopy ( s, pp, n -1 );
	p = pp;
	count = n - 1;
	while ( count ) {
	    if ( ((*p != '\t') && (XTextWidth ( text -> font, p, 1 ) == 0)) || (*p==0) )
	      *p = (unsigned char) BIZ_CHAR;
	    p++;
	    count --;
	}
	if ( n > 1 )
	  XDrawImageString ( dpy, text -> window, text -> Cgc, x, y, pp, n-1 );
    }

    if ( binary_flag == False )
      return ( x + XTextWidth ( text -> font, s, n-1 ));
    else {
	x += XTextWidth ( text -> font, pp, n-1 );
	(void) free (pp);
	return (x);
    }
}

/*
**	Function name : LastPage
**
**	Description : Affiche la derniere page du buffer courant.
**
**	Input : Le text courant.
**	Ouput :
*/
void LastPage ( text )
    Text *text;
{
    Buf *buf;
    
    buf = text -> buf;
    if ( text -> stat & LASTPAGE ) {
	DisplayMessage ( text -> mwin, END_MESS );
	MSET ( text -> stat, MESSAGE );
	return;
    }
    HoleToRight ( buf );
    text -> no_current_line = text -> lines_in_buf;
    text -> n1 = text -> lines_in_page - 1;
    text -> n2 = 0;
    
    SetAndDisplayPage ( text );
    (void) MoveScrollBar ( dpy, text -> swin,
			  CURRENT, text -> no_current_line - text -> n1 - 1 );
    DisplayMessage ( text -> mwin, " " );
    MSET ( text -> stat, LASTPAGE );
    MCLEAR ( text -> stat, FIRSTPAGE );
    MCLEAR ( text -> stat, MESSAGE );
    
#ifdef DEBUG
    (void) fprintf ( stderr, "cur_line = %d n1 = %d n2 = %d\n",
		    text -> no_current_line, text -> n1, text -> n2 );
#endif
}


/*
**	Function name : PreviousPage
**
**	Description : Affiche la page precedente.
**
**	Input : Le text courant.
**	Ouput :
*/
void PreviousPage ( text )
    Text *text;
{
    Buf *buf;
    int len;
    
    buf = text -> buf;
    
#ifdef DEBUG
    (void) fprintf ( stderr, "n1 = %d ncl = %d\n",
		    text -> n1, text -> no_current_line );
#endif
    if ( (text -> no_current_line == 1)
	|| (! GetBackwardLine ( buf, - ( text -> n1 + 1 ), &len ))) {
	DisplayMessage ( text -> mwin, "zz top..." );
	MSET ( text -> stat, MESSAGE );
	MSET ( text -> stat, FIRSTPAGE );
	return;
    }
    
    if ( MoveToLine ( buf, - text -> n1 ) == -1 ) {
	(void) fprintf ( stderr, "Move error\n" );
	return;
    }
    text -> no_current_line -= text -> n1;
    text -> n1 = text -> lines_in_page - 1;
    text -> n2 = 0;
    
    SetAndDisplayPage ( text );
    
    if ( text -> no_current_line - text -> n1 == 1 )
      (void) MoveScrollBar ( dpy, text -> swin, FIRST, 0 );
    else
      (void) MoveScrollBar ( dpy, text -> swin, PREVIOUS, 0 );
    
    DisplayMessage ( text -> mwin, " " );
    MCLEAR ( text -> stat, MESSAGE );
    MCLEAR ( text -> stat, FIRSTPAGE );
    MCLEAR ( text -> stat, LASTPAGE );
    
#ifdef DEBUG
    (void) fprintf ( stderr, "cur_line = %d n1 = %d n2 = %d\n",
		    text -> no_current_line, text -> n1, text -> n2 );
#endif
    
}


/*
**	Function name : NextPage
**
**	Description : Affiche la page suivante.
**
**	Input : Le text courant.
**	Ouput :
*/
void NextPage ( text )
    Text *text;
{
    Buf *buf;
    
    buf = text -> buf;
    
#ifdef DEBUG
    (void) fprintf ( stderr, "n2 = %d\n", text -> n2 );
#endif
    if ( text -> stat & LASTPAGE ) {
	DisplayMessage ( text -> mwin, END_MESS );
	MSET ( text -> stat, MESSAGE );
	return;
    }
    
    if ( MoveToLine ( buf, text -> n2 ) == -1 ) { 
	DisplayMessage ( text -> mwin, END_MESS );
	MSET ( text -> stat, LASTPAGE );
	MSET ( text -> stat, MESSAGE );
	
	return;
    }
    
    text -> no_current_line += text -> n2;
    text -> n1 = 0;
    text -> n2 = text -> lines_in_page - 1;
    
    SetAndDisplayPage ( text );
    
    (void) MoveScrollBar ( dpy, text -> swin, NEXT, 0 );
    
    DisplayMessage ( text -> mwin, " " );
    MCLEAR ( text -> stat, MESSAGE );
    MCLEAR ( text -> stat, FIRSTPAGE );
    MCLEAR ( text -> stat, LASTPAGE );
    
#ifdef DEBUG
    (void) fprintf ( stderr, "cur_line = %d n1 = %d n2 = %d\n",
		    text -> no_current_line, text -> n1, text -> n2 );
#endif
}

/*
**	Function name : GotoEndOfBuf
**
**	Description : 
**	Input : 
**	Ouput :
*/
void GotoEndOfBuf ( text )
    Text *text;
{
    if (! TextInBuf(text))
      return;
    StorePosition(text);
    HoleToRight ( text -> buf );
    GotoLineNumber ( text, text -> lines_in_buf );
    SetPosition(text);
    UpdatePage(text);
}

/*
**	Function name : CurrentLineToTop
**
**	Description : Met la ligne courante au debut de la
**		page.
**
**	Input : Le text courant.
**	Ouput :
*/
void CurrentLineToTop ( text )
    Text *text;
{
    int n;
    
    text -> n1 = 0;
    text -> n2 = text -> lines_in_page - 1;
    
    SetAndDisplayPage ( text );
    
    n = text -> no_current_line - text -> n1 - 1;
    
    if ( n > 0 )
      (void) MoveScrollBar ( dpy, text -> swin, CURRENT, n );
    else
      (void) MoveScrollBar ( dpy, text -> swin, FIRST, 0 );
}


/*
**	Function name : CurrentLineToTopFromMenu
**
**	Description : 
**	Input : 
**	Ouput :
*/
void CurrentLineToTopFromMenu ( text )
    Text *text;
{
    TextCursorOff ( text );
    CurrentLineToTop ( text );
    TextCursorOn ( text );
}

/*
**	Function name : GotoLineNumber
**
**	Description : Va a la ligne n.
**
**	Input : Le text courant, le no de ligne.
**	Ouput :
*/
void GotoLineNumber ( text, n )
    Text *text;
    int n;
{
    if ( n < 1 ) return;
    if ( n > text -> lines_in_buf ) 
      n = text -> lines_in_buf;
    
    text -> sl = 0;
    (void) MoveToLine ( text -> buf, n - text -> no_current_line );
    
    text -> no_current_line += ( n - text -> no_current_line );

    MCLEAR ( text -> stat, MESSAGE );
    MCLEAR ( text -> stat, FIRSTPAGE );
    MCLEAR ( text -> stat, LASTPAGE );
}


/*
**	Function name : ScrollNline
**
**	Description : Scroll n lignes de texte.
**		Vers le debut ou la fin du buffer suivant
**		le signe de n.
**
**	Input : Le nombre de ligne a scroller, le text courant.
**	Ouput :
*/
void ScrollNLine ( n, text )
    int n;
    Text *text;
{
    Buf *buf;
#define PAS 5
    
    buf = text -> buf;
    if ( n < 0 ) {        /* On va vers le debut du buffer */
	if ( -n > PAS ) {
	    (void) MoveToLine ( buf, n );
	    text -> no_current_line -= (-n);
	    if ( text -> no_current_line < text -> n1 ) {
		text -> n1 = text -> no_current_line - 1;
		text -> n2 = text -> lines_in_page - 1 - text -> n1;
	    }
	    SetAndDisplayPage ( text );
	}
	else
	  (void) ScrollUpFewLines ( text, n );
    }
    else {             /* On va vers ls fin du buffer */
	if ( n > PAS ) {
	    int tmp;
	    tmp = text -> lines_in_buf - text -> no_current_line;
	    if ( n <= tmp ) {
		(void) MoveToLine ( buf, n );
		text -> no_current_line += n;
	    }
	    else {
		(void) MoveToLine ( buf, tmp );
		text -> no_current_line += tmp;
		text -> n1 -= ( n - tmp );
		text -> n2 = text -> lines_in_page - 1 - text -> n1;
	    }
	    SetAndDisplayPage ( text );
	}
	else 
	  (void) ScrollDownFewLines ( text, n );
    }
}
		       

/*
**	Function name : ScrollUpFewLines
**
**	Description : Scroll n lignes. Le texte defile
**		vers le bas.
**
**	Input : Le text courant, le nombre de ligne a scroller.
**	Ouput :
*/
static void ScrollUpFewLines ( text, n )
    Text *text;
    int n;
{
    Buf *buf;
    int len;
    
    buf = text -> buf;
#ifdef DEBUG
    (void) fprintf ( stderr, "Up n = %d\n", n );
#endif
    ClipOn ( text, 0 );
    while ( n ) {
	(void) ShiftLinesTable ( text, DOWN );
	text -> page.sline [1] = 
	  GetBackwardLine ( buf, - ( text -> n1 + 1 ), &len ); 

/*	RefreshPage ( text ); */
	if ( text -> n2 == 0 ) {
	    (void) MoveToLine ( buf, -1 );
	    text -> no_current_line --;
	}
	else {
	    text -> n1 ++;
	    text -> n2 --;
	}
	RefreshPage ( text );
	n++;
    }
    ClipOff ( text );
    
    DisplayMessage ( text -> mwin, " " );
    MCLEAR ( text -> stat, MESSAGE );
    MCLEAR ( text -> stat, FIRSTPAGE );
    MCLEAR ( text -> stat, LASTPAGE );
    
#ifdef DEBUG
    (void) fprintf ( stderr, "cur_line = %d n1 = %d n2 = %d\n",
		    text -> no_current_line, text -> n1, text -> n2 );
#endif
}


/*
**	Function name : ScrollDownFewLines
**
**	Description : Scroll n ligne. Le texte defile vers
**		le haut.
**
**	Input : Le text courant, le nombre de lignes a scroller.
**	Ouput :
*/
static void ScrollDownFewLines ( text, n )
    Text *text;
    int n;
{
    Buf *buf;
    int len;
    
    buf = text -> buf;
#ifdef DEBUG
    (void) fprintf ( stderr, "Down n = %d\n", n );
#endif
    ClipOn ( text, 0 );
    while ( n ) {
	(void) ShiftLinesTable ( text, UP );
	text -> page.sline [text -> lines_in_page] = 
	  GetForwardLine ( buf, text -> n2 + 1, &len );

/*	RefreshPage ( text ); */
	if ( text -> n1 == 0 ) {
	    (void) MoveToLine ( buf, 1 );
	    text -> no_current_line ++;
	}
	else {
	    text -> n1 --;
	    text -> n2 ++;
	}
	RefreshPage ( text );
	n--;
    }
    ClipOff ( text );
    DisplayMessage ( text -> mwin, " " );
    MCLEAR ( text -> stat, MESSAGE );
    MCLEAR ( text -> stat, FIRSTPAGE );
    MCLEAR ( text -> stat, LASTPAGE );
    
#ifdef DEBUG
    (void) fprintf ( stderr, "cur_line = %d n1 = %d n2 = %d\n",
		    text -> no_current_line, text -> n1, text -> n2 );
#endif
}


/*
**	Function name : ShiftLinesTable
**
**	Description : Delalage des pointeurs de la table
**		des lignes dans le cas d'un scroll.
**
**	Input : Le text courant, la direction du scroll.
**	Ouput :
*/
static void ShiftLinesTable ( text, dir )
    Text *text;
    int dir;
{
    char  *save[256];
    int i = 1;
    
    do {
	save [i] = text -> page.sline [i];
	i++;
    } while ( i < text -> lines_in_page + 1 );
    
    switch ( dir ) {
    case UP: 
      i = 1;
      while ( i < text -> lines_in_page ) {
	  text -> page.sline [i] = save [i+1];
	  i++;
      }
      break;
    case DOWN:
      i = 1;
      while ( i < text -> lines_in_page ) {
	  text -> page.sline [i+1] = save [i];
	  i++;
      }
      break;
    }
}


/*
**	Function name : CurrentLineToMiddle
**
**	Description : Met la ligne courante au milieu
**		de la page.
**
**	Input : Le text courant.
**	Ouput :
*/
void CurrentLineToMiddle ( text )
    Text *text;
{
    int n;
    
    if ( text -> no_current_line >= ( text -> lines_in_page / 2 )) { 
	text -> n1 = text -> lines_in_page / 2;
	text -> n2 = text -> lines_in_page - text -> n1 - 1;
    }
    else {
	text -> n1 = text -> no_current_line - 1;
	text -> n2 = text -> lines_in_page - text -> n1 - 1;
    }
    
    SetAndDisplayPage ( text );
    
    n = text -> no_current_line - text -> n1 - 1;
    
#ifdef DEBUG
    (void) fprintf ( stderr, "n = %d n1 = %d n2 = %d current = %d\n", 
		    n, text -> n1, text -> n2, text -> no_current_line );
#endif
    
    if ( n != 0 )
      (void) MoveScrollBar ( dpy, text -> swin, CURRENT, n );
    else
      (void) MoveScrollBar ( dpy, text -> swin, FIRST, 0 );
    
    MCLEAR ( text -> stat, MESSAGE );
    MCLEAR ( text -> stat, FIRSTPAGE );
    MCLEAR ( text -> stat, LASTPAGE );
}


/*
**	Function name : SetAndDisplayPage
**
**	Description : Met la table des lignes a jour,
**		et affiche la page courante.
**
**	Input : Le text courant.
**	Ouput :
*/
void SetAndDisplayPage ( text )
    Text *text;
{
    ClipOn ( text, 0 );
    (void) SetLinesTable ( text );
    RefreshPage ( text );
    ClipOff ( text );
}


/*
**	Function name : ScrollDownCont
**
**	Description : Scroll tant que le 'ButtonPress' est 
**		maintenu. Le Texte defile vers le haut.
**	Input : 
**	Ouput :
*/
void ScrollDownCont ( text )
    Text *text;
{
    XEvent event;
    
    for (;;) {
	if ( XCheckMaskEvent ( dpy,  ButtonReleaseMask, &event ) == True )
	  break;
	if ( text -> no_current_line == text -> lines_in_buf ) continue;
	ScrollNLine ( 1, text );
	(void) MoveScrollBar ( dpy, text -> swin, CURRENT,
			      text -> no_current_line - text -> n1 - 1 );
	XSync ( dpy, False );
    }
}


/*
**	Function name : ScrollUpCont
**
**	Description : Scroll tant que le 'ButtonPress' est
**		maintenu. Le Texte defile vers le bas.
**
**	Input : Le text courant.
**	Ouput :
*/
void ScrollUpCont ( text )
    Text *text;
{
    XEvent event;
    
    for (;;) {
	if ( XCheckMaskEvent ( dpy,  ButtonReleaseMask, &event ) == True )
	  break;
	if ( (text -> no_current_line - text -> n1) == 1 ) {
	    (void) MoveScrollBar ( dpy, text -> swin, FIRST, 0 );
	    continue;
	}
	ScrollNLine ( -1, text );
	(void) MoveScrollBar ( dpy, text -> swin, CURRENT,
			      text -> no_current_line - text -> n1 - 1 );
	XSync ( dpy, False );
    }
}


/*
**	Function name : klaxon
**
**	Description : 
**	Input : 
**	Ouput :
*/
void klaxon ()
{
    XBell ( dpy, 5 );
}

/*
**	Function name : UpdatePage
**
**	Description : Mise a jour de la table des lignes, de la
**            scrollbar et raffraissement de la page courante
**            avec la ligne courante au milieu de la page.
**	Input : 
**	Ouput :
*/
void UpdatePage ( text )
    Text *text;
{
    int n;
    
    TextCursorOff ( text );
    
    SetAndDisplayPage ( text ); 
    
    SetScrollLine ( text -> swin , text -> lines_in_buf );
    ShowScrollFrame ( dpy, text -> swin );
    if ( (n=(text -> no_current_line - text -> n1 - 1)) != 0 )
      (void) MoveScrollBar ( dpy, text -> swin, CURRENT, n );
    else {
	(void) MoveScrollBar ( dpy, text -> swin, FIRST, 0 );
	MCLEAR ( text -> stat, LASTPAGE );
    }
    
    TextCursorOn ( text );
}


/*
**	Function name : StorePosition
**
**	Description : 
**	Input : 
**	Ouput :
*/
void StorePosition ( text )
    Text *text;
{
    text -> old_n1 = text -> n1;
    text -> old_n2 = text -> n2;
    text -> old_current_line = text -> no_current_line;

#ifdef DEBUG
    fprintf ( stderr, "Store Position oldn = %d oldn1 = %d oldn2 = %d\n",
	     text -> old_current_line, text -> old_n1, text -> old_n2 );
#endif /* DEBUG */
}


/*
**	Function name : SetPosition
**
**	Description : 
**	Input : 
**	Ouput :
*/
void SetPosition ( text )
    Text *text;
{
    int delta = text -> old_current_line - text -> no_current_line;
#ifdef DEBUG    
    fprintf ( stderr, "Set position old_n1 = %d old_n2 = %d n1 = %d n2 = %d\n",
	  text -> old_n1, text -> old_n2, text -> n1, text -> n2 );
#endif /* DEBUG */  

    /* Si ya eu modification depuis StorePosition
       on se casse. (c'est le cas avec ie_forward_search,
       ie_backward_search).
       */
/*    if ( text -> old_n1 != text -> n1 ) 
      return;
*/    
    if ( delta > 0 ) {
	/* Deplacement vers le debut du buffer */
	if ( delta > text -> old_n1 ) {
	    if ( text -> no_current_line >= ( text -> lines_in_page / 2 )) { 
		text -> n1 = text -> lines_in_page / 2;
		text -> n2 = text -> lines_in_page - text -> n1 - 1;
	    }
	    else {
		text -> n1 = text -> no_current_line - 1;
		text -> n2 = text -> lines_in_page - text -> n1 - 1;
	    }
#ifdef DEBUG
    fprintf ( stderr, "1 delta = %d n = %d n1 = %d n2 = %d\n",
	     delta, text -> no_current_line, text -> n1, text -> n2 );
#endif /* DEBUG */
	}
	else {
/*	  
	    text -> n1 -= delta;
	    text -> n2 += delta;
*/	    
	    text -> n1 = text -> old_n1 - delta;
	    text -> n2 = text -> old_n2 + delta;
	}
#ifdef DEBUG
    fprintf ( stderr, "2 delta = %d n = %d n1 = %d n2 = %d\n",
	     delta, text -> no_current_line, text -> n1, text -> n2 );
#endif /* DEBUG */
    }
    else if ( delta < 0 ) {
	/* Deplacement vers la fin du buffer */
	delta *= -1;
	if ( delta > text -> old_n2 ) {
	    if ( text -> no_current_line >= ( text -> lines_in_page / 2 )) { 
		text -> n1 = text -> lines_in_page / 2;
		text -> n2 = text -> lines_in_page - text -> n1 - 1;
	    }
	    else {
		text -> n1 = text -> no_current_line - 1;
		text -> n2 = text -> lines_in_page - text -> n1 - 1;
	    }
#ifdef DEBUG
    fprintf ( stderr, "3 delta = %d n = %d n1 = %d n2 = %d\n",
	     delta, text -> no_current_line, text -> n1, text -> n2 );
#endif /* DEBUG */
	}
	else {
/*	  
	    text -> n1 += delta;
	    text -> n2 -= delta;
*/	    
	    text -> n1 = text -> old_n1 + delta;
	    text -> n2 = text -> old_n2 - delta;
	  
#ifdef DEBUG
    fprintf ( stderr, "4 delta = %d n = %d n1 = %d n2 = %d\n",
	     delta, text -> no_current_line, text -> n1, text -> n2 );
#endif /* DEBUG */
	}
    }
    else {
#ifdef DEBUG
      fprintf ( stderr, "delta = 0\n");
#endif /* DEBUG */
      text -> n1 = text -> old_n1;
      text -> n2 = text -> old_n2;
      return;
    }
}



/*
**	Function name : RunScroll
**
**	Description : 
**	Input : 
**	Ouput :
*/
void RunScroll ( text, result )
    Text *text;
    int result;
{
    SetAndDisplayPage ( text );
    
    switch ( result ) {
    case CURSOR:
      if ( GetCursorStat (  text ) != OFF )
	TextCursorOff (  text );
      if ( GetScrollStat(  text ))
	GotoLeft (  text );
      if ( (TextInBuf (  text ) == True) 
	  && ( GetNbLinesInBuf (  text ) > 1))			
	HandleScrollBar ( dpy, text -> swin, ScrollNLine );
      if ( GetCursorStat (  text ) != ON )
	TextCursorOn (  text );
      RefreshScrollBar ( dpy, text -> swin ); 
      break;
    case NEXT:
      TextCursorOff (  text );
      NextPage (  text  );
      TextCursorOn (  text );
      break;
    case PREVIOUS:
      TextCursorOff (  text );
      PreviousPage (  text );
      TextCursorOn (  text );
      break;
    }
    return;
    
}

/*
**	Function name : UpdateTextItem
**
**	Description : 
**	Input : 
**	Ouput :
*/
void UpdateTextItem ( text, n )
    Text *text;
    int n;
{
    int y;
    
    if ( (n == 0)
	|| ( n < (text -> no_current_line - text -> n1))
	|| ( n > (text -> no_current_line + text -> n2)))
      return;
    
    y = n - ( text -> no_current_line - text -> n1 );
    y *= text -> font_height;
    y += text -> y_or;
    
    XFillRectangle ( dpy, text -> window,
		    text -> Igc, text -> x_or,  y, 
		    text -> width - ( 2 * text -> x_or ),
		    text -> font_height + 1 );
    
}

/*
**	Function name : SelectTextItem
**
**	Description : Eteint l'item courant et allume l'item
**         correspondant au click
**  
**	Input : Le Text courant, la position du click, l'item courant.
**	Ouput : Retourne le numero de l'item selectionne
*/
int SelectTextItem ( text, x, y, select )
    Text *text;
    int x, y, select;
{
    int n;
    int scroll = text -> sl;
    
    if ( (text -> lines_in_buf == 1) 
	|| MoveToXYinTextWindow (  text,  x, y ) == -1 )
      return 0;
    
    TextCursorOn ( text );
    TextCursorOff ( text );
    
    if ( (n = text -> no_current_line) >= text -> lines_in_buf ) {
	return 0;
    }
    
    if ( text -> sl == scroll ) {
	UpdateTextItem ( text, select ); /* Eteint l'item courant */
    }
    UpdateTextItem ( text, n );	 /* Allume l'item selectionne */
    
    return n;
}

/*
**	Function name : CurrentTextItem
**
**	Description : 
**	Input : 
**	Ouput : 
*/
char *CurrentTextItem ( text )
    Text *text;
{
    int len;
    char *str, *tmp;
    
    tmp = (char *) GetCurrentLine ( text -> buf, &len );
    if ( (tmp == 0) || (len == 0) )
      return 0;
    tmp [len] = 0;
    str = (char *) malloc ( (unsigned) len + 2 );
    (void) sprintf ( str, "%s", tmp );
    return (str);
    
}

/*
**	Function name : FirstPageAndUpdateScroll
**
**	Description : 
**	Input : 
**	Ouput :
*/
void FirstPageAndUpdateScroll ( text )
    Text *text;
{
    ClipOn ( text, 0 );
    HoleToLeft ( text -> buf ); 
    text -> lines_in_buf = GetNumberOfLineInBuf ( text -> buf );
    SetScrollLine ( text -> swin, text -> lines_in_buf );
    ShowScrollFrame ( dpy, text -> swin );
    FirstPage ( text );
    RefreshScrollBar ( dpy, text -> swin );
    ClipOff ( text );
}

/*
**	Function name : RefreshPageAndUpdateScroll
**
**	Description : 
**	Input : 
**	Ouput :
*/
void RefreshPageAndUpdateScroll ( text )
    Text *text;
{
    XClearWindow ( dpy, text -> window );
    Display3D ( dpy, text -> window, text -> top_sh, text -> bot_sh, 2, 1 );
    SetAndDisplayPage ( text );
    ShowScrollFrame ( dpy, text -> swin );
}


/*
**	Function name : RunScrollAndUpdateItem
**
**	Description : 
**	Input : 
**	Ouput :
*/
void RunScrollAndUpdateItem ( text, select, s_type )
    Text *text;
    int select, s_type;
{
    UpdateTextItem ( text, select );
    RunScroll ( text, s_type );
    TextCursorOn ( text );
    TextCursorOff ( text );
    UpdateTextItem ( text, select );
}

/*
**	Function name : AtLineDisplayPage
**
**	Description : 
**	Input : 
**	Ouput :
*/
void AtLineDisplayPage ( text, line )
    Text *text;
    int line;
{
    TextCursorOff ( text );
    GotoLineNumber ( text, line );
    CurrentLineToTop ( text );
    TextCursorOn ( text );
}

/*
**	Function name : WatchOn
**
**	Description : 
**	Input : 
**	Ouput :
*/
void WatchOn ( win )
    Window win;
{
  if ( win == 0 )
    XDefineCursor ( dpy, 
		    DefaultRootWindow ( dpy ),
		   XCreateFontCursor ( dpy, XC_watch ) );
  else
    XDefineCursor ( dpy, win, XCreateFontCursor ( dpy, XC_watch ) );
  
  XFlush (dpy ); 
}

/*
**	Function name : WatchOff
**
**	Description : 
**	Input : 
**	Ouput :
*/
void WatchOff ( win )
    Window win;
{
  if ( win == 0 )
    XUndefineCursor ( dpy,  DefaultRootWindow ( dpy ) );
  else
    XUndefineCursor ( dpy, win );	
}


