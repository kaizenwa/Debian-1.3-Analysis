/* ########################################################################

			      color_area.c

   File: color_area.c
   Path: /home/fournigault/c/X11/xcoral-2.31/color_area.c
   Description: 
   Created: Fri Jan 27 10:54:22 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:54:23 MET 1995
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
#ifdef __FreeBSD__
#include <stdlib.h>
#else /* !__FreeBSD__ */
#include <malloc.h>
#endif
#include <string.h>

#include "main_text.h"
#include "color_area.h"

extern Display *dpy;
/*
#define DEBUG2
#define DEBUG
*/
FCT (static void, InsertInsideElement, (Text *text, ColorElement *new_ce, ColorElement *ce) );
FCT (static void, InsertWithRightElement, (Text *text, ColorElement *new_ce, ColorElement *ce) );
FCT (static void, InsertWithLeftElement, (Text *text, ColorElement *new_ce, ColorElement *ce) );
FCT (static void, DisplayColorList, (Text *text) );
FCT (static void, CheckColorList, (Text *text, ColorElement *new_ce) );
FCT (static ColorElement *, CreateColorElement, (int start, int end, unsigned long color ) );
FCT (static void, DeleteColorElement, (Text *text, ColorElement *ce ) );
FCT (static void, IncrColorList, (Text *text, int nbytes) );
FCT (static void, DecrColorList, (Text *text, int nbytes) );

/*
**	Function name : SetColorArea
**
**	Description : Construction et mise a jour de la liste des
**          elements colories. Cette liste est a double chainage.
**          Si la couleur est non valide on ne fait rien.
**  
**	Input : Le Text courant, le debut et la fin (au sens Smac)
**         de la zone a colorier et la couleur de l'element.
**	Output :
*/
void SetColorArea ( text, start, end, colorname )
    Text *text;
    int start, end;
    char *colorname;
{
    ColorElement *new_ce, *tmp, *tmp1;
    int e_file = ie_end_of_file(text);
    extern Display *dpy;
    XColor  color, tcolor;
    int status;
    
    if ( (! TextInBuf (text)) || ((e_file-1) < end) )
      return;
    
    status = XAllocNamedColor ( dpy,
	       DefaultColormap ( dpy, DefaultScreen ( dpy ) ), 
	       colorname, &color, &tcolor );

    if ( status == False )
      return;

    new_ce = CreateColorElement ( start, end, (unsigned long) color.pixel );
    
    if ( text -> current_ce == 0 ) {
	/* C'est le premier element de couleur */
	text -> last_ce = new_ce;
	text -> current_ce = new_ce;
#ifdef DEBUG    
	DisplayColorList (text);
#endif /* DEBUG */    
	return;
    }

    tmp = text -> current_ce;
    for(;;) {
	tmp1 = tmp;
#ifdef DEBUG    
	(void) fprintf (stderr,"tmp = %d\n", tmp);
#endif /* DEBUG */    
	if ((start >= tmp -> pos) && (start <= (tmp -> pos + tmp -> len -1))) {
	    /* On est a l'interieur d'un element */
	    InsertInsideElement ( text, new_ce, tmp );
	    break;
	}
	else if (start > (tmp -> pos + tmp -> len -1)) {
	    /* On va plus loin si possible */
	    if ( ((tmp = tmp -> next) == 0)
		|| (start < tmp -> pos) ) {
		InsertWithLeftElement ( text, new_ce, tmp1);
		break;
	    }
	}
	else {
	    /* On va sur l'element precedent si possible */
	    if ( ((tmp = tmp -> previous) == 0)
		|| (start > (tmp -> pos + tmp -> len -1))) {
		InsertWithRightElement ( text, new_ce, tmp1);
		break;
	    }
	}
    }
    text -> current_ce = new_ce;
#ifdef DEBUG    
	(void) fprintf (stderr,"new_ce = %d new_ce_next = %d new_ce_previous = %d\n",
			new_ce, new_ce -> next, new_ce -> previous);
#endif /* DEBUG */    
    
    CheckColorList ( text, new_ce );
#ifdef DEBUG    
    DisplayColorList (text);
#endif /* DEBUG */    
}

/*
**	Function name : CreateColorElement
**
**	Description :
**	Input :
**	Output :
*/
static ColorElement *CreateColorElement ( start, end, color )
    int start, end;
    unsigned long color;
{
    ColorElement *new_ce;
    
    new_ce = (ColorElement *) malloc ( (unsigned) sizeof(ColorElement) + 2);
    new_ce -> pos = start;
    new_ce -> len = end - start + 1;
    new_ce -> color = color;
    new_ce -> next = 0;
    new_ce -> previous = 0;
    
    return (new_ce);
}


/*
**	Function name : InsertWithLeftElement
**
**	Description : Attache un element par la gauche
**	Input : Le text courant, l'element a attacher et
**          l'element qui sera situe a gauche.  
**	Output :
*/
static void InsertWithLeftElement ( text, new_ce, ce)
    Text *text;
    ColorElement *new_ce, *ce;
{
    if (ce == 0) {
#ifdef DEBUG    
	(void) fprintf (stderr," BRRR...InsertWithLeftElement\n");
#endif /* DEBUG */    
      return;
    }
#ifdef DEBUG    
    (void) fprintf (stderr,"  InsertWithLeftElement\n");
#endif /* DEBUG */    

    new_ce -> previous = ce;
    new_ce -> next = ce -> next;
    ce -> next = new_ce;
    if (new_ce -> next)
      (new_ce -> next) -> previous = (ColorElement *) new_ce;

    if (ce == text -> last_ce) {
#ifdef DEBUG    
    (void) fprintf (stderr,"  InsertWithLeftElement Last\n");
#endif /* DEBUG */    
      new_ce = text -> last_ce;
    }
}

/*
**	Function name : InsertWithRightElement
**
**	Description : Attache un element par la droite
**	Input : Le text courant, l'element a attacher et
**          l'element qui sera situe a droite. 
**	Output :
*/
static void InsertWithRightElement ( text, new_ce, ce)
    Text *text;
    ColorElement *new_ce, *ce;
{
    if (ce == 0) {
#ifdef DEBUG    
    (void) fprintf (stderr," BRRR...InsertWithRightElement\n");
#endif /* DEBUG */
      return;
    }
    
    new_ce -> previous = ce -> previous;
    new_ce -> next = ce;
    ce -> previous = new_ce;
    if ( new_ce -> previous )
      (new_ce -> previous) -> next = (ColorElement *) new_ce;
#ifdef DEBUG    
    (void) fprintf (stderr,"  InsertWithRightElement\n");
    (void) fprintf (stderr,"\tnew_ce_next = %d new_ce_previous = %d\n",
		  new_ce -> next, new_ce -> previous );
#endif /* DEBUG */
}


/*
**	Function name : InsertInsideElement
**
**	Description : Le nouvel element a inserer se trouve
**          dans un element existant.
**	Input : Le text courant, le nouvel element et l'ancien.
**	Output :
*/
static void InsertInsideElement (text, new_ce, ce)
    Text *text;
    ColorElement *new_ce, *ce;
{
    ColorElement *tmp;

    if (new_ce -> pos == ce -> pos) {
	if (new_ce -> len >= ce -> len) {
#ifdef DEBUG    
	    (void) fprintf (stderr,"  InsertInsideElement 1 \n");
#endif /* DEBUG */    
	    InsertWithRightElement (text, new_ce, ce);
	}
	else {
#ifdef DEBUG    
    (void) fprintf (stderr,"  InsertInsideElement 2\n");
#endif /* DEBUG */    
	    /* La position de l'element ce ainsi que sa longeur
	       doivent etre modifies.
	    */
	    ce -> pos += new_ce -> len;
	    ce -> len -= new_ce -> len;
	    InsertWithRightElement ( text, new_ce, ce );
	}
    }
    else if (new_ce -> pos == (ce -> pos + ce -> len -1)) {
#ifdef DEBUG    
    (void) fprintf (stderr,"  InsertInsideElement 3\n");
#endif /* DEBUG */    
	ce -> len -= 1;
	InsertWithLeftElement ( text, new_ce, ce );
    }
    else {
	/* A l'interieur */
	if ((new_ce -> pos + new_ce -> len -1) < (ce -> pos + ce -> len -1 )) {
#ifdef DEBUG    
    (void) fprintf (stderr,"  InsertInsideElement 4\n");
#endif /* DEBUG */    
	    /* Il faut couper en deux l'element ce */
	    tmp = CreateColorElement ( new_ce -> pos + new_ce -> len,
				      ce -> pos + ce -> len - 1, ce -> color );
	    InsertWithLeftElement ( text, new_ce, ce );
	    InsertWithLeftElement ( text, tmp, new_ce );

	    ce -> len = new_ce -> pos - ce -> pos;
	}
	else {
#ifdef DEBUG    
    (void) fprintf (stderr,"  InsertInsideElement 5\n");
#endif /* DEBUG */    
	    ce -> len = new_ce -> pos - ce -> pos;
	    InsertWithLeftElement ( text, new_ce, ce );
	}
    }
}

/*
**	Function name : CheckColorList
**
**	Description : Apres insertion d'un element, on
**          vire ceux qui ne sont plus necessaires.
**          (l'element nouveau peut en recouvrir d'autres)
**	Input : Le text courant, l'element insere.
**	Output :
*/
static void CheckColorList ( text, new_ce )
    Text *text;
    ColorElement *new_ce;
{
    ColorElement *tmp = new_ce;
    ColorElement *previous;
    
    while (tmp = tmp -> next) {
	previous = tmp -> previous;
	if ( (previous -> pos +  previous -> len -1) < tmp -> pos )
	  break;
	else if ((previous -> pos +  previous -> len -1) >= (tmp -> pos + tmp -> len -1 )) {
#ifdef DEBUG    
	    (void) fprintf (stderr,"  Recouvrement total\n");
#endif /* DEBUG */
	    DeleteColorElement (text, tmp);
	    tmp = previous;
	}
	else {
#ifdef DEBUG    
    (void) fprintf (stderr,"  Recouvrement partiel\n");
#endif /* DEBUG */    
	    /* Il faut changer la position et la longueur */
	    tmp -> len -= previous -> pos +  previous -> len - tmp -> pos;
	    tmp -> pos = previous -> pos +  previous -> len;
	    break;
	}
    }
    if (new_ce -> next == 0)
      text -> last_ce = new_ce;
}

/*
**	Function name : DeleteColorElement
**
**	Description : Retire un element de la liste. 
**         Le chainage doit etre mis a jour.  
**	Input : Le text courant et l'element a enlever.
**	Output :
*/
static void DeleteColorElement ( text, ce )
    Text *text;
    ColorElement *ce;
{
    if (ce == 0)
      return;

    if (ce -> next)
	(ce -> next) -> previous = (ColorElement *) ce -> previous;
    if (ce -> previous)
	(ce -> previous) -> next = (ColorElement *) ce -> next;
    
    if (ce == text -> last_ce ) {
	if (ce -> previous)
	  text -> last_ce = ce -> previous;
	else {
	   text -> last_ce = 0;
	   text -> current_ce = 0;
	}
    }
#ifdef DEBUG    
    (void) fprintf (stderr,"Free %d\n", ce);
    DisplayColorList (text);
#endif /* DEBUG */    
    (void) free (ce);
}


/*
**	Function name : DisplayColorList
**
**	Description : Pour le DEBUG
**	Input :
**	Output :
*/
static void DisplayColorList (text)
    Text *text;
{
    ColorElement *tmp;
    
    if ((tmp = text -> current_ce) == 0 ) {
      (void) fprintf ( stderr, "Color list empty\n");
      return;
    }

    (void) fprintf ( stderr, "\tce = %d ce -> previous %d\n", 
		    tmp, tmp -> previous );
    while (tmp -> previous != 0) {
      tmp = tmp -> previous;
    }

    /* On est bien sur le premier element de la liste */
    do {
      (void) fprintf ( stderr, "======tmp %d pos = %d len = %d\n",
		      tmp, tmp -> pos, tmp -> len );
      tmp = tmp -> next;
    } while (tmp != 0);
    (void) fprintf ( stderr, "\n");

}


/*
**	Function name : DeleteColorList
**
**	Description : Detruit la liste pour le buffer courant.
**  
**	Input : Le contexte courant.
**	Output :
*/
void DeleteColorList ( text )
    Text *text;
{
    ColorElement *tmp = text -> current_ce;
    ColorElement *todel;

    if ( tmp == 0 )
      return;
    
    while (tmp -> next != 0)
      tmp = tmp -> next;

    /* ont est bien sur le dernier element de la liste */
    
    do {
      todel = tmp;
      tmp = tmp -> previous;
#ifdef DEBUG
      (void) fprintf ( stderr, "free %d\n", todel );
#endif /* DEBUG */    
      (void) free (todel);
    } while (tmp != 0) ;
    text -> current_ce = 0;
}

/*
**	Function name : UpdateColorList
**
**	Description : Mises a jour de la liste des elements colories
**         apres une modification du buffer (add ou del n caracteres)  
**         
**	Input : Le contexte courant, nbytes caracteres a ajouter (>0)
**         ou a enlever (<0) a partir de la position courante.
**	Output :
*/
void UpdateColorList ( text, nbytes )
    Text *text;
    int nbytes;
{
    ColorElement *tmp = text -> last_ce;

    if ( nbytes == 0 || tmp == 0 || text -> current_ce == 0 )
      return;
    
    if ( nbytes > 0 )
      IncrColorList (text, nbytes);
    else
      DecrColorList (text, -nbytes);
}

/*
**	Function name : IncrColorList
**
**	Description :
**	Input :
**	Output :
*/
static void IncrColorList (text, nbytes)
    Text *text;
    int nbytes;
{
    ColorElement *tmp = text -> last_ce;
    int pos_before;

/* #define DEBUG */

    pos_before = ie_current_position (text) - nbytes;

    /*
       Il faut remonter jusqu'a la position
       courante en modifiant la position de chaque element.
    */  
    do {
	if ((tmp -> pos + tmp -> len -1) < pos_before) {
	    /* L'element courant est strictement la 
	       position de l'element ajoute.
	       Donc on ne va pas plus loin.
	    */
#ifdef DEBUG    
    (void) fprintf (stderr,"1 Incr. [el] est avant Pos\n");
#endif /* DEBUG */    
	  break;
	}
	else if (tmp -> pos == pos_before) {
	    /*
	       On est sur le debut d'un element de couleur.
	       On change simplement la position de l'element.
	    */
	    tmp -> pos += nbytes;
#ifdef DEBUG    
    (void) fprintf (stderr,"2 Incr. Pos == debut\n");
#endif /* DEBUG */    
	}	    
	else if ((tmp -> pos < pos_before)
	    && ((tmp -> pos + tmp -> len -1) >= pos_before)) {
	    /* 
	       On est strictement a l'interieur d'un element de couleur.
	       Il faut simplement incrementer la longueur de l'element.
	    */
	    tmp -> len += nbytes;
#ifdef DEBUG    
    (void) fprintf (stderr,"3 Incr in Color List. Pos est dans [el] \n");
#endif /* DEBUG */    
	}
	else if (tmp -> pos > pos_before){
	    tmp -> pos += nbytes;
#ifdef DEBUG    
    (void) fprintf (stderr,"4 Incr. [el] est apre Pos\n");
#endif /* DEBUG */    
	}
	else
	  ;
#ifdef DEBUG    
    (void) fprintf (stderr,"Incr len = %d pos = %d\n",
		    tmp -> len, tmp -> pos);
#endif /* DEBUG */    
	tmp = tmp -> previous;
    } while (tmp != 0) ;

#ifdef DEBUG    
    /* DisplayColorList (text); */
#endif /* DEBUG */    
}

/*
**	Function name : DecrColorList
**
**	Description :
**	Input : Le text courant, le nombre de bytes qui ont
**                  ete enleves.
**	Output :
*/
static void DecrColorList (text, nbytes)
    Text *text;
    int nbytes; /* > 0 */
{
    ColorElement *ce, *next, *previous;
    ColorElement *tmp = text -> last_ce;
    int pos_before, pos_after;
  
    pos_before = ie_current_position (text) + nbytes;
    pos_after = pos_before - nbytes;

/* #define DEBUG */
    
    /* On parcout la liste de elements colories en partant
       du dernier. On s'arrete lorsque l'element courant
       est stritement avant la position courante. En effet,
       a partir de la, les positions des elements restent
       inchanges */
      
    do {
	if ((tmp -> pos + tmp -> len -1) < pos_after) {
#ifdef DEBUG    
	    (void) fprintf (stderr,"Bye [el] < pos\n");
#endif /* DEBUG */
	    /* 1 On se casse, la position de l'element courant est
	       strictement avant la position courante
	       
	       ---++++++++++---********--------
	                       p      pb
	    */
	    break;
	}
	else if ((tmp -> pos >= pos_after)
		 && ((tmp -> pos + tmp -> len -1) < pos_before)) {
	    /* 2 On doit enlever cet element 
	       ---++++++++++********--------
	          p                  pb
	    */ 
#ifdef DEBUG
	    (void) fprintf (stderr,"Remove [el]\n");
	    (void) fprintf (stderr,"  pos <= [el] < pos_before rm\n");
#endif /* DEBUG */
	    
	    if (tmp -> previous) {
	      (tmp -> previous) -> next = (ColorElement *) tmp -> next;
#ifdef DEBUG    
	    (void) fprintf (stderr,"    previous != 0\n");
#endif /* DEBUG */
	    }
	    else {
		/* First element */
#ifdef DEBUG    
	    (void) fprintf (stderr,"    first [el]\n");
#endif /* DEBUG */
	    }
	    if (tmp -> next) {
	      (tmp -> next) -> previous = (ColorElement *) tmp -> previous;
#ifdef DEBUG    
	    (void) fprintf (stderr,"    next != 0\n");
#endif /* DEBUG */
	    }
	    else {
		/* Last element */
#ifdef DEBUG    
	    (void) fprintf (stderr,"    last [el]\n");
#endif /* DEBUG */
		text -> last_ce = tmp -> previous;
	    }
	    
	    /* On enleve l'element et on met les liens a jour. */
	    ce = tmp -> previous;
	    if (text -> current_ce = tmp)
	      text -> current_ce = tmp -> previous;
	    (void) free (tmp);
	    
	    tmp = ce;
	    if (tmp == 0) {
	      break;
	    }
	    else {
	      continue;
	    }
	}
	else if (tmp -> pos >= pos_before) {
	    tmp -> pos -= nbytes;
	    /* 3 On update seulement la position 
	       l'element
	    --*******+++++++------
	      p      pb 
	    */
#ifdef DEBUG    
	    (void) fprintf (stderr,"  3 pos < [el], update pos : -nbytes\n");
#endif /* DEBUG */    
	}
	else if ((tmp -> pos <= pos_after)
		 && ((tmp -> pos + tmp -> len -1) >= pos_before)){
	    tmp -> len -= nbytes;
	    /* 4 On est dans le cas suivant
	     ---++++++++++----
	     pppp        pbpbpb
	     */
	    if(tmp -> len == 0) {
#ifdef DEBUG    
	    (void) fprintf (stderr, "1 brrr,,,  len = 0\n" );	
#endif /* DEBUG */    
	    }
#ifdef DEBUG    
	    (void) fprintf (stderr,
			    "  4 pos >= [el] >= pos_before update len : -nbytes\n");
#endif /* DEBUG */    
	}
	else if ((tmp -> pos > pos_after)
		 && ((tmp -> pos + tmp -> len -1) >= pos_before)){
	    /* 5 On est dans le cas suivant
	       -----+++++++-----
	       p    el    pb
	    */
#ifdef DEBUG    
	    (void) fprintf (stderr,"  5 pos < [el] >= pos_before\n");
#endif /* DEBUG */
	    tmp -> len -= (pos_before - tmp -> pos - 1);
	    tmp -> pos -= (nbytes - (pos_before - tmp -> pos));
	    
#ifdef DEBUG    
	    (void) fprintf (stderr, "pos_before = %d tmp -> pos = %d\n",
			   pos_before,tmp -> pos );
#endif /* DEBUG */
	    if(tmp ->len == 0) {
#ifdef DEBUG    
	    (void) fprintf (stderr, "2 brrr,,,  len = 0\n" );	
#endif /* DEBUG */    
	    }
	}
	else {
#ifdef DEBUG    
	    (void) fprintf (stderr,"nothing\n");
#endif /* DEBUG */    
	  ;
	}

	tmp = tmp -> previous;
    } while (tmp != 0);
#ifdef DEBUG    
/*    DisplayColorList (text); */
#endif /* DEBUG */    
#undef DEBUG    
}

/*
**	Function name : ColorLineInPage
**
**	Description : Affichage d'une ligne avec eventuellement des
**         elements de couleur.
**  
**	Input : Le text courant, la position en pixel du premier
**         caractere a afficher, le numero de la ligne dans la
**         page, le chaine et sa longueur.
**	Output :
*/
void ColorLineInPage ( text, x, y, line, s, n )
    Text *text;
    int x, y, line;
    char *s;
    int n;
{
    int pos; /* Position de s au sens SMAC */
    ColorElement *tmp = 0;
    int nce, fnext, fprevious;
    
    /* DEBUG */
    if ( text -> current_ce == 0 )
      return;

#ifdef DEBUG
    (void) fprintf (stderr,"LIGNE %d n1 = %d\n", line, text -> n1);
#endif DEBUG    

    if ( line == text -> n1 + 1 ) {
	/*
	   On est sur la ligne du curseur.
	   Le pointeur s ne correspond pas a une position dans
	   le buffer. En fait il pointe sur une copie de la ligne
	   courante. Il faut donc rechercher le debut de la ligne
	   et ajouter eventuellement hs_delta dans le cas d'une
	   ligne scrollee horizontalement;
	*/
	pos = ie_beginning_of_line(text) + text -> page.hs_delta;
	/*
	   Si ya des tab au debut de la ligne ,
	   il ne faut pas en tenir compte
	*/
	while (ie_the_char(text,pos) == '\t') {
	    pos++;
	}
#ifdef DEBUG
    (void) fprintf (stderr,"\tcurrent line = %d pos = %d hs = %d char = %c\n",
		    line, pos, text -> page.hs_delta, ie_the_char(text,pos));
#endif /* DEBUG */
    }
    else {
	/*
	   s correspond bien a une position dans le buffer.
	   Deux cas sont possibles : s est avant ou apres la bulle.
	 */
	if ( s <= LeftBuf(text->buf) 
	    && LeftBuf(text->buf) != TopBuf(text->buf)) {
	    pos = s - TopBuf(text->buf);
#ifdef DEBUG
    (void) fprintf (stderr,"\tleft line = %d pos = %d char = %c\n",
		    line, pos, ie_the_char(text,pos));
#endif /* DEBUG */
	}
	else {
	    if ( LeftBuf(text->buf) != TopBuf(text->buf))
	      pos = s - RightBuf(text->buf) 
		+ (LeftBuf(text->buf)-TopBuf(text->buf)) + 1;
	    else
	      pos = s - RightBuf(text->buf);
#ifdef DEBUG
    (void) fprintf (stderr,"\tright line = %d pos = %d char = %c\n",
		    line, pos, ie_the_char(text,pos));
#endif /* DEBUG */
	}
    }
    
    /*
       A partir de la, on doit donc afficher une ligne
       de 'n' caracteres commencant a la position pos (SMAC).
       Il faut donc rechercher les elements de couleurs
       compris totalement ou partiellement dans les 
       bornes pos et pos + n.
       On cherche donc le premier element concerne.
    */
    tmp = text -> current_ce;
    
    fnext = False;
    fprevious = False;
    for(;;) {
	if ((pos >= tmp -> pos) && (pos <= (tmp -> pos + tmp -> len -1))) {
	    /* 	Trouve, le debut de la ligne est a l'interieur d'un element. */
#ifdef DEBUG
    (void) fprintf (stderr,"\tLe debut de la ligne est a l'interieur d'un element\n");
#endif /* DEBUG */    
	    break;
	}
	else if ((tmp -> pos > pos) && (tmp -> pos <= (pos + n -1))) {
	    /* Un element de couleur commence dans la ligne. */
#ifdef DEBUG
    (void) fprintf (stderr,"\tUn element de couleur commence dans la ligne.\n");
#endif /* DEBUG */    
	    if ( tmp -> previous == 0 || fnext ) {
#ifdef DEBUG
    (void) fprintf (stderr,"\t\tPas d'element avant ou on en vient\n");
#endif /* DEBUG */    
		break;
	    }
	    /* Allons voir si le precedent est toujours dans la ligne. */
	    tmp = tmp -> previous;
	    fnext = False;
	    fprevious = False;
	    continue;
	}
	else if (pos > (tmp -> pos + tmp -> len -1)) {
	    /* On va plus loin si possible */
#ifdef DEBUG
    (void) fprintf (stderr,"\tOn va plus loin si possible\n");
#endif /* DEBUG */    
	    if ( (fprevious == True)
		|| ((tmp = tmp -> next) == 0)
		|| ((pos + n -1) < tmp -> pos) ) {
		/* 
		   Pas d'element de couleur dans la ligne
		*/
		tmp = 0;
#ifdef DEBUG
    (void) fprintf (stderr,"\t\tPlus loin si possible...pas d'element dans la ligne\n");
#endif /* DEBUG */    
		break;
	    }
	    fnext = True;
	}
	else if ((pos + n -1) < tmp -> pos) {
	    /* On va sur l'element precedent si possible */
#ifdef DEBUG
    (void) fprintf (stderr,"\tOn va sur l'element precedent si possible\n");
#endif /* DEBUG */    
	    if ( (fnext == True)
		|| ((tmp = tmp -> previous) == 0)
		|| ((tmp -> pos + tmp -> len -1) < pos) ) {
		/* 
		   Pas d'element de couleur dans la ligne.
		*/
		tmp = 0;
#ifdef DEBUG
    (void) fprintf (stderr,"\t\tElement precedent...pas d'element dans la ligne\n");
#endif /* DEBUG */  
		break;
	    }
	    fprevious = True;
	}
	else {
	  ; /* Brrrr.... */
#ifdef DEBUG
	  (void) fprintf (stderr,"BRRR...\n");
#endif /* DEBUG */
	}
    }
#undef DEBUG
    
    if ( tmp == 0 ) {
	XDrawImageString ( dpy, text -> window, text -> Cgc, x, y, s, n );
#ifdef DEBUG
	(void) fprintf (stderr,"\tPas d'elements de couleur\n");
#endif DEBUG    
	return;
    }
    text -> current_ce = tmp;
    /*
       L'element de couleur trouve est le plus pres de la ligne
       a afficher ou bien il se trouve a l'interieur de celle-ci,
       partiellement ou totalement.
    */
#ifdef DEBUG
    (void) fprintf (stderr,"LIGNE %d\n", line);
#endif DEBUG    

    while ( n > 0 ) {
#ifdef DEBUG
    (void) fprintf (stderr,"tmp = %d pos = %d\n", tmp, pos);
#endif DEBUG    
	if (pos == tmp -> pos) {
	    /* La position courante est sur un element de couleur */
	    if (n >= tmp -> len)
	      nce = tmp -> len;
	    else
	      nce = n;
	    XSetForeground (dpy, text -> Cgc, tmp -> color );
	    XDrawImageString ( dpy, text -> window, text -> Cgc, x, y, s, nce );
	    XSetForeground ( dpy, text -> Cgc, text -> fg );
#ifdef DEBUG
	    (void) fprintf (stderr,"\tpos est sur le debut d'un element de couleur\n");
	    write(1,s,nce);
	    write(1,"\n",1);
#endif /* DEBUG */
	}
	else if (pos > tmp -> pos ) {
	    nce = tmp -> len - (pos - tmp -> pos);
	    if (nce > n)
	      nce = n;
	    XSetForeground (dpy, text -> Cgc, tmp -> color );
	    XDrawImageString ( dpy, text -> window, text -> Cgc, x, y, s, nce );
	    XSetForeground ( dpy, text -> Cgc, text -> fg );
#ifdef DEBUG
	    (void) fprintf (stderr,"\tpos est apres le debut de l'element\n");
	    write(1,s,nce);
	    write(1,"\n",1);
#endif /* DEBUG */
	}
	else { /* pos < tmp -> pos */
	    nce = tmp -> pos - pos;
	    XDrawImageString ( dpy, text -> window, text -> Cgc, x, y, s, nce );
#ifdef DEBUG
	    (void) fprintf (stderr,"\tpos est avant le debut de l'element\n");
	    write (1,s,nce);
	    write(1,"\n",1);
#endif /* DEBUG */

	    x += XTextWidth ( text -> font, s, nce );
	    n -= nce;
	    s += nce;
	    pos += nce;
	    /* Maintenenant, l'element lui-meme au prochain tour */
	    continue;
	}
	
	x += XTextWidth ( text -> font, s, nce );
	n -= nce;
	s += nce;
	pos += nce;
	if ( ((tmp = tmp -> next) == 0)
	    || (tmp -> pos > (pos + n -1)) ) {
	    break;
	}
    }

    XSetForeground ( dpy, text -> Cgc, text -> fg );
    if ( n > 0) {
	/* Il reste des caracteres a afficher */
#ifdef DEBUG
	(void) fprintf (stderr,"\tLa fin de la ligne\n");
	write (1,s,n);
	write(1,"\n",1);
#endif /* DEBUG */
	XDrawImageString ( dpy, text -> window, text -> Cgc, x, y, s, n );
    }
}

/*
**	Function name : GetColorCursor
**
**	Description :
**	Input :
**	Output :
*/
unsigned long GetCursorColor (text)
    Text *text;
{
    int pos = ie_current_position (text);
    ColorElement *tmp = text -> current_ce;
    int fnext, fprevious;
  
    if (tmp == 0)
      return (text -> fg ^ text -> bg);
    
    /*
       Il faut rechercher si la position courante est dans
       un element de couleur.
    */
    fnext = False;
    fprevious = False;
    for(;;) {
	if ( pos > (tmp -> pos + tmp -> len - 1) ) {
	    /* Il faut aller voir apres */
	    if ((fprevious == True)
		|| (tmp = tmp -> next) == 0) {
	      tmp = 0;
	      break;
	    }
	    fnext = True;
	}
	else if ( pos < tmp -> pos ) {
	    /* Il faut aller voir avant */
	    if ((fnext == True)
		|| (tmp = tmp -> previous) == 0) {
	      tmp = 0;
	      break;
	    }
	    fprevious = True;
	}
	else {
	    /* On est a l'interieur d'un element */
	    break;
	}
    }
    if (tmp == 0) {
	return (text -> fg ^ text -> bg);
    }
    else {
	text -> current_ce = tmp;
	return (tmp -> color ^ text -> bg );
    }
}


